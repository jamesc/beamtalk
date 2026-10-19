// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Windows Job Object wrapper.
//!
//! **Why this exists at all:** `crate::spawn`'s Windows launch path invokes
//! `bin\bt_attach.bat` — a `.bat`, which `CreateProcessW` cannot execute
//! directly. Since Rust 1.77.2 (the "BatBadBut" `CVE-2024-24576` fix),
//! `std::process::Command` silently routes any `.bat`/`.cmd` program through
//! `cmd.exe /c` instead of failing. That means the process
//! [`std::process::Child`] actually tracks is **`cmd.exe`**, not the BEAM VM
//! `bin\bt_attach.bat` eventually execs into — so `Child::id()` is the wrong
//! PID for [`crate::sname::predict_node_name`] (Windows never uses that
//! prediction, by design — see `sname`'s module doc for the
//! epmd-query replacement), and plain `Child::kill()` only terminates
//! `cmd.exe`, orphaning `erl.exe` underneath it (with a live workspace cookie
//! and a bound port) every time a front is detached.
//!
//! A Windows Job Object with `JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE` fixes the
//! orphan half of that (not the PID-prediction half — nothing here changes
//! what `Child::id()` reports): every process assigned to the job, plus
//! every child *those* processes spawn afterward, is terminated together the
//! moment the job's last handle closes — including when this broker process
//! itself terminates uncleanly (a crash, `taskkill /F`, anything), since
//! Windows always tears down a dead process's handle table as part of exit,
//! with no cooperation from that process required. That is strictly better
//! than `crate::reap`'s PID-file sweep (a *next-restart* net): the tree dies
//! with the broker instead of surviving until some future broker start
//! happens to run a sweep.
//!
//! **Assign before the child runs, so there is no pre-assignment window**
//! (BT-3880): [`JobHandle::assign`] can only run after
//! [`std::process::Command::spawn`] returns, and `AssignProcessToJobObject`
//! covers the named process plus whatever it spawns *afterwards* — a
//! grandchild the child had already launched by then is born outside the
//! job and survives the kill-on-close. For a `.bat` front that window is
//! real: `cmd.exe` parses the batch file and `CreateProcess`es `erl.exe`
//! within milliseconds of starting, and a broker thread descheduled between
//! `spawn()` and `assign()` on a loaded machine loses that race (observed
//! on CI as a `ping.exe` grandchild surviving its `cmd.exe`'s job being
//! closed). `crate::spawn::spawn_front` therefore spawns the child with
//! [`CREATE_SUSPENDED`], assigns it while it has not yet executed a single
//! instruction, and only then releases it via [`resume_initial_thread`] —
//! so every descendant is created inside the job by construction, however
//! the broker happens to be scheduled. `std::process::Child` exposes the
//! process handle but not the initial thread's, so [`resume_initial_thread`]
//! finds that thread again by owner PID through a Toolhelp thread snapshot
//! rather than reimplementing `CreateProcessW` to keep the thread handle it
//! returns.
//!
//! **DDD Context:** Desktop Shell

use std::io;
use std::os::windows::io::AsRawHandle;
use std::process::Child;

use windows_sys::Win32::Foundation::{CloseHandle, FALSE, HANDLE, INVALID_HANDLE_VALUE};
use windows_sys::Win32::System::Diagnostics::ToolHelp::{
    CreateToolhelp32Snapshot, TH32CS_SNAPTHREAD, THREADENTRY32, Thread32First, Thread32Next,
};
use windows_sys::Win32::System::JobObjects::{
    AssignProcessToJobObject, CreateJobObjectW, JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE,
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION, JobObjectExtendedLimitInformation,
    SetInformationJobObject,
};
use windows_sys::Win32::System::Threading::{OpenThread, ResumeThread, THREAD_SUSPEND_RESUME};

/// `CreateProcessW`'s `CREATE_SUSPENDED` creation flag: the new process's
/// initial thread starts suspended and executes nothing until
/// [`resume_initial_thread`] releases it. `crate::spawn` ORs this into the
/// front's creation flags so [`JobHandle::assign`] can run before the child
/// does — see this module's doc comment.
pub const CREATE_SUSPENDED: u32 = windows_sys::Win32::System::Threading::CREATE_SUSPENDED;

/// RAII handle to an anonymous Windows Job Object configured with
/// `JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE` — see this module's doc comment for
/// why that flag specifically. Dropping (or the owning process dying) closes
/// the handle and kills every process still assigned to it.
#[derive(Debug)]
pub struct JobHandle(HANDLE);

impl JobHandle {
    /// Create a new job object with the kill-on-close limit already set.
    ///
    /// # Errors
    ///
    /// Returns the OS error if `CreateJobObjectW` or `SetInformationJobObject`
    /// fails.
    ///
    /// # Panics
    ///
    /// Never in practice: `size_of::<JOBOBJECT_EXTENDED_LIMIT_INFORMATION>()`
    /// is a small, fixed struct size that always fits in a `u32`; the
    /// `try_from`/`expect` here exists only to avoid an `as` truncating cast,
    /// not because this can realistically overflow.
    pub fn new() -> io::Result<Self> {
        // SAFETY: FFI per the documented Win32 contract. A null security-
        // attributes pointer and null name are both explicitly valid inputs
        // (default security descriptor, anonymous job) — nothing here reads
        // through the returned handle except via the further-checked calls
        // below.
        let handle = unsafe { CreateJobObjectW(std::ptr::null(), std::ptr::null()) };
        if handle.is_null() {
            return Err(io::Error::last_os_error());
        }
        let job = Self(handle);

        // SAFETY: zero-initializing this struct is valid — every field is a
        // plain integer/handle-sized type with no invariant zero would
        // violate (the same pattern Win32 docs themselves show for this
        // call: zero everything, then set only the one flag needed).
        let mut info: JOBOBJECT_EXTENDED_LIMIT_INFORMATION = unsafe { std::mem::zeroed() };
        info.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
        // SAFETY: `info` is a validly-initialized, correctly-sized instance
        // of the struct this information class expects; `job.0` is the
        // just-created, still-owned handle.
        let ok = unsafe {
            SetInformationJobObject(
                job.0,
                JobObjectExtendedLimitInformation,
                std::ptr::addr_of!(info).cast(),
                u32::try_from(std::mem::size_of::<JOBOBJECT_EXTENDED_LIMIT_INFORMATION>())
                    .expect("struct size fits in u32"),
            )
        };
        if ok == 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(job)
    }

    /// Assign `process` — and everything it spawns from this point on — to
    /// this job. Only descendants created *after* this call are covered, so
    /// `process` should still be suspended (spawned with
    /// [`CREATE_SUSPENDED`]) when it runs — see this module's doc comment.
    ///
    /// # Errors
    ///
    /// Returns the OS error if `AssignProcessToJobObject` fails (e.g. the
    /// process already exited before this call ran).
    pub fn assign(&self, process: &Child) -> io::Result<()> {
        let process_handle = process.as_raw_handle().cast::<core::ffi::c_void>();
        // SAFETY: `process_handle` is a valid, currently-open process handle
        // owned by `process` for the duration of this call; `self.0` is a
        // valid job handle owned by `self`.
        let ok = unsafe { AssignProcessToJobObject(self.0, process_handle) };
        if ok == 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(())
    }

    /// Whether `process` (an open process handle with at least
    /// `PROCESS_QUERY_LIMITED_INFORMATION` access) is currently assigned to
    /// this job. Test-only: `crate::spawn`'s process-tree-kill test uses it
    /// to pick the grandchild that is actually a member of the front's job,
    /// and to fail precisely if the assign-before-run guarantee in this
    /// module's doc comment ever regresses.
    ///
    /// # Errors
    ///
    /// Returns the OS error if `IsProcessInJob` fails (e.g. `process` lacks
    /// the query access right).
    #[cfg(test)]
    pub(crate) fn contains_process(&self, process: HANDLE) -> io::Result<bool> {
        use windows_sys::Win32::System::JobObjects::IsProcessInJob;

        let mut result = FALSE;
        // SAFETY: `process` is a valid open handle per this function's
        // contract; `self.0` is a valid job handle owned by `self`; `result`
        // is a live out-pointer for the duration of the call.
        let ok = unsafe { IsProcessInJob(process, self.0, &raw mut result) };
        if ok == 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(result != FALSE)
    }
}

/// Release a process spawned with [`CREATE_SUSPENDED`], after
/// [`JobHandle::assign`] has attached it to its job.
///
/// `std::process::Child` keeps the process handle `CreateProcessW` returned
/// but closes the initial thread's, so that thread is located again here by
/// owner PID through a `TH32CS_SNAPTHREAD` snapshot. A freshly created,
/// still-suspended process has exactly one thread (nothing has run yet that
/// could create another), so resuming every thread owned by `process.id()`
/// is resuming the initial thread; the PID cannot have been recycled under
/// us because `process` still holds the process handle open.
///
/// # Errors
///
/// Returns the OS error if the thread snapshot, `OpenThread` or
/// `ResumeThread` fails, or an [`io::ErrorKind::NotFound`] error if the
/// snapshot lists no thread for `process` at all (it was terminated out from
/// under us before it ever ran). On any error the process is left suspended
/// inside its job — the caller kills it (`crate::spawn::kill_orphaned_child`)
/// rather than returning a front that will never boot.
pub fn resume_initial_thread(process: &Child) -> io::Result<()> {
    let pid = process.id();

    // SAFETY: plain Win32 call; the `th32ProcessID` argument is ignored for
    // thread snapshots (every thread on the system is listed — filtered by
    // owner below, per the API contract), and the returned handle is
    // checked before use.
    let snapshot = unsafe { CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0) };
    if snapshot == INVALID_HANDLE_VALUE {
        return Err(io::Error::last_os_error());
    }
    let result = resume_threads_of(snapshot, pid);
    // SAFETY: `snapshot` is a valid handle owned by this function, not used
    // again after this point.
    unsafe { CloseHandle(snapshot) };
    result
}

/// [`resume_initial_thread`]'s walk over an already-open `TH32CS_SNAPTHREAD`
/// `snapshot`, split out so the caller closes the snapshot after every exit
/// path of this function with a single `CloseHandle`.
fn resume_threads_of(snapshot: HANDLE, pid: u32) -> io::Result<()> {
    let mut entry = THREADENTRY32 {
        dwSize: u32::try_from(std::mem::size_of::<THREADENTRY32>())
            .expect("struct size fits in u32"),
        ..Default::default()
    };
    let mut resumed = 0u32;
    // SAFETY: `snapshot` is a valid, open thread snapshot owned by the
    // caller; `entry` is a validly-initialized, correctly-sized buffer for
    // this call (`dwSize` set per the API's documented requirement).
    if unsafe { Thread32First(snapshot, &raw mut entry) } != 0 {
        loop {
            if entry.th32OwnerProcessID == pid {
                resume_thread(entry.th32ThreadID)?;
                resumed += 1;
            }
            // SAFETY: same snapshot/entry as above, called in a loop until
            // it reports no more entries.
            if unsafe { Thread32Next(snapshot, &raw mut entry) } == 0 {
                break;
            }
        }
    }
    if resumed == 0 {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("no thread found for suspended process {pid}"),
        ));
    }
    Ok(())
}

/// `OpenThread` + `ResumeThread` + `CloseHandle` for one thread id.
fn resume_thread(thread_id: u32) -> io::Result<()> {
    // SAFETY: plain Win32 call; a null return (failure) is handled below.
    let thread = unsafe { OpenThread(THREAD_SUSPEND_RESUME, FALSE, thread_id) };
    if thread.is_null() {
        return Err(io::Error::last_os_error());
    }
    // SAFETY: `thread` is the valid handle just opened above. `ResumeThread`
    // returns the thread's previous suspend count, or `(DWORD)-1` on
    // failure — only the latter is an error (a count of 0 would mean the
    // thread was not suspended, which is harmless here).
    let previous = unsafe { ResumeThread(thread) };
    // SAFETY: `thread` is a valid handle owned by this function, not used
    // again after this point.
    unsafe { CloseHandle(thread) };
    if previous == u32::MAX {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

impl Drop for JobHandle {
    fn drop(&mut self) {
        // SAFETY: `self.0` is a valid handle owned by this struct, not
        // shared or already closed elsewhere. Closing it (rather than
        // calling `TerminateJobObject` first) is deliberate: with
        // `JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE` set, the close itself
        // terminates every assigned process — the exact behavior this
        // module exists to get "for free" on ungraceful broker exit too,
        // where no `Drop` runs at all and only the OS's own handle-table
        // teardown fires this.
        unsafe {
            CloseHandle(self.0);
        }
    }
}

// SAFETY: a job object HANDLE is not thread-affine — every Win32 API used
// here (`AssignProcessToJobObject`, `CloseHandle`, …) is documented safe to
// call from any thread holding the handle.
unsafe impl Send for JobHandle {}
// SAFETY: same as the `Send` impl above — no interior mutability here beyond
// the handle itself, and the underlying Win32 calls take `&self`/a raw
// handle value, never requiring exclusive access.
unsafe impl Sync for JobHandle {}
