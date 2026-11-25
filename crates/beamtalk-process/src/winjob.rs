// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Windows Job Object wrapper, shared by `beamtalk-exec` and
//! `beamtalk-desktop-broker` (BT-5195).
//!
//! A Windows Job Object with `JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE` ties a
//! child's whole process tree to the job handle: every process assigned to
//! the job, plus every child *those* processes spawn afterward, is terminated
//! together the moment the job's last handle closes, including when the
//! owning process itself terminates uncleanly (a crash, `taskkill /F`,
//! anything), since Windows always tears down a dead process's handle table
//! as part of exit, with no cooperation from that process required.
//!
//! The desktop broker needs this because it launches `bin\bt_attach.bat`:
//! `std::process::Command` routes a `.bat` through `cmd.exe /c`, so the
//! process `Child` tracks is `cmd.exe`, and `Child::kill` alone would orphan
//! the `erl.exe` underneath it. `beamtalk-exec` needs it so a subprocess tree
//! can be killed as a group and dies with the port.
//!
//! **Assign before the child runs, so there is no pre-assignment window**
//! (BT-3880): [`JobHandle::assign`] can only run after
//! [`std::process::Command::spawn`] returns, and `AssignProcessToJobObject`
//! covers the named process plus whatever it spawns *afterwards*, so a
//! grandchild the child had already launched by then is born outside the
//! job and survives the kill-on-close. For a `.bat` that window is real:
//! `cmd.exe` parses the batch file and `CreateProcess`es `erl.exe` within
//! milliseconds of starting, and a thread descheduled between `spawn()` and
//! `assign()` on a loaded machine loses that race (observed on CI as a
//! `ping.exe` grandchild surviving its `cmd.exe`'s job being closed).
//! [`spawn_in_job`] therefore spawns the child with [`CREATE_SUSPENDED`],
//! assigns it while it has not yet executed a single instruction, and only
//! then releases it via [`resume_initial_thread`], so every descendant is
//! created inside the job by construction. If either step fails, the child
//! is killed and reaped rather than left running outside any job (or
//! suspended forever). `std::process::Child` exposes the process handle but
//! not the initial thread's, so [`resume_initial_thread`] finds that thread
//! again by owner PID through a Toolhelp thread snapshot rather than
//! reimplementing `CreateProcessW` to keep the thread handle it returns.
//!
//! **DDD Context:** Shared Infrastructure

use std::io;
use std::os::windows::io::AsRawHandle;
use std::process::{Child, Command};

use windows_sys::Win32::Foundation::{CloseHandle, FALSE, HANDLE, INVALID_HANDLE_VALUE};
use windows_sys::Win32::System::Diagnostics::ToolHelp::{
    CreateToolhelp32Snapshot, TH32CS_SNAPTHREAD, THREADENTRY32, Thread32First, Thread32Next,
};
use windows_sys::Win32::System::JobObjects::{
    AssignProcessToJobObject, CreateJobObjectW, JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE,
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION, JobObjectExtendedLimitInformation,
    SetInformationJobObject, TerminateJobObject,
};
use windows_sys::Win32::System::Threading::{OpenThread, ResumeThread, THREAD_SUSPEND_RESUME};

/// `CreateProcessW`'s `CREATE_SUSPENDED` creation flag: the new process's
/// initial thread starts suspended and executes nothing until
/// [`resume_initial_thread`] releases it. A caller of [`spawn_in_job`] ORs
/// this into the command's creation flags so [`JobHandle::assign`] can run
/// before the child does — see this module's doc comment.
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
    /// this job. Tests use it to pick the grandchild that is actually a
    /// member of a job, and to fail precisely if the assign-before-run
    /// guarantee in this module's doc comment ever regresses.
    ///
    /// # Errors
    ///
    /// Returns the OS error if `IsProcessInJob` fails (e.g. `process` lacks
    /// the query access right).
    ///
    /// # Safety
    ///
    /// `process` must be an open process handle that stays open for the
    /// duration of the call.
    pub unsafe fn contains_process(&self, process: HANDLE) -> io::Result<bool> {
        use windows_sys::Win32::System::JobObjects::IsProcessInJob;

        let mut result = FALSE;
        // SAFETY: `process` is a valid open handle per this function's
        // safety contract; `self.0` is a valid job handle owned by `self`; `result`
        // is a live out-pointer for the duration of the call.
        let ok = unsafe { IsProcessInJob(process, self.0, &raw mut result) };
        if ok == 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(result != FALSE)
    }

    /// Terminate every process in the job with `exit_code`.
    ///
    /// # Errors
    ///
    /// Returns the OS error if `TerminateJobObject` fails.
    pub fn terminate(&self, exit_code: u32) -> io::Result<()> {
        // SAFETY: `self.0` is a valid job handle owned by `self`.
        let ok = unsafe { TerminateJobObject(self.0, exit_code) };
        if ok == 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(())
    }
}

/// Spawn `cmd` inside a fresh kill-on-close job, with no window in which the
/// child (or anything it starts) runs outside the job.
///
/// The caller must have set [`CREATE_SUSPENDED`] in `cmd`'s creation flags
/// (`CommandExt::creation_flags` replaces rather than adds, so only the
/// caller can combine it with its own flags). The child is assigned to the
/// job while suspended and then resumed; if either step fails it is killed
/// and reaped before the error is returned, so a failure never leaves a
/// process running outside a job or suspended forever.
///
/// # Errors
///
/// Creating the job, spawning, assigning or resuming fails.
pub fn spawn_in_job(cmd: &mut Command) -> io::Result<(Child, JobHandle)> {
    spawn_in_job_with(cmd, JobHandle::assign)
}

/// [`spawn_in_job`] with the assignment step injected, so a test can make it
/// fail and observe that the child is killed.
fn spawn_in_job_with(
    cmd: &mut Command,
    assign: impl FnOnce(&JobHandle, &Child) -> io::Result<()>,
) -> io::Result<(Child, JobHandle)> {
    // Created before `spawn`, so a failure here has nothing to unwind.
    let job = JobHandle::new()?;
    let mut child = cmd.spawn()?;
    if let Err(e) = assign(&job, &child).and_then(|()| resume_initial_thread(&child)) {
        // `TerminateProcess` (what `Child::kill` is on Windows) works on a
        // still-suspended process too. Best-effort: the setup error is the
        // one worth reporting.
        let _ = child.kill();
        let _ = child.wait();
        return Err(e);
    }
    Ok((child, job))
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
/// inside its job; [`spawn_in_job`] kills it rather than returning a child
/// that will never run.
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
        // module exists to get "for free" on ungraceful owner exit too,
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

#[cfg(test)]
mod tests {
    use super::*;
    use windows_sys::Win32::Foundation::STILL_ACTIVE;
    use windows_sys::Win32::System::Threading::{
        GetExitCodeProcess, OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION,
    };

    const CREATE_NO_WINDOW: u32 = 0x0800_0000;

    /// A child that would run for a minute, started suspended.
    fn long_running() -> Command {
        use std::os::windows::process::CommandExt;
        let mut cmd = Command::new("ping");
        cmd.args(["-n", "60", "127.0.0.1"])
            .stdout(std::process::Stdio::null())
            .creation_flags(CREATE_NO_WINDOW | CREATE_SUSPENDED);
        cmd
    }

    #[test]
    fn spawn_in_job_assigns_the_child_and_resumes_it() {
        let (mut child, job) = spawn_in_job(&mut long_running()).expect("spawn_in_job");
        let handle = child.as_raw_handle().cast::<core::ffi::c_void>();
        // SAFETY: `handle` is `child`'s process handle, open while `child` lives.
        let in_job = unsafe { job.contains_process(handle) };
        assert!(in_job.expect("IsProcessInJob"));
        // Resumed: still running (a suspended child would be too, so also
        // check it ends when the job is terminated).
        assert!(child.try_wait().expect("try_wait").is_none());
        job.terminate(7).expect("terminate");
        assert_eq!(child.wait().expect("wait").code(), Some(7));
    }

    #[test]
    fn assignment_failure_kills_the_child() {
        let mut observed: Option<HANDLE> = None;
        let result = spawn_in_job_with(&mut long_running(), |_, child| {
            // SAFETY: plain Win32 call on the PID of a live child we hold;
            // the returned handle is checked and closed below.
            let h = unsafe { OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, child.id()) };
            assert!(!h.is_null(), "OpenProcess failed");
            observed = Some(h);
            Err(io::Error::other("injected assignment failure"))
        });
        assert!(result.is_err());
        let h = observed.expect("assign was called");
        let mut code = 0u32;
        // SAFETY: `h` is the valid handle opened above; `code` is a live
        // out-pointer.
        let ok = unsafe { GetExitCodeProcess(h, &raw mut code) };
        // SAFETY: `h` is not used after this.
        unsafe { CloseHandle(h) };
        assert_ne!(ok, 0, "GetExitCodeProcess failed");
        assert_ne!(
            i32::try_from(code).ok(),
            Some(STILL_ACTIVE),
            "the child must be killed when it cannot be assigned"
        );
    }
}
