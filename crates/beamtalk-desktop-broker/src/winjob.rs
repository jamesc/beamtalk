// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Windows Job Object wrapper (BT-2988, adversarial-review follow-up).
//!
//! **Why this exists at all:** `crate::spawn`'s Windows launch path invokes
//! `bin\bt_attach.bat` — a `.bat`, which `CreateProcessW` cannot execute
//! directly. Since Rust 1.77.2 (the "BatBadBut" `CVE-2024-24576` fix),
//! `std::process::Command` silently routes any `.bat`/`.cmd` program through
//! `cmd.exe /c` instead of failing. That means the process
//! [`std::process::Child`] actually tracks is **`cmd.exe`**, not the BEAM VM
//! `bin\bt_attach.bat` eventually execs into — so `Child::id()` is the wrong
//! PID for [`crate::sname::predict_node_name`] (fixed on Windows, BT-3045, by
//! not using that prediction there at all — see `sname`'s module doc for the
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
//! **Known residual race, not fully closable through `std::process::Command`**:
//! [`JobHandle::assign`] must run after [`std::process::Command::spawn`]
//! returns, so a child that spawns its own children in that brief window
//! would have those grandchildren born outside the job. The fully race-free
//! fix is `CREATE_SUSPENDED` + assign + `ResumeThread`, but
//! `std::process::Child` exposes the process handle, not the initial
//! thread — reaching `ResumeThread` would mean reimplementing
//! `CreateProcessW` by hand instead of going through `Command`. Calling
//! [`JobHandle::assign`] immediately after `spawn()` (as
//! `crate::spawn::spawn_front` does) minimizes the window to essentially
//! nothing in practice (a freshly-created `cmd.exe` has not yet parsed the
//! `.bat` it was told to run), but this has not been verified against a real
//! Windows boot — no Windows sandbox was available to test it in.
//!
//! **DDD Context:** Desktop Shell

use std::io;
use std::os::windows::io::AsRawHandle;
use std::process::Child;

use windows_sys::Win32::Foundation::CloseHandle;
use windows_sys::Win32::Foundation::HANDLE;
use windows_sys::Win32::System::JobObjects::{
    AssignProcessToJobObject, CreateJobObjectW, JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE,
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION, JobObjectExtendedLimitInformation,
    SetInformationJobObject,
};

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
    /// this job. See this module's doc comment for the residual assign-race
    /// this does not close.
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
