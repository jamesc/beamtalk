# beamtalk-process

Shared leaf crate for the OS child-process mechanics that several Beamtalk
binaries need (BT-5195):

- `winjob` (Windows): a kill-on-close Job Object, and `spawn_in_job`, which
  starts a child suspended, assigns it to the job, then resumes it, so no
  grandchild is ever born outside the job. Used by `beamtalk-exec` and
  `beamtalk-desktop-broker`.
- `escalate`: the SIGTERM → grace → SIGKILL policy loop. Used by
  `beamtalk-exec`, `beamtalk-desktop-broker` (`SpawnedFront::stop`) and
  `beamtalk-workspace` (`pid_liveness::kill_process`).
- `exit_code`: the shell convention for a child's exit status (`128 + signal`
  for a signal death). Used by `beamtalk-exec` and `beamtalk run`.

It has no dependencies beyond the platform bindings, so any crate can sit on
it.
