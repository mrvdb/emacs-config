;;; worklog-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (worklog-summarize-tasks-between-dates worklog-summarize-tasks
;;;;;;  worklog-quick-stop worklog-quick-start worklog-task-done
;;;;;;  worklog-task-stop worklog-task-begin worklog-do-task) "worklog"
;;;;;;  "worklog.el" (19289 33379))
;;; Generated autoloads from worklog.el

(autoload 'worklog-do-task "worklog" "\
Append TASK to the worklog.
If there is an ongoing task, you are given the option to declare it done or
stopped, or to cancel the operation.

`worklog-do-task' allows you to insert tasks into your worklog without
the need to interactively call it.

If autostop is set to non-nil, any running task will automatically be stopped.

\(fn TASK &optional AUTOSTOP)" nil nil)

(autoload 'worklog-task-begin "worklog" "\
Convenience function for `worklog-do-task'.

Does the same as calling`worklog-do-task' with a string parameter.

\(fn TASK)" t nil)

(autoload 'worklog-task-stop "worklog" "\
Append a \"stop\" entry to the worklog.

\(fn)" t nil)

(autoload 'worklog-task-done "worklog" "\
Append a \"done\" entry to the worklog.

\(fn)" t nil)

(autoload 'worklog-quick-start "worklog" "\
Quickly start a task, without switching to the worklog buffer

\(fn TASK)" t nil)

(autoload 'worklog-quick-stop "worklog" "\
Quickly stop a task, without switching to the worklog buffer

\(fn)" t nil)

(autoload 'worklog-summarize-tasks "worklog" "\
Display a summary of the worklog in two sections.
The first section is a reverse-chronological list of tasks and their durations,
and the second is an unsorted compendium of all tasks and total durations.
Durations are measured in hours.  If invoked non-interactively (i.e., \"emacs
--batch\"), the display buffer is sent to `message'.

This summary is displayed over the entire worklog file, it is an convenience 
function for `worklog-do-summarize-tasks-between-dates'

\(fn)" t nil)

(autoload 'worklog-summarize-tasks-between-dates "worklog" "\
Display a summary of the worklog in two sections.
The first section is a reverse-chronological list of tasks and their durations,
and the second is an unsorted compendium of all tasks and total durations.
Durations are measured in hours.  If invoked non-interactively (i.e., \"emacs
--batch\"), the display buffer is sent to `message'.

This summary is displayed over a section of the worklog file, it is an convenience 
function for `worklog-summarize-tasks-between-dates'

\(fn FROM TO)" t nil)

;;;***

;;;### (autoloads nil nil ("worklog-pkg.el") (19289 33380 13772))

;;;***

(provide 'worklog-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; worklog-autoloads.el ends here
