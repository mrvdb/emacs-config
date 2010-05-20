;
; Org mode specific settings
; TODO make this literate by using org-mode + org-babel

; Directly tie into the GIT repository on this machine
(add-to-list 'load-path "~/dev/emacs/packages/org-mode/lisp/")
(add-to-list 'load-path "~/dev/emacs/packages/org-mode/contrib/lisp/")
(add-to-list 'load-path "~/dev/emacs/packages/org-icons/lisp/")

; bootstrap
; FIXME: how do we know this does load the development tree instead of the builtin one?
(require 'org-install)
(require 'org-mouse)

;
; Org based extensions load as soon as possible 
(require 'org-babel-init)
; Languages we use
(require 'org-babel-emacs-lisp)
(require 'org-babel-sql)
(require 'org-babel-python)
(require 'org-babel-sh)
(require 'org-icons)
(require 'org-babel-ditaa)
(require 'org-babel-R)  ;; Could not get R to work properly
(org-babel-load-library-of-babel)

; Having the verbs is a bit disturbing
;(require 'org-action-verbs)
(setq org-action-todo-verbs
 '(
    (("TODO" "NEXT") . 
     ( "Assemble" "Build" "Buy" "Call" "Cancel" "Change" "Check" "Clean" "Collect" "Combine" "Configure" "Construct" "Create"
       "Decide" "Disassemble" "Document" "Exchange" "Find" "Finish" "Fix" "Improve" "Insert" "Install" "Learn" "Look"
       "Mail" "Measure"
       "Pay" "Paint" "Process" "Read" "Remove" "Rename" "Research" "Repair" "Replace" "Resolve" "Review" "Rewrite"
       "Schedule" "Sell" "Summarize" "Translate" "Try" "Update" "Upgrade" "Use" "Watch" "Write"
     )
    )
 )
)

; Keybindings we want to have available all the time
; even when not in org mode.
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cr" 'make-remember-frame)

; Keybindings which only make sense when having an orgmode file
(define-key org-mode-map "\C-ce" 'org-export)

; Map some keys to be the same as in Things.app
(define-key org-mode-map [(super .)] 'org-todo)
(define-key org-agenda-mode-map [(super .)] 'org-agenda-todo)

; Map ⌘t to schedule in both task and agenda-view
(define-key org-mode-map [(super t)] 'org-schedule)
(define-key org-agenda-mode-map [(super t)] 'org-agenda-schedule)

(setq
 ; Files and directories
 org-directory "~/.outlet/"                                     ; Main dir
 org-metadir (concat org-directory "_orgmeta/")                 ; Org system files go here
 org-mobile-directory "/plato:/var/dav/mrb/"                    ; Remote org dir
 org-archive-location (concat org-metadir "archive.org::* %s")  ; Default archive location

 org-default-notes-file (concat org-directory "GTD.org")
 diary-file (concat org-metadir "DIARY")
 org-mobile-inbox-for-pull (concat org-metadir "from-mobile.org")


 ; I want to hide the leading stars, and do it *exactly* in the
 ; background-color
 org-hide-leading-stars t
 org-hide-emphasis-markers t

 ; Which string signals that an outline is collapsed
 org-ellipsis "…"

 org-log-done (quote time)

 ; We support task dependencies
 org-enforce-todo-dependencies t
 
 ; We dont do priorities
 org-enable-priority-commands nil

 ; Tags
 org-tags-exclude-from-inheritance (quote ("prj"))
 org-tags-column 90
 org-agenda-tags-column 90

 ; Hide / and * markers when doing /italic/ and *bold* markup
 org-hide-emphasis-markers t

 ; Also in checkboxes
 org-enforce-todo-checkbox-dependencies t

 org-mobile-force-id-on-agenda-items nil

 ; Indent properly, in such a way that if we view the file without
 ; orgmode, it looks proper too.
 ; - use stars for level indication
 ; - make it look better by indent-mode having do its thing
 org-startup-indented t

 ; Agenda settings
 org-agenda-include-diary t
 org-agenda-start-with-log-mode t
 org-agenda-todo-ignore-scheduled "future"

 ; Habits
 org-habit-graph-column 100
 org-habit-show-habits-only-for-today nil

 ; Autolinks can be entered like [[keyword:parameter]]
 org-link-abbrev-alist
 '(("wiki" . "http://en.wikipedia.org/wiki/Search?search=")
   ("math" . "http://mathworld.wolfram.com/%s.html")
   ("google"   . "http://www.google.com/search?q="))

 ; Pressing enter on a link should activate it
 org-return-follows-link t
 org-support-shift-select (quote always)
 org-special-ctrl-k t

 ; Refiling only the agenda files and projects
 ;org-refile-targets (quote ((org-agenda-files :tag . "prj")))
 ;org-fast-tag-selection-include-todo t
)

;
; Dynamic behaviour
(defun gtd()
  "Start my GTD system"
  (interactive)
  (find-file org-default-notes-file)
)

; Check if an entry needs to be a project 
; Definition: 
; A project is defined by having more than one TODO child entries. If
; the entry is itself marked as a TODO entry, this is included in the
; count. A project is identified by a :prj: tag.
;
; It gets this tag in one of two ways:
; 1. Manually assigned by user;
; 2. Automatically if it has > 1 children which are TODO's
;(defun ensure-project-tags()
;  "Ensure a header gets a project tag if there is more than 1 TODO child"
;  (interactive)
;  (org-toggle-tag "prj"
;    (if (> (length (org-map-entries t "/+TODO|DONE|CANCELLED" 'tree)) 1) 'on 'off)
;    )
;)

;(defun ensure-prj-tag ()
;  (save-excursion
;    (when (org-up-heading-safe)
;      (let ((beg (point))
;	    (end (org-end-of-subtree t t))
;	    two-not-done)
;	(goto-char beg)
;	(goto-char (point-at-eol))
;	(setq two-not-done
;	      (and (re-search-forward org-not-done-heading-regexp end t)
;		   (re-search-forward org-not-done-heading-regexp end t)))
;	(goto-char beg)
;	(org-toggle-tag "prj" (if two-not-done 'on 'off))))))
;(add-hook 'org-after-todo-state-change-hook 'ensure-prj-tag)

;(defun ensure-project-tag (n-done n-not-done)
;  "Switch entry to DONE when all subentries are done, to TODO otherwise."
;  (org-toggle-tag "prj" (if (> (+ n-done n-not-done) 1) 'on 'off)))
   
;(add-hook 'org-todo-statistics-hook 'ensure-project-tag)

; Make sure we play nice with yasnippet.el
(add-hook 'org-mode-hook
	  (lambda ()
	    (org-set-local 'yas/trigger-key [tab])
	    (define-key yas/keymap [tab] 'yas/next-field-group)
	    (auto-fill-mode 1)
	    )
)

; When in agenda mode, show the line we're working on.
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

;
; Capturing with Remember
;
; We capture with C-S-spc which is an OSX shortcut, not an emacs one
; This calls a shell script calling out to emacsclient in batch mode
; and runs the make-remember-frame() function below.
(org-remember-insinuate)

(defadvice remember-finalize (after delete-remember-frame activate)
  "Advise remember-finalize to close the frame if it is the remember frame"
  (if (equal "remember" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice remember-destroy (after delete-remember-frame activate)
  "Advise remember-destroy to close the frame if it is the rememeber frame"
  (if (equal "remember" (frame-parameter nil 'name))
      (delete-frame)))

;; make the frame contain a single window. by default org-remember
;; splits the window.
(add-hook 'remember-mode-hook 'delete-other-windows)

; We call this from the outside
(defun make-remember-frame ()
  "Create a new frame and run org-remember."
  (interactive)
  (make-frame '((name . "remember") (width . 80) (height . 15)))
  (select-frame-by-name "remember")
  (org-remember))

; Capturing with org-remember
(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n  %i\n ")
        )
)

;
; Keep an automatic history of saves with git 
; 
; TODO: make adds/deletes work too?
(defun git-commit ()
  (when (eq major-mode 'org-mode)
    (shell-command "git commit -a -m 'Auto commit.'")))
(add-hook 'after-save-hook 'git-commit)

;
; When a tag change adds the waiting that, make sure it gets scheduled
; 1 week from now if it is not already.
;
;(defun autoschedule-waiting()
;  ; variable 'tags' contains the values of the tag-string
;  ; If tags has the tag :waiting:, schedule this 
;  (if includes tags "waiting")
;  (message "Running my own hook")
;  (message tags)
;  ;(org-schedule nil (org-timestring-to-seconds "+1w"))
;)

;(add-hook 'org-after-tags-change-hook 'autoschedule-waiting)

;
; Exporting and Publishing related settings.
;

; Apart from the normal export menu, I need something that exports
; just the body, so the resulting html is suitable to be used inside a
; blogging system like wordpress for example

; This puts the body inside a buffer called  blog-entry
(defun org-export-body-as-html ()
  (interactive)
  (org-export-as-html 3 nil nil "blog-entry" t))
; Make sure we export in css mode, meaning no inline css crap
(setq org-export-htmlize-output-type 'css)
(define-key org-mode-map "\C-c\C-e" 'org-export-body-as-html)


(provide 'org-settings)

