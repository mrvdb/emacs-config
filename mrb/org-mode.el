; Org mode specific settings

; Directly tie into the GIT repository on this machine
(add-to-list 'load-path "~/dev/emacs/packages/org-mode/")
(add-to-list 'load-path "~/dev/emacs/packages/org-mode/lisp/")
(add-to-list 'load-path "~/dev/emacs/packages/org-mode/contrib/lisp/")
(add-to-list 'load-path "~/dev/emacs/packages/org-icons/lisp/")

; bootstrap
(require 'org-install)           ;; This is required, see installation docs
(require 'org-mouse)             ;; Enable menu on right mouse button and other mouse functions
(require 'org-agenda)            ;; Enable the agenda functions, isn't this the case automatically then?
(require 'org-special-blocks)    ;; Generalizes the #+begin_foo and #+end_foo blocks, useful on latex (export) 

(add-hook 'org-mode-hook 'my-org-init)

(defun my-org-init ()
  ;; Autofill is nice when writing larger pieces of text, which I do a lot in org
  (turn-on-auto-fill)

  ;; Do proper quotes which look nicer. English
  (require 'typopunct)
  (typopunct-change-language 'english)
  (typopunct-mode 1)

  ;; Enable org2blog minor mode by default, can't hurt
  (org2blog/wp-mode 1)
)

; Allow automatically handing of created/expired meta data.
(require 'org-expiry)
;; Configure it a bit to my liking
(setq
  org-expiry-created-property-name "CREATED" ; Name of property when an item is created
  org-expiry-expiry-property-name  "EXPIRY"  ; Name of property to hold expiry date
  org-expiry-keyword               "EXPIRED" ; Name of property to hold date when it expired (which can differ from expiry)
  org-expiry-inactive-timestamps   t
)
(org-expiry-insinuate)           ;; Use hooks to insert created property
; Add a created property also when capturing an item with remember
; obviously this should be in org-expiry, make a patch!!
(add-hook 'org-remember-before-finalize-hook 'org-expiry-insert-created)
(add-hook 'org-capture-before-finalize-hook 'org-expiry-insert-created)

; Activate Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t) (ditaa . t) (sql . t) 
   (sh . t) (emacs-lisp t) (lisp t) 
   (css t) (awk t) (js t) (lisp t)
   (org t)
   (plantuml t)
))

(setq org-use-fast-todo-selection t)

; Define common tags here, so they function in all org files

; Make sure actions are distinguishable
(setq org-todo-keyword-faces '(
  ("DONE"   .    (:foreground "#afd8af"     :weight bold)) ;; This one didn't work as of [2011-10-24], had to customize manually
  ("WAITING"   . (:foreground "dark salmon" :weight bold))
  ("CANCELLED" . (:foreground "dim gray"    :weight bold))
))

; Make sure we keep a clean tag slate when changing tag state
(setq org-todo-state-tags-triggers 
      (quote (
	      ("TODO"      ("inactive") ) ; remove inactive tags if moved to TODO
	      ("DONE"      ("inactive") ("fork") ) ; remove inactive tags if moved to DONE
	      )))

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
	   ))))

; Keybindings we want to have available all the time
; even when not in org mode.
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cl" 'org-store-link)

; Keybindings which only make sense when having an orgmode file
(define-key org-mode-map "\C-ce" 'org-export)
(define-key org-mode-map [(super .)] 'org-todo)
(defun force-org-todo()
  (interactive)
  (org-todo "DONE")
)
(define-key org-mode-map [(control super .)] 'force-org-todo)

(define-key org-agenda-mode-map [(super .)] 'org-agenda-todo)
; Map ⌘t to schedule in both task and agenda-view
(define-key org-mode-map [(super t)] 'org-schedule)
(define-key org-agenda-mode-map [(super t)] 'org-agenda-schedule)
(define-key org-mode-map [(meta p)] 'org-set-property)
(define-key org-agenda-mode-map [(meta p)] 'org-set-property)
(define-key org-mode-map [(control super s)] 'org-save-all-org-buffers)

(setq
 ; Files and directories
 org-directory "~/.outlet/"                                     ; Main dir
 org-metadir (concat org-directory "_orgmeta/")                 ; Org system files go here
 org-mobile-directory "/plato:/var/dav/mrb/"                    ; Remote org dir
 org-archive-location (concat org-metadir "archive.org::* %s")  ; Default archive location

 org-default-notes-file (concat org-directory "GTD.org")
 org-agenda-files (quote (
			  "~/.outlet/GTD.org"
			  "~/.outlet/orgmode.org"
			  "~/.outlet/cobra.org"
			  "~/.outlet/habits.org"
			  "~/.outlet/meetings.org"
			  "~/.outlet/blogs.org"
			  "~/.outlet/keuken.org"))
 	 
 diary-file (concat org-metadir "DIARY")
 org-mobile-inbox-for-pull (concat org-metadir "from-mobile.org")


 ; I want to hide the leading stars, and do it *exactly* in the
 ; background-color
 org-hide-leading-stars t
 org-hide-emphasis-markers t

 ; Which string signals that an outline is collapsed
 org-ellipsis "..."

 ; We support task dependencies
 org-enforce-todo-dependencies t
 ; but relax checkbox constraints
 org-enforce-todo-checkbox-dependencies nil

 ; We dont do priorities
 org-enable-priority-commands nil

 ; Tags
 org-tags-column 90
 org-agenda-tags-column 90

 ;; Hide / and * markers when doing /italic/ and *bold* markup
 org-hide-emphasis-markers t
 ;; But show fancy entries
 org-pretty-entities 1

 org-mobile-force-id-on-agenda-items nil
 org-mobile-use-encryption nil
;; org-mobile-encryption-password "PASSWORDHERE"

 ; Agenda settings
 org-agenda-include-diary t
 org-agenda-start-with-log-mode t
 org-agenda-todo-ignore-scheduled "future"

 ; Habits
 org-habit-graph-column 100
 org-habit-show-habits-only-for-today nil

 ; Pressing enter on a link should activate it
 org-return-follows-link t
 org-support-shift-select (quote always)

 ; Refiling
 org-reverse-note-order nil    ; File at the bottom of an entry
 org-refile-allow-creating-parent-nodes (quote confirm)
 org-refile-targets (quote ((org-agenda-files :maxlevel . 10 )))
 org-refile-use-outline-path t

 org-agenda-dim-blocked-tasks t
 org-agenda-log-mode-items (quote (closed clock state))
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-deadline-prewarning-if-scheduled t
 org-agenda-skip-scheduled-if-done t
 org-agenda-start-with-log-mode t
 org-agenda-text-search-extra-files (quote (agenda-archives))
 org-agenda-todo-ignore-scheduled (quote all)
 org-babel-interpreters (quote ("emacs-lisp" "python" "ditaa" "sql" "sh" "R"))
 org-blank-before-new-entry (quote ((heading) (plain-list-item)))
 org-export-htmlize-output-type (quote css)
 org-fast-tag-selection-single-key (quote expert)
 org-file-apps (quote ((auto-mode . emacs) ("\\.mm\\'" . default) ("\\.x?html?\\'" . "google-chrome %s") ("\\.pdf\\'" . default)))
 org-fontify-done-headline t
 org-goto-interface (quote outline-path-completion)
 org-hierarchical-todo-statistics nil
 org-log-into-drawer t
 org-log-redeadline (quote note)
 org-log-reschedule (quote time)
 ;org-mobile-force-id-on-agenda-items nil t
 org-modules (quote (org-info org-jsinfo org-habit org-inlinetask org-irc org-toc org-mac-iCal org-mouse))
 org-remember-default-headline ""
 org-special-ctrl-a/e t
 org-stuck-projects (quote ("-inactive/TODO" ("TODO" "WAITING") nil ""))
 org-track-ordered-property-with-tag nil
 ;; This had a serious bug in the past making it very slow, seems better now.
 org-src-fontify-natively t
)

; Custom icons for the categories
(setq org-agenda-category-icon-alist 
      '(
	("Afspraak"      "~/.outlet/images/stock_new-meeting.png" nil nil :ascent center)
	("Blogging"      "~/.outlet/images/edit.png" nil nil :ascent center)
	("Cobra"         "~/.outlet/images/car.png" nil nil :ascent center)
	("DVD"           "~/.outlet/images/media-cdrom.png" nil nil :ascent center)
	("Emacs"         "~/.outlet/images/emacs.png" nil nil :ascent center)
	("Finance"       "~/.outlet/images/finance.png" nil nil :ascent center)
	("Habitat"       "~/.outlet/images/house.png" nil nil :ascent center)
	("Habit"         "~/.outlet/images/stock_task-recurring.png" nil nil :ascent center)
	("Hobbies"       "~/.outlet/images/hobbies.png" nil nil :ascent center)
	("Partners"      "~/.outlet/images/partners.png" nil nil :ascent center)
	("Task"          "~/.outlet/images/stock_todo.png" nil nil :ascent center)
	("Org"           "~/.outlet/images/org-mode-unicorn.png" nil nil :ascent center)
	("Statusnet"     "~/.outlet/images/statusnet.png" nil nil :ascent center)
	("Systeem"       "~/.outlet/images/systeembeheer.png" nil nil :ascent center)
	("Wordpress"     "~/.outlet/images/wordpress.png" nil nil :ascent center)
))
;
; Dynamic behaviour
(defun gtd()
  "Start my GTD system"
  (interactive)
  (find-file org-default-notes-file)
  ;; This should not be needed, but autofill does not come on automatically
  (turn-on-auto-fill)
)
(global-set-key [(control c) (g)] 'gtd)


(defun mrb/is-project-p ()
  "This function returns true if the entry is considered a project.
   A project is defined to be:
   - having a TODO keyword itself;
   - having at least one todo entry, regardless of their state."
  (let ((has-todokeyword)
	(has-subtask)
        (subtree-end (save-excursion (org-end-of-subtree t)))
	(is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (forward-line 1)
      (while (and (not has-subtask)
                  (< (point) subtree-end)
                  (re-search-forward "^\*+ " subtree-end t))
        (when (member (org-get-todo-state) org-todo-keywords-1)
          (setq has-subtask t))))
    ;; both subtasks and a keyword on the container need to be present.
    (and is-a-task has-subtask)
    )
)

; FIXME: testing for tag presence should be easier than a re-search forward
(defun mrb/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
         (has-next (save-excursion
                     (forward-line 1)
                     (and (< (point) subtree-end)
                          (re-search-forward "^*+ \\(TODO\\|WAITING\\)" subtree-end t)))))
    (if (and (mrb/is-project-p) (not has-next))
        nil ; a stuck project, has subtasks but no next task
      subtree-end)))

(defun mrb/skip-non-projects ()
  "Skip trees that are not projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (mrb/is-project-p)
        nil
      subtree-end)))

(defun mrb/skip-projects ()
  "Skip trees that are projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (mrb/is-project-p)
        subtree-end
      nil)))

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

(defun save-containing-org-file()
  (org-save-all-org-buffers) ;; bit over the top, no?
)
(add-hook 'org-after-todo-state-change-hook 'save-containing-org-file)


;;
;; Encryption settings
;;
(require 'org-crypt)
;; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
;;
;; Exclude some tags from trickling down to their children: - encrypt:
;; why was this again?  - area: the tag is only valid for the parent,
;; within an area there are tasks and projects (do I still use this?)
;; - fix: the tag is typically used on the smallest units. If it is
;; used on the parent, that does not directly mean that the children
;; also are fixing tasks.
(setq org-tags-exclude-from-inheritangce (quote ("area" "fix" "encrypt")))

;(defun ensure-project-tag (n-done n-not-done)
;  "Switch entry to DONE when all subentries are done, to TODO otherwise."
;  (org-toggle-tag "prj" (if (> (+ n-done n-not-done) 1) 'on 'off)))
   
;(add-hook 'org-todo-statistics-hook 'ensure-project-tag)

; Custom sort function for scheduled property
;; (defun org-cmp-scheduled(a b)
;;   "Compare the scheduled time of two agenda items and return according to their values:
;;      - if a.SCHEDULED is before    b.SCHEDULED return 1
;;      - if a.SCHEDULED is after     b.SCHEDULED return 0
;;      - if a.SCHEDULED is equial to b.SCHEDULED return nil"
;; ; Not sure if this works like this, but what is above is what I want
;; )
;(setq org-agenda-cmp-user-defined 'org-cmp-scheduled)

; When in agenda mode, show the line we're working on.
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

;
; Capturing with org-capture
;

; Define the templates
(setq org-capture-templates
      (quote (
	      ("t" "Todo" 
	       entry (id "new-todo-receiver") "* TODO %?" :prepend t)
	      ("j" "Journal" 
	       plain (file+datetree (concat org-directory "journal.org")) 
	       "___________________________________________________________ *%U* ___\n\n%?\n" )))
)

(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  ;; Create and select the frame
  (select-frame (make-frame '((name . "capture") 
		(width . 80) (height . 15)
		(menu-bar-lines . 0) (tool-bar-lines . 0))))
  ;; Capture a Todo entry
  (org-capture nil "t")
)
(global-set-key "\C-cc" 'make-capture-frame)

(defadvice org-capture-finalize (after delete-capture-frame activate)
  "Advise org-capture-finalize to close the frame if it is the capture frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy (after delete-capture-frame activate)
  "Advise org-capture-destroy to close the frame if it is the capture frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

; org-capture by default splits the window, we don't want that
(add-hook 'org-capture-mode-hook 'delete-other-windows)

(defun make-journal-entry ()
  "Create a journal entry"
  (interactive)
  (org-capture nil "j")
)
(global-set-key "\C-cj" 'make-journal-entry)


;
; Keep an automatic history of saves with git 
; 
; FIXME: make adds/deletes work too?
(defun git-commit ()
  ; This could be applied to other modes easily, so consider using a
  ; list of modes and defining the hook definition in a more common place.
  (when (eq major-mode 'org-mode)
    (shell-command "git commit -a -m 'Auto commit.'")))
(add-hook 'after-save-hook 'git-commit)

;
; When a tag change adds the waiting tag, make sure it gets scheduled
; 1 week from now if it is not already.
;
(defun autoschedule-waiting()
  ; variable 'tags' contains the values of the tag-string
  ; If tags has the tag :waiting:, schedule this 
  (if includes tags "waiting")
  (message "Running my own hook")
  (message tags)
  (org-schedule nil (org-timestring-to-seconds "+1w"))
)
; Activate it
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

(defun org-export-body-as-html-batch ()
  "Call `org-export-as-html', may be used in batch processing as
emacs   --batch
        --load=$HOME/lib/emacs/org.el
        --eval \"(setq org-export-headline-levels 2)\"
        --visit=MyFile --funcall org-export-body as-html-batch"
  (interactive)
  (org-export-as-html 3 nil nil nil t))

; Remove empty property drawers
(defun mrb/org-remove-empty-propert-drawers ()
  "*Remove all empty property drawers in current file."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "You need to turn on Org mode for this function."))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":PROPERTIES:" nil t)
      (save-excursion
        (org-remove-empty-drawer-at "PROPERTIES" (match-beginning 0))))))

(defun mrb/org-remove-redundant-tags ()
  "Remove redundant tags of headlines in current buffer.

A tag is considered redundant if it is local to a headline and
inherited by a parent headline."
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (org-map-entries
       '(lambda ()
          (let ((alltags (split-string (or (org-entry-get (point) "ALLTAGS") "") ":"))
                local inherited tag)
            (dolist (tag alltags)
              (if (get-text-property 0 'inherited tag)
                  (push tag inherited) (push tag local)))
            (dolist (tag local)
              (if (member tag inherited) (org-toggle-tag tag 'off)))))
       t nil))))


(defvar org-agenda-group-by-property nil
  "Set this in org-mode agenda views to group tasks by property")

(defun org-group-bucket-items (prop items)
  (let ((buckets ()))
    (dolist (item items)
      (let* ((marker (get-text-property 0 'org-marker item))
             (pvalue (org-entry-get marker prop t))
             (cell (assoc pvalue buckets)))
        (if cell
            (setcdr cell (cons item (cdr cell)))
          (setq buckets (cons (cons pvalue (list item))
                              buckets)))))
    (setq buckets (mapcar (lambda (bucket)
                            (cons (car bucket)
                                  (reverse (cdr bucket))))
                          buckets))
    (sort buckets (lambda (i1 i2)
                    (string< (car i1) (car i2))))))

(defadvice org-finalize-agenda-entries (around org-group-agenda-finalize
                                               (list &optional nosort))
  "Prepare bucketed agenda entry lists"
  (if org-agenda-group-by-property
      ;; bucketed, handle appropriately
      (let ((text ""))
        (dolist (bucket (org-group-bucket-items
                         org-agenda-group-by-property
                         list))
          (let ((header (concat "Property "
                                org-agenda-group-by-property
                                " is "
                                (or (car bucket) "<nil>") ":\n")))
            (add-text-properties 0 (1- (length header))
                                 (list 'face 'org-agenda-structure)
                                 header)
            (setq text
                  (concat text header
                          ;; recursively process
                          (let ((org-agenda-group-by-property nil))
                            (org-finalize-agenda-entries
                             (cdr bucket) nosort))
                          "\n\n"))))
        (setq ad-return-value text))
    ad-do-it))
(ad-activate 'org-finalize-agenda-entries)

; Directly tie into the GIT org2blog repository
(add-to-list 'load-path "~/dev/emacs/packages/org2blog/")
(add-to-list 'load-path "~/dev/emacs/packages/xml-rpc-el/")
(require 'org2blog-autoloads)

(setq
 org2blog/wp-server-weblog-id ""
 org2blog/wp-default-title "<Untitled>"
 org2blog/wp-default-categories ""
 org2blog/wp-confirm-post t
 org2blog/wp-track-posts (list (concat org-directory "/blogs/org2blog-track.org") "To be filed properly")
)


; Per blog configuration
(setq org2blog/wp-blog-alist
      '(
	("mrblog"
	 :url "http://mrblog.nl/xmlrpc.php"
	 :username "mrb"   
	 :track-posts ("blogs.org" "mrblog.nl")
	 )
	("cobra"             
	 :url "http://cobra.mrblog.nl/xmlrpc.php"
	 :username "mrb"
	 :track-posts ("blogs.org" "cobra.mrblog.nl")
	 )
	("hsd"
	 :url "http://test.hsdev.com/xmlrpc.php"
	 :username "mrb"
	 :track-posts ("blogs.org" "hsdev.com")
	)
      )
)

;; Shorten url at point
;; This is a stripped down version of the code in identica-mode
(defun mrb/ur1ca-get (api longurl)
  "Shortens url through ur1.ca free service 'as in freedom'"
  (let* ((url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data (concat "longurl=" (url-hexify-string longurl)))
	(buffer (url-retrieve-synchronously api)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (prog1
	  (setq ur1short
		(if (search-forward-regexp "Your .* is: .*>\\(http://ur1.ca/[0-9A-Za-z].*\\)</a>" nil t)
		    (match-string-no-properties 1)
		  (error "URL shortening service failed: %s" longurl)))
	    (kill-buffer buffer)))))

(defun mrb/shortenurl-replace-at-point ()
  "Replace the url at point with a tiny version."
  (interactive)
  (let ((url-bounds (bounds-of-thing-at-point 'url)))
    (when url-bounds
      (let ((url (mrb/ur1ca-get "http://ur1.ca" (thing-at-point 'url))))
	(when url
	  (save-restriction
	    (narrow-to-region (car url-bounds) (cdr url-bounds))
	    (delete-region (point-min) (point-max))
	    (insert url)))))))


(defvar mrb/org-my-archive-expiry-days 365
  "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value.")

(defun mrb/org-my-archive-done-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((done-regexp
           (concat "\\* \\(" (regexp-opt org-done-keywords) "\\) "))
          (state-regexp
           (concat "- State \"\\(" (regexp-opt org-done-keywords)
                   "\\)\"\\s-*\\[\\([^]\n]+\\)\\]")))
      (while (re-search-forward done-regexp nil t)
        (let ((end (save-excursion
                     (outline-next-heading)
                     (point)))
              begin)
          (goto-char (line-beginning-position))
          (setq begin (point))
          (when (re-search-forward state-regexp end t)
            (let* ((time-string (match-string 2))
                   (when-closed (org-parse-time-string time-string)))
              (if (>= (time-to-number-of-days
                       (time-subtract (current-time)
                                      (apply #'encode-time when-closed)))
                      org-my-archive-expiry-days)
                  (org-archive-subtree)))))))))

(defalias 'archive-done-tasks 'mrb/org-my-archive-done-tasks)


;; Map it to Ctrl-C S in orgmode (consider a global key assignment?
(define-key org-mode-map "\C-cs" 'mrb/shortenurl-replace-at-point)
;; END shorten url functionality

(add-to-list 'load-path "~/dev/emacs/packages/org-bom/")
(require 'org-bom)

(add-to-list 'load-path "~/dev/emacs/packages/org-toodledo")
(require 'org-toodledo)
(setq org-toodledo-userid "td4f031ad301856")
(setq org-toodledo-password "PASSWORDHERE")
;; Useful key bindings for org-mode
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-unset-key "\C-o")
	    (local-set-key "\C-od" 'org-toodledo-mark-task-deleted)
	    (local-set-key "\C-os" 'org-toodledo-sync)
	    )
	  )
(setq org-toodledo-debug 1)

(provide 'org-settings)

