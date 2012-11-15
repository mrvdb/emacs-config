;;
;; Org mode specific settings
;;

;; Directly tie into the GIT repository on this machine
;;(add-to-list 'load-path "~/dev/emacs/packages/org-mode/")
;;(add-to-list 'load-path "~/dev/emacs/packages/org-mode/lisp/")
(add-to-list 'load-path "~/dev/emacs/packages/org-mode/contrib/lisp/")
(add-to-list 'load-path "~/dev/emacs/packages/org-mode/contrib/babel/langs/")

;; bootstrap
;; CHECKME: is this still the proper installation method, it has changed a lot lately
(require 'org)           ;; This is required, see installation docs
(require 'org-clock)             ;; Should not be needed, but otherwise links don't work
(require 'org-mouse)             ;; Enable menu on right mouse button and other mouse functions
(require 'org-special-blocks)    ;; Generalizes the #+begin_foo and #+end_foo blocks, useful on latex (export) 
(require 'org-datetree)          ;; Allows for archiving and refiling in a date organised tree
(require 'org-mobile)            ;; Only org-mobile-push and pull get autoloaded and we neede the file list before that.

;; When going into org-mode, do this.
(defun my-org-init ()
  ;; Autofill is nice when writing larger pieces of text, which I do a lot in org
  (turn-on-auto-fill)

  ;; Do proper quotes which look nicer. English
  (require 'typopunct)
  (typopunct-change-language 'english)
  (typopunct-mode 1)
)
(add-hook 'org-mode-hook 'my-org-init)

;;
;; General settings
(setq
 ; Files and directories
 org-directory "~/.outlet/"                                          ; Main dir
 org-metadir (concat org-directory "_orgmeta/")                      ; Org system files go here
 org-archive-location (concat org-metadir "archive.org::date-tree")  ; Default archive location

 org-default-notes-file (concat org-directory "GTD.org")
 	 
 diary-file (concat org-metadir "DIARY")

 ; I want to hide the leading stars, and do it *exactly* in the
 ; background-color
 org-hide-leading-stars t
 org-hide-emphasis-markers t

 ; Which string signals that an outline is collapsed
 org-ellipsis "..."

 org-use-fast-todo-selection t

 ; We support task dependencies
 org-enforce-todo-dependencies t
 ; but relax checkbox constraints
 org-enforce-todo-checkbox-dependencies nil

 ; We dont do priorities
 org-enable-priority-commands nil

 ; Tags
 org-tags-column -110
 org-agenda-tags-column -110

 ;; Hide / and * markers when doing /italic/ and *bold* markup
 org-hide-emphasis-markers t
 ;; But show fancy entries
 org-pretty-entities 1
 org-pretty-entities-include-sub-superscripts nil

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
 org-hierarchical-todo-statistics t
 org-provide-todo-statistics t
 org-log-into-drawer t
 org-log-redeadline (quote note)
 org-log-reschedule (quote time)
 org-modules (quote (org-info org-jsinfo org-habit org-inlinetask org-irc org-toc org-mac-iCal org-mouse))
 org-remember-default-headline ""
 org-special-ctrl-a/e t
 org-stuck-projects (quote ("-inactive/TODO" ("TODO" "WAITING") nil ""))
 org-track-ordered-property-with-tag nil
 ;; This had a serious bug in the past making it very slow, seems better now.
 org-src-fontify-natively t
)

;;
;; Allow automatically handing of created/expired meta data.
;;
(require 'org-expiry)
;; Configure it a bit to my liking
(setq
  org-expiry-created-property-name "CREATED" ; Name of property when an item is created
  org-expiry-inactive-timestamps   t         ; Don't have everything in the agenda view
)

(defun mrb/insert-created-timestamp()
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (org-expiry-insert-created)
  (org-back-to-heading)
  (org-end-of-line)
  (insert " ")
)
  
;; Whenever a TODO entry is created, I want a timestamp
;; Advice org-insert-todo-heading to insert a created timestamp using org-expiry
(defadvice org-insert-todo-heading (after mrb/created-timestamp-advice activate)
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (mrb/insert-created-timestamp)
)
;; Make it active
(ad-activate 'org-insert-todo-heading)

(require 'org-capture)

(defadvice org-capture (after mrb/created-timestamp-advice activate)
  "Insert a CREATED property using org-expiry.el for TODO entries"
  ; Test if the captured entry is a TODO, if so insert the created
  ; timestamp property, otherwise ignore
  (when (member (org-get-todo-state) org-todo-keywords-1)
    (mrb/insert-created-timestamp)))
(ad-activate 'org-capture)

;; Add feature to allow easy adding of tags in a capture window
(defun mrb/add-tags-in-capture()
  (interactive)
  "Insert tags in a capture window without losing the point"
  (save-excursion
    (org-back-to-heading)
    (org-set-tags)))
;; Bind this to a reasonable key
(define-key org-capture-mode-map "\C-c\C-t" 'mrb/add-tags-in-capture)

;; Activate Babel languages
(require 'ob-gnuplot)
(require 'ob-mathomatic)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t) (ditaa . t) (sql . t) (sh . t) (emacs-lisp t) (lisp t) 
   (css t) (awk t) (js t) (lisp t) (org t) (plantuml t) (gnuplot . t)))


; Define common tags here, so they function in all org files
; Make sure actions are distinguishable
(setq org-todo-keyword-faces '(
  ("DONE"   .    (:foreground "#afd8af"     :weight bold))
  ("WAITING"   . (:foreground "dark salmon" :weight bold))
  ("CANCELLED" . (:foreground "dim gray"    :weight bold))
  ("BUY      " . (:foreground "goldenrod"   :weight bold))
))

; Make sure we keep a clean tag slate when changing tag state
; Note: caputuring does not honour this, i.e. when creating a new item.
(setq org-todo-state-tags-triggers 
      (quote (
	      ('todo ("inactive"))          ; remove inactive tags if moved to any active state
	      ('done ("inactive") ("fork")) ; remove tags from any inactive state
	      ("BUY"  ("buy" . t)))))       ; add buy tag when this is a buying action 


; Keybindings we want to have available all the time
; even when not in org mode.
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cl" 'org-store-link)

; Keybindings which only make sense when having an orgmode file
(define-key org-mode-map "\C-ct" 'org-set-tags)
(define-key org-mode-map "\C-ce" 'org-export)
(define-key org-mode-map [(super .)] 'org-todo)
(defun force-org-todo()
  (interactive)
  (let ((current-prefix-arg '(64)))  ;; Triple C-u (4^3)
    (call-interactively 'org-todo))
)
(define-key org-mode-map [(control super .)] 'force-org-todo)
(define-key org-agenda-mode-map [(control super .)] 'force-org-todo)

(define-key org-agenda-mode-map [(super .)] 'org-agenda-todo)
; Map ⌘t to schedule in both task and agenda-view
(define-key org-mode-map [(super t)] 'org-schedule)
(define-key org-agenda-mode-map [(super t)] 'org-agenda-schedule)
(define-key org-mode-map [(meta p)] 'org-set-property)
(define-key org-agenda-mode-map [(meta p)] 'org-set-property)
(define-key org-mode-map [(control super s)] 'org-save-all-org-buffers)


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
; FIXNE: are we not searching for all 'incomplete' type keywords here?, there must be an org function for that
(defun mrb/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
         (has-next (save-excursion
                     (forward-line 1)
                     (and (< (point) subtree-end)
                          (re-search-forward "^*+ \\(TODO\\|BUY\\|WAITING\\)" subtree-end t)))))
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

(defun save-containing-org-file()
  (org-save-all-org-buffers) ;; FIXME: bit over the top, no?  
)
(add-hook 'org-after-todo-state-change-hook 'save-containing-org-file)


;;
;; Encryption settings
;;
(require 'org-crypt)
;; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-crypt-key nil)
;;
;; Exclude some tags from trickling down to their children: 
;;   - encrypt: why was this again?  
;;   - area: the tag is only valid for the parent, within an area there are tasks and projects (do I still use this?)
;;   - fix: the tag is typically used on the smallest units. If it is
;;          used on the parent, that does not directly mean that the children
;;          also are fixing tasks.
(setq org-tags-exclude-from-inheritance (quote ("area" "fix" "encrypt" "crypt" "sell")))

; When in agenda mode, show the line we're working on.
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

;;
;; Capturing with org-capture
;; TODO: probably split this off into its own file

; Define the templates
(setq org-capture-templates
      (quote (
	      ("b" "Buy" 
	       entry (id "new-todo-receiver") "* BUY %? :buy:\n" :prepend t)
	      ("t" "Todo" 
	       entry (id "new-todo-receiver") "* TODO %?" :prepend t)
	      ("j" "Journal" 
	       entry (file+datetree (concat org-directory "journal.org")) 
	       "* ___________________________________________________________ *%U* ___\n\n%?\n" )))
)

(defun make-capture-frame ()
  "Create a new frame for org-capture to use."
  ;; Create and select the frame FIXME: the frame needs to be at least
  ;; 95 to be able to display the tag on the same line. Perhaps we
  ;; should temporarily set the tag column to something less, so we
  ;; can keep the capture window small.
  (select-frame (make-frame '((name . "capture") 
		(width . 115) (height . 15)
		(menu-bar-lines . 0) (tool-bar-lines . 0))))
)

(defun capture-todo ()
  "Capture a TODO item"
  (interactive)
  (make-capture-frame)
  (org-capture nil "t")
)

(defun capture-buy ()
  "Capture a BUY item"
  (interactive)
  (make-capture-frame)
  (org-capture nil "b")
)


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

(defadvice org-agenda-finalize-entries (around org-group-agenda-finalize
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
                            (org-agenda-finalize-entries
                             (cdr bucket) nosort))
                          "\n\n"))))
        (setq ad-return-value text))
    ad-do-it))
(ad-activate 'org-agenda-finalize-entries)

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
	 :track-posts ("blogs/blogs.org" "mrblog.nl")
	 )
	("cobra"             
	 :url "http://cobra.mrblog.nl/xmlrpc.php"
	 :username "mrb"
	 :track-posts ("blogs/blogs.org" "cobra.mrblog.nl")
	 )
	("hsd"
	 :url "http://test.hsdev.com/xmlrpc.php"
	 :username "mrb"
	 :track-posts ("blog/blogs.org" "hsdev.com")
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
                      mrb/org-my-archive-expiry-days)
                  (org-archive-subtree)))))))))

(defalias 'archive-done-tasks 'mrb/org-my-archive-done-tasks)


;; Map it to Ctrl-C S in orgmode (consider a global key assignment?
(define-key org-mode-map "\C-cs" 'mrb/shortenurl-replace-at-point)
;; END shorten url functionality

(add-to-list 'load-path "~/dev/emacs/packages/org-bom/")
(require 'org-bom)

;; (add-to-list 'load-path "~/dev/emacs/packages/org-toodledo")
;; (require 'org-toodledo)
;; (setq org-toodledo-userid "td4f031ad301856")
;; (setq org-toodledo-password "PASSWORDHERE")
;; ;; Useful key bindings for org-mode
;; (add-hook 'org-mode-hook
;; 	  (lambda ()
;; 	    (local-unset-key "\C-o")
;; 	    (local-set-key "\C-od" 'org-toodledo-mark-task-deleted)
;; 	    (local-set-key "\C-os" 'org-toodledo-sync)
;; 	    )
;; 	  )
;; (setq org-toodledo-debug 1)

;; archive entries into a date-tree
;; (setq org-archive-location "%s_archive::date-tree")
(defadvice org-archive-subtree
  (around org-archive-subtree-to-data-tree activate)
  "org-archive-subtree to date-tree"
  (if
      (string= "date-tree"
               (org-extract-archive-heading
                (org-get-local-archive-location)))
      (let* ((dct (decode-time (org-current-time)))
             (y (nth 5 dct))
             (m (nth 4 dct))
             (d (nth 3 dct))
             (this-buffer (current-buffer))
             (location (org-get-local-archive-location))
             (afile (org-extract-archive-file location))
             (org-archive-location
              (format "%s::*** %04d-%02d-%02d %s" afile y m d
                      (format-time-string "%A" (encode-time 0 0 0 d m y)))))
        (message "afile=%s" afile)
        (unless afile
          (error "Invalid `org-archive-location'"))
        (save-excursion
          (switch-to-buffer (find-file-noselect afile))
          (org-datetree-find-year-create y)
          (org-datetree-find-month-create y m)
          (org-datetree-find-day-create y m d)
          (widen)
          (switch-to-buffer this-buffer))
        ad-do-it)
    ad-do-it))

;;
;; Org-mobile configuration
(setq
 org-mobile-directory "/plato.hsdev.com:/home/mrb/data/mobileorg"              ; Remote org dir
 org-mobile-inbox-for-pull (concat org-metadir "from-mobile.org")    ; Where to place items which needs resolving
 org-mobile-force-id-on-agenda-items nil                             ; No id yet, don't see the advatage yet
 org-mobile-use-encryption nil                                       ; No encryption yet.
 org-mobile-agendas (quote all)
 org-mobile-files (quote ("~/.outlet/GTD.org" "~/.outlet/habits.org" "~/.outlet/_calendars/meetings.org"))
 org-mobile-use-encryption nil
)

;; Define timer variables for pull and push operations
(defvar org-mobile-push-timer nil)
(defvar org-mobile-pull-timer nil)

;; Define notificaters
(require 'notifications)

(defun org-mobile-notify (type result)
  (notifications-notify
   :title (concat type " complete:")
   :body  (format (concat "Org-mobile-" type ": %s") result)))

(defun notify-push (result) (org-mobile-notify "Push" result))
(defun notify-pull (result) (org-mobile-notify "Pull" result))

;; Fork the work of pushing to mobile
(defun fork-org-mobile-push()
  (async-start
   ;; What to do in the child process
   `(lambda ()
      ,(async-inject-variables "org-\\(mobile-\\|directory\\)")
      (org-mobile-push))
   
   ; What to do when it finishes
   (lambda (result)
     (notify-push result)
     (message "Push of mobile org complete"))))

;; Push to mobile when the idle timer runs out
(defun org-mobile-push-with-delay (secs)
  (when org-mobile-push-timer
    (cancel-timer org-mobile-push-timer))
  (setq org-mobile-push-timer
        (run-with-idle-timer
         (* 1 secs) nil 'fork-org-mobile-push)))

;; After saving files, start a 30 seconds idle timer after which we
;; are going to push
;; (add-hook
;;  'after-save-hook
;;  (lambda () 
;;    (when (eq major-mode 'org-mode)
;;      (dolist (file (org-mobile-files-alist))
;;        (if (string= (expand-file-name (car file)) (buffer-file-name))
;; 	   (org-mobile-push-with-delay 30))))))

;; Fork the work of pushing to mobile
(defun fork-org-mobile-pull ()
  (async-start
   ;; What to do in the child process
   `(lambda ()
      ,(async-inject-variables "org-\\(mobile-\\|directory\\)")
      (org-mobile-pull))
   
   ; What to do when it finishes
   (lambda (result)
     (notify-pull result)
     (message "Pull of mobile org complete"))))

;; Construct the name of the remote file
(setq remote-org-mobile-file
      (file-truename
       (concat
	(file-name-as-directory org-mobile-directory)
	"mobileorg.org")))

;; Pull by monitoring the file mobile-org writes to
(defun install-monitor (file secs)
  ;; Cancel an existing timer, if any
  (when org-mobile-pull-timer
    (cancel-timer org-mobile-pull-timer))
  ;; And set up a new one
  (setq org-mobile-pull-timer
	(run-with-timer
	 0 secs
	 (lambda (f p)
	   ;; If the remote file has been changed within out repeat
	   ;; period, we need a new copy
	   (unless (< p (second (time-since (elt (file-attributes f) 5))))
	     (fork-org-mobile-pull)))
	 file
	 secs)))

;; Install a monitor on the remote org file. Don't make the time too
;; short, otherwise the file might nog get pulled in.
;; (install-monitor remote-org-mobile-file 30)

;; Mail facilities related to org-mode
(require 'org-mime)
(defun mrb/mail-subtree-from-org-agenda ()
  (interactive)
  (org-agenda-goto)
  (org-mime-subtree))
;; Bind it to a C-c m (similar to C-x m which opens an empty mail)
(define-key org-mode-map [(control c) m] 'org-mime-subtree)
;; This does not work
;;(define-key org-agenda-mode-map [(control c) m] 'mrb/mail-subtree-from-org-agenda)
;; This does
(define-key org-agenda-mode-map "\C-cm" 'mrb/mail-subtree-from-org-agenda)
;;

(require 'stripe-buffer)
(defun mrb/enable-org-table-striping ()
  (interactive)
  (stripe-org-tables-enable))

;; TODO I want to have this in the agenda mode too
(provide 'org-settings)

