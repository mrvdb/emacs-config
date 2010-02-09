;
; Org mode specific settings
;
; Directly tie into the GIT repository on this machine
(add-to-list 'load-path "~/dev/emacs/packages/org-mode/lisp/")
(add-to-list 'load-path "~/dev/emacs/packages/org-mode/contrib/lisp/")

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
(require 'org-babel-ditaa)
;;(require 'org-babel-R)  ;; Could not get R to work properly
(org-babel-load-library-of-babel)

; Capturing with org-remember
(org-remember-insinuate)

; Keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cr" 'org-remember)

(define-key org-mode-map [(super .)] 'org-todo)

(setq
 ; Main dir
 org-directory "~/.outlet/"
 ; Org system files go here
 org-metadir (concat org-directory "_orgmeta/")
 ; Remote org dir
 org-mobile-directory "/plato:/var/dav/mrb/"
 

 org-default-notes-file (concat org-directory "GTD.org")
 
 ; Meta files
 diary-file (concat org-metadir "DIARY")
 org-mobile-inbox-for-pull (concat org-metadir "from-mobile.org")
 ; Default archive location
 org-archive-location (concat org-metadir "archive.org::* %s")

 org-special-ctrl-k t


 ; I want to hide the leading stars, and do it *exactly* in the
 ; background-color
 org-hide-leading-stars t
 org-hide-emphasis-markers t

 org-log-done (quote time)
 org-modules (quote (
		     org-info 
		     org-jsinfo 
		     org-habit 
		     org-irc 
		     org-mac-protocol 
		     org-rmail 
		     org-toc   
		     org-mac-iCal 
		     org-mouse 
		     )
		)


  org-support-shift-select (quote always)

 ; We support task dependencies
 org-enforce-todo-dependencies t
 
 ; We dont do priorities
 org-enable-priority-commands nil

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
 org-indent-indentation-per-level 2

 ; Agenda settings
 org-agenda-include-diary t
 org-link-abbrev-alist
 '(("wiki" . "http://en.wikipedia.org/wiki/Search?search=")
   ("math" . "http://mathworld.wolfram.com/%s.html")
   ("google"   . "http://www.google.com/search?q="))

 ; Pressing enter on a link should activate it
 org-return-follows-link t

 ; Refiling only the agenda files and projects
 org-refile-targets (quote ((org-agenda-files :todo . "AREA")))
)

;
; Workflow states
;
; Default
(setq org-todo-keywords 
      '((type "TODO" "PROJECT" "AREA"  "|" "DONE" "CANCELLED(@)")
	)
)


; Capturing with org-remember
(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n  %i\n  %a")
        )
)

; Publish settings
(setq org-publish-project-alist
      '(
        ("blog-posts"
         :base-directory "~/dev/blog/_org/posts/"
         :base-extension "org"
         :publishing-directory "~/dev/blog/_posts"
         :inline-images t
         :table-of-contents nil
         :drawers nil
         :todo-keywords nil ; Skip todo keywords
         :exclude "draft*" ; TODO fix
         :section-numbers nil
         :auto-preamble nil
         :auto-postamble nil
         )
        ("blog-pages"
         :base-directory "~/dev/blog/_org/pages/"
         :base-extension "org"
         :publishing-directory "~/dev/blog/pages"
         :inline-images t
         :table-of-contents nil
         :drawers nil
         :todo-keywords nil ; Skip todo keywords
         :section-numbers nil
         :auto-preamble nil
         :auto-postamble nil
         ;; :completion-function
         )
        ("blog" :components ("blog-posts" "blog-pages"))))

;
; Dynamic behaviour

; Let the state of the parent be a result of the states of its child
; items, i.e. mark it as DONE or PROJECT This has the added advantage
; that TODO's which receive subtasks automatically become projects,
; which is exactly what I want
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to PROJECT otherwise."
  (let (org-log-done org-log-states)   ; turn off logging for these transitions (should I?)
    (org-todo (if (= n-not-done 0) "DONE" "PROJECT"))))
; Run the above on todo-statistics
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

; Make sure we play nice with yasnippet.el
(add-hook 'org-mode-hook
	  (lambda ()
	    (org-set-local 'yas/trigger-key [tab])
	    (define-key yas/keymap [tab] 'yas/next-field-group)
	    (auto-fill-mode 1)
	    )
)

;
; Capturing with Remember
;
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

;
; Keep an automatic history
;
(defun git-commit ()
  (when (eq major-mode 'org-mode)
    (shell-command "git commit -a -m 'Auto commit.'")))

(add-hook 'after-save-hook 'git-commit)

;
; What is below needs more work 
;
(require 'iimage)
(setq iimage-mode-image-search-path (expand-file-name "~/"))
;; Match org file: links
(add-to-list 'iimage-mode-image-regex-alist
             (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
                             "\\)\\]")  1))
(defun org-toggle-iimage-in-org ()
  (interactive)
  (let ((turning-on (not iimage-mode)))
    (set-face-underline-p 'org-link (not turning-on))
    (iimage-mode (or turning-on 0))))

(provide 'org-settings)

