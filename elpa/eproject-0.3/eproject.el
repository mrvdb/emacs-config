;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; eproject.el --- project workspaces for emacs
;;
;; Copyright (C) 2008,2009 grischka
;;
;; Author: grischka -- grischka@users.sourceforge.net
;; Created: 24 Jan 2008
;; Version: 0.3
;;
;; This program is free software, released under the GNU General
;; Public License (GPL, version 2). For details see:
;;
;;     http://www.fsf.org/licenses/gpl.html
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; There is a global file
(defun prj-globalfile ()
  (unless (boundp 'user-emacs-directory)
     (setq user-emacs-directory "~/.emacs.d/")
     )
  (let ((d (expand-file-name user-emacs-directory)))
    (make-directory d t)
    (concat d "eproject.lst")
    ))

;; with the list of all projects
(defvar prj-list)

;; and the project that was open in the last session (if any)
(defvar prj-last-open nil)

;; and the frame coords from last session
(defvar prj-frame-pos nil)

;; eproject version that created the config file
(defvar prj-version nil)

;; Here is a function to reset these
(defun prj-init ()
  (setq prj-version nil)
  (setq prj-list nil)
  (setq prj-last-open nil)
  (setq prj-frame-pos nil)
)

;; Each project has a directory
(defvar prj-directory)

;; with a configuration files in it
(defun prj-localfile ()
  (expand-file-name "eproject.cfg" prj-directory)
  )

;; This file defines:

;; the list of files
(defvar prj-files)

;; the current file
(defvar prj-curfile)

;; an alist of settings
(defvar prj-config)

;; a list of tools
(defvar prj-tools)

;; a list of utility functions (feature incomplete)
(defvar prj-functions nil)

;; directory to run commands, default to prj-directory
(defvar prj-directory-run)

;; Here are some default tools for new projects,
;; (which you might want to adjust to your needs)

(defun prj-default-config ()
  (setq prj-tools (copy-tree '(
     ("Make"         "make" "f9")
     ("Clean"        "make clean" "C-f9")
     ("Run"          "echo run what" "f8")
     ("Stop"         "-e eproject-killtool" "C-f8")
     ("---")
     ("Configure"    "./configure")
     ("---")
     ("Explore Project" "nautilus --browser `pwd` &")
     ("XTerm In Project" "xterm &")
     )))
  )

;; This defines the current project
(defvar prj-current)

;; There is an internal list with generated functions
;; for each tool
(defvar prj-tools-fns)

;; and a list with files removed from the project
(defvar prj-removed-files)

;; Here is a function to reset/close the project
(defun prj-reset ()
  (setq prj-version nil)
  (setq prj-current nil)
  (setq prj-directory nil)
  (setq prj-directory-run nil)
  (setq prj-files nil)
  (setq prj-removed-files nil)
  (setq prj-curfile nil)
  (setq prj-config nil)
  (setq prj-tools nil)
  (setq prj-tools-fns nil)
  (prj-reset-functions)
  (prj-default-config)
  )

(defun prj-reset-functions ()
  (dolist (l prj-functions)
    (if (eq (car l) 'setq)
        (makunbound (cadr l))
      (fmakunbound (cadr l))
      ))
  (setq prj-functions nil)
  )

(defun prj-set-functions (s)
  (prj-reset-functions)
  (setq prj-functions s)
  (dolist (l s) (eval l))
  )

;; Some more variables

;; the frame that exists on startup
(defvar prj-initial-frame nil)

;; this is put into minor-mode-alist
(defvar eproject-mode t)

;; where this file is in
(defvar eproject-directory)

;; eproject version that created the files
(defvar eproject-version "0.3")

;; Configuration UI
(eval-and-compile
  (defun eproject-setup-toggle () (interactive))
  (defun eproject-setup-quit () (interactive))
  (defun prj-config-get-result (s))
  (defun prj-config-reset ())
  (defun prj-config-print ())
  (defun prj-config-parse ())
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Small functions

(defun prj-del-list (l e)
  (let ((a (assoc (car e) l)))
    (if a
        (delq a l)
      l)))

(defun prj-add-list (l e)
  (nconc (prj-del-list l e) (list e))
  )

(defun prj-next-file (l e)
  (let ((a (assoc (car e) l)))
    (when a
      (setq l (memq a l))
      (if (cdr l) (cadr l) a)
      )))

(defun prj-prev-file (l e)
  (let ((a (assoc (car e) l)) (p l))
    (when a
      (while (and l (null (eq (car l) a)))
        (setq p l l (cdr l))
        )
      (car p)
      )))

;; replace a closed file, either by the previous or the next.
(defun prj-otherfile (l f)
  (let ((n (prj-prev-file l f)))
    (when (equal f n)
      (setq n (prj-next-file l f))
      (when (equal f n)
        (setq n nil)
        ))
    n))

(defun caddr (l) (car (cddr l)))

;; make relative path, but only up to the second level of ..
(defun prj-relative-path (f)
  (let ((r (file-relative-name f prj-directory)))
    (if (string-match "^\\.\\.[/\\]\\.\\.[/\\]\\.\\.[/\\]" r)
        f
      r
      )))

;; friendly truncate filename
(defun prj-shortname (s)
  (let ((l (length s)) (x 30) n)
    (cond ((>= x l) s)
          ((progn
             (setq x (- x 3))
             (setq n (length (file-name-nondirectory s)))
             (if (< n l) (setq n (1+ n)))
             (>= x n)
             )
           (concat (substring s 0 (- x n)) "..." (substring s (- n)))
           )
          ((= n l)
           (concat (substring s 0 x) "...")
           )
          (t
           (concat "..." (substring s (- n) (- (- x 3) n)) "...")
           ))))

(defun prj-settitle ()
  (modify-frame-parameters
   nil
   (list (cons 'title
               (and prj-current
                    (format "emacs - %s" (car prj-current))
                    )))))

(defun eproject-addon (f)
  (concat eproject-directory f)
  )

(defun prj-goto-line (n)
  (goto-char 1)
  (beginning-of-line n)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write configuration to file

(defun prj-print-list (s fp)
  (let ((v (eval s)))
    (setq v (list 'setq s
      (if (and (atom v) (null (and (symbolp v) v)))
          v
          (list 'quote v)
          )))
    ;;(print v fp)
    (pp v fp) (princ "\n" fp)
    ))

(defun prj-create-file (filename)
  (let ((fp (generate-new-buffer filename)))
    (princ ";; -*- mode: Lisp; -*-\n\n" fp)
    fp))

(defun prj-close-file (fp)
  (with-current-buffer fp
    (condition-case nil
      (write-region nil nil (buffer-name fp) nil 0)
      (error nil)
      ))
  (kill-buffer fp)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load/Save global project list and initial frame sizes

(defun prj-loadlist ()
  (prj-init)
  (load (prj-globalfile) t t)
  (setq prj-version eproject-version)
  )

(defun prj-get-frame-pos (f)
  (mapcar
   (lambda (parm) (cons parm (frame-parameter f parm)))
   '(top left width height)
   ))

(defun prj-savelist ()
  (let ((g (prj-globalfile))
        fp
        )
    (unless (file-exists-p g)
      (make-directory (file-name-directory g) t)
      )
    (setq prj-last-open (car prj-current))
    (when (frame-live-p prj-initial-frame)
      (setq prj-frame-pos (prj-get-frame-pos prj-initial-frame))
      )
    (setq fp (prj-create-file g))
    (when fp
      (prj-print-list 'prj-version fp)
      (prj-print-list 'prj-list fp)
      (prj-print-list 'prj-last-open fp)
      (prj-print-list 'prj-frame-pos fp)
      (prj-close-file fp)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load/Save local per-project configuration file

(defun prj-update-config ()
  (setq prj-directory-run
        (file-name-as-directory
         (expand-file-name
          (or (prj-getconfig "run-directory") ".")
          prj-directory
          )))
  )

(defun prj-loadconfig (a)
  (let (lf e)
    (prj-reset)
    (setq prj-current a)
    (setq prj-directory
          (file-name-as-directory
           (expand-file-name (cadr a))
           ))

    (when (file-exists-p (setq lf (prj-localfile)))
      (load lf nil t)
      (setq prj-curfile
        (or (assoc prj-curfile prj-files)
	    (car prj-files)
	    ))
      )
    (if (setq e (prj-getconfig "project-name"))
        (setcar a e)
        (prj-setconfig "project-name" (car a))
        )
    (prj-update-config)
    (prj-set-functions prj-functions)
    (setq prj-version eproject-version)
    ))

(defun prj-saveconfig ()
  (when prj-current
    (let (w c b files)
      (prj-removehooks)
      (setq w (selected-window))
      (setq c (window-buffer w))
      (dolist (f prj-files)
        (cond ((setq b (get-buffer (car f)))
               (set-window-buffer w b t)
               (with-current-buffer b
                 (let ((s (line-number-at-pos (window-start w)))
                       (p (line-number-at-pos (window-point w)))
                       )
                   (push (list (car f) s p) files)
                   )))
              ((consp (cdr f))
               (push f files)
               )))
      (set-window-buffer w c t)
      (prj-addhooks)
      (let ((fp (prj-create-file (prj-localfile)))
            (prj-curfile (car prj-curfile))
            (prj-files (nreverse files))
            )
        (when fp
          (prj-print-list 'prj-version fp)
          (prj-print-list 'prj-config fp)
          (prj-print-list 'prj-tools fp)
          (prj-print-list 'prj-files fp)
          (prj-print-list 'prj-curfile fp)
          (prj-print-list 'prj-functions fp)
          (prj-close-file fp)
          ))
      )))

(defun prj-saveall ()
  (prj-saveconfig)
  (prj-savelist)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The core functions:  Open / Close / Add / Remove  Project

(defun eproject-open (a)
  "Open another project."
  (interactive
   (list
    (or (prj-config-get-result 'p)
        (completing-read "Open Project: " (mapcar 'car prj-list))
        )))
  (unless (consp a)
    (let ((b (assoc a prj-list)))
      (unless b
        (error "No such project: %s" a)
        )
      (setq a b)
      ))
  (setq a (or (car (member a prj-list)) a))
  (unless (eq a prj-current)
    (unless (file-directory-p (cadr a))
      (error "Error: No such directory: %s" (cadr a))
      )
    (setq prj-list (cons a (delq a prj-list)))
    (eproject-close)
    (prj-loadconfig a)
    )
  (prj-addhooks)
  (prj-setup-all)
  (cd prj-directory)
  (unless (prj-edit-file prj-curfile)
    (eproject-dired)
    ))

(defun eproject-close ()
  "Close the current project."
  (interactive)
  (when prj-current
    (prj-saveconfig)
    (prj-removehooks)
    (let (f)
      (unwind-protect
          (progn
            (save-some-buffers nil)
            (eproject-killbuffers t)
            (setq f t)
            )
        (or f (prj-addhooks))
        ))
    (prj-reset)
    (prj-config-reset)
    (prj-setup-all)
    ))

(defun eproject-killbuffers (&optional from-project)
  "If called interactively kills all buffers that
do not belong to  project files"
  (interactive)
  (let (a b)
    (dolist (f prj-files)
      (setq b (get-buffer (car f)))
      (if b
          (setq a (cons (list b) a))
          ))
    (dolist (b (buffer-list))
      (when (eq (consp (assoc b a)) from-project)
        (kill-buffer b)
        ))))

(defun eproject-add (d)
  "Add a new or existing project to the list."
  (interactive
   (list
    (read-directory-name "Add project in directory: " prj-directory nil t)
    ))
  (when d
    (setq d (directory-file-name d))
    )
  (when (= 0 (length d))
    (error "Error: Empty directory name.")
    )
  (let (n a)
    (setq n (file-name-nondirectory d))
    (setq a (list n d))
    (push a prj-list)
    (prj-setup-all)
    ))

(defun eproject-remove (a)
  "Remove a project from the list."
  (interactive
   (list
    (or (prj-config-get-result 'p)
        (completing-read "Remove project: " (mapcar 'car prj-list))
        )))
  (unless (consp a)
    (let ((b (assoc a prj-list)))
      (unless b
        (error "No such project: %s" a)
        )
      (setq a b)
      ))
  (when (progn
          (beep)
          (prog1 (y-or-n-p (format "Remove \"%s\"? " (car a)))
            (message "")
            ))
    (setq prj-list (prj-del-list prj-list a))
    (prj-setup-all)
    ))

(defun eproject-save ()
  "Save the project configuration to file."
  (interactive)
  (prj-config-parse)
  (prj-config-print)
  (prj-saveall)
  )

(defun eproject-revert ()
  "Reload the project configuration from file."
  (interactive)
  (prj-loadlist)
  (if prj-current
      (prj-loadconfig prj-current)
    )
  (prj-setup-all)
  )

(defun eproject-addfile (f)
  "Add a file to the current project."
  (interactive
   (and prj-current
        (list
         (read-file-name "Add file to project: " nil nil t nil)
         )))
  (unless prj-current (error "No project open"))
  (let ((a (prj-insert-file f (prj-config-get-result 'f))))
    (unless (cdr a)
      (message "Added to project: %s" (car a))
      ))
  (prj-config-print)
  (prj-setmenu)
  )

(defun eproject-removefile (a)
  "Remove a file from the current project."
  (interactive (prj-get-existing-file-1 "Remove file from project: "))
  (setq a (prj-get-existing-file-2 a))
  (prj-remove-file a)
  )

(defun eproject-visitfile (a)
  "Visit a file from the current project."
  (interactive (prj-get-existing-file-1 "Visit file: "))
  (setq a (prj-get-existing-file-2 a))
  (prj-edit-file a)
   )

(defun prj-get-existing-file-1 (msg)
  (and prj-current
       (list
        (or (prj-config-get-result 'f)
            (completing-read msg (mapcar 'car prj-files))
            ))))

(defun prj-get-existing-file-2 (a)
   (unless prj-current (error "No project open"))
   (if (consp a)
       a
     (let ((b (assoc (prj-relative-path a) prj-files)))
       (unless b (error "No such file in project: %s" a))
       b
       )))

(defun eproject-help ()
  "Show the eproject README."
  (interactive)
  (view-file (eproject-addon "eproject.txt"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hook functions to track opening/closing files from emacs

(defun prj-addhooks ()
  (add-hook 'kill-buffer-hook 'prj-kill-buffer-hook)
  (add-hook 'find-file-hook 'prj-find-file-hook)
  (add-hook 'window-configuration-change-hook 'prj-wcc-hook)
  )

(defun prj-removehooks ()
  (remove-hook 'window-configuration-change-hook 'prj-wcc-hook)
  (remove-hook 'find-file-hook 'prj-find-file-hook)
  (remove-hook 'kill-buffer-hook 'prj-kill-buffer-hook)
  )

(defun prj-wcc-hook ()
  (let ((w (selected-window)) (b (window-buffer (selected-window))))
    ;; (message "wcc-hook: %s" (prin1-to-string (list wcc-count w b n)))
    (prj-register-buffer b)
    ))

(defun prj-find-file-hook ()
  (run-with-idle-timer
   0
   nil
   `(lambda () (prj-register-buffer ,(current-buffer)))
   ))

(defun prj-kill-buffer-hook ()
  (let ((b (current-buffer)) a)
    (if (setq a (rassq b prj-files))
        (prj-remove-file a t)
        (if (setq a (rassq b prj-removed-files))
            (setq prj-removed-files (delq a prj-removed-files))
          ))))

(defun prj-register-buffer (b)
  (let (f a i)
    (setq f (buffer-file-name b))
    (when f
      (setq a (rassq b prj-files))
      (unless a
        (setq a (prj-insert-file f nil t))
        (when a
          (unless (cdr a)
            (message "Added to project: %s" (car a))
            )
          (setcdr a b)
          (with-current-buffer b
            (rename-buffer (car a) t)
            )))
      (when (and a (null (eq a prj-curfile)))
        (setq prj-curfile a)
        (prj-setmenu)
        ))
    a))

(defun prj-insert-file (f &optional after on-the-fly)
  (let ((r (prj-relative-path f)) a m)
    (setq a (assoc r prj-files))
    (unless (or a (and on-the-fly (assoc r prj-removed-files)))
      (setq a (list r))
      (setq m (memq (or after prj-curfile) prj-files))
      (if m
          (setcdr m (cons a (cdr m)))
          (setq prj-files (prj-add-list prj-files a))
          )
      (setq prj-removed-files (prj-del-list prj-removed-files a))
      )
    a))

(defun prj-remove-file (a &optional on-the-fly)
  (let ((n (prj-otherfile prj-files a)) b)
    (setq prj-files (prj-del-list prj-files a))
    (when (eq prj-curfile a)
      (setq prj-curfile n)
      )
    (unless on-the-fly
        (setq prj-removed-files (prj-add-list prj-removed-files a))
        )
    (unless (prj-config-print)
      (prj-edit-file prj-curfile)
      )
    (prj-setmenu)
    (message "Removed from project: %s" (car a))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit another file

(defun prj-edit-file (a)
  (when a
    (let ((n (car a)) f b pos)
      (setq f (expand-file-name n prj-directory))
      (setq b (get-file-buffer f))
      (unless b
        (prj-removehooks)
        (setq b (find-file-noselect f))
        (prj-addhooks)
        (when b
          (with-current-buffer b
            (rename-buffer n t)
            )
          (setq pos (cdr a))
          ))
      (when b
        (setcdr a b)
        (eproject-setup-quit)
        (switch-to-buffer b)
        (prj-restore-edit-pos pos (selected-window))
        (prj-setmenu)
        )))
  (setq prj-curfile a)
  )

(defun prj-restore-edit-pos (pos w)
  (when (consp pos)
    (let ((b (current-buffer)) (top (car pos)) (line (cadr pos)))
      (when (and (numberp top) (numberp line))
        (prj-goto-line top)
        (set-window-start w (point))
        (prj-goto-line line)
        ))))

(defun prj-select-window (w)
  (let (focus-follows-mouse)
    (select-window w)
    (select-frame-set-input-focus (window-frame w))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; choose next/previous file

(defun eproject-nextfile ()
  "Switch to the next file that belongs to the current project."
  (interactive)
  (prj-switch-file 'prj-next-file 'next-buffer)
  )

(defun eproject-prevfile ()
  "Switch to the previous file that belongs to the current project."
  (interactive)
  (prj-switch-file 'prj-prev-file 'previous-buffer)
  )

(defun prj-switch-file (fn1 fn2)
  (let ((a (rassoc (current-buffer) prj-files)))
    (cond (a
           (prj-edit-file (funcall fn1 prj-files a))
           )
          (prj-curfile
           (prj-edit-file prj-curfile)
           )
          (t
           (funcall fn2)
           ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set key shortcuts

(defun prj-setkeys ()
  (let ((f (consp prj-current))
	(a (assoc 'eproject-mode minor-mode-map-alist))
	(map (make-sparse-keymap))
   	)
    (if a
	(setcdr a map)
        (push (cons 'eproject-mode map) minor-mode-map-alist)
	)
    (when f
      (define-key map [M-right] 'eproject-nextfile)
      (define-key map [M-left] 'eproject-prevfile)
      (define-key map [C-f5] 'eproject-dired)
      (let ((n 0) fn s)
        (dolist (a prj-tools)
          (unless (setq fn (nth n prj-tools-fns))
            (setq fn (list 'lambda))
            (setq prj-tools-fns (nconc prj-tools-fns (list fn)))
            )
          (setcdr fn `(() (interactive) (prj-run-tool ',a)))
          (setq n (1+ n))
          (when (setq s (caddr a))
            (define-key map (prj-parse-key s) (and f fn))
            ))))
    (define-key map [f5] 'eproject-setup-toggle)
    ))

(defun prj-parse-key (s)
  (read
   (if (string-match "[a-z][a-z0-9]+$" s)
       (concat "[" s "]")
       (concat "\"\\" s "\""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set menus

(defun prj-list-sorted ()
  (sort (append prj-list nil)
        '(lambda (a b) (string-lessp (car a) (car b)))
        ))

(defun prj-setmenu ()
  (let ((f (consp prj-current)) m1 m2 m3)

    (setq m1
          `(("Open" open ,@(prj-menulist-maker prj-list prj-current 'prj-menu-open))
            ("Add/Remove" other
             ("Add ..." "Add new or existing project to the list" . eproject-add)
             ("Remove ..." "Remove project from the list" . eproject-remove)
             ,@(and f '(("Close" "Close current project" . eproject-close)))
             ("--")
             ("Setup" "Enter the project setup area." . eproject-setup-toggle)
             ("Help" "View eproject.txt" . eproject-help)
             )
            ))
    (when f
      (nconc m1 (cons '("--") (prj-menulist-maker prj-tools nil prj-tools-fns)))
      (setq m2
            `(("Dired" "Browse project directory in Dired - Use 'a' to add file(s) to the project" . eproject-dired)
              ("--")
              ,@(prj-menulist-maker prj-files prj-curfile 'prj-menu-edit)
              )))

    (prj-menu-maker
     global-map
     `((buffer "Project" project ,@m1)
       (file "List" list ,@m2)
       )
     '(menu-bar)
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-menu-edit ()
  (interactive)
  (let ((a (nth last-command-event prj-files)))
    (if a (prj-edit-file a))
    ))

(defun prj-menu-open ()
  (interactive)
  (let ((a (nth last-command-event prj-list)))
    (if a (eproject-open (car a)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-menu-maker (map l v)
  (let ((e (list nil)))
    (setq v (append v e))
    (dolist (k (reverse l))
      (let (s a)
	(when (symbolp (car k))
	  (setq a (pop k))
	  )
	(cond
	 ((numberp (car k))
	  (setcar e (pop k))
	  )
	 ((and (consp (cdr k)) (symbolp (cadr k)))
	  (setcar e (cadr k))
	  (setq s (cddr k))
	  (setq k (and s (cons (car k) (make-sparse-keymap (car k)))))
	  )
	 (t
	  (setcar e (intern (downcase (car k))))
	  ))
	(if a
	    (define-key-after map (vconcat v) k a)
	    (define-key map (vconcat v) k)
	    )
	(if s (prj-menu-maker map s v))
	))))

(defun prj-copy-head (l n)
  (let (r)
    (while (and l (> n 0))
      (push (pop l) r)
      (setq n (1- n))
      )
    (nreverse r)
    ))

(defun prj-split-list (l n)
  (let (r)
    (while l
      (push (prj-copy-head l n) r)
      (setq l (nthcdr n l))
      )
    (nreverse r)
    ))

(defun prj-menulist-maker (l act fns)
  (let (r (w 30) s (m 0) (n 0) k)
    (cond
     ((< (length l) w)
      (prj-menulist-maker-1 (list l fns n) act)
      )
     (t
      ;; menu too long; split into submenus
      (setq s (prj-split-list l w))
      (setq k (prj-menulist-maker-1 (list (append (pop s) '(("--"))) fns n) act))
      (setq r (nreverse k))
      (dolist (l s)
	(when (consp fns)
          (setq fns (nthcdr w fns))
          )
        (setq n (+ n w))
        (setq k (prj-menulist-maker-1 (list l fns n) act))
      	(push (cons (concat (prj-shortname (caar l)) " ...")
		    (cons (intern (format "m_%d" (setq m (1+ m))))
                          k)) r)
        )
      (nreverse r)
      ))))

(defun prj-menulist-maker-1 (l act)
  (let (r e f s i n a)
    (while (car l)
      (setq a (caar l))
      (setcar l (cdar l))
      (setq n (caddr l))
      (setcar (cddr l) (1+ n))
      (setq f (if (consp (cadr l))
                  (prog1 (car (cadr l)) (setcar (cdr l) (cdr (cadr l))))
                  (cadr l)))

      (setq i (car a))
      (unless (string-match "^ *#" i)
        (setq s (if (and (consp (cdr a)) (stringp (cadr a))) (cadr a) i))
        (cond ((equal ">" i)
               (setq e (cons s (cons (intern s) (prj-menulist-maker-1 l act))))
               (setq r (cons e r))
               )
              ((equal "<" i)
               (setq l nil)
               )
              (t
               (setq i (prj-shortname i))
               (setq e (cons n (if (eq a act)
                                   `(menu-item ,i ,f :button (:toggle . t) :help ,s)
                                 (cons i (cons s f)))))
               (setq r (cons e r))
               )))
      )
    (nreverse r)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run make and other commands

(defun prj-setup-tool-window ()
  (let ((bn "*compilation*") w h b c f)
    (unless (get-buffer-window bn t)
      (setq b (get-buffer-create bn))
      (setq f (frame-list))
      (cond ((cdr f)
             (setq w (frame-first-window (car f)))
             (delete-other-windows w)
             )
            (t
             (setq h (/ (* 70 (frame-height)) 100))
             (delete-other-windows w)
             (setq w (split-window w h))
             ))
      (set-window-buffer w b)
      )))

(defun prj-run (cmd)
  (let (dir)
    (when (string-match "^-in +\\([^[:space:]]+\\) +" cmd)
      (setq dir (match-string-no-properties 1 cmd))
      (setq cmd (substring cmd (match-end 0)))
      )
    (when prj-directory-run
      (setq dir (expand-file-name (or dir ".") prj-directory-run))
      )
    (if dir (cd dir))
    (cond ((string-match "^-e +" cmd)
           (setq cmd (read (substring cmd (match-end 0))))
           (unless (commandp cmd)
             (setq cmd `(lambda () (interactive) ,cmd))
             )
           (command-execute cmd)
           )
          ((string-match "\\(.+\\)& *$" cmd)
           (start-process-shell-command "eproject-async" nil (match-string 1 cmd))
           (message (match-string 1 cmd))
           )
          (t
           (unless (or (fboundp 'ecb-activate) (fboundp 'ewm-init))
             (prj-setup-tool-window)
             )
           (let ((display-buffer-reuse-frames t))
             (compile cmd)
             )))))

(defun prj-run-tool (a)
  (unless (string-match "^--+$" (car a))
    (prj-run (or (cadr a) (car a)))
    ))

(defun eproject-killtool ()
  (interactive)
  (let ((bn "*compilation*") w0 w1)
    (when (setq w1 (get-buffer-window bn t))
      (when (fboundp 'kill-compilation)
        (setq w0 (selected-window))
        (select-window w1)
        (kill-compilation)
        (select-window w0)
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run grep on project files

(require 'grep)

(defun eproject-grep (command-args)
  "Run the grep command on all the project files."
  (interactive
   (progn
     (grep-compute-defaults)
     (let ((default (grep-default-command)))
       (list (read-from-minibuffer
              "Run grep on project files: "
              (if current-prefix-arg default grep-command)
              nil
              nil
              'grep-history
              (if current-prefix-arg nil default)
              )))))
    (let ((default-directory prj-directory))
    (dolist (f (mapcar 'car prj-files))
      (setq command-args (concat command-args " " f))
      )
    (grep command-args)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add files to the project with dired

(require 'dired)

(defun prj-dired-addfiles ()
  (interactive)
  (when prj-current
    (let ((n 0) a)
      (dolist (f (dired-get-marked-files))
        (setq a (prj-insert-file f))
        (unless (cdr a)
          (setq n (1+ n))
          (setq prj-curfile a)
          ))
      (message "Added to project: %d file(s)" n)
      (prj-setmenu)
      )))

(defun prj-dired-run ()
  (interactive)
  (let ((f (dired-get-marked-files)) c)
    (and (setq c (pop f))
         (null f)
         (let ((prj-directory (file-name-directory c)))
           (prj-run c)))))

(defun eproject-dired ()
  "Start a dired window with the project directory."
  (interactive)
  (when prj-directory-run
    (eproject-setup-quit)
    ;;(message "Use 'a' to add marked or single files to the project.")
    (dired prj-directory-run)
    (let ((map dired-mode-map))
      (define-key map [mouse-2] 'dired-find-file)
      (define-key map "a" 'prj-dired-addfiles)
      (define-key map "r" 'prj-dired-run)
      (define-key map [menu-bar operate command] '("Add to Project"
	"Add current or marked file(s) to project" . prj-dired-addfiles))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-setup-all ()
  (prj-setkeys)
  (prj-setmenu)
  (prj-settitle)
  (prj-config-print)
)

(defun prj-getconfig (n)
  (let ((a (cdr (assoc n prj-config))))
    (and (stringp a) a)
    ))

(defun prj-setconfig (n v)
  (let ((a (assoc n prj-config)))
    (unless a
      (setq a (list n))
      (setq prj-config (nconc prj-config (list a)))
      )
    (setcdr a v)
    ))

(defun prj-on-kill ()
  (save-some-buffers t)
  (prj-saveall)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize

(defun prj-startup-delayed ()
  ;; where is this file
  (setq eproject-directory
    (file-name-directory (symbol-file 'eproject-startup)))

  ;; load UI support
  (load (eproject-addon "eproject-config") nil t)

  ;; When no projects are specified yet, load the eproject project itself.
  (unless prj-list
    (load (eproject-addon "eproject.cfg"))
    )

  ;; no project so far
  (prj-reset)
  (prj-setup-all)
  (add-hook 'kill-emacs-hook 'prj-on-kill)

  ;; inhibit open last project when a file was on the commandline
  (unless (buffer-file-name (window-buffer))
    (when prj-last-open

      ;; open last project
      (eproject-open prj-last-open)

      ;; restore frame position
      (unless (fboundp 'ewm-init)
        (when (and prj-frame-pos prj-initial-frame)
          (modify-frame-parameters prj-initial-frame prj-frame-pos)
          ;; emacs bug: when it's too busy it doesn't set frames correctly.
          (sit-for 0.2)
          ))))

  (when (fboundp 'ecb-activate)
    (ecb-activate)
    )
  )

(defun prj-command-line-switch (option)
  (setq prj-last-open (pop argv))
  (setq inhibit-startup-screen t)
  )

(defun eproject-startup ()
  (if (boundp 'prj-list)
    (progn
      (load (eproject-addon "eproject-config"))
      (prj-setup-all))
    (progn
      (prj-loadlist)
      (when prj-last-open (setq inhibit-startup-screen t))
      (when (display-graphic-p) (setq prj-initial-frame (selected-frame)))
      (push '("project" . prj-command-line-switch) command-switch-alist)
      (run-with-idle-timer 0.1 nil 'prj-startup-delayed)
      )))

;;;###autoload(require 'eproject)
(provide 'eproject)
(eproject-startup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eproject.el ends here
