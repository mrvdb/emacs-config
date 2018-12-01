;;;; Package --- Emacs initialisation of mrvdb
;;; Commentary:
;; Emacs initialisation starting point
;;
;; I want to have as little in here as possible.  The configuration is
;; org-babel based.  This means the bootstrap here is to load a proper
;; (part of) org-mode and be on our way.

;;; Code:

;; Some performance tweaks
;; These two lines prevent a stuttering cursor for me, in most cases
;; FIXME: this sets gc to 500Mb / but in idle 5 seconds Obviously wrong!!
(setq gc-cons-threshold 500000000)
(run-with-idle-timer 5 t #'garbage-collect)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents) ;; Just to make sure (note that refresh only runs if use-package is NOT installed)
  (package-install 'use-package))

;; Load in the main org file which starts up configuration This will
;; lead to an mrb.el file automatically, so that can't exist in the
;; current directory for this to work.
(use-package org)

;; We have to set the local variables safe for the file (writefreely related)
(add-to-list 'safe-local-variable-values '(writefreely-post-id . "wf83bq5jwz"))
(add-to-list 'safe-local-variable-values '(writefreely-post-token))
(org-babel-load-file "~/.emacs.d/mrb.org")

;; END init.el
;; This is all there should be in this file, the rest is handled in org-mode.

;; Exception 1:
;; Apparently when disabled functions get enabled, Emacs puts them here
;;

;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'list-timers 'disabled nil)
