;;;; Package --- Emacs initialisation of mrvdb
;;; Commentary:
;; Emacs initialisation starting point
;;
;; I want to have as little in here as possible.  The configuration is
;; org-babel based.  This means the bootstrap here is to load a proper
;; (part of) org-mode and be on our way.

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/dat/src/emacs/packages/org-mode/lisp/")

;; Load in the main org file which starts up configuration This will
;; lead to an mrb.el file automatically, so that can't exist in the
;; current directory for this to work.
(require 'org)
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
