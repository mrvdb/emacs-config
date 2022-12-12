;;;; Package --- Emacs initialisation of mrvdb
;;; Commentary:
;; Emacs initialisation starting point
;;
;; I want to have as little in here as possible.  The configuration is
;; org-babel based.  This means the bootstrap here is to load a proper
;; (part of) org-mode and be on our way.

;;; Code:

(require 'cl)                           ; for remove-if

;; Set gc really large, especially when loading the config file
;; These two lines prevent a stuttering cursor for me, in most cases
;; FIXME: gc collection in idle time is not the way to do this, but it works for me
(setq gc-cons-threshold (* 200 1024 1024))
(run-with-idle-timer 5 t #'garbage-collect)

;; If we have the native compiler, use it
(message (concat
          "Native compilation is "
          (if (and (fboundp 'native-comp-available-p) (native-comp-available-p))
              (progn
                (setq comp-deferred-compilation t)
                "")
            "*not* ")
          "available"))

;; HACK: Disable Org-mode that was shipped with Emacs and add one I control
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))
(add-to-list 'load-path "~/.emacs.d/straight/repos/org/lisp")

;; My org file is posted using writefreely, which uses local variables, we need them right away
(add-to-list 'safe-local-variable-values '(writefreely-post-id . "wf83bq5jwz"))
(add-to-list 'safe-local-variable-values '(writefreely-post-token . nil))

;; config-file var gets used in mrb.el as well, not sure I like that
(setq config-file (expand-file-name "mrb.org" user-emacs-directory))

;; This produces mrb.el which is then loaded. It checks datetime before tangling.
(org-babel-load-file config-file) ; Now uses the org in the custom location

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
