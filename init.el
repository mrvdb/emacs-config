;;
;; Emacs initialisation starting point
;;
;; I want to have as little in here as possible. The configuration is
;; org-babel based. This means the bootstrap here is to load a proper
;; (part of) org-mode and be on our way.
(add-to-list 'load-path "~/dev/emacs/packages/org-mode/lisp/")

;; Load in the main org file which starts up configuration This will
;; lead to an mrb.el file automatically, so that can't exist in the
;; current directory for this to work. 
(org-babel-load-file "~/.emacs.d/mrb.org")

;; END init.el
;; This is all there should be in this file, the rest is handled in org-mode.
;; Everything below here must be migrated to mrb.org!!!!

;; Make sure erase works properly
;; (not sure I completely understand the rationale behind this)
(if window-system  (normal-erase-is-backspace-mode t))

;; Tramp
(setq tramp-default-method "ssh")

;; Default browswer 
(setq browse-url-browser-function (quote browse-url-generic))
(setq browse-url-generic-program "chromium-browser")

;; I want to manage my own templates
(require 'xlicense)
(setq license-directory "~/.emacs.d/licenses")
(add-to-list 'license-types '(agpl . "AGPL"))

