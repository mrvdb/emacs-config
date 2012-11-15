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

;; Interactively do things
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching
(ido-everywhere)


;;; Expand region global key, move to bindings later
;;; packge should have been loaded by elpa
(require 'expand-region)

;; Do smart tabbing, this does mostly the right thing everywhere
(require 'smart-tab)
;; Make sure it does the right thing in some modes, notably erc
(setq smart-tab-completion-functions-alist
      (quote (
	      (emacs-lisp-mode . lisp-complete-symbol)
	      (text-mode . dabbrev-completion)n
	      (erc-mode . pcomplete)))
      smart-tab-disabled-major-modes
      (quote (org-mode org-agenda-mode term-mode)))

;; Enable everywhere
(global-smart-tab-mode 1)

;; Multiple cursors sounds interesting
(require 'multiple-cursors)

;; I want to manage my own templates
(require 'xlicense)
(setq license-directory "~/.emacs.d/licenses")
(add-to-list 'license-types '(agpl . "AGPL"))

;; Wrap region mode to simplify quoting etc.
;; For now, enable globally, use exceptions when we find problems
(add-to-list 'load-path "~/dev/emacs/packages/wrap-region")

;; Wrap region allows to delimit a region with quotes, comment chars
;; or whatever is configured.
(require 'wrap-region)
(wrap-region-global-mode 1)
;; (add-to-list 'wrap-region-except-modes 'conflicting-mode)

