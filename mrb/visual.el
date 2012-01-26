;; 
;; Visual customizations, make it look the way I want to
;;
;; We use a dark backgroun
(setq-default frame-background-mode 'dark)

;; no splash screen
(setq inhibit-startup-screen  t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; TODO: check speed consequences of this
(setq column-number-mode t)

; Zenburn theme with a slightly darker background, the default looks too misty for me
(defvar zenburn-bg "#303030")
(defvar zenburn-bg-1 "#303030")
(load-theme 'zenburn)

;
; Zenburn corrections
;
; The default zenburn colors for the modeline, esp. the active one
; confuses me to no end, make it stand out more
(set-face-background  'mode-line "#4c7073")
; Zenburn underlines date face in org, no go
(set-face-attribute 'org-date nil :underline nil)

(custom-theme-set-faces 'zenburn
)


;
; When I am not typing, the cursor should become more visible, so I
; don't lose it.
(require 'cursor-chg)  ; Load this library
(change-cursor-mode 0) ; On for overwrite/read-only/input mode
(toggle-cursor-type-when-idle 1) ; On when idle
(setq curchg-default-cursor-color "LightGreen")
(setq curchg-default-cursor-type (quote bar\ \.\ 1))
 
; Default frame properties frame position, color, etc
(setq default-frame-alist
      '((cursor-type . (bar . 1))
        (cursor-color . "LightGreen")
	(height . 60)
	(width . 100)
))

; Parenthesis matching
(show-paren-mode 1)
(setq show-paren-style (quote expression))
(setq show-paren-delay 0)
; Just change the background when matching the expression, leave other
; fontifying as is
(set-face-attribute 'show-paren-match nil :inherit nil :background "#3c3c3c")

;; As we start emacs in daemon mode, certain (mostly visual) settings
;; do not get applied. This is because starting the daemon does not
;; open an X connection. This only happens when frames are being
;; opened. By adding these settings to the 'server-visit-hook' we can
;; still seemingly painless apply these settings.
(defun run-client-settings()
  ;; When making a selection, keep all font-locking too, but make it
  ;; stand out from the matching background
  (set-face-attribute 'region nil :inherit nil :background "#242424" :foreground nil)
  (tool-bar-mode -1)   ;; No tool-bar
  (scroll-bar-mode -1) ;; No scroll-bar
)
;; This seems to work if we start up emacs using filename on cli and
;; server was not running yet. It does however not work if we just
;; startup emacs (through my edit script) without a filename on the cli 
(add-hook 'server-visit-hook 'run-client-settings)

;; Make colorful balanced parentheses etc. in different modes
(add-hook 'lisp-mode 'rainbow-delimiters-mode)

(provide 'visual)
