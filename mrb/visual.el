;; 
;; Visual customizations, make it look the way I want to
;;

;; no splash screen
(setq inhibit-startup-screen  t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Screen organisation
(menu-bar-mode -1)     ;; No menu-bar
(when window-system 
  (scroll-bar-mode -1) ;; No scroll-bar
  (tool-bar-mode -1)   ;; No tool-bar
)
(setq column-number-mode t)

; Zenburn theme with a slightly darker background, the default looks too misty for me
(defvar zenburn-bg "#303030")
; Also change the bg-1 background 
; TODO: this it probably not the way
; to do this, but i could not customize the org-hide face to stick,
; while this method did.
(defvar zenburn-bg-1 "#303030")
(require 'zenburn)
(color-theme-zenburn)

;
; When I am not typing, the cursor should become more visible, so I don't lose it.
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