;; 
;; Visual customizations, make it look the way I want to
;;

;; no splash screen
(setq inhibit-startup-screen  t)
(setq inhibit-startup-message t)

;; Screen organisation
(menu-bar-mode -1)   ;; No menu-bar 
(scroll-bar-mode -1) ;; No scroll-bar
;
; Zenburn theme with a slightly darker background, the default looks too misty for my
(defvar zenburn-bg "#303030")
(require 'zenburn)
(color-theme-zenburn)

;
; When I am not typing, the cursor should become more visible, so I don't lose it.
(require 'cursor-chg)  ; Load this library
(change-cursor-mode 0) ; On for overwrite/read-only/input mode
(toggle-cursor-type-when-idle 1) ; On when idle

; Default frame properties frame position, color, etc
(setq default-frame-alist
      '((cursor-type . (bar . 1))
        (cursor-color . "LightGreen")
	(height . 60)
	(width . 100)
))
