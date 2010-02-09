;;
;; This file contains general initialization of XEmacs on this computer
;; 
;; uptimes
(setq emacs-load-start-time (current-time))

;; Make sure we load from the directory of this file
;; which should be .emacs.d and add to our path
(setq dotfiles-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

;; All the rest we get from a local packages dir
(add-to-list 'load-path (concat dotfiles-dir "local/"))

;; Other setttings related to loading settings
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

; Main sections of configuation
(require 'cl)
(require 'elpa)
(require 'loaddefs)
(require 'global)
(require 'bindings)
(require 'buffers)
(require 'modes)

; Now load the customisations we've added through another way, so we
; make sure they overide the settings.
(load custom-file 'noerror)

;;
;; Specials
;;
(if window-system  (normal-erase-is-backspace-mode t))

;; Make sure we have a reasonably sized frame to work with initially.
(require 'maxframe)
(setq 
 mf-max-width 1000
 mf-offset-x 80
)
(add-hook 'window-setup-hook 'maximize-frame t)

;; Enable specific platform support for MAC
(require 'ns-platform-support)
(ns-extended-platform-support-mode 1)

;; Mail and News
(setq 
  user-full-name "Marcel van der Boom"
  user-mail-address "mrb@hsdev.com"
  smtpmail-default-smtp-server "smtp.hsdev.com"
  smtpmail-local-domain nil
  send-mail-function 'smtpmail-send-it
  mail-yank-prefix ">> "
)
(load-library "smtpmail")


;; Org-mode settings
(require 'org-settings)

;; Statusnet mode
(require 'statusnet)

;; Tramp
(setq tramp-default-method "ssh")

;; Firefox integration 
(global-set-key (kbd "C-x p")
		(lambda ()
                  (interactive)
                  (comint-send-string (inferior-moz-process)
 
                                     "BrowserReload();")))

; in-frame speedbar
(require 'sr-speedbar)
(global-set-key (kbd "C-x C-s") 'sr-speedbar-toggle)

;; Finally, start a server (if not already)
(server-start)

(provide 'init)
