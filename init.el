;
; Emacs initialisation starting point
; 
(add-to-list 'load-path "~/.emacs.d")

; Define where customization should be stored
; anything done in custom.el can be overridden in explicit files
(setq custom-file (concat "~/.emacs.d/mrb/custom.el"))
(load custom-file 'noerror)

;; TEMPORARILY
;;(add-to-list 'load-path (concat dotfiles-dir "mrb/"))

; Load all my configuration files
(load "mrb/el-get-settings")
(load "mrb/global")
(load "mrb/visual")
(load "mrb/bindings")
(load "mrb/buffers")
(load "mrb/modes")
(load "mrb/org-settings")
(load "mrb/statusnet")
(load "mrb/google-map-settings")
(load "mrb/jabber-settings")
(load "mrb/openscad")

; External packages
(require 'sudo-save)

;;
;; Specials
;;
(if window-system  (normal-erase-is-backspace-mode t))

;; Mail and News
(setq 
 ; User agent style is message mode (gnus, but independent of it)
  mail-user-agent 'message-user-agent

 ; My properties
  user-full-name "Marcel van der Boom"
  ; I'd like to use mrb@hsdev.com, but s/mime certificate is bound to
  ; marcel@hsdev.com although not all cleints are as picky as Mail.app
  ; for example. Claws thought mail sent with From: set to
  ; mrb@hsdev.com was OK.  
  ; On the other hand, Claws was much more picky about					
  user-mail-address "marcel@hsdev.com"

  ; Sending it
  smtpmail-default-smtp-server "localhost"
  smtpmail-local-domain "hsdev.com"
  smtpmail-sendto-domain "hsdev.com"
  send-mail-function 'smtpmail-send-it                 ; This is for mail
  message-send-mail-function 'message-smtpmail-send-it ; This is for gnus

  ; Always put one in the Sent folder on sending
  message-default-mail-headers "Bcc: mrb+Sent@hsdev.com\n"
  mail-yank-prefix ">> "
)
(load-library "smtpmail")

; S/MIME signing always and automatically
; TODO: where can I toggle this on/off while composing?
;(add-hook 'message-send-hook 'mml-secure-message-sign-smime) 
(setq password-cache t)            ; default is true, so no need to set this actually
(setq password-cache-expiry 28800) ; default is 16 seconds


;; Tramp
(setq tramp-default-method "ssh")

; LDAP integration
(require 'ldap)
(require 'eudc)

(setq 
   eudc-default-return-attributes nil
   eudc-strict-return-matches nil
   ldap-ldapsearch-args (quote ("-tt" "-LLL" "-x"))
   eudc-inline-query-format '((name)
                                 (firstname)
                                 (firstname name)
                                 (email)
                                  ))
(defun enz-eudc-expand-inline()
  (interactive)
  (move-end-of-line 1)
  (insert "*")
  (unless (condition-case nil
              (eudc-expand-inline)
            (error nil))
    (backward-delete-char-untabify 1))
  )
(eval-after-load "message"
  '(define-key message-mode-map (kbd "TAB") 'enz-eudc-expand-inline))
(eval-after-load "sendmail"
  '(define-key mail-mode-map (kbd "TAB") 'enz-eudc-expand-inline))
(eval-after-load "post"
  '(define-key post-mode-map (kbd "TAB") 'enz-eudc-expand-inline))


;; Default browswer is chromium, why does emacs not find that automatically?
(setq browse-url-browser-function (quote browse-url-generic))
(setq browse-url-generic-program "chromium-browser")

; Make gnome compliant
(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(global-set-key [f11] 'switch-full-screen)

; Interactively do things
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching
(ido-everywhere)

; Commands are a plenty, smex is a one
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; ;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


; erc
(and
     (require 'erc-highlight-nicknames)
     (add-to-list 'erc-modules 'highlight-nicknames)
     (erc-update-modules))


