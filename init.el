;
; Emacs initialisation starting point
; 
(add-to-list 'load-path "~/.emacs.d")

; Define where customization should be stored
; anything done in custom.el can be overridden in explicit files
(setq custom-file (concat "~/.emacs.d/mrb/custom.el"))
(load custom-file 'noerror)

; Load all my configuration files
(load "mrb/el-get")                ; Package handling, do this first.
(load "mrb/global")                ; Generic settings
(load "mrb/visual")                ; Make things look the way I want them 
(load "mrb/bindings")              ; Keyboard control
(load "mrb/buffers")               ;
(load "mrb/modes")                 ; Setting about modes in general, not specific to one mode 
(load "mrb/org-mode")              ; Orgmode configuration
(load "mrb/statusnet")             ; Statusnet configuration
(load "mrb/google-map")            ; 
(load "mrb/xmpp")                  ;
(load "mrb/openscad")              ; OpenSCAD mode
(load "mrb/ldap")                  ; LDAP integration


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
(require 'smtpmail)

; S/MIME signing always and automatically
; TODO: where can I toggle this on/off while composing?
;(add-hook 'message-send-hook 'mml-secure-message-sign-smime) 
(setq password-cache t)            ; default is true, so no need to set this actually
(setq password-cache-expiry 28800) ; default is 16 seconds


;; Tramp
(setq tramp-default-method "ssh")



;; Default browswer is chromium, why does emacs not find that automatically?
(setq browse-url-browser-function (quote browse-url-generic))
(setq browse-url-generic-program "chromium-browser")

; Interactively do things
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching
(ido-everywhere)

; erc
(and
     (require 'erc-highlight-nicknames)
     (add-to-list 'erc-modules 'highlight-nicknames)
     (erc-update-modules))

(require 'edit-server)
(setq edit-server-new-frame nil)
(edit-server-start)
