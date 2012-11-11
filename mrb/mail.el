;; Mail and News configuration
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
  smtpmail-smtp-service 24
  smtpmail-local-domain "hsdev.com"
  smtpmail-sendto-domain "hsdev.com"
  send-mail-function 'smtpmail-send-it                 ; This is for mail
  message-send-mail-function 'message-smtpmail-send-it ; This is for gnus

  ; Always put one in the Sent folder on sending
  message-default-mail-headers "Bcc: mrb+Sent@hsdev.com\n"
  mail-yank-prefix ">> "
)
;; Automatically sign outgoing messages, be part of the solution here,
;; not the problem
;; TODO: where can I toggle this on/off while composing?
;;(setq smime-keys (quote (("marcel@hsdev.com" "~/keys/comodo-2012-06-12.pem" nil))))
;;(add-hook 'message-send-hook 'mml-secure-message-sign-smime)
(setq password-cache t)            ; default is true, so no need to set this actually
(setq password-cache-expiry 28800) ; default is 16 seconds, which is ridiculously low
(require 'smtpmail)

;;(add-hook 'message-mode-hook 'orgstruct++-mode 'append)
(add-hook 'message-mode-hook 'turn-on-auto-fill 'append)

