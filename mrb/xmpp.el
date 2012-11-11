;
; Jabber specific settings
;
; Directly tie into the GIT repository on this machine if needed
(add-to-list 'load-path "~/dev/emacs/packages/emacs-jabber/")
(add-to-list 'load-path "~/dev/emacs/packages/emacs-jabber/compat/")
;;(require 'jabber-autoloads)
(require 'jabber)

; Configuration variables
(setq 
 jabber-show-offline-contacts nil
 jabber-default-priority 30
 ;; Get the password outa here
 jabber-account-list (quote (("marcel@hsdev.com/Emacs" (:password . "PASSWORDHERE") (:connection-type . ssl))))
 jabber-alert-message-hooks (quote (jabber-message-libnotify jabber-message-echo jabber-message-scroll))
 jabber-message-alert-same-buffer nil
 jabber-roster-show-bindings nil
)

;; Me wants the smileys too
;; FIXME: I suspect this makes chats annoyingly slow to type in?
;;(require 'autosmiley)
;;(add-hook 'jabber-chat-mode-hook 'autosmiley-mode)

;; Show some info in the modeline
(jabber-mode-line-mode 1)

;; Do not steal my focus in the mini buffer
;; Message alert hooks
(define-jabber-alert echo "Show a message in the echo area"
  (lambda (msg)
    (unless (minibuffer-prompt)
      (message "%s" msg))))

