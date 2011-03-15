;
; Jabber specific settings
;
; Directly tie into the GIT repository on this machine if needed
(add-to-list 'load-path "~/dev/emacs/packages/emacs-jabber/")
(add-to-list 'load-path "~/dev/emacs/packages/emacs-jabber/compat/")
;;(require 'jabber-autoloads)
(require 'jabber)

;; Send notifications about new messages to notify
(defvar libnotify-program "/usr/bin/notify-send")

(defun notify-send (title message)
  (start-process "notify" " notify"
		 libnotify-program "--expire-time=4000" title message))

(defun libnotify-jabber-notify (from buf text proposed-alert)
  "(jabber.el hook) Notify of new Jabber chat messages via libnotify"
  (when (or jabber-message-alert-same-buffer
            (not (memq (selected-window) (get-buffer-window-list buf))))
    (if (jabber-muc-sender-p from)
        (notify-send (format "(PM) %s"
                       (jabber-jid-displayname (jabber-jid-user from)))
               (format "%s: %s" (jabber-jid-resource from) text)))
      (notify-send (format "%s" (jabber-jid-displayname from))
             text)))

(add-hook 'jabber-alert-message-hooks 'libnotify-jabber-notify)

;; Show some info in the modeline
(jabber-mode-line-mode 1)

(provide 'jabber-settings)
