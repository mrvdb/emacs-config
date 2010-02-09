; Identica comes directly from its git repository
(add-to-list 'load-path  "~/dev/emacs/packages/identica-mode")
(load-library "identica-mode.el")
(setq 
 identica-password "PASSWORDHERE"
 identica-username "mrb"
 statusnet-server "o.mrblog.nl"
 identica-display-success-messages nil
 identica-soft-wrap-status t
 identica-status-format "%i %s %r: %t"
 identica-timer-interval 10
 identica-update-status-method (quote minibuffer)
)


(global-set-key "\C-cip" 'identica-update-status-interactive)
(global-set-key "\C-cid" 'identica-direct-message-interactive)

(provide 'statusnet)