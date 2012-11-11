; Identica comes directly from its git repository
(add-to-list 'load-path  "~/dev/emacs/packages/identica-mode")
(load-library "identica-mode.el")
(require 'longlines)
(setq 
 statusnet-server "o.mrblog.nl"
 statusnet-access-url "http://o.mrblog.nl/api/oauth/access_token"
 statusnet-authorize-url "http://o.mrblog.nl/api/oauth/authorize"
 statusnet-request-url "http://o.mrblog.nl/api/oauth/request_token"
 identica-username "mrb"
 identica-password "PASSWORDHERE" ;; .authinfo does not work for me
 identica-display-success-messages nil
 identica-soft-wrap-status t
 identica-status-format "%i %s %r: %t"
 identica-timer-interval 120
 identica-update-status-method (quote minibuffer)
 identica-oldest-first nil
 identica-soft-wrap-status nil
 identica-urlshortening-service (quote isgd) 
 identica-enable-striping t
 identica-enable-highlighting t
)


(global-set-key "\C-cip" 'identica-update-status-interactive)
(global-set-key "\C-cid" 'identica-direct-message-interactive)
(global-set-key "\C-cis" 'identica-shortenurl-replace-at-point)

;; I'm using a floating frame in awesome WM for dents
(defun make-dent-frame ()
  "Create a new frame and run identica-update status."
  (interactive)
  ;; Create and select the frame
  (select-frame (make-frame '((name . "dent") 
		(width . 80) (height . 15)
		(menu-bar-lines . 0) (tool-bar-lines . 0))))
  ;; Capture a Todo entry, force edit-window method
  (identica-update-status 'edit-buffer)

  ;; Once there, make sure we're the only one
  (delete-other-windows)
)

;; Make sure we remove our frame too
(defadvice identica-update-status-from-edit-buffer-send (after delete-dent-frame-on-send activate)
  "Advise to close the frame"
  (if (equal "dent" (frame-parameter nil 'name))
      (delete-frame))
)
(defadvice identica-update-status-from-edit-buffer-cancel (after delete-dent-frame-on-cancel activate)
  "Advise to close the frame"
  (if (equal "dent" (frame-parameter nil 'name))
      (delete-frame))
)
(global-set-key "\C-cd" 'make-dent-frame)

(provide 'statusnet)
