;
; Unset some keys to let modes explicitly set them to their value (case in point: orgmode)
;
; Unset the standard right mouse click behaviour (it kills parts of regions)
(global-unset-key (kbd "<mouse-3>"))

;
; Let marks be set when shift arrowing, everybode does this
;
(setq shift-select-mode t)
(delete-selection-mode 1)

;
; Setup function keys the way I like it.

(global-set-key [f1] 'help-command)
(global-set-key [f2] 'save-buffer)
(global-set-key [f3] 'find-file)
(global-set-key [XF86MenuKB] 'accelerate-menu)

; Font scaling, like in chrome
(global-set-key [(super =)] 'text-scale-increase)
(global-set-key [(super -)] 'text-scale-decrease)
; Font scaling, like in firefox
(global-set-key [(control +)] 'text-scale-increase)
(global-set-key [(control -)] 'text-scale-decrease)

; Line handling functions
(global-set-key [(?\s-\§)] 'toggle-truncate-lines)

; Moving back and forth in frames, disregarding frames
(define-key global-map [(super \')] 'next-multiframe-window)
(define-key global-map [(super \")] 'previous-multiframe-window)
(global-set-key (kbd "M-s-<left>") 'previous-buffer)
(global-set-key (kbd "M-s-<right>") 'next-buffer)
(define-key global-map [?\s-~] 'ns-prev-frame)
(global-set-key [(control tab)] 'switch-to-other-buffer)
(global-set-key [(super k)] 'kill-this-buffer)

; cut, copy and paste with cmd-key (like on osx). 
(global-set-key [(super z)] 'undo)
(global-set-key [(super x)] 'clipboard-kill-region)
(global-set-key [(super c)] 'clipboard-kill-ring-save)
(global-set-key [(super v)] 'yank)
(global-set-key [(super a)] 'mark-whole-buffer)

(global-set-key [(kp-delete)] 'delete-char)

; Make `C-x C-m' and `C-x RET' be different (since I tend
; to type the latter by accident sometimes.)
(define-key global-map [(control x) return] nil)

;
; Key bindings I am used to somehow
;
(global-set-key [(super /)] 'comment-or-uncomment-region)
(global-set-key [(super l)] 'goto-line)
(global-set-key [(super s)] 'save-buffer)

(defun new-empty-buffer ()
  "Opens a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (make-frame)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
;; note: emacs won't offer to save a buffer that's
;; not associated with a file,
;; even if buffer-modified-p is true.
;; One work around is to define your own my-kill-buffer function
;; that wraps around kill-buffer, and check on the buffer modification
;; status to offer save
;; This custome kill buffer is close-current-buffer.
(global-set-key [(super n)] 'new-empty-buffer)

(provide 'bindings)
