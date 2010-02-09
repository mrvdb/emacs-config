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
;
(global-set-key [f1] 'help-command)
(global-set-key [f2] 'save-buffer)
(global-set-key [f3] 'find-file)
(global-set-key [f12] 'todo-show)   ; this does not work, interferes with global OSX dashboard key

(global-set-key [(control f4)] 'kill-this-buffer)

;
; Set up other key bindings, possibly redundant
;

; Font scaling, like in chrome
(global-set-key [(super =)] 'text-scale-increase)
(global-set-key [(super -)] 'text-scale-decrease)
; Font scaling, like in firefox
(global-set-key [(control +)] 'text-scale-increase)
(global-set-key [(control -)] 'text-scale-decrease)



; Moving back and forth in frames, disregarding frames
(define-key global-map [?\s-\'] 'next-multiframe-window)
(define-key global-map [?\s-\"] 'previous-multiframe-window)
(global-set-key (kbd "M-s-<left>") 'previous-buffer)
(global-set-key (kbd "M-s-<right>") 'next-buffer)
(define-key global-map [?\s-~] 'ns-prev-frame)
(global-set-key [(control tab)] 'switch-to-other-buffer)

;
; Maps confusing keys
(global-set-key [(control z)] 'undo)
(global-set-key [(kp-delete)] 'delete-char)

; Make `C-x C-m' and `C-x RET' be different (since I tend
; to type the latter by accident sometimes.)
(define-key global-map [(control x) return] nil)


(global-set-key [(super /)] 'comment-or-uncomment-region)

(provide 'bindings)