;;
;; Global settings
;;
;; disable auto-save files (#foo#)
(setq 
 auto-save-default nil
 
 ; disable backup files (foo~)
 backup-inhibited t
 
 ; disable auto-save-list/.saves
 auto-save-list-file-prefix nil

 ; move files to the trash instead of rm
 delete-by-moving-to-trash t
 
 ; use clipboard
 x-select-enable-clipboard t

 display-warning-minimum-level 'error
 large-file-warning-threshold nil
 tab-width 4
 find-file-use-truenames nil
 find-file-compare-truenames t

 minibuffer-max-depth nil
 minibuffer-confirm-incomplete t
 complex-buffers-menu-p t
 next-line-add-newlines nil
 kill-whole-line t
 truncate-lines t
)


(global-visual-line-mode 1)

;; Only require to type 'y' or 'n' instead of 'yes' or 'no' when prompted
(fset 'yes-or-no-p 'y-or-n-p)

; Use auto revert mode globally 
; This is save because emacs tracks if the file is saved in the editting buffer
; and if so, it will not revert to the saved file.
(global-auto-revert-mode t)

;; Turn on auto-fill minor mode for all text buffers
(add-hook 'text-mode-hook 'turn-on-auto-fill)


;; Should this be here?
;; Try to have urls and mailto links clickable everywhere
(define-global-minor-mode global-goto-address-mode
  goto-address-mode
  (lambda ()
    (goto-address-mode 1)))
(global-goto-address-mode t)

(provide 'global)


