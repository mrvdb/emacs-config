;;
;; Global settings
;;
;; disable auto-save files (#foo#)
(setq auto-save-default nil)
 
;; disable backup files (foo~)
(setq backup-inhibited t)
 
;; disable auto-save-list/.saves
(setq auto-save-list-file-prefix nil)

;; move files to the trash instead of rm
(setq delete-by-moving-to-trash t)
 
;; use clipboard
(setq x-select-enable-clipboard t)

;; Global settings
(setq 
 display-warning-minimum-level 'error
 minibuffer-max-depth nil
 tab-width 4
 find-file-use-truenames nil
 find-file-compare-truenames t
 minibuffer-confirm-incomplete t
 complex-buffers-menu-p t
 next-line-add-newlines nil
 kill-whole-line t
 column-number-mode t
 truncate-lines t
 
)

;; Full utf-8 support
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq current-language-environment "UTF-8")

(global-visual-line-mode 1)

;; fix a mac-specific problem with ptys?
;; unset does not work
;; nil works, but doesn't return the process
;; t seems to work best
;;(setq process-connection-type t)

;; Only require to type 'y' or 'n' instead of 'yes' or 'no' when prompted
(fset 'yes-or-no-p 'y-or-n-p)

; Aliases
; TODO: move this to a more logical place
(defalias 'at 'ansi-term)

(provide 'global)
