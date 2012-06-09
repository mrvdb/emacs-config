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

;; Full utf-8 support
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq current-language-environment "UTF-8")

(global-visual-line-mode 1)

;; Only require to type 'y' or 'n' instead of 'yes' or 'no' when prompted
(fset 'yes-or-no-p 'y-or-n-p)

; Use auto revert mode globally 
; This is save because emacs tracks if the file is saved in the editting buffer
; and if so, it will not revert to the saved file.
(global-auto-revert-mode t)

;; Turn on auto-fill minor mode for all text buffers
(add-hook 'text-mode-hook 'turn-on-auto-fill)


(require 'term)
(defun mrb/ansi-term (&optional new-buffer-name)
  "Start a terminal-emulator in a new buffer."
  (interactive) 

  (setq program "/bin/bash")

  ;; Pick the name of the new buffer.
  (setq term-ansi-buffer-name
	(if new-buffer-name
	    new-buffer-name
	  (if term-ansi-buffer-base-name
	      (if (eq term-ansi-buffer-base-name t)
		  (file-name-nondirectory program)
		term-ansi-buffer-base-name)
	    "ansi-term")))

  (setq term-ansi-buffer-name (concat "*" term-ansi-buffer-name "*"))

  ;; In order to have more than one term active at a time
  ;; I'd like to have the term names have the *term-ansi-term<?>* form,
  ;; for now they have the *term-ansi-term*<?> form but we'll see...

  (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
  (setq term-ansi-buffer-name (term-ansi-make-term term-ansi-buffer-name program))

  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-char-mode)

  ;; I wanna have find-file on C-x C-f -mm
  ;; your mileage may definitely vary, maybe it's better to put this in your
  ;; .emacs ...

  (term-set-escape-char ?\C-x)

  (switch-to-buffer term-ansi-buffer-name))

; Aliases
; TODO: move this to a more logical place
(defalias 'at 'mrb/ansi-term)

(provide 'global)
