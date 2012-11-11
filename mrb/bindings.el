;
; Unset some keys to let modes explicitly set them to their value (case in point: orgmode)
;
; Unset the standard right mouse click behaviour (it kills parts of regions)
(global-unset-key (kbd "<mouse-3>"))

;
; Let marks be set when shift arrowing, everybody does this
;
(setq shift-select-mode t)
(delete-selection-mode 1)

;
; Setup function keys the way I like it.

; Menu key does M-x, if we have it.
(global-set-key (kbd "<apps>") 'execute-extended-command)
(global-set-key [f1] 'help-command)
(global-set-key [f2] 'save-buffer)
(global-set-key [f3] 'find-file)

; Make gnome compliant
(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
(global-set-key [f11] 'switch-full-screen)
(global-set-key [XF86MenuKB] 'accelerate-menu)

; Font scaling, like in chrome
(global-set-key [(super =)] 'text-scale-increase)
(global-set-key [(super -)] 'text-scale-decrease)
; Font scaling, like in firefox
(global-set-key [(control +)] 'text-scale-increase)
(global-set-key [(control -)] 'text-scale-decrease)

; Line handling functions
;; For external keyboard FIXME: these bindings contain dead characters
;; if such a keyboard is used, which can be confusing.
(global-set-key [(?\s-\§)] 'toggle-truncate-lines)
;; For t510 keyboard (the same place)
(global-set-key [(super \`)] 'toggle-truncate-lines)

;; Most of the time I want return to be newline and indent
;; Every mode can augment this at will obviously (org-mode does, for example)
(global-set-key (kbd "RET") 'newline-and-indent)


; Moving back and forth in frames, disregarding frames
(define-key global-map [(super \\)] 'next-multiframe-window)
(define-key global-map [(super \|)] 'previous-multiframe-window)

;; Alt-Cmd left-right arrows browse through buffers within the same frame
(global-set-key (kbd "M-s-<left>") 'previous-buffer)
(global-set-key (kbd "M-s-<right>") 'next-buffer)

;; Ctrl Cmd moves buffers up and down in the tiling of emacs
(global-set-key (kbd "<C-s-up>")     'buf-move-up)
(global-set-key (kbd "<C-s-down>")   'buf-move-down)
;; These would conflict with awesome bindings
;; (global-set-key (kbd "<C-S-left>")   'buf-move-left)
;; (global-set-key (kbd "<C-S-right>")  'buf-move-right)

(global-set-key (kbd "M-s-<left>") 'previous-buffer)
(global-set-key (kbd "M-s-<right>") 'next-buffer)

(define-key global-map [?\s-~] 'ns-prev-frame)
;; FIME: does not work anymore
(global-set-key [(control tab)] 'switch-to-other-buffer)

(global-set-key [(super k)] 'ido-kill-buffer)


;; Resizing windows
;; Introduce a bit of intelligence so the shrink and enlarge know what window I'm in.
(defun xor (b1 b2)
  "Exclusive or between arguments"
  (or (and b1 b2)
      (and (not b1) (not b2))))

(defun move-border-left-or-right (arg dir)
  "General function covering move-border-left and move-border-right. If DIR is
  t, then move left, otherwise move right."
  (interactive)
  (if (null arg) (setq arg 5))
  (let ((left-edge (nth 0 (window-edges))))
    (if (xor (= left-edge 0) dir)
	(shrink-window arg t)
      (enlarge-window arg t)))
  )

(defun move-border-left (arg)
  (interactive "P")
  (move-border-left-or-right arg t))

(defun move-border-right (arg)
  (interactive "P")
  (move-border-left-or-right arg nil))

;; Same for up and down
(defun move-border-up-or-down (arg dir)
  "General function covering move-border-up and move-border-down. If DIR is
  t, then move up, otherwise move down."
  (interactive)
  (if (null arg) (setq arg 5))
  (let ((top-edge (nth 1 (window-edges))))
    (if (xor (= top-edge 0) dir)
	(shrink-window arg nil)
      (enlarge-window arg nil))))

(defun move-border-up (arg)
  (interactive "P")
  (move-border-up-or-down arg t))

(defun move-border-down (arg)
  (interactive "P")
  (move-border-up-or-down arg nil))

;; Use Super + Arrows to steer the borders
(global-set-key [(super right)] 'move-border-right)
(global-set-key [(super left)]  'move-border-left)
(global-set-key [(super up)] 'move-border-up)
(global-set-key [(super down)] 'move-border-down)


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

;; Bit of experimenting with keys chords to minimize thumb bending.
(require 'key-chord)
(key-chord-mode 1)    ;; Turn it on

(key-chord-define-global "df" 'smex)        ; Pressing d and f together does the exectute extended command, smex style
(key-chord-define-global "()"     "()\C-b") ; Brackets go together when pressed together

;; Rebind M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Commands are a plenty, smex is a one
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)



(provide 'bindings)
