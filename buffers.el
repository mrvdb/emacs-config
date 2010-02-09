
;; nicer buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; No scroll bar needed
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Minibuffer prompt is a prompt, don't enter it as text.
(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

;; Save places in buffers between sessions
(setq-default save-place t)
(require 'saveplace)

(provide 'buffers)