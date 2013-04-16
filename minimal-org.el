;;; Minimal setup to load latest `org-mode'
     
;; activate debugging
(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)
     
;; add latest org-mode to load path
(add-to-list 'load-path (expand-file-name "~/dev/emacs/packages/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/dev/emacs/packages/org-mode/contrib/lisp" t))
