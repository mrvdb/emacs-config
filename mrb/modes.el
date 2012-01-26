;;
;; Extension mappings
;;

;; First, specify which files to load when functions are called
(autoload 'markdown-mode "markdown-mode" "Markdown." t)
(autoload 'gnuplot-mode  "gnuplot"       "GNU-Plot" t)
(autoload 'php-mode      "php-mode"      "PHP" t)
(autoload 'css-mode      "css-mode"      "Mode for editing CSS file" t)
(autoload 'apache-mode   "apache-mode"   "Apache config files" t)

(add-hook 'javascript-mode-hook 'javascript-custom-setup)
    (defun javascript-custom-setup ()
      (moz-minor-mode 1))

;; Second, specify the extension to function mappings
(add-to-list 'auto-mode-alist '("\\.org\\'"      . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'"      . org-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.gp\\'"       . gnuplot-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'"      . php-mode))
(add-to-list 'auto-mode-alist '("\\.css$"        . css-mode))
(add-to-list 'auto-mode-alist '("\\.js$"         . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.htaccess"    . apache-mode))
(add-to-list 'auto-mode-alist '("\\.patch"       . diff-mode))

;; Open scratch buffer by default in the mode we are in at the moment
;; with C-u prefix a mode will be asked to use
(require 'scratch)
(provide 'modes)
