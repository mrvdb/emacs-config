; Enable the best of textmate
;(textmate-mode 1)

; Assist with completion
;(company-mode 1)

;;
;; Extension mappings
;;

;; First, specifiy which files to load when functions are called
(autoload 'markdown-mode "markdown-mode" "Markdown." t)
(autoload 'gnuplot-mode  "gnuplot" "GNU-Plot" t)
(autoload 'php-mode  "php-mode" "PHP" t)
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(add-hook 'javascript-mode-hook 'javascript-custom-setup)
    (defun javascript-custom-setup ()
      (moz-minor-mode 1))

;; Second, specify the extension to function mappings
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; CSS
(autoload 'css-mode "css-mode" "Mode for editing CSS file" t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

;; javascript
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

(provide 'modes)
