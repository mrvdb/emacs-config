;
; el-get
;
; TODO: this could manage all emacs packages, including the ones gotten from apt-get, git and elpa
; The reason this is nice that it provides one neat umbrella (in Emacs) to manage packages for Emacs.

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

; FIXME: this has to be here before the color-theme install to prevent a load error
(add-to-list 'load-path "~/.emacs.d/el-get/color-theme")

(require 'el-get)
(setq el-get-sources
      '(
	(:name color-theme)
	(:name color-theme-zenburn)
	(:name smex)
	(:name rainbow-mode)
	(:name gnuplot-mode)
	(:name markdown-mode)
	(:name cursor-chg)
	(:name sudo-save)
	(:name php-mode-improved)
))
(el-get)

(provide 'el-get-settings)