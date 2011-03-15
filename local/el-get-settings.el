;
; el-get
;
; TODO: this could manage all emacs packages, including the ones gotten from apt-get, git and elpa
; The reason this is nice that it provides one neat umbrella (in Emacs) to manage packages for Emacs.

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(setq el-get-sources
      '((:name zenburn)
	(:name smex)
	(:name rainbow-mode)
	(:name gnuplot-mode)
	(:name markdown-mode)
))
(el-get)

(provide 'el-get-settings)