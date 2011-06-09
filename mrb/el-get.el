;
; el-get
;
; TODO: this could manage all emacs packages, including the ones gotten from apt-get, git and elpa
; The reason this is nice that it provides one neat umbrella (in Emacs) to manage packages for Emacs.

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(require 'el-get)
(setq el-get-sources
      '(
	(:name el-get
	       :type git
	       :url "git://github.com/dimitri/el-get.git"
	       :features "el-get")
	color-theme color-theme-zenburn
	smex
	apache-mode
	gnuplot-mode
	markdown-mode
	rainbow-mode
	cursor-chg
	sudo-save
	erc-highlight-nicknames
	edit-server
	magit
	(:name php-mode-improved)         ; Just to have example for other syntax
	(:name gisthub
	       :type git
	       :url  "git://github.com/defunkt/gist.el.git"
	       :features gist)
	(:name newlua-mode
	       :type git 
	       :url "git://github.com/immerrr/lua-mode.git"
	       :features lua-mode)
	auto-complete
	rainbow-delimiters
))
(el-get)

(provide 'el-get-settings)
