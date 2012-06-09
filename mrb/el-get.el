;
; el-get
;
; IMPORTANT: this manages *ALL* my emacs packages, including the ones gotten from apt-get, git and elpa
; The reason this is nice that it provides one neat umbrella (in Emacs) to manage packages for Emacs.

;; This holds the generated initialisation file
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(require 'el-get)

;;
;; This holds two types of source specifications:
;; 1. just a name : the recipe is on file in el-get (see recipes dir)
;; 2. a full spec : the source where the package comes from is spelled out.
;; 
;; When installing new package, add a recipe in el-get-sources below,
;; eval it and then run el-get-install on the package name.

(setq el-get-sources
      '(
	;; Obviously we need el-get itself, so we can run a self-update
	;; There is a recipe, but that is conservative, runs version 3.stable
	(:name el-get
	       :type git
	       :url "git://github.com/dimitri/el-get.git"
	       :features "el-get")
	;; Using a different source than the built-in recipe (why was this again?)
	(:name zenburn-emacs
	       :type git 
	       :url "https://github.com/bbatsov/zenburn-emacs.git")
	;; Version from elpa does not work for me (init problem?)
	smex
	ace-jump-mode
	apache-mode
	(:name cursor-chg
	       :type git
	       :url  "https://github.com/emacsmirror/cursor-chg.git"
	       :features cursor-chg)
	gnuplot-mode
	sudo-save
	erc-highlight-nicknames
	edit-server
	magit
	markdown-mode
	(:name php-mode-improved)         ; Just to have example for other syntax
	(:name newlua-mode
	       :type git 
	       :url "git://github.com/immerrr/lua-mode.git"
	       :features lua-mode)
	rainbow-delimiters
	xlicense
	(:name typopunct
	       :type git
	       :url "https://github.com/emacsmirror/typopunct.git"
	       :features typopunct)
	(:name oauth
	       :type git
	       :url "https://github.com/psanford/emacs-oauth.git")
	scratch
	highlight-parentheses
	(:name sauron
	       :type git
	       :url "https://github.com/djcb/sauron.git")

	(:name calfw
	       :type git
	       :url "https://github.com/kiwanami/emacs-calfw.git")

	(:name expand-region 
	       :type git 
	       :url "https://github.com/magnars/expand-region.el.git")

	(:name mark-multiple 
	       :type git 
	       :url "https://github.com/magnars/mark-multiple.el.git")

	(:name multiple-cursors 
	       :type git 
	       :url "https://github.com/magnars/multiple-cursors.el.git")

	(:name fixed-point-completion
	       :type git
	       :url "https://github.com/smithzvk/fixed-point-completion.git")

	))
(el-get)

(provide 'el-get-settings)
