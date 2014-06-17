;;
;; Emacs initialisation starting point
;;
;; I want to have as little in here as possible. The configuration is
;; org-babel based. This means the bootstrap here is to load a proper
;; (part of) org-mode and be on our way.
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Load in the main org file which starts up configuration This will
;; lead to an mrb.el file automatically, so that can't exist in the
;; current directory for this to work.
(require 'use-package)
(use-package org
	     :commands org-babel-load-file)
(org-babel-load-file "~/.emacs.d/mrb.org")

;; END init.el
;; This is all there should be in this file, the rest is handled in org-mode.

;; Exception 1:
;; Apparently when disabled functions get enabled, Emacs puts them here
;;
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
