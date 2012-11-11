;;
;; Emacs initialisation starting point
;;
;; FIXME: is .emacs.d not on the load path already, how else did we get here?
(add-to-list 'load-path "~/.emacs.d")

;; I am moving my configuration to an org-babel based system
(org-babel-load-file "mrb.org")
		     
;; Define where customization should be stored
;; anything done in custom.el can be overridden in explicit files
;; so we want to load our custom file first  and then the
;; crafted ones.
(setq custom-file (concat "~/.emacs.d/mrb/custom.el"))
(load custom-file)

;; Load all my configuration files
(load "mrb/packages")              ; Package handling, do this first, so we know how to load things
(load "mrb/global")                ; Generic settings
(load "mrb/terminal")              ; character/encoding/commandline and emulation handling.
(load "mrb/visual")                ; Make things look the way I want them 
(load "mrb/bindings")              ; Keyboard control
(load "mrb/buffers")               ; Buffer configuration
(load "mrb/modes")                 ; Setting about modes in general, not specific to one mode 
(load "mrb/org-mode")              ; Orgmode configuration
(load "mrb/statusnet")             ; Statusnet configuration
(load "mrb/google-map")            ; Google map integration for Emacs
(load "mrb/xmpp")                  ; XMPP configuration
(load "mrb/mail")                  ; Mail confguration
(load "mrb/openscad")              ; OpenSCAD mode
(load "mrb/ldap")                  ; LDAP integration
(load "mrb/eshell")                ; Eshell configuration

;; Make sure erase works properly
;; (not sure I completely understand the rationale behind this)
(if window-system  (normal-erase-is-backspace-mode t))

;; Tramp
(setq tramp-default-method "ssh")

;; Default browswer 
(setq browse-url-browser-function (quote browse-url-generic))
(setq browse-url-generic-program "chromium-browser")

;; Interactively do things
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching
(ido-everywhere)

;; erc
;; Probably move this to a file of its own once it gets longer
;; TODO: prevent the continuous asking for username
(require 'erc-services)
(and
     (require 'erc-highlight-nicknames)
     (add-to-list 'erc-modules 'highlight-nicknames)
     (erc-update-modules))
(setq
  erc-prompt-for-nickserv-password nil
  erc-hide-list '("JOIN" "PART" "QUIT")
  erc-nick '("Marcel|HSD" "Marcel||HSD")
  erc-nickserv-passwords '((freenode (("Marcel|HSD" . "PASSWORDHERE"))))
)



;; Enable highlight parentheses for all buffers
;; FIXME: this needs one more color than actually used. The last one does not get highlighted!
(setq hl-paren-colors (quote ("firebrick" "lightgreen" "orange" "cyan" "yellow" "blue")))

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;;; Expand region global key, move to bindings later
;;; packge should have been loaded by elpa
(require 'expand-region)

;; Do smart tabbing, this does mostly the right thing everywhere
(require 'smart-tab)
;; Make sure it does the right thing in some modes, notably erc
(setq smart-tab-completion-functions-alist
      (quote (
	      (emacs-lisp-mode . lisp-complete-symbol)
	      (text-mode . dabbrev-completion)
	      (erc-mode . pcomplete)))
      smart-tab-disabled-major-modes
      (quote (org-mode org-agenda-mode term-mode)))

;; Enable everywhere
(global-smart-tab-mode 1)

;; Multiple cursors sounds interesting
(require 'multiple-cursors)

;; I want to manage my own templates
(require 'xlicense)
(setq license-directory "~/.emacs.d/licenses")
(add-to-list 'license-types '(agpl . "AGPL"))

;; Wrap region mode to simplify quoting etc.
;; For now, enable globally, use exceptions when we find problems
(add-to-list 'load-path "~/dev/emacs/packages/wrap-region")

;; Wrap region allows to delimit a region with quotes, comment chars
;; or whatever is configured.
(require 'wrap-region)
(wrap-region-global-mode 1)
;; (add-to-list 'wrap-region-except-modes 'conflicting-mode)

