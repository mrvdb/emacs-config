;
; Emacs initialisation starting point
; 
(add-to-list 'load-path "~/.emacs.d")

; Define where customization should be stored
; anything done in custom.el can be overridden in explicit files
; so we want to load our custom file first  and then the
; crafted ones.
(setq custom-file (concat "~/.emacs.d/mrb/custom.el"))
(load custom-file 'noerror)

; Load all my configuration files
(load "mrb/el-get")                ; Package handling, do this first.
(load "mrb/global")                ; Generic settings
(load "mrb/visual")                ; Make things look the way I want them 
(load "mrb/bindings")              ; Keyboard control
(load "mrb/buffers")               ;
(load "mrb/modes")                 ; Setting about modes in general, not specific to one mode 
(load "mrb/org-mode")              ; Orgmode configuration
(load "mrb/statusnet")             ; Statusnet configuration
(load "mrb/google-map")            ; 
(load "mrb/xmpp")                  ;
(load "mrb/mail")                  ; Mail confguration
(load "mrb/openscad")              ; OpenSCAD mode
(load "mrb/ldap")                  ; LDAP integration

; External packages
(require 'sudo-save)

;;
;; Specials
;;
(if window-system  (normal-erase-is-backspace-mode t))


; S/MIME signing always and automatically
; TODO: where can I toggle this on/off while composing?
;(add-hook 'message-send-hook 'mml-secure-message-sign-smime) 
(setq password-cache t)            ; default is true, so no need to set this actually
(setq password-cache-expiry 28800) ; default is 16 seconds


;; Tramp
(setq tramp-default-method "ssh")



;; Default browswer is chromium, why does emacs not find that automatically?
(setq browse-url-browser-function (quote browse-url-generic))
(setq browse-url-generic-program "google-chrome")

; Interactively do things
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching
(ido-everywhere)

;; erc
;; Probably move this to a file of its own
(and
     (require 'erc-highlight-nicknames)
     (add-to-list 'erc-modules 'highlight-nicknames)
     (erc-update-modules))
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(require 'edit-server)
(setq edit-server-new-frame nil)
(edit-server-start)
