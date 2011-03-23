
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(*pastie-restricted* nil)
 '(erc-modules (quote (autojoin button completion fill irccontrols keep-place list log match menu move-to-prompt netsplit networks noncommands readonly replace ring smiley stamp track unmorse)))
 '(flyspell-issue-message-flag nil)
 '(global-auto-revert-mode t)
 '(gnuplot-program "/opt/local/bin/gnuplot")
 '(gud-gdb-command-name "gdb --annotate=1")
 '(ispell-use-framepop-p t)
 '(markdown-css-path "/home/mrb/.markdown.css")
 '(org-agenda-custom-commands (quote (("w" "Waiting For list" tags "+waiting-inactive" ((org-agenda-overriding-header "WAITING FOR-list") (org-agenda-view-columns-initially t) (org-agenda-overriding-columns-format "%65ITEM %25Responsible %SCHEDULED %TAGS") (org-agenda-dim-blocked-tasks t))) ("b" "Buying list" tags "+buy-inactive-waiting+TODO=\"TODO\"" ((org-agenda-overriding-header "Buying list") (org-agenda-dim-blocked-tasks t))) ("p" "Active project list" tags "-inactive/PROJ" ((org-agenda-overriding-header "Active project list"))) ("r" "To Review" ((stuck "" ((org-agenda-overriding-header "STUCK projects"))) (tags-todo "SCHEDULED=\"\"+DEADLINE=\"\"-{.}-BLOCKED=\"t\"/TODO" ((org-agenda-overriding-header "Untagged items"))) (tags-todo "-inactive+SCHEDULED=\"\"+DEADLINE=\"\"-BLOCKED=\"t\"+TODO=\"TODO\"+{.}" ((org-agenda-overriding-header "Unscheduled active items")))) nil) ("S" "Someday/Maybe List" ((tags "+inactive/PROJ" ((org-agenda-overriding-header "Inactive projects"))) (tags-todo "+inactive-BLOCKED=\"t\"/TODO" ((org-agenda-overriding-header "Inactive TODO items")))) nil nil) ("c" "Scheduled overview" tags "SCHEDULED<>\"\"|DEADLINE<>\"\"/TODO" ((org-agenda-overriding-header "SCHEDULED") (org-agenda-view-columns-initially t) (org-agenda-overriding-columns-format "%65ITEM %25Responsible %SCHEDULED %DEADLINE %TAGS") (org-agenda-dim-blocked-tasks t))) ("l" "Blocked projects and tasks" ((tags-todo "+BLOCKED=\"t\"/PROJ" ((org-agenda-overriding-header "Blocked projects") (org-agend-dim-blocked-tasks t))) (tags-todo "+BLOCKED=\"t\"/TODO" ((org-agenda-overriding-header "Blocked tasks") (org-agend-dim-blocked-tasks t)))) nil nil) ("n" "Next Action List [hides blocked/inactive/waiting/INBOX-ed]" tags-todo "+SCHEDULED=\"\"+DEADLINE=\"\"-BLOCKED=\"t\"-inactive-waiting" ((org-agenda-overriding-header "Next Action List"))) ("g" "AGENDA" agenda "" ((org-agenda-filter-preset (quote ("-inactive" "-waiting"))) (org-agenda-skip-function (quote (org-agenda-skip-entry-if (quote regexp) "CANCELLED"))) (org-agenda-span (quote day)))))))
 '(smime-keys (quote (("marcel@hsdev.com" "~/keys/thawte-email-marcel@hsdev.com.pem" nil))))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#303030" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "microsoft" :family "Consolas"))))
 '(flyspell-duplicate ((t (:foreground "goldenrod" :underline t :weight bold))))
 '(flyspell-incorrect ((t (:foreground "tomato" :underline t :weight bold))))
 '(org-agenda-date ((t (:inherit org-agenda-structure :height 1.2))) t)
 '(org-agenda-date-today ((t (:foreground "white" :slant italic :weight bold :height 1.2))))
 '(org-date ((t (:height 0.85))))
 '(org-document-title ((((class color) (background light)) (:foreground "light blue" :weight bold :height 1.44))))
 '(org-hide ((((background dark)) (:foreground "#303030"))))
 '(org-property-value ((t (:height 0.85))) t)
 '(org-sexp-date ((t (:height 0.85))))
 '(org-special-keyword ((t (:height 0.85)))))

