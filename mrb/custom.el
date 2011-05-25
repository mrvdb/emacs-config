
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(*pastie-restricted* nil)
 '(erc-modules (quote (autojoin button completion fill irccontrols keep-place list log match menu move-to-prompt netsplit networks noncommands readonly replace ring smiley stamp track unmorse)))
 '(flyspell-issue-message-flag nil)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(ispell-use-framepop-p t)
 '(jabber-message-alert-same-buffer nil)
 '(markdown-css-path "/home/mrb/.markdown.css")
 '(org-agenda-custom-commands (quote (("w" "Waiting For list" tags "-inactive/WAITING" ((org-agenda-overriding-header "WAITING FOR-list") (org-agenda-view-columns-initially t) (org-agenda-overriding-columns-format "%65ITEM %Responsible %SCHEDULED %TAGS") (org-agenda-dim-blocked-tasks t) (org-agenda-group-by-property "Responsible"))) ("b" "Buying list" tags "+buy-inactive-waiting+TODO=\"TODO\"" ((org-agenda-overriding-header "Buying list") (org-agenda-dim-blocked-tasks t))) ("p" "Active project list" tags "-inactive+LEVEL>1-TODO=\"DONE\"" ((org-agenda-overriding-header "Active project list") (org-agenda-skip-function (quote mrb/skip-non-projects)))) ("r" "To Review" ((stuck "" ((org-agenda-overriding-header "STUCK projects"))) (tags-todo "SCHEDULED=\"\"+DEADLINE=\"\"-{.}-BLOCKED=\"t\"/TODO" ((org-agenda-overriding-header "Untagged items"))) (tags-todo "-inactive+SCHEDULED=\"\"+DEADLINE=\"\"-BLOCKED=\"t\"+TODO=\"TODO\"+{.}" ((org-agenda-overriding-header "Unscheduled active items")))) nil) ("S" "Someday/Maybe List" ((tags "+inactive/PROJ" ((org-agenda-overriding-header "Inactive projects"))) (tags-todo "+inactive-BLOCKED=\"t\"/TODO" ((org-agenda-overriding-header "Inactive TODO items")))) nil nil) ("c" "Scheduled overview" tags "SCHEDULED<>\"\"|DEADLINE<>\"\"/TODO" ((org-agenda-overriding-header "SCHEDULED") (org-agenda-view-columns-initially t) (org-agenda-overriding-columns-format "%65ITEM %25Responsible %SCHEDULED %DEADLINE %TAGS") (org-agenda-dim-blocked-tasks t))) ("l" "Blocked projects and tasks" ((tags-todo "+BLOCKED=\"t\"/PROJ" ((org-agenda-overriding-header "Blocked projects") (org-agend-dim-blocked-tasks t))) (tags-todo "+BLOCKED=\"t\"/TODO" ((org-agenda-overriding-header "Blocked tasks") (org-agend-dim-blocked-tasks t)))) nil nil) ("n" "Next Action List [hides blocked/inactive/waiting/INBOX-ed]" tags-todo "+SCHEDULED=\"\"+DEADLINE=\"\"-BLOCKED=\"t\"-inactive-waiting" ((org-agenda-overriding-header "Next Action List"))) ("g" "AGENDA" agenda "" ((org-agenda-filter-preset (quote ("-inactive"))) (org-agenda-skip-function (quote (org-agenda-skip-entry-if (quote todo) (quote ("WAITING"))))) (org-agenda-span (quote day)))) ("o" "Ouderdomsanalyse" tags-todo "-BLOCKED=\"t\"-inactive/-WAITING" ((org-agenda-files (quote ("GTD.org"))) (org-agenda-group-by-property "CREATED"))))))
 '(org-agenda-files (quote ("~/.outlet/GTD.org" "~/.outlet/orgmode.org" "~/.outlet/cobra.org" "~/.outlet/habits.org" "~/.outlet/meetings.org" "~/.outlet/blogs.org")))
 '(org-capture-templates nil)
 '(org-export-latex-classes (quote (("article" "\\documentclass[11pt,a4paper,twoside]{article}" ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}") ("\\paragraph{%s}" . "\\paragraph*{%s}") ("\\subparagraph{%s}" . "\\subparagraph*{%s}")) ("report" "\\documentclass[11pt]{report}" ("\\part{%s}" . "\\part*{%s}") ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}")) ("book" "\\documentclass[11pt]{book}" ("\\part{%s}" . "\\part*{%s}") ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}")) ("beamer" "\\documentclass{beamer}" org-beamer-sectioning))))
 '(org-export-latex-title-command "")
 '(org-time-stamp-custom-formats (quote ("<%m/%d/%y %a>" . "<%H:%M>")))
 '(smime-keys (quote (("marcel@hsdev.com" "~/keys/thawte-email-marcel@hsdev.com.pem" nil))))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#303030" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "microsoft" :family "Consolas"))))
 '(flyspell-duplicate ((t (:foreground "goldenrod" :underline t :weight bold))))
 '(flyspell-incorrect ((t (:foreground "tomato" :underline t :weight bold))))
 '(lazy-highlight ((t (:inherit isearch-lazy-highlight :background "dark slate gray"))))
 '(org-agenda-date ((t (:inherit org-agenda-structure :height 1.2))) t)
 '(org-agenda-date-today ((t (:foreground "white" :slant italic :weight bold :height 1.2))))
 '(org-column ((t (:strike-through nil :underline nil :slant normal :weight normal :height 98 :family "Consolas"))))
 '(org-date ((t (:height 0.85 :underline nil :foreground "light blue"))))
 '(org-document-info ((((class color) (background light)) (:inherit org-date))))
 '(org-document-title ((((class color) (background light)) (:foreground "light blue" :weight bold :height 1.44))))
 '(org-property-value ((t (:height 0.85))) t)
 '(org-sexp-date ((t (:height 0.85))))
 '(org-special-keyword ((t (:height 0.85)))))

