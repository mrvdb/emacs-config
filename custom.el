;;;; Custom file generated by emacs: best not to edit
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3440" "#88c0d0" "#bf616a" "#5e81ac" "#ebcb8b" "#a3be8c" "#ebcb8b" "#e5e9f0"])
 '(command-frequency-autosave-mode t)
 '(compilation-scroll-output t)
 '(custom-safe-themes t)
 '(dired-bind-jump nil)
 '(epg-gpg-program "/usr/bin/gpg2")
 '(eshell-modules-list
   (quote
    (eshell-alias eshell-banner eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-rebind eshell-script eshell-term eshell-unix)))
 '(explicit-shell-file-name "/bin/bash")
 '(flyspell-issue-message-flag nil)
 '(font-lock-maximum-size nil)
 '(goto-address-url-mouse-face (quote default))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(guess-language-languages (quote (en de nl)))
 '(haskell-interactive-popup-errors nil)
 '(ido-vertical-define-keys (quote C-n-C-p-up-down-left-right))
 '(ispell-use-framepop-p t)
 '(mail-signature nil)
 '(markdown-css-path "/home/mrb/.markdown.css")
 '(mm-text-html-renderer (quote shr))
 '(nxml-heading-element-name-regexp "\\|.*")
 '(nxml-section-element-name-regexp "\\|file\\|.+")
 '(ocpf-frame-parameters
   (quote
    ((name . "org-capture-pop-frame")
     (width . 115)
     (height . 15)
     (tool-bar-lines . 0)
     (menu-bar-lines . 0))))
 '(org-M-RET-may-split-line (quote ((default . t) (headline))))
 '(org-agenda-custom-commands
   (quote
    (("w" "Waiting For list" tags-todo "-inactive/WAITING"
      ((org-agenda-overriding-header "WAITING FOR-list")
       (org-agenda-dim-blocked-tasks t)
       (org-agenda-group-by-property "Responsible")))
     ("b" "Buying list"
      ((tags-todo "-inactive+buy/TODO"
		  ((org-agenda-overriding-header "Buying list (tagged)")))
       (tags-todo "-inactive/BUY"
		  ((org-agenda-overriding-header "Buying list (keyword)"))))
      nil)
     ("p" "Active project list" tags-todo "-ignore-inactive+LEVEL>1-TODO=\"DONE\"-TODO=\"CANCELLED\""
      ((org-agenda-overriding-header "Active project list")
       (org-agenda-skip-function
	(quote mrb/skip-non-projects))
       (org-agenda-dim-blocked-tasks nil)
       (org-agenda-group-by-property "Group")
       (org-agenda-sorting-strategy
	(quote
	 (alpha-up)))))
     ("A" "Active task list" tags-todo "+SCHEDULED=\"\"-inactive/TODO"
      ((org-agenda-group-by-property "Group")
       (org-agenda-dim-blocked-tasks
	(quote invisible))))
     ("r" "To Review"
      ((tags-todo "SCHEDULED=\"\"+DEADLINE=\"\"-{.}/TODO"
		  ((org-agenda-overriding-header "Untagged items")))
       (tags-todo "-inactive+SCHEDULED=\"\"+DEADLINE=\"\"+TODO=\"TODO\"+{.}"
		  ((org-agenda-overriding-header "Unscheduled active items"))))
      ((org-agenda-dim-blocked-tasks
	(quote invisible))))
     ("S" "Someday/Maybe List"
      ((tags "+inactive"
	     ((org-agenda-overriding-header "Inactive projects")
	      (org-agenda-skip-function
	       (quote mrb/skip-non-projects))))
       (tags-todo "+inactive-BLOCKED=\"t\"/TODO"
		  ((org-agenda-overriding-header "Inactive TODO items"))))
      nil nil)
     ("c" "Scheduled overview" tags-todo "SCHEDULED<>\"\"|DEADLINE<>\"\"/TODO"
      ((org-agenda-overriding-header "SCHEDULED")
       (org-agenda-view-columns-initially t)
       (org-agenda-overriding-columns-format "%65ITEM %25Responsible %SCHEDULED %DEADLINE %TAGS")
       (org-agenda-dim-blocked-tasks t)))
     ("l" "Blocked projects and tasks"
      ((tags-todo "+BLOCKED=\"t\"/PROJ"
		  ((org-agenda-overriding-header "Blocked projects")
		   (org-agenda-dim-blocked-tasks t)))
       (tags-todo "+BLOCKED=\"t\"/TODO"
		  ((org-agenda-overriding-header "Blocked tasks")
		   (org-agenda-dim-blocked-tasks t)
		   (org-agenda-group-by-property "Group"))))
      nil nil)
     ("n" "Next Action List [hides blocked/inactive/waiting/INBOX-ed]" tags-todo "+SCHEDULED=\"\"+DEADLINE=\"\"-BLOCKED=\"t\"-inactive-habit-ARCHIVE/-WAITING-INFO-HOWTO"
      ((org-agenda-overriding-header "Next Action List")
       (org-agenda-dim-blocked-tasks
	(quote invisible))
       (org-agenda-group-by-property "CREATED")))
     ("x" "List of stuck projects (debug)" tags "-inactive+LEVEL>1-TODO=\"DONE\"-TODO=\"CANCELLED\""
      ((org-agenda-skip-function
	(quote mrb/skip-non-stuck-projects))
       (org-agenda-overriding-header "List of STUCK projects")))
     ("D" "Items ready for archiving" todo "DONE"
      ((org-agenda-overriding-header "Items ready for archiving")
       (org-agenda-group-by-property "CREATED")))
     ("g" "AGENDA"
      ((agenda ""
	       ((org-agenda-filter-preset
		 (quote
		  ("-inactive")))
		(org-agenda-span
		 (quote day))
		(org-agenda-overriding-header "")))
       (tags-todo "carryover"
		  ((org-agenda-overriding-header "Carry along list"))))
      ((org-agenda-archives-mode t)))
     ("$" "Expected revenue" tags "Effort<>\"\""
      ((org-agenda-overriding-columns-format "%40ITEM %10Effort")
       (org-agenda-sorting-strategy
	(quote
	 (effort-down))))))))
 '(org-agenda-ignore-properties (quote (effort appt category)))
 '(org-agenda-text-search-extra-files
   (quote
    (agenda-archives "~/dat/org/_orgmeta/archive-2016.org" "~/dat/org/_orgmeta/archive-2015.org" "~/dat/org/_orgmeta/archive-2014.org" "~/dat/org/_orgmeta/archive-2013.org" "~/dat/org/_orgmeta/archive-2012.org" "~/dat/org/_orgmeta/archive-2011.org")))
 '(org-agenda-todo-ignore-scheduled (quote future))
 '(org-beamer-outline-frame-options nil)
 '(org-beamer-outline-frame-title "Onderwerpen")
 '(org-blocker-ignore-ancestor-siblings t)
 '(org-clock-x11idle-program-name "xprintidle")
 '(org-closed-keep-when-no-todo t)
 '(org-ditaa-jar-path "/home/mrb/bin/ditaa.jar")
 '(org-entities-user (quote (("cmd" "\\cmd{}" nil "⌘" "⌘" "⌘" "⌘"))))
 '(org-export-backends (quote (ascii html icalendar latex md odt org texinfo)))
 '(org-export-copy-to-kill-ring (quote if-interactive))
 '(org-export-docbook-xsl-fo-proc-command "fop %i %o" t)
 '(org-export-docbook-xslt-proc-command "xsltproc --output %o %s %i" t)
 '(org-export-htmlize-output-type (quote css) t)
 '(org-export-latex-classes
   (quote
    (("article" "\\documentclass[11pt,a4paper,twoside]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("beamer" "\\documentclass{beamer}" org-beamer-sectioning))) t)
 '(org-export-latex-hyperref-format "\\ref{%s}:{%s}" t)
 '(org-export-latex-title-command " " t)
 '(org-export-with-tags nil)
 '(org-export-with-todo-keywords nil)
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.dia\\'" . "dia %s")
     ("\\.mm\\'" . default)
     ("\\.pdf\\'" . emacs))))
 '(org-html-toplevel-hlevel 3)
 '(org-insert-heading-respect-content nil)
 '(org-latex-default-packages-alist
   (quote
    (("QX" "fontenc" t)
     ("" "lmodern" t)
     ("AUTO" "inputenc" t)
     ("" "fixltx2e" nil)
     ("" "graphicx" t)
     ("" "longtable" nil)
     ("" "float" nil)
     ("" "wrapfig" nil)
     ("" "soul" nil)
     ("" "textcomp" t)
     ("" "marvosym" t)
     ("" "wasysym" t)
     ("" "latexsym" t)
     ("" "amssymb" t)
     ("" "amstext" nil)
     ("hidelinks" "hyperref" nil)
     "\\tolerance=1000")))
 '(org-latex-pdf-process
   (quote
    ("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))
 '(org-latex-title-command " ")
 '(org-latex-to-pdf-process
   (quote
    ("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")) t)
 '(org-list-allow-alphabetical t)
 '(org-mime-use-property-inheritance t)
 '(org-org-htmlized-css-url "orgmode.css")
 '(org-plantuml-jar-path
   "/home/mrb/dat/src/emacs/packages/org-mode/contrib/scripts/plantuml.jar")
 '(org-stuck-projects (quote ("-inactive/+TODO" ("TODO" "WAITING") nil "")))
 '(org-tags-column -110)
 '(org-tags-exclude-from-inheritance (quote ("area" "encrypt")))
 '(org-time-stamp-custom-formats (quote ("<%m/%d/%y %a>" . "<%H:%M>")))
 '(org-todo-state-tags-triggers
   (quote
    (("TODO"
      ("inactive"))
     ("DONE"
      ("inactive")
      ("fork"))
     ("BUY"
      ("buy" . t)))))
 '(org-use-fast-tag-selection (quote auto))
 '(pdf-misc-print-programm "/usr/bin/lpr")
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(reb-re-syntax (quote string))
 '(safe-local-variable-values
   (quote
    ((TeX-master . t)
     (hamlet/basic-offset . 4)
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)
     (encoding . utf-8)
     (buffer-auto-save-file-name))))
 '(scad-keywords (quote ("return" "true" "false" "include")))
 '(sgml-xml-mode t)
 '(shr-bullet "• ")
 '(shr-use-colors nil)
 '(shr-width 72)
 '(sieve-manage-authenticators (quote (plain digest-md5 cram-md5 scram-md5 ntlm login)))
 '(sql-postgres-options (quote ("-P" "pager=off" "-p 5434")))
 '(sql-server "dbserver.hsdev.com")
 '(twittering-status-format
   "%i %s, %RT{(%FACE[bold]{RT} by %s)} %@:
%FOLD[  ]{%T |%L%r%QT{
┌────
%FOLD[|]{%i %s, %RT{(%FACE[bold]{RT} by %s)} %@:
%FOLD[  ]{%T |%L%r}}
└────}}
 ")
 '(warning-suppress-types (quote ((undo))))
 '(x-select-enable-clipboard-manager nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :slant normal :weight normal :height 113 :width normal))))
 '(fixed-pitch ((t (:family "DefjaVu Sans Mono")))))
