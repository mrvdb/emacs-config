;;; org-action-verbs.el --- Highlight potentially un-doable headlines.

;; Copyright (C) 2008 Tim O'Callaghan

;; Author: Tim O'Callaghan <timo@dspsrv.com>
;; Version: 0.1

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is an implementation of an idea I had to keep me honest. In
;; GTD each 'next action' is supposed to be an actual doable thing to
;; further the project it is associated with.

;; When creating next actions or project headlines there is sometimes
;; a tendency to use a heading that is a bit vague/undoable. This is
;; usually a sign that the task the headline represents needs more
;; thinking about and will probably need more than one next action or
;; spawn another project.

;; org-action-verbs was created to use a list of GTD 'action verbs' to
;; diagnose non-functional next action and project headlines. It searches
;; for headlines of a specific TODO type and checks to see if the first
;; word in the headline is specified as an action verb for that TODO
;; type. If not, then it highlights that first non actionable word. 

;; Its a bit like flyspell mode but for checking doable org headlines.

;; To use put the following in your .emacs:
;; (require 'org-action-verbs)

;; To change the default TODO Type->Action Verb table you can set
;; 'org-action-todo-verbs'. Below is an example. The first checks for the
;; right spaceship name associated with the 'SPACESHIP' TODO type, and
;; the right colour for the 'COLOR' and 'COLOUR' TODO types.  

;;(setq org-action-todo-verbs
;;   '(
;;     (("SPACESHIP") . ("Challenger" "Voyager" "Enterprise" "Nostromo" "Apollo" ))
;;     (("COLOUR" "COLOR") . ("Red" "Yellow" "Green" "Aquamarine" "Blue" "Black"))
;;     )
;;

(require 'org)

(defvar org-action-todo-verbs
  '(
    (("TODO" "NEXT") . 
     ("Address" "Ask" "Avoid" "Buy" "Change" "Clarify" "Collect" "Commend" "Confront"
      "Consider" "Create" "Decide" "Defer" "Develop" "Discard" "Do Again" "Download"
      "Enter" "File" "Fix" "Follow Up" "Hire" "Improve" "Increase" "Inform" "Inquire"
      "Maintain" "Measure" "Monitor" "Order" "Paint" "Phone" "Prioritize" "Purchase"
      "Question " "Reduce" "Remember" "Repair" "Reply" "Report" "Re-Do" "Research" "Resolve"
      "Review" "Schedule" "Sell" "Send" "Service" "Specify" "Start" "Stop" "Suggest"
      "Tidy" "Train" "Update" "Upgrade" "Write"
      "Call" "Email" "Fix" "Find" "Fill out" "Give" "Print" "Re-Do" "Take"))
    (("PROJECT") . 
     ("Finalize" "Resolve" "Handle" "Look-Into" "Submit" "Maximize" "Organize"
      "Design" "Complete" "Ensure" "Research" "Roll-Out" "Update" "Install"
      "Implement" "Set-Up" "Configure" "Draft" "Purge" "Gather"))
    )
  "org-action todo keywords to apply to incorrect action verb overlay to.")

(defface org-action-incorrect-face
  '((((class color) (background light)) (:foreground "orange" :bold t :underline t))
    (((class color) (background dark)) (:foreground "orange" :bold t :underline t))
    (t (:bold t :underline t)))
  "Used by org-action-verbs to help mark bad 'un-doable' headlines.")

;; backward-compatibility alias
(put 'org-action-incorrect-face 'face-alias 'org-action-incorrect)

(defun org-font-lock-add-action-faces (limit)
  "Add the special action word faces."
  (let (rtn a)
    ;; check variable is set, and buffer left to search
    (when (and (not rtn) org-action-todo-verbs)
      ;; for each todo/action verb set
      (dolist (todo org-action-todo-verbs)
        ;; build regexps
        (let ((todo-keywords-regexp
               (concat "^\\*+[ 	]+" (regexp-opt (car todo) 'words)))
              (todo-action-verbs-regexp
               (concat "[ 	]+" (regexp-opt (cdr todo) 'words))))
          ;; while we can find a todo keyword
          (while (re-search-forward todo-keywords-regexp limit t)
            (progn
              ;; if found remove any overlay from ok word and rest of line
              (if (looking-at "\\(.*\\)$")
                  (remove-overlays (match-beginning 1) (match-end 1) 'org-action-overlay t))
              ;; check for action verb
              (if (looking-at todo-action-verbs-regexp)
                  nil
                ;; not an action verb, reset match data to next word
                (if (looking-at "[ 	]+\\(\\<\\w+\\>\\)")
                    ;; apply new overlay
                    (let ((overlay (make-overlay (match-beginning 1) (match-end 1) nil t nil)))
                      (overlay-put overlay 'org-action-overlay t)
                      (overlay-put overlay 'face 'org-action-incorrect)
                      (overlay-put overlay 'evaporate t)
                      overlay))))
            ;; reset search point?
            (backward-char 1)))))
    rtn))

(defun org-mode-action-verbs-hook ()
  "Initalise org-action-verbs."
  (interactive) 
  (font-lock-add-keywords nil '((org-font-lock-add-action-faces))))

;; Turn on action verb font locking.
(add-hook 'org-mode-hook 'org-mode-action-verbs-hook)

(provide 'org-action-verbs)
;;; org-annotate-file.el ends here
