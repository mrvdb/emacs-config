;; blog.el -- a wordpress posting client
;; Copyright (C) 2008 Ashish Shukla

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

(require 'muse-mode)
(require 'muse-html)
(require 'metaweblog)

(defgroup blog nil "Post to weblogs from Emacs" :group 'emacs)

(defcustom blog-server-url nil "Weblog XML-RPC URL" :group 'blog :type 'string)
(defcustom blog-server-user nil "Weblog server username" :group 'blog :type 'string)
(defcustom blog-server-pass nil "Weblog server password. If this is
nil you'll be prompted." :group 'blog :type 'string) 
(defcustom blog-server-weblog-id nil "Weblog ID" :group 'blog :type 'string)
(defcustom blog-default-categories '("Uncategorized") "Default list of categories" :group 'blog :type '(repeat string))
(defcustom blog-default-title "Hello, World" "Title of the new post" :group 'blog :type 'string)

(defvar blog-categories-list nil "List of weblog categories")
(defvar blog-server-xmlrpc-url nil "Weblog server XML-RPC URL")
(defvar blog-server-userid nil "Weblog server user id")
(defvar blog-server-blogid nil "Weblog ID")
(defvar blog-entry-mode-map nil "Keymap for blog entry buffer")
(defvar blog-logged-in nil "Flag whether user is logged-in or not")
(defvar blog-buffer-name "*blog*" "Name of the blog buffer")
(defconst blog-version "0.1" "Current version of blog.el")

(unless blog-entry-mode-map
  (setq blog-entry-mode-map
	(let ((blog-map (make-sparse-keymap)))
	  (set-keymap-parent blog-map muse-mode-map)
	  (define-key blog-map (kbd "C-c C-c") (lambda() (interactive) (blog-post-entry t)))
	  (define-key blog-map "\t"   'blog-complete-category)
	  (define-key blog-map (kbd "<tab>")   'blog-complete-category)
	  (define-key blog-map (kbd "C-c C-s") 'blog-post-entry)
	  blog-map)))

(defun blog-login()
  "Logs into the blog. Initializes the internal data structures."
  (interactive)
  (setq blog-server-xmlrpc-url (or blog-server-url
				   (read-no-blanks-input "Weblog XML-RPC URL ? ")))
  (setq blog-server-userid (or blog-server-user
			       (read-no-blanks-input "Weblog User ID ? ")))
  (setq blog-server-blogid (or blog-server-weblog-id
			       (read-no-blanks-input "Weblog ID ? ")))
  (setq blog-categories-list
	(mapcar (lambda (category) (cdr (assoc "categoryName" category)))
		(metaweblog-get-categories blog-server-xmlrpc-url
					   blog-server-userid
					   (or blog-server-pass
					       (read-passwd "Weblog Password ? "))
					   blog-server-weblog-id)))
  (setq blog-logged-in t))

(defun blog-logout()
  "Logs out from the blog and clears. Clears the internal data structures."
  (interactive)
  (setq blog-server-xmlrpc-url nil
	blog-server-userid nil
	blog-server-blogid nil
	blog-categories-list nil
	blog-logged-in nil))

(defun blog-new-entry()
  "Creates a new blog entry"
  (interactive)
  (unless blog-logged-in
    (error "Please log-in to the blog first"))
  (let ((blog-buffer (generate-new-buffer blog-buffer-name)))
    (switch-to-buffer blog-buffer)
    (muse-mode)
    (mapc
     (lambda (header)
       (let ((p1 (point))
	     (p2 0))
	 (insert (format "**%s**: %s"
			 (car header)
			 (if (stringp (cdr header))
			     (cdr header)
			   (let* (cats not-first-element)
			     (dolist (val (cdr header) cats)
			       (setq cats
				     (concat
				      val
				      (if not-first-element
					", " (progn (setq not-first-element t) ""))
				      cats))))))) (newline)))
     `(("Date" . ,(format-time-string "%Y-%m-%dT%T%z" (current-time)))
       ("Subject" . ,(or blog-default-title ""))
       ("Categories" .  ,blog-default-categories)))
    (newline)
    (insert "--[post] Type your post below this line [post]--")
    (newline)

    (use-local-map blog-entry-mode-map)))

(defun blog-post-entry(&optional publish)
  "Posts blog entry to the blog. If PUBLISH is not-nil, then publishes entry"
  (interactive)
  (unless blog-logged-in (error "Please log-in to the blog first"))
  (let* (r1 r2 html-text list-headers)
    (save-excursion
      (setq list-headers '())
      (goto-char (point-min))
      (dolist (item '("Date" "Subject" "Categories"))
	(when (looking-at (concat "**" item))
	  (add-to-list 'list-headers (cons item (buffer-substring-no-properties 
						(search-forward ": ")
						(point-at-eol))))
	  (forward-line)))
      (setcdr (assoc "Categories" list-headers)
	      (split-string (cdr (assoc "Categories" list-headers)) ", " t))
      (search-forward "--[post] Type your post below this line [post]--")
      (forward-line)
      (setq r1 (point))
      (setq r2 (point-max))
      (end-of-line)
      (muse-publish-region r1 r2 (cdr (assoc "Subject" list-headers)) (muse-style "xhtml"))
      (search-forward "<!-- Page published by Emacs Muse begins here -->")
      (forward-line)
      (setq r1 (point))
      (search-forward "<!-- Page published by Emacs Muse ends here -->")
      (forward-line -1)
      (end-of-line)
      (setq r2 (point))
      (setq html-text (buffer-substring-no-properties r1 r2))
      (kill-buffer))
      (goto-char (point-max))
      (metaweblog-new-post blog-server-xmlrpc-url
			   blog-server-userid
			   (or blog-server-pass
			       (read-passwd "Weblog Password ? "))
			   blog-server-blogid
			   `(("description" . ,html-text)
			     ("title" . ,(cdr (assoc "Subject" list-headers)))
			     ("categories" . ,(cdr (assoc "Categories" list-headers))))
			   publish)
      (kill-buffer)))

(defun blog-complete-category()
  (interactive)
  (let* (current-pos)
    (setq current-pos (point))
    (forward-line 0)
    (if (looking-at "**Categories")
	(progn
	  (goto-char current-pos)
	  (let ((word-match (or (current-word t) ""))
		(completion-match nil))
	    (when word-match
	      (setq completion-match (completing-read "Category ? " blog-categories-list nil nil word-match))
	      (when (stringp completion-match)
		(search-backward word-match nil t)
	      (replace-match (concat completion-match ", ") nil t)))))
      (progn
	(goto-char current-pos)
	(command-execute (lookup-key muse-mode-map "\t"))))))

(provide 'blog)