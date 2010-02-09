;; metaweblog.el -- an emacs library to access metaweblog based weblogs
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

(require 'xml-rpc)

(defun metaweblog-get-categories(blog-xmlrpc user-name password blog-id)
  "Retrieves list of categories from the weblog system"
  (xml-rpc-method-call blog-xmlrpc
		       "metaWeblog.getCategories"
		       blog-id
		       user-name
		       password))

(defun metaweblog-new-post(blog-xmlrpc user-name password blog-id content publish)
  "Sends a new post to the blog. If PUBLISH is non-nil, the post is
published, otherwise it is saved as draft. CONTENT will be an alist
title, description, categories, and date as keys (string-ified) mapped to the
title of the post, post contents, list of categories, and date respectively." 
  (let ((post-title (cdr (assoc "title" content)))
	(post-description (cdr (assoc "description" content)))
	(post-categories (cdr (assoc "categories" content)))
	(post-date (cdr (assoc "date" content))))
  ;;; since xml-rpc-method-call entitifies the HTML text in the post
  ;;; we've to use raw
  (xml-rpc-xml-to-response (xml-rpc-request
   blog-xmlrpc
   `((methodCall
      nil
      (methodName nil "metaWeblog.newPost") 
      (params nil 
	      (param nil (value nil (string nil ,blog-id)))
	      (param nil (value nil (string nil ,user-name)))
	      (param nil (value nil (string nil ,password)))
	      (param nil (value nil
				(struct
				 nil
				 (member nil
					 (name nil "title")
					 (value nil ,post-title))
				 (member nil
					 (name nil "description")
					 (value nil ,post-description))
				 (member nil
					 (name nil "dateCreated")
					 (value nil ,post-date))
				 ,(when post-categories
				    `(member nil 
					     (name nil "categories")
					     (value nil
						    (array
						     nil
						     ,(append 
						       '(data nil)
						       (mapcar
							(lambda(f)
							  `(value nil (string nil ,f)))
							post-categories)))))))))
	      (param nil (value nil (boolean nil ,(if publish "1" "0")))))))))))

(defun metaweblog-get-post(blog-xmlrpc user-name password post-id)
  "Retrieves a post from the weblog. POST-ID is the id of the post
which is to be returned"
  (xml-rpc-method-call blog-xmlrpc
		       "metaWeblog.getPost"
		       post-id
		       user-name
		       password))

(defun metaweblog-get-recent-posts(blog-xmlrpc blog-id user-name password number-of-posts)
  "Fetches the recent posts from the weblog. NUMBER-OF-POSTS is the
no. of posts that should be returned."
  (xml-rpc-method-call blog-xmlrpc
		       "metaWeblog.getRecentPosts"
		       blog-id
		       user-name
		       password
		       number-of-posts))

(provide 'metaweblog)