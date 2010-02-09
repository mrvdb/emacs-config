;;; tumble.el --- an Emacs mode for Tumblr
     
;; Copyright (C) 2008 Federico Builes
     
;; Author: Federico Builes <federico.builes@gmail.com>
;; Created: 1 Dec 2008
;; Version: 1.1
;; Keywords: tumblr
;; Package-Requires: ((http-post-simple "1.0"))
     
;; This file is NOT part of GNU Emacs.
       
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
          
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
          
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA
    
 
;;; Commentary:

;; Tumble is a mode for interacting with Tumblr inside Emacs. It currently
;; provides the following functions:
;;
;; tumble-text-from-region 
;;     Posts the selected region as a "Text".
;; tumble-text-from-buffer 
;;     Posts the current buffer as a "Text".

;; tumble-quote-from-region 
;;     Posts the current region as a "Quote". Prompts
;;     for an optional "source" parameter.

;; tumble-link 
;;     Prompts for a title and a URL for a new "Link".
;; tumble-link-with-description 
;;     Prompts for a title and a URL for a new "Link" and
;;     uses the selected region as the link's description.

;; tumble-chat-from-region 
;;     Posts the selected region as a "Chat".
;; tumble-chat-from-buffer 
;;     Posts the current buffer as a "Chat".

;; tumble-photo-from-url 
;;     Prompts for a file URL, a caption and a clickthrough and
;;     posts the result as a "Photo".
;; tumble-photo-from-file 
;;     Prompts for a local file, a caption and a clickthrough and
;;     posts the result as a Photo.
;; tumble-audio 
;;     Prompts for a local file and an optional caption to 
;;     upload a MP3 file.

;; tumble-video-from-url 
;;     Prompts for an embed code and an optional caption to post a video
;;     to Tumblr.

;; A word of caution: Audio files can take a while to upload and will 
;; probably freeze your Emacs until it finishes uploading.

;; You can always find the latest version of Tumble at: http://github.com/febuiles/tumble

;; Installation: 

;;
;; Download Tumble to some directory:
;; $ git clone git://github.com/febuiles/tumble.git
;;
;; Add it to your load list and require it:
;;
;; (add-to-list 'load-path "~/some_directory/tumble.el")
;; (require 'tumble)
;;
;; Open tumble.el (this file) and modify the following variables:
;; (setq tumble-email "your_email@something.com")
;; (setq tumble-password "your_password")
;; (setq tumble-url "your_tumblelog.tumblr.com")
;; 
;; Tumble uses no group for posting and Markdown as the default 
;; format but you can change these:
;; (setq tumble-group "your_group.tumblr.com")
;; (setq tumble-format "html")

;;; Code:

(require 'http-post-simple)

;; Personal information
(setq tumble-email "your_email@something.com")
(setq tumble-password "your_password")
(setq tumble-url "your_tumblelog.tumblr.com")
;; Optional information
(setq tumble-group "")                  ; uncomment to use a group.
(setq tumble-format "markdown")         ; you can change this to html

;;;###autoload
(defun tumble-text-from-region (min max title)
  "Post the current region as a text in Tumblr"
  (interactive "r \nsTitle: ")
  (tumble-post-text title (tumble-region-text)))

;;;###autoload
(defun tumble-text-from-buffer (title)
  "Post the current buffer as a text in Tumblr"
  (interactive "sTitle: ")
  (tumble-text-from-region (point-min) (point-max) title))

;;;###autoload
(defun tumble-quote-from-region (min max source)
  "Post a region as a quote in Tumblr"
  (interactive "r \nsSource (optional): " )
  (tumble-http-post
   (list (cons 'type "quote")
         (cons 'quote (tumble-region-text))
         (cons 'source source))))

;;;###autoload
(defun tumble-link-with-description (min max name url)
  "Posts a Tumblr link using the region as the description"
  (interactive "r \nsName (optional): \nsLink: ")
  (tumble-post-link name url (tumble-region-text)))

;;;###autoload
(defun tumble-link (name url)
  "Posts a Tumblr link without description"
  (interactive "sName (optional): \nsLink: ")
  (tumble-post-link name url ""))

;;;###autoload
(defun tumble-chat-from-region (min max title)
  "Posts a chat to Tumblr using the current region"
  (interactive "r \nsTitle (optional): ")
  (tumble-post-chat title (tumble-region-text)))

;;;###autoload
(defun tumble-chat-from-buffer (title)
  "Posts a chat to Tumblr using the current buffer"
  (interactive "sTitle (optional): ")
  (tumble-chat-from-region (point-min) (point-max) title))

;;;###autoload
(defun tumble-photo-from-url (source caption url)
  "Posts a photo to Tumblr using an URL as the source"
  (interactive "sURL: \nsCaption (optional): \nsLink (optional): ")
  (tumble-post-photo source caption url))

;;;###autoload
(defun tumble-photo-from-file (filename caption url)
  "Posts a local photo to Tumblr"
  (interactive "fPhoto: \nsCaption (optional): \nsLink (optional): ")
  (let* ((file-format  (format "image/%s" (file-name-extension filename))) ; hack
         (data (tumble-file-data filename))
         (request (list (cons 'type "photo")
                        (cons 'caption caption)
                        (cons 'click-through-url url))))
    (tumble-multipart-http-post request 
                                filename
                                file-format
                                data)))

;;;###autoload
(defun tumble-audio (filename caption)
  "Posts an audio file to Tumblr"
  (interactive "fAudio: \nsCaption (optional): ")
  (let* ((data (tumble-file-data filename))
         (request (list (cons 'type "audio")
                        (cons 'caption caption))))
    (tumble-multipart-http-post request 
                                filename 
                                "audio/mpeg" 
                                data)))

;;;###autoload
(defun tumble-video-from-url ()
  "Uses EMBED to post a video to Tumblr"
  (interactive)
  ;; interactive dies with unquoted strings such as YouTube embed codes
  ;; so we call (read-string) directly.
  (let* ((embed (read-string "Source (embed): "))
         (caption (read-string "Caption (optional): ")))
    (tumble-post-video embed caption)))

(defun tumble-post-text (title body)
  "Posts a new text to a tumblelog" 
  (tumble-http-post 
   (list (cons 'type "regular")
         (cons 'title title)
         (cons 'body body))))

(defun tumble-post-chat (title chat)
  "Posts a new chat to a tumblelog"
  (tumble-http-post
   (list (cons 'type "conversation")
         (cons 'title title)
         (cons 'conversation chat))))

(defun tumble-post-link (name url description)
  "Posts a link to a tumblelog"
  (tumble-http-post
   (list (cons 'type "link")
         (cons 'name name)
         (cons 'url url)
         (cons 'description description))))

(defun tumble-post-photo (source caption url)
  "Posts a photo to a tumblelog"
  (tumble-http-post
   (list (cons 'type "photo")
         (cons 'source source)
         (cons 'caption caption)
         (cons 'click-through-url url))))

(defun tumble-post-video (embed caption)
  "Embeds a video in a tumblelog" 
  (tumble-http-post
   (list (cons 'type "video")
         (cons 'embed embed)
         (cons 'caption caption))))

(defun tumble-default-headers ()
  "Generic Tumblr headers"
  (list (cons 'email tumble-email) 
        (cons 'password tumble-password)
        (cons 'format tumble-format)
        (cons 'generator "tumble.el")
        (cons 'group tumble-group)))

(defun tumble-http-post (request)
  "Send the POST to Tumblr"
  (let* ((resp (http-post-simple "http://www.tumblr.com/api/write" 
                                 (append (tumble-default-headers) request))))
    (tumble-process-response resp)))

(defun tumble-multipart-http-post (request filename mime data)
  "Multipart POST used to upload files to Tumblr"
  (let* ((resp (http-post-simple-multipart "http://www.tumblr.com/api/write" 
                                           (append (tumble-default-headers)
                                                   request)
                                           (list (list "data" filename mime data)))))
    (tumble-process-response resp)))

;; RESPONSE is a simple http response list with (url response code)
(defun tumble-process-response (response)
  "Returns a message based on the response code"
  (let* ((code (third response)))       ; 
    (message
     (cond ((eq code 200) "No post created")
           ((eq code 201) 
            (tumble-paste-url (car response)) 
            "Post created" )
           ((eq code 400) "Bad Request")
           ((eq code 403) "Authentication Failed")
           (t "Unknown Response")))))

(defun tumble-paste-url (id)
  "Adds the response URL to the kill ring"
  (let* ((last-char (substring tumble-url -1)))
    (cond ((string= last-char "/") 
           (kill-new (concat tumble-url id))) ; url has a trailing slash
          (t (kill-new (concat tumble-url (concat "/" id)))))))

(defun tumble-region-text() 
  "Returns the text of the region inside an (interactive 'r') function"
  (buffer-substring-no-properties min max))

(defun tumble-file-data (filename)
  "Reads filename and returns the data"
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (buffer-substring-no-properties (point-min)
                                    (point-max))))

(provide 'tumble)
;;; tumble.el ends here
