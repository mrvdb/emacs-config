;;; tumble-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (tumble-video-from-url tumble-audio tumble-photo-from-file
;;;;;;  tumble-photo-from-url tumble-chat-from-buffer tumble-chat-from-region
;;;;;;  tumble-link tumble-link-with-description tumble-quote-from-region
;;;;;;  tumble-text-from-buffer tumble-text-from-region) "tumble"
;;;;;;  "tumble.el" (19289 34949))
;;; Generated autoloads from tumble.el

(autoload 'tumble-text-from-region "tumble" "\
Post the current region as a text in Tumblr

\(fn MIN MAX TITLE)" t nil)

(autoload 'tumble-text-from-buffer "tumble" "\
Post the current buffer as a text in Tumblr

\(fn TITLE)" t nil)

(autoload 'tumble-quote-from-region "tumble" "\
Post a region as a quote in Tumblr

\(fn MIN MAX SOURCE)" t nil)

(autoload 'tumble-link-with-description "tumble" "\
Posts a Tumblr link using the region as the description

\(fn MIN MAX NAME URL)" t nil)

(autoload 'tumble-link "tumble" "\
Posts a Tumblr link without description

\(fn NAME URL)" t nil)

(autoload 'tumble-chat-from-region "tumble" "\
Posts a chat to Tumblr using the current region

\(fn MIN MAX TITLE)" t nil)

(autoload 'tumble-chat-from-buffer "tumble" "\
Posts a chat to Tumblr using the current buffer

\(fn TITLE)" t nil)

(autoload 'tumble-photo-from-url "tumble" "\
Posts a photo to Tumblr using an URL as the source

\(fn SOURCE CAPTION URL)" t nil)

(autoload 'tumble-photo-from-file "tumble" "\
Posts a local photo to Tumblr

\(fn FILENAME CAPTION URL)" t nil)

(autoload 'tumble-audio "tumble" "\
Posts an audio file to Tumblr

\(fn FILENAME CAPTION)" t nil)

(autoload 'tumble-video-from-url "tumble" "\
Uses EMBED to post a video to Tumblr

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("tumble-pkg.el") (19289 34949 376232))

;;;***

(provide 'tumble-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tumble-autoloads.el ends here
