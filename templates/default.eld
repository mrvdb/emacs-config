org-mode

(src "#+begin_src " p n> r> n> "#+end_src" :post (org-edit-src-code))
(elsp "#+begin_src emacs-lisp" n> r> n "#+end_src" :post (org-edit-src-code))
