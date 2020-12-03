(in-package #:asdf-user)

(defsystem "advent-of-code-2020"
  :author "Svante v. Erichsen <svante.v.erichsen@web.de>"
  :license "public domain/CC0"
  :depends-on ("alexandria"
               "arrows"
               "cl-ppcre"
               "let-plus"
               "split-sequence")
  :serial t
  :components ((:file "base")
               (:file "1")))