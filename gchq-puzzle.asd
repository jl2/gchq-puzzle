;;;; gchq-puzzle.asd
;;;;
;;;; Copyright (c) 2015 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:gchq-puzzle
  :description "Describe gchq-puzzle here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:cl-cairo2)
  :serial t
  :components ((:file "package")
               (:file "gchq-puzzle")))

