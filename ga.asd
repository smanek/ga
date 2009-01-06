(in-package :cl-user)

(defpackage :ga
  (:use :cl :asdf)
  (:export :solution))

(in-package :ga)

;;This creates an ASDF package. You can then simply add a symlink from this file
;;to your asdf:*LOCATIONS* directory (e.g., ~/.asdf-install-dir/site/"). 
;;Then, use "(asdf:operate 'asdf:load-op :ga)" to automatically load this, and 
;;all dependencies.

(asdf:defsystem :ga
  :version "0.1"
  :author "Shaneal Manek (smanek@gmail.com)"
  :serial t
  :components
  ((:file "global") ;;global variables and settings
   (:file "misc") ;;misc utility functions
   (:file "weighted-list") ;;set of functions for dealing with lists weighted by probability
   (:file "color") ;;color manipulation
   (:file "bitmap") ;;bitmap manipulation, i/o
   (:file "polygon") ;;a simple polygon class
   (:file "chromosome") ;;a chromosome class
   (:file "evolve"))) ;;actually run the evolution