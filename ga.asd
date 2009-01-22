(in-package :cl-user)

(defpackage :ga
  (:use :cl :asdf)
  (:export :polygon-evolution))

(in-package :ga)

;;This creates an ASDF package. You can then simply add a symlink from this file
;;to your asdf:*LOCATIONS* directory (e.g., ~/.asdf-install-dir/site/"). 
;;Then, use "(asdf:operate 'asdf:load-op :ga)" to automatically load this, and 
;;all dependencies.

(asdf:defsystem :ga
  :version "0.2"
  :author "Shaneal Manek (smanek@gmail.com)"
  :serial t
  :components
  ((:file "global") ;;global variables and settings
   (:file "misc") ;;misc utility functions
   (:file "weighted-list") ;;set of functions for dealing with lists weighted by probability
   (:file "color") ;;color class, and manipulation methods
   (:file "bitmap") ;;bitmap manipulation, file i/o
   (:file "threading") ;;detects if you are using SBCL with multithreading support, and if so enables it.
   (:file "polygon") ;;a simple polygon/edge class, along with some methods to draw on a bitmap
   (:file "chromosome") ;;a generic chromosome class, which all other chromosomes must derive from
   (:file "polygon-chromosome") ;;a chromosome sub-type, of images of polygons
   (:file "evolve"))) ;;actually run the evolution, keep track of population