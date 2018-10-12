;;; omdsg - A library of the author's tools for OpenMusic
;;; Copyright (C) 2018  David Stephen Grant

(defvar dsg)

(defpackage dsg)

(in-package :dsg)

;; Load sources

(defvar *omdsg-files* nil)

(setf *omdsg-files*
      (list
       (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "chord-utils" :type "lisp")
       ))

(mapc #'om::compile&load *omdsg-files*)

;; Setup menu structure and (sub) packages

;  Syntax: ("sub package name" subpackages-list class-list function-list class-alias-list)
(om::fill-library 
 '(("Chord Utilities" nil nil (split-chords remove-unisons realize-offsets) nil)
   ))

;; Dependencies

;(require-library "omdsg")