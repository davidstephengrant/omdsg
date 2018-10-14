;;;; omdsg - A library of the author's tools for OpenMusic
;;;; Copyright (C) 2018  David Stephen Grant
;;;
;;;  This file is part of omdsg.
;;; 
;;;  omdsg is free software: you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation, either version 3 of the License, or
;;;  any later version.
;;; 
;;;  omdsg is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;; 
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program.  If not, see <https://www.gnu.org/licenses/>.

(defvar dsg)

(defpackage dsg)

(in-package :om)

(defvar dsg::*lib* (find-library "omdsg"))

;;; Load sources
(defvar dsg::*srcfiles* nil)

(setf dsg::*srcfiles*
      (list
       (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "chord-utils" :type "lisp")
       ;; Unit tests
       (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "unittests")) :name "unittest" :type "lisp")
       (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "unittests")) :name "test-chord-utils" :type "lisp")
       ))

(mapc #'compile&load dsg::*srcfiles*)

;;; Setup menu structure and (sub) packages
;; Syntax: ("sub package name" subpackages-list class-list function-list class-alias-list)
(fill-library 
 '(("Chord Utilities" nil nil (dsg::split-chords dsg::remove-unisons dsg::realize-offsets) nil)
   ("Unit Tests" nil nil (dsg-test::run-unittests) nil)
   ))

;;; Dependencies
;(require-library "omdsg")

;;; Documentation
(set-lib-release 0.01 dsg::*lib*)

;; Lib docstring for {$RESOURCES}/reference/index.html
(doc-library (def-lib-doc-string dsg::*lib*) dsg::*lib*)

;;; Generate reference documentation
;;; Should be commented out before commit to repo
(gen-lib-reference dsg::*lib*)

(print "
 ==============================
 omdsg
 ==============================
 A library of the author's tools for OpenMusic
 Copyright (C) 2018  David Stephen Grant
 ==============================
")