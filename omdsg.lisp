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