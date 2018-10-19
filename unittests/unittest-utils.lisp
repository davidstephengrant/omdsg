;=========================================================================
;  omdsg - A library of the author's tools for OpenMusic
;  Copyright (C) 2018  David Stephen Grant
;
;    This file is part of omdsg.
;
;    omdsg is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    any later version.
;
;    omdsg is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with omdsg.  If not, see <https://www.gnu.org/licenses/>.
;=========================================================================

(in-package :om)

(defmacro dsg-test::deftest (label form)
  `(progn (defun ,label ()
            ,form)
     (dsg-test::add-unittest ',label #',label)))

(defun dsg-test::add-unittest (label test-function)
  (setf dsg-test::*unittests* (cons (cons label test-function) dsg-test::*unittests*)))

(defmethod! dsg::run-unittests ()
            :icon 800
            :doc "
Runs all unit tests registered for library omdsg.

Returns t if all tests passed or nil if any failed.
"
            (om-print "==============================
Running unit tests...
==============================" nil)
            (let* ((tests (reverse dsg-test::*unittests*))
                   (lbls (mapcar #'car tests))
                   (results (mapcar #'(lambda (test) (funcall (cdr test))) tests))
                   (res-lst (mapcar #'(lambda (r l) (report-result r l)) results lbls)))
              (notany #'null results)))

(defun report-result (result label)
  (let ((str (format nil "~&~:[FAIL~;pass~] ... ~a" result label)))
    (write str :stream om::*om-stream* :escape nil))
  (terpri om::*om-stream*)
  (list label result))
