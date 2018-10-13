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


;;;  Unit test framework based on "Chapter 9: Building a Unit Test Framework"
;;;  in Peter Seibel's book "Practical Common Lisp" (Apress, 2005)
;;;  
;;;  Copyright (c) 2005, Peter Seibel All rights reserved.
;;;
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions are
;;;  met:
;;;
;;;      * Redistributions of source code must retain the above copyright
;;;        notice, this list of conditions and the following disclaimer.
;;;
;;;      * Redistributions in binary form must reproduce the above
;;;        copyright notice, this list of conditions and the following
;;;        disclaimer in the documentation and/or other materials provided
;;;        with the distribution.
;;;
;;;      * Neither the name of the Peter Seibel nor the names of its
;;;        contributors may be used to endorse or promote products derived
;;;        from this software without specific prior written permission.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defvar dsg-test)

(defpackage dsg-test)

(in-package :dsg-test)

(defvar *test-name* nil)
(defvar *unittests* nil)

;; Set vars to nil (in case lib is reloaded)
(setf *unittests* nil *test-name* nil)

(om::defmethod! run-unittests ()
            ;:icon 700
  (loop for test in *unittests*
        collect (funcall test) into results
        finally return (notany #'null results)))

(defun add-unittest (test-function)
  (setf *unittests* (append *unittests* (list test-function))))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (let ((str (format nil "~&~:[FAIL~;pass~] ... ~a: ~a" result *test-name* form)))
    (write str :stream om::*om-stream* :escape nil))
  (terpri om::*om-stream*)
  result)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))