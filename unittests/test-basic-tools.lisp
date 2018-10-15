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

(dsg-test::add-unittest
 (dsg-test::deftest dsg-test::equivalent-chord-seq ()
   ;; Very basic test for the time being...
   (let* ((a (progn
               (let ((cseq (mki 'chord-seq :empty t)))
                 (setf (LMidic cseq) '((6000 6400 6700)))
                 cseq)))
          (b (progn
               ;; Same notes but different order
               (let ((cseq (mki 'chord-seq :empty t)))
                 (setf (LMidic cseq) '((6400 6700 6000)))
                 cseq)))
          (c (progn
               ;; Different note(s)
               (let ((cseq (mki 'chord-seq :empty t)))
                 (setf (LMidic cseq) '((6000 6500 6700)))
                 cseq))))
     (dsg-test::check
       (dsg::equivalent a (clone a))
       (dsg::equivalent a b)
       (not (dsg::equivalent a c))))))