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

(let* ((a (progn
            (let ((cseq (mki 'chord-seq :empty t)))
              (setf (LMidic cseq) '((6000 6400 6700)))
              cseq)))
       (b (progn
            (let ((cseq (mki 'chord-seq :empty t)))
              (setf (LMidic cseq) '((6400 6700 6000)))
              cseq)))
       (c (progn
            (let ((cseq (mki 'chord-seq :empty t)))
              (setf (LMidic cseq) '((6000 6500 6700)))
              cseq))))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::equivalent-chord-seq---cloned-chord
                      ;; PASS CONDITION
                      (dsg::equivalent a (clone a)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::equivalent-chord-seq---same-notes-different-order
                      ;; PASS CONDITION
                      (dsg::equivalent a b))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::equivalent-chord-seq---different-notes
                      ;; PASS CONDITION
                      (not (dsg::equivalent a c)))

  )