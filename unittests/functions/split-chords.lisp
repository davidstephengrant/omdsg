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

;==============================
; CHORD-SEQ
;==============================
(dsg-test::deftest dsg-test::split-chords---single-one-note-chord
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                     (setf (LMidic self) '((6000)))
                                     self))
                         (result (dsg::split-chords original)))
                     (dsg::equivalent original result)))

(dsg-test::deftest dsg-test::split-chords---single-chord
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000 6400 6700)))
                                      self))
                          (expected (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000) (6400) (6700)))
                                      (setf (LOnset self) '(0 0 0))
                                      self))
                          (result (dsg::split-chords original)))
                     (dsg::equivalent expected result)))

(dsg-test::deftest dsg-test::split-chords---chord-sequence
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000 6400 6700)))
                                      self))
                          (expected (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000) (6400) (6700)))
                                      (setf (LOnset self) '(0 0 0))
                                      self))
                          (result (dsg::split-chords original)))
                     (dsg::equivalent expected result)))