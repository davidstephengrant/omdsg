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
(dsg-test::deftest dsg-test::truncate-overlaps---single-note--truncate
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000)))
                                      self))
                          (result (dsg::truncate-overlaps original 'truncate)))
                     (dsg::equivalent original result)))

(dsg-test::deftest dsg-test::truncate-overlaps---single-note--extend
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000)))
                                      self))
                          (result (dsg::truncate-overlaps original 'extend)))
                     (dsg::equivalent original result)))

(dsg-test::deftest dsg-test::truncate-overlaps---no-mode-argument
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000 6400) 6400))
                                      (setf (LOnset self) '(0 500))
                                      (setf (LDur self) '((1000 2500) 100))
                                      self))
                          (expected (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000 6400) 6400))
                                      (setf (LOnset self) '(0 500))
                                      (setf (LDur self) '((1000 500) 100))
                                      self))
                          (result (dsg::truncate-overlaps original)))
                     (dsg::equivalent expected result)))

(dsg-test::deftest dsg-test::truncate-overlaps---trunc
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000 6400) 6400))
                                      (setf (LOnset self) '(0 500))
                                      (setf (LDur self) '((1000 2500) 100))
                                      self))
                          (expected (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000 6400) 6400))
                                      (setf (LOnset self) '(0 500))
                                      (setf (LDur self) '((1000 500) 100))
                                      self))
                          (result (dsg::truncate-overlaps original 'truncate)))
                     (dsg::equivalent expected result)))

(dsg-test::deftest dsg-test::truncate-overlaps---extend
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000 6400) 6400))
                                      (setf (LOnset self) '(0 500))
                                      (setf (LDur self) '((1000 2500) 100))
                                      self))
                          (expected (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000 6400) 6400))
                                      (setf (LOnset self) '(0 500))
                                      (setf (LDur self) '((1000 500) 2000))
                                      self))
                          (result (dsg::truncate-overlaps original 'extend)))
                     (dsg::equivalent expected result)))

(dsg-test::deftest dsg-test::truncate-overlaps---chord-to-chord--trunc
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000 6400) (6400 6800)))
                                      (setf (LOnset self) '(0 500))
                                      (setf (LDur self) '((1000 2500) (100 2500)))
                                      self))
                          (expected (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000 6400) (6400 6800)))
                                      (setf (LOnset self) '(0 500))
                                      (setf (LDur self) '((1000 500) (100 2500)))
                                      self))
                          (result (dsg::truncate-overlaps original)))
                     (dsg::equivalent expected result)))

(dsg-test::deftest dsg-test::truncate-overlaps---chord-to-chord--extend
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000 6400) (6400 6800)))
                                      (setf (LOnset self) '(0 500))
                                      (setf (LDur self) '((1000 2500) (100 2500)))
                                      self))
                          (expected (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000 6400) (6400 6800)))
                                      (setf (LOnset self) '(0 500))
                                      (setf (LDur self) '((1000 500) (2000 2500)))
                                      self))
                          (result (dsg::truncate-overlaps original 'extend)))
                     (dsg::equivalent expected result)))

(dsg-test::deftest dsg-test::truncate-overlaps---seq-overlaps
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000) (6000) (6000)))
                                      (setf (LOnset self) '(0 500 1000))
                                      (setf (LDur self) '((1000) (1000) (1000)))
                                      self))
                          (expected (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000) (6000) (6000)))
                                      (setf (LOnset self) '(0 500 1000))
                                      (setf (LDur self) '((500) (500) (1000)))
                                      self))
                          (result (dsg::truncate-overlaps original)))
                     (dsg::equivalent expected result)))

(dsg-test::deftest dsg-test::truncate-overlaps---fan--trunc
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000) (6000) (6000)))
                                      (setf (LOnset self) '(0 1000 2000))
                                      (setf (LDur self) '((5000) (3000) (1000)))
                                      self))
                          (expected (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000) (6000) (6000)))
                                      (setf (LOnset self) '(0 1000 2000))
                                      (setf (LDur self) '((1000) (1000) (1000)))
                                      self))
                          (result (dsg::truncate-overlaps original)))
                     (dsg::equivalent expected result)))

(dsg-test::deftest dsg-test::truncate-overlaps---fan--extend
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000) (6000) (6000)))
                                      (setf (LOnset self) '(0 1000 2000))
                                      (setf (LDur self) '((5000) (3000) (1000)))
                                      self))
                          (expected (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000) (6000) (6000)))
                                      (setf (LOnset self) '(0 1000 2000))
                                      (setf (LDur self) '((1000) (1000) (3000)))
                                      self))
                          (result (dsg::truncate-overlaps original 'extend)))
                     (dsg::equivalent expected result)))

(dsg-test::deftest dsg-test::truncate-overlaps---intermittent--trunc
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000) (6000) (6000)))
                                      (setf (LOnset self) '(0 1000 2000))
                                      (setf (LDur self) '((5000) (200) (200)))
                                      self))
                          (expected (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000) (6000) (6000)))
                                      (setf (LOnset self) '(0 1000 2000))
                                      (setf (LDur self) '((1000) (200) (200)))
                                      self))
                          (result (dsg::truncate-overlaps original)))
                     (dsg::equivalent expected result)))

(dsg-test::deftest dsg-test::truncate-overlaps---intermittent--extend
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000) (6000) (6000)))
                                      (setf (LOnset self) '(0 1000 2000))
                                      (setf (LDur self) '((5000) (200) (200)))
                                      self))
                          (expected (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000) (6000) (6000)))
                                      (setf (LOnset self) '(0 1000 2000))
                                      (setf (LDur self) '((1000) (1000) (3000)))
                                      self))
                          (result (dsg::truncate-overlaps original 'extend)))
                     (dsg::equivalent expected result)))

(dsg-test::deftest dsg-test::truncate-overlaps---intermittent--trunc--different-notes
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000) (6100) (6200)))
                                      (setf (LOnset self) '(0 1000 2000))
                                      (setf (LDur self) '((5000) (200) (200)))
                                      self))
                          (result (dsg::truncate-overlaps original)))
                     (dsg::equivalent original result)))

(dsg-test::deftest dsg-test::truncate-overlaps---intermittent--extend--different-notes
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((6000) (6100) (6200)))
                                      (setf (LOnset self) '(0 1000 2000))
                                      (setf (LDur self) '((5000) (200) (200)))
                                      self))
                          (result (dsg::truncate-overlaps original 'extend)))
                     (dsg::equivalent original result)))

(dsg-test::deftest dsg-test::truncate-overlaps---schaathun-ex1--trunc
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((8200) (8300) (7000) (8200) (8100)))
                                      (setf (LOnset self) '(0 661 950 1500 1700 3100))
                                      (setf (LDur self) '((1500) (1053) (2150) (1500) (1238)))
                                      self))
                          (expected (let ((self (mki 'chord-seq :empty t)))
                                      (setf (LMidic self) '((8200) (8300) (7000) (8200) (8100)))
                                      (setf (LOnset self) '(0 661 950 1500 1700 3100))
                                      (setf (LDur self) '((1500) (1053) (2150) (1500) (1238)))
                                      self))
                          (result (dsg::truncate-overlaps original)))
                     (dsg::equivalent expected result)))
