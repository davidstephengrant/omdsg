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

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000)))
                     cseq)))
       (result (dsg::split-chords original)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::split-chords---single-one-note-chord
                      ;; PASS CONDITION
                      (dsg::equivalent original result)))

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000 6400 6700)))
                     cseq)))
       (expected (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000) (6400) (6700)))
                     (setf (LOnset cseq) '(0 0 0))
                     cseq)))
       (result (dsg::split-chords original)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::split-chords---single-chord
                      ;; PASS CONDITION
                      (dsg::equivalent expected result)))

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000 6400 6700) (6000) (6400 6700)))
                     (setf (LOnset cseq) '(1000 1500 1700))
                     (setf (LDur cseq) '((100 500 750) (500) (750 500)))
                     (setf (LVel cseq) '((100 120 110) (90) (80 70)))
                     (setf (LOffset cseq) '((100 0 110) (90) (80 70)))
                     (setf (LChan cseq) '((1 2 3) (4) (5 6)))
                     cseq)))
       (expected (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000) (6400) (6700) (6000) (6400) (6700)))
                     (setf (LOnset cseq) '(1000 1000 1000 1500 1700 1700))
                     (setf (LDur cseq) '((100) (500) (750) (500) (750) (500)))
                     (setf (LVel cseq) '((100) (120) (110) (90) (80) (70)))
                     (setf (LOffset cseq) '((100) (0) (110) (90) (80) (70)))
                     (setf (LChan cseq) '((1) (2) (3) (4) (5) (6)))
                     cseq)))
       (result (dsg::split-chords original)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::split-chords---chord-sequence
                      ;; PASS CONDITION
                      (dsg::equivalent expected result)))

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000)))
                     cseq)))
       (result (dsg::truncate-overlaps original 'truncate)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::truncate-overlaps---single-note--truncate
                      ;; PASS CONDITION
                      (dsg::equivalent original result)))

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000)))
                     cseq)))
       (result (dsg::truncate-overlaps original 'extend)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::truncate-overlaps---single-note--extend
                      ;; PASS CONDITION
                      (dsg::equivalent original result)))

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000 6400) 6400))
                     (setf (LOnset cseq) '(0 500))
                     (setf (LDur cseq) '((1000 2500) 100))
                     cseq)))
       (expected (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000 6400) 6400))
                     (setf (LOnset cseq) '(0 500))
                     (setf (LDur cseq) '((1000 500) 100))
                     cseq)))
       (result (dsg::truncate-overlaps original)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::truncate-overlaps---no-mode-argument
                      ;; PASS CONDITION
                      (dsg::equivalent expected result)))

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000 6400) 6400))
                     (setf (LOnset cseq) '(0 500))
                     (setf (LDur cseq) '((1000 2500) 100))
                     cseq)))
       (expected (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000 6400) 6400))
                     (setf (LOnset cseq) '(0 500))
                     (setf (LDur cseq) '((1000 500) 100))
                     cseq)))
       (result (dsg::truncate-overlaps original 'truncate)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::truncate-overlaps---trunc
                      ;; PASS CONDITION
                      (dsg::equivalent expected result)))

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000 6400) 6400))
                     (setf (LOnset cseq) '(0 500))
                     (setf (LDur cseq) '((1000 2500) 100))
                     cseq)))
       (expected (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000 6400) 6400))
                     (setf (LOnset cseq) '(0 500))
                     (setf (LDur cseq) '((1000 500) 2000))
                     cseq)))
       (result (dsg::truncate-overlaps original 'extend)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::truncate-overlaps---extend
                      ;; PASS CONDITION
                      (dsg::equivalent expected result)))

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000 6400) (6400 6800)))
                     (setf (LOnset cseq) '(0 500))
                     (setf (LDur cseq) '((1000 2500) (100 2500)))
                     cseq)))
       (expected (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000 6400) (6400 6800)))
                     (setf (LOnset cseq) '(0 500))
                     (setf (LDur cseq) '((1000 500) (100 2500)))
                     cseq)))
       (result (dsg::truncate-overlaps original)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::truncate-overlaps---chord-to-chord--trunc
                      ;; PASS CONDITION
                      (dsg::equivalent expected result)))

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000 6400) (6400 6800)))
                     (setf (LOnset cseq) '(0 500))
                     (setf (LDur cseq) '((1000 2500) (100 2500)))
                     cseq)))
       (expected (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000 6400) (6400 6800)))
                     (setf (LOnset cseq) '(0 500))
                     (setf (LDur cseq) '((1000 500) (2000 2500)))
                     cseq)))
       (result (dsg::truncate-overlaps original 'extend)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::truncate-overlaps---chord-to-chord--extend
                      ;; PASS CONDITION
                      (dsg::equivalent expected result)))

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000) (6000) (6000)))
                     (setf (LOnset cseq) '(0 500 1000))
                     (setf (LDur cseq) '((1000) (1000) (1000)))
                     cseq)))
       (expected (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000) (6000) (6000)))
                     (setf (LOnset cseq) '(0 500 1000))
                     (setf (LDur cseq) '((500) (500) (1000)))
                     cseq)))
       (result (dsg::truncate-overlaps original)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::truncate-overlaps---seq-overlaps
                      ;; PASS CONDITION
                      (dsg::equivalent expected result)))

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000) (6000) (6000)))
                     (setf (LOnset cseq) '(0 1000 2000))
                     (setf (LDur cseq) '((5000) (3000) (1000)))
                     cseq)))
       (expected (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000) (6000) (6000)))
                     (setf (LOnset cseq) '(0 1000 2000))
                     (setf (LDur cseq) '((1000) (1000) (1000)))
                     cseq)))
       (result (dsg::truncate-overlaps original)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::truncate-overlaps---fan--trunc
                      ;; PASS CONDITION
                      (dsg::equivalent expected result)))

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000) (6000) (6000)))
                     (setf (LOnset cseq) '(0 1000 2000))
                     (setf (LDur cseq) '((5000) (3000) (1000)))
                     cseq)))
       (expected (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000) (6000) (6000)))
                     (setf (LOnset cseq) '(0 1000 2000))
                     (setf (LDur cseq) '((1000) (1000) (3000)))
                     cseq)))
       (result (dsg::truncate-overlaps original 'extend)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::truncate-overlaps---fan--extend
                      ;; PASS CONDITION
                      (dsg::equivalent expected result)))

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000) (6000) (6000)))
                     (setf (LOnset cseq) '(0 1000 2000))
                     (setf (LDur cseq) '((5000) (200) (200)))
                     cseq)))
       (expected (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000) (6000) (6000)))
                     (setf (LOnset cseq) '(0 1000 2000))
                     (setf (LDur cseq) '((1000) (200) (200)))
                     cseq)))
       (result (dsg::truncate-overlaps original)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::truncate-overlaps---intermittent--trunc
                      ;; PASS CONDITION
                      (dsg::equivalent expected result)))

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000) (6100) (6200)))
                     (setf (LOnset cseq) '(0 1000 2000))
                     (setf (LDur cseq) '((5000) (200) (200)))
                     cseq)))
       (result (dsg::truncate-overlaps original)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::truncate-overlaps---intermittent--trunc--different-notes
                      ;; PASS CONDITION
                      (dsg::equivalent original result)))

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000) (6000) (6000)))
                     (setf (LOnset cseq) '(0 1000 2000))
                     (setf (LDur cseq) '((5000) (200) (200)))
                     cseq)))
       (expected (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000) (6000) (6000)))
                     (setf (LOnset cseq) '(0 1000 2000))
                     (setf (LDur cseq) '((1000) (1000) (3000)))
                     cseq)))
       (result (dsg::truncate-overlaps original 'extend)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::truncate-overlaps---intermittent--extend
                      ;; PASS CONDITION
                      (dsg::equivalent expected result)))

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((6000) (6100) (6200)))
                     (setf (LOnset cseq) '(0 1000 2000))
                     (setf (LDur cseq) '((5000) (200) (200)))
                     cseq)))
       (result (dsg::truncate-overlaps original 'extend)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::truncate-overlaps---intermittent--extend--different-notes
                      ;; PASS CONDITION
                      (dsg::equivalent original result)))

(let* ((original (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((8200) (8300) (7000) (8200) (8100)))
                     (setf (LOnset cseq) '(0 661 950 1500 1700 3100))
                     (setf (LDur cseq) '((1500) (1053) (2150) (1500) (1238)))
                     cseq)))
       (expected (progn
                   (let ((cseq (mki 'chord-seq :empty t)))
                     (setf (LMidic cseq) '((8200) (8300) (7000) (8200) (8100)))
                     (setf (LOnset cseq) '(0 661 950 1500 1700 3100))
                     (setf (LDur cseq) '((1500) (1053) (2150) (1500) (1238)))
                     cseq)))
       (result (dsg::truncate-overlaps original)))

  ;;                  ;; TEST NAME
  (dsg-test::unittest dsg-test::truncate-overlaps---schaathun-ex1--trunc
                      ;; PASS CONDITION
                      (dsg::equivalent expected result)))