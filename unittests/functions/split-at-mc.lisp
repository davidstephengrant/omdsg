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
; CHORD
;==============================
(dsg-test::deftest dsg-test::split-at-mc--chord---single-note-above-split
         (let* ((original (let ((self (mki 'chord :empty t)))
                    (setf (LMidic self) '(6000))
                    self))
               (expected (let ((self1 (mki 'chord :empty t))
                               (self2 (mki 'chord :empty t)))
                           (setf (LMidic self1) '(6000))
                           (list self1 self2)))
               (result (dsg::split-at-mc original)))
           (notany #'null (mapcar #'dsg::equivalent expected result))))

(dsg-test::deftest dsg-test::split-at-mc--chord---single-note-below-split
         (let* ((original (let ((self (mki 'chord :empty t)))
                    (setf (LMidic self) '(5900))
                    self))
               (expected (let ((self1 (mki 'chord :empty t))
                               (self2 (mki 'chord :empty t)))
                           (setf (LMidic self2) '(5900))
                           (list self1 self2)))
               (result (dsg::split-at-mc original)))
           (notany #'null (mapcar #'dsg::equivalent expected result))))

(dsg-test::deftest dsg-test::split-at-mc--chord---chord
         (let* ((original (let ((self (mki 'chord :empty t)))
                    (setf (LMidic self) '(5900 6000))
                    self))
               (expected (let ((self1 (mki 'chord :empty t))
                               (self2 (mki 'chord :empty t)))
                           (setf (LMidic self1) '(6000))
                           (setf (LMidic self2) '(5900))
                           (list self1 self2)))
               (result (dsg::split-at-mc original)))
           (notany #'null (mapcar #'dsg::equivalent expected result))))

(dsg-test::deftest dsg-test::split-at-mc--chord---chord--move-split-up
         (let* ((original (let ((self (mki 'chord :empty t)))
                    (setf (LMidic self) '(5900 6000))
                    self))
               (expected (let ((self1 (mki 'chord :empty t))
                               (self2 (mki 'chord :empty t)))
                           (setf (LMidic self2) '(5900 6000))
                           (list self1 self2)))
               (result (dsg::split-at-mc original 6100)))
           (notany #'null (mapcar #'dsg::equivalent expected result))))

(dsg-test::deftest dsg-test::split-at-mc--chord---chord--move-split-down
         (let* ((original (let ((self (mki 'chord :empty t)))
                    (setf (LMidic self) '(5900 6000))
                    self))
               (expected (let ((self1 (mki 'chord :empty t))
                               (self2 (mki 'chord :empty t)))
                           (setf (LMidic self1) '(5900 6000))
                           (list self1 self2)))
               (result (dsg::split-at-mc original 5900)))
           (notany #'null (mapcar #'dsg::equivalent expected result))))

(dsg-test::deftest dsg-test::split-at-mc--chord---chord-with-offsets
         (let* ((original (let ((self (mki 'chord :empty t)))
                    (setf (LMidic self) '(5800 5900 6000 6100))
                    (setf (LOffset self) '(100 200 300 400))
                    self))
               (expected (let ((self1 (mki 'chord :empty t))
                               (self2 (mki 'chord :empty t)))
                           (setf (LMidic self1) '(6000 6100))
                           (setf (LOffset self1) '(300 400))
                           (setf (LMidic self2) '(5800 5900))
                           (setf (LOffset self2) '(100 200))
                           (list self1 self2)))
               (result (dsg::split-at-mc original)))
           (notany #'null (mapcar #'dsg::equivalent expected result))))

;==============================
; CHORD-SEQ
;==============================
(dsg-test::deftest dsg-test::split-at-mc--chord-seq
                   (let* ((original (let ((self (mki 'chord-seq :empty t)))
                                     (setf (LMidic self) '((5800 5900 6000 6100) (7200 7300) (4800 4900)))
                                     (setf (LOnset self) '(100 500 2500))
                                     (setf (LDur self) '((200 400 500 1000) (2000 2500) (100 50)))
                                     (setf (LVel self) '((90 100 110 120) (80 70) (40 20)))
                                     (setf (LOffset self) '((100 200 300 400) (30 0) (0 0)))
                                     (setf (LChan self) '((1 2 3 4) (1 1) (2 3)))
                                     self))
                          (expected (let ((self (mki 'multi-seq :empty t))
                                     (self-above (mki 'chord-seq :empty t))
                                     (self-below (mki 'chord-seq :empty t)))
;                                 ;; self-above
                                      (setf (LMidic self-above) '((6000 6100) (7200 7300)))
                                      (setf (LOnset self-above) '(100 500))
                                      (setf (LDur self-above) '((500 1000) (2000 2500)))
                                      (setf (LVel self-above) '((110 120) (80 70)))
                                      (setf (LOffset self-above) '((300 400) (30 0)))
                                      (setf (LChan self-above) '((3 4) (1 1)))
;                                 ;; self-below
                                      (setf (LMidic self-below) '((5800 5900) (4800 4900)))
                                      (setf (LOnset self-below) '(100 2500))
                                      (setf (LDur self-below) '((200 400) (100 50)))
                                      (setf (LVel self-below) '((90 100) (40 20)))
                                      (setf (LOffset self-below) '((100 200) (0 0)))
                                      (setf (LChan self-below) '((1 2) (2 3)))
;                                 ;; self
                                      (setf (chord-seqs self) (list self-above self-below))
                                      self))
                     (result (dsg::split-at-mc original)))
                  (dsg::equivalent expected result)))
