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
(dsg-test::deftest dsg-test::equivalent-chord---same-chord
         (let ((a (let ((self (mki 'chord :empty t)))
                    (setf (LMidic self) '(6000 6400 6700))
                    (setf (LOffset self) '(0 200 400))
                    self)))
           (dsg::equivalent a a)))

(dsg-test::deftest dsg-test::equivalent-chord---cloned-chord
         (let ((a (let ((self (mki 'chord :empty t)))
                    (setf (LMidic self) '(6000 6400 6700))
                    (setf (LOffset self) '(0 200 400))
                    self)))
           (dsg::equivalent a (clone a))))

(dsg-test::deftest dsg-test::equivalent-chord---different-order
         (let ((a (let ((self (mki 'chord :empty t)))
                    (setf (LMidic self) '(6000 6400 6700))
                    (setf (LOffset self) '(0 200 400))
                    self))
               (b (let ((self (mki 'chord :empty t)))
                    (setf (LMidic self) '(6400 6700 6000))
                    (setf (LOffset self) '(200 400 0))
                    self)))
           (dsg::equivalent a b)))

(dsg-test::deftest dsg-test::equivalent-chord---different-notes
         (let ((a (let ((self (mki 'chord :empty t)))
                    (setf (LMidic self) '(6000 6400 6700))
                    (setf (LOffset self) '(0 200 400))
                    self))
               (c (let ((self (mki 'chord :empty t)))
                    (setf (LMidic self) '(6000 6500 6700))
                    (setf (LOffset self) '(0 200 400))
                    self)))
           (not (dsg::equivalent a c))))

(dsg-test::deftest dsg-test::equivalent-chord---different-offset
         (let ((a (let ((self (mki 'chord :empty t)))
                    (setf (LMidic self) '(6000 6400 6700))
                    (setf (LOffset self) '(0 200 400))
                    self))
               (d (let ((self (mki 'chord :empty t)))
                    (setf (LMidic self) '(6000 6400 6700))
                    self)))
           (not (dsg::equivalent a d))))

;==============================
; CHORD-SEQ
;==============================
(dsg-test::deftest dsg-test::equivalent-chord-seq---same-chord
                   (let ((a (let ((self (mki 'chord-seq :empty t)))
                              (setf (LMidic self) '((6000 6400 6700)))
                              (setf (LOffset self) '((0 200 400)))
                              self)))
                     (dsg::equivalent a a)))

(dsg-test::deftest dsg-test::equivalent-chord-seq---cloned-chord
                   (let ((a (let ((self (mki 'chord-seq :empty t)))
                              (setf (LMidic self) '((6000 6400 6700)))
                              (setf (LOffset self) '((0 200 400)))
                              self)))
                     (dsg::equivalent a (clone a))))

(dsg-test::deftest dsg-test::equivalent-chord-seq---different-order
                   (let ((a (let ((self (mki 'chord-seq :empty t)))
                              (setf (LMidic self) '((6000 6400 6700)))
                              (setf (LOffset self) '((0 200 400)))
                              self))
                         (b (let ((self (mki 'chord-seq :empty t)))
                              (setf (LMidic self) '((6400 6700 6000)))
                              (setf (LOffset self) '((200 400 0)))
                              self)))
                     (dsg::equivalent a b)))

(dsg-test::deftest dsg-test::equivalent-chord-seq---different-notes
                   (let ((a (let ((self (mki 'chord-seq :empty t)))
                              (setf (LMidic self) '((6000 6400 6700)))
                              (setf (LOffset self) '((0 200 400)))
                              self))
                         (c (let ((self (mki 'chord-seq :empty t)))
                              (setf (LMidic self) '((6000 6500 6700)))
                              (setf (LOffset self) '((0 200 400)))
                              self)))
                     (not (dsg::equivalent a c))))

(dsg-test::deftest dsg-test::equivalent-chord-seq---different-offset
                   (let ((a (let ((self (mki 'chord-seq :empty t)))
                              (setf (LMidic self) '((6000 6400 6700)))
                              (setf (LOffset self) '((0 200 400)))
                              self))
                         (d (let ((self (mki 'chord-seq :empty t)))
                              (setf (LMidic self) '((6000 6400 6700)))
                              self)))
                     (not (dsg::equivalent a d))))