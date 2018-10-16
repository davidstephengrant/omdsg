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

(defmethod! dsg::split-chords ((self chord-seq))
            :icon 700
            :doc "Split chords into single-note chords.

<self> can be a chord (yield a list of chords) or a chord-seq (yields a new chord-seq).
"
            :indoc '("chord or chord-seq")
  (let* ((chords-and-onsets (loop for chord in (inside self)
                                  for onset in (LOnset self)
                                  collect (dsg::split-chords chord) into chords
                                  collect (repeat-n onset(length (dsg::split-chords chord))) into onsets
                                  finally return (list (flat chords) (flat onsets))))
         (c-seq (objfromobjs (car chords-and-onsets) (mki 'chord-seq))))
    (setf (LOnset c-seq) (cadr chords-and-onsets))
    c-seq))

(defmethod! dsg::split-chords ((self chord))
  (loop for note in (inside self)
        for offset in (LOffset self)
        do (setf (slot-value note 'offset) offset)
        collect (objfromobjs note (mki 'chord))))

(defmethod dsg::remove-unison-roots ((self chord))
  (objfromobjs (dsg::remove-unison-roots (inside self)) self))

(defmethod dsg::remove-unison-roots ((self list)) ;; root assumed to be first note in chord <self>
  (cond ((null self) nil)
        (t (loop for note in (cdr self) with root = (car self)
                 if (eql (midic note) (midic root))
                 do (if (> (dur note) (dur root))
                        (setf root note))
                 else collect note into non-unisons
                 finally return (x-append root non-unisons)))))

(defmethod! dsg::remove-unisons ((self chord-seq))
            :icon 700
            :doc "Removes unisons from a chord, chord-seq or list of notes.

In a chord-seq notes must be in the same chord to be detected as unison.
"
            :indoc '("chord, chord-seq or list of notes")
  (let* ((clone (clone (dsg::realize-offsets self)))
         (onsets (LOnset clone)))
    (setf clone (objfromobjs (mapcar #'dsg::remove-unisons (inside clone)) clone))
    (setf (LOnset clone) onsets) ; Restore onsets
    clone))

(defmethod! dsg::remove-unisons ((self chord))
  (objfromobjs (dsg::remove-unisons (inside self)) self))

(defmethod! dsg::remove-unisons ((self list))
  (cond ((null self) nil)
        (t (let ((this-note-list (dsg::remove-unison-roots self)))
             (cons (car this-note-list) (dsg::remove-unisons (cdr this-note-list)))))))

(defmethod! dsg::realize-offsets ((self chord-seq) &optional (compensate-dur-p t))
            :icon 700
            :doc "Realize offsets by adjusting onsets and durations for notes in a chord-seq."
  (let ((clone (clone (dsg::split-chords self))))
    (cond (compensate-dur-p
           (setf clone (loop for chord in (inside clone)
                             ;; Shorten dur by offset or
                             ;; by dur (whichever is shortest)
                             ;; so that durations do not
                             ;; become negative
                             do (setf (LDur chord)
                                      (list (- (car (LDur chord))
                                               (min (car (LOffset chord))
                                                    (car (LDur chord))))))
                             collect chord into chords
                             ;; Restore onsets before returning
                             finally return (setf (LOnset (objfromobjs chords clone))
                                                  (LOnset clone))))))
    (setf (LOnset clone) (mapcar #'+ (notEndLOnset clone) (flat (LOffset clone)))) ; Add offset to onset
    (setf (LOffset clone) (mapcar #'(lambda (offset) (list 0)) (LOffset clone)))   ; Set offsets to zero
    (align-chords (temporal-sort clone) 0)))

(defmethod! dsg::truncate-overlaps ((self chord-seq) &optional (mode 'truncate))
            :icon 700
            :initvals (list (make-instance 'chord-seq) 'truncate)
            :indoc '("chord-seq" "symbol ('truncate or 'extend)")
            :menuins '((1 (("truncate" 'truncate) ("extend" 'extend))))
            :doc "Removes overlapping notes in a chord-seq."
            (let* ((out-cs (mki 'chord-seq :empty t))
                  (adj-self (temporal-sort (dsg::split-chords (dsg::remove-unisons (dsg::realize-offsets (clone self))))))
                  (data (dsg::truncate-if-overlap (dsg::chord-seq->onset-chord-pairs adj-self) mode)))
              (setf (inside out-cs) (mapcar #'cdr data))
              (setf (lonset out-cs) (mapcar #'car data))
              (align-chords out-cs 0)))

(defmethod dsg::chord-seq->onset-chord-pairs ((self chord-seq))
  "Takes a chord-seq and returns a list of dotted pairs: (onset . chord)."
  (mapcar #'(lambda (onset data) (cons onset data))
          (LOnset self)
          (inside self)))

(defun dsg::truncate-if-overlap (lst mode)
  "
Takes a list of dotted pairs (onset . chord) which represents a chord-seq.
Use chord-seq->onset-chord-pairs to generate <lst>.
"
  (labels ((enharmonic-p (notepair1 notepair2)
             (= (car (LMidic (cdr notepair1)))
                (car (LMidic (cdr notepair2)))))
           (overlap-p (notepair1 notepair2)
             (> (+ (car notepair1) (car (LDur (cdr notepair1))))
                (car notepair2)))
           (second-ends-before-first-p (notepair1 notepair2)
             (> (+ (car notepair1) (car (LDur (cdr notepair1))))
                (+ (car notepair2) (car (LDur (cdr notepair2))))))
           (trunc-first (notepair1 notepair2)
             (let ((clone (clone (cdr notepair1))))
               (setf (LDur clone) (list (- (car notepair2) (car notepair1))))
               (cons (car notepair1) clone)))
           (extend-second (notepair1 notepair2)
             (let ((clone (clone (cdr notepair2))))
               (setf (LDur clone) (list (- (+ (car notepair1) (car (LDur (cdr notepair1))))
                                           (car notepair2))))
               (cons (car notepair2) clone)))
           (check-notes (elem rest mode accum)
             (cond ((null rest) (cons elem (nreverse accum)))
                   ((not (enharmonic-p elem (car rest))) (check-notes elem (cdr rest) mode (cons (car rest) accum)))
                   ((overlap-p elem (car rest))
                    (if (and (eql mode 'extend)
                             (second-ends-before-first-p elem (car rest)))
                        (cons (trunc-first elem (car rest)) (nreverse (cons (extend-second elem (car rest)) accum)))
                      (cons (trunc-first elem (car rest)) (nreverse (cons (car rest) accum)))))
                   (t (check-notes elem (cdr rest) mode accum)))))

    (cond ((null lst) nil)
          (t (let* ((adjusted-notes (check-notes (car lst) (cdr lst) mode nil))
                    (new-lst (x-append adjusted-notes (last-n lst (- (length lst) (length adjusted-notes))))))
               (cons (pop new-lst) (dsg::truncate-if-overlap new-lst mode)))))))

