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