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

(defmethod! dsg::equivalent ((a multi-seq) (b multi-seq))
            :icon 100
            (cond ((eq a b) t)
                  ((not (= (length (inside a)) (length (inside b)))) nil)
                  (t (notany #'null (loop for cseq-a in (inside a)
                                          for cseq-b in (inside b)
                                          collect (dsg::equivalent cseq-a cseq-b))))))

(defmethod! dsg::equivalent ((a chord-seq) (b chord-seq))
            (cond ((eq a b) t)
                  ;; For documentation: extent of chord-seqs is ignored
                  (t (notany #'null (mapcar #'equalp
                                            (dsg::self-to-data (dsg::order-chords a))
                                            (dsg::self-to-data (dsg::order-chords b)))))))

(defmethod! dsg::equivalent ((a chord) (b chord))
            (cond ((eq a b) t)
                  (t (notany #'null (mapcar #'equalp
                                            (dsg::self-to-data (dsg::order-chords a))
                                            (dsg::self-to-data (dsg::order-chords b)))))))

(defmethod dsg::self-to-data ((self chord-seq))
  (list (LMidic self)
        ;; Ignores extent of chord-seq
        (notEndLOnset self)
        (LDur self)
        (LVel self)
        (LOffset self)
        (LChan self)))

(defmethod dsg::self-to-data ((self chord))
  (list (LMidic self)
        (LVel self)
        (LOffset self)
        (LDur self)
        (LChan self)))