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

(defmethod! dsg::equivalent ((a chord-seq) (b chord-seq))
            :icon 100
            (notany #'null (mapcar #'equalp
                                   (dsg::self-to-data (dsg::order-chords a))
                                   (dsg::self-to-data (dsg::order-chords b)))))

(defmethod! dsg::order-chords ((self chord))
            :icon 700
  (let ((out-chrd (mki 'chord :empty t))
        (data (sort (mapcar #'(lambda (pitch data) (cons pitch data))
          (LMidic self)
          (inside self))
                   #'< :key #'car)))
    (setf (inside out-chrd) (mapcar #'cdr data))
    out-chrd))

(defmethod! dsg::order-chords ((self chord-seq))
            (let ((out-cs (mki 'chord-seq :empty t))
                  (data (mapcar #'(lambda (ons dat) (cons ons dat))
                                (lonset self)
                                (mapcar #'dsg::order-chords (inside self)))))
              (setf (inside out-cs) (mapcar #'cdr data))
              (setf (lonset out-cs) (mapcar #'car data))
              out-cs))

(defmethod dsg::self-to-data ((self chord-seq))
  (list (LMidic self)
        (LOnset self)
        (LDur self)
        (LVel self)
        (LOffset self)
        (LChan self)))