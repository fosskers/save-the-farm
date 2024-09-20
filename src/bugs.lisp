;;; Handling of all bug logic.

(in-package :save-the-farm)

(define-shader-entity bug-fly (animated-sprite facing-entity located-entity)
  ((sprite-data :initform (asset 'farm 'bug-fly))
   (facing :initform :left :accessor facing)))

(defmethod min-x ((bug-fly bug-fly))
  (- (vx (location bug-fly)) 7))
(defmethod max-x ((bug-fly bug-fly))
  (+ 8 (vx (location bug-fly))))
(defmethod min-y ((bug-fly bug-fly))
  (- (vy (location bug-fly)) 8))
(defmethod max-y ((bug-fly bug-fly))
  (+ 7 (vy (location bug-fly))))
