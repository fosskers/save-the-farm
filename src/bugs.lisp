;;; Handling of all bug logic.

(in-package :save-the-farm)

(define-shader-entity bug-fly (animated-sprite facing-entity located-entity)
  ((sprite-data :initform (asset 'farm 'bug-fly))
   (facing :initform :left :accessor facing)))
