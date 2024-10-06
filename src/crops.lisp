;;; Crop types and their logic.

(in-package :save-the-farm)

;; --- Types --- ;;

(defclass crop ()
  ((health :accessor health :documentation "Remaining health of the crop."))
  (:documentation "Common fields for crops."))

(define-shader-entity lemon (crop animated-sprite located-entity)
  ((sprite-data :initform (asset 'farm 'lemon))
   (health      :initform 2)))

(defmethod min-x ((lemon lemon))
  (- (vx (location lemon)) 7))
(defmethod max-x ((lemon lemon))
  (+ 8 (vx (location lemon))))
(defmethod min-y ((lemon lemon))
  (- (vy (location lemon)) 8))
(defmethod max-y ((lemon lemon))
  (+ 6 (vy (location lemon))))

;; --- Handlers --- ;;

(define-handler (crop tick :before) ()
  (let ((nearby-bug (collision-candidate crop *bugs*)))
    (when (and nearby-bug (overlapping? crop nearby-bug))
      (decf (health crop) (damage nearby-bug))
      (leave nearby-bug (container nearby-bug))
      (when (<= (health crop) 0)
        (leave crop (container crop))))))

;; --- Misc. --- ;;

(defun spawn-crops (crop-type container)
  (let ((locs '((1 . 10) (1 . 9) (1 . 8) (1 . 7) (1 . 6) (1 . 5) (1 . 4)
                (2 . 10) (2 . 9) (2 . 8) (2 . 7) (2 . 6) (2 . 5) (2 . 4))))
    (dolist (loc locs)
      (let ((crop (make-instance crop-type)))
        (enter crop container)
        (setf (location crop) (grid->pixel (car loc) (cdr loc)))))))
