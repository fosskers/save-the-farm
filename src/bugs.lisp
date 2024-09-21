;;; Handling of all bug logic.

(in-package :save-the-farm)

#+nil
(launch)

#+nil
(maybe-reload-scene)

;; --- Types --- ;;

(defclass bug ()
  ((movement-scheme :initform #'move-straight :accessor movement-scheme)
   (movement-speed :initform (+ 0.5 (cl:random 0.5)) :accessor movement-speed))
  (:documentation "Behaviour common to all bugs."))

(define-shader-entity bug-fly (bug animated-sprite facing-entity located-entity)
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

;; --- Handlers --- ;;

(define-handler (bug tick :before) ()
  (funcall (movement-scheme bug) bug)
  ;; Automatic despawn when out of bounds.
  (when (not (in-x-bounds? (max-x bug)))
    (leave bug (container bug))))

;; --- Movement --- ;;

(defun move-straight (bug)
  "Move in a straight line in the bug's current direction."
  (incf (vx (location bug))
        (if (eq :left (facing bug))
            (* (movement-speed bug) -1)
            (* (movement-speed bug) 1))))
