;;; Handling of all bug logic.

(in-package :save-the-farm)

#+nil
(launch)

#+nil
(maybe-reload-scene)

;; --- Types --- ;;

(defclass bug ()
  ((movement-scheme :accessor movement-scheme)
   (movement-speed  :accessor movement-speed)
   (health          :accessor health))
  (:documentation "Behaviour common to all bugs."))

(define-shader-entity bug-fly (bug animated-sprite facing-entity located-entity)
  ((sprite-data     :initform (asset 'farm 'bug-fly))
   (movement-scheme :initform #'move-straight)
   (movement-speed  :initform (+ 0.5 (cl:random 0.5)))
   (facing          :initform :left)
   (health          :initform 2)))

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
  (let ((nearby-puff (collision-candidate bug *puffs*)))
    (when (and nearby-puff (overlapping? bug nearby-puff))
      (decf (health bug) +puff-damage+)
      (leave nearby-puff (container nearby-puff))
      (when (zerop (health bug))
        (leave bug (container bug)))))
  (funcall (movement-scheme bug) bug)
  ;; Automatic despawn when out of bounds.
  (when (not (in-x-bounds? (max-x bug)))
    (v:info :stf "Bug dead.")
    (leave bug (container bug))))

;; --- Movement --- ;;

(defun move-straight (bug)
  "Move in a straight line in the bug's current direction."
  (incf (vx (location bug))
        (if (eq :left (facing bug))
            (* (movement-speed bug) -1)
            (* (movement-speed bug) 1))))
