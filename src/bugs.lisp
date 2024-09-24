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
   (health          :accessor health :documentation "Remaining health of the bug.")
   (damage          :accessor damage :documentation "Damage dealt to a crop upon contact."))
  (:documentation "Behaviour common to all bugs."))

(define-shader-entity bug-fly (bug animated-sprite facing-entity located-entity)
  ((sprite-data     :initform (asset 'farm 'bug-fly))
   ;; (movement-scheme :initform #'move-straight)
   (movement-scheme :initform #'move-sin-wave)
   (movement-speed  :initform (+ 0.25 (cl:random 0.25)))
   (facing          :initform :left)
   (health          :initform 2)
   (damage          :initform 1)))

(defmethod min-x ((bug-fly bug-fly))
  (- (vx (location bug-fly)) 7))
(defmethod max-x ((bug-fly bug-fly))
  (+ 8 (vx (location bug-fly))))
(defmethod min-y ((bug-fly bug-fly))
  (- (vy (location bug-fly)) 8))
(defmethod max-y ((bug-fly bug-fly))
  (+ 7 (vy (location bug-fly))))

;; --- Handlers --- ;;

(define-handler (bug tick :before) (fc)
  (let ((nearby-puff (collision-candidate bug *puffs*)))
    (when (and nearby-puff (overlapping? bug nearby-puff))
      (decf (health bug) +puff-damage+)
      (leave nearby-puff (container nearby-puff))
      (when (<= (health bug) 0)
        (leave bug (container bug)))))
  (funcall (movement-scheme bug) bug fc)
  ;; Automatic despawn when out of bounds.
  (when (not (in-x-bounds? (max-x bug)))
    (leave bug (container bug))))

;; --- Movement --- ;;

(defun move-straight (bug fc)
  "Move in a straight line in the bug's current direction."
  (declare (ignore fc))
  (incf (vx (location bug))
        (if (eq :left (facing bug))
            (* (movement-speed bug) -1)
            (* (movement-speed bug) 1))))

(defun move-sin-wave (bug fc)
  "Move along a simple sin wave in the bug's current direction."
  (let ((x-diff (if (eq :left (facing bug))
                    (* (movement-speed bug) -1)
                    (* (movement-speed bug) 1)))
        (y-diff (* 1/2 (sin (* pi 1/64 (vx (location bug)))))))
    (incf (vx (location bug)) x-diff)
    ;; Clamping Y to the bounds of the field.
    (when (or (and (> y-diff 0)
                   (<= (+ y-diff (max-y bug)) +field-max-y+))
              (and (< y-diff 0)
                   (>= (+ y-diff (min-y bug)) +field-min-y+)))
      (incf (vy (location bug)) y-diff))))
