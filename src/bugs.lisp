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
   (damage          :accessor damage :documentation "Damage dealt to a crop upon contact.")
   (orig-y          :accessor orig-y :initarg :orig-y :documentation "The originally assigned Y-value. Used in complex movement equations."))
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

(define-handler (bug tick :before) ()
  (let ((nearby-puff (collision-candidate bug *puffs*)))
    (when (and nearby-puff (overlapping? bug nearby-puff))
      (decf (health bug) +puff-damage+)
      (leave nearby-puff (container nearby-puff))
      (when (<= (health bug) 0)
        (leave bug (container bug)))))
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

(defun move-sin-wave (bug)
  "Move along a simple sin wave in the bug's current direction."
  (let ((x-diff (if (eq :left (facing bug))
                    (* (movement-speed bug) -1)
                    (* (movement-speed bug) 1)))
        (y-diff (* 16 (sin (* pi 1/32 (vx (location bug)))))))
    (incf (vx (location bug)) x-diff)
    (let ((new-y (+ y-diff (orig-y bug))))
      ;; Clamping Y to the bounds of the field.
      (when (or (<= new-y +field-max-y+)
                (>= new-y +field-min-y+))
        (setf (vy (location bug)) new-y)))))

(defun move-at-farmer ()
  "Yield a lambda that captures the `location' of the farmer and has the bug try to
always move toward him."
  (move-at (node :farmer (scene +main+))))

(defun move-at-crop ()
  "Yield a lambda that captures the `location' of a random crop and has the bug
always move toward it."
  (if (zerop (length *crops*))
      #'move-straight
      (let* ((ix (cl:random (length *crops*)))
             ;; HACK: 2024-09-26 Direct access to inner vector.
             (entity (aref (slot-value *crops* 'trial::%objects) ix)))
        (move-at entity))))

(defun move-at (thing)
  "Yield a lambda that captures the `location' of some located entity and has the
bug try to always move toward it."
  (let ((loc (location thing))
        ;; A normal vector that represents the bug's "heading". See below.
        (vtr (vec -1 0)))
    (lambda (bug)
      ;; (1) Overall, the bug is seeing where the entity is and shifts its
      ;; direction to between its current heading and where the entity stands.
      (let* ((bet-x (- (vx loc) (vx (location bug))))
             (bet-y (- (vy loc) (vy (location bug)))))
        ;; (2) If the bug is close enough, don't attempt to course-correct. This
        ;; creates a "hovering" effect where the bug initially goes too far,
        ;; then turns back around.
        (if (<= (sqrt (+ (expt bet-x 2) (expt bet-y 2))) 16)
            (progn (incf (vx (location bug)) (vx vtr))
                   (incf (vy (location bug)) (vy vtr)))
            ;; (3) The vector between the bug's current heading and a straight
            ;; line from it to the entity.
            (let* ((avg-x (/ (+ bet-x (vx vtr)) 2))
                   (avg-y (/ (+ bet-y (vy vtr)) 2))
                   ;; (4) Renormalize.
                   (mag   (sqrt (+ (expt avg-x 2) (expt avg-y 2))))
                   (nor-x (/ avg-x mag))
                   (nor-y (/ avg-y mag))
                   ;; (5) Slow the bug back down or he zooms towards the target.
                   (slo-x (* (movement-speed bug) nor-x))
                   (slo-y (* (movement-speed bug) nor-y)))
              (setf (vx vtr) slo-x)
              (setf (vy vtr) slo-y)
              (incf (vx (location bug)) slo-x)
              (incf (vy (location bug)) slo-y)))))))

#+nil
(let* ((loc (grid->pixel +grid-max-x+ 7))
       (bug (make-instance 'bug-fly :orig-y (vy (location loc)))))
  (setf (movement-scheme bug) (move-at-crop))
  (enter bug *bugs*)
  (setf (location bug) loc))

#+nil
(maybe-reload-scene)
