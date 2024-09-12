;;; Handling of all Farmer (player) logic.

(in-package :save-the-farm)

#+nil
(launch)

;; --- Types --- ;;

(defclass facing-entity (scaled-entity)
  ((facing :initarg :facing :initform :right :accessor facing))
  (:documentation "Things that can face left or right in a meaningful way."))

(define-shader-entity farmer (animated-sprite facing-entity located-entity)
  ((sprite-data :initform (asset 'farm 'farmer))))

(define-shader-entity puff (animated-sprite facing-entity located-entity)
  ((sprite-data :initform (asset 'farm 'puff))))

#+nil
(find-class 'puff)

;; --- Handlers --- ;;

(define-handler (facing-entity tick :after) ()
  (case (facing facing-entity)
    (:left  (setf (vx (scaling facing-entity)) -1))
    (:right (setf (vx (scaling facing-entity)) +1))))

;; NOTE: Movement directions are oriented with the origin at the bottom left.
;; Location coordinates are also oriented in the same way, meaning we can
;; naively add the movement to the location and everything "just works".
;;
;; NOTE: This handler must be marked `:before' in order for the handler of the
;; parent `animated-sprite' class to be automatically called afterwards. It is
;; that handler that's performing the actual animation logic. Without doing so,
;; calls to `play' won't do anything and the sprite will be stuck in its first
;; frame.
(define-handler (farmer tick :before) ()
  (let ((movement (directional 'move)))
    ;; (when (moved? movement)
    ;; (incf (vx (location farmer)) (* dt speed (vx movement)))
    ;; (incf (vy (location farmer)) (* dt speed (vy movement)))
    (cond ((and (> (vx movement) 0)
                (in-x-bounds? (max-x farmer)))
           (incf (vx (location farmer)) (vx movement)))
          ((and (< (vx movement) 0)
                (in-x-bounds? (min-x farmer)))
           (incf (vx (location farmer)) (vx movement))))
    (cond ((and (> (vy movement) 0)
                (in-y-bounds? (max-y farmer)))
           (incf (vy (location farmer)) (vy movement)))
          ((and (< (vy movement) 0)
                (in-y-bounds? (min-y farmer)))
           (incf (vy (location farmer)) (vy movement))))
    ;; Flips the sprite if the player has pressed "left".
    (set-facing movement farmer)))

(define-handler (farmer shoot) ()
  (enter (make-instance 'puff
                        :location (let ((loc (location farmer)))
                                    (vec (vx loc) (vy loc) 0))
                        ;; If the farmer is facing left, the puff should move
                        ;; left, etc.
                        :facing (facing farmer))
         (container farmer)))

(define-handler (farmer kick) ()
  (play 'kick farmer))

(define-handler (farmer reset) ()
  (v:info :stf "Resetting farmer position.")
  (setf (location farmer) (vec 0 0 0)))

;; TODO: 2024-09-06 Just for debugging - eventually remove.
(define-handler (farmer gamepad-press :after) (button)
  (v:info :stf "Button: ~a, Type: ~a" button (type-of button)))

(define-handler (puff tick :before) ()
  (incf (vx (location puff))
        (if (eq :left (facing puff)) -1 1))
  (when (not (in-x-bounds? (vx (location puff))))
    (leave puff (container puff))))

;; --- Utils --- ;;

(defun set-facing (movement entity)
  "Assumes that the sprite naturally faces to the right, and flips it if it happens
to be moving to the left."
  (cond ((> (vx movement) 0) (setf (facing entity) :right))
        ((< (vx movement) 0) (setf (facing entity) :left))))

(defgeneric max-x (entity))

(defmethod max-x ((farmer farmer))
  (+ 9 (vx (location farmer))))

(defgeneric min-x (entity))

(defmethod min-x ((farmer farmer))
  (- (vx (location farmer))
     8))

(defgeneric max-y (entity))

(defmethod max-y ((farmer farmer))
  (+ 8 (vy (location farmer))))

(defgeneric min-y (entity))

(defmethod min-y ((farmer farmer))
  (- (vy (location farmer))
     9))
