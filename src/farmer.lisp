;;; Handling of all Farmer (player) logic.

(in-package :save-the-farm)

#+nil
(launch)

#+nil
(maybe-reload-scene)

;; --- Types --- ;;

(define-shader-entity farmer (animated-sprite facing-entity located-entity)
  ((sprite-data :initform (asset 'farm 'farmer))
   (stunned?    :initform nil :accessor stunned?)))

#+nil
(node :farmer (scene +main+))

(define-shader-entity puff (animated-sprite facing-entity located-entity)
  ((sprite-data :initform (asset 'farm 'puff))))

;; --- Handlers --- ;;

;; NOTE: Movement directions are oriented with the origin at the bottom left.
;; Location coordinates are also oriented in the same way, meaning we can
;; naively add the movement to the location and everything "just works".
;;
;; NOTE: This handler must be marked `:before' in order for the handler of the
;; parent `animated-sprite' class to be automatically called afterwards. It is
;; that handler that's performing the actual animation logic. Without doing so,
;; calls to `play' won't do anything and the sprite will be stuck in its first
;; frame.
(define-handler (farmer tick :before) (fc)
  (let ((nearby-bug (collision-candidate farmer *bugs*)))
    (when (and nearby-bug
               (not (stunned? farmer))
               (overlapping? farmer nearby-bug))
      (v:info :stf "Stunned!")
      (leave nearby-bug (container nearby-bug))
      (setf (stunned? farmer) fc)
      (play 'stunned farmer)))
  (let ((stunned-frame (stunned? farmer)))
    (cond ((and stunned-frame (> (- fc stunned-frame) +stun-timeout+))
           (setf (stunned? farmer) nil)
           (play 'idle farmer)
           (move-farmer farmer)
           (maybe-shoot-puff farmer fc))
          ;; Case: The farmer is stunned and cannot move.
          (stunned-frame nil)
          ;; Case: Everything is fine, move as normal.
          (t (move-farmer farmer)
             (maybe-shoot-puff farmer fc)))))

(defun move-farmer (farmer)
  (let ((movement (directional 'move)))
    ;; (incf (vx (location farmer)) (* dt speed (vx movement)))
    ;; (incf (vy (location farmer)) (* dt speed (vy movement)))
    (move-if-in-bounds movement farmer)
    ;; Flips the sprite if the player has pressed "left".
    (set-facing movement farmer)))

#+nil
(progn
  (observe! (slot-value *bugs* 'trial::%objects) :title "Bugs")
  (observe! (slot-value *bugs* 'trial::%count) :title "Bug Count")
  (observe! (t:transduce #'t:pass #'t:cons *bugs*) :title "Transduced"))

(define-handler (farmer shoot) ()
  (shoot-puff farmer))

(defun maybe-shoot-puff (farmer fc)
  (when (and (retained 'shoot)
             (zerop (mod fc 30)))
    (shoot-puff farmer)))

(defun shoot-puff (farmer)
  (when (not (stunned? farmer))
    (let* ((loc (vec (vx (location farmer)) (vy (location farmer)) 0))
           (puff (make-instance 'puff
                                :location loc
                                ;; If the farmer is facing left, the puff should move
                                ;; left, etc.
                                :facing (facing farmer))))
      (enter puff *puffs*))))

(define-handler (farmer kick) ()
  (play 'kick farmer))

(define-handler (farmer pause) ()
  (v:info :stf "Pausing.")
  (break))

;; TODO: 2024-09-06 Just for debugging - eventually remove.
#+nil
(define-handler (farmer gamepad-press :after) (button)
  (v:info :stf "Button: ~a, Type: ~a" button (type-of button)))

(define-handler (puff tick :before) ()
  (incf (vx (location puff))
        (if (eq :left (facing puff)) -1 1))
  (when (or (not (in-x-bounds? (max-x puff)))
            (not (in-x-bounds? (min-x puff))))
    (leave puff (container puff))))

;; --- Movement --- ;;

;; Confirmed via the "rainbow grid" test sprite and pencil-and-paper.
(defmethod min-x ((farmer farmer))
  (- (vx (location farmer)) 7))
(defmethod max-x ((farmer farmer))
  (+ 8 (vx (location farmer))))
(defmethod min-y ((farmer farmer))
  (- (vy (location farmer)) 8))
(defmethod max-y ((farmer farmer))
  (+ 7 (vy (location farmer))))

;; TODO: 2024-09-18 Move these.
(defmethod min-x ((puff puff))
  (vx (location puff)))
(defmethod max-x ((puff puff))
  (1+ (vx (location puff))))
(defmethod min-y ((puff puff))
  (vy (location puff)))
(defmethod max-y ((puff puff))
  (1+ (vy (location puff))))

(defun move-if-in-bounds (movement entity)
  (move-if-in-x-bounds movement entity)
  (move-if-in-y-bounds movement entity))

(defun move-if-in-x-bounds (movement entity)
  (cond ((and (> (vx movement) 0)
              (in-aisle-x-bounds? (max-x entity)))
         (incf (vx (location entity)) (vx movement)))
        ((and (< (vx movement) 0)
              (in-aisle-x-bounds? (min-x entity)))
         (incf (vx (location entity)) (vx movement)))))

(defun move-if-in-y-bounds (movement entity)
  (cond ((and (> (vy movement) 0)
              (in-aisle-y-bounds? (max-y entity)))
         (incf (vy (location entity)) (vy movement)))
        ((and (< (vy movement) 0)
              (in-aisle-y-bounds? (min-y entity)))
         (incf (vy (location entity)) (vy movement)))))
