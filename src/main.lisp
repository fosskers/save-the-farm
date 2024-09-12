(defpackage save-the-farm
  (:use :cl+trial :arrow-macros)
  (:shadow :main :launch)
  (:local-nicknames (:v :org.shirakumo.verbose)
                    (:gamepad :org.shirakumo.fraf.gamepad)
                    (:harmony :org.shirakumo.fraf.harmony)
                    (:trial-harmony :org.shirakumo.fraf.trial.harmony)
                    (:t :transducers))
  (:export :main :launch))

(in-package :save-the-farm)

(setf +app-system+ "save-the-farm")

(defparameter +max-x+ 128)
(defparameter +max-y+ 79)
(defparameter +min-x+ -127)
(defparameter +min-y+ -96)

(defclass stf-main (trial-harmony:settings-main)
  ())

(define-pool farm :base #p"../../data/")
(define-asset (farm lemon) sprite-data #p"sprites/lemon.json")
(define-asset (farm farmer) sprite-data #p"sprites/farmer.json")
(define-asset (farm origin-dot) sprite-data #p"sprites/dot.json")
(define-asset (farm puff) sprite-data #p"sprites/puff.json")
(define-asset (farm tilemap) tile-data #p"map/field.tmj")

(defclass facing-entity (scaled-entity)
  ((facing :initarg :facing :initform :right :accessor facing))
  (:documentation "Things that can face left or right in a meaningful way."))

(define-shader-entity farmer (animated-sprite facing-entity located-entity)
  ((sprite-data :initform (asset 'farm 'farmer))))

(define-shader-entity puff (animated-sprite facing-entity located-entity)
  ((sprite-data :initform (asset 'farm 'puff))))

#+nil
(find-class 'puff)

(define-shader-entity dot (animated-sprite located-entity)
  ((sprite-data :initform (asset 'farm 'origin-dot))))

(define-shader-entity lemon (animated-sprite located-entity)
  ((sprite-data :initform (asset 'farm 'lemon))))

(defun set-facing (movement entity)
  "Assumes that the sprite naturally faces to the right, and flips it if it happens
to be moving to the left."
  (cond ((> (vx movement) 0) (setf (facing entity) :right))
        ((< (vx movement) 0) (setf (facing entity) :left))))

(define-action-set in-game)
(define-action move (directional-action in-game))
(define-action shoot (in-game))
(define-action kick (in-game))
(define-action reset (in-game))

(defun moved? (movement)
  "Did movement occur since the last tick?"
  (or (> (vx movement) 0)
      (< (vx movement) 0)
      (> (vy movement) 0)
      (< (vy movement) 0)))

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
    (incf (vx (location farmer)) (vx movement))
    (incf (vy (location farmer)) (vy movement))
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

(define-handler (puff tick :before) ()
  (incf (vx (location puff))
        (if (eq :left (facing puff)) -1 1))
  (when (not (in-x-bounds? (vx (location puff))))
    (leave puff (container puff))))

;; TODO: 2024-09-06 Just for debugging - eventually remove.
(define-handler (farmer gamepad-press :after) (button)
  (v:info :stf "Button: ~a, Type: ~a" button (type-of button)))

(defun in-x-bounds? (x)
  "Is a given X location within the bounds of the field?"
  (<= +min-x+ x +max-x+))

(defun in-y-bounds? (y)
  "Is a given Y location within the bounds of the field?"
  (<= +min-y+ y +max-y+))

(defun grid->pixel (x y)
  "Given XY grid coordinates of the 16x15 grid screen, convert it to a pixel
location vector such that were a sprite's `location' set to that, its bounding
box and the bounding box of the grid tile would be perfectly aligned."
  (vec (+ -120 (* 16 x))
       (+ -120 (* 16 y))
       0))

(defun spawn-crops (crop-type scene)
  (let ((locs '((1 . 10) (1 . 9) (1 . 8) (1 . 7) (1 . 6) (1 . 5) (1 . 4)
                (2 . 10) (2 . 9) (2 . 8) (2 . 7) (2 . 6) (2 . 5) (2 . 4))))
    (dolist (loc locs)
      (let ((crop (make-instance crop-type)))
        (enter crop scene)
        (setf (location crop) (grid->pixel (car loc) (cdr loc)))))))

(defmethod setup-scene ((main stf-main) scene)
  (enter (make-instance 'tile-layer :tile-data (asset 'farm 'tilemap) :name :field) scene)
  (enter (make-instance 'dot :name :origin-dot) scene)
  (enter (make-instance 'dot :name :bottom-left-dot) scene)
  (enter (make-instance 'dot :name :bottom-right-dot) scene)
  (enter (make-instance 'dot :name :top-left-dot) scene)
  (enter (make-instance 'dot :name :top-right-dot) scene)
  ;; NOTE: No need to manually setf the camera slot of the `scene', as an
  ;; `:after' defmethod on camera+scene already does this.
  ;; (enter (make-instance 'sidescroll-camera :zoom 5.0 :target (node :farmer scene)) scene)
  (enter (make-instance 'sidescroll-camera :zoom 3.0 :name :camera) scene)
  (enter (make-instance 'render-pass) scene)
  (enter (make-instance 'farmer :name :farmer) scene)
  ;; Necessary to prevent a crash when spawning the first puff.
  (preload (make-instance 'puff) scene))

;; NOTE: The sidescroll-camera insists on being at the origin. Even if you move
;; it here, it automatically glides back to the origin across the next second or
;; so.
(defmethod setup-scene :after ((main stf-main) scene)
  (let ((bottom-left-dot  (node :bottom-left-dot scene))
        (bottom-right-dot (node :bottom-right-dot scene))
        (top-left-dot     (node :top-left-dot scene))
        (top-right-dot    (node :top-right-dot scene))
        (farmer           (node :farmer scene)))
    ;; These four dot locations represent the bounds of the (NES) screen.
    ;; Projectiles should:
    ;;
    ;; - Not render past the edge of the map, culling gradually as their pixels
    ;; pass the edge.
    ;; - Fully despawn when ther left-most pixels have passed beyond the edge.
    (setf (location bottom-left-dot) (vec -127 -96 0))
    (setf (location bottom-right-dot) (vec 128 -96 0))
    (setf (location top-left-dot) (vec -127 79 0))
    (setf (location top-right-dot) (vec 128 79 0))
    (setf (location farmer) (grid->pixel 4 7))
    (spawn-crops 'lemon scene)))

#+nil
(maybe-reload-scene)

(defmethod setup-rendering :after ((main stf-main))
  (disable-feature :cull-face))

(defun launch (&rest args)
  "Convenience function for launching the game. Also possible to do any other
  pre-launch initialisation that may be necessary."
  (v:info :stf "Launching Save the Farm.")
  ;; From the Trial quickstart:
  ;;
  ;; > We bind the *package* to the one of our current source file to ensure that
  ;; > all symbols in the keymap are resolved to the ones from our package.
  (let ((*package* #.*package*))
    (load-keymap :reset t)
    ;; Register our custom actions.
    (setf (active-p (action-set 'in-game)) t)
    (apply #'trial:launch 'stf-main args))
  (v:info :stf "Exiting launch function."))

#+nil
(launch)
