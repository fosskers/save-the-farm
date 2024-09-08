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

(defclass stf-main (trial-harmony:settings-main)
  ())

(define-pool farm :base #p"../../data/")
(define-asset (farm lemon) sprite-data #p"sprites/lemon.json")
(define-asset (farm farmer) sprite-data #p"sprites/farmer.json")
(define-asset (farm origin-dot) sprite-data #p"sprites/dot.json")
(define-asset (farm tilemap) tile-data #p"map/field.tmj")

(define-shader-entity farmer (animated-sprite located-entity transformed-entity)
  ((sprite-data :initform (asset 'farm 'farmer))
   (direction :initarg :direction :initform 1 :accessor direction
              :type integer :documentation "-1 for left, +1 for right")))

(define-shader-entity dot (animated-sprite located-entity)
  ((sprite-data :initform (asset 'farm 'origin-dot))))

(define-shader-entity lemon (animated-sprite located-entity)
  ((sprite-data :initform (asset 'farm 'lemon))))

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
    (cond ((> (vx movement) 0) (setf (direction farmer) 1))
          ((< (vx movement) 0) (setf (direction farmer) -1)))))

;; (when (moved? movement)
;;   (v:info :stf "Location: ~a, tt: ~a, fc: ~a" (location farmer) tt fc))))

(defmethod apply-transforms progn ((farmer farmer))
  ;; FIXME Turning left makes him disappear.
  (scale-by (direction farmer) 1 1))

#+nil
(find-class 'transformed)

(define-handler (farmer kick) ()
  (play 'kick farmer))

(define-handler (farmer reset) ()
  (v:info :stf "Resetting farmer position.")
  (setf (location farmer) (vec 0 0 0)))

;; TODO: 2024-09-06 Just for debugging - eventually remove.
(define-handler (farmer gamepad-press :after) (button)
  (v:info :stf "Button: ~a, Type: ~a" button (type-of button)))

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
  (enter (make-instance 'dot :name :corner-dot) scene)
  ;; NOTE: No need to manually setf the camera slot of the `scene', as an
  ;; `:after' defmethod on camera+scene already does this.
  ;; (enter (make-instance 'sidescroll-camera :zoom 5.0 :target (node :farmer scene)) scene)
  (enter (make-instance 'sidescroll-camera :zoom 3.0 :name :camera) scene)
  (enter (make-instance 'render-pass) scene)
  (enter (make-instance 'farmer :name :farmer) scene))

;; NOTE: The sidescroll-camera insists on being at the origin. Even if you move
;; it here, it automatically glides back to the origin across the next second or
;; so.
(defmethod setup-scene :after ((main stf-main) scene)
  (let ((corner-dot (node :corner-dot scene))
        (farmer     (node :farmer scene)))
    (setf (location corner-dot) (vec -127 -128 0))
    (setf (location farmer) (grid->pixel 4 7))
    (spawn-crops 'lemon scene)))

#+nil
(maybe-reload-scene)

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
