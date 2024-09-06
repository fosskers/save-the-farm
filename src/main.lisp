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

(define-shader-entity farmer (animated-sprite located-entity)
  ((sprite-data :initform (asset 'farm 'farmer))))

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
(define-handler (farmer tick :before) (tt fc)
  (let ((movement (directional 'move)))
    ;; (incf (vx (location farmer)) (* dt speed (vx movement)))
    ;; (incf (vy (location farmer)) (* dt speed (vy movement)))
    (incf (vx (location farmer)) (vx movement))
    (incf (vy (location farmer)) (vy movement))
    (when (moved? movement)
      (v:info :stf "Location: ~a, tt: ~a, fc: ~a" (location farmer) tt fc))))

#+nil
(find-class 'located-entity)

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

#+nil
(find-class 'sidescroll-camera)

(defmethod setup-scene ((main stf-main) scene)
  (enter (make-instance 'tile-layer :tile-data (asset 'farm 'tilemap) :name :field) scene)
  (enter (make-instance 'lemon :name :lemon) scene)
  ;; (enter (make-instance 'farmer :name :farmer) scene)
  (enter (make-instance 'dot :name :origin-dot) scene)
  (enter (make-instance 'dot :name :corner-dot) scene)
  ;; NOTE: No need to manually setf the camera slot of the `scene', as an
  ;; `:after' defmethod on camera+scene already does this.
  ;; (enter (make-instance 'sidescroll-camera :zoom 5.0 :target (node :farmer scene)) scene)
  (enter (make-instance 'sidescroll-camera :zoom 3.0 :name :camera) scene)
  (enter (make-instance 'render-pass) scene)
  (v:info :stf "Dot Location: ~a" (location (node :origin-dot scene))))

;; NOTE: The sidescroll-camera insists on being at the origin. Even if you move
;; it here, it automatically glides back to the origin across the next second or
;; so.
(defmethod setup-scene :after ((main stf-main) scene)
  (let ((corner-dot (node :corner-dot scene))
        (lemon (node :lemon scene)))
    (setf (location corner-dot) (vec -127 -128 0))
    (setf (location lemon) (grid->pixel 1 7))))

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
