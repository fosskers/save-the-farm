(defpackage save-the-farm
  (:use :cl+trial :arrow-macros)
  (:shadow :main :launch)
  (:local-nicknames (:v :org.shirakumo.verbose)
                    (:gamepad :org.shirakumo.fraf.gamepad)
                    (:harmony :org.shirakumo.fraf.harmony)
                    (:trial-harmony :org.shirakumo.fraf.trial.harmony))
  (:export :main :launch))

(in-package :save-the-farm)

(setf +app-system+ "save-the-farm")

(defclass stf-main (trial-harmony:settings-main)
  ())

(define-pool farm :base #p"../../data/")
(define-asset (farm lemon) sprite-data #p"sprites/lemon.json")
(define-asset (farm farmer) sprite-data #p"sprites/farmer.json")
(define-asset (farm tilemap) tile-data #p"map/field.tmj")

(define-shader-entity farmer (animated-sprite located-entity)
  ((sprite-data :initform (asset 'farm 'farmer))))

(define-shader-entity my-lemon (animated-sprite located-entity)
  ((sprite-data :initform (asset 'farm 'lemon))))

(define-action-set in-game)
(define-action move (directional-action in-game))
(define-action shoot (in-game))

;; NOTE: Movement directions are oriented with the origin at the bottom left.
;; Location coordinates are also oriented in the same way, meaning we can
;; naively add the movement to the location and everything "just works".
(define-handler (farmer tick) (dt)
  (let ((movement (directional 'move))
        (speed 10.0))
    (incf (vx (location farmer)) (* dt speed (vx movement)))
    (incf (vy (location farmer)) (* dt speed (vy movement)))))

(defmethod setup-scene ((main stf-main) scene)
  (enter (make-instance 'tile-layer :tile-data (asset 'farm 'tilemap)) scene)
  ;; (enter (make-instance 'my-lemon :name :lemon) scene)
  (enter (make-instance 'farmer :name :farmer) scene)
  ;; NOTE: No need to manually setf the camera slot of the `scene', as an
  ;; `:after' defmethod on camera+scene already does this.
  ;; (enter (make-instance 'sidescroll-camera :zoom 5.0 :target (node :farmer scene)) scene)
  (enter (make-instance 'sidescroll-camera :zoom 5.0) scene)
  (enter (make-instance 'render-pass) scene))

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
    (load-keymap)
    ;; Register our custom actions.
    (setf (active-p (action-set 'in-game)) t)
    (apply #'trial:launch 'stf-main args)))

#+nil
(launch)
