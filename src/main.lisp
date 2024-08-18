(defpackage save-the-farm
  (:use :cl+trial)
  (:shadow :main :launch)
  (:local-nicknames (:v :org.shirakumo.verbose))
  (:export :main :launch))

(in-package :save-the-farm)

(setf +app-system+ "save-the-farm")

(defclass main (trial:main)
  ())

(define-asset (trial cat) image
    #p"cat.png")

;; Defining custom actions.
(define-action-set in-game)
(define-action move (directional-action in-game))
(define-action hide (in-game))

(define-shader-entity my-cube (vertex-entity colored-entity textured-entity transformed-entity listener)
  ((vertex-array :initform (// 'trial 'unit-cube))
   (texture :initform (// 'trial 'cat))
   (color :initform (vec 1 1 1 1))))

(define-handler (my-cube hide) ()
  (setf (vw (color my-cube)) (if (= (vw (color my-cube)) 1.0) 0.1 1.0)))

(define-handler (my-cube tick) (tt dt)
  (setf (orientation my-cube) (qfrom-angle +vy+ tt))
  (let ((movement (directional 'move))
        (speed 10.0))
    (incf (vx (location my-cube)) (* dt speed (- (vx movement))))
    (incf (vz (location my-cube)) (* dt speed (vy movement)))))

(defmethod setup-scene ((main main) scene)
  (enter (make-instance 'my-cube) scene)
  (enter (make-instance '3d-camera :location (vec 0 0 -3)) scene)
  (enter (make-instance 'render-pass) scene))

(defun launch (&rest args)
  "Convenience function for launching the game. Also possible to do any other
  pre-launch initialisation that may be necessary."
  ;; From the Trial quickstart:
  ;;
  ;; > We bind the *package* to the one of our current source file to ensure that
  ;; > all symbols in the keymap are resolved to the ones from our package.
  (let ((*package* #.*package*))
    (load-keymap)
    ;; Register our custom actions.
    (setf (active-p (action-set 'in-game)) t)
    (apply #'trial:launch 'main args)))

#+nil
(launch)

#+nil
(maybe-reload-scene)
