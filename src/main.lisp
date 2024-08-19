(defpackage save-the-farm
  (:use :cl+trial)
  (:shadow :main :launch)
  (:local-nicknames (:v :org.shirakumo.verbose))
  (:export :main :launch))

(in-package :save-the-farm)

(setf +app-system+ "save-the-farm")

(defclass main (trial:main)
  ())

;; `base' is somewhat sensitive. If you give it an absolute path it'll believe
;; it as is, but relative paths seem to be dead-set on staying within `src/' and
;; don't result in paths relative to the project root.
(define-pool farm :base #p"/home/colin/code/common-lisp/save-the-farm/data/")

(define-asset (farm jack) image
    #p"jack.jpg")

(define-asset (trial cat) image
    #p"cat.png")

;; Defining custom actions.
(define-action-set in-game)
(define-action move (directional-action in-game))
(define-action hide (in-game))
(define-action shoot (in-game))

(define-shader-entity bullet (vertex-entity colored-entity transformed-entity listener)
  ((vertex-array :initform (// 'trial 'unit-sphere))
   (color :initform (vec 1 0 0 1))
   (velocity :initform (vec 0 0 0) :initarg :velocity :accessor velocity)))

(define-handler (bullet tick) (dt)
  (nv+* (location bullet) (velocity bullet) dt))

(define-shader-entity my-cube (vertex-entity colored-entity textured-entity transformed-entity listener)
  ((vertex-array :initform (// 'trial 'unit-cube))
   (texture :initform (// 'farm 'jack))
   (color :initform (vec 1 1 1 1))))

#+nil
(define-handler (my-cube key-press) (key)
  (case key
    (:f (enter (make-instance 'bullet
                              :location (location my-cube)
                              :scaling (vec 0.1 0.1 0.1)
                              :velocity (nv* (q* (orientation my-cube) +vx3+) 5))
               (container my-cube)))))

(define-handler (my-cube gamepad-press) (button)
  (format t "Button: ~a, Type: ~a~%" button (type-of button)))

#+nil
(find-class 'gamepad-press)

(define-handler (my-cube shoot) ()
  (enter (make-instance 'bullet
                        :location (location my-cube)
                        :scaling (vec 0.1 0.1 0.1)
                        :velocity (nv* (q* (orientation my-cube) +vx3+) 5))
         (container my-cube)))

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
  (enter (make-instance 'render-pass) scene)
  ;; Doesn't atually add a bullet to the scene graph, but does everything else
  ;; necessary to prepare this entity to be eventually loaded later.
  (preload (make-instance 'bullet) scene))

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

;; Dynamically reload the Keymap and make sure the global one is resaved, or
;; else local changes won't be reapplied on subsequent launches.
#+nil
(let ((*package* #.*package*))
  (load-keymap :reset t))
