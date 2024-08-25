(in-package :save-the-farm)

(define-asset (farm jack) image
    #p"jack.jpg")

(define-asset (farm meow) trial-harmony:sound
    #p"meow.mp3")

(defmethod stage :after ((jack my-cube) (area staging-area))
  (stage (// 'farm 'meow) area))

#+nil
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

#+nil
(define-handler (my-cube gamepad-press) (button)
  (v:info :stf "Button: ~a, Type: ~a" button (type-of button)))

#+nil
(define-handler (my-cube gamepad-move) ()
  (format t "Movement: ~a~%" gamepad-move))

(define-handler (my-cube shoot) ()
  (enter (make-instance 'bullet
                        :location (location my-cube)
                        :scaling (vec 0.1 0.1 0.1)
                        :velocity (nv* (q* (orientation my-cube) +vx3+) 5))
         (container my-cube)))

(define-handler (my-cube hide) ()
  (setf (vw (color my-cube)) (if (= (vw (color my-cube)) 1.0) 0.1 1.0))
  (harmony:play (// 'farm 'meow)))

(define-handler (my-cube tick) (tt dt)
  (setf (orientation my-cube) (qfrom-angle +vy+ tt))
  (let ((movement (directional 'move))
        (speed 10.0))
    (incf (vx (location my-cube)) (* dt speed (- (vx movement))))
    (incf (vz (location my-cube)) (* dt speed (vy movement)))))

(defmethod setup-scene ((main stf-main) scene)
  (enter (make-instance 'my-cube) scene)
  (enter (make-instance '3d-camera :location (vec 0 0 -3)) scene)
  (enter (make-instance 'render-pass) scene)
  ;; (enter (make-instance 'fps-counter) scene)
  ;; Doesn't atually add a bullet to the scene graph, but does everything else
  ;; necessary to prepare this entity to be eventually loaded later.
  (preload (make-instance 'bullet) scene))

