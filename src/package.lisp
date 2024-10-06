(defpackage save-the-farm
  (:use :cl+trial :arrow-macros)
  (:shadow :main :launch)
  (:local-nicknames (:v :org.shirakumo.verbose)
                    (:gamepad :org.shirakumo.fraf.gamepad)
                    (:harmony :org.shirakumo.fraf.harmony)
                    (:trial-harmony :org.shirakumo.fraf.trial.harmony)
                    (:t :transducers))
  (:export #:main #:launch))

(in-package :save-the-farm)

(setf +app-system+ "save-the-farm")

;; --- Assets --- ;;

(define-pool farm :base #p"../../data/")
(define-asset (farm lemon)      sprite-data #p"sprites/lemon.json")
(define-asset (farm farmer)     sprite-data #p"sprites/farmer.json")
(define-asset (farm origin-dot) sprite-data #p"sprites/dot.json")
(define-asset (farm puff)       sprite-data #p"sprites/puff.json")
(define-asset (farm grid-test)  sprite-data #p"sprites/grid-test.json")
(define-asset (farm bug-fly)    sprite-data #p"sprites/bug-fly.json")
(define-asset (farm post)       sprite-data #p"sprites/post.json")
(define-asset (farm numbers)    sprite-data #p"sprites/numbers.json")
(define-asset (farm tilemap)    tile-data   #p"map/field.tmj")

;; --- Actions --- ;;

(define-action-set in-game)
(define-action move  (directional-action in-game))
(define-action shoot (in-game))
(define-action kick  (in-game))
(define-action pause (in-game))
(define-action start (in-game))

;; --- Global Containers --- ;;

;; As "bags" these become official Trial "node containers" and can be injected
;; into the scene. By keeping a global reference to them as well, we can easily
;; add to them, clear them, and call upon their contents for collision detection
;; without having to traverse the entire scene graph.
;;
;; The farmer himself is a simpler case and doesn't require a global; he can
;; easily be obtained via the `node' function and his name `:farmer'.
(defparameter *crops* nil)
(defparameter *bugs*  nil)
(defparameter *puffs* nil)

;; --- Grid and Pixel Coordinates --- ;;

(defparameter +grid-min-x+ 0)
(defparameter +grid-max-x+ 15)
(defparameter +grid-min-y+ 0)
(defparameter +grid-max-y+ 14)

;; The pixel-bounds of the field.
(defparameter +field-min-x+ -127)
(defparameter +field-max-x+ 128)
(defparameter +field-min-y+ -96)
(defparameter +field-max-y+ 79)

;; The pixel-bounds of the NES screen (not the user's window itself).
(defparameter +screen-min-x+ +field-min-x+)
(defparameter +screen-max-x+ +field-max-x+)
(defparameter +screen-min-y+ (- +field-min-y+ 32))
(defparameter +screen-max-y+ (+ +field-max-y+ 32))

(defun in-x-bounds? (x)
  "Is a given X location within the bounds of the entire field?"
  (< +field-min-x+ x +field-max-x+))

(defun in-y-bounds? (y)
  "Is a given Y location within the bounds of the entire field?"
  (< +field-min-y+ y +field-max-y+))

(defun pixel->grid (x y)
  "Given XY pixel coordinates, determine where on the 16x15 grid it is."
  ;; Subtracting by negative numbers shifts all points into a positive range
  ;; without needing to call `abs'.
  (let ((shifted-x (- x +screen-min-x+))
        (shifted-y (- y +screen-min-y+)))
    ;; Dividing by 16, since each grid block is 16x16 pixels.
    (values (floor (/ shifted-x 16))
            (floor (/ shifted-y 16)))))

;; The origin is at 7x8
#+nil
(pixel->grid 0 0)

#+nil
(let* ((scene  (scene +main+))
       (farmer (node :farmer scene))
       (dot    (make-instance 'dot :name :farmer-dot :location (location farmer))))
  (enter dot scene)
  (observe! (pixel->grid (vx (location farmer))
                         (vy (location farmer)))
            :title "Farmer Grid Loc"))

(defun grid->pixel (x y)
  "Given XY grid coordinates of the 16x15 grid screen, convert it to a pixel
location vector such that were a sprite's `location' set to that, its bounding
box and the bounding box of the grid tile would be perfectly aligned."
  (vec (+ -120 (* 16 x))
       (+ -120 (* 16 y))
       0))

(defun grid->min-x (grid-x)
  "Given grid coordinates, yield the minimum pixel-based X value that it contains.
In other words, the common X value of the left side of its bounding box."
  (+ -127 (* 16 grid-x)))

(defun grid->max-x (grid-x)
  "Given grid coordinates, yield the maximum pixel-based X value that it contains.
In other words, the common X value of the right side of its bounding box."
  (+ -112 (* 16 grid-x)))

(defun grid->min-y (grid-y)
  "Given grid coordinates, yield the minimum pixel-based Y value that it contains.
In other words, the common Y value of the bottom side of its bounding box."
  (+ -128 (* 16 grid-y)))

(defun grid->max-y (grid-y)
  "Given grid coordinates, yield the maximum pixel-based Y value that it contains.
In other words, the common Y value of the top side of its bounding box."
  (+ -113 (* 16 grid-y)))

;; The pixel-bounds of the farmer's walkable area; his "aisle".
(defparameter +aisle-min-x+ (grid->min-x 4))
(defparameter +aisle-max-x+ (grid->max-x 6))
(defparameter +aisle-min-y+ (grid->min-y 3))
(defparameter +aisle-max-y+ (grid->max-y 11))

(defun in-aisle-x-bounds? (x)
  "Is a given X location within the bounds of the farmer's walkable aisle? Touching
is considered out of bounds, thus preventing movement."
  (< +aisle-min-x+ x +aisle-max-x+))

(defun in-aisle-y-bounds? (y)
  "Is a given Y location within the bounds of the farmer's walkable aisle? Touching
is considered out of bounds, thus preventing movement."
  (< +aisle-min-y+ y +aisle-max-y+))

;; --- Entities --- ;;

(defclass facing-entity (scaled-entity)
  ((facing :initarg :facing :initform :right :accessor facing))
  (:documentation "Things that can face left or right in a meaningful way."))

(define-handler (facing-entity tick :after) ()
  (case (facing facing-entity)
    (:left  (setf (vx (scaling facing-entity)) -1))
    (:right (setf (vx (scaling facing-entity)) +1))))

(defun set-facing (movement entity)
  "Assumes that the sprite naturally faces to the right, and flips it if it happens
to be moving to the left."
  (cond ((> (vx movement) 0) (setf (facing entity) :right))
        ((< (vx movement) 0) (setf (facing entity) :left))))

;; --- Misc. Globals --- ;;

(defparameter +framerate+ 60)

(defparameter +stun-timeout+ 90
  "The number of frames to maintain the farmer's stun status.")

(defparameter +puff-damage+ 1
  "The amount of damage inflicted by a farmer's gas puff.")

(defparameter *score* nil
  "The player's current score.")

(defparameter *game-over* nil
  "Is the game over?")

;; --- Utilities --- ;;

(defun random-elt (vector)
  "Yield a random element from a vector."
  (let ((ix (cl:random (length vector))))
    (aref vector ix)))
