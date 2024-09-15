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

;; The pixel-bounds of the field.
(defparameter +max-x+ 128)
(defparameter +max-y+ 79)
(defparameter +min-x+ -127)
(defparameter +min-y+ -96)

;; The pixel-bounds of the farmer's walkable area; his "aisle".
(defparameter +aisle-min-x+ (grid->min-x 4))
(defparameter +aisle-max-x+ (grid->max-x 6))
(defparameter +aisle-min-y+ (grid->min-y 2))
(defparameter +aisle-max-y+ (grid->max-y 12))

(define-pool farm :base #p"../../data/")
(define-asset (farm lemon) sprite-data #p"sprites/lemon.json")
(define-asset (farm farmer) sprite-data #p"sprites/farmer.json")
(define-asset (farm origin-dot) sprite-data #p"sprites/dot.json")
(define-asset (farm puff) sprite-data #p"sprites/puff.json")
(define-asset (farm tilemap) tile-data #p"map/field.tmj")

(define-action-set in-game)
(define-action move (directional-action in-game))
(define-action shoot (in-game))
(define-action kick (in-game))
(define-action reset (in-game))

(defun in-x-bounds? (x)
  "Is a given X location within the bounds of the entire field?"
  (<= +min-x+ x +max-x+))

(defun in-y-bounds? (y)
  "Is a given Y location within the bounds of the entire field?"
  (<= +min-y+ y +max-y+))

(defun in-aisle-x-bounds? (x)
  "Is a given X location within the bounds of the farmer's walkable aisle?"
  (<= +aisle-min-x+ x +aisle-max-x+))

(defun in-aisle-y-bounds? (y)
  "Is a given Y location within the bounds of the farmer's walkable aisle?"
  (<= +aisle-min-y+ y +aisle-max-y+))

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

#+nil
(let ((dot (make-instance 'dot :name :tempdot)))
  (enter dot (scene +main+))
  (setf (location dot) (vec 0 (grid->max-y 1) 0)))

#+nil
(let* ((scene (scene +main+))
       (dot (node :tempdot scene)))
  (leave dot scene))
