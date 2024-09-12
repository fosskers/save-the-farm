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

(defparameter +max-x+ 128)
(defparameter +max-y+ 79)
(defparameter +min-x+ -127)
(defparameter +min-y+ -96)

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
