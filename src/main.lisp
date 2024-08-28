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

;; `base' is somewhat sensitive. If you give it an absolute path it'll believe
;; it as is, but relative paths seem to be dead-set on staying within `src/' and
;; don't result in paths relative to the project root.
(define-pool farm :base #p"/home/colin/code/common-lisp/save-the-farm/data/")
(define-asset (farm tilemap) tile-data #p"map/field.tmj")

(define-action-set in-game)
(define-action move (directional-action in-game))
(define-action shoot (in-game))

(defmethod setup-scene ((main stf-main) scene)
  (enter (make-instance 'tile-layer :tile-data (asset 'farm 'tilemap)) scene)
  (enter (make-instance 'sidescroll-camera :zoom 12.0) scene)
  (enter (make-instance 'render-pass) scene))

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
