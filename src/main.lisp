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

(defclass stf-main (trial-harmony:settings-main trial:main)
  ())

;; `base' is somewhat sensitive. If you give it an absolute path it'll believe
;; it as is, but relative paths seem to be dead-set on staying within `src/' and
;; don't result in paths relative to the project root.
(define-pool farm :base #p"/home/colin/code/common-lisp/save-the-farm/data/")

(define-action-set in-game)
(define-action move (directional-action in-game))
(define-action shoot (in-game))

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
