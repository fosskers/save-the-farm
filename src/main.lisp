(in-package :save-the-farm)

(defparameter *game-level* +level-1+)

(defclass stf-main (trial-harmony:settings-main)
  ())

(define-shader-entity dot (animated-sprite located-entity)
  ((sprite-data :initform (asset 'farm 'origin-dot))))

#+nil
(defun moved? (movement)
  "Did movement occur since the last tick?"
  (or (> (vx movement) 0)
      (< (vx movement) 0)
      (> (vy movement) 0)
      (< (vy movement) 0)))

(defmethod setup-scene ((main stf-main) scene)
  (setf *crops* (make-instance 'bag))
  (setf *bugs* (make-instance 'bag))
  (setf *puffs* (make-instance 'bag))
  (enter (make-instance 'tile-layer :tile-data (asset 'farm 'tilemap) :name :field) scene)
  (enter (make-instance 'dot :name :origin-dot) scene)
  (enter (make-instance 'dot :name :bottom-left-dot) scene)
  (enter (make-instance 'dot :name :bottom-right-dot) scene)
  (enter (make-instance 'dot :name :top-left-dot) scene)
  (enter (make-instance 'dot :name :top-right-dot) scene)
  (enter (make-instance 'dot :name :truly-bottom-dot) scene)
  (enter (make-instance 'dot :name :truly-top-dot) scene)
  ;; NOTE: No need to manually setf the camera slot of the `scene', as an
  ;; `:after' defmethod on camera+scene already does this.
  ;; (enter (make-instance 'sidescroll-camera :zoom 5.0 :target (node :farmer scene)) scene)
  (enter (make-instance 'sidescroll-camera :zoom 3.0 :name :camera) scene)
  (enter (make-instance 'render-pass) scene)
  (enter (make-instance 'farmer :name :farmer) scene)
  (enter *crops* scene)
  (enter *bugs* scene)
  (enter *puffs* scene)
  (enter (make-instance 'display-controller) scene)
  (enter *game-level* scene)
  ;; Necessary to prevent a crash when spawning the first puff.
  (preload (make-instance 'bug-fly) scene)
  (preload (make-instance 'puff) scene))

;; NOTE: The sidescroll-camera insists on being at the origin. Even if you move
;; it here, it automatically glides back to the origin across the next second or
;; so.
(defmethod setup-scene :after ((main stf-main) scene)
  (let ((bottom-left-dot  (node :bottom-left-dot scene))
        (bottom-right-dot (node :bottom-right-dot scene))
        (top-left-dot     (node :top-left-dot scene))
        (top-right-dot    (node :top-right-dot scene))
        (truly-bottom     (node :truly-bottom-dot scene))
        (truly-top        (node :truly-top-dot scene))
        (farmer           (node :farmer scene)))
    ;; These four dot locations represent the bounds of the (NES) screen.
    ;; Projectiles should:
    ;;
    ;; - Not render past the edge of the map, culling gradually as their pixels
    ;; pass the edge.
    ;; - Fully despawn when ther left-most pixels have passed beyond the edge.
    (setf (location truly-bottom) (vec +screen-min-x+ +screen-min-y+ 0))
    (setf (location truly-top) (vec +screen-max-x+ +screen-max-y+ 0))
    (setf (location bottom-left-dot) (vec +min-x+ +min-y+ 0))
    (setf (location bottom-right-dot) (vec +max-x+ +min-y+ 0))
    (setf (location top-left-dot) (vec +min-x+ +max-y+ 0))
    (setf (location top-right-dot) (vec +max-x+ +max-y+ 0))
    (setf (location farmer) (grid->pixel 4 7))
    (spawn-crops 'lemon *crops*)
    (observe! (location (node :farmer scene)) :title "Farmer")))

(defmethod setup-rendering :after ((main stf-main))
  (disable-feature :cull-face))

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
    (setf (setting :display :target-framerate) +framerate+)
    ;; Register our custom actions.
    (setf (active-p (action-set 'in-game)) t)
    (apply #'trial:launch 'stf-main args))
  (v:info :stf "Exiting launch function."))

#+nil
(launch)

#+nil
(maybe-reload-scene)

#+nil
(scene +main+)
