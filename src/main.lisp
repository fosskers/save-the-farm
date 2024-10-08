(in-package :save-the-farm)

#+nil
(launch)

#+nil
(maybe-reload-scene)

(defclass stf-main (trial-harmony:settings-main)
  ())

(define-shader-entity post (animated-sprite located-entity)
  ((sprite-data :initform (asset 'farm 'post))))

(define-shader-entity dot (animated-sprite located-entity)
  ((sprite-data :initform (asset 'farm 'origin-dot))))

(defun spawn-posts (scene)
  "Spawn in the posts at the bottom of the field in a way that makes them look like
part of the map."
  (dotimes (n 16)
    (let ((post (make-instance 'post))
          (loc  (grid->pixel n 2)))
      (incf (vy loc) 11)
      (enter post scene)
      (setf (location post) loc))))

(defmethod stage :after ((scene scene) (area staging-area))
  (stage (// 'farm 'bgm) area)
  (harmony:play (// 'farm 'bgm) :repeat 3 :volume 0.75))

(defmethod setup-scene ((main stf-main) scene)
  (setf *game-over* nil)
  (setf *crops* (make-instance 'bag))
  (setf *bugs*  (make-instance 'bag))
  (setf *puffs* (make-instance 'bag))
  (enter (make-instance 'tile-layer :tile-data (asset 'farm 'tilemap) :name :field) scene)
  ;; (enter (make-instance 'dot :name :origin-dot) scene)
  ;; (enter (make-instance 'dot :name :bottom-left-dot) scene)
  ;; (enter (make-instance 'dot :name :bottom-right-dot) scene)
  ;; (enter (make-instance 'dot :name :top-left-dot) scene)
  ;; (enter (make-instance 'dot :name :top-right-dot) scene)
  ;; (enter (make-instance 'dot :name :truly-bottom-dot) scene)
  ;; (enter (make-instance 'dot :name :truly-top-dot) scene)
  ;; NOTE: No need to manually setf the camera slot of the `scene', as an
  ;; `:after' defmethod on camera+scene already does this.
  ;; (enter (make-instance 'sidescroll-camera :zoom 5.0 :target (node :farmer scene)) scene)
  (enter (make-instance 'sidescroll-camera :zoom 4.0 :name :camera) scene)
  (enter (make-instance 'render-pass) scene)
  (enter (make-instance 'farmer :name :farmer) scene)
  (enter *crops* scene)
  (enter *bugs* scene)
  (enter *puffs* scene)
  ;; (enter (make-instance 'display-controller) scene)
  (enter (start-level :level-1) scene)
  ;; Necessary to prevent a crash when spawning the first puff.
  (preload (make-instance 'bug-fly) scene)
  (preload (make-instance 'puff) scene)
  (preload (make-instance 'digit) scene))

;; NOTE: The sidescroll-camera insists on being at the origin. Even if you move
;; it here, it automatically glides back to the origin across the next second or
;; so.
(defmethod setup-scene :after ((main stf-main) scene)
  (let (
        ;; (bottom-left-dot  (node :bottom-left-dot scene))
        ;; (bottom-right-dot (node :bottom-right-dot scene))
        ;; (top-left-dot     (node :top-left-dot scene))
        ;; (top-right-dot    (node :top-right-dot scene))
        ;; (truly-bottom     (node :truly-bottom-dot scene))
        ;; (truly-top        (node :truly-top-dot scene))
        (farmer           (node :farmer scene)))
    ;; These four dot locations represent the bounds of the (NES) screen.
    ;; Projectiles should:
    ;;
    ;; - Not render past the edge of the map, culling gradually as their pixels
    ;; pass the edge.
    ;; - Fully despawn when ther left-most pixels have passed beyond the edge.
    ;; (setf (location truly-bottom) (vec +screen-min-x+ +screen-min-y+ 0))
    ;; (setf (location truly-top) (vec +screen-max-x+ +screen-max-y+ 0))
    ;; (setf (location bottom-left-dot) (vec +field-min-x+ +field-min-y+ 0))
    ;; (setf (location bottom-right-dot) (vec +field-max-x+ +field-min-y+ 0))
    ;; (setf (location top-left-dot) (vec +field-min-x+ +field-max-y+ 0))
    ;; (setf (location top-right-dot) (vec +field-max-x+ +field-max-y+ 0))
    (setf (location farmer) (grid->pixel 4 7))
    (spawn-crops 'lemon *crops*)
    (spawn-posts scene)
    (setf *score* (spawn-score scene (grid->pixel 12 14)))
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
