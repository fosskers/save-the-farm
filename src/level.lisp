;;; Handling of level durations, transitions, bug spawning, etc.

(in-package :save-the-farm)

#+nil
(launch)

#+nil
(maybe-reload-scene)

;; --- Types --- ;;

(defclass game-level (entity listener)
  ((level :initarg :level    :reader level)
   (bug   :initarg :bug      :reader bug)
   (state :initform :pending :accessor state)
   (frame :initform nil      :accessor frame)
   (spawn-interval :initarg :spawn-interval :accessor spawn-interval))
  (:documentation "A game level in which bugs are spawned, usually lasting 20 seconds."))

;; --- Handlers --- ;;

(define-handler (game-level tick) (fc)
  (case (state game-level)
    (:pending
     (setf (state game-level) :running)
     (setf (frame game-level) fc))
    (:running
     (when (zerop (mod fc (spawn-interval game-level)))
       ;; FIXME: 2024-09-23 Magic numbers.
       (let* ((loc (grid->pixel +grid-max-x+ (+ 3 (cl:random 9))))
              (bug (make-instance (bug game-level) :orig-y (vy loc)))
              (mov (case (cl:random 4)
                     (0 #'move-straight)
                     (1 #'move-sin-wave)
                     (2 (move-at-crop))
                     (3 (move-at-farmer)))))
         ;; (v:info :stf (format nil "Frame: ~a" fc))
         (enter bug *bugs*)
         (setf (location bug) loc)
         (setf (movement-scheme bug) mov)
         (when (> (spawn-interval game-level) +framerate+)
           (decf (spawn-interval game-level))))))))

#+nil
(observe! (slot-value *bugs* 'trial::%count) :title "Bugs")

;; --- Misc. --- ;;

(defun start-level (level)
  "Spawn a designated level."
  (case level
    (:level-1 (make-instance 'game-level :level 1 :bug 'bug-fly :spawn-interval 120 :name :level))))

(defun game-over ()
  "End the game."
  (let ((scene (scene +main+)))
    (clear *bugs*)
    (leave (node :farmer scene) scene)
    (leave (node :level scene) scene)
    (setf *game-over* t)))

#+nil
(game-over)
