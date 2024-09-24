;;; Handling of level durations, transitions, bug spawning, etc.

(in-package :save-the-farm)

#+nil
(launch)

(defclass game-level (scene-node listener)
  ((level :initarg :level    :reader level)
   (bug   :initarg :bug      :reader bug)
   (state :initform :pending :accessor state)
   (frame :initform nil      :accessor frame)
   (spawn-interval :initarg :spawn-interval :reader spawn-interval))
  (:documentation "A game level in which bugs are spawned, usually lasting 20 seconds."))

(defparameter +level-1+
  (make-instance 'game-level :level 1 :bug 'bug-fly :spawn-interval 60))

(define-handler (game-level tick) (fc)
  (case (state game-level)
    (:pending
     (setf (state game-level) :running)
     (setf (frame game-level) fc))
    (:running
     (when (zerop (mod fc (spawn-interval game-level)))
       ;; FIXME: 2024-09-23 Magic numbers.
       (let* ((loc (grid->pixel +grid-max-x+ (+ 3 (cl:random 9))))
              (bug (make-instance (bug game-level) :orig-y (vy loc))))
         ;; (v:info :stf (format nil "Frame: ~a" fc))
         (enter bug *bugs*)
         (setf (location bug) loc))))))

#+nil
(observe! (slot-value *bugs* 'trial::%count) :title "Bugs")
