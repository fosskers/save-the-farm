;;; Sprite-on-sprite collision.
;;;
;;; This has to be hand-written as Trial's built-in collision is intended for 3D.

(in-package :save-the-farm)

(defgeneric max-x (entity)
  (:documentation "The pixel-X coordinate of the right-most part of the entity."))
(defgeneric min-x (entity)
  (:documentation "The pixel-X coordinate of the left-most part of the entity."))
(defgeneric max-y (entity)
  (:documentation "The pixel-Y coordinate of the top-most part of the entity."))
(defgeneric min-y (entity)
  (:documentation "The pixel-Y coordinate of the bottom-most part of the entity."))

(defun in-vacinity? (a-x a-y b-x b-y)
  "Given the XY grid coordinates of two objects, is the second no more than 1 grid
block away from the first in any direction?"
  (and (<= (1- a-x) b-x (1+ a-x))
       (<= (1- a-y) b-y (1+ a-y))))

#+nil
(let* ((scene  (scene +main+))
       (farmer (node :farmer scene))
       (lemon  (make-instance 'lemon :name :templemon)))
  (enter lemon scene)
  (setf (location lemon) (grid->pixel 5 7))
  (observe! (location lemon) :title "Lemon")
  (observe! (multiple-value-bind (f-x f-y) (pixel->grid (vx (location farmer))
                                                        (vy (location farmer)))
              (multiple-value-bind (l-x l-y) (pixel->grid (vx (location lemon))
                                                          (vy (location lemon)))
                (in-vacinity? f-x f-y l-x l-y)))
            :title "Vacinity?"))

(defun overlapping? (a b)
  "Do the bounding boxes of two entities overlap? Overlapping is defined as a state
where any one of the corners of A are fully within the bounds of B. A is assumed
to be to the left of B, but the algorithm will adjust if this isn't so."
  (if (< (min-x b) (min-x a))
      (overlapping? b a)
      (let ((a-max-x (max-x a))
            (a-min-y (min-y a))
            (a-max-y (max-y a))
            (b-min-x (min-x b))
            (b-max-x (max-x b))
            (b-min-y (min-y b))
            (b-max-y (max-y b)))
        ;; Because we know that A is always on the left, we only need to test
        ;; its two right corners.
        (or (and (<= b-min-y a-max-y b-max-y)
                 (<= b-min-x a-max-x b-max-x))
            (and (<= b-min-y a-min-y b-max-y)
                 (<= b-min-x a-max-x b-max-x))))))

(defun collision-candidate (entity container)
  "Given an ENTITY and another CONTAINER of entities, determine the first (if any)
that is in the vacinity of the ENTITY."
  (multiple-value-bind (a-x a-y) (pixel->grid (vx (location entity))
                                              (vy (location entity)))
    (t:transduce (t:filter (lambda (other)
                             (multiple-value-bind (b-x b-y) (pixel->grid (vx (location other))
                                                                         (vy (location other)))
                               (in-vacinity? a-x a-y b-x b-y))))
                 #'t:first container))) ;; TODO: Don't throw when there's no match.

#+nil
(let ((farmer (node :farmer (scene +main+))))
  (collision-candidate farmer *bugs*))

;; --- Collision Testing --- ;;

#+nil
*bugs*

#+nil
(launch)

#+nil
(maybe-reload-scene)

#+nil
(let ((dot (make-instance 'lemon :name :templemon)))
  (enter dot (scene +main+))
  (setf (location dot) (grid->pixel 5 7)))

#+nil
(let* ((scene (scene +main+))
       (dot (node :templemon scene)))
  (leave dot scene))

#+nil
(let* ((scene  (scene +main+))
       (farmer (node :farmer scene))
       (lemon  (make-instance 'lemon :name :templemon)))
  (enter lemon scene)
  (setf (location lemon) (grid->pixel 5 7))
  (observe! (location lemon) :title "Lemon")
  (observe! (vec (min-x farmer) (max-x farmer) (min-y farmer) (max-y farmer)) :title "FarmerXY")
  (observe! (vec (min-x lemon) (max-x lemon) (min-y lemon) (max-y lemon)) :title "LemonXY")
  (observe! (overlapping? farmer lemon) :title "Overlapping?"))
