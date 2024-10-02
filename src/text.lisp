(in-package :save-the-farm)

#+nil
(launch)

#+nil
(maybe-reload-scene)

;; --- Types --- ;;

(define-shader-entity digit (animated-sprite located-entity)
  ((sprite-data :initform (asset 'farm 'numbers))
   (value       :initform 0 :accessor value :documentation "The digit's true inner value."))
  (:documentation "A digit to be displayed on the screen."))

(define-shader-entity score (listener)
  ((digits :initform (vector (make-instance 'digit)
                             (make-instance 'digit)
                             (make-instance 'digit)
                             (make-instance 'digit)
                             (make-instance 'digit)
                             (make-instance 'digit)
                             (make-instance 'digit))
           :accessor digits)
   (score  :initform 0 :accessor score))
  (:documentation "The player's current score."))

(defun spawn-score (scene location)
  "Spawn and move the digits of a score board to a given LOCATION."
  (let ((score (make-instance 'score))
        (y (vy location)))
    (enter score scene)
    (t:transduce #'t:pass
                 (t:fold (lambda (x digit)
                           (enter digit scene)
                           (setf (vx (location digit)) x)
                           (setf (vy (location digit)) y)
                           (+ 8 x))
                         (vx location))
                 (digits score))
    score))

;; --- Handlers --- ;;

(define-handler (score tick) ()
  (t:transduce #'t:pass
               (t:fold (lambda (acc digit)
                         (multiple-value-bind (next this) (floor acc 10)
                           (setf (value digit) this)
                           next))
                       (score score))
               (t:reversed (digits score))))

(define-handler (digit tick :before) ()
  (play (number->animation (value digit)) digit))

(defun number->animation (n)
  "The Aseprite animation tag corresponding to the given number."
  (case n
    (0 'n0)
    (1 'n1)
    (2 'n2)
    (3 'n3)
    (4 'n4)
    (5 'n5)
    (6 'n6)
    (7 'n7)
    (8 'n8)
    (9 'n9)))
