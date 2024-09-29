(in-package :save-the-farm)

#+nil
(launch)

;; --- Types --- ;;

(define-shader-entity digit (animated-sprite located-entity)
  ((sprite-data :initform (asset 'farm 'numbers))
   (value       :initform 0 :accessor value :documentation "The digit's true inner value."))
  (:documentation "A digit to be displayed on the screen."))

;; --- Handlers --- ;;

(define-handler (digit tick :before) ()
  ;; Testing
  (let ((digit (node :digit (container digit))))
    (setf (value digit) (length *bugs*)))
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
