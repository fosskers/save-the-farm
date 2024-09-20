;; Experimental `transducers' integration for Trial container types.

(in-package :save-the-farm)

(defmethod t:transduce (xform f (source bag))
  (bag-transduce xform f source))

(defun bag-transduce (xform f bag)
  (let* ((init   (funcall f))
         (xf     (funcall xform f))
         (result (bag-reduce xf init bag)))
    (funcall xf result)))

(defun bag-reduce (f identity bag)
  (let ((vec (slot-value bag 'trial::%objects))
        (max (slot-value bag 'trial::%count)))
    (labels ((recurse (acc i)
               (if (= i max)
                   acc
                   (let ((acc (t::safe-call f acc (aref vec i))))
                     (if (t:reduced-p acc)
                         (t:reduced-val acc)
                         (recurse acc (1+ i)))))))
      (recurse identity 0))))

(defun first-or (default)
  "Reducer: A variant of `first' that doesn't raise a condition when the
transduction is empty."
  (lambda (&optional (acc nil a-p) (input nil i-p))
    (cond ((and a-p i-p) (t:make-reduced :val input))
          ((and a-p (not i-p)) acc)
          (t default))))

#+nil
(t:transduce #'t:pass (first-or 7) '())
