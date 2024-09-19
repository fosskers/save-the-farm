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

#+nil
(slot-value *bugs* 'trial::%count)
#+nil
(slot-value *bugs* 'trial::%count)
