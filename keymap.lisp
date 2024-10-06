(directional move
             ;; (stick :one-of ((:l-h :l-v)))
             (stick :one-of ((:dpad-h :dpad-v)))
             (keys :one-of ((:w :a :s :d))))

#+nil
(trigger hide
         (button :one-of (:a))
         (key :one-of (:space)))

(trigger kick
         (button :one-of (:a)))

(trigger shoot
         (button :one-of (:b))
         (key :one-of (:space)))

(trigger pause
         (button :one-of (:select)))

(trigger start
         (button :one-of (:start)))
