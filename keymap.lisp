(directional move
             ;; (stick :one-of ((:l-h :l-v)))
             (stick :one-of ((:dpad-h :dpad-v)))
             (keys :one-of ((:w :a :s :d))))

(trigger hide
         (button :one-of (:a))
         (key :one-of (:space)))

(trigger shoot
         (button :one-of (:b))
         (key :one-of (:f)))
