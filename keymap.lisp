(directional move
             (stick :one-of ((:l-h :l-v)))
             (keys :one-of ((:w :a :s :d))))

(trigger hide
         (button :one-of (:a))
         (key :one-of (:space)))

(trigger shoot
         (key :one-of (:f)))
