(in-package :save-the-farm)

;; --- Loading the Game/Scene --- ;;

#+nil
(launch)

#+nil
(maybe-reload-scene)

;; --- Resetting the Keybindings --- ;;

;; Dynamically reload the Keymap and make sure the global one is resaved, or
;; else local changes won't be reapplied on subsequent launches.
#+nil
(let ((*package* #.*package*))
  (load-keymap :reset t))

;; --- Controller Experiments --- ;;

#+nil
(->> (gamepad:list-devices)
  (car))
;; (gamepad:configure-device))

;; --- Scene Graph --- ;;

#+nil
(describe (scene +main+))

;; --- Sound --- ;;

#+nil
(harmony:play
 #p"/home/colin/code/common-lisp/save-the-farm/data/meow.mp3")

#+nil
(harmony:play (// 'farm 'meow))

;; --- Misc. Debugging --- ;;

#+nil
(debug-text (vec 0 1 -5) "Jack")

#+nil
(debug-clear)

