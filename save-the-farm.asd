(defsystem "save-the-farm"
  :version "0.0.0"
  :depends-on (:trial
               :trial-glfw
               :trial-harmony
               :trial-png
               :trial-jpeg
               :cl-mixed-mpg123
               :arrow-macros
               :transducers)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "text")
                 (:file "transducers")
                 (:file "collision")
                 (:file "bugs")
                 (:file "crops")
                 (:file "farmer")
                 (:file "level")
                 (:file "main")))))
