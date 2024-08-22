(defsystem "save-the-farm"
  :version "0.0.0"
  :depends-on (:trial
               :trial-glfw
               :trial-harmony
               :trial-png
               :trial-jpeg
               :cl-mixed-mpg123
               :arrow-macros)
  :components ((:module "src"
                :components
                ((:file "main")))))
