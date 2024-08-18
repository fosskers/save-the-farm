(defsystem "save-the-farm"
  :version "0.0.0"
  :depends-on (:trial
               :trial-glfw
               :trial-png)
  :components ((:module "src"
                :components
                ((:file "main")))))
