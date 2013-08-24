;;;; cepl-classimp.asd

(asdf:defsystem #:cepl-classimp
  :serial t
  :description "Uses classimp to load 3d models but returns the data as cepl c-arrays using formats that are ready to be used with the gpu"
  :author "Baggers"
  :license "LLGPL"
  :depends-on (#:cepl
               #:classimp)
  :components ((:file "package")
               (:file "cepl-classimp")))

