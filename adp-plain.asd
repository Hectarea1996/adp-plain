
(defsystem "adp-plain"
  :author "Héctor Galbis Sanchis"
  :description "Add Documentation, Please... using plain text. An extension of ADP to generate files with barely additional features."
  :license "MIT"
  :defsystem-depends-on ("adp")
  :components ((:file "package")
               (:file "adp-plain")
               (:scribble "README")))
