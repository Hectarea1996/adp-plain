
(defsystem "adp-plain"
  :defsystem-depends-on ("adp")
  :components ((:file "package")
               (:file "adp-plain")
               (:scribble "README")))
