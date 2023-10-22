
(defsystem "adp-plain-docs"
  :author "Héctor Galbis Sanchis"
  :description "Documentation system of ADP-PLAIN"
  :license "MIT"
  :defsystem-depends-on ("adp-plain")
  :build-operation "adp-plain-op"
  :components ((:scribble "README")))
