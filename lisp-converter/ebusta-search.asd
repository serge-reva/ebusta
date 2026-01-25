(defsystem "ebusta-search"
  :defsystem-depends-on (:cl-protobufs.asdf)
  :depends-on (:cl-protobufs :grpc)
  :components ((:protobuf-source-file "search")))
