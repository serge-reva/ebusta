(defpackage #:example-client
  (:use #:cl)
  (:local-nicknames (#:pb #:cl-protobufs.lisp.grpc.integration-testing)
                    (#:pb-rpc #:cl-protobufs.lisp.grpc.integration-testing-rpc)
                    (#:grpc #:grpc)))

(in-package #:example-client)

(defun run ()
  (grpc:init-grpc)
  (grpc:with-insecure-channel (channel "localhost:50051")
    (format t "Sending request...~%")
    (let* ((request (pb:make-hello-request :name "Admin"))
           (response (pb-rpc:call-say-hello channel request)))
      (format t "RESPONSE: ~A~%" (pb:hello-reply.message response))))
  (grpc:shutdown-grpc)
  (sb-ext:exit))

(run)
