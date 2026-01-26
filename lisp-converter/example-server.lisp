(defpackage #:example-server
  (:use #:cl)
  (:local-nicknames (#:pb #:cl-protobufs.lisp.grpc.integration-testing)
                    (#:pb-rpc #:cl-protobufs.lisp.grpc.integration-testing-rpc)
                    (#:grpc #:grpc)))

(in-package #:example-server)

;; Используем полное имя метода из RPC пакета, чтобы не заходить в него
(defmethod pb-rpc:say-hello ((request pb:hello-request) rpc)
  (declare (ignore rpc))
  (let ((name (pb:hello-request.name request)))
    (format t ">>> SERVER: Received request for name: ~A~%" name)
    (pb:make-hello-reply
     :message (concatenate 'string "Hello " name " (via ASDF Build)"))))

(defun run ()
  (grpc:init-grpc)
  (format t "=== Server listening on :50051 ===~%")
  (grpc:run-grpc-proto-server
   "0.0.0.0:50051"
   'pb:greeter
   :num-threads 1))

(run)
