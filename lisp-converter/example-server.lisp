(defpackage #:example-server
  (:use #:cl)
  (:local-nicknames (#:pb #:cl-protobufs.lisp.grpc.integration-testing)
                    (#:pb-rpc #:cl-protobufs.lisp.grpc.integration-testing-rpc)
                    (#:grpc #:grpc)))

(in-package #:example-server)

;; Определяем метод ВНУТРИ нашего пакета, но для символа из RPC пакета.
;; ВАЖНО: Мы не делаем in-package в pb-rpc, чтобы иметь доступ к cl:format и cl:concatenate.

(defmethod pb-rpc:say-hello ((request pb:hello-request) rpc)
  (declare (ignore rpc))
  (format t ">>> SERVER: Received request for name: ~A~%" 
          (pb:hello-request.name request))
  (pb:make-hello-reply
   :message (concatenate 'string "Hello " 
                         (pb:hello-request.name request)
                         " (via ASDF Build)")))

(defun run ()
  (grpc:init-grpc)
  (format t "=== Server listening on :50051 ===~%")
  (grpc:run-grpc-proto-server
   "0.0.0.0:50051"
   'pb:greeter
   :num-threads 1))

(run)
