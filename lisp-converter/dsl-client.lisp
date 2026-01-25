(defpackage #:dsl-client
  (:use #:cl)
  (:local-nicknames (#:pb #:cl-protobufs.ebusta.library.v1)
                    (#:pb-rpc #:cl-protobufs.ebusta.library.v1-rpc)
                    (#:grpc #:grpc)))

(in-package #:dsl-client)

(defun run ()
  (grpc:init-grpc)
  ;; Тот самый запрос: И (title="Lisp") ИЛИ (author="Serge" | author="Reva")
  (let ((dsl-string "(:and (:field \"title\" \"Lisp\") (:or (:field \"author\" \"Serge\") (:field \"author\" \"Reva\")))"))
    
    (format t ">>> Sending DSL: ~A~%" dsl-string)
    
    (grpc:with-insecure-channel (channel "localhost:50052")
      (let* ((request (pb:make-convert-request :raw-query dsl-string))
             ;; Вызываем метод Convert
             (response (pb-rpc:call-convert channel request)))
        
        (format t ">>> SERVER RESPONSE:~%")
        (format t "~S~%" response)
        
        ;; Дополнительная проверка структуры (для наглядности)
        (let ((logical (pb:search-query.logical response)))
          (when logical
            (format t "Root is LOGICAL node (Op: ~A)~%" (pb:logical-node.op logical))
            (format t "Children count: ~A~%" (length (pb:logical-node.nodes logical))))))))

  (grpc:shutdown-grpc)
  (sb-ext:exit))

(run)
