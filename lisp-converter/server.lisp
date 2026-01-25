(ql:quickload '(:cl-protobufs :grpc) :silent t)

(load "/home/serge/projects/ebusta/lisp-converter/search.lisp")

(defpackage :ebusta.service
  (:use :cl)
  (:local-nicknames (#:pb #:cl-protobufs.ebusta.library.v1)
                    (#:grpc #:grpc)))

(in-package :ebusta.service)

(defun dsl-to-pb (dsl)
  (let ((query (make-instance 'pb:search-query)))
    (cond
      ((member (car dsl) '(:and :or))
       (let ((l-node (make-instance 'pb:logical-node)))
         (setf (pb:logical-node.op l-node) (if (eq (car dsl) :and) 1 2))
         (dolist (sub (cdr dsl))
           (push (dsl-to-pb sub) (pb:logical-node.nodes l-node)))
         (setf (pb:search-query.logical query) l-node)))
      ((eq (car dsl) :field)
       (let ((f-node (make-instance 'pb:filter-node)))
         (setf (pb:filter-node.field f-node) (getf dsl :field)
               (pb:filter-node.value f-node) (getf dsl :val)
               (pb:filter-node.operator f-node) 1)
         (setf (pb:search-query.filter query) f-node))))
    query))

(defun convert-handler (request)
  (let* ((raw (pb:convert-request.raw-query request))
         (dsl (read-from-string raw)))
    (format t "Processing query: ~A~%" raw)
    (dsl-to-pb dsl)))

(defun start-service ()
  (grpc:init-grpc)
  (format t "=== EBusta Lisp Service starting on 0.0.0.0:50052 ===~%")
  ;; Используем символ сервиса и явно указываем типы запроса/ответа 
  ;; для каждого метода, чтобы библиотека могла корректно связать их с cl-protobufs.
  (grpc:run-grpc-proto-server "0.0.0.0:50052"
                              (list (list 'pb:message-converter
                                          (list (list "Convert" 
                                                      #'convert-handler 
                                                      'pb:convert-request 
                                                      'pb:search-query))))))

(start-service)
