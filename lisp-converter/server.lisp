(ql:quickload '(:cl-protobufs :grpc) :silent t)

;; 1. Загружаем сгенерированные определения
(load "/home/serge/projects/ebusta/lisp-converter/search.lisp")

(defpackage :ebusta.service
  (:use :cl)
  (:local-nicknames (#:pb #:cl-protobufs.ebusta.library.v1)
                    (#:grpc #:grpc)))

(in-package :ebusta.service)

;; --- Логика конвертации ---
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

;; --- Реализация метода сервера ---
;; Мы добавляем метод к обобщенной функции, которую создал cl-protobufs.
;; Имя пакета RPC формируется автоматически: <package-name>-rpc
(in-package :cl-protobufs.ebusta.library.v1-rpc)

(defmethod convert ((request cl-protobufs.ebusta.library.v1:convert-request) rpc)
  (declare (ignore rpc))
  (format t "Request received: ~S~%" request)
  (let* ((raw (cl-protobufs.ebusta.library.v1:convert-request.raw-query request))
         (dsl (read-from-string raw)))
    (format t "Parsed DSL: ~S~%" dsl)
    (ebusta.service::dsl-to-pb dsl)))

(in-package :ebusta.service)

;; --- Запуск ---
(defun start-service ()
  (grpc:init-grpc)
  (format t "=== EBusta Lisp Service starting on port 50052 ===~%")
  ;; qitab/grpc умеет сам находить методы по имени сервиса
  (grpc:run-grpc-proto-server 
   "0.0.0.0:50052" 
   'pb:message-converter
   :num-threads 2))

(start-service)
