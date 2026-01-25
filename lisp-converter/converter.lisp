(ql:quickload '(:cl-protobufs :cl-base64))

(defpackage :ebusta.app
  (:use :cl :cl-protobufs)
  (:export :run-test))

(in-package :ebusta.app)

;; --- 1. СХЕМА (NATIVE) ---
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-schema libraryv1
    (:package "libraryv1")))

(define-message filter-node ()
  (:schema libraryv1)
  (field :index 1 :type string :kind :optional)
  (value :index 2 :type string :kind :optional)
  (operator :index 3 :type int32 :kind :optional))

(define-message logical-node ()
  (:schema libraryv1)
  (op :index 1 :type int32 :kind :optional) ;; 1 = AND, 2 = OR
  (nodes :index 2 :type filter-node :kind :repeated))

(define-message search-query ()
  (:schema libraryv1)
  (filter :index 1 :type filter-node :kind :optional)
  (logical :index 2 :type logical-node :kind :optional))

;; --- 2. ПАРСЕР ---
(defun to-pb (dsl)
  (let ((query (make-instance 'search-query)))
    (cond 
      ((member (car dsl) '(:and :or))
       (let ((l-node (make-instance 'logical-node :op (if (eq (car dsl) :and) 1 2))))
         (dolist (sub (cdr dsl))
           (push (make-instance 'filter-node
                                :field (getf sub :field)
                                :value (getf sub :val)
                                :operator 1)
                 (logical-node.nodes l-node)))
         (setf (search-query.logical query) l-node)))
      ((eq (car dsl) :field)
       (setf (search-query.filter query)
             (make-instance 'filter-node
                            :field (getf dsl :field)
                            :value (getf dsl :val)
                            :operator 1))))
    query))

;; --- 3. ТЕСТОВЫЙ ЗАПУСК ---
(defun run-test ()
  (format t "~%--- [ CONVERTER START ] ---~%")
  (let* ((dsl '(:and (:field "author" :val "Bulgakov") 
                     (:field "title" :val "Master")))
         (pb-obj (to-pb dsl)))
    (format t "DSL input: ~A~%" dsl)
    (format t "Protobuf Text Format:~%~%")
    (print-text-format pb-obj :stream t)
    (format t "~%--- [ CONVERTER SUCCESS ] ---~%")))

(run-test)
