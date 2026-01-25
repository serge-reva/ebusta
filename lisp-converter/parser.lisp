(defpackage :ebusta.parser
  (:use :cl)
  (:export :to-pb))

(in-package :ebusta.parser)

(defun to-pb (dsl)
  (let ((query (make-instance 'cl-protobufs.libraryv1:search-query)))
    (cond 
      ;; Если это AND/OR
      ((member (car dsl) '(:and :or))
       (let ((l-node (make-instance 'cl-protobufs.libraryv1:logical-node
                                    :op (if (eq (car dsl) :and) 1 2))))
         (dolist (sub (cdr dsl))
           (let ((f-node (make-instance 'cl-protobufs.libraryv1:filter-node
                                        :field (getf sub :field)
                                        :value (getf sub :val)
                                        :operator 1)))
             (push f-node (cl-protobufs.libraryv1:logical-node.nodes l-node))))
         (setf (cl-protobufs.libraryv1:search-query.logical query) l-node)))
      
      ;; Если это просто поле
      ((eq (car dsl) :field)
       (setf (cl-protobufs.libraryv1:search-query.filter query)
             (make-instance 'cl-protobufs.libraryv1:filter-node
                            :field (getf dsl :field)
                            :value (getf dsl :val)
                            :operator 1))))
    query))
