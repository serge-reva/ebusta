(ql:quickload '(:cl-protobufs :cl-base64))

(load "/home/serge/projects/ebusta/lisp-converter/search.lisp")

(defpackage :ebusta.converter
  (:use :cl)
  (:local-nicknames (#:pb #:cl-protobufs.ebusta.library.v1)))

(in-package :ebusta.converter)

(defun dsl-to-pb (dsl)
  (let ((request (make-instance 'pb:search-request)))
    (cond
      ((member (car dsl) '(:and :or))
       (let ((l-node (make-instance 'pb:logical-node)))
         (setf (pb:logical-node.op l-node) (if (eq (car dsl) :and) 1 2))
         (dolist (sub (cdr dsl))
           (push (dsl-to-pb sub) (pb:logical-node.nodes l-node)))
         (setf (pb:search-request.logical request) l-node)))
      ((eq (car dsl) :field)
       (let ((f-node (make-instance 'pb:filter-node)))
         (setf (pb:filter-node.field f-node) (getf dsl :field)
               (pb:filter-node.value f-node) (getf dsl :val)
               (pb:filter-node.operator f-node) 1)
         (setf (pb:search-request.filter request) f-node))))
    request))

(defun run-system-uuencode (pb-obj)
  (let* ((octets (cl-protobufs:serialize-to-bytes pb-obj))
         (b64 (cl-base64:usb8-array-to-base64-string octets)))
    ;; Вызываем системную утилиту через shell
    (uiop:run-program (format nil "echo ~A | base64 -d | uuencode query.bin" b64)
                      :output t)))

;; ТЕСТ
(format t "~%--- STARTING RECURSIVE TEST ---~%")
(let* ((complex-dsl '(:and (:field "author" :val "Bulgakov") 
                           (:or (:field "title" :val "Master") 
                                (:field "title" :val "Margarita"))))
       (pb-obj (dsl-to-pb complex-dsl)))
  (format t "~%--- UUENCODE OUTPUT ---~%")
  (run-system-uuencode pb-obj)
  (format t "~%--- SUCCESS ---~%"))
