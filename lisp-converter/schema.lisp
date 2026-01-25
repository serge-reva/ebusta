(ql:quickload :cl-protobufs)

(defpackage :ebusta.schema
  (:use :cl :cl-protobufs))

(in-package :ebusta.schema)

(cl-protobufs:define-schema libraryv1
    (:package "libraryv1"))

(cl-protobufs:define-message filter-node ()
  (:schema libraryv1)
  (field :index 1 :type string :kind :optional)
  (value :index 2 :type string :kind :optional)
  (operator :index 3 :type int32 :kind :optional))

;; Добавляем узел для AND/OR
(cl-protobufs:define-message logical-node ()
  (:schema libraryv1)
  (op :index 1 :type int32 :kind :optional) ;; 1 - AND, 2 - OR
  (nodes :index 2 :type filter-node :kind :repeated))

(cl-protobufs:define-message search-query ()
  (:schema libraryv1)
  (filter :index 1 :type filter-node :kind :optional)
  (logical :index 2 :type logical-node :kind :optional))
