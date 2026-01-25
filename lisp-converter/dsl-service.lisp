(defpackage #:ebusta-service
  (:use #:cl)
  (:local-nicknames (#:pb #:cl-protobufs.ebusta.library.v1)
                    (#:pb-rpc #:cl-protobufs.ebusta.library.v1-rpc)
                    (#:grpc #:grpc)))

(in-package #:ebusta-service)

;; --- Логика конвертации ---

(defun parse-dsl (dsl)
  "Рекурсивно превращает S-expression DSL в Protobuf объект SearchQuery"
  (let ((query (pb:make-search-query)))
    (cond
      ;; Логический узел: (:and ...) или (:or ...)
      ((member (car dsl) '(:and :or))
       (let ((l-node (pb:make-logical-node)))
         ;; 1 = AND, 2 = OR
         (setf (pb:logical-node.op l-node) (if (eq (car dsl) :and) 1 2))
         ;; Рекурсивно обрабатываем детей
         (dolist (sub (cdr dsl))
           (push (parse-dsl sub) (pb:logical-node.nodes l-node)))
         ;; Из-за push порядок обратный, разворачиваем (опционально)
         (setf (pb:logical-node.nodes l-node) (nreverse (pb:logical-node.nodes l-node)))
         (setf (pb:search-query.logical query) l-node)))
      
      ;; Листовой узел: (:field "key" "val")
      ((eq (car dsl) :field)
       (let ((f-node (pb:make-filter-node)))
         ;; DSL: (:field "title" "Linux") -> field="title", value="Linux"
         ;; (getf list key) не сработает удобно, берем по индексу
         (setf (pb:filter-node.field f-node) (second dsl)
               (pb:filter-node.value f-node) (third dsl)
               (pb:filter-node.operator f-node) 1)
         (setf (pb:search-query.filter query) f-node)))
      
      (t (error "Unknown DSL node: ~S" dsl)))
    query))

;; --- Реализация gRPC метода ---

;; Определяем метод convert для generic-функции из сгенерированного пакета RPC
(defmethod pb-rpc:convert ((request pb:convert-request) rpc)
  (declare (ignore rpc))
  (format t ">>> REQUEST: ~A~%" (pb:convert-request.raw-query request))
  
  (handler-case
      (let* ((raw-str (pb:convert-request.raw-query request))
             ;; Опасный момент: read-from-string выполняет любой Lisp код.
             ;; В продакшене нужен безопасный парсер, но для dev ок.
             (dsl-sexp (read-from-string raw-str)))
        
        (format t "Parsed S-Exp: ~S~%" dsl-sexp)
        (let ((result (parse-dsl dsl-sexp)))
          (format t "Conversion success.~%")
          result))
    (error (e)
      (format t "ERROR processing request: ~A~%" e)
      ;; В gRPC можно вернуть ошибку, но пока вернем пустой ответ или упадем
      (error e))))

;; --- Запуск сервера ---

(defun start ()
  (grpc:init-grpc)
  (format t "=== EBusta DSL Service listening on :50052 ===~%")
  (grpc:run-grpc-proto-server
   "0.0.0.0:50052"
   'pb:message-converter
   :num-threads 2))

(start)
