(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:cl-ppcre :grpc :cl-protobufs :bordeaux-threads) :silent t))

(defpackage #:ebusta-service
  (:use #:cl)
  (:export #:start #:stop #:parse-raw-to-sexp #:parse-sexp-to-ast #:build-binary)
  (:local-nicknames (#:pb #:cl-protobufs.ebusta.library.v1)
                    (#:pb-rpc #:cl-protobufs.ebusta.library.v1-rpc)
                    (#:grpc #:grpc)
                    (#:re #:cl-ppcre)
                    (#:bt #:bordeaux-threads)))

(in-package #:ebusta-service)

(defvar *verbose* nil)

;; --- 1. ЛЕКСИКА ---

(defun get-priority (op)
  (cond ((string-equal op "NOT") 3)
        ((string-equal op "AND") 2)
        ((string-equal op "OR") 1)
        (t 0)))

(defun tokenize (str)
  "Токенизатор с поддержкой закавыченных строк как единых блоков."
  (re:all-matches-as-strings "(\"[^\"]+\"|[a-zA-Z0-9_]+:|AND|OR|NOT|\\(|\\)|/|\\S+)" str))

(defun numeric-p (s)
  (if (re:scan "^[0-9]+$" s) t nil))

;; --- 2. ОБРАБОТКА ПОЛЕЙ (ЖАДНЫЙ ЗАХВАТ) ---

(defun make-field-node (field val-raw)
  (let* ((val (string-trim " \"" val-raw))
         (is-regex (re:scan "^/.*/$" val)))
    `(:field ,field ,(if is-regex (string-trim "/" val) val) ,@(when is-regex '(:op :regex)))))

(defun process-field (token rest-tokens-var)
  "Реализация жадного захвата согласно требованиям:
   1. Если значение в кавычках - берем только его.
   2. Если нет - берем все токены до первого оператора или скобки."
  (let* ((pos (position #\: token))
         (field (subseq token 0 pos))
         (val-part (subseq token (1+ pos)))
         (first-next (car rest-tokens-var)))
    (cond 
      ;; Случай field:"value"
      ((string/= "" val-part)
       (values (make-field-node field val-part) rest-tokens-var))
      ;; Случай field: "quoted value"
      ((and first-next (re:scan "^\".*\"$" first-next))
       (values (make-field-node field (pop rest-tokens-var)) rest-tokens-var))
      ;; Жадный захват: field: Стивен Кинг AND ... -> заберет 'Стивен Кинг'
      (t (let (collected)
           (loop while (and (car rest-tokens-var)
                            (zerop (get-priority (car rest-tokens-var)))
                            (not (member (car rest-tokens-var) '("(" ")") :test #'string-equal)))
                 do (push (pop rest-tokens-var) collected))
           (values (make-field-node field (format nil "~{~A~^ ~}" (nreverse collected)))
                   rest-tokens-var))))))

;; --- 3. CORE ENGINE ---

(defun parse-raw-to-sexp (str)
  (let ((token-list (tokenize str))
        (output nil)
        (stack nil))
    (loop while token-list do
      (let ((token (pop token-list)))
        (cond
          ((> (get-priority token) 0)
           (loop while (and stack 
                            (> (get-priority (car stack)) 0)
                            (>= (get-priority (car stack)) (get-priority token)))
                 do (push (pop stack) output))
           (push token stack))
          ((string= token "(") (push token stack))
          ((string= token ")")
           (loop while (and stack (string/= (car stack) "(")) 
                 do (push (pop stack) output))
           (pop stack))
          ((re:scan "^[a-zA-Z0-9_]+:$" token)
           (multiple-value-bind (node remaining) (process-field token token-list)
             (push node output)
             (setf token-list remaining)))
          (t (let ((field (if (numeric-p token) "id" "any")))
               (push (make-field-node field token) output))))))
    (loop while stack do (push (pop stack) output))
    (build-tree-from-rpn (nreverse output))))

(defun build-tree-from-rpn (rpn)
  (let (stack)
    (dolist (token rpn (car stack))
      (if (and (listp token) (eq (car token) :field))
          (push token stack)
          (let ((op (string-upcase (string token))))
            (cond 
              ((string= op "NOT") (push `(:not ,(pop stack)) stack))
              ((member op '("AND" "OR") :test #'string=)
               (let* ((right (pop stack)) (left (pop stack))
                      (key (if (string= op "AND") :and :or)))
                 (push `(,key ,left ,right) stack)))))))))

;; --- 4. AST & gRPC (БЕЗ ИЗМЕНЕНИЙ) ---

(defun parse-sexp-to-ast (sexp &key request-id canonical-form)
  (let ((query (pb:make-search-query)))
    (when (and sexp (listp sexp))
      (let ((head (car sexp)))
        (cond
          ((member head '(:and :or))
           (let ((node (pb:make-logical-node :op (if (eq head :and) 1 2))))
             (setf (pb:logical-node.nodes node) 
                   (mapcar (lambda (s) (parse-sexp-to-ast s)) (cdr sexp)))
             (setf (pb:search-query.logical query) node)))
          ((eq head :not)
           (let ((node (pb:make-logical-node :op 3)))
             (setf (pb:logical-node.nodes node) (list (parse-sexp-to-ast (second sexp))))
             (setf (pb:search-query.logical query) node)))
          ((eq head :field)
           (let ((node (pb:make-filter-node :field (second sexp) :value (third sexp)
                                            :operator (if (eq (getf (cdddr sexp) :op) :regex) 6 1))))
             (setf (pb:search-query.filter query) node))))))
    (when request-id (setf (pb:search-query.request-id query) request-id))
    (when canonical-form (setf (pb:search-query.canonical-form query) canonical-form))
    query))

(defmethod pb-rpc:convert ((request pb:convert-request) rpc)
  (declare (ignore rpc))
  (let* ((raw (pb:convert-request.raw-query request))
         (request-id (format nil "req-~A" (get-universal-time))))
    (handler-case
        (let* ((sexp (parse-raw-to-sexp raw))
               (ast (parse-sexp-to-ast sexp 
                                      :request-id request-id 
                                      :canonical-form (format nil "~S" sexp))))
          (when *verbose*
            (format t "[~A] Raw: ~A~%[~A] S-Exp: ~S~%" request-id raw request-id sexp)
            (finish-output))
          ast)
      (error (e)
        (format t "[ERR ~A] ~A~%" request-id e)
        (finish-output)
        (pb:make-search-query :request-id request-id)))))

(defun start (&key (port 50052) (workers 8) (verbose nil))
  (setf *verbose* verbose)
  (grpc:init-grpc)
  (format t "=== EBusta DSL Engine V21 [Port ~A, Workers ~A, Verbose ~A] ===~%" port workers *verbose*)
  (finish-output)
  (grpc:run-grpc-proto-server (format nil "0.0.0.0:~A" port) 'pb:message-converter)
  (loop (sleep 1)))

(defun build-binary ()
  #+sbcl
  (sb-ext:save-lisp-and-die "dsl-converter"
                            :executable t
                            :toplevel (lambda ()
                                        (let ((args sb-ext:*posix-argv*))
                                          (start :port 50052 
                                                 :workers 8
                                                 :verbose (or (member "-v" args :test #'string=)
                                                              (member "--verbose" args :test #'string=)))))))
