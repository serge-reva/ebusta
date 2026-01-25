(ql:quickload '(:cl-protobufs :grpc) :silent t)
(load "/home/serge/projects/ebusta/lisp-converter/search.lisp")

(format t "~%--- Проверка поиска сервиса ---~%")
(let* ((sym 'cl-protobufs.ebusta.library.v1:message-converter)
       (sd (cl-protobufs:find-service-descriptor sym))
       (s (cl-protobufs.implementation:find-service sym)))
  (format t "Символ: ~A~%" sym)
  (format t "Service Descriptor: ~A~%" sd)
  (format t "Service Object: ~A~%" s)
  
  (when sd
    (format t "Методы через Descriptor: ~A~%" 
            (ignore-errors (cl-protobufs:proto-methods sd))))
  (when s
    (format t "Методы через Service: ~A~%" 
            (ignore-errors (cl-protobufs:proto-methods s)))))
(finish-output)
