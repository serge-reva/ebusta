(ql:quickload '(:cl-protobufs :grpc) :silent t)
(load "/home/serge/projects/ebusta/lisp-converter/search.lisp")

(format t "~%=== 1. ПРОВЕРКА ЭКСПОРТОВ ПАКЕТА GRPC ===~%")
(let ((symbols '()))
  (do-external-symbols (s :grpc) (push s symbols))
  (format t "External symbols in GRPC: ~{~A~^, ~}~%" (sort symbols #'string<)))

(format t "~%=== 2. ГЛУБОКАЯ ИНСПЕКЦИЯ СЕРВИСА ===~%")
(let* ((sym 'cl-protobufs.ebusta.library.v1:message-converter)
       (sd (cl-protobufs:find-service-descriptor sym)))
  (if sd
      (progn
        (format t "Descriptor found: ~S~%" sd)
        (describe sd)
        (let ((methods (ignore-errors (cl-protobufs:proto-methods sd))))
          (format t "~%Methods list: ~S~%" methods)
          (dolist (m methods)
            (format t "~%--- Инспекция объекта метода ---~%")
            (describe m))))
      (format t "Descriptor NOT found.~%")))
(finish-output)
