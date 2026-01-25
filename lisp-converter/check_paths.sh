sbcl --noinform --load "$HOME/quicklisp/setup.lisp" \
     --eval '(format t "~%--- ПРОВЕРКА ПУТЕЙ ---~%")' \
     --eval '(format t "cl-protobufs: ~A~%" (ql:where-is "cl-protobufs"))' \
     --eval '(format t "grpc: ~A~%" (ql:where-is "grpc"))' \
     --quit
