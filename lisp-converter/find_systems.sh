sbcl --noinform --load "$HOME/quicklisp/setup.lisp" \
     --eval '(ql:quickload "cl-protobufs" :silent t)' \
     --eval '(ql:quickload "grpc" :silent t)' \
     --eval '(format t "~%--- ПУТИ К ИСХОДНИКАМ ---~%")' \
     --eval '(format t "cl-protobufs: ~A~%" (asdf:system-source-directory (asdf:find-system "cl-protobufs")))' \
     --eval '(format t "grpc: ~A~%" (asdf:system-source-directory (asdf:find-system "grpc")))' \
     --quit
