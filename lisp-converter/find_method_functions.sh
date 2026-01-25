sbcl --noinform --load "$HOME/quicklisp/setup.lisp" \
     --eval '(ql:quickload :cl-protobufs :silent t)' \
     --eval '(do-external-symbols (s :cl-protobufs)
              (let ((name (symbol-name s)))
                (when (or (search "METHOD" name) (search "NAME" name))
                  (format t "EXPORT: CL-PROTOBUFS:~A~%" name))))' \
     --quit
