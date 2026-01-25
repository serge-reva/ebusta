sbcl --noinform --load "$HOME/quicklisp/setup.lisp" \
     --eval '(ql:quickload :cl-protobufs)' \
     --eval '(do-all-symbols (s)
              (when (and (search "SERVICE" (symbol-name s))
                         (fboundp s))
                (format t "FOUND: ~A:~A~%" (package-name (symbol-package s)) s)))' \
     --quit
