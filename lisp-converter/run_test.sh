#!/bin/bash
sbcl --noinform --eval '(ql:register-local-projects)' \
     --load "/home/serge/projects/ebusta/lisp-converter/schema.lisp" \
     --load "/home/serge/projects/ebusta/lisp-converter/parser.lisp" \
     --eval '(let* ((dsl-single '(:field "title" :val "Master"))
                    (dsl-multi '(:and (:field "author" :val "Bulgakov") (:field "year" :val "1940")))
                    (pb-single (ebusta.parser:to-pb dsl-single))
                    (pb-multi (ebusta.parser:to-pb dsl-multi)))
               (format t "~%--- [ TEST 1: SINGLE FILTER ] ---~%")
               (cl-protobufs:print-text-format pb-single :stream t)
               (format t "~%--- [ TEST 2: AND LOGIC ] ---~%")
               (cl-protobufs:print-text-format pb-multi :stream t))' \
     --quit
