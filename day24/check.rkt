#lang racket

(module+ test
  (require rackunit racket/runtime-path "solution.rkt")
  (define-runtime-path input "input")
  (check-equal? (call-with-values (thunk (part1+2 input)) list) '(79197919993985 13191913571211)))
