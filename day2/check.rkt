#lang racket

(module+ test
  (require rackunit)

  (define out (with-output-to-string (thunk (dynamic-require "solution.rkt" #f))))
  (define lines (port->lines (open-input-string out)))
  (define answer (read (open-input-string (second lines))))
  (check-equal? answer (list 1936494 1997106066)))
