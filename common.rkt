#lang racket

(provide (all-defined-out))

(define (read-ignore-comma in)
  (match (read in)
    [(list 'unquote x) x]
    [x x]))
