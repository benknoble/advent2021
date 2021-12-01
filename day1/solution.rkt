#lang racket

(provide (all-defined-out))

(require qi)

(define-flow partn
  (~>> X (-< drop-right drop)
       (count <)))

(define part1* (curry partn 1))
(define part1 (compose1 part1* file->list))
(define part2* (curry partn 3))
(define part2 (compose1 part2* file->list))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
