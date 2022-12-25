#lang racket

(provide (all-defined-out))

(require qi
         (prefix-in list: racket/list))

(define-flow partn (~>> (-< drop-right drop) (list:count <)))

(define-flow part1* (partn 1))
(define-flow part1 (~> file->list part1*))
(define-flow part2* (partn 3))
(define-flow part2 (~> file->list part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
