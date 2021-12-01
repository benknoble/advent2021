#lang racket

(provide (all-defined-out))

(define (part1* scans)
  (count < (drop-right scans 1) (drop scans 1)))

(define part1 (compose1 part1* file->list))

(module+ main
  (command-line
    #:args (input)
    (part1 input)))
