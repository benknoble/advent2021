#lang racket

(require qi
         math/statistics
         "../common.rkt")

(define-flow best-position
  (median < _))

(define (cost best posns)
  (~> (posns)
       sep
       (amp (~> (- best) abs))
       +))

(define-flow part1* (~> (-< best-position _) cost))
(define-flow part1 (~> (file->list read-ignore-comma) part1*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))))
