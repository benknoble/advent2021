#lang racket

(require qi
         math/statistics
         "../common.rkt")

(define-flow best-position-linear
  (median < _))

(define-flow (cost-linear best posns)
  (~> (== (clos (~> X - abs)) sep)
      amp +))

(define-flow (cost-quad best posns)
  (~> (== (clos (~> X - abs (-< _ add1) * (/ 2))) sep)
      amp +))

(define (best-position-quad posns)
  (~>> (posns)
       mean
       (-< floor ceiling)
       collect
       (argmin (flow (cost-quad posns)))))

(define-flow part1* (~> (-< best-position-linear _) cost-linear))
(define-flow part1 (~> (file->list read-ignore-comma) part1*))
(define-flow part2* (~> (-< best-position-quad _) cost-quad))
(define-flow part2 (~> (file->list read-ignore-comma) part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
