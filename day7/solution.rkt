#lang racket

(require qi
         math/statistics
         "../common.rkt")

(define-flow best-position-linear
  (median < _))

(define (cost-linear best posns)
  (~> (posns)
       sep
       (amp (~> (- best) abs))
       +))

(define (cost-quad best posns)
  (~> (posns)
      sep
      (amp (~> (- best) abs (-< _ add1) * (/ 2)))
      +))

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
