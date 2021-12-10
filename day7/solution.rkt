#lang racket

(require qi
         math/statistics
         "../common.rkt")

(define-flow best-position-linear
  (median < _))

(define-flow (dist p1 p2) (~> - abs))

;; best -> posn -> cost
(define-flow (coster-linear best)
  (clos dist))

(define-flow (coster-quad best)
  (clos (~> dist (-< _ add1) * (/ 2))))

;; (best -> posn -> cost) -> best -> posns -> cost
(define-flow (cost coster best posns)
  (~> (-< (~> (select 1 2) apply) ;; (coster best)
          (~> 3> sep)) ;; posns
      amp +))

(define-flow (cost-linear best posns) (cost coster-linear __))
(define-flow (cost-quad best posns) (cost coster-quad __))

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
