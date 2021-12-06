#lang typed/racket

(provide (all-defined-out))

(require math/matrix)

(define step : (Matrix Number)
  (matrix ([0 1 0 0 0 0 0 0 0]
           [0 0 1 0 0 0 0 0 0]
           [0 0 0 1 0 0 0 0 0]
           [0 0 0 0 1 0 0 0 0]
           [0 0 0 0 0 1 0 0 0]
           [0 0 0 0 0 0 1 0 0]
           [1 0 0 0 0 0 0 1 0]
           [0 0 0 0 0 0 0 0 1]
           [1 0 0 0 0 0 0 0 0])))


(define (make-fishes [fishes : (Listof Number)]) : (Matrix Number)
  (define num-each-fish
    (for/hash : (HashTable Number Number)
      ([group (in-list (group-by (λ (x) x) fishes))])
      (values (first group) (length group))))
  (build-matrix 9 1 (λ ([y : Index] [_ : Index])
                      (hash-ref num-each-fish y (const 0)))))

(define (simulate [fishes : (Matrix Number)] [days : Integer]) : (Matrix Number)
  (matrix* (matrix-expt step days)
           fishes))

(define (solution [fish : (Listof Number)] [days : Integer])
  (matrix-1norm (simulate (make-fishes fish) days)))

(define (part1* [fish : (Listof Number)])
  (solution fish 80))
(define (part2* [fish : (Listof Number)])
  (solution fish 256))

(module* main racket
  (require (submod "..")
           "../common.rkt"
           qi)
  (define-flow part1 (~> (file->list read-ignore-comma) part1*))
  (define-flow part2 (~> (file->list read-ignore-comma) part2*))
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
