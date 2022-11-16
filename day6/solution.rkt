#lang racket

(provide (all-defined-out))

(require "../common.rkt"
         qi)

(define (make-fishes fishes)
  (for/fold ([f (hash)])
    ([fish (in-list fishes)])
    (hash-update f fish add1 0)))

(define (step fishes)
  (for/fold ([f (hash)])
    ([(age n) (in-hash fishes)])
    (if (zero? age)
      (~> (f) (hash-set 8 n) (hash-set 6 n))
      (hash-update f (sub1 age) (flow (+ n)) 0))))

(define (simulate fishes n)
  (let loop ([fishes fishes]
             [n n])
    (if (<= n 0)
      fishes
      (loop (step fishes) (sub1 n)))))

(define (count-all-generations fishes days-left)
  (define fishes* (make-fishes fishes))
  (apply + (hash-values (simulate fishes* days-left))))

(define-flow part1* (~> (count-all-generations 80)))
(define-flow part1 (~> (file->list read-ignore-comma) part1*))
(define-flow part2* (~> (count-all-generations 256)))
(define-flow part2 (~> (file->list read-ignore-comma) part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
