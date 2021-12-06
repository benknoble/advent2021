#lang racket

(require "../common.rkt"
         qi)

(define (simulate fishes days-left)
  (let loop ([n days-left]
             [fishes fishes])
    (if (<= n 0)
      fishes
      (loop (sub1 n)
            (append-map (λ (fish)
                          (if (zero? fish)
                            (list 6 8)
                            (list (sub1 fish))))
                        fishes)))))

(define-flow part1* (~> (simulate 80) length))
(define-flow part1 (~> (file->list read-ignore-comma) part1*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))))
