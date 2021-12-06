#lang racket

(require "../common.rkt"
         qi
         memoize)

(module+ test (require rackunit))

(define/memo (children-days fish days-left)
  (cond
    [(days-left . <= . fish) #f]
    [else
      (define creation-day (add1 fish))
      (define days-left* (- days-left creation-day))
      (define children (add1 (quotient days-left* 7)))
      (list children
            (build-list children (flow (~>> (* 7) (+ creation-day) (- days-left)))))]))

(module+ test
  (check-equal? (children-days 3 0) #f)
  (check-equal? (children-days 3 1) #f)
  (check-equal? (children-days 3 2) #f)
  (check-equal? (children-days 3 3) #f)
  (check-equal? (children-days 3 4) '(1 (0)))
  (check-equal? (children-days 3 5) '(1 (1)))
  (check-equal? (children-days 3 12) '(2 (8 1)))
  (check-equal? (children-days 3 13) '(2 (9 2)))
  (check-equal? (children-days 3 20) '(3 (16 9 2))))

(define (next-generation birthdays)
  (filter-map (flow (~>> (children-days 8))) birthdays))

(define/memo (count-generations fish days-left)
  (match  (children-days fish days-left)
    [#f 1]
    [`(,size ,days-left-per-child)
      (let loop ([size (add1 size)]
                 [days-left-per-child days-left-per-child])
        (cond
          [(empty? days-left-per-child) size]
          [else
            (~> (days-left-per-child)
                next-generation
                (-< (~>> (map first) sep (+ size))
                    (~>> (append-map second)))
                loop)]))]))

(module+ test
  (check-equal? (count-generations 3 0) 1)
  (check-equal? (count-generations 3 1) 1)
  (check-equal? (count-generations 3 2) 1)
  (check-equal? (count-generations 3 3) 1)
  (check-equal? (count-generations 3 4) 2)
  (check-equal? (count-generations 3 5) 2)
  (check-equal? (count-generations 3 12) 3)
  (check-equal? (count-generations 3 13) 4)
  (check-equal? (count-generations 3 20) 7))

(define (count-all-generations fishes days-left)
  (define fishes*
    (for/fold ([f (hash)])
      ([fish (in-list fishes)])
      (hash-update f fish add1 0)))
  (for/sum ([(fish num-fish) (in-hash fishes*)])
    (* num-fish (count-generations fish days-left))))

(module+ test
  (check-equal? (count-all-generations '(3) 0) 1)
  (check-equal? (count-all-generations '(3) 1) 1)
  (check-equal? (count-all-generations '(3) 2) 1)
  (check-equal? (count-all-generations '(3) 3) 1)
  (check-equal? (count-all-generations '(3) 4) 2)
  (check-equal? (count-all-generations '(3) 5) 2)
  (check-equal? (count-all-generations '(3) 12) 3)
  (check-equal? (count-all-generations '(3) 13) 4)
  (check-equal? (count-all-generations '(3) 20) 7)
  (check-equal? (count-all-generations '(3 4 3 1 2) 18) 26)
  (time (check-equal? (count-all-generations '(3 4 3 1 2) 80) 5934))
  #;(time (check-equal? (count-all-generations '(3 4 3 1 2) 256) 26984457539)))

(define-flow part1* (~> (count-all-generations 80)))
(define-flow part1 (~> (file->list read-ignore-comma) part1*))
(define-flow part2* (~> (count-all-generations 256) length))
(define-flow part2 (~> (file->list read-ignore-comma) part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
