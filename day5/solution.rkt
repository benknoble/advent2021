#lang racket

(require rebellion/streaming/transducer
         rebellion/streaming/reducer
         rebellion/collection/list
         rebellion/collection/multiset
         qi)

(define (my-read in)
  (match (read in)
    [(list 'unquote x) x]
    [x x]))

(struct point [x y] #:transparent)
(struct segment [a b] #:transparent)

(define (structing num-fields constructor)
  (batching (reducer-limit
              (reducer-map into-list #:range (flow (~> sep constructor)))
              num-fields)))

(define (list->segments xs)
  (transduce xs
             (filtering number?)
             (structing 2 point)
             (structing 2 segment)
             #:into into-list))

(define/match (segment-cardinal? s)
  [((segment (point x1 y1) (point x2 y2)))
   (or (= x1 x2) (= y1 y2))])

(define/match (segment->list s)
  ;; horizontal
  [((segment (point x y1) (point x y2)))
   (define y0 (min y1 y2))
   (build-list (add1 (abs (- y1 y2)))
               (flow (~>> (+ y0) (point x))))]
  ;; vertical
  [((segment (point x1 y) (point x2 y)))
   (define x0 (min x1 x2))
   (build-list (add1 (abs (- x1 x2)))
               (flow (~> (+ x0) (point y))))]
  ;; diagonal (45 degrees)
  [((segment (point x1 y1) (point x2 y2)))
   (define (range-direction a b) (if (<= a b) 1 -1))
   (define x-direction (range-direction x1 x2))
   (define y-direction (range-direction y1 y2))
   (for/list ([x (in-range x1 (+ x2 x-direction) x-direction)]
              [y (in-range y1 (+ y2 y-direction) y-direction)])
     (point x y))])

(define-flow (points-covered segments)
  (~>> sep
       (amp (~> segment->list sep))
       multiset))

(define-flow (count-overlaps segments)
  (~>> points-covered
       multiset-frequencies
       hash-values
       (count (flow (>= 2)))))

(define-flow part1* (~>> list->segments (filter segment-cardinal?) count-overlaps))
(define-flow part1 (~> (file->list my-read) part1*))

(define-flow part2* (~> list->segments count-overlaps))
(define-flow part2 (~> (file->list my-read) part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
