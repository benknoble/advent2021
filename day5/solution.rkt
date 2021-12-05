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

(define/match (cardinal-segment->list s)
  [((segment (point x y1) (point x y2)))
   (build-list (add1 (abs (- y1 y2)))
               (λ (y*) (point x (+ y* (min y1 y2)))))]
  [((segment (point x1 y) (point x2 y)))
   (build-list (add1 (abs (- x1 x2)))
               (λ (x*) (point (+ x* (min x1 x2)) y)))])

(define (points-covered segments)
  (transduce segments
             (append-mapping cardinal-segment->list)
             #:into into-multiset))

(define-flow (count-overlaps segments)
  (~>> points-covered
       multiset-frequencies
       hash-values
       (count (flow (>= 2)))))

(define-flow part1* (~>> list->segments (filter segment-cardinal?) count-overlaps))
(define-flow part1 (~> (file->list my-read) part1*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))))
