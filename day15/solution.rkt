#lang racket

(provide (all-defined-out))

(require qi
         graph)

(struct posn [x y] #:transparent)

(define (make-directed-graph size weights)
  (weighted-graph/directed
    (for*/list ([y (in-range size)]
                [x (in-range size)]
                [dx (in-list '(-1 0 1))]
                [dy (in-list '(-1 0 1))]
                #:when (~> (dx dy) (and (not (all zero?)) (any zero?)))
                [xy* (in-value (~> (x y) (== (+ dx) (+ dy)) posn))]
                #:when (~> (xy*) (-< posn-x posn-y) (and (all (>= 0)) (all (< size)))))
      (list (vector-ref weights (xy->i size x y))
            xy* (posn x y)))))

(define-flow (list->graph+size ns)
  (~> (-< length
          (~> sep
              (amp (~> ~a string->list sep
                       (amp (~> string open-input-string read))))
              vector))
      (-< make-directed-graph 1>)))
(define-flow file->graph+size (~> file->list list->graph+size))

(define (step-weights weights)
  (vector-map
    (flow (~> add1 (if (> 9) 1 _)))
    weights))

(define (iterate n f init)
  (for/fold ([acc (list init)])
    ([_ (in-range n)])
    (cons (f (car acc)) acc)))

(define-flow (xy->i n x y)
  (~> (-< 2> (~> (block 2) *))
      +))
(define-flow (i->xy n i)
  (~> X quotient/remainder X))

;; w0 w1 w2 w3 w4
;; w1 w2 w3 w4 w5
;; w2 w3 w4 w5 w6
;; w3 w4 w5 w6 w7
;; w4 w5 w6 w7 w8
;;
;; the first |size| rows are from w0 to w4, in order, taking indices
;; - 0 to (n-1),
;; - n to (2n-1),
;; - 2n to (3n-1),
;; - … up to n(n-1) to (n² -1)
;;
;; subsequent rows use the same indexing pattern, but incrementing the wn by 1
(define (make-big-graph-weights size w0 w1 w2 w3 w4 w5 w6 w7 w8)
  (define big-size (* 5 size))
  (define w (vector w0 w1 w2 w3 w4
                    w1 w2 w3 w4 w5
                    w2 w3 w4 w5 w6
                    w3 w4 w5 w6 w7
                    w4 w5 w6 w7 w8))
  (build-vector
    (sqr big-size)
    (flow
      (~>> (i->xy big-size)
           (-< (~>> (amp (quotient size)) (xy->i 5) ;; iw
                    (vector-ref w)) ;; (vector-ref w iw)
               (~>> (amp (remainder size)) (xy->i size))) ;; i-in-w
           vector-ref)))) ;; (vector-ref (vector-ref w iw) i-in-w)

(define (list->big-graph+size ns)
  (define-values (size weights)
    (~> (ns)
        (-< length (~> sep
                       (amp (~> ~a string->list sep
                                (amp (~> string open-input-string read))))
                       vector))))
  (define big-size (* 5 size))
  (match-define (list w8 w7 w6 w5 w4 w3 w2 w1 (== weights))
    (iterate 8 step-weights weights))
  (values (make-directed-graph big-size (make-big-graph-weights size weights w1 w2 w3 w4 w5 w6 w7 w8))
          big-size))
(define-flow file->big-graph+size (~> file->list list->big-graph+size))

(define (part1* g size)
  (~> (g)
      (dijkstra _ (posn 0 0)) 1>
      (hash-ref (posn (sub1 size) (sub1 size)))))
(define-flow part1 (~> file->graph+size part1*))
(define part2* part1*)
(define-flow part2 (~> file->big-graph+size part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
