#lang racket

(require qi
         graph)

(define (list->graph+size ns)
  (define-values (size weights)
    (~> (ns)
        (-< length (~> sep
                       (amp (~> ~a string->list sep
                                (amp (~> string open-input-string read))))
                       vector))))

  (values (weighted-graph/directed
            (for*/list ([y (in-range size)]
                        [x (in-range size)]
                        [dx (in-list '(-1 0 1))]
                        [dy (in-list '(-1 0 1))]
                        #:when (~> (dx dy) (and (not (all zero?)) (any zero?)))
                        [xy* (in-value (~> (x y) (== (+ dx) (+ dy)) cons))]
                        #:when (~> (xy*) (-< car cdr) (and (all (>= 0)) (all (< size)))))
              (list (vector-ref weights (+ x (* size y)))
                    xy* (cons x y))))
          size))
(define-flow file->graph+size (~> file->list list->graph+size))

(define (part1* g size)
  (~> (g)
      (dijkstra _ (cons 0 0)) 1>
      (hash-ref (cons (sub1 size) (sub1 size)))))
(define-flow part1 (~> file->graph+size part1*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))))
