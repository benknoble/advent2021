#lang racket

(require qi)

(define risk add1)

(define-flow lines->rows
  (~> sep
      (amp (~> string->list sep
               (amp (~> string open-input-string read))
               collect))
      collect))

(define (rows->grid rows)
  (for*/hash ([(row y) (in-indexed (in-list rows))]
              [(col x) (in-indexed (in-list row))])
    (values (cons x y) col)))

(define-flow lines->grid (~> lines->rows rows->grid))

(define (low-points grid)
  (for/list ([(coord height) (in-hash grid)]
             #:when
             (for*/and ([dx (in-list '(-1 0 1))]
                        [dy (in-list '(-1 0 1))]
                        #:when (~> (dx dy) (and (not (all zero?)) (any zero?))))
               (~>> (coord)
                    (-< (~> car (+ dx))
                        (~> cdr (+ dy)))
                    cons
                    (hash-ref grid _ 10)
                    (< height))))
    height))

(define-flow part1*
  (~> lines->grid low-points sep (amp risk) +))
(define-flow part1 (~> file->lines part1*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))))
