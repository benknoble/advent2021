#lang racket

(provide (all-defined-out))

(require qi)

(define-flow lines->rows
  (~> sep
      (amp (~> string->list sep
               (amp (~> string open-input-string read))
               collect))
      collect))

(define (rows->grid rows)
  (for*/hash ([(row y) (in-indexed (in-list rows))]
              [(col x) (in-indexed (in-list row))])
    (values (cons x y) (cons col #f))))

(define-flow lines->grid (~> lines->rows rows->grid))
(define-flow file->grid (~> file->lines lines->grid))

(define (display-grid grid)
  (define-values (xh yh)
    (~>> (grid)
         hash-keys
         sep
         (-< (~> (amp car) max add1)
             (~> (amp cdr) max add1))))
  (for ([y (in-range yh)])
    (for ([x (in-range xh)])
      (display (hash-ref grid (cons x y))))
    (printf "\n")))

(define (increase-energy grid coords)
  (for/fold ([grid grid])
    ([coord (in-list coords)]
     #:when (hash-has-key? grid coord))
    (hash-update grid coord (flow (~> (-< (~> car add1) cdr) cons)))))

(define-flow (increase-all-energy grid)
  (~> (-< _ hash-keys)
      increase-energy))

(define (flash-once grid)
  (for*/fold ([grid grid])
    ([(coord oct) (in-hash grid)]
     #:when (~> (oct) (-< car cdr) (and% (> 9) false?))
     [dx (in-list '(-1 0 1))]
     [dy (in-list '(-1 0 1))])
    (if (~> (dx dy) (all zero?))
      ;; mark flashed
      (hash-update grid coord (flow (~> (-< car #t) cons)))
      ;; increase energy
      (~>> (coord)
           (-< car cdr)
           (== (+ dx) (+ dy))
           cons
           list
           (increase-energy grid)))))

(define-flow (flash grid)
  (~> (-< _ #f)
      (feedback (while (not equal?))
                (then 1>)
                (-< (~> 1> flash-once) 1>))))

(define (reset-flash grid)
  (for/fold ([grid grid]
             [flashes 0])
    ([(coord oct) (in-hash grid)]
     #:when (~> (oct) cdr))
    (~> (grid flashes)
        (== (hash-set coord (cons 0 #f))
            add1))))

(define (step grid [flashes 0])
  (~> (grid)
      (~> increase-all-energy flash reset-flash)
      (== _ (+ flashes))))

(define (stepN n) (flow (feedback n step)))

(define-flow part1*
  (~> (stepN 100) 2>))
(define-flow part1 (~> file->grid part1*))

(define-flow part2*
  (~> (-< _ 0 0)
      (feedback (while (~> (-< (~> 1> hash-count) 2>) (not =)))
                (then 3>)
                (-< (~> 1> step)
                    (~> 3> add1)))))
(define-flow part2 (~> file->grid part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
