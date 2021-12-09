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
                    (hash-ref grid _ 9)
                    (< height))))
    (cons coord height)))

(define ((basin-size* grid) low-point seen)
  (match-define `((,x . ,y) . ,height) low-point)
  (for*/fold ([size 1]
              [seen seen])
    ([dx (in-list '(-1 0 1))]
     [dy (in-list '(-1 0 1))]
     #:when (~> (dx dy) (and (not (all zero?)) (any zero?))))
    (~>> (x y)
         (== (+ dx) (+ dy))
         cons
         (-< _ (hash-ref grid _ 9))
         (if (and% (not (set-member? seen _))
                   (and (< height _) (not (>= 9))))
           (~> (-< cons (~>> 1> (set-add seen)))
               (esc (basin-size* grid))
               (== (+ size) _))
           (gen size seen)))))

(define (basin-size low-point grid)
  (~> (low-point)
      ((basin-size* grid) (set))
      1>))

(define-flow (basin-sizes grid)
  (~>> low-points
       sep
       (amp (basin-size grid))
       collect))

(define-flow (top3 xs)
  (~> (sort >)
      (take 3)
      sep))

(define-flow part1*
  (~> lines->grid low-points sep (amp (~> cdr risk)) +))
(define-flow part1 (~> file->lines part1*))

(define-flow part2*
  (~> lines->grid basin-sizes top3 *))
(define-flow part2 (~> file->lines part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
