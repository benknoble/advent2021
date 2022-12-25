#lang racket

(provide (all-defined-out))

(require qi
         "../common.rkt")

(define-flow (list->manual xs)
  (~> sep
      (sieve number?
             ;; ->points
             (~> collect (-< _ '())
                 (feedback (while (~> 1> (not empty?)))
                           (then 2>)
                           (-< (~> 1> cddr)
                               (~> (== (~> (take 2) sep cons) _)
                                   cons)))
                 sep set)
             ;; ->instructions
             (~> (amp (~> symbol->string (string-split "=")))
                 (pass (~> length (= 2)))
                 (amp (map (flow (~> open-input-string read)) _))
                 collect))))
(define-flow file->manual (~> (file->list read-ignore-comma) list->manual))

(define x car)
(define y cdr)

(define (update-first n)
  (flow (if (~> 1> (>= n))
          (~> (== (~>> (- n) abs (- n)) _))
          _)))

(define (fold1 instruction points)
  (match instruction
    [`(x ,n)
      (~> (points)
          (set-map (flow (~> (-< x y) (esc (update-first n)) cons)))
          sep set)]
    [`(y ,n)
      (~> (points)
          (set-map (flow (~> (-< y x) (esc (update-first n)) X cons)))
          sep set)]))

(define (fold instructions points)
  (foldl fold1 points instructions))

(define-flow part1*
  (~> X (== car _) fold1 set-count))
(define-flow part1 (~> file->manual part1*))

(define (points-graphic points)
  (define-values (xh yh)
    (~> (points) set->list sep
        (-< (~> (amp car) max add1)
            (~> (amp cdr) max add1))))
  (for/list ([y (in-range yh)])
    (for/list ([x (in-range xh)])
      (if (set-member? points (cons x y))
        #\█
        #\space))))

(define (display-points graphic)
  (for ([row (in-list graphic)])
    (~> (row) sep string (ε displayln))))

(define-flow part2*
  (~> X fold points-graphic (ε display-points _)))
(define-flow part2 (~> file->manual part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
