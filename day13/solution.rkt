#lang racket

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

(define (fold1 instruction points)
  (match instruction
    [`(x ,n)
      (~> (points)
          (set-map (flow (if (~> x (>= n))
                           (~> (-< (~>> x (- n) abs (- n))
                                   y)
                               cons)
                           _)))
          sep set)]
    [`(y ,n)
      (~> (points)
          (set-map (flow (if (~> y (>= n))
                           (~> (-< x
                                   (~>> y (- n) abs (- n)))
                               cons)
                           _)))
          sep set)]))

(define (fold instructions points)
  (foldl fold1 points instructions))

(define-flow part1*
  (~> X (== car _) fold1 set-count))
(define-flow part1 (~> file->manual part1*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))))
