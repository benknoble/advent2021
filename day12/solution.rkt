#lang racket

(require qi)

(define-flow lines->caves
  (~>> sep
       (amp (~> (string-split "-") sep
                (-< (~> _ cons)
                    (~> X cons))))
       collect (group-by car) sep
       (amp (~> (-< (~> car car)
                    (map cdr _))))
       hash))
(define-flow file->caves (~> file->lines lines->caves))

(define (traverse caves visit? [path '("start")])
  (if (~> (path) car (equal? "end"))
    path
    (~>> (path)
         car
         (hash-ref caves) sep
         (pass (visit? path))
         (amp (~>> (cons _ path) (traverse caves visit?))))))

(define-flow big?
  (~> 1> string->list sep (all char-upper-case?)))

(define-flow part1*
  (~> (traverse (flow (or big? (not member))))
      count))
(define-flow part1 (~> file->lines lines->caves part1*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))))
