#lang racket

(require qi
         memoize)

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
  (~> (path)
      (if (~> car (equal? "end"))
        _
        (~>> car
             (hash-ref caves) sep
             (pass (visit? path))
             (amp (~>> (cons _ path) (traverse caves visit?)))))))

(define-flow big?
  (~> string->list sep (all char-upper-case?)))

(define-flow part1*
  (~> (traverse (flow (or (~> 1> big?) (not member))))
      count))
(define-flow part1 (~> file->lines lines->caves part1*))

(define/memo (has-doubled? path)
  (~>> (path)
       sep (pass (not big?))
       collect (group-by identity) sep
       (none (~> length (= 2)))))

(define-flow part2*
  (~> (traverse (flow (switch
                        [(~> 1> big?) #t]
                        [(not member) #t]
                        [(~> 1> (equal? "start")) #f]
                        [else (~> 2> has-doubled?)])))
      count))
(define-flow part2 (~> file->lines lines->caves part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
