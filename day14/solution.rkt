#lang racket

(provide (all-defined-out))

(require qi
         memoize)

(define-flow recover-polymer
  (~> (-< caar (map cdr _)) cons))

(define-flow lines->pairs+rules
  (~> sep
      (block 2)
      (group 1
             (~>> string->list
                  (-< (drop-right 1) (drop 1))
                  (map cons))
             (~> (amp (~> (string-split " -> ") sep
                          (== (~> string->list sep) (~> string->list car))
                          (-< (~> (select 1 2) cons)
                              (~> (-< (~> (select 1 3) cons)
                                      (~> (select 3 2) cons))
                                  list))))
                 hash))))
(define-flow file->pairs+rules (~> file->lines lines->pairs+rules))

(define/memo (stepN* n pairs rules)
  (if (zero? n)
    (for/fold ([counts (hash)])
      ([pair (in-list pairs)])
      (hash-update counts (cdr pair) add1 0))
    (for*/fold ([counts (hash)])
      ([pair (in-list pairs)]
       [(seg k) (in-hash (stepN* (sub1 n) (hash-ref rules pair) rules))])
      (hash-update counts seg (flow (+ k)) 0))))

(define (stepN n pairs rules)
  (hash-update (stepN* n pairs rules)
               (caar pairs)
               add1 0))

(define-flow (solve n pairs rules)
  (~>> stepN
       hash-values sep
       (-< max min)
       -))

(define-flow (part1* pairs rules) (solve 10 __))
(define-flow part1 (~> file->pairs+rules part1*))
(define-flow part2* (solve 40 __))
(define-flow part2 (~> file->pairs+rules part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
