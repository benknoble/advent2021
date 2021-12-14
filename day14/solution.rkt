#lang racket

(require qi)

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
                          (== (~> string->list sep cons)
                              (~> string->list car))))
                 hash))))
(define-flow file->pairs+rules (~> file->lines lines->pairs+rules))

(define (step pairs rules)
  (append-map
    (λ (p)
      (define insert (hash-ref rules p p))
      (~> (p)
          (-< (~> car (cons insert))
              (~> cdr (cons insert _)))
          list))
    pairs))

(define (stepN n pairs rules)
  (~> (pairs rules)
      (feedback n (-< step 2>))
      1>))

(define-flow (solve n pairs rules)
  (~>> stepN
       recover-polymer
       (group-by identity)
       (map length) sep
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
