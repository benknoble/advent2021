#lang racket

(provide #%top-interaction
         #%app
         #%datum
         #%top
         (rename-out [module-begin #%module-begin])
         course
         forward
         up
         down
         handle-directions)

(module reader racket
  (provide read-syntax)

  (require "lexer.rkt"
           "parser.rkt"
           syntax/strip-context)

  (define (read-syntax src in)
    (strip-context
      #`(module sub advent2021/day2/sub
          #,(parse src (tokenizer in src))))))

(require syntax/parse/define)

(define-syntax-parse-rule (module-begin course)
  (#%module-begin
   course))

(define-syntax-parse-rule (course direction ...)
  (begin
    (define dirs (list direction ...))
    (displayln (time (handle-directions dirs)))))

(define (handle-directions directions)
  (for/fold ([pos 0]
             [depth 0]
             [aim 0] ;; like depth in part1
             #:result (list (* pos aim) (* pos depth)))
    [(direction (in-list directions))]
    (direction pos depth aim)))

(define-syntax-parse-rule (forward num:number)
  (λ (pos depth aim)
    (values (+ pos num) (+ depth (* aim num)) aim)))

(define-syntax-parse-rule (up num:number)
  (λ (pos depth aim)
    (values pos depth (- aim num))))

(define-syntax-parse-rule (down num:number)
  (λ (pos depth aim)
    (values pos depth (+ aim num))))
