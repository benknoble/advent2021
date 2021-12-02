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
         handle-directions1
         handle-directions2)

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
    (displayln (time (handle-directions1 dirs)))
    (displayln (time (handle-directions2 dirs)))))

(define (handle-directions1 directions)
  (for/fold ([pos 0] [depth 0] #:result (* pos depth))
    [(direction (in-list directions))]
    (match direction
      [`(forward ,num) (values (+ pos num) depth)]
      [`(up ,num) (values pos (- depth num))]
      [`(down ,num) (values pos (+ depth num))])))

(define (handle-directions2 directions)
  (for/fold ([pos 0] [depth 0] [aim 0] #:result (* pos depth))
    [(direction (in-list directions))]
    (match direction
      [`(forward ,num) (values (+ pos num) (+ depth (* aim num)) aim)]
      [`(up ,num) (values pos depth (- aim num))]
      [`(down ,num) (values pos depth (+ aim num))])))

(define-syntax-parse-rule (forward num:number)
  `(forward ,num))

(define-syntax-parse-rule (up num:number)
  `(up ,num))

(define-syntax-parse-rule (down num:number)
  `(down ,num))
