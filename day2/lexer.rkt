#lang racket

(provide lexer
         tokenizer)

(require brag/support)

(define lexer
  (lexer-srcloc
    [whitespace (token lexeme #:skip? #t)]
    ["forward" (token 'FORWARD lexeme)]
    ["down" (token 'DOWN lexeme)]
    ["up" (token 'UP lexeme)]
    [(:+ numeric) (token 'NUMBER (string->number lexeme))]))

(define (tokenizer in src)
  (port-count-lines! in)
  (lexer-file-path src)
  (thunk (lexer in)))
