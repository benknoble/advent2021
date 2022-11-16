#lang racket

(provide #%datum #%app quote
         (rename-out [module-begin #%module-begin]))

(require syntax/parse/define
         (for-syntax racket/base)
         rackunit
         racket/runtime-path)

(define-syntax-parse-rule (module-begin part1-ans:expr {~optional part2-ans:expr})
  ;; #:do [(println (syntax-source-module this-syntax #t))
  ;;       (println (syntax-source this-syntax))]

  #| Make the runtime-path relative to the parsed file, not this expander |#
  #| module. Uncomment the `#:do` blocks to see the difference, noting that |#
  #| `define-runtime-path` uses `syntax-source-module` before `syntax-source`. |#

  ;; Syntax-quote `define-runtime-path` so it has a binding. Syntax-quote
  ;; `"input"` because otherwise I get an error when expanding modules in this
  ;; `#lang`:
  ;; test.rkt:1:6: ?: literal data is not allowed;
  ;;  no #%datum syntax transformer is bound in the transformer phase
  ;;   at: "input"
  ;;   location...:
  ;;    test.rkt:1:6
  ;; This in spite of providing `#%datum`. Providing `(for-syntax #%datum)`
  ;; doesn't work either.
  #:with input-defn (datum->syntax #f (list #'define-runtime-path 'input #'"input") this-syntax)

  ;; #:do [(println (syntax-source-module #'input-defn #t))
  ;;       (println (syntax-source #'input-defn))]

  (#%module-begin
   (module+ test
     (require "solution.rkt")
     input-defn
     (check-equal? (part1 input) part1-ans)
     (~? (check-equal? (part2 input) part2-ans)))))

(module reader syntax/module-reader advent2021/check)
