#lang racket

(provide (all-defined-out))

(require qi)

(define-flow line->i
  (~> open-input-string (-< read read read)
      (pass (not eof-object?))
      collect))
(define-flow lines->ins (amp line->i))
(define-flow file->ins (~> file->lines sep lines->ins))

;; 14-digit numbes (10^14) are bounded by 2^46 and 2^47 !
;; Reports of multiple hours to search the entire space by counting (not even
;; running the interpreter)

(define (lift f v)
  (λ (s . args)
    (hash-set v s (apply f args))))

(define v0 (hash 'w 0 'x 0 'y 0 'z 0))

(define (interpret v ins)
  (match ins
    ['() v]
    [(cons i ins)
     (~> (i) sep
         (-< 1> 2> (~> (block 1) (amp (~>> (fanout 2) (hash-ref v)))))
         (switch (% 1> (block 1))
           [(eq? 'inp)
            (esc (λ (s . _)
                   (flow (~> (hash-set v s _) (interpret ins)))))]
           [(eq? 'add) (~> (lift + v) (interpret ins))]
           [(eq? 'mul) (~> (lift * v) (interpret ins))]
           [(eq? 'div) (~> (lift quotient v) (interpret ins))]
           [(eq? 'mod) (~> (lift modulo v) (interpret ins))]
           [(eq? 'eql) (~> (lift (flow (if = 1 0)) v) (interpret ins))]))]))

(define-flow valid-state? (~> (hash-ref 'z) zero?))

;; read (inp n) from highest to lowest, so that the highest n is the first input
;; very inefficient on large inputs
;; the first 100 instructions of my input give a string of length 765127
(define (propagate init ins)
  (define input-n 0)
  (for/fold ([eqn init])
    ([i (in-list (reverse ins))])
    (define to-repl (~> (i) cadr symbol->string))
    (define repl
      (match i
        [`(inp ,_) (begin0 (format "(inp ~a)" input-n)
                           (set! input-n (add1 input-n)))]
        [`(add ,l ,r) (format "(~a + ~a)" l r)]
        [`(mul ,l ,r) (format "(~a * ~a)" l r)]
        [`(div ,l ,r) (format "(~a / ~a)" l r)]
        [`(mod ,l ,r) (format "(~a % ~a)" l r)]
        [`(eql ,l ,r) (format "(~a=~a/1/0)" l r)]))
    (string-replace eqn to-repl repl)))

(define (propagate-eqn e ins)
  (define input-n 0)
  (for/fold ([eqn e])
    ([i (in-list (reverse ins))])
    (define to-repl (cadr i))
    (define repl
      (optimize
        (match i
          [`(inp ,_)
            (begin0
              `(inp ,input-n)
              (set! input-n (add1 input-n)))]
          [`(add ,l ,r) `(,l + ,r)]
          [`(mul ,l ,r) `(,l * ,r)]
          [`(div ,l ,r) `(,l / ,r)]
          [`(mod ,l ,r) `(,l % ,r)]
          [`(eql ,l ,r) `(,l = ,r)])))
    (if (eq? to-repl repl)
      eqn
      (tree-sub eqn to-repl repl))))

(define (tree-sub* e xss)
  (for/fold ([e e])
    ([xs (in-list xss)])
    (tree-sub e (car xs) (cdr xs))))

(define (tree-sub e x s)
  (match e
    [(== x) s]
    [`(,l ,op ,r) (optimize `(,(optimize (tree-sub l x s))
                               ,op
                               ,(optimize (tree-sub r x s))))]
    [e e]))

(define optimize
  (match-lambda
    [`(,(? number? l) ,op ,(? number? r))
      ((case op
         [(+) +]
         [(*) *]
         [(/) quotient]
         [(%) modulo]
         [(=) (flow (if = 1 0))]
         [(<>) (flow (if (not =) 1 0))])
       l r)]
    [`(,x / 1) (optimize x)] [`(,x % 1) (optimize x)]
    [`(0 % ,_) 0]
    [`(0 + ,x) (optimize x)] [`(,x + 0) (optimize x)]
    [`(0 * ,_) 0] [`(,_ * 0) 0]
    [`(1 * ,x) (optimize x)] [`(,x * 1) (optimize x)]
    ;; we know the input will never be 0
    [`((inp ,_) = 0) 0]
    [`((inp ,_) <> 0) 1]
    [`((,l = ,r) = 0)
      (optimize `(,(optimize l) <> ,(optimize r)))]
    [e e]))

;; my total expression compiles to a 134630137-char-long string
(define (pi n [fin #f])
  (~> ("input") file->ins collect
      (-< _ (~> length (- n)))
      drop
      (propagate-eqn 'z _)
      (if (esc (thunk* fin))
        (tree-sub* '([w . 0] [x . 0] [y . 0] [z . 0]))
        _)))

(define-flow digits
  (~> number->string string->list sep (amp (~> string string->number))))

(define (is-valid i ins)
  (define (is-valid* digits v-or-p)
    (cond
      [(procedure? v-or-p) (is-valid* (cdr digits) (v-or-p (car digits)))]
      ;; no more inputs
      [else (valid-state? v-or-p)]))
  (is-valid* (~> (i) digits collect) (interpret v0 ins)))

;; also too slow: the space is too big
(define (find-model ins)
  (for/first ([i (in-range (~> (10) (expt 14) sub1) (expt 10 13) -1)]
              #:unless (~> (i) digits (any zero?))
              #:when (is-valid i ins))
    i))

;; based on Reddit thread :((((((
;; https://topaz.github.io/paste/#XQAAAQBpBQAAAAAAAAA0m0pnuFI8c/fBNAn6x25rti77on4e8DYCelyI4Xj/SWO86l3MhYKSt/IwY1X8XDsPi6oUd359E/fP3WUCy+Hd0NBX3ScDH1UMDMoIn89DqtRJAkuU26H+bJQMhuQJZGvHfbRq+cNenkcVuZMyoJg2X38kr/tdzPWRs0R3nEQAYf3r0cXmSlac2aJH0P2sl7z4weDgKeKkKfE5swiQJ2MN12HwuoRR3LBTiJQjtT73JpWF+KtQBulka0/rhUSDOrztKM4biu1JoxqydIgyDfWupEKKtAiW75B1XW73P7TSQe8BI9O2T12ql8E/CBnsomkNwZLvIqQuyxA8lRBFyEb7T2Ofx8p8uaMPHbMv786Ho5P2KtCBwYdoX3z3fIV+cETYydTzjakKrUdUMq7dRV/kbM91elWwnwaxWByBhJS7jtOshbq8mO2W3BfCQ48WxEmwVrceIMSV2txALQlDxsy1lduebYKTzWCNl58cRdbnvICOxfoV9eofWAd6dE/BpGhxqy4snlEFMPa26JyVjXfXpD+Sh4Vchgcj3Kov+/Dy78f33t8khcwkJaKtJgg7+E+i+YRS7ql81NzjItIrTm1KnqMrxqq0Hl+1zxqTbrn9BDkVsNNu9Mbab72K6iYsNta7J52Io0LObTiUJ289oAzc4RGO/ZCtY4a9Pv0VI4kTpEYmTEUYac9twmKZkClSuD/U8SNOLFtHUI9giLRa+6nUHUGr7LCCvmI3lP10Nh0MlDB3Olffd11IPud3A9Wh9M6eEaXcq0bzn7BUOrQ8ff+FFf33ixp2
(define (solve ins)
  (define insv (list->vector ins))
  (define group-size 18)
  (define consts
    (for/list ([i (in-range 14)])
      (~> (i)
          (* group-size)
          (-< (+ 4) (+ 5) (+ 15))
          (amp (~> (vector-ref insv _) caddr))
          collect)))
  (define (build-deps i [zl (list 0)] [levels (hash)])
    (match-define (list A B C) (list-ref consts i))
    (define sols
      (for*/fold ([sols (hash)])
        ([w (in-range 9 0 -1)]
         [z (in-list zl)]
         [a (in-range A)])
        (define pz (+ a (* A z)))
        (define sols*
          (cond
            [(and (~> (pz) (modulo 26) (+ B) (= w))
                  (~> (pz) (quotient A) (= z)))
             (hash-update sols pz (flow (~>> (cons (list w z)))) null)]
            [else sols]))
        (define pz* (~> ((- z w C)) (/ 26) (* A) (+ a) round))
        (cond
          [(and (~> (pz*) (modulo 26) (+ B) (not (= w)))
                (~> (pz*) (quotient A) (* 26) (+ w C) (= z)))
           (hash-update sols pz* (flow (~>> (cons (list w z)))) null)]
          [else sols*])))
    (when (hash-empty? sols)
      (error "sols empty" i sols))
    (if (> i 0)
      (build-deps (sub1 i) (hash-keys sols) (hash-set levels i sols))
      (hash-set levels i sols)))
  (define levels (build-deps 13))
  (define (inner-solve i z sol order)
    (cond
      [(= i 14) (~> (sol) sep (amp ~a) string-append string->number)]
      [(not (hash-has-key? (hash-ref levels i) z)) #f]
      [else
        (define wnzs (~> (levels) (hash-ref i) (hash-ref z)
                         (sort order #:key car)))
        (let loop ([wnzs wnzs])
          (match wnzs
            ['() #f]
            [(cons (list w nz) wnzs)
             (define s? (inner-solve (add1 i) nz (append sol (list w)) order))
             (if s?
               s?
               (loop wnzs))]))]))
  (values (inner-solve 0 0 null >)
          (inner-solve 0 0 null <)))

(define-flow part1+2 (~> file->ins collect solve))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (~> (input) part1+2 collect)))))
