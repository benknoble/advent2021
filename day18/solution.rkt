#lang racket

(provide (all-defined-out))

(require qi)

;; Instead of keeping the tree structure, which makes splitting and exploding a
;; little hard, lets represent a number as a list of elts, each of which tracks
;; the number (n) and the depth in the tree.

(struct elt [n depth] #:transparent)

(define (flatten-num n [depth 0])
  (cond
    [(number? n) (list (elt n depth))]
    [(pair? n)
     (append (flatten-num (car n) (add1 depth))
             (flatten-num (cdr n) (add1 depth)))]))

;; Restores the tree structure by collapsing pairs leftwards when possible. We
;; keep the stack elements wrapped in elt to track depth, since that's our
;; collapsing criterion. But we pull out the tree at the end.
(define (unflatten-elts n)
  (let loop ([stack null]
             [n n])
    (match* (n stack)
      [('() `(,s)) (elt-n s)]
      [((cons x rest) _) (loop (collapse-stack (cons x stack))
                               rest)])))

;; Here's the brunt of the leftwards collapsing. Notice that the depth
;; decreases when a pair is collapsed.
(define (collapse-stack s)
  (match s
    [(list* (elt y d) (elt x d) rest)
     (collapse-stack
       (cons (elt (cons x y) (sub1 d))
             rest))]
    [_ s]))

(define-flow string->num
  (~> (string-replace "," " . ")
      open-input-string
      read
      flatten-num))

(define-flow file->nums
  (~> file->lines sep
      (amp string->num)))

;; Thank you to https://github.com/Bogdanp/aoc2021/blob/master/day18.rkt
(define (write-num n)
  (cond
    [(number? n) (write n)]
    [else
      (display #\[) (write-num (car n)) (display #\,) (write-num (cdr n)) (display #\])]))

(define inc-depth
  (match-lambda
    [(elt n depth) (elt n (add1 depth))]))

;; If we used trees, add would be `cons`. But then `try-explode` and `try-split`
;; would require walking binary trees with updates.
(define-flow add
  (~>> append (map inc-depth)))

;; Each part of the reduction returns two values: succeeded? and n*.

(define (try-explode n)
  (let loop ([left null]
             [n n])
    (match n
      ['() (values #f (reverse left))]
      [(list* (elt x 5) (elt y 5) rest)
       (define left*
         (if (empty? left)
           left
           (cons (match (car left)
                   [(elt n depth) (elt (+ n x) depth)])
                 (cdr left))))
       (define rest*
         (if (empty? rest)
           rest
           (cons (match (car rest)
                   [(elt n depth) (elt (+ n y) depth)])
                 (cdr rest))))
       (~>> ((reverse left*) (list (elt 0 4)) rest*)
            append
            (-< #t _))]
      [(cons x rest) (loop (cons x left) rest)])))

(define (try-split n)
  (let loop ([left null]
             [n n])
    (match n
      ['() (values #f (reverse left))]
      [(cons (elt (? (flow (>= 10)) x) depth) rest)
       (define depth* (add1 depth))
       (define replacement
         (~> (x)
             (/ 2)
             (-< floor ceiling)
             (amp (elt depth*))
             collect))
       (~>> ((reverse left) replacement rest)
            append
            (-< #t _))]
      [(cons x rest) (loop (cons x left) rest)])))

;; Try the explode first; if it failed, try the split.
(define-flow reduce-once
  (~> try-explode
      (if 1>
        _
        (~> 2> try-split))))

;; Reduce until both explode and split are out of steam
(define-flow reduce
  (~> reduce-once (switch (% 1> 2>)
                    [_ reduce]
                    [else _])))

(define-flow add-reduce (~> add reduce))

(define (add-reduce-many x . xs)
  (foldl (flow (~> X add-reduce)) x xs))

(define magnitude
  (match-lambda
    [(? number? n) n]
    [(cons x y)
     (~> (x y)
         (amp magnitude)
         (== (* 3) (* 2))
         +)]))

(define-flow part1*
  (~> add-reduce-many unflatten-elts magnitude))
(define-flow part1 (~> file->nums part1*))

(define-flow part2*
  (~> collect
      (fanout 2)
      cartesian-product
      sep
      (pass (~> sep (not eq?)))
      (amp (~> sep add-reduce unflatten-elts magnitude))
      max))
(define-flow part2 (~> file->nums part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
