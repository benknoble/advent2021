#lang racket

(require qi
         ;; needs a 64-bit platform or part2 will overflow
         (only-in racket/fixnum make-fxvector in-fxvector)
         racket/unsafe/ops)

(struct p [pos score] #:transparent)

(define-flow port->ps
  (~>> (port->list read) sep
       (select 5 10)
       (amp (~> sub1 (p 0)))))
(define-flow file->ps
  (call-with-input-file port->ps))

(define board-size 10)


(define (play1 p1 p2)
  (define deterministic-dice
    (let ([d 1])
      (thunk
        (cond
          ;; sum of three consecutive numbers a,a+1,a+2 is 3a+3
          [(<= d 98) (begin0
                       (~> (d) (* 3) (+ 3))
                       (set! d (modulo (+ 3 d) 100)))]
          [(= d 99) (begin0
                      (+ 99 100 1)
                      (set! d 2))]
          [(= d 100) (begin0
                       (+ 100 1 2)
                       (set! d 3))]))))
  (for/fold ([p1 p1]
             [p2 p2]
             [rolls 0]
             #:result (values p1 p2 (* rolls 3)))
    ([_ (in-naturals)]
     #:break (~> (p1 p2) (any (~> p-score (>= 1000)))))
    (define roll (deterministic-dice))
    #;(displayln (list roll p1 p2))
    (define pos (~> (p1) p-pos (+ roll) (modulo board-size)))
    (define score (~> (p1) p-score (+ 1 pos)))
    (values p2 (p pos score) (add1 rolls))))

;; essential ideas from
;; https://github.com/massung/advent2021/blob/main/day21/day21.lisp

(define rolls #(0 0 0 1 3 6 7 6 3 1))
(module+ test
  (require rackunit)
  (check-equal? 27 (~> (rolls) vector->values +)))

(define (unsafe-fxvector-update! v p f)
  (unsafe-fxvector-set! v p (f (unsafe-fxvector-ref v p))))

(define (play2 p1 p2)
  (define p1-wins (make-fxvector 11))
  (define p2-wins (make-fxvector 11))

  (define (play-p1 p1-pos p1-score p2-pos p2-score turn combs)
    (for ([roll (in-vector rolls)]
          [i (in-naturals)]
          #:when (> roll 0))
      (define pos (~> (p1-pos) (unsafe-fx+ i) (unsafe-fxmodulo board-size)))
      (define score (~> (p1-score) (unsafe-fx+ 1 pos)))
      (define combs* (unsafe-fx* combs roll))
      (if (unsafe-fx>= score 21)
        (unsafe-fxvector-update! p1-wins turn (flow (unsafe-fx+ combs*)))
        (play-p2 pos score p2-pos p2-score turn combs*))))

  (define (play-p2 p1-pos p1-score p2-pos p2-score turn combs)
    (for ([roll (in-vector rolls)]
          [i (in-naturals)]
          #:when (> roll 0))
      (define pos (~> (p2-pos) (unsafe-fx+ i) (unsafe-fxmodulo board-size)))
      (define score (~> (p2-score) (unsafe-fx+ 1 pos)))
      (define combs* (unsafe-fx* combs roll))
      (if (unsafe-fx>= score 21)
        (unsafe-fxvector-update! p2-wins turn (flow (unsafe-fx+ combs*)))
        (play-p1 p1-pos p1-score pos score (unsafe-fx+ 1 turn) combs*))))

  (~> (p1 p2)
      (amp (-< p-pos p-score))
      (play-p1 1 1))
  (values p1-wins p2-wins))

(define-flow part1*
  (~> play1
      (-< (~> (block 3) (amp p-score) min)
          3>)
      *))
(define-flow part1
  (~> file->ps part1*))

(define-flow part2*
  (~> play2
      (amp (esc (λ (v)
                  (for/sum ([x (in-fxvector v)])
                    x))))
      max))
(define-flow part2
  (~> file->ps part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
