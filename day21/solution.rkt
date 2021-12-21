#lang racket

(require qi)

(struct p [pos score] #:transparent)

(define-flow port->ps
  (~>> (port->list read) sep
       (select 5 10)
       (amp (~> sub1 (p 0)))))
(define-flow file->ps
  (call-with-input-file port->ps))

(define board-size 10)

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

(define (play p1 p2 d)
  (for/fold ([p1 p1]
             [p2 p2]
             [rolls 0]
             #:result (values p1 p2 (* rolls 3)))
    ([_ (in-naturals)]
     #:break (~> (p1 p2) (any (~> p-score (>= 1000)))))
    (define roll (d))
    #;(displayln (list roll p1 p2))
    (define pos (~> (p1) p-pos (+ roll) (modulo board-size)))
    (define score (~> (p1) p-score (+ 1 pos)))
    (values p2 (p pos score) (add1 rolls))))

(define-flow part1*
  (~> (play deterministic-dice)
      (-< (~> (block 3) (amp p-score) min)
          3>)
      *))
(define-flow part1
  (~> file->ps part1*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))))
