#lang racket

(provide (all-defined-out))

(require qi)

(define-flow line->step
  (~> (string-split " ")
      (-< (~> car string->symbol)
          (~> cadr (string-split ",") sep
              (amp (~> (string-split "=") cadr (string-split "..")
                       (map string->number _)))
              (map list __) sep
              (amp (~> sep p))))))
(define-flow lines->steps
  (amp (~> line->step collect)))
(define-flow file->steps
  (~> file->lines sep lines->steps))

(struct p [x y z] #:transparent)

;; with great applause to https://todd.ginsberg.com/post/advent-of-code/2021/day22/
(define (r-intersects low1 hi1 low2 hi2)
  (and (<= low1 hi2) (>= hi1 low2)))

(define (r-intersect low1 hi1 low2 hi2)
  (values (max low1 low2) (min hi1 hi2)))

(define-flow c-intersects
  (~> (amp (-< cadr caddr))
      (and (~> (amp p-x) r-intersects)
           (~> (amp p-y) r-intersects)
           (~> (amp p-z) r-intersects))))

(define-flow c-intersect
  (if c-intersects
    (~> (-< (~> 1> car (switch
                         [(eq? 'on) 'off]
                         [(eq? 'off) 'on]))
            (~> (amp (-< cadr caddr))
                (-< (~> (amp p-x) r-intersect)
                    (~> (amp p-y) r-intersect)
                    (~> (amp p-z) r-intersect))
                (-< (~> (select 1 3 5) p)
                    (~> (select 2 4 6) p))))
        collect)
    #f))

(define-flow volume
  (~> sep (-< (~> 1> (switch
                       [(eq? 'on) 1]
                       [(eq? 'off) -1]))
              (~> (block 1)
                  (-< (~> (amp p-x) X - add1)
                      (~> (amp p-y) X - add1)
                      (~> (amp p-z) X - add1))))
      *))

(define (solve . cubes)
  (for/fold ([volumes null]
             #:result (~> (volumes) sep (amp volume) +))
    ([cube (in-list cubes)])
    (define to-add
      (~> (volumes)
          sep
          (amp (c-intersect cube))
          (pass _)
          collect))
    (if (~> (cube) car (eq? 'on))
      (cons cube (append to-add volumes))
      (append to-add volumes))))

(define cube50
  (list 'on (p -50 -50 -50) (p 50 50 50)))

(define-flow part1* (~> (pass (c-intersects cube50)) solve))
(define-flow part1 (~> file->steps part1*))

(define-flow part2* solve)
(define-flow part2 (~> file->steps part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
