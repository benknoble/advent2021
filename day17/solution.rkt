#lang racket

(provide (all-defined-out))

(require qi)

;; all variables are integers
;; assume t ≥ 0

;; assume x'0 ≥ 0
;; x(x'0, t) = sum_{0 ≤ i ≤ min{x'0, t-1}} (x'0 - i)
;;           = (1 + min{x'0, t-1}) * x'0 - sum_{…} i
;;           = (1 + min{x'0, t-1}) * x'0 - (1 + min{x'0, t-1}) * (min{x'0, t-1}) / 2

(define-flow (x x*0 t)
  (~> (== _ sub1) min
      (-< (~> add1 (* x*0))
          (~> (-< _ add1) * (/ 2)))
      -))

;; want: given X, find {(x'0, t) | x(x'0, t) = X}
;; X = x(x'0, t)
;;   = (1 + min{x'0, t}) * x'0 - (1 + min{x'0, t}) * (min{x'0, t}) / 2
;;
;; x(x'0, t) is maximized whenever t ≥ x'0
;; x(x'0, t) | t ≥ x'0 = (1 + x'0) * x'0 - (1 + x'0) * x'0 / 2
;;                     = (1 + x'0) * x'0 / 2
;; (/) is integer-division, sometimes called div, divide, or quotient
;; but because this is Gauss's formula, we can use regular (/).
;;
;; So, with t ≥ x'0, the maximum of x is (x'0^2 + x'0)/2
;; Then the minimum x'0 value we can use is the solution to
;; 2X = x'0^2 + x'0
;; 2X + 1/4 = x'0^2 + x'0 + 1/4
;; 2X + 1/4 = (x'0 + 1/2)^2
;; (taking positive root because x'0 ≥ 0)
;; ⌈sqrt{2X + 1/4} - 1/2⌉ = x'0
;;
;; NB This may still not fall exactly on X (check x(x'0, x'0)).
;; NB There may be other solutions (x'0*, t*) with x'0* > x'0, 1 ≤ t* ≤ t.
;;    I think I can increase x'0* and decrease the bound on t* in parallel?
;;
;; We will tag the solution as being exact (meaning only t works for a
;; particular x'0) or open (meaning any t* ≥ t works for a particular x'0). The
;; open case corresponds exactly to when x'0 is t-max.

(struct exact [x*0 t] #:transparent)
(struct open [x*0 t] #:transparent)

(define (x-soln X)
  (define min-x*0
    (~> (X) (* 2) (+ 1/4) sqrt (- 1/2) exact-ceiling))
  (for/fold ([solns (set (exact X 1))])
    ;; hack: expand t-max range to make sure we consider enough values for each
    ;; x*0 what we really want is a nested loop, and I'm too lazy to refactor
    ;; this one
    ([t-max (in-range (* 4 min-x*0) 0 -1)]
     [x*0 (in-naturals min-x*0)])
    (set-union solns
               (for/set ([t (in-range (add1 t-max))]
                         #:when (~> (x*0 t) x (= X)))
                 ((if (= t x*0) open exact)
                  x*0 t)))))

;; y(y'0, t) = y'0 * t - 1/2 * g * t^2
;; (g = 1)
;; y(y'0, t) = y'0 * t - 1/2 * t^2
;;
;; but for the integers this underestimates the trajectory?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define-flow (y y*0 t)
;;   (~> (-< * (~> 2> sqr (* 1/2)))
;;       -))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With some thought:
;;
;; y(y'0, t) = sum_{0 ≤ i ≤ t} (y'0 - i)
;;           = (1 + t) * y'0 - sum_{…} i
;;           = (1 + t) * y'0 - (1 + t) * t / 2
(define-flow (y y*0 t)
  (~> (-< (~> (== add1 _) *)
          (~> 2> (-< add1 _) * (/ 2)))
      -))

;; want: given Y, find {(y'0, t) | y(y'0, t) = Y}
;; Y = y(y'0, t)
;;   = (1 + t) * y'0 - (1 + t) * t / 2
;;   = (1 + t) * (y'0 - t / 2)
;;
;; We know y(y'0, 2 * y'0 + 1) = 0
;; We know y(y'0, t) is maximized at t=y'0
;; We know after t = 2 * y'0 + 1, y is strictly decreasing

;; Original idea:
;; Let L = U_{(X,Y) in target} {(x'0,y'0,t) | (x'0,t) ∈ (x-soln X), (y'0,t) ∈ (y-soln Y)};
;; Find max_{(x'0,y'0,t) ∈ L} (y y'0 t).

;; New idea:
;; Let L = U_{(X,Y) in target} {(x'0,y'0,t) | xs ∈ (x-soln X), (y'0, t) ∈ (y-soln Y xs)};
;; Find max_{(x'0,y'0,t) ∈ L} (y-max y'0).

;; want: given (Y, T), find {y'0 | y(y'0, T) = Y}
;; Y = (1 + T) * y'0 - (1 + T) * T / 2
;; Y + (1 + T) * T / 2 = (1 + T) * y'0
;; (Y + (1 + T) * T / 2) / (1 + T) = y'0
;; Y / (1 + T) + T / 2 = y'0
;;
;; we use exact-floor to account for rounding
;; still need to double-check ourselves
;; this may fail on positive inputs, I'm not sure.
(define-flow (y-soln-t Y T)
  (~> (-< (~> (== _ add1) / exact-floor)
          (~> 2> (/ 2) exact-floor))
      +
      ;; hack, try lots of values around the computed one
      (if zero?
        (-< (- 2) sub1 _ add1 (+ 2))
        (~> (* 2) (-< - _) (if > X _) (== _ add1) range sep))
      (amp
        (if (~> (y T) (= Y))
          set
          (gen (set))))
      set-union))

;; compute {t | t ≥ T, y(y'0, t) = Y}
(define (y-soln-y-t Y T y*0)
  (let loop ([s (set)]
             [t T])
    (define Y*? (y y*0 t))
    (cond
      [(< Y*? Y) s]
      [(= Y*? Y) (loop (set-add s y*0)
                       (add1 t))]
      [else (loop s (add1 t))])))

;; compute {(y'0,t) | y(y'0, t) = Y, valid(t, xs)}
;; valid(t, exact(_, t)).
;; valid(t, open(_, T)) :- t ≥ T.
(define (y-soln Y xs)
  (match xs
    [(exact x*0 1) (set Y)]
    [(exact _ t) (y-soln-t Y t)]
    [(open _ t)
     ;; Notice that if y'0 is ever greater than -(1 + Y) (assuming Y ≤ 0), we
     ;; can _never_ hit the Y. Why? Because the y-velocity at x=0, t>0 is
     ;; exactly -(1 + y'0). The projectile would jump right over the Y.
     ;;
     ;; This is not to say that _every_ y'0 ≤ -(1 + Y) will work, but it does
     ;; give us an upper bound for y'0.
     ;;
     ;; The lower bound is then 1 + Y ≤ y'0, for the same reason (except with
     ;; x=0,t=0).
     (for/fold ([s (set)])
       ([y*0 (in-range (add1 Y) (add1 (- (add1 Y))))])
       (set-union s (y-soln-y-t Y t y*0)))]))

(define-flow (y-max y*0)
  (~> (fanout 2) y))

(define (solve xm xM ym yM)
  (for*/set ([X (in-range xm (add1 xM))]
             [xs (in-set (x-soln X))]
             [Y (in-range ym (add1 yM))]
             [y (in-set (y-soln Y xs))])
    (list (match xs
            [(exact x*0 _) x*0]
            [(open x*0 _) x*0])
          y)))

(define-flow string->xm-xM-ym-yM
  (~> (string-split "=") sep
      (select 2 3)
      (amp (~> (string-split "..") sep))
      (amp (~> open-input-string read))))
(define-flow file->xm-xM-ym-yM
  (~> file->string string->xm-xM-ym-yM))

(define-flow part1*
  (~> solve
      (set-map (flow (~> sep 2> y-max))) sep
      max))
(define-flow part1 (~> file->xm-xM-ym-yM part1*))

(define-flow part2* (~> solve set-count))
(define-flow part2 (~> file->xm-xM-ym-yM part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
