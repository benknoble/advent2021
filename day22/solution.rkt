#lang racket

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

(define (all-ps keep? low hi)
  (match-define `(,(p xl yl zl) ,(p xh yh zh)) (list low hi))
  (for*/set ([x (in-inclusive-range xl xh)]
             #:when (keep? x)
             [y (in-inclusive-range yl yh)]
             #:when (keep? y)
             [z (in-inclusive-range zl zh)]
             #:when (keep? z))
    (p x y z)))

(define ((turn-on keep?) on low hi)
  (set-union on (all-ps keep? low hi)))

(define ((turn-off keep?) on low hi)
  (set-subtract on (all-ps keep? low hi)))

;; keep? -> on on? low high -> on
(define (flip keep?)
  (flow (switch
          (% 2> (block 2))
          [(eq? 'on) (turn-on keep?)]
          [(eq? 'off) (turn-off keep?)])))

;; keep? -> step … -> on
(define (flip* keep?)
  (flow (>> (~> (-< 2> (~> 1> sep)) (flip keep?)) set)))

(define-flow keep-50 (<= -50 _ 50))

(module+ test
  (require rackunit)

  (define example
    (list (list 'on (p 10 10 10) (p 12 12 12))
          (list 'on (p 11 11 11) (p 13 13 13))
          (list 'off (p 9 9 9) (p 11 11 11))
          (list 'on (p 10 10 10) (p 10 10 10))))

  (check-equal? 39 (~> (example) sep (flip* keep-50) set-count)))

(define-flow part1* (~> (flip* keep-50) set-count))
(define-flow part1 (~> file->steps part1*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))))
