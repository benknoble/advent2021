#lang racket

(require qi
         data/queue
         "../common.rkt")

(struct p [x y z] #:transparent)

(define/match (reld p1 p2)
  [((p x1 y1 z1) (p x2 y2 z2))
   (p (- x1 x2) (- y1 y2) (- z1 z2))])

(define/match (addp p1 p2)
  [((p x1 y1 z1) (p x2 y2 z2))
   (p (+ x1 x2) (+ y1 y2) (+ z1 z2))])

(define/match (manhattan p1 p2)
  [((p x1 y1 z1) (p x2 y2 z2))
   (+ (abs (- x1 x2))
      (abs (- y1 y2))
      (abs (- z1 z2)))])

;; we almost want (combinations 2) instead of (fanout 2) cartesian-product, but
;; then the ability to find matching pairs is highly dependnent on ordering
(define-flow (reld* ps)
  (~> (fanout 2) cartesian-product sep
      (pass (~> sep (not eq?)))
      (amp (~> sep (-< _ reld) collect))
      collect))

(struct station [p beacons relds] #:transparent)

(define (fix-p s new-p [rel-to (p 0 0 0)])
  (struct-copy station s [p (addp new-p rel-to)]))

(define fix-beacons
  (match-lambda
    [(station pp bs _)
     (station pp (map (flow (addp pp)) bs) #f)]))

;; gives position for the station seeing qs relative to position seeing ps if
;; one exists, or #f
;; assumes both stations are oriented in the same direction
(define-flow (p-for-s-or-false overlap-needed ps qs)
  (~> (block 1)
    ;; grab all the reld's
    (amp station-relds)
    ;; find matching reld's
    cartesian-product sep
    (pass (~> sep (amp caddr) equal?))
    ;; reorganize so that
    ;; (list (list p1 p2 rd) (list q1 q2 rd))
    ;; becomes
    ;; (list rd (list p1 p2) (list q1 q2))
    (amp (~> (-< (~> car caddr)
                 (~> sep (amp (drop-right 1))))
             collect))
    ;; pair up the extraneous solutions from cartesian product, *i.e.*,
    ;; (list rd (list p1 p2) …) and
    ;; (list -rd (list p2 p1) …)
    collect
    (group-by cadr _ (flow (or equal? (~> (== reverse _) equal?))))
    sep
    ;; drop the extraneous one
    (amp car)
    (if (~> count (>= overlap-needed))
      ;; found a match, compute position of station relative to other station
      (~> 1> (-< caadr caaddr)
          reld)
      ;; no match
      #f)))

(define rotate-beacon
  (match-lambda
    [(p χ η ζ)
     (list (p χ η ζ)
           (p χ (- η) (- ζ))
           (p (- χ) (- η) ζ)
           (p (- χ) η (- ζ))

           (p χ (- ζ) η)
           (p χ ζ (- η))
           (p (- χ) ζ η)
           (p (- χ) (- ζ) (- η))

           (p η (- χ) ζ)
           (p η χ (- ζ))
           (p (- η) χ ζ)
           (p (- η) (- χ) (- ζ))

           (p η ζ χ)
           (p η (- ζ) (- χ))
           (p (- η) (- ζ) χ)
           (p (- η) ζ (- χ))

           (p ζ (- η) χ)
           (p ζ η (- χ))
           (p (- ζ) η χ)
           (p (- ζ) (- η) (- χ))

           (p ζ χ η)
           (p ζ (- χ) (- η))
           (p (- ζ) (- χ) η)
           (p (- ζ) χ (- η)))]))

(struct station-r [rotations] #:transparent)

(define-flow make-station-r
  (~>> (map rotate-beacon)
       (apply map list)
       (map (flow (~> (-< #f _ reld*) station)))
       station-r))

;; summary of structs:
;; - p [x y z] posn, used for both beacon and location
;; - station: [p is location]
;;            [beacons is original (possibly rotated) beacon list]
;;            [relds is cached (reld* beacons)]
;; - station-r: [rotations is list of station]

(define (fix-all-ps overlap-needed s0 . srs)
  (define q (make-queue))
  (for ([(sr i) (in-indexed (in-list srs))])
    (enqueue! q (cons (add1 i) sr)))

  (define fixed-stations
    (let loop ([fixed (hash 0 s0)])
      (cond
        [(queue-empty? q)
         (build-list (add1 (length srs))
                     (flow (hash-ref fixed _)))]
        [else
          (define-values (i sr-to-try-to-fix) (~> (q) dequeue! (-< car cdr)))
          (define found
            (for*/first ([s-to-try (station-r-rotations sr-to-try-to-fix)]
                         [already-fixed (in-hash-values fixed)]
                         [offset (in-value (p-for-s-or-false overlap-needed already-fixed s-to-try))]
                         #:when offset)
              (list s-to-try already-fixed offset)))
          (match found
            [(list s base offset)
             (define fixed-s (fix-p s offset (station-p base)))
             (loop (hash-set fixed i fixed-s))]
            [#f
             (enqueue! q (cons i sr-to-try-to-fix))
             (loop fixed)])])))

  (map fix-beacons fixed-stations))

(define unique-beacons
  (~>> (map station-beacons) sep (amp sep)
       set))

(define-flow max-distance
  (~>> (map station-p)
       (fanout 2)
       cartesian-product sep
       (amp (~> sep manhattan))
       max))

;;;; experiments

;; (begin
;;   (define s0 (station
;;                (p 0 0 0)
;;                (list (p 0 2 0) (p 4 1 0) (p 3 3 0))
;;                (reld* (list (p 0 2 0) (p 4 1 0) (p 3 3 0)))))
;;   (define s1 (station
;;                #f
;;                (list (p -1 -1 0) (p -5 0 0) (p -2 1 0))
;;                (reld* (list (p -1 -1 0) (p -5 0 0) (p -2 1 0)))))
;;   (time (fix-p s1 (p-for-s-or-false 3 s0 s1))))

;; (begin
;;   (define s0bs (list (p 0 2 0) (p 4 1 0) (p 3 3 0)))
;;   (define s1bs (list (p -1 -1 0) (p -5 0 0) (p -2 1 0)))
;;   (define s0 (station (p 0 0 0) s0bs (reld* s0bs)))
;;   (define s1r (make-station-r s1bs))
;;   (time (fix-all-ps 12 s0 s1r))
;;   (time (unique-beacons (fix-all-ps 3 s0 s1r))))

;;;; parsing

(define-flow string->ss
  (~>> (string-split _ #px"\n?--- scanner \\d+ ---\n")
       (map (flow (~>> (string-split _ "\n")
                       (map (flow (~>> open-input-string
                                       (port->list read-ignore-comma)
                                       sep p))))))
       (-< (~>> car (-< _ reld*) (station (p 0 0 0)))
           (~> cdr sep (amp make-station-r)))))

;;;; runners

(define-flow part1+2*
  (~>> (fix-all-ps 12)
       (-< (~> unique-beacons set-count)
           max-distance)))
(define-flow part1+2 (~> file->string string->ss part1+2*))

(module+ main
  (command-line
    #:args (input)
    (~>> (input)
         list
         (time-apply part1+2)
         (-< (~>> (block 1) (format "cpu time: ~a real time: ~a gc time: ~a"))
             (~> 1> sep))
         (amp displayln))))
