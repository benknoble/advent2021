#lang racket

(require qi)

(define-flow light? (equal? #\#))
(define-flow dark? (equal? #\.))

(define (image-map-xs+ys f)
  (flow (~> hash-keys sep
            (-< (~> (amp car) f)
                (~> (amp cdr) f)))))

(define image->xs+ys (image-map-xs+ys (flow (~> (-< min max) in-inclusive-range))))
(define image->ext-xs+ys (image-map-xs+ys (flow (-< min max))))

(define (make-image . rows)
  (define image
    (for*/hash ([(row y) (in-indexed rows)]
                [(col x) (in-indexed (in-string row))])
      (values (cons x y) col)))
  (~> (image) (-< _ image->ext-xs+ys)))

(define-flow string->image+decoder
  (~> (string-split "\n\n") sep
      (== _
          (~> (string-split "\n") sep make-image))))
(define-flow file->image+decoder
  (~> file->string string->image+decoder))

(define-flow bits->integer
  (~> (amp ~a)
      string-append
      (string->number 2)))

(define-flow px->bit
  (switch
    [light? 1]
    [dark? 0]))

(define-flow pxs->bits (amp px->bit))
(define-flow pxs->int (~> pxs->bits bits->integer))

(define (display-image image)
  (define-values (xs ys) (image->xs+ys image))
  (for ([y ys])
    (for ([x xs])
      (display (hash-ref image (cons x y))))
    (newline)))

(define (neighbors p)
  (for*/list ([dy (in-range -1 2)]
              [dx (in-range -1 2)])
    (~> (p) (-< car cdr) (== (+ dx) (+ dy)) cons)))

(define (enhance-p p image decoder bg)
  (~>> (p)
       neighbors sep
       (amp (hash-ref image _ bg))
       pxs->int
       (string-ref decoder)))

(define (pad-image image xm xM ym yM bg)
  (define (update-with-pad image x y)
    (~> (x y) cons
        (hash-set image _ bg)))
  (define rectangles
    (list (list (inclusive-range (- xm 2) (+ xM 2))
                (inclusive-range (- ym 2) ym))
          (list (inclusive-range (- xm 2) (+ xM 2))
                (inclusive-range yM (+ yM 2)))
          (list (inclusive-range (- xm 2) xm)
                (inclusive-range (- ym 2) (+ yM 2)))
          (list (inclusive-range xM (+ xM 2))
                (inclusive-range (- ym 2) (+ yM 2)))))

  (for*/fold ([image image])
    ([rect (in-list rectangles)]
     [x (in-list (car rect))]
     [y (in-list (cadr rect))])
    (update-with-pad image x y)))

(define (enhance-image image xm xM ym yM decoder bg)
  (for/hash ([p (in-hash-keys (pad-image image xm xM ym yM bg))])
    (~> (p) (-< _ (enhance-p image decoder bg)))))

(define-flow (compute-bg image decoder)
  (~>> 1> (image-map-xs+ys min) cons (hash-ref image)))

(define (enhance-image-n n decoder image xm xM ym yM)
  (define image* (enhance-image image xm xM ym yM decoder #\.))
  (for/fold ([image image*]
             [xm (- xm 2)]
             [xM (+ xM 2)]
             [ym (- ym 2)]
             [yM (+ yM 2)])
    ([_ (in-range (sub1 n))])
    (values (enhance-image image xm xM ym yM decoder (compute-bg image decoder))
            (- xm 2) (+ xM 2)
            (- ym 2) (+ yM 2))))

(define-flow count-light
  (~>> hash-values (count light?)))

(define (count-light-n n)
  (flow (~>> (enhance-image-n n) 1> (ε display-image count-light))))

(define part1* (count-light-n 2))
(define-flow part1 (~> file->image+decoder part1*))
(define part2* (count-light-n 50))
(define-flow part2 (~> file->image+decoder part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
