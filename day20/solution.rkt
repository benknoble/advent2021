#lang racket

(require qi)

(define-flow light? (or (eq? 1) (eq? #\#)))
(define-flow dark? (or (eq? 0) (eq? #\.)))

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
      (values (cons x y) (px->bit col))))
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

(define-switch px->bit
  [light? 1]
  [dark? 0])

(define-switch bit->px
  [light? #\#]
  [dark? #\.])

(define (display-image image)
  (define-values (xs ys) (image->xs+ys image))
  (for ([y ys])
    (for ([x xs])
      (display (bit->px (hash-ref image (cons x y)))))
    (newline)))

(define (neighbors p)
  (for*/list ([dy (in-range -1 2)]
              [dx (in-range -1 2)])
    (~> (p) (-< car cdr) (== (+ dx) (+ dy)) cons)))

(define (enhance-p p image decoder bg)
  (~>> (p)
       neighbors sep
       (amp (hash-ref image _ bg))
       bits->integer
       (string-ref decoder)
       px->bit))

(define (pad-image image xm xM ym yM bg)
  (define (update-with-pad image x y)
    (~> (x y) cons
        (hash-set image _ bg)))
  (define rectangles
    (list (list (inclusive-range (sub1 xm) (add1 xM))
                (inclusive-range (sub1 ym) ym))
          (list (inclusive-range (sub1 xm) (add1 xM))
                (inclusive-range yM (add1 yM)))
          (list (inclusive-range (sub1 xm) xm)
                (inclusive-range (sub1 ym) (add1 yM)))
          (list (inclusive-range xM (add1 xM))
                (inclusive-range (sub1 ym) (add1 yM)))))

  (for*/fold ([image image])
    ([rect (in-list rectangles)]
     [x (in-list (car rect))]
     [y (in-list (cadr rect))])
    (update-with-pad image x y)))

(define (enhance-image image xm xM ym yM decoder bg)
  (for/hash ([p (in-hash-keys (pad-image image xm xM ym yM bg))])
    (~> (p) (-< _ (enhance-p image decoder bg)))))

(define (compute-bg image xm ym decoder)
  (hash-ref image (cons xm ym)))

(define (enhance-image-n n decoder image xm xM ym yM)
  (define image* (enhance-image image xm xM ym yM decoder 0))
  (for/fold ([image image*]
             [xm (sub1 xm)]
             [xM (add1 xM)]
             [ym (sub1 ym)]
             [yM (add1 yM)])
    ([_ (in-range (sub1 n))])
    (values (enhance-image image xm xM ym yM decoder (compute-bg image xm ym decoder))
            (sub1 xm) (add1 xM)
            (sub1 ym) (add1 yM))))

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
