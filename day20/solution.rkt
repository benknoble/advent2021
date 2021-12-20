#lang racket

(require qi)

(define-flow light? (equal? #\#))
(define-flow dark? (equal? #\.))

(define-flow (make-decoder s)
  (~>> string->list
       (-< (~> length range) _)
       (map cons)
       make-immutable-hash))

(define (make-image . rows)
  (for*/hash ([(row y) (in-indexed rows)]
              [(col x) (in-indexed (in-string row))])
    (values (cons x y) col)))

(define-flow string->decoder+image
  (~> (string-split "\n\n") sep
      (== make-decoder
          (~> (string-split "\n") sep make-image))))
(define-flow file->decoder+image
  (~> file->string string->decoder+image))

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

(define-flow extended-range
  (~> (-< (~> min (- 2))
          (~> max (+ 2)))
      in-inclusive-range))

(define (image-map-xs+ys f)
  (flow (~> hash-keys sep
            (-< (~> (amp car) f)
                (~> (amp cdr) f)))))

(define image->xs+ys (image-map-xs+ys (flow (~> (-< min max) in-inclusive-range))))
(define image->ext-xs+ys (image-map-xs+ys extended-range))

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
       (hash-ref decoder)))

(define (pad-image image bg)
  (define-values (xs ys) (image->ext-xs+ys image))
  (for*/hash ([x (in-stream xs)]
              [y (in-stream ys)])
    (~> (x y) cons
        (-< _ (hash-ref image _ bg)))))

(define (enhance-image image decoder bg)
  (for/hash ([p (in-hash-keys (pad-image image bg))])
    (~> (p) (-< _ (enhance-p image decoder bg)))))

(define-flow (compute-bg image decoder)
  (~>> 1> (image-map-xs+ys min) cons (hash-ref image)))

(define (enhance-image-n n image decoder)
  (define image* (enhance-image image decoder #\.))
  (for/fold ([image image*])
    ([_ (in-range (sub1 n))])
    (enhance-image image decoder (compute-bg image decoder))))

(define-flow count-light
  (~>> hash-values (count light?)))

(define (count-light-n n)
  (flow (~>> X (enhance-image-n n) (ε display-image count-light))))

(define part1* (count-light-n 2))
(define-flow part1 (~> file->decoder+image part1*))
(define part2* (count-light-n 50))
(define-flow part2 (~> file->decoder+image part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
