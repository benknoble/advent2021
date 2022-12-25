#lang racket

(provide (all-defined-out))

(require qi
         rebellion/binary/bit)

(define-flow char->num
  (~> string
      open-input-string
      read))

(define-flow lines->rows
  (~> sep
      (amp (~>> string->list (map char->num)))
      collect))

(define-flow rows->columns
  (~>> (apply map list)))

(define-flow lines->columns
  (~>> lines->rows rows->columns))

(define-flow most-common-bit
  (~> (-< (~> sep +) (~> length (/ 2)))
      >=
      boolean->bit))

(define-flow least-common-bit
  (~>> most-common-bit (- 1)))

(define-flow invert-bit-list
  (~>> sep (amp (~>> (- 1))) collect))

(define-flow columns->gamma+epsilon
  (~> sep
      (amp most-common-bit)
      collect
      (-< _ invert-bit-list)))

;; string->number is faster?!
(define (bit-list->integer1 bs)
  (for/fold ([num 0])
    ([(b i) (in-indexed (in-list (reverse bs)))])
    (+ num (* b (expt 2 i)))))

(define-flow bit-list->integer2
  (~> sep
      (amp ~a)
      string-append
      (string->number 2)))

(define-flow part1*
  (~> lines->columns
      columns->gamma+epsilon
      (amp bit-list->integer2)
      *))

(define-flow part1
  (~> file->lines part1*))

(define-flow (fieldth field) (clos (~> X list-ref)))
(define (criterion-solver selector rows)
  (define (filterc field criterion rows)
    (define-flow matches (~> (esc (fieldth field)) (= criterion)))
    (filter matches rows))
  ;; we cannot name this loop, because qi's flow has a loop syntactic form that
  ;; confuses our intent.
  (let solver-loop ([rows rows]
                    [field 0])
    (~>> (rows)
         (-< (~>> (map (fieldth field)) selector) _)
         (filterc field)
         (if (~> length (= 1))
           (~> first bit-list->integer2)
           (~> (solver-loop (add1 field)))))))

(define-flow part2*
  (~> lines->rows
      (-< (~>> (criterion-solver most-common-bit))
          (~>> (criterion-solver least-common-bit)))
      *))

(define-flow part2
  (~> file->lines part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
