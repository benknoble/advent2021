#lang racket

(require qi
         rebellion/binary/bit
         )

(define-flow char->num
  (~> string
      open-input-string
      read))

(define-flow lines->columns
  (~>> sep
       (amp (~>> string->list (map char->num)))
       collect
       (apply map list)))

(define-flow most-common-bit
  (~> (-< (~> sep +) (~> length (/ 2)))
      >))

(define-flow column->bit
  (~> most-common-bit boolean->bit))

(define-flow invert-bit-list
  (~>> sep (amp (~>> (- 1))) collect))

(define-flow columns->gamma+epsilon
  (~> sep
      (amp column->bit)
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

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))))
