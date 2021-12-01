#lang racket

(provide (all-defined-out))

(define ((partn n) scans)
  (count < (drop-right scans n) (drop scans n)))

(define part1* (partn 1))
(define part1 (compose1 part1* file->list))
(define part2* (partn 3))
(define part2 (compose1 part2* file->list))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
