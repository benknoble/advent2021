#lang racket

(provide (all-defined-out))

(require qi
         rebellion/binary/bitstring
         rebellion/binary/bit)

;; necessary because (~> (string->number 16) (number->string 2)) does not
;; preserve left-padded 0s
(define (decode-hex char)
  (case char
    [(#\0) (values 0 0 0 0)]
    [(#\1) (values 0 0 0 1)]
    [(#\2) (values 0 0 1 0)]
    [(#\3) (values 0 0 1 1)]
    [(#\4) (values 0 1 0 0)]
    [(#\5) (values 0 1 0 1)]
    [(#\6) (values 0 1 1 0)]
    [(#\7) (values 0 1 1 1)]
    [(#\8) (values 1 0 0 0)]
    [(#\9) (values 1 0 0 1)]
    [(#\A) (values 1 0 1 0)]
    [(#\B) (values 1 0 1 1)]
    [(#\C) (values 1 1 0 0)]
    [(#\D) (values 1 1 0 1)]
    [(#\E) (values 1 1 1 0)]
    [(#\F) (values 1 1 1 1)]))

(define-flow string->packet-string
  (~> string->list sep
      (amp decode-hex)
      bitstring))
(define-flow file->packet-string
  (~> file->lines car
      string->packet-string))

;; version: 3
;; type: 3 (literal-value? operator?)
(struct packet [version type data] #:transparent)
;; groups of 5 bits:
;; 1xxxx (read next group, save xxxx)
;; 0xxxx (done, save xxxx)
(struct literal-value [n] #:transparent)
;; length-type: 1 (0/1)
;; length: 0=>15 (length in bits of subpackets) 1=>11 (number of packets)
(struct operator [length-type length data] #:transparent)

(define-flow list->number
  (~> sep ~a (string->number 2)))
(define-flow bitstring->number
  (~> in-bitstring stream->list list->number))

(define (read-n-bits n string [start 0])
  (map (flow (~>> (+ start) (bitstring-ref string))) (range n)))

;; -> packet? new-offset
(define (decode packet-string [start 0])
  (define version (list->number (read-n-bits 3 packet-string start)))
  (define type (list->number (read-n-bits 3 packet-string (+ 3 start))))
  (define-values (data new-offset)
    ((case type
       [(4) decode-literal]
       [else decode-operator])
     packet-string (+ 6 start)))
  (values (packet version type data) new-offset))

(define (decode-literal packet-string [start 0])
  (let loop ([acc null]
             [offset start])
    (define group (read-n-bits 5 packet-string offset))
    (case (car group)
      [(1) (loop (cons (cdr group) acc)
                 (+ offset 5))]
      [(0) (values (~>> (acc)
                        (cons (cdr group))
                        reverse append* list->number
                        literal-value)
                   (+ offset 5))])))

(define (decode-operator packet-string [start 0])
  (define type (bitstring-ref packet-string start))
  (case type
    [(0)
     (define length (list->number (read-n-bits 15 packet-string (add1 start))))
     (let loop ([acc null]
                [offset (+ 15 1 start)]
                [bits-read 0])
       (define-values (p offset*) (decode packet-string offset))
       (define bits-read* (+ bits-read (- offset* offset)))
       (if (= bits-read* length)
         (values (~>> (acc) (cons p) reverse (operator type length))
                 offset*)
         (loop (cons p acc)
               offset*
               bits-read*)))]
    [(1)
     (define length (list->number (read-n-bits 11 packet-string (add1 start))))
     (define-values (data offset)
       (for/fold ([data null]
                  [offset (+ 11 1 start)])
         ([_ (in-range length)])
         (~>> (offset)
              (decode packet-string)
              (== (cons data) _))))
     (~> (data offset)
         (== (~>> reverse (operator type length))
             _))]))

(define sum-version
  (match-lambda
    [(packet version _ (literal-value _)) version]
    [(packet version _ (operator _ _ data))
     (apply + version (map sum-version data))]))

(define part1* sum-version)
(define-flow part1 (~> file->packet-string decode 1> part1*))

(define interpret
  (match-lambda
    [(packet _ _ (literal-value n)) n]
    [(packet _ type (operator _ _ data))
     (apply (case type
              [(0) +]
              [(1) *]
              [(2) min]
              [(3) max]
              [(5) (flow (~> > boolean->bit))]
              [(6) (flow (~> < boolean->bit))]
              [(7) (flow (~> = boolean->bit))])
            (map interpret data))]))

(define part2* interpret)
(define-flow part2 (~> file->packet-string decode 1> part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
