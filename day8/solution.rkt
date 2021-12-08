#lang racket

(require qi)

(define wires->digit
  (hash (set 'a 'b 'c 'e 'f 'g) 0
        (set 'c 'f) 1
        (set 'a 'c 'd 'e 'g) 2
        (set 'a 'c 'd 'f 'g) 3
        (set 'b 'c 'd 'f) 4
        (set 'a 'b 'd 'f 'g) 5
        (set 'a 'b 'd 'e 'f 'g) 6
        (set 'a 'c 'f) 7
        (set 'a 'b 'c 'd 'e 'f 'g) 8
        (set 'a 'b 'c 'd 'f 'g) 9))

(define digit->wires
  (for/hash ([(wires digit) (in-hash wires->digit)])
    (values digit wires)))

(struct note [pats outputs] #:transparent)

(define (line->note line)
  (define-flow char->symbol (~> string string->symbol))
  (define-flow string->symbols (~>> string->list sep (amp char->symbol) set))
  (~> (line)
       (string-split " | ")
       sep
       (amp (~>> string-split (map string->symbols)))
       note))

(define-flow lines->notes (~>> (map line->note)))
(define-flow file->notes (~> file->lines lines->notes))

(define (find-pats-matching-count pats count)
  (filter (flow (~> set-count (= count)))
          pats))

(define (find-pats-matching-digit pats digit)
  (~>> (digit)
       (hash-ref digit->wires)
       set-count
       (find-pats-matching-count pats)))

(define (find-pats-matching-digits pats digits)
  (append-map
    (flow (find-pats-matching-digit pats _))
    digits))

(define (part1* notes)
  (~> (notes)
      sep
      (amp (~> note-outputs sep))
      collect
      (find-pats-matching-digits '(1 4 7 8))
      sep
      (amp 1)
      +))
(define-flow part1 (~> file->notes part1*))

(define solve-note
  (match-lambda
    [(note pats outputs)

     (hash 'a 'c
           'b 'f)
     ]))

(define-flow digits->number
  (~> sep (amp ~a) string-append string->number))

(define-flow (decode-note note)
  (~> (-< (~> solve-note
              (esc (λ (wire->wire)
                     (flow
                       (~> (set-map (flow (~>> (hash-ref wire->wire))))
                           sep set
                           (hash-ref wires->digit _))))))
          note-outputs)
      map
      digits->number))

(define-flow part2*
  (~> sep (amp decode-note) +))
(define-flow part2 (~> file->notes part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))))
