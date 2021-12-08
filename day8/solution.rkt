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

(define (part1* notes)
  (define unique-digits (list 1 4 7 8))
  (define unique-lengths
    (~> (unique-digits)
        sep
        (amp (~>> (hash-ref digit->wires) set-count))
        set))
  (~> (notes)
      sep
      (amp (~> note-outputs sep))
      (amp set-count)
      (pass (~>> (set-member? unique-lengths)))
      (amp 1)
      +))
(define-flow part1 (~> file->notes part1*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))))
