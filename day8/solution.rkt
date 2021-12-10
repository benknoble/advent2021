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

(define-flow char->symbol (~> string string->symbol))
(define-flow string->symbols (~>> string->list sep (amp char->symbol) set))
(define-flow (line->note line)
  (~> (string-split " | ")
      sep
      (amp (~>> string-split (map string->symbols)))
      note))

(define-flow lines->notes (~>> (map line->note)))
(define-flow file->notes (~> file->lines lines->notes))

(define-flow (find-pats-matching-count count pats)
  (~> (== (clos (~> (== _ set-count) =)) sep)
      pass
      collect))

(define-flow (find-pats-matching-digit digit pats)
  (~>> (== (~>> (hash-ref digit->wires) set-count) _)
       find-pats-matching-count))

(define (find-pats-matching-digits pats digits)
  (append-map
    (flow (find-pats-matching-digit _ pats))
    digits))

(define (part1* notes)
  (~> (notes)
      sep
      (amp (~> note-outputs sep))
      collect
      (find-pats-matching-digits '(1 4 7 8))
      sep
      count))
(define-flow part1 (~> file->notes part1*))

(define solve-note
  (match-lambda
    [(note pats _)
     ;;  a
     ;; b c
     ;;  d
     ;; e f
     ;;  g
     ;; Algorithm:
     ;; 0. Identify patterns for 1, 4, 7, 8 (I will pun the digits to the sets
     ;;    representing the corresponding pattern).
     ;; 1. 7-1 tells us what maps to a (call it A).
     ;; 2. Collect groups 2,3,5 and 0,6,9 (same numbers of segments)
     ;; 3. Separate 6 from 0,6,9. 6 is the only member X of the group such
     ;;    that X∩1 ≠ 1, _i.e._, such that ¬(1⊆X).
     ;; 4. 1-6 tells us what maps to c.
     ;; 5. Since we know what maps to c, and 1 is {C,F}, we can find what maps
     ;;    to f by 1-{C}.
     ;; 6. Separate 0 from 0 by 9∩4 = 4 (4⊆9), 0∩4 ≠ 4 (¬(4⊆0)).
     ;; 7. 8-0 tells us what maps to d.
     ;; 8. Separate 2,3,5 by 3∩1=1 (1⊆3), [C∈2, ¬(F∈2)], [¬(C∈5), F∈5]
     ;; 9. 6-5 tells us what maps to e.
     ;; 10. 5-3 tells us what maps to b.
     ;; 11. 8-{ABCDEF} tells us what maps to g.

     ;; 0
     (match-define `(,one) (find-pats-matching-digit 1 pats))
     (match-define `(,four) (find-pats-matching-digit 4 pats))
     (match-define `(,seven) (find-pats-matching-digit 7 pats))
     (match-define `(,eight) (find-pats-matching-digit 8 pats))
     ;; 1
     (define A (set-first (set-subtract seven one)))
     ;; 2
     (define two-three-five
       (remove-duplicates (find-pats-matching-digits pats '(2 3 5))))
     (define zero-six-nine
       (remove-duplicates (find-pats-matching-digits pats '(0 6 9))))
     ;; 3
     (match-define-values (`(,six) zero-nine)
       (partition (flow (~>> (subset? one) not)) zero-six-nine))
     ;; 4
     (define C (set-first (set-subtract one six)))
     ;; 5
     (define F (set-first (set-remove one C)))
     ;; 6
     (match-define-values (`(,nine) `(,zero))
       (partition (flow (subset? four _)) zero-nine))
     ;; 7
     (define D (set-first (set-subtract eight zero)))
     ;; 8
     (match-define-values (`(,three) two-five)
       (partition (flow (subset? one _)) two-three-five))
     (match-define-values (`(,two) `(,five))
       (partition (flow (set-member? _ C)) two-five))
     ;; 9
     (define E (set-first (set-subtract six five)))
     ;; 10
     (define B (set-first (set-subtract five three)))
     ;; 11
     (define G (set-first (set-subtract eight (set A B C D E F))))

     (hash A 'a
           B 'b
           C 'c
           D 'd
           E 'e
           F 'f
           G 'g)]))

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
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
