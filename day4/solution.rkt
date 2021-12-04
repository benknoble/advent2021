#lang racket

(require qi)

(define board-size 5)

(struct board [board index] #:transparent)

(define (rows->board+index rows)
  (for*/fold ([b (hash)]
              [index (hash)])
    ([(row y) (in-indexed (in-list rows))]
      [(col x) (in-indexed (in-list row))])
    (values (hash-set b (cons x y) col)
            (hash-set index col (cons x y)))))

(define-flow (make-board rows)
  (~> rows->board+index board))

(define-flow (string->moves+boards text)
  (~> (string-split "\n\n")
      (-< (~> first ;; moves
              (string-split ",") (sep string->number) collect)
          (~> rest sep ;; boards
              (amp (~> (string-split "\n") sep
                       (amp (~> (string-split #rx" +") (sep string->number) collect))
                       collect))
              (amp make-board)
              collect))))

(define-flow (winning-rows rows)
  (~> sep (amp (~> sep none?)) any?))

(define-flow (winning-rows-or-columns rows)
  (or winning-rows
      (~>> sep (map list) winning-rows)))

(define (winning b)
  (define b* (board-board b))
  (winning-rows-or-columns
    (build-list
      board-size
      (λ (y)
        (build-list
          board-size
          (λ (x) (hash-ref b* (cons x y))))))))

(define (mark b n)
  (match-define (board b* index) b)
  (define (update-board-board coord)
    (struct-copy board b [board (hash-set b* coord #f)]))
  (~> (index)
      (hash-ref n #f)
      (if _
        update-board-board
        (const b))))

(define (find-first-winner moves boards)
  (let loop ([boards boards]
             [moves moves])
    (match-define (cons move moves*) moves)
    (define boards* (map (curryr mark move) boards))
    (if (~> (boards*) sep (any winning))
      (values move (first (filter winning boards*)))
      (loop boards* moves*))))

(define-flow (score-winner move board)
  (~> (== _
          (~> board-board hash-values sep (pass number?) +))
      *))

(define-flow part1*
  (~> string->moves+boards find-first-winner score-winner))
(define-flow part1 (~> file->string part1*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))))
