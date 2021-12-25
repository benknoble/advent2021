#lang racket

(require qi)

(struct cucumbers [x y map] #:transparent)

(define display-cucumbers
  (match-lambda
    [(cucumbers X Y m)
     (for ([y (in-range Y)])
       (for ([x (in-range X)])
         (display (~> (m) (hash-ref (cons x y) #f)
                      (switch
                        [(eq? 'east) #\>]
                        [(eq? 'south) #\v]
                        [else #\.]))))
       (newline))]))

(define-flow lines->rows
  (~> sep
      (amp string->list)
      collect))

(define (rows->cucumbers rows)
  (cucumbers
    (length (first rows)) (length rows)
    (for*/hash ([(row y) (in-indexed (in-list rows))]
                [(col x) (in-indexed (in-list row))]
                #:unless (eq? col #\.))
      (values (cons x y) (case col
                           [(#\>) 'east]
                           [(#\v) 'south])))))

(define-flow lines->cucumbers (~> lines->rows rows->cucumbers))
(define-flow file->cucumbers (~> file->lines lines->cucumbers))

(define (next-x x)
  (flow (~> (-< car cdr)
            (== (~> add1 (modulo x)) _)
            cons)))

(define (next-y y)
  (flow (~> (-< car cdr)
            (== _ (~> add1 (modulo y)))
            cons)))

(define step
  (match-lambda
    [(cucumbers x y m)
     (define nx (next-x x))
     (define ny (next-y y))
     (define easts-moved
       (~> (m)
           (hash-map
             (flow (~> (if (and (~> 2> (eq? 'east))
                                (~> 1> nx
                                    (not (hash-has-key? m _))))
                         (== nx _)
                         _)
                       cons)))
           make-immutable-hash))
     (define souths-moved
       (~> (easts-moved)
           (hash-map
             (flow (~> (if (and (~> 2> (eq? 'south))
                                (~> 1> ny
                                    (not (hash-has-key? easts-moved _))))
                         (== ny _)
                         _)
                       cons)))
           make-immutable-hash))
     (cucumbers x y souths-moved)]))

(define-flow step-until-stopped
  (~> (-< 0 #f _)
      (feedback (while (~> (block 1) (not equal?)))
                (then (select 1 2))
                (-< (~> 1> add1) 3> (~> 3> step)))))

(define-flow part1* (~> step-until-stopped 1>))
(define-flow part1 (~> file->cucumbers part1*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))))
