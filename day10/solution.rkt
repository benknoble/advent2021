#lang racket

(provide (all-defined-out))

(require qi
         math/statistics)

(define closers
  (hash #\) #\( #\> #\< #\} #\{ #\] #\[))
(define openers
  (for/hash ([(k v) (in-hash closers)])
    (values v k)))

;; string -> list? | char?
(define ((make-checker closers) s)
  (define-values (opens closes)
    (~> (closers)
        (-< hash-values hash-keys)
        (amp list->set)))
  (let/ec return
    (~> (s)
        string->list sep
        (>>
          (switch (% 1> _)
            [(set-member? opens _) cons]
            [(set-member? closes _)
             (if (~> (== (hash-ref closers _) car) eqv?)
               (~> 2> cdr)
               (~> 1> return))]
            [else 2>])
          '()))))

(define checker-scores (hash #\) 3 #\] 57 #\} 1197 #\> 25137))

(define checker (make-checker closers))

(define-flow part1*
  (~> sep
      (amp checker)
      (pass char?)
      (amp (hash-ref checker-scores _))
      +))
(define-flow part1 (~> file->lines part1*))

(define-flow incomplete?
  (~> checker
      (if (or char? null?) ground _)))

(define complete-scores (hash #\) 1 #\] 2 #\} 3 #\> 4))

(define-flow score-repair
  (>> (~> (== (hash-ref complete-scores _) (* 5)) +)
      0))

(define-flow part2*
  (~>> sep
       ;; needs to be separate stage from below because grounds some inputs
       (amp incomplete?)
       (amp (~> sep (amp (hash-ref openers _)) score-repair))
       collect
       (median <)))
(define-flow part2 (~> file->lines part2*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))
    (displayln (time (part2 input)))))
