#lang racket

(require qi)

(define closers
  (hash #\) #\( #\> #\< #\} #\{ #\] #\[))

;; string -> null? | char?
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

(define scores (hash #\) 3 #\] 57 #\} 1197 #\> 25137))

(define-flow part1*
  (~> sep
      (amp (make-checker closers))
      (pass char?)
      (amp (hash-ref scores _))
      +))
(define-flow part1 (~> file->lines part1*))

(module+ main
  (command-line
    #:args (input)
    (displayln (time (part1 input)))))
