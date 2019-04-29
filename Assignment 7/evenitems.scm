#lang scheme
; evenitems.scm
; Michael Bilan
; 28 Apr 2019
; CS331 Assignment 7

(define (evenitems xs)
  (cond
    [(not (list? xs))   (error "evenitems: argument is not a list")]
    [(null? xs)        '()]
    [else              (cons
                        (car xs)
                        (if
                         (null? (cdr xs)) null
                         (evenitems (cdr (cdr xs)))
                         )
                        )
                       ]
    )
  )