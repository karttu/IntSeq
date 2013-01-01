#lang r5rs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;; This module Copyright (C) 2009-2013 Antti Karttunen, subject to GPL v2.  ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Our implementation of vector-grow for lesser Schemes.
;; See: http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Construction-of-Vectors.html
;;

;; This small piece of code by karttu, 2012-12-17 for example for Racket R5RS,
;; which doesn't have vector-grow.

(define (vector-grow old-vec new-size)
  (let ((new-vec (make-vector new-size #f))
        (old-size (vector-length old-vec))
       )
    (let copyloop ((i 0))
         (cond ((= i old-size) new-vec)
               (else
                  (begin
                     (vector-set! new-vec i (vector-ref old-vec i))
                     (copyloop (+ 1 i))
                  )
               )
         )
    )
  )
)

