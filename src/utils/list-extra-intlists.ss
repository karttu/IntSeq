#!r6rs
(library (intseq utils list-extra-intlists)
  (export iota0 iota1 iota2 ilist-firstdiffs ilist-firstdiffs_v2 ilist-firstdiffs1 ilist-partsums ilist-revdeltas)
  (import (rnrs base (6))
          (rnrs lists (6))
          (rnrs mutable-pairs (6))
          (intseq utils list-extra-mutating)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;; This module Copyright (C) 2009-2015 Antti Karttunen, subject to GPL v2.  ;;
;;                                                                          ;;
;; Started writing this R6RS-module Dec 04 2015 by scavenging some useful   ;;
;; code from lstfuns1.scm                                                   ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (iota0 upto-n)
   (let loop ((n upto-n) (result (list)))
      (cond ((zero? n) (cons 0 result))
            (else (loop (- n 1) (cons n result)))
      )
   )
)


(define (iota1 upto-n)
   (let loop ((n upto-n) (result (list)))
      (cond ((zero? n) result)
            (else (loop (- n 1) (cons n result)))
      )
   )
)


(define (iota2 upto-n)
   (let loop ((n upto-n) (result (list)))
      (cond ((< n 2) result)
            (else (loop (- n 1) (cons n result)))
      )
   )
)


(define (ilist-firstdiffs a)
  (map - (cdr a) (reverse! (cdr (reverse a))))
)


(define (ilist-firstdiffs_v2 numlist)
  (if (or (null? numlist) (null? (cdr numlist)))
      (list)
      (cons (- (cadr numlist) (car numlist))
            (ilist-firstdiffs_v2 (cdr numlist))
      )
  )
)

;; Like above, but keeps the first element of the original list, ints:
(define (ilist-firstdiffs1 ints) (reverse (fold-left (lambda (xs x) (cons (- x (apply + xs)) xs)) '() ints)))

(define (ilist-partsums a)
  (cdr (reverse! (fold-left (lambda (psums n) (cons (+ n (car psums)) psums)) (list 0) a)))
)

(define (ilist-revdeltas ints) (ilist-partsums (reverse (ilist-firstdiffs1 ints))))


) ;; End of module list-extra-intlists.ss
