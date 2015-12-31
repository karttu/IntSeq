#!r6rs
(library (IntSeq Seqs Sieves sieve-ludic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; sieve-ludic.ss - OEIS-sequences related to sieve generating Ludic numbers ;;
;;                                                                           ;;
;; Antti Karttunen started writing this Scheme R6RS-module Dec 04 2015 by    ;;
;; extracting the related functions from MIT/GNU-Scheme module intfun_a.scm  ;;
;; written in 2002-2015.                                                     ;;
;;                                                                           ;;
;; The OEIS-sequence data (also defs & programs) has been submitted as per   ;;
;;   http://oeis.org/wiki/The_OEIS_Contributor's_License_Agreement           ;;
;; and it is made available with                                             ;;
;;   http://oeis.org/wiki/The_OEIS_End-User_License_Agreement                ;;
;; which uses the Creative Commons Attribution Non-Commercial 3.0 license    ;;
;;   http://creativecommons.org/licenses/by-nc/3.0/                          ;;
;;                                                                           ;;
;; Thus, this module uses the same CC BY-NC 3.0 license.                     ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (export
A003309 ;; o=1: Ludic numbers: apply the same sieve as Eratosthenes, but cross off every k-th /remaining/ number.
A003309v2
A192607 ;; o=1: Nonludic numbers: complement of A003309.

A260717 ;; o=1: Square array: row n gives the numbers remaining before the stage n of Ludic sieve.
A260717bi ;; o=[1,1]
A260718 ;; o=1: Square array A260717 transposed.
A260714 ;; o=1: Row 4 of A260717.
A260715 ;; o=1: Row 5 of A260717.
A236863 ;; o=0: Number of nonludic numbers (A192607) not greater than n.
A192490 ;; o=1: Characteristic function of ludic numbers (A003309).
A192512 ;; o=1: Number of ludic numbers (A003309) not greater than n.
A260723 ;; o=1: First differences of Ludic numbers: a(n) = A003309(n+1) - A003309(n).

rowfun_n_for_A003309sieve
rowfun_n_for_remaining_numbers
rowfun_n_for_A255127
A255127 ;; o=2: Ludic array: square array A(row,col), where row n lists the numbers removed at stage n in the sieve which produces Ludic numbers. Array is read by antidiagonals A(1,1), A(1,2), A(2,1), A(1,3), A(2,2), A(3,1), ... 
A255128 ;; o=1: Inverse permutation to A255127.
A255127bi ;; o=[1,1]
A255129 ;; o=2: Transposed Ludic array.
A255129bi ;; o=[1,1]
A255130 ;; o=1: Inverse permutation to A255129.
A254100 ;; o=1: Postludic numbers: Second column of Ludic array A255127.
A256482 ;; o=1: a(n) = A254100(n) - A003309(n+1).
A256483 ;; o=1: a(n) = A256482(n)/2 = (A254100(n) - A003309(n+1)) / 2.
A255410 ;; o=1: Main diagonal of Ludic array A255127 (and A255129): a(n) = A255127(n,n).
A255413 ;; o=1: Row 3 of Ludic array A255127: a(n) = A007310((5*n)-3).
A255413v2
A255413rec
A255414 ;; o=1: Row 4 of Ludic array A255127.
A255415 ;; o=1: Row 5 of Ludic array A255127.
A255416 ;; o=1: Row 6 of Ludic array A255127.
A255417 ;; o=1: Row 7 of Ludic array A255127.
A255418 ;; o=1: Row 8 of Ludic array A255127.
A255419 ;; o=1: Row 9 of Ludic array A255127.
A260738 ;; o=1: Row index to A255127: a(1) = 0; for n > 1, a(n) = number of the stage where n is removed in the sieve which produces Ludic numbers.
A260739 ;; o=1: Column index to A255127: a(1) = 0; for n > 1, a(n) = the position at the stage where n is removed in the sieve which produces Ludic numbers.
A237126 ;; o=0: a(0)=0, a(1) = 1, a(2n) = nonludic(a(n)), a(2n+1) = ludic(a(n)+1), where ludic = A003309, nonludic = A192607.
A237427 ;; o=0: a(0)=0, a(1)=1; thereafter, if n is k-th ludic number [i.e., n = A003309(k)], a(n) = 1 + (2*a(k-1)); otherwise, when n is k-th nonludic number [i.e., n = A192607(k)], a(n) = 2*a(k).
A235491 ;; o=0: Self-inverse permutation of natural numbers: complementary pair ludic/nonludic numbers (A003309/A192607) entangled with the same pair in the opposite order, nonludic/ludic. See Formula.


  )
  (import (rnrs base (6))
          (Intseq Memoize memoize-definec)
          (Intseq Transforms transforms-core)
          (IntSeq Seqs Triangles triangles-core)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;; This module Copyright (C) 2007-2015 Antti Karttunen, subject to GPL v2.  ;;
;;                                                                          ;;
;; Started writing this R6RS-module Dec 04 2015 by extracting the related   ;;
;; functions from MIT/GNU-Scheme module intfun_a.scm                        ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Ludic numbers:
;; A003309 = 2,3,5,7,11,13,17,23,25,29,37,41,43,47,53,61,67,71,77,83,89,91,97,
;; Ludic numbers: apply the same sieve as Eratosthenes, but cross off every k-th /remaining/ number. 

;; Row 1 = 2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,...
;; Row 2 = 3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,...
;; Row 3 = 5,7,11,13,17,19,23,25,29,31,35,37,41,...
;; Row 4 = 7,11,13,17,23,25,29,31,37,41,...

(define (A260717 n) (A260717bi (A002260 n) (A004736 n)))
(define (A260718 n) (A260717bi (A004736 n) (A002260 n)))

(define (A260717bi row col) ((rowfun_n_for_A003309sieve row) col))

(define (A260714 n) (A260717bi 4 n))

(define (A260715 n) (A260717bi 5 n))

(definec (rowfun_n_for_A003309sieve n) ;;
  (if (= 1 n)
      add1 ;; We return +1 function, that gives 2,3,4,5,6,7,8,9,10,11,...
      (let* ((prevrowfun (rowfun_n_for_A003309sieve (- n 1)))
             (everynth (prevrowfun 1))
            )
         (compose-funs prevrowfun (NONZERO-POS 1 1 (lambda (i) (modulo (- i 1) everynth))))
      )
  )
)

;; When larger values are needed:

;; Say:
;; (define A003309org A003309)

(define file4944199 "seqs/b003309.from_Donovan_Johnson_upto_4944199.txt")
(define file100000 "seqs/b003309.byDonovanJohnson_upto100000.txt")

;; (define A003309vec (read-b-file-to-vector file4944199 4944200))
;; or
;; (define A003309vec (read-b-file-to-vector file100000 100001))
;; (define (A003309 n) (vector-ref A003309vec n))

(definec (A003309 n) (if (= 1 n) 1 ((rowfun_n_for_A003309sieve (- n 1)) 1)))
(define A192607 (COMPLEMENT 1 A003309)) ;; Nonludic numbers.
(define A192512 (LEFTINV-LEASTMONO 1 1 A003309)) ;; Number of ludic numbers <= n.

(define (A236863 n) (if (zero? n) n (- n (A192512 n)))) ;; Number of nonludic numbers <= n.

(define (A192490 n) (if (< n 2) n (- (A192512 n) (A192512 (- n 1))))) ;; Characteristic function of ludic numbers (A003309).

;;;;;;;;;;;;;;;;;;;;


;; Each rowfun is zero-based. It's easier that way.
(definec (rowfun_n_for_remaining_numbers n) ;;
  (if (= 1 n)
      (lambda (n) (+ n n 3)) ;; We return as the first row odd numbers from 3 onward.
      (let* ((rowfun_for_prevrow (rowfun_n_for_remaining_numbers (- n 1)))
             (off (rowfun_for_prevrow 0))
            )
         (COMPOSE rowfun_for_prevrow (lambda (n) (+ 1 n (floor->exact (/ n (- off 1))))))
      )
  )
)

(definec (rowfun_n_for_A255127 n) ;; Should use A260717bi instead of rowfun_n_for_remaining_numbers
  (if (= 1 n)
      (lambda (n) (+ n n)) ;; We return even numbers as the first row, i.e. A005843.
      (let* ((rowfun_for_remaining (rowfun_n_for_remaining_numbers (- n 1)))
             (eka (rowfun_for_remaining 0))
            )
         (COMPOSE rowfun_for_remaining (lambda (n) (* eka (- n 1))))
      )
  )
)


(define (A255127bi row col) ((rowfun_n_for_A255127 row) col))

(define (A255127 n) (if (<= n 1) n (A255127bi (A002260 (- n 1)) (A004736 (- n 1)))))

(define (A255129bi row col) ((rowfun_n_for_A255127 col) row))

(define (A255129 n) (if (<= n 1) n (A255129bi (A002260 (- n 1)) (A004736 (- n 1)))))

(define (A003309v2 n) (if (= 1 n) n (A255127bi (- n 1) 1)))

(define (A254100 n) (A255127bi n 2))

(define (A256482 n) (- (A254100 n) (A003309 (+ 1 n)))) ;; Column 1 of A257257


(define (A256483 n) (/ (A256482 n) 2)) ;; Column 1 of A257258

(define (A260723 n) (- (A003309 (+ 1 n)) (A003309 n)))

;; A255407-A255426 are now reserved for your use. 

(define (A255410 n) (A255127bi n n))

(define (A255413 n) (A255127bi 3 n))
(define (A255413v2 n) (A007310 (- (* 5 n) 3)))

(definec (A255413rec n)
  (cond ((= 1 n) 5)
        ((= 2 n) 19)
        ((= 3 n) 35)
        (else (+ (A255413rec (- n 1)) (A255413rec (- n 2)) (- (A255413rec (- n 3)))))
  )
)

(define (A255414 n) (A255127bi 4 n))
(define (A255415 n) (A255127bi 5 n))
(define (A255416 n) (A255127bi 6 n))
(define (A255417 n) (A255127bi 7 n))
(define (A255418 n) (A255127bi 8 n))
(define (A255419 n) (A255127bi 9 n))

;;;;;;;;;;;;;;;;;;;;



(definec (A260738 n) ;; Row index to A255127.
  (cond ((= 1 n) 0) ;; 1 is outside of A255127.
        ((even? n) 1) ;; Optimization. All even numbers are on the row 1.
        (else ;; We have to search for it, in a two naive loops. (XXX - Could use a binary search in the inner one?)
          (let searchrow ((row 2))
             (let searchcol ((col 1))
                (cond ((>= (A255127bi row col) n)
                         (if (= (A255127bi row col) n)
                             row
                             (searchrow (+ 1 row))
                         )
                      )
                      (else (searchcol (+ 1 col)))
                )
             )
          )
        )
  )
)


(definec (A260739 n) ;; Column index to A255127.
  (cond ((= 1 n) 0) ;; 1 is outside of A255127.
        ((even? n) (/ n 2)) ;; Optimization. All even numbers are on the row 1.
        (else ;; We have to search for it, in a two naive loops. (XXX - Could use a binary search in the inner one?)
          (let searchrow ((row 2))
             (let searchcol ((col 1))
                (cond ((>= (A255127bi row col) n)
                         (if (= (A255127bi row col) n)
                             col
                             (searchrow (+ 1 row))
                         )
                      )
                      (else (searchcol (+ 1 col)))
                )
             )
          )
        )
  )
)


(definec (A237126 n)  ;; Inverse: A237427. Cf. A227413
   (cond ((< n 2) n)
         ((even? n) (A192607 (A237126 (/ n 2))))
         (else (A003309 (+ 1 (A237126 (/ (- n 1) 2)))))
   )
)

(definec (A237427 n)
   (cond ((< n 2) n)
         ((= 1 (A192490 n)) (+ 1 (* 2 (A237427 (- (A192512 n) 1)))))
         (else (* 2 (A237427 (A236863 n))))
   )
)


(definec (A235491 n)
   (cond ((< n 2) n)
         ((= 1 (A192490 n))  (A192607 (A235491 (- (A192512 n) 1))))
         (else (A003309 (+ 1 (A235491 (A236863 n)))))
   )
)


) ;; End of module sieve-ludic.ss
