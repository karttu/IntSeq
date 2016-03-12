#!r6rs
(library (IntSeq Seqs Sieves sieve-lucky)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; sieve-lucky.ss - OEIS-sequences related to sieve generating Lucky numbers ;;
;;                                                                           ;;
;; Antti Karttunen started writing this Scheme R6RS-module Dec 04 2015 by    ;;
;; extracting the related functions from MIT/GNU-Scheme module intfun_a.scm  ;;
;; written in 2002-2015.                                                     ;;
;;                                                                           ;;
;; This module last edited 2016-03-12.                                       ;;
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
A258207 ;; o=1: Square array: row n gives the numbers remaining after the stage n of Lucky sieve.
A258208 ;; o=1: Transpose of array A258207 which gives the numbers remaining after the stage n of Lucky sieve.
A258207bi ;; o=[1,1]
A047241 ;; o=1: Numbers that are congruent to {1, 3} mod 6. XXX - Move!
;; A047241v2
A016969 ;; o=0: a(n) = 6n + 5. XXX - Move!
A258011 ;; o=1: Numbers remaining after the third stage of Lucky sieve.
A258016 ;; o=1: Unlucky numbers removed at the stage three of Lucky sieve.
;; A258016v2
A260440 ;; o=1: Unlucky numbers removed at the stage four of Lucky sieve.
;; A260440v2
rowfun_n_for_A000959sieve
A000959 ;; o=1: Lucky numbers.
A031883 ;; o=1: First differences of lucky numbers.
;; A254967 ;; o=0: Triangle of iterated absolute differences of lucky numbers read by anti-diagonals upwards. XXX: Do!
A050505 ;; o=1: Unlucky numbers.
A109497 ;; o=0: Number of lucky numbers <= n.
A145649 ;; o=1: Characteristic function of the lucky numbers.
A219178 ;; o=1: a(n) = first unlucky number removed at the n-th stage of Lucky sieve.
rowfun_n_for_A255543
A255543 ;; o=1: Unlucky array: Row n consists of unlucky numbers removed at the stage n of Lucky sieve.
A255543bi
A255544 ;; o=1: Unlucky array, transposed.
A255545 ;; o=1: Lucky / Unlucky array: Each row starts with n-th lucky number, followed by all unlucky numbers removed at stage n of the sieve.
A255545bi ;; o=[1,1]
A255546 ;; o=1: Inverse permutation to A255545.
A255547 ;; o=1: Lucky / Unlucky array (A255545), transposed.
A255548 ;; o=1: Inverse permutation to A255547.
A255549 ;; o=1: Main diagonal of Unlucky array: a(n) = A255543(n,n).
A255550 ;; o=1: Main diagonal of array A255551.
;; A255550v2
A255551 ;; o=2: Lucky / Unlucky array, shifted version, read by antidiagonals A(1,1), A(1,2), A(2,1), A(1,3), A(2,2), A(3,1), ...
A255551bi ;; o=[1,1]
A255552 ;; o=1: Inverse permutation to A255551.
A260438 ;; o=1: Row index to A255545: If n is k-th Lucky number then a(n) = k, otherwise a(n) = number of the stage where n is removed in Lucky sieve.
A260429 ;; o=1: Column index to A255545: if n is Lucky number, then a(n) = 1, otherwise a(n) = 1 + the position at the stage where n is removed in the Lucky sieve.
A260439 ;; o=1: Column index to A255551: a(1) = 0; for n > 1: if n is Lucky number then a(n) = 1, otherwise for a(2k) = k, and for odd unlucky numbers, a(n) = 1 + the position at the stage where n is removed in the Lucky sieve.
A260437 ;; o=1: Column index to A255543: if n is Lucky number then a(n) = 0, otherwise a(n) = the position at the stage where n is removed in the Lucky sieve.
A269369 ;; [AK] o=1: a(1) = 1, a(n) = A260439(n)-th number k for which A260438(k) = A260438(n)+1; a(n) = A255551(A260438(n)+1, A260439(n)).
A269370 ;; [AK] o=1: a(1) = 1, after which, for odd n: a(n) = A260439(n)-th number k for which A260438(k) = A260438(n)-1, and for even n: a(n) = a(n/2). 
A269372 ;; [AK] o=0: Permutation of even numbers: a(n) = A269369(n+1) - 1.
;; The rest should be in a module of their own, sieve-lucky-permutations.ss
A269373 ;; [AK] o=1: Permutation of natural numbers: a(1) = 1, a(n) = A000079(A260438(n+1)-1) * ((2 * a(A260439(n+1))) - 1).
A269374 ;; [AK] o=1: Permutation of natural numbers: a(1) = 1, a(n) = A255551(A001511(n), a(A003602(n))) - 1.
A269375 ;; [AK] o=0: Tree of Lucky sieve, mirrored: a(0) = 1, a(1) = 2; after which a(2n) = 2*a(n), a(2n+1) = A269369(a(n)).
A269376 ;; [AK] o=1: Permutation of nonnegative integers: a(1) = 0, a(2) = 1, a(2n) = 2*a(n), a(2n+1) = 1 + 2*a(A269370(2n+1)).
A269377 ;; [AK] o=0: Tree of Lucky sieve: a(0) = 1, a(1) = 2; after which a(2n) = A269369(a(n)), a(2n+1) = 2*a(n).
A269378 ;; [AK] o=1: Permutation of natural numbers: a(1) = 0, after which a(2n) = 1 + 2*a(n), a(2n+1) = 2 * a(A269370(n)).

  )
  (import (rnrs base (6))
          (Intseq Memoize memoize-definec)
          (Intseq Transforms transforms-core)
          (IntSeq Seqs Triangles triangles-core)
          (IntSeq Seqs Base-2 base2-core)
  )


(definec (A005408shifted n) (- (* 2 n) 1)) ;; Cached variant of odd numbers, one-based.

;; Lucky numbers:
;; A000959 = 1,3,7,9,13,15,21,25,31,33,37,43,49,51,63,67,69,73,75,79,87,93,99,

;; Delete every 2nd number (of naturals),
;;   leaving 1 3 5 7 ...;
;; the 2nd number remaining is 3, so delete every 3rd number,
;;   leaving 1 3 7 9 13 15 ...;
;; now delete every 7th number,
;;   leaving 1 3 7 9 13 ...;
;; now delete every 9th number; etc.


(define (A258207 n) (A258207bi (A002260 n) (A004736 n)))
(define (A258208 n) (A258207bi (A004736 n) (A002260 n)))

(define (A258207bi row col) ((rowfun_n_for_A000959sieve row) col)) 

(define (A047241v2 n) (A258207bi 2 n))
(define A047241 A047241v2) ;; XXX - Copy from OEIS the original definition.

(define (A016969 n) (+ (* 6 n) 5))


(define (A258011 n) (A258207bi 3 n))

(define (A258016 n) (A047241 (* 7 n)))
(define (A258016v2 n) (A255543bi 3 n))

(define (A260440 n) (A258011 (* 9 n)))
(define (A260440v2 n) (A255543bi 4 n))

(definec (rowfun_n_for_A000959sieve n) ;;
  (if (= 1 n)
      A005408shifted
;; Else, remove every (prevrowfun n):th number from the previous row.
      (let* ((prevrowfun (rowfun_n_for_A000959sieve (- n 1)))
             (everynth (prevrowfun n)) ;; to be removed.
            )
         (COMPOSE prevrowfun (NONZERO-POS 1 1 (lambda (i) (mod i everynth))))
      )
  )
)

(definec (A000959 n) (A258207bi n n)) ;; ((rowfun_n_for_A000959sieve n) n)

(define (A031883 n) (- (A000959 (+ 1 n)) (A000959 n))) ;; First differences of lucky numbers.

(define A050505 (COMPLEMENT 1 A000959)) ;; Unlucky numbers.
(define A109497 (LEFTINV-LEASTMONO 1 1 A000959))
(define (A145649 n) (if (< n 2) n (- (A109497 n) (A109497 (- n 1))))) ;; Characteristic function of lucky numbers (A000959).

;; (definec (A109497 n) (if (zero? n) n (+ (A145649 n) (A109497 (- n 1))))) ;; Number of lucky numbers <= n.
;;  (lambda (n) (* (A145649 n) (A109497 n))) not in OEIS (Inverse for A000959, with zeros).

(definec (rowfun_n_for_A255543 n) ;;
  (if (= 1 n)
      (lambda (n) (+ n n)) ;; We return even numbers as the first row: 2,4,6,8,10,12,...
      (let* ((rowfun_for_remaining (rowfun_n_for_A000959sieve (- n 1)))
             (eka (A000959 n))
            )
         (COMPOSE rowfun_for_remaining (lambda (n) (* eka n)))
      )
  )
)


(define (A255543bi row col) ((rowfun_n_for_A255543 row) col))

(define (A255543 n) (A255543bi (A002260 n) (A004736 n)))
(define (A219178 n) (A255543bi n 1)) ;; First numbers removed by each lucky number in the lucky number sieve. (off=2)

(define (A255549 n) (A255543bi n n))

(define (A255550 n) (if (= 1 n) 2 (A255543bi n (- n 1))))
(define (A255550v2 n) (A255551bi n n))

(define (A255544 n) (A255543bi (A004736 n) (A002260 n)))

(define (A255545bi row col) (if (= 1 col) (A000959 row) (A255543bi row (- col 1))))
(defineperm1 (A255545 n) (A255545bi (A002260 n) (A004736 n)))
(define (A255546 n) (A255545 (- n)))

;; (map A255546 (iota 99))
;; (1 2 3 4 5 7 6 11 10 16 8 22 15 29 21 37 12 46 9 56 28 67 17 79 36 92 14 106 23 121 45 137 55 154 30 172 66 191 13 211 38 232 78 254 20 277 47 301 91 326 105 352 57 379 27 407 19 436 68 466 18 497 120 529 80 562 136 596 153 631 93 667 171 704 190 742 107 781 210 821 24 862 122 904 35 947 231 991 138 1036 25 1082 253 1129 155 1177 26 1226 276)

(defineperm1 (A255547 n) (A255545bi (A004736 n) (A002260 n)))
(define (A255548 n) (A255547 (- n)))


(define (A255551bi row col) (cond ((= 1 row) (+ col col)) ((= 1 col) (A000959 row)) (else (A255543bi row (- col 1)))))
(defineperm1 (A255551 n) (if (<= n 1) n (A255551bi (A002260 (- n 1)) (A004736 (- n 1)))))

(define (A255552 n) (A255551 (- n)))

;;

(definec (A260438 n) ;; Row index to A255545.
  (cond ((not (zero? (A145649 n))) (A109497 n)) ;; If n is a Lucky number, then return its index.
        ((even? n) 1) ;; Optimization. All even numbers are on the row 1.
        (else ;; We have to search for it, in a two naive loops. (XXX - Could use a binary search in inner one?)
          (let searchrow ((row 2))
             (let searchcol ((col 1))
                (cond ((>= (A255543bi row col) n)
                         (if (= (A255543bi row col) n)
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

(definec (A260429 n) ;; Column index to A255545.
  (cond ((not (zero? (A145649 n))) 1) ;; If n is a Lucky number, then return 1.
        ((even? n) (+ 1 (/ n 2))) ;; Optimization. All even numbers are on the row 1, after that 1.
        (else ;; We have to search for it, in a two naive loops. (XXX - Could use a binary search in inner one?)
          (let searchrow ((row 2))
             (let searchcol ((col 1))
                (cond ((>= (A255543bi row col) n)
                         (if (= (A255543bi row col) n)
                             (+ 1 col)
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


(definec (A260439 n) ;; Column index to A255551.
  (cond ((= 1 n) 0) ;; 1 is outside of A255551.
        ((not (zero? (A145649 n))) 1) ;; If n is a Lucky number, then return 1.
        ((even? n) (/ n 2)) ;; Optimization. All even numbers are on the row 1.
        (else ;; We have to search for it, in a two naive loops. (XXX - Could use a binary search in inner one?)
          (let searchrow ((row 2))
             (let searchcol ((col 1))
                (cond ((>= (A255543bi row col) n)
                         (if (= (A255543bi row col) n)
                             (+ 1 col)
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



(definec (A260437 n) ;; Column index to A255543.
  (cond ((not (zero? (A145649 n))) 0) ;; If n is a Lucky number, then return 0.
        ((even? n) (/ n 2)) ;; Optimization. All even numbers are on the row 1.
        (else ;; We have to search for it, with two naive loops. (XXX - Could use a binary search in inner one?)
          (let searchrow ((row 2))
             (let searchcol ((col 1))
                (cond ((>= (A255543bi row col) n)
                         (if (= (A255543bi row col) n)
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

(define (A269369 n) (if (= 1 n) n (A255551bi (+ (A260438 n) 1) (A260439 n))))

(definec (A269370 n) (cond ((= 1 n) n) ((even? n) (A269370 (/ n 2))) (else (A255551bi (- (A260438 n) 1) (A260439 n)))))

(define (A269372 n) (- (A269369 (+ 1 n)) 1))

(definec (A269373 n)
   (cond ((<= n 1) n)
         (else (* (A000079 (- (A260438 (+ 1 n)) 1)) (+ -1 (* 2 (A269373 (A260439 (+ 1 n)))))))
   )
)

(definec (A269374 n)
   (cond ((<= n 1) n)
         ((even? n) (A269372 (A269374 (/ n 2))))
         (else (+ -1 (* 2 (A269374 (/ (+ n 1) 2)))))
   )
)

(definec (A269374v2 n)
   (cond ((<= n 1) n)
         (else (+ -1 (A255551bi (A001511 n) (A269374v2 (A003602 n)))))
   )
)


(definec (A269375 n)
  (cond ((<= n 1) (+ n 1))
        ((even? n) (* 2 (A269375 (/ n 2))))
        (else (A269369 (A269375 (/ (- n 1) 2))))
  )
)

(definec (A269376 n)
  (cond ((<= n 2) (- n 1))
        ((even? n) (* 2 (A269376 (/ n 2))))
        (else (+ 1 (* 2 (A269376 (A269370 n)))))
  )
)



(definec (A269377 n)
   (cond ((<= n 2) (+ 1 n))
         ((even? n) (A269369 (A269377 (/ n 2))))
         (else (* 2 (A269377 (/ (- n 1) 2))))
   )
)

(definec (A269378 n)
   (cond ((= 1 n) (- n 1))
         ((even? n) (+ 1 (* 2 (A269378 (/ n 2)))))
         (else (* 2 (A269378 (A269370 n))))
   )
)



) ;; End of module sieve-lucky.ss
