#!r6rs
(library (IntSeq Seqs Triangles triangles-core)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; triangles-core.ss - OEIS-index-functions for regular tables and triangles ;;
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
A000124 ;; o=0: Central polygonal numbers (the Lazy Caterer's sequence): n(n+1)/2 + 1; or, maximal number of pieces formed when slicing a pancake with n cuts. 
A000217 ;; o=0: Triangular numbers: a(n) = C(n+1,2) = n(n+1)/2 = 0+1+2+...+n.
A002024 ;; o=1: n appears n times; floor(sqrt(2n) + 1/2).
A002260 ;; o=1: Triangle T(n,k) = k for k = 1..n.
A002262 ;; o=0: Triangle read by rows: T(n,k), 0 <= k <= n, in which row n lists the first n+1 nonnegative integers.
A003056 ;; o=0: n appears n+1 times. Also table T(n,k) = n+k read by antidiagonals.
A004736 ;; o=1: Triangle read by rows: row n lists the first n positive integers in decreasing order.
A010054 ;; o=0: a(n) = 1 if n is a triangular number else 0.
A014132 ;; o=1: T(n,k) = ((n+k)^2 + n-k)/2, n, k > 0, read by antidiagonals. Complement of triangular numbers A000217.
A025581 ;; o=0: Triangle T(n, k) = n-k, 0 <= k <= n.
A083920 ;; o=0: Number of nontriangular numbers <= n.
  )
  (import (rnrs base (6))
          (Intseq Transforms transforms-core)
  )



(define (A000124 n) (+ 1 (A000217 n)))

(define (A000217 n) (/ (* n (+ n 1)) 2))

(define (A002024 n) (+ 1 (A003056 (- n 1))))

(define (A002260 n) (+ 1 (A002262 (- n 1))))

(define (A002262 n) (- n (A000217 (A003056 n)))) ;; a(n) = n - the largest triangular number <= n. (Murthy)

(define A003056 (LEFTINV-LEASTMONO-NC2NC 0 0 A000217))

(define (A004736 n) (+ 1 (A025581 (- n 1))))

(define (A010054 n) (if (zero? n) 1 (- (A002024 (+ n 1)) (A002024 n)))) ;; The charfun of A000217.

;; Complement of A000217:
(define (A014132 n) (+ n (A002024 n)))

;; (define (A014132v2 n) (A014132bi (A002260 n) (A004736 n))) ;; T(n,k) = ((n+k)^2 + n-k)/2, n, k > 0, read by antidiagonals. 
;; (define (A014132bi n k) (/ (+ (expt (+ n k) 2) n (- k)) 2))


(define (A025581 n) (- (A003056 n) (A002262 n)))

(define (A083920 n) (- n (A003056 n))) ;; Number of nontriangular numbers <= n.


) ;; End of module triangles-core.ss

