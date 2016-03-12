#!r6rs
(library (IntSeq Seqs Base-2 base2-core)
  (export
A000035 ;; [NJAS] o=0: Period 2: (0, 1) repeated; a(n) = n mod 2; parity of n.
;;   A000079 ;; Powers of 2: a(n) = 2^n.
A000079 ;; [NJAS] o=0: Powers of 2: a(n) = 2^n.
A000265 ;; [NJAS] o=1: Remove 2's from n; or largest odd divisor of n; or odd part of n. 
A001511 ;; [NJAS] o=1: The ruler function: 2^a(n) divides 2n. Or, a(n) = 2-adic valuation of 2n.
A003602 ;; [NJAS, Mira Bernstein] o=1: Kimberling's paraphrases: if n = (2k-1)*2^m then a(n) = k.
A006519 ;; [NJAS, Plouffe] o=1: Highest power of 2 dividing n.
A007814 ;; [John Tromp] o=1: Exponent of highest power of 2 dividing n, a.k.a. the binary carry sequence, the ruler sequence, or the 2-adic valuation of n.
A059841 ;; [Alford Arnold] o=0: Period 2: Repeat (1,0). a(n) = 1 - (n mod 2).
  )
  (import (rnrs base (6))
          (IntSeq Transforms transforms-core)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; base2-core.ss - OEIS-sequences related to binary representation           ;;
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

(define (A000035 n) (mod n 2))
(define (A059841 n) (- 1 (A000035 n)))
(define (A000079 n) (expt 2 n))

(define (A006519 n) ;; Highest power of 2 dividing n: 1,2,1,4,1,2,1,8,1,2,1,4,1,2,1,16
   (cond ((zero? n) 0)
         (else (let loop ((n n) (i 1))
                 (cond ((odd? n) i)
                       (else (loop (/ n 2) (* i 2)))
                 )
               )
         )
   )
)


;; Note that (A007814 33574912) = 12.
(define (A007814 n) ;; Exponent of the A006519.
   (cond ((zero? n) 0)
         (else (let loop ((n n) (i 0))
                 (cond ((odd? n) i)
                       (else (loop (/ n 2) (+ 1 i)))
                 )
               )
         )
   )
)

(define (A001511 n) (+ (A007814 n) 1))

(define (A000265 n) (/ n (A006519 n))) ;; Remove 2s from n; or largest odd divisor of n.

(define (A003602 n) (let loop ((n n)) (if (even? n) (loop (/ n 2)) (/ (+ 1 n) 2)))) ;; Stand-alone version.


) ;; End of module base2-core.ss


