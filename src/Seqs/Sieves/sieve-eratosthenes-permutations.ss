#!r6rs
(library (IntSeq Seqs Sieves sieve-eratosthenes-permutations.ss)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; sieve-eratosthenes-permutations.ss - OEIS-sequences for permutations      ;;
;;                                      obtained from Sieve of Eratosthenes  ;;
;;                                                                           ;;
;; Antti Karttunen started writing this Scheme R6RS-module Mar 12 2016 by    ;;
;; extracting the related functions from MIT/GNU-Scheme modules intfun*.scm  ;;
;; written in 2002-2016.                                                     ;;
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
A135141 ;; [Matylla] o=1: a(1)=1, a(p_n)=2*a(n), a(c_n)=2*a(n)+1, where p_n = n-th prime, c_n = n-th composite number.
A227413 ;; [AK] o=1: a(1)=1, a(2n)=nthprime(a(n)), a(2n+1)=nthcomposite(a(n)), where nthprime = A000040, nthcomposite = A002808.
A246377 ;; [AK] o=1: Permutation of natural numbers: a(1) = 1, a(p_n) = 2*a(n)+1, a(c_n) = 2*a(n), where p_n = n-th prime = A000040(n), c_n = n-th composite number = A002808(n).
A246378 ;; [AK] o=1: Permutation of natural numbers: a(1) = 1, a(2n) = nthcomposite(a(n)), a(2n+1) = nthprime(a(n)), where nthcomposite = A002808, nthprime = A000040.
  )
  (import (rnrs base (6))
          (Intseq Memoize memoize-definec)
          (Intseq Transforms transforms-core)
          (IntSeq Seqs Triangles triangles-core)
          (IntSeq Seqs Sieves sieve-eratosthenes)
          (IntSeq Seqs Base-2 base2-core)
  )


(definec (A135141 n)
   (cond ((= 1 n) n)
         ((= 1 (A010051 n)) (* 2 (A135141 (A000720 n))))
         (else (+ 1 (* 2 (A135141 (A065855 n)))))
   )
)


(definec (A227413 n) ;; This is inverse permutation of A135141.
   (cond ((< n 2) n)
         ((even? n) (A000040 (A227413 (/ n 2))))
         (else (A002808 (A227413 (/ (- n 1) 2))))
   )
)


(definec (A246377 n)
   (cond ((< n 2) n)
         ((= 1 (A010051 n)) (+ 1 (* 2 (A246377 (A000720 n)))))
         (else (* 2 (A246377 (A065855 n))))
   )
)

(definec (A246378 n) ;; This is inverse permutation of A246377.
   (cond ((< n 2) n)
         ((even? n) (A002808 (A246378 (/ n 2))))
         (else (A000040 (A246378 (/ (- n 1) 2))))
   )
)


) ;; End of module sieve-eratosthenes-permutations.ss

