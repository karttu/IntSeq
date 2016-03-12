#!r6rs
(library (IntSeq Seqs Sieves sieve-eratosthenes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; sieve-eratosthenes.ss - OEIS-sequences related to Eratosthenes' sieve     ;;
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
A000040 ;; o=1: The prime numbers.
A083221 ;; o=2: Sieve of Eratosthenes arranged as an array and read by antidiagonals as A(1,1), A(1,2), A(2,1), A(1,3), A(2,2), A(3,1), ... 
A083221bi ;; o=[1,1].
A083140 ;; o=2: Sieve of Eratosthenes arranged as an array and read by antidiagonals in the up direction; n-th row has property that smallest prime factor is prime(n). (Transpose of A083221).
A083141 ;; Main diagonal of array in A083140.
A114881 ;; o=1: Sieve of Eratosthenes minus 1.
A249741 ;; o=1: Sieve of Eratosthenes minus one: a(n) = A083221(n+1) - 1. (Transpose of A114881.)
A249743 ;; o=1: Main diagonal of square arrays A114881 and A249741.
A000720 ;; o=1: pi(n), the number of primes <= n. Sometimes called PrimePi(n)
A001223 ;; o=1: Differences between consecutive primes.
A007504 ;; o=0: Sum of first n primes.
A010051 ;; o=1: Characteristic function of primes: 1 if n is prime else 0. 
A256956 ;; o=1: Product of two consecutive terms of pi(n): a(n) = pi(n) * pi(n+1), where pi(n) = A000720(n) gives the number of primes <= n.
A046992 ;; o=1: a(n) = Sum_{k=1..n} pi(k) (cf. A000720).
A249727 ;; o=1: Start with a(1) = 1; then numbers 1 .. primepi(2), followed by numbers 1 .. primepi(3), and then numbers 1 .. primepi(4), ..., etc, where A000720 gives primepi.
A249728 ;; o=1: After a(1) = 1 each n appears A000720(n) times. 
A065855 ;; o=1: Number of composites <= n. 
A062298 ;; o=1: Number of nonprimes <= n.
A002808 ;; o=1: The composite numbers: numbers n of the form x*y for x > 1 and y > 1.
A018252 ;; o=1: The nonprime numbers (1 together with the composite numbers, A002808). 
A007821 ;; o=1: Primes p such that pi(p) is not prime.
A049078 ;; o=1: Primes prime(k) for which A049076(k) = 2.
A138042 ;; o=1: Numbers n such that A096379(n)=A096379(n+1).
A066495 ;; o=1: Numbers n such that f(n) = f(n-1)+f(n-2) where f denotes the prime gaps function given by f(m) = p(m+1)-p(m) and p(m) denotes the m-th prime.
A117876 ;; o=1: Primes p=prime(k) of level (1,2), i.e. such that A118534(k) = prime(k-2).
A083375 ;; o=1: n appears prime(n) times.
;; A252460 ;; [AK] o=1: Inverse permutation to A083221 considered as a permutation of natural numbers (with assumption that a(1) = 1).
  )
  (import (rnrs base (6))
          (Intseq Memoize memoize-definec)
          (Intseq Transforms transforms-core)
          (IntSeq Seqs Triangles triangles-core)
  )


(definec (rowfun_n_for_Esieve n) ;;
  (if (= 1 n)
      (lambda (n) (+ 1 n)) ;; We return +1 (add1) function, that gives 2,3,4,5,6,7,8,9,10,11,...
      (let* ((prevrowfun (rowfun_n_for_Esieve (- n 1)))
             (prevprime (prevrowfun 1))
            )
         (COMPOSE prevrowfun (NONZERO-POS 1 1 (lambda (i) (mod (prevrowfun i) prevprime))))
      )
  )
)

;; Row 1 = 2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,...
;; Row 2 = 3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,...
;; Row 3 = 5,7,11,13,17,19,23,25,29,31,35,37,41,43,47,49,53
;; Row 4 = 7,11,13,17,19,23,29,31,41,43,47,49,53,...


(definec (A000040 n) ((rowfun_n_for_Esieve n) 1))
(define A000720 (LEFTINV-LEASTMONO 1 1 A000040)) ;; We have a more efficient definition somewhere...

(define (A010051 n) (if (<= n 1) 0 (- (A000720 n) (A000720 (- n 1)))))

(define (A256956 n) (* (A000720 n) (A000720 (+ 1 n))))


(definec (A046992 n) (if (<= n 1) 0 (+ (A000720 n) (A046992 (- n 1))))) ;; a(n) = Sum_{k=1..n} pi(k) (cf. A000720). 

(define (A249727 n) (if (= 1 n) 1 (- n (+ 1 (A046992 (- (A249728 n) 1))))))
(define A249728 (COMPOSE (LEAST-GTE-I 0 1 A046992) sub1)) ;; One-based.

(define (A065855 n) (- n (A000720 n) 1))
(define (A062298 n) (- n (A000720 n)))

(define (A001223 n) (- (A000040 (+ 1 n)) (A000040 n)))

(define A018252 (COMPLEMENT 1 A000040)) ;; 1 and composites.
(define (A002808 n) (A018252 (+ 1 n)))
;; (define A002808 (COMPOSE A018252 add1)) ;; Composites.

(define (A007821 n) (A000040 (A018252 n)))

(define (A049078 n) (A000040 (A007821 n)))

(define A138042 (MATCHING-POS 1 1 (lambda (n) (= 2 (/ (+ (A000040 n) (A000040 (+ n 3))) (A000040 (+ n 2)))))))
(define A066495 (MATCHING-POS 1 1 (lambda (n) (and (> n 2) (= (A001223 n) (+ (A001223 (- n 1)) (A001223 (- n 2))))))))
;; (define (A066495v2 n) (+ 2 (A138042 n)))

(define (A117876 n) (A000040 (A066495 (+ 1 n))))

;; Some .... what would be needed for A227421
;; (define Aalussa
;;     (MATCHING-POS 1 1
;;         (lambda (n)
;;            (let loop ((k 3))
;;                 (cond ((= (A000040 (+ n 1)) (/ (+ (A000040 n) (A000040 (+ n k))) 2)) #t)
;;                       ((< (A000040 (+ n 1)) (/ (+ (A000040 n) (A000040 (+ n k))) 2)) #f)
;;                       (else (loop (+ k 1)))
;;                 )
;;            )
;;         )
;;     )
;; )
;; 
;; 
;; (define Alopussa ;; Differs from A138042 for the first time at n=28, as A138042(28)=625 and Alopussa(28)=571
;;     (MATCHING-POS 1 1
;;         (lambda (n)
;;            (let loop ((k 3))
;;                 (cond ((= (A000040 (+ n k -1)) (/ (+ (A000040 n) (A000040 (+ n k))) 2)) #t)
;;                       ((> (A000040 (+ n k -1)) (/ (+ (A000040 n) (A000040 (+ n k))) 2)) #f)
;;                       (else (loop (+ k 1)))
;;                 )
;;            )
;;         )
;;     )
;; )
;; 
;; 
;; (define (Aprime_offset_over_1 n)
;;     (let loop ((k 2))
;;          (cond ((= (A000040 (+ n k -1)) (/ (+ (A000040 n) (A000040 (+ n k))) 2)) k)
;;                ((> (A000040 (+ n k -1)) (/ (+ (A000040 n) (A000040 (+ n k))) 2)) 0)
;;                (else (loop (+ k 1)))
;;          )
;;     )
;; )
;; 
;; (define (Aprime_offset_over_2 n)
;;     (let loop ((k 3))
;;          (cond ((= (A000040 (+ n k -1)) (/ (+ (A000040 n) (A000040 (+ n k))) 2)) k)
;;                ((> (A000040 (+ n k -1)) (/ (+ (A000040 n) (A000040 (+ n k))) 2)) 0)
;;                (else (loop (+ k 1)))
;;          )
;;     )
;; )
;; 
;; (define Aeka_apu (COMPOSE Aprime_offset_over_1 (NONZERO-POS 1 1 Aprime_offset_over_1)))
;; (define Atoinenapu (COMPOSE Aprime_offset_over_2 (NONZERO-POS 1 1 Aprime_offset_over_2)))
;; 
;; 
;; (define Anew_primes_and_composited_convolved (CONVOLVE 1 A000040 A002808)) ;; Noise?
;; (define Anew_primes_and_nonprimes_convolved (CONVOLVE 1 A000040 A018252)) ;; More of the same, noise?

;; (definec (A000010slow n) ;; A slow implementation. Faster implementation in miscnum2.scm
;;   (let loop ((i n) (s 0))
;;        (cond ((zero? i) s)
;;              (else
;;                (let ((a (gcd n i)))
;;                  (loop (sub1 i)
;;                        (+ s (if (= 1 (gcd n i)) 1 0))
;;                  )
;;                )
;;              )
;;        )
;;   )
;; )
;; 

;; In real life we would implement A010051 with some probabilistic or deterministic
;; primeness function, and then have:
;; (define A000040 (NONZERO-POS 1 1 A010051))
;; and
;; (define A000720 (PARTIALSUMS 1 1 A010051))
;;
;; (possibly with some clever optimization traversing only 6n+1 and 6n+5, etc.
;;  also caching a lots of smaller terms).


(define A007504 (PARTIALSUMS 1 1 A000040))

;; A083375 n appears prime(n) times.
(define A083375 (LEAST-GTE-I 1 1 A007504))


;; Compute these sieve arrays as A-entries, and also similar tables as Yasutoshi's A083140.

(definec (rowfun_n_for_A083221 n) ;;
  (if (= 1 n)
      (lambda (n) (+ n n)) ;; We return even numbers as the first row, i.e. A005843.
      (let ((rowfun_of_esieve (rowfun_n_for_Esieve n))
            (prime (A000040 n))
           )
         (COMPOSE rowfun_of_esieve
                  (MATCHING-POS 1 1 (lambda (i) (zero? (mod (rowfun_of_esieve i) prime))))
         )
      )
  )
)

(define (A083221bi row col) ((rowfun_n_for_A083221 row) col)) ;; swapped the argument order Nov 14 2014.

;; (define (A083221 n) (A083221bi (add1 (A025581 (sub1 n))) (add1 (A002262 (sub1 n)))))
;; (define (A083140 n) (A083221bi (add1 (A002262 (sub1 n))) (add1 (A025581 (sub1 n)))))
;; Analogous tables for A000959 and A003309 should be permutations of natural numbers >= 1.
;; (define (A083221 n) (A083221bi (A002260 n) (A004736 n)))
;; (define (A114881 n) (- (A083140 n) 1))

;; A255543-A255574 are now reserved for your use. 

(define (A083221 n) (if (<= n 1) n (A083221bi (A002260 (- n 1)) (A004736 (- n 1)))))

(define (A083140 n) (if (<= n 1) n (A083221bi (A004736 (- n 1)) (A002260 (- n 1)))))

(define (A083141 n) (if (= 1 n) 2 (* (A000040 n) (A000040 (* 2 (- n 1))))))
(define (A083141v2 n) (A083221bi n n))


(define (A249741 n) (- (A083221 (+ n 1)) 1))
(define (A249741v2 n) (- (A083221bi (A002260 n) (A004736 n)) 1))
(define (A114881 n) (- (A083140 (+ n 1)) 1))


(define (A249743 n) (if (= 1 n) n (- (* (A000040 n) (A000040 (+ n n -2))) 1)))
(define (A249743v2 n) (- (A083221bi n n) 1))


;; See also Y. Motohashi, "An Overview of the Sieve Method and its History"
;; http://arxiv.org/abs/math.NT/0505521

;; Similarly, implement rowfun_n_for_A014580, provided there is an easily
;; implementable Euclidean algorithm for GCD's of GF(2)[X] polynomials.

;; When A055396 & A078898 are ready:
;;
;; (definec (A252460 n)
;;   (if (<= n 1)
;;       n
;;       (let ((x (A055396 n))
;;             (y (A078898 n))
;;            )
;;         (+ 1 (* (/ 1 2) (- (expt (+ x y) 2) x y y y -2)))
;;       )
;;   )
;; )
;; 

) ;; End of module sieve-eratosthenes.ss
