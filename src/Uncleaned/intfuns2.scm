
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;  http://www.iki.fi/~kartturi/matikka/Schemuli/intfuns1.scm             ;;
;;  - Often needed integer functions. Now divided crudely into two.       ;;
;;                                                                        ;;
;;  Coded by Antti Karttunen (my_firstname.my_surname@gmail.com),         ;;
;;   2002-2010                                                            ;;
;;                                                                        ;;
;;  This Scheme-code is in Public Domain and runs (at least)              ;;
;;  in MIT Scheme Release 7.6.0/7.7.?, for which one can find documents   ;;
;;  and the pre-compiled binaries (for various OS's running in            ;;
;;  Intel x86 architecture) under the URL:                                ;;
;;  http://www.swiss.ai.mit.edu/projects/scheme/                          ;;
;;                                                                        ;;
;;  Last edited  Dec 19 2015 by Antti Karttunen.                          ;;
;;                                                                        ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *MAX-CACHE-SIZE-FOR-DEFINEC* 290512) ;; Was 131072

(load "../Schemuli/lstfuns1")
(load "../Schemuli/intfun_a")
(load "../Schemuli/intfun_b")
(load "../Schemuli/intfun_c")

(load "../Schemuli/miscnum2.scm") ;; .com has been out of date for a long time.

(load "../Schemuli/gf2xfuns")

(load "../Schemuli/permfuns")


(define A000120grs (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS (COMPOSE A000079 -1+)))

(define A053735grs (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS (COMPOSE A000244 -1+))) ;; Sum of digits of (n written in base 3).

(define A007895grs (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS (COMPOSE A000045 1+)))

(define A014420grs (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS A000108))

(define A034968grs (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS A000142))

;; A053610: Number of positive squares needed to sum to n using the greedy algorithm. Cf. A002828, A255131.
(define A053610 (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS A000290))

;; A055401 Number of positive cubes needed to sum to n using the greedy algorithm. 
(define A055401grs (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS A000578))

;; Number of triangular numbers needed to represent n with greedy algorithm. 
(define A057945 (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS A000217))

(define A072491 (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS A008578))

;; XXX - Not yet in OEIS:
(define A265743 (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS A005187)) ;; Greedy A005187-sums
;; XXX: To do: Number of greedy carryless sums of A005187,
;; "carryless" meaning that n = A005187(x) + A005187(y) + A005187(z) = A005187(x) XOR A005187(y) XOR A005187(z).
;; Similar construction for A003714 (Fibbinary numbers).

(define A265744 (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS A000129)) ;; Greedy Pell sums

(define A130249almost (LEFTINV-LEASTMONO 0 2 A001045))

(define (A130249 n) (if (zero? n) n (A130249almost n))) ;; o=0:	Maximal index k of a Jacobsthal number such that A001045(k)<=n (the 'lower' Jacobsthal inverse). 

(definec (A265745 n) (if (zero? n) n (+ 1 (A265745 (- n (A001045 (A130249 n)))))))

(define A265745v2 (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS (COMPOSE A001045 1+))) ;; Greedy Jacobsthal sums


(define (A197911 n) ;; o=0: Representable by A001045 (Jacobsthal sequence). Complement of A003158.
  (let loop ((n n) (s 0) (i 2))
     (cond ((zero? n) s)
           ((odd? n) (loop (/ (- n 1) 2) (+ s (A001045 i)) (+ 1 i)))
           (else (loop (/ n 2) s (+ 1 i)))
     )
  )
)

;; Where A023645 a(n) = tau(n)-1 if n is odd or tau(n)-2 if n is even. 

(definec (A023645_after_some_n_check_it n)
   (if (<= n 2) (- n 1)
      (let ((jn (A001045 n)))
          (let loop ((s 0) (i 2) (ji (A001045 2)))
             (cond ((= i n) s)
                   (else (loop (+ s (if (zero? (modulo jn ji)) 1 0))
                               (+ i 1)
                               (A001045 (+ 1 i))
                         )
                   )
             )
          )
      )
   )
)

(definec (A265746 n) (if (zero? n) n (+ (A000244 (- (A130249 n) 2)) (A265746 (- n (A001045 (A130249 n)))))))

(definec (A265747 n) (if (zero? n) n (+ (expt 10 (- (A130249 n) 2)) (A265747 (- n (A001045 (A130249 n)))))))
(define (A265747v2 n) (A007089 (A265746 n)))


(define A_greedy_A003714_sums (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS A003714))

