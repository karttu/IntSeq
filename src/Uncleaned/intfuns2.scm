
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
;;  Last edited  Aug 26 2015 by Antti Karttunen.                          ;;
;;                                                                        ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *MAX-CACHE-SIZE-FOR-DEFINEC* 290512) ;; Was 131072

(load "../Schemuli/lstfuns1")
(load "../Schemuli/intfun_a")
(load "../Schemuli/intfun_b")
(load "../Schemuli/intfun_c")

(load "../Schemuli/miscnum2")

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
(define A_greedy_Pell_sums (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS A000129))

(define A_greedy_Jacob_sums (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS (COMPOSE A001045 1+)))

(define A_greedy_A005187_sums (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS A005187))

(define A_greedy_A003714_sums (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS A003714))


