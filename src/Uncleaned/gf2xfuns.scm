;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;  http://www.research.att.com/~njas/sequences/GF2Xfuns.scm.txt          ;;
;;                                                                        ;;
;;  Coded by Antti Karttunen (Antti.Karttunen(-AT-)iki.fi),               ;;
;;  at the turn of 2003-2004 with some additions in May 2005.             ;;
;;                                                                        ;;
;;  This file contains the Scheme-functions that compute the sequences    ;;
;;                 A091202-A091233 & A091238-A091257                      ;;
;;        A106442-A106447 & A106451-A106457 & A106490-A106495             ;;
;;                             found in                                   ;;
;;     Neil Sloane's On-Line Encyclopedia of Integer Sequences (OEIS)     ;;
;;                            available at                                ;;
;;             http://www.research.att.com/~njas/sequences/               ;;
;;                                                                        ;;
;;  Copy of THIS source file is also available at:                        ;;
;;  http://www.iki.fi/~kartturi/matikka/Schemuli/GF2Xfuns.scm             ;;
;;                                                                        ;;
;;  This Scheme-code is in Public Domain and runs (at least)              ;;
;;  in MIT Scheme Release 7.7.x, for which one can find documentation     ;;
;;  and the pre-compiled binaries (for various OS's running in            ;;
;;  Intel x86 architecture) under the URL:                                ;;
;;  http://www.swiss.ai.mit.edu/projects/scheme/                          ;;
;;                                                                        ;;
;;  Aubrey Jaffer's SLIB Scheme library is available at:                  ;;
;;  http://www.swiss.ai.mit.edu/~jaffer/SLIB.html                         ;;
;;  I have used the version 3a1 from November 2003.                       ;;
;;                                                                        ;;
;;  If you have improvements, suggestions, additions, please send them    ;;
;;  to me with e-mail (with subject TOPIC: GF2Xfuns.scm) and I can edit   ;;
;;  them to this program. Alternatively, you can send the improved        ;;
;;  program directly to Neil Sloane.                                      ;;
;;                                                                        ;;
;;  Last edited  Aug 13 2016 by Antti Karttunen.                          ;;
;;  Trying to abolish sqrt, also A1163xx -> A1143xx, A1164xx -> A1144xx   ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; To be able to load this module, you must have
;; started your MIT Scheme interpreter as:
;;
;; export MITSCHEME_LARGE_HEAP=5000
;; scheme -large
;;


(declare (usual-integrations))


;; (load "/usr/local/lib/slib/mitscheme.init") ;; A. Jaffer's SLIB Scheme library.
(load "c:\\program files (x86)\\slib\\mitscheme.init") ;; A. Jaffer's SLIB Scheme library.
(require 'factor)                 ;; 

(define *where-I-am* "/users/karttu/A/Matikka/Schemuli/")

(define (load-me) (load (string-append *where-I-am* "GF2Xfuns.scm")))


;; See the C++ program GF2Xfacs.c at the this file, to understand
;; how these precomputed vectors have been computed:

(define GF2X_FACVEC (fasload (string-append *where-I-am* "GF2Xffff.vec")))

(define (GF2Xfactor n) (vector-ref GF2X_FACVEC n))

;; (define Xn_1_FACVEC (fasload (string-append *where-I-am* "Xn_1_511.vec"))) ;; Not available now.

(define (Xn_1factor n) (vector-ref Xn_1_FACVEC n))


;; Example: (map GF2Xfactor (iota 17))
;; results: (() (2) (3) (2 2) (3 3) (2 3) (7) (2 2 2) (3 7) (2 3 3) (11)
;;           (2 2 3) (13) (2 7) (3 3 3) (2 2 2 2) (3 3 3 3))
;;
;; Example: (map Xn_1factor (iota 17))
;; results: ((3) (3 3) (3 7) (3 3 3 3) (3 31) (3 3 7 7) (3 11 13)
;;           (3 3 3 3 3 3 3 3) (3 7 73) (3 3 31 31) (3 2047)
;;           (3 3 3 3 7 7 7 7) (3 8191) (3 3 11 11 13 13) (3 7 19 25 31)
;;           (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3) (3 313 471))
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  Copied from http://www.iki.fi/~kartturi/matikka/Schemuli/definech.scm  ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; define unary cached functions. Syntax is like
;; (define (func arg) ...) of Scheme.

;; Note that this and other cached functions depend on MIT Scheme
;; peculiarities, like that vectors are initialized to contain #f's
;; and also that #f is actually same thing as (). To be corrected.

;; Added this 10. July 2002 to avoid allocation catastrophes
;; caused by the careless use of the cached integer functions:
(define *MAX-CACHE-SIZE-FOR-DEFINEC-IN-GF2XFUNS* 290512) ;; Was 131072


(define-syntax definec
  (syntax-rules ()
   ((definec (name arg) e0 ...)
      (define name
        (letrec
           ((_cache_ (vector #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
            (name
             (lambda (arg)
               (cond ((null? arg) _cache_)
                     ((>= arg *MAX-CACHE-SIZE-FOR-DEFINEC-IN-GF2XFUNS*)
                          e0 ...
                     )
                     (else
                         (if (>= arg (vector-length _cache_))
                             (set! _cache_
                                   (vector-grow
                                           _cache_
                                           (min *MAX-CACHE-SIZE-FOR-DEFINEC-IN-GF2XFUNS*
                                                (max (1+ arg)
                                                     (* 2 (vector-length _cache_))
                                                )
                                           )
                                   )
                             )
                         )
                         (or (vector-ref _cache_ arg)
                             ((lambda (res)
                                (vector-set! _cache_ arg res)
                                res
                              )
                               (begin e0 ...)
                             )
                         )
                     )
               ) ; cond
             )
            )
           ) ; letrec-definitions
          name
        ) ; letrec
      ) ;; (define name ...)
   )
  ) ;; syntax-rules
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (fun-succ-matching-is0 pred_on_i?)
 (letrec
   ((_cache_ (vector #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
    (tvimadur ;; The function we are defining and returning here.
      (lambda (n)
       (cond ((not (integer? n)) _cache_) ;; Just for debugging.
             (else
               (if (>= n (vector-length _cache_))
                   (set! _cache_
                        (vector-grow _cache_
                                     (max (1+ n)
                                          (* 2 (vector-length _cache_))
                                     )
                        )
                   )
               )
               (or (vector-ref _cache_ n)
                   ((lambda (result)
                      (vector-set! _cache_ n result)
                      result
                    )
                     (cond ((= 0 n) n)
                           (else
                             (let loop ((i (1+ (tvimadur (-1+ n)))))
                                   (cond ((pred_on_i? i) i)
                                         (else (loop (1+ i)))
                                   )
                             )
                           )
                     )
                   ) ;; Invocation of the lambda-form
               ) ;; or
             ) ;; else
       ) ;; cond
      ) ; lambda (n)
    )
   ) ;; letrec-definitions.
  tvimadur
 ) ;; letrec
)


;; fun_on_i should be N -> N function, preferably cached.

(define (fun-succ-distincts1 fun_on_i)
 (letrec
   ((_cache_ (vector #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
    (belgthor ;; The function we are defining and returning here.
      (lambda (n)
       (cond ((not (integer? n)) _cache_) ;; Just for debugging.
             (else
               (if (>= n (vector-length _cache_))
                   (set! _cache_
                        (vector-grow _cache_
                                     (max (1+ n)
                                          (* 2 (vector-length _cache_))
                                     )
                        )
                   )
               )
               (or (vector-ref _cache_ n)
                   ((lambda (result)
                      (vector-set! _cache_ n result)
                      result
                    )
                     (cond ((< n 2) n)
                           (else
                             (let outloop ((i (1+ (belgthor (-1+ n))))
                                           (val_here (fun_on_i (1+ (belgthor (-1+ n)))))
                                          )
                                (let inloop ((j (-1+ n))) ;; ((j (-1+ i)))
;; If we didn't find any j < i where fun_on_i(belgthor(j)) would have been belgthor(i), then ...
                                   (cond ((zero? j) i) ;; ... we found a new distinct value.
                                         ((= (fun_on_i (belgthor j)) val_here)
                                              (outloop (+ i 1) (fun_on_i (+ i 1)))
                                         )

;; NOT this (SLOW) way: (j beginning from (-1+ i))
;;                                       ((= (fun_on_i j) val_here)
;;                                            (outloop (+ i 1) (fun_on_i (+ i 1)))
;;                                       )

                                         (else (inloop (- j 1)))
                                   )
                                )
                             )
                           )
                     )
                   ) ;; Invocation of the lambda-form
               ) ;; or
             ) ;; else
       ) ;; cond
      ) ; lambda (n)
    )
   ) ;; letrec-definitions.
  belgthor
 ) ;; letrec
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  Copied from http://www.iki.fi/~kartturi/matikka/Schemuli/intfuns1.scm  ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (first_pos_with_funs_val_gte fun n)
   (let loop ((i 0))
      (if (>= (fun i) n) i
          (loop (1+ i))
      )
   )
)


;; Factorial with syntax modified from the post-fix to pre-fix !:
(definec (! n) (if (zero? n) 1 (* n (! (-1+ n))))) ;; A000142


;; CatTrianglDirect := (r,m) -> `if`((m < 0),0,((r-m+1)*(r+m)!)/(r! * m! * (r+1)));
;; A009766 gives also:
;; a(n,m) = C(n+m,n)*(n-m+1)/(n+1), n >= m >= 0.

(define (CatTriangle r m)
   (if (or (> m r) (< m 0))
       0
       (/ (* (1+ (- r m)) (! (+ r m)))
          (* (! r) (! m) (1+ r))
       )
   )
)

(definec (binomial_n_2 n) (/ (* (-1+ n) n) 2))

;; These now in intfun_a.scm with better, sqrt-free definitions:

;; (definec (A025581 n) ;; The X component (column) of square {0..inf} arrays
;;   (- (binomial_n_2 (1+ (floor->exact (flo:+ 0.5 (flo:sqrt (exact->inexact (* 2 (1+ n)))))))) (1+ n))
;; )
;; 
;; ;; (map A002262 (cons 0 (iota 20))) --> (0 0 1 0 1 2 0 1 2 3 0 1 2 3 4 0 1 2 3 4 5)
;; (definec (A002262 n) ;; The Y component (row) of square {0..inf} arrays
;;   (- n (binomial_n_2 (floor->exact (flo:+ 0.5 (flo:sqrt (exact->inexact (* 2 (1+ n))))))))
;; )
;; 
;; 
;; (define (A002024 n) ;; repeat n n times, starting from n = 1.
;;   (floor->exact (+ (/ 1 2) (sqrt (* 2 n))))
;; )
;; 
;; (define (A003056 n) ;; repeat n n+1 times, starting from n = 0.
;;   (floor->exact (- (sqrt (* 2 (1+ n))) (/ 1 2)))
;; )
;;

(define (A001477 n) n)

(define (A000079 n) (expt 2 n))
(define (A000225 n) (-1+ (A000079 n)))
(define (A000051 n) (1+ (A000079 n)))
;; Offset=0: 2,3,5,9,17,33,65,129,257,513,

(define (A000290 n) (* n n))

;; Moser-de Bruijn sequence: 0,1,4,5,16,17,20,21,64,65,68,69,80,...
;; I.e. (A048720bi n n)
(define (A000695 n) ;; Expand bits, 0->00, 1->01, i.e. from base-2 to base-4.
  (let loop ((n n) (x 1) (z 0))
     (cond ((zero? n) z)
           (else (loop (floor->exact (/ n 2)) (* x 4) (+ z (* (modulo n 2) x))))
     )
  )
)

(define (A000523 n) (cond ((zero? n) -1) (else (floor->exact (/ (log n) (log 2))))))

(define (binwidth n) ;; = A029837(n+1)
  (let loop ((n n) (i 0))
     (if (zero? n)
         i
         (loop (floor->exact (/ n 2)) (1+ i))
     )
  )
)

(define (halve n) (/ n 2))

(define (obtain-integer-bitwise-function bit-string-FUN)
  (lambda (x y)
    (let ((size (max (binwidth x) (binwidth y))))
      (bit-string->unsigned-integer
        (bit-string-FUN (unsigned-integer->bit-string size x)
                        (unsigned-integer->bit-string size y)
        )
      )
    )
  )
)

(define A003986bi  (obtain-integer-bitwise-function bit-string-or))
(define A003987bi  (obtain-integer-bitwise-function bit-string-xor))
(define A004198bi  (obtain-integer-bitwise-function bit-string-and))


(define (pow2? n) (and (> n 0) (zero? (A004198bi n (- n 1)))))

(define (A007088 n) ;; 0,1,10,11,100,101,110,111,1000,... (Show binary form in decimal)
  (let loop ((z 0) (i 0) (n n))
    (if (zero? n)
        z
        (loop (+ z (* (expt 10 i) (modulo n 2)))
              (1+ i)
              (floor->exact (/ n 2))
        )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  Copied from http://www.iki.fi/~kartturi/matikka/Schemuli/lstfuns1.scm  ;;
;;  (always useful!)                                                       ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (compose-funlist funlist)
 (cond ((null? funlist) (lambda (x) x))
       (else (lambda (x) ((car funlist) ((compose-funlist (cdr funlist)) x))))
 )
)


(define (compose-funs . funlist)
 (cond ((null? funlist) (lambda (x) x))
       (else (lambda (x) ((car funlist) ((apply compose-funs (cdr funlist)) x))))
 )
)

(define reversed_iota
  (lambda (n)
      (if (zero? n) (list)
          (cons n (reversed_iota (- n 1)))
      )
  )
)

(define iota (lambda (n) (reverse! (reversed_iota n))))

(define (iota0 upto_n)
   (let loop ((n upto_n) (result (list)))
      (cond ((zero? n) (cons 0 result))
            (else (loop (- n 1) (cons n result)))
      )
   )
)

;; For testing whether we have an identity permutation or not.
(define (first-dislocated lista)
   (let loop ((lista lista) (i 0))
     (cond ((null? lista) lista)
           ((not (equal? (car lista) i)) lista)
           (else (loop (cdr lista) (1+ i)))
     )
   )
)

(define (pos-of-first-matching lista pred?)
   (let loop ((lista lista) (i 0))
     (cond ((null? lista) #f)
           ((pred? (car lista)) i)
           (else (loop (cdr lista) (1+ i)))
     )
   )
)

(define (first-n-where-fun_n-is-i1 fun i)
   (let loop ((n 1))
     (cond ((= i (fun n)) n)
           (else (loop (+ n 1)))
     )
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;; Useful when counting the number of distinct prime divisors:             ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (distinct-elems lista)
   (if (not (pair? lista))
       0
       (let loop ((n 0)
                  (lista lista)
                  (prev (not (car lista)))
                 )
           (cond ((not (pair? lista)) n)
                 ((equal? (car lista) prev) (loop n (cdr lista) prev))
                 (else                      (loop (1+ n) (cdr lista) (car lista)))
           )
       )
   )
)


(define (multiplicities lista) ;; Of numeric elements.
       (let loop ((mults (list))
                  (lista lista)
                  (prev #f)
                 )
           (cond ((not (pair? lista)) (reverse! mults))
                 ((equal? (car lista) prev)
                     (set-car! mults (+ 1 (car mults)))
                     (loop mults (cdr lista) prev)
                 )
                 (else
                   (loop (cons 1 mults) (cdr lista) (car lista))
                 )
           )
       )
)


(define (uniq lista) ;; Assumed to be sorted already with (sort lista <)
   (let loop ((lista lista) (z (list)))
        (cond ((null? lista) (reverse! z))
              ((and (pair? z) (equal? (car z) (car lista)))
                    (loop (cdr lista) z)
              )
              (else (loop (cdr lista) (cons (car lista) z)))
        )
   )
)

(define (factlist->expfactors factors)
   (map cons (uniq factors) (multiplicities factors))
)

(definec (prime-expfactor n) (factlist->expfactors (ifactor n)))

(definec (GF2X-expfactor n) (factlist->expfactors (GF2Xfactor n)))



;; Examples:

;; (prime-expfactor 0    -> ()
;; (prime-expfactor 1    -> ()
;; (prime-expfactor 2)   -> ((2 . 1))
;; (prime-expfactor 3)   -> ((3 . 1))
;; (prime-expfactor 4)   -> ((2 . 2))
;; (prime-expfactor 5)   -> ((5 . 1))
;; (prime-expfactor 6)   -> ((2 . 1) (3 . 1))
;; (prime-expfactor 8)   -> ((2 . 3))
;; (prime-expfactor 9)   -> ((3 . 2))
;; (prime-expfactor 12)  -> ((2 . 2) (3 . 1))
;; (prime-expfactor 360) -> ((2 . 3) (3 . 2) (5 . 1))


;; For doing GCD:
(define (m-intersect a b) ;; of sorted multisets of numeric elements.
    (let loop ((a a)
               (b b)
               (c (list))
              )
        (cond ((or (not (pair? a)) (not (pair? b))) (reverse! c))
              ((equal? (car a) (car b))
                  (loop (cdr a) (cdr b) (cons (car a) c))
              )
              ((< (car a) (car b))
                  (loop (cdr a) b c)
              )
              (else ;; I.e. (> (car a) (car b))
                  (loop a (cdr b) c)
              )
        )
    )
)

;; For doing LCM:
(define (m-union a b) ;; of sorted multisets of numeric elements.
    (let loop ((a a)
               (b b)
               (c (list))
              )
        (cond ((and (not (pair? a)) (not (pair? b))) (reverse! c))
              ((not (pair? a)) (loop a (cdr b) (cons (car b) c)))
              ((not (pair? b)) (loop (cdr a) b (cons (car a) c)))
              ((equal? (car a) (car b))
                  (loop (cdr a) (cdr b) (cons (car a) c))
              )
              ((< (car a) (car b))
                  (loop (cdr a) b (cons (car a) c))
              )
              (else ;; I.e. (> (car a) (car b))
                  (loop a (cdr b) (cons (car b) c))
              )
        )
    )
)

;; For doing GF(2)[X] polynomial division:
;; Return a list where the elements of b have been removed from a.
;; Raise an error if not all the elements of b were in a.
;; Lists a and b are multisets of numeric elements,
;; sorted in the ascending order.
(define (m-difference a b)
    (let loop ((a a)
               (b b)
               (c (list))
              )
        (cond ((not (pair? b)) (append! (reverse! c) a))
              ((not (pair? a))
                (error
                   "The list a does not contain all the elements of list b "
                   b
                )
              )
              ((equal? (car a) (car b)) ;; "Subtract" (car  b) from the a.
                  (loop (cdr a) (cdr b) c)
              )
              ((< (car a) (car b)) ;; Keep (car a)
                  (loop (cdr a) b (cons (car a) c))
              )
              (else ;; I.e. (> (car a) (car b))
                (error
                   "The list a " a
                   " does not contain all the elements of list b " b
                )
              )
        )
    )
)

;; Examples:
;; (m-intersect (list 2 2 3 3 3 7 7 11 23 29 29 37) (list 2 3 3 3 3 3 3 5 7 29 31 37 37))
;; --> (2 3 3 3 7 29 37)

;; (m-union (list 2 2 3 3 3 7 7 11 23 29 29 37) (list 2 3 3 3 3 3 3 5 7 29 31 37 37))
;; --> (2 2 3 3 3 3 3 3 5 7 7 11 23 29 29 31 37 37)

;; (m-intersect (list 3 7 9) (list 2 4 6 8)) --> ()
;; (m-union (list 3 7 9) (list 2 4 6 8))     --> (2 3 4 6 7 8 9)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;; First, a few ordinary number theoretic functions.                       ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; It's mandatory that the factors are sorted!
;; And we can cache them as well... And make other functions to
;; return correct values for n=1.
(definec (ifactor n) (cond ((< n 2) (list)) (else (sort (factor n) <))))


;; Prepared with:
;; (define vecA000040 (list->vector (primes> 0 6542)))
;; (vector-length vecA000040) --> 6542
;; (vector-ref vecA000040 0)  --> 2
;; (vector-ref vecA000040 2)  --> 5
;; (vector-ref vecA000040 6541) --> 65521
;; (map prime? (map (lambda (n) (+ 65520 n)) (iota 37)))
;; (#t () () () () () () () () () () () () () () () #t () #t () () () #t () () () () () () () #t () () () () () #t)
;; (fasdump vecA000040 "sA000040.vec")

;; (define vecA000040 (fasload (string-append *where-I-am* "sA000040.vec")))

(define vecA008578 (fasload (string-append *where-I-am* "sA008578.vec")))

(define (A008578 n) ;; A008578 (non-composite numbers), note the changed offset, now starts with 1
      (vector-ref vecA008578 (- n 1))
)

(define (A000040 n) (A008578 (+ n 1)))

;; Be sure that we won't use any Erasthothenes Sieve for computing the composites!
(define A018252 (COMPLEMENT 1 A000040)) ;; 1 and composites.
(define A002808 (COMPOSE A018252 1+)) ;; Composites.

(define (A049084fast n)
   (if (< n 2)
       0
       (or (pos_of_k_in_vector n vecA008578) 0)
   )
)



;; (define (A008578 n) ;; A008578 (non-composite numbers), note the changed offset, now starts with 1
;;    (cond ((< n 3) (1+ n)) ;; 0 -> 1, 1 -> 2, 2 -> 3,
;;          (else (vector-ref vecA000040 (- n 1)))
;;    )
;; )

;; (define (A000040 n) (vector-ref vecA000040 (- n 1)))
;; (define A000040 A008578) ;; In practice it is good to covetly return also (A000040 0) as 1.

(define vecA001037 (vector 1 2 1 2 3 6 9 18 30 56 99 186 335 630 1161 2182 4080 7710 14532 27594))
(define (A001037 n) (vector-ref vecA001037 n))
(define vecA036378 (vector 0 1 1 2 2 5 7 13 23 43 75 137 255 464 872 1612 3030 5709 10749 20390))
(define (A036378 n) (vector-ref vecA036378 n))

(define (A010051 n) (if (prime? n) 1 0))

(define (A066247 n) (if (and (> n 3) (not (prime? n))) 1 0))

;; A000720: pi(n), the number of primes <= n.
;; i.e. partial sums of the corresponding characteristic function.
(definec (A000720 n)
   (cond ((< n 2) 0)
         (else (+ (A010051 n) (A000720 (- n 1))))
   )
)

(definec (A007097 n) (if (zero? n) 1 (A000040 (A007097 (- n 1)))))

;; A049084: 0 unless n is a prime p(k) when a(n) = k.
;; 0,1,2,0,3,0,4,0,0,0,5,0,6,0,0,0,7,0,8,0,0,0,9,0,0,0,0,0,10,

(define (A049084 n) (* (A000720 n) (A010051 n)))

(definec (A049084corrupt-check-everything n)
   (cond ((not (prime? n)) 0)
         (else
            (let loop ((i 1))
                  (cond ((= (A008578 i) n) i)
                        ((> i n)
                          (error "Error detected in A049084, index i " i
                                 "larger than n: " n)
                        )
                        (else (loop (1+ i)))
                  )
            )
         )
   )
)

;; tau(n): number of divisors of n.
(definec (A000005 n) (apply * (map 1+ (multiplicities (ifactor n)))))
(definec (A001221 n) (distinct-elems (ifactor n)))
(definec (A001222 n) (length (ifactor n)))

(define (A008683 n) ;; Moebius mu
    (cond ((= 1 n) n)
          ((not (= (A001221 n) (A001222 n))) 0) ;; Well, contains a square!
          (else (expt -1 (A001222 n)))
    )
)

;; Easy definition for Thue-Morse (parity) sequence:
(definec (A010060 n)
    (cond ((zero? n) 0)
          ((even? n) (A010060 (/ n 2)))
          (else (- 1 (A010060 (/ (- n 1) 2))))
    )
)

;; %S A061775 1,2,3,3,4,4,4,4,5,5,5,5,5,5,6,5,5,6,5,6,6,6,6,6,7,6,7,6,6,7,6,6,7,6,7,7,
;; %N A061775 Number of nodes in rooted tree with Goebel number n.

(definec (A061775 n)
    (cond ((<= n 1) n)
          ((= 1 (A010051 n)) (+ 1 (A061775 (A049084 n)))) ;; Planted tree.
          (else (fold-left + 1 (map -1+ (map A061775 (ifactor n))))) ;; Combination of trees.
    )
)


(define vecA000081 (vector 0 1 1 2 4 9 20 48 115 286 719 1842 4766 12486 32973 87811 235381))
(define (A000081 n) (vector-ref vecA000081 n))


;; This data taken directly from OEIS:
(define vecA005517 (vector 1 2 3 5 9 15 25 45 75 125 225 375 625 1125 1875 3125
                           5625 9375 15625 28125 46875 78125 140625 234375
                           390625 703125 1171875 1953125 3515625 5859375
                           9765625 17578125 29296875 48828125))

(define vecA005518 (vector 1 2 4 8 19 67 331 2221 19577 219613 3042161 50728129 
                           997525853 22742734291 592821132889 17461204521323))

(define (A005517 n) (vector-ref vecA005517 (- n 1)))
(define (A005518 n) (vector-ref vecA005518 (- n 1)))

;; We can also compute them, but it is extremely slow when done dumbly:
(definec (A005517v2 n)
   (cond
      ((<= n 1) n)
      (else
         (let loop ((i 0)) ;; Do it in dumb way!
            (cond ((= (A061775 i) n) i)
                  (else (loop (+ 1 i)))
            )
         )
      )
   )
)

(definec (A005518v2 n)
   (cond
      ((<= n 1) n)
      (else
         (let loop ((i 0) ;; Do it in dumb way!
                    (k-to-skip (- (A000081 n) 1))
                   )
            (cond ((= (A061775 i) n)
                      (if (zero? k-to-skip)
                          i
                          (loop (+ 1 i) (- k-to-skip 1))
                      )
                  )
                  (else (loop (+ 1 i) k-to-skip))
            )
         )
      )
   )
)

;; Note that (A091238 n) = (A061775 (A091205 n))
;; and vice versa, (A061775 n) = (A091238 (A091204 n))


(definec (A091238 n)
    (cond ((<= n 1) n)
          ((= 1 (A091225 n)) (+ 1 (A091238 (A091227 n)))) ;; Planted tree.
          (else (fold-left + 1 (map -1+ (map A091238 (GF2Xfactor n))))) ;; Combination of trees.
    )
)



(definec (A091239 n)
   (cond
      ((<= n 1) n)
      (else
         (let loop ((i 0)) ;; Do it in dumb way!
            (cond ((= (A091238 i) n) i)
                  (else (loop (+ 1 i)))
            )
         )
      )
   )
)

(definec (A091240 n)
   (cond
      ((<= n 1) n)
      (else
         (let loop ((i 0) ;; Do it in dumb way!
                    (k-to-skip (- (A000081 n) 1))
                   )
            (cond ((= (A091238 i) n)
                      (if (zero? k-to-skip)
                          i
                          (loop (+ 1 i) (- k-to-skip 1))
                      )
                  )
                  (else (loop (+ 1 i) k-to-skip))
            )
         )
      )
   )
)

;; Compare to A091233:
(define (A091241 n) (1+ (- (A091240 n) (A091239 n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;; GF2[X] related functions follow.                                        ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A091202: "Multiplicative" isomorphism from integers to GF2X-polynomials:
(definec (A091202 n)
   (cond ((< n 2) n)
         ((= 1 (A010051 n)) (A014580 (A049084 n)))
         (else (reduce A048720bi 1 (map A091202 (ifactor n))))
   )
)

;; A091203: "Multiplicative" isomorphism from GF2X-polynomials to integers:
(definec (A091203 n)
   (cond ((< n 2) n)
         ((= 1 (A091225 n)) (A000040 (A091227 n)))
         (else (reduce A004247bi 1 (map A091203 (GF2Xfactor n))))
   )
)

;; Here is another way to define A091202 & A091203
;; (Compare to the definitions of A106442v2 and A106443v2):
(definec (A091202v2 n)
  (Nvector->GF2Xfactorization! (explist->Nvector! (primefactorization->explist n)))
)

(definec (A091203v2 n)
  (Nvector->primefactorization! (explist->Nvector! (GF2Xfactorization->explist n)))
)

;; A091204: "Deeply multiplicative" isomorphism from integers to GF2X-polynomials:
(definec (A091204 n)
   (cond ((< n 3) n)
         ((= 1 (A010051 n)) (A014580 (A091204 (A049084 n))))
         (else (reduce A048720bi 1 (map A091204 (ifactor n))))
   )
)

;; A091205: "Deeply multiplicative" isomorphism from GF2X-polynomials to integers:
(definec (A091205 n)
   (cond ((< n 3) n)
         ((= 1 (A091225 n)) (A000040 (A091205 (A091227 n))))
         (else (reduce A004247bi 1 (map A091205 (GF2Xfactor n))))
   )
)





;; Data taken from OEIS: (Because this is somewhat slowish to compute.)
(define vecA007053 (vector 0 1 2 4 6 11 18 31 54 97 172 309 564 1028 1900 3512
                     6542 12251 23000 43390 82025 155611 295947 564163 1077871 
                     2063689 3957809 7603553 14630843 28192750 54400028 
                     105097565 203280221 393615806 762939111 1480206279 
                     2874398515 5586502348 10866266172 21151907950 41203088796 
                     80316571436 156661034233 305761713237 597116381732 
                     1166746786182 2280998753949 4461632979717 8731188863470 
                     17094432576778 33483379603407 65612899915304 
                     128625503610475))

(define vecA062692 (vector 0 2 3 5 8 14 23 41 71 127 226 412 747 1377 2538 4720
                     8800 16510 31042 58636 111013 210871 401428 766150 1465020
                     2807196 5387991 10358999 19945394 38458184 74248451 
                     143522117 277737797 538038783 1043325198))

(define (A007053 n) (vector-ref vecA007053 n))
(define (A062692 n) (vector-ref vecA062692 n))

(define (A007053v2 n) (A000720 (A000079 n)))
(define (A062692v2 n) (A091226 (A000225 (+ n 1))))



;; Intersect of A014580 & A000040.
;; Compare to A027697: Primes with odd number of 1's in their binary expansion. ???
;; (C.f. A027698).
;; Note that all elements of A014580 (except 3) seem to have an odd number of 1's in their binary expansion!
;; (thus also this and A091214)
(define (A091206 n) (A000040 (A091207 n)))

;; Positions of intersect of A014580 & A000040 in A000040:
;; One-based. Complement of A091210.
(definec (A091207 n)
   (cond
      ((<= n 1) n)
      (else
         (let loop ((i (+ 1 (A091207 (- n 1)))))
            (cond ((= (A091225 (A000040 i)) 1) i)
                  (else (loop (+ 1 i)))
            )
         )
      )
   )
)

;; Positions of intersect of A014580 & A000040 in A014580:
;; One-based. Complement of A091215.
(definec (A091208 n)
   (cond
      ((<= n 1) n)
      (else
         (let loop ((i (+ 1 (A091208 (- n 1)))))
            (cond ((= (A010051 (A014580 i)) 1) i)
                  (else (loop (+ 1 i)))
            )
         )
      )
   )
)


;; Set-wise difference of A000040 & A014580 i.e. intersect of A000040 & A091242
;; (i.e. those primes which are composite when inteprereted as GF2[X] polynomials).
(define (A091209 n) (A000040 (A091210 n)))

;; Positions of A091209 in A000040:
;; One-based. Complement of A091207.
(definec (A091210 n)
   (cond
      ((= 1 n) 3) ;; p3 = 5 is the first prime, which when intepreted as 101 = x^2 + 1 = (x+1)^2 is not irreducible.
      (else
         (let loop ((i (+ 1 (A091210 (- n 1)))))
            (cond ((= (A091225 (A000040 i)) 0) i)
                  (else (loop (+ 1 i)))
            )
         )
      )
   )
)

;; Positions of A091209 in A091242:
;; One-based. Complement of A091213.
(definec (A091211 n)
   (cond
      ((= 1 n) 2) ;; position of 5 in A091242 is 2.
      (else
         (let loop ((i (+ 1 (A091211 (- n 1)))))
            (cond ((= (A010051 (A091242 i)) 1) i)
                  (else (loop (+ 1 i)))
            )
         )
      )
   )
)


;; Intersect of A091242 & A002808. I.e. composites in both domains.
(define (A091212 n) (A091242 (A091213 n)))

;; Its complement:
(define A257688 (MATCHING-POS 1 1 (lambda (n) (or (= 1 n) (= 1 (A091225 n)) (= 1 (A010051 n))))))

(definec (A255574 n) (if (zero? n) n (+ (A257000 n) (A255574 (- n 1))))) ;; Inverse for A206074.

;; Positions of intersect of A091242 & A002808 in A091242:
;; One-based. Complement of A091211.
(definec (A091213 n)
   (cond
      ((<= n 1) n) ;; 4 is the first composite, in both.
      (else
         (let loop ((i (+ 1 (A091213 (- n 1)))))
            (cond ((= (A066247 (A091242 i)) 1) i)
                  (else (loop (+ 1 i)))
            )
         )
      )
   )
)



;; Set-wise difference of A014580 & A000040 i.e. intersect of A014580 & A002808
;; (i.e. those irreducible GF2[X] polynomials which are composites in N).
(define (A091214 n) (A014580 (A091215 n)))

;; Positions of A091214 in A014580:
;; One-based. Complement of A091208.
(definec (A091215 n)
   (cond
      ((= 1 n) 7) ;; A014580(7) = 25 is the first irreducible which is not prime.
      (else
         (let loop ((i (+ 1 (A091215 (- n 1)))))
            (cond ((= (A066247 (A014580 i)) 1) i)
                  (else (loop (+ 1 i)))
            )
         )
      )
   )
)

(define (A091219 n) ;; Moebius mu
    (cond ((not (= (A091221 n) (A091222 n))) 0) ;; Well, contains a square!
          (else (expt -1 (A091222 n)))
    )
)

;; A091220 is analogous to A000005: tau(n): number of divisors of n.

(definec (A091220 n) (apply * (map 1+ (multiplicities (GF2Xfactor n)))))

;; A091221 is analogous to A001221: Number of distinct primes dividing n.
;; (omega(n))
;; A091222 is analogous to A001222: Number of prime divisors of n
;; (with multiplicity). Also called (bigomega(n) or Omega(n)).
;; A091223 is analogous to A001223: Differences between consecutive primes.
;; i.e. the first differences of A014580.

(define (A091221 n) (distinct-elems (GF2Xfactor n)))
(define (A091222 n) (length (GF2Xfactor n)))


(definec (A091223 n)
   (cond ((= n 1) n)
      (else
         (let loop ((i (1+ (A014580 n))))
            (cond ((= 1 (A091225 i)) (- i (A014580 n)))
                  (else (loop (+ 1 i)))
            )
         )
      )
   )
)

;; A091224 is analogous to A028334: Differences between consecutive primes,
;; divided by 2. Starts from offset 2:

(define (A091224 n) (/ (A091223 n) 2))

;; A014580 is analogous to A000040 (primes). I think it should be 1-based!
;; 2,3,7,11,13,19,25,31,37,41,47,55,59,61,67,73,87,91,97,103,

;; Note: (a014580 4720) = 65533


(definec (A014580 n)
   (cond ((<= n 2) (+ 1 n))
         (else (+ (A014580 (- n 1)) (A091223 (- n 1))))
   )
)


;; A091225 is analogous to A010051:
;; Characteristic function of primes: 1 if n is prime else 0.

(define (A091225 n)
   (if (= 1 (length (GF2Xfactor n))) 1 0)
)

;; A091226 is analogous to A000720: pi(n), the number of primes <= n.
;; i.e. partial sums of the corresponding characteristic function.
(definec (A091226 n)
   (cond ((< n 2) 0)
         (else (+ (A091225 n) (A091226 (- n 1))))
   )
)


;; A091227 is analogous to A049084: 0 unless n is a prime p(k) when a(n) = k.
(definec (A091227 n) (* (A091225 n) (A091226 n)))

;; A091228 is analogous to A007918 Smallest prime >= n. I.e. gives the next irreducible n.
(definec (A091228 n)
   (cond
      ((<= n 2) 2)
      ((>= (A091228 (- n 1)) n) (A091228 (- n 1))) ;; Optimizes as this is cached function.
      (else ;; Find next irreducible one.
         (let loop ((i n))
            (cond ((= 1 (A091225 i)) i)
                  (else (loop (+ 1 i)))
            )
         )
      )
   )
)


;; A091229 is analogous to A007920 Smallest k such that n+k is prime. (Smarandache complementary prime function) a(n) = A007918(n) - n.

(define (A091229 n) (- (A091228 n) n))

;; An analog for the primeth recurrence:
(definec (A091230 n) (if (zero? n) 1 (A014580 (A091230 (- n 1)))))


;; How many more primes there are in range [0,2^n] in total than GF(2)[X]-irreducibles:
;; (0 0 0 1 1 3 4 8 13 26 45)
;; Compute also the first differences.
(define (A091231 n)
    (if (< n 2)
        0
        (- (A007053 n) (A062692 (- n 1)))
    )
)

(define (A091231v2 n)
    (if (< n 2)
        0
        (- (A007053v2 n) (A062692v2 (- n 1)))
    )
)

;; There are saner ways to count this! Use A001037
;;   A001037 1,2,1,2,3,6,9,18,30,56,99,186,335,630,1161,2182,4080,7710,14532,27594, (offset 0)
;; A036378 = 1,1,2,2,5,7,13,23,43,75,137,255,464,872,1612,3030,5709,10749,20390, (offset 1.)

;; (0 0 1 0 2 1 4 5 13 19 38)
;; How many more primes there are in range [2^n,2^(n+1)] than GF(2)[X]-irreducibles:
;; Offset 0.


(define (A091232 n) (- (A091231 (+ 1 n)) (A091231 n))) ;; Upto n=34

(define (A091232v2 n) (if (< n 2) 0 (- (A036378 (+ n 1)) (A001037 n)))) ;; upto n=18.


;; Compare to A091241:
(define (A091233 n) (1+ (- (A005518 n) (A005517 n))))

;; A091242 is analogous to A002808 (composites). I think it should be 1-based!

(definec (A091242 n)
   (cond ((= n 1) 4)
         (else (+ (A091242 (- n 1)) (A091243 (- n 1))))
   )
)


;; Both offset=1:
;; A091243 is analogous to A073783:      Differences between composite numbers.

(definec (A091243 n)
   (cond ((= n 1) 1)
      (else
         (let loop ((i (1+ (A091242 n))))
            (cond ((= 1 (A091247 i)) (- i (A091242 n)))
                  (else (loop (+ 1 i)))
            )
         )
      )
   )
)

;; A091244 is analogous to A073784: Number of primes between successive composites.

(define (A091244 n) (- (A091243 n) 1))

;; A091245 is analogous to A065855: number of composites <= n.
;; i.e. partial sums of the corresponding characteristic function.
(definec (A091245 n)
   (cond ((< n 4) 0)
         (else (+ (A091247 n) (A091245 (- n 1))))
   )
)

;; A091246 is analogous to A066246: 0 unless n is a composite, otherwise its index.
(definec (A091246 n) (* (A091247 n) (A091245 n)))


;; A091247 is analogous to A066247 (characteristic function of composites)
(define (A091247 n) (if (and (> n 3) (zero? (A091225 n))) 1 0))



;; Here we are computing the primitive polynomials in very straightforward way,
;; as adapted from: http://mathworld.wolfram.com/PrimitivePolynomial.html
;; and: http://mathworld.wolfram.com/PolynomialOrder.html

;; There are probably cleverer ways of computing these,
;; see e.g.
;; http://www.theory.csc.uvic.ca/~cos/inf/neck/PolyInfo.html




(define (A000374 n) (distinct-elems (Xn_1factor n)))
(define (A091248 n) (length (Xn_1factor n)))

;; The first x^n + 1 polynomial where the nth irreducible GF(2)[X] -polynomial
;; is found as a factor, 0 if never.
;; In OEIS given with name:
;; Arrange irreducible polynomials over GF(2) in lexicographic order
;; and write down the order of each polynomial.
;; with a(1)=1, instead of 0.

(definec (A059478 n)
  (let ((p (A014580 n)))
     (cond ((= 1 n) 1)
           (else (let loop ((i 1))
                   (if (member p (Xn_1factor i))
                       i
                       (loop (+ 1 i))
                   )
                 )
           )
     )
  )
)

;; Give the positions of primitive GF(2)[X]-polynomials in A014580:

;; (map A014580 (iota 10))               --> (2 3 7 11 13 19 25 31 37 41)
;; (map A059478 (iota 10))               --> (1 1 3  7  7 15 15  5 31 31)
;; (map A000523 (map A014580 (iota 10))) --> (1 1 2  3  3  4  4  4  5 5)

(definec (A091249 n)
   (cond
      ((= n 1) 2) ;; The first primitive is A014580[2] = 3 = x+1.
      (else
         (let loop ((i (+ 1 (A091249 (- n 1)))))
            (cond ((equal? (A059478 i) (A000225 (A000523 (A014580 i)))) i)
                  (else (loop (+ 1 i)))
            )
         )
      )
   )
)

(define (A091250 n) (A014580 (A091249 n)))

(define (A058947 n) (A007088 (A091250 n)))

;; A058947 = A007088(A091250(n))

;; Give the positions of non-primitive GF(2)[X]-polynomials in A014580:

(definec (A091251 n)
   (cond
      ((= n 1) 1) ;; The first non-primitive is A014580[1] = 2 = x.
      (else
         (let loop ((i (+ 1 (A091251 (- n 1)))))
            (cond ((not (equal? (A059478 i) (A000225 (A000523 (A014580 i))))) i)
                  (else (loop (+ 1 i)))
            )
         )
      )
   )
)

(define (A091252 n) (A014580 (A091251 n)))


(define (A091253 n) (A007088 (A091252 n)))
(define (A091254 n) (A007088 (A091242 n)))



;; 

(define A004247bi *)
(define (A004247 n) (* (A002262 n) (A025581 n)))

;; A048720 is analogous to A004247 (the ordinary multiplication table):
;; Let's use Henry Bottomley's formula:
;; T(2b,c)=T(c,2b)=T(b,2c)=2T(b,c); T(2b+1,c)=T(c,2b+1)=2T(b,c) XOR c

(define (A048720bi x y)
   (cond ((zero? x) 0)
         ((zero? y) 0)
         ((even? x) (* 2 (A048720bi (/ x 2) y)))
         (else (A003987bi (* 2 (A048720bi (/ (- x 1) 2) y)) y))
   )
)

(define (A048720 n) (A048720bi (A002262 n) (A025581 n)))

;; Horribly unoptimized:
(define (A048723bi x y) ;; x is the base, y the exponent.
   (cond ((zero? y) 1)
         (else (A048720bi x (A048723bi x (- y 1))))
   )
)

(define (A048723 n) (A048723bi (A002262 n) (A025581 n)))

;; Note the different indexing (here offset=1) from OEIS (there, offset=2):
(define (A003057 n) (+ (1+ (A002262 (-1+ n))) (1+ (A025581 (-1+ n)))))

;; Slightly differently defined in intfuns1.scm:
(define (A003989 n) (gcd (1+ (A002262 (-1+ n))) (1+ (A025581 (-1+ n)))))

;; One-based variant of A004247:
(define (A003991 n) (A004247bi (1+ (A002262 (-1+ n))) (1+ (A025581 (-1+ n)))))

(define (A106448bi x y) (/ (+ x y) (gcd x y)))
(define (A106448 n) (A106448bi (1+ (A002262 (-1+ n))) (1+ (A025581 (-1+ n)))))
(define (A106448v2 n) (/ (A003057 n) (A003989 n))) ;; Beware with the indices!

(define (A106449bi x y) (GF2Xdivide (A003987bi x y) (A091255bi x y)))
(define (A106449 n) (A106449bi (1+ (A002262 (-1+ n))) (1+ (A025581 (-1+ n)))))

(define (A106450 x) (A106449bi x 2))



;; Analogous to A003991:
;; One-based variant of A048720, that's why the calisthenics with the indices:
(define (A091257 n) (A048720bi (1+ (A002262 (-1+ n))) (1+ (A025581 (-1+ n)))))

(define (A001317 n)
  (fold-left a048720bi 1 (map (lambda (x) 3) (iota n)))
)


;; Divide the polynomial x with the polynomial y.
;; Return zero, if x is zero, otherwise raise an
;; error if y does not divide x:
(define (GF2Xdivide x y)
  (if (zero? x)
      0
      (fold-left a048720bi 1 (m-difference (GF2Xfactor x) (GF2Xfactor y)))
  )
)

;; Analogous to A003989, GCD.
(define (A091255bi x y)
   (fold-left a048720bi 1 (m-intersect (GF2Xfactor x) (GF2Xfactor y)))
)

;; Analogous to A003990, LCM.
(define (A091256bi x y)
   (fold-left a048720bi 1 (m-union (GF2Xfactor x) (GF2Xfactor y)))
)

;; Note that it is one-based, that's why the calisthenics with the indices:
(define (A091255 n) (A091255bi (1+ (A002262 (-1+ n))) (1+ (A025581 (-1+ n)))))

(define (A091256 n) (A091256bi (1+ (A002262 (-1+ n))) (1+ (A025581 (-1+ n)))))

;; One way to check that we are doing it right:
;; X*Y = GCD(X,Y)*LCM(X,Y)
;; (equal? (map A091257v2 (iota 2048)) (map A091257 (iota 2048))) --> #t

(define (A091257v2 n) (A048720bi (A091255 n) (A091256 n)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;; A075161 - A075167 - Relating to Catalan structures                      ;;
;; (e.g. general plane  trees) represented with Awbreyesque                ;;
;; "index-on-prime factorization technique".                               ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;
;; First all the machinery for unranking & ranking Catalan Structures,
;; in the standard lexical (A014486)-order.
;; See http://www.iki.fi/~kartturi/matikka/tab9766.htm
;; to understand this.
;;


(define (self-convolve fun upto_n)
  (+ (if (zero? (modulo upto_n 2)) (expt (fun (/ upto_n 2)) 2) 0)
     (* 2
        (let loop ((i 0) (j upto_n))
          (if (>= i j)
              0
              (+ (* (fun i) (fun j))
                 (loop (1+ i) (-1+ j))
              )
          )
        )
     )
  )
)

(definec (A000108 n) ;; Catalan's numbers.
   (if (zero? n)
       1
       (self-convolve A000108 (-1+ n))
   )
)

(definec (A014137 n) ;; Partial sums of A000108: 1,2,4,9,23,65,197,...
   (if (zero? n)
       1
       (+ (A014137 (-1+ n)) (A000108 n))
   )
)

(define (A014138 n) (-1+ (A014137 (1+ n))))

;; (map A072643 (iota0 23)) ;; n occurs (A000108 n) times.
;; --> (0 1 2 2 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5)

(define (A072643 n) (first_pos_with_funs_val_gte A014137 (1+ n))) ;; Was ranks_w/2

;; (map A014486 (cons 0 (iota 23)))
;; --> (0 2 10 12 42 44 50 52 56 170 172 178 180 184 202 204 210 212 216 226 228 232 240 682)

(definec (A014486 n)
   (let ((w/2 (A072643 n)))
      (CatalanUnrank w/2 (if (zero? n) 0 (- n (A014137 (-1+ w/2)))))
   )
)




;; Here we implement Frank Ruskey´s unranking algorithm at
;; http://www.cs.uvic.ca/~fruskey/Publications/Thesis/ThesisPage22.png
;; Note that there the loop condition is given erroneously as m > 0,
;; while it should be m >= 0, i.e. we stop only when m goes negative.

;; Constructs the totally balanced binary string a
;; from the left (msb) to the right (lsb):
 
(define (CatalanUnrank size rank)
  (let loop ((a 0)
             (m (-1+ size))        ;; The row on A009766
             (y size)              ;; The position on row m of A009766
             (rank rank)
             (c (CatTriangle (-1+ size) size))
            )
      (if (negative? m) a
          (if (>= rank c)
              (loop (1+ (* 2 a))   ;; Up the mountain high
                    m
                    (-1+ y)
                    (- rank c)
                    (CatTriangle m (-1+ y))
              )
              (loop (* 2 a)        ;; Down to the valley low
                    (-1+ m)
                    y
                    rank
                    (CatTriangle (-1+ m) y)
              )
          )
      )
  )
)

;; Rank a symbolless S-expression directly.
;; See Frank Ruskey's thesis at:
;; http://www.cs.uvic.ca/~fruskey/Publications/Thesis/Thesis.html
;; especially the page 19:
;; http://www.cs.uvic.ca/~fruskey/Publications/Thesis/ThesisPage19.png

(define (CatalanRankSexpAux size node)
  (let ((m (-1+ size))        ;; The row on A009766
        (y size)              ;; The position on row m of A009766
        (rank 0)
       )
    (let TreeRank ((node node))
      (cond ((not (pair? node)) (set! m (-1+ m)))
            (else
                  (set! rank (+ rank (CatTriangle m y)))
                  (set! y (-1+ y))
                  (TreeRank (car node))
                  (TreeRank (cdr node))
            )
      )
    )
    rank
  )
)

(define (count-pars a)
    (cond ((not (pair? a)) 0)
          (else (+ 1 (count-pars (car a)) (count-pars (cdr a))))
    )
)

(define (CatalanRankSexp s)
    (let ((size (count-pars s)))
       (if (zero? size)
           0
           (+ (A014137 (-1+ size)) (CatalanRankSexpAux size s))
       )
    )
)


;; (parenthesization->binexp '())      --> 0
;; (parenthesization->binexp '(()))    --> 2
;; (parenthesization->binexp '(() ())) --> 10
;; (parenthesization->binexp '((())))  --> 12

(define (parenthesization->binexp p)
   (let loop ((s 0) (p p))
      (if (null? p)
          s
          (let* ((x (parenthesization->binexp (car p)))
                 (w (binwidth x))
                )
             (loop
                   (+ (* s (expt 2 (+ w 2))) (expt 2 (1+ w)) (* 2 x))
                   (cdr p)
             )
          )
      )
   )
)

;; The inverse function of above:
(define (binexp->parenthesization n)
   (let loop ((n n) (stack (list (list))))
       (cond ((zero? n) (car stack))
             ((zero? (modulo n 2))
                (loop (floor->exact (/ n 2)) (cons (list) stack))
             )
             (else
                (loop (floor->exact (/ n 2)) (cons2top! stack))
             )
       )
   )
)

;; Needs this:
;; Convert (a . (b . rest)) --> ((a . b) . rest)
;; with no cons cells wasted.
(define (cons2top! stack)
  (let ((ex-cdr (cdr stack)))
      (set-cdr! stack (car ex-cdr))
      (set-car! ex-cdr stack)
      ex-cdr
  )
)


(define A001477yet_another_version9
  (compose-funs CatalanRankSexp binexp->parenthesization A014486)
)


(define (cons-n-times n what lista)
   (cond ((zero? n) lista)
         (else (cons-n-times (-1+ n) what (cons what lista)))
   )
)



(define (generic-factorization->explist factors facts-ind-fun)
      (let loop ((factors factors) (pf 1) (el (list)))
         (cond ((null? factors) el)
               ((= (car factors) pf)
                   (set-car! el (1+ (car el)))
                   (loop (cdr factors) (car factors) el)
               )
               (else
                   (loop (cdr factors)
                         (car factors)
                         (cons 1
                               (cons-n-times
                                         (-1+ (- (facts-ind-fun (car factors))
                                                 (facts-ind-fun pf)
                                              )
                                         )
                                         0
                                         el
                               )
                         )
                   )
               )
         )
      )
)

;; (primefactorization->explist 1) -->  ()
;; (primefactorization->explist 2) -->  (1)
;; (primefactorization->explist 3) -->  (1 0)
;; (primefactorization->explist 4) -->  (2)
;; (primefactorization->explist 5) -->  (1 0 0)
;; (primefactorization->explist 6) -->  (1 1)
;; (primefactorization->explist 7) -->  (1 0 0 0)
;; (primefactorization->explist 8) -->  (3)
;; (primefactorization->explist 9) -->  (2 0)
;; (primefactorization->explist 10) --> (1 0 1)
;; (primefactorization->explist 11) --> (1 0 0 0 0)
;; (primefactorization->explist 12) --> (1 2)

(define (primefactorization->explist n)
  (if (= 1 n) (list) (generic-factorization->explist (ifactor n) A049084))
)

;; (GF2Xfactorization->explist 1) -->  ()
;; (GF2Xfactorization->explist 2) -->  (1)
;; (GF2Xfactorization->explist 3) -->  (1 0)
;; (GF2Xfactorization->explist 4) -->  (2)
;; (GF2Xfactorization->explist 5) -->  (2 0)
;; (GF2Xfactorization->explist 6) -->  (1 1)
;; (GF2Xfactorization->explist 7) -->  (1 0 0)
;; (GF2Xfactorization->explist 8) -->  (3)
;; (GF2Xfactorization->explist 9) -->  (1 1 0)
;; (GF2Xfactorization->explist 10) --> (2 1)
;; (GF2Xfactorization->explist 11) --> (1 0 0 0)
;; (GF2Xfactorization->explist 12) --> (1 2)

(define (GF2Xfactorization->explist n)
  (if (= 1 n) (list) (generic-factorization->explist (GF2Xfactor n) A091227))
)

(define (apply-to-the-rest! el what)
   (cond
     ((pair? el)
       (let loop ((el (cdr el)))
          (cond ((pair? el)
                   (set-car! el (what (car el)))
                   (loop (cdr el))
                )
          )
       )
     )
   )
   el
)

;; Just increment the tail elems by +1.
(define (explist->Nvector! el) (apply-to-the-rest! el 1+))

;; Just decrement the tail elems by 1.
(define (Nvector->explist! el) (apply-to-the-rest! el -1+))


;; Mapping from exponent lists to list structures:
;;
;;  1 -> ()        -> ()       -> ()
;;  2 -> (1)       -> (1)      -> (())
;;  3 -> (1 0)     -> (1 1)    -> (() ())
;;  4 -> (2)       -> (2)      -> ((()))
;;  5 -> (1 0 0)   -> (1 1 1)  -> (() () ()) 
;;  6 -> (1 1)     -> (1 2)    -> (() (()))
;;  7 -> (1 0 0 0) -> (1 1 1 1)-> (() () () ())
;;  8 -> (3)       -> (3)      -> ((() ()))
;;  9 -> (2 0)     -> (2 1)    -> ((()) ())
;; 10 -> (1 0 1)   -> (1 1 2)  -> (() () (()))

(define (primefactorization->parenthesization n)
  (map primefactorization->parenthesization
       (explist->Nvector! (primefactorization->explist n))
  )
)

(define (GF2Xfactorization->parenthesization n)
  (map GF2Xfactorization->parenthesization
       (explist->Nvector! (GF2Xfactorization->explist n))
  )
)

(define (parenthesization->primefactorization p)
  (Nvector->primefactorization! (map parenthesization->primefactorization p))
)

(define (parenthesization->GF2Xfactorization p)
  (Nvector->GF2Xfactorization! (map parenthesization->GF2Xfactorization p))
)


(define (explist->generic-factorization! el primefun mulfun expfun)
  (let loop ((el (reverse! el)) (i 1) (z 1))
    (cond ((null? el) z)
          (else
            (loop (cdr el)
                  (+ i 1)
                  (mulfun (expfun (primefun i) (car el)) z)
            )
          )
    )
  )
)


(define (explist->primefactorization! el)
    (explist->generic-factorization! el A000040 * expt)
)

(define (explist->GF2Xfactorization! el)
    (explist->generic-factorization! el A014580 A048720bi A048723bi)
)


(define (Nvector->primefactorization! el)
    (explist->primefactorization! (Nvector->explist! el))
)

(define (Nvector->GF2Xfactorization! el)
    (explist->GF2Xfactorization! (Nvector->explist! el))
)

(define (multiply-generic-expfactor-list base-explist mulfun expfun)
  (reduce mulfun 1 (map (lambda (p) (expfun (car p) (cdr p))) base-explist))
)

;; The inverse-function of prime-expfactor:
(define (multiply-expfactor-list-in-N base-explist)
  (multiply-generic-expfactor-list base-explist * expt)
)

;; The inverse-function of GF2X-expfactor:
(define (multiply-expfactor-list-in-GF2X base-explist)
  (multiply-generic-expfactor-list base-explist A048720bi A048723bi)
)


;; Starts with offset 0.
(define (A075161 n)
  (CatalanRankSexp (primefactorization->parenthesization (1+ n)))
)

(define (A075162 n)
   (-1+ (parenthesization->primefactorization
          (binexp->parenthesization (A014486 n))
        )
   )
)

;; Starts with offset 1
(define (A075163 n) (1+ (A075161 (-1+ n))))
(define (A075164 n) (1+ (A075162 (-1+ n))))

;; Starts with offset 1
(define (A075165 n)
   (parenthesization->binexp (primefactorization->parenthesization n))
)

;; O=1
(define (A075166 n)
   (A007088 (parenthesization->binexp (primefactorization->parenthesization n)))
)

;; O=1
(define (A075167 n)
 (halve (binwidth (parenthesization->binexp (primefactorization->parenthesization n))))
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;; Similar functions using GF(2)[X]-factorization, and permutations        ;;
;; giving isomorphisms between these two different representations.        ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A106442 is in a certain way analogous to A091202:
(definec (A106442v2 n)
   (cond ((< n 2) n)
;;       ((= 1 (A010051 n)) (A014580 (A049084 n))) ;; Obeys this!
         (else (parenthesization->GF2Xfactorization
                     (primefactorization->parenthesization n)
               )
         )
   )
)

;; A106443 is in a certain way analogous to A091203:
(definec (A106443v2 n)
   (cond ((< n 2) n)
;;       ((= 1 (A091225 n)) (A000040 (A091227 n))) ;; Obeys this!
         (else (parenthesization->primefactorization
                     (GF2Xfactorization->parenthesization n)
               )
         )
   )
)


;; Compare to the definitions of A091202v2 and A091203v2:
(definec (A106442 n)
  (Nvector->GF2Xfactorization! (map A106442 (explist->Nvector! (primefactorization->explist n))))
)

(definec (A106443 n)
  (Nvector->primefactorization! (map A106443 (explist->Nvector! (GF2Xfactorization->explist n))))
)

;; Compare to the definitions of A091202v2 and A091203v2:
(definec (A106444 n)
  (if (zero? n) n
    (explist->GF2Xfactorization! (map A106444 (primefactorization->explist n)))
  )
)

(definec (A106445 n)
  (if (zero? n) n
    (explist->primefactorization! (map A106445 (GF2Xfactorization->explist n)))
  )
)

(definec (A106444v2 n)
  (if (<= n 1) n
    (multiply-expfactor-list-in-GF2X
      (map (lambda (p) (cons (A014580 (A049084 (car p))) (A106444v2 (cdr p))))
           (prime-expfactor n)
      )
    )
  )
)

(definec (A106445v2 n)
  (if (<= n 1) n
    (multiply-expfactor-list-in-N
      (map (lambda (p) (cons (A000040 (A091227 (car p))) (A106445v2 (cdr p))))
           (GF2X-expfactor n)
      )
    )
  )
)


;; Combines the "deepness" of both A091204 & A106444.
(definec (A106446 n)
  (if (<= n 1) n
    (multiply-expfactor-list-in-GF2X
          (map (lambda (p)
                  (cons (A014580 (A106446 (A049084 (car p)))) (A106446 (cdr p)))
               )
               (prime-expfactor n)
          )
    )
  )
)

;; Combines the "deepness" of both A091205 & A106445.
(definec (A106447 n)
  (if (<= n 1) n
    (multiply-expfactor-list-in-N
          (map (lambda (p)
                  (cons (A000040 (A106447 (A091227 (car p)))) (A106447 (cdr p)))
               )
               (GF2X-expfactor n)
          )
    )
  )
)





;; Starts with offset 0.
(define (A106451 n)
  (CatalanRankSexp (GF2Xfactorization->parenthesization (1+ n)))
)

(define (A106452 n)
   (-1+ (parenthesization->GF2Xfactorization
          (binexp->parenthesization (A014486 n))
        )
   )
)

;; Starts with offset 1
(define (A106453 n) (1+ (A106451 (-1+ n))))
(define (A106454 n) (1+ (A106452 (-1+ n))))

;; Starts with offset 1
(define (A106455 n)
   (parenthesization->binexp (GF2Xfactorization->parenthesization n))
)

;; O=1
(define (A106456 n)
   (A007088 (parenthesization->binexp (GF2Xfactorization->parenthesization n)))
)

;; O=1
(define (A106457 n)
 (halve (binwidth (parenthesization->binexp (GF2Xfactorization->parenthesization n))))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;::;;
;;                                                                           ;;
;; See Leroy Quet's mail on SeqFan-mailing list,                             ;;
;; with Subject: 'Super-Factoring' An Integer                                ;;
;; posted on Sat, 6 Dec 03 19:03:43 -0700                                    ;;
;; Message-Id: <E1ASoIn-0006VN-00@mclean.mail.mindspring.net>                ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (GenericSFseq n prweightfun factorfun)
  (let ((factors (factorfun n)))
    (cond ((<= n 1) (prweightfun n))
          (else (+ (apply + (map prweightfun (uniq factors)))
                   (apply + (map (lambda (x) (GenericSFseq x prweightfun factorfun))
                                 (multiplicities factors)
                            )
                   )
                )
          )
    )
  )
)


(define (QuetsSFseq n prweightfun) (GenericSFseq n prweightfun ifactor))

(define (GF2X-SFseq n prweightfun) (GenericSFseq n prweightfun GF2Xfactor))


;; a(p) = 1, C(1) = 0: 0, 1, 1, 2, 1, 2, 1, 2, 2, 2, 1, 3, 1, 2, 2, 3
;; Is neither A038548 nor A068108
(definec (A106490 n) (QuetsSFseq n A010051))

;; a(p) = 1, C(1) = 1: 1, 2, 2, 3, 2, 4, 2, 3, 3, 4, 2, 5, 2, 4, 4 ,4
(definec (A106491 n) (QuetsSFseq n (lambda (n) (if (< n 2) n (A010051 n)))))

;; a(p) = p, C(1) = 0: 0, 2, 3, 4, 5, 5, 7, 5, 5, 7, 11, 7, 13, 9, 8, 6
(definec (A106492 n) (QuetsSFseq n (lambda (n) (if (< n 2) 0 n))))

;; Note that the following one given by Leroy Quet:
;; a(p) = ln(p), C(0) = 0: exp(C(m)) -> 1, 2, 3, 4, 5, 6, 7, 6, 6, 10, 11, 12, 13, 14, 15, 8
;; seems to match A000026 "Mosaic numbers or multiplicative projection of n."
;; which has a formula:
;; n = Product (p_j^k_j) -> a(n) = Product (p_j * k_j).
;; and is multiplicative with a(p^e) = p*e.

;; This one is not implemented here:
;; a(p) = C(pi(p)), C(0) = 1: (pi(p) = order of prime p)
;; 1, 2, 3, 4, 4, 7, 5, 5, 5, 8, 6, 8
;; (And neither it is found in OEIS, in May 2005.)



;; A064372: Additive function a(n) defined by the recursive formula a(1)=1 and
;; a(p^k)=a(k) for any prime p.
;; Comments:  Starts almost the same as A001221 (the number of distinct primes
;;            dividing n): the first twelve terms which are different are a(1),
;;            a(64), a(192), a(320), a(448), a(576), a(704), a(729), a(832),
;;            a(960), a(1024), and a(1088), since the first non-unitary values of n
;;            are a(6) and(10). - Henry Bottomley (se16(AT)btinternet.com), Sep 23
;;            2002
;;


(definec (A064372 n)
  (if (<= n 1) n (apply + (map A064372 (primefactorization->explist n))))
)

(definec (A064372v2 n) (- (A106491 n) (A106490 n)))


;; Analogous to A106490:
(definec (A106493 n) (GF2X-SFseq n A091225))

;; Analogous to A106491:
(definec (A106494 n) (GF2X-SFseq n (lambda (n) (if (< n 2) n (A091225 n)))))

;; Analogous to A064372:
(definec (A106495 n)
  (if (<= n 1) n (apply + (map A106495 (GF2Xfactorization->explist n))))
)

(definec (A106495v2 n) (- (A106494 n) (A106493 n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Experimentation...                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mult_and_Xmult p q upto_n)
   (let loop ((lista (list))
              (i 0)
             )
      (cond ((> i upto_n) (reverse! lista))
            ((= (* p i) (A048720bi q i))
                (loop (cons i lista) (+ i 1))
            )
            (else (loop lista (+ i 1)))
      )

   )
)


(define (mult_and_Xmult_first_nonzero p q upto_n)
   (let loop ((i 1))
      (cond ((> i upto_n) 0)
            ((= (* p i) (A048720bi q i)) i)
            (else (loop (+ i 1)))
      )

   )
)


(define (mult_and_Xmult_nth_nonzero p q n upto_n)
   (let loop ((i 1) (n n))
      (cond ((> i upto_n) 0)
            ((= (* p i) (A048720bi q i))
                    (if (= 1 n)
                        i
                        (loop (+ i 1) (- n 1))
                    )
            )
            (else (loop (+ i 1) n))
      )
   )
)

(define (pn_XOR_qn_equ_rn p q r upto_n)
   (let loop ((lista (list))
              (i 0)
             )
      (cond ((> i upto_n) (reverse! lista))
            ((= (A003987bi (* p i) (* q i)) (* r i))
                (loop (cons i lista) (+ i 1))
            )
            (else (loop lista (+ i 1)))
      )

   )
)

;; Optimization: Return 0 immediately for all n > 3, and also for all powers of 2.
(define (find_smallest_GF2X_multiplier n uplimit uplimit2)
  (if (or (< n 3) (zero? (A004198bi n (- n 1))))
      0
      (let loop ((i (+ 1 n)))
         (cond ((> i uplimit) 0) ;; Not found, return zero.
               ((not (zero? (mult_and_Xmult_first_nonzero n i uplimit2)))
                  (format #t "A115857(~a)=~a\n" n i)
                  i
               )
               (else (loop (+ i 1)))
         )

      )
  )
)

;; Optimization: Return 0 immediately for all n > 3, and also for all powers of 2.
;; "some" e.g. returns 29 for n = 13, not the smallest, which is 21.
(define (find_some_nontrivial_comultiplier n uplimit uplimit2)
  (if (or (< n 3) (zero? (A004198bi n (- n 1))))
      0
      (let outloop ((i 1))
        (cond
           ((> i uplimit2) 0) ;; Not found, return zero.
           (else (let inloop ((j (+ 1 n)))
                    (cond ((> j uplimit) (outloop (+ 1 i)))
                          ((= (* n i) (A048720bi j i)) j)
                          (else (inloop (+ j 1)))
                    )
                 )
           )
        )
      )
  )
)


(define (find_largest_N_multiplier n uplimit2)
  (if (or (< n 3) (zero? (A004198bi n (- n 1))))
      0
      (let loop ((i (- n 1)))
         (cond ((zero? i) 0) ;; Not found, return zero.
               ((not (zero? (mult_and_Xmult_first_nonzero i n uplimit2)))
                  (format #t "A115859(~a)=~a\n" n i)
                  i
               )
               (else (loop (- i 1)))
         )
      )
  )
)


(define (find_all_N_multipliers n uplimit2)
  (if (or (< n 3) (zero? (A004198bi n (- n 1))))
      (list n)
      (let loop ((i (- n 0))
                 (lista ())
                )
         (cond ((zero? i)
                  (format #t "find_all_N_multipliers(~a)=~a\n" n lista)
                  (reverse! lista) ;; At the bottom, return the results
               )
               ((not (zero? (mult_and_Xmult_first_nonzero i n uplimit2)))
                  (loop (- i 1) (cons i lista))
               )
               (else (loop (- i 1) lista))
         )
      )
  )
)

(definec (find_all_N_multipliers_262144 n) (find_all_N_multipliers n 262144))

;; Here are the 5 A-numbers: A115857 --- A115861.
;; Here are the 10 A-numbers you requested: A115868 --- A115877.

(definec (A115857 n)
   (if (even? n)
       (* 2 (A115857 (/ n 2)))
       (find_smallest_GF2X_multiplier n 1024 262144) ;; 2097152
   )
)

(define (A115858 n) (A115857 (+ 1 (* 2 n))))


(define (A115859 n)
   (let ((all_muls (find_all_N_multipliers_262144 n)))
     (if (not (null? (cdr all_muls)))
         (cadr all_muls)
         0
     )
   )
;; (if (even? n)
;;     (* 2 (A115859 (/ n 2)))
;;     (find_largest_N_multiplier n 262144) ;; 2097152
;; )
)

(define (A115860 n) (A115859 (+ 1 (* 2 n))))

(define (A115861 n) (-1+ (length (find_all_N_multipliers_262144 n))))

(define (A115869 n)
   (let ((all_muls (find_all_N_multipliers_262144 n)))
     (if (not (null? (cdr all_muls)))
         (car (last-pair all_muls))
         0
     )
   )
)

(define (A115870 n) (A115869 (+ 1 (* 2 n))))

;; A115861(1)=3, A115861(2)=7
;; A115861(3)=5, A115861(4)=13
;; A115861(5)=7, A115861(6)=11

;; (define (A115861 n) (if (odd? n) (+ n 2) (A115857 (+ n 1))))


(define (A048724 n) (A003987bi n (* 2 n)))

(define (A065621 n) (- (A048724 (- n 1)) (expt -1 n)))

(define (A115872bi row col) (mult_and_Xmult_nth_nonzero row (A065621 row) col 262144))
(define (A115872 n) (A115872bi (1+ (A002262 (-1+ n))) (1+ (A025581 (-1+ n)))))

(definec (A115873 n) (mult_and_Xmult_first_nonzero n (A065621 n) 262144))

;; Here are the 3 A-numbers you requested: 116395 --- 116397.
(define A114395 (fun-succ-distincts1 A115873))

(define (A114396 n) (A115873 (A114395 n)))
(define (A114397 n) (A007088 (A114396 n)))

(define A115874 (fun-succ-matching-is0 (lambda (i) (= (* 19 i) (A048720bi 55 i)))))
(define (A115875 n) (A007088 (A115874 n)))

(define A115876 (fun-succ-matching-is0 (lambda (i) (= (* 41 i) (A048720bi 105 i)))))
(define (A115877 n) (A007088 (A115876 n)))

;; Here are the 5 A-numbers you requested: 116384 --- 116388.

(define A114384 (fun-succ-matching-is0 (lambda (i) (= (* 49 i) (A048720bi 81 i)))))
(define (A114385 n) (A007088 (A114384 n)))

(define A114386 (fun-succ-matching-is0 (lambda (i) (= (* 57 i) (A048720bi 73 i)))))
(define (A114387 n) (A007088 (A114386 n)))

(define (A114388 n) (A115872bi (1+ (A025581 (-1+ n))) (1+ (A002262 (-1+ n)))))

;; Here are the 6 A-numbers you requested: 116389 --- 116394.
;; A114389

(define (A114389 n) (A065621 (-1+ (* 2 n)))) ;; Bisection of A065621.

(define (A114390 n) (A065621 (* n n))) ;; A065621(A000290(n))

(define A114391 (fun-succ-matching-is0 (lambda (n) (square? (A114390 n)))))

(define (A114392 n) (A114390 (A114391 n)))

(define (A114393 n) (A000196 (A114392 n)))

(define (A114394 n) (- (A114391 (+ n 1)) (A114391 n)))


;; Here are the 3 A-numbers you requested: 116398 --- 116400.

(define A114398 (fun-succ-matching-is0 (lambda (n) (square? (A000695 n)))))
(define (A114399 n) (A000695 (A114398 n)))
(define (A114400 n) (A000196 (A000695 (A114398 n))))

;; Here are the 2 A-numbers you requested: 116401 --- 116402.

(define (A114401 n) (- (A114398 n) (A114400 n)))
(define (A114402 n) (/ (A114401 n) 2))

;; I.e. 5*i = 5Xi
(define A048716 (fun-succ-matching-is0 (lambda (i) (= (A003987bi (* 4 i) (* 1 i)) (* 5 i)))))

(define A048717 (fun-succ-matching-is0  ;; intersection of A048716 and A115767
                   (lambda (i) (and (= (* 3 i) (A048720bi 7 i))))))

(define A048719 (fun-succ-matching-is0  ;; intersection of A048716 and A048717
                   (lambda (i) (and (= (* 3 i) (A048720bi 7 i))
                                    (= (* 5 i) (A048720bi 5 i))))))


;; A115767 --- A115776.

(define A048719v2 (fun-succ-matching-is0  ;; intersection of A048716 and A115767
                   (lambda (i) (and (= (A003987bi (* 2 i) (* 3 i)) (* 5 i))
                                    (= (A003987bi (* 4 i) (* 1 i)) (* 5 i))))))

;; Not in the table! 0,7,14,15,28,30,31,56,60,62,63,103,112,115,120,124,126,127,199,206,207,224,227,...
(define A0joku (fun-succ-matching-is0  ;; A048717 \ (A048716 AND A115767)
                   (lambda (i) (and (= (* 3 i) (A048720bi 7 i))
                                    (not (and (= (A003987bi (* 2 i) (* 3 i)) (* 5 i))
                                              (= (A003987bi (* 4 i) (* 1 i)) (* 5 i))))))))


(define A003714v2 (fun-succ-matching-is0
                   (lambda (i) (= (* (A003987bi 2 3) i) ;; i.e. i
                                  (A003987bi (* 2 i) (* 3 i))))))

(define A115767 (fun-succ-matching-is0 (lambda (i) (= (A003987bi (* 2 i) (* 3 i)) (* 5 i)))))

(define A115768 (fun-succ-matching-is0  ;; A115767 \ A048719 ???
                   (lambda (i) (and (= (A003987bi (* 2 i) (* 3 i)) (* 5 i))
                                    (not (= (A003987bi (* 4 i) (* 1 i)) (* 5 i)))))))

(define (A115769 n) (A007088 (A115768 n)))

(define A115770 (fun-succ-matching-is0 (lambda (i) (= (* 7 i) (A048720bi 11 i)))))

(define (A115771 n) (A007088 (A115770 n)))

(define A115772 (fun-succ-matching-is0 (lambda (i) (= (* 13 i) (A048720bi 21 i)))))

(define (A115773 n) (A007088 (A115772 n)))

;; Differs from A062052 for the first time at n=18, where A115774(18)=645 while A062052(18)=672.
(define A115774 (fun-succ-matching-is0 (lambda (i) (= (* 15 i) (A048720bi 23 i)))))

(define (A115775 n) (A007088 (A115774 n)))

(define A115776 (fun-succ-matching-is0
                   (lambda (i) (and (= (* 13 i) (A048720bi 21 i))
                                    (not (= (* 15 i) (A048720bi 23 i)))))))

(define (A115781 n) (A007088 (A115776 n)))

;; Here are the 42 A-numbers you requested: A115793 --- A115834

(define A115793 (fun-succ-matching-is0 (lambda (i) (= (A003987bi (* 1 i) (* 10 i)) (* 11 i)))))
(define (A115794 n) (A007088 (A115793 n)))

(define A115795 (fun-succ-matching-is0 (lambda (i) (= (A003987bi (* 2 i) (* 9 i)) (* 11 i)))))
(define (A115796 n) (A007088 (A115795 n)))

(define A115797 (fun-succ-matching-is0 (lambda (i) (= (A003987bi (* 3 i) (* 8 i)) (* 11 i)))))
(define (A115798 n) (A007088 (A115797 n)))

(define A115799 (fun-succ-matching-is0 (lambda (i) (= (A003987bi (* 4 i) (* 7 i)) (* 11 i)))))
(define (A115800 n) (A007088 (A115799 n)))

;; A115845 --- A115850
(define A115845 (fun-succ-matching-is0 (lambda (i) (= (* 9 i) (A048720bi 9 i)))))
(define (A115846 n) (A007088 (A115845 n)))

(define A115847 (fun-succ-matching-is0 (lambda (i) (= (* 17 i) (A048720bi 17 i)))))
(define (A115848 n) (A007088 (A115847 n)))

(define A115849 (fun-succ-matching-is0 (lambda (i) (not (= (* 17 i) (A048720bi 17 i))))))
(define (A115850 n) (A007088 (A115849 n)))

(define A115801 (fun-succ-matching-is0 (lambda (i) (= (* 9 i) (A048720bi 25 i)))))
(define (A115802 n) (A007088 (A115801 n)))

(define A115803 (fun-succ-matching-is0 (lambda (i) (= (* 11 i) (A048720bi 31 i)))))
(define (A115804 n) (A007088 (A115803 n)))


(define A115805 (fun-succ-matching-is0 (lambda (i) (= (* 13 i) (A048720bi 29 i)))))
(define (A115806 n) (A007088 (A115805 n)))


(define A115807 (fun-succ-matching-is0 (lambda (i) (= (* 15 i) (A048720bi 27 i)))))
(define (A115808 n) (A007088 (A115807 n)))


(define A115809 (fun-succ-matching-is0 (lambda (i) (= (* 17 i) (A048720bi 49 i)))))
(define (A115810 n) (A007088 (A115809 n)))

(define A115811 (fun-succ-matching-is0
                   (lambda (i) (and (= (* 9 i) (A048720bi 25 i))
                                    (not (= (* 17 i) (A048720bi 49 i)))))))
(define (A115812 n) (A007088 (A115811 n)))


(define A115813 (fun-succ-matching-is0 (lambda (i) (= (A003987bi (* 2 i) (* 5 i)) (* 7 i)))))
(define (A115814 n) (A007088 (A115813 n)))

(define A115815 (fun-succ-matching-is0 (lambda (i) (= (A003987bi (* 2 i) (* 7 i)) (* 9 i)))))
(define (A115816 n) (A007088 (A115815 n)))

(define A115817 (fun-succ-matching-is0 (lambda (i) (= (A003987bi (* 4 i) (* 13 i)) (* 17 i)))))
(define (A115818 n) (A007088 (A115817 n)))

(define A115819 (fun-succ-matching-is0 (lambda (i) (= (A003987bi (* 4 i) (* 15 i)) (* 19 i)))))
(define (A115820 n) (A007088 (A115819 n)))

(define A115821 (fun-succ-matching-is0 (lambda (i) (= (A003987bi (* 6 i) (* 15 i)) (* 21 i)))))
(define (A115822 n) (A007088 (A115821 n)))

(define A115823 (fun-succ-matching-is0 (lambda (i) (= (A003987bi (* 8 i) (* 13 i)) (* 21 i)))))
(define (A115824 n) (A007088 (A115823 n)))

(define A115825 (fun-succ-matching-is0
                   (lambda (i) (and (= (A003987bi (* 8 i) (* 13 i)) (* 21 i))
                                    (not (= (* 13 i) (A048720bi 29 i)))))))
(define (A115826 n) (A007088 (A115825 n)))

(define A115827 (fun-succ-matching-is0 (lambda (i) (= (A003987bi (* 5 i) (* 6 i)) (* 11 i)))))
(define (A115828 n) (A007088 (A115827 n)))

(define A115829 (fun-succ-matching-is0 (lambda (i) (= (A003987bi (* 10 i) (* 11 i)) (* 21 i)))))
(define (A115830 n) (A007088 (A115829 n)))

(define A115831 (fun-succ-matching-is0 (lambda (i) (= (A003987bi (* 9 i) (* 16 i)) (* 25 i)))))
(define (A115832 n) (A007088 (A115831 n)))

(define A115833 (fun-succ-matching-is0 (lambda (i) (= (A003987bi (* 16 i) (* 17 i)) (* 33 i)))))
(define (A115834 n) (A007088 (A115833 n)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;; The following C++ program has been used to precompute the vectors      ;;
;; GF2X_FACVEC and Xn_1_FACVEC used in this Scheme-program.               ;;
;; After compiling the program I have performed                           ;;
;;   GF2Xfacs 65536 > GF2Xfacv.scm                                        ;;
;; and                                                                    ;;
;;   GF2Xfacs -511  > Xn_1facv.scm                                        ;;
;;                                                                        ;;
;; Then loaded them into a Scheme-interpreter as:                         ;;
;; (load "GF2Xfacv.scm")                                                  ;;
;; (load "Xn_1facv.scm")                                                  ;;
;; and dumped out in binary format as:                                    ;;
;; (fasdump GF2X_FACVEC "GF2Xffff.vec")                                   ;;
;; (fasdump Xn_1_FACVEC "Xn_1_511.vec")                                   ;;
;; so that they can be quickly loaded to this program with fasload.       ;;
;;                                                                        ;;
;; Copies of GF2Xfacv.scm and Xn_1facv.scm can be found at:               ;;
;;    http://www.iki.fi/~kartturi/matikka/Schemuli/GF2Xfacv.scm.gz        ;;
;; and:                                                                   ;;
;;    http://www.iki.fi/~kartturi/matikka/Schemuli/Xn_1facv.scm.gz        ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; /*
;;  * Get Victor Shoup's NTL-library (version 5.3.1) from http://shoup.net/ntl/
;;  *
;;  * And after installing the library, compile this program as
;;  * g++ -I/usr/local/include -L/usr/local/lib GF2Xfacs.c -o GF2Xfacs -lntl -lm
;;  *
;;  * where g++ --version gives:
;;  * g++ (GCC) 3.2.2
;;  * Copyright (C) 2002 Free Software Foundation, Inc.
;;  *
;;  * for the compiler what I have used now in January 2004.
;;  *
;;  */
;; 
;; #include <NTL/GF2XFactoring.h>
;; 
;; NTL_CLIENT
;; 
;; 
;; typedef unsigned long int ULLI;
;; 
;; /*
;;    n =  1:     x + 1                    11
;;    n =  2:    x² + 1                   101
;;    n =  3:    x³ + 1                  1001
;;    n =  4:   x^4 + 1                 10001
;;    n =  5:   x^5 + 1                100001
;;    n =  6:   x^6 + 1               1000001
;;    n =  7:   x^7 + 1              10000001 (1 byte)
;;    n =  8:   x^8 + 1             100000001 (2 bytes)
;;    n =  9:   x^9 + 1            1000000001 (2 bytes)
;;    n = 10:  x^10 + 1           10000000001 (2 bytes)
;; 
;;    n = 15:  x^15 + 1      1000000000000001 (2 bytes)
;;    n = 16:  x^16 + 1     10000000000000001 (3 bytes)
;; 
;;    Note: we should have: (list 3 3 137438953471 137438953471)
;;    instead of (3 3 4294967295 4294967295)
;;    for (Xn_1factor 74)
;;    I.e. use bignums, not the integers!
;;  */
;; 
;; 
;; void initXn_1(GF2X *p_z2p,ULLI n) /* n >= 1 */
;; {
;;     int i;
;;     unsigned char bytes[65];
;; 
;;     /* n divided by eight tells the number of intermediate zero-bytes needed. */
;;     for(i=0; i < (n >> 3);)
;;      {
;;        bytes[i++] = 0;
;;      }
;; 
;;     bytes[i] = 0;
;; 
;;     /* i now (n >> 3) */
;;     bytes[0] = 1; /* The constant coefficient 1. */
;; 
;;     /* Then the leading coefficient: */
;;     bytes[i++] |= (1 << (n & 7)); /* I.e. shift 1 to position (n mod 8). */
;; 
;;     /* i now (n >> 3)+1, the total number of bytes. */
;; 
;;     *p_z2p = GF2XFromBytes(bytes,(long)i);
;; 
;;     p_z2p->normalize();  /* Not necessary here?. */
;; }
;; 
;; 
;; void initFromBinexp(GF2X *p_z2p,ULLI n)
;; {
;;     int i = 0;
;;     unsigned char bytes[65];
;; 
;;     while(0 != n)
;;      {
;;        bytes[i] = (n&255);
;;        i++;
;;        n >>= 8;
;;      }
;; 
;;     *p_z2p = GF2XFromBytes(bytes,(long)i);
;; 
;; /*  p_z2p->normalize();  Not necessary here. */
;; 
;; }
;; 
;; ULLI GF2XtoBinexp(GF2X p_z2p)
;; {
;;     ULLI n = 0;
;;     int i;
;; 
;;     unsigned char bytes[65];
;;     long int n_bytes = NumBytes(p_z2p);
;;  
;;     BytesFromGF2X(bytes,p_z2p,n_bytes);
;; 
;;     for(i=n_bytes; i > 0; )
;;      {
;;        n <<= 8;
;;        n |= bytes[--i];
;;      }
;; 
;;     return(n);
;; }
;; 
;; 
;; ZZ GF2XtoZZ(GF2X p_z2p)
;; {
;;     ULLI n = 0;
;;     int i;
;; 
;;     unsigned char bytes[65];
;;     long int n_bytes = NumBytes(p_z2p);
;;  
;;     BytesFromGF2X(bytes,p_z2p,n_bytes);
;;     return(ZZFromBytes(bytes,n_bytes));
;; }
;; 
;; 
;; int main(int argc, char **argv)
;; {
;;    int i,upto_n;
;;    GF2X z2p;
;;    vec_pair_GF2X_long factors;
;;    const char *vec_name = "GF2X_FACVEC";
;;    int compute_Xn_1_factorizations = 0;
;; 
;;    if(argc <= 1) { cerr << "Usage: GF2Xfacs upto_n\n"; exit(1); }
;;    else { upto_n = atol(argv[1]); }
;; 
;;    if(upto_n < 0)
;;     {
;;       vec_name = "Xn_1_FACVEC";
;;       upto_n = -upto_n;
;;       compute_Xn_1_factorizations = 1;
;;     }
;; 
;;    cout << "(define "; cout << vec_name; cout << " (make-vector ";
;;    cout << (upto_n+1);  cout << "))\n\n";
;; 
;;    for(i=1; i <= upto_n; i++)
;;     {
;;       int j,u;
;;       if(compute_Xn_1_factorizations) { initXn_1(&z2p,i); }
;;       else { initFromBinexp(&z2p,i); }
;; 
;;       CanZass(factors, z2p);
;;       cout << "(vector-set! "; cout << vec_name; cout << " ";
;;       cout << i; cout << " (sort (list";
;;       u=factors.length();
;;       for(j=0; j<u; j++)
;;        {
;;          int h = factors[j].b;
;;          ZZ n = GF2XtoZZ(factors[j].a); /* Was: ULLI n = GF2XtoBinexp(factors[j].a); */
;;          while(h--) { cout << " "; cout << n; }
;;        }
;;       cout << ") <))\n";
;;     }
;; }
;; 
;; 


;;
;; Or:
;;  (output-entries-to-file GF2Xfuns_list "A91202-57.seqs" "Jan 03 2004")

(define GF2Xfuns_list
 (list
;; Here just for checking that certain permutations are inverses of each other:
  (list 001477 "Non-negative integers, identity permutation in S_inf."
        '(off: 0)
        '(indentries: Nperm)
        '(comps: (091202 091203) (091203 091202) (091204 091205) (091205 091204))
  )

  (list 000040 "Prime numbers, irreducible elements in domain of natural numbers."
        '(off: 1)
        '(comps: (091203 014580))
        '(inv: 049084)
        '(y: "Union of A091206 & A091209.")
  )

  (list 049084 "Inverse function of A000040: position in A000040 if n is prime, 0 otherwise."
        '(off: 1)
        '(comps: (091227 091202))
        '(inv: 000040)
  )

  (list 001221 "omega(n): Number of distinct primes dividing n."
        '(off: 1)
        '(comps: (091221 091202) (091221 091204))
  )

  (list 001222 "bigomega(n): Number of prime divisors of n (counted with multiplicity)."
        '(off: 1)
        '(comps: (091222 091202) (091222 091204))
  )

  (list 014580 "Irreducible GF(2)[X]-polynomials."
        '(off: 1)
        '(inv: 091227)
        '(comps: (091202 000040))
        '(indentries: GF2X)
        '(y: "Almost complement of A091242. Union of A091206 & A091214, and also of A091250 & A091252. First differences: A091223.")
  )

  (list 091202 "Multiplicative isomorphism from integers to GF2X-polynomials."
        '(off: 0)
        '(indentries: GF2X Nperm)
        '(inv: 091203)
  )

  (list 091203 "Multiplicative isomorphism from GF2X-polynomials to integers."
        '(off: 0)
        '(indentries: GF2X Nperm)
        '(inv: 091202)
  )

  (list 091204 "Deeply multiplicative isomorphism from integers to GF2X-polynomials."
        '(off: 0)
        '(upto: 256)
        '(indentries: GF2X Nperm)
        '(comps: (91227 091204 000040))
        '(inv: 091205)
  )

  (list 091205 "Deeply multiplicative isomorphism from GF2X-polynomials to integers."
        '(off: 0)
        '(upto: 256)
        '(indentries: GF2X Nperm)
        '(comps: (049084 091205 014580))
        '(inv: 091204)
  )

  (list 091206 "Primes that are also irreducible GF(2)[X]-polynomials."
        '(off: 1)
        '(comps: (000040 091207) (014580 091208))
        '(y: "Intersect of A014580 & A000040. Apart from a(2)=3 a subset of A027697.")
        '(indentries: GF2X)
  )

  (list 091207 "Indices of primes that are also irreducible GF(2)[X]-polynomials."
        '(off: 1)
        '(comps: (049084 091206))
        '(y: "Complement of A091210.")
  )

  (list 091208 "A014580-indices of irreducible GF(2)[X]-polynomials that are also primes."
        '(off: 1)
        '(comps: (091227 091206))
        '(y: "Complement of A091215.")
  )

  (list 091209 "Primes that are composite GF(2)[X]-polynomials."
        '(off: 1)
        '(comps: (000040 091210) (091242 091211))
        '(y: "Intersect of A000040 and A091242.")
        '(indentries: GF2X)
  )

  (list 091210 "Indices of primes that are composite GF(2)[X]-polynomials."
        '(off: 1)
        '(comps: (049084 091209))
        '(y: "Complement of A091207.")
        '(indentries: GF2X)
  )

  (list 091211 "A091242-indices of primes that are composite GF(2)[X]-polynomials."
        '(off: 1)
        '(comps: (091246 091209))
        '(y: "Complement of A091213.")
        '(indentries: GF2X)
  )

  (list 091212 "Composite GF(2)[X]-polynomials that are also composite integers."
        '(off: 1)
        '(comps: (091242 091213))
        '(y: "Intersect of A002808 and A091242.")
        '(indentries: GF2X)
  )

  (list 091213 "A091242-indices of composite GF(2)[X]-polynomials that are also composite integers."
        '(off: 1)
        '(comps: (091246 091212))
        '(y: "Complement of A091211.")
        '(indentries: GF2X)
  )

  (list 091214 "Irreducible GF(2)[X]-polynomials that are composite integers."
        '(off: 1)
        '(comps: (014580 091215))
        '(y: "Intersect of A002808 and A014580.")
        '(indentries: GF2X)
  )

  (list 091215 "A014580-indices of irreducible GF(2)[X]-polynomials that are composite integers."
        '(off: 1)
        '(comps: (091227 091214))
        '(y: "Complement of A091208.")
  )


  (list 091219 "Moebius-analog for the domain GF(2)[X]: a(n)=0 if A091221(n)!=A091222(n), otherwise -1^A091222(n)."
        '(off: 1)
        '(comps: (008683 091203) (008683 091205))
        '(indentries: GF2X)
  )

  (list 091220 "Number of divisors in the nth GF(2)[X]-polynomial."
        '(off: 1)
        '(comps: (000005 091203) (000005 091205))
        '(indentries: GF2X)
  )

  (list 091221 "Number of distinct irreducible polynomials dividing nth GF(2)[X]-polynomial."
        '(off: 1)
        '(comps: (001221 091203) (001221 091205))
        '(indentries: GF2X)
        '(y: "A000374(n) = a(A000051(n)).")
  )

  (list 091222 "Number of irreducible polynomials dividing nth GF(2)[X]-polynomial. (counted with multiplicity)."
        '(off: 1)
        '(comps: (001222 091203) (001222 091205))
        '(indentries: GF2X)
        '(y: "A091248(n) = a(A000051(n)).")
  )

  (list 091223 "Differences between consecutive irreducible GF(2)[X]-polynomials."
        '(off: 1)
        '(c: "Analogous to A001223.")
        '(y: "First differences of A014580. Divided by 2: A091224.")
        '(indentries: GF2X)
  )

  (list 091224 "Differences between consecutive irreducible GF(2)[X]-polynomials, divided by 2."
        '(off: 2)
        '(c: "Analogous to A028334.")
        '(y: "a(n) = A091223(n)/2.")
  )

  (list 091225 "Characteristic function of A014580: 1 if the nth GF(2)[X] polynomial is irreducible, 0 otherwise."
        '(off: 0)
        '(comps: (010051 091203) (010051 091205))
        '(y: "Partial sums give A091226. C.f. A091227. Complementary to A091247.")
        '(indentries: GF2X)
  )

  (list 091226 "Number of irreducible GF(2)[X]-polynomials in range [0,n]."
        '(off: 0)
        '(c: "Analogous to A000720.")
        '(y: "Partial sums of A091225. A062692(n) = a(2^n).")
        '(indentries: GF2X)
  )

  (list 091227 "Inverse function of A014580: position in A014580 if the nth GF(2)[X] polynomial is irreducible, 0 otherwise."
        '(off: 1)
        '(comps: (049084 091203))
        '(inv: 014580)
        '(f: "a(n) = A091225(n) * A091226(n).")
        '(indentries: GF2X)
  )

  (list 091228 "Smallest m >= n, such that m is irreducible when interpreted as GF(2)[X]-polynomial."
        '(off: 0)
        '(f: "a(n) = n + A091229(n).")
        '(c: "Analogous to A007918.")
        '(indentries: GF2X)
  )

  (list 091229 "Smallest k such that n+k is irreducible when interpreted as GF(2)[X]-polynomial."
        '(off: 0)
        '(f: "a(n) = A091228(n) - n.")
        '(c: "Analogous to A007920.")
  )

  (list 091230 "Iterates of A014580."
        '(off: 0)
        '(upto: 7) ;; Grows so fast, compute only up to 7. (1 2 3 7 25 137 1123 13103)
        '(f: "a(0)=1, a(n) = A014580(a(n-1)).")
        '(y: "A091238(a(n)) = n+1.")
        '(comps: (091204 007097))
        '(indentries: GF2X)
  )

  (list 091231 "How many more primes than irreducible GF(2)[X] polynomials there are in range [0,2^n]."
        '(off: 0)
        '(upto: 34)
        '(indentries: GF2X)
        '(f: "a(0)=a(1)=0, a(n) = A007053(n)-A062692(n-1).")
        '(y: "Partial sums of A091232.")
  )

  (list 091232 "How many more primes than irreducible GF(2)[X] polynomials there are in range [2^n,2^(n+1)]."
        '(off: 0)
        '(upto: 34)
        '(indentries: GF2X)
        '(f: "a(0)=a(1)=0, a(n) = A036378(n+1)-A001037(n).")
        '(y: "First differences of A091231.")
  )

  (list 091233 "Size of range [Smallest Matula number coding a tree of n nodes,Largest Matula number coding a tree of n nodes]."
        '(off: 1)
        '(upto: 16)
        '(f: "a(n) = (A005518(n)-A005517(n))+1.")
        '(y: "Compare to A091241.")
  )

  (list 091238 "Number of nodes in rooted tree with GF2X-Matula number n."
        '(off: 1)
        '(c: "Each n occurs A000081(n) times.")
        '(y: "a(A091230(n)) = n+1. C.f. A091239-A091241.")
        '(comps: (061775 091205))
  )

  (list 091239 "Smallest GF2X-Matula number i with number of nodes = n in the corresponding tree."
        '(off: 1)
        '(upto: 31)
        '(y: "Analogous to A005517. A091240 gives the largest i with number of nodes = n. C.f. A091241.")
  )

  (list 091240 "Largest GF2X-Matula number i with number of nodes = n in the corresponding tree."
        '(off: 1)
        '(upto: 8)
        '(c: "Apparently from n=4 onward given by recurrence a(4)=A014580(4), a(5)=A014580(A014580(4)), a(6)=A014580(A014580(A014580(4))), etc.")
        '(y: "Analogous to A005518. A091239 gives the smallest i with number of nodes = n. C.f. A091241.")
  )

  (list 091241 "Size of range [Smallest GF2X-Matula number coding a tree of n nodes,Largest GF2X-Matula number coding a tree of n nodes]."
        '(off: 1)
        '(upto: 8)
        '(f: "a(n) = (A091240(n)-A091239(n))+1.")
        '(y: "Compare to A091233.")
  )

  (list 091242 "Reducible GF(2)[X]-polynomials."
        '(off: 1)
        '(inv: 091246)
        '(indentries: GF2X)
        '(c: "Analogous to A002808.")
        '(y: "Almost complement of A014580. Union of A091209 & A091212. First differences: A091243. Characteristic function: A091247. In binary format: A091254.")
  )

  (list 091243 "Differences between consecutive reducible GF(2)[X]-polynomials."
        '(off: 1)
        '(c: "Analogous to A073783.")
        '(y: "First differences of A091242. a(n) = A091244(n)+1.")
        '(indentries: GF2X)
  )

  (list 091244 "Number of irreducible polynomials between successive reducible GF(2)[X]-polynomials."
        '(off: 1)
        '(c: "Analogous to A073784.")
        '(y: "a(n) = A091243(n)-1.")
        '(indentries: GF2X)
  )

  (list 091245 "Number of reducible GF(2)[X]-polynomials in range [0,n]."
        '(off: 0)
        '(c: "Analogous to A065855.")
        '(y: "Partial sums of A091247.")
        '(indentries: GF2X)
  )

  (list 091246 "Inverse function of A091242: position in A091242 if the nth GF(2)[X] polynomial is reducible, 0 otherwise."
        '(off: 1)
        '(inv: 091242)
        '(c: "Analogous to A066246.")
        '(f: "a(n) = A091245(n) * A091247(n).")
        '(indentries: GF2X)
  )

  (list 091247 "Characteristic function of A091242: 1 if the nth GF(2)[X] polynomial is composite, 0 otherwise."
        '(off: 0)
        '(comps: (066247 091203) (066247 091205))
        '(y: "Complementary to A091225. Partial sums give A091245. C.f. A091246")
        '(indentries: GF2X)
  )

  (list 091248 "Number of irreducible factors in the factorization of GF(2)[X]-polynomial x^n+1 (counted with multiplicity)."
        '(off: 1)
        '(upto: 120)
        '(c: "C.f. A000374")
        '(f: "a(n) = A091222(A000051(n)).")
;;      '(comps: (091222 000051))
        '(indentries: GF2X)
  )

  (list 091249 "A014580-indices of primitive irreducible GF(2)[X]-polynomials."
        '(off: 1)
        '(upto: 100)
        '(comps: (091227 091250))
        '(y: "Complement of A091251.")
        '(indentries: GF2X)
  )

  (list 091250 "Primitive irreducible polynomials over GF(2)."
        '(off: 1)
        '(upto: 100)
        '(comps: (014580 091249))
        '(y: "C.f. A011260, A091252. A058947(n) = A007088(a(n)).")
        '(indentries: GF2X)
  )

  (list 091251 "A014580-indices of non-primitive irreducible GF(2)[X]-polynomials."
        '(off: 1)
        '(upto: 27)
        '(comps: (091227 091252))
        '(y: "Complement of A091249.")
        '(indentries: GF2X)
  )

  (list 091252 "Non-primitive irreducible polynomials over GF(2)."
        '(off: 1)
        '(upto: 27)
        '(comps: (014580 091251))
        '(y: "C.f. A091250. In binary format: A091253.")
        '(indentries: GF2X)
  )

  (list 091253 "Non-primitive irreducible polynomials over GF(2), in binary format."
        '(off: 1)
        '(upto: 27)
        '(comps: (007088 091252))
        '(indentries: GF2X)
  )

  (list 091254 "Reducible polynomials over GF(2), in binary format."
        '(off: 1)
        '(comps: (007088 091242))
        '(y: "C.f. A058943.")
        '(indentries: GF2X)
  )

  (list 091255 "Table of GCD(x,y) computed for polynomials over GF(2), where (x,y) runs as (1,1),(1,2),(2,1),(1,3),(2,2),(3,1),..."
        '(off: 1)
        '(keywords: "tabl")
        '(c: "Analogous to A003989.")
        '(y: "C.f. A091256, A091257.")
        '(indentries: GF2X Lattices)
  )

  (list 091256 "Table of LCM(x,y) computed for polynomials over GF(2), where (x,y) runs as (1,1),(1,2),(2,1),(1,3),(2,2),(3,1),..."
        '(off: 1)
        '(keywords: "tabl")
        '(c: "Analogous to A003990.")
        '(y: "C.f. A091255, A091257.")
        '(indentries: GF2X Lattices)
  )

  (list 091257 "Multiplication table A x B computed for polynomials over GF(2), where (A,B) runs as (1,1),(1,2),(2,1),(1,3),(2,2),(3,1),..."
        '(off: 1)
        '(keywords: "tabl")
        '(c: "Essentially same as A048720 but computed starting from offset one instead of zero. Analogous to A003991.")
        '(y: "a(n) = A048720bi(A091255(n),A091256(n)), because A x B = GCD(A,B) x LCM(A,B) holds also in the polynomial ring GF(2)[X].")
        '(indentries: GF2X)
  )

  (list 061775 "Number of nodes in rooted tree with Matula-Goebel number n."
        '(off: 1)
        '(c: "Each n occurs A000081(n) times.")
        '(comps: (091238 091204))
  )


 )
)

;; For May 2005 additions (A106442 --- A106457) and their Sep 2002 precedents:
;; Also A106485 --- A106495 (Quetian "superfactoring" sequences).

(define May2005-list
 (list

  (list 075161 "Position of A075165(n+1) in A014486."
        '(off: 0)
        '(indentries: Nperm)
        '(inv: 075162)
        '(c: "See A075166.")
        '(y: "a(n) = A075163(n+1)-1.")
  )

  (list 075162 "Position of A014486(n) in A075165, minus one."
        '(off: 0)
        '(indentries: Nperm)
        '(inv: 075161)
        '(upto: 32)
        '(c: "See A075166.")
        '(y: "a(n) = A075164(n+1)-1. A000040(n) = a(A014137(n))+1.")
  )

  (list 075163 "Position of A075165(n) in A014486 plus one."
        '(off: 1)
        '(indentries: Nperm)
        '(inv: 075164)
        '(comps: (106453 106442))
        '(c: "See A075166.")
        '(y: "a(n) = A075161(n-1)+1.")
  )

  (list 075164 "Position of A014486(n-1) in A075165."
        '(off: 1)
        '(indentries: Nperm)
        '(inv: 075163)
        '(comps: (106443 106454))
        '(upto: 32)
        '(c: "See A075166.")
        '(y: (string-append
                "a(n) = A075162(n-1)+1. A000040(n) = a(1+A014137(n))."
                " The powers of two are located at Axxxxxx.")
         )
  )

  (list 075165 "Sequence A075166 interpreted as binary numbers and converted to decimal."
        '(off: 1)
        '(comps: (106455 106442))
        '(upto: 120)
        '(y: (string-append
                "Permutation of A014486. Same sequence shown in binary: A075166."
                " The binary width of each term / 2 is given by A075167.")
         )
  )

  (list 075166 (string-append
                  "Natural numbers mapped to Dyck path encodings of the rooted plane trees"
                  " obtained by recursing on the exponents of the prime factorization of n.")
        '(off: 1)
        '(comps: (007088 075165) (106456 106442))
        '(upto: 120)
        '(y: (string-append
                "Permutation of A063171. Same sequence shown in decimal: A075165."
                " The digital length of each term / 2 (the number of o-nodes in the"
                " corresponding trees) is given by A075167. C.f. A075161-A075164.")
         )
  )

  (list 075167 (string-append
                  "Number of edges in each rooted plane tree produced with the prime"
                  " factorization unranking algorithm presented in A075166.")
        '(off: 1)
        '(upto: 512)
        '(comps: (106457 106442))
        '(c: "Also the digital length of A075166(n)/ 2. Each value v occurs A000108(v) times.")
        '(y: "Permutation of A072643.")
  )

;; Two permutations that map between the domains:

  (list 106442 "Exponent-recursed cross-domain bijection from N to GF(2)[X]. Position of A075166(n) in A106456."
        '(off: 1) ;; a(0)=0 might be manually added.
        '(indentries: Nperm)
        '(inv: 106443)
        '(comps: (106454 075163))
;;      '(upto: 32)
        '(c: (string-append
   "This map from the multiplicative domain of N to that of GF(2)[X] preserves"
   " Catalan-family structures, e.g."
   " A106454(n) = a(A075164(n)), A075163(n) = A106453(a(n)), A075165(n) = A106455(a(n)),"
   " A075166(n) = A106456(a(n)), A075167(n) = A106457(a(n))."
   " Shares with A091202 and A106444 the property that maps A000040(n) to A014580(n)."
   " Differs from the former for the first time at n=32, where A091202(32)=32, while a(32)=128."
   " Differs from the latter for the first time at n=48, where A106444(48)=48, while a(48)=192."
             )
         )
  )

  (list 106443 "Exponent-recursed cross-domain bijection from GF(2)[X] to N. Position of A106456(n) in A075166."
        '(off: 1) ;; a(0)=0 might be manually added.
        '(indentries: Nperm)
        '(inv: 106442)
        '(comps: (075164 106453))
;;      '(upto: 128)
        '(c: (string-append
   "This map from the multiplicative domain of GF(2)[X] to that of N preserves"
   " Catalan-family structures, e.g."
   " A075164(n) = a(A106454(n)), A106453(n) = A075163(a(n)), A106455(n) = A075165(a(n)),"
   " A106456(n) = A075166(a(n)), A106457(n) = A075167(a(n))."
   " Shares with A091203 and A106445 the property that maps A014580(n) to A000040(n)."
   " Differs from the former for the first time at n=32, where A091203(32)=32, while a(32)=512."
   " Differs from the latter for the first time at n=48, where A106445(48)=48, while a(48)=768."
             )
         )
  )

  (list 106444 "Exponent-recursed cross-domain bijection from N to GF(2)[X]. Variant of A091202 and A106442."
        '(off: 0)
        '(indentries: Nperm)
        '(inv: 106445)
;;      '(upto: 32)
        '(c: (string-append
   "This map from the multiplicative domain of N to that of GF(2)[X] preserves"
   " 'superfactorized' structures,"
   " e.g. A106490(n) = A106493(a(n)), A106491(n) = A106494(a(n)), A064372(n) = A106495(a(n))."
   " Shares with A091202 and A106442 the property that maps A000040(n) to A014580(n)."
   " Differs from A091202 for the first time at n=32, where A091202(32)=32, while a(32)=128."
   " Differs from A106442 for the first time at n=48, where A106442(48)=192, while a(48)=48."
   " Differs from A106446 for the first time at n=11, where A106446(11)=25, while a(11)=13."
             )
         )
  )

  (list 106445 "Exponent-recursed cross-domain bijection from GF(2)[X] to N. Variant of A091203 and A106443."
        '(off: 0)
        '(indentries: Nperm)
        '(inv: 106444)
;;      '(upto: 128)
        '(c: (string-append
   "This map from the multiplicative domain of GF(2)[X] to that of N preserves"
   " 'superfactorized' structures,"
   " e.g. A106493(n) = A106490(a(n)), A106494(n) = A106491(a(n)), A106495(n) = A064372(a(n))."
   " Shares with A091203 and A106443 the property that maps A014580(n) to A000040(n)."
   " Differs from the plain variant A091203 for the first time at n=32, where A091203(32)=32, while a(32)=512."
   " Differs from the variant A106443 for the first time at n=48, where A106443(48)=768, while a(48)=48."
   " Differs from a yet deeper variant A106447 for the first time at n=13, where A106447(13)=23, while a(13)=11."
             )
         )
  )

  (list 106446 "Doubly-recursed cross-domain bijection from N to GF(2)[X]. Variant of A091204 and A106444."
        '(off: 0)
        '(indentries: Nperm)
        '(inv: 106447)
;;      '(upto: 32)
        '(c: (string-append
         " Differs from A091204 for the first time at n=32, where A091204(32)=32, while a(32)=128."
         " Differs from A106444 for the first time at n=11, where A106444(11)=13, while a(11)=25."
             )
         )
  )

  (list 106447 "Doubly-recursed cross-domain bijection from GF(2)[X] to N. Variant of A091205 and A106445."
        '(off: 0)
        '(indentries: Nperm)
        '(inv: 106446)
        '(upto: 128)
        '(c: (string-append
       " Differs from A091205 for the first time at n=32, where A091205(32)=32, while a(32)=512."
       " Differs from A106445 for the first time at n=13, where A106445(13)=11, while a(13)=23."
             )
         )
  )

;; GF2X-analogues follow:

  (list 106451 "Position of A106455(n+1) in A014486."
        '(off: 0)
        '(indentries: Nperm)
        '(inv: 106452)
        '(c: "See A106456.")
        '(y: "a(n) = A106453(n+1)-1. GF(2)[X]-analogue of A075161.")
  )

  (list 106452 "Position of A014486(n) in A106455, minus one."
        '(off: 0)
        '(indentries: Nperm)
        '(inv: 106451)
        '(upto: 32)
        '(c: "See A106456.")
        '(y: "a(n) = A106454(n+1)-1. GF(2)[X]-analogue of A075162.")
  )

  (list 106453 "Position of A106455(n) in A014486 plus one."
        '(off: 1)
        '(indentries: Nperm)
        '(inv: 106454)
        '(comps: (075163 106443))
        '(c: "See A106456.")
        '(y: "a(n) = A106451(n-1)+1. GF(2)[X]-analogue of A075163.")
  )

  (list 106454 "Position of A014486(n-1) in A106455."
        '(off: 1)
        '(indentries: Nperm)
        '(inv: 106453)
        '(comps: (106442 075164))
        '(upto: 32)
        '(c: "See A106456.")
        '(y: "a(n) = A106452(n-1)+1. GF(2)[X]-analogue of A075164.")
  )

  (list 106455 "Sequence A106456 interpreted as binary numbers and converted to decimal."
        '(off: 1)
        '(comps: (075165 106443))
        '(upto: 120)
        '(y: (string-append
                "Permutation of A014486. Same sequence shown in binary: A106456."
                " The binary width of each term / 2 is given by A106457."
                "  GF(2)[X]-analogue of A075165.")
         )
  )

  (list 106456 (string-append
                  "Natural numbers mapped to Dyck path encodings of the rooted plane trees"
                  " obtained by recursing on the exponents of the GF(2)[X] factorization of n.")
        '(off: 1)
        '(comps: (007088 106455) (075166 106443))
        '(upto: 120)
        '(y: (string-append
                "GF(2)[X]-analogue of A075166."
                " Permutation of A063171. Same sequence shown in decimal: A106455."
                " The digital length of each term / 2 (the number of o-nodes in the"
                " corresponding trees) is given by A106457. C.f. A106451-A106454.")
         )
  )

  (list 106457 (string-append
                  "Number of edges in each rooted plane tree produced with the GF(2)[X]"
                  " factorization unranking algorithm presented in A106456.")
        '(off: 1)
        '(upto: 512)
        '(comps: (075167 106443))
        '(c: "Also the digital length of A106456(n)/ 2. Each value v occurs A000108(v) times.")
        '(y: "Permutation of A072643. C.f. A075167.")
  )

  (list 064372 "Number of leaves in Quetian superfactorization of n."
        '(f: "a(n) = A106491(n)-A106490(n).")
        '(off: 1)
        '(comps: (106495 106444))
        '(y: (string-append
    "After n=1 differs from A001221 for the first time at n=64, where A001221(64)=1,"
    " while a(64)=2.")
         )
  )

  (list 106490 (string-append
                  "Total number of bases and exponents in Quetian Superfactorization of n,"
                  " excluding the unity-exponents at the tips of branches."
               )
        '(off: 1)
        '(c: (string-append
 "Quetian Superfactorization proceeds by factoring a natural number to its unique"
 " prime-exponent factorization (p1^e1 * p2^e2 * ... pj^ej) and then factoring"
 " recursively each of the (non-zero) exponents in similar manner, until unity-exponents"
 " are finally encountered."
 " a(64) = 3, as 64 = 2^6 = 2^(2^1*3^1), and there are 3 non-1 nodes in that superfactorization."
 " Similarly, for 360 = 2^(3^1) * 3^(2^1) * 5^1 we get a(360) = 5."
 " a(65536) = a(2^(2^(2^(2^1)))) = 4."
             )
         )
        '(comps: (106493 106444))
        '(y: (string-append "a(n) = A106491(n)-A064372(n). C.f. A106492."
       " After n=1 differs from A038548 for the first time at n=24, where A038548(24)=4, while a(24)=3."
             )
         )
  )

  (list 106491 (string-append
                  "Total number of bases and exponents in Quetian Superfactorization of n,"
                  " including the unity-exponents at the tips of branches."
               )
        '(off: 1)
        '(c: "a(64) = 5, as 64 = 2^6 = 2^(2^1*3^1), and there are 5 nodes in that superfactorization. Similarly, for 360 = 2^(3^1) * 3^(2^1) * 5^1 we get a(360) = 8. See comments at A106490.")
        '(comps: (106494 106444))
        '(y: (string-append "a(n) = A106490(n)+A064372(n). C.f. A106492."))
  )

  (list 106492 (string-append
                  "Total sum of bases and exponents in Quetian Superfactorization of n,"
                  " excluding the unity-exponents at the tips of branches."
               )
        '(off: 1)
        '(c: "a(64) = 7, as 64 = 2^6 = 2^(2^1*3^1), and 2+2+3=7. Similarly, for 360 = 2^(3^1) * 3^(2^1) * 5^1 we get a(360) = 2+3+3+2+5 = 15. See comments at A106490.")
        '(y: (string-append "C.f. A106490-A106491."))
  )

  (list 106493 (string-append
                  "Total number of bases and exponents in GF(2)[X] Superfactorization of n,"
                  " excluding the unity-exponents at the tips of branches."
               )
        '(off: 1)
        '(c: (string-append
 "GF(2)[X] Superfactorization proceeds in a manner analogous to normal superfactorization"
 " explained in A106490, but using factorization in domain GF(2)[X], instead of normal"
 " integer factorization in N."
 " a(64) = 3, as 64 = 2^6 = 2^(2^1 X 3^1), and there are 3 non-1 nodes in that superfactorization."
 " Similarly, for 27 = 3^(2^1) X 7^1 we get a(27) = 3. The operation X stands for GF(2)[X]"
 " multiplication defined in A048720."
             )
         )
        '(comps: (106490 106445))
        '(y: (string-append "a(n) = A106494(n)-A106495(n)."
             )
         )
  )

  (list 106494 (string-append
                  "Total number of bases and exponents in GF(2)[X] Superfactorization of n,"
                  " including the unity-exponents at the tips of branches."
               )
        '(off: 1)
        '(c: (string-append
  "a(64) = 5, as 64 = 2^6 = 2^(2^1 X 3^1), and there are 5 nodes in that superfactorization."
  " Similarly, for 27 = 3^(2^1) X 7^1 we get a(27) = 5. See comments at A106493."
             )
         )
        '(comps: (106491 106445))
        '(y: (string-append "a(n) = A106493(n)+A106495(n)."))
  )

  (list 106495 "Number of leaves in GF(2)[X] superfactorization of n."
        '(f: "a(n) = A106494(n)-A106493(n).")
        '(off: 1)
        '(comps: (064372 106445))
        '(y: (string-append
    "After n=1 differs from A091221 for the first time at n=64, where A091221(64)=1,"
    " while a(64)=2.")
         )
  )
 )
)


(define May2005-list2
 (list
  (list 106448 "Table of (x+y)/GCD(x,y) where (x,y) runs as (1,1),(1,2),(2,1),(1,3),(2,2),(3,1),..."
        '(off: 1)
        '(upto: 105)
        '(keywords: "tabl")
        '(f: "A003057/A003989")
        '(y: "GF(2)[X] analogue: A106449. Row 1 is n+1, row 2 is LEFT(LEFT(LEFT(A026741))), row 3 is LEFT^4(A051176).")
  )

  (list 106449 "Table of (x XOR y)/GCD(x,y) computed for binary codes of polynomials over GF(2), where (x,y) runs as (1,1),(1,2),(2,1),(1,3),(2,2),(3,1),..."
        '(off: 1)
        '(upto: 105)
        '(keywords: "tabl")
        '(c: "GF(2)[X] analogue of A106448.")
        '(y: "A003987(x,y) = A048720(A106448(x,y),A091255(x,y)). Row 1: LEFT(A004442), row 2: A106450.")
        '(indentries: GF2X)
  )

  (list 106450 "a(n) = A004443(n) if n is odd, a(n) = A004443(n)/2 if n is even."
        '(off: 0)
        '(y: "Skipping the initial term (a(0)=2), this is row 2 of A106449.")
  )

 )
)


(define Jan2006-list
 (list

  (list 115767 "Integers i such that 2*i XOR 5*i = 3*i."
        '(off: 0)
;;      '(indentries: Nperm)
        '(c: "XOR is A003987.")
        '(y: "Superset of A048719. A115768 gives the terms which are not in A048719.")
  )

  (list 115768 "Integers i such that 2*i XOR 5*i = 3*i, but 4*i XOR i is not 5*i."
        '(off: 1)
;;      '(indentries: Nperm)
        '(c: "XOR is A003987.")
        '(y: "Setwise difference of A115767 and A048719. A115769 shows this sequence in binary.")
  )

  (list 115769 "Sequence A115768 in binary."
        '(off: 1)
        '(keywords: "base")
        '(comps: (007088 115768))
  )

  (list 115770 "Integers i such that 7*i = A048720bi(11,i)."
        '(off: 0)
        '(indentries: GF2X)
        '(c: "Here * stands for ordinary multiplication, and A048720 is the carryless (GF(2)[X]) multiplication.")
        '(y: "A048719, A115767, A115772, A115774, A115776. A115771 shows this sequence in binary.")
  )

  (list 115771 "Sequence A115770 in binary."
        '(off: 0)
        '(keywords: "base")
        '(comps: (007088 115770))
  )

  (list 115772 "Integers i such that 13*i = A048720bi(21,i)."
        '(off: 0)
        '(indentries: GF2X)
        '(c: "Here * stands for ordinary multiplication, and A048720 is the carryless (GF(2)[X]) multiplication.")
        '(y: (string-append "A048719, A115767, A115770."
                            " Superset of A115774 ? A115776 gives the terms which are not in A115774."
                            " A115773 shows this sequence in binary."
             )
         )
  )

  (list 115773 "Sequence A115772 in binary."
        '(off: 0)
        '(keywords: "base")
        '(comps: (007088 115772))
  )

  (list 115774 "Integers i such that 15*i = A048720bi(23,i)."
        '(off: 0)
        '(indentries: GF2X)
        '(c: "Here * stands for ordinary multiplication, and A048720 is the carryless (GF(2)[X]) multiplication.")
        '(y: (string-append "A048719, A115767, A115770."
                            " Subset of A115772 ? A115776 gives the terms of A115772 which do not occur here."
                            " Differs from A062052 for the first time at n=18,"
                            " where A115774(18)=645 while A062052(18)=672."
                            " A115775 shows this sequence in binary."
             )
         )
  )

  (list 115775 "Sequence A115774 in binary."
        '(off: 0)
        '(keywords: "base")
        '(comps: (007088 115774))
  )

  (list 115776 "Integers i such that 13*i = A048720bi(21,i), but 15*i <> A048720bi(23,i)."
        '(off: 0)
        '(indentries: GF2X)
        '(c: "Here * stands for ordinary multiplication, and A048720 is the carryless (GF(2)[X]) multiplication.")
        '(off: 1)
        '(y: "Setwise difference of A115772 and A115774. A115781 shows this sequence in binary.")
  )

  (list 115781 "Sequence A115776 in binary."
        '(off: 1)
        '(keywords: "base")
        '(comps: (007088 115776))
  )
 )
)


(define Feb2006-list
 (list
  (list 115793 "Integers i such that 1*i XOR 10*i = 11*i."
        '(off: 0)
        '(indentries: XORcongruent)
        '(c: "XOR is A003987.")
        '(y: "A115794 shows this sequence in binary.")
  )
  (list 115794 "Sequence A115793 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115793)))

  (list 115795 "Integers i such that 2*i XOR 9*i = 11*i."
        '(off: 0)
        '(indentries: XORcongruent)
        '(c: "XOR is A003987.")
        '(y: "A115796 shows this sequence in binary.")
  )
  (list 115796 "Sequence A115795 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115795)))

  (list 115797 "Integers i such that 3*i XOR 8*i = 11*i."
        '(off: 0)
        '(indentries: XORcongruent)
        '(c: "XOR is A003987.")
        '(y: "A115798 shows this sequence in binary.")
  )
  (list 115798 "Sequence A115797 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115797)))

  (list 115799 "Integers i such that 4*i XOR 7*i = 11*i."
        '(off: 0)
        '(indentries: XORcongruent)
        '(c: "XOR is A003987.")
        '(y: "A115800 shows this sequence in binary.")
  )
  (list 115800 "Sequence A115799 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115799)))

  (list 115801 "Integers i such that 9*i = 25 X i."
        '(off: 0)
        '(indentries: crossdomain)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720).")
        '(y: "Seems to be a superset of A115809. A115811 gives the terms not present in A115809."
             " A115802 shows this sequence in binary."
         )
  )
  (list 115802 "Sequence A115801 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115801)))

  (list 115803 "Integers i such that 11*i = 31 X i."
        '(off: 0)
        '(indentries: crossdomain)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720).")
        '(y: "A115804 shows this sequence in binary.")
  )
  (list 115804 "Sequence A115803 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115803)))

  (list 115805 "Integers i such that 13*i = 29 X i."
        '(off: 0)
        '(indentries: crossdomain)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720).")
        '(y: "Subset of A115823 ?"
                            " Differs from A115829 for the first time at n=21,"
                            " where A115829(21)=2979 while a(21)=3072."
                            " A115806 shows this sequence in binary."
         )
  )
  (list 115806 "Sequence A115805 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115805))
        '(y: "Differs from A115830 for the first time at n=21,"
             " where A115830(21)=101110100011 while a(21)=110000000000."
         )
  )

  (list 115807 "Integers i such that 15*i = 27 X i."
        '(off: 0)
        '(indentries: crossdomain)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720).")
        '(y: "A115808 shows this sequence in binary.")
  )
  (list 115808 "Sequence A115807 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115807)))

  (list 115809 "Integers i such that 17*i = 49 X i."
        '(off: 0)
        '(indentries: crossdomain)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720).")
        '(y: "Subset of A115801 ? A115810 shows this sequence in binary.")
  )
  (list 115810 "Sequence A115809 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115809)))

  (list 115811 "Integers i such that 9*i = 25 X i, but 17*i is not 49 X i."
        '(off: 1)
        '(indentries: crossdomain)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720).")
        '(y: "Setwise difference of A115801 and A115809. A115812 shows this sequence in binary.")
  )
  (list 115812 "Sequence A115811 in binary." '(off: 1) '(keywords: "base") '(comps: (007088 115811)))

  (list 115813 "Integers i such that 2*i XOR 5*i = 7*i."
        '(off: 0)
        '(indentries: XORcongruent)
        '(c: "XOR is A003987.")
        '(y: "A115814 shows this sequence in binary.")
  )
  (list 115814 "Sequence A115813 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115813)))

  (list 115815 "Integers i such that 2*i XOR 7*i = 9*i."
        '(off: 0)
        '(indentries: XORcongruent)
        '(c: "XOR is A003987.")
        '(y: "A115816 shows this sequence in binary.")
  )
  (list 115816 "Sequence A115815 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115815)))

  (list 115817 "Integers i such that 4*i XOR 13*i = 17*i."
        '(off: 0)
        '(indentries: XORcongruent)
        '(c: "XOR is A003987.")
        '(y: "A115818 shows this sequence in binary.")
  )
  (list 115818 "Sequence A115817 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115817)))

  (list 115819 "Integers i such that 4*i XOR 15*i = 19*i."
        '(off: 0)
        '(indentries: XORcongruent)
        '(c: "XOR is A003987.")
        '(y: "A115820 shows this sequence in binary.")
  )
  (list 115820 "Sequence A115819 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115819)))

  (list 115821 "Integers i such that 6*i XOR 15*i = 21*i."
        '(off: 0)
        '(indentries: XORcongruent)
        '(c: "XOR is A003987.")
        '(y: "A115822 shows this sequence in binary.")
  )
  (list 115822 "Sequence A115821 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115821)))

  (list 115823 "Integers i such that 8*i XOR 13*i = 23*i."
        '(off: 0)
        '(indentries: XORcongruent)
        '(c: "XOR is A003987.")
        '(y: "Seems to be a superset of A115805. A115825 gives the terms not present in A115805."
             " A115824 shows this sequence in binary."
         )
  )
  (list 115824 "Sequence A115823 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115823)))

  (list 115825 "Integers i such that 8*i XOR 13*i = 23*i, but 13*i is not 29 X i."
        '(off: 1)
;;      '(indentries: crossdomain XORcongruent)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720). XOR is given at A003987.")
        '(y: "Setwise difference of A115823 and A115805."
             " A115826 shows this sequence in binary."
         )
  )
  (list 115826 "Sequence A115825 in binary." '(off: 1) '(keywords: "base") '(comps: (007088 115825)))

  (list 115827 "Integers i such that 5*i XOR 6*i = 11*i."
        '(off: 0)
        '(indentries: XORcongruent)
        '(c: "XOR is A003987.")
        '(y: "Seems to be a superset of A115805. A115825 gives the terms not present in A115805."
             " A115828 shows this sequence in binary."
         )
  )
  (list 115828 "Sequence A115827 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115827)))

  (list 115829 "Integers i such that 10*i XOR 11*i = 21*i."
        '(off: 0)
        '(indentries: XORcongruent)
        '(c: "XOR is A003987.")
        '(y:
                            "Differs from A115805 for the first time at n=21,"
                            " where A115805(21)=3072 while a(21)=2979."
                            " A115830 shows this sequence in binary."
         )
  )
  (list 115830 "Sequence A115829 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115829))
        '(y: "Differs from A115806 for the first time at n=21,"
             " where A115806(21)=110000000000 while a(21)=101110100011."
         )
  )

  (list 115831 "Integers i such that 9*i XOR 16*i = 25*i."
        '(off: 0)
        '(indentries: XORcongruent)
        '(c: "XOR is A003987.")
        '(y: "A115832 shows this sequence in binary.")
  )
  (list 115832 "Sequence A115831 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115831)))

  (list 115833 "Integers i such that 16*i XOR 17*i = 33*i."
        '(off: 0)
        '(indentries: XORcongruent)
        '(c: "XOR is A003987.")
        '(y: "A115834 shows this sequence in binary.")
  )
  (list 115834 "Sequence A115833 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115833)))

  (list 115845 "Integers i such that 9*i = 9 X i, i.e. 8*i XOR i = 9*i."
        '(off: 0)
        '(indentries: crossdomain XORcongruent)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720).")
        '(y: "A115846 shows this sequence in binary.")
  )
  (list 115846 "Sequence A115845 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115845)))

  (list 115847 "Integers i such that 17*i = 17 X i, i.e. 16*i XOR i = 17*i."
        '(off: 0)
        '(indentries: crossdomain XORcongruent)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720).")
        '(y: "A115848 shows this sequence in binary. Complement of A115849."
             " Differs from A032966 for the first time at n=25,"
             " where A032966(25)=34 while a(25)=33."
         )
  )
  (list 115848 "Sequence A115847 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115847)))

  (list 115849 "Integers i such that 17*i is not 17 X i."
        '(off: 1)
        '(indentries: crossdomain)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720).")
        '(y: "A115850 shows this sequence in binary. Complement of A115847.")
  )
  (list 115850 "Sequence A115849 in binary." '(off: 1) '(keywords: "base") '(comps: (007088 115849)))


 )
)



(define Feb2006-list-b
 (list

  (list 115859 "Largest natural number m < n, such that there exists non-zero solutions to the cross-domain congruence m*i = n X i, zero if no such integer exists."
        '(off: 1)
        '(upto: 129)
        '(keywords: "hard")
        '(indentries: crossdomain)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720).")
        '(y: "a(2n) = 2*a(n). Bisection A115860 gives only the odd terms.")
  )

  (list 115860 "Odd terms of A115859"
        '(off: 1)
        '(upto: 64)
        '(keywords: "hard")
        '(indentries: crossdomain)
        '(y: "Bisection of A115859.")
  )

  (list 115861 "Number of distinct m's < n, such that there exists non-zero solutions to the cross-domain congruence m*i = n X i, zero if no such m's exist."
        '(off: 1)
        '(upto: 129)
        '(keywords: "hard")
        '(indentries: crossdomain)
  )

  (list 115869 "Smallest natural number m < n, such that there exists non-zero solutions to the cross-domain congruence m*i = n X i, zero if no such integer exists."
        '(off: 1)
        '(upto: 129)
        '(keywords: "hard")
        '(indentries: crossdomain)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720).")
        '(y: "a(2n) = 2*a(n). Bisection A115870 gives only the odd terms.")
  )

  (list 115870 "Odd terms of A115869"
        '(off: 1)
        '(upto: 64)
        '(keywords: "hard")
        '(indentries: crossdomain)
        '(y: "Bisection of A115869.")
  )

  (list 115857 "Smallest integer m > n, such that there exists non-zero solutions to the cross-domain congruence n*i = m X i, zero if no such integer exists."
        '(off: 1)
        '(upto: 127)
        '(keywords: "hard")
        '(indentries: crossdomain)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720).")
        '(y: "a(2n) = 2*a(n). Bisection A115858 gives only the odd terms.")
  )

  (list 115858 "Odd terms of A115857"
        '(off: 1)
        '(upto: 63)
        '(keywords: "hard")
        '(indentries: crossdomain)
        '(y: "Bisection of A115857.")
  )

 )
)


(define Feb2006-list-c
 (list

  (list 115872 "Table where row n gives all non-zero solutions to the cross-domain congruence n*i = A065621(n) X i, zero sequence (A000004) if no such solutions exist."
        '(off: 1)
        '(upto: 105)
        '(keywords: "tabl")
        '(indentries: crossdomain)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720).")
        '(y: "C.f. A115857, A115871. Transpose: A114388. First column: A115873. Rows at positions 2^k are A000027."
             " Row at the position 2n is equal to the row at position n. Some odd-positioned rows are:"
 " Row 1: A000027, Row 3: A048717, Row 5: A115770 (? Checked for all values less than 2^20), Row 7: A115770,"
 " Row 9: A115801, Row 11: A115803, Row 13: A115772, Row 15: A115801 (? Checked for all values less than 2^20),"
 " Row 17: A115809, Row 19: A115874, Row 49: A114384, Row 57: A114386."
         )
  )

  (list 115873 "The first column of A115872."
        '(off: 1)
;;      '(indentries: crossdomain)
        '(c: "a(2^k) = 1, a(2n) = a(n).")
        '(y: "C.f. A114395, 116396.")
  )

  (list 115874 "Integers i such that 19*i = 55 X i."
        '(off: 0)
        '(indentries: crossdomain)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720).")
        '(y: "Row 19 of A115872. Superset of A115876 ? A115875 shows this sequence in binary.")
  )
  (list 115875 "Sequence A115874 in binary." '(off: 0) '(keywords: "base") '(comps: (007088 115874)))

  (list 115876 "Integers i such that 41*i = 105 X i."
        '(off: 0)
        '(upto: 60)
        '(indentries: crossdomain)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720).")
        '(y: "Subset of A115874 ? A115877 shows this sequence in binary.")
  )
  (list 115877 "Sequence A115876 in binary." '(off: 0) '(upto: 60) '(keywords: "base") '(comps: (007088 115876)))

  (list 116384 "Integers i such that 49*i = 81 X i."
        '(off: 0)
        '(upto: 60)
        '(indentries: crossdomain)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720).")
        '(y: "Row 49 of A115872. A114385 shows this sequence in binary.")
  )
  (list 116385 "Sequence A114384 in binary." '(off: 0) '(upto: 60) '(keywords: "base") '(comps: (007088 116384)))

  (list 116386 "Integers i such that 57*i = 73 X i."
        '(off: 0)
        '(upto: 60)
        '(indentries: crossdomain)
        '(c: "Here * stands for ordinary multiplication, and X means carryless (GF(2)[X]) multiplication (A048720).")
        '(y: "Row 57 of A115872. A114387 shows this sequence in binary.")
  )
  (list 116387 "Sequence A114386 in binary." '(off: 0) '(upto: 60) '(keywords: "base") '(comps: (007088 116386)))

  (list 116388 "Transpose of table A115872."
        '(off: 1)
        '(upto: 105)
        '(keywords: "tabl")
        '(y: "First row: A115873.")
  )

  (list 116389 "Bisection of A065621."
        '(off: 1)
        '(keywords: "nonn")
  )

  (list 116390 "A065621(n^2)."
        '(off: 1)
        '(keywords: "nonn")
        '(comps: (065621 000290))
        '(y: "A114391 gives the positions where a(n) is square, A114392 gives the corresponding values (squares), and A114393 gives their square roots.")
  )

  (list 116391 "Positions i where A114390(i) is a square."
        '(off: 1)
        '(keywords: "nonn")
        '(y: "First differences: A114394. A114392 gives the corresponding values.")
  )

  (list 116392 "Squares in A114390, in the order of appearance."
        '(off: 1)
        '(keywords: "nonn")
        '(comps: (116390 116391))
        '(y: "a(n) = A114393(n)^2.")
  )

  (list 116393 "Square-roots of A114392."
        '(off: 1)
        '(keywords: "nonn")
        '(y: "A114392(n) = a(n)^2.")
  )

  (list 116394 "First differences of A114391."
        '(off: 1)
        '(keywords: "nonn")
  )

  (list 116395 "Positions where A115873 obtains first time a value distinct from any of its earlier values."
        '(off: 1)
        '(upto: 95)
        '(keywords: "nonn")
        '(y: "A114396 gives the corresponding values.")
  )

  (list 116396 "Distinct values in A115873, in the order of appearance."
        '(off: 1)
        '(upto: 95)
        '(keywords: "nonn")
        '(comps: (115873 116395))
        '(y: "Same sequence in binary: A114397.")
  )

  (list 116397 "Sequence A114396 in binary." '(off: 1) '(upto: 60) '(keywords: "base") '(comps: (007088 116396)))

  (list 116398 "Positions where A000695 is a square."
        '(off: 0)
        '(upto: 90)
        '(keywords: "nonn")
        '(y: "A114399 gives the corresponding values. C.f. A114401.")
  )

  (list 116399 "Squares in A000695."
        '(off: 0)
        '(upto: 90)
        '(keywords: "nonn")
        '(indentries: GF2X)
        '(comps: (000695 116398) (000290 116400))
        '(y: "Intersection of A000290 and A000695.")
  )

  (list 116400 "Square-roots of A114399."
        '(off: 0)
        '(upto: 90)
        '(keywords: "nonn")
        '(y: "A114399(n) = a(n)^2. C.f. A114401.")
  )

  (list 116401 "a(n) = A114398(n)-A114400(n)."
        '(off: 0)
        '(upto: 90)
        '(keywords: "nonn")
        '(y: "A114402 gives the same sequence divided by 2.")
  )

  (list 116402 "a(n) = A114401(n)/2."
        '(off: 0)
        '(upto: 90)
        '(keywords: "nonn")
  )

 )
)

