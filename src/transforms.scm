;; #lang r5rs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;; transforms.scm -- Basic higher order functions for creating new          ;;
;; integer sequence functions from existing integer sequence functions      ;;
;; via a set of elementary, standard transformations of integer sequences.  ;;
;;                                                                          ;;
;; This module Copyright (C) 2009-2013 Antti Karttunen, subject to GPL v2.  ;;
;;                                                                          ;;
;; Extracted 2012-12-17 from:                                               ;;
;; http://www.iki.fi/~kartturi/matikka/Schemuli/intfun_a.scm                ;;
;;                                                                          ;;
;; Requires memoization.scm (or definech.scm) as in most cases an efficient ;;
;; (or even semi-efficient) implementation is not possible without caching  ;;
;; all the previous values.                                                 ;;
;;                                                                          ;;
;; Transformation functionals implemented here:                             ;;
;;                                                                          ;;
;; (PARTIALSUMS soff1 soff2 Afun)                                           ;;
;;   soff1 = starting offset for the partial sum function to be defined.    ;;
;;   soff2 = starting offset for function Afun, i.e. its domain [soff2,inf] ;;
;;   With argument n, the defined function sums the first (n-soff1)+1       ;;
;;   values of function Afun in domain [soff2,soff2+1,...,soff2+(n-soff1)]  ;;
;;   When n < soff1, the defined function returns 0.                        ;;
;;   Examples:                                                              ;;
;;     (define A000217 (PARTIALSUMS 0 0 A001477))                           ;;
;;     (define A007504 (PARTIALSUMS 1 1 A000040)) ;; Both one-based.        ;;
;;                                                                          ;;
;;                                                                          ;;
;; (MATCHING-POS soff1 soff2 pred_on_i?)                                    ;;
;;   soff1 = starting offset for the partial sum function to be defined.    ;;
;;   soff2 = starting offset for function Afun, i.e. its domain [soff2,inf] ;;
;;   Examples:                                                              ;;
;;     (define A005408 (MATCHING-POS 0 0 odd?)) ;; Slow, but works...       ;;
;;                                                                          ;;
;; (NONZERO-POS soff1 soff2 Afun)                                           ;;
;;   This is equal to:                                                      ;;
;;   (MATCHING-POS soff1 soff2 (lambda (i) (not (zero? (Afun i)))))         ;;
;;   Examples:                                                              ;;
;;     (define A014486 (NONZERO-POS 0 0 A080116)) ;; Slow, but works...     ;;
;;     (define A005117 (NONZERO-POS 1 1 A008683)) ;; Both one-based.        ;;
;;                                                                          ;;
;; (ZERO-POS soff1 soff2 Afun)                                              ;;
;;   This is equal to:                                                      ;;
;;   (MATCHING-POS soff1 soff2 (lambda (i) (zero? (Afun i))))               ;;
;;   Examples:                                                              ;;
;;     (define A218607 (ZERO-POS 0 0 A218603))                              ;;
;;     (define A000079 (ZERO-POS 0 1 (lambda (x) (A004198bi x (- x 1)))))   ;;
;;     [In the latter example we start searching 2's powers from i=1.]      ;;
;;                                                                          ;;
;; (FIXED-POINTS soff1 soff2 Afun)                                          ;;
;;   This is equal to:                                                      ;;
;;   (MATCHING-POS soff1 soff2 (lambda (i) (= i (Afun i))))                 ;;
;;   Examples:                                                              ;;
;;     (define A163901 (FIXED-POINTS 0 0 A163355))                          ;;
;;                                                                          ;;
;;                                                                          ;;
;; (NEXT-MATCHING-POS pred_on_i?)                                           ;;
;;   The function to be defined, Anew, returns for (Anew n) the next i>n    ;; 
;;   for which (pred_on_i? i) returns non-false.                            ;;
;;   Examples:                                                              ;;
;;     (define A151800 (NEXT-MATCHING-POS (lambda (i) (= 1 (A010051 i)))))  ;;
;;                                                                          ;;
;; (PREV-OR-SAME-MATCHING-POS pred_on_i?)                                   ;;
;;   The function to be defined, Anew, returns for (Anew n) the next i<=n   ;; 
;;   for which (pred_on_i? i) returns non-false                             ;;
;;   and zero if no such i exists.                                          ;;
;;   Examples:                                                              ;;
;;     (define A007917 ;; Starting offset=2                                 ;;
;;             (PREV-OR-SAME-MATCHING-POS (lambda (i) (= 1 (A010051 i))))   ;;
;;     )                                                                    ;;
;;                                                                          ;;
;;                                                                          ;;
;; (DISTINCT-POS soff1 soff2 Afun)                                          ;;
;;   soff1 = starting offset for the function to be defined.                ;;
;;   soff2 = starting offset for function Afun, its domain is [soff2,inf]   ;;
;;   With argument n, the defined function returns the first i for which    ;;
;;   (Afun i) has for the first time obtained n distinct values since       ;;
;;   (Afun soff2)                                                           ;;
;;   When n = soff1, the defined function returns soff2.                    ;;
;;   Examples:                                                              ;;
;;     (define A022342 (DISTINCT-POS 1 0 A219641))                          ;;
;;     (define A210719 (DISTINCT-POS 1 1 A000010))                          ;;
;;                                                                          ;;
;; (DISTINCT-VALS soff1 soff2 Afun)                                         ;;
;;   This is equal to:                                                      ;;
;;   (COMPOSE Afun (DISTINCT-POS soff1 soff2 Afun))                         ;;
;;   Examples:                                                              ;;
;;     (define A090127 (DISTINCT-VALS 1 1 A000010))                         ;;
;;                                                                          ;;
;;                                                                          ;;
;; (RECORD-POS soff1 soff2 Afun)                                            ;;
;;   Examples:                                                              ;;
;;   (define A006005 (RECORD-POS 1 1 A000010))                              ;;
;;                                                                          ;;
;; (RECORD-VALS soff1 soff2 Afun)                                           ;;
;;   This is equal to:                                                      ;;
;;   (COMPOSE Afun (RECORD-POS soff1 soff2 Afun))                           ;;
;;   Examples:                                                              ;;
;;   (define A006093 (RECORD-VALS 1 1 A000010))                             ;;
;;                                                                          ;;
;; (LEAST-I-WITH-FUN-I-EQ-N soff1 soff2 Afun)                               ;;
;;   soff1 = starting offset for this function to be defined. (not used).   ;; 
;;   soff2 = starting offset for fun_on_i (i.e. its domain is [soff2,inf]   ;;
;;   The new function returns the smallest i, such that (Afun i) = n.       ;;
;;   Note: Afun doesn't need to be monotone.                                ;;
;;   Examples:                                                              ;;
;;   (define A166087 (LEAST-I-WITH-FUN-I-EQ-N 0 0 A166040))                 ;;
;;                                                                          ;;
;; (LEAST-GTE-I soff1 soff2 Afun)                                           ;;
;;   soff1 = starting offset for the function to be defined.                ;;
;;   soff2 = starting offset for function Afun, its domain is [soff2,inf]   ;;
;;   Note: Afun doesn't need to be monotone!                                ;;
;;   Returns the smallest i, such that (Afun i) >= n.                       ;;
;;                                                                          ;;
;; (LEAST-EXCEEDING-I soff1 soff2 Afun)                                     ;;
;;   soff1 = starting offset for the function to be defined.                ;;
;;   soff2 = starting offset for function Afun, its domain is [soff2,inf]   ;;
;;   Note: Afun must be monotone!                                           ;;
;;   Returns the smallest i, such that (Afun i) > n.                        ;;
;;                                                                          ;;
;;                                                                          ;;
;; (PSEUDOINVERSE1 soff1 soff2 Afun)                                        ;;
;;   soff1 = starting offset for the function to be defined.                ;;
;;   soff2 = starting offset for function Afun, its domain is [soff2,inf]   ;;
;;   Returns the smallest such i, that (Afun i) = n, if such i exists,      ;;
;;   otherwise the largest i such that (Afun i) < n.                        ;;
;;                                                                          ;;
;;                                                                          ;;
;; (PSEUDOINVERSE2 soff1 soff2 Afun)                                        ;;
;;   soff1 = starting offset for the function to be defined.                ;;
;;   soff2 = starting offset for function Afun, its domain is [soff2,inf]   ;;
;;   Note: Afun must be monotone!                                           ;;
;;   Returns the largest i, such that (Afun i) <= n.                        ;;
;;   This is equal to:                                                      ;;
;;   (COMPOSE -1+ (LEAST-EXCEEDING-I soff1 soff2 Afun))                     ;;
;;                                                                          ;;
;; (COMPLEMENT soff Afun)                                                   ;;
;;   soff = starting offset for both Afun and the function to be defined.   ;;
;;   Note: Afun must be monotone!                                           ;;
;;   The defined function returns all the natural numbers                   ;;
;;   which do not occur in Afun's range.                                    ;;
;;                                                                          ;;
;;                                                                          ;;
;; (EIGEN-CONVOLUTION firstvals mulfun)                                     ;;
;;   firstvals: if an integer, specifies what is the value of defined       ;;
;;              function at point n=0.                                      ;;
;;              if a list, then gives the initial values for the function   ;;
;;              to be defined, for n=0, n=1, ... n=k,                       ;;
;;              where k = (length firstvals)-1                              ;;
;;   Examples:                                                              ;;
;;   (define A007460 (EIGEN-CONVOLUTION 1 A003986bi)) ;; For OR.            ;;
;;   (define A007461 (EIGEN-CONVOLUTION 1 A004198bi)) ;; For AND.           ;;
;;   (define A007462 (EIGEN-CONVOLUTION '(0 1) A003987bi)) ;; For XOR.      ;;
;;   (define A007463 (EIGEN-CONVOLUTION 1 lcm)) ;; For LCM.                 ;;
;;   (define A007464 (EIGEN-CONVOLUTION 1 gcd)) ;; For GCD.                 ;;
;;                                                                          ;;
;;                                                                          ;;
;; (GEN-CONVOLVE soff mulfun Afun1 Afun2)                                   ;;
;;   soff = starting offset for both Afun1 and AFun2, and also of the       ;;
;;   function to be defined.                                                ;;
;;   The defined function Anew computes (Anew n) as                         ;;
;;   Sum_{i=soff..n} (mulfun (Afun1 i) (Afun2 (+ soff (- n i))))            ;;
;;   mulfun doesn't need to be commutative, as there is no attempt to       ;;
;;   optimize half of the loop away.                                        ;;
;;                                                                          ;;
;; (CONVOLVE soff Afun1 Afun2)                                              ;;
;;   This is equal to:                                                      ;;
;;   (GEN-CONVOLVE soff * Afun1 Afun2))                                     ;;
;;   Examples:                                                              ;;
;;   (define A090826 (CONVOLVE 0 A000045 A000108))                          ;;
;;                                                                          ;;
;; (INVERT Afun)                                                            ;;
;;   starting offset for both Afun and the function to be defined is 1.     ;;
;;   The defined function Anew computes (Anew n) as                         ;;
;;   (Afun n) + Sum_{i=1..n-1} (* (Anew i) (Afun (- n i)))                  ;;
;;   The name "INVERT" comes from the generating-functionological fact      ;;
;;   how the g.f.'s of Afun and Anew are related to each other.             ;;
;;                                                                          ;;
;;                                                                          ;;
;; (RECORD-ABSVALS-BETWEEN-ZEROS-POS Afun zeroposfun)                       ;;
;;   The defined function finds a point i between zeroposfun(n) and         ;;
;;   zeroposfun(n+1) with a maximum absolute value of fun(i).               ;;
;;   Both zeroposfun and the function to be defined should have the same    ;;
;;   starting offset.                                                       ;;
;;                                                                          ;;
;;                                                                          ;;
;; (NUMBER-OF-CHANGES soff Afun)                                            ;;
;;   soff = starting for both Afun and the function to be defined.          ;;
;;   The defined function, gives a natural number one more than the number  ;;
;;   of times the value of function Afun has changed when going from        ;;
;;   (Afun i) to (Afun i+1), for i=soff to n-1.                             ;;
;;   For injective functions the resulting function is always A000027.      ;;
;;                                                                          ;;
;;                                                                          ;;
;;                                                                          ;;
;;  To do:                                                                  ;;
;;                                                                          ;;
;;  Implement more transformations mentioned in                             ;;
;;   http://oeis.org/wiki/Sequence_transforms                               ;;
;;                                                                          ;;
;;                                                                          ;;
;;  CHANGE LOG:                                                             ;;
;;                                                                          ;;
;;  2013-01-01 karttu The first version which is checked in to github.      ;;
;;             Extracted and cleaned a little from the code in              ;;
;;             http://www.iki.fi/~kartturi/matikka/Schemuli/intfun_a.scm    ;;
;;             although there are still inconsistencies, especially what    ;;
;;             comes to the starting offsets.                               ;;
;;             (Also, in some cases we would do better without memoization) ;;
;;             This code is still intended just for MIT/GNU Scheme,         ;;
;;             although it should work with a little editing also in        ;;
;;             other Schemes, like in Racket for example.                   ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (usual-integrations)) ;; This for MIT/GNU Scheme compilation.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Something we need a lot:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (1+ n) (+ 1 n))  ;; These are built-in functions in MIT/GNU Scheme.
;; (define (-1+ n) (- n 1))


(define (COMPOSE . funlist) ;; A macro would produce faster code...
 (cond ((null? funlist) (lambda (x) x))
       (else (lambda (x) ((car funlist) ((apply COMPOSE (cdr funlist)) x))))
 )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; soff1 = starting offset for this function to be defined.
;; soff2 = starting offset for function fun, i.e. its domain: [soff2,infinity]
;; With argument n, the defined function sums the first (n-soff1)+1 values
;; of function 'fun' in domain [soff2,soff2+1,soff2+2,...,soff2+(n-soff1)]
(define (PARTIALSUMS soff1 soff2 fun) ;; soff = starting offset.
 (implement-cached-function 0 (partsumsfun n)
  (let ((coff (- soff2 soff1))) ;; Correction offset.
   (cond ((< n soff1) 0)
         ((= n soff1) (fun soff2))
         (else (+ (fun (+ n coff)) (partsumsfun (-1+ n))))
   )
  )
 )
)

;; soff1 = starting offset for this function to be defined.
;; soff2 = starting offset for pred_on_i? function,
(define (MATCHING-POS soff1 soff2 pred_on_i?)
 (implement-cached-function 0 (tvimadur n)
   (let loop ((i (if (= soff1 n) soff2 (1+ (tvimadur (-1+ n))))))
         (cond ((pred_on_i? i) i)
               (else (loop (1+ i)))
         )
   )
 )
)



(define (NEXT-MATCHING-POS pred_on_i?)
 (implement-cached-function 0 (tvimadur n)
   (let loop ((i (1+ n)))
         (cond ((pred_on_i? i) i)
               (else (loop (1+ i)))
         )
   )
 )
)


(define (PREV-OR-SAME-MATCHING-POS pred_on_i?)
 (implement-cached-function 0 (tvimadur n)
   (let loop ((i n))
         (cond ((zero? i) i) ;; Failed?
               ((pred_on_i? i) i)
               (else (loop (-1+ i)))
         )
   )
 )
)



;; Give an "extended" characteristic function of foo for this, and you get foo:
;; E.g. (define A005117 (NONZERO-POS 1 1 A008683))

;; soff1 = starting offset for this function to be defined.
;; soff2 = starting offset for charfun (characteristic function)

(define (NONZERO-POS soff1 soff2 charfun) (MATCHING-POS soff1 soff2 (lambda (i) (not (zero? (charfun i))))))

(define (ZERO-POS soff1 soff2 fun) (MATCHING-POS soff1 soff2 (lambda (i) (zero? (fun i)))))

(define (FIXED-POINTS soff1 soff2 fun) (MATCHING-POS soff1 soff2 (lambda (i) (= i (fun i)))))

;; soff1 = starting offset for this function to be defined.
;; soff2 = starting offset for fun_on_i (i.e. its domain is [soff2,infinity])
(define (DISTINCT-POS soff1 soff2 fun_on_i)
 (implement-cached-function 0 (belgthor n)
   (cond ((<= n soff1) soff2)
         (else
            (let outloop ((i (1+ (belgthor (-1+ n))))
                          (val_here (fun_on_i (1+ (belgthor (-1+ n)))))
                         )
              (let inloop ((j (-1+ n))) ;; ((j (-1+ i)))
;; If we didn't find any j < i where fun_on_i(belgthor(j)) would have been belgthor(i), then ...
                    (cond ((< j 0) i) ;; ... we found a new distinct value, return its pos.
                          ((= (fun_on_i (belgthor j)) val_here) ;; This value has occurred before.
                                 (outloop (+ i 1) (fun_on_i (+ i 1))) ;; Try the next candidate.
                          )
                          (else (inloop (- j 1)))
                    )
              )
            )
         )
   )
 )
)

(define (DISTINCT-VALS soff1 soff2 fun_on_i) (COMPOSE fun_on_i (DISTINCT-POS soff1 soff2 fun_on_i)))

;; soff1 = starting offset for this function to be defined.
;; soff2 = starting offset for fun_on_i (i.e. its domain is [soff2,infinity])
(define (RECORD-POS soff1 soff2 fun_on_i)
 (implement-cached-function 0 (arlaug n)
   (cond ((<= n soff1) soff2) ;; Was:  ((<= n soff) n)
         (else
            (let* ((prevrecpos (arlaug (- n 1)))
                   (prev_record (fun_on_i prevrecpos))
                  )
               (let loop ((i (+ 1 prevrecpos))) ;; Starting index.
                     (cond ((> (fun_on_i i) prev_record) i)
                           (else (loop (+ i 1)))
                     )
               )
            )
         )
   )
 )
)

(define (RECORD-VALS soff1 soff2 fun_on_i) (COMPOSE fun_on_i (RECORD-POS soff1 soff2 fun_on_i)))


;;
;; (arlaug n): Find a point i between zeroposfun(n) and zeroposfun(n+1) with
;; a maximum absolute value of fun(i).
;;

(define (RECORD-ABSVALS-BETWEEN-ZEROS-POS Afun zeroposfun)
 (implement-cached-function 0 (arlaug n)
            (let* ((nextzeropos (zeroposfun (1+ n))))
               (let loop ((i (zeroposfun n)) ;; Starting index.
                          (m 0)
                          (mp (zeroposfun n))
                         )
                     (cond ((= i nextzeropos) mp)
                           ((> (abs (Afun i)) m)
                                 (loop (1+ i) (abs (Afun i)) i)
                           )
                           (else (loop (1+ i) m mp)) 
                     )
               )
            )
 )
)


;; This forms a function (cfun n), that gives the number of times (+ 1 !) the value of function Afun
;; has changed from (Afun i) to (Afun i+1), for i=soff to n-1.
;; For genuinely monotone functions this is always A000027.
(define (NUMBER-OF-CHANGES soff Afun) ;; soff = starting offset.
 (implement-cached-function 0 (cfun n)
    (cond ((< n soff) 0) ;; Maybe we should raise an error instead?!
          ((= soff n) 1) ;; For the starting offset we return 1, as there is our first value.
          ((= (Afun n) (Afun (- n 1))) (cfun (- n 1))) ;; Afun stays same,  use the previous value.
          (else (1+ (cfun (- n 1)))) ;; Afun obtains a new value here, return one more than last time
    )
 )
)


;; (PARSUMS_OF_CHARFUN soff Afun):
;; Returns the partial sums of the characteristic function of Afun, which should be growing,
;; but not necessarily genuinely so.
;; In other words, how many distinct values function Afun has obtained up to and including (Afun n).

;; E.g. (define A000720 (PARSUMS_OF_CHARFUN 1 A000040))

;; A072649 = (COMPOSE -1+ (PARSUMS_OF_CHARFUN 0 A000045))


;; With LEAST-I-WITH-FUN-I-EQ-N we don't assume that Afun is monotone.
;; smallest i, such that (Afun i) = n.
;; soff1 = starting offset for this function to be defined. (not used).
;; soff2 = starting offset for Afun (i.e. its domain is [soff2,infinity])
(define (LEAST-I-WITH-FUN-I-EQ-N soff1 soff2 Afun) ;; soff = starting offset.
 (implement-cached-function 0 (fun_defined n)
     (let loop ((i soff2))
            (cond ((= (Afun i) n) i)
                  (else (loop (+ i 1)))
            )
     )
 )
)


;; smallest i, such that (Afun i) >= n.
;; soff1 = starting offset for this function to be defined.
;; soff2 = starting offset for Afun (i.e. its domain is [soff2,infinity])
(define (LEAST-GTE-I soff1 soff2 Afun) ;; soff = starting offset.
 (implement-cached-function 0 (fun_defined n)
     (let loop ((i (if (= soff1 n) soff2 (fun_defined (- n 1)))))
            (cond ((>= (Afun i) n) i)
                  (else (loop (+ i 1)))
            )
     )
 )
)


;; Note: With the following it is required that Afun is monotone!
;; smallest i, such that (Afun i) > n.
;; soff1 = starting offset for this function to be defined.
;; soff2 = starting offset for Afun (i.e. its domain is [soff2,infinity])
(define (LEAST-EXCEEDING-I soff1 soff2 Afun) ;; soff = starting offset.
 (implement-cached-function 0 (fun_defined n)
     (let loop ((i (if (= soff1 n) soff2 (fun_defined (- n 1)))))
            (cond ((> (Afun i) n) i)
                  (else (loop (+ i 1)))
            )
     )
 )
)


(define (first_pos_with_funs_val_gte fun n)
   (let loop ((i 0))
      (if (>= (fun i) n) i
          (loop (1+ i))
      )
   )
)



(define (first-n-where-fun_n-is-i0 fun i)
   (let loop ((n 0))
     (cond ((= i (fun n)) n)
           (else (loop (+ n 1)))
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

;; When for some i, (Afun i) = n, return the smallest such i,
;; otherwise the largest i such that (Afun i) < n.
;; soff1 = starting offset for this function to be defined.
;; soff2 = starting offset for Afun (i.e. its domain is [soff2,infinity])
(define (PSEUDOINVERSE1 soff1 soff2 Afun)
 (implement-cached-function 0 (fun_defined n)
     (let loop ((i (if (= soff1 n) soff2 (fun_defined (- n 1)))))
            (cond ((= (Afun i) n) i)
                  ((> (Afun i) n) (- i 1))
                  (else (loop (+ i 1)))
            )
     )
 )
)


;; Returns the largest i, such that (Afun i) <= n.
;; soff1 = starting offset for this function to be defined.
;; soff2 = starting offset for Afun (i.e. its domain is [soff2,infinity])
(define (PSEUDOINVERSE2 soff1 soff2 Afun) (COMPOSE -1+ (LEAST-EXCEEDING-I soff1 soff2 Afun)))

;; Not yet correct:
;; (define (PARSUMS_OF_CHARFUN soff Afun) ;; soff = starting offset.
;;  (implement-cached-function 0 (cfun n)
;;     (cond ((< n soff) 0)
;;           ((= soff n) (if (> (Afun soff) soff) 0 1))
;;           (else
;;              (let ((preval (cfun (- n 1))))
;;                 (if (= (Afun (+ 1 soff preval)) n) (+ 1 preval) preval)
;;              )
;;           )
;;     )
;;  )
;; )

;;                              0,1,2,3,4,5,6,7 , 8,
;; A000045 begins from zero as: 0,1,1,2,3,5,8,13,21,34,55,89,144,...
;; A001477:                     0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,...
;; Characteristic function:     1,1,1,1,0,1,0,0,1,0 ,0, 0, 0, 1, 0, 0, 0
;; Its partial sums:            1,2,3,4,4,5,5,5,6,6, 6, 6, 6, 7, 7, 7, 7
;; "pseudo inverse 1":          0,1,3,4,4,5,5,5,6,6, 6, 6, 6, 7, 7, 7, 7
;; "pseudo inverse 2":          0,2,3,4,4,5,5,5,6,6, 6, 6, 6, 7, 7, 7, 7

;; A125975 begins from zero as: 0,1,2,3,5,7,10,11,12,13,15,21,31,38,39,42,43,44,...
;; A001477:                     0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,...
;; Characteristic function:     1,1,1,1,0,1,0,1,0,0, 1, 1, 1, 1, 0, 1, 0,
;; Its partial sums:            1,2,3,4,4,5,5,6,6,6, 7, 8, 9,10,10,11,11,
;; "pseudo inverse":            0,1,2,3,3,4,4,5,5,5, 6, 7, 8, 9, 9,10,10,...

;; A000040 begins from one as:  2,3,5,7,11,13,17,19,23,29,31,,...
;; A000027:                     1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,...
;; "pseudo inverse": (A000720)  0,1,2,2,3,3,4,4,4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 8, 8, 8,


;; A000523 begins from one as:  0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,4,4
;; Characteristic function:     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,... (all-ones, A000012)
;; Its partial sums = A000027:  1,2,3,4,5,6,7,8,9,10,11,...

;; A000079 begins from zero as: 1,2,4,8,16,32,64,128,256,512,1024,2048,4096,...
;; A001477:                     0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,...
;; Characteristic function:     0,1,1,0,1,0,0,0,1,0, 0, 0, 0, 0, 0, 0, 1, 0,...
;; Its partial sums:            0,1,2,2,3,3,3,3,4,4, 4, 4, 4, 4, 4, 4, 5, 5, ...

;; A000079 begins from one as:  2,4,8,16,32,64,128,256,512,1024,2048,4096,...
;; A001477:                     1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,...
;; Characteristic function:     0,1,0,1,0,0,0,1,0, 0, 0, 0, 0, 0, 0, 1, 0,...
;; Its partial sums:            0,1,1,2,2,2,2,3,3, 3, 3, 3, 3, 3, 3, 4, 4, ...


;; (define (PARSUMS_OF_CHARFUN soff Afun) ;; soff = starting offset.
;;  (implement-cached-function 0 (cfun n)
;;     (cond ((< n soff) 0)
;;           ((= soff n) (if (> (Afun soff) soff) 0 1))
;;           (else
;;              (let ((preval (cfun (- n 1))))
;;                 (if (= (Afun (+ 1 soff preval)) n) (+ 1 preval) preval)
;;              )
;;           )
;;     )
;;  )
;; )


;; Afun should be a monotone function, we return the v which do not occur in its range.
;; Is there a way to do this with one cached function less?

(define (COMPLEMENT soff Afun) ;; soff = starting offset.
  (let ((inv (PSEUDOINVERSE2 soff soff Afun)))
    (MATCHING-POS soff soff
      (lambda (i) (let ((inv_i (inv i))) (or (< inv_i soff) (not (= i (Afun inv_i))))))
    )
  )
)



;; The overriding of the initial values should be implemented in implement-cached-function,
;; so that there would not be any overhead at the runtime.
;; (They would be written directly to the cache, when it is initialized!)

(define (EIGEN-CONVOLUTION firstvals mulfun) ;; These begin always from zero.
 (implement-cached-function 0 (fun_defined n)
   (cond ((and (integer? firstvals) (= 0 n)) firstvals)
         ((and (list? firstvals) (< n (length firstvals))) (list-ref firstvals n))
         (else
             (let loop ((s 0) (i 0) (j (- n 1)))
                  (if (>= i j)
                      (+ (* 2 s) (if (= i j) (let ((c (fun_defined i))) (mulfun c c)) 0))
                      (loop (+ s (mulfun (fun_defined i) (fun_defined j))) (1+ i) (-1+ j))
                  )
             )
         )
   )
 )
)

(define (GEN-CONVOLVE soff mulfun Afun1 Afun2)
 (implement-cached-function 0 (fun_defined n)
     (let loop ((s 0) (i soff) (j n))
          (if (< j soff) s
              (loop (+ s (mulfun (Afun1 i) (Afun2 j))) (1+ i) (-1+ j))
          )
     )
 )
)

(define (CONVOLVE soff Afun1 Afun2) (GEN-CONVOLVE soff * Afun1 Afun2))


;; Didn't realize that it is this easy. See Joshua Zucker's mail
;; "Re: [SeqFan]: Repeated iterations of INVERT starting from A019590 ?"
;; on SeqFan-mailing list, Jun 7, 2006 6:46 PM

(define (INVERT Afun) ;; Always one-based!
 (implement-cached-function 0 (fun_defined n)
   (+ (Afun n) ;; The whole space can be filled by (Afun n) objects of size n.
      (let loop ((s 0) (i 1) (j (- n 1))) ;; In how many ways smaller parts can be filled?
           (if (< j 1) s
               (loop (+ s (* (fun_defined i) (Afun j))) (1+ i) (-1+ j))
           )
      )
   )
 )
)



