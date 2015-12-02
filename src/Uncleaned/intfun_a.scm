
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;  http://www.iki.fi/~kartturi/matikka/Schemuli/intfuns1.scm             ;;
;;  - Often needed integer functions.                                     ;;
;;                                                                        ;;
;;  Coded by Antti Karttunen (my_firstname.my_surname@gmail.com),         ;;
;;   2002-2014                                                            ;;
;;                                                                        ;;
;;  This Scheme-code is in Public Domain and runs (at least)              ;;
;;  in MIT Scheme Release 7.6.0/7.7.?, for which one can find documents   ;;
;;  and the pre-compiled binaries (for various OS's running in            ;;
;;  Intel x86 architecture) under the URL:                                ;;
;;  http://www.swiss.ai.mit.edu/projects/scheme/                          ;;
;;                                                                        ;;
;;  Last edited  Dec 02 2015 by Antti Karttunen.                          ;;
;;  Last version before starting break-up to smaller modules.             ;;
;;                                                                        ;;
;;  PSEUDOINVERSE2 is now LEFTINV-LEASTMONO                               ;;
;;                                                                        ;;
;;  Oct 02 2009: Changed macros MATCHING-POS, NONZERO-POS, ZERO-POS,      ;;
;;  DISTINCT-POS, DISTINCT-VALS, RECORD-POS, RECORDS-VALS, PARTIALSUMS    ;;
;;  to use TWO separate starting offsets, the other for the function to   ;;
;;  be defined, and the other for the function whose values are           ;;
;;  searched/summed for.                                                  ;;
;;                                                                        ;;
;;  Oct 05 2009: Changed macros LEAST-EXCEEDING-I, PSEUDOINVERSE1,        ;;
;;  LEFTINV-LEASTMONO in similar way, to use soff1 and soff2.             ;;
;;                                                                        ;;
;;  May 19 2015: Created a new variant LEFTINV-LEASTMONO-NC2NC            ;;
;;  which uses binary search algorithm to find an inverse for any         ;;
;;  monotonic injective function, preferably one which is not cached      ;;
;;  itself, and is computed quickly (like e.g. A000217 and A000290).      ;;
;;  This is now used for the definitions of A000196 & A003056             ;;
;;  to avoid use of sqrt, which is now explicitly disabled here.          ;;
;;  (Note: this is probably far from optimal for any specific sequence,   ;;
;;   as say, when compared to Newton's iteration method for A000196       ;;
;;   but at least it's generic enough to work with many functions.)       ;;
;;                                                                        ;;
;;  Refactored implement-cached-function macro out of definec,            ;;
;;  and used it to implement memoizing functionals                        ;;
;;  MATCHING-POS, NONZERO-POS,                                            ;;
;;  DISTINCT-POS, DISTINCT-VALS, RECORD-POS, RECORDS-VALS,                ;;
;;  NUMBER-OF-CHANGES, LEAST-EXCEEDING-I, PSEUDOINVERSE1                  ;;
;;  LEFTINV-LEASTMONO and COMPLEMENT.                                     ;;
;;  These are still first-cut, to be polished. Need better names for some.;;
;;                                                                        ;;
;;  Added Jan 08 2007 a few sieving functions and eigen-convolutions.     ;;
;;  (Operators: EIGEN-CONVOLUTION, GEN-CONVOLVE, CONVOLVE, INVERT).       ;;
;;                                                                        ;;
;;  grep -c "^(define[c]* [(]*A[0-9][0-9][0-9][0-9][0-9][0-9] " --> 226   ;;
;;                                                                        ;;
;;  To do:                                                                ;;
;;                                                                        ;;
;;  Implement most of the transformations in                              ;;
;;   http://www.research.att.com/~njas/sequences/transforms.txt           ;;
;;                                                                        ;;
;;  Implement all OEIS-sequences (functions) related to the               ;;
;;  binary fiddling.                                                      ;;
;;  All http://www.research.att.com/~njas/sequences/Sindx_Bi.html#binary  ;;
;;  and much more:                                                        ;;
;; A000120, A000788, A000069, A001969, A023416, A059015, A007088, A070939 ;;
;; A005536, A003159, A006995, A006364, A054868, A070940, A070941, A001511 ;;
;; A029837, A037800, A014081, A014082                                     ;;
;;                                                                        ;;
;;  Also all 156 or so "core" sequences, at least those that can be       ;;
;;  easily computed. (Cf. A000001), the most cross-referenced,            ;;
;;  etc.  (Huh huh!)                                                      ;;
;;                                                                        ;;
;;  But first think about the appropriate presentation, with which we     ;;
;;  can keep more information than with just basic define and definec.    ;;
;;                                                                        ;;
;;  E.g. the starting offset, the validity range, if the sequence         ;;
;;  is finite (cf. A007623), alternative versions and compositions        ;;
;;  (E.g. we have a _DEFINING_ definition (might be very SLOW),           ;;
;;  then _IMPLEMENTING_ definition (should be faster)                     ;;
;;  and various alternative compositions and other definitions,           ;;
;;  that can be used for checking.)                                       ;;
;;  Also, what subset of Scheme is allowed, and what kind of operators    ;;
;;  (PARTIALSUMS, COMPLEMENT, INVERT, etc.) are allowed,                  ;;
;;  so that definitions can be automatically converted to say, Python?    ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Compile as:
;;
;; (cf "../Schemuli/definech" "../Schemuli/")
;; 
;; (load  "../Schemuli/definech")
;; ;Loading "matikka\\schemuli\\definech.com" -- done
;; ;Value: definec
;; 
;; (fluid-let ((sf/default-syntax-table user-initial-environment))
;;   (cf "../Schemuli/intfuns1" "../Schemuli/")
;; )
;; 

;; From http://www.swiss.ai.mit.edu/projects/scheme/documentation/user_5.html
;;
;; If you redefine some global name in your code, for example, car, cdr
;; and cons, you should indicate it in the declaration as:
;; (declare (usual-integrations car cdr cons))

;; (Beware of using an argument name like list in the function definitions?)

(declare (usual-integrations))

(load-option 'format)


(define COMPOSE compose-funs)
;; All macros related to caching transferred to definech.scm



(define (ROWSUMS0 antidiagfun) (lambda (n) (add antidiagfun (A000217 n) (-1+ (A000217 (1+ n))))))

(define (ROWSUMS1 antidiagfun) (lambda (n) (add antidiagfun (A000124 (-1+ n)) (A000217 n))))


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

;; Original version:
;; (define (PARTIALSUMS soff fun) ;; soff = starting offset.
;;  (implement-cached-function 0 (partsumsfun n)
;;    (cond ((< n soff) 0)
;;          ((= n soff) (fun n))
;;          (else (+ (fun n) (partsumsfun (-1+ n))))
;;    )
;;  )
;; )


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

;; Original version:
;; (define (MATCHING-POS soff pred_on_i?) ;; soff = starting offset.
;;  (implement-cached-function 0 (tvimadur n)
;;    (let loop ((i (if (= soff n) soff (1+ (tvimadur (-1+ n))))))
;;          (cond ((pred_on_i? i) i)
;;                (else (loop (1+ i)))
;;          )
;;    )
;;  )
;; )


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

;; (define-syntax define-MATCHING-POS
;;   (syntax-rules ()
;;    ((define-MATCHING-POS soff (fun_defined n) pred_on_i?)
;;       (define fun_defined
;;         (implement-cached-function 0 (fun_defined n)
;;            (let loop ((i (if (= soff n) soff (1+ (fun_defined (-1+ n))))))
;;                  (cond ((pred_on_i? i) i)
;;                        (else (loop (1+ i)))
;;                  )
;;            )
;;         )
;;       ) ;; (define fun_defined ...)
;;    )
;;   ) ;; syntax-rules
;; )
;;
;; (define-MATCHING-POS 0 (A125975 n) (lambda (i) (= i (A125974 (A125974 i)))))
;;

(define (fun-succ-matching-is0 pred_on_i?) (MATCHING-POS 0 0 pred_on_i?))

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
   (cond ((<= n soff1) soff2) ;; Was:  ((<= n soff) n)
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

(define (DISTINCT-VALS soff1 soff2 fun_on_i) (compose-funs fun_on_i (DISTINCT-POS soff1 soff2 fun_on_i)))

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

(define (RECORD-VALS soff1 soff2 fun_on_i) (compose-funs fun_on_i (RECORD-POS soff1 soff2 fun_on_i)))


;;
;; (arlaug n): Find a point i between zeroposfun(n) and zeroposfun(n+1) with
;; a maximum absolute value of fun(i).
;;

(define (RECORD-ABSVALS-BETWEEN-ZEROS-POS soff fun zeroposfun) ;; soff = starting offset.
 (implement-cached-function 0 (arlaug n)
            (let* ((nextzeropos (zeroposfun (1+ n))))
               (let loop ((i (zeroposfun n)) ;; Starting index.
                          (m 0)
                          (mp (zeroposfun n))
                         )
                     (cond ((= i nextzeropos) mp)
                           ((> (abs (fun i)) m)
                                 (loop (1+ i) (abs (fun i)) i)
                           )
                           (else (loop (1+ i) m mp)) 
                     )
               )
            )
 )
)


;; This forms a function (cfun n), that gives the number of times the value of function foo
;; has changed from (foo i) to (foo i+1), for i=soff to n-1.
;; For genuinely monotone functions this is always A000027.
(define (NUMBER-OF-CHANGES soff foo) ;; soff = starting offset.
 (implement-cached-function 0 (cfun n)
    (cond ((< n soff) 0) ;; Maybe we should raise an error instead?!
          ((= soff n) 1) ;; For the starting offset we return 1, as there is our first value.
          ((= (foo n) (foo (- n 1))) (cfun (- n 1))) ;; foo stays same,  use the previous value.
          (else (1+ (cfun (- n 1)))) ;; Foo obtains a new value here, return one more than last time
    )
 )
)


;; (define (GEN_CONVOLVE soff mulfun fun1 fun2) ...)
;; (define (CONVOLVE soff fun1 fun2) ...)


;; (PARSUMS_OF_CHARFUN soff foo):
;; Returns the partial sums of the characteristic function of foo, which should be growing,
;; but not necessarily genuinely so.
;; In other words, how many distinct values function foo has obtained up to and including (foo n).

;; E.g. (define A000720 (PARSUMS_OF_CHARFUN 1 A000040))

;; A072649 = (compose-funs -1+ (PARSUMS_OF_CHARFUN 0 A000045))


;; With LEAST-I-WITH-FUN-I-EQ-N we don't assume that fun_on_i is monotone.
;; smallest i, such that (foo i) = n.
;; soff1 = starting offset for this function to be defined. (not used).
;; soff2 = starting offset for fun_on_i (i.e. its domain is [soff2,infinity])
(define (LEAST-I-WITH-FUN-I-EQ-N soff1 soff2 fun_on_i) ;; soff = starting offset.
 (implement-cached-function 0 (fun_defined n)
     (let loop ((i soff2))
            (cond ((= (fun_on_i i) n) i)
                  (else (loop (+ i 1)))
            )
     )
 )
)


;; Note: With the following it is required that fun_on_i is monotone!
;; smallest i, such that (foo i) > n.
;; soff1 = starting offset for this function to be defined.
;; soff2 = starting offset for fun_on_i (i.e. its domain is [soff2,infinity])
(define (LEAST-EXCEEDING-I soff1 soff2 fun_on_i) ;; soff = starting offset.
 (implement-cached-function 0 (fun_defined n)
     (let loop ((i (if (= soff1 n) soff2 (fun_defined (- n 1)))))
            (cond ((> (fun_on_i i) n) i)
                  (else (loop (+ i 1)))
            )
     )
 )
)

;; smallest i, such that (foo i) >= n.
;; soff1 = starting offset for this function to be defined.
;; soff2 = starting offset for fun_on_i (i.e. its domain is [soff2,infinity])
(define (LEAST-GTE-I soff1 soff2 fun_on_i) ;; soff = starting offset.
 (implement-cached-function 0 (fun_defined n)
     (let loop ((i (if (= soff1 n) soff2 (fun_defined (- n 1)))))
            (cond ((>= (fun_on_i i) n) i)
                  (else (loop (+ i 1)))
            )
     )
 )
)



;; Was:
;; (define (LEAST-EXCEEDING-I soff fun_on_i) ;; soff = starting offset.
;;  (implement-cached-function 0 (fun_defined n)
;;      (let loop ((i (if (= soff n) n (fun_defined (- n 1)))))
;;             (cond ((> (fun_on_i i) n) i)
;;                   (else (loop (+ i 1)))
;;             )
;;      )
;;  )
;; )


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

;; When for some i, (foo i) = n, return the smallest such i,
;; otherwise the largest i such that (foo i) < n.
;; soff1 = starting offset for this function to be defined.
;; soff2 = starting offset for fun_on_i (i.e. its domain is [soff2,infinity])
(define (PSEUDOINVERSE1 soff1 soff2 fun_on_i) ;; soff = starting offset.
 (implement-cached-function 0 (fun_defined n)
     (let loop ((i (if (= soff1 n) soff2 (fun_defined (- n 1)))))
            (cond ((= (fun_on_i i) n) i)
                  ((> (fun_on_i i) n) (- i 1))
                  (else (loop (+ i 1)))
            )
     )
 )
)

;; (define (PSEUDOINVERSE1 soff fun_on_i) ;; soff = starting offset.
;;  (implement-cached-function 0 (fun_defined n)
;;      (let loop ((i (if (= soff n) n (fun_defined (- n 1)))))
;;             (cond ((= (fun_on_i i) n) i)
;;                   ((> (fun_on_i i) n) (- i 1))
;;                   (else (loop (+ i 1)))
;;             )
;;      )
;;  )
;; )



;;

;; XXX - Invent a more respectable name for PSEUDOINVERSE2 and the next one, it is our good friend.

;; Returns the largest i, such that (foo i) <= n.
;; soff1 = starting offset for this function to be defined.
;; soff2 = starting offset for fun_on_i (i.e. its domain is [soff2,infinity])
(define (LEFTINV-LEASTMONO soff1 soff2 fun_on_i) (COMPOSE -1+ (LEAST-EXCEEDING-I soff1 soff2 fun_on_i)))


(define (LEFTINV-LEASTMONO-NC2NC soff1 soff2 foo)
 (lambda (n)
  (let ((minind soff2)
        (maxind n) ;; Assuming foo is a monotonic injection, then foo(n) >= n, so we don't need search farther.
       )
        (let loop ((imin minind) (imax maxind))
             (cond ((< imax imin) 0) ;; Didn't found any k such that foo(k) <= n, return zero.
                   (else
                      (let* ((imid (+ imin (/ (- imax imin (A000035 (- imax imin))) 2))) ;; imid = avg(imin,imax)
                             (val (foo imid)) ;; What's there?
                            )
                        (cond ((< val imid)
                                 (error (format #f
"LEFTINV-LEASTMONO-NC2NC: argument function returns f(~a) = ~a, thus it cannot be monotoninc injection!\n"
                                                imid val
                                        )
                                 )
                              )
                              ((= val n) imid) ;; Found it?
                              ((< val n)
                                    (if (> (foo (+ 1 imid)) n)
                                        imid ;; If foo(imid) < n, but foo(imid+1) > n, then return imid
                                        (loop (+ imid 1) imax) ;; Otherwise search from the range [imid+1,imax]
                                    )
                              )
                              (else ;; i.e. if (> val n)
                                 (loop imin (- imid 1)) ;; Search from the range [imin, imid-1]
                              )
                        )
                      )
                   )
             )
        )
  )
 )
)



;; Not yet correct:
;; (define (PARSUMS_OF_CHARFUN soff foo) ;; soff = starting offset.
;;  (implement-cached-function 0 (cfun n)
;;     (cond ((< n soff) 0)
;;           ((= soff n) (if (> (foo soff) soff) 0 1))
;;           (else
;;              (let ((preval (cfun (- n 1))))
;;                 (if (= (foo (+ 1 soff preval)) n) (+ 1 preval) preval)
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


;; (define (PARSUMS_OF_CHARFUN soff foo) ;; soff = starting offset.
;;  (implement-cached-function 0 (cfun n)
;;     (cond ((< n soff) 0)
;;           ((= soff n) (if (> (foo soff) soff) 0 1))
;;           (else
;;              (let ((preval (cfun (- n 1))))
;;                 (if (= (foo (+ 1 soff preval)) n) (+ 1 preval) preval)
;;              )
;;           )
;;     )
;;  )
;; )


;; fun_on_i should be a monotone function, we return the v which do not occur in its range.
;; Is there a way to do this with one cached function less?

(define (COMPLEMENT soff fun_on_i) ;; soff = starting offset.
  (let ((inv (LEFTINV-LEASTMONO soff soff fun_on_i)))
    (MATCHING-POS soff soff
      (lambda (i) (let ((inv_i (inv i))) (or (< inv_i soff) (not (= i (fun_on_i inv_i))))))
    )
  )
)



;; The overriding of the initial values should be implemented in implement-cached-function,
;; so that there would not be any overhead at the runtime.
;; (They would be written directly to the cache, when it is initialized!)

(define (EIGEN-CONVOLUTION soffval mulfun) ;; These begin always from zero.
 (implement-cached-function 0 (fun_defined n)
   (cond ((and (integer? soffval) (= 0 n)) soffval)
         ((and (list? soffval) (< n (length soffval))) (list-ref soffval n))
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

(define (GEN-CONVOLVE soff mulfun fun1 fun2)
 (implement-cached-function 0 (fun_defined n)
     (let loop ((s 0) (i soff) (j n))
          (if (< j soff) s
              (loop (+ s (mulfun (fun1 i) (fun2 j))) (1+ i) (-1+ j))
          )
     )
 )
)

(define (CONVOLVE soff fun1 fun2) (GEN-CONVOLVE soff * fun1 fun2))


;; Didn't realize that it is this easy. See Joshua Zucker's mail
;; "Re: [SeqFan]: Repeated iterations of INVERT starting from A019590 ?"
;; on SeqFan-mailing list, Jun 7, 2006 6:46 PM

(define (INVERT fun) ;; Always one-based!
 (implement-cached-function 0 (fun_defined n)
   (+ (fun n) ;; The whole space can be filled by (fun n) objects of size n.
      (let loop ((s 0) (i 1) (j (- n 1))) ;; In how many ways smaller parts can be filled?
           (if (< j 1) s
               (loop (+ s (* (fun_defined i) (fun j))) (1+ i) (-1+ j))
           )
      )
   )
 )
)

;;;;;;;;;;;;;;;;;;;;;;

;; (define define-for-backward-compatibility define)

;; (define-for-backward-compatibility PSEUDOINVERSE2 LEAST_MONOTONIC_LEFT_INVERSE)
;; (define-for-backward-compatibility PSEUDOINVERSE2NONCACHED LEAST_MONOTONIC_LEFT_INVERSE_NC2NC)

;;;;;;;;;;;;;;;;;;;;;;

;; Naive search, no caching:

(define (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS monotonic_injection_with_a1_1)
   (lambda (n)
       (let outloop ((n n) (s 0))
          (cond ((zero? n) s)
                (else
                  (let inloop ((i 2) (prev_k (monotonic_injection_with_a1_1 1)))
                      (let ((k (monotonic_injection_with_a1_1 i)))
                         (cond ((> k n) (outloop (- n prev_k) (+ 1 s)))
                               (else (inloop (+ 1 i) k))
                         )
                      )
                  )
                )
          )
       )
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(definec (add1 n) (1+ n)) ;; Our cached version.

(definec (rowfun_n_for_Esieve n) ;;
  (if (= 1 n)
      (lambda (n) (+ 1 n)) ;; We return +1 (add1) function, that gives 2,3,4,5,6,7,8,9,10,11,...
      (let* ((prevrowfun (rowfun_n_for_Esieve (- n 1)))
             (prevprime (prevrowfun 1))
            )
         (COMPOSE prevrowfun (NONZERO-POS 1 1 (lambda (i) (modulo (prevrowfun i) prevprime))))
      )
  )
)

;; Row 1 = 2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,...
;; Row 2 = 3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,...
;; Row 3 = 5,7,11,13,17,19,23,25,29,31,35,37,41,43,47,49,53
;; Row 4 = 7,11,13,17,19,23,29,31,41,43,47,49,53,...


(definec (A000040 n) ((rowfun_n_for_Esieve n) 1))
(define A000720 (LEFTINV-LEASTMONO 1 1 A000040)) ;; We have a more efficient definition somewhere...

(define (A256956 n) (* (A000720 n) (A000720 (+ 1 n))))


(definec (A046992 n) (if (<= n 1) 0 (+ (A000720 n) (A046992 (- n 1))))) ;; a(n) = Sum_{k=1..n} pi(k) (cf. A000720). 

(define (A249727 n) (if (= 1 n) 1 (- n (+ 1 (A046992 (- (A249728 n) 1))))))
(define A249728 (COMPOSE (LEAST-GTE-I 0 1 A046992) -1+)) ;; One-based.

(define (A065855 n) (- n (A000720 n) 1))
(define (A062298 n) (- n (A000720 n)))

(define (A001223 n) (- (A000040 (+ 1 n)) (A000040 n)))

(define A018252 (COMPLEMENT 1 A000040)) ;; 1 and composites.
(define A002808 (compose-funs A018252 1+)) ;; Composites.

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
;;                  (loop (-1+ i)
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

(definec (A165569 n)
  (if (= 1 n) 1
      (let* ((i (A165569 (-1+ n)))
             (champion (abs (- *Phi* (/ (A108539 i) (A000040 i)))))
            )
        (let loop ((i (1+ i)))
              (cond ((< (abs (- *Phi* (/ (A108539 i) (A000040 i)))) champion)
                      i ;; Found a better specimen than the current champion.
                    )
                    (else (loop (1+ i)))
              )
        )
      )
  )
)

;; Compute these sieve arrays as A-entries, and also similar tables as Yasutoshi's A083140.

(define (Esievebi col row) ((rowfun_n_for_Esieve row) col))

(definec (rowfun_n_for_A083221 n) ;;
  (if (= 1 n)
      (lambda (n) (+ n n)) ;; We return even numbers as the first row, i.e. A005843.
      (let ((rowfun_of_esieve (rowfun_n_for_Esieve n))
            (prime (A000040 n))
           )
         (COMPOSE rowfun_of_esieve
                  (MATCHING-POS 1 1 (lambda (i) (zero? (modulo (rowfun_of_esieve i) prime))))
         )
      )
  )
)

(define (A083221bi row col) ((rowfun_n_for_A083221 row) col)) ;; swapped the argument order Nov 14 2014.

;; (define (A083221 n) (A083221bi (1+ (A025581 (-1+ n))) (1+ (A002262 (-1+ n)))))
;; (define (A083140 n) (A083221bi (1+ (A002262 (-1+ n))) (1+ (A025581 (-1+ n)))))
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


(definec (A005408shifted n) (- (* 2 n) 1)) ;; Cached variant of odd numbers, one-based.

(define A001651c (compose-funs (NONZERO-POS 1 1 (lambda (i) (modulo i 3))) 1+)) ;; Zero-based in OEIS.

(define (A001651 n) (if (even? n) (+ 1 (* 3 (/ n 2))) (- (* 3 (/ (+ n 1) 2)) 1)))

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
         (compose-funs prevrowfun (NONZERO-POS 1 1 (lambda (i) (modulo i everynth))))
      )
  )
)

;; When large values are needed, do:
;; (define A000959vec (read-b-file-to-vector "seqs/b000959_upto200000_from_OEIS.txt" 200001))
;; (define (A000959 n) (vector-ref A000959vec n))
;; Also useful, before we get better implementation for things like LEFTINV-LEASTMONO and COMPLEMENT:

(define (precompute Afun upto)
   (let loop ((i 1))
     (cond
       ((<= i upto)
          (display (* i 10000)) (display " ") (display (Afun (* i 10000))) (newline)
          (loop (+ i 1))
       )
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

(definec (A255553 n) (A255551 (A252460 n)))
(definec (A255554 n) (A083221 (A255552 n)))

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
        (else ;; We have to search for it, in a two naive loops. (XXX - Could use a binary search in inner one?)
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


(definec (A260738 n) ;; Row index to A255127.
  (cond ((= 1 n) 0) ;; 1 is outside of A255127.
        ((even? n) 1) ;; Optimization. All even numbers are on the row 1.
        (else ;; We have to search for it, in a two naive loops. (XXX - Could use a binary search in inner one?)
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
        (else ;; We have to search for it, in a two naive loops. (XXX - Could use a binary search in inner one?)
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

(define (A260435 n) (if (<= n 1) n (A255127bi (A260438 n) (A260439 n))))

(define (A260436 n) (if (<= n 1) n (A255551bi (A260738 n) (A260739 n))))

(definec (A260741 n) (if (<= n 1) n (A255127bi (A260438 n) (A260741 (A260439 n)))))
(definec (A260742 n) (if (<= n 1) n (A255551bi (A260738 n) (A260742 (A260739 n)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define A192503 (MATCHING-POS 1 1 (lambda (n) (and (= 1 (A192490 n)) (= 1 (A010051 n)))))) ;; Ludic prime numbers. 

(define A192504 (MATCHING-POS 1 1 (lambda (n) (and (= 1 (A192490 n)) (zero? (A010051 n)))))) ;; Ludic nonprime numbers. 

(define A192505 (MATCHING-POS 1 1 (lambda (n) (and (zero? (A192490 n)) (= 1 (A010051 n)))))) ;; Nonludic numbers that are prime numbers. 

(define A192506 (MATCHING-POS 1 1 (lambda (n) (and (zero? (A192490 n)) (zero? (A010051 n)))))) ;; Numbers that are neither ludic nor prime.



(define A257689 (MATCHING-POS 1 1 (lambda (n) (or (= 1 (A192490 n)) (= 1 (A010051 n))))))
;; (define A257689 (MATCHING-POS 1 1 (lambda (n) (not (zero? (+ (A192490 n) (A010051 n)))))))


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

(define (A256486 n) (- (A003309 n) (A000959 n))) ;; Difference between n-th Ludic and n-th Lucky number.

(define (A256487 n) (- (A254100 n) (A219178 n))) ;; Difference between n-th Postludic and n-th Postlucky number.

(define (A256488 n) (/ (A256487 n) 2)) ;; Same halved.

(define (A260722 n) (if (= 1 n) 0 (- (A003309 (+ 1 n)) (A000959 n)))) ;; Diff between n-th odd Ludic and n-th Lucky
(define (A260721 n) (if (= 1 n) 0 (/ (- (A003309 (+ 1 n)) (A000959 n)) 2))) ;; Same halved.

(define (A260723 n) (- (A003309 (+ 1 n)) (A003309 n)))

;; A255407-A255426 are now reserved for your use. 

(definec (A255407 n) (A255127 (A252460 n)))
(definec (A255408 n) (A083221 (A255128 n)))

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


;;;;;;;;;;;;;;;;;;;;;


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

(define (A255324 n) (- (A003309 n) (A008578 n)))  ;; Is it always positive?! One-based. Cf. A032600.
(define (A255325 n) (/ (A255324 n) 2))

(define (A032600 n) (- (A000959 n) (A000040 n))) ;; halved versions not in OEIS.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Similar entanglement-permutations for lucky/unlucky numbers.

;; A257725 a(0)=0; thereafter, if n is k-th lucky number [i.e., n = A000959(k)], a(n) = 1 + (2*a(k-1)); otherwise, when n is k-th unlucky number [i.e., n = A050505.(k)], a(n) = 2*a(k). 
;; Cf. A237427.

;; A257726 a(0)=0; a(2n) = unlucky(a(n)), a(2n+1) = lucky(a(n)+1), where lucky = A000959, unlucky = A050505.
;; Cf. A237126, A227413

(definec (A257725 n)
   (cond ((zero? n) n)
         ((= 1 (A145649 n)) (+ 1 (* 2 (A257725 (- (A109497 n) 1)))))
         (else (* 2 (A257725 (- n (A109497 n)))))
   )
)

(define (A257725v2 n) (if (zero? n) n (A246377 (A257732 n))))
(define (A257725v3 n) (if (zero? n) n (A237427 (A257734 n))))

(definec (A257726 n)  ;; Inverse: A257725.
   (cond ((zero? n) n)
         ((even? n) (A050505 (A257726 (/ n 2))))
         (else (A000959 (+ 1 (A257726 (/ (- n 1) 2)))))
   )
)

(define (A257726v2 n) (if (zero? n) n (A257731 (A246378 n))))
(define (A257726v3 n) (if (zero? n) n (A257733 (A237126 n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interlude: A version with Kimberling's initial condition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definec (A183089 n)
   (cond ((<= n 1) n)
         ((even? n) (A050505 (A183089 (/ n 2))))
         (else (A000959 (A183089 (/ (+ n 1) 2))))
   )
)


(definec (A257690 n)
   (cond ((= 1 n) n)
         ((= 1 (A145649 n)) (+ -1 (* 2 (A257690 (A109497 n)))))
         (else (* 2 (A257690 (- n (A109497 n)))))
   )
)



;; And cross-compositions with the other versions. I always like subtle differences...

(define (A257735 n) (A257725 (A183089 n)))
(define (A257736 n) (A257690 (A257726 n)))

(define (A257737 n) (A183089 (A257725 n)))
(define (A257738 n) (A257726 (A257690 n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A257731 Permutation of natural numbers: a(1) = 1, a(prime(n)) = lucky(1+a(n)), a(composite(n)) = unlucky(a(n)), where prime(n) = n-th prime number, A000040, composite(n) = n-th composite number A002808 and lucky = A000959, unnlucky = A050505.
;; Cf. A255421


(definec (A257731 n)
   (cond ((= 1 n) n)
         ((= 1 (A010051 n)) (A000959 (+ 1 (A257731 (A000720 n)))))
         (else (A050505 (A257731 (A065855 n))))
   )
)


(define (A257731v2 n) (A257726 (A246377 n)))
(define (A257731v3 n) (A257733 (A255421 n)))

(definec (A257732 n)
   (cond ((= 1 n) n)
         ((= 1 (A145649 n)) (A000040 (A257732 (- (A109497 n) 1))))
         (else (A002808 (A257732 (- n (A109497 n)))))
   )
)

(define (A257732v2 n) (A246378 (A257725 n)))
(define (A257732v3 n) (A255422 (A257734 n)))

;;;

(definec (A257801 n)
   (cond ((<= n 2) n)
         ((= 1 (A010051 n)) (A000959 (+ 1 (A257801 (+ -1 (A000720 n))))))
         (else (A050505 (A257801 (A062298 n))))
   )
)

(definec (A257802 n)
   (cond ((= 1 n) n)
         ((= 1 (A145649 n)) (A065091 (A257802 (- (A109497 n) 1))))
         (else (A065090 (+ 1 (A257802 (- n (A109497 n))))))
   )
)

(define (A257801v2 n) (A257726 (A257727 n))) ;; odd primes -> lucky. Cf. A257731

(define (A257802v2 n) (A257728 (A257725 n))) ;; lucky -> odd primes. Cf. A257732

;;;

;; A257733 Permutation of natural numbers: a(1) = 1, a(ludic(n)) = lucky(1+a(n-1)), a(nonludic(n)) = unlucky(a(n)), where ludic(n) = n-th ludic number, A003309, nonludic(n) = n-th nonludic number A192607 and lucky = A000959, unnlucky = A050505.


(definec (A257733 n)
   (cond ((= 1 n) n)
         ((= 1 (A192490 n)) (A000959 (+ 1 (A257733 (+ -1 (A192512 n))))))
         (else (A050505 (A257733 (A236863 n))))
   )
)

(define (A257733v2 n) (A257731 (A255422 n)))
(define (A257733v3 n) (A257726 (A237427 n)))

(definec (A257734 n)
   (cond ((= 1 n) n)
         ((= 1 (A145649 n)) (A003309 (+ 1 (A257734 (- (A109497 n) 1)))))
         (else (A192607 (A257734 (- n (A109497 n)))))
   )
)

(define (A257734v2 n) (A255421 (A257732 n)))
(define (A257734v3 n) (A237126 (A257725 n)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Row-wise first differences:

;; A257251 A083221              Sieve of Eratosthenes

(define (A257251 n) (A257251bi (A002260 n) (A004736 n)))
(define (A257251bi row col) (- (A083221bi row (+ 1 col)) (A083221bi row col)))
 

(define (A257252 n) (A257251bi (A004736 n) (A002260 n)))

;; (define (A257253 n) (/ (A257251 n) 2))
(define (A257253 n) (A257253bi (A002260 n) (A004736 n)))
(define (A257253bi row col) (* (/ 1 2) (- (A083221bi row (+ 1 col)) (A083221bi row col))))

(define (A257254 n)  (A257253bi (A004736 n) (A002260 n)))


;; A257255 A255545              Lucky / Unlucky array

(define (A257255 n) (A257255bi (A002260 n) (A004736 n)))
(define (A257255bi row col) (- (A255545bi row (+ 1 col)) (A255545bi row col)))

(define (A257256 n) (- (A219178 n) (A000959 n)))

;; A257257 A255127              Ludic array

(define (A257257 n) (A257257bi (A002260 n) (A004736 n)))
(define (A257257bi row col) (- (A255127bi row (+ 1 col)) (A255127bi row col)))

(define (A257258 n) (A257258bi (A002260 n) (A004736 n)))
(define (A257258bi row col) (* (/ 1 2) (- (A255127bi row (+ 1 col)) (A255127bi row col))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Column-wise first differences:

;; A257513 & A257514 for A083221              Sieve of Eratosthenes

(define (A257513 n) (A257513bi (A002260 n) (A004736 n)))
(define (A257513bi row col) (- (A083221bi (+ 1 row) col) (A083221bi row col)))
 
(define (A257514 n) (A257513bi (A004736 n) (A002260 n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definec (A255420 n) (if (zero? n) 1 (A003309 (+ 1 (A255420 (- n 1))))))

(definec (A255421 n)
   (cond ((= 1 n) n)
         ((= 1 (A010051 n)) (A003309 (+ 1 (A255421 (A000720 n)))))
         (else (A192607 (A255421 (A065855 n))))
   )
)



(define (A255421v2 n) (A237126 (A246377 n)))

(definec (A255422 n)
   (cond ((= 1 n) n)
         ((= 1 (A192490 n)) (A000040 (A255422 (- (A192512 n) 1))))
         (else (A002808 (A255422 (A236863 n))))
   )
)

(define (A255422v2 n) (A246378 (A237427 n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define *Sqrt5* (sqrt 5))
(define *Phi* (/ (1+ *Sqrt5*) 2))
(define *LogPhi* (log *Phi*))

(define (sgn n) (cond ((zero? n) 0) ((< n 0) -1) (else 1)))

(define (A000004 n) (- n n))

(define (A000012 n) (1+ (- n n)))


(define (fix:jacobi-symbol p q)
 (if (not (and (fix:fixnum? p) (fix:fixnum? q) (fix:= 1 (fix:and q 1))))
     (error
       "fix:jacobi-symbol: args must be fixnums, and 2. arg should be odd: "
            p q
     )
     (let loop ((p p)
                (q q)
                (s 0))  ;; 0 in bit-1 stands for +1, 1 in bit-1 for -1.
       (cond ((fix:zero? p) 0)
             ((fix:= 1 p) (fix:- 1 (fix:and s 2)))
             ((fix:= 1 (fix:and p 1)) ;; Odd p ?
                (loop (fix:remainder q p) p (fix:xor s (fix:and p q)))
             )
             (else ;; It's even, get rid of one 2:
                (loop (fix:lsh p -1) q (fix:xor s (fix:xor q (fix:lsh q -1))))
             )
       )
     )
 )
)


;; Here's our C-version. The teaching: Never implement mathematician's
;; or logician's elegant formulae directly, without thinking!
;; Working in GF(2) with XOR & AND is much more natural for
;; computers than working in isomorphic multiplicative group of {+1, -1}
;; with MUL.
;; We also convert the recursion in our formula to a simple loop.
;; q should be always odd!
;;
;; int js_ULI(ULI p,ULI q)
;; {
;;     int s = 0; /* 0 in bit-1 stands for +1, 1 in bit-1 for -1. */
;;     ULI new_p;
;; loop:
;;     if(0 == p) { return(p); }
;;     if(1 == p) { return(1-(s&2)); } /* Convert 1 in bit-1 to -1, 0 to +1. */
;; 
;;     if(p&1) /* We have an odd p. */
;;      {
;; /* If both p & q are 3 mod 4, then sign changes, otherwise stays same: */
;;        s ^= (p&q); /* Only the bit-1 is significant, others are ignored. */
;;        new_p = q % p; /* Could we have a simple - here as with Euclid? */
;;        q = p;
;;        p = new_p;
;;        goto loop;
;;      }
;;     else /* We have an even p. So (2k/q) = (2/q)*(k/q) */
;;      {   /* where (2/q) = 1 if q is +-1 mod 8 and -1 if q is +-3 mod 8. */
;;          /* I.e. sign changes only if q's lower bits are (011) or (101),
;;             i.e. if the bit-1 and bit-2 xored yield 1. */
;;        s ^= (q^(q>>1));         /* Thus, this does it. */
;;        p >>= 1;
;;        goto loop;
;;      }
;; }
;; 

(define fix:legendre-symbol fix:jacobi-symbol)
(define jacobi-symbol fix:jacobi-symbol)
(define legendre-symbol fix:legendre-symbol)

;; Useful.
(define (ratio->float-str x y prec)
  (if (or (zero? x) (zero? y))
      (if (equal? x y) "1" "0")
      (fluid-let ((flonum-unparser-cutoff (list 'absolute prec)))
        (string-append (if (< x y) "0" "")
                       (number->string (exact->inexact (/ x y)))
        )
      )
  )
)




;; A007918 Least prime >= n (version 1 of the "next prime" function).
;; (Offset 0)
;; 2,2,2,3,5,5,7,7,11,11,11,11,13,13,17,17,17,17,19,19,23,23,23,23,29,29,29,


;; A151800 Least prime > n (version 2 of the "next prime" function).
;; 2,2,3,5,5,7,7,11,11,11,11,13,13,17,17,17,17,19,19,23,23,23,23,29,29,29,29,
;; (Offset 0)

;; A007917 Version 1 of the "previous prime" function: largest prime <= n.
;; (Offset 2)
;; 2,3,3,5,5,7,7,7,7,11,11,13,13,13,13,17,17,19,19,19,19,23,23,23,23,23,23,

;; A151799 Version 2 of the "previous prime" function: largest prime < n.
;; (Offset 3)
;; 2,3,3,5,5,7,7,7,7,11,11,13,13,13,13,17,17,19,19,19,19,23,23,23,23,23,23,

;; A108539 Prime p such that p/prime(n) is nearest to phi, the golden ratio.

;; Note: A010051 is in module GF2Xfuns.scm/com in this same directory.

(define A151800 (NEXT-MATCHING-POS (lambda (i) (= 1 (A010051 i)))))

(define A007917 (PREV-OR-SAME-MATCHING-POS (lambda (i) (= 1 (A010051 i)))))

(definec (A108539 n)
 (let* ((p1 (A000040 n))
        (n2 (A000201 p1))
        (q1 (A151800 n2))
        (q2 (A007917 n2))
       )
   (if (< (abs (- *Phi* (/ q1 p1))) (abs (- *Phi* (/ q2 p1))))
       q1
       q2
   )
 )
)


(definec (fibo nakki) ;; I.e., A000045
   (if (< nakki 2)
       nakki
       (+ (fibo (-1+ nakki)) (fibo (- nakki 2)))
   )
)

(define A000045 fibo)

(define (A000071 n) (- (A000045 n) 1))

;; A000129: Pell numbers: a(0) = 0, a(1) = 1; for n > 1, a(n) = 2*a(n-1) + a(n-2). 
(definec (A000129 n) (if (<= n 1) n (+ (* 2 (A000129 (- n 1))) (A000129 (- n 2)))))

;; A001045: Jacobsthal sequence (or Jacobsthal numbers): a(n) = a(n-1) + 2*a(n-2), with a(0) = 0, a(1) = 1. 
(definec (A001045 n) (if (<= n 1) n (+ (A001045 (- n 1)) (* 2 (A001045 (- n 2))))))


(definec (A178590 n) (cond ((<= n 1) n) ((even? n) (* 3 (A178590 (/ n 2)))) (else (+ (A178590 (/ (- n 1) 2)) (A178590 (/ (+ n 1) 2))))))

(definec (A001177 n) ;; Fibonacci entry points: a(n) = least k such that n divides Fibonacci number F_k (=A000045(k)).
   (let loop ((k 1))
      (cond ((zero? (modulo (A000045 k) n)) k)
            (else (loop (+ k 1)))
      )
   )
)

(definec (A233287 n)
   (if (< n 2)
       1
       (lcm (A001177 n) (A233287 (- n 1)))
   )
)

(define A233282 (RECORD-POS 1 1 A233287))
(define (A233283 n) (A233287 (A233282 n)))

(define (A051122 n) (A004198bi (A000045 n) (A000045 (1+ n))))
(define (A051123 n) (A003986bi (A000045 n) (A000045 (1+ n))))
(define (A051124 n) (A003987bi (A000045 n) (A000045 (1+ n))))
(define (A219660 n) (A000120 (A051122 n))) ;; Number of (first-level) carries produced when summing Fib_n + Fib_{n+1}.

(define (A001906 n) (A000045 (A005843 n)))
(define (A122367 n) (A000045 (A005408 n)))
(define (A027941 n) (-1+ (A122367 n)))


(define (A000201 n) (floor->exact (* n *Phi*)))
(define (A004956 n) (ceiling->exact (* n *Phi*)))
(define (A007067 n) (round->exact (* n *Phi*)))

(define (A003622 n) (+ n (A022342 n))) ;; One-based also. ;; Integers whose Z.E. ends with 1.

(define (A022342 n) (A022290 (* 2 (A003714 (- n 1)))))
(define (A022342dont_use_as_involves_floats n) (- (A000201 n) 1)) ;; Note: one-based ;; Integers whose Z.E. ends with 0.

(definec (A002326 n) ;; Multiplicative order of 2 mod 2n+1. 
   (if (zero? n)
       1
       (let ((m (A005408 n)))
         (let loop ((p 2) (s 1))
              (cond ((= 1 p) s)
                    (else (loop (modulo (+ p p) m) (+ s 1)))
              )
         )
       )
   )
)

(define (A005408 n) (+ (* 2 n) 1))
(define (A004767 n) (+ (* 4 n) 3))
(define (A016813 n) (+ (* 4 n) 1))
(definec (A016813c n) (+ (* 4 n) 1)) ;; Cached version for debugging.
(define (A017557 n) (+ (* 12 n) 3))
(define (A017605 n) (+ (* 12 n) 7))

(define (A019590 n) (if (< n 3) 1 0))

(define A000045_shifted_left (INVERT A019590))

(definec (A000142 n) (if (zero? n) 1 (* n (A000142 (+ -1 n))))) ;; Factorial numbers: n! = 1*2*3*4*...*n (order of symmetric group S_n, number of permutations of n letters). 

(define ! A000142) ;; Factorial with syntax modified from the post-fix to pre-fix ! (don't confuse with A003422)


(define (A033312 n) (+ -1 (A000142 n)))

;; (define A033312from1 (RECORD-POS 1 1 A257079))

(define (isA000142? n)
  (and (> n 0)
       (let loop ((n n) (i 2))
            (cond ((= 1 n) #t)
                  ((not (zero? (modulo n i))) #f)
                  (else (loop (/ n i) (1+ i)))
            )
       )
  )
)


(define (which_in_A000142? n)
  (and (> n 0)
       (let loop ((n n) (i 2))
            (cond ((= 1 n) (- i 1))
                  ((not (zero? (modulo n i))) #f)
                  (else (loop (/ n i) (1+ i)))
            )
       )
  )
)

(definec (A001142 n) (mul (lambda (k) (expt k (+ k k -1 (- n)))) 1 n)) ;; a(n) = product{k=1..n} k^(2k-1-n).


(definec (A001088 n) (if (= 1 n) n (* (A000010 n) (A001088 (- n 1))))) ;; "Phitorial"

(definec (A231721 n) (if (< n 2) n (+ (A001088 n) (A231721 (- n 1)))))
(define (A231722 n) (- (A231721 n) 1)) ;; a(n) = A231721(n)-1.


(define (A230403 n)
  (if (zero? n) 0
      (let loop ((n n) (i 2))
           (cond ((not (zero? (modulo n i))) (- i 2))
                 (else (loop (/ n i) (1+ i)))
           )
      )
  )
)

(define (A230404 n) (A230403 (* 2 n)))

(define (A055881 n) (let loop ((n n) (i 2)) (cond ((not (zero? (modulo n i))) (- i 1)) (else (loop (/ n i) (+ 1 i))))))
(define (A055881v2 n) (+ (A230403 n) 1))

(define (A055881v3 n) (A230415bi (- n 1) n))

(define (A055881v4 n) (A230417 (+ (A000217 n) (- n 1))))

(define (!cut i j) ;; Compute the product i*(i+1)*(i+2)*...*j
   (cond ((eq? i j) i)
         ((> i j) 1)
         (else (* i (!cut (1+ i) j)))
   )
)

;; (C r n) stands for the binomial {r choose n}
(define (A007318tr r n) (/ (!cut (1+ n) r) (! (- r n))))
(define C A007318tr)

(define (A047999 n) (A000035 (A007318 n)))
(define (A083093 n) (A010872 (A007318 n)))

(define (A065040 n) (A007814 (A007318 n))) ;; Triangle T(m,k): exponent of the highest power of 2 dividing the binomial coefficient binomial(m,k). 
(define (A243759 n) (A007949 (A007318 n))) ;; Triangle T(m,k): exponent of the highest power of 3 dividing the binomial coefficient binomial(m,k). 


(define (A249343 n) (add A243759 (A000217 n) (A000096 n))) ;; Row sums of A243759.
(define (A249343v2 n) (A007949 (A001142 n)))

(define (A249345 n) (add (lambda (n) (A112765 (A007318 n))) (A000217 n) (A000096 n)))
(define (A249345v2 n) (A112765 (A001142 n)))

(define (A249346 n) (min (A187059 n) (A249343 n)))
(define (A249346v2 n) (A122841 (A001142 n)))


(define (A249347 n) (add (lambda (n) (A214411 (A007318 n))) (A000217 n) (A000096 n)))
(define (A249347v2 n) (A214411 (A001142 n)))

(define (A001405 n) (A007318tr n (floor->exact (/ n 2))))

(definec (A036256 n) (if (zero? n) 1 (+ (A001405 n) (A036256 (- n 1)))))

;;;;;;

(define (inA001109? n) (= (* 8 n n) (floor->exact (* (sqrt 8) n (ceiling->exact (* (sqrt 8) n))))))
(define A001109v2 (MATCHING-POS 0 0 inA001109?)) ;; Not a practical version, just for testing the above one!
(definec (A001109 n) (if (< n 2) n (- (* 6 (A001109 (- n 1))) (A001109 (- n 2))))) ;; a(n) = 6*a(n-1) - a(n-2) with a(0)=0, a(1)=1.
(define (A233269 n) (A055881 (A001109 n))) ;;
;; Records occur at: 1, 2, 8, 24, 48, 384, 3456, ...
(define Apalit1109 (MATCHING-POS 1 1 (lambda (n) (equal? (map A233269 (cdr (iota0 n))) (reverse!  (map A233269 (cdr (iota0 n))))))))
;; 1, 3, 5, 7, 15, 23, 47, 95, 143, 191, 239, 287, 335, 383, 767, 1151, 1535, 1919, 2303, 2687, 3071, 3455, ...

(define (inA001110? n) (and (zero? (A068527 n)) (inA001109? (floor->exact (sqrt n)))))
(define A001110v2 (MATCHING-POS 0 0 inA001110?)) ;; Not a practical version, just for testing the above one!

(definec (A001110 n) (if (< n 2) n (+ 2 (- (* 34 (A001110 (- n 1))) (A001110 (- n 2))))))

;; A233267-A233287 are now reserved for your use.

(define (A233267 n) (A055881 (A001110 n))) ;; A fractal sequence, palindromic?
;; (define Auudet (RECORD-POS 1 1 A233267)) ;;  --> (1 2 4 12 48 288 2016 4032) ;; *2 - 1 --> 3,7,23,95,575
;; (define Auudet2 (DISTINCT-POS 1 1 A233267))
;; (map A233267 (map Auudet (iota 8))) --> (1 3 4 7 11 12 13 14)
;; Also (A055881 (A006472 n)) ???
;; Appallingly inefficient:
;; (define Apalit (MATCHING-POS 1 1 (lambda (n) (equal? (map A233267 (cdr (iota0 n))) (reverse!  (map A233267 (cdr (iota0 n))))))))

(define (A233284 n) (A055874 (A000045 n)))
(define (A233285 n) (A055881 (A000045 n)))
(define (A233286 n) (A230403 (A000045 n)))
(define (A233286v2 n) (- (A233285 n) 1))

(define A000108 (EIGEN-CONVOLUTION 1 *))
(define A000108one_based (INVERT (compose-funs A000108 -1+)))

(definec (A007477 n) ;; Just quickly something. Totally unoptimized.
  (if (< n 2) 1
    (let loop ((s 0) (i 0) (j (- n 2)))
        (cond ((negative? j) s)
              (else (loop (+ s (* (A007477 i) (A007477 j))) (1+ i) (-1+ j)))
        )
    )
  )
)

(define (A213705 n) (if (< n 2) n (+ (A007477 (- n 1)) (A007477 n)))) ;; Etsivät etsivät etsivät!



;; A007477 := proc(n) option remember; local k; if n <= 1 then 1 else add(A007477(k)*A007477(n-k-2), k=0..n-2); fi; end;

(define (A007318 n) (C (A003056 n) (A002262 n)))

(definec (A000984 n) (/ (! (* 2 n)) (expt (! n) 2))) ;; Central binomial coefficients: C(2n,n) = (2n)!/(n!)^2.

(definec (binomial_n_2 n) (/ (* (-1+ n) n) 2))
;; A000217 = LEFT(binomial_n_2)



(define A003989bi gcd)
(define A003990bi lcm)
(define (A003989 n) (A003989bi (1+ (A002262 n)) (1+ (A025581 n))))
(define (A003990 n) (A003990bi (1+ (A002262 n)) (1+ (A025581 n))))

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

(define (A101080bi x y)  (A000120 (A003987bi x y)))
(define (A101080 n) (A101080bi (A002262 n) (A025581 n)))



(definec (nth-row-of-A047999 n)
   (cond ((zero? n) (unsigned-integer->bit-string 1 1))
         (else (next-row-of-A047999 (nth-row-of-A047999 (-1+ n))))
   )
)

(define (next-row-of-A047999 gen)
  (let ((one-zero (make-bit-string 1 #f)))
     (bit-string-xor (bit-string-append gen one-zero)
                     (bit-string-append one-zero gen)
     )
  )
)


(define (A001317 n) (bit-string->unsigned-integer (nth-row-of-A047999 n)))


(definec (nth-gen-of-rule90 n)
   (cond ((zero? n) (unsigned-integer->bit-string 1 1))
         (else (next-rule90-gen (nth-gen-of-rule90 (-1+ n))))
   )
)

(define (next-rule90-gen gen)
  (let ((two-zeros (make-bit-string 2 #f)))
     (bit-string-xor (bit-string-append gen two-zeros)
                     (bit-string-append two-zeros gen)
     )
  )
)


(define (A038183 n) (bit-string->unsigned-integer (nth-gen-of-rule90 n)))
(define (A038183v2 n) (A001317 (* 2 n)))

(define (bit-i n i) (modulo (floor->exact (/ n (expt 2 i))) 2))

(define (factbaseR->n rex) ;; Convert the reversed factorial expansion list to an integer.
   (let loop ((rex rex) (n 0) (i 1))
      (cond ((null? rex) n)
            (else (loop (cdr rex) (+ n (* (! i) (car rex))) (1+ i)))
      )
   )
)


(define (n->factbase n) ;; Convert an integer to a factorial expansion list.
   (let loop ((n n) (fex (if (zero? n) (list 0) (list))) (i 2))
      (cond ((zero? n) fex)
            (else (loop (floor->exact (/ n i)) (cons (modulo n i) fex) (1+ i)))
      )
   )
)

(define (factbase->n lista) ;; Convert a factorial expansion list to an integer.
   (let loop ((s 0) (lista (reverse lista)) (f 1) (i 2))
      (cond ((null? lista) s)
            (else (loop (+ s (* (car lista) f)) (cdr lista) (* f i) (+ 1 i)))
      )
   )
)

;; (same-intfuns0? A001477 (COMPOSE factbase->n n->factbase) 40320) --> #t

(define A046807 (MATCHING-POS 1 1 (lambda (n) (let ((rep (n->factbase n))) (equal? (reverse rep) rep)))))

(define (A099563 n) (car (n->factbase n)))

(define (A246359 n)
 (let loop ((n n) (i 2) (md 0)) (if (zero? n) md (loop (floor->exact (/ n i)) (+ i 1) (max (modulo n i) md))))
)

(definec (A246359v2 n) (if (zero? n) n (max (A099563 n) (A246359v2 (A257687 n)))))

(definec (A246359v3 n) (if (zero? n) n (+ 1 (A246359v3 (A257684 n)))))

(define (A246359v4 n) (apply max (n->factbase n)))

(define (A208575 n) (apply * (n->factbase n)))

(define (A227153 n) (apply * (delete-matching-items (n->factbase n) zero?))) ;; A227153(A227157(n)) = A208575(A227157(n))

(definec (A227153v2 n) (cond ((zero? n) 1) ((zero? (A257687 n)) (A099563 n)) (else (* (A099563 n) (A227153v2 (A257687 n))))))

(define (A227154 n) (apply * (map 1+ (n->factbase n))))

(definec (A227187 n)
   (if (<= n 1)
       (+ n n)
       (let ((prev (A227187 (- n 1))))
          (cond ((odd? prev) (+ 1 prev)) ;; If previous term odd, then at least the next even term has zero.
                ((> (A257510 prev) 1) (+ 1 prev)) ;; If previous term even, but there is a zero also at non lsb place?
                (else (+ 2 prev)) ;; Otherwise, prev is an even number, with no other zeros left from lsb, add two.
          )
       )
   )
)


(define A227157 (NONZERO-POS 1 1 A208575))
(define A227187slow (ZERO-POS 1 1 A208575))

(define (A227191 n) (if (zero? n) n (- n (A227153 n)))) ;; Cf. A219651.

(define (A162319bi n base)
  (cond ((zero? n) n)
        ((= 1 base) n)
        (else (+ 1 (A162319bi (floor->exact (/ n base)) base))) ;; I don't trust the log!
  )
)

(definec (A162319 n) (A162319bi (A002260 n) (A004736 n)))

(definec (A162320 n) (A162319bi (A002260 n) (+ 1 (A004736 n))))


(define (baselist->n base bex) ;; Convert base n expansion list to an integer.
   (let loop ((bex bex) (n 0))
      (cond ((null? bex) n)
            (else (loop (cdr bex) (+ (* n base) (car bex))))
      )
   )
)

(define (baselist-as-binary  lista)    (baselist->n  2 lista))
(define (baselist-as-ternary lista)    (baselist->n  3 lista))
(define (baselist-as-quaternary lista) (baselist->n  4 lista))
(define (baselist-as-decimal lista)    (baselist->n 10 lista))


(define (basevec->n base bex) ;; Convert base n expansion vector to an integer.
   (let loop ((i 0) (n 0))
      (cond ((= i (vector-length bex)) n)
            (else (loop (+ i 1) (+ (* n base) (vector-ref bex i))))
      )
   )
)

(define (basevec-as-binary  vec)    (basevec->n  2 vec))
(define (basevec-as-ternary vec)    (basevec->n  3 vec))
(define (basevec-as-quaternary vec) (basevec->n  4 vec))
(define (basevec-as-decimal vec)    (basevec->n 10 vec))


;; Note: A link from A007623 to A060130 is completely off the wall! (Really?)
;; This is of the limited usability:
(define (A007623 n) (baselist-as-decimal (n->factbase n)))

;; A257079-A257081 are now reserved for your use. 

(define (A257079 n)
  (let loop ((digs (uniq (sort (n->factbase n) <))) (mnp 1))
       (cond ((null? digs) mnp)
             ((zero? (car digs)) (loop (cdr digs) mnp))
             ((= (car digs) mnp) (loop (cdr digs) (+ 1 mnp)))
             (else mnp)
       )
  )
)

(define (A257080 n) (* n (A257079 n)))

(define (A257081 n) (let loop ((oldn n) (n (A257080 n)) (s 0)) (if (= oldn n) s (loop n (A257080 n) (+ 1 s)))))
(definec (A257081rec n) (if (= 1 (A257079 n)) 0 (+ 1 (A257081rec (A257080 n)))))

;; A257503-A257514 are now reserved for your use. 

(define (A257510 n) ;; One-based, to avoid ambiguity with zero.
   (let loop ((n n) (i 2) (s 0))
      (cond ((zero? n) s)
            (else (loop (floor->exact (/ n i)) (+ 1 i) (+ s (if (zero? (modulo n i)) 1 0))))
      )
   )
)

(define (A257511 n) ;; Zero-based.
   (let loop ((n n) (i 2) (s 0))
      (cond ((zero? n) s)
            (else (loop (floor->exact (/ n i)) (+ 1 i) (+ s (if (= 1 (modulo n i)) 1 0))))
      )
   )
)

(definec (A257511v2 n) (if (zero? n) n (+ (if (= 1 (A099563 n)) 1 0) (A257511v2 (A257687 n)))))


(define (A257260 n) ;; Start indexing from n=1.
   (let loop ((n n) (i 2))
      (cond ((zero? n) 0)
            ((zero? (modulo n i)) (- i 1))
            (else (loop (floor->exact (/ n i)) (+ 1 i)))
      )
   )
)

(define (A257261 n)
   (let loop ((n n) (i 2))
      (cond ((zero? n) 0)
            ((= 1 (modulo n i)) (- i 1))
            (else (loop (floor->exact (/ n i)) (+ 1 i)))
      )
   )
)

(define (A257679 n) ;; Start indexing from n=0.
   (let loop ((n n) (i 2) (mind 0))
      (if (zero? n)
          mind
          (let ((d (modulo n i)))
             (loop (/ (- n d) i) (+ 1 i) (cond ((zero? mind) d) ((zero? d) mind) (else (min d mind))))
          )
      )
   )
)

(definec (A257679v2 n) (if (zero? (A257687 n)) (A099563 n) (min (A099563 n) (A257679v2 (A257687 n)))))

(definec (A257679v3 n) (cond ((zero? n) n) ((= 1 (A257680 n)) 1) (else (+ 1 (A257679v3 (A257684 n))))))


(define A257692 (MATCHING-POS 1 1 (lambda (n) (= 2 (A257679 n)))))

(define A257693 (MATCHING-POS 1 1 (lambda (n) (= 3 (A257679 n)))))


(define (A255411 n) ;; Shift left & increment by 1 all other digits except zeros. Cf. A231720
   (let loop ((n n) (z 0) (i 2) (f 2))
      (cond ((zero? n) z)
            (else
              (let ((d (remainder n i)))
                      (loop (quotient n i)
                            (+ z (* f (+ d (if (zero? d) 0 1))))
                            (+ 1 i)
                            (* f (+ i 1))
                      )
              )
            )
      )
   )
)

(define (isA255411? n)
   (let loop ((n n) (i 2))
      (cond ((zero? n) #t)
            ((= 1 (modulo n i)) #f)
            (else (loop (floor->exact (/ n i)) (+ 1 i)))
      )
   )
)


(define A255411slow (MATCHING-POS 0 0 isA255411?))
(define A255411v1 (ZERO-POS 0 0 (COMPOSE -1+ A257079)))
(define A255411v2 (FIXED-POINTS 0 0 A257080))
(define A255411v3 (ZERO-POS 0 0 A257081))
(define A255411v3 (ZERO-POS 0 0 A257511))


(define (A257684 n) ;; Shift right & decrement by 1 all other digits except zeros. Cf. A255411
   (let loop ((n n) (z 0) (i 2) (f 0))
      (cond ((zero? n) z)
            (else
              (let ((d (remainder n i)))
                      (loop (quotient n i)
                            (+ z (* f (- d (if (zero? d) 0 1))))
                            (+ 1 i)
                            (if (zero? f) 1 (* f (- i 1)))
                      )
              )
            )
      )
   )
)

;; XXX - Submit these!:
(definec (A257694 n) (if (zero? n) 1 (* (A060130 n) (A257694 (A257684 n)))))

(definec (A257695 n) (if (zero? n) 1 (lcm (A060130 n) (A257695 (A257684 n)))))

(definec (A257696 n) (if (zero? n) n (gcd (A060130 n) (A257696 (A257684 n)))))

(definec (A260736 n) (if (zero? n) 0 (+ (A000035 n) (A260736 (A257684 n)))))

(define (A257685 n) (* (- 1 (A257680 n)) (A257684 n)))


(define (A257680 n) ;; Char fun for A256450.
   (let loop ((n n) (i 2))
      (cond ((zero? n) 0)
            ((= 1 (modulo n i)) 1)
            (else (loop (floor->exact (/ n i)) (+ 1 i)))
      )
   )
)

(definec (A257680rec n)
    (cond ((zero? n) 0)
          ((= 1 (A099563 n)) 1)
          (else (A257680rec (A257687 n)))
    )
)

(definec (A257681 n) (if (zero? (A257685 n)) (A257682 n) (A257681 (A257685 n))))

(definec (A257681v2 n) (if (or (zero? n) (= 1 (A257680 n))) (A257682 n) (A257681v2 (A257685 n))))

(definec (A257682 n) (if (zero? n) n (+ (A257680 n) (A257682 (- n 1)))))

;; Note:
;; (same-intfuns0? A257685 (lambda (n) (if (not (zero? (A257680 n))) 0 (- n (A257682 n)))) 40321) -- #t

(define A256450slow (NONZERO-POS 1 0 A257680))
(define A256450v2 (COMPOSE (COMPLEMENT 0 A255411) -1+)) ;; We want this to be one-based.
(define A256450v3 (MATCHING-POS 1 1 (lambda (n) (= 1 (A257679 n)))))

;; Ah, the recurrence, even more naive than a similar one for A227187:
(definec (A256450naive n)
   (if (= 1 n) 1
       (let ((prev (A256450naive (- n 1))))
           (cond ((even? prev) (+ 1 prev)) ;; Actually unnecessary, but optimizes for the next clause:
                 ((> (A257511 (+ 1 prev)) 0) (+ 1 prev))
                 (else (+ 2 prev))
           )
       )
   )
)

;; XXX - Rethink changing the offset of A256450 to zero, which would entail changing
;;  A257682, A257503, A257505, A255565 (description), A255566 and A255567.
(definec (A256450off0 n)
   (let* ((k (A258198 n))
          (d (- n (A258199 n)))
          (f (A000142 (+ 1 k)))
         )
      (cond ;; ((<= n 1) n)
            ((< d f) (+ f d))
            (else
               (+ (* f (+ 2 (floor->exact (/ (- d f) (A258199 n)))))
                  (A256450off0 (modulo (- d f) (A258199 n)))
               )
            )
      )
   )
)


(define (A256450 n) (A256450off0 (- n 1)))


;; (define (A001563wrong n) (* n (! (1+ n)))) ;; First differences of A000142.
(define (A001563 n) (* n (! n))) ;; /XFER IntSeq.facbase or IntSeq.core Row 1 of A257505.
(define (A001563v2 n) (- (! (1+ n)) (! n)))


;; (define A258198 (LEFTINV-LEASTMONO-NC2NC 0 0 A001563)) ;; Shooting flies with a cannon...
;; (define (A258199 n) (A001563 (A258198 n)))

(define (A258198 n) (let loop ((k 1) (f 1)) (if (> (* k f) n) (- k 1) (loop (+ k 1) (* (+ k 1) f)))))


(define (A258199 n)
    (let loop ((k 1) (f 1))
          (if (> (* k f) n)
              (* (- k 1) (/ f k))
              (loop (+ k 1) (* (+ k 1) f))
          )
    )
)



(define (A257503bi row col) (if (= 1 row) (A256450 col) (A255411 (A257503bi (- row 1) col))))
(define (A257503 n) (A257503bi (A002260 n) (A004736 n)))

(define (A257505bi row col) (if (= 1 col) (A256450 row) (A255411 (A257505bi row (- col 1)))))
(define (A257505 n) (A257505bi (A002260 n) (A004736 n)))

(define (A257504 n) (let ((col (A257681 n)) (row (A257679 n))) (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2))))

(define (A257506 n) (let ((row (A257681 n)) (col (A257679 n))) (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2))))



;;;;;;;;;;;;;;;;;;;;
;; Entanglement-permutations:

(definec (A255565 n)
  (cond ((zero? n) n)
        ((zero? (A257680 n)) (* 2 (A255565 (A257685 n))))
        (else (+ 1 (* 2 (A255565 (+ -1 (A257682 n))))))
  )
)


(definec (A255566 n)
  (cond ((zero? n) n)
        ((even? n) (A255411 (A255566 (/ n 2))))
        (else (A256450 (+ 1 (A255566 (/ (- n 1) 2)))))
  )
)

;; Offset 2:
;; (definec (A255567 n) (cond ((= n 2) n) ((odd? n) (+ 1 (A255567 (- n 1)))) (else (A255411 (A255567 (/ n 2))))))

;; Now offset 1:
(definec (A255567 n) (cond ((<= n 2) n) ((odd? n) (+ 1 (A255567 (- n 1)))) (else (A255411 (A255567 (/ n 2))))))

;; At least same for terms a(2) - a(255):
(define A255567v2 (MATCHING-POS 2 1 (lambda (n) (= (+ 1 (A256450 (+ 1 (A255411 n)))) (A255411 (A256450 (+ 1 n)))))))


;;;;;;;;;;;;;;;;;;;;

(define A257262 (MATCHING-POS 1 0 (lambda (n) (= 2 (A257079 n)))))
(define A257263 (MATCHING-POS 1 0 (lambda (n) (= 3 (A257079 n)))))

(define A255341 (MATCHING-POS 1 0 (lambda (n) (= 1 (A257511 n)))))
(define A255342 (MATCHING-POS 1 0 (lambda (n) (= 2 (A257511 n)))))
(define A255343 (MATCHING-POS 1 0 (lambda (n) (= 3 (A257511 n)))))

(define (traceA257080iterations n)
  (let loop ((n n))
        (format #t "~a (~a) x ~a =" n (A007623 n) (A257079 n))
        (let ((nextn (A257080 n)))
           (if (= nextn n)
               (format #t " ~a\n" nextn)
               (begin (newline) (loop (A257080 n)))
           )
        )
  )
)


(define (A034968 n)
   (let loop ((n n) (i 2) (s 0))
      (cond ((zero? n) s)
            (else (loop (quotient n i)
                        (1+ i)
                        (+ s (remainder n i))
                  )
            )
      )
   )
)

(definec (A034968v2 n) (if (zero? n) n (+ (A099563 n) (A034968v2 (A257687 n)))))

(definec (A034968v3 n) (if (zero? n) n (+ (A060130 n) (A034968v3 (A257684 n)))))


;; %N A060130 Minimum number of transpositions needed to compose each
;; permutation in the lists A060117/A060118; number of nonzero digits
;; in factorial base representation (A007623) of n.


(define (A060130 n)
   (let loop ((n n) (i 2) (s 0))
      (cond ((zero? n) s)
            (else (loop (quotient n i)
                        (1+ i)
                        (+ s (if (zero? (remainder n i)) 0 1))
                  )
            )
      )
   )
)

(definec (A060130v2 n) (if (zero? n) n (+ 1 (A060130v2 (A257687 n)))))

(define A227130 (MATCHING-POS 1 0 (lambda (i) (even? (A060130 i)))))
(define A227132 (MATCHING-POS 1 0 (lambda (i) (odd? (A060130 i)))))

(define A227148 (MATCHING-POS 1 0 (lambda (i) (even? (A034968 i)))))
(define A227149 (MATCHING-POS 1 0 (lambda (i) (odd? (A034968 i)))))


(define (permute-A060118 elems size permrank)
  (let ((p (vector-head elems size)))
    (let unrankA060118 ((r permrank)
                        (i 1)
                       )
          (cond ((zero? r) p)
                (else
                   (let* ((j (1+ i))
                          (m (modulo r j))
                         )
                      (cond ((not (zero? m)) ;; Swap at i and (i-(r mod (i+1)))
                                (let ((org-i (vector-ref p i)))
                                   (vector-set! p i (vector-ref p (- i m)))
                                   (vector-set! p (- i m) org-i)
                                )
                            )
                      )
                      (unrankA060118 (/ (- r m) j) j)
                   )
                )
          )
    )
  )
)


(define (avg lista) (/ (reduce + 0 lista) (length lista)))

(define Ajoku1 (MATCHING-POS 0 0 (lambda (i) (integer? (avg (n->factbase i))))))

(define Ajoku2
  (MATCHING-POS 0 0
   (lambda (i)
       (let* ((fex (n->factbase i))
              (m (avg fex))
             )
          (and (integer? m) (gs2gs? (list->vector fex) m 0))
       )
   )
  )
)

(define Ajoku3 (MATCHING-POS 0 0 (lambda (i) (integer? (avg (n->factbase (A000290 i)))))))

(define Ajoku4 (MATCHING-POS 0 0 (lambda (i) (= 1 (avg (n->factbase (A000290 i)))))))

(define Ajoku5 (MATCHING-POS 0 0 (lambda (i) (= 2 (avg (n->factbase (A000290 i)))))))

(define Ajoku6 (MATCHING-POS 0 0 (lambda (i) (= 3 (avg (n->factbase (A000290 i)))))))

;; (map Ajoku7 (iota0 6)) --> (0 1 26 195 4666 19888 780568)

(definec (Ajoku7 n) (first-n-where-fun_n-is-i0 (lambda (i) (avg (n->factbase (A000290 i)))) n))

;; A127230 ???
(definec (Ajoku0 n) (first-n-where-fun_n-is-i0 (lambda (i) (avg (n->factbase i))) n))


(define (gs2gs? tv b type)
  (let ((gs (-1+ (expt 2 b))) ;; The ground state, e.g. 7 for b=3.
        (p (vector-length tv)) ;; Period of our tentative pattern
       )
    (let loop ((s gs) (i 0) (visited (list)))
;; When finished, return true only if we have returned back to the ground state:
       (cond ((= i p) (= s gs))
             (else
               (let ((tt (vector-ref tv i)))
                  (cond ((and (= 1 type) (> i 0) (= s gs)) #f)
                        ((and (= 2 type) (memq s visited)) #f)
                        ((and (even? s) (not (zero? tt))) #f) ;; Zero-throw expected!
                        ((not (even? (floor->exact (/ s (expt 2 tt)))))
                                                          #f  ;; Collision!
                        )
                        (else
                           (loop (floor->exact (/ (+ s (expt 2 tt)) 2))
                                 (1+ i)
                                 (if (= 2 type) (cons s visited) visited)
                           )
                        )
                  )
               )
             )
       ) ;; cond
    ) ;; let loop
  )
)


;; Shift factorial expansion of n one digit left:

(define (A153880 n)
   (let loop ((n n) (z 0) (i 2) (f 2))
      (cond ((zero? n) z)
            (else (loop (quotient n i)
                        (+ (* f (remainder n i)) z)
                        (1+ i)
                        (* f (+ i 1))
                  )
            )
      )
   )
)

;; And the same halved:
(define (A153883 n) (/ (A153880 n) 2))

(define (A231720 n) ;; Shift left & rise up. Cf. A255411.
   (let loop ((n n) (z 1) (i 2) (f 2))
      (cond ((zero? n) z)
            (else (loop (quotient n i)
                        (+ (* f (+ 1 (remainder n i))) z)
                        (+ 1 i)
                        (* f (+ i 1))
                  )
            )
      )
   )
)

(define (A231720v2 n) (A220655 (A153880 n)))


  

(define (A001048 n) (+ (A000142 n) (A000142 (- n 1)))) ;; n! + (n-1)!.

;; Other rows of A257505, rows 2 - 5:
(define (A062119 n) (* (- n 1) (A000142 n))) ;; a(n) = n! * (n-1).  /XFER IntSeq.facbase or IntSeq.core
(define (A062119v2 n) (- (A001563 n) (A000142 n)))

(define (A130744 n) (* n (+ 2 n) (A000142 n))) ;; n*(n+2)*n!.   /XFER IntSeq.facbase or IntSeq.core
(define (A130744v2 n) (* n (A001048 (+ 1 n))))
(define (A130744v3 n) (* (A005563 n) (A000142 n)))
(define (A130744v4 n) (- (A000142 (+ 2 n)) (A000142 (+ 1 n)) (A000142 n)))

(define (A213167 n) (- (A000142 n) (A000142 (- n 2)))) ;; n!-(n-2)! Offset=2.

(define (A052571 n) (if (< n 2) 0 (* (- n 2) (A000142 n)))) ;; E.g.f. x^3/(1-x)^2.


;; From juggling.scm, all zero-based, still factorial related:
;; (definec (A007489 n) (if (zero? n) 0 (+ (! n) (A007489 (-1+ n)))))
(define A007489 (PARTIALSUMS 1 1 A000142))
(define (A003422 n) (if (zero? n) n (+ 1 (A007489 (- n 1)))))

(definec (A084555 n) (if (zero? n) 0 (+ (A084556 n) (A084555 (-1+ n))))) ;; PSUM of next
(definec (A084556 n) (first_pos_with_funs_val_gte A007489 n)) ;; n occurs n! times.
(definec (A084557 n) (first_pos_with_funs_val_gte A084555 n)) ;; n occurs A084556(n) times
(definec (A084558 n) (cond ((zero? n) 0) (else (length (n->factbase n)))))
;; After 0, which occurs once, each n occurs A001563(n) times

(define (A048764 n) (A000142 (A084558 n))) ;; Largest factorial <= n.

(define (A257686 n) (if (zero? n) n (* (A099563 n) (A048764 n))))

(define (A257687 n) (- n (A257686 n))) ;; Discard the most significant digit from factorial base representation of n, then convert back to decimal.
(define (A257687v2 n) (factbase->n (cdr (n->factbase n))))


(define (A230415bi x y)
  (let loop ((x x) (y y) (i 2) (d 0))
     (cond
       ((and (zero? x) (zero? y)) d)
       (else (loop (floor->exact (/ x i))
                   (floor->exact (/ y i))
                   (+ i 1)
                   (+ d (if (= (modulo x i) (modulo y i)) 0 1))
             )
       )
     )
  )
)

(define (A230415 n) (A230415bi (A025581 n) (A002262 n)))

(define (A230417 n) (A230415bi (A003056 n) (A002262 n)))


(define (A230419bi x y)
  (let loop ((x x) (y y) (i 2) (d 0))
     (cond
       ((and (zero? x) (zero? y)) d)
       (else (loop (floor->exact (/ x i))
                   (floor->exact (/ y i))
                   (+ i 1)
                   (+ d (- (modulo x i) (modulo y i)))
             )
       )
     )
  )
)

(define (A230419 n) (A230419bi (A025581 n) (A002262 n)))
(define (A230419v2 n) (- (A034968 (A025581 n)) (A034968 (A002262 n))))

(define (A055874 n) (let loop ((m 1)) (if (not (zero? (modulo n m))) (- m 1) (loop (+ 1 m)))))

(define (A007978 n) (let loop ((m 1)) (if (not (zero? (modulo n m))) m (loop (+ 1 m)))))
(define (A236454 n) (A007978 (A000290 n)))

;; By Michel Marcus, January 2014:

(define (A235918 n) (A055874 (A000290 n))) ;; = A236454(n)-1.

(define A235921oldversion (MATCHING-POS 1 1 (lambda (n) (not (= (A071222 (+ n 29)) (A055874 (A000290 n)))))))

(define A235921 (MATCHING-POS 1 1 (lambda (n) (not (= (A236454 n) (A053669 n))))))
(define A235921v2 (MATCHING-POS 1 1 (lambda (n) (not (= (A071222 (- n 1)) (A055874 (A000290 n)))))))

(define (A236432 n) (* (+ n n -1) 210))
(define (A236432v2 n) (- (* 420 n) 210))

(definec (A235921old_variant n)
  (cond ((= n 1) 180)
        ((even? n) (+ 30 (A235921old_variant (- n 1))))
        (else (+ 180 (A235921old_variant (- n 1))))
  )
)

;; (define (A235921v3 n) (- (* (floor->exact (/ (+ 1 n) 2)) 210) (* (A000035 n) 30)))

(define (A236432v2 n) (* (A005408 (- n 1)) 210))

(definec (A020639 n) ;; Lpf(n): least prime dividing n (with a(1)=1).
  (if (< n 2)
      n
      (let loop ((k 2))
           (cond ((zero? (modulo n k)) k)
                 (else (loop (+ 1 k)))
           )
      )
  )
)

(define (A046666 n) (- n (A020639 n)))
(definec (A175126 n) (if (<= n 1) 0 (+ 1 (A175126 (A046666 n)))))


(define (A053669 n) ;; Smallest k >= 2 coprime to n. (We don't need A000040 for this version).
  (let loop ((k 2))
        (cond ((= 1 (gcd n k)) k)
              (else (loop (+ 1 k)))
        )
  )
)

;; Smallest prime not dividing n.
(define (A053669v2 n) (let loop ((i 1)) (cond ((zero? (modulo n (A000040 i))) (loop (+ i 1))) (else (A000040 i)))))

(definec (A053669v3 n) ;; Smallest k >= 2 such that gcd(n-1,k-1) = gcd(n,k).
  (let loop ((k 2))
        (cond ((= (gcd (- n 1) (- k 1)) (gcd n k)) k)
              (else (loop (+ 1 k)))
        )
  )
)

(definec (A071222off1 n) ;; Smallest k >= 2 such that gcd(n-1,k) = gcd(n,k+1).
  (let loop ((k 1))
        (cond ((= (gcd (- n 1) k) (gcd n (+ k 1))) k)
              (else (loop (+ 1 k)))
        )
  )
)

(define (A053669v2 n) (let loop ((i 1)) (cond ((zero? (modulo n (A000040 i))) (loop (+ i 1))) (else (A000040 i)))))

;;  Different from A235918(n)+1. !
(define (A235918plus1 n) (let loop ((nn (* n n)) (i 1)) (cond ((zero? (modulo nn i)) (loop nn (+ i 1))) (else i))))


(definec (A071222 n) ;; Smallest k such that gcd(n,k) = gcd(n+1,k+1).
  (let loop ((k 1))
        (cond ((= (gcd n k) (gcd (+ n 1) (+ k 1))) k)
              (else (loop (+ 1 k)))
        )
  )
)


;; A232094-A232101 are now reserved for your use.

(define (A232094 n) (A060130 (A000217 n)))
(define (A232095 n) (A034968 (A000217 n)))
(define (A232096 n) (A055881 (A000217 n)))


(definec (A232097 n)
   (let ((increment (* 2 (A060818 n))))
      (let loop ((k increment))
            (cond
               ((>= (A232096 (- k 1)) n) (- k 1))
               ((>= (A232096 k) n) k)
;;             ((>= (A232096 (+ k 1)) n) (+ k 1)) ;; Unnecessary.
               (else (loop (+ k increment)))
            )
      )
   )
)

(define (A232097v2 n) (let loop ((k 1)) (if (>= (A232096 k) n) k (loop (+ 1 k)))))

(define (A232101 n) (/ (A000217 (A232097 n)) (A000142 n)))

;; (define A232097 (DISTINCT-POS 1 1 A232096)) ;; (1 3 4 15 32 224 575 4095 76544 512000)

(define (A232097-OLDVERSION-compute-and-output-up-to uplimit outfile) ;; Dirty but quick, until I improve DISTINCT-POS
  (let ((enc-before (make-vector (+ 1 uplimit) 0)))
   (call-with-output-file outfile
     (lambda (outport)
       (let loop ((n 1) (found-n-cases 0) (list-of-found (list)))
          (let ((x (A232096 n)))
            (cond ((> x uplimit)
                     (format #t "Found one more at n=~A: A232096(~A)=~A. Exiting.~%" (+ 1 found-n-cases) n x)
                     (reverse! list-of-found)
                  )
                  ((zero? (vector-ref enc-before x))
                     (vector-set! enc-before x 1)
                     (format #t "n=~A: A232096(~A)=~A~%" (+ 1 found-n-cases) n x)
                     (format outport "~A ~A~%" (+ 1 found-n-cases) n)
                     (flush-output outport)
                     (if (< (+ 1 found-n-cases) uplimit)
                         (loop (+ 1 n) (+ 1 found-n-cases) (cons n list-of-found))
                         (reverse! list-of-found)
                     )
                  )
                  (else (loop (+ 1 n) found-n-cases list-of-found))
            )
          )
       )
     )
   )
  )
)

(define (A232098 n) (A055881 (A000290 n))) ;; Differs from A055874 for the first time at n=840.
(define A232099 (MATCHING-POS 1 1 (lambda (n) (not (= (A232098 n) (A055874 n)))))) ;; 840, 2520, 4200, 5880, ...
(define A055926 (MATCHING-POS 1 1 (lambda (n) (not (= (A055874 n) (A055881 n)))))) ;;
;; (define komps-1 (map -1+ (map A002808 (iota 50))))
;; (define A055926v2 (MATCHING-POS 1 1 (lambda (i) (or (and (zero? (modulo i 12)) (odd? (/ i 12))) (memq (A055881 i) (cdr komps-1))))))


(definec (A232100 n)
  (let ((x (A232099 n)))
     (let loop ((i (if (= 1 n) 1 (A232100 (- n 1)))))
        (if (= (A055926 i) x)
            i
            (loop (+ 1 i))
        )
     )
  )
)


;; A231713-A231724 are now reserved for your use.

(define (A231713bi x y)
  (let loop ((x x) (y y) (i 2) (d 0))
     (cond
       ((and (zero? x) (zero? y)) d)
       (else (loop (floor->exact (/ x i))
                   (floor->exact (/ y i))
                   (+ i 1)
                   (+ d (abs (- (modulo x i) (modulo y i))))
             )
       )
     )
  )
)

(define (A231713 n) (A231713bi (A025581 n) (A002262 n)))
(define (A231714 n) (A231713bi (A003056 n) (A002262 n)))


(define (A231715 n)
  (let loop ((n n) (i 2) (p 1))
     (cond
       ((zero? n) p)
       (else (loop (floor->exact (/ n i))
                   (+ i 1)
                   (* p (modulo (gcd (modulo n i) i) i))
             )
       )
     )
  )
)

(define A231716 (MATCHING-POS 1 1 (lambda (k) (= 1 (A231715 k))))) ;; Cf. A227157


(definec (A084519 n)
  (cond ((< n 3) 1)
        ((= n 3) 3)
        (else (+ (* 3 (A084519 (- n 1))) (* 2 (A084519 (- n 2))) (* 2 (A084519 (- n 3)))))
  )
)

(definec (A084509 n) (if (< n 4) (! n) (* 4 (A084509 (-1+ n)))))
(define A084509v2 (INVERT A084519))


(define (shr n) (if (odd? n) (/ (- n 1) 2) (/ n 2)))

(define (>> n i) (if (zero? i) n (>> (floor->exact (/ n 2)) (- i 1))))
(define (<< n i) (if (<= i 0) (>> n (- i)) (<< (+ n n) (- i 1))))



(define (A005843 n) (* 2 n))
(define (A008585 n) (* 3 n))
(define (A008586 n) (* 4 n))
(define (A008587 n) (* 5 n))
(define (A008588 n) (* 6 n))
(define (A008589 n) (* 7 n))
(define (A008590 n) (* 8 n))
(define (A008591 n) (* 9 n))

(define (A008598 n) (* 16 n))

(define (A016777 n) (1+ (* 3 n)))
(define (A016789 n) (+ 2 (* 3 n)))

(define (A016825 n) (A005843 (A005408 n))) ;; Numbers congruent to 2 mod 4: a(n) = 4n+2.

(define (A004526 n) (floor->exact (/ n 2)))
(define (A002264 n) (floor->exact (/ n 3)))

(define (A102283 n) (- (A000035 (A010872 n)) (A004526 (A010872 n)))) ;; Period 3: repeat (0,1,-1).

(define (A000035 n) (modulo n 2))
(define (A059841 n) (- 1 (A000035 n)))

(define (A010872 n) (modulo n 3))
(define (A010873 n) (modulo n 4))

(define (A021913 n) (floor->exact (/ (modulo n 4) 2))) ;; Periodic with period 4: repeat 0,0,1,1. 
(define (A133872 n) (- 1 (A021913 n))) ;; Period 4: repeat 1,1,0,0. 

(define (A006370 n) (if (even? n) (/ n 2) (+ 1 n n n))) ;; Image of n under the `3x+1' map.
(define (A139391 n) (A000265 (A006370 n)))  ;; Next odd term in Collatz trajectory with starting value n. 

(definec (A258098 n) (if (zero? n) 79 (A006370 (A258098 (- n 1))))) ;; 3x + 1 sequence starting at 79.


(define (A165355 n) (if (even? n) (+ n n n 1) (/ (+ n n n 1) 2)))

(define (A007494 n) (/ (+ n n n (modulo n 2)) 2)) ;; Congruent to 0 or 2 mod 3. 

(define (A032766 n) (+ n (floor->exact (/ n 2)))) ;; Easy!
(define (A032766v2 n) (+ (* 3 (floor->exact (/ n 2))) (modulo n 2))) ;; Numbers that are congruent to 0 or 1 mod 3. 
(define (inv_for_A032766 n) (+ (* 2 (floor->exact (/ n 3))) (modulo n 3)))


(definec (A253786 n) (if (= 2 (modulo n 3)) (+ 1 (A253786 (/ (+ 1 n) 3))) 0))
(definec (A254046 n) (if (= 2 (modulo n 3)) (+ 1 (A254046 (/ (+ 1 n) 3))) 1))

(definec (A253887 n) (if (= 2 (modulo n 3)) (A253887 (/ (+ 1 n) 3)) (inv_for_A032766 n)))


(definec (A126759 n)
  (cond ((zero? n) 1)
        ((even? n) (A126759 (/ n 2)))
        ((zero? (modulo n 3)) (A126759 (/ n 3)))
        ((= 1 (modulo n 6)) (+ 2 (/ (- n 1) 3)))
        (else (+ 1 (/ (+ n 1) 3))) ;; Alternatively: (+ 3 (/ (- n 5) 3)))
  )
)


(definec (A126760 n)
  (cond ((zero? n) n)
        ((even? n) (A126760 (/ n 2)))
        ((zero? (modulo n 3)) (A126760 (/ n 3)))
        ((= 1 (modulo n 6)) (+ 1 (/ (- n 1) 3)))
        (else (/ (+ n 1) 3)) ;; Alternatively: (+ 2 (/ (- n 5) 3)))
  )
)

(define (A126760v2 n) (- (A126759 n) 1))
(define (A126760v3 n) (if (zero? n) n (A253887 (A003602 n))))

(define (A254048 n) (A126760 (A007494 n)))
(define (A254048v2 n) (A126760 (A139391 n))) ;; One-based.


(define (A191450 n) (A191450bi (A002260 n) (A004736 n)))
(define (A191450bi row col) (if (= 1 col) (A032766 row) (A016789 (- (A191450bi row (- col 1)) 1))))

(define (A191450v2 n) (A191450biv2 (A002260 n) (A004736 n)))
(define (A191450biv2 row col) (/ (+ 3 (* (A000244 col) (- (* 2 (A032766 row)) 1))) 6))

(define (A254051bi row col) (/ (+ 3 (* (A000244 row) (- (* 2 (A032766 col)) 1))) 6))
(define (A254051 n) (A254051bi (A002260 n) (A004736 n)))

(define (A254051v2 n) (A191450biv2 (A004736 n) (A002260 n))) ;; Transpose of A191450

(define (A254051v3 n) (A254051biv3 (A002260 n) (A004736 n)))
(define (A254051biv3 row col) (/ (+ 1 (A003961 (* (A000079 (- row 1)) (+ -1 (* 2 (A249745 col)))))) 2))

(define (A254051v4 n) (A254051biv4 (A002260 n) (A004736 n)))
(define (A254051biv4 row col) (/ (+ 1 (A003961 (* (A000079 (- row 1)) (A254050 col)))) 2))

(define (A254051v5 n) (A254051biv5 (A002260 n) (A004736 n)))
(define (A254051biv5 row col) (/ (+ 1 (* (A000244 (- row 1)) (A007310 col))) 2))

(define (A254055 n) (A254055bi (A002260 n) (A004736 n)))
(define (A254055bi row col) (A003602 (A254051bi row col)))
(define (A254055v2 n) (A254055biv2 (A002260 n) (A004736 n)))
(define (A254055biv2 row col) (A003602 (A048673 (A135764bi row (A249745 col)))))

(define (A254101 n) (A254101bi (A002260 n) (A004736 n)))
(define (A254101bi row col) (+ -1 (* 2 (A254055bi row col))))

(define (A254101v2 n) (A254101biv2 (A002260 n) (A004736 n)))
(define (A254101biv2 row col) (A003961 (A254055bi row (A249745 col))))


(define (A254102 n) (A254102bi (A002260 n) (A004736 n)))
(define (A254102bi row col) (A126760 (A254051bi row col)))

(define (A254102v1 n) (A254102biv1 (A002260 n) (A004736 n)))
(define (A254102biv1 row col) (A253887 (A254055bi row col)))

(define (A254102v2 n) (A254102biv2 (A002260 n) (A004736 n)))
(define (A254102biv2 row col) (A126760 (A254101bi row col)))


;; (2n-1) * n^(2n-1) * 4^(n-1)

(define (A067745 n) (numerator (/ (+ n n n -2) (* (expt n (+ n n -1)) (+ n n -1) (expt 4 (- n 1))))))
(define (A067745v2 n) (denominator (/ (* (expt n (+ n n -1)) (+ n n -1) (expt 4 (- n 1))) (+ n n n -2))))

(define (A067745conjectured n) (A000265 (+ n n n -2)))

(define (A067745conjecturedv2 n) (/ (+ n n n -2) (expt 2 (A007814 (+ n n n -2)))))


(define (A067746 n) (denominator (/ (+ n n n -2) (* (expt n (+ n n -1)) (+ n n -1) (expt 4 (- n 1))))))


(define (A254047 n) ;; Inverse to A191450.
  (let ((x (A253887 n))
        (y (A254046 n))
       )
    (* (/ 1 2) (- (expt (+ x y) 2) x y y y -2))
  )
)


(define (A254052 n) ;; Inverse to A254051.
  (let ((x (A254046 n))
        (y (A253887 n))
       )
    (* (/ 1 2) (- (expt (+ x y) 2) x y y y -2))
  )
)


(definec (A183209 n)
   (cond ((<= n 1) n)
         ((even? n) (A016789 (- (A183209 (/ n 2)) 1)))
         (else (A032766 (A183209 (/ (+ n 1) 2))))
   )
)

;; A254099-A254120 are now reserved for your use. 

(definec (A254103 n) 
  (cond ((zero? n) n)
        ((even? n) (+ -1 (* 3 (A254103 (/ n 2)))))
        (else (floor->exact (/ (* 3 (+ 1 (A254103 (/ (- n 1) 2)))) 2)))
  )
)

(definec (A254103v2 n)
   (cond ((< n 1) n)
         ((even? n) (A016789 (- (A254103v2 (/ n 2)) 1)))
         (else (A032766 (+ 1 (A254103v2 (/ (- n 1) 2)))))
   )
)

(definec (A254104 n)
   (cond ((< n 1) n)
         ((= 2 (modulo n 3)) (* 2 (A254104 (/ (+ n 1) 3))))
         (else (+ 1 (* 2 (A254104 (+ -1 (inv_for_A032766 n))))))
   )
)



(define (A016945 n) (+ 3 (* 6 n)))

(define (A007310 n) (- (* 3 n) 1 (A000035 n)))

(define (A084967 n) (* 5 (A007310 n)))
(define (A084968 n) (* 7 (A007775 n)))



(define halve  A004526)
(define double A005843)

(define lsb    A000035)

(define (A000079 n) (expt 2 n))
(define (A000225 n) (- (A000079 n) 1))
(define (A000918 n) (- (expt 2 n) 2)) ;; 2^n - 2. 

(define (A011782 n) (if (zero? n) 1 (A000079 (- n 1))))

(define (A000215 n) (+ 1 (A000079 (A000079 n)))) ;; Fermat numbers: 2^(2^n) + 1,

(define (A051179 n) (A000225 (A000079 n))) ;; 2^(2^n)-1. 

(define (A083318 n) (if (zero? n) 1 (+ 1 (expt 2 n)))) ;; a(0) = 1; for n>0, a(n) = 2^n+1.

(define (A000244 n) (expt 3 n))
(define (A007051 n) (/ (+ 1 (A000244 n)) 2)) ;; (3^n + 1)/2. 

(define (A057198 n) (/ (+ 1 (* 5 (A000244 (- n 1)))) 2))  ;; (5*3^(n-1)+1)/2. 

(define (A029744 n) (cond ((<= n 1) n) ((even? n) (expt 2 (/ n 2))) (else (* 3 (expt 2 (/ (- n 3) 2))))))

(define (A246360 n)
    (cond ((<= n 1) n)
          ((even? n) (/ (+ 1 (A000244 (/ n 2))) 2))
          (else (/ (+ 1 (* 5 (A000244 (/ (- n 3) 2)))) 2))
    )
)


(define (A077957 n) (* (- 1 (A000035 n)) (A016116 n)))
(define (A131575 n) (if (zero? n) 1 (A077957 (- n 1))))

(define (A051437 n) (if (zero? n) 1 (+ (A000079 n) (A077957 (- n 1)))))

;; Note for n > 0, (floor-log-2 n) = (- (binwidth n) 1)

(define (binwidth n) ;; = A029837(n+1)
  (let loop ((n n) (i 0))
     (if (zero? n)
         i
         (loop (floor->exact (/ n 2)) (1+ i))
     )
  )
)

;; This doesn't seem to work for numbers > 1023 bits:
(define (A000523faster n) (cond ((zero? n) -1) (else (floor->exact (/ (log n) (log 2))))))

;; Have to do it with a reliable loop!
(define (A000523 n) (-1+ (binwidth n)))

(define floor-log-2 A000523) ;; An old alias.

(define (A070939 n) (if (zero? n) 1 (binwidth n)))

(define (A070941 n) (binwidth (+ n n 1)))

;; Number of eigenvalues equal to 1 of n X n matrix A(i,j)=1 if j=1 or i divides j.
(define (A083058 n) (if (< n 2) n (- n (A070939 n)))) ;; Offset=1.

;; Largest number k such that there exists an extensional acyclic digraph on n labeled nodes with k sources. Offset=1.
(define (A182220 n) (if (< n 2) n (- (- n 1) (A000523 (- n 1))))) ;; Conjectured!


;; "Odious numbers: numbers with an odd number of 1's in their binary expansion."
(define (A000069off0 n) (/ (+ (* 4 n) 1 (expt -1 (A000120 n))) 2)) ;; Formula by Ralf Stephan, for offset=0
(define (A000069 n) (/ (+ (* 4 n) -3 (expt -1 (A000120 (- n 1)))) 2)) ;; Formula by Ralf Stephan, offset corrected

(define (A232637 n) (A000069 (A000069 n)))
(define (A234011 n) (+ (A000069 n) (A000069 (+ n 1))))

;; Evil numbers: numbers with an even number of 1's in their binary expansion. 
(define (A001969off0 n) (+ n n (A010060 n)))
(define (A001969 n) (+ n n -2 (A010060 (- n 1)))) ;; Starting offset=1.

(define (A052928 n) (+ n (- (modulo n 2))))

(define (A131865 n) (/ (+ (expt 16 (1+ n)) -1) 15)) ;; Partial sums of powers of 16.
(define (A182512 n) (/ (+ (expt 16 n) -1) 5)) ;; Name: "(16^n - 1)/5."
(define (A182512v2 n) (* 3 (A131865 (- n 1)))) ;; Note that A131865(-1) = 0.

(define (A037481 n) (/ (- (/ (+ (expt 4 (1+ n)) (expt -1 n)) 5) 1) 2)) ;; Based on Ralph Stephan's ((4^(n+1) - (-1)^(n+1))/5 - 1)/2


(define (A037481v2 n) (A226062 (A129594 (A227451 n))))

;; A227450-A227454 are now reserved for your use.

;; (map A227451 (iota0 10)) --> (0 1 5 18 77 306 1229 4914 19661 78642 314573)
(define (A227451 n) (if (< n 2) n (+ (A053645 (* 2 (A037481 n))) (- 1 (modulo n 2)))))

(define (A227451v2 n)
      (cond ((< n 2) n)
            ((even? n) (+ 1 (* 4 (+ (A182512 (/ (- n 2) 2)))) (expt 2 (* 2 (- n 1)))))
            (else      (+ 2 (* 16 (A182512 (/ (- n 3) 2))) (expt 2 (* 2 (- n 1)))))
      )
)

;; (define (A227451_even_bisection n) (+ 1 (* 4 (+ (A182512 n))) (expt 2 (+ 2 (* n 4)))))
;; (define (A227451_odd_bisection n) (+ 2 (* 16 (A182512 (- n 2))) (expt 2 (* (- n 1) 4))))


(define (A016116 n) (expt 2 (floor->exact (/ n 2))))

;; 1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,...
(define (A081604 n) ;; Except we have a(0)=0, not 1, as in OEIS!
  (let loop ((n n) (i 0))
     (if (zero? n)
         i
         (loop (floor->exact (/ n 3)) (1+ i))
     )
  )
)

;; 0,0,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,
(define (A062153 n) (- (A081604 n) 1)) ;; Floor [ log_3(n) ]. One-based.
  

(define (A053644 n) (if (zero? n) n (expt 2 (A000523 n))))
;; (define (A053645 n) (- n (expt 2 (A000523 n))))
(define (A053645 n) (cond ((zero? n) n) (else (- n (expt 2 (A000523 n)))))) ;; One-based.

(define (A006257 n) (if (zero? n) 0 (A005408 (A053645 n))))

(define (A004760 n) (if (zero? n) 0 (- (A008585 n) 2 (A006257 (-1+ n)))))

(define (A004754 n) (+ n (A053644 n)))
(define (A004758 n) (+ n (* 5 (expt 2 (A000523 n)))))

(define (A004755 n) (+ n (* 2 (A053644 n))))

(define (A072376 n) (if (< n 2) n (/ (A053644 n) 2)))

;; A run of 2^n 0's followed by a run of 2^n 1's, for n=0, 1, 2, ... 
(define (A079944off2 n) (A000035 (floor->exact (/ n (A072376 n))))) ;; The second most significant bit of n.
(define (A079944 n) (A079944off2 (+ n 2)))

;; A080541 In binary representation: keep the first digit and rotate left the others. 
(define (A080541 n) (if (< n 2) n (A003986bi (A053644 n) (+ (* 2 (A053645 n)) (A079944off2 n)))))

;; A080542 In binary representation: keep the first digit and rotate right the others. 
(define (A080542 n) (if (< n 2) n (+ (A053644 n) (* (A000035 n) (A072376 n)) (A004526 (A053645 n)))))

;; (define A000031slow (cc-generic-Afun A080541 indices-of-nth-binary-forest))
;; (define A000031slow2 (cc-generic-Afun A080542 indices-of-nth-binary-forest))


(define (for-all-nonmsb-bit-rotations? n pred?)
  (let loop ((k (A080542 n)))
        (cond ((= k n) (pred? k)) ;; We came full circle?
;; Then the result just depends whether also the first one of the cycle also fulfills pred?
              ((not (pred? k)) #f) ;; Meanwhile, if in the middle of cycle we find a nonconformant number, then fail.
              (else (loop (A080542 k)))
        )
  )
)


(define (for-any-nonmsb-bit-rotation? n pred?)
  (let loop ((k (A080542 n)))
        (cond
 ;; If in the middle of cycle we find a conformant number or other non-false value, we return it immediately:
              ((pred? k) => (lambda (x) x))
              ((= k n) #f) ;; We came full circle, without finding any matching, return false.
              (else (loop (A080542 k)))
        )
  )
)

(define (A256999 n) ;; largest-of-all-nonmsb-bit-rotations
  (let loop ((k (A080542 n)) (m n))
        (cond ((= k n) m) ;; We came full circle?
              (else (loop (A080542 k) (max m k)))
        )
  )
)

(define (A257697 n) (A053645 (A256999 n)))

(define A257250 (MATCHING-POS 0 0 (lambda (n) (= n (A256999 n)))))
(define A257739 (MATCHING-POS 1 1 (lambda (n) (< n (A256999 n)))))



(define (A003817 n) (if (zero? n) n (-1+ (A005843 (A053644 n)))))
(definec (A003817v2 n) (if (zero? n) n (A003986bi (A003817v2 (-1+ n)) n)))

(define (A062383 n) (1+ (A003817 n)))

;; A065120 Highest power of 2 dividing A057335(n).
;; 0,1,2,1,3,2,1,1,4,3,2,2,1,1,1,1,5,4,3,3,2,2,2,2,1,1,1,1,1,1,1,1,6,...
(define (A065120 n) (if (zero? n) n (1+ (length-of-zero-bit-run-before-msb n))))

;; Brute and dumb:
(define (length-of-zero-bit-run-before-msb n)
  (let loop ((i 0) (maskbit (A072376 n)))
       (if (or (zero? maskbit)
               (not (zero? (modulo (floor->exact (/ n maskbit)) 2)))
           )
           i
           (loop (1+ i) (floor->exact (/ maskbit 2)))
       )
  )
)

(define (A000120 n)
  (let loop ((n n) (i 0))
     (if (zero? n)
         i
         (loop (floor->exact (/ n 2)) (+ i (modulo n 2)))
     )
  )
)


(define (A053829 n) ;; Sum of digits of (n written in base 8).
  (let loop ((n n) (i 0))
     (if (zero? n)
         i
         (loop (floor->exact (/ n 8)) (+ i (modulo n 8)))
     )
  )
)

(define (A053836 n) ;; Sum of digits of n written in base 16.
  (let loop ((n n) (i 0))
     (if (zero? n)
         i
         (loop (floor->exact (/ n 16)) (+ i (modulo n 16)))
     )
  )
)
  

(define A245802 (MATCHING-POS 1 1 (lambda (n) (zero? (modulo n (A053829 n))))))
(define A241989 (MATCHING-POS 1 1 (lambda (n) (zero? (modulo n (A053836 n))))))

(define (A023416 n) (if (zero? n) 1  (- (A029837 (+ 1 n)) (A000120 n)))) ;; Number of 0's in binary expansion of n.
(define (A080791 n) (- (A029837 (+ 1 n)) (A000120 n)))
(definec (A080791v2 n) (if (zero? n) 0 (+ (A080791v2 (- n 1)) (A007814 n) (A036987 (- n 1)) -1)))
(define (A080791v3 n) (if (zero? n) n (- (A000120 (A054429 n)) 1)))

(define (A037861 n)  (- (A023416 n) (A000120 n)))

(define A255568 (MATCHING-POS 1 1 (lambda (n) (= -6 (A037861 n)))))

(define (A102364 n) (A080791 (A003714 n))) ;; Number of 0's in Zeckendorf-binary representation of n.

(define (A092339 n) (A080791 (A003188 n)))

(definec (A004001 n) (if (<= n 2) 1 (+ (A004001 (A004001 (- n 1))) (A004001 (- n (A004001 (- n 1)))))))

(define (A004074 n) (- (* 2 (A004001 n)) n))

(define (A249071 n) (- (A004001 (* 2 n)) n))
(define (A249071v2 n) (/ (A004074 (* 2 n)) 2))

(define (A006949almost_off1 n) (+ n n (A080791 (* 2 n)))) ;; XXX - A well-behaved cousin of the Hofstadter sequence. ?
;; Whose cousin? A004001 or A005185's?

 
;; f(z) = 2z if z>0 else 2|z|+1
(define (Z->N n) (if (positive? n) (* n 2) (+ 1 (* 2 (- n)))))

;; g(n) = n/2 if n even, else (1-n)/2.
(define (N->Z n) (if (even? n) (/ n 2) (/ (- 1 n) 2)))

(define (A001057 n) (N->Z (+ 1 n))) ;; Zero-based version of N->Z

(define (A033999 n) (expt -1 n)) ;; A033999 := n->(-1)^n;

(define (A010060 n) (modulo (A000120 n) 2))
(define (A010059 n) (- 1 (A010060 n)))

(define (A092391 n) (+ n (A000120 n)))


(definec (A010062 n) (if (zero? n) 1 (A092391 (A010062 (- n 1)))))

(definec (A115384 n) (if (zero? n) n (+ (A010060 n) (A115384 (- n 1))))) ;; Partial sums of Thue-Morse numbers A010060
(define (A159481 n) (+ 1 (- n (A115384 n))))
(definec (A245710v2 n) (if (zero? n) n (+ (A010059 n) (A245710v2 (- n 1)))))
(define (A245710 n) (- n (A115384 n)))

(define (A246159 n) (* -1 (A010059 n) (A065620 n)))
(define (A246159v2 n) (* (A010059 n) (A006068 (A245710 n))))
(define (A246159v3 n) (- (A246160 n) (A065620 n)))
(define (A246159v4 n) (* (/ 1 2) (A010059 n) (A006068 n)))

(define (A246160 n) (* (A010060 n) (A065620 n)))
(define (A246160v2 n) (if (zero? n) n (* (A010060 n) (1+ (A006068 (-1+ (A115384 n)))))))
(define (A246160v3 n) (+ (A065620 n) (A246159 n)))

(definec (A233904 n) ;; a(0) = 0, a(2n) = a(n) - n, a(2n+1) = a(n) + n
  (cond ((zero? n) n)
        ((even? n) (- (A233904 (/ n 2)) (/ n 2)))
        (else (+ (A233904 (/ (- n 1) 2)) (/ (- n 1) 2)))
  )
)

(definec (A233905 n) ;; a(0) = 0, a(2n) = a(n), a(2n+1) = a(n) + n.
  (cond ((zero? n) n)
        ((even? n) (A233905 (/ n 2)))
        (else (+ (A233905 (/ (- n 1) 2)) (/ (- n 1) 2)))
  )
)

(definec (A233931 n) ;; a(0) = 0, a(2n) = a(n) + n, a(2n+1) = a(n).
  (cond ((zero? n) n)
        ((even? n) (+ (A233931 (/ n 2)) (/ n 2)))
        (else (A233931 (/ (- n 1) 2)))
  )
)


(define (A228083 n) (A228083bi (A002260 n) (A004736 n))) ;; Binary analogue for A151942. Offset=1.
(define (A228083bi row col) ((rowfun-for-A228083 row) col))

(definec (rowfun-for-A228083 n)
  (implement-cached-function 0 (rowfun-n k)
      (cond ((= 1 k) (A010061 n))
            (else (A092391 (rowfun-n (- k 1))))
      )
  )
)


(define (A228084 n) (A228083bi (A004736 n)  (A002260 n)))

(definec (A228085_slow_but_sure n) (add (lambda (k) (if (= n (A092391 k)) 1 0)) 0 n))
(definec (A228085 n) (add (lambda (k) (if (= n (A092391 k)) 1 0)) (A083058 n) n))
(define (A228085nc n) (add (lambda (k) (if (= n (A092391 k)) 1 0)) (A083058 n) n))


;; A228082-A228091 are now reserved for your use.
;; (define A228082upto1024safely (uniq (sort (map A092391 (iota0 2048)) <)))

;; (define (A228082quick_and_dirty n) (list-ref A228082upto1024safely (- n 1))) ;; One-based. First quick and dirty version.

(define A228082 (NONZERO-POS 1 0 A228085))


;; "Binary self or Colombian numbers (not of form n + sum of binary digits of n)."
;; (define A010061 (COMPLEMENT 1 A228082))
(define  A010061 (ZERO-POS 1 0 A228085))

;; (define (foo n) (if (zero? (A228085 n)) 0 (- (A228086 n) (+ (A083058 n) 1))))

(definec (A228086_slow_but_sure n)
 (let loop ((k 0))
    (cond ((> k n) 0)
          ((= n (A092391 k)) k)
          (else (loop (+ 1 k)))
    )
 )
)

(definec (A228086 n)
 (if (zero? n) n
     (let loop ((k (+ (A083058 n) 1)))
        (cond ((> k n) 0)
              ((= n (A092391 k)) k)
              (else (loop (+ 1 k)))
        )
     )
 )
)

(definec (A228087_slow_but_sure n)
 (let loop ((k n))
    (cond ((zero? k) 0)
          ((= n (A092391 k)) k)
          (else (loop (- k 1)))
    )
 )
)


(definec (A228087 n)
 (let loop ((k n))
    (cond ((<= k (A083058 n)) 0)
          ((= n (A092391 k)) k)
          (else (loop (- k 1)))
    )
 )
)

(define A228088 (MATCHING-POS 1 0 (lambda (k) (= 1 (A228085 k))))) ;; Positions of 1's in A228085.

(define (A228089 n) (A228086 (A228088 n)))

(define A228090 (MATCHING-POS 1 0 (lambda (k) (= 1 (A228085 (A092391 k))))))

;; (define (A228089 n) (A228086 (A228082 n))) ;; Old, different versions.
;; (define (A228090 n) (A228087 (A228082 n)))
;; (define A228089sorted_upto1024 (sort (map A228089 (iota 1024)) <))
;; (define (A228089sorted n) (list-ref A228089sorted_upto1024 (- n 1)))
;; (define A228091old (COMPLEMENT 1 A228089sorted))
;;(define A228091 (MATCHING-POS 1 1 (lambda (n) (and (> (A228085 (A092391 n)) 1) (not (= n (A228086 (A092391 n))))))))
;;(define A228091old (MATCHING-POS 1 1 (lambda (n) (and (> (A228085 (A092391 n)) 1) (> n (A228086 (A092391 n)))))))

(define A228091 (MATCHING-POS 1 1 (lambda (n) (> n (A228086 (A092391 n)))))) ;; The first condition was unnecessary.

;; A228236-A228239 are now reserved for your use.
(define A228236 (MATCHING-POS 1 0 (lambda (k) (> (A228085 (A092391 k)) 1))))
(define A228236v2 (COMPLEMENT 1 A228090))

(define A228237 (MATCHING-POS 1 1 (lambda (n) (< n (A228087 (A092391 n))))))


(definec (A227643deficient n)
  (cond ((zero? n) 1)
        ((zero? (A228085 n)) 1)
        ((= 1 (A228085 n)) (+ 1 (A227643deficient (A228086 n))))
        ((= 2 (A228085 n)) (+ 1 (A227643deficient (A228086 n)) (A227643deficient (A228087 n))))
        (else (error "Not yet implemented for cases where n has more than two immediate children!"))
  )
)

;; Mirrors the "formula":
;; a(0)=1; and for n>0, if A228085(n)=0 then a(n)=1; else 1+sum_{i=A228086(n)..A228087(n)} [A092391(i) = n]*a(i). 
(definec (A227643slow n)
  (cond ((zero? n) 1)
;;      ((zero? (A228085 n)) 1) ;; Actually unnecessary.
        (else (+ 1 (add (lambda (i) (* (if (= (A092391 i) n) 1 0) (A227643slow i)))
                        (A228086 n)
                        (A228087 n)
                   )
              )
        )
  )
)

(definec (A227643full n)
  (cond ((zero? n) 1)
        (else (+ 1 (add (lambda (i) (if (= (A092391 i) n) (A227643full i) 0))
                        (A228086 n)
                        (A228087 n)
                   )
              )
        )
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A007895 n)  (A000120 (A003714 n))) ;; Number of 1-fibits in Z.E.

(define (A095076 n)  (A010060 (A003714 n))) ;; Parity of 1-fibits in Z.E.
(define (A095111 n)   (- 1 (A095076 n))) ;; Its complement.

(define (A002450 n) (/ (- (expt 4 n) 1) 3))
(define (A020988 n) (* 2 (A002450 (+ n 1))))

(define (A028552 n) (* n (+ n 3)))
(define (A028387 n) (1+ (A028552 n)))

(define (A028347 n) (- (* n n) 4)) ;; n^2 - 4. From n=2: 0, 5, 12, 21, ...
(define (A028875 n) (- (* n n) 5)) ;; n^2 - 5. From n=3: 4,11,20,31,...
(define (A028884 n) (- (* n n) 8)) ;; n^2 - 8. From n=3: 1,8,17,28,41,...

(define (A072197 n) (/ (- (* 10 (expt 4 n)) 1) 3)) ;; I trust Bottomley's formula.
(define (A072197v2 n) (+ 3 (* 10 (A002450 n))))

(define (A080675 n) (/ (- (* 5 (expt 4 n)) 8) 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definec (A005179 n) (let loop ((k 1)) (if (= n (A000005 k)) k (loop (+ 1 k))))) ;; Smallest number with exactly n divisors.


(definec (A036459 n) (if (<= n 2) 0 (+ 1 (A036459 (A000005 n))))) ;; Number of iterations required to reach stationary value when repeatedly applying d, the number of divisors function (A000005)


(define A182859 (MATCHING-POS 1 1 (COMPOSE even? A036459))) ;; Numbers n such that A036459(n) is even. 

(define A080218 (MATCHING-POS 1 1 (COMPOSE odd? A036459))) ;; Numbers n such that A036459(n) is odd. 

(define (A262683 n) (- 1 (A000035 (A036459 n)))) ;; Characteristic function for A182859
(define (A262684 n) (A000035 (A036459 n))) ;; Characteristic function for A080218.

(definec (A262685 n) (if (zero? n) n (+ (A262683 n) (A262685 (- n 1))))) ;; Least monotonic left inverse for A182859
(definec (A262685off1 n) (if (= 1 n) n (+ (A262683 n) (A262685off1 (- n 1))))) ;; In OEIS with offset=1.

;; entanglement permutations:

(definec (A262691 n)
  (cond ((<= n 1) n)
        ((zero? (A262684 n)) (* 2 (A262691 (+ -1 (A262685 n))))) ;; One of A182859 > 1, mapped to even numbers.
        (else (+ 1 (* 2 (A262691 (- n (A262685 n)))))) ;; One of A080218, 3, 5, 6, 7, 8, 10, mapped to odd numbers.
  )
)


(definec (A262692 n)
  (cond ((<= n 1) n)
        ((even? n) (A182859 (+ 1 (A262692 (/ n 2)))))
        (else (A080218 (A262692 (/ (- n 1) 2))))
  )
)

;; (same-intfuns1? A000027 (COMPOSE A262691 A262692) 256) --> #t
;; (same-intfuns1? A000027 (COMPOSE A262692 A262691) 256) --> #t


(define (A049820 n) (- n (A000005 n)))

(define A245388 (MATCHING-POS 1 0 (lambda (n) (square? (A049820 n)))))

(define (A099777 n) (A000005 (+ n n))) ;; Number of divisors of 2n. 
(define (A099774 n) (A000005 (+ n n -1))) ;; Number of divisors of 2*n-1.

(define (A263084 n) (- (A263086 n) (A263085 n)))

(definec (A263085 n) (if (= 1 n) (A099774 n) (+ (A099774 n) (A263085 (- n 1))))) ;; Partial sums of A099774.
(definec (A263086 n) (if (= 1 n) (A099777 n) (+ (A099777 n) (A263086 (- n 1))))) ;; Partial sums of A099777.


;; (define A002182 (RECORD-POS 1 1 A000005))
(define A002182 (RECORD-POS 1 1 (lambda (n) (A000005 n)))) ;; Because A000005 is not loaded until after this module.

(definec (A002183 n) (A000005 (A002182 n)))

(define (A054481 n) (gcd (A002182 (+ 1 n)) (A002182 n))) ;; Note the offset in OEIS!

;; A262501-A262522 are now reserved for your use. 
(define (A262501 n) (- (A002182 (+ 1 n)) (A002182 n)))


(define A261100 (LEFTINV-LEASTMONO 1 1 A002182))

(define (A261100v2 n) (let loop ((k 1)) (if (> (A002182 k) n) (- k 1) (loop (+ 1 k)))))

(definec (A070319 n) (if (= 1 n) n (max (A000005 n) (A070319 (- n 1)))))

(define (A070319v2 n) (A002183 (A261100 n))) ;; Max( tau(k) : k=1,2,3,...,n ) where tau(n)=A000005(n) is the number of divisors of x. 

(define (A263096 n) (A000196 (A002182 n)))

(define (A263097 n) (- (A263096 (+ n 1)) (A263096 n)))

(define (A263098 n) (A070319 (A000290 n)))

(definec (A155043 n) (if (zero? n) n (+ 1 (A155043 (A049820 n)))))


(define (A262518 n) (A155043 (+ n n))) ;; Even bisection, offset=0.

(define (A262519 n) (A155043 (+ n n 1))) ;; Odd bisection, offset=0.

(define (A262520 n) (- (A262519 n) (A262518 n)))

(define A262521 (MATCHING-POS 1 1 (COMPOSE negative? A262520)))
;; (112 113 544 545 684 760 930 1306 1514 1522)
;; Not: (113 221 223 338 512 518 545 547 578 613 764 930 1013 1301 1303 1513 1515 1625 1627 1985 1987)

(definec (A262676 n) (if (zero? n) n (+ (- 1 (A000035 n)) (A262676 (A049820 n))))) ;; How many even numbers
(definec (A262677 n) (if (zero? n) n (+ (A000035 n) (A262677 (A049820 n))))) ;; and odd numbers encountered on path?

;; (same-intfuns0? A155043 (lambda (n) (+ (A262676 n) (A262677 n))) 1024) --> #t

;; How many times a perfect square > 0 is encountered on path? (Including the initial term if it is a square, but excluding the terminal zero.) How many times the parity of terms on the path change?
;; Is even for even terms, odd for odd terms.
;; If A262514 is an infinite sequence (and A259934 unique), then the values grow without limit, i.e. there exists
;; an ever-increasing lower bound for each a(n):
(definec (A262680 n) (if (zero? n) n (+ (A010052 n) (A262680 (A049820 n)))))


(define (A262681 n) (A262680 (+ n n 1))) ;; Odd bisection, offset=0.
(define (A262682 n) (A262680 (+ n n))) ;; Even bisection, offset=0.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Run this first:
;; (define vecA259934 (read-b-file-to-vector "seqs/b259934_upto64800_from_Robert_Israel.txt" 64801))

(define (A259934 n) (vector-ref vecA259934 n)) ;; Falcao, (unique?) infinite trunk

(define (A259935 n) (- (A259934 n) (A259934 (- n 1))))

(definec (A262679 n)
  (cond ((= 1 (A262693 n)) n)
        (else (A262679 (A049820 n)))
  )
)

(definec (A262904 n)
  (cond ((= 1 (A262693 n)) (A262694 n))
        (else (A262904 (A049820 n)))
  )
)

(define (A262904v2 n) (A262694 (A262679 n)))

(definec (A263254 n) (if (= 1 (A262693 n)) 0 (+ 1 (A263254 (A049820 n)))))
(define (A263254v2 n) (- (A155043 n) (A262904 n)))

;; (same-intfuns0? A263254 A263254v2 32769) --> #t

(definec (A263275 n) (if (= 1 (A262693 n)) 1 (+ 1 (A263275 (A049820 n))))) ;; Row for n in A263255. Cf. A263274.
(define (A263275v2 n) (+ 1 (A263254 n)))


(define (A263255 n) (A263255bi (A002262 n) (+ 1 (A025581 n)))) ;; Zero-based.
(define (A263256 n) (A263255bi (A025581 n) (+ 1 (A002262 n))))

(define (A263255bi row col) ((rowfun-for-A263255 row) col))

(definec (rowfun-for-A263255 n) (if (zero? n) (COMPOSE A259934 -1+) (MATCHING-POS 1 0 (lambda (k) (= n (A263254 k))))))

(definec (A263274 n) ;; Column-index for n in A263255. Cf. A262904, A263275.
  (let ((rowf (rowfun-for-A263255 (A263254 n))))
           (let loop ((i 1)) (if (= n (rowf i)) i (loop (+ 1 i))))
  )
)


(definec (A263274old n) ;; Column-index for n in A263255. Cf. A262904.
  (cond ((= 1 (A262693 n)) (+ 1 (A262694 n))) ;; Unnecessary clause.
        (else
          (let ((rowf (rowfun-for-A263255 (A263254 n))))
             (let loop ((i 1)) (if (= n (rowf i)) i (loop (+ 1 i))))
          )
        )
  )
)

;; (map A263274 (iota0 45))
;; (1 1 2 1 2 1 3 1 2 2 3 3 4 3 4 4 5 2 5 1 6 2 6 1 3 4 5 1 6 1 7 1 5 2 8 1 2 1 7 2 7 1 9 1 6 2)


;; (map (lambda (row) (map (lambda (col) (A263255bi row col)) (iota 14))) (iota0 19))
;;    0,   2,   6,  12,  18,  22,  30,  34,  42,  46,  54,  58,  62,  70
;;    1,   9,  10,  25,  26,  28,  38,  49,  52,  66,  68,  74,  76,  80
;;    3,   4,  11,  14,  32,  44,  48,  81,  86,  88, 116, 130, 135, 175
;;    5,   8,  13,  15,  16,  20,  40,  50,  56,  60,  83,  85,  92, 134
;;    7,  17,  24,  72,  87,  98, 139, 141, 142, 150, 202, 208, 225, 228
;;   19,  21,  84,  89,  91, 143, 145, 146, 147, 148, 206, 220, 227, 301
;;   23,  93,  95,  96, 100, 149, 153, 154, 160, 212, 229, 240, 305, 356
;;   27,  97,  99, 104, 108, 151, 158, 224, 248, 307, 309, 379, 381, 385
;;   29,  36, 101, 105, 120, 155, 164, 232, 260, 264, 311, 324, 383, 387
;;   31,  33, 103, 107, 128, 132, 157, 159, 276, 280, 313, 389, 391, 453
;;   35, 109, 111, 136, 140, 161, 165, 393, 395, 399, 461, 465, 532, 540
;;   37,  39, 113, 115, 117, 163, 167, 171, 397, 401, 403, 405, 463, 467
;;   41,  45, 119, 173, 407, 471, 473, 475, 568, 571, 572, 573, 575, 659
;;   43,  47, 123, 177, 409, 411, 477, 483, 484, 577, 578, 579, 580, 585
;;   51, 179, 413, 415, 479, 481, 495, 581, 583, 587, 589, 594, 671, 676
;;   53,  55, 181, 183, 417, 485, 591, 595, 602, 612, 673, 681, 877, 879
;;   57, 185, 187, 189, 419, 423, 487, 489, 593, 610, 683, 685, 693, 881
;;   59,  63,  64, 191, 195, 196, 421, 425, 427, 491, 493, 597, 614, 618
;;   61, 193, 197, 429, 435, 497, 599, 603, 622, 691, 705, 893, 895, 897
;;   65, 199, 201, 431, 499, 501, 601, 605, 626, 628, 695, 711, 899, 901

(define A263257 (RECORD-POS 0 0 A263254))
(define (A263257v2 n) (A263255bi n 1)) ;; First column, zero-based. All terms odd after 0, and in monotone order?


(define (A263258 n) (A049820 (A263257 n))) ;; One-based.

;;
;; Claim 1:
;;  Sequence is strictly increasing.
;;
;; Remark: First recall that the function A049820(n),
;; even though it holds that A049820(n) < n for all n >= 1,
;; it still does not generally imply that if a < b, then A049820(a) < A049820(b).
;, E.g. as a counter-example we have: A049820(81) = 76 and A049820(79) = 77.

;; Proof for claim 1, by contradiction:
;;   Suppose we had a(n+1) < a(n)
;;   then we would have A049820(a(n+1)) < a(n) as well,
;;   but by the definition of A253255 (how the table is organized) we know
;;   that for all terms k of row n, A263254(k) = n,
;;   and that A263254(A049820(a(n+1))) = A263254(a(n+1))-1 = A263254(a(n)) = n
;;   which in this case means that A049820(a(n+1)) should have been
;;   listed instead of a(n) as the earliest representative of all numbers k with A263254(k) = n,
;;   thus it cannot be true that a(n+1) < a(n) for any n.
;;
;; Claim 2:
;;  Sequence A049820(a(n+1)) is strictly increasing.
;;
;; Proof.
;;  a(n) < a(n+1) for all n. [Proved above.]
;; Either A049820(a(n+1)) = a(n) [a(n+1) is a(n)'s direct descendant],
;; or A049820(a(n+1)) is in some other column (than the first one) at row n,
;; but as all rows are by definition monotonic, we have:
;;  a(n) <= A049820(a(n+1)). As A049820(n) < n, we get
;; a strict inequality: A049820(a(n)) < A049820(a(n+1)).
;; QED.

;; Maybe a shorter proof: By definition of table A263255, the metric A263254(k) grows when going
;;  downwards each column, and on the other hand, terms k on each row are listed
;;  in strictly increasing order.

;;
;; Do we have any cases such as a(n) = A049820(^d)(a(n+d)), where d > 1,
;; but between at a(n+1) .. a(n+d-1) there would be nodes from some other branches?
;;
;; That is, there would be trajectory of A049820(^k)(a(x)) that applying column index of A263255
;; to its members would not be a monotonic function, but would move among columns back and forth?
;;



;; Yes, A263257 is strictly increasing, because ...
;; (An attempt of proof follows):
;;
;; At least we know that A049820(n) < n,
;; and if there were any a(n+1) < a(n) [this does not necessarily imply A049820(a(n+1)) < A049820(a(n)) !]
;; then A049820(a(n+1)) < a(n) as well, but A263254(A049820(a(n+1))) = A263254(a(n+1))-1
;; which means that A049820(a(n+1)) should have been listed as A263257(n), instead
;; of what it really is, unless A049820(a(n+1)) >= A049820(a(n)),
;; and on the other hand, it cannot be A049820(a(n+1)) = a(n), because a(n+1) < a(n).
;; So we are concerned about the case A049820(a(n+1)) >= A049820(a(n)), is it possible?
;; The equivalence A049820(a(n+1)) = A049820(a(n)) is not possible, because
;; A263254(A049820(a(n+1))) = n but A263254(A049820(a(n))) = n-1.

;; So is it possible to have a(n+1) < a(n), but A049820(a(n+1)) > A049820(a(n)) 
;; In that case A049820(a(n+1)) should have been listed instead of A049820(a(n))
;; as the term a(n-1).

;; If we have a(n) > a(n+1), then a(n) cannot be a direct descendant of a(n+1). (because A049820 always subtracts)
;; 
;; Also if we apply A049820 same number of times to both of them, it cannot make them
;; equal, because their distance to the main trunk is different.
;; But if a(n) > a(n+1), then a(n) > A049820(a(n+1)) as well, and
;; thus A049820(a(n+1)) [which must be a different node than a(n), see above]
;; should have been listed here as the n-th term instead of a(n).

;; Does a(n) < a(n+1) [proved above] imply that A049820(a(n)) < A049820(a(n+1)) ?
;; (in all cases, even at positions where one branch is finished, and the next
;; term is picked from some other branch?)

;; A049820(A263257(n+1)) is either A263257(n) (= a(n) in this notation) or in some other column in
;; row n, but as all rows are by definition monotonic, we have:
;; a(n) <= A049820(a(n+1)). As A049820 always subtracts, we have A049820(a(n)) < A049820(a(n+1)). QED.
;;

;; By definition, A263254(A049820(a(n+1))) = n, A263254(A049820(a(n))) = n-1.
;;
;; Reductio ad absurdum?

;; Also after repeated applications of A049820 ? And after applying A262904 ?
(define (Ajokutus1 n) (A263255bi n 2)) ;; Second column, zero-based.

(define (Ajokutus2 n) (A262904 (A263257 n))) ;; The point where connects to the A259934. Must be monotonic?

;; Another array: Apply A049820 to all rows. (Shifts the row 0 one right. Term at the topleft corner, keep as zero?).

(define A263261 (MATCHING-POS 1 0 (lambda (n) (= 1 (A263254 n)))))
(define A263262 (MATCHING-POS 1 0 (lambda (n) (= 2 (A263254 n)))))
(define A263263 (MATCHING-POS 1 0 (lambda (n) (= 3 (A263254 n)))))
(define A263264 (MATCHING-POS 1 0 (lambda (n) (= 4 (A263254 n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definec (A262905 n) (if (zero? n) n (+ 1 (A262905 (A262904 n)))))

(define (A262906 n) (- n (A262904 n)))

(definec (A262907 n) (if (zero? (A262904 n)) 0 (+ 1 (A262907 (A262906 n)))))

(define A262694 (LEFTINV-LEASTMONO 0 0 A259934))

(define (A262693 n) (if (zero? n) 1 (- (A262694 n) (A262694 (- n 1)))))


(definec (A262522 n)
  (cond ((= 1 (A262693 n)) 0)
        ((= 0 (A060990 n)) n)
        (else
           (let loop ((m 0) (k (A262686 n)))
              (cond ((<= k n) m)
                    ((= n (A049820 k)) (loop (max m (A262522 k)) (- k 1)))
                    (else (loop m (- k 1)))
              )
           )
        )
  )
)

(definec (A262695 n)
  (cond ((= 1 (A262693 n)) 0)
;;      ((= 0 (A060990 n)) 1) ;; Actually, unnecessary!
        (else
           (let loop ((s 0) (k (A262686 n)))
              (cond ((<= k n) (+ 1 s))
                    ((= n (A049820 k)) (loop (max s (A262695 k)) (- k 1)))
                    (else (loop s (- k 1)))
              )
           )
        )
  )
)


(definec (A262696 n)
  (cond ((= 1 (A262693 n)) 0)
        ((= 0 (A060990 n)) 1)
        (else
           (let loop ((s 0) (k (A262686 n)))
              (cond ((<= k n) s)
                    ((= n (A049820 k)) (loop (+ s (A262696 k)) (- k 1)))
                    (else (loop s (- k 1)))
              )
           )
        )
  )
)


(definec (A262697 n)
  (cond ((= 1 (A262693 n)) 0)
;;      ((= 0 (A060990 n)) 1) ;; Actually, unnecessary!
        (else
           (let loop ((s 0) (k (A262686 n)))
              (cond ((<= k n) (+ 1 s))
                    ((= n (A049820 k)) (loop (+ s (A262697 k)) (- k 1)))
                    (else (loop s (- k 1)))
              )
           )
        )
  )
)



(define (A263087 n) (A060990 (A000290 n)))
(define (A263088 n) (A262697 (A000290 n)))

;; A263250 - A263281 reserved for you.

;; A263250 - A263252: Starting offset 0.
(define (A263250 n) (A263087 (+ n n)))
(define (A263251 n) (A263087 (+ n n 1)))

(definec (A263252 n) (if (zero? n) (A263250 n) (+ (A263250 n) (A263252 (- n 1)))))
(definec (A263253 n) (if (zero? n) (A263251 n) (+ (A263251 n) (A263253 (- n 1)))))


(define A263092 (NONZERO-POS 0 0 A263087))

(define A263093 (ZERO-POS 1 0 A263087))
(define A263093v2 (ZERO-POS 1 0 (COMPOSE -1+ A263088)))



;; A262888-A262909 

;; (same-intfuns0? A262890 (lambda (n) (+ (A262888 n) (A262889 n))) 8107) --> #t

(definec (A262888 n)
  (let ((t (A259934 n)))
    (let loop ((s 0) (k (A259934 (+ 1 n))))
         (cond ((<= k t) s)
               ((= t (A049820 k)) (loop (+ s (A262697 k)) (- k 1)))
               (else (loop s (- k 1)))
         )
    )
  )
)

(definec (A262889 n)
  (let ((t (A259934 n)) (u (A259934 (+ 1 n))))
    (let loop ((s 0) (k (A262686 t)))
         (cond ((<= k u) s)
               ((= t (A049820 k)) (loop (+ s (A262697 k)) (- k 1)))
               (else (loop s (- k 1)))
         )
    )
  )
)

(definec (A262890 n)
  (let ((t (A259934 n)))
    (let loop ((s 0) (k (A262686 t)))
         (cond ((<= k t) s)
               ((= t (A049820 k)) (loop (+ s (A262697 k)) (- k 1)))
               (else (loop s (- k 1)))
         )
    )
  )
)

(define (A262891 n) (A060990 (A259934 n))) ;; Branching degree for nodes in the infinite trunk of Falcao's tree.

(define A262892 (ZERO-POS 1 0 (COMPOSE -1+ A262891)))
(define A262892v2 (ZERO-POS 1 0 A262890))

;; (same-intfuns1? A262892 A262892v2 120) --> #t


(definec (A262893 n) (if (zero? n) (A262890 0) (+ (A262893 (- n 1)) (A262890 n)))) ;; Partial sums of A262890. Cf. A255333

(define (A262894 n) (- (A262888 n) (A262889 n))) ;; Cf. A255331.

(definec (A262895 n) (if (zero? n) (A262894 0) (+ (A262895 (- n 1)) (A262894 n)))) ;; Partial sums of A262894. Cf. A255332

(definec (A262896 n) ;; A259934(n) = A262679(A262896(n)).
  (let ((t (A259934 n)))
    (let loop ((m t) (k (A262686 t)))
         (cond ((<= k t) m)
               ((= t (A049820 k)) (loop (max m (A262522 k)) (- k 1)))
               (else (loop m (- k 1)))
         )
    )
  )
)

(define (A262897 n) (A259934 (A262892 n)))
(define A262897v2 (MATCHING-POS 1 0 (lambda (n) (and (= 1 (A060990 n)) (= 1 (A262693 n))))))

(define (A262898 n) (A262898bi (A002260 n) (A004736 n)))

(define (A262898bi row col) (if (= 1 row) (A045765 col) (if (zero? (A262898bi (- row 1) col)) 0 (A049820 (A262898bi (- row 1) col)))))

(define (A262899 n) (A262898bi (A004736 n) (A002260 n)))

(definec (A262900 n)
   (let loop ((s 0) (k (A262686 n)))
         (cond ((<= k n) s)
               ((= n (A049820 k)) (loop (+ s (if (zero? (A060990 k)) 1 0))  (- k 1)))
               (else (loop s (- k 1)))
         )
   )
)

(define A262901 (NONZERO-POS 1 0 A262900))


(define (A262902 n) (A049820 (A045765 n)))

(define A262903 (MATCHING-POS 1 0 (lambda (n) (and (not (zero? (A060990 n))) (= (A060990 n) (A262900 n))))))
(define A262903v2 (MATCHING-POS 1 0 (lambda (n) (= 2 (A262695 n)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A262676-A262697 are now reserved for your use.


(define (A261085 n) (A155043 (A000040 n)))

(define A261086 (MATCHING-POS 1 1 (lambda (n) (and (> n 1) (> (A261085 (- n 1)) (A261085 n))))))

(define (A261087 n) (A000040 (A261086 n)))

(define (A261088 n) (A155043 (A000290 n)))


(define A261089 (RECORD-POS 0 0 A155043))

(definec (A261089v2 n) (let loop ((k 0)) (if (= n (A155043 k)) k (loop (+ 1 k)))))

(define (A261103 n) (- (A259934 n) (A261089 n)))

(definec (A261104 n) (if (zero? n) n (+ 1 (A261104 (- n (A070319 n))))))

(define A262502 (RECORD-POS 0 0 A261104))
(definec (A262502v2 n) (let loop ((k 0)) (if (= n (A261104 k)) k (loop (+ 1 k)))))

(definec (A262503 n) (let loop ((k (A262502 (+ 2 n)))) (if (= (A155043 k) n) k (loop (- k 1)))))

(define (A262504 n) (- (A262502 (+ 1 n)) (A262502 n)))

(define (A262505 n) (- (A262503 n) (A261089 n)))

(define (A262506 n) (- (A262503 n) (A259934 n)))

(define (A262509 n) (A261089 (A262508 n)))

;; (same-intfuns0? A262509  (COMPOSE A262503 A262508) 38) --> #t
;; (same-intfuns0? A262509  (COMPOSE A259934 A262508) 38) --> #t

(definec (A262908 n)
  (let ((t (A262509 n)))
    (let loop ((k (A002183 (+ 2 (A261100 t)))))
          (cond ((<= (A049820 (+ t k)) t) k)
                (else (loop (- k 1)))
          )
    )
  )
)

(definec (A262909 n)
  (let* ((u (A262509 n))
         (t (A155043 u))
        )
    (let loop ((k u))
          (cond ((< (A155043 (+ u k)) t) k)
                (else (loop (- k 1)))
          )
    )
  )
)


(definec (A060990 n) (if (zero? n) 2 (add (lambda (k) (if (= (A049820 k) n) 1 0)) n (+ n (A002183 (+ 2 (A261100 n)))))))

(definec (A262507 n) (add (lambda (k) (if (= (A155043 k) n) 1 0)) n (A262502 (+ 2 n))))

;;;;;;;;;;;;;;;;;;;;;;;;

(define A263089 (LEFTINV-LEASTMONO 0 0 A261089))

(definec (A263259 n)
   (cond ((zero? n) 1)
         ((= 1 (- (A263089 n) (A263089 (- n 1)))) 1)
         (else
           (let ((d (A155043 n)))
             (let loop ((k (- n 1)))
                (if (= (A155043 k) d)
                    (+ 1 (A263259 k))
                    (loop (- k 1))
                )
             )
           )
         )
   )
)

(definec (A263260 n) (if (zero? n) (A262507 n) (+ (A262507 n) (A263260 (- n 1)))))

(define (A263279 n) (A263259 (A259934 n)))
(define (A263280 n) (- (A262507 n) (A263279 n)))

(define A263270 (COMPOSE 1+ (LEFTINV-LEASTMONO 0 0 A263260)))

;; For preparing big b-files, say first:
;; (define vecA262507 (read-b-file-to-vector "seqs/b262507_upto110880.txt" 110881))
;; 
;; (define A262507old A262507)
;; 
;; (define (A262507 n) (vector-ref vecA262507 n))

(definec (A263265 n) ;; Was defineperm1
   (cond ((zero? n) n)
         ((= 1 (- (A263270 n) (A263270 (- n 1)))) (A261089 (A263270 n)))
         (else
           (let ((p (A263265 (- n 1))) (d (A263270 n))) ;; d = (A155043 p)
             (let loop ((k (+ p 1)))
                (if (= (A155043 k) d)
                    k
                    (loop (+ k 1))
                )
             )
           )
         )
   )
)

(definec (A263265row n)
   (cond ((zero? n) (list n))
         (else
           (let ((end (A263260 (- n 1))))
             (let loop ((k (+ -1 (A263260 n)))
                        (terms (list))
                       )
               (cond ((= k end) (cons (A263265 k) terms))
                     (else (loop (- k 1) (cons (A263265 k) terms)))
               )
             )
           )
         )
   )
)


(define (children-of-n-in-A049820-tree n)
  (let loop ((k (A262686 n)) (children (list)))
        (cond ((<= k n) children)
              ((= (A049820 k) n) (loop (- k 1) (cons k children)))
              (else (loop (- k 1) children))
        )
  )
)


(define (A263271bi row col)
  (cond ((zero? col) row)
        ((A262686 row) =>
           (lambda (lad)
             (if (zero? lad)
                 lad 
                 (A263271bi lad (- col 1))
             )
           )
        )
  )
)

(define (A263271 n) (A263271bi (A002262 n) (A025581 n)))


;; A264965-A264996 are now reserved for your use. 

(definec (A264970 n) (cond ((A262686 n) => (lambda (lad) (if (zero? lad) 0 (+ 1 (A264970 lad)))))))
(definec (A264971 n) (cond ((A262686 n) => (lambda (lad) (if (zero? lad) 1 (+ 1 (A264971 lad)))))))


(define (A263267list_up_to_n_iterations_slow n)
  (let ((terms-produced (list 0)))
    (let loop ((listp terms-produced) (k n))
           (cond ((zero? k) terms-produced)
                 (else
                    (begin
                       (append! terms-produced (children-of-n-in-A049820-tree (car listp)))
                       (loop (cdr listp) (- k 1))
                    )
                 )
           )
    )
  )
)

;; The above is of course much less than optimal with large values of n, so here's an improved version,
;; keeping another list-pointer (endp) pointing to the last non-empty children-list (which are never
;; that long, so their end is found quickly by append!):

(define (A263267list_up_to_n_terms_at_least n)
  (let ((terms-produced (list 0)))
    (let loop ((startp terms-produced) (endp terms-produced) (k (- n 1)))
           (cond ((<= k 0) terms-produced)
                 (else
                    (let ((children (children-of-n-in-A049820-tree (car startp))))
                       (cond ((null? children) (loop (cdr startp) endp k))
                             (else
                                (begin
                                    (append! endp children)
                                    (loop (cdr startp) children (- k (length children)))
                                )
                             )
                       )
                    )
                 )
           )
    )
  )
)

;; Say: (define listA263267 (A263267list_up_to_n_iterations_slow 10411)) ;; A263260(1000) = 10411, (length listaA263267) = 10426

;; Or:  (define listA263267 (A263267list_up_to_n_terms_at_least 10426))

(defineperm1 (A263267 n) (list-ref listA263267 n))
(define (A263268 n) (A263267 (- n)))

;; (define (A263266 n) (A263265 (- n)))

(definec (A263266 n) (if (zero? n) n (+ -1 (A263259 n) (A263260 (- (A155043 n) 1)))))

(define A263281 (FIXED-POINTS 0 0 A263266))


(define (A263269 n) (A263267 (- (A263260 n) 1))) ;; The right edge of A263267.
(define (A264988 n) (if (zero? n) n (A263267 (A263260 (- n 1))))) ;; The left edge of A263267.

;;;;;;;;;;;;;;;;;;;;;;;;

(define A262511 (ZERO-POS 1 1 (COMPOSE -1+ A060990)))

(define A045765 (ZERO-POS 1 1 A060990)) ;; n - d(n) never takes these values. 

(define A263095 (MATCHING-POS 1 1 (lambda (n) (and (= 1 (A010052 n)) (zero? (A060990 n)))))) ;; Squares in A045765.

(define A236562 (NONZERO-POS 1 0 A060990)) ;; Numbers n such that A049820(x) = n has a solution.

(definec (A262512 n) (let ((s (A262511 n))) (let loop ((k s)) (if (= s (A049820 k)) k (loop (+ 1 k))))))

;; (same-intfuns1? A262512  (COMPOSE A082284 A262511) 128) --> #t
;; (same-intfuns1? A262512  (COMPOSE A262686 A262511) 128) --> #t

;; (same-intfuns1? A262511 (COMPOSE A049820 A262512) 1024) --> #t

(define A262513 (MATCHING-POS 1 1 (lambda (n) (= 1 (A060990 (A049820 n))))))

;; a(n) = smallest number k such that k - tau(k) = n, or 0 if no such number exists. 
(definec (A082284 n) (if (zero? n) 1 (let ((u (+ n (A002183 (+ 2 (A261100 n)))))) (let loop ((k n)) (cond ((= (A049820 k) n) k) ((> k u) 0) (else (loop (+ 1 k))))))))

;; a(n) = largest number k such that k - tau(k) = n, or 0 if no such number exists.
(definec (A262686 n) (if (zero? n) 2 (let ((u (+ n (A002183 (+ 2 (A261100 n)))))) (let loop ((k u)) (cond ((= (A049820 k) n) k) ((< k n) 0) (else (loop (- k 1))))))))


(define A263079 (MATCHING-POS 1 1 (COMPOSE negative? (lambda (n) (A263078 n)))))

(definec (A263082 n) (if (zero? n) n (max (A262503 n) (A263082 (- n 1)))))


(definec (A263077 n)
  (let ((t (A155043 n)))
    (let loop ((k (A263082 (+ -1 (A155043 n)))))
         (if (< (A155043 k) t) k (loop (- k 1)))
    )
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some binary stuff transferred from intfun_b.scm 2013-07-09:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A001511 n) (+ (A007814 n) 1))

(define (A136480 n) (if (zero? n) 1 (A007814 (+ n (modulo n 2)))))

(define (A163575 n) (floor->exact (/ n (expt 2 (A136480 n)))))

(definec (A005187 n) (if (< n 2) n (+ n (A005187 (floor->exact (/ n 2))))))
;; (define A005187v2 (PARTIALSUMS 1 1 A001511))

(define (A257126 n) (- (A055938 n) (A005187 n)))

(define A257130 (RECORD-POS 1 1 A257126))


(define (A011371 n) (- n (A000120 n)))

(definec (A174605 n) (if (zero? n) n (+ (A174605 (- n 1)) (A011371 n)))) ;; Also a(n) = A007814(A000178(n)).
;; And: A187059(n) = A249152(n) - A174605(n).

(define (A091512 n) (* n (A001511 n)))

(definec (A143157 n) (if (zero? n) n (+ (A143157 (- n 1)) (A091512 n))))

(define (A249150 n) (A230403 (A001142 n)))
(define (A249151 n) (A055881 (A001142 n)))

(define (A249152 n) (* 2 (A143157 (floor->exact (/ n 2)))))
;; (define (A249152v2 n) (A007814 (A002109 n)))

(define (A249153 n) (* 2 (A143157 n)))
;; (define (A249153v2 n) (A249152 (+ n n))) ;; a(n) = 2*A143157(n).

(define (A060818 n) (expt 2 (A011371 n)))

(define A055938 (COMPLEMENT 1 A005187))
;; It seems that for n>1, A055938(2^n) = (2^(n+1)) + 1,
;; and that in each range [2^n,2^(n+1)-1], there are
;; 2^(n-1) terms.

(define (A254105 n) (A254105bi (A002260 n) (A004736 n)))

(define (A254105bi row col) (if (= 1 col) (A005187 row) (A055938 (A254105bi row (- col 1)))))

(define (A254107 n) (A254105bi (A004736 n) (A002260 n)))

(define (A254105v2 n) (A234017 (A254105bi (A002260 n) (+ 1 (A004736 n)))))
(define (A254107v2 n) (A234017 (A254105bi (A004736 n) (+ 1 (A002260 n)))))


(define (A254106 n) ;; Inverse to A254105
  (let ((col (A254112 n))
        (row (A254111 n))
       )
    (* (/ 1 2) (- (expt (+ col row) 2) col row row row -2))
  )
)


;; (same-intfuns1? A000027 (COMPOSE A254108 A254107) 65) --> #t
;; (same-intfuns1? A000027 (COMPOSE A254107 A254108) 65) --> #t

(define (A254108 n) ;; Inverse to A254107
  (let ((col (A254111 n))
        (row (A254112 n))
       )
    (* (/ 1 2) (- (expt (+ col row) 2) col row row row -2))
  )
)


(definec (A254110 n) (if (zero? (A234017 n)) 0 (+ 1 (A254110 (A234017 n)))))
(definec (A254111 n) (if (zero? (A234017 n)) 1 (+ 1 (A254111 (A234017 n)))))

;; First 18 terms match with A244567: (A pure or a half coincidence?)
(definec (A254112 n) (if (zero? (A234017 n)) (A213714 n) (A254112 (A234017 n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A255555 - A255558, A255559 A255560


(define (A255555 n) (A255555bi (A002260 n) (A004736 n)))

(define (A255555bi row col) (if (= 1 col) (if (= 1 row) 1 (A055938 (- row 1))) (A005187 (+ 1 (A255555bi row (- col 1))))))

(define (A255557 n) (A255555bi (A004736 n) (A002260 n)))

(define (A255555v2 n) (A234017 (A255555bi (A002260 n) (+ 1 (A004736 n)))))
(define (A255557v2 n) (A234017 (A255555bi (A004736 n) (+ 1 (A002260 n)))))


(define (A255556 n) ;; Inverse to A255555.
  (let ((col (A255559 n))
        (row (A255560 n))
       )
    (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2))
  )
)


;; (same-intfuns1? A000027 (COMPOSE A255558 A255557) 65) --> #t
;; (same-intfuns1? A000027 (COMPOSE A255557 A255558) 65) --> #t

(define (A255558 n) ;; Inverse to A255557.
  (let ((row (A255559 n))
        (col (A255560 n))
       )
    (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2))
  )
)

;; One-based column index of n in A255555:
(definec (A255559 n) (if (or (= 1 n) (zero? (A213714 n))) 1 (+ 1 (A255559 (+ -1 (A213714 n))))))

;; One-based row index of n in A255555:
(definec (A255560 n) (cond ((= 1 n) n) ((zero? (A213714 n)) (+ 1 (A234017 n))) (else (A255560 (+ -1 (A213714 n))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A256989-A257000 are now reserved for your use. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (A256995 n) (if (<= n 1) n (A256995bi (A002260 (- n 1)) (A004736 (- n 1)))))

(define (A256995bi row col) (if (= 1 col) (A055938 row) (A005187 (A256995bi row (- col 1)))))

(define (A256996 n) ;; Inverse to A256995.
  (if (= 1 n) n
      (let ((col (A256989 n))
            (row (A256990 n))
           )
        (+ 1 (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2)))
      )
  )
)



;; (same-intfuns1? A000027 (COMPOSE A256995 A256996) 128) --> #t
;; (same-intfuns1? A000027 (COMPOSE A256996 A256995) 128) --> #t


(define (A256997 n) (if (<= n 1) n (A256997bi (A002260 (- n 1)) (A004736 (- n 1)))))

(define (A256997bi row col) (if (= 1 row) (A055938 col) (A005187 (A256997bi (- row 1) col))))

(define (A256998 n) ;; Inverse to A256997.
  (if (= 1 n) n
      (let ((row (A256989 n))
            (col (A256990 n))
           )
        (+ 1 (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2)))
      )
  )
)

;; (same-intfuns1? A000027 (COMPOSE A256997 A256998) 128) --> #t
;; (same-intfuns1? A000027 (COMPOSE A256998 A256997) 128) --> #t

(define (A257264 n) (A257264bi (A002260 n) (A004736 n)))
(define (A257264bi row col) (if (= 1 row) (A055938 col) (A011371 (A257264bi (- row 1) col))))

(define (A257507 n) (A011371 (A055938 n)))
(define (A257507v2 n) (A257264bi 2 n))


;; One-based column index of n in A256995:
(definec (A256989 n) (cond ((= 1 n) 0) ((zero? (A213714 n)) 1) (else (+ 1 (A256989 (A213714 n))))))

;; One-based row index of n in A256995:
(definec (A256990 n) (cond ((= 1 n) 0) ((zero? (A213714 n)) (A234017 n)) (else (A256990 (A213714 n)))))

(definec (A256991 n) (if (not (zero? (A079559 n))) (+ -1 (A213714 n)) (A234017 n)))

(definec (A000523otherway n) (if (= 1 n) 0 (+ 1 (A000523otherway (A256991 n)))))

(definec (A070939otherway n) (if (= 1 n) 1 (+ 1 (A070939otherway (A256991 n)))))

;; (same-intfuns1? A070939  (lambda (n) (+ (A256478 n) (A256479 n))) 8192) --> #t


(define (A256992 n) (+ (A213714 n) (A234017 n)))
(define (A256992v2 n) (if (not (zero? (A079559 n))) (A213714 n) (A234017 n)))

;; (definec (A256992v2 n) (cond ((< n 1) n) ((zero? (A213714 n)) (A234017 n)) (else (A213714 n))))

(definec (A256993 n) (if (= 1 n) 0 (+ 1 (A256993 (A256992 n)))))

(define (A256994 n) (if (< n 4) (+ n 1) (+ (A000079 (- n 2)) 3)))

(definec (A256994v2 n) (if (= 1 n) 2 (A005187 (A256994v2 (- n 1))))) ;; Iterates of A005187 starting from a(1) = 2.



(definec (A256478 n) (if (< n 1) n (+ (A079559 n) (A256478 (if (zero? (A079559 n)) (A234017 n) (+ -1 (A213714 n)))))))

(define (A256478v2 n) (A000120 (A233277 n)))

(define (A256478v3 n) (if (zero? n) n (+ 1 (A080791 (A233275 n)))))


(definec (A256479 n) (if (<= n 1) 0 (+ (- 1 (A079559 n)) (A256479 (if (zero? (A079559 n)) (A234017 n) (+ -1 (A213714 n)))))))

(define (A256479v2 n) (A080791 (A233277 n)))

(define (A256479v3 n) (+ -1 (A000120 (A233275 n))))

(define (A256479v4 n) (+ 1 (- (A000523 n) (A256478 n))))

(definec (A257248 n) (if (= 1 n) 0 (+ (A079559 n) (A257248 (if (zero? (A079559 n)) (A234017 n) (+ -1 (A213714 n)))))))

(definec (A257249 n) (if (zero? n) 1 (+ (- 1 (A079559 n)) (A257249 (if (zero? (A079559 n)) (A234017 n) (+ -1 (A213714 n)))))))


(define (A257248v2 n) (- (A256478 n) 1))
(define (A257249v2 n) (+ 1 (A256479 n)))

;; (same-intfuns1? A256478 (lambda (n) (- (A070939 n) (A256479 n))) 1024) --> #t
;; (same-intfuns1? A256479 (lambda (n) (- (A070939 n) (A256478 n))) 1024) --> #t


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A007949 n)
  (if (zero? n)
      0
      (let loop ((n n) (k 0))
         (cond ((not (zero? (modulo n 3))) k)
               (else (loop (/ n 3) (+ 1 k)))
         )
      )
  )
)

(define (A053735 n) ;; Sum of digits of (n written in base 3). 
  (let loop ((n n) (s 0))
     (if (zero? n)
         s
         (loop (floor->exact (/ n 3)) (+ s (modulo n 3)))
     )
  )
)


(define (A054861 n) (/ (- n (A053735 n)) 2))

(definec (A054861rec n) (if (zero? n) 0 (+ (A007949 n) (A054861rec (- n 1)))))

(define (A060828 n) (expt 3 (A054861 n)))

(define (A118381 n) (* (A060818 n) (A060828 n)))

(define (A122586 n) ;; Leading digit of n expressed in base 3.
  (let loop ((n n) (lastm 0))
     (if (zero? n)
         lastm
         (loop (floor->exact (/ n 3)) (modulo n 3))
     )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beanstalks interlude for base-3:

(definec (A261231 n) (if (zero? n) n (+ 1 (A261231 (* 2 (A054861 n))))))

(definec (A261232 n) (if (zero? n) 1 (+ (A261234 (- n 1)) (A261232 (- n 1)))))
(define (A261232v2 n) (A261231 (A000244 n))) ;; Zero-based.

(definec (A261233 n) (if (zero? n) n (+ (A261234 (- n 1)) (A261233 (- n 1)))))
(define (A261233v2 n) (A261231 (- (A000244 n) 1))) ;; Zero-based.


;; How many steps from (3^(n+1))-1 to (3^n)-1 by iterating 2*A054861(n) ? Cf. also A213709, A261091.
 
(definec (A261234 n) 
 (let ((end (- (A000244 n) 1)))
  (let loop ((k (- (A000244 (+ 1 n)) 1)) (s 0))
        (if (= k end) s (loop (* 2 (A054861 k)) (+ 1 s)))
  )
 )
)



(define (A261234v2 n) (- (A261233 (+ 1 n)) (A261233 n))) ;; Zero-based.
(define (A261234v3 n) (- (A261231 (A000244 (+ 1 n))) (A261231 (A000244 n))))

(define (A261235 n) (- (A261234 (+ 1 n)) (A261234 n))) ;; First differences of A261234.


;; How many steps are needed by iterating 2*A054861(n) to go from (3^(n+1))-1 to first such number whose base-3 representation begins with digit other than 2 ?
 
(definec (A261237 n) 
  (let loop ((k (- (A000244 (+ 1 n)) 1)) (s 0))
        (if (< (A122586 k) 2) s (loop (* 2 (A054861 k)) (+ 1 s)))
  )
)

(define (A261236 n) (- (A261234 n) (A261237 n)))

(define (A261230 n) (- (A261236 n) (A261237 n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (A112765 n)
  (if (zero? n)
      0
      (let loop ((n n) (k 0))
         (cond ((not (zero? (modulo n 5))) k)
               (else (loop (/ n 5) (+ 1 k)))
         )
      )
  )
)


(define (A122841 n)
  (if (zero? n)
      0
      (let loop ((n n) (k 0))
         (cond ((not (zero? (modulo n 6))) k)
               (else (loop (/ n 6) (+ 1 k)))
         )
      )
  )
)


(define (A214411 n)
  (if (zero? n)
      0
      (let loop ((n n) (k 0))
         (cond ((not (zero? (modulo n 7))) k)
               (else (loop (/ n 7) (+ 1 k)))
         )
      )
  )
)


(definec (A046699off0 n) (if (< n 2) 1 (+ (A046699off0 (- n (A046699off0 (- n 1)))) (A046699off0 (- n 1 (A046699off0 (- n 2)))))))

;; a(1) = a(2) = 1, a(n) = a(n - a(n-1)) + a(n-1 - a(n-2)) if n > 2.
(definec (A046699 n)
   (if (< n 3)
       1
       (+ (A046699 (- n (A046699 (- n 1))))
          (A046699 (- n 1 (A046699 (- n 2))))
       )
   )
)


(define (A079559 n) (if (<= n 1) 1 (- (A046699off0 (+ n 1)) (A046699off0 n)))) ;; Charfun of A005187.

(definec (A079559v2 n)
  (if (< n 3)
      (if (< n 2) 1 0)
      (let ((k (ApartsumsA079559 (- n 1))))
         (- 1 (- (floor->exact (/ k 2)) (- (ApartsumsA079559 (- n k)) 1)))
      )
  )
)

(definec (A079559v3 n)
  (if (< n 3)
      (if (< n 2) 1 0)
      (let ((k (A046699off0v2 n)))
         (- 1 (- (floor->exact (/ k 2)) (- (A046699off0v2 (+ 1 (- n k))) 1)))
      )
  )
)

(definec (ApartsumsA079559 n) (if (zero? n) (A079559v2 n) (+ (ApartsumsA079559 (- n 1)) (A079559v2 n))))

(definec (A046699off0v2 n) (if (zero? n) 1 (ApartsumsA079559 (- n 1))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Of course many of these (especially those that occur as columns of
;; Wythoff array, A035513) have elegant formulae as well.
;; See the corresponding entries in OEIS. Here I just wanted
;; to be thorough and systematic:

(definec (A095096 n) ;; fibevil numbers, complement of A020899.
  (cond
    ((= 0 n) 0) ;; 0 is the first fibevil number.
    (else
      (let loop ((i (+ 1 (A095096 (- n 1)))))
         (cond
           ((= 0 (A095076 i)) i) ;; parity of 1-fibits is even?
           (else (loop (+ 1 i))) ;; Try the next one then.
         )
      )
    )
  )
)


(definec (A020899 n) ;; fibodious numbers
  (cond
    ((= 0 n) 1) ;; 1 is the first fibodious number.
    (else
      (let loop ((i (+ 1 (A020899 (- n 1)))))
         (cond
           ((= 1 (A095076 i)) i) ;; parity of 1-fibits is odd?
           (else (loop (+ 1 i))) ;; Try the next one then.
         )
      )
    )
  )
)


(definec (A026274 n) ;; fib00 numbers. Check this!
  (cond
    ((= 0 n) 3) ;; 3 is the first member. it's z.e. being 100.
    (else
      (let loop ((i (+ 1 (A026274 (- n 1)))))
         (cond
           ((= 0 (modulo (A003714 i) 4)) i) ;; z.e. ends as ...00 ?
           (else (loop (+ 1 i))) ;; Try next one then.
         )
      )
    )
  )
)


(definec (A095097 n) ;; fib000 numbers.
  (cond
    ((= 0 n) 5) ;; 5 is the first member, it's z.e. being 1000.
    (else
      (let loop ((i (+ 1 (A095097 (- n 1)))))
         (cond
           ((= 0 (modulo (A003714 i) 8)) i) ;; z.e. ends as ...000 ?
           (else (loop (+ 1 i))) ;; Try next one then.
         )
      )
    )
  )
)


(definec (A095098 n) ;; fib001 numbers.
  (cond
    ((= 0 n) 1) ;; 1 is the first member, it's z.e. being 1.
    (else
      (let loop ((i (+ 1 (A095098 (- n 1)))))
         (cond
           ((= 1 (modulo (A003714 i) 8)) i) ;; z.e. ends as ...001 ?
           (else (loop (+ 1 i))) ;; Try next one then.
         )
      )
    )
  )
)


(definec (A035336 n) ;; fib010 numbers, second column of Wythoff array.
  (cond
    ((= 0 n) 2) ;; 2 is the first member, it's z.e. being 10.
    (else
      (let loop ((i (+ 1 (A035336 (- n 1)))))
         (cond
           ((= 2 (modulo (A003714 i) 8)) i) ;; z.e. ends as ...010 ?
           (else (loop (+ 1 i))) ;; Try next one then.
         )
      )
    )
  )
)


(definec (A035337 n) ;; fib100 numbers, third column of Wythoff array.
  (cond
    ((= 0 n) 3) ;; 3 is the first member. it's z.e. being 100.
    (else
      (let loop ((i (+ 1 (A035337 (- n 1)))))
         (cond
           ((= 4 (modulo (A003714 i) 8)) i) ;; z.e. ends as ...100 ?
           (else (loop (+ 1 i))) ;; Try next one then.
         )
      )
    )
  )
)

(definec (A095099 n) ;; fib101 numbers.
  (cond
    ((= 0 n) 4) ;; 4 is the first member. it's z.e. being 101.
    (else
      (let loop ((i (+ 1 (A095099 (- n 1)))))
         (cond
           ((= 5 (modulo (A003714 i) 8)) i) ;; z.e. ends as ...101 ?
           (else (loop (+ 1 i))) ;; Try next one then.
         )
      )
    )
  )
)


(define (A029837 n)
  (cond ((< n 1) (error "A029837 supplied with argument less than one: " n))
        (else (binwidth (-1+ n)))
  )
)

(definec (A030101 nn) ;; Was: binrev
  (let loop ((z 0) (n nn))
    (if (zero? n)
        z
        (loop (+ (* 2 z) (modulo n 2))
              (floor->exact (/ n 2)) ;; Doesn't work when n is big enough: (fix:lsh n -1) ;; n >>= 1
        )
    )
  )
)

(definec (A035327 n) (- (-1+ (expt 2 (binwidth n))) n)) ;; Complement the binary exp.

(definec (A036044 n) ;; i.e. binrevcompl, differs from EIS-seq as a(0)=0, not 1.
  (let loop ((z 0) (n n))
    (if (zero? n)
        z
        (loop (+ (* 2 z) (- 1 (modulo n 2)))
              (floor->exact (/ n 2)) ;; (fix:lsh n -1) ;; n >>= 1
        )
    )
  )
)

;; Odd numbers whose square's binary reversal is also a square:
(define A229766 (MATCHING-POS 1 1 (lambda (n) (and (odd? n) (zero? (A068527 (A030101 (* n n))))))))
(define (A229687 n) (A000290 (A229766 n)))


(define (A163380 n)
    (let* ((w (A000523 n))
           (msb (A000079 w))
          )
       (let loop ((max-n n) (n n) (tries w))
          (cond ((zero? tries) max-n)
                (else
                  (let ((new-n (+ (* (modulo n 2) msb) (floor->exact (/ n 2)))))
                     (loop (max max-n new-n) new-n (- tries 1))
                  )
                )
          )
       )
    )
)


(define (isA065609? org-n)
    (let* ((w (A000523 org-n))
           (msb (A000079 w))
          )
       (let loop ((n org-n) (tries w))
          (cond ((zero? tries) #t)
                (else
                  (let ((new-n (+ (* (modulo n 2) msb) (floor->exact (/ n 2)))))
                     (if (> new-n org-n) #f (loop new-n (- tries 1)))
                  )
                )
          )
       )
    )
)


(define A065609 (MATCHING-POS 1 1 isA065609?))

(define A065609v2 (FIXED-POINTS 1 1 A163380))

;; Note that 
;;  (fetch-from-bits-of-n-given-by-column-x-of-table  
;;    (store-n-to-bits-given-by-column-x-of-table n col some-NxN->N-map)
;;      col  some-NxN->N-map)
;; is an identity function on n, with all values of col >= 0.

;; Corresponds to *A057163
;; Fails at: (A054429 281474976710655), giving: 562949953421312
;; Note that A007088(281474976710655) = 111111111111111111111111111111111111111111111111

(define (A054429old n) ;; -> (0),1,3,2,7,6,5,4,15,14,13,12,11,10,9,8,31,30,...
  (if (zero? n)
      n
      (-1+ (- (* 3 (expt 2 (A000523 n))) n))
  )
)

;; This works:
;; (A054429 281474976710655) = 140737488355328
;; (A054429 (A054429 281474976710655)) = 281474976710655
;;

(define (A054429 n)
  (if (< n 2)
      n
      (let ((size (binwidth n)))
        (bit-string->unsigned-integer
           (bit-string-xor
              (bit-string-not
                 (bit-string-append (unsigned-integer->bit-string (- size 1) 0) (unsigned-integer->bit-string 1 1))
              )
              (unsigned-integer->bit-string size n)
           )
        )
      )
  )
)


;; Corresponds to *A069770
;; A063946 Write n in binary and complement second bit (from the left), with a(0)=0 and a(1)=1.
;; if(n<2, n>0, 3/2*2^floor(log(n)/log(2))-2^floor(log(4/3*n)/log(2))+n) (from R. Stephan)
(define (A063946 n) ;; -> (0),1,3,2,6,7,4,5,12,13,14,15,8,9,10,11,24,25,26,...
   (if (< n 2) n
       (let ((sm (A072376 n)))
          (+ n (* sm (- 1 (* 2 (modulo (floor->exact (/ n sm)) 2)))))
       )
   )
)

(define (A059893 n)
  (if (zero? n)
      n
      (let loop ((n n) (s 1))
        (cond ((= 1 n) s)
              (else (loop (/ (- n (A000035 n)) 2) (+ s s (A000035 n))))
        )
      )
  )
)


(definec (A059893rec n)
  (if (<= n 1)
      n
      (let* ((k (- (A000523 n) 1))
             (r (A059893rec (- n (A000079 k)))))
         (if (= 2 (floor->exact (/ n (A000079 k))))
             (* 2 r)
             (+ 1 r)
         )
      )
  )
)

(define A059894 (compose-funs A054429 A059893))

;; A065190 = A059893 o A063946 o A059893
(define (A065190 n) (if (< n 2) n (+ n (expt -1 n))))

(define (A056539 n) (if (odd? n) (A030101 n) (A036044 n)))

;; There exists similar rotate & deep rotate variants of this same idea.
(define (A056539v2 n) (runcount1list->binexp (reverse! (binexp->runcount1list n))))

;; How to prove that this is an involution?
;; By (strong) induction, or by simpler means?
;; (E.g. if a Life pattern is symmetric, it just cannot
;;  but stay symmetric ever after.)

(definec (A105726 n)
  (cond ((< n 2) n)
        (else (runcount1list->binexp (map A105726 (reverse! (binexp->runcount1list n)))))
  )
)

(definec (A001477yet_another_variant n)
  (cond ((< n 2) n)
        (else (runcount1list->binexp (map A001477yet_another_variant (binexp->runcount1list n))))
  )
)


;; This one by Leroy Quet, Oct 08 2009:
;; a(9)=27, because (binexp->runcount1list  9)=(1 2 1), so replacing
;; run of 1 ones with run of 2 ones, run of 2 0's with run of 1 0's
;; and finally run of 1 ones with run of 2 ones, thus we get 11011 = 27.
;; a(11)=25 because (binexp->runcount1list 11)=(1 1 2) (1011), so
;; replacing run of 1 ones with run of 2 ones, run of 1 0's with run
;; of 2 zeros, and run of 2 ones with run of 1 ones, we get 11001 = 25.
(definec (A166166 n)
  (let* ((runlens (binexp->runcount1list n))
         (rlsorted (uniq (sort runlens <)))
         (lenrls (length rlsorted))
        )
     (let loop ((rl runlens) (s 0) (b 1))
           (cond ((null? rl) s)
                 (else
                   (let* ((nrl (list-ref rlsorted
                                      (- lenrls 1 (nthmemq (car rl) rlsorted))
                               )
                          )
                          (p2 (expt 2 nrl))
                         )
                       (loop (cdr rl) (+ (* s p2) (* b (-1+ p2))) (- 1 b))
                   )
                 )
           )
     )
  )
)



(define (interleave a b)
  (let loop ((z (list)) (a a) (b b))
        (cond ((and (null? a) (null? b)) (reverse! z))
              ((null? a) (loop (cons (car b) z) a (cdr b)))
              ((null? b) (loop (cons (car a) z) (cdr a) b))
              (else (loop (cons (car b) (cons (car a) z)) (cdr a) (cdr b)))
        )
  )
)

;; Cf. A056539
(definec (A166404 n) ;; begins as 0,1,2,3,6,5,4,7,14,13,10,11,12,9,8,15,30,29,26,27,22,21,20,23,28,25,...
  (let ((runlens (binexp->runcount1list n)))
     (runcount1list->binexp (interleave (bisect runlens 1) (bisect runlens 0)))
  )
)

;; Not an involution like the previous one:
(define (Ajoku2 n) ;; begins as 1,2,3,4,5,6,7,8,11,10,9,12,13,14,15,16,23,22,19,20,21
  (let ((runlens (binexp->runcount1list n)))
     (runcount1list->binexp (append (bisect runlens 0) (bisect runlens 1)))
  )
)


;; Also by Leroy Quet. I want to compute the b-file.
;; a(0)=0, a(1)=1, a(2)=2. For n>=3, a(n) = a(n-1) - min(a(n-2),a(n-3)).
(definec (A163495 n)
  (if (< n 3) n
      (- (A163495 (-1+ n)) (min (A163495 (- n 2)) (A163495 (- n 3))))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(definec (on-bit-indices n)
   (let loop ((n n)
              (i 0)
              (c (list))
             )
      (cond ((zero? n) (reverse! c))
            ((odd? n) (loop (/ (- n 1) 2) (+ 1 i) (cons i c)))
            (else     (loop (/ n 2) (+ 1 i) c))
      )
   )
)

(define (halve n) (/ n 2))
(define (shr n) (if (odd? n) (/ (- n 1) 2) (/ n 2)))

(definec (left-options n)  (map halve (keep-matching-items (on-bit-indices n) even?)))
(definec (right-options n) (map halve (map -1+ (keep-matching-items (on-bit-indices n) odd?))))


;; . = 0

;; \ = 1

;; / = 2

;; \/ = 3

;; \
;; /  = 8

;; /
;; \ = 16


;; \
;;  \ = 4

;;  /
;; /  = 32

;;                                 \
;;   \      \ \        \/         \/
;;  \/ = 9   \/ = 12  \/ = 129   \/ = 524289 = 2^(2*9+1)+1


(definec (A057300 n)
  (reduce + 0 (map (lambda (i) (expt 2 i)) (map A004442 (on-bit-indices n))))
)


(definec (A126006 n) ;; Swap the positions of quaternary digits q0 <-> q1, q2 <-> q3, q4 <-> q5, ...
  (let loop ((n n) (s 0) (p 1))
     (cond ((zero? n) s)
           (else (loop (floor->exact (/ n 16))
                       (+ s (* p (+ (* 4 (modulo n 4)) (modulo (floor->exact (/ n 4)) 4))))
                       (* p 16)
                 )
           )
     )
  )
)

(define (A126007 n) ;; Swap the positions of quaternary digits q0, q1 <-> q2, q3 <-> q4, ...
  (+ (modulo n 4) (* 4 (A126006 (floor->exact (/ n 4)))))
)

(define A126008 (compose-funs A057300 A126007)) ;; The first 64 terms are same as in A106485.
(define A126008v2 (compose-funs A126007 A057300))

;; Return the number of leading 1-bits in the binary expansion of n:
;; The current version (< 2007) in OEIS has been shifted once left!
(define (A090996 n) (A007814 (+ 1 (A030101 n)))) ;; Zero-based, 0,1,1,2,1,1,2,3,1,1,1,1,2,2,3,4,1,...

(definec (A106485 n) ;; negate, i.e. take the mirror-image of a CGT-tree.
   (let loop ((n n)
              (i 0)
              (s 0)
             )
      (cond ((zero? n) s)
            ((odd? n)
                (loop (/ (- n 1) 2)
                      (+ 1 i)
                      (+ s
                         (if (even? i)
                             (expt 2 (+ 1 (* 2 (A106485 (/ i 2)))))
                             (expt 2 (* 2 (A106485 (/ (- i 1) 2))))
                         )
                      )
                )
            )
            (else (loop (/ n 2) (+ 1 i) s))
      )
   )
)


;; 85159 --- 85228

;; Note that: A071156 = A085198 o A014486
(define (A085198 n)
  (let loop ((n n) (s 0) (h 1) (i 2) (fi 1))
     (cond ((zero? n) s)
           ((odd? n) (loop (/ (-1+ n) 2) s (-1+ h) i fi))
           (else (loop (/ n 2) (+ s (* h fi)) (1+ h) (1+ i) (* fi i)))
     )
  )
)

;; Note that: A126302 = A125989 o A014486
(define (A125989 n)
  (let loop ((n n) (s 0) (h 0))
     (cond ((zero? n) s)
           ((= 2 (modulo n 4)) (loop (/ (- n 2) 4) (+ s h 1) h))
           ((odd? n) (loop (/ (- n 1) 2) s (- h 1)))
           (else (loop (/ n 2) s (+ 1 h)))
     )
  )
)


;; binexp->A071158-list o A014486 produces the terms of A071158 in list form:
;; (() (1) (1 1) (2 1) (1 1 1) (1 2 1) ...)
(define (binexp->A071158-list n)
  (let loop ((n n) (lista (list)) (h 1))
     (cond ((zero? n) lista)
           ((odd? n) (loop (/ (- n 1) 2) lista (- h 1)))
           (else (loop (/ n 2) (cons h lista) (1+ h)))
     )
  )
)

;;
;; Algorithm for Anewone1: divide the run-lengths of n into
;; two separate lists, those that correspond with runs of ones
;; (starting with the first run from the msb-end of n)
;; and that correspond with runs of zeros in the binary expansion
;; of n.
;; A) Beginning from zero-runs, add 1's to the result (from msb to lsb-end)
;; as long as there are 1's in the zero-runs-list (making it shorter all the time).
;;
;; When there's a first run larger than one in the zero-list,
;; add also one 1 to the result, subtract than run by one,
;; and switch the runlists for zeros and ones, make b=0 (start adding 0's
;; to result), and go back to A.
;;
;; If the run-list of zeros is finished completely
;; (after taking the last 1 from it and adding it to the 2*sum)
;; then switch also in that case to the list of ones.
;;
;; When the run-list of ones is also finished
;; (after taking the last 1 from it and adding zero to the 2*sum)
;; then the result is ready. This is the case for n whose
;; binary expansion is balanced.
;;
;; Note: there are n for which there is only runs of ones (no zeros),
;; e.g. 3, 7, 15.
;; and there are n for which there is one list more of ones than
;; zeros (e.g. 5).
;;
;;
;;

(define (Ajoku_not_submitted n)
  (let ((runlens (binexp->runcount1list n)))
    (let loop ((chosen  (bisect runlens 1)) ;; initially zeros.
               (others  (bisect runlens 0)) ;; initially ones.
               (s 0)
               (b 1)
              )
       (format #t "chosen=~a, others=~a, b=~a, s=~a\n" chosen others b s)
       (cond ((and (null? chosen) (null? others)) s)
             ((and (pair? chosen) (= 1 (car chosen)) (pair? (cdr chosen)))
                 (loop (cdr chosen) others (+ s s b) b)
             )
             (else ;; next run in result, or chosen will be finished, swap chosen and others.
               (loop others (if (or (null? chosen) (= 1 (car chosen)))
                                '()
                                (cons (- (car chosen) 1) (cdr chosen))
                            )
                     (+ s s b) (- 1 b)
               )
             )
       )
    )
  )
)

;; Like previous, but start from the lsb-end.

;; Here are the 40 A-numbers you requested: A125974 --- A126013.

(define (A125974v2 n)
  (let ((runlens (binexp->runcount1list n)))
    (let loop ((chosen (reverse! (bisect runlens 0))) ;; initially ones.
               (others (reverse! (bisect runlens 1))) ;; initially zeros.
               (s 0)
               (b (modulo n 2))
               (p 1)
              )
;;     (format #t "chosen=~a, others=~a, b=~a, s=~a, p=~a\n" chosen others b s p)
       (cond ((and (null? chosen) (null? others)) s)
             ((and (pair? chosen) (= 1 (car chosen)) (pair? (cdr chosen)))
                 (loop (cdr chosen) others (+ s (* b p)) b (+ p p))
             )
             (else ;; next run in result, or chosen will be finished, swap chosen and others.
               (loop others (if (or (null? chosen) (= 1 (car chosen)))
                                '()
                                (cons (- (car chosen) 1) (cdr chosen))
                            )
                     (+ s (* b p)) (- 1 b) (+ p p)
               )
             )
       )
    )
  )
)

;; Here 1(0/1)* refers to the msb-prefix of the n:
;;
;; 0                    Chosen=0 (but others is not 0), swap to others.
;; 1                    Chosen=1, last one will follow.
;; (1+)0                Chosen=0, last zero will follow. One of A000918 (2^n - 2) (pow2? n+2)
;; 1(0/1)*11            Chosen=1, and one or more ones will follow.  (= 3 (modulo chosen 4))
;; 1(0/1)*00            Chosen=0, and one or more zeros will follow. (= 0 (modulo chosen 4))
;;
;; Checked after the above cases:
;; 1(0/1)*(0+)1         Chosen=1, and one or more zeros follow, after which follow more ones.
;;                                 (= 1 (modulo chosen 4))
;; 1(0/1)*(0+)(1+)0     Chosen=0, and one or more ones follow, after which follow more zeros.
;;                                 (= 2 (modulo chosen 4))


(define (A125974 n)
 (if (zero? n) n
  (let loop ((chosen (/ n (A006519 n)))                      ;; Initially ones, get rid of lsb-0's.
             (others (floor->exact (/ n (A006519 (+ 1 n))))) ;; Initially zeros, get rid of lsb-1's.
             (s 0)
             (b (modulo n 2))
             (p 1)
            )
;;   (format #t "chosen=~a, others=~a, b=~a, s=~a, p=~a\n" chosen others b s p)
     (cond ((and (zero? chosen) (zero? others)) s)
           ((or (= 1 chosen) (pow2? (+ chosen 2))) ;; Last one or zero at hand.
             (loop others 0 (+ s (* b p)) (- 1 b) (+ p p))
           )
           ((or (= 0 (modulo chosen 4)) (= 3 (modulo chosen 4))) ;; source run continues, dest changes.
             (loop others (floor->exact (/ chosen 2)) (+ s (* b p)) (- 1 b) (+ p p))
           )
           ((= 1 (modulo chosen 4)) ;; source run changes, from ones to zeros, skip past zeros.
             (loop (floor->exact (/ chosen (A006519 (- chosen 1))))
                   others (+ s (* b p)) b (+ p p)
             )
           )
           (else ;; (= 2 (modulo chosen 4)) ;; source run changes, from zeros to ones, skip past ones.
             (loop (floor->exact (/ chosen (A006519 (+ chosen 2))))
                   others (+ s (* b p)) b (+ p p)
             )
           )
     )
  )
 )
)

;; (define A125975 (fun-succ-matching-is0 (lambda (i) (= i (A125974 (A125974 i))))))
(define A125975 (MATCHING-POS 0 0 (lambda (i) (= i (A125974 (A125974 i))))))

;; A154103 --- A154104.

;; Gives 1, if y should come before x, 0 otherwise:
(define (A154103bi x y)
   (cond ((and (zero? y) (not (zero? x))) 1)
         ((> (A085207bi (* 2 x) y) (A085207bi (* 2 y) x)) 1)
         (else 0)
   )
)

(define (A154103 n) (A154103bi (A002262 n) (A025581 n)))
(define (A154104 n) (A154103bi (A025581 n) (A002262 n)))

;; Was: (define (A054582bi col row) (* (A000079 col) (+ 1 row row))) ;; Table T(m,k) = 2^m * (2k+1) with m,k >= 0.
;; (define (A054582 n) (A054582bi (A025581 n) (A002262 n))) ;; offset=0.
(define (A054582bi row col) (* (A000079 col) (+ 1 row row))) ;; Table T(m,k) = 2^m * (2k+1) with m,k >= 0.
(define (A054582 n) (A054582bi (A002262 n) (A025581 n))) ;; offset=0.


;; Inverse of above, but with offset=1:
(define (A209268 n) (* (/ 1 2) (+ (expt (+ (A003602 n) (A007814 n)) 2) (- (A007814 n)) (A003602 n)))) ;; offset 1.

(define (A249725 n) (+ 1 (* (/ 1 2) (+ (expt (+ (A003602 n) (A007814 n)) 2) (- (A003602 n)) (A007814 n)))))

;; (same-intfuns1? A001477 (COMPOSE A249725 A135764) 65537) --> #t
;; (same-intfuns1? A001477 (COMPOSE A135764 A249725) 1024) --> #t

;; (same-intfuns0? A001477 (COMPOSE -1+ A209268 A054582) 65537) -->  #t
;; (same-intfuns1? A001477 (COMPOSE A054582 -1+ A209268) 65537) -->  #t

(define (A249745 n) (/ (+ 1 (A064989 (A007310 n))) 2))

;; (same-intfuns1? (COMPOSE A064216 A249745) (COMPOSE A064989 A064989 A007310) 1024) --> #t
;; (same-intfuns1? A249823 (COMPOSE A064216 A249745) 1024) --> #t
;; (same-intfuns1? A249745 (COMPOSE A048673 A249823) 2048) --> #t

(define (A249746 n)
  (define (Ainv_of_A007310off0 n) (+ (* 2 (floor->exact (/ n 6))) (/ (- (modulo n 6) 1) 4)))
  (+ 1 (Ainv_of_A007310off0 (A003961 (+ n n -1))))
)


;; (same-intfuns1? A007310 (COMPOSE A249735 A249745) 1024)

(define (A135764 n) (A135764bi (A002260 n) (A004736 n)))
(define (A135764bi row col) (* (A000079 (- row 1)) (+ -1 col col)))

(define (A135764biold row col) (A054582bi (- col 1) (- row 1)))
(define (A135764off1 n) (A054582biold (A025581 (-1+ n)) (A002262 (-1+ n))))
(define (A135764old n) (A135764biold (A002260 n) (A004736 n)))
(define A135764v2 A135764off1)

(define (A135765 n) (A135765bi (A002260 n) (A004736 n)))
(define (A135765bi row col) (* (A000244 (- row 1)) (A007310 col)))
(define (A135765v2 n) (+ -1 (* 2 (A254051 n))))

;; Binary concatenation: (0 understood as an empty string).
(define (A085207bi x y) (+ (* (expt 2 (A029837 (1+ y))) x) y))
(define (A085207 n) (A085207bi (A025581 n) (A002262 n)))
(define (A085208 n) (A085207bi (A002262 n) (A025581 n)))
(define (A085209 n) (A007088 (A085207 n)))
(define (A085210 n) (A007088 (A085208 n)))
;; Strange binary concatenation:
(define (A085211bi x y)
   (cond ((zero? x) (A085207bi x y))
         (else (* (+ (* (expt 2 (A029837 (1+ y))) (A000265 x)) y) (A006519 x)))
   )
)

(define (A085211 n) (A085211bi (A025581 n) (A002262 n)))
(define (A085212 n) (A085211bi (A002262 n) (A025581 n)))
(define (A085213 n) (A007088 (A085211 n)))
(define (A085214 n) (A007088 (A085212 n)))

;; Factorial expansion concatenation: (0 understood as an empty string).
;; ! is cached, so it's not so foolish as it looks like:
(define (A085215bi x y)
  (let loop ((x x) (y y) (i 2) (j (1+ (A084558 y))))
    (cond ((zero? x) y)
          (else (loop (floor->exact (/ x i)) (+ (* (! j) (modulo x i)) y) (1+ i) (1+ j)))
    )
  )
)

(define (A085215 n) (A085215bi (A025581 n) (A002262 n)))
(define (A085216 n) (A085215bi (A002262 n) (A025581 n)))
(define (A085217 n) (A007623 (A085216 n)))
(define (A085218 n) (A007623 (A085217 n)))

;; Rised factorial expansion concatenation: (0 understood as an empty string).
;; To the each digit of x we add 'r', the first digit of y.
;; (to get a more or less continuous "staircase").
(define (A085219bi x y)
  (let loop ((x x) (y y) (i 2) (j (1+ (A084558 y))) (r (car (n->factbase y))))
    (cond ((zero? x) y)
          (else (loop (floor->exact (/ x i))
                      (+ (* (! j) (+ r (modulo x i))) y)
                      (1+ i)
                      (1+ j)
                      r
                )
          )
    )
  )
)

(define (A085219 n) (A085219bi (A025581 n) (A002262 n)))
(define (A085220 n) (A085219bi (A002262 n) (A025581 n)))
(define (A085221 n) (A007623 (A085219 n)))
(define (A085222 n) (A007623 (A085220 n)))


;; I bet this is the correct formula:
;; Maple: A002542 := n -> 2^((2^n)-1)*((2^((2^n)-1))-1);
(define (A002542 n)
  (* (expt 2 (-1+ (expt 2 n)))
     (-1+ (expt 2 (-1+ (expt 2 n))))
  )
)


;; ID Number: A002542 (Formerly M2174 and N0869)
;; Sequence:  0,2,56,16256,1073709056,4611686016279904256,
;;            85070591730234615856620279821087277056
;; Name:      Complete Post functions of n variables.
;; References Wheeler, Roger F.; Complete connectives for the $3$-valued
;;               propositional calculus. Proc. London Math. Soc. (3) 16 1966 167-191.
;; See also:  Cf. A002543.
;; Keywords:  nonn
;; Offset:    1
;; Author(s): njas

(definec (A080303 n) ;; rewrite 1->1, 0->100 in the binary expansion.
   (cond ((zero? n) 4)
         ((= n 1) n)
         ((odd? n) (1+ (* 2 (A080303 (/ (-1+ n) 2)))))
         (else (+ 4 (* 8 (A080303 (/ n 2)))))
   )
)

(define (A080310 n) (/ (A080303 (* 2 n)) 2))


(definec (A126308 n) ;; rewrite 10->'' in the binary expansion. Cf. A080303.
   (cond ((zero? n) 0)
         ((= 2 (modulo n 4)) (A126308 (/ (- n 2) 4)))
         (else (+ (modulo n 2) (* 2 (A126308 (floor->exact (/ n 2))))))
   )
)



(define (A079946 n) (* 2 (+ (A000079 (1+ (A000523 n))) n)))

;; A079446 := n -> 2*(2^(1+A000523(n))+n);
;; A079446v2 := n -> `if`(0=n,2,2^A000523(4*n)+2*n);
(define (A079946v2 n) (if (zero? n) 2 (+ (A000079 (A000523 (* 4 n))) (* 2 n))))


(define (bit_i n i) (modulo (floor->exact (/ n (expt 2 i))) 2))

(define (A110591 n) (if (zero? n) 1 (/ (+ (A000523 n) (- 2 (modulo (A000523 n) 2))) 2)))

;; (define (A048703auxiliary n) (+ (A000523 n) (- 1 (modulo (A000523 n) 2))))

(define (A030103 n)
   (if (zero? n)
       n
       (let ((uplim (+ (A000523 n) (- 1 (modulo (A000523 n) 2)))))
          (add (lambda (i)
                  (* (bit_i n (+ i (expt -1 i)))
                     (expt 2 (- uplim i))
                  )
               )
               0
               uplim
          )
       )
   )
)

(define (A048703 n)
   (if (zero? n)
       n
       (let ((uplim (+ (A000523 n) (- 1 (modulo (A000523 n) 2)))))
         (+ (* (expt 2 (+ 1 uplim)) n)
            (add (lambda (i)
                    (* (bit_i n (+ i (expt -1 i)))
                       (expt 2 (- uplim i))
                    )
                 )
                 0
                 uplim
            )
         )
       )
   )
)

(define (A048703v2 n) (if (zero? n) n (+ (* (A000302 (A110591 n)) n) (A030103 n))))

(define (A048703v3 n) (+ (* (expt 2 (+ (A000523 n) (- 2 (modulo (A000523 n) 2)))) n) (A030103 n)))



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

;; (Show ternary form in decimal)
(define (A007089 n) ;; 0,1,2,10,11,12,20,21,22,100,101,102,...
  (let loop ((z 0) (i 0) (n n))
    (if (zero? n)
        z
        (loop (+ z (* (expt 10 i) (modulo n 3)))
              (1+ i)
              (floor->exact (/ n 3))
        )
    )
  )
)


;; Tersum n + n:
(definec (A004488 n) ;; 0,2,1,6,8,7,3,5,4,18,20,19,24,26,25,21,23
  (let loop ((z 0) (i 0) (n n))
    (if (zero? n)
        z
        (loop (+ z (* (expt 3 i) (modulo (- (modulo n 3)) 3)))
              (1+ i)
              (floor->exact (/ n 3))
        )
    )
  )
)


(define (in_A051382? n)
  (let loop ((n n) (seen02yet? #f))
    (cond ((zero? n) #t) ;; or #f ?
          ((= 1 n) #t)
          ((modulo n 3)
            =>
             (lambda (r)
                (cond ((= r 2)
                         (if (or seen02yet? (not (zero? (modulo (/ (- n r) 3) 3))))
                             #f ;; Either 12 or 22 or we encounter a second 02
                             (loop (/ (- n r) 3) #t)
                         )
                      )
                      (else (loop (/ (- n r) 3) seen02yet?))
                )
             )
          )
    )
  )
)

(define A051382 (MATCHING-POS 0 0 in_A051382?))

(define A249719 (COMPLEMENT 1 A051382))

(define (A249720 n) (* 2 (A249719 n)))


;;  Numbers whose base 3 representation consists entirely of 0's and 2's, except possibly for a single pair of adjacent 1's among them. 
(define (in_A249721? n)
  (let loop ((n n) (seen11yet? #f))
    (cond ((zero? n) #t)
          ((= 2 n) #t)
          ((modulo n 3)
            =>
             (lambda (r)
               (let ((next_n (/ (- n r) 3))) ;; Shift one digit off.
                (cond ((= r 1)
                         (if (or seen11yet? (not (= 1 (modulo next_n 3))))
                             #f ;; Either 01 or 21 or we encounter a second 11
                             (loop (/ (- next_n 1) 3) #t)
                         )
                      )
                      (else (loop next_n seen11yet?))
                )
               )
             )
          )
    )
  )
)

(define A249721 (MATCHING-POS 0 0 in_A249721?))

(define (A249721v2 n) (* 2 (A051382 n)))


(define (A007090 n) ;; 0,1,2,3,10,10,11,12,13,20,... (Show quaternary form in decimal)
  (let loop ((z 0) (i 0) (n n))
    (if (zero? n)
        z
        (loop (+ z (* (expt 10 i) (modulo n 4)))
              (1+ i)
              (floor->exact (/ n 4))
        )
    )
  )
)

(define (A007094 n) ;; For octal.
  (let loop ((z 0) (i 0) (n n))
    (if (zero? n)
        z
        (loop (+ z (* (expt 10 i) (modulo n 8)))
              (1+ i)
              (floor->exact (/ n 8))
        )
    )
  )
)


(define (A235049 n) ;; Decimal, with one subtracted from each nonzero digit.
  (let loop ((z 0) (i 0) (n n))
    (if (zero? n)
        z
        (let ((d (modulo n 10)))
          (loop (+ z (* (expt 10 i) (if (zero? d) d (- d 1))))
                (1+ i)
                (floor->exact (/ n 10))
          )
        )
    )
  )
)


(definec (A235049v2 n) ;; As a recurrence.
  (if (zero? n) n
      (let ((d (modulo n 10)))
        (+ (* 10 (A235049v2 (floor->exact (/ n 10))))
           (if (zero? d) d (- d 1))
        )
      )
  )
)


(definec (A028897 n) ;; If n = Sum c_i 10^i then a(n) = Sum c_i 2^i.
  (let loop ((z 0) (i 0) (n n))
    (if (zero? n)
        z
        (loop (+ z (* (modulo n 10) (A000079 i)))
              (1+ i)
              (floor->exact (/ n 10))
        )
    )
  )
)

(define (A081594 n) (+ (* 2 (floor->exact (/ n 10))) (modulo n 10)))

(define (A055642 n) (if (< n 10) 1 (+ 1 (A055642 (floor->exact (/ n 10))))))


;; Convert from "Catalan Base" (various) represented as a decimal number, back to n it represents.
(define (A244158 n) ;; If n = Sum c_i * 10^i then a(n) = Sum c_i * Cat(i+1), where Cat(k) = A000108(k).
  (let loop ((z 0) (i 1) (n n))
    (if (zero? n)
        z
        (loop (+ z (* (modulo n 10) (A000108 i)))
              (1+ i)
              (floor->exact (/ n 10))
        )
    )
  )
)

(definec (A197433 n)
  (let loop ((z 0) (i 1) (n n))
    (if (zero? n)
        z
        (loop (+ z (* (modulo n 2) (A000108 i)))
              (1+ i)
              (floor->exact (/ n 2))
        )
    )
  )
)

(define A244230 (LEAST-GTE-I 0 0 A197433))

(define (A176137 n) (if (zero? n) 1 (- (A244230 (+ n 1)) (A244230 n))))

(definec (A244159 n)
  (if (not (zero? (A176137 n)))
      (A007088 (A244230 n))
      (+ (A007088 (-1+ (A244230 n)))
         (A244159 (- n (A197433 (-1+ (A244230 n)))))
      )
  )
)

(definec (A244232 n)
  (if (not (zero? (A176137 n)))
      (A000120 (A244230 n))
      (+ (A000120 (-1+ (A244230 n)))
         (A244232 (- n (A197433 (-1+ (A244230 n)))))
      )
  )
)


(define (A244234 n) (- n (A244232 n)))

(definec (A244315 n)
  (cond ((zero? n) n)
        ((not (zero? (A176137 n))) (A007814 (A244230 n)))
        (else (A244315 (- n (A197433 (-1+ (A244230 n))))))
  )
)

(definec (A244316 n)
  (cond ((zero? n) n)
        ((not (zero? (A176137 n))) (A001511 (A244230 n)))
        (else (A244316 (- n (A197433 (-1+ (A244230 n))))))
  )
)

(definec (A244317 n)
  (if (zero? n)
      n
      (let loop ((k 0))
            (if (>= (A014143 k) n)
                (+ 1 k)
                (loop (+ 1 k))
            )
      )
  )
)


(define (A244314 n)
  (cond ((zero? n) 0)
        ((= 1 n) 2)
        (else (+ -1 n (- (A000108 (+ 1 (A244317 n))) (A014143 (- (A244317 n) 2)))))
  )
)



;; n = A197433(v) + r
;; With which 1-bits b_i of v, A244231(r + C(b_i * 2^i)) > A244231(r) ?
;; (Precisely, the difference can be at most one, add those 1-bits to new result).
;;
;; For n=10, for which A244159(n)=121 (19 = 5 + 2*2 + 1)
;; v = 7 (111 in binary), and r = 2 (10 in A244159, so we have 111 + 10 = 121)
;;
;;
;; For n=12, for which A244159(n)=123 (12 = 5 + 2*2 + 3*1),
;; v = 7 (111 in binary), and r = 4 (12 in A244159, so we have 111 + 012 = 123,
;; and A_something_else_ should be 1.

;; For n=18, A244159(n) = 1012
;; v = 11 (1011 in binary), and r = 1
;; and A_something_else_ should be 1.

(definec (A_something_else_not_this_easy_at_all n)
  (if (not (zero? (A176137 n)))
      (A244230 n)
      (let* ((v (-1+ (A244230 n)))
             (r (- n (A197433 v)))
             (prevmax (A244231 r))
            )
         (let loop ((v v) (z 0))
           (cond ((zero? v) z)
                 (else
                   (loop (- v (A006519 v))
                         (+ z (* (- (A244231 (+ r (A000108 (A001511 v)))) prevmax)
                                 (A006519 v)
                              )
                         )
                   )
                 )
           )
         )
      )
  )
)


;; E.g. (A000695 n) = (expand-n-x-fold n 2) 
(define (expand-n-x-fold n x) ;; Expand bits of n, by scale 2^x.
  (if (zero? n)
      n
      (+ (modulo n 2)
         (* (expt 2 x) (expand-n-x-fold (floor->exact (/ n 2)) x))
      )
  )
)


(define (A000302 n) (expt 4 n))
(define (A002001 n) (if (zero? n) 1 (* 3 (A000302 (-1+ n)))))
;; A002001 = 1,3,12,48,192,768,3072,12288,49152,196608,786432,3145728,
;; a(n) = 3*4^(n-1), n>0; a(0)=1.


(define (A080116 c) ;; Characteristic function of A014486
  (let loop ((c c) (lev 0))
    (cond ((zero? c) (if (zero? lev) 1 0))
          ((< lev 0) 0)
          (else
              (loop (floor->exact (/ c 2)) (+ lev (- 1 (* 2 (modulo c 2)))))
          )
    )
  )
)


(define (store-n-to-bits-given-by-column-x-of-table n x NxN->N)
  (let loop ((n n) (i 0) (s 0))
     (if (zero? n)
         s
         (loop (floor->exact (/ n 2))
               (1+ i)
               (+ s (* (modulo n 2) (expt 2 (NxN->N x i))))
         )
     )
  )
)


;; The checking of upper-limit is very naive, and to work it requires
;; that all columns of the map NxN->N are monotone (growing).
(define (fetch-from-bits-of-n-given-by-column-x-of-table n x NxN->N)
  (let ((upper-limit (A000523 n)))
   (let loop ((i 0) (s 0) (bitpos (NxN->N x 0)))
     (if (> bitpos upper-limit)
         s
         (loop (1+ i)
               (+ s (* (bit-i n bitpos) (expt 2 i)))
               (NxN->N x (1+ i))
         )
     )
   )
  )
)



;; ;; Moser-de Bruijn sequence: 0,1,4,5,16,17,20,21,64,65,68,69,80,...
;; (definec (A000695 n) ;; Expand bits, 0->00, 1->01, i.e. from base-2 to base-4.
;;   (if (zero? n)
;;       n
;;       (+ (modulo n 2) (fix:lsh (A000695 (fix:lsh n -1)) 2))
;;   )
;; )
;; 
;; (definec (A059905 n) ;; Take the even-positioned bits and contract them.
;;   (if (zero? n)
;;       n
;;       (+ (modulo n 2) (fix:lsh (A059905 (fix:lsh n -2)) 1))
;;   )
;; )
;; 

;; Moser-de Bruijn sequence: 0,1,4,5,16,17,20,21,64,65,68,69,80,...
(define (A000695 n) ;; Expand bits, 0->00, 1->01, i.e. from base-2 to base-4.
  (if (zero? n)
      n
      (+ (modulo n 2) (* 4 (A000695 (floor->exact (/ n 2)))))
  )
)

(define (A059905 n) ;; Take the even-positioned bits and contract them.
  (if (zero? n)
      n
      (+ (modulo n 2) (* 2 (A059905 (floor->exact (/ n 4)))))
  )
)


(definec (A000695c n) (A000695 n)) ;; The cached variant. Be careful with this!
(definec (A059905c n) (A055905 n)) ;; The cached variant. Be careful with this!

(define (A059906 n) (A059905 (floor->exact (/ n 2)))) ;; Take the odd bits and contract
(define (A059906*2 n) (* 2 (A059906 n)))

;; a(0) = 0, a(1) = 1, a(2k) = 3*a(k), a(2k+1) = 2*a(k) + a(k+1).
(definec (A006046 n)
   (cond ((< n 2) n)
         ((even? n) (* 3 (A006046 (/ n 2))))
         (else (+ (* 2 (A006046 (/ (- n 1) 2)))
                  (A006046 (/ (+ n 1) 2))
               )
         )
   )
)


(define (A080978 n) (1+ (* 2 (A006046 n))))

(define (A006519 n) ;; Highest power of 2 dividing n: 1,2,1,4,1,2,1,8,1,2,1,4,1,2,1,16
   (cond ((zero? n) 0)
         (else (let loop ((n n) (i 1))
                 (cond ((odd? n) i)
                       (else (loop (floor->exact (/ n 2)) (* i 2)))
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
;;                     (else (loop (fix:lsh n -1) (1+ i))) ;; Dangerous code.
                       (else (loop (floor->exact (/ n 2)) (1+ i)))
                 )
               )
         )
   )
)

(define (A000265 n) (/ n (A006519 n))) ;; Remove 2s from n; or largest odd divisor of n.

(define (A003602 n) (let loop ((n n)) (if (even? n) (loop (/ n 2)) (/ (+ 1 n) 2)))) ;; Stand-alone version.
(define (A003602old n) (/ (+ 1 (A000265 n)) 2)) ;; Kimberling's paraphrases: if n = (2k-1)*2^m then a(n) = k


(define (A038189 n) (if (zero? n) n (A000035 (/ (- (A000265 n) 1) 2)))) ;; Bit to left of least significant 1-bit in binary expansion of n. 
(define (A025480 n) (- (A003602 (+ 1 n)) 1)) ;; Zero based
(define A091067 (MATCHING-POS 1 1 (COMPOSE even? A003602)))
(define A091067v2 (NONZERO-POS 1 0 A038189))

(define (A106836 n) (if (= 1 n) 3 (- (A091067 n) (A091067 (- n 1)))))

(define (A255068 n) (- (A091067 (+ n 1)) 1))

(definec (A241816 n)
  (cond ((zero? n) n)
        ((odd? n) (+ 1 (* 2 (A241816 (/ (- n 1) 2)))))
        ((zero? (modulo n 4)) (* 2 (A241816 (/ n 2))))
        (else (- n 1))
  )
)


;; One-based:
(define (A018900 n) (+ (expt 2 (A002024 n)) (expt 2 (A002262 (-1+ n)))))

(definec (A036987 n) (if (eq? (1+ n) (A006519 (1+ n))) 1 0))

(define (A209229 n) (if (pow2? n) 1 0)) ;; Characteristic function of powers of 2. (A036987 shifted once right)

(define A073268shifted_once_left (CONVOLVE 0 A000108 A036987))
;; (map A073268shifted_once_left (iota0 25))
;; --> (1 2 3 8 20 58 179 576 1902 6426 22092 77026 271702 967840 3476555 12578728 45800278 167693698 617037126 2280467586 8461771342 31510700712 117725789124 441141656810 1657559677646 6243810767912)

(define (A073265bi n k)
   (cond ((zero? n) n)
         ((zero? k) k)
         ((> k n) 0)
         ((eq? 1 k) (if (eq? n (A006519 n)) 1 0))
         (else
           (let sumloop ((i (A000523 (-1+ n))) (s 0))
                 (cond ((negative? i) s)
                       (else
                         (sumloop (-1+ i)
                                  (+ s (A073265bi (- n (fix:lsh 1 i)) (-1+ k)))
                         )
                       )
                 )
           )
         )
   )
)


;;
(define seqA003319 (list 1 1 3 13 71 461 3447 29093 273343 2829325 
                         31998903 392743957 5201061455 73943424413 
                         1123596277863 18176728317413 311951144828863 
                         5661698774848621 108355864447215063 
                         2181096921557783605
                   )
)

(define A000142test (INVERT (lambda (n) (list-ref seqA003319 (-1+ n)))))

(define A051296shifted_left (INVERT A000142))
(define A051295shifted_left (INVERT (compose-funs A000142 -1+)))

;; (define A061922 (EIGEN-CONVOLUTION 1 A048720bi))
(define A007460 (EIGEN-CONVOLUTION 1 A003986bi)) ;; OR-convolution with itself.
(define A007461 (EIGEN-CONVOLUTION 1 A004198bi)) ;; AND-convolution with itself.
(define A007462 (EIGEN-CONVOLUTION '(0 1) A003987bi)) ;; XOR-convolution with itself.
(define A007463 (EIGEN-CONVOLUTION 1 lcm)) ;; LCM-convolution with itself.
(define A007464 (EIGEN-CONVOLUTION 1 gcd)) ;; GCD-convolution with itself.
(define A025192 (EIGEN-CONVOLUTION 1 +))

(define A090826 (CONVOLVE 0 A000045 A000108))

(define A090826_shifted_left_check (CONVOLVE 0 (compose-funs A000045 1+) A000108))



;; A073265 - A073270 reserved for us.

(define (A073265 n) (A073265bi (1+ (A025581 n)) (1+ (A002262 n))))
(define (A073266 n) (A073265bi (1+ (A003056 n)) (1+ (A002262 n))))
(define (A073267 n) (A073265bi n 2)) ;; Occurs also as the FIX-counts of form 105. 6o6
;; (A073265bi n 1) ;; gives the characteristic function of A000079,
;; i.e. A036987 with offset 1 instead of 0, and a(0) = 0.

;; A073265(6,1) = 0
;; A073265(6,2) = 2 4+2, 2+4 -> 3+(1)+1, 1+(1)+3
;; A073265(6,3) = 4 2+2+2, 4+1+1, 1+4+1, 1+1+4 -> 1+(1)+1+(1)+1, 3+(1)+0+(1)+0, etc.
;; A073265(6,4) = 6 2+2+1+1, 2+1+2+1, 2+1+1+2, 1+2+1+2, 1+2+2+1, 1+1+2+2
;; A073265(6,5) = 5 2+1+1+1+1 * 5 --> 1+(1)+0+(1)+0+(1)+0+(1)+0
;; A073265(6,6) = 1 1+1+1+1+1+1 --> 0+(1)+0+(1)+0+(1)+0+(1)+0+(1)+0

(define (A073268 n) ;; The fix sequence for form 41, SwapBinTree o SwapDownCar (0 o 6)
  (if (zero? n)
      1
      (let sumloop ((i (floor->exact (/ (log n) (log 2)))) (s 0))
        (cond ((negative? i) s)
              (else (sumloop (-1+ i) (+ s (A000108 (- n (expt 2 i))))))
        )
      )
  )
)


(define (A073345bi n k)
  (cond ((zero? n) (if (zero? k) 1 0))
        ((zero? k) k)
        ((> k n) 0)
        (else
          (let ((half-n (fix:lsh (-1+ n) -1))
                (k-1 (-1+ k))
               )
           (+ (* 2 (add (lambda (i)
                          (* (A073345bi (- (-1+ n) i) k-1)
                             (add (lambda (j) (A073345bi i j))
                                  0 k-1
                             )
                          )
                        )
                        0 half-n
                   )
              )
              (* 2 (add (lambda (i)
                          (* (A073345bi (- (-1+ n) i) k-1)
                             (add (lambda (j) (A073345bi i j))
                                  0 (- k 2)
                             )
                          )
                        )
                        (1+ half-n) (-1+ n)
                   )
              )
              (if (odd? n) (* -1 (expt (A073345bi half-n k-1) 2)) 0)
           ) ;; +
          ) ;; let
        ) ;; else
  ) ;; cond
)


(define (A073346bi n k)
  (cond ((zero? k) (A036987 n))
        ((zero? n) 0)
        ((> k n) 0)
        (else
          (let ((half-n (fix:lsh (-1+ n) -1))
                (k-1 (-1+ k))
               )
           (+ (* 2 (add (lambda (i)
                          (* (A073346bi (- (-1+ n) i) k-1)
                             (add (lambda (j) (A073346bi i j))
                                  0 k-1
                             )
                          )
                        )
                        0 half-n
                   )
              )
              (* 2 (add (lambda (i)
                          (* (A073346bi (- (-1+ n) i) k-1)
                             (add (lambda (j) (A073346bi i j))
                                  0 (- k 2)
                             )
                          )
                        )
                        (1+ half-n) (-1+ n)
                   )
              )
              (if (odd? n) (* -1 (expt (A073346bi half-n k-1) 2)) 0)
              (if (= 1 k) (* -1 (A036987 n)) 0)
           ) ;; +
          ) ;; let
        ) ;; else
  ) ;; cond
)


(define (A073345 n) (A073345bi (A025581 n) (A002262 n)))
(define (A073346 n) (A073346bi (A025581 n) (A002262 n)))
(define (A073429 n) (A073345bi (A003056 n) (A002262 n)))
(define (A073430 n) (A073346bi (A003056 n) (A002262 n)))

(define (A073431 n)
   (cond
     ((zero? n) 1)
     (else
        (/ (add (lambda (i) (add (lambda (j) (A073346bi n j)) 0 (A007814 i)))
                1 (expt 2 (-1+ n))
           )
           (expt 2 (-1+ n))
        )
     )
   )
)


;; For some reason this seems to produce the same answers:
;; (Some peculiar interaction of A007814 & A073346 ?)

(define (A073431v2 n)
   (cond
     ((zero? n) 1)
     (else
       (-1+ (/ (add (lambda (i) (A073346bi n (A007814 i)))
                    1 (expt 2 (-1+ n))
               )
               (expt 2 (- n 2))
            )
       )
     )
   )
)



(definec (A048678 n) ;; Rewrite to Zeckendorf-expansion: 0->0, 1->01
  (cond ((zero? n) n)
        ((even? n) (* 2 (A048678 (/ n 2))))
        (else      (1+ (* 4 (A048678 (/ (- n 1) 2)))))
  )
)

;; 1, 1, 3, 2, 3, 3, 7, 4, 5, 3, 7, 6, 7, 7, 15, 8, 9, 5, ...
(definec (A106151 n) ;; In binary representation of n: delete one zero in each contiguous block of zeros.
  (cond ((zero? n) n) ;; Zero has either no zeros or infinite of them, in its bin.exp, depending on one's view.
        ((= (modulo n 4) 2) (+ (* 2 (A106151 (/ (- n 2) 4))) 1))
        ((= (modulo n 4) 0) (* 2 (A106151 (/ n 2))))
        (else (+ 1 (* 2 (A106151 (floor->exact (/ (- n 1) 2))))))
  )
)

;; A227349-A227352 are now reserved for your use.

(define (A227351 n) (A006068 (A048679 n)))
(define (A227351v2 n) (A006068 (A106151 (* 2 (A003714 n)))))
(define (A227352 n) (A048680 (A003188 n)))

(define (A022290 n) ;; I.e. interpret_as_zeckendorf_expansion
  (let loop ((n n) (s 0) (i 2))
     (cond ((zero? n) s)
           ((even? n) (loop (/ n 2) s (+ 1 i)))
           (else (loop (/ (- n 1) 2)
                       (+ s (A000045 i))
                       (+ 1 i)
                 )
           )
     )
  )
)


(define (A072648 n) ;; An approximate "inverse" of A000045 (of the fibonacci numbers)
   (cond ((zero? n) n)
         (else (floor->exact (/ (log (* n *Sqrt5*)) *LogPhi*)))
   )
)

;; 1,2,3,4,5,6,7,8,9,A,B,C,D,E,F,         ,21
;; 1,2,3,3,4,4,4,5,5,5,5,5,6,6,6,6,6,6,6,6,7,...
(define (A072649_dont_use_this_one n) ;; n occurs fibo(n) times. [This impl. uses floating point and log, DON'T USE!]
  (let ((b (A072648 n)))
     (+ -1 b (floor->exact (/ n (fibo (1+ b)))))
  )
)

(define A072649auxiliary (LEFTINV-LEASTMONO-NC2NC 0 0 (COMPOSE A000045 1+))) ;; This involves only integer arithmetic.

(define (A072649v2 n) (if (<= n 1) n (A072649auxiliary n))) ;; Doesn't work with large values (caching, memory?)

(define (A072649 n) ;; Reasonably fast iterative one. Search the smallest k for which F(k) > n, and return k-2.
  (if (<= n 3)
      n
      (let loop ((k 5)) (if (> (A000045 k) n) (- k 2) (loop (+ 1 k))))
  )
)


(define (A072650 n) ;; rewrite_0to0_x1to1
  (let loop ((n n) (s 0) (i 0))
     (cond ((zero? n) s)
           ((even? n) (loop (floor->exact (/ n 2)) s (1+ i)))
           (else (loop (floor->exact (/ n 4)) (+ s (expt 2 i)) (1+ i)))
     )
  )
)



(definec (A003714c n) ;; The cached variant.
  (cond ((< n 3) n)
        (else (+ (expt 2 (-1+ (A072649 n)))
                 (A003714c (- n (fibo (1+ (A072649 n)))))
              )
        )
  )
)


(define (A003714 n) ;; Iterative (tail-recursive) variant, not cached.
   (let loop ((n n) (s 0))
     (cond ((< n 3) (+ s n))
           (else (loop (- n (fibo (1+ (A072649 n))))
                       (+ s (expt 2 (-1+ (A072649 n))))
                 )
           )
     )
  )
)


(define (A014417 n) (A007088 (A003714 n)))


(define (A048679 n) (A072650 (A003714 n)))
(define (A048680 n) (A022290 (A048678 n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some edits of  Ctibor O. Zizka's sequences.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here are the 10 A-numbers you requested: A166012 --- A166021.


(definec (A133923 n)
  (cond ((< n 2) n)
        ((even? (A133923 (-1+ n))) (/ (A133923 (-1+ n)) 2))
        (else (A000005 (* n (A133923 (-1+ n)))))
  )
)


;; Auxiliary sequence for A138606:
(definec (A166012 n) (+ 1 (modulo n 2) (* 2 (- (A000045 n) (modulo n 2)))))

;; Note: A138606(n) = A072649(n) modulo 2.

(defineperm1 (A138606 n)
  (if (zero? n) n
      (+ (A166012 (-1+ (A072649 n))) (* 2 (- n (A000045 (1+ (A072649 n))))))
  )
)

(define (A166013 n) (A138606 (- n)))


(defineperm1 (A138607 n)
  (if (< n 3) n
      (let ((k (A083375 (- n 2))))
          (if (= (- n 2) (A007504 k))
              (+ 2 (A138607 (- n 1 (A000040 k))))
              (+ 2 (A138607 (-1+ n)))
          )
      )
  )
)

(define (A166014 n) (A138607 (- n)))

(defineperm1 (A138608 n)
  (if (< n 4) n
      (let ((k (A072649 n)))
          (if (= n (A000045 (1+ k)))
              (+ 3 (A138608 (- n 1 (A000045 k))))
              (+ 3 (A138608 (-1+ n)))
          )
      )
  )
)

(define (A166015 n) (A138608 (- n)))

(defineperm1  (A074147 n) ;; (2n-1) odd numbers followed by 2n even numbers.
   (if (zero? n) n
       (+ (A061925 (-1+ (A002024 n))) (* 2 (A002262 (-1+ n))))
   )
)

;; a(n) = n + {1,2,0,1} according as n == {0,1,2,3} mod 4.
(define (A116966 n) (+ n (list-ref '(1 2 0 1) (modulo n 4))))

(defineperm1 (A138609 n) (if (zero? n) n (A116966 (-1+ (A074147 n)))))

(define (A166016 n)  (A138609 (- n)))

;; Must be computed term-by-term before its inverse A166017 !

(defineperm1 (A138612 n)
  (if (< n 3) n
      (let loop ((k (if (zero? (A002262 (-1+ n))) 1 (A138612 (-1+ n))))
                 (i 1)
                )
            (cond ((not-lte? (A166017 i) (-1+ n))
                       (if (= 1 k) i (loop (-1+ k) (1+ i)))
                  )
                  (else (loop k (1+ i)))
            )
      )
  )
)

(define (A166017 n) (A138612 (- n)))
(define (A166018 n) (A138612 (A000124 (-1+ n)))) ;; Leading edge
(define (A166019 n) (A138612 (A000217 n))) ;; Trailing edge.
(define A166020 (ROWSUMS1 A138612))


;; A126917 a(1)=1. a(n) = the (largest proper divisor of n)th integer from among those positive integers not occurring earlier in the sequence.

;; The term k does not occur in a(1) .. a(n-1) if A126918(k) > n-1 (or #f)

(defineperm1 (A126917 n)
   (if (<= n 1)
       n
       (let loop ((i (A243069 (- n 1))) ;; Could be (i 1) as well, A243069 is just for optimization.
                  (the-nth-one (- (A032742 n) 1))
                 )
             (cond ((not-lte? (A126918 i) n) ;; Found i such that A126918(i) > n, thus that i unused.
                        (if (zero? the-nth-one) ;; Is it ours?
                            i
                            (loop (+ i 1) (- the-nth-one 1))
                        )
                   )
                   (else (loop (+ i 1) the-nth-one)) ;; i already encountered, keep on searching.
             )
       )
   )
)

(define (A126918 n) (A126917 (- n)))

(definec (A243069 n)
  (cond ((<= n 3) (+ 1 n))
        ((= (A126917 n) (A243069 (- n 1)))
           (let loop ((i (A126917 n)))
              (if (not-lte? (A126918 i) n) ;; Found i such that A126918(i) > n, thus i has not occurred yet.
                  i
                  (loop (+ 1 i)) ;; Otherwise, A126918(i) <= n, thus keep on searching.
              )
           )
        )
        (else (A243069 (- n 1)))
  )
)

(define (A243488 n) (- (A126917 (+ 1 n)) (A243069 n)))
(define (A243498 n) (A243069 (A008578 n)))

;; A119435 a(n) = (binary reversal of n)th integer among those pos. integers not occurring earlier in the sequence.
(defineperm1 (A119435 n)
   (if (<= n 1)
       n
       (let loop ((i 1) ;; No optimization.
                  (the-nth-one (- (A030101 n) 1))
                 )
             (cond ((not-lte? (A119436 i) n) ;; Found i such that A119436(i) > n, thus that i unused.
                        (if (zero? the-nth-one) ;; Is it ours?
                            i
                            (loop (+ i 1) (- the-nth-one 1))
                        )
                   )
                   (else (loop (+ i 1) the-nth-one)) ;; i already encountered, keep on searching.
             )
       )
   )
)

(define (A119436 n) (A119435 (- n)))

;; A246156-A246167 are now reserved for your use.
;; A246165 a(n) = prime factorization shifted once right(n)-th integer among those pos. integers not occurring earlier in the sequence.
(defineperm1 (A246165 n)
   (if (<= n 1)
       n
       (let loop ((i 1) ;; No optimization.
                  (the-nth-one (- (A064989 n) 1))
                 )
             (cond ((not-lte? (A246166 i) n) ;; Found i such that A246166(i) > n, thus that i unused.
                        (if (zero? the-nth-one) ;; Is it ours?
                            i
                            (loop (+ i 1) (- the-nth-one 1))
                        )
                   )
                   (else (loop (+ i 1) the-nth-one)) ;; i already encountered, keep on searching.
             )
       )
   )
)

(define (A246166 n) (A246165 (- n)))


;; tabl
(definec (A166021 n)
     (if (zero? (A002262 (-1+ n))) (* 2 (A000124 (A003056 (-1+ n)))) (1+ (A166021 (-1+ n))))
)

(define A136272 (COMPLEMENT 1 A166021))

;; Related to binary beanstalk:

;; From Nico Brown:
;; A218252 Start with 1. For each term m, the next term is the smallest positive integer k such that k - (sum of base 2 digits of k) = m. If no such k exists, use the first number not already in the sequence.


(defineperm1 (A218252 n)
  (cond ((<= n 1) n)
        ((A213723 (A218252 (- n 1)))
          => (lambda (next_maybe)
               (if (not (zero? next_maybe))
                   next_maybe
                   (let loop ((i (A000012 (- n 1)))) ;; Could be optimized, replace A000012 with ???
                       (if (not-lte? (A257683 i) n) ;; Found i such that A257683(i) > n, thus that i unused.
                           i
                           (loop (+ i 1))
                       )
                   )
               )
             )
        )
  )
)

;; A257676-A257697 are now reserved for your use. 
(define (A257683 n) (A218252 (- n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (A000217 n) (/ (* n (+ n 1)) 2)) ;;  ;; /XFER: core.triang

(define (A000096 n) (* n (+ n 3) (/ 1 2))) ;; n*(n+3)/2.

(define (A010054 n) (if (zero? n) 1 (- (A002024 (+ n 1)) (A002024 n)))) ;; The charfun of A000217.

(define (A000124 n) (1+ (A000217 n)))

;; Complement of A000217:
(define (A014132 n) (+ n (A002024 n)))

(define (A014132v2 n) (A014132bi (A002260 n) (A004736 n))) ;; T(n,k) = ((n+k)^2 + n-k)/2, n, k > 0, read by antidiagonals. 
(define (A014132bi n k) (/ (+ (expt (+ n k) 2) n (- k)) 2))

;;;;;;;;;;;;
;; Interlude: binary trees & other entanglements:

;; In Kimberling style:

(definec (A183079 n) ;; Inverse: A220347.
   (cond ((<= n 1) n)
         ((even? n) (A014132 (A183079 (/ n 2))))
         (else (A000217 (A183079 (/ (+ n 1) 2))))
   )
)


(definec (A220347 n)
   (cond ((<= n 1) n)
         ((zero? (A010054 n)) (* 2 (A220347 (A083920 n))))
         (else (+ -1 (* 2 (A220347 (A002024 n)))))
   )
)

(define (A220348 n) (+ 1 (A029837 (A220347 n)))) ;; Number of row, where n occurs in A183079.
(define (A220348v2 n) (A070941 (+ -1 (A220347 n))))


;;;; In our style:

(definec (A257797 n)
   (cond ((<= n 1) n)
         ((zero? (A010054 n)) (* 2 (A257797 (A083920 n))))
         (else (+ 1 (* 2 (A257797 (+ -1 (A002024 n))))))
   )
)


(definec (A257798 n)
   (cond ((<= n 1) n)
         ((even? n) (A014132 (A257798 (/ n 2))))
         (else (A000217 (+ 1 (A257798 (/ (- n 1) 2)))))
   )
)


;;;;;;;;;;;;


;; The smallest m such that the m-th triangular number is greater than or equal to half the n-th triangular number. 
;; (define (A257175unreliable n) (ceiling->exact (/ (+ -1 (sqrt (+ (* 2 n n) n n 1))) 2))) ;; By Greathouse's formula
(define (A257175 n) (let ((t_per_2 (/ (A000217 n) 2))) (let loop ((m 0)) (if (>= (A000217 m) t_per_2) m (loop (+ 1 m))))))

(define (A000290 n) (* n n)) ;; The squares: a(n) = n^2.
;; For clojure the syntax is: (defn A000290 "The squares: a(n) = n^2." [n] (* n n))
;; In Racket we can also say: (define [A000290 n] "The squares: a(n) = n^2." (* n n))
;; For python the syntax is:
;; def A000290(n):
;;	"The squares: a(n) = n^2."
;;	return(n*n)
;;
;; (defseq 0 (A000290 n) "The squares: a(n) = n^2." (* n n))
;; (defseq A000290 "The squares: a(n) = n^2." 0 (n) (* n n))
;;

(define (A016754 n) (A000290 (A005408 n))) ;; Odd squares, offset=0.

(define (A000578 n) (* n n n)) ;; The cubes: a(n) = n^3. 

(define (A048762 n) ;; Largest cube <= n. Do not use any floating point operations here. Should go quite quickly.
  (let loop ((i 0))
     (let ((k (expt i 3)))
       (if (> k n) (expt (- i 1) 3) (loop (+ 1 i)))
     )
  )
)

(define (A055400 n) (- n (A048762 n)))

(definec (A055401 n) (if (zero? n) n (+ 1 (A055401 (A055400 n)))))


(define (sqrt n) (error "Function sqrt disabled in IntSeq-library. Use A000196 instead to get correct floored-down integer values. Called with: " n)) ;; We don't deprecate, we disable!

(define A000196 (LEFTINV-LEASTMONO-NC2NC 0 0 A000290)) ;; /XFER: core.squares
;; (define (A000196unreliable n) (floor->exact (sqrt n)))




(define (A010052 n) (if (zero? n) 1 (- (A000196 n) (A000196 (- n 1))))) ;; Characteristic function of squares: 1 if n is a square else 0. 

;; (define (square? n) (= n ((lambda (r) (* r r)) (floor->exact (sqrt n)))))
(define (square? n) (not (zero? (A010052 n)))) ;; If somebody ever uses this one...

(define (A003059 n) (+ (A000196 n) (- 1 (A010052 n)))) ;; Add one to A000196 when n is not a square.
;; (define (A003059unreliable n) (ceiling->exact (sqrt n))) ;; n appears 2n-1 times.


(define (A000330 n) (* (/ 1 6) n (+ n 1) (+ n n 1))) ;; Square pyramidal numbers: 0^2 + 1^2 + 2^2 +...+ n^2 = n*(n+1)*(2*n+1)/6.

(define A074279 (LEAST-GTE-I 1 1 A000330)) ;; off=1 version.

;; Write numbers 1, then 1 up to 2^2, then 1 up to 3^2, then 1 up to 4^2 and so on.
(define (A064866 n) (- n (A000330 (- (A074279 n) 1))))

;; A237447-A237452 are now reserved for your use.

(define (A237450 n) (+ (A003422 (A002024 n)) (* (A002262 (- n 1)) (A000142 (- (A002024 n) 1)))))

(define (A237451 n) (modulo (-1+ (A064866 n)) (A074279 n))) ;; The zero-based column index for subsquares.
(define (A237452 n) (floor->exact (/ (-1+ (A064866 n)) (A074279 n)))) ;; The zero-based row index for subsquares.


(definec (A237265 n)
    (cond
       ((zero? (A237452 (+ n (A074279 n))))
          (+ (A237451 n) (if (zero? (A237451 n)) (A074279 n) 0))
       )
       ((zero? (A237451 (+ n 1))) (A074279 n))
       (else (A237265 (+ 1 (A000330 (- (A074279 n) 2)) (* (- (A074279 n) 1) (A237452 n)) (A237451 n))))
    )
)

(define (A237265v3 n)
  (let ((col (A237451 n))
        (row (A237452 n))
       )
    (A237447 (+ 1 (/ (+ (expt (+ col row) 2) col (* 3 row)) 2)))
  )
)


(define (A237447 n)
  (+ (* (A010054 n) (A002024 n)) ;; For leftmost column.
     (* (- 1 (A010054 n)) (- (A004736 n) (if (>= (A002260 n) (A004736 n)) 1 0)))
  )
)


(define (A237447v2 n)
  (let* ((row (A002260 n))
         (col (A004736 n))
         (sss (max row col))
         (sof (+ 1 (A000330 (- sss 1))))
        )
      (A237265 (+ sof (* sss (- row 1)) (- col 1)))
  )
)


(define (A237448 n)
  (cond ((= 1 (A010054 (- n 1))) (A002024 n)) ;; For topmost row.
        ((< (A004736 n) (A002260 n)) (A002260 n))
        (else (- (A002260 n) 1))
  )
)


(define (A237448v2 n) ;; Transpose of A237447
  (let* ((row (A002260 n))
         (col (A004736 n))
         (sss (max row col))
         (sof (+ 1 (A000330 (- sss 1))))
        )
      (A237265 (+ sof (* sss (- col 1)) (- row 1)))
  )
)

(definec (A074148 n) (if (< n 2) n (+ (A074148 (- n 2)) n n))) ;; a(n) = a(n-2)+2n, a(0)=0, a(1)=1.
(define (A074148v2 n) (add A237447 (A000124 (- n 1)) (A000217 n))) ;; Seems to be, check!
(define (A074148v3 n) (add A237448 (A000124 (- n 1)) (A000217 n))) ;; Follows from above.


(definec (A237265v2 n)
    (cond
       ((> (A074279 (+ n (A074279 n))) (A074279 n))
          (+ (A237451 n) (if (zero? (A237451 n)) (A074279 n) 0))
       )
       ((zero? (A237451 (+ n 1))) (A074279 n))
       (else (A237265v2 (+ 1 (A000330 (- (A074279 n) 2)) (* (- (A074279 n) 1) (A237452 n)) (A237451 n))))
    )
)

 
;; A237265 the last row of each subsquare: (A074279(n+A074279(n))-A074279(n))*((A064866(n)-1 mod A074279(n))+([(A064866(n) mod A074279(n))=1]*A074279(n)))
;; A new sequence a(n) = (A064866(n)-1 mod A074279(n)) would be good,
;; also a(n) = (A064866(n) mod A074279(n)).

(define (A028391 n) (- n (A000196 n)))

(define (A000267 n) (A000196 (A016813 n))) ;; Integer part of square root of 4n+1.
(definec (A000267off1 n) (if (zero? n) 0 (+ 1 (A000267off1 (A028391 n))))) ;; Check.


(define (A000037 n) (+ n (A000194 n))) ;; Numbers that are not squares.

(define (A002061 n) (+ 1 (* n (- n 1)))) ;; n^2 - n + 1 = (n * (n-1))+1

(define (A006527 n) (/ (+ n n (* n n n)) 3)) ;; (n^3 + 2*n)/3 ;; Partial sums of A002061.
;; "Proof": ((n^3) + 2n) - (((n-1)^3) + (2(n-1))) =  ((n^3) + 2n) - (n^3 - 3n^2 + 3n -1) - 2n+2
;; = 3n^2 - 3n +3 divided by 3 is n^2 - n +1

;; Zero-based:
(define A227177 (LEAST-GTE-I 0 0 A006527)) ;; n occurs A002061(n) times: 0,1,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,

;; 1; 2,2,2; 3,3,3,3,3,3,3; 4,4,4,4,4,4,4,4,4,4,4,4,4,4;  5,5
;; 0; 0,1,2; 0,1,2,3,4,5,6; 0,1,2,3,4,5,6,7,8,9,10,11,12; 0,
(define (A227179 n) (- n (+ 1 (A006527 (- (A227177 n) 1)))))
;; (0 0 0 1 2 0 1 2 3 4 5 6 0 1 2 3 4 5 6 7 8 9 10 11 12 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 0 ...)

(define (A227181 n) (+ (A227177 n) (A227179 n)))

;; Simple self-inverse permutation of natural numbers: List each block of A002061(n) numbers from A116731(n) to A006527(n+1) in reverse order.
(define (A227182 n) (- (A006527 (A227177 n)) (A227179 n)))

;; A227786-A227790 are now reserved for your use.
(definec (A168276 n) (if (< n 2) (+ n 1) (- (* 4 n) (A168276 (- n 1)) 4))) ;; a(n)=4*n-a(n-1)-4 (with a(1)=2)

(definec (A227786 n) (if (< n 2) n (+ (A227786 (- n 1)) (A168276 (+ n 1)))))

(define (A227786v2 n) (- (A000290 (+ n 1)) 2 (modulo n 2)))


(define (A061925 n) (+ 1 (/ (+ (A000290 n) (A000035 n)) 2))) ;; Ceiling[n^2/2]+1
;; (define (A061925 n) (1+ (ceiling->exact (/ (A000290 n) 2))))

(define (A046092 n) (*  2 n (1+ n))) ;; This gives the central diagonal from zero-indexed arrays/tables.


(define A003056 (LEFTINV-LEASTMONO-NC2NC 0 0 A000217))

;; (define (A003056unreliable n) ;; repeat n n+1 times, starting from n = 0.
;;  (floor->exact (- (sqrt (* 2 (1+ n))) (/ 1 2)))
;; )

(define (A083920 n) (- n (A003056 n))) ;; Number of nontriangular numbers <= n.

(define (A002024 n) (+ 1 (A003056 (- n 1))))

;; (define (A002024unreliable n) ;; repeat n n times, starting from n = 1.
;;  (floor->exact (+ (/ 1 2) (sqrt (* 2 n))))
;; )


;; Repeat n 2n times, starting from n=1:
(define (A000194 n) (A002024 (+ 1 (/ (- n 1 (A000035 (- n 1))) 2))))
;; (define (A000194 n) (A002024 (1+ (floor->exact (/ (-1+ n) 2)))))



;; (map A002262 (cons 0 (iota 20))) --> (0 0 1 0 1 2 0 1 2 3 0 1 2 3 4 0 1 2 3 4 5)

(define (A002262 n) (- n (A000217 (A003056 n)))) ;; a(n) = n - the largest triangular number <= n. (Murthy)


;; (definec (A002262unreliable n) ;; The Y component (row) of square {0..inf} arrays
;;  (- n (binomial_n_2 (floor->exact (flo:+ 0.5 (flo:sqrt (exact->inexact (* 2 (1+ n))))))))
;; )


(define (A002260 n) (+ 1 (A002262 (- n 1))))


;; (map A025581 (cons 0 (iota 20))) --> (0 1 0 2 1 0 3 2 1 0 4 3 2 1 0 5 4 3 2 1 0)

;; (definec (A025581 n) ;; The X component (column) of square {0..inf} arrays
;;   (- (binomial_n_2 (1+ (floor->exact (+ (/ 1 2) (sqrt (* 2 (1+ n))))))) (1+ n))
;; )
;; 
;; ;; (map A002262 (cons 0 (iota 20))) --> (0 0 1 0 1 2 0 1 2 3 0 1 2 3 4 0 1 2 3 4 5)
;; (definec (A002262 n) ;; The Y component (row) of square {0..inf} arrays
;;   (- n (binomial_n_2 (floor->exact (+ (/ 1 2) (sqrt (* 2 (1+ n)))))))
;; )

;; At some point these will produce incorrect values, because of the
;; limited precision of IEEE 64-bit floating point numbers.
;; What is that point, and how to recode these with strictly fixnum-only
;; way? (I need a fixnum-only square root algorithm...)

(define (A025581 n) (- (A003056 n) (A002262 n)))

;; (definec (A025581unreliable n) ;; The X component (column) of square {0..inf} arrays
;;  (- (binomial_n_2 (1+ (floor->exact (flo:+ 0.5 (flo:sqrt (exact->inexact (* 2 (1+ n)))))))) (1+ n))
;; )


(define (A004736 n) (+ 1 (A025581 (- n 1))))



;; Further reduced?
(define (A236345 n) (+ (- (A002024 (A000290 n)) (A002024 n)) (abs (- (A002260 (A000290 n)) (A002260 n)))))


;; Integers 1 to 2k followed by integers 1 to 2(k+1) and so on.
;; 1,2,1,2,3,4,1,2,3,4,5,6,1,2,3,4,5,6,7,8,
(define (A074294 n) (- n (* 2 (A000217 (-1+ (A000194 n))))))

;; Integers (2k)-1..0 followed by integers (2k)+1..0 and so on:
;; 1,0,3,2,1,0,5,4,3,2,1,0,7,6,5,4,3,2,1,0,...
(define (A179753 n) (- (* 2 (A000194 n)) (A074294 n)))

(define (A135034 n) (if (zero? n) n (A003059 n))) ;; Nonnegative integers repeated n times, where n = A005408, with a leading 0.

(define (A048761 n) (A000290 (A135034 n))) ;; Smallest square >= n.

(define (A068527 n) (- (A048761 n) n)) ;; Difference between smallest square >= n and n.

;; Could be wittier and smarter: (e.g. factor (* n n) out from the latter...)
(define (A053698 n) (+ (* n n n) (* n n) n 1)) ;; n^3 + n^2 + n + 1.
(define (A053699 n) (+ (* n n n n) (* n n n) (* n n) n 1)) ;; n^4 + n^3 + n^2 + n + 1.

(define (A232395 n) (A068527 (A053698 n))) ;; (ceiling(sqrt(n^3 + n^2 + n + 1)))^2 - (n^3 + n^2 + n + 1).

(define (A232397 n) (A068527 (A053699 n)))


(define (A048760 n) (A000290 (A000196 n))) ;; Largest square <= n. 

(define (A053186 n) (- n (A000290 (A000196 n)))) ;; Square excess of n: difference between n and largest square <= n. 

(define (A071797 n) (+ 1 (A053186 (- n 1)))) ;; Restart counting after each new odd integer (a fractal sequence) (o=1)

;; (define (A071797 n) (- n (expt (floor->exact (sqrt (- n 1))) 2)))




(define (A001477 n) n)

(define A000027 A001477)

(define (A001489 n) (- n))

(define (A023443 n) (- n 1))

(define (A020725 n) (+ n 1)) ;; Actually "Integers >= 2.", with offset=1.

(define (packA001477 x y) (/ (+ (expt (+ x y) 2) x (* 3 y)) 2))
(define (packA061579 x y) (/ (+ (expt (+ x y) 2) (* 3 x) y) 2))

(define A001477bi packA001477)


(define (packA000027 x y) (* (/ 1 2) (- (expt (+ x y) 2) x y y y -2))) ;; XXX - Same direction as with packA001477 ?!
(define (packA000027v2 x y) (* (/ 1 2) (+ (expt (+ x y) 2) (- x) (- (* 3 y)) 2)))
;; (first-dislocated (cons 0 (map (lambda (n) (packA000027 (A002260 n) (A004736 n))) (iota 120)))) --> ()
(define A000027bi packA000027)

;; I.e. (define (id n) (packA001477 (A025581 n) (A002262 n)))
;; and

(define (A061579 n) (packA061579 (A025581 n) (A002262 n)))

(define (A038722 n) (if (zero? n) n (1+ (A061579 (-1+ n)))))

;; Gives id (A001477): (+ (A000695 (A059905 n)) (* 2 (A000695 (A059906 n))))
(define (A057300 n) (+ (A000695 (A059906 n)) (* 2 (A000695 (A059905 n)))))
(define (A054238 n) (+ (A000695 (A025581 n)) (* 2 (A000695 (A002262 n)))))
(define (A054239 n) (packA001477 (A059905 n) (A059906 n)))

;; Added February 18, 2013 after Robert Israel's May 08 2012 formula for A061857:

;; Let n+1 = b mod k with 0<=b<k, q = (n+1-b)/k.  Let k = c mod 2, c = 0 or 1.
;; If b = 0 or 1 then a(n,k) = q^2*k/2 + q*b - 2*q - b + 1 + c*q/2.
;; If b >= (k+3)/2 then a(n,k) = q^2*k/2 + q*b - 2*q + b - 1 - k/2 + c*(q+1)/2.
;; Otherwise a(n,k) = q^2*k/2 + q*b - 2*q + c*q/2.
;;

(define (A220691bi n k)
   (let* ((b (modulo (+ 1 n) k))
          (q (/ (- (+ 1 n) b) k)) ;; (floor-exact (/ (+ 1 n) k))
          (c (modulo k 2))
         )
      (cond ((< b 2)
                 (+ (* q q k (/ 1 2)) (* q b) (* -2 q) (* -1 b) +1 (* c q (/ 1 2)))
            )
            ((>= b (/ (+ k 3) 2)) 
                 (+ (* q q k (/ 1 2)) (* q b) (* -2 q) b -1 (* (/ k -2)) (* c (+ 1 q) (/ 1 2)))
            )
            (else
                 (+ (* q q k (/ 1 2)) (* q b) (* -2 q) (* c q (/ 1 2)))
            )
      )
   )
)


(define (A061857 n) (A220691bi (A002024 n) (A002260 n)))

(define (A220691 n) (A220691bi (A002260 n) (A004736 n)))
(define (A220692 n) (A220691bi (A004736 n) (A002260 n)))
(define (A220693 n) (A220691bi (A003059 n) (A071797 n)))


(define (A234575bi n k) (+ (floor->exact (/ n k)) (modulo n k)))
(define (A234575 n) (A234575bi (A002024 n) (A002260 n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A236677-A236678 are now reserved for your use.

(definec (A236677 n)
  (let loop ((n n) (i 0))
     (cond ((zero? n) 1)
           ((odd? n)
               (if (= 1 (A236677 i))
                   0
                   (loop (/ (- n 1) 2) (+ i 1))
               )
           )
           (else (loop (/ n 2) (+ i 1)))
     )
  )
)

(definec (A236677v2 n) (if (zero? n) 1 (* (- 1 (A236677v2 (A000523 n))) (A236677v2 (A053645 n)))))

(definec (A236678 n) (if (zero? n) 1 (+ (A236678 (- n 1)) (A236677 n))))

(define A079599v2 (MATCHING-POS 0 0 (lambda (n) (= 1 (A236677 n)))))

;;    0    0            0
;;    1    2           1x
;;    2    8         1x0x
;;    3   10         1x1x
;;    4   16        10x0x
;;    5   18        10x1x
;;    6   24        11x0x
;;    7   26        11x1x
;;    8   32       100x0x
;;    9   34       100x1x
;;   10   40       101x0x
;;   11   42       101x1x

;;   16   64      1000x0x
;;   32  128     10000x0x
;;   64  512   1x00000x0x
;;  127  762   1x11111x1x
;;  128 2048 1x0x00000x0x
;;  129 2050 1x0x00000x1x

(define (A079599 n)
  (let loop ((n n) (i 0) (j 0) (s 0))
     (cond ((zero? n) s)
           ((odd? n) (loop (/ (- n 1) 2) (+ i 1) (+ j 1 (A236677 j)) (+ s (expt 2  (+ j (A236677 j))))))
           (else (loop (/ n 2) (+ i 1) (+ j 1 (A236677 j)) s))
     )
  )
)


;; d = (A236678 (A000523 (A079599 (A072376 n))))
;; ( (lambda (n) (A236678 (A070939 (A079599 (floor->exact (/ n 2)))))) 64) = 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



