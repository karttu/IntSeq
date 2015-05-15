;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;  http://www.research.att.com/~njas/sequences/miscnum1.scm.txt          ;;
;;                                                                        ;;
;;  Coded by Antti Karttunen (Antti.Karttunen(-AT-)iki.fi), 2004          ;;
;;                                                                        ;;
;;  This file contains the Scheme-functions that compute the sequences    ;;
;;                 A0xxxxx                                                ;;
;;                             found in                                   ;;
;;     Neil Sloane's On-Line Encyclopedia of Integer Sequences (OEIS)     ;;
;;                            available at                                ;;
;;             http://www.research.att.com/~njas/sequences/               ;;
;;                                                                        ;;
;;  Copy of THIS source file is also available at:                        ;;
;;  http://www.iki.fi/~kartturi/matikka/Schemuli/miscnum1.scm             ;;
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
;;  to me with e-mail (with subject TOPIC: miscnum1.scm) and I can edit   ;;
;;  them to this program. Alternatively, you can send the improved        ;;
;;  program directly to Neil Sloane.                                      ;;
;;                                                                        ;;
;;  Edited 05. Sep 2005 by Antti Karttunen.                               ;;
;;  Last edited 31 Oct 2009. Note that some of the Cxxxxxx functions      ;;
;;  might have been submitted to OEIS now with some A-numbers.            ;;
;;                                                                        ;;
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
;; (load "c:\\slib\\mitscheme.init") ;; A. Jaffer's SLIB Scheme library.
;; (load "c:\\Program Files (x86)\\slib\\mitscheme.init")
;; (require 'factor)                           ;; 

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
(define *MAX-CACHE-SIZE-FOR-DEFINEC* 290512) ;; Was 131072


(define-syntax definec
  (syntax-rules ()
   ((definec (name arg) e0 ...)
      (define name
        (letrec
           ((_cache_ (vector #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
            (name
             (lambda (arg)
               (cond ((null? arg) _cache_)
                     ((>= arg *MAX-CACHE-SIZE-FOR-DEFINEC*)
                          e0 ...
                     )
                     (else
                         (if (>= arg (vector-length _cache_))
                             (set! _cache_
                                   (vector-grow
                                           _cache_
                                           (min *MAX-CACHE-SIZE-FOR-DEFINEC*
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


;; fun_on_i should be N -> N function, preferably cached.

(define (fun-succ-recpositions1 fun_on_i)
 (letrec
   ((_cache_ (vector #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
    (arlaug ;; The function we are defining and returning here.
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
                   ) ;; Invocation of the lambda-form
               ) ;; or
             ) ;; else
       ) ;; cond
      ) ; lambda (n)
    )
   ) ;; letrec-definitions.
  arlaug
 ) ;; letrec
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  Our own naive version of factor. But note that it is cached.           ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definec (factor n)
  (cond ((< n 2) (list)) ;; 0,1
        ((< n 4) (list n)) ;; 2,3
        (else
           (let divloop ((i (floor->exact (sqrt n))))
               (cond ((= i 1) (list n)) ;; It's a prime.
                     ((integer? (/ n i)) (append (factor (/ n i)) (factor i)))
                     (else (divloop (- i 1)))
               )
           )
        )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  Copied from http://www.iki.fi/~kartturi/matikka/Schemuli/intfuns1.scm  ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(definec (binomial_n_2 n) (/ (* (-1+ n) n) 2))

(define (A000217 n) (binomial_n_2 (+ n 1)))

(definec (A025581 n) ;; The X component (column) of square {0..inf} arrays
  (- (binomial_n_2 (1+ (floor->exact (flo:+ 0.5 (flo:sqrt (exact->inexact (* 2 (1+ n)))))))) (1+ n))
)

;; (map A002262 (cons 0 (iota 20))) --> (0 0 1 0 1 2 0 1 2 3 0 1 2 3 4 0 1 2 3 4 5)
(definec (A002262 n) ;; The Y component (row) of square {0..inf} arrays
  (- n (binomial_n_2 (floor->exact (flo:+ 0.5 (flo:sqrt (exact->inexact (* 2 (1+ n))))))))
)


(define (A002024 n) ;; repeat n n times, starting from n = 1.
  (floor->exact (+ (/ 1 2) (sqrt (* 2 n))))
)

(define (A003056 n) ;; repeat n n+1 times, starting from n = 0.
  (floor->exact (- (sqrt (* 2 (1+ n))) (/ 1 2)))
)

(define (A001477 n) n)

(define (A000079 n) (expt 2 n))
(define (A000225 n) (-1+ (A000079 n)))
(define (A000051 n) (1+ (A000079 n)))
;; Offset=0: 2,3,5,9,17,33,65,129,257,513,

(define (A000523 n) (cond ((zero? n) -1) (else (floor->exact (/ (log n) (log 2))))))

(define (binwidth n) ;; = A029837(n+1)
  (let loop ((n n) (i 0))
     (if (zero? n)
         i
         (loop (floor->exact (/ n 2)) (1+ i))
     )
  )
)


(definec (A030101 nn) ;; Was: binrev
  (let loop ((z 0) (n nn))
    (if (zero? n)
        z
        (loop (+ (* 2 z) (modulo n 2))
              (fix:lsh n -1) ;; n >>= 1
        )
    )
  )
)

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


;; "Binary asymmetry index", 0 if n is binary palindrome.
(define (A037888 n) (/ (A000120 (A003987bi n (A030101 n))) 2))

(define (A095734 n) (A037888 (A003714 n)))


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

(define attach! ; Borrowed from Franz lisp, is like destructive cons.
  (lambda (elem lista)
     (set-cdr! lista (cons (car lista) (cdr lista)))
     (set-car! lista elem)
     lista
  )
)

;; Why not use reduce or fold-left or fold-right here?
;; Because I didn't know about them at the time I wrote these.
;; And then I would need to generate the list of indices
;; first, which is wasteful. But for Sum over divisors
;; it is nice.

;; Implement sum_{i=lowlim..uplim} intfun(i)
(define (add intfun lowlim uplim)
   (let sumloop ((i lowlim) (res 0))
      (cond ((> i uplim) res)
            (else (sumloop (1+ i) (+ res (intfun i))))
      )
   )
)

(define (mul intfun lowlim uplim)
   (let multloop ((i lowlim) (res 1))
      (cond ((> i uplim) res)
            (else (multloop (1+ i) (* res (intfun i))))
      )
   )
)


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


(define (DIFF a)
  (map - (cdr a) (reverse! (cdr (reverse a))))
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



(define (remove-duplicates lista) ;; lista should be sorted.
   (let loop ((lista lista)
              (res (list))
             )
      (cond ((null? lista) (reverse! res))
            ((null? res) (loop (cdr lista) (cons (car lista) res)))
            ((equal? (car lista) (car res)) (loop (cdr lista) res))
            (else (loop (cdr lista) (cons (car lista) res)))
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


(define (do-not-use-multiplicities lista) ;; Of numeric elements.
       (let loop ((mults (list))
                  (lista lista)
                  (prev #f)
                 )
           (cond ((not (pair? lista)) mults)
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


(define (elemcountpairs lista) ;; Of numeric elements, already sorted.
       (let loop ((pairs (list))
                  (lista lista)
                  (prev #f)
                 )
           (cond ((not (pair? lista)) (reverse! pairs))
                 ((equal? (car lista) prev)
                     (set-cdr! (car pairs) (+ 1 (cdar pairs)))
                     (loop pairs (cdr lista) prev)
                 )
                 (else ;; A new item?
                   (loop (cons (cons (car lista) 1) pairs) (cdr lista) (car lista))
                 )
           )
       )
)


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

;; Examples:
;; (m-intersect (list 2 2 3 3 3 7 7 11 23 29 29 37) (list 2 3 3 3 3 3 3 5 7 29 31 37 37))
;; --> (2 3 3 3 7 29 37)

;; (m-union (list 2 2 3 3 3 7 7 11 23 29 29 37) (list 2 3 3 3 3 3 3 5 7 29 31 37 37))
;; --> (2 2 3 3 3 3 3 3 5 7 7 11 23 29 29 31 37 37)

;; (m-intersect (list 3 7 9) (list 2 4 6 8)) --> ()
;; (m-union (list 3 7 9) (list 2 4 6 8))     --> (2 3 4 6 7 8 9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;                                                                         ;;
;;                                                                         ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; %F A000010 Multiplicative with a(p^e) = (p-1)*p^(e-1). - David W. Wilson, Aug 01, 2001.

(define (sub1from1st_nums lista)
  (let loop ((prev 0)
             (lista lista)
             (res (list))
            )
     (cond ((null? lista) res)
           (else
              (loop (car lista)
                    (cdr lista)
                    (cons (- (car lista) (if (= (car lista) prev) 0 1)) res)
              )
           )
     )
  )
)

(define (A000010 n) (apply * (sub1from1st_nums (sort (factor n) <))))

;; %F A000203 Multiplicative with a(p^e) = (p^(e+1)-1)/(p-1). - David W. Wilson, Aug 01, 2001.

(define (A000203 n)
   (fold-left (lambda (prod p.e) (* prod (/ (- (expt (car p.e) (+ 1 (cdr p.e))) 1) (- (car p.e) 1))))
              1
              (if (= 1 n) (list) (elemcountpairs (sort (factor n) <)))
   )
)


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

(define vecA000040 (fasload "sA000040.vec"))

(define (A008578 n) ;; A008578 (non-composite numbers) ;; Was ithprime
   (cond ((< n 3) (1+ n)) ;; 0 -> 1, 1 -> 2, 2 -> 3,
         (else (vector-ref vecA000040 (- n 1)))
   )
)

;; (define (A000040 n) (vector-ref vecA000040 (- n 1)))
(define A000040 A008578) ;; In practice it is good to covetly return also (A000040 0) as 1.

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

(definec (A049084dont_use n)
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


(definec (ifactor n) (cond ((< n 2) (list)) (else (sort (factor n) <))))

(definec (A001221 n) (distinct-elems (ifactor n)))
(definec (A001222 n) (length (ifactor n)))

(define (A008683 n) ;; Moebius mu
    (cond ((= 1 n) n)
          ((not (= (A001221 n) (A001222 n))) 0) ;; Well, contains a square!
          (else (expt -1 (A001222 n)))
    )
)


(define (A005408 n) (+ 1 (* 2 n))) ;; Odd numbers.
(define (A000290 n) (* n n)) ;; The squares,
(define (A000037 n) (+ n (floor->exact (+ (/ 1 2) (sqrt n))))) ;; Non-sq. off=1


(definec (A005117 n) ;; Square-free-numbers
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A005117 (- n 1)))))
         (if (not (zero? (A008683 i))) i (loop (+ 1 i)))
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A040892
;; Sequence:  30,2,1,1,1,2,60,2,1,1,1,2,60,2,1,1,1,2,60,2,1,1,1,2,60,2,1,
;;            1,1,2,60,2,1,1,1,2,60,2,1,1,1,2,60,2,1,1,1,2,60,2,1,1,1,2,
;;            60,2,1,1,1,2,60,2,1,1,1,2,60,2,1,1,1,2,60,2,1,1,1,2,60,2,1,
;;            1,1,2,60,2,1
;; Name:      Continued fraction for sqrt(923).

(definec (confrac923remainders n)
   (if (zero? n)
       (sqrt 923)
       (/ 1 (- (confrac923remainders (- n 1))  (confrac923 (- n 1))))
   )
)

(define (confrac923 n) (floor->exact (confrac923remainders n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;; Prime & Jacobi-symbol related functions follow.                         ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (gcd a b)
;;  (cond ((zero? b) a)
;;        (else (gcd b (modulo a b)))
;;  )
;; )


(load-option 'format)


;; Modified from prime:jacobi-symbol of module factor.scm in A. Jaffer's SLIB
;; (version slib3a1) and available at:
;;  http://swiss.csail.mit.edu/~jaffer/slib_toc


;;; Solovay-Strassen Prime Test
;;;   if n is prime, then J(a,n) is congruent mod n to a**((n-1)/2)

;;; (modulo p 16) is because we care only about the low order bits.
;;; The odd? tests are inline of (expt -1 ...)

;;; Here's the original:

(define (prime:jacobi-symbol p q)
  (cond ((zero? p) 0)
	((= 1 p) 1)
	((odd? p)
	 (if (odd? (quotient (* (- (modulo p 16) 1) (- q 1)) 4))
	     (- (prime:jacobi-symbol (modulo q p) p))
	     (prime:jacobi-symbol (modulo q p) p)))
	(else
	 (let ((qq (modulo q 16)))
	   (if (odd? (quotient (- (* qq qq) 1) 8))
	       (- (prime:jacobi-symbol (quotient p 2) q))
	       (prime:jacobi-symbol (quotient p 2) q))))))

;;; Here's my slightly faster fix-version, but still quite stupid:

(define (ofix:jacobi-symbol p q)
  (cond ((zero? p) 0)
	((= 1 p) 1)
	((odd? p)
	 (if (odd? (fix:lsh (* (- (fix:and p 15) 1) (- (fix:and q 15) 1)) -2))
	     (- (ofix:jacobi-symbol (modulo q p) p))
	     (ofix:jacobi-symbol (modulo q p) p)))
	(else
	 (let ((qq (fix:and q 15)))
	   (if (odd? (fix:lsh (- (* qq qq) 1) -3))
	       (- (ofix:jacobi-symbol (fix:lsh p -1) q))
	       (ofix:jacobi-symbol (fix:lsh p -1) q))))))


;; And here's the new, ultra-fast tail-recursive one, keeping
;; the sign-information in bit-1 of bit-mask s:
;; Project: give functions like this and Euclid's GCD similarly
;; implemented to a GA/GP-system and see what emerges...
;; E.g. what about using any other bits of s, its parity, or even
;; wilder: use Zeckendrof-expansion in masks, etc?
;; Or can we use the parallelity of bit-masks to compute
;; more than one jacobi-symbol at time?

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

(define jacobi-symbol fix:jacobi-symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Here are the 45 A-numbers you requested: 112044 --- 112088.
;; C00000 -> A11204
;; C00001 -> A11205
;; C00002 -> A11206
;; C00003 -> A11207
;; C00004 -> A11208

;; Characteristic function of squares:
(define (A010052 n) (if (= (floor->exact (sqrt n)) (sqrt n)) 1 0))
;; Characteristic function of non-squares: = A049240 ?
(definec (A049240 n) (- 1 (A010052 n)))

;; Partial sums of A049240: (n - number of squares before n)
(define (A028391 n) (- n (floor->exact (sqrt n))))
(definec (A028391v2 n) (if (< n 2) 0 (+ (A049240 n) (A028391 (- n 1)))))


;; Starting from offset 1 (i.e. the odd number 3), the first value i, for which (J n i) = 0.
;; I.e. A020639(A005408(n)) where A020639 is l.p.f.
(define (A090368 n) (jacobi-vectors-first-zero (A005408 n)))
;; Starting from offset 1 (i.e. the odd number 3), the first value i, for which (J n i) = 0
;; or -1.


;; Positions of primes among square-free numbers A000037:
(define (A112045 n) (A028391 (A000040 n)))

;; Are these always primes? Why? (It's easy to see why those in above (c.f. A090368) are).
(definec (A112046 n) (jacobi-vectors-first-non-one (A005408 n)))
;; One-based bisections:
(define (A112047 n) (A112046 (- (* 2 n) 1)))
(define (A112048 n) (A112046 (* 2 n)))


(define (A112049 n) (A049084 (A112046 n)))

;; The length of 111..111 prefix of the Jacobi-vector of odd numbers (2n+1):
(define (A112050 n) (- (A112046 n) 1))

;; The first index i (> a(n-1)), where A112046(i) (or A112049(i)) gets a value
;; distinct from any values A112046(1)..A112046(a(n-1))

;; Compute upto n=75:
(definec (A112051 n)
  (cond
    ((= 1 n) 1) ;; A112046(1) = 2
    (else
     (let ((b (A112051 (- n 1))))
       (let outloop ((i (+ b 1)))
        (let inloop ((k (A112046 i)) (j 1))
         (cond
           ((> j b) i)
           ((= (A112046 j) k) (outloop (+ 1 i)))
           (else (inloop k (+ 1 j)))
         )
        )
       )
     )     
    )
  )
)

;; From n>=4 onward seems to be squares of primes (A001248):
;; Column 1 of A112070 (row 1 of A112071)
(define (A112052 n) (A005408 (A112051 n)))

(define (A000040seems_to_be n) (A112046 (A112051 n)))
(define (A000027seems_to_be n) (A112049 (A112051 n)))

(define (A112053 n) (- (A112048 n) (A112047 n)))

;; Values of i where A112053(i) is not zero.
(definec (A112054 n)
  (cond
    ((= 0 n) 0) ;, Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112054 (- n 1)))))
         (cond
           ((not (zero? (A112053 i))) i)
           (else (loop (+ 1 i))) ;; Try the next one then.
         )
      )
    )
  )
)

;; The numerator of A112054(n)/6. All the terms of A112054 seem
;; to be divisible by 6. If that is true, then this is a complement of A112082:
(define (A112055 n) (/ (A112054 n) 6))


(define (A112056 n) (- (* 4 (A112054 n)) 1))
(define (A112057 n) (+ (* 4 (A112054 n)) 1))

(define (A112058 n) (* 4 (A112054 n)))

(define (A112059 n)
  (- (jacobi-vectors-first-non-one (A112057 n))
     (jacobi-vectors-first-non-one (A112056 n))
  )
)

(define (A112080 n)
  (- (jacobi-vectors-first-non-one (+ (* 24 n) 1))
     (jacobi-vectors-first-non-one (- (* 24 n) 1))
  )
)

(define (A112081 n) (/ (A112080 n) 2))

;; Values of i where A112080(i) is zero.
(definec (A112082 n) ;; Complement of A112055
  (cond
    ((= 0 n) 0) ;, Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112082 (- n 1)))))
         (cond
           ((zero? (A112080 i)) i)
           (else (loop (+ 1 i))) ;; Try the next one then.
         )
      )
    )
  )
)

;; Values of i where A112080(i) is not zero.
(definec (A112055v2 n)
  (cond
    ((= 0 n) 0) ;, Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112055v2 (- n 1)))))
         (cond
           ((not (zero? (A112080 i))) i)
           (else (loop (+ 1 i))) ;; Try the next one then.
         )
      )
    )
  )
)


(define (nth-int-with-A112049-k n k)
  (let loop ((i 1) (n n))
       (cond ((= k (A112049 i))
                 (if (= 1 n)
                     i
                     (loop (+ 1 i) (- n 1))
                 )
             )
             (else (loop (+ 1 i) n))
       )
  )
)

;; Here it is assumed that fun is cached, for
;; performance's sake! Otherwise this is absolutely
;; dumb.
(define (find-putative-period fun check-n-cycles)
       (let loop ((i 1) ;; The "master index" to fun
                  (n-same 0)
                  (perlen 1)
                  (n-cycles-to-check (- check-n-cycles 1))
                 )
         (cond ((not (= (fun i) (fun (+ i perlen))))
                  (loop 1 0 (+ 1 perlen) check-n-cycles) ;; Start from beginning.
               )
               ((= n-same perlen) ;; OK, here seems to be a cycle?
                   (if (zero? n-cycles-to-check)
                       perlen ;; OK, now we are assured, return the period length.
                       (loop (+ 1 i) 1 perlen (- n-cycles-to-check 1))
                   )
               )
               (else ;; (fun j) must be (fun (+ i perlen))
                  (loop (+ 1 i) (+ n-same 1) perlen n-cycles-to-check)
               )
         )
       )
)

(definec (A112060 n) (nth-int-with-A112049-k (1+ (A025581 (- n 1))) (1+ (A002262 (- n 1)))))
(definec (A112061 n) (nth-int-with-A112049-k (1+ (A002262 (- n 1))) (1+ (A025581 (- n 1)))))

(define (A112070 n) (A005408 (A112060 n)))
(define (A112071 n) (A005408 (A112061 n)))

;; Seems to be A042963: Congruent to 1 or 2 mod 4.
(definec (C0000XX n) ;; Positive integers i for which A112049(i) == 1
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (C0000XX (- n 1)))))
         (cond
           ((= 1 (A112049 i)) i)
           (else (loop (+ 1 i))) ;; Try the next one then.
         )
      )
    )
  )
)

(definec (A112062 n) ;; Positive integers i for which A112049(i) == 2
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112062 (- n 1)))))
         (if (= 2 (A112049 i)) i (loop (+ 1 i)))
      )
    )
  )
)

(definec (A112063 n) ;; Positive integers i for which A112049(i) == 3
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112063 (- n 1)))))
         (if (= 3 (A112049 i)) i (loop (+ 1 i)))
      )
    )
  )
)

(definec (A112064 n) ;; Positive integers i for which A112049(i) == 4
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112064 (- n 1)))))
         (if (= 4 (A112049 i)) i (loop (+ 1 i)))
      )
    )
  )
)

(definec (A112065 n) ;; Positive integers i for which A112049(i) == 5
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112065 (- n 1)))))
         (if (= 5 (A112049 i)) i (loop (+ 1 i)))
      )
    )
  )
)

(definec (A112066 n) ;; Positive integers i for which A112049(i) == 6
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112066 (- n 1)))))
         (if (= 6 (A112049 i)) i (loop (+ 1 i)))
      )
    )
  )
)

(definec (A112067 n) ;; Positive integers i for which A112049(i) == 7
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112067 (- n 1)))))
         (if (= 7 (A112049 i)) i (loop (+ 1 i)))
      )
    )
  )
)

(definec (A112068 n) ;; Positive integers i for which A112049(i) == 8
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112068 (- n 1)))))
         (if (= 8 (A112049 i)) i (loop (+ 1 i)))
      )
    )
  )
)

(definec (A112069 n) ;; Positive integers i for which A112049(i) == 9
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112069 (- n 1)))))
         (if (= 9 (A112049 i)) i (loop (+ 1 i)))
      )
    )
  )
)


;; A005408(C0000XX(n)) seems to be A047621 (= duplicate of A066587 ?), congruent to 3 or 5 mod 8.

(define (A112072 n) (A005408 (A112062 n)))
(define (A112073 n) (A005408 (A112063 n)))
(define (A112074 n) (A005408 (A112064 n)))
(define (A112075 n) (A005408 (A112065 n)))
(define (A112076 n) (A005408 (A112066 n)))
(define (A112077 n) (A005408 (A112067 n)))
(define (A112078 n) (A005408 (A112068 n)))
(define (A112079 n) (A005408 (A112069 n)))

;; Here are the 10 A-numbers you requested: 112132 --- 112141.

;; The first differences of these sequences seem to be periodic,
;; (as expected if they are all formed as the intersections of
;; various n={a,b,...} modulo m sequences), and their periods
;; seem to grow as 2,4,6,16,72,420,3240,... (not in OEIS, and neither is 1,2,3,8,36).

(define (A112132 n) (- (A112062 (+ n 1)) (A112062 n)))
(define (A112133 n) (- (A112063 (+ n 1)) (A112063 n)))
(define (A112134 n) (- (A112064 (+ n 1)) (A112064 n)))
(define (A112135 n) (- (A112065 (+ n 1)) (A112065 n)))
(define (A112136 n) (- (A112066 (+ n 1)) (A112066 n)))
(define (A112137 n) (- (A112067 (+ n 1)) (A112067 n)))
(define (A112138 n) (- (A112068 (+ n 1)) (A112068 n)))
(define (A112139 n) (- (A112069 (+ n 1)) (A112069 n)))


(definec (A112083org n) (nth-int-with-A112049-k 2 n))
(define (A112084org n) (A005408 (A112083 n)))

;; These were computed with above functions with the
;; compiled code. For n=22 A112083 will fail, as then
;; will appear the first value larger than 2^25 - 1 (= 33554431),
;; and thus the fixnum-routines of MIT Scheme used in fix:jacobi-symbol
;; will fail.

(define seqA112083 (list 2 4 12 35 155 239 779 2855 5279 9095 15695 59135
     350699 183395 1352339 1477295 1077959 6922920 3038555 12705840 14199120)
)

(define seqA112084 (list 5 9 25 71 311 479 1559 5711 10559 18191 31391 118271
     701399 366791 2704679 2954591 2155919 13845841 6077111 25411681 28398241)
)

(define (A112083 n) (list-ref seqA112083 (-1+ n))) ;; One-based sequence.
(define (A112084 n) (list-ref seqA112084 (-1+ n))) ;; One-based sequence.

;; Note the non-monotone drop from A112084(13)=701399 to A112084(14)=366791
;; and the similarity with A045535 (of which A062241 is duplicate ?):

;; 7,23,71,311,479,1559,5711,10559,18191,31391,118271,366791,366791,
;; 2155919,2155919,2155919,6077111,6077111,98538359,120293879,
;; 131486759,131486759,508095719,2570169839

;; Is this periodic or not?
(define (A112085 n) (- (A112055 (+ n 1)) (A112055 n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (nth-odd-with-jacobi-A005117-k+1-equal-plus1 n k)
  (let loop ((i 1) (n n))
       (cond ((= (jacobi-symbol (A005117 (+ 1 k)) i) +1)
                 (if (= 1 n)
                     i
                     (loop (+ 2 i) (- n 1))
                 )
             )
             (else (loop (+ 2 i) n))
       )
  )
)

(define (nth-odd-with-jacobi-A005117-k+1-equal-minus1 n k)
  (let loop ((i 1) (n n))
       (cond ((= (jacobi-symbol (A005117 (+ 1 k)) i) -1)
                 (if (= 1 n)
                     i
                     (loop (+ 2 i) (- n 1))
                 )
             )
             (else (loop (+ 2 i) n))
       )
  )
)

(definec (A112140 n) (nth-odd-with-jacobi-A005117-k+1-equal-plus1
                             (1+ (A025581 (- n 1))) (1+ (A002262 (- n 1))))
)

;; Transpose of A112140:
(definec (A112187 n) (nth-odd-with-jacobi-A005117-k+1-equal-plus1
                             (1+ (A002262 (- n 1))) (1+ (A025581 (- n 1))))
)


(definec (A112141 n) (nth-odd-with-jacobi-A005117-k+1-equal-minus1
                             (1+ (A025581 (- n 1))) (1+ (A002262 (- n 1))))
)

;; Transpose of A112141:
(definec (A112188 n) (nth-odd-with-jacobi-A005117-k+1-equal-minus1
                             (1+ (A002262 (- n 1))) (1+ (A025581 (- n 1))))
)


(definec (A047522 n) ;; Odd integers i >= 1 for which J(A000037(1),i) = J(2,i) == +1
  (cond
    ((= 0 n) 1) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 2 (A047522 (- n 1)))))
         (if (= (jacobi-symbol (A005117 2) i) +1) i (loop (+ 2 i)))
      )
    )
  )
)


(definec (A047621 n) ;; Odd integers i >= 1 for which J(A000037(1),i) = J(2,i) == -1
  (cond
    ((= 0 n) 1) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 2 (A047621 (- n 1)))))
         (if (= (jacobi-symbol (A005117 2) i) -1) i (loop (+ 2 i)))
      )
    )
  )
)


(definec (A091998 n) ;; Odd integers i >= 1 for which J(A000037(2),i) = J(3,i) == +1, i.e. 12n+-1
  (cond
    ((= 0 n) 1) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 2 (A091998 (- n 1)))))
         (if (= (jacobi-symbol (A005117 3) i) +1) i (loop (+ 2 i)))
      )
    )
  )
)


(definec (A092242 n) ;; Odd integers i >= 1 for which J(A000037(2),i) = J(3,i) == -1, 12n +5 +7
  (cond
    ((= 0 n) 1) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 2 (A092242 (- n 1)))))
         (if (= (jacobi-symbol (A005117 3) i) -1) i (loop (+ 2 i)))
      )
    )
  )
)


(definec (A090771 n) ;; Odd integers i >= 1 for which J(A000037(3),i) = J(5,i) == +1, 10n +- 1 (?)
  (cond
    ((= 0 n) 1) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 2 (A090771 (- n 1)))))
         (if (= (jacobi-symbol (A005117 4) i) +1) i (loop (+ 2 i)))
      )
    )
  )
)

;; Here are the 8 A-numbers you requested: 112305 --- 112312.

;; Both A063226 and A063239 consist of the first 50 terms given by this: 3 - 247
;; How do they differ?
(definec (A112306 n) ;; Odd integers i >= 1 for which J(A000037(3),i) = J(5,i) == -1, 10n +- 3 (?)
  (cond
    ((= 0 n) 3) ;; Starting point. Compute from n=0 onward.
    (else
      (let loop ((i (+ 2 (A112306 (- n 1)))))
         (if (= (jacobi-symbol (A005117 4) i) -1) i (loop (+ 2 i)))
      )
    )
  )
)

;; Seems to be A074229, {1,5,19,23} mod 24.
(definec (A074229 n) ;; Odd integers i >= 1 for which J(A000037(4),i) = J(6,i) == +1,
  (cond
    ((= 0 n) 1) ;; Starting point. Compute from n=0 onward.
    (else
      (let loop ((i (+ 2 (A074229 (- n 1)))))
         (if (= (jacobi-symbol (A005117 5) i) +1) i (loop (+ 2 i)))
      )
    )
  )
)

;; Not in OEIS, {7,11,13,17} mod 24.
(definec (A112307 n) ;; Odd integers i >= 1 for which J(A000037(4),i) = J(6,i) == -1,
  (cond
    ((= 0 n) 7) ;; Starting point. Compute from n=0 onward.
    (else
      (let loop ((i (+ 2 (A112307 (- n 1)))))
         (if (= (jacobi-symbol (A005117 5) i) -1) i (loop (+ 2 i)))
      )
    )
  )
)

;;       3   9  19  25  27
;;  29  31  37  47  53  55
;;  57  59  65  75  81  83
;;  85  87  93 103 109 111
;; 113 115 121 131 137 139
;; 141 143 149 159 165 167 ...

;; Not in OEIS, {1,3,9,19,25,27} mod 28.

(definec (A112308 n) ;; Odd integers i >= 1 for which J(A000037(5),i) = J(7,i) == +1,
  (cond
    ((= 0 n) 1) ;; Starting point. Compute from n=0 onward.
    (else
      (let loop ((i (+ 2 (A112308 (- n 1)))))
         (if (= (jacobi-symbol (A005117 6) i) +1) i (loop (+ 2 i)))
      )
    )
  )
)

;;   5  11  13  15  17  23
;;  33  39  41  43  45  51
;;  61  67  69  71  73  79
;;  89  95  97  99 101 107
;; 117 123 125 127 129 135
;; 145 151 153 155 157 163

;; Not in OEIS {5,11,13,15,17,23} mod 28.

(definec (A112309 n) ;; Odd integers i >= 1 for which J(A000037(5),i) = J(7,i) == -1,
  (cond
    ((= 0 n) 5) ;; Starting point. Compute from n=0 onward.
    (else
      (let loop ((i (+ 2 (A112309 (- n 1)))))
         (if (= (jacobi-symbol (A005117 6) i) -1) i (loop (+ 2 i)))
      )
    )
  )
)


(definec (A047522copy n) ;; Odd integers i >= 1 for which J(A000037(6),i) = J(8,i) == +1,
  (cond
    ((= 0 n) 1) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 2 (A047522copy (- n 1)))))
         (if (= (jacobi-symbol (A000037 6) i) +1) i (loop (+ 2 i)))
      )
    )
  )
)

(definec (A047621copy n) ;; Odd integers i >= 1 for which J(A000037(6),i) = J(8,i) == -1,
  (cond
    ((= 0 n) 1) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 2 (A047621copy (- n 1)))))
         (if (= (jacobi-symbol (A000037 6) i) -1) i (loop (+ 2 i)))
      )
    )
  )
)

;; Seems to be {1,3,9,13,27,31,37,39} mod 40.
;;       3   9  13  27  31  37  39
;;  41  43  49  53  67  71  77  79
;;  81  83  89  93 107 111 117 119
;; 121 123 129 133 147 151 157 159

(definec (A112310 n) ;; Odd integers i >= 1 for which J(A005117(7),i) = J(10,i) == +1,
  (cond
    ((= 0 n) 1) ;; Starting point. Compute from n=0 onward.
    (else
      (let loop ((i (+ 2 (A112310 (- n 1)))))
         (if (= (jacobi-symbol (A005117 7) i) +1) i (loop (+ 2 i)))
      )
    )
  )
)

;; Seems to be {7,11,17,19,21,23,29,33} mod 40.
;;   7  11  17  19  21  23  29  33
;;  47  51  57  59  61  63  69  73
;;  87  91  97  99 101 103 109 113
;; 127 131 137 139 141 143 149 153
;; 167 171 177 179 181 183 189 193

(definec (A112311 n) ;; Odd integers i >= 1 for which J(A005117(7),i) = J(10,i) == -1,
  (cond
    ((= 0 n) 7) ;; Starting point. Compute from n=0 onward.
    (else
      (let loop ((i (+ 2 (A112311 (- n 1)))))
         (if (= (jacobi-symbol (A005117 7) i) -1) i (loop (+ 2 i)))
      )
    )
  )
)


(define (A112305 n) (nth-odd-with-jacobi-a005117-k+1-equal-plus1 2 n))
(define (A112312 n) (nth-odd-with-jacobi-a005117-k+1-equal-minus1 1 n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;; Some zeckendorf-expansion related functions follow.                     ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define *Sqrt5* (sqrt 5))
(define *Phi* (/ (1+ *Sqrt5*) 2))
(define *LogPhi* (log *Phi*))


(definec (fibo nakki) ;; I.e., A000045
   (if (< nakki 2)
       nakki
       (+ (fibo (-1+ nakki)) (fibo (- nakki 2)))
   )
)

(define A000045 fibo)

;; Sum of all 1-fibits in Zeckendorf-expansion A014417(i) summed for
;; all integers i in range [Fib(n+1),Fib(n+2)[
(definec (A010049 n)
   (if (<= n 1)
       n
       (+ (A010049 (- n 1)) (A010049 (- n 2)) (A000045 (- n 2)))
   )
)


(define (A072648 n) ;; An approximate "inverse" of A000045 (of the fibonacci numbers)
   (cond ((zero? n) n)
         (else (floor->exact (/ (log (* n *Sqrt5*)) *LogPhi*)))
   )
)

;; 1,2,3,4,5,6,7,8,9,A,B,C,D,E,F,         ,21
;; 1,2,3,3,4,4,4,5,5,5,5,5,6,6,6,6,6,6,6,6,7,...
(define (A072649 n) ;; n occurs fibo(n) times.
  (let ((b (A072648 n)))
     (+ -1 b (floor->exact (/ n (fibo (1+ b)))))
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


(define (A000120 n) ;; = A029837(n+1)
  (let loop ((n n) (i 0))
     (if (zero? n)
         i
         (loop (floor->exact (/ n 2)) (+ i (modulo n 2)))
     )
  )
)

(define (A010060 n) (modulo (A000120 n) 2))
(define (A010059 n) (- 1 (A010060 n)))

(define (A007895 n)  (A000120 (A003714 n))) ;; Number of 1-fibits in Z.E.

(define (A095076 n)  (A010060 (A003714 n))) ;; Parity of 1-fibits in Z.E.
(define (A095111 n)   (- 1 (A095076 n))) ;; Its complement.

(definec (A095276 n) ;; One-based.
  (cond
    ((= 1 n) 1) ;; The first run is of length 1.
    (else
      (let loop ((i (A095279 (- n 1))) ;; No need to add one as A095076's off=0
                 (this-run 1))
         (cond
           ((not (= (A095076 i) (A095076 (+ 1 i)))) this-run)
           (else (loop (+ 1 i) (+ 1 this-run)))
         )
      )
    )
  )
)


(definec (A095279 n) ;; Partials sums of A095276.
  (if (zero? n) 0 (+ (A095279 (- n 1)) (A095276 n)))
)


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
    ((= 1 n) 3) ;; 3 is the first member. it's z.e. being 100.
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



;; Binary encoding of quadratic residue set for each prime.
;; A080146(n) = A055094(A000040(n)).
(define (A080146 n)
 (let ((p (A000040 n)))
   (let loop ((s 0)
              (i 1)
             )
      (cond ((= i p) s)
            (else (loop (+ (* 2 s) (/ (+ 1 (jacobi-symbol i p)) 2))
                        (+ i 1)
                  )
            )
      )
   )
 )
)


(define (jacobi-vectors-first-zero n)
    (let loop ((i 1))
         (cond ((zero? (jacobi-symbol i n)) i)
               (else (loop (+ i 1)))
         )
    )
)

;; Note this loops when the argument is an odd square like 9:
(define (jacobi-vectors-first-minus-one n)
    (let loop ((i 1))
         (cond ((= -1 (jacobi-symbol i n)) i)
               (else (loop (+ i 1)))
         )
    )
)

(define (jacobi-vectors-first-non-one n)
    (let loop ((i 1))
         (cond ((not (= 1 (jacobi-symbol i n))) i)
               (else (loop (+ i 1)))
         )
    )
)

;; How many times jacobi symbols sum to zero from 1 to n-1 ?
(define (jacobi-sums-zeros n)
    (let loop ((i 2) (s 1) (zeros 0))
         (cond ((>= i n) (+ zeros (if (zero? s) 1 0)))
               (else (loop (+ i 1)
                           (+ s (jacobi-symbol i n))
                           (+ zeros (if (zero? s) 1 0))
                     )
               )
         )
    )
)

(define (jacobi-vector-begins-as? p k)
    (let loop ((i (binwidth k)) (k k))
         (cond ((>= i p) #f) ;; If k has more bits than p-1.
               ((zero? i) #t)
               ((not (= (modulo k 2) (/ (+ 1 (jacobi-symbol i p)) 2))) #f)
               (else (loop (- i 1) (floor->exact (/ k 2))))
         )
    )
)

(define (sum-of-jacobi-symbols n)
    (add (lambda (i) (jacobi-symbol i n)) 1 (- n 1))
)


(definec (sum-of-jacobi-symbols-halfway n)
    (add (lambda (i) (jacobi-symbol i n)) 1 (/ (- n 1) 2))
)


(define (all-partial-sums-of-jacobi-symbols-non-negative-upto-k? n k)
    (let loop ((i 1) (s 1)) ;; Note that J(1,n) = 1 always.
         (cond ((fix:< s 0) #f)
               ((fix:= i k)
;; This ending test added to make us more certain about the correctness
;; of our implementation of jacobi-symbol. Of course it fails on squares:
                  (if (and (fix:= (fix:1+ k) n) (not (fix:zero? s)))
                      (error
                         (format #f
                                 "Sum of J(1,~a)..J(n-1,~a) is not zero: ~a"
                                 n n s
                         )
                      )
                      #t ;; Otherwise OK, return true.
                  )
               )
               (else
                 (let ((i+1 (fix:1+ i)))
                   (loop i+1 (fix:+ s (fix:jacobi-symbol i+1 n)))
                 )
               )
         )
    )
)

;; A080110 is the characteristic function of A080112.
(define (A080110 n)
  (let ((p (A000040 n)))
    (if (all-partial-sums-of-jacobi-symbols-non-negative-upto-k? p
                                                                 (/ (- p 1) 2)
        )
        1
        0
    )
  )
)

(definec (A080112 n)
  (cond
    ((= 1 n) 2)
    (else
      (let loop ((i (+ 1 (A080112 (- n 1)))))
         (cond ((not (zero? (A080110 i))) i)
               (else (loop (+ 1 i)))
         )
      )
    )
  )
)

(define (A080114 n) (A000040 (A080112 n)))



;; B000001 is the characteristic function of B000002.
(define (B000001 n)
  (let ((p (A000040 n)))
    (if (all-partial-sums-of-jacobi-symbols-non-negative-upto-k? p (- p 1))
        1
        0
    )
  )
)

(definec (B000002 n)
  (cond
    ((= 1 n) 2) ;; 3 is the 2nd prime.
    (else
      (let loop ((i (+ 1 (B000002 (- n 1)))))
         (cond ((not (zero? (B000001 i))) i)
               (else (loop (+ 1 i)))
         )
      )
    )
  )
)

(define (B000003 n) (A000040 (B000002 n)))

;; Because B000003 contains only primes of the form 4k+3, we can
;; shift them two bits right:
(define (B000004 n) (/ (- (B000003 n) 3) 4))

;; A subset of A080118, intersect of A080146 & A014486.
(define (B000005 n) (A080146 (B000002 n)))

;; A014486-indices of them.
(define (B000006 n) (A080300 (B000005 n)))


(define (nth-prime-with-jacobi-prefix-k n k)
  (let loop ((i 1) (n n))
       (cond ((jacobi-vector-begins-as? (A000040 i) k)
                 (if (= 1 n)
                     i
                     (loop (+ 1 i) (- n 1))
                 )
             )
             (else (loop (+ 1 i) n))
       )
  )
)

(define (A003629 n) (A000040 (nth-prime-with-jacobi-prefix-k n 2)))
(define (A001132 n) (A000040 (nth-prime-with-jacobi-prefix-k n 3)))

(define (B000007 n) (A000040 (nth-prime-with-jacobi-prefix-k n 4)))
(define (B000008 n) (A000040 (nth-prime-with-jacobi-prefix-k n 5)))
(define (B000009 n) (A000040 (nth-prime-with-jacobi-prefix-k n 6)))
(define (B000010 n) (A000040 (nth-prime-with-jacobi-prefix-k n 7)))


(define (B000011 n) (sum-of-jacobi-symbols (A000290 (A005408 n))))
;; Should be easy to compute.

(define (B000012 n) (/ (B000011 n) 2))


(definec (A088828 n) ;; Odd non-squares.
  (cond
    ((= 1 n) 3) ;; 3 is the 1st odd non-square number.
    (else
      (let loop ((i (+ 2 (A088828 (- n 1)))))
         (cond ((not (integer? (sqrt i))) i)
               (else (loop (+ 2 i)))
         )
      )
    )
  )
)

;; ID Number: A088828
;; URL:       http://www.research.att.com/projects/OEIS?Anum=A088828
;; Sequence:  3,5,7,11,13,15,17,19,21,23,27,29,31,33,35,37,39,41,43,45,47,
;;            51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,83,85,87,89,91,
;;            93,95,97,99,101,103,105,107,109,111,113,115,117,119,123,125,
;;            127,129,131,133,135,137,139
;; Name:      Odd numbers with even abundance: primes and some composites too.
;; Comments:  Odd numbers with odd abundance are in A016754. (odd squares)

;; Example:   n=p prime, abundance=1-p=even and negative;
;;            n=21, sigma=1+3+7+21=32,abundance=32-42=-20;
;; Math'ca:   Do[s=DivisorSigma[1,n]-2*n; If[
;;               !OddQ[s]&&OddQ[n],Print[{n,s}]],{n,1,1000}]
;; Offset:    1
;; Author(s): Labos E. (labos(AT)ana1.sote.hu), Oct 28 2003
;;


(definec (A095100 n)
  (cond
    ((= 1 n) 3) ;; 3 is the 1st 4k+3 number with a valid Motzkin path.
    (else
      (let loop ((i (+ 4 (A095100 (- n 1)))))
         (cond
           ((all-partial-sums-of-jacobi-symbols-non-negative-upto-k?
                                                         i (/ (- i 1) 2)
            )
             i ;; We have found the next i which matches the criteria.
           )
           (else (loop (+ 4 i))) ;; Try number which is four larger.
         )
      )
    )
  )
)


(definec (A095101 n)
  (cond
    ((= 1 n) 19) ;; 19 is the 1st 4k+3 number with a diving path.
    (else
      (let loop ((i (+ 4 (A095101 (- n 1)))))
         (cond
           ((not (all-partial-sums-of-jacobi-symbols-non-negative-upto-k?
                                                         i (/ (- i 1) 2)
                 )
            )
             i ;; We have found the next i which matches the criteria.
           )
           (else (loop (+ 4 i))) ;; Try number which is four larger.
         )
      )
    )
  )
)


(definec (A002144 n)
  (cond
    ((= 1 n) 5) ;; 1 is the 1st prime of the form 4n+1.
    (else
      (let loop ((i (+ 4 (A002144 (- n 1)))))
         (cond
           ((= 1 (length (factor i))) i)
           (else (loop (+ 4 i))) ;; Try number which is four larger.
         )
      )
    )
  )
)

(definec (A002145 n)
  (cond
    ((= 1 n) 3) ;; 3 is the 1st prime of the form 4n+3.
    (else
      (let loop ((i (+ 4 (A002145 (- n 1)))))
         (cond
           ((= 1 (length (factor i))) i)
           (else (loop (+ 4 i))) ;; Try number which is four larger.
         )
      )
    )
  )
)



(definec (A091113 n)
  (cond
    ((= 1 n) 9) ;; 9 is the 1st composite of the form 4n+1.
    (else
      (let loop ((i (+ 4 (A091113 (- n 1)))))
         (cond
           ((> (length (factor i)) 1) i)
           (else (loop (+ 4 i))) ;; Try number which is four larger.
         )
      )
    )
  )
)

(definec (A091236 n)
  (cond
    ((= 1 n) 15) ;; 15 is the 1st composite of the form 4n+3.
    (else
      (let loop ((i (+ 4 (A091236 (- n 1)))))
         (cond
           ((> (length (factor i)) 1) i)
           (else (loop (+ 4 i))) ;; Try number which is four larger.
         )
      )
    )
  )
)

;; 1,3,4,7,9,10,13,15,18,22,24,25,27,28,34,37,39,43,45,48,49,
;; Numbers n such that 4n+1 is prime.
(define (A005098 n) (/ (- (A002144 n) 1) 4))
(define (A045751 n) (if (zero? n) n (/ (- (A091113 n) 1) 4))) ;; With off=0.

;; Numbers n such that 4n+3 is prime.
(define (A095278 n) (/ (- (A002145 n) 3) 4))
;; Numbers n such that 4n+3 is composite.
(define (A095277 n) (/ (- (A091236 n) 3) 4))



;; A095100 is for all odd numbers of form 4k+3 whose
;; jacobi-vector = valid Motzkin path.
;; and A095101 is for its complement (among 4k+3 integers).
;; (compute also sum of their diving indices,
;; and its average for each ]2^n,2^(n+1)] range. Moments, etc.)
;;
;; A095090 and A095091 are for the respective counts.
;;



;; All odd, non-square integers 2n+1 where all partial sums of Jacobi-symbols
;; from 1 to n are non-negative, and the difference from A095100.
;; (all of the form 4k+1)

;; Compute also the complement (in odd non-squares),
;; (relative abundances in ranges 2^n,2^(n+1)
;; (with respect to all 4k+3 integers, which necessarily are non-squares, etc.)
;; and intersect with A001358 (semiprimes)

;; Because A095100 contains only numbers of the form 4k+3, we can
;; shift them two bits right:
(define (B000024 n) (/ (- (A095100 n) 3) 4))


;; How many such integers in each 2^n - 2^(n+1) range?
;; Begins as (the first nineteen terms):
(define seqA095090 (list 1 1 2 3 6 9 17 33 60 108 202 360 703 1328 2519 4779 9103 17501 33473))

(define (A095090 n) (list-ref seqA095090 (- n 1)))

;; All such integers are of the form 4k+3, with complementarily
;; symmetric Jacobi/Legendre vectors, thus it is enough to check
;; only up to midway that all the partial sums are positive.
(definec (A095090org n)
  (if (< n 2)
      n  ;; 0 in range [1,2], 1 in range [2,4]
      (let loop ((i (+ 3 (expt 2 n)))
                 (s 0)
                 (stop-at (expt 2 (+ 1 n)))
                )
        (cond
          ((> i stop-at) s)
          ((all-partial-sums-of-jacobi-symbols-non-negative-upto-k?
                                           i (/ (- i 1) 2)
           )
                (loop (+ i 4) (+ s 1) stop-at)
          )
          (else (loop (+ i 4) s stop-at))
        )
      )
  )
)


;; All such integers are of the form 4k+3, with complementarily
;; symmetric Jacobi/Legendre vectors, thus it is enough to check
;; only up to midway that all the partial sums are positive.
;; This definition is just for checking:
(definec (A095091org n)
  (if (< n 2)
      0  ;; 0 in ranges [1,2], [2,4], [4,8], [8,16], 1 (19) in range [16,32]
      (let loop ((i (+ 3 (expt 2 n)))
                 (s 0)
                 (stop-at (expt 2 (+ 1 n)))
                )
        (cond
          ((> i stop-at) s)
          ((not (all-partial-sums-of-jacobi-symbols-non-negative-upto-k?
                                          i (/ (- i 1) 2)
                )
           )
                (loop (+ i 4) (+ s 1) stop-at)
          )
          (else (loop (+ i 4) s stop-at))
        )
      )
  )
)


;; Of course it is easier to compute as:
(define (A095091 n) (if (< n 3) 0 (- (expt 2 (- n 2)) (A095090 n))))

;; Call e.g. as (compute-foo-upto-n "C:\\karttu\\Schemuli\\A095090.t24" A095090 24)
(define (compute-foo-upto-n filename fun upto-n)
   (call-with-output-file filename
     (lambda (out)
        (let loop ((i 1)
                   (zs (list))
                  )
             (cond ((<= i upto-n)
                      (let ((z (fun i)))
                        (format out "zs=~a (fun ~a)=~a\n" zs i z)
                        (flush-output out)
                        (loop (+ 1 i) (append zs (list z)))
                      )
                   )
             )
        )
     )
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive roots.                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Not checked yet how this works with composite n. Might return spurious
;; results.

(define (primitive-root-of? pr n)
  (let* ((ord (A000010 n))
         (ord_per_2 (/ ord 2))
        )
    (let loop ((i (modulo (* pr pr) n))
               (steps 1)
              )
         (cond ((zero? i) #f) ;; n is composite and gcd(pr,n) is not 1.
               ((= i pr) (= steps ord)) ;; Came whole cycle, is it maximal?
;;             ((> steps ord_per_2) #t) ;; Optimization, we know it's full cycle. Only for primes.
               (else (loop (modulo (* pr i) n) (+ 1 steps)))
         )
    )
  )
)

;; Note that 1 is one and only primitive root of 2.
(define (smallest-primitive-root n)
    (let loop ((i 1))
       (cond ((= i n) 0) ;; Not found.
             ((primitive-root-of? i n) i)
             (else (loop (+ i 1)))
       )
    )
)

;; Comment to add: for primes of the form p = 2*q + 1 (safe primes, A005385, all 4k+3)
;; this is always prime, because primitive roots cannot be quadratic
;; residues, and with primes of the form 2q+1, there are q
;; primitive roots (neither 1 nor p-1 can be a primitive root of p),
;; thus there are 2*q - q = q positions for primitive roots
;; in range [2,2*q-1], ... etc. blaa, blaa, blaa. XXX
;;  C.f. A112046.

(define A001918 (compose-funs smallest-primitive-root A000040))


;; See A046147 and A063988.
(define (all-primitive-roots n)
   (let ((pr (smallest-primitive-root n))
         (ord (A000010 n))
        )
     (let loop ((roots (list))
                (i 1)
                (prnow pr)
               )
         (cond ((> i ord) (sort roots <))
               (else
                  (loop (if (= 1 (gcd i ord))
                            (cons prnow roots)
                            roots
                        )
                        (+ 1 i)
                        (modulo (* pr prnow) n)
                  )
               )
         )
     )
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here row >= 0, and 0 <= pos-on-row <= row.
(define (increment-triangle-vec! vec row pos-on-row)
   (let* ((vec-ind (+ (A000217 row) pos-on-row))
          (new-count (+ 1 (vector-ref vec vec-ind))))
      (vector-set! vec vec-ind new-count)
;;    (format #t "vec=~a row=~a pos-on-row=~a vec-ind=~a\n"
;;                vec row pos-on-row vec-ind)
      new-count
   )
)

;; Here both row starts from 0. col is kept in subvec pos 0.
;; Nothing is done if either of them is equivalent of greater than upto-n.
(define (append-to-array-vecs vec-of-vecs upto-n row item)
  (if (< row upto-n)
      (let* ((vec (vector-ref vec-of-vecs row))
             (col (+ 1 (vector-ref vec 0)))) ;; Index to the last is kept in 0.
         (cond ((<= col upto-n)
                  (vector-set! vec col item)
                  (vector-set! vec 0 col)
               )
         )
      )
  )
)

(define (make-vec-of-vecs size init-elem)
  (let ((vec-of-vecs (make-vector size)))
     (let loop ((i 0))
       (cond ((= i size) vec-of-vecs)
             (else (vector-set! vec-of-vecs i
                                (make-vector (+ 1 size) init-elem)
                   )
                   (loop (+ 1 i))
             )
       )
     )
  )
)


(define (collect-from-array-vecs vec-of-vecs upto-n)
  (let loop ((row 1) (col 1) (lista (list)))
       (cond ((> col upto-n) (reverse! lista))
             (else 
               (loop
                      (if (= 1 col) 1 (+ 1 row)) ;; Row grows unless col=1.
                      (if (= 1 col) (+ 1 row) (- col 1))
                                ;; Col is decremented unless it is 1.
                      (cons (vector-ref (vector-ref vec-of-vecs (- row 1)) col)
                            lista
                      )
               )
             )
       )
  )
)



(define (compute-prime-assymetricity-triangles-upto upto-n)
  (define vecA095759 (make-vector (A000217 upto-n) 0))
  (define vecsA095749 (make-vec-of-vecs upto-n 0))
  (let loop ((n 1)
             (p 2))
    (let ((w (- (binwidth p) 1))
          (ai (A037888 p))
         )
      (cond
       ((> w upto-n)
           (format #t "A095759=~a\n" vecA095759)
           (format #t "A095749=~a\n" vecsA095749)
           (list  (collect-from-array-vecs vecsA095749 upto-n)
                  (vector->list vecA095759)
           )
       )
       (else
;;         (format #t "Prime ~a, w=~a, ai=~a\n" p w ai)
           (if (> p 2) (increment-triangle-vec! vecA095759 (- w 1) ai))
           (append-to-array-vecs vecsA095749 upto-n ai p)
           (loop (+ 1 n) (A000040 (+ 1 n)))
      )
     )
    )
  )
)

;; Pre-computed:

(define seqA095749
 (list 3 5 2 7 11 43 17 13 53 151 31 19 71 179 599 73 23 79 233 683
       2111 107 29 83 241 739 2143 8543 127 37 101 271 797 2503 9103
       33023 257 41 109 311 853 2731 9623 33151 131839 313 47 113 331
       937 3011 10427 33599 135647 531071 443 59 139 347 977 3413 10651 34591
       139759 537919 2102783 1193 61 149 397 1087 3673 10859 38231 144719
       539711 2108159 8401919))

(define seqA095759
  (list 1 2 0 0 2 0 2 3 0 0 0 5 2 0 0 3 4 6 0 0 0 0 15 4 4 0 0 0 3 18
    15 7 0 0 0 0 0 32 20 16 7 0 0 0 0 7 33 63 24 10 0 0 0 0 0 0 63 62 88
    33 9 0 0 0 0 0 12 81 135 154 56 26 0 0 0 0 0 0 0 119 150 314 197 72 20
    0 0 0 0 0 0 23 144 398 479 430 95 43 0 0 0 0 0 0 0 0 256 347 959 773
    519 149 27 0 0 0 0 0 0 0 40 318 913 1568 1482 1095 219 74 0 0 0 0 0 0
    0 0 0 527 895 2620 2737 2421 1186 322 41 0 0 0 0 0 0 0 0 94 640 2196
    4394 5769 4254 2394 537 112 0 0 0 0 0 0 0 0 0 0 1029 1907 7110 8912
    9969 6240 2720 655 93 0 0 0 0 0 0 0 0 0 142 1281 5151 11987 17912
    18847 11335 5752 998 181 0 0 0 0 0 0 0 0 0 0 0 2236 4483 18434 26885
    35861 28315 16476 6170 1309 167 0 0 0 0 0 0 0 0 0 0 271 2566 11932
    31444 55407 65001 56330 29588 13323 1859 495 0 0 0 0 0 0 0 0 0 0 0 0
    4273 10033 46474 77839 119488 114235 83475 41068 13765 2784 274 0 0 0
    0 0 0 0 0 0 0 0))

(define (A095749 n) (list-ref seqA095749 (- n 1))) ;; One-based.
(define (A095759 n) (list-ref seqA095759 n))       ;; Zero-based.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Blum numbers: of form P*Q where P&Q are distinct primes of form 4k+3.
(definec (A016105 n)
  (cond
    ((= 1 n) 21) ;; 21 is the 1st Blum integer.
    (else
      (let loop ((i (+ 4 (A016105 (- n 1)))))
         (let ((factors (factor i)))
            (cond ((and (= 2 (length factors))
                        (= 3 (modulo (car factors) 4))
                        (not (= (car factors) (cadr factors))) ;; no squares!
                   ) ;; If the other factor is 3 mod 4, then the other is also.
                   i
                  )
                  (else (loop (+ 4 i))) ;; All are of the form 4k+1.
            )
         )
      )
    )
  )
)


;; Because A016105 contains only numbers of the form 4k+1, we can
;; shift them two bits right:
(define (B000031 n) (/ (- (A016105 n) 1) 4))

(define (A020639 n) (car (sort (factor n) <))) ;; Least prime dividing n.
(define (A006530 n) (car (sort (factor n) >))) ;; Largest prime dividing n.

(definec (B000032 n) (A020639 (A016105 n))) ;; Smaller factor of the nth Blum
(definec (B000033 n) (A006530 (A016105 n))) ;; Larger factor of the nth Blum

;; Return as a list the quadratic residue set of n.
;; This is surely the most straightforward way to compute this.
(define (quadratic-residues-of n)
   (let loop ((i 1)
              (qurs (list))
             )
       (cond ((= i n) (remove-duplicates (sort qurs <)))
             ((zero? (modulo (* i i) n)) (loop (+ 1 i) qurs)) ;; No zeros!
             (else (loop (+ 1 i) (cons (modulo (* i i) n) qurs)))
       )
   )
)

(define (qr-list->cycles pl) ;; Not ready at all, use this as a model.
  (if (null? pl)
      pl
      (let ((cs (list (list 1)))
            (p (cons #f (list-copy (cdr pl))))
           )
       (let loop ((i (car pl)))
             (cond ((list-ref p (-1+ i)) ;; I.e. (not (memq i (car cs))) holds.
                      => (lambda (next-i)
                            (set-car! (list-tail p (-1+ i)) #f)
                            (set-car! cs (cons i (car cs)))
                            (loop next-i)
                         )
                   )
                   ((pos-of-first-matching p (lambda (x) x)) ;, Returns zero-based i.
                      => (lambda (i)
                            (set-car! cs (reverse! (car cs)))
                            (attach! (list) cs)
                            (loop (1+ i))
                         )
                   )
                   (else
                         (set-car! cs (reverse! (car cs)))
                         (reverse! cs)
                   )
             )
       )
      )
  )
)

(definec (B000034 n) (length (quadratic-residues-of (A016105 n))))
(define (B000034v2 n) (* (/ 1 4) (- (B000032 n) 1) (- (B000033 n) 1)))

;; Then the cycles, averages, etc.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;; Output functions and check lists.                                       ;;
;;                                                                         ;;
;; Note: I should make this a separate module of its own,                  ;;
;; available under http://www.research.att.com/~njas/sequences/format.html ;;
;; say with the name:                                                      ;;
;;  http://www.research.att.com/~njas/sequences/formatSC.txt               ;;
;; for all OEIS-Scheme-hackers to enjoy.                                   ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Call e.g. as:
;;  (map check-entry10 miscnum1_list)
;;  (map check-entry25 miscnum1_list)
;;  (map check-entry55 miscnum1_list)
;;
;; Or:
;;  (output-entries-to-file miscnum1_list "A95096-279.txt" "Jun 01 2004")

(define miscnum1_list
 (list
  (list 095076 "Parity of 1-fibits in Zeckendorf expansion A014417(n)."
    '(off: 0)
    '(comps: (010060 003714))
    '(y: "a(n) = 1 - A095111(n). Characteristic function of A020899. Run counts are given by A095276.")
  )

  (list 095111 "One minus the parity of 1-fibits in Zeckendorf expansion A014417(n)."
    '(off: 0)
    '(comps: (010059 003714))
    '(y: "a(n) = 1 - A095076(n). Characteristic function of A095096. Run counts are given by A095276.")
  )

  (list 020899 "Fibodious numbers: those n for which the parity of 1-fibits in Zeckendorf expansion A014417(n) is odd, i.e. for which A095076(n)=1."
    '(off: 0)
    '(y: "Complement of A095096. Cf. A000069 (odious numbers), A095083 (fibodious primes).")
  )

  (list 095096 "Fibevil numbers: those n for which the parity of 1-fibits in Zeckendorf expansion A014417(n) is even, i.e. for which A095076(n)=0."
    '(off: 0)
    '(y: "Characteristic function: A095111. Complement of A020899. Cf. A001969 (evil numbers), A095084 (fibevil primes).")
  )

  (list 026274 "Fib00 numbers: those n for which the Zeckendorf expansion A014417(n) ends with two zeros."
    '(off: 1)
    '(y: "Set-wise difference of A022342 - A035336. Union of A095097 & A035337. Cf. A095082 (fib00 primes).")
  )

  (list 095097 "Fib000 numbers: those n for which the Zeckendorf expansion A014417(n) ends with three zeros."
    '(off: 1)
    '(y: "Set-wise difference of A026274 - A035337. Cf. A095085 (fib000 primes).")
  )

  (list 095098 "Fib001 numbers: those n for which the Zeckendorf expansion A014417(n) ends with two zeros and a final one."
    '(off: 1)
    '(y: "Set-wise difference of A003622 - A095099. Cf. A095086 (fib001 primes).")
  )

  (list 035336 "Fib010 numbers: those n for which the Zeckendorf expansion A014417(n) ends with zero, one, zero."
    '(off: 1)
    '(y: "Set-wise difference of A022342 - A026274. Cf. A095087 (fib010 primes).")
  )

  (list 035337 "Fib100 numbers: those n for which the Zeckendorf expansion A014417(n) ends with one and two final zeros."
    '(off: 1)
    '(y: "Set-wise difference of A026274 - A095097. Cf. A095088 (fib100 primes).")
  )

  (list 095099 "Fib101 numbers: those n for which the Zeckendorf expansion A014417(n) ends with one, zero, one."
    '(off: 1)
    '(y: "Set-wise difference of A003622 - A095098. Cf. A095089 (fib101 primes).")
  )

  (list 095276 "Length of n-th run of identical symbols in A095076 & A095111."
    '(off: 1)
    '(y: "Partials sums: A095279. Cf. also A026465.")
  )

  (list 095277 "Numbers n such that 4n+3 is composite."
    '(off: 1)
    '(y: "a(n) = (A091236(n)-3)/4. Complement of A095278. Cf. also A045751.")
  )

  (list 095278 "Numbers n such that 4n+3 is prime."
    '(off: 1)
    '(y: "a(n) = (A002145(n)-3)/4. Complement of A095277. Union of A095272 & A095273. Cf. also A005098.")
  )

  (list 095279 "Partials sums of A095276."
    '(off: 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (list 095749 "Square array A(row>=1, col>=1) by antidiagonals: A(r,c) contains c:th prime p for which A037888(p)=(row-1)."
    '(off: 1) '(upto: 78)
    '(keywords: "tabl")
     (list 'e: (string-append
            "a(1) = A(1,1) = 3 (11 in binary) as it is the first prime"
            " whose binary expansion is palindromic."
            " a(2) = A(1,2) = 5 (101 in binary) as it is the second prime"
            " whose binexp is palindromic."
            " a(3) = A(2,1) = 2 (10 in binary) as it is the first prime"
            " whose binexp needs a flip of just one bit to become palindrome."
            " a(4) = A(1,3) = 7 (111 in binary) as it is the third prime"
            " whose binexp is palindromic."
            " a(5) = A(2,2) = 11 (1011 in binary) as it is the second prime"
            " whose binexp needs a flip of just one bit to become palindrome."
              )
     )
     (list 'y: (string-append
        "Row 1: A016041, 2: A095743, 3: A095744, 4: A095745, 5: A095746."
        " Cf. also A095759. A095747-A095748. Permutation of primes (A000040)."
         )
     )
  )

  (list 095759 "Triangle T(row>=0, 0<= pos <=row) by rows: T(r,p) contains number of odd primes p in range [2^(r+1),2^(r+2)] for which A037888(p)=pos."
    '(off: 0)
    '(keywords: "tabl")
    (list 'e: (string-append
         "a(0) = T(0,0) = 1 as there is one prime 3 (11 in binary)"
         " in range ]2^1,2^2[ whose binary expansion is palindromic. "
         "a(1) = T(1,0) = 2 as there are two primes, 5 and 7"
         " (101 and 111 in binary) in range ]2^2,2^3[ whose binary expansions"
         " are palindromic. a(2) = T(1,1) = 0, as there are no other primes"
         " in that range. "
         "a(3) = T(2,0) = 0, as there are no palindromic primes in range"
         " ]2^3,2^4[, but a(4) = T(2,1) = 2 as in the same range there are"
         " two primes 11 and 13 (1011 and 1101 in binary), whose binary"
         " expansion needs a flip of just one bit to become palindrome."
              )
    )
    (list 'y: (string-append
       "Row sums: A036378. Bisection of the leftmost diagonal: A095741."
       " Next diagonals: A095753, A095754, A095755, A095756."
       " Central diagonal (column): A095760."
       " The rightmost nonzero terms from each row: A095757"
       " (i.e. central diagonal and next-to-central diagonal interleaved)."
       " The penultimate nonzero terms from each row: A095758."
       " Cf. also A095749, A048700-A048704, A095742."
              )
     )
  )

 )
)

;; For August 26.-27. 2005 additions.
(define miscnum2_list
 (list
  (list 112045 "Position of primes (A000040) among  square-free numbers A000037."
    '(off: 1)
    '(comps: (028391 000040))
  )

  (list 112046 "The first i >= 1, for which Jacobi symbol J(i,2n+1) is not +1 (i.e. is either 0 or -1)."
    '(off: 1)
     (list 'e: (string-append
            "It appears that every term is prime (A000040)."
               )
     )
     (list 'c: (string-append
            "If we instead list the first i >= 1, for which Jacobi symbol J(i,2n+1) is 0,"
            " we get A090368."
               )
     )
     (list 'y: (string-append
       "a(n)=A112050(n)+1. Bisections: A112047, A112048. Their difference: A112053."
       " C.f. A112049, A112060, A112070."
               )
     )
  )

  (list 112047 "Bisection of A112046."
    '(off: 1)
     (list 'f: (string-append "A112046(2n-1)"))
     (list 'y: (string-append "C.f. A112048, A112053."))
  )

  (list 112048 "Bisection of A112046."
    '(off: 1)
     (list 'f: (string-append "A112046(2n)"))
     (list 'y: (string-append "C.f. A112047, A112053."))
  )

  (list 112049 "a(n) = Position of A112046(n) in A000040, 0 if it is not prime."
    '(off: 1)
     (list 'y: (string-append "C.f. A112060."))
     (list 'f: (string-append "A049084(A112046(n))"))
  )

  (list 112050 (string-append "The longest prefix of 1's in the Jacobi-vector"
                              " {J(2n+1,1),J(2n+1,2),...,J(2n+1,2n)} of 2n+1."
               )
    '(off: 1)
     (list 'f: (string-append "a(n) = A112046(n)-1"))
  )

  (list 112051 (string-append
                 "a(1)=1, a(n) = The first value i (> a(n-1)), where A112046(i)"
                 " gets a value distinct from any values A112046(1)..A112046(a(n-1))."
               )
    '(off: 1)
    '(upto: 75)
     (list 'y: (string-append
                   "Column 1 of A112060 (row 1 of A112061)."
                   " C.f. A112052."
               )
     )
  )

  (list 112052 "a(n) = 2*A112051(n)+1."
    '(off: 1)
    '(upto: 75)
     (list 'f: (string-append "A005408(A112051(n))"))
     (list 'c: "From n>=4 onward seems to be squares of primes (A001248).")
     (list 'y: (string-append "Column 1 of A112070 (row 1 of A112071)."))
  )

  (list 112053 "A112048 - A112047."
    '(off: 1)
     (list 'f: (string-append "a(n) = A112048(n)-A112047(n)."))
     (list 'y: (string-append
                   "Points where a(n) is not zero: A112054. Values at those points: A112059."
               )
     )
  )

  (list 112054 "Points where A112053 is not zero."
    '(off: 1)
     (list 'c: (string-append
         "Apparently these are all divisible by 6. See A112055."
               )
     )
  )

  (list 112055 (string-append "The numerator of A112054(n)/6.")
    '(off: 1)
     (list 'c: (string-append
         "All the terms of A112054 seem to be divisible by 6."
         " If that is true, then this is a complement of A112082."
               )
     )
  )

  (list 112056 (string-append
     "Odd numbers of the form 4n-1 for which jacobi-first-non-one(4n-1) differs from jacobi-first-non-one(4n+1)."
               )
    '(off: 1)
     (list 'c: (string-append
        "Here jacobi-first-non-one(m) (for odd numbers m) is defined as the first value of i >= 1,"
        " for which Jacobi symbol J(i,m) is not +1 (i.e. is either 0 or -1)."
               )
     )
     (list 'f: "a(n) = 4*A112054(n)-1.")
     (list 'y: (string-append
                "a(n) = A112057(n)-2 = A112058(n)-1."
              )
     )
  )

  (list 112057 (string-append
     "Odd numbers of the form 4n+1 for which jacobi-first-non-one(4n-1) differs from jacobi-first-non-one(4n+1)."
               )
    '(off: 1)
     (list 'c: (string-append
        "The definition of jacobi-first-non-one is given in A112056."
               )
     )
     (list 'f: "a(n) = 4*A112054(n)+1.")
     (list 'y: (string-append
                "a(n) = A112056(n)+2 = A112058(n)+1."
              )
     )
  )

  (list 112058 (string-append
     "Mean of A112056 and A112057."
               )
    '(off: 1)
     (list 'f: "a(n) = 4*A112054(n).")
     (list 'y: (string-append
                "a(n) = A112056(n)+1 = A112057(n)-1 = (A112056(n)+A112057(n))/2."
              )
     )
  )

  (list 112059 (string-append "Non-zero terms of A112053."
               )
    '(off: 1)
    '(comps: (112053 112054))
    '(y: "C.f. A112080.")
  )

  (list 112080 (string-append
     "a(n) = Difference between jacobi-first-non-one(24n+1) and jacobi-first-non-one(24n-1)."
               )
    '(off: 1)
     (list 'c: (string-append
        "The definition of jacobi-first-non-one is given in A112056."
               )
     )
     (list 'f: "a(n) = 4*A112054(n)+1.")
     (list 'y: (string-append
                "A112081(n) = a(n)/2. A112082 gives the points where a(n) is zero."
              )
     )
  )

  (list 112081 "A112080(n)/2."
    '(off: 1)
  )

  (list 112082 "Points where A112080 is zero."
    '(off: 1)
     (list 'y: (string-append "C.f. A112055.")
     )
  )


  (list 112060 "Square array A(col>=1, row>=1) by antidiagonals A(1,1), A(2,1), A(1,2), A(3,1), A(2,2), ..., where A(x,y) contains y:th natural number i for which A112049(i)=x, and 0 if no such i exists."
    '(off: 1)
    '(upto: 78)
    '(keywords: "tabl")
    '(indentries: Nperm)
     (list 'e: (string-append
            "The top left corner of the array:"
            " 1,2,5,6,9,10,..."
            "\n3,4,7,8,15,16,,..."
            "\n11,12,23,36,47,..."
               )
     )
     (list 'c: (string-append
      "This is a permutation of natural numbers provided that the sequence A112046"
      " contains only prime values and every prime occurs infinitely many times there."
               )
     )
     (list 'y: (string-append
   "A112070(x,y) = 2*A(X,Y)+1. Transpose: A112061. Column 1: A112051."
   " Row 1: A042963, Row 2: A112062, Row 3: A112063, Row 4: A112064"
   ", Row 5: A112065, Row 6: A112066, Row 7: A112067, Row 8: A112068, Row 9: A112069."
               )
     )
  )

  (list 112061 "Transpose of A112060."
    '(off: 1)
    '(upto: 78)
    '(keywords: "tabl")
    '(indentries: Nperm)
  )

  (list 112070 "Square array A(col>=1, row>=1) by antidiagonals A(1,1), A(2,1), A(1,2), A(3,1), A(2,2), ..., where A(x,y) contains y:th odd number 2i+1 (i>=1) for which A112049(2i+1)=x, and 0 if no such i exists."
    '(off: 1)
    '(upto: 78)
    '(keywords: "tabl")
     (list 'e: (string-append
            "The top left corner of the array:"
            " 3,5,11,13,19,21,..."
            "\n7,9,15,17,31,33,,..."
            "\n23,25,47,73,95,..."
               )
     )
     (list 'c: (string-append
      "This is a permutation of odd numbers greater than unity provided that the sequence A112046"
      " contains only prime values and every prime occurs infinitely many times there."
      " Because the Jacobi symbol is multiplicative, it follows that"
      " if n occurs on row i, and m occurs on row j, then n*m cannot occur before row min(i,j)."
               )
     )

     (list 'y: (string-append
   "A(x,y) = 2*A112060(x,y)+1. Transpose: A112071. Column 1: A112052."
   "Row 1: A047621, Row 2: A112072 Row 3: A112073, Row 4: A112074"
   ", Row 5: A112075, Row 6: A112076, Row 7: A112077, Row 8: A112078, Row 9: A112079."
               )
     )
  )

  (list 112071 "Transpose of A112070."
    '(off: 1)
    '(upto: 78)
    '(keywords: "tabl")
    '(indentries: Nperm)
  )

  (list 112062 "Positive integers i for which A112049(i) == 2"
    '(off: 1)
     (list 'y: "Row 2 of A112060.")
  )

  (list 112063 "Positive integers i for which A112049(i) == 3"
    '(off: 1)
     (list 'y: "Row 3 of A112060.")
  )
  (list 112064 "Positive integers i for which A112049(i) == 4"
    '(off: 1)
     (list 'y: "Row 4 of A112060.")
  )
  (list 112065 "Positive integers i for which A112049(i) == 5"
    '(off: 1)
     (list 'y: "Row 5 of A112060.")
  )
  (list 112066 "Positive integers i for which A112049(i) == 6"
    '(off: 1)
     (list 'y: "Row 6 of A112060.")
  )
  (list 112067 "Positive integers i for which A112049(i) == 7"
    '(off: 1)
     (list 'y: "Row 7 of A112060.")
  )
  (list 112068 "Positive integers i for which A112049(i) == 8"
    '(off: 1)
     (list 'y: "Row 8 of A112060.")
  )
  (list 112069 "Positive integers i for which A112049(i) == 9"
    '(off: 1)
     (list 'y: "Row 9 of A112060.")
  )

  (list 112072 "Odd numbers n for which 3 is the smallest i (>= 1) with Jacobi symbol J(i,n) getting either a value 0 or -1."
    '(off: 1)
    '(y: "Row 2 of A112070. a(n) = 2*A112062(n)+1.")
  )
  (list 112073 "Odd numbers n for which 5 is the smallest i (>= 1) with Jacobi symbol J(i,n) getting either a value 0 or -1."
    '(off: 1)
    '(y: "Row 3 of A112070. a(n) = 2*A112063(n)+1.")
  )
  (list 112074 "Odd numbers n for which 7 is the smallest i (>= 1) with Jacobi symbol J(i,n) getting either a value 0 or -1."
    '(off: 1)
    '(y: "Row 4 of A112070. a(n) = 2*A112064(n)+1.")
  )
  (list 112075 "Odd numbers n for which 11 is the smallest i (>= 1) with Jacobi symbol J(i,n) getting either a value 0 or -1."
    '(off: 1)
    '(y: "Row 5 of A112070. a(n) = 2*A112065(n)+1.")
  )
  (list 112076 "Odd numbers n for which 13 is the smallest i (>= 1) with Jacobi symbol J(i,n) getting either a value 0 or -1."
    '(off: 1)
    '(y: "Row 6 of A112070. a(n) = 2*A112066(n)+1.")
  )
  (list 112077 "Odd numbers n for which 17 is the smallest i (>= 1) with Jacobi symbol J(i,n) getting either a value 0 or -1."
    '(off: 1)
    '(y: "Row 7 of A112070. a(n) = 2*A112067(n)+1.")
  )
  (list 112078 "Odd numbers n for which 19 is the smallest i (>= 1) with Jacobi symbol J(i,n) getting either a value 0 or -1."
    '(off: 1)
    '(y: "Row 8 of A112070. a(n) = 2*A112068(n)+1.")
  )
  (list 112079 "Odd numbers n for which 23 is the smallest i (>= 1) with Jacobi symbol J(i,n) getting either a value 0 or -1."
    '(off: 1)
    '(y: "Row 9 of A112070. a(n) = 2*A112069(n)+1.")
  )

 )
)


;; The first differences of these sequences seem to be periodic,
;; (as expected if they are all formed as the intersections of
;; various n={a,b,...} modulo m sequences), and their periods
;; seem to grow as 2,4,6,16,72,... (not in OEIS, and neither is 1,2,3,8,36).

(define seqA112086 (list 2 4 6 16 72 420 3240))

(define (A112086 n) (list-ref seqA112086 (-1+ n))) ;; One-based sequence.

;; If we sum the period length prefixes of these sequences, as:
;; (add A112132 1 (A112086 2))
;; (add A112133 1 (A112086 3))
;; (add A112134 1 (A112086 4))
;; (add A112135 1 (A112086 5))
;; (add A112136 1 (A112086 6))
;; (add A112137 1 (A112086 7))
;;
;; we get the sequence 4, 12, 60, 420, 4620, 60060, 1021020, ...
;; which could be A097250
;; and if doubled, then it yields: 8, 24, 120, 840, 9240, 120120, 2042040, ...
;; which could be A066631 or A102476.

(definec (A112083org n) (nth-int-with-A112049-k 2 n))
(define (A112084org n) (A005408 (A112083 n)))

;; These were computed with above functions with the
;; compiled code. For n=22 A112083 will fail, as then
;; will appear the first value larger than 2^25 - 1 (= 33554431),
;; and thus the fixnum-routines of MIT Scheme used in fix:jacobi-symbol
;; will fail.

(define seqA112083 (list 2 4 12 35 155 239 779 2855 5279 9095 15695 59135
     350699 183395 1352339 1477295 1077959 6922920 3038555 12705840 14199120)
)

(define seqA112084 (list 5 9 25 71 311 479 1559 5711 10559 18191 31391 118271
     701399 366791 2704679 2954591 2155919 13845841 6077111 25411681 28398241)
)

(define (A112083 n) (list-ref seqA112083 (-1+ n))) ;; One-based sequence.
(define (A112084 n) (list-ref seqA112084 (-1+ n))) ;; One-based sequence.

;; Note the non-monotone drop from A112084(13)=701399 to A112084(14)=366791
;; and the similarity with A045535 (of which A062241 is duplicate ?):

;; 7,23,71,311,479,1559,5711,10559,18191,31391,118271,366791,366791,
;; 2155919,2155919,2155919,6077111,6077111,98538359,120293879,
;; 131486759,131486759,508095719,2570169839

;; Is this periodic or not?
(define (A112085 n) (- (A112055 (+ n 1)) (A112055 n)))

;; For August 27. 2005 additions.
(define miscnum3_list
 (list
  (list 112083 "Column 2 of A112060, i.e. row 2 of A112061."
    '(off: 1)
    '(upto: 21)
     (list 'y: (string-append
                   "Column 2 of A112060 (row 2 of A112061)."
                   " C.f. A112084."
               )
     )
  )

  (list 112084 "a(n) = 2*A112083(n)+1."
    '(off: 1)
    '(upto: 21)
    '(keywords: "more")
     (list 'f: (string-append "A005408(A112083(n))"))
     (list 'c: (string-append
       "Note the subsequence equal to the portion of A045535 / A062241, and"
       " also the non-monotone drops, like one from A112084(13)=701399 to A112084(14)=366791."
       "\n%C A112084 Could somebody recompute this sequence with e.g. Mathematica, as"
       " an independent confirmation of its correctness?"
               )
     )
     (list 'y: (string-append
                   "Column 2 of A112070 (row 2 of A112071)."
               )
     )
  )

  (list 112085 "First differences of A112055."
    '(off: 1)
     (list 'f: (string-append "a(n) = A112055(n+1)-A112055(n)."))
     (list 'c: (string-append
                   "Will other terms than 1, 2 or 4 ever appear?"
               )
     )
  )

  (list 112086 (string-append
             "a(n) = the period of the first differences of the nth row of A112060 (or A112070),"
             " or 0 if that row has not a periodic first difference."
               )
    '(off: 1)
    '(upto: 7)
    '(keywords: "more")
     (list 'c: (string-append
                 "These values have been computed empirically."
                 " An independent recomputation or a mathematical proof"
                 " would be welcome."
                 " The initial terms factored:"
                 " 2, 2*2, 2*3, 2*2*2*3*3, 2*2*7*3*5, 2*2*2*3*3*3*3*5, ..."
               )
     )
     (list 'y: (string-append
                 "These are the periods of A010684, A112132, A112133, A112134, A112135,"
                 " A112136, A112137, etc. (For A112138 & A112139 not computed yet)."
               )
     )
  )

  (list 112132 "First differences of A112062."
    '(off: 1)
     (list 'y: "Period=4. Also half of the first differences of A112072. C.f. A112086.")
  )

;; seem to grow as 2,4,6,16,72,... (not in OEIS, and neither is 1,2,3,8,36).

  (list 112133 "First differences of A112063."
    '(off: 1)
     (list 'y: "Period=6. Also half of the first differences of A112073. C.f. A112086.")
  )

  (list 112134 "First differences of A112064."
    '(off: 1)
     (list 'y: "Period=16. Also half of the first differences of A112074. C.f. A112086.")
  )

  (list 112135 "First differences of A112065."
    '(off: 1)
     (list 'y: "Period=72. Also half of the first differences of A112075. C.f. A112086.")
  )

  (list 112136 "First differences of A112066."
    '(off: 1)
     (list 'y: "Period=420. Also half of the first differences of A112076. C.f. A112086.")
  )

  (list 112137 "First differences of A112067."
    '(off: 1)
     (list 'y: "Period=3240. Also half of the first differences of A112077. C.f. A112086.")
  )

  (list 112138 "First differences of A112068."
    '(off: 1)
     (list 'y: "Period=?. Also half of the first differences of A112078. C.f. A112086.")
  )

  (list 112139 "First differences of A112069."
    '(off: 1)
     (list 'y: "Period=?. Also half of the first differences of A112079. C.f. A112086.")
  )

 )
)

;; For September 03. 2005 additions.
(define miscnum4_list
 (list
  (list 112140 "Square array A(col>=1, row>=1) by antidiagonals A(1,1), A(2,1), A(1,2), A(3,1), A(2,2), ..., where A(x,y) contains x:th odd number i for which J(A005117(y+1),i) = +1, where J is the Jacobi symbol."
    '(off: 1)
    '(upto: 105)
    '(keywords: "tabl")
     (list 'e: (string-append
            "The top left corner of the array:"
            " 1,7,9,15,17,23,25,..."
            "\n1,11,13,23,25,35,..."
            "\n1,9,11,19,21,29,..."
               )
     )
     (list 'y: (string-append
   "C.f. A112141, A112070. Transpose: A112187. Column 2: A112305."
   " Row 1: A047522, Row 2: A091998, Row 3: A090771, Row 4: A074229"
   ", Row 5: A112308, Row 6: A112310."
               )
     )
  )

  (list 112187 "Transpose of A112140."
    '(off: 1)
    '(upto: 105)
    '(keywords: "tabl")
  )


  (list 112141 "Square array A(col>=1, row>=1) by antidiagonals A(1,1), A(2,1), A(1,2), A(3,1), A(2,2), ..., where A(x,y) contains x:th odd number i for which J(A005117(y+1),i) = -1, where J is the Jacobi symbol."
    '(off: 1)
    '(upto: 105)
    '(keywords: "tabl")
     (list 'e: (string-append
            "The top left corner of the array:"
            " 3,5,11,13,19,21,27,..."
            "\n5,7,17,19,29,31,41,..."
            "\n3,7,13,17,23,27,33,..."
               )
     )
     (list 'y: (string-append
   "C.f. A112140, A112070. Transpose: A112188. Column 1: A112312."
   " Row 1: A047621, Row 2: A092242, Row 3: A112306, Row 4: A112307"
   ", Row 5: A112309, Row 6: A112311."
               )
     )
  )

  (list 112188 "Transpose of A112141."
    '(off: 1)
    '(upto: 105)
    '(keywords: "tabl")
  )

  (list 112305 "a(n) = First odd number i > 1 such that J(A005117(n+1),i) = +1, where J is the Jacobi symbol."
    '(off: 1)
     (list 'y: (string-append
                   "Column 2 of A112140 (row 2 of A112187)."
                   " C.f. A112312."
               )
     )
  )

  (list 112312 "a(n) = First odd number i > 1 such that J(A005117(n+1),i) = -1, where J is the Jacobi symbol."
    '(off: 1)
     (list 'y: (string-append
                   "Column 1 of A112141 (row 1 of A112188)."
                   " C.f. A112305."
               )
     )
  )

  (list 112306 "Natural numbers whose decimal expansion ends either in 3 or 7."
    '(off: 0)
    '(keywords: "base")
     (list 'c: (string-append
                   "Numbers that are congruent to {3, 7} mod 10."
                   " Also odd numbers i such that J(5,i) = -1,"
                   " where J is Jacobi-symbol."
               )
     )
     (list 'y: (string-append
                   "Row 3 of A112141. Complement of A090771 relative to A045572."
                   " Will sequences A063226 and A063239 eventually diverge from this one,"
                   " and from each other? C.f. also A040106, A090298."
               )
     )
  )

  (list 112307 "Natural numbers that are congruent to {7, 11, 13, 17} mod 24."
    '(off: 0)
     (list 'c: (string-append
                   "Also odd numbers i such that J(6,i) = -1,"
                   " where J is Jacobi-symbol."
               )
     )
     (list 'y: "Row 4 of A112141. Complement of A074229 relative to A007310.")
  )

  (list 112308 "Natural numbers that are congruent to {1, 3, 9, 19, 25, 27} mod 28."
    '(off: 0)
     (list 'c: (string-append
                   "Also odd numbers i such that J(7,i) = +1,"
                   " where J is Jacobi-symbol."
               )
     )
     (list 'y: "Row 5 of A112140. C.f. also A112309.")
  )

  (list 112309 "Natural numbers that are congruent to {5, 11, 13, 15, 17, 23} mod 28."
    '(off: 0)
     (list 'c: (string-append
                   "Also odd numbers i such that J(7,i) = -1,"
                   " where J is Jacobi-symbol."
               )
     )
     (list 'y: "Row 5 of A112141. C.f. also A112308.")
  )

  (list 112310 "Natural numbers that are congruent to {1, 3, 9, 13, 27, 31, 37, 39} mod 40."
    '(off: 0)
     (list 'c: (string-append
                   "Also odd numbers i such that J(10,i) = +1,"
                   " where J is Jacobi-symbol."
               )
     )
     (list 'y: "Row 6 of A112140. Complement of A112311 relative to A045572.")
  )

  (list 112311 "Natural numbers that are congruent to {7, 11, 17, 19, 21, 23, 29, 33} mod 40."
    '(off: 0)
     (list 'c: (string-append
                   "Also odd numbers i such that J(10,i) = -1,"
                   " where J is Jacobi-symbol."
               )
     )
     (list 'y: "Row 6 of A112141. Complement of A112310 relative to A045572.")
  )

 )
)



(definec (A112062 n) ;; Positive integers i for which A112049(i) == 2
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112062 (- n 1)))))
         (if (= 2 (A112049 i)) i (loop (+ 1 i)))
      )
    )
  )
)

(definec (A112063 n) ;; Positive integers i for which A112049(i) == 3
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112063 (- n 1)))))
         (if (= 3 (A112049 i)) i (loop (+ 1 i)))
      )
    )
  )
)

(definec (A112064 n) ;; Positive integers i for which A112049(i) == 4
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112064 (- n 1)))))
         (if (= 4 (A112049 i)) i (loop (+ 1 i)))
      )
    )
  )
)

(definec (A112065 n) ;; Positive integers i for which A112049(i) == 5
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112065 (- n 1)))))
         (if (= 5 (A112049 i)) i (loop (+ 1 i)))
      )
    )
  )
)

(definec (A112066 n) ;; Positive integers i for which A112049(i) == 6
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112066 (- n 1)))))
         (if (= 6 (A112049 i)) i (loop (+ 1 i)))
      )
    )
  )
)

(definec (A112067 n) ;; Positive integers i for which A112049(i) == 7
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112067 (- n 1)))))
         (if (= 7 (A112049 i)) i (loop (+ 1 i)))
      )
    )
  )
)

(definec (A112068 n) ;; Positive integers i for which A112049(i) == 8
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112068 (- n 1)))))
         (if (= 8 (A112049 i)) i (loop (+ 1 i)))
      )
    )
  )
)

(definec (A112069 n) ;; Positive integers i for which A112049(i) == 9
  (cond
    ((= 0 n) 0) ;; Starting point. Compute from n=1 onward.
    (else
      (let loop ((i (+ 1 (A112069 (- n 1)))))
         (if (= 9 (A112049 i)) i (loop (+ 1 i)))
      )
    )
  )
)


;; A005408(C0000XX(n)) seems to be A047621 (= duplicate of A066587 ?), congruent to 3 or 5 mod 8.

(define (A112072 n) (A005408 (A112062 n)))
(define (A112073 n) (A005408 (A112063 n)))
(define (A112074 n) (A005408 (A112064 n)))
(define (A112075 n) (A005408 (A112065 n)))
(define (A112076 n) (A005408 (A112066 n)))
(define (A112077 n) (A005408 (A112067 n)))
(define (A112078 n) (A005408 (A112068 n)))
(define (A112079 n) (A005408 (A112069 n)))


;;;

;; TO BE SUBMITTED! (Most probably submitted already now in Oct 31 2009).

;; Number of Jacobi sums that are zeros, for odd numbers:
(definec (C000001 n) (jacobi-sums-zeros (A005408 n)))

;; Is it that each even result (2, 4, 6, 8, ) occurs only a finite number of times,
;; but check also 118, 128, whatever ?8 ?
;; but not so with odd results?


;; For odd primes, start n from 2.
(definec (C000002 n) (jacobi-sums-zeros (A000040 n)))

;; The first i, where C000001(i) gets a value
;; distinct from any values C000001(0)..C000001(a(n-1))
(definec (C000003 n)
  (cond
    ((<= n 1) n) ;; C000001(0) = 0, C000001(1) = 1.
    (else
     (let ((b (C000003 (- n 1))))
       (let outloop ((i (+ b 1)))
        (let inloop ((k (C000001 i)) (j 0))
         (cond
           ((> j b) i)
           ((= (C000001 j) k) (outloop (+ 1 i)))
           (else (inloop k (+ 1 j)))
         )
        )
       )
     )     
    )
  )
)

;; The first i, where C000002(i) gets a value
;; distinct from any values C000002(1)..C000002(a(n-1))
;; Make this one-based.
(definec (C000004 n)
  (cond
    ((<= n 1) n) ;; C000002(1) = 0, C000002(2) = 1.
    (else
     (let ((b (C000004 (- n 1))))
       (let outloop ((i (+ b 1)))
        (let inloop ((k (C000002 i)) (j 1))
         (cond
           ((> j b) i)
           ((= (C000002 j) k) (outloop (+ 1 i)))
           (else (inloop k (+ 1 j)))
         )
        )
       )
     )     
    )
  )
)

;; The first odd number 2*i+1, where C000001(i) gets a value
;; distinct from any values C000001(0)..C000001(a(n-1))

;; Compute upto n=75:
(define (C000005 n) (A005408 (C000003 n)))

(define (C000005v2 n)
  (cond
    ((<= n 1) (A005408 n)) ;; C000001(0) = 0, C000001(1) = 1.
    (else
     (let ((b (/ (- (C000005v2 (- n 1)) 1) 2)))
       (let outloop ((i (+ b 1)))
        (let inloop ((k (C000001 i)) (j 1))
         (cond
           ((> j b) (A005408 i))
           ((= (C000001 j) k) (outloop (+ 1 i)))
           (else (inloop k (+ 1 j)))
         )
        )
       )
     )     
    )
  )
)

(define (C000007 n) (C000001 (C000003 n)))

(define (C000006 n) (A000040 (C000004 n))) ;; Add one here somewhere?
(define (C000008 n) (C000002 (C000004 n)))

(define C000009
  (compose-funs
     A005408
     (fun-succ-matching-is0 (lambda (n) (zero? (sum-of-jacobi-symbols-halfway (A005408 n)))))
  )
)

(define (C000010 n) (jacobi-sums-zeros (C000009 n)))

(define C000011 ;; Primes of the form 4k+1 ???
  (compose-funs
     A000040
     (fun-succ-matching-is0 (lambda (n) (zero? (sum-of-jacobi-symbols-halfway (A000040 n)))))
  )
)

(define C000012 (compose-funs sum-of-jacobi-symbols-halfway A000040))

(define C000013 (compose-funs A000040 (fun-succ-distincts1 C000012)))

;; If all odd numbers occur in C000012, then this is a permutation of odd numbers:
;; Subset: C000016.
(define C000014 (compose-funs sum-of-jacobi-symbols-halfway C000013))

;; Subset of C000013. C.f. A002148.
(define C000015 (compose-funs A000040 (fun-succ-recpositions1 C000012)))

;; Records in C000012 and C000014.
(define C000016 (compose-funs sum-of-jacobi-symbols-halfway C000015))

;; Primes whose least primitive root is the first quadratic non-residue, (subset: safe primes)
;; and their complement, primes where it is not.

;; All 4k+3 primes in each range [2^n,2^(n+1)] with the minimal sum-of-jacobi-symbols-halfway
;; minimal value for each such range, number of primes with that value (in that range).




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;; Copied and modified from:                                               ;;
;; http://www.iki.fi/~kartturi/matikka/Nekomorphisms/gato-out.scm          ;;
;; functions output-gatomorphism-entry-aux et al.                          ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-option 'format)


(define (Anum->str Anum)
  (string-append "A"
      (string-pad-left (if (string? Anum) Anum (number->string Anum)) 6 #\0)
  )
)


(define output_seq
  (lambda (seq)
      (cond ((null? seq)) ;; No (newline) this time!
            (else (write (car seq))
                  (if (not (null? (cdr seq))) (write-string ","))
                  (output_seq (cdr seq))
            )
      )
  )
)


(define (len-of-max-full-line-prefix seq max-line-len)
 (let loop ((seq seq)
            (terms 0)
            (room-left max-line-len)
           )
   (cond ((negative? room-left) (max 1 (- terms 1))) ;; At least one term, even if it is overlength!
         ((not (pair? seq)) terms)
         (else (loop (cdr seq)
                     (+ 1 terms)
                     (- room-left (string-length (format #f ",~A" (car seq))))
               )
         )
   )
 )
)



(define (html-out-sequence-search-link out seq seeklen)
  (with-output-to-port out
   (lambda ()
     (let ((seek-seq (cond ((< seeklen (- (length seq) 2)) (list-head (cddr seq) seeklen)) (else (cddr seq)))))
      (write-string "<A HREF=\"http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eishis.cgi?sequence=")
      (output_seq seek-seq)
      (write-string "\">")
      (output_seq (list-head seq (min seeklen (length seq))))
      (write-string "</A>\n")
     )
   )
  )
)

(define (html-out-Anchor Anum out)
  (format out "<A NAME=\"~A\"></A>" (Anum->str Anum))
)

(define (html-out-sequence-A-link Anum out)
  (let ((Astr (Anum->str Anum)))
       (format out "<A HREF=\"http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=~A\">~A</A>" Astr Astr)
  )
)


;; Works in MIT Scheme:
(define (Anum->Afun Anum)
  (eval (string->symbol (string-downcase (Anum->str Anum))) user-initial-environment)
)

;; (complist->exprstr (list 1 2 3 4)) --> "A000001(A000002(A000003(A000004(n))))"
(define (complist->exprstr complist)
   (with-string-output-port
      (lambda (outport)
          (for-each (lambda (anum) (format outport "~A(" (Anum->str anum)))
                    complist
          )
          (format outport "n")
          (for-each (lambda (x) (format outport ")")) complist)
      )
   )
)


(define (check-composition outport comp base-seq Aseq Astr check-only?)
   (let ((Acomp (compose-funlist (map Anum->Afun comp))))
     (cond ((not (equal? (map Acomp base-seq) Aseq))
              (format outport "!!! The composition ~A = ~A is not correct!\n"
                                     Astr (complist->exprstr comp)
              )
           )
           (check-only?
              (format outport "Yes, the composition ~A = ~A is correct.\n"
                                     Astr (complist->exprstr comp)
              )
           )
     )
   )
)


(define (check-entry10 listlet) (output-OEIS-entry listlet 1024 10 #t "Kuu 00 2004" (current-output-port)))
(define (check-entry25 listlet) (output-OEIS-entry listlet 1024 25 #t "Kuu 00 2004" (current-output-port)))
(define (check-entry55 listlet) (output-OEIS-entry listlet 1024 45 #t "Kuu 00 2004" (current-output-port)))

(define (output-entries-to-file listlets outfile subm-date)
   (call-with-output-file outfile
     (lambda (outport)
       (map (lambda (listlet) (output-OEIS-entry listlet 120 45 #f subm-date outport))
            listlets
       )
     )
   )
)


(define (output-OEIS-entry listlet check-upto-n seek-len check-only? subm-date outport)
  (let* ((Anum           (list-ref listlet 0))
         (name           (list-ref listlet 1))
         (rest-of        (cddr listlet))
         (c              (cond ((assoc 'c: rest-of) => cadr) (else #f)))
         (e              (cond ((assoc 'e: rest-of) => cadr) (else #f)))
         (f              (cond ((assoc 'f: rest-of) => cadr) (else #f)))
         (y              (cond ((assoc 'y: rest-of) => cadr) (else #f)))
         (off            (cond ((assoc 'off: rest-of) => cadr) (else (error "output-entry: field 'off:' (starting offset) required!"))))
         (keywords       (cond ((assoc 'keywords: rest-of) => cadr) (else #f)))
         (Ainv           (cond ((assoc 'inv: rest-of) => cadr) (else #f)))
         (comps          (cond ((assoc 'comps: rest-of) => cdr) (else #f))) ;; '(comps: (000040 091207) (014580 091208))
         (scheme         (cond ((assoc 'scheme: rest-of) => cdr) (else #f)))
         (indentries     (cond ((assoc 'indentries: rest-of) => cdr) (else (list))))
         (check-only?    (or check-only? (assoc 'check-only! rest-of)))

         (check-upto-n   (cond ((assoc 'upto: rest-of) => cadr) (else check-upto-n)))

         (Astr (Anum->str Anum))
         (Afun (Anum->Afun Anum))

         (Ainvstr (and Ainv (Anum->str Ainv)))
         (Ainvfun (and Ainv (Anum->Afun Ainv)))

         (base-seq (map (lambda (n) (+ n off)) (iota0 (- check-upto-n off))))
         (Aseq (map Afun base-seq))
         (negative-terms? (there-exists? Aseq negative?))
         (one-based-pos-of-first-term-gte-2
               (cond ((pos-of-first-matching Aseq (lambda (x) (>= (abs x) 2))) => 1+)
                     (else 1) ;; If no terms other than 0,1 or -1, then use 1 as the second elem of offset pair.
               )
         )
         (more-than-one-zero? (cond ((and Ainvfun (memq 0 Aseq)) => (lambda (r) (memq 0 (cdr r)))) (else #f)))
;;       (no-larger-than-abs-1-terms? (for-all? Aseq (lambda (n) (< (abs n) 2)))) ;; Not needed.
         (Y-line-started? #f) ;; Not yet.
        )

     (cond (Ainvfun
;; If Aseq has more than one 0, then the given inverse function is non-surjective injection from N to N,
;; and we have to check Afun(Ainvfun(x)), and otherwise Ainvfun(Afun(x)):
                (cond (more-than-one-zero?
                        (let ((Ainvseq (map Ainvfun base-seq)))
                          (cond ((not (equal? (map Afun Ainvseq) base-seq))
                                  (format outport "!!! This function ~A IS NOT an inverse function of injection ~A (checked up to n=~A).\n"
                                     Astr Ainvstr
                                  )
                                )
                                (check-only?
                                  (format outport "Yes, this function ~A seems to be an inverse function of non-surjective injection ~A (checked up to n=~A).\n"
                                     Astr Ainvstr check-upto-n
                                  )
                                )
                          )
                        ) ;; let
                      )
                      ((not (equal? (map Ainvfun Aseq) base-seq))
                         (format outport "!!! The inverse ~A for ~A is not correct\n"
                             Ainvstr Astr
                         )
                      )
                      (check-only?
                         (format outport "Yes, function ~A seems to be an inverse of ~A when computed up to n=~A\n"
                             Ainvstr Astr check-upto-n
                         )
                      )
                )
           )
     )

     (cond (comps
             (for-each (lambda (comp)
                         (check-composition outport comp base-seq Aseq Astr check-only?)
                       )
                       comps
             )
           )
     )

     (cond (check-only?
               (html-out-sequence-A-link Anum outport)
               (format outport " = \n")
               (html-out-sequence-search-link outport Aseq seek-len)
           )
           (else
               (format outport "%I ~A\n" Astr)
               (let* ((max-term-line-len 69) ;; As (string-length "%S A012345") = 10.
                      (part1len (len-of-max-full-line-prefix Aseq max-term-line-len))
                      (part2len (len-of-max-full-line-prefix (list-tail Aseq part1len) max-term-line-len)) ;; Could be zero!
                      (part3len (len-of-max-full-line-prefix (list-tail Aseq (+ part1len part2len)) max-term-line-len)) ;; Could be zero!
                      (part1 (sublist Aseq 0 part1len))
                      (part2 (sublist Aseq part1len (+ part1len part2len))) ;; results () if part2len = 0.
                      (part3 (sublist Aseq (+ part1len part2len) (+ part1len part2len part3len))) ;; results () if part2len = 0 or part3len = 0.
                     )
                 (with-output-to-port outport
                   (lambda ()
                     (format outport "%S ~A " Astr) (output_seq (map abs part1)) (format outport ",\n")
                     (cond ((pair? part2)
                              (format outport "%T ~A " Astr) (output_seq (map abs part2)) (format outport ",\n")
                           )
                     )
                     (cond ((pair? part3)
                              (format outport "%U ~A " Astr) (output_seq (map abs part3)) (newline outport)
                           )
                     )

                     (cond (negative-terms?
                              (format outport "%V ~A " Astr) (output_seq part1) (format outport ",\n")
                              (cond ((pair? part2)
                                       (format outport "%W ~A " Astr) (output_seq part2) (format outport ",\n")
                                    )
                              )
                              (cond ((pair? part3)
                                       (format outport "%X ~A " Astr) (output_seq part3) (newline outport)
                                    )
                              )
                           )
                     )
                   )
                 )
                ) ;; let*

                (format outport "%N ~A ~A\n" Astr name)


                (cond (f
                         (format outport "%F ~A ~A\n" Astr f)
                      )
                )
             
                (cond (c
                         (format outport "%C ~A ~A\n" Astr C)
                      )
                )

                (cond (e
                         (format outport "%e ~A ~A\n" Astr e)
                      )
                )


;;              (format outport "%H ~A A. Karttunen, <a href=\"http://www.research.att.com/~~njas/sequences/GF2Xfuns.scm.txt\">Scheme-program for computing this sequence.</a>\n"
;;                      Astr
;;              )

                (cond
                  ((pair? indentries)
                    (cond ((memq 'GF2X indentries)
                            (format outport "%H ~A <a href=\"http://www.research.att.com/~~njas/sequences/Sindx_Ge.html#GF2X\">Index entries for sequences operating on GF(2)[X]-polynomials</a>\n"
                                    Astr
                            )
                         )
                    )
                    (cond ((memq 'Lattices indentries)
                            (format outport "%H ~A <A HREF=\"http://www.research.att.com/~~njas/sequences/Sindx_La.html#Lattices\">Index entries for sequences related to Lattices</A>\n"
                                    Astr
                            )
                         )
                    )
                    (cond ((memq 'Nperm indentries)
                            (format outport "%H ~A <a href=\"http://www.research.att.com/~~njas/sequences/Sindx_Per.html#IntegerPermutation\">Index entries for sequences that are permutations of the natural numbers</a>\n"
                                    Astr
                            )
                         )
                    )
                  )
                )
             
                (cond (Ainv
                        (cond ((not Y-line-started?)
                                 (format outport "%Y ~A" Astr)
                                 (set! Y-line-started? #t)
                              )
                        )
                        (cond (more-than-one-zero?
                                  (format outport " Inverse of ~A." Ainvstr)
                              )
                              (else
                                  (format outport " Inverse: ~A." Ainvstr)
                              )
                        )
                     )
                )

             
             ;; We should also check them, but can't do this with just A-numbers, as
             ;; we need the function definitions. (Here the old-fashioned Lisp would beat Scheme.)
                (cond (comps
                        (cond ((not Y-line-started?)
                                 (format outport "%Y ~A" Astr)
                                 (set! Y-line-started? #t)
                              )
                        )

                        (format outport " a(n)")
                        (for-each (lambda (comp)
                                     (format outport " = ~A" (complist->exprstr comp))
                                  )
                                  comps
                        )
                        (format outport ".")
                      )
                )
             
                (cond (y
                        (cond ((not Y-line-started?)
                                 (format outport "%Y ~A" Astr)
                                 (set! Y-line-started? #t)
                              )
                        )

                        (format outport " ~A" y)
                      )
                )

                (if Y-line-started? (newline outport))

                (format outport "%K ~A ~A~A\n"
                        Astr (if negative-terms? "sign" "nonn")
                             (if keywords (string-append "," keywords) "")
                )

                (format outport "%O ~A ~A,~A\n"
                        Astr off one-based-pos-of-first-term-gte-2
                )

                (format outport "%A ~A Antti Karttunen (His-Firstname.His-Surname(AT)iki.fi), ~A\n"
                        Astr subm-date
                )

                (cond (scheme
                        (format outport "%o ~A (MIT Scheme:)\n" Astr)
                        (for-each (lambda (schemedef) (format outport "%o ~A ~A\n" Astr schemedef)) scheme)
                      )
                )
             
                (newline outport)
           ) ;; else
     ) ;; cond
  ) ;; let*
)


