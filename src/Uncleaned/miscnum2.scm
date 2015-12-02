
(declare (usual-integrations))

(load "definech")

(load "c:\\program files (x86)\\slib\\mitscheme.init") ;; A. Jaffer's SLIB Scheme library.


(require 'factor) ;; This is for prime? from SLIB-library.


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

(definec (A000010 n) (apply * (sub1from1st_nums (sort (factor n) <))))

;; %F A000203 Multiplicative with a(p^e) = (p^(e+1)-1)/(p-1). - David W. Wilson, Aug 01, 2001.

(definec (A000203 n)
   (fold-left (lambda (prod p.e) (* prod (/ (- (expt (car p.e) (+ 1 (cdr p.e))) 1) (- (car p.e) 1))))
              1
              (if (= 1 n) (list) (elemcountpairs (ifactor n))) ;; (sort (factor n) <)
   )
)


(definec (A054973 n)
  (let loop ((k n) (s 0))
       (cond ((zero? k) s)
             ((= n (A000203 k)) (loop (- k 1) (+ s 1)))
             (else (loop (- k 1) s))
       )
  )
)

(define A007369 (ZERO-POS 1 1 A054973))

(define A159886 (MATCHING-POS 1 1 (lambda (n) (< 1 (A054973 n)))))

;; Largest squarefree number dividing n: the squarefree kernel of n, rad(n), radical of n.
(definec (A007947 n) (apply * (uniq (ifactor n))))

(define (A003557 n) (/ n (A007947 n)))

(define (A066503 n) (- n (A007947 n))) ;; a(n) = n - squarefree kernel of n, A007947. 

(definec (A255326 n) (if (zero? n) n (+ 1 (A255326 (A066503 n)))))

(define A255409 (RECORD-POS 0 0 A255326))

(define (isA255334? n)
 (let ((sig_n (A000203 n)) (rad_n (A007947 n)))
  (let loop ((try (+ n rad_n)))
       (cond ((>= try sig_n) #f)
             ((and (= sig_n (A000203 try)) (= rad_n (A007947 try))) #t)
             (else (loop (+ try rad_n)))
       )
  )
 )
)

(define A255334 (MATCHING-POS 1 1 isA255334?)) ;; Very slow.

(define (A255412 n) (A000203 (A255334 n)))

(define (A255424 n) (A007947 (A255334 n)))

(define (A255425 n) (A003557 (A255334 n)))

(define (A255426 n) (A003557 (A255423 n)))


(define (isA255335? n)
 (let ((sig_n (A000203 n)) (rad_n (A007947 n)))
  (let loop ((try (- n rad_n)))
       (cond ((< try rad_n) #f)
             ((and (= sig_n (A000203 try)) (= rad_n (A007947 try))) #t)
             (else (loop (- try rad_n)))
       )
  )
 )
)

(define A255335 (MATCHING-POS 1 1 isA255335?)) ;; Slowenly...

(define (A036689 n) (* (A000040 n) (- (A000040 n) 1))) ;; Product of a prime and the previous number.
(define (A008837 n) (/ (A036689 n) 2)) ;; p(p-1)/2 for p prime. 

(define A001359 (MATCHING-POS 1 1 (lambda (n) (and (= 1 (A010051 n)) (= 1 (A010051 (+ 2 n))))))) ;; Lesser of twin primes.
 
(definec (A002110 n) (if (zero? n) 1 (* (A000040 n) (A002110 (- n 1)))))

(definec (A005867 n) (if (zero? n) 1 (* (- (A000040 n) 1) (A005867 (- n 1)))))

(define (A006094 n) (* (A000040 n) (A000040 (+ 1 n)))) ;; Products of 2 successive primes.

(define (A001248 n) (A000290 (A000040 n)))

(define A001358 (MATCHING-POS 1 1 (lambda (n) (= 2 (A001222 n))))) ;; Semiprimes (or biprimes): products of two primes. 

(define (A100484 n) (* 2 (A000040 n))) ;; Even semiprimes.

;; (define A100484v2 (MATCHING-POS 1 1 (lambda (n) (and (even? n) (= 2 (A001222 n))))))

(definec (A251725 n)
    (let ((spf (A020639 n))
          (gpf (A006530 n))
         )
       (if (= spf gpf)
           1
           (let outerloop ((k 2))
             (let innerloop ((r 1))
                (cond ((and (<= r spf) (< gpf (* k r))) k)
                      ((<= r spf) (innerloop (* k r)))
                      (else (outerloop (+ 1 k)))
                )
             )
           )
       )
    )
)

(define (A251725v2 n)
   (if (= 1 n) 1
       (let ((fs (uniq (ifactor n))))
          (if (= 1 (length fs))
              1
              (let outerloop ((base 2))
                (let innerloop ((fs fs) (prevlen #f))
                   (cond ((null? fs) base)
                         ((not prevlen) (innerloop (cdr fs) (A162319bi (car fs) base)))
                         ((= (A162319bi (car fs) base) prevlen) (innerloop (cdr fs) prevlen))
                         (else (outerloop (+ 1 base)))
                   )
                )
              )
          )
       )
   )
)

;; A252370-A252375 are now reserved for your use.

(define (A252372 n) (if (< (A252375 n) (+ 1 (A006530 n))) 1 0)) ;; Characteristic function for A251726

(definec (A252373 n) (if (<= n 1) 0 (+ (A252372 n) (A252373 (- n 1)))))

;; Entanglement-permutations between complementary pairs A251726/A251727 and even/odd numbers:

(definec (A252757 n)
   (cond ((<= n 1) n)
         ((= 1 (A252372 n)) (* 2 (A252757 (A252373 n)))) ;; if n is in A251726 ?
         (else (+ 1 (* 2 (A252757 (- n (A252373 n) 1))))) ;; otherwise in A251727.
   )
)

(definec (A252758 n)
   (cond ((<= n 1) n)
         ((even? n) (A251726 (A252758 (/ n 2))))
         (else (A251727 (A252758 (/ (- n 1) 2))))
   )
)


;; A252459-A252464 are now reserved for your use. 

(definec (A252459 n) (cond ((= 1 n) 0) ((not (zero? (A252372 n))) 0) (else (+ 1 (A252459 (A003961 n))))))


(definec (A252374 n)
  (let ((spf (A020639 n))
        (gpf (A006530 n))
       )
    (let outerloop ((r 2))
          (let innerloop ((rx 1) (k 0))
             (cond ((and (<= rx spf) (< gpf (* r rx))) k)
                   ((<= rx spf) (innerloop (* r rx) (+ 1 k)))
                   (else (outerloop (+ 1 r)))
             )
          )
    )
  )
)


(definec (A252375 n)
  (let ((spf (A020639 n))
        (gpf (A006530 n))
       )
    (let outerloop ((r 2))
          (let innerloop ((rx 1))
             (cond ((and (<= rx spf) (< gpf (* r rx))) r)
                   ((<= rx spf) (innerloop (* r rx)))
                   (else (outerloop (+ 1 r)))
             )
          )
    )
  )
)

(define (A252375v2 n) (let ((x (A251725 n))) (if (= 1 x) 2 x)))

;; An interlude:
(define A261073 (MATCHING-POS 1 1 (lambda (n) (and (= 2 (A001222 n)) (= (A000523 (A020639 n)) (A000523 (A006530 n))) (= 1 (A101080bi (A020639 n) (A006530 n)))))))

(define A261074 (MATCHING-POS 1 1 (lambda (n) (and (= 2 (A001222 n)) (= (A000523 (A020639 n)) (A000523 (A006530 n))) (= 2 (A101080bi (A020639 n) (A006530 n)))))))

(define A261075 (MATCHING-POS 1 1 (lambda (n) (and (= 2 (A001222 n)) (= (A000523 (A020639 n)) (A000523 (A006530 n))) (= 3 (A101080bi (A020639 n) (A006530 n)))))))

(define A261077 (MATCHING-POS 1 1 (lambda (n) (and (= 2 (A001222 n)) (= 1 (A101080bi (A020639 n) (A006530 n)))))))

(define A261078 (MATCHING-POS 1 1 (lambda (n) (and (= 2 (A001222 n)) (pow2? (- (A006530 n) (A020639 n)))))))


(definec (A260737 n)
   (let loop ((s 0) (pfs (ifactor n)))
      (cond ((or (null? pfs) (null? (cdr pfs))) s)
            (else (loop (fold-left (lambda (a p) (+ a (A101080bi (car pfs) p))) s (cdr pfs))
                        (cdr pfs)
                  )
            )
      )
   )
)

(definec (A261079 n)
   (let loop ((s 0) (pfs (ifactor n)))
      (cond ((or (null? pfs) (null? (cdr pfs))) s)
            (else (loop (fold-left (lambda (a p) (+ a (abs (- (A000720 (car pfs)) (A000720 p))))) s (cdr pfs))
                        (cdr pfs)
                  )
            )
      )
   )
)

(define A261080 (MATCHING-POS 1 1 (lambda (n) (and (= 1 (A260737 n)) (= 1 (A261079 n))))))

;; (definec (A260737 n)
;;    (let loop ((s 0) (pfs (ifactor n)))
;;       (cond ((or (null? pfs) (null? (cdr pfs))) s)
;;             (else (loop (+ s (fold-left (lambda (a p) (+ a (A101080bi (car pfs) p))) 0 (cdr pfs)))
;;                         (cdr pfs)
;;                   )
;;             )
;;       )
;;    )
;; )



(define A251726 (MATCHING-POS 1 2 (lambda (n) (< (A006530 n) (A000290 (A020639 n))))))
(define A251726v1 (MATCHING-POS 1 2 (lambda (n) (< (A252375 n) (+ 1 (A006530 n))))))
(define A251726v2 (MATCHING-POS 1 2 (lambda (n) (< (A251725 n) (+ 1 (A006530 n))))))

(define A251727 (MATCHING-POS 1 2 (lambda (n) (> (A006530 n) (A000290 (A020639 n))))))
(define A251727v1 (MATCHING-POS 1 2 (lambda (n) (= (A252375 n) (+ 1 (A006530 n))))))
(define A251727v2 (MATCHING-POS 1 2 (lambda (n) (= (A251725 n) (+ 1 (A006530 n))))))

(define (A138510 n) (A251725 (A001358 n)))

(define A138511 (MATCHING-POS 1 2 (lambda (n) (and (= 2 (A001222 n)) (> (A006530 n) (A000290 (A020639 n)))))))

(define A138511v1 (MATCHING-POS 1 2 (lambda (n) (and (= 2 (A001222 n)) (= (A252375 n) (+ 1 (A006530 n)))))))

(define A138511v2 (COMPOSE A001358 (MATCHING-POS 1 1 (lambda (n) (= (A138510 n) (+ 1 (A006530 (A001358 n))))))))


(define (numbers-densely-distributed? lista)
   (cond ((null? lista) #t)
         ((null? (cdr lista)) #t)
         ((< (A000290 (car lista)) (cadr lista)) #f)
         (else (numbers-densely-distributed? (cdr lista)))
   )
)

(define A253784 (MATCHING-POS 1 1 (lambda (n) (numbers-densely-distributed? (ifactor n)))))

(define A253785 (MATCHING-POS 1 1 (lambda (n) (not (numbers-densely-distributed? (ifactor n))))))

(define (numbers-sparsely-distributed? lista)
   (cond ((null? lista) #t)
         ((null? (cdr lista)) #t)
         ((> (A000290 (car lista)) (cadr lista)) #f)
         (else (numbers-sparsely-distributed? (cdr lista)))
   )
)

(define A253569 (MATCHING-POS 1 1 (lambda (n) (and (> (A001222 n) 1) (numbers-sparsely-distributed? (ifactor n))))))
(define A253567 (MATCHING-POS 1 1 (lambda (n) (or (< (A001222 n) 2) (not (numbers-sparsely-distributed? (ifactor n)))))))

(define A253567v2 (COMPLEMENT 1 A253569))

(define (charfun_for_A245729 n) (if (and (> (A001222 n) 1) (> (A032742 n) (A000290 (A020639 n)))) (+ (A010051 (A032742 n)) (charfun_for_A245729 (A032742 n))) 0))

(define A245729 (NONZERO-POS 1 1 charfun_for_A245729))

(define A251728 (MATCHING-POS 1 2 (lambda (n) (and (= 2 (A001222 n)) (< (A006530 n) (A000290 (A020639 n)))))))
(define A251728v1 (MATCHING-POS 1 2 (lambda (n) (and (= 2 (A001222 n)) (< (A252375 n) (+ 1 (A006530 n)))))))
(define A251728v2 (COMPOSE A001358 (MATCHING-POS 1 1 (lambda (n) (<= (A138510 n) (A006530 (A001358 n)))))))

(define (A252370 n) (A243055 (A251726 n)))
(define (A252371 n) (A243055 (A251727 n)))

(define (A030078 n) (expt (A000040 n) 3)) ;; Cubes of primes.

(define (A030514 n) (expt (A000040 n) 4)) ;; 4th powers of primes. 

(define (A251720 n) (* (A000040 n) (A000040 n) (A000040 (+ 1 n))))

(define (A251720v2 n) (* (A000040 n) (A006094 n)))

(define (A251720v3 n) (* (A001248 n) (A000040 (+ n 1))))

(define (A250477 n) (A078898 (A251720 n)))

(define (A250478 n) (A078898 (A030514 n)))

(define (A005171 n) (- 1 (A010051 n)))
(define (A250480 n) (if (prime? n) n (- (A020639 n) 1)))

(define (A250480v2 n) (- (A020639 n) (A005171 n)))


(define (A070826 n) (/ (A002110 n) 2))

(define (A006093 n) (- (A000040 n) 1))
(define (A040976 n) (- (A000040 n) 2))

(define A071904 (MATCHING-POS 1 1 (lambda (n) (and (> n 1) (odd? n) (not (prime? n)))))) ;; Odd composite numbers.

(define A084345 (MATCHING-POS 1 0 (lambda (n) (not (prime? (A000120 n))))))

(define A027699 (MATCHING-POS 1 1 (lambda (n) (and (prime? n) (even? (A000120 n))))))

(define A255564 (MATCHING-POS 1 1 (lambda (n) (and (prime? n) (not (prime? (A000120 n)))))))


(definec (A003415 n) ;; a(n) = n' = arithmetic derivative of n: a(0) = a(1) = 0, a(prime) = 1, a(mn) = m*a(n) + n*a(m)
  (cond ((zero? n) 0)
        ((= 1 n) 0)
        ((prime? n) 1)
        (else
          (let* ((a (A020639 n))
                 (b (/ n a))
                )
             (+ (* a (A003415 b)) (* b (A003415 a)))
          )
        )
  )
)


;; Variant:
(definec (Auusi n)
  (cond ((zero? n) 0)
        ((= 1 n) 0)
        ((prime? n) (A000720 n))
        (else
          (let* ((a (A020639 n))
                 (b (/ n a))
                )
             (+ (* a (Auusi b)) (* b (Auusi a)))
          )
        )
  )
)


(define (A031215 n) (A000040 (* 2 n)))
(define (A031368 n) (A000040 (+ n n -1)))

(define (A036234 n) (+ 1 (A000720 n)))


(define A244990 (MATCHING-POS 1 1 (lambda (n) (even? (A061395 n))))) ;; 1 together with n for which A006530(n) is in A031215

(define A244991 (MATCHING-POS 1 1 (lambda (n) (odd? (A061395 n))))) ;; Numbers for which A006530(n) is in A031368

(define (A244992 n) (A000035 (A061395 n)))

(definec (A244989 n) (if (<= n 1) 0 (+ (A244992 n) (A244989 (-1+ n)))))
(define  (A244988 n) (- n (A244989 n)))



;; And entanglements:

;; (map A244990 (iota 12)) --> (1 3 6 7 9 12 13 14 18 19 21 24)

;; (map A244991 (iota 12)) --> (  2   4 5     8  10 11    15 16 17 20 22 23)

;; (map A244992 (iota 17)) --> (0 1 0 1 1 0 0 1 0 1 1 0 0 0 1 1 1)
;; (map A244988 (iota 17)) --> (1 1 2 2 2 3 4 4 5 5 5 6 7 8 8 8 8)
;; (map A244989 (iota 17)) --> (0 1 1 2 3 3 3 4 4 5 6 6 6 6 7 8 9)

;; (map A244321 (iota 32))
;; (1 2 3 4 6 5 7 8 9 12 10 13 11 15 14 16 18 17 19 24 25 20 26 21 22 27 23 31 29 30 28 32)
;; (same-intfuns1? A001477 (COMPOSE A244322 A244321) 1200)
;; (same-intfuns1? A001477 (COMPOSE A244321 A244322) 1200)

(definec (A244321 n) ;; A244991 -> evens, A244990 -> odds.
   (cond ((= 1 n) 1)
         ((= 1 (A244992 n)) (* 2 (A244321 (A244989 n))))
         (else (+ 1 (* 2 (A244321 (-1+ (A244988 n))))))
   )
)

;; (map A244322 (iota 32))
;; (1 2 3 4 6 5 7 8 9 11 13 10 12 15 14 16 18 17 19 22 24 25 27 20 21 23 26 31 29 30 28 32)
(definec (A244322 n) ;; odds -> A244990, evens -> A244991
   (cond ((= 1 n) 1)
         ((even? n) (A244991 (A244322 (/ n 2))))
         (else (A244990 (+ 1 (A244322 (/ (- n 1) 2)))))
   )
)

(define (A055037 n) (- n (A055038 n)))
(definec (A055038 n) (if (<= n 1) 0 (+ (A066829 n) (A055038 (-1+ n)))))

;; A245603-A245614 are now reserved for your use.

(define (A143691 n) (if (even? n) (A026424 (/ n 2)) (A028260 (/ (+ 1 n) 2))))

;; (define (A143692 n) (if (= 1 (A066829 n)) (* 2 (A055038 n)) (+ 1 (* 2 (-1+ (A055037 n))))))
(define (A143692 n) (if (= 1 (A066829 n)) (* 2 (A055038 n)) (-1+ (* 2 (A055037 n)))))

(definec (A245603 n) ;; A028260 -> odds, A026424 -> evens
   (cond ((= 1 n) 1)
         ((= 1 (A066829 n)) (* 2 (A245603 (A055038 n))))
         (else (+ 1 (* 2 (A245603 (-1+ (A055037 n))))))
   )
)

(definec (A245604 n) ;; odds -> A028260, evens -> A026424
   (cond ((= 1 n) 1)
         ((even? n) (A026424 (A245604 (/ n 2))))
         (else (A028260 (+ 1 (A245604 (/ (- n 1) 2)))))
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; a(1) = 1, a(2n) = 2 * a(A064989(2n-1)), a(2n-1) = 1 + (2 * a(A064989(2n-1)-1)).
(definec (A245605 n) ;; A243501 -> evens, A003961 -> odds.
   (cond ((= 1 n) 1)
         ((even? n) (* 2 (A245605 (A064989 (- n 1)))))
         (else (+ 1 (* 2 (A245605 (-1+ (A064989 n))))))
   )
)


;; a(1) = 1, a(2n) = 2 * a(A064216(n)), a(2n-1) = 1 + (2 * a(A064216(n)-1)).
;; (definec (A245605v2 n) ;; A243501 -> evens, A003961 -> odds.
;;    (cond ((= 1 n) 1)
;;          ((even? n) (* 2 (A245605v2 (A064216 (/ n 2)))))
;;          (else (+ 1 (* 2 (A245605v2 (-1+ (A064216 (/ (1+ n) 2)))))))
;;    )
;; )


;; At n=945, a(945) = A003961(107021) = p_ ? = 107033
(definec (A245606 n) ;; odds -> A003961, evens -> A243501
   (cond ((= 1 n) 1)
         ((even? n) (A243501 (A245606 (/ n 2))))
         (else (A003961 (+ 1 (A245606 (/ (- n 1) 2)))))
   )
)

(define (A245607 n) (A245605 (A064216 n)))
(define (A245608 n) (A048673 (A245606 n)))



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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interlude: Analogous permutations for odd primes (A065091) & their complement:

(definec (A257727 n)
   (cond ((<= n 2) n)
         ((= 1 (A010051 n)) (+ 1 (* 2 (A257727 (+ -1 (A000720 n))))))
         (else (* 2 (A257727 (A062298 n))))
   )
)


(definec (A257728 n)
   (cond ((< n 2) n)
         ((even? n) (A065090 (+ 1 (A257728 (/ n 2)))))
         (else (A065091 (A257728 (/ (- n 1) 2))))
   )
)

;; And cross-permutations:

(definec (A257729 n)
   (cond ((<= n 1) n)
         ((= 1 (A010051 n)) (A065091 (A257729 (A000720 n))))
         (else (A065090 (+ 1 (A257729 (A065855 n)))))
   )
)


(definec (A257730 n)
   (cond ((<= n 1) n)
         ((and (odd? n) (= 1 (A010051 n))) (A000040 (A257730 (+ -1 (A000720 n)))))
         (else (A002808 (A257730 (A062298 n))))
   )
)

(define (A257729v2 n) (A257728 (A246377 n)))

(define (A257730v2 n) (A246378 (A257727 n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(definec (A071574 n)
   (cond ((= 1 n) 0)
         ((= 1 (A010051 n)) (+ 1 (* 2 (A071574 (A000720 n)))))
         (else (* 2 (A071574 (+ 1 (A065855 n)))))
   )
)

(definec (A237739 n) ;; This is inverse permutation of A071574.
   (cond ((zero? n) 1)
         ((odd? n) (A000040 (A237739 (/ (- n 1) 2))))
         (else (A002808 (+ -1 (A237739 (/ n 2)))))
   )
)


(define (A246377v2 n) (A054429 (A135141 n)))
(define (A246377v3 n) (A135141 (A236854 n)))
(define (A246377v4 n) (A246376 (A246379 n)))

(define (A246378v2 n) (A227413 (A054429 n)))
(define (A246378v3 n) (A236854 (A227413 n)))
(define (A246378v4 n) (A246380 (A246375 n)))


(define A246346 (RECORD-POS 1 1 A135141))

(define A246347 (COMPOSE A135141 A246346))
;; Equally: (define (A246347 n) (A135141 (A246346 n)))


(definec (A246348 n)
   (cond ((= 1 n) 1)
         ((= 1 (A010051 n)) (+ 1 (A246348 (A000720 n))))
         (else (+ 1 (A246348 (A065855 n))))
   )
)

(define (A246348v2 n) (A070939 (A135141 n)))
(define (A246348v3 n) (+ 1 (A246369 n) (A246370 n)))
(define (A246348v4 n) (A070939 (A246377 n)))

(definec (A246369 n)
   (cond ((= 1 n) 0)
         ((= 1 (A010051 n)) (A246369 (A000720 n)))
         (else (+ 1 (A246369 (A065855 n))))
   )
)

(define (A246369v2 n) (-1+ (A000120 (A135141 n))))
(define (A246369v3 n) (- (A246348 n) (A246370 n) 1))
(define (A246369v4 n) (A080791 (A246377 n)))

(definec (A246370 n)
   (cond ((= 1 n) 0)
         ((= 1 (A010051 n)) (+ 1 (A246370 (A000720 n))))
         (else (A246370 (A065855 n)))
   )
)

(define (A246370v2 n) (A080791 (A135141 n)))
(define (A246370v3 n) (- (A246348 n) (A246369 n) 1))
(define (A246370v4 n) (-1+ (A000120 (A246377 n))))




;; A245701-A245712 are now reserved for your use.

(definec (A245701 n)
   (cond ((= 1 n) n)
         ((= 1 (A091225 n)) (* 2 (A245701 (A091226 n))))
         (else (+ 1 (* 2 (A245701 (A091245 n)))))
   )
)

(definec (A245702 n) ;; Cf. A227413
   (cond ((< n 2) n)
         ((even? n) (A014580 (A245702 (/ n 2))))
         (else (A091242 (A245702 (/ (- n 1) 2))))
   )
)

(definec (A245703 n)
   (cond ((= 1 n) n)
         ((= 1 (A010051 n)) (A014580 (A245703 (A000720 n))))
         (else (A091242 (A245703 (A065855 n))))
   )
)

(definec (A245704 n)
   (cond ((= 1 n) n)
         ((= 1 (A091225 n)) (A000040 (A245704 (A091226 n))))
         (else (A002808 (A245704 (A091245 n))))
   )
)

(define (A245703v2 n) (A245702 (A135141 n)))
(define (A245704v2 n) (A227413 (A245701 n)))

;; A246200-A246211 are now reserved for your use.

(definec (A246201 n)
   (cond ((= 1 n) n)
         ((= 1 (A091225 n)) (+ 1 (* 2 (A246201 (A091226 n)))))
         (else (* 2 (A246201 (A091245 n))))
   )
)

(definec (A246202 n) ;; Cf. A227413
   (cond ((< n 2) n)
         ((odd? n) (A014580 (A246202 (/ (- n 1) 2))))
         (else (A091242 (A246202 (/ n 2))))
   )
)


(definec (A246161 n)
   (cond ((= 1 n) n)
         ((= 1 (A091225 n)) (A000069 (+ 1 (A246161 (A091226 n)))))
         (else (A001969 (+ 1 (A246161 (A091245 n)))))
   )
)

(define (A246161v2 n) (A233280 (A245701 n)))

(definec (A246162 n)
   (cond ((= 1 n) n)
         ((= 1 (A010060 n)) (A014580 (A246162 (- (A115384 n) 1))))
         (else (A091242 (A246162 (A245710 n))))
   )
)

(define (A246162v2 n) (A245702 (A233279 n)))

(definec (A246163 n)
   (cond ((= 1 n) n)
         ((= 1 (A091225 n)) (A065621 (+ 1 (A246163 (A091226 n)))))
         (else (A048724 (A246163 (A091245 n))))
   )
)


(definec (A246164 n)
   (cond ((= 1 n) n)
         ((= 1 (A010060 n)) (A014580 (A246164 (- (A246160 n) 1))))
         (else (A091242 (A246164 (A246159 n))))
   )
)

(definec (A246164v2 n)
   (cond ((= 1 n) n)
         ((= 1 (A010060 n)) (A014580 (A246164v2 (- (A065620 n) 1))))
         (else (A091242 (A246164v2 (- (A065620 n)))))
   )
)

(define (A246163v2 n) (A193231 (A246201 n)))
(define (A246164v3 n) (A246202 (A193231 n)))


(define (A246203 n) (A246201 (A193231 n))) ;; XXX - No recurrence for
(define (A246204 n) (A193231 (A246202 n))) ;; XXX - these two yet found!


(definec (A246205 n)
   (cond ((= 1 n) n)
         ((= 1 (A091225 n)) (A117968 (A246205 (A091226 n))))
         (else (A117967 (+ 1 (A246205 (A091245 n)))))
   )
)

(definec (A246206 n)
   (cond ((= 1 n) n)
         ((negative? (A117966 n)) (A014580 (A246206 (- (A117966 n)))))
         (else (A091242 (A246206 (- (A117966 n) 1))))
   )
)


(definec (A246207 n)
   (cond ((<= n 1) n)
         ((even? n) (A117968 (A246207 (/ n 2))))
         (else (A117967 (+ 1 (A246207 (/ (- n 1) 2)))))
   )
)

(definec (A246208 n)
   (cond ((<= n 1) n)
         ((negative? (A117966 n)) (* 2 (A246208 (- (A117966 n)))))
         (else (+ 1 (* 2 (A246208 (- (A117966 n) 1)))))
   )
)


(definec (A246209 n)
   (cond ((<= n 1) n)
         ((odd? n) (A117968 (A246209 (/ (- n 1) 2))))
         ((even? n) (A117967 (+ 1 (A246209 (/ n 2)))))
   )
)

(definec (A246210 n)
   (cond ((<= n 1) n)
         ((negative? (A117966 n)) (+ 1 (* 2 (A246210 (- (A117966 n))))))
         (else (* 2 (A246210 (- (A117966 n) 1))))
   )
)

(define (A246209v2 n) (A246207 (A054429 n)))

(define (A246210v2 n) (A054429 (A246208 n)))


(definec (A246211 n)
   (cond ((<= n 1) n)
         ((negative? (A117966 n)) (A117967 (+ 1 (A246211 (- (A117966 n))))))
         (else (A117968 (A246211 (- (A117966 n) 1))))
   )
)


(definec (Asolmu n) ;; XXX - & inverse.
   (cond ((<= n 1) n)
         ((negative? (A065620 n)) (A117967 (+ 1 (Asolmu (- (A065620 n))))))
         (else (A117968 (Asolmu (- (A065620 n) 1))))
   )
)


(define (A244987 n) (A245704 (A193231 (A245703 n))))
(define (A245450 n) (A245704 (A057889 (A245703 n))))
(define (A245453 n) (A235042 (A057889 (A235041 n)))) ;; Cf. A234748, A245450

;; (map A007097 (iota0 8)) --> (1 2 3 5 11 31 127 709 5381)
;; (map A091230 (iota0 7)) --> (1 2 3 7 25 137 1123 13103)
;; (map A091204 (map A007097 (iota0 7))) --> (1 2 3 7 25 137 1123 13103)
;; (map A245703 (map A007097 (iota0 7))) --> (1 2 3 7 25 137 1123 13103)
;; 
;; (map A091205 (map A091230 (iota0 7))) --> (1 2 3 5 11 31 127 709)
;; (map A245704 (map A091230 (iota0 7))) --> (1 2 3 5 11 31 127 709)


;; A245812-A245823 are now reserved for your use.

(define (A245813 n) (if (<= n 1) n (A062298 (A091205 (A091242 (- n 1))))))
(define (A245814 n) (+ 1 (A091245 (A091204 (A018252 n)))))

(define (A245815 n) (A062298 (A245821 (A018252 n))))
(define (A245816 n) (A062298 (A245822 (A018252 n))))

(define (A245819 n) (+ 1 (A091245 (A245703 (A018252 n)))))
(define (A245819v2 n) (if (<= n 1) n (+ 1 (A245703 (- n 1)))))
(define (A245819v3 n) (+ 1 (A091226 (A245703 (A008578 n)))))

(define (A245820 n) (if (<= n 1) n (A062298 (A245704 (A091242 (- n 1))))))
(define (A245820v2 n) (if (<= n 1) n (+ 1 (A245704 (- n 1)))))
(define (A245820v3 n) (if (<= n 1) n (A036234 (A245704 (A014580 (- n 1))))))


;; (same-intfuns1?  (COMPOSE A245813 A245819) A245815 1024)

;; Note that:
;; (same-intfuns1? A245817 (COMPOSE A245817 A000040) 128) --> #t
;; (same-intfuns1? A245818 (COMPOSE A245818 A000040) 128) --> #t
;; (same-intfuns1? A245817 (COMPOSE A245817 A000040 A000040) 42) --> #t, etc.

(define (A245817 n) (- (A061775 (A245821 n)) (A061775 n)))
(define (A245818 n) (- (A061775 (A245822 n)) (A061775 n)))

(define (A245821 n) (A091205 (A245703 n)))
(define (A245822 n) (A245704 (A091204 n)))
(define A245823 (FIXED-POINTS 1 1 A245821)) ;; A007097 is a subsequence.


(define (A245707 n) (* (/ 1 2) (+ 1 (A245605 (-1+ (* 2 n))))))

(define (A245707v2 n) (if (= 1 n) n (+ 1 (A245605 (-1+ (A064216 n))))))

;; (same-intfuns0? (COMPOSE A245608 A000079) (COMPOSE A245708 A000079) 12) --> #t

(define (A245708 n) (* (/ 1 2) (+ 1 (A245606 (-1+ (* 2 n))))))

(define (A245708v2 n) (if (= 1 n) n (A048673 (+ 1 (A245606 (- n 1))))))

(define (A245705 n) (A245607 (A245708 n)))
(define (A245706 n) (A245707 (A245608 n)))

(define (A245711 n) (/ (+ 1 (A245705 (+ n n -1))) 2))
(define (A245712 n) (/ (+ 1 (A245706 (+ n n -1))) 2))

(define A245709 (FIXED-POINTS 1 1 A245705)) ;; Cf. A029747
(define A245709v2 (MATCHING-POS 1 1 (lambda (k) (= (A245608 k) (A245708 k)))))


(definec (A244319 n) ;; A243501 -> A003961, A003961 -> A243501.
  (cond ((= 1 n) 1)
        ((even? n) (A003961 (+ 1 (A244319 (A064989 (- n 1))))))
        (else (A243501 (A244319 (-1+ (A064989 n)))))
  )
)

(define (A245609 n) (A244319 (A064216 n)))
(define (A245610 n) (A048673 (A244319 n)))


;; (map A244319 (iota 32))
;; 1,3,2,9,6,5,26,11,4,21,8,125,56,25,16,15,344,115,36,1015,10,39,204,41,14,7,52,45,86,301,176,155


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (map A244152 (iota 32))
;; (1 4 10 2 24 7 6 55 18 3 16 15 121 44 12 11 39 9 36 35 105 31 250 5 29 28 93 26 25 86 22 82)
(definec (A244152 n) ;; 1 -> 1, A026424 -> A028260(n+1), A028260(n+1) -> A026424
   (cond ((= 1 n) 1)
         ((= 1 (A066829 n)) (A028260 (+ 1 (A244152 (A055038 n)))))
         (else (A026424 (A244152 (-1+ (A055037 n)))))
   )
)


;; (same-intfuns1? A245613 (COMPOSE A244322 A245603) 8192) --> #t
(definec (A245613 n) ;; A026424 -> A244991, A028260 -> A244990
   (cond ((= 1 n) 1)
         ((= 1 (A066829 n)) (A244991 (A245613 (A055038 n))))
         (else (A244990 (+ 1 (A245613 (-1+ (A055037 n))))))
   )
)

;; (same-intfuns1? A245614 (COMPOSE A245604 A244321) 8192) --> #t
(definec (A245614 n) ;; A244991 -> A026424, A244990 -> A028260
   (cond ((= 1 n) 1)
         ((= 1 (A244992 n)) (A026424 (A245614 (A244989 n))))
         (else (A028260 (+ 1 (A245614 (-1+ (A244988 n))))))
   )
)



;; (map Asomething_else_is_it_permutation_really (iota 32))
;; (1 2 3 4 6 5 7 8 12 11 9 10 13 15 14 16 24 23 21 22 18 17 19 20 26 25 28 31 27 30 29 32)
(definec (Asomething_else_is_it_permutation_really n) ;; odds -> A244990, evens -> A244991
   (cond ((= 1 n) 1)
         ((even? n) (A244991 (Asomething_else_is_it_permutation_really (/ n 2))))
         (else (A244990 (Asomething_else_is_it_permutation_really (/ (+ n 1) 2))))
   )
)

(define (A066829 n) (A000035 (A001222 n)))

(define (A008836 n) (expt -1 (A001222 n))) ;; Liouville's function lambda(n) = (-1)^bigomega(n).

(define A028260 (MATCHING-POS 1 1 (lambda (n) (even? (A001222 n)))))
(define A026424 (MATCHING-POS 1 1 (lambda (n) (odd? (A001222 n)))))

(define (A105560 n) (A008578 (+ 1 (A001222 n))))
;; (define (A105560 n) (if (<= n 1) n (A000040 (A001222 n)))) ;; Offset 2 really. Should be nice with a(1) = 1.

;; (define (A243054 n) (if (zero? n) 1 (* (/ 1 2) (A006530 (A002110 n)) (A002110 n))))
(define (A243054 n) (if (zero? n) 1 (* (/ 1 2) (A000040 n) (A002110 n)))) ;; Fixed points of A243051.
(define (A243054v2 n) (if (<= n 1) (+ n 1) (* (A006530 (A070826 n)) (A070826 n))))


(define (A008966 n) ((lambda (x) (* x x)) (A008683 n))) ;; Where A008683 = Moebius mu.

(define A005117 (NONZERO-POS 1 1 A008966))
(define A013929 (ZERO-POS 1 1 A008966))

(definec (A013928 n) (if (< n 2) 0 (+ (A013928 (- n 1)) (A008966 (- n 1)))))

(define (A057627 n) (- n (A013928 (+ n 1))))

;; A246342-A246353 are now reserved for your use.


(define (A019565 n)
  (let loop ((n n) (i 1) (p 1))
       (cond ((zero? n) p)
             ((odd? n) (loop (/ (- n 1) 2) (+ 1 i) (* p (A000040 i))))
             (else (loop (/ n 2) (+ 1 i) p))
       )
  )
)

(definec (A246353 n) ;; a(n) = A064273(n) + 1. Starts from zero.
  (let loop ((n n) (i 1) (p 1))
       (cond ((zero? n) (A013928 (+ 1 p)))
             ((odd? n) (loop (/ (- n 1) 2) (+ 1 i) (* p (A000040 i))))
             (else (loop (/ n 2) (+ 1 i) p))
       )
  )
)

(define (A246353v2 n) (A013928 (+ 1 (A019565 n))))
(define (A246353v3 n) (+ 1 (A013928 (A019565 n))))

(definec (A064273 n) ;; a(n) = A246353(n) - 1. Starts from zero.
  (let loop ((n n) (i 1) (p 1))
       (cond ((zero? n) (- (A013928 (+ 1 p)) 1))
             ((odd? n) (loop (/ (- n 1) 2) (+ 1 i) (* p (A000040 i))))
             (else (loop (/ n 2) (+ 1 i) p))
       )
  )
)

(define (A064273v2 n) (A013928 (A019565 n)))

(define (A064273v3 n) (- (A246353 n) 1))

;; A243343-A243354 are now reserved for your use.

;; A005117: 1, 2, 3, 5, 6, 7, 10, 11, 13, 14, 15, 17, 19, 21, 22,
;; A013929: 4, 8, 9, 12, 16, 18, 20, 24, 25, 27, 28, 32, 36, 40, 44,

(define (A088610 n) (if (even? n) (A013929 (/ n 2)) (A005117 (/ (+ 1 n) 2))))

(definec (A243352 n)
   (if (zero? (A008966 n))
       (* 2 (A057627 n)) ;; If n is "squareful"
       (+ (* 2 (A013928 n)) 1) ;; nth squarefree to nth odd.
   )
)


;; (map A243343 (iota 32))
;; (1 3 7 2 15 5 31 6 14 11 63 4 13 29 23 30 127 10 9 62 27 59 47 12 28 61 22 126 255 21 19 8)

;; (map A243344 (iota 32))
;; (1 4 2 12 6 8 3 32 19 18 10 24 13 9 5 84 53 50 31 49 30 27 15 63 38 36 21 25 14 16 7 220)

(definec (A243343 n) ;; squarefrees to odds, "squarefuls" to evens
   (cond ((<= n 1) n)
         ((zero? (A008966 n)) (* 2 (A243343 (A057627 n)))) ;; If n is "squareful"
         (else (+ (* 2 (A243343 (- (A013928 (+ n 1)) 1))) 1)) ;; nth squarefree larger than 1
   )
)


(definec (A243344 n) ;; Evens to "squarefuls", Odds to Squarefrees.
   (cond ((<= n 1) n)
         ((even? n) (A013929 (A243344 (/ n 2))))
         (else (A005117 (+ 1 (A243344 (/ (- n 1) 2)))))
   )
)


;; A005117: 1, 2, 3, 5, 6, 7, 10, 11, 13, 14, 15, 17, 19, 21, 22,
;; A013929: 4, 8, 9, 12, 16, 18, 20, 24, 25, 27, 28, 32, 36, 40, 44,

(definec (A243345 n) ;; squarefrees to evens (after 1), "squareful" to odds >= 3
   (cond ((<= n 1) n)
         ((zero? (A008966 n)) (+ 1 (* 2 (A243345 (A057627 n))))) ;; If n is "squareful"
         (else (* 2 (A243345 (A013928 n)))) ;; squarefree
   )
)

;;       (else (* 2 (A243345 (- (A013928 (+ n 1)) 1)))) ;; squarefree (Optimized!)

(definec (A243346 n) ;; Evens to squarefree > 1, Odds to "Squareful".
   (cond ((<= n 1) n)
         ((even? n) (A005117 (+ 1 (A243346 (/ n 2)))))
         (else (A013929 (A243346 (/ (- n 1) 2))))
   )
)

;; Self-inverse: 1,4,12,2,32,8,84,6,19,24,220,3,18,50,63,53,564,13,9,138,49,128,162,10,31,136,38,365,1448,36,25,5
(definec (A243347 n) ;; 1 -> 1, other squarefrees to "squareful", "squarefuls" to squarefrees > 1.
   (cond ((<= n 1) n)
         ((zero? (A008966 n)) (A005117 (+ 1 (A243347 (A057627 n)))))
         (else (A013929 (A243347 (A013928 n))))
   )
)

;; Older version:
;; (definec (A243347 n) ;; 1 -> 1, other squarefrees to "squareful", "squarefuls" to squarefrees > 1.
;;    (cond ((<= n 1) n)
;;          ((zero? (A008966 n)) (A005117 (+ 1 (A243347 (A057627 n)))))
;;          (else (A013929 (A243347 (- (A013928 (+ n 1)) 1)))) ;; Can be optimized!
;;    )
;; )

(define (A088609 n)
   (cond ((<= n 1) n)
         ((zero? (A008966 n)) (A005117 (+ 1 (A057627 n))))
         (else (A013929 (A013928 n)))
   )
)


(definec (A067029 n)
  (if (< n 2)
      0
      (let ((mp (A020639 n)))
        (let loop ((e 0) (n (/ n mp)))
          (cond ((integer? n) (loop (+ e 1) (/ n mp)))
                (else e)
          )
        )
      )
  )
)

(define (A032742 n) (/ n (A020639 n))) ;; a(1) = 1; for n > 1, a(n) = largest proper divisor of n.

(define (A060681 n) (- n (A032742 n))) ;; Our version starts from n=1, with a(1) = 0.

(definec (A064097 n) (if (= 1 n) 0 (+ 1 (A064097 (A060681 n)))))


(define (A006530 n) (if (< n 2) n (last (ifactor n)))) ;; Gpf(n): greatest prime dividing n (with a(1)=1). 

(define (A066048 n) ( * (A020639 n) (A006530 n))) ;; Product of smallest and greatest prime factors of n.

(define (A052126 n)  (/ n (A006530 n)))

(definec (A076271 n) (if (= 1 n) n (+ (A076271 (- n 1)) (A006530 (A076271 (- n 1))))))

(define (A028233 n) (expt (A020639 n) (A067029 n)))

(define (A028234 n) (/ n (A028233 n)))

(definec (A048675 n)
  (cond ((= 1 n) (- n 1))
        (else (+ (A000079 (- (A055396 n) 1))
                 (A048675 (A032742 n))
              )
        )
  )
)

(definec (A048675v2 n)
  (cond ((= 1 n) (- n 1))
        (else (+ (* (A067029 n) (A000079 (- (A055396 n) 1)))
                 (A048675v2 (A028234 n))
              )
        )
  )
)

;; Even-indexed terms equal the preceding term plus its largest prime factor, odd-indexed terms equal the preceding term plus its smallest prime factor, a(1)=2. 
(definec (A256393 n)
  (cond ((= 1 n) 2)
        ((even? n) (+ (A256393 (- n 1)) (A006530 (A256393 (- n 1)))))
        (else (+ (A256393 (- n 1)) (A020639 (A256393 (- n 1)))))
  )
)

(define (A257244 n) (- (A256393 (+ n 1)) (A256393 n)))
(define (A257244v2 n) (gcd (A256393 (+ n 1)) (A256393 n)))

(define (A257245 n) (A257244 (+ n n -1)))
(define (A257245v2 n) (A006530 (A256393 (+ n n -1))))

(define (A257246 n) (A257244 (+ n n)))
(define (A257246v2 n) (A020639 (A256393 (+ n n))))

(define A257247 (MATCHING-POS 1 1 (lambda (n) (= (A257245 n) (A257246 n)))))
(define A257247v2 (MATCHING-POS 1 1 (lambda (n) (= (gcd (A256393 (+ -1 n n)) (A256393 (+ n n))) (gcd (A256393 (+ n n)) (A256393 (+ 1 n n)))))))
(define A257247v3 (MATCHING-POS 1 1 (lambda (n) (= (A006530 (A256393 (+ n n -1))) (A020639 (A256393 (+ n n)))))))


(define (A055396 n) (A049084 (A020639 n)))

(definec (A061395 n) (if (= 1 n) 0 (A049084 (last (ifactor n)))))

(define (A159081 n) (+ 1 (A061395 n)))

(definec (A203623 n) (if (< n 2) 0 (+ (A061395 n) (A203623 (- n 1))))) ;; Partial sums of A061395. (XXX: Cf. A022559)


(define (A249808bi row col) (if (= 1 row) 0 (+ (A249808bi (- row 1) col) (if (= (A055396 row) col) 1 0))))

(define (A249808 n) (A249808bi (A002024 n) (A002260 n))) ;; tabl, triangular

(define (A249809 n) (A249808bi (A249728 n) (A249727 n))) ;; Tabf, offset = 2.



(definec (A249738 n)
   (cond ((or (= 1 n) (prime? n)) 0)
;;       ((and (= 1 (A010052 n)) (prime? (A000196 n))) 1) ;; Prime squares result 1 (in any case)
         (else
           (let ((lpf (A020639 n)))
             (let loop ((k (- (A032742 n) 1))) ;; k = A032742(n)-1, k * lpf = n - lpf.
                (cond ((= 1 k) k)
                      ((>= (A020639 k) lpf) k)
                      (else (loop (- k 1)))
                )
             )
           )
         )
   )
)

(define (A249744 n) (* (A249738 n) (A020639 n)))

(definec (A078898 n) (if (< n 2) n (+ 1 (A078898 (A249744 n)))))
(definec (A078898v0 n) (if (or (= 1 n) (prime? n)) 1 (+ 1 (A078898v0 (A249744 n)))))

(definec (A078898v1 n)
   (cond ((or (= 1 n) (prime? n)) 1)
;;       ((even? n) (/ n 2))
         (else (+ 1 (A078898v1 (* (A249738 n) (A020639 n)))))
   )
)

(definec (A078898slow n) (if (= n 1) n (A249808bi n (A055396 n))))

(definec (A078898v2 n)
   (cond ((or (= 1 n) (prime? n)) 1)
         ((even? n) (/ n 2))
         ((and (= 1 (A010052 n)) (prime? (A000196 n))) 2) ;; Prime squares get value 2.
         (else
           (let ((lpf (A020639 n)))
             (let loop ((k (- (A032742 n) 1))) ;; k = A032742(n)-1, k * lpf = n - lpf.
                (cond ((= 1 k) 1)
                      ((>= (A020639 k) lpf) (+ 1 (A078898v2 (* k lpf))))
                      (else (loop (- k 1)))
                )
             )
           )
         )
   )
)

(definec (A078898v3 n) (cond ((<= n 1) n) ((even? n) (/ n 2)) (else (A078898v3 (A250470 n))))) ;; Cf. A246277
 
;; This might work with the help of some permutation:
;;
;; (definec (A078898 n)
;;   (cond ((= 1 n) n)
;;         ((prime? n) 1)
;;         (else (- (A032742 n) (add (lambda (i) (A078898 (- n (modulo n (A000040 i))))) 1 (- (A055396 n) 1))))
;;   )
;; )


(definec (A251717 n) (let loop ((i 1)) (if (<= (A001222 (A083221bi i n)) 2) i (loop (+ i 1)))))

(definec (A251718 n)
  (let loop ((i 1))
        (if (and (<= (A001222 (A083221bi i n)) 2) (<= (A001222 (A083221bi (+ i 1) n)) 2))
            i
            (loop (+ i 1))
        )
  )
)

(definec (A251719 n) (let loop ((k 1)) (if (> (A250474 k) n) k (loop (+ 1 k)))))


(definec (A251719v2 n)
  (if (= 1 n)
      1
      (let loop ((i 1) (eka (A083221bi 1 n)) (toka (A083221bi 2 n)))
           (if (and (= (A001222 eka) 2)
                    (= toka (A003961 eka))
               )
               i
               (loop (+ i 1) toka (A083221bi (+ i 2) n))
           )
      )
  )
)

;; (definec (A251719old n)
;;   (if (= 1 n)
;;       1
;;       (let loop ((i 1) (eka (A083221bi 1 n)) (toka (A083221bi 2 n)))
;;            (if (and (= (A001222 eka) 2)   ;; Both ...
;;                     (= (A001222 toka) 2)  ;;  ... semiprimes
;;                     (= (A243056 eka) (A243056 toka)) ;; with the same distance between spf and gpf
;;                     (= (A055396 toka) (+ 1 (A055396 eka))) ;; and the latter shifted by one step?
;;                )
;;                i
;;                (loop (+ i 1) toka (A083221bi (+ i 2) n))
;;            )
;;       )
;;   )
;; )
;; 


(define (A249810 n) (if (= 1 n) 0 (A078898 (A003961 n))))
(define (A249820 n) (if (= 1 n) 0 (- (A249810 n) (A078898 n)))) ;; Not: (A246277 n)

;; n>1 appears A061395(n) times, where A061395 gives the index of the largest prime factor of n, a(1)=1.
(define A241920 (COMPOSE-FUNS (LEAST-GTE-I 0 1 A203623) -1+)) ;; Cf. A082288

;; Exponent of the largest prime factor of n.
(definec (A071178 n)
  (if (< n 2)
      0
      (let ((mp (A006530 n)))
        (let loop ((e 0) (n (/ n mp)))
          (cond ((integer? n) (loop (+ e 1) (/ n mp)))
                (else e)
          )
        )
      )
  )
)

(define (A053585 n) (expt (A006530 n) (A071178 n)))
 
(define (A051119 n) (/ n (A053585 n)))

(define (A243074 n) (* (A051119 n) (A006530 n)))

(definec (A065338 n)
  (cond ((<= n 1) n)
        ((prime? n) (modulo n 4))
        (else (apply * (map A065338 (ifactor n))))
  )
)

(define (A065339v2 n) (A007949 (A065338 n))) ;; Number of primes congruent to 3 modulo 4 dividing n (with multiplicity).


(definec (A065339 n) (cond ((< n 3) 0) ((even? n) (A065339 (/ n 2))) (else (+ (/ (- (modulo (A020639 n) 4) 1) 2) (A065339 (A032742 n))))))

(definec (A083025 n) (cond ((< n 3) 0) ((even? n) (A083025 (/ n 2))) (else (+ (/ (- 3 (modulo (A020639 n) 4)) 2) (A083025 (A032742 n))))))


(definec (A065339r2 n) ;; Add exponents of 4k+3 primes (of prime factorization of n) together.
    (cond ((< n 3) 0)
          ((even? n) (A065339r2 (/ n 2)))
          ((= 1 (modulo (A020639 n) 4)) (A065339r2 (A032742 n)))
          (else (+ (A067029 n) (A065339r2 (A028234 n))))
    )
)

(define A004613 (MATCHING-POS 1 1 (lambda (k) (= 1 (A065338 k)))))


(definec (A260728 n) ;; (Bitwise-) Or exponents of 4k+3 primes (of prime factorization of n) together.
    (cond ((< n 3) 0)
          ((even? n) (A260728 (/ n 2)))
          ((= 1 (modulo (A020639 n) 4)) (A260728 (A032742 n)))
          (else (A003986bi (A067029 n) (A260728 (A028234 n))))
    )
)

(definec (A097706 n) ;; Part of n composed of prime factors of form 4k+3.
    (cond ((< n 3) 1)
          ((even? n) (A097706 (/ n 2)))
          ((= 1 (modulo (A020639 n) 4)) (A097706 (A032742 n)))
          (else (* (A028233 n) (A097706 (A028234 n))))
    )
)


(definec (A_mieti_uudestaan_apu n) ;; (Bitwise-) and exponents of 4k+3 primes (of prime factorization of n) together.
    (cond ((< n 3) 0)
          ((even? n) 0)
          ((= 1 (modulo (A020639 n) 4)) 0)
          ((= 1 (A028234 n)) (A067029 n))
          (else (A004198bi (A067029 n) (A_mieti_uudestaan_apu (A028234 n))))
    )
)

(define (A_mieti_uudestaan_ n) (A_mieti_uudestaan_apu (A097706 n))) ;; (Bitwise-) and exponents of 4k+3 primes (of prime factorization of n) together.


(define A260730 (MATCHING-POS 1 1 (lambda (n) (> (A065339 n) (A260728 n)))))
;; (define A260730 (NONZERO-POS 1 1 (lambda (n) (- (A065339 n) (A260728 n)))))


(define (A229062 n) (- 1 (A000035 (A260728 n)))) ;; 1 if n is representable as sum of two nonnegative squares. 
;; Characteristic function of A001481.

(define (isA001481? n) (even? (A260728 n)))

(define A001481 (MATCHING-POS 1 0 isA001481?)) ;; Numbers that are the sum of 2 squares. 


(define (isA022544? n) (odd? (A260728 n)))

(define A022544 (MATCHING-POS 1 1 isA022544?)) ;; Numbers that are not the sum of 2 squares. 

(define (A072401 n) (cond ((zero? n) 0) ((and (even? (A007814 n)) (= 7 (modulo (A000265 n) 8))) 1) (else 0))) ;; 1 iff n is of the form 4^m*(8k+7). 


(define A004215 (ZERO-POS 1 1 (COMPOSE -1+ A072401)))

;; Least number of squares that add up to n. This formula after _Charles R Greathouse IV_'s Jul 19 2011 PARI-code:
(define (A002828 n) (cond ((zero? n) n) ((= 1 (A010052 n)) 1) ((= 1 (A229062 n)) 2) (else (+ 3 (A072401 n)))))

(define (A255131 n) (- n (A002828 n)))

(definec (A262689 n) (if (= 1 (A010052 n)) (A000196 n) (let ((t (- (A002828 n) 1))) (let loop ((k (A000196 n))) (if (= t (A002828 (- n (* k k)))) k (loop (- k 1)))))))


(definec (A262689v2 n)
 (let ((k (A000196 n)))
   (if (= 1 (A010052 n))
       k
       (let loop ((k k) (m #f) (mk #f))
          (cond ((zero? k) mk)
                (else
                   (let* ((c (A002828v2 (- n (* k k)))))
                     (if (or (not m) (< c m)) (loop (- k 1) c k) (loop (- k 1) m mk))
                   )
                )
          )
       )
   )
 )
)

(definec (A002828v2 n) (if (zero? n) n (+ 1 (A002828v2 (- n (A000290 (A262689v2 n)))))))


(define (A262690 n) (A000290 (A262689 n))) ;; Largest square k <= n such that A002828(n-k) = A002828(n)-1.

(define (A262678 n) (- n (A262690 n)))

(definec (A260731 n) (if (zero? n) n (+ 1 (A260731 (A255131 n)))))

(definec (A260732 n) (if (<= n 1) n (+ (A260734 (- n 1)) (A260732 (- n 1)))))
(define (A260732v2 n) (A260731 (* n n))) ;; Zero-based.


(definec (A260733 n) (if (= 1 n) 0 (+ (A260734 (- n 1)) (A260733 (- n 1)))))
(define (A260733v2 n) (A260731 (- (* n n) 1))) ;; One-based.


;; How many steps from (A000290(n+1))-1 to (n^2)-1 by iterating A255131(n) ? Cf. also A213709, A261091, A261224, A261234
 
(definec (A260734 n) ;; One-based.
 (let ((end (- (A000290 n) 1)))
  (let loop ((k (- (A000290 (+ 1 n)) 1)) (s 0))
        (if (= k end) s (loop (A255131 k) (+ 1 s)))
  )
 )
)

(define (A260734v1 n) (- (A260732 (+ 1 n)) (A260732 n)))
(define (A260734v2 n) (- (A260733 (+ 1 n)) (A260733 n))) ;; One-based.
(define (A260734v3 n) (- (A260731 (- (A000290 (+ 1 n)) 1)) (A260731 (- (A000290 n) 1))))
(define (A260734v4 n) (- (A260731 (A000290 (+ 1 n))) (A260731 (A000290 n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A260740v2 n) (- n (A053610 n)))
(definec (A260740 n) (if (zero? n) n (+ -1 (A048760 n) (A260740 (- n (A048760 n))))))

 ;; A261216-A261237 are now reserved for your use. 

(definec (A261221 n) (if (zero? n) n (+ 1 (A261221 (A260740 n)))))


(definec (A261222 n) (if (<= n 1) n (+ (A261224 (- n 1)) (A261222 (- n 1)))))
(define (A261222v2 n) (A261221 (* n n))) ;; Zero-based.


(definec (A261223 n) (if (= 1 n) 0 (+ (A261224 (- n 1)) (A261223 (- n 1)))))
(define (A261223v2 n) (A261221 (- (* n n) 1))) ;; One-based.


;; How many steps from (A000290(n+1))-1 to (n^2)-1 by iterating A260740(n) ? Cf. also A213709, A261091, A260734, A261234
 
(definec (A261224 n) ;; One-based.
 (let ((end (- (A000290 n) 1)))
  (let loop ((k (- (A000290 (+ 1 n)) 1)) (s 0))
        (if (= k end) s (loop (A260740 k) (+ 1 s)))
  )
 )
)

(define (A261224v2 n) (- (A261223 (+ 1 n)) (A261223 n))) ;; One-based.
(define (A261224v3 n) (- (A261221 (A000290 (+ 1 n))) (A261221 (A000290 n))))


;;;;;;;;
;; For cube-beanstalks:

(definec (A261225 n) (if (zero? n) n (+ -1 (A048762 n) (A261225 (- n (A048762 n))))))
(define (A261225v2 n) (- n (A055401 n)))

;; Not A003108 Number of partitions of n into cubes (!)
(definec (A261226 n) (if (zero? n) n (+ 1 (A261226 (A261225 n)))))


(definec (A261227 n) (if (<= n 1) n (+ (A261229 (- n 1)) (A261227 (- n 1)))))
(define (A261227v2 n) (A261226 (* n n n))) ;; Zero-based.


(definec (A261228 n) (if (= 1 n) 0 (+ (A261229 (- n 1)) (A261228 (- n 1)))))
(define (A261228v2 n) (A261226 (- (* n n n) 1))) ;; One-based.


;; How many steps from ((n+1)^3)-1 to (n^3)-1 by iterating A261225(n) ? Cf. also A261224.
 
(definec (A261229 n) ;; One-based.
 (let ((end (- (A000578 n) 1)))
  (let loop ((k (- (A000578 (+ 1 n)) 1)) (s 0))
        (if (= k end) s (loop (A261225 k) (+ 1 s)))
  )
 )
)

(define (A261229v2 n) (- (A261228 (+ 1 n)) (A261228 n))) ;; One-based.
(define (A261229v3 n) (- (A261226 (A000578 (+ 1 n))) (A261226 (A000578 n))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definec (A057521 n)
   (fold-left (lambda (prod p.e) (* prod (expt (car p.e) (cdr p.e))))
              1
              (if (= 1 n) (list) (remove (lambda (p.e) (= (cdr p.e) 1)) (elemcountpairs (sort (factor n) <))))
   )
)

(define A240370 (MATCHING-POS 1 1 (lambda (k) (= 1 (A065338 (A057521 k))))))

(define (A132739 n) (apply * (delete 5 (ifactor n))))

;; (define A193304 (NONZERO-POS 1 1 (COMPOSE A008683 A132739)))
(define A193304 (NONZERO-POS 1 1 (lambda (k) (A008683 (A132739 k)))))

;; Fully multiplicative with a(p(k)) = p(k+1) for k-th prime p(k).
(definec (A003961 n) (apply * (map A000040 (map 1+ (map A049084 (ifactor n))))))
(define (A003961fast n) (if (= 1 n) n (apply * (map A000040 (map 1+ (map A049084fast (factor n)))))))

(definec (A048673 n) (/ (+ 1 (A003961 n)) 2))

;; (define (A048673v2 n) (if (= 1 n) n (A245708 (1+ (A245605 (-1+ n)))))) ;; For testing.


(definec (A048673v2 n)
  (cond ((= 1 n) n)
        ((even? n) (+ -1 (* 3 (A048673v2 (/ n 2)))))
        (else (A032766 (A249746 (/ (+ 1 n) 2))))
  )
)

(definec (A254115 n)
  (cond ((<= n 1) n)
        ((even? n) (* 2 (A254115 (/ n 2))))
        (else (+ 1 (* 2 (A254104 (+ -1 (A249746 (/ (+ 1 n) 2)))))))
  )
)

(define (Ainv_of_A007310off0 n) (+ (* 2 (floor->exact (/ n 6))) (/ (- (modulo n 6) 1) 4)))

(definec (A254115v0 n)
  (cond ((<= n 1) n)
        ((even? n) (* 2 (A254115v0 (/ n 2))))
        (else (+ 1 (* 2 (A254104 (Ainv_of_A007310off0 (A003961 n))))))
  )
)


(define (A254115v2 n) (A254104 (A048673 n)))

(define (A254116 n) (A064216 (A254103 n)))

(define (A254117 n) (A254104 (+ -1 (A249746 (+ 1 n)))))

(define (A254117v2 n) (/ (+ -1 (A254115 (+ 1 n n))) 2))

(define (A254118 n) (+ -1 (A249745 (+ 1 (A254103 n)))))

(define (A254118v2 n) (/ (+ -1 (A254116 (+ 1 n n))) 2))

(define (A254120 n) (A254118 (A000079 n)))


(define A254099 (FIXED-POINTS 1 1 A254115))


(define (A252461 n) (if (= 1 n) n (* (A008578 (A055396 n)) (A032742 n))))

(define (A252462 n) (if (= 1 n) n (* (A008578 (A061395 n)) (A052126 n))))

;; Assuming our silent definition that A000040(0) = 1:
;; (define (A252462v2 n) (* (A000040 (A252735 n)) (A052126 n)))

(define (A253550 n) (if (= 1 n) n (* (A065091 (A061395 n)) (A052126 n))))

(define (A253560 n) (* (A006530 n) n))
;; (define (A253560 n) (if (= 1 n) n (* (A006530 n) n)))
(define (A253560v2 n) (if (= 1 n) n (* (A000040 (A061395 n)) n)))

(define (A129598 n) (if (= 1 n) 2 (A253560 n)))


;; Multiplicative with a(2^e)=1 and a(p^e)=prevprime(p)^e for odd primes p.
(definec (A064989 n) (apply * (map (lambda (k) (if (zero? k) 1 (A000040 k))) (map -1+ (map A049084 (ifactor n))))))

(definec (A064989v0 n) (if (= 1 n) n (* (A008578 (A055396 n)) (A064989v0 (A032742 n)))))
(definec (A064989v5 n) (if (= 1 n) n (* (A008578 (A061395 n)) (A064989v5 (A052126 n)))))
;; Assuming our silent definition that A000040(0) = 1:
;; (definec (A064989v6 n) (if (= 1 n) n (* (A000040 (A252735 n)) (A064989v6 (A052126 n))))) 


(define (A064989fast n) (if (= 1 n) n (apply * (map (lambda (k) (if (zero? k) 1 (A000040 k))) (map -1+ (map A049084fast (factor n)))))))

;; Note: A064989(A003961(n)) = n for all n>=1.
(define (A064989v2 n) (if (= 1 n) n (apply * (map (lambda (k) (if (zero? k) 1 (A000040 k))) (map -1+ (map A049084 (factor n)))))))

;; Obfusc...
(definec (A064989v3 n) (cond ((= 1 n) n) ((even? n) (A064989v3 (/ n 2))) (else (A163511 (/ (- (A243071 n) 1) 2)))))

(define (A252463 n) (cond ((<= n 1) n) ((even? n) (/ n 2)) (else (A064989 n))))
(definec (A252464 n) (if (<= n 1) 0 (+ 1 (A252464 (A252463 n)))))
(define (A252464v2 n) (A029837 (+ 1 (A243071 n))))
(define (A252464v3 n) (A029837 (A005941 n)))

(definec (A252464v4 n) (if (<= n 1) 0 (+ 1 (A252464v4 (A253553 n)))))

(define (A253553 n) (cond ((<= n 1) n) ((zero? (A241917 n)) (A052126 n)) (else (A252462 n))))

(define (A253554 n) (cond ((<= n 1) n) ((even? n) (/ n 2)) (else (A250470 n))))

(definec (A253555 n) (if (<= n 1) 0 (+ 1 (A253555 (A253554 n)))))

(define (A253555v2 n) (A029837 (+ 1 (A252754 n))))

(define (A253555v3 n) (A029837 (+ 1 (A252756 n))))

(definec (A253556 n)
   (cond ((= 1 n) 0)
         ((odd? n) (+ 1 (A253556 (A250470 n))))
         (else (A253556 (/ n 2)))
   )
)

(definec (A253557 n)
   (cond ((= 1 n) 0)
         ((odd? n) (A253557 (A250470 n)))
         (else (+ 1 (A253557 (/ n 2))))
   )
)

(define (A253558 n) (+ 1 (A253556 n)))
(define (A253559 n) (if (= 1 n) 0 (+ -1 (A253557 n))))

;; (same-intfuns1? (COMPOSE A252735 1+) (COMPOSE -1+ A061395 1+) 1024) --> #t
;; (same-intfuns1? (COMPOSE A252735 1+) (COMPOSE -1+ A000120 A243071 1+) 1024) --> #t

(definec (A252735 n)
   (cond ((= 1 n) 0)
         ((odd? n) (+ 1 (A252735 (A064989 n))))
         (else (A252735 (/ n 2)))
   )
)


;; (same-intfuns1? A252736 (COMPOSE A080791 A243071) 1024) --> #t
;; (same-intfuns1? (COMPOSE A252736 1+) (COMPOSE -1+ A001222 1+) 1024) --> #t

(definec (A252736 n)
   (cond ((<= n 2) 0)
         ((odd? n) (A252736 (A064989 n)))
         (else (+ 1 (A252736 (/ n 2))))
   )
)


(definec (A252736v2 n)
   (cond ((= 1 n) 0)
         ((zero? (A241917 n)) (+ 1 (A252736v2 (A052126 n))))
         (else (A252736v2 (A252462 n)))
   )
)

(definec (A061395v2 n)
   (cond ((= 1 n) 0)
         ((zero? (A241917 n)) (A061395v2 (A052126 n)))
         (else (+ 1 (A061395v2 (A252462 n))))
   )
)

(definec (A252737 n) (if (zero? n) 1 (add A163511 (A000079 (- n 1)) (A000225 n))))
(define (A252737v2 n) (if (zero? n) 1 (add (COMPOSE A005940 1+) (A000079 (- n 1)) (A000225 n))))

(definec (A252738 n) (if (zero? n) 1 (mul A163511 (A000079 (- n 1)) (A000225 n))))
(definec (A252738rec n) (if (<= n 1) (+ 1 n) (* (A000079 (A000079 (- n 2))) (A252738rec (- n 1)) (A003961 (A252738rec (- n 1))))))

(define (A252739 n) (/ (A252738 n) n))

(define (A252740 n) (if (zero? n) 1 (/ (A252738 n) (A252738 (- n 1)))))
;; EI: (definec (A252740rec n) (if (<= n 1) (+ 1 n) (* (A000079 (A000079 (- n 2))) (A003961 (A252738rec (- n 1))))))

(define (A252741 n) (/ (A252738 n) (A000142 n)))

(define (A252742 n) (if (> (A003961 n) (* 2 n)) 1 0))
(define (A252742v2 n) (if (> (A048673 n) n) 1 0))

(define (A252743 n) (A252742 (A005940 (+ 1 n))))
(define (A252744 n) (A252742 (A163511 n)))

(definec (A252745 n) (if (zero? n) 0 (add A252744 (A000079 (- n 1)) (A000225 n))))
(define (A252745v2 n) (if (zero? n) 0 (add A252743 (A000079 (- n 1)) (A000225 n))))

(define (A252746 n) (if (= 0 n) 1 (- (A000079 (- n 1)) (A252745 n))))

;; (same-intfuns0? A252744 (COMPOSE A252743 A054429) 4096) --> #t
;; (same-intfuns0? A252743 (COMPOSE A252744 A054429) 4096) --> #t

(define (A252748 n) (- (A003961 n) (* 2 n)))

(definec (A252749 n) (if (zero? n) n (+ (A252748 n) (A252749 (- n 1)))))


(define (A252750 n) (- (A003961 (A005940 (+ 1 n))) (* 2 (A005940 (+ 1 n)))))

(definec (A252751 n) (if (zero? n) n (+ (A252750 n) (A252751 (- n 1)))))

;; (same-intfuns1? A252750 (COMPOSE A252748 A005940 1+) 1024) --> #t

(define (A252759 n) (if (= 1 n) 0  (+ -1 (A055396 n) (A246277 n))))

;; A108951	Multiplicative with a(p^e) equal to the product of the e-th powers of all primes at most p.
(definec (A108951 n) (if (= 1 n) n (* n (A108951 (A064989 n)))))

(definec (A200746 n) (if (<= n 2) n (* n (A064989 n))))


(definec (A206296 n) ;; Numbers matched to the Fibonacci polynomials.
  (cond ((<= n 1) (+ 1 n))
        (else (* (A003961 (A206296 (- n 1))) (A206296 (- n 2))))
  )
)

;; A073491 Numbers having no prime gaps in their factorization. 
(definec (isA073491? n)
  (cond ((= 1 n) #t)
        ((= 1 (A010051 n)) #t)
        ((> (- (A055396 (A032742 n)) (A055396 n)) 1) #f)
        (else (isA073491? (A032742 n)))
  )
)

(define A073491 (MATCHING-POS 1 1 isA073491?))


;; A260443: a(0) = 1, a(1) = 2, a(2n) = A003961(a(n)), a(2n+1) = a(n)*a(n+1).
;; The exponents in the prime factorization of term a(n) give the coefficients of the n-th Stern polynomial. See A125184.

(definec (A260443 n)
  (cond ((<= n 1) (+ 1 n))
        ((even? n) (A003961 (A260443 (/ n 2))))
        (else (* (A260443 (/ (- n 1) 2)) (A260443 (/ (+ n 1) 2))))
  )
)



(define (A104244 n) (A104244bi (A002260 n) (A004736 n)))

(define (A104245 n) (A104244bi (A004736 n) (A002260 n)))

(define (A104244bi row col)
   (fold-left (lambda (sum p.e) (+ sum (* (cdr p.e) (expt row (- (A000720 (car p.e)) 1)))))
              0
              (if (= 1 col) (list) (elemcountpairs (ifactor col))) ;; (sort (factor n) <)
   )
)

(define (A048675v3 n) (A104244bi 2 n))

(define (A054841 n) (A104244bi 10 n))

(definec (A090880 n) (if (= 1 n) (- n 1) (+ (expt 3 (- (A055396 n) 1)) (A090880 (A032742 n)))))
(definec (A090881 n) (if (= 1 n) (- n 1) (+ (expt 4 (- (A055396 n) 1)) (A090881 (A032742 n)))))
(definec (A090882 n) (if (= 1 n) (- n 1) (+ (expt 5 (- (A055396 n) 1)) (A090882 (A032742 n)))))

(define (A090883 n) (A104244bi n n))


(define (A181812 n) (A108951 (A064216 n))) ;; !!! - Check!

;; A072028		 Swap twin prime pairs of form (4*k+1,4*k+3) in prime factorization of n.
;; A072029		 Swap twin prime pairs of form (4*k+3,4*(k+1)+1) in prime factorization of n.
;; A108548		 Multiplicative with p -> A108546(A049084(p)), p prime.
;; A108546		 Lexicographically smallest permutation of primes such that for n>1 forms 4*k+1 and 4*k+3 alternate.
;; A108549		 Fixed points for A108548.
;; A108547		 Fixed points for prime number permutation A108546.

;; Instead of A108546
;; we need 2,5,3,13,17,7,11,29,
;; Lexicographically smallest permutation of primes such that 4*k+1 prime is replaced
;; with smallest unused 4k+3, and vice versa: A002144, A002145.
;; Then insert that below, between A003961 or A064989: (is it involution or not?)

;; Apufunktiot: (Aprev_4k1_prime n) and (Aprev_4k3_prime n)
;; Apufunktiot: (Asucc_4k1_prime p) and (Asucc_4k3_prime p)
;;
;; If n is even, 2n+1 will produce 4k+1 form,
;; if n is odd, 2n+1 will produce 4k+3 form.

;; Is such a prime-shift left well-defined were each is mapped to the next largest prime of the opposite form?
;; Or a variant of above, which shifts left and swaps 4k+1/+3 forms?
;; On the other hand, we could conjugate as: A108546 A003961 A108546 ?
;; Instead of these:
;; (define (Ahassu0 n)    (/ (+ 1 (A108548 (A003961 n)) 2)))
;; (define (Ahassu0inv n) (A064989 (A108548 (- (+ n n) 1))))

;; (define (Ahassu1 n)    (/ (+ 1 (A072028 (A003961 n)) 2)))
;; (define (Ahassu1inv n) (A064989 (A072028 (- (+ n n) 1))))

;; (define (Ahassu2 n)    (/ (+ 1 (A072029 (A003961 n)) 2)))
;; (define (Ahassu2inv n) (A064989 (A072029 (- (+ n n) 1))))

(define (A064216 n) (A064989 (- (+ n n) 1))) ;; A064216(n) = A064216((3*n)-1)/2 = A064989(6n-3)/2 ???
(define (A064216fast n) (A064989fast (- (+ n n) 1))) ;; A064216(n) = A064216((3*n)-1)/2 = A064989(6n-3)/2 ???


(define (A008508 n) (+ -1 (A000720 (A071904 n))))
(define (A008508v2 n) (- (A053726 n) n 1))
(define (A053726 n) (+ n (A000720 (A071904 n))))
(define (A053726v0 n) (+ 1 n (A008508 n)))
(define A053726v1 (MATCHING-POS 1 1 (lambda (n) (and (> n 1) (not (prime? (+ n n -1)))))))
(define A053726v2 (MATCHING-POS 1 1 (lambda (n) (and (> n 1) (not (prime? (A064216 n)))))))

(define (A104275 n) (if (= 1 n) 1 (A053726 (- n 1))))

(define (A144650 n)  (A144650tr (A002024 n) (A002260 n)))
(define (A144650tr m n) (+ (* 2 m n) m n 1))

(define A199593 (MATCHING-POS 1 2 (lambda (n) (not (prime? (A003986bi (+ n n n -2) 1))))))
(define A199593v1 (MATCHING-POS 1 1 (lambda (n) (and (> n 1) (not (prime? (A249823 n))))))) ;; Important, check!
(define A199593v2 (MATCHING-POS 1 1 (lambda (n) (> (A001222 (A084967 n)) 2)))) ;; Above follows from this.
(define A199593v3 (MATCHING-POS 1 1 (lambda (n) (and (> n 1) (not (prime? (A007310 n))))))) ;; And above from this!


(define (A245447 n) (A048673 (A048673 n)))
(define (A245448 n) (A064216 (A064216 n)))

(define A245449 (FIXED-POINTS 1 1 A245447))


(define (A243061 n) (A064216 (A241909 n)))
(define (A243061v2 n) (A241909 (A243065 (A241909 n))))
(define (A243062 n) (A241909 (A048673 n)))
(define (A243062v2 n) (A241909 (A243066 (A241909 n))))

(define (A243065 n) (A241909 (A064216 n)))
(define (A243065v2 n) (A241909 (A243061 (A241909 n))))
(define (A243066 n) (A048673 (A241909 n)))
(define (A243066v2 n) (A241909 (A243062 (A241909 n))))

(define (A243505 n) (A122111 (A064216 n)))
(define (A243506 n) (A048673 (A122111 n)))

;; A244981-A244992 are now reserved for your use.

(define (A244981 n) (/ (A122111 (A102750 n)) 2))

(define (A244982 n) (A243285 (A122111 (* 2 n))))

(define (A244983 n) (if (= 1 n) 1 (/ (+ 1 (A122111 (A070003 (- n 1)))) 2)))

(define (A244984 n) (A243283 (A122111 (-1+ (* 2 n)))))

;; To compute more terms, create an appropriately sized b-file for b070003.txt, with
;; the following Pari-code for example (by Charles Greathouse):
;; isA070003(n)=my(f=factor(n)[, 2]); f[#f]>1 
;; i=0; for(n=2, 2^30, if(isA070003(n), i++; write("b070003_upto_term_2pow30.txt", i, " ", n)))
;; then read the b-file in as a vector:
;; (define vecA070003 (read-b-file-to-vector "seqs/b070003_upto_281236.txt" 281237))
;; And then use a binary search to find the inverses of A070003 fast:
(define (A244984fast n) (if (= 1 n) n (+ 1 (pos_of_k_in_vector (A122111 (-1+ (* 2 n))) vecA070003))))

(define (A244153 n) (A156552 (A064216 n))) ;; offset 1, a(1) = 0.
(define (A244154 n) (A048673 (A005940 (+ 1 n)))) ;; offset 0, a(0) = 1.

(define (A245611 n) (A243071 (A064216 n))) ;; offset 1, a(1) = 0.
(define (A245612 n) (A048673 (A163511 n))) ;; offset 0, a(0) = 1.

(define (A243353 n) (A005940 (1+ (A003188 n))))
(define (A243354 n) (A006068 (A156552 n)))


(define (A253791 n) (A244153 (A005940 (+ 1 n))))

(define (A253792 n) (A156552 (A244154 n)))

(definec (A156552v3 n)
  (cond ((= n 1) (- n 1))
        ((even? n) (+ 1 (* 2 (A156552v3 (/ n 2)))))
        (else (* 2 (A253791 (A156552v3 (/ (+ n 1) 2)))))
  )
)

(define (A253891 n) (A245611 (A163511 n)))

(define (A253892 n) (A243071 (A245612 n)))

(definec (A243071v2 n)
  (cond ((<= n 2) (- n 1))
        ((even? n) (* 2 (A243071v2 (/ n 2))))
        (else (+ 1 (* 2 (A253891 (A243071v2 (/ (+ n 1) 2))))))
  )
)


(define (A253883 n) (A243505 (A122111 n)))
(define (A253884 n) (A122111 (A243506 n)))
(define (A253890 n) (A122111 (- (* 2 (A122111 n)) 1)))
(define (A253890v2 n) (A122111 (A005408 (- (A122111 n) 1))))
(define (A253890v3 n) (A253560 (A253883 n)))

(define (A254049 n) (A048673 (+ n n -1)))
(define (A254049v2 n) (/ (+ 1 (A003961 (+ n n -1))) 2))

(define (A254050 n) (+ -1 (* 2 (A249745 n))))
(define (A254050v2 n) (A064989 (A007310 n)))


(define (A253888 n) (if (zero? n) 1 (A048673 (+ 1 (* 2 (A064216 n))))))

(define (A253888v2 n) (if (zero? n) 1 (A254049 (+ 1 (A064216 n)))))

(define (A253889 n) (A048673 (floor->exact (/ (A064216 n) 2))))

(definec (A253893 n) (if (= 1 n) 0 (+ 1 (A253893 (A253889 n)))))

(definec (A253894 n) (if (= 1 n) 1 (+ 1 (A253894 (A253889 n)))))
(define (A253894v2 n) (A070939 (A064216 n)))

(definec (A254044 n) (if (= 1 n) 1 (+ (A254044 (A253889 n)) (- 1 (floor->exact (/ (modulo n 3) 2))))))

(definec (A254044v1 n)
  (cond ((= 1 n) 1)
        ((= 2 (modulo n 3)) (A254044v1 (/ (+ 1 n) 3)))
        (else (+ 1 (A254044v1 (A253889 n))))
  )
)

(define (A254044v2 n) (A000120 (A064216 n)))


(definec (A254045 n) (if (= 1 n) 0 (+ (A254045 (A253889 n)) (floor->exact (/ (modulo n 3) 2)))))

(definec (A254045v1 n)
  (cond ((= 1 n) 0)
        ((= 2 (modulo n 3)) (+ 1 (A254045v1 (/ (+ 1 n) 3))))
        (else (A254045v1 (A253889 n)))
  )
)

(define (A254045v2 n) (A080791 (A064216 n)))



(definec (A165199 n) (if (zero? n) n (+ (* 2 (A165199 (floor->exact (/ n 2)))) (A000035 (+ (A000523 n) n)))))

;; A245443-A245454 are now reserved for your use.

(define (A245443 n) (A165199 (A193231 n)))
(define (A245444 n) (A193231 (A165199 n)))

(define (A245445 n) (A056539 (A193231 n)))
(define (A245446 n) (A193231 (A056539 n)))

(define (A245451 n) (+ 1 (A075157 (A003188 (A075158 (- n 1))))))
(define (A245452 n) (+ 1 (A075157 (A006068 (A075158 (- n 1))))))

(define (A245454 n) (+ 1 (A075157 (A193231 (A075158 (- n 1))))))

(definec (A165199org n)
  (cond ((<= n 1) n)
        (else (let ((r (A165199org (floor->exact (/ n 2)))))
                 (+ r r (modulo (+ r (- 1 (modulo (+ n (floor->exact (/ n 2))) 2))) 2))
              )
        )
  )
)

(define (A165199v2 n) (A075158 (-1+ (A243353 n)))) ;; Check! (Leroy Quet)

(define (A165199v3 n) (A075158 (-1+ (A241909 (+ 1 (A075157 n))))))

(define (A209281 n) (A000120 (A006068 (- n 1)))) ;; XXX - Check! (offset=1)

;; (map A243500 (iota 32))
;; 2,1,4,3,6,5,8,7,10,9,22,27,20,15,14,33,18,17,28,13,34,11,62,29,26,25,12,19,24,75,68,43


(define (A243500 n) (cond ((odd? n) (A243502 (A064989 n))) (else (A003961 (A048673 (/ n 2))))))
(define (A243500v2 n) (if (even? n) (A003961 (A048673 (/ n 2))) (A243502 (A064216 (/ (1+ n) 2)))))
(define (A243500v3 n) (if (even? n) (A003961 (A048673 (/ n 2))) (* 2 (A245448 (/ (1+ n) 2)))))

;; Various permutations of even numbers:
(define (A243501 n) (+ 1 (A003961 n)))
(define (A243501v2 n) (* 2 (A048673 n)))

(define (A243502 n) (* 2 (A064216 n)))

(define (A253885 n) (- (A003961 (+ n 1)) 1)) ;; Offset=0.
(define (A253886 n) (- (A250469 (+ n 1)) 1)) ;; Offset=0.


(definec (Asomeother n)
  (cond ((= 1 n) 1)
        ((odd? n) (* 2 (A064216 (Asomeother (/ (-1+ n) 2)))))
        (else (A003961 (+ 1 (Asomeother (/ n 2)))))
  )
)


;; 
;; entanglements...

;; A244152-A244161 are now reserved for your use.


;; XXX - The following are not final correct versions:

;; Self-inverse:
;; 1 -> 1, after which swap other bisection of A241909 (at 2,4,6,8, ...) with other bisection of it (3,5,7,9,11)
;; i.e. 2,3,9,5,27,25,81,7,15,125,... with 4,8,16,6,32,64,18,128,256,54,512,12,10,1024,...
;; (map Anot_this_too_wild (iota 19))
;; After 1 and 2, a(n) = n+1 modulo 2:
;; 1, 2, 4, 3, 8, 7, 6, 5, 16, 205891132094649, 128, 319, 5832, 113, 2048, 9, 24, 27, 54

(definec (Anot_this_too_wild n)
   (cond ((<= n 1) n)
         ((or (= n 2) (odd? n)) ;; n is in even bisection, which contains 2 and then odd numbers >= 3.
            (* 2 (A243065 (Anot_this_too_wild (/ (A241909 n) 2))))
         )
         (else ;; n is in odd bisection (from n>=3 onward), which contains even numbers from 4 onward.
            (+ -1 (* 2 (A243066 (Anot_this_too_wild (/ (+ 1 (A241909 n)) 2)))))
         )
   )
)


;; even bisection (2,3,9,16,...) to odds (3,5,7,...), odd bisection (4,8,16,6,32,64,18,128,...) to evens (2,4,6,...)
(definec (Ajoku_extra3 n)
   (cond ((<= n 1) n)
         ((or (= n 2) (odd? n)) ;; n is in even bisection, which contains 2 and then odd numbers >= 3.
            (+ 1 (* 2 (Ajoku_extra3 (/ (A241909 n) 2))))
         )
         (else ;; n is in odd bisection (from n>=3 onward), which contains even numbers from 4 onward.
            (* 2 (Ajoku_extra3 (/ (- (A241909 n) 1) 2)))
         )
   )
)


(definec (Ajoku_extra4 n) ;; Evens to odd bisection, Odds to even bisection
   (cond ((<= n 1) n)
         ((even? n) (* 2 (A243065 (+ 1 (Ajoku_extra4 (/ n 2))))))
         (else (+ 1 (* 2 (A243066 (Ajoku_extra4 (/ (- n 1) 2))))))
   )
)

(definec (Ajoku_extra4 n) ;; Evens to odd bisection, Odds to even biection
   (cond ((<= n 1) n)
         ((even? n) (* 2 (A243065 (Ajoku_extra4 (/ n 2)))))
         (else (+ -1 (* 2 (A243066 (Ajoku_extra4 (/ (- n 1) 2))))))
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A243282-A243291 are now reserved for your use.

(define (A242378bi row col) (if (zero? col) col (apply * (map A000040 (map (lambda (k) (+ k row)) (map A049084 (ifactor col)))))))

(define (A242378 n) (A242378bi (A002262 n) (A025581 n)))
(define (A242379 n) (A242378bi (A025581 n) (A002262 n)))

(define (A243289 n) (- n (A243290 n)))
(define (A243290 n) (A061395 (A005117 n)))
(define (A243291 n) (- n (A061395 n)))

(define (A243349 n) (A243291 (A005117 n)))
(define (A243348 n) (- (A005117 n) n))
(define (A243348v2 n) (- (A243349 n) (A243289 n)))
(define (Afirst_diffs_of_A243348 n) (if (< n 2) (- n 1) (- (A243348 n) (A243348 (- n 1)))))
(define Aincr_points_of_A243348 (COMPOSE -1+ (NONZERO-POS 1 1 Afirst_diffs_of_A243348)))
(define (A120992 n)
   (if (= n 1)
       (Aincr_points_of_A243348 n)
       (- (Aincr_points_of_A243348 n) (Aincr_points_of_A243348 (- n 1)))
   )
)

(define (A243351 n) (- (* 2 n) (A005117 n)))


(definec (A022559 n) (if (< n 2) 0 (+ (A022559 (- n 1)) (A001222 n)))) ;; Partial sums of A001222. (XXX: Cf. A203623)

;; n>1 appears bigomega(n) times, where bigomega(n)=A001222(n) is the number of prime factors of n (with repetition), a(1)=1.
(define A082288 (COMPOSE-FUNS (LEAST-GTE-I 0 1 A022559) -1+)) ;; Cf. A241920

;; A241909-A241920 are now reserved for your use.

(define (A241910 n) (if (= n 1) 0 (- n (+ 2 (A022559 (- (A082288 n) 1)))))) ;; Cf. A241914

(define (A241911 n) (if (= n 1) 1 (- n (A022559 (- (A082288 n) 1)) 1))) ;; Cf. A241915

(define (A241917 n) (- (A061395 n) (A061395 (A052126 n))))

(define (A241919 n) (- (A061395 n) (A061395 (A051119 n))))

(define (A242411 n) (if (= 1 (A001221 n)) 0 (A241919 n)))
(define (A242411v2 n) (if (= 1 (A001221 n)) 0 (- (A061395 n) (A061395 (A051119 n)))))

(define (A243055 n) (- (A061395 n) (A055396 n))) ;; Differs from A242411 for the first time at n=30.
(define (A243056 n) (if (prime? n) (A049084 n) (A243055 n)))

(define A000961 (ZERO-POS 1 1 A243055))
(define A025475 (ZERO-POS 1 1 A243056))

(define (A181062 n) (- (A000961 n) 1))

(define (A249435 n) (- (A025475 n) 1))

(define A070003 (ZERO-POS 0 1 A241917))
(define A070003fast (MATCHING-POS 1 2 (lambda (k) (let ((fs (reverse! (ifactor k)))) (and (not (null? (cdr fs))) (= (car fs) (cadr fs))))))) ;; Really not that fast with SLIB ! Precompute b070003.txt with Pari-code instead.

;; (map A070003 (iota 12)) --> (4 8 9 16 18 25 27 32 36 49 50 54)
(define A102750 (NONZERO-POS 1 1 A241917))

(define (A243282 n) (- (A243283 n) 1))
(definec (A243283 n) (if (<= n 1) n (+ (A243283 (- n 1)) (if (zero? (A241917 n)) 1 0))))
(define (A243285 n) (- n (A243283 n)))

(define (A243286 n) (if (<= n 1) n (+ 1 (A243285 (A243057 (A102750 (- n 1)))))))
(define (A243286v2 n) (if (<= n 1) n (+ 1 (A243285 (A243059 (A102750 (- n 1)))))))
(define (A243286v3 n) (if (<= n 1) n (+ 1 (A243285 (A242420 (A102750 (- n 1)))))))

(definec (A243287 n)
   (cond ((<= n 1) n)
         ((zero? (A241917 n)) (+ 1 (* 2 (A243287 (A243282 n))))) ;; If n is in A070003?
         (else (* 2 (A243287 (A243285 n))))
   )
)

(definec (A243288 n)
   (cond ((<= n 1) n)
         ((even? n) (A102750 (A243288 (/ n 2))))
         (else (A070003 (A243288 (/ (- n 1) 2))))
   )
)

(definec (A122111 n) (if (<= n 1) n (* (A000079 (A241917 n)) (A003961 (A122111 (A052126 n))))))

(definec (A122111v2 n) (if (<= n 1) n (* (A000040 (A001222 n)) (A122111v2 (A064989 n)))))

(definec (A122111v3 n) (if (<= n 1) n (* (expt (A000040 (A071178 n)) (A241919 n)) (A242378bi (A071178 n) (A122111v3 (A051119 n))))))

(definec (A122111v4 n)
  (cond ((<= n 2) n)
        ((even? n) (A253550 (A122111v4 (/ n 2))))
        (else (A253560 (A122111v4 (A064989 n))))
  )
)

(definec (A122111v5 n)
   (cond ((= n 1) n)
         ((zero? (A241917 n)) (A003961 (A122111v5 (A052126 n))))
         (else (* 2 (A122111v5 (A252462 n))))
   )
)

(definec (A122111v6 n)
  (cond ((<= n 2) n)
        ((even? n) (A253550 (A122111v6 (/ n 2))))
        (else (A253890 (A122111v6 (/ (+ 1 n) 2))))
  )
)


(define A088902 (FIXED-POINTS 1 1 A122111))

(definec (A153212 n) ;; By Luchezar Belev
  (if (<= n 1) n
      (let ((r (A153212 (A051119 n))))
        (* r (expt (A000040 (+ (A061395 r) (A071178 n))) (A241919 n)))
      )
  )
)

(define (A153212v2 n) (A122111 (A242419 n))) ;; Simple ...
(define (A153212v3 n) (A242419 (A122111 n))) ;;  ... group theory.

(definec (A242419 n) ;; Inspired...
   (cond ((<= n 1) n)
         (else (* (expt (A000040 (A241919 n)) (A071178 n)) (A242378bi (A241919 n) (A242419 (A051119 n)))))
   )
)


(definec (A242419v0 n)
  (if (<= n 1)
      n
      (let* ((pfs (ifactor n)))
         (apply * (map expt (map A000040 (revdeltas (map A049084 (uniq pfs)))) (reverse (multiplicities pfs))))
      )
  )
)

(define (A242419v2 n) (A122111 (A153212 n))) ;; XXX - Prove that equal!
(define (A242419v3 n) (A153212 (A122111 n))) ;; XXX - Prove that equal!
(define (A242419v4 n) (A069799 (A242415 n))) ;; Likewise...
(define (A242419v5 n) (A242415 (A069799 n))) ;; Likewise...

(define A242417 (FIXED-POINTS 1 1 A242419))

(define A242417v2 (MATCHING-POS 1 1 (lambda (n) (and (= n (A069799 n)) (= n (A242415 n)))))) ;; Just for checking

;; Injective on A102750:
(definec (A243057 n) (if (<= n 1) n (* (if (zero? (A243056 n)) 1 (A000040 (A243056 n))) (A243057 (A032742 n)))))

(definec (A243059 n) (if (<= n 1) n (* (if (zero? (A243056 n)) 0 (A000040 (A243056 n))) (A243059 (A032742 n)))))

(define A243058 (FIXED-POINTS 1 1 A243057))
(define A243058v2 (FIXED-POINTS 1 1 A243059))

(define (A242420 n) (* (expt (A006530 n) (- (A071178 n) 1)) (A243057 n)))
(define (A242420v2 n) (A225891 (A242419 n)))
(define A243068 (FIXED-POINTS 1 1 A242420))

(define A242421 (FIXED-POINTS 1 1 A153212))


;; A000975 gives the positions where primorials occur, after which only larger terms occur:
;; 1, 2, 5, 10, 21, 42, 85, 170, 341, 682, 1365, 2730,
;; E.g. (A242421_in_wrong_order 85) = 510510

(definec (A242421_in_wrong_order n)
   (let ((complist (binexp->runcount1list n)))
       (apply * (map (lambda (i e) (expt (A000040 i) e)) (partsums complist) complist))
   )
)


(definec (A069799 n)
  (let ((pf (ifactor n)))
     (apply * (map expt (uniq pf) (reverse (multiplicities pf))))
  )
)

(define A242414 (FIXED-POINTS 1 1 A069799))
(define A242416 (COMPLEMENT 1 A242414))


(definec (A105119 n) ;; Numbers obtained by rotating right the indices in the prime signature of n.
  (let ((pf (ifactor n)))
     (apply * (map expt (uniq pf) (rotright (multiplicities pf))))
  )
)

(definec (A225891 n) ;; Numbers obtained by rotating left the indices in the prime signature of n.
  (let ((pf (ifactor n)))
     (apply * (map expt (uniq pf) (rotleft (multiplicities pf))))
  )
)

;; (define (DIFF2 ints) (reverse (fold-left (lambda (xs x) (cons (- x (apply + xs)) xs)) '() ints)))

(definec (A242415 n)
  (if (<= n 1)
      n
      (let* ((pfs (ifactor n)))
         (apply * (map expt (map A000040 (revdeltas (map A049084 (uniq pfs)))) (multiplicities pfs)))
      )
  )
)

(definec (A242415v0 n) ;; Inspired...
   (cond ((<= n 1) n)
         (else (* (expt (A000040 (A241919 n)) (A067029 n)) (A242378bi (A241919 n) (A242415v0 (A051119 (A225891 n))))))
   )
)

(define (A242415v2 n) (A122111 (A069799 (A122111 n))))
(define (A242415v3 n) (A153212 (A069799 (A153212 n))))

(define A242413 (FIXED-POINTS 1 1 A242415))


(definec (A242424 n) (if (<= n 1) n (* (A000040 (A001222 n)) (A064989 n))))

(definec (A243072 n) (if (<= n 1) n (* (A000040 (A001222 n)) (A242424 (A064989 n)))))
(definec (A243073 n) (if (<= n 1) n (* (A000040 (A001222 n)) (A243072 (A064989 n)))))
;; (definec (Ajokuvus4 n) (if (<= n 1) n (* (A000040 (A001222 n)) (A243073 (A064989 n)))))

(define (A243070bi row col)
   (cond ((<= col 1) col)
         ((= 1 row) (A242424 col))
         (else (* (A000040 (A001222 col)) (A243070bi (- row 1) (A064989 col))))
   )
)

(define (A243070 n) (A243070bi (A002260 n) (A004736 n)))


(define (A003963 n) (apply * (map A049084 (ifactor n))))

(definec (A078442 n) (if (zero? (A049084 n)) 0 (+ 1 (A078442 (A049084 n)))))
(define (A049076 n) (+ 1 (A078442 n)))

(define (A243503 n) (A056239 (A241909 n)))
(define (A243504 n) (A003963 (A241909 n)))

(definec (A241909 n)
   (cond ((<= n 2) n)
         ((pow2? n) (A000040 (A007814 n)))
         (else (* (A000040 (A001511 n)) (A242378bi (A007814 n) (A241909 (A064989 n)))))
   )
)

(definec (A241909v2 n)
  (cond ((<= n 1) n)
        ((prime? n) (A000079 (A049084 n)))
        (else (* (A052126 (A241909v2 (A052126 n)))
                 (expt (A000040 (A001222 n)) (+ 1 (A241917 n)))
              )
        )
  )
)


(definec (A156552 n) ;; A156552 Unary-encoded compressed factorization of natural numbers. - XXX: Check!
  (cond ((= n 1) 0)
;;      ((prime? n) (A000079 (- (A049084 n) 1))) ;; Unnecessary!
        (else (+ (A000079 (+ -2 (A001222 n) (A061395 n)))
                 (A156552 (A052126 n))
              )
        )
  )
)


(definec (A156552v2 n)
   (cond ((= 1 n) (- n 1))
         ((even? n) (+ 1 (* 2 (A156552v2 (/ n 2)))))
         (else (* 2 (A156552v2 (A064989 n))))
   )
)

(define (A005941 n) (+ 1 (A156552 n)))

(define (A005940 n) (A005940off0 (- n 1))) ;; Starting offset = 1.

(definec (A005940off0 n)
   (cond ((< n 2) (+ 1 n))
;;       ((pow2?v2 n) => (lambda (k) (A000040 (+ 1 k)))) ;; Unnecessary!
         (else
            (* (A000040 (- (A070939 n) (- (A000120 n) 1)))
               (A005940off0 (A053645 n)) ;; Recurse with msb-bit taken off.
            )
         )
   )
)

(definec (A005940off0v2 n)
   (cond ((<= n 2) (+ 1 n))
         ((even? n) (A003961 (A005940off0v2 (/ n 2))))
         (else (* 2 (A005940off0v2 (/ (- n 1) 2))))
   )
)


(define (A005940off0loop n)
  (let loop ((n n) (i 1) (x 1))
    (cond ((zero? n) x)
          ((even? n) (loop (/ n 2) (+ i 1) x))
          (else (loop (/ (- n 1) 2) i (* x (A000040 i))))
    )
  )
)

;; A083221-analogues for A005940, A156552, A163511, A243071:

(definec (A252753 n)
   (cond ((<= n 2) (+ 1 n))
         ((even? n) (A250469 (A252753 (/ n 2))))
         (else (* 2 (A252753 (/ (- n 1) 2))))
   )
)

(definec (A252754 n)
   (cond ((= 1 n) (- n 1))
         ((even? n) (+ 1 (* 2 (A252754 (/ n 2)))))
         (else (* 2 (A252754 (A250470 n))))
   )
)



(definec (A252755 n)
  (cond ((<= n 1) (+ n 1))
        ((even? n) (* 2 (A252755 (/ n 2))))
        (else (A250469 (A252755 (/ (- n 1) 2))))
  )
)

(definec (A252756 n)
  (cond ((<= n 2) (- n 1))
        ((even? n) (* 2 (A252756 (/ n 2))))
        (else (+ 1 (* 2 (A252756 (A250470 n)))))
  )
)

(define A253789 (FIXED-POINTS 1 1 (COMPOSE A252753 -1+)))
(define A253789v2 (FIXED-POINTS 1 1 (COMPOSE 1+ A252754)))

(define A253790 (MATCHING-POS 1 1 (lambda (n) (or (= 1 n) (and (odd? n) (= (A055396 n) (A001511 (- n 1))))))))

(define (A241916 n) (A122111 (A241909 n)))
(define (A241916v2 n) (A241909 (A122111 n)))
;; (define (A241916v3 n) (apply * (map A000040 (conjugate-partition (prime-exps_to_ascpart (primefacs->explist n))))))

(define A241912 (FIXED-POINTS 1 1 A241916)) ;; Cf. A088902.

(define A241913 (COMPLEMENT 1 A241912))

(define (A137502 n) (if (< n 2) n (/ (* 2 (A241916 n)) (A006530 n))))

(define A242418 (FIXED-POINTS 1 1 A137502))

;; A243051-A243062 are now reserved for your use
;; A243065-A243074 are now reserved for your use.
(define (A243051 n) (A241909 (A242424 (A241909 n))))

(define (A243052 n) (A241909 (A243072 (A241909 n))))

(define (A243053 n) (A241909 (A243073 (A241909 n))))


(define (A112798 n) (list-ref (A112798row (A082288 n)) (A241910 n))) ;; offset=2.

(definec (A112798row n)
  (let loop ((indices (list)) (n n) (i 1))
       (cond ((= 1 n) (reverse! indices))
             ((zero? (modulo n (A000040 i))) (loop (cons i indices) (/ n (A000040 i)) i))
             (else (loop indices n (+ i 1)))
       )
  )
)

(define (A056239 n) (apply + (map A049084 (ifactor n))))
(define (A056239v2 n) (apply + (A112798row n))) ;; "Excellent musical properties" (Denmark, Rasmussen) ???

(define (A248692 n) (A000079 (A056239 n)))
(define (A248692v2 n) (apply * (map A000079 (map A049084 (ifactor n)))))

(define A242422 (NONZERO-POS 1 1 (COMPOSE A010054 A056239)))
(define A242422v2 (ZERO-POS 1 1 (COMPOSE A002262 A056239)))
(define A242423 (ZERO-POS 1 1 (COMPOSE  A010054 A056239)))
(define A242423v2 (NONZERO-POS 1 1 (COMPOSE A002262 A056239)))

(define (A065091 n) (A000040 (+ 1 n)))
(define A065090 (COMPLEMENT 1 A065091))

(define (A006254 n) (/ (+ 1 (A065091 n)) 2))

;; Relates to some (trivial?) identity concerning A078442, A001222, A007814 and A135141/A227413:
(define (A087436 n) (- (A001222 n) (A007814 n))) ;; Number of odd prime-factors of n, counted with repetitions.

(define A038550 (MATCHING-POS 1 1 (lambda (n) (prime? (A000265 n))))) ;; Products of an odd prime and a power of two (sorted).
(define A093641 (MATCHING-POS 1 1 (lambda (n) (or (prime? (A000265 n)) (= 1 (A000265 n)))))) ;; Numbers of form 2^i * prime(j), i>=0, j>0, together with 1.


(definec (A001414 n) (reduce + 0 (ifactor n))) ;; Integer log of n: sum of primes dividing n (with repetition). 

;; In prime factorization of n replace multiplication by bitwise logical 'and', 'or' or 'xor':

(definec (A072591 n) (reduce A004198bi 1 (ifactor n)))

(definec (A072593 n) (if (< n 2) n (reduce A003986bi 1 (ifactor n))))

(definec (A072594 n) (reduce A003987bi 1 (ifactor n)))

(define A072595 (MATCHING-POS 1 1 (lambda (n) (zero? (A072594 n)))))

(define A072596 (MATCHING-POS 1 1 (lambda (n) (and (not (zero? (A068527 n))) (zero? (A072594 n))))))


(define A235050 (MATCHING-POS 1 1 (lambda (n) (or (= 1 n) (= (A001414 n) (A072593 n))))))
(define A235050v2 (MATCHING-POS 1 1 (lambda (n) (or (= 1 n) (= (A001414 n) (A072594 n))))))

;; Subset of A072596, square-free in A072595:
(define A235488 (MATCHING-POS 1 1 (lambda (n) (and (not (zero? (A008683 n))) (zero? (A072594 n))))))

;; (define A235490 (MATCHING-POS 1 1 (lambda (n) (and (not (zero? (A008683 n))) (= (A001414 n) (A072593 n)) (pow2? (1+ (A001414 n)))))))

;; A000668 gives the odd terms in this sequence after 1:
(define A235490 (MATCHING-POS 1 1 (lambda (n) (or (= 1 n) (and (= (A001414 n) (A072593 n)) (pow2? (1+ (A001414 n))))))))



(define A235488 (MATCHING-POS 1 1 (lambda (n) (and (not (zero? (A008683 n))) (zero? (A072594 n))))))

(define A232667 (MATCHING-POS 1 1 (lambda (n) (and (prime? n) (prime? (- (* 2 n) 1)) (odd? (A000120 n))))))

(define A232667v2 (COMPOSE A000040 (MATCHING-POS 1 1 (lambda (k) (prime? (A000069 (A000040 k)))))))

(define A227930 (MATCHING-POS 1 1 (lambda (n) (and (even? (A000120 (- n 1))) (even? (A000120 (+ n 1))) (prime? n)))))
(define A227930v2 (MATCHING-POS 1 1 (lambda (n) (and (odd? n) (odd? (A000120 n)) (even? (A007814 (+ n 1))) (prime? n)))))

(define A055926v2
   (MATCHING-POS 1 1
     (lambda (n)
        (cond ((and (integer? (/ n 12)) (odd? (/ n 12))))
              ((A055881 n) => (lambda (k) (and (> k 4) (not (prime? (+ k 1))))))
        )
     )
   )
)

(define A232741 (MATCHING-POS 1 1 (lambda (n) (prime? (+ 1 (A055881 n))))))
(define A232742 (MATCHING-POS 1 1 (lambda (n) (not (prime? (+ 1 (A055881 n)))))))

(define A232743 (MATCHING-POS 1 1 (lambda (n) (cond ((A055881 n) => (lambda (k) (and (> k 4) (not (prime? (+ k 1))))))))))


(definec (A232744 n) (if (<= n 1) n (+ (A232744 (- n 1)) (- 2 (A000035 (A055881 (+ 1 (A232744 (- n 1)))))))))

(define A232744v2 (MATCHING-POS 1 1 (lambda (n) (odd? (A055881 n)))))

(define A232745v2 (MATCHING-POS 1 1 (lambda (n) (even? (A055881 n)))))


(define (A249150 n) (A230403 (A001142 n)))

(define (A249151 n) (A055881 (A001142 n)))

(define A249423 (FIXED-POINTS 1 0 A249150))

(define A249424 (MATCHING-POS 1 0 (lambda (n) (and (odd? n) (= (/ (- n 1) 2) (A249151 n))))))

(define (A249428 n) (A249151 (A249424 n)))

(define A249425 (RECORD-POS 1 0 A249150))

(define (A249426 n) (A249150 (A249425 n)))

(define (A249427 n) (A249151 (A249425 n)))

(define A249429 (MATCHING-POS 1 0 (lambda (n) (> (A249151 n) n))))

(definec (A249431 n) (- (A249151 n) n))

(define (A249430 n) (let loop ((k 0)) (cond ((= n (A249431 k)) k) (else (loop (+ 1 k))))))

(define A249432 (RECORD-POS 1 0 A249431))

(define A249433 (MATCHING-POS 1 0 (COMPOSE negative? A249431)))

(define A249434 (MATCHING-POS 1 0 (COMPOSE not negative? A249431)))

(define A249436 (MATCHING-POS 1 0 (lambda (n) (and (> (A249151 n) (/ n 2)) (< (A249151 n) n)))))

(define A249436v2
  (MATCHING-POS 1 0
     (lambda (n) (and (zero? (modulo (A001142 n) (A000142 (floor->exact (/ (+ n 1) 2)))))
                      (not (zero? (modulo (A001142 n) (A000142 n))))
                 )
     )
  )
)

(define (A249437 n) (A249151 (A249436 n)))

;; (define A249438lista (list-head (uniq (sort (map A249437 (iota 115)) <)) 56))

;;;;;;

;; From Stepasimov, A236747: "Number of 0 <= k <= sqrt(n) such that n-k and n+k are both prime."
(definec (A236747 n) (add (lambda (k) (* (A010051 (- n k)) (A010051 (+ n k)))) 0 (A000196 n)))


(definec (A246702 n)
 (let ((u (A016754 (- n 1))))
  (let loop ((k (- u 1)) (s 0))
        (cond ((zero? k) s)
              ((zero? (modulo (A000225 k) u)) (loop (- k 1) (+ s 1)))
              (else (loop (- k 1) s))
        )
  )
 )
)

(define A246717 (MATCHING-POS 1 1 (lambda (n) (and (odd? n) (= 2 (A246702 (/ (+ 1 n) 2)))))))

;; Some from Alex Ratushnyak:
(define A236866 (MATCHING-POS 1 1 (lambda (n) (prime? (A007775 n)))))
(define A007775 (MATCHING-POS 1 1 (lambda (n) (= 1 (gcd n 30)))))


(define (inA001109? n) (= (* 8 n n) (floor->exact (* (sqrt 8) n (ceiling->exact (* (sqrt 8) n))))))
(define A001109 (MATCHING-POS 0 0 inA001109?)) ;; Not a practical version, just for testing the above one!

(define (inA001110? n) (and (zero? (A068527 n)) (inA001109? (floor->exact (sqrt n)))))
(define A001110v2 (MATCHING-POS 0 0 inA001110?)) ;; Not a practical version, just for testing the above one!

(definec (A001110 n) (if (< n 2) n (+ 2 (- (* 34 (A001110 (- n 1))) (A001110 (- n 2))))))

;; A233267-A233287 are now reserved for your use.

(define (A233267 n) (A055881 (A001110 n))) ;; A fractal sequence, palindromic?
(define Auudet (DISTINCT-POS 1 1 A233267))

;; Also (A055881 (A006472 n)) ???
;; (map Auudet (iota 8)) --> (1 2 4 12 48 288 2016 4032) ;; *2 - 1 --> 3,7,23,95,575
;; (map Auusi (map Auudet (iota 8))) --> (1 3 4 7 11 12 13 14)

(define A000290hardway (MATCHING-POS 0 0 (lambda (n) (zero? (A068527 n)))))

(define A232847 (MATCHING-POS 1 1 (lambda (n) (inA001110? (A000203 n)))))

(define A233281 (MATCHING-POS 1 1 (lambda (n) (prime? (A001177 n)))))
;; Cf. A092395: Primes occurring as divisors of fibonacci(p) with p prime ???


(define A234218 (MATCHING-POS 1 1 (lambda (n) (and (prime? n) (odd? (A000120 (expt n 3)))))))



(define (A234011 n) (+ (A000069 n) (A000069 (+ n 1))))

(define (A234431 n) (+ (A001969 n) (A001969 (+ n 1))))

(define A233388 (COMPOSE A234011 (MATCHING-POS 1 1 (lambda (n) (odd? (A000120 (A234011 n)))))))


(define (A234541 n)
   (let loop ((k 1))
     (cond ((prime? (A234575bi n k)) k)
           ((> k n) 0)
           (else (loop (+ 1 k)))
     )
   )
)


;; (define (A234746_not_this n) (A091203 (A234751 (A091202 n)))) ;;; ??? Analogous to A234749. Not (A000720 (A234747 (A000040 n)))

;; Completely multiplicative with p_i = p_{A234751(i)}
;; (uniq (map (lambda (n) (- (A234747 (* (A002260 n) (A004736 n))) (* (A234747 (A002260 n)) (A234747 (A004736 n)))))  (iota 5050))) --> (0)

(define (A234747 n) (A091203 (A193231 (A091202 n))))

;; Multiplicative with p_i = p_{A234749(i)}
(define (A234748old_variant n) (A091205 (A193231 (A091204 n))))
(define (A234748 n) (A235042 (A193231 (A235041 n))))

;; Needed ? (define (A234749 n) (A091205 (A234751 (A091204 n))))

(define A118666 (FIXED-POINTS 0 0 A193231))

;; A066247 is in GF2Xfuns.scm:
(define (A066246 n) (* (A066247 n) (- n (A000720 n) 1))) ;; 0 unless n is a composite number A002808(k) when a(n) = k.

(define (A066136 n) (+ (A049084 n) (A066246 n)))

(define (A234750 n) (A193231 (A014580 n))) ;; Blue-code restricted to irreducible GF(2)[X]-polynomials
(define (A234751 n) (A091227 (A234750 n))) ;; ... and a self-inverse permutation of integers induced by that.
(define (A234751v2 n) (A091227 (A193231 (A014580 n))))
(define (A234751v3 n) (A000720 (A234747 (A000040 n))))

(define (A234745 n) (A193231 (A091242 n))) ;; Blue-code restricted to reducible GF(2)[X]-polynomials
(define (A234746 n) (A091246 (A234745 n))) ;; ... and a self-inverse permutation of integers induced by that.


(define A234749 (FIXED-POINTS 1 1 A234746))
(define (A234752 n) (A091242 (A234749 n)))
;; (define Alater (FIXED-POINTS 1 1 A234751)) ;; The first 22 values are: (3 6 21 52 55 200 203 212 461 462 479 480 483 2150 2151 2176 2177 2190 2195 2208 2209 2214)

(define (A008578 n) (if (< n 2) n (A000040 (- n 1)))) ;; Correct offset. One in GF2Xfuns.scm has off=0.


;; My contest entry:
(definec (A234840 n)
   (cond ((< n 2) n)
         ((= n 2) 3)
         ((= n 3) 2)
         ((= 1 (A010051 n)) (A000040 (-1+ (A234840  (+ 1 (A000720 n))))))
         (else (reduce * 1 (map A234840  (ifactor n))))
   )
)

;; A235199-A235201 are now reserved for your use.

(definec (A235199 n)
   (cond ((< n 4) n)
         ((= n 5) 7)
         ((= n 7) 5)
         ((= 1 (A010051 n)) (A000040 (A235199  (A000720 n))))
         (else (reduce * 1 (map A235199  (ifactor n))))
   )
)


(definec (A235200 n)
   (cond ((< n 2) n)
         ((= n 3) 5)
         ((= n 5) 3)
         ((= 1 (A010051 n)) (A000040 (+ 1 (A235200 (- (A000720 n) 1)))))
         (else (reduce * 1 (map A235200 (ifactor n))))
   )
)


(definec (A235201 n)
   (cond ((< n 2) n)
         ((= n 3) 4)
         ((zero? (modulo n 4)) (* 3 (A235201 (/ n 4))))
         ((= 1 (A010051 n)) (A000040 (A235201  (A000720 n))))
         (else (reduce * 1 (map A235201 (ifactor n))))
   )
)

;; A235485-A235491 are now reserved for your use.

(define (A235485 n) (A235201 (A235487 n)))
(define (A235486 n) (A235487 (A235201 n)))


(definec (A235485v2 n)
   (cond ((< n 3) n)
;;       ((= n 3) 4)
         ((zero? (modulo n 3)) (* 4 (A235485 (/ n 3))))
         ((zero? (modulo n 8)) (* 5 (A235485v2 (/ n 8))))
         ((zero? (modulo n 4)) (* 3 (A235485v2 (/ n 4))))
         ((zero? (modulo n 14)) (* 9 (A235485v2 (/ n 14))))
         ((zero? (modulo n 49)) (* 27 (A235485v2 (/ n 49))))
         ((zero? (modulo n 7)) (* 6 (A235485v2 (/ n 7))))
         ((= 1 (A010051 n)) (A000040 (A235485v2  (A000720 n))))
         (else (reduce * 1 (map A235485v2 (ifactor n))))
   )
)



(define (A235493 n) (A235201 (A235489 n)))
(define (A235494 n) (A235489 (A235201 n)))

;; A235493- A235494


;; a(7) = 8, a(2^(3k)) = 7^k, a(2^(3k+1)) = 2*7^k, a(2^(3k+2)) = 4*7^k,
;; and a(p_i) = p_{a(i)} for primes with index i > 4.

(definec (A235487 n)
   (cond ((< n 4) n)
         ((= n 7) 8)
         ((zero? (modulo n 8)) (* 7 (A235487 (/ n 8))))
         ((= 1 (A010051 n)) (A000040 (A235487  (A000720 n))))
         (else (reduce * 1 (map A235487 (ifactor n))))
   )
)


;; a(3^(2k)) = 2^3k = 8^k, a(3^(2k+1)) = 3*2^3k,
;; and a(2^(3k)) = 3^2k = 9^k, a(2^(3k+1)) = 2*9^k, a(2^(3k+2)) = 4*9^k,
;; and a(p_i) = p_{a(i)} for primes with index i > 4.

(definec (A235489 n)
   (cond ((< n 8) n)
         ((zero? (modulo n 8)) (* 9 (A235489 (/ n 8))))
         ((zero? (modulo n 9)) (* 8 (A235489 (/ n 9))))
         ((= 1 (A010051 n)) (A000040 (A235489  (A000720 n))))
         (else (reduce * 1 (map A235489 (ifactor n))))
   )
)



;; Variations. Not permutations now, problems with 7.

;;       1  2     3     4
;; 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, ...
;; 0, 1, 3, 5, 9, 2,15,

(define (A234743 n) (A235199 (A234840 n)))

(define (A234744 n) (A234840 (A235199 n)))

(define (A235047 n) (A235199 (- (A234840 (+ n 1)) 1)))

(define (A235048 n) (- (A234840 (+ 1 (A235199 n))) 1))

(define (A098957variant n) (A056539 (A000040 n))) ;; With a(1)=2 instead of 1.

;; Not a permutation: E.g. (A056539 19) = 25 = 5*5 breaks it. Fixed points? A057889 is a GF(2)[X]-analogue

(definec (A235027 n) (if (< n 2) n (reduce * 1 (map A056539 (ifactor n)))))

;; As of 2014-01-06, we don't have A020639:
(definec (A235027v2 n) (cond ((< n 2) n) (else (* (A056539 (A020639 n)) (A235027v2 (/ n (A020639 n)))))))

(definec (A235027urpo n)
   (cond ((< n 2) n)
         ((= 1 (A010051 n)) (A056539 n))
         (else (reduce * 1 (map A235027urpo (ifactor n))))
   )
)


(define A016041 (MATCHING-POS 1 1 (lambda (n) (and (odd? n) (= n (A056539 n)) (prime? n)))))


(define A235028 (FIXED-POINTS 1 0 A235027))
;; Very stupid, just for checking: NOT TRUE
;; (define Asomething_else (MATCHING-POS (lambda (n) (every (lambda (d) (memq d (map A016041 (iota 22)))) (ifactor (A000265 n))))))

(define A235030 (MATCHING-POS 1 0 (lambda (n) (not (= (A235027 (A235027 n)) n)))))
(define A235030v2 (MATCHING-POS 1 2 (lambda (n) (any (lambda (x) (not (prime? x))) (map A056539 (ifactor n))))))



(definec (A235145 n)
   (cond ((= (A235027 (A235027 n)) n) 0)
         (else (+ 1 (A235145 (A235027 n))))
   )
)


(define (factor-and-bitrev n) (apply * (map A056539 (ifactor n))))

(definec (A235145v2 n)
   (cond ((= (factor-and-bitrev (factor-and-bitrev n)) n) 0)
         (else (+ 1 (A235145v2 (factor-and-bitrev n))))
   )
)

(definec (A235145v3 n)
   (cond ((= (A001222 (A235027 n)) (A001222 n)) 0)
         (else (+ 1 (A235145v3 (A235027 n))))
   )
)



(define A235146 (RECORD-POS 1 0 A235145))


(define (A057889 n) (if (zero? n) n (* (A030101 (A000265 n)) (A006519 n))))

(definec (A057889v2 n)
   (cond ((< n 2) n)
         ((= 1 (A091225 n)) (A056539 n))
         (else (reduce A048720bi 1 (map A057889v2 (GF2Xfactor n))))
   )
)

;; Fully multiplicative in GF(2)[X], of course:
;; (uniq (map (lambda (n) (- (A057889 (A048720bi (A002262 n) (A025581 n))) (A048720bi (A057889 (A002262 n)) (A057889 (A025581 n)))))  (iota0 1024))) -- >  (0)

(define (A246200 n) (/ (A057889 (* 3 n)) 3))


;; A235041: Special "cross-multiplicative" isomorphism from integers to GF2X-polynomials:
;, E.g. map A091206 ("Primes that are also irreducible GF(2)[X]-polynomials") to itself
;; and each A091214 ("Irreducible GF(2)[X]-polynomials that are composite integers")
;; to A091209 ("Primes that are composite GF(2)[X]-polynomials") and vice versa.
;; Cycle-structure when taking powers of, conjugating something?
;; (Of the other set of primes all are fixed, of the other, none). Which composites are fixed?
;; (Those with the same factorization in both Z and GF(2)[X] ?)
;; Also, for n whose all prime divisors are in A091206, the result is A234741(n) = n ? (For both permutations)

(definec (A235041 n)
   (cond ((< n 2) n)
         ((= 1 (A010051 n) (A091225 n)) n) ;; Primes in A091206 are fixed.
         ((= 1 (A010051 n)) (A091214 (A235043 n))) ;; A prime in A091209 maps to A091214.
         (else (reduce A048720bi 1 (map A235041 (ifactor n))))
   )
)

;; A235042: Special "cross-multiplicative" isomorphism from GF2X-polynomials to integers:
(definec (A235042 n)
   (cond ((< n 2) n)
         ((= 1 (A010051 n) (A091225 n)) n) ;; Primes in A091206 are fixed.
         ((= 1 (A091225 n)) (A091209 (A235044 n))) ;; An irreducible in A091214 maps to A091209.
         (else (reduce A004247bi 1 (map A235042 (GF2Xfactor n))))
   )
)

;; Partial sums of the characteristic function of A091209.
;; Note A235043(A091209(n)) = n for all n>=1.
(definec (A235043 n) (if (zero? n) n (+ (A235043 (- n 1)) (+ (* (A010051 n) (A091247 n))))))

;; Partial sums of the characteristic function of A091214.
;; Note A235044(A091214(n)) = n for all n>=1.
(definec (A235044 n) (if (zero? n) n (+ (A235044 (- n 1)) (+ (* (A066247 n) (A091225 n))))))

(define A235045 (FIXED-POINTS 1 0 A235041))
(define A235045v2 (FIXED-POINTS 1 0 A235042))


(define (Acontest2 n) (A056539 (A193231 (A056539 n))))

;; Not multiplicative, at least not completely. But is multiplicative at least for some n?
;; (map (lambda (n) (- (Acontest2 (A048720bi (A002262 n) (A025581 n))) (A048720bi (Acontest2 (A002262 n)) (Acontest2 (A025581 n)))))  (iota0 44))
;; --> (0 0 0 0 0 0 0 0 0 0 0 0 -1 0 0 0 0 -1 -1 0 0 0 0 -3 2 -3 0 0 0 0 2 2 2 2 0 0 0 0 -5 2 12 2 -5 0 0)

;; Not for all odd numbers:
;; (map (lambda (n) (- (Acontest2 (A048720bi (A005408 (A002262 n)) (A005408 (A025581 n)))) (A048720bi (Acontest2 (A005408 (A002262 n))) (Acontest2 (A005408 (A025581 n))))))  (iota0 44))
;; --> (0 0 0 0 2 0 0 2 2 0 0 -6 10 -6 0 0 8 6 6 8 0 0 -8 8 0 8 -8 0 0 -2 4 -14 -14 4 -2 0 0 2 -6 0 42 0 -6 2 0)

;; Cf. A135141/A227413
(definec (A236854 n)
   (cond ((< n 2) n)
         ((prime? n) (A002808 (A236854 (A000720 n))))
         (else (A000040 (A236854 (A065855 n))))
   )
)

(definec (A026239 n) ;; Beginning with the natural numbers, swap the k-th prime and k-th composite.
   (cond ((< n 2) n)
         ((prime? n) (A002808 (A000720 n)))
         (else (A000040 (A065855 n)))
   )
)


;; IDEAS, IDEAS, IDEAS!

;; A235032-A235036 are now reserved for your use.

(definec (A234741v2 n) (if (zero? n) n (reduce A048720bi 1 (ifactor n))))


(definec (A234742 n) (if (zero? n) n (reduce * 1 (GF2Xfactor n))))

(definec (A260712 n) (let ((next (A234742 n))) (if (= next n) 0 (+ 1 (A260712 next)))))
(definec (A260712loop n) (let loop ((n (A234742 n)) (prev_n n) (i 0)) (if (= n prev_n) i (loop (A234742 n) n (+ 1 i)))))

(define (A260713 n) (A260712 (A236844 n)))

(definec (A244323 n) (if (zero? n) 23 (A234742 (A244323 (- n 1)))))

(definec (A260729 n) (if (zero? n) 29 (A234742 (A260729 (- n 1)))))

(definec (A260735 n) (if (zero? n) 455 (A234742 (A260735 (- n 1)))))

(definec (A260441 n) (if (zero? n) 1361 (A234742 (A260441 (- n 1)))))

(define (A260719 n) (A091222 (A260735 n)))
(define (A260720 n) (A091222 (A260441 n)))


;; Rest transferred to intfun_a.scm : (and now transferred back!)



(definec (A234741 n) (if (< n 2) n (A048720bi (A020639 n) (A234741 (/ n (A020639 n))))))

(define (A236378 n) (- n (A234741 n)))
(define (A236379 n) (- (A234742 n) n))
(define (A236380 n) (- (A234742 n) (A234741 n)))
(define (A236380v2 n) (+ (A236378 n) (A236379 n))) ;; (n - A234741(n)) + (A234742(n) - n) = A234742(n) - A234741(n)

(define A235034v2 (ZERO-POS 1 0 A236378))
(define A235035v2 (ZERO-POS 1 0 A236379))
(define A235032v3 (ZERO-POS 1 0 A236380))
(define A235033v2 (NONZERO-POS 1 0 A236380))

;; Also things like (and also using A091255bi instead of gcd):
;; (map (lambda (n) (gcd (A234741 n) n)) (iota 128))
;; (map (lambda (n) (gcd (A234742 n) n)) (iota 128))
;; (map (lambda (n) (gcd (A234741 n) (A234742 n))) (iota 128))


(define A235032 (MATCHING-POS 1 0 (lambda (n) (or (zero? n) (equal? (ifactor n) (GF2Xfactor n))))))
(define A235032v2
   (MATCHING-POS 1 0
     (lambda (n) (and (= n (A234741 n) (A234742 n))))
   )
)


(define A235033 (MATCHING-POS 1 0 (lambda (n) (not (or (zero? n) (equal? (ifactor n) (GF2Xfactor n)))))))

(define (A235046 n) (if (zero? (A236380 n)) 0 1)) ;; X_A235033


;; Note that:
;; (uniq (map (lambda (n) (floor->exact (halve (- (A235046 (* (A002260 n) (A004736 n))) (A003986bi (A235046 (A002260 n)) (A235046 (A004736 n)))))))  (iota 5050))) --> (0)
;; (uniq (map (lambda (n) (floor->exact (halve (- (A235046 (A048720bi (A002260 n) (A004736 n))) (A003986bi (A235046 (A002260 n)) (A235046 (A004736 n)))))))  (iota 5050))) --> (0)
;; (uniq (map (lambda (n) (floor->exact (halve (- (A235046 (* (A002260 n) (A004736 n))) (- 1 (* (- 1 (A235046 (A002260 n))) (- 1 (A235046 (A004736 n)))))))))  (iota 5050))) --> (0) [Same as first one].

(define A235034 (MATCHING-POS 1 0 (lambda (n) (or (zero? n) (= n (reduce A048720bi 1 (ifactor n)))))))
(define A235035 (MATCHING-POS 1 0 (lambda (n) (or (zero? n) (= n (reduce * 1 (GF2Xfactor n)))))))

(define A235036 (MATCHING-POS 0 1 (lambda (n) (and (not (prime? n)) (equal? (ifactor n) (GF2Xfactor n))))))

;; A235039-A235050 are now reserved for your use.

(define A235039 (MATCHING-POS 0 1 (lambda (n) (and (odd? n) (not (prime? n)) (equal? (ifactor n) (GF2Xfactor n))))))
(define A235040 (MATCHING-POS 0 1 (lambda (n) (and (odd? n) (not (prime? n)) (= n (reduce A048720bi 1 (ifactor n)))))))

;; Multiplicative GF(2)[X] - Z conversions, with special pairing of irreducible polynomials and primes.

;; Or likewise, use at Z-end the complementary sets of A074832 and A204219 (or move 2 to the other set)
;; Odious primes and evil primes, etc.
;; 4k+1 and 4k+3 primes? (4k+1)(4k+1)=(4k+1), (4k+3)(4k+3)=(4k+1) Is this true also in GF(2)[X]?
;; If we map odious primes to A091206 (which is its subset), (and likewise, evil primes to A091214),
;; (maybe transferring the 3 to the other set?)
;; then what we get if we take multiple powers and consider their fixed points?








;; (define A234741_sorted_lista (sort (map A234741 (iota0 8192)) <))

;; (define A234741_uniq_sorted_lista (uniq A234741_sorted_lista))


;; (length A234741_uniq_sorted_lista) 4696

;; A236833-A236842 are now reserved for your use.

;; (A000695 n) or (A000290 n) ?
;; Of course the best upper bound is A234742 ?
(define A234742 A234742) ;; 

;; a(n) = Number of times n occurs in A234741. For all n, A234741(n) <= n.
;; Slow, quadratic algorithm. Compute (and cache) this first up to some big number, then compute the rest after that.
(definec (A236833 n)
  (let ((u (A234742 n)))
      (let loop ((k n) (ntimes 0))
             (cond ((> k u) ntimes)
                   ((= (A234741 k) n) (loop (+ k 1) (+ ntimes 1)))
                   (else (loop (+ k 1) ntimes))
             )
      )
  )
)


(define A236834 (ZERO-POS 1 0 A236833))
(define A236835 (MATCHING-POS 1 0 (lambda (n) (> (A236833 n) 1)))) ;; ;; a(1)=5, a(2)=17, ...

(define A236841 (NONZERO-POS 1 0 A236833))

(define A236839 (COMPOSE A236841 (MATCHING-POS 1 2 (lambda (n) (any (lambda (p) (= 1 (A066247 p))) (GF2Xfactor (A236841 n)))))))


(define (A236851 n) (A234741 (A234742 n)))
(define (A236852 n) (A234742 (A234741 n)))


(define A236850 (MATCHING-POS 1 0 (lambda (n) (or (zero? n) (every (lambda (p) (= 1 (A010051 p) (A091225 p))) (GF2Xfactor n))))))
(define A236850v2 (FIXED-POINTS 1 0 A236851))
(define A236850v3 (ZERO-POS 1 0  (lambda (n) (- (A234742 n) (A236837 n)))))

;; (same-intfuns? (COMPOSE A236850 1+) (COMPOSE A236850v2 1+) 5001) -->  #t
;; (same-intfuns? (COMPOSE A236850 1+) (COMPOSE A236850v3 1+) 5001) -->  #t


(define A236838 (MATCHING-POS 1 1 (lambda (n) (any (lambda (p) (= 1 (A066247 p))) (GF2Xfactor n)))))
(define A236838v2 (COMPLEMENT 1 A236850))
;; (same-intfuns? (COMPOSE A236838 1+) (COMPOSE A236838v2 1+) 15001) --> #t


(define A236860 (MATCHING-POS 1 0 (lambda (n) (or (zero? n) (every (lambda (p) (= 1 (A091225 p))) (ifactor n))))))
(define A236860v2 (FIXED-POINTS 1 0 A236852))

;; (same-intfuns? (COMPOSE A236860v2 1+) (COMPOSE A236860 1+) 5001) --> #t

(define Anot_same_as_A236860 (ZERO-POS 1 0  (lambda (n) (- (A234741 n) (A236846 n))))) ;; 741 does not occur here.


;; (map A236850 (iota 50))
;; (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 44 45 46 47 48 49 51 52)

;; (define Amuulit (COMPOSE A234741 (MATCHING-POS 1 1 (lambda (n) (any (lambda (p) (= 1 (A066247 p))) (GF2Xfactor (A234741 n)))))))
;; Cf. to: A236849:
;;
;; (map Amuulit (iota 37))
;; (43 79 86 89 125 135 149 158 172 178 181 209 227 235 250 263 270 281 293 298 311 316 317 331 344 349 356 362 371 383 393 399 401 418 421 443 447)


;; (uniq (map (lambda (n) (any (lambda (p) (= 1 (A066247 p))) (GF2Xfactor (A236834 n)))) (iota 1200))) -> (#t) ?!


(definec (A236836 n)
  (let ((u (A234742 n)))
   (let loop ((i 0))
        (let ((k (A234741 i)))
           (cond ((> k u) 0)
                 ((= k n) i)
                 (else (loop (+ i 1)))
           )
        )
   )
  )
)


(definec (A236837 n)
  (let ((u (A234742 n)))
   (let loop ((i u))
        (let ((k (A234741 i)))
           (cond ((< i n) 0)
                 ((= k n) i)
                 (else (loop (- i 1)))
           )
        )
   )
  )
)

;;;;;;;;;;;;;;;;


;; (define A234742_sorted_lista (sort (map A234742 (iota0 8192)) <))
;; (define A234742_uniq_sorted_lista (uniq A234742_sorted_lista))

;; (define (A236842 n) (list-ref A234742_uniq_sorted_lista (- n 1)))

;; A236844-A236863 are now reserved for your use.

;; (define (A236853_first_cut n) ;; a(n) = Number of times n occurs in A234742.
;;    (let loop ((i 0) (s 0))
;;         (let ((k (list-ref A234742_sorted_lista i)))
;;            (cond ((> k n) s)
;;                  ((= k n) (loop (+ i 1) (+ s 1)))
;;                  (else (loop (+ i 1) s))
;;            )
;;         )
;;    )
;; )


;; Note that A234742(n) >= n which helps us here.
;; a(n) = Number of times n occurs in A234742.
;; Slow, quadratic algorithm. Compute (and cache) this first up to some big number, then compute the rest after that.
(definec (A236853 n)
  (if (zero? n)
      1
      (let loop ((k n) (ntimes 0))
             (cond ((zero? k) ntimes)
                   ((= (A234742 k) n) (loop (- k 1) (+ ntimes 1)))
                   (else (loop (- k 1) ntimes))
             )
      )
  )
)


(definec (A236853v2 n) ;; Improve, far from correct!
  (if (< n 2)
      1
      (+ (A091225 n)
         (let loop ((d 2) (s 0))
              (cond ((= d n) s)
                    ((and (integer? (/ n d))
                          (not (zero? (A236853v2 (/ n d))))
                     )
                       (loop (+ d 1) (+ s (A236853v2 d)))
                    )
                    (else (loop (+ d 1) s))
              )
         )
      )
  )
)

;; My comments from the history-log of A235034:
;; a(n) = Sum_{d|n} A061858(n/d,d): A yet another sequence to be submitted.
;; And the positions of its nonzero terms, which is the complement of this sequence (A235034).
;; No, it is not true. A counter-example: 63 = 3*3*7. When we consider its (d,n/d) pairs (3,21)
;; and (9,7), in binary ('11','10101') and ('1001','111') we see that they will multiply
;; flawlessly without carries to give original 63 back, while still, if we do
;; A048720(3,3) = 5 = '101', which GF(2)[X]-multiplied with 7 '111' yields '11011' = 27.
;; So, we should examine all possible combinations of two (or more?) prime divisors of n to
;; determine whether n belongs to this sequence. Still, such a proposed sequence
;; a(n) = Sum_{d|n} A061858(n/d,d) would be interesting, together with say gcd(n,a(n)), etc.

(definec (A236862 n) ;; Characteristic function for A236842 (and of range of A234742). XXX - Check!
   (cond ((< n 2) 1)
         ((= 1 (A091225 n)) 1)
         ((prime? n) 0)
         (else
           (let loop ((d 2))
              (cond ((= d n) 0)
                    ((and (integer? (/ n d)) ;; Equally, we could use here condition: (= d (gcd n d))
                          (not (zero? (* (A236862 d) (A236862 (/ n d)))))
                     )
                       1
                    )
                    (else (loop (+ d 1)))
              )
           )
         )
   )
)


(definec (A236861 n) (if (zero? (A236833 n)) 0 1))

(define A236841v2 (NONZERO-POS 1 0 A236861))

(definec (A236861v2 n) ;; Characteristic function for A236841 (= range of A234741).
   (cond ((< n 2) 1)
         ((prime? n) 1)
         ((= 1 (A091225 n)) 0)
         (else
           (let loop ((d 2))
              (cond ((= d n) 0)
                    ((and (= d (A091255bi n d))
                          (not (zero? (* (A236861v2 d) (A236861v2 (GF2Xdivide n d)))))
                     )
                       1
                    )
                    (else (loop (+ d 1)))
              )
           )
         )
   )
)



(define A236842 (NONZERO-POS 1 0 A236862))
(define A236842v2 (NONZERO-POS 1 0 A236853))
(define A236844 (ZERO-POS 1 0 A236862))
(define A236844v2 (ZERO-POS 1 0 A236853))
(define A236845 (MATCHING-POS 1 0 (lambda (n) (> (A236853 n) 1))))

(define A236848 (MATCHING-POS 1 1 (lambda (n) (any (lambda (p) (= 1 (A091247 p))) (ifactor n)))))

(define A236848v2 (COMPLEMENT 1 A236860))
;; (same-intfuns? (COMPOSE A236848 1+) (COMPOSE A236848v2 1+) 15001) --> #t

(define A236849 (COMPOSE A236842 (MATCHING-POS 1 1 (lambda (n) (any (lambda (p) (= 1 (A091247 p))) (ifactor (A236842 n)))))))

;; Note that A234742(n) >= n which helps us here.
;; Least inverse of A234742: a(n) = maximal k such that A234742(k)=n, and 0 if no such k exists.

(definec (A236846 n)
   (let loop ((k n) (minv 0))
           (cond ((zero? k) minv)
                 ((= (A234742 k) n) (loop (- k 1) k))
                 (else (loop (- k 1) minv))
           )
   )
)


;; Note that A234742(n) >= n which helps us here.
;; Greatest inverse of A234742: a(n) = maximal k such that A234742(k)=n, and 0 if no such k exists.
(definec (A236847 n)
   (let loop ((i n))
           (cond ((zero? i) i)
                 ((= (A234742 i) n) i)
                 (else (loop (- i 1)))
           )
   )
)

(define Akuut (MATCHING-POS 1 0 (lambda (n) (and (> (A234742 n) n) (= 1 (- (A234742 (+ n 1)) (A234742 n)))))))

;; (map Akuut (iota 65))
;; --> (70 158 517 723 898 1046 1086 1166 1186 1498 1722 1986 2302 2454 2459 2460 2802 2970 2978 3093 3982 4057 4143 4194 4543 4604 4605 4622 4758 4858 4987 5428 6101 6851 8271 8390 8725 8848 9294 9366 9510 10175 10236 11311 12156 13718 16126 16378 16674 16751 18310 18466 19590 19773 20802 21852 22765 22838 23473 23504 24697 26269 29598 30635 31791)
;; ///
;; (map A234742 (map Akuut (iota 65)))
;; --> (182 350 1525 1299 1146 3030 3006 2702 2778 2522 1850 2106 5886 5718 5723 5724 4850 5018 5082 4069 4206 4641 12207 12186 11711 11772 11773 10766 11094 11010 11387 9924 6437 7475 24399 24374 24037 23376 21678 21846 22182 22463 22524 21423 20604 14934 16638 16386 48474 48495 47222 43098 45702 46269 37442 39324 59085 42294 24641 42000 31609 31005 35742 34859 33711)
;; ///
;; (map A234742 (map 1+ (map Akuut (iota 65))))
;; --> (183 351 1526 1300 1147 3031 3007 2703 2779 2523 1851 2107 5887 5719 5724 5725 4851 5019 5083 4070 4207 4642 12208 12187 11712 11773 11774 10767 11095 11011 11388 9925 6438 7476 24400 24375 24038 23377 21679 21847 22183 22464 22525 21424 20605 14935 16639 16387 48475 48496 47223 43099 45703 46270 37443 39325 59086 42295 24642 42001 31610 31006 35743 34860 33712)
;; 
;; 


(define Amuut (MATCHING-POS 1 0 (lambda (n) (and (> (A234742 n) n) (= -3 (- (A234742 (+ n 1)) (A234742 n)))))))

;; (map Amuut (iota 65))
;; --> (5 9 141 177 201 281 365 417 681 1157 1437 2097 2117 2725 3021 3041 3129 3137 3473 3557 3693 4241 5449 5645 5765 6401 6841 7341 7397 7957 9069 9781 10329 11345 11661 11969 12453 13841 14093 14221 14265 16397 16673 17153 17957 18093 18137 20505 20525 20645 21529 24249 24905 27857 28089 28521 32781 32817 33381 33489 33665 35473 38049 50021 51337)
;; 


(definec (A243916 n)
  (if (< n 3)
      0
      (let loop ((i (- (A000079 n) 1)))
           (cond ((and (prime? i) (prime? (/ (- i 1) 2))) i)
                 (else (loop (- i 2)))
           )
      )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define A246156 (COMPOSE A091242 (MATCHING-POS 1 1 (COMPOSE odd? A091242))))

;; (define A246157 (COMPOSE A091242 (MATCHING-POS 1 1 (COMPOSE (lambda (n) (and (odd? n) (= 1 (A010060 n)))) A091242))))

;; (define A246158 (COMPOSE A091242 (MATCHING-POS 1 1 (COMPOSE (lambda (n) (= 1 (A010060 n))) A091242))))

(define A246156aux (MATCHING-POS 1 1 (lambda (n) (odd? (A091242 n)))))
(define (A246156 n) (A091242 (A246156aux n)))

(define A246157aux (MATCHING-POS 1 1 (lambda (n) (let ((k (A091242 n))) (and (odd? k) (= 1 (A010060 k)))))))
(define (A246157 n) (A091242 (A246157aux n)))

(define A246157v2 (MATCHING-POS 1 1 (lambda (n) (= 1 (* (A000035 n) (A010060 n) (A091247 n))))))

(define A246158aux (MATCHING-POS 1 1 (lambda (n) (= 1 (A010060 (A091242 n))))))
(define (A246158 n) (A091242 (A246158aux n)))


;; A246258-A246282 are now reserved for your use.

(define (A246260 n) (A000035 (A048673 n)))
(define A246261 (MATCHING-POS 1 1 (lambda (n) (= 1 (modulo (A003961 n) 4))))) ;; I.e. k for which A048673(k) is odd.
(definec (A246262 n) (if (zero? n) n (+ (A246260 n) (A246262 (- n 1))))) ;; Inverse for A246261: A246262(A246261(n)) = n for all n >= 1.

(define A246263 (MATCHING-POS 1 1 (lambda (n) (= 3 (modulo (A003961 n) 4))))) ;; I.e. k for which A048673(k) is even.
(define (A246264 n) (- n (A246262 n))) ;; For all n >= 1, A246264(A246263(n)) = n.

(define (A246265 n) (/ (+ 1 (A048673 (A246261 n))) 2))
(define (A246266 n) (A246262 (A064216 (+ -1 n n))))

(define (A246267 n) (/ (A048673 (A246263 n)) 2))
(define (A246268 n) (A246264 (A064216 (+ n n))))

;; (same-intfuns1? A001477 (COMPOSE A246265 A246266) 1024) --> #t
;; (same-intfuns1? A001477 (COMPOSE A246266 A246265) 1024) --> #t
;; (same-intfuns1? A001477 (COMPOSE A246267 A246268) 1024) --> #t
;; (same-intfuns1? A001477 (COMPOSE A246268 A246267) 1024) --> #t

;; (map (COMPOSE halve 1+ A048673 Arapu) (iota 32))
;; 1,2,3,7,6,4,12,5,9,21,8,13,32,27,10,17,15,20,57,11,18,22,48,42,30,14,102,31,39,75,24,16
;; (map (COMPOSE halve A048673 Akapu) (iota 32))
;; 1,2,4,3,7,9,5,19,6,16,14,10,34,13,25,8,61,11,12,44,22,37,24,15,94,23,29,79,17,28,69,30

;; (define (A004613char n) (if (= 1 (A065338 n)) 1 0))
;; (define (Apulu n) (A004613char (A003961 n))) ;; as well, because A000035(Alulu(n)).
(define (A246269 n) (A065338 (A003961 n))) ;; mult.
(define (A246270 n) (A065339 (A003961 n)))
(define (A246270v2 n) (A007949 (A246269 n)))


(definec (A246271 n) (if (= 1 (A246260 n)) 0 (+ 1 (A246271 (A003961 n)))))

(define (A246271slow n) ;; A246261 gives the positions of zeros.
    (let loop ((i 0) (n n))
       (let ((next (A003961 n))) ;; And A048673 ?
          (if (= 1 (modulo next 4))
             i
             (loop (+ i 1) next)
          )
       )
    )
)

;; Records occur at positions: 1,2,5,21,46,574,3497,...
;; and they are: 0,1,2,6,7,11,15,...
;; First positions for distinct new values are: 1,2,5,21,46,55,66,91,574,1362,1419,1654,3497,4607,5263,6463
;; and they occur in order: 0,1,2,6,7,5,3,4,11,8,10,9,15,14,13,12
(define (Aekat n) (let loop ((i 1)) (if (= (A246271 i) n) i (loop (+ i 1)))))
;; (map Aekat (iota0 15)) --> (1 2 5 66 91 55 21 46 1362 1654 1419 574 6463 5263 4607 3497)

(definec (A246272 n) (if (= 1 (A065338 n)) 0 (+ 1 (A246272 (A003961 n)))))

(define (A246272slow n)
    (let loop ((i 0) (n n))
        (if (= 1 (A065338 n))
            i
            (loop (+ i 1) (A003961 n))
        )
    )
)

(define A246349 (RECORD-POS 1 1 A246272))
(define (A246350 n) (A246272 (A246349 n)))


;; A055396

(definec (A246277 n) (cond ((= 1 n) 0) ((even? n) (/ n 2)) (else (A246277 (A064989 n)))))

(define (A246277slow n) (if (= 1 n) 0 (let loop ((n n)) (if (even? n) (/ n 2) (loop (A064989 n))))))


(define (A246278bi row col) (if (= 1 row) (* 2 col) (A003961 (A246278bi (- row 1) col))))

;; Old offset=1 versions:
;; (define (A246278 n) (A246278bi (A002260 n) (A004736 n))) ;; Cf. A083221, A114537, A242378.
;; (define (A246279 n) (A246278bi (A004736 n) (A002260 n))) ;; Cf. A083140.

;; New offset=2 versions, with tacitly defined a(1) = 1.
(define (A246278 n) (if (<= n 1) n (A246278bi (A002260 (- n 1)) (A004736 (- n 1))))) ;; Cf. A083221, A114537, A242378.
(define (A246279 n) (if (<= n 1) n (A246278bi (A004736 (- n 1)) (A002260 (- n 1)))))

;; A253550-A253569 are now reserved for your use. 
(define (A253551 n) (A156552 (A246278 (+ 1 n))))
(define (A253551v2 n) (A135764bi (A002260 n) (A005941 (A004736 n))))
(define (A253551v3 n) (* (A000079 (- (A002260 n) 1)) (+ -1 (* 2 (A005941 (A004736 n))))))
(define (A253551v4 n) (* (A000079 (- (A002260 n) 1)) (A005408 (A156552 (A004736 n)))))
;; I.e. (define (A253551v4 n) (* (A000079 (- (A002260 n) 1)) (+ 1 (* 2 (A156552 (A004736 n))))))

(define (A253552 n) (+ -1 (A252752 (A005940 (+ 1 n)))))


(define (A254053 n) (A135764bi (A002260 n) (A249745 (A004736 n))))
(define (A254053bi row col) (A064216 (A254051bi row col)))
(define (A254053v2 n) (A254053bi (A002260 n) (A004736 n)))
(define (A254053v3 n) (A064216 (A254051 n)))
(define (A254053v4 n) (* (A000079 (- (A002260 n) 1)) (+ -1 (* 2 (A249745 (A004736 n))))))

(define (A254053v5 n) (* (A000079 (- (A002260 n) 1)) (A254050 (A004736 n))))


(define (A254054 n) (A254052 (A048673 n)))


(define (A253561bi row col) (A122111 (A246278bi row col)))
(define (A253561 n) (A122111 (A246278 n)))
(define (A253562 n) (A252752 (A122111 n)))

(define (A253568 n) (A122111 (* 2 n))) ;; The row 1 of A253561. Offset=1.

(definec (A253563 n)
   (cond ((< n 2) (+ 1 n))
         ((even? n) (A253560 (A253563 (/ n 2))))
         (else (A253550 (A253563 (/ (- n 1) 2))))
   )
)

(define (A253563v2 n) (A122111 (A005940 (+ 1 n))))

(define (A253564 n) (A156552 (A122111 n)))


(definec (A253565 n)
   (cond ((< n 2) (+ 1 n))
         ((even? n) (A253550 (A253565 (/ n 2))))
         (else (A253560 (A253565 (/ (- n 1) 2))))
   )
)

(define (A253565v2 n) (A122111 (A163511 n)))

(define (A253566 n) (A243071 (A122111 n)))


(define (A246273 n) (- (A246279 (+ 1 n)) 1)) ;; Cf. A114881.
(define (A246273v2 n) (A246676 (A054582 (- n 1))))
(define (A246273v3 n) (A246275 (A038722 n)))


;; (define (A246274 n) (+ 1 (packA001477 (- (A055396 (+ 1 n)) 1) (- (A246277 (+ 1 n)) 1))))
;; (define (A246274 n) (+ 1 (packA001477 (- (A055396 (+ 1 n)) 1) (- (A246277 (+ 1 n)) 1))))

(define (A246274 n)
  (let ((x (A246277 (+ 1 n)))
        (y (A055396 (+ 1 n)))
       )
    (* (/ 1 2) (- (expt (+ x y) 2) x y y y -2))
  )
)

(define (A246275 n) (- (A246278 (+ 1 n)) 1))
(define (A246275v3 n) (A246273 (A038722 n)))

;; (define (A246276 n) (+ 1 (packA001477 (- (A246277 (+ 1 n)) 1) (- (A055396 (+ 1 n)) 1))))

(define (A246276 n)
  (let ((x (A055396 (+ 1 n)))
        (y (A246277 (+ 1 n)))
       )
    (* (/ 1 2) (- (expt (+ x y) 2) x y y y -2))
  )
)


(definec (A252752 n)
  (if (<= n 1)
      n
      (let ((x (A055396 n))
            (y (A246277 n))
           )
        (+ 1 (* (/ 1 2) (- (expt (+ x y) 2) x y y y -2)))
      )
  )
)

(define (A252752v2 n) (if (<= n 1) n (+ 1 (A246276 (- n 1)))))

(definec (A249742 n)
  (let ((x (A055396 (+ 1 n)))
        (y (A078898 (+ 1 n)))
       )
    (* (/ 1 2) (- (expt (+ x y) 2) x y y y -2))
  )
)

(definec (A252460 n)
  (if (<= n 1)
      n
      (let ((x (A055396 n))
            (y (A078898 n))
           )
        (+ 1 (* (/ 1 2) (- (expt (+ x y) 2) x y y y -2)))
      )
  )
)

(define (A252460v2 n) (if (<= n 1) n (+ 1 (A249742 (- n 1)))))


(definec (A250252 n)
  (let ((x (A078898 (+ 1 n)))
        (y (A055396 (+ 1 n)))
       )
    (* (/ 1 2) (- (expt (+ x y) 2) x y y y -2))
  )
)


;; Cf. A054582 - A209268, A135764

(define (A246675 n) (* (A000079 (- (A055396 (+ 1 n)) 1)) (-1+ (* 2 (A246277 (+ 1 n))))))

(define (A246675v2 n) (A135764 (A246276 n)))
(define (A246675v3 n) (A054582 (-1+ (A246274 n))))

(define (A246676 n) (+ -1 (A242378bi (A007814 n) (+ 1 (A000265 n)))))

(define (A246676v2 n) (A246273 (A209268 n)))



(define (A249811 n) (+ -1 (A083221bi (A001511 n) (A003602 n))))
(define (A249811v2 n) (+ -1 (A083221bi (A001511 n) (/ (+ 1 (A000265 n)) 2))))
(define (A249812 n) (* (A000079 (- (A055396 (+ 1 n)) 1)) (+ -1 (* 2 (A078898 (+ 1 n))))))

(define (A249811v3 n) (A249815 (A246676 n)))
(define (A249812v2 n) (A246675 (A249816 n)))

;; (define (A000027yet_another n) (-1+ (A083221bi (A055396 (+ 1 n)) (A078898 (+ 1 n)))))
;; (define (A000027yet_another2 n) (+ -1 (A246278bi (A055396 (+ 1 n)) (A246277 (+ 1 n)))))

(define (A249815 n) (+ -1 (A083221bi (A055396 (+ 1 n)) (A246277 (+ 1 n)))))
(define (A249816 n) (+ -1 (A246278bi (A055396 (+ 1 n)) (A078898 (+ 1 n)))))

(define (A249815v2 n) (A249811 (A246675 n)))
(define (A249816v2 n) (A246676 (A249812 n)))

(define (A249815v3 n) (- (A249817 (+ 1 n)) 1))
(define (A249816v3 n) (- (A249818 (+ 1 n)) 1))

(define (A249817 n) (if (= 1 n) n (A083221bi (A055396 n) (A246277 n))))
(define (A249818 n) (if (= 1 n) n (A246278bi (A055396 n) (A078898 n))))

(define (A249817v2 n) (if (= 1 n) n (+ 1 (A249815 (- n 1)))))
(define (A249818v2 n) (if (= 1 n) n (+ 1 (A249816 (- n 1)))))

(define (A249817v3 n) (if (= 1 n) n (A083221bi (A055396 n) (A249821bi (A055396 n) (A078898 n)))))
(define (A249818v3 n) (if (= 1 n) n (A246278bi (A055396 n) (A249822bi (A055396 n) (A246277 n)))))

(define (A249821bi row col) (A246277 (A083221bi row col)))
(define (A249822bi row col) (A078898 (A246278bi row col)))

(define (A249821 n) (A249821bi (A002260 n) (A004736 n)))
(define (A249822 n) (A249822bi (A002260 n) (A004736 n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interlude:
;; Like A249818 and A250248, do similar cross-ref permutations between A095904 (or its transpose: A179216
;;   Permutation of triangular array of numbers (greater than 1) arranged by prime signature)
;;
;; by comparing with (a) A083221 and (b) A246278 and (c) a similar array but dispersing with A253550 and A253563.
;;
;; As all these have primes as their leftmost column, and apart from A083221 all others have squares of primes
;; as their second column.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A251717-A251728 are now reserved for your use. 

(define (A251721bi row col) (A249822bi row (A249821bi (+ row 1) col)))
(define (A251721 n) (A251721bi (A002260 n) (A004736 n)))

(define (A251721bi_v2 row col) (A078898 (A246278bi row (A246277 (A083221bi (+ row 1) col)))))
(define (A251721v2 n) (A251721bi_v2 (A002260 n) (A004736 n)))

(define (A251722bi row col) (A249822bi (+ row 1) (A249821bi row col)))
(define (A251722 n) (A251722bi (A002260 n) (A004736 n)))

(define (A251722bi_v2 row col) (A078898 (A246278bi (+ row 1) (A246277 (A083221bi row col)))))
(define (A251722v2 n) (A251722bi_v2 (A002260 n) (A004736 n)))


(define (A250469 n) (A249817 (A003961 (A249818 n))))
(define (A250469v2 n) (if (= 1 n) n (A083221bi (+ (A055396 n) 1) (A078898 n))))

(define (A250470 n) (A249817 (A064989 (A249818 n))))
(define (A250471 n) (/ (+ 1 (A250469 n)) 2))
(define (A250472 n) (A250470 (+ n n -1)))
(define (A250472v2 n) (if (= 1 n) n (A083221bi (- (A055396 (+ n n -1)) 1) (A078898 (+ n n -1)))))

(define (A250479 n) (A250470 (+ n n)))



(define (A249734 n) (A003961 (+ n n)))
(define (A249734v2 n) (* 3 (A003961 n)))

(define (A249735 n) (A003961 (+ n n -1)))

(define (A249827 n) (A003961 (A249734 n)))
(define (A249827v2 n) (A246278bi 3 n))

;; (same-intfuns1? A249827 (COMPOSE A003961 A016945 -1+ A048673) 1024) --> #t

;; A250469-A250480 are now reserved for your use. 
(define (A250473 n) (- (A250474 n) 1))

(define (A000879 n) (A000720 (A001248 n)))
(definec (A054272 n) (+ 1 (A000879 n) (- n)))
(define (A250474 n) (+ 2 (A054272 n)))

(define (A251723 n) (- (A054272 (+ n 1)) (A054272 n)))
(define (A251723v2 n) (- (A250474 (+ n 1)) (A250474 n)))
(define (A251723v3 n) (+ (A256447 n) (A256448 n)))

(define (A256446 n) (- (A250477 (+ n 1)) (A250477 n)))
(define (A256447 n) (- (A250477 n) (A250474 n)))

(define (A256448 n) (- (A250474 (+ n 1)) (A250477 n)))

(define (A256449 n) (- (A256447 n) (A256448 n)))

(define (A256449v2 n) (- (* 2 (A250477 n)) (A250474 n) (A250474 (+ n 1))))

;; A256468(n) = A256447(n)-1.
(definec (A256468 n)
 (let* ((p (A000040 n))
        (p2 (* p p))
       )
  (let loop ((s 0) (k (* p (A000040 (+ 1 n)))))
        (cond ((= k p2) s)
              (else (loop (+ s (if (prime? k) 1 0)) (- k 1)))
        )
  )
 )
)

;; A256469(n) = A256448(n)+2.
(definec (A256469 n)
 (let* ((p (A000040 n))
        (q (A000040 (+ 1 n)))
        (q2 (* q q))
       )
  (let loop ((s 0) (k (* p q)))
        (cond ((= k q2) s)
              (else (loop (+ s (if (prime? k) 1 0)) (+ k 1)))
        )
  )
 )
)

;; (define (A256470 n) (- (A256469fast n) (A256468fast n)))
(define (A256470 n) (- (A256469 n) (A256468 n)))
(define A256471 (ZERO-POS 1 1 A256470))

(define (A256472 n) (A000040 (A256471 n)))
(define (A256473 n) (A000040 (+ 1 (A256471 n))))

(define A256474 (MATCHING-POS 1 1 (lambda (n) (<= 0 (A256470 n)))))
(define A256475 (MATCHING-POS 1 1 (lambda (n) (negative? (A256470 n)))))

(define (A256484 n) (A000040 (A256474 n)))
(define (A256485 n) (A000040 (A256475 n)))

(define A256476 (MATCHING-POS 1 1 (lambda (n) (< 0 (A256470 n)))))
(define A256477 (MATCHING-POS 1 1 (lambda (n) (>= 0 (A256470 n)))))

(define (A251724 n) (if (= 1 n) 2 (* (A000040 (A251719 n)) (A000040 (+ (A251719 n) n -2)))))

(define (A251724v2 n) (A083221bi (A251719 n) n))

(definec (A250474slow n) (let loop ((k 2)) (if (not (prime? (A249821bi n k))) k (loop (+ k 1)))))

(define (A249747 n)  (floor->exact (/ (* (A002110 n) (A054272 n)) (A001248 n))))

;; (same-intfuns1? A249734 (COMPOSE A016945 -1+ A048673) 256) --> #t
;; (same-intfuns1? A048673 (COMPOSE A078898 A249734) 64) --> #t
;; (same-intfuns1? (COMPOSE A249734 A064216) (COMPOSE A003961 A243502) 1024) --> #t
;; (same-intfuns1? A249734 (COMPOSE A003961 A243501 A064216) 1024) --> #t

;; (define (A249823 n) (A246277 (A016945 (- n 1)))) ;; = A064216 ?
;; (define (A249824 n) (A078898 (A246278bi 2 n))) ;; = A048673 ?

(define (A249823 n) (A246277 (A084967 n)))
(define (A249824 n) (A078898 (A003961 (A003961 (* 2 n)))))
(define (A249824v2 n) (A078898 (A246278bi 3 n)))

(define (A249825 n) (A246277 (A084968 n)))
(define (A249826 n) (A078898 (A003961 (A003961 (A003961 (* 2 n))))))

(define (A250475 n) (A249824 (A249825 n)))
(define (A250476 n) (A249826 (A249823 n)))

(definec (A246677 n)
   (cond ((<= n 1) n)
         ((odd? n) (+ 1 (* 2 (A246677 (/ (- n 1) 2)))))
         (else (* (A000079 (- (A055396 (+ 1 n)) 1)) (-1+ (* 2 (A246277 (+ 1 n))))))
   )
)

(definec (A246678 n)
   (cond ((<= n 1) n)
         ((odd? n) (+ 1 (* 2 (A246678 (/ (- n 1) 2)))))
         (else (+ -1 (A242378bi (A007814 n) (+ 1 (A000265 n)))))
   )
)

(define (A246679 n) (/ (A246675 (* 2 n)) 2))
(define (A246679v2 n) (/ (A246677 (* 2 n)) 2))

(define (A246680 n) (/ (A246676 (* 2 n)) 2))
(define (A246680v2 n) (/ (A246678 (* 2 n)) 2))



(definec (A246683 n)
   (cond ((<= n 1) n)
         (else (* (A000079 (- (A055396 (+ 1 n)) 1)) (-1+ (* 2 (A246683 (A246277 (+ 1 n)))))))
   )
)


(definec (A246684 n)
   (cond ((<= n 1) n)
         ((even? n) (A253885 (A246684 (/ n 2))))
         (else (+ -1 (* 2 (A246684 (/ (+ n 1) 2)))))
   )
)

(definec (A246684v2 n)
   (cond ((<= n 1) n)
         (else (+ -1 (A242378bi (A007814 n) (* 2 (A246684v2 (A003602 n))))))
   )
)

;; Equal to A246684:
;; (definec (A246684v2 n)
;;    (cond ((<= n 1) n)
;;          ((even? n) (+ -1 (A003961 (+ 1 (A246684v2 (/ n 2))))))
;;          (else (+ -1 (* 2 (A246684v2 (/ (+ n 1) 2)))))
;;    )
;; )


(definec (A249813 n)
   (cond ((<= n 1) n)
         (else (* (A000079 (- (A055396 (+ 1 n)) 1)) (+ -1 (* 2 (A249813 (A078898 (+ 1 n)))))))
   )
)


(definec (A249814 n)
   (cond ((<= n 1) n)
         ((even? n) (A253886 (A249814 (/ n 2))))
         (else (+ -1 (* 2 (A249814 (/ (+ n 1) 2)))))
   )
)

(definec (A249814v1 n)
   (cond ((<= n 1) n)
         (else (+ -1 (A083221bi (A001511 n) (A249814v1 (A003602 n)))))
   )
)

(definec (A249814v2 n)
   (cond ((<= n 1) n)
         (else (+ -1 (A083221bi (A001511 n) (A249814v2 (/ (+ 1 (A000265 n)) 2)))))
   )
)

;; (definec (A249814v3 n)
;;   (cond ((<= n 1) n)
;;         ((even? n) (+ -1 (A250469 (+ 1 (A249814v3 (/ n 2))))))
;;         (else (+ -1 (* 2 (A249814v3 (/ (+ n 1) 2)))))
;;   )
;; )


(definec (A250243 n) ;; Deep version of A249816 /// Cf. also A249813 and A250246.
   (cond ((<= n 1) n)
         (else (+ -1 (A246278bi (A055396 (+ 1 n)) (A250243 (A078898 (+ 1 n))))))
   )
)

(definec (A250244 n) ;; Deep version of A249815 /// Cf. also A249814 and A250245.
   (cond ((<= n 1) n)
         (else (+ -1 (A083221bi (A055396 (+ 1 n)) (A250244 (A246277 (+ 1 n))))))
   )
)


(definec (A250245 n) ;; Deep v01 version of A249817.
   (cond ((<= n 1) n)
         (else (A083221bi (A055396 n) (A250245 (A246277 n))))
   )
)


(definec (A250246 n) ;; Deep v01 version A249818.
   (cond ((<= n 1) n)
         (else (A246278bi (A055396 n) (A250246 (A078898 n))))
   )
)

;; Same as "entanglement permutations":
(definec (A250245v2 n)
   (cond ((<= n 1) n)
         ((even? n) (* 2 (A250245v2 (/ n 2))))
         (else (A250469 (A250245v2 (A064989 n))))
   )
)

(definec (A250246v2 n)
   (cond ((<= n 1) n)
         ((even? n) (* 2 (A250246v2 (/ n 2))))
         (else (A003961 (A250246v2 (A250470 n))))
   )
)

(define (A250245v3 n) (A252755 (A243071 n)))

(define (A250246v3 n) (A163511 (A252756 n)))


;;;;

(definec (A250247 n) ;; Deep v10 version of A249817.
   (cond ((<= n 1) n)
         (else (A083221bi (A250247 (A055396 n)) (A246277 n)))
   )
)


(definec (A250248 n) ;; Deep v10 version A249818.
   (cond ((<= n 1) n)
         (else (A246278bi (A250248 (A055396 n)) (A078898 n)))
   )
)

;;;;

(definec (A250249 n) ;; Deep v11 version of A249817.
   (cond ((<= n 1) n)
         (else (A083221bi (A250249 (A055396 n)) (A250249 (A246277 n))))
   )
)


(definec (A250250 n) ;; Deep v11 version A249818.
   (cond ((<= n 1) n)
         (else (A246278bi (A250250 (A055396 n)) (A250250 (A078898 n))))
   )
)

;; (Non-) Fixed points, etc:
(define A249729 (MATCHING-POS 1 1 (lambda (n) (not (= n (A250249 n))))))
(define A249730 (MATCHING-POS 1 1 (lambda (n) (and (odd? n) (not (prime? n)) (= n (A250249 n))))))
(define A250251 (FIXED-POINTS 1 1 A250249))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; (map (lambda (n) (A246259bi 1 n)) (iota 32))
;; (1 3 4 9 10 11 12 13 14 16 23 25 27 30 31 33 34 35 36 37 38 39 40 42 44 47 48 49 52 56 58 59)
;; (map A246261 (iota 32))
;; (1 3 4 9 10 11 12 13 14 16 23 25 27 30 31 33 34 35 36 37 38 39 40 42 44 47 48 49 52 56 58 59)
;; 
;; (map (lambda (n) (A246259bi n 1)) (iota 10))
;; (1 2 5 66 91 55 21 46 1362 1654)
;; 
;; 
;; (define first_timers  (read-b-file-to-vector "seqs/b246271_n_occurs_for_the_first_time_at.txt" 22))
;; 
;; (map (lambda (n) (vector-ref first_timers n)) (iota0 21))
;; (1 2 5 66 91 55 21 46 1362 1654 1419 574 6463 5263 4607 3497 589843 430261 574823 567583 554111 545869)
;; 
;; (map A246259 (iota 55))
;; ;Value 67: (1 3 2 4 7 5 9 8 6 66 10 15 17 70 91 11 18 20 94 197 55 12 19 24 186 259 155 21 13 22 26 187 364 220 84 46 14 28 41 199 377 238 87 184 1362 16 29 45 237 413 467 189 414 1981 1654)
;; 

(define (A246280 n)  (A246259bi n 1)) ;; The leftmost column of A246259.

(define (A246258 n) (A246259bi (A004736 n) (A002260 n)))

(define (A246259 n) (A246259bi (A002260 n) (A004736 n)))
;; 
(define (A246259bi row col) ((rowfun-for-A246259 row) col))
;; 
(definec (rowfun-for-A246259 row) (MATCHING-POS 1 1 (lambda (k) (= (- row 1) (A246271 k)))))


;;;;;;;;;;;;;;;;;;

(definec (A246342 n) (if (zero? n) 12 (A048673 (A246342 (- n 1)))))
(definec (A246343 n) (if (zero? n) 12 (A064216 (A246343 (- n 1)))))

(definec (A246344 n) (if (zero? n) 16 (A048673 (A246344 (- n 1)))))
(definec (A246345 n) (if (zero? n) 16 (A064216 (A246345 (- n 1)))))

(define Ajotkut1 (MATCHING-POS 1 1 (lambda (n) (= (A000035 n) (A246260 n)))))
(define Ajotkut2 (MATCHING-POS 1 1 (lambda (n) (not (= (A000035 n) (A246260 n))))))

(define A048674 (MATCHING-POS 1 1 (lambda (n) (= n (A064989 (+ n n -1))))))

(define A246281 (MATCHING-POS 1 1 (lambda (n) (< (A003961 n) (* 2 n)))))
(define A246282 (MATCHING-POS 1 1 (lambda (n) (> (A003961 n) (* 2 n)))))
(define A246281v2 (MATCHING-POS 1 1 (lambda (n) (<= (A048673 n) n))))
(define A246282v2 (MATCHING-POS 1 1 (lambda (n) (> (A048673 n) n))))

(define A246351 (MATCHING-POS 1 1 (lambda (n) (< (A048673 n) n))))
(define A246352 (MATCHING-POS 1 1 (lambda (n) (>= (A048673 n) n))))

;; A246359-A246380 are now reserved for your use.

(define (A246360v2 n) (A048673 (A029744 n)))

;; An interleaving of A005010 (9*2^n) and A175806 (27*2^n): 27, 54, 108, 216, 432, 864, 1728, ...
;; A116453 Third smallest number with exactly n prime factors: 5, 9, 18, 36, 72, 144, 288, ...
;; 5,7,9,15,18,27,36,54,72,108,144,216,288,432,576,864,1152,1728,2304,3456,4608,6912,9216,13824,...

;; A247282-A247284 are now reserved for your use. 

;; (BISECT (map A247283 (iota 24)) 1) --> 7,15,27,54,108,216,432,864,1728,3456,6912,13824

(definec (A247283 n) (max_pt_in_range A048673 (+ (A029744 (+ n 3)) 1) (- (A029744 (+ n 4)) 1)))

(define (A247284 n) (A048673 (A247283 n)))

(define A246361 (MATCHING-POS 1 1 (lambda (n) (<= (A064216 n) n))))
(define A246362 (MATCHING-POS 1 1 (lambda (n) (> (A064216 n) n))))

(define A246371 (MATCHING-POS 1 1 (lambda (n) (< (A064216 n) n))))
(define A246372 (MATCHING-POS 1 1 (lambda (n) (>= (A064216 n) n))))

(define A246373 (MATCHING-POS 1 1 (lambda (n) (and (prime? n) (>= (A064216 n) n)))))
(define A246374 (MATCHING-POS 1 1 (lambda (n) (and (prime? n) (< (A064216 n) n)))))

(define A005382 (MATCHING-POS 1 1 (lambda (n) (and (prime? n) (prime? (+ n n -1)))))) ;; Is a subsequence of above.



(define ArekposA048673 (RECORD-POS 1 1 A048673)) ;; Seems to be A029744 Numbers of the form 2^n or 3*2^n.

(define (ArekvalA048673 n) (A048673 (ArekposA048673 n)))

(define (Ajoku n) ;; For how many next iterations the value is less than the value we started from?
  (let loop ((i 0) (n n))
     (let ((next (A064216 n)))
         (if (>= next n) i (loop (+ 1 i) next))
     )
  )
)



(define (A246363 n) (A135141 (A048673 n)))
(define (A246364 n) (A064216 (A227413 n)))

;; (sp (multiset->countpairs (map (lambda (n) (modulo n 8)) (map A246363 (iota 2047)))))
;; ((1223 . 7) (228 . 6) (226 . 3) (190 . 5) (61 . 2) (54 . 1) (47 . 4) (18 . 0))

;; (sp (multiset->countpairs (map (lambda (n) (modulo n 8)) (map A246364 (iota 4095)))))
;; ((643 . 1) (613 . 7) (606 . 5) (587 . 3) (570 . 2) (526 . 6) (362 . 4) (188 . 0))


;; A005940 as it is: maps evens to evens, odds to odds.
;; A246365 thus maps some subset of odds (those which A005940 maps to primes) to evens, and all evens to odds, like A135141 does.

;; (sp (multiset->countpairs (map (lambda (n) (modulo n 8)) (map A246365 (iota 488)))))
;; ((338 . 7) (68 . 3) (56 . 5) (17 . 1) (3 . 2) (2 . 0) (2 . 4) (2 . 6))

(define (A246365 n) (A135141 (A005940 n)))
(define (A246366 n) (A005941 (A227413 n)))

;; (sp (multiset->countpairs (map (lambda (n) (modulo n 8)) (map A246367 (iota 8192)))))
;; ((1801 . 3) (1797 . 1) (1785 . 5) (1781 . 7) (428 . 2) (428 . 6) (133 . 4) (39 . 0))

;; (sp (multiset->countpairs (map (lambda (n) (modulo n 8)) (map A246367 (iota 10000)))))
;; ((2208 . 1) (2202 . 3) (2183 . 5) (2178 . 7) (518 . 2) (510 . 6) (155 . 4) (46 . 0))


(define (A246367 n) (A005940 (A135141 n)))
(define (A246368 n) (A227413 (A005941 n)))


;; What we actually need:

;;
;; a(n) = A054429(A135141(n)) (entangling nonprimes/primes with even/odd numbers)
;; and its inverse b(n) = A227413(A054429(n)).
;; 1->1 version of A163511 which entangles even and odd numbers (A005843/A005408) with (A005843/A003961).
;; (Thus preserving the parity, at least.) Let's call this c, and its inverse d.
;; Then what kind of permutations are:
;; b(d(n)) (entangling A005843/A003961 with nonprimes/primes)
;;  and
;; c(a(n)) (entangling nonprimes/primes with even/A003961)
;; ?
;;

;; Here are c and d:

(definec (A246375 n) ;; cf. A163511
  (cond ((<= n 1) n)
        ((even? n) (* 2 (A246375 (/ n 2))))
        (else (A003961 (+ 1 (A246375 (/ (- n 1) 2)))))
  )
)


(definec (A246376 n) ;; cf. A243071
  (cond ((<= n 1) n)
        ((even? n) (* 2 (A246376 (/ n 2))))
        (else (+ 1 (* 2 (A246376 (- (A064989 n) 1)))))
  )
)

(define (A246375v2 n) (A246379 (A246378 n)))
(define (A246376v2 n) (A246377 (A246380 n)))


(definec (A246379 n)
   (cond ((< n 2) n)
         ((= 1 (A010051 n)) (A003961 (+ 1 (A246379 (A000720 n)))))
         (else (* 2 (A246379 (A065855 n))))
   )
)

(definec (A246380 n) ;; This is inverse permutation to A246379.
   (cond ((< n 2) n)
         ((even? n) (A002808 (A246380 (/ n 2))))
         (else (A000040 (A246380 (- (A064989 n) 1))))
   )
)

(define (A246379v2 n) (A246375 (A246377 n)))
(define (A246380v2 n) (A246378 (A246376 n)))


;; A246674-A246685 are now reserved for your use.

(definec (A246681 n)
   (cond ((<= n 1) (+ n 1))
         ((= 1 (A010051 n)) (A003961 (A246681 (A000720 n))))
         (else (* 2 (A246681 (A065855 n))))
   )
)

;; (map A246681v2 (iota0 32))
;; (1 2 3 5 4 7 6 9 10 8 14 11 12 15 18 20 16 25 28 21 22 24 30 27 36 40 32 50 56 33 42 13 44)

(define (A246681v2 n) (if (zero? n) 1 (A163511 (A246377 n))))
(define (A246682v2 n) (A246378 (A243071 n)))

;; (same-intfuns0? A001477 (COMPOSE A246682 A246681) 128) --> #t
;; (same-intfuns1? A001477 (COMPOSE A246681 A246682) 27) --> #t


(definec (A246682 n) ;; This is inverse permutation to A246681
   (cond ((<= n 2) (- n 1))
         ((even? n) (A002808 (A246682 (/ n 2))))
         (else (A000040 (A246682 (A064989 n))))
   )
)



;; (sp (multiset->countpairs (map (lambda (n) (modulo n 8)) (map (COMPOSE A246375 A054429 A135141) (iota 1024)))))
;; ((577 . 0) (127 . 4) (77 . 2) (70 . 6) (51 . 1) (45 . 3) (40 . 7) (37 . 5))
;;
;; (sp (multiset->countpairs (map (lambda (n) (modulo n 8)) (map (COMPOSE A246375 A054429 A135141) (iota 2048)))))
;; ((1234 . 0) (233 . 4) (139 . 2) (132 . 6) (88 . 1) (82 . 3) (71 . 7) (69 . 5))
;;
;; (/ 577 1024.0) = 0.5634765625
;; (/ 1234 2048.0) = 0.6025390625

;; (define (Anewperm_not_this n) (A135141 (A163511 n))) ;; Zero-based. As A005940(n+1) = A163511(A054429(n)).

;;
;; (define (sp spl) (sort spl (lambda (a b) (> (car a) (car b)))))
;;
;; (define ekat256 (map (COMPOSE A135141 A163511) (iota0 255)))
;; ;Value: ekat256
;; 
;; (sp (multiset->countpairs (map (lambda (n) (modulo n 8)) ekat256)))
;; ;Value 27: ((160 . 7) (42 . 3) (33 . 5) (13 . 1) (3 . 2) (2 . 0) (2 . 4) (1 . 6))
;; 
;; (define tokat256 (map (COMPOSE A135141 A005940) (iota 256)))
;; ;Value: tokat256
;; 
;; (sp (multiset->countpairs (map (lambda (n) (modulo n 8)) tokat256)))
;; ;Value 31: ((160 . 7) (42 . 3) (33 . 5) (13 . 1) (3 . 2) (2 . 0) (2 . 4) (1 . 6))
;; 

;; Note:


;; (sp (multiset->countpairs (map (lambda (n) (modulo n 8)) (map (COMPOSE A135141 A048673) (iota 256)))))
;; ((121 . 7) (32 . 3) (32 . 6) (29 . 5) (15 . 1) (12 . 2) (9 . 4) (6 . 0))

;; (sp (multiset->countpairs (map (lambda (n) (modulo n 8)) (map (COMPOSE A135141 A064216) (iota 256)))))
;; ((93 . 7) (51 . 6) (30 . 3) (25 . 5) (21 . 2) (15 . 4) (12 . 1) (9 . 0))

;; (sp (multiset->countpairs (map (lambda (n) (modulo n 8)) (map A135141 (iota 256)))))
;; ((118 . 7) (37 . 3) (32 . 5) (25 . 6) (15 . 1) (13 . 2) (10 . 4) (6 . 0))



;; (sp (multiset->countpairs (map (lambda (n) (modulo n 8)) (map A246363 (iota 10000)))))
;; ((6579 . 7) (988 . 6) (946 . 3) (878 . 5) (209 . 2) (180 . 4) (176 . 1) (44 . 0))

;; (sp (multiset->countpairs (map (lambda (n) (modulo n 8)) (map A246365 (iota 10000)))))
;; ((8266 . 7) (825 . 3) (792 . 5) (103 . 1) (5 . 2) (3 . 0) (3 . 4) (3 . 6))

;; (sp (multiset->countpairs (map (lambda (n) (modulo n 8)) (map A246367 (iota 10000))))) ;; favors odds over evens.
;; ((2208 . 1) (2202 . 3) (2183 . 5) (2178 . 7) (518 . 2) (510 . 6) (155 . 4) (46 . 0))

;; (sp (multiset->countpairs (map (lambda (n) (modulo n 8)) (map A246379 (iota 2048)))))
;; ((1234 . 0) (233 . 4) (139 . 2) (132 . 6) (88 . 1) (82 . 3) (71 . 7) (69 . 5))

;; (sp (multiset->countpairs (map (lambda (n) (modulo n 8)) (map A246681 (iota 10000)))))
;; ((6703 . 0) (974 . 4) (549 . 2) (545 . 6) (314 . 1) (312 . 3) (303 . 7) (300 . 5))


;; By Eric Angelini:

(definec (A248034 n) (if (zero? n) n (vector-ref (A248034aux_digit_counts (- n 1)) (modulo (A248034 (- n 1)) 10))))

(definec (A248034aux_digit_counts n)
  (cond ((zero? n) (vector 1 0 0 0 0 0 0 0 0 0))
        (else
          (let loop ((digcounts-for-n (vector-copy (A248034aux_digit_counts (- n 1))))
                     (n (A248034 n))
                    )
            (cond ((zero? n) digcounts-for-n)
                  (else
                      (vector-set! digcounts-for-n (modulo n 10) (+ 1 (vector-ref digcounts-for-n (modulo n 10))))
                      (loop digcounts-for-n (floor->exact (/ n 10)))
                  )
            )
          )
        )
  )
)

;; Variants:

(define (A000030 n) (let loop ((n n)) (if (< n 10) n (loop (floor->exact (/ n 10))))))

(definec (A249009 n) (if (zero? n) n (vector-ref (A249009aux_digit_counts (- n 1)) (A000030 (A249009 (- n 1))))))

(definec (A249009aux_digit_counts n)
  (cond ((zero? n) (vector 1 0 0 0 0 0 0 0 0 0))
        (else
          (let loop ((digcounts-for-n (vector-copy (A249009aux_digit_counts (- n 1))))
                     (n (A249009 n))
                    )
            (cond ((zero? n) digcounts-for-n)
                  (else
                      (vector-set! digcounts-for-n (modulo n 10) (+ 1 (vector-ref digcounts-for-n (modulo n 10))))
                      (loop digcounts-for-n (floor->exact (/ n 10)))
                  )
            )
          )
        )
  )
)


(definec (A249068 n) (if (zero? n) n (vector-ref (A249068aux_digit_counts (- n 1)) (modulo (A249068 (- n 1)) 8))))

(definec (A249068aux_digit_counts n)
  (cond ((zero? n) (vector 1 0 0 0 0 0 0 0))
        (else
          (let loop ((digcounts-for-n (vector-copy (A249068aux_digit_counts (- n 1))))
                     (n (A249068 n))
                    )
            (cond ((zero? n) digcounts-for-n)
                  (else
                      (vector-set! digcounts-for-n (modulo n 8) (+ 1 (vector-ref digcounts-for-n (modulo n 8))))
                      (loop digcounts-for-n (floor->exact (/ n 8)))
                  )
            )
          )
        )
  )
)


;; a(n+1) gives the number of occurrences of the first digit of a(n) in factorial base (i.e. A099563(a(n))) so far,
;; up to and including a(n), with a(0)=0. 
;; i.e.
;; a(0) = 0 and a(n) gives the number of occurrences of the first digit of a(n-1) in factorial base (i.e. A099563(a(n-1))) so far, up to and including a(n-1). 


;; a(0) = 0
;; a(1) = 1
;; a(2) = 1
;; a(3) = 2 = '10'
;; a(4) = 3 = '11' as '1' occurs at a(1) and a(2) and a(3))
;; a(5) = 5 = '21' as '1' occurs once at a(1), a(2), a(3) and twice at a(4).
;; a(6) = 1 as '2' so far occurs on at a(5)
;; a(7) = 7 = '101'
;; a(8) = 9 = '111' in factorial base
;; a(9) = 12 = '200' in factorial base
;; a(10) = 2 as '2' occurs so far only once at a(5) and once at a(9).

(definec (A249069 n) (if (zero? n) n (vector-ref (A249069aux_digit_counts (- n 1)) (A099563 (A249069 (- n 1))))))


(definec (A249069aux_digit_counts n)
  (cond ((zero? n) (vector 1))
        (else
          (let* ((start_n (A249069 n))
                 (copy-of-prevec (vector-copy (A249069aux_digit_counts (- n 1))))
                 (newsize (max (vector-length copy-of-prevec) (+ 1 (A246359 start_n))))
                 (digcounts-for-n (vector-grow copy-of-prevec newsize))
                )
             (let loop ((n start_n)
                        (i 2)
                       )
                (cond ((zero? n) digcounts-for-n)
                      (else
                         (vector-set! digcounts-for-n (modulo n i)
                                      (+ 1 (or (vector-ref digcounts-for-n (modulo n i)) 0))
                         )
                         (loop (floor->exact (/ n i)) (+ i 1))
                      )
                )
            )
          )
        )
  )
)



(definec (A249070 n) (if (zero? n) n (vector-ref (A249070aux_digit_counts (- n 1)) (A246359 (A249070 (- n 1))))))


(definec (A249070aux_digit_counts n)
  (cond ((zero? n) (vector 1))
        (else
          (let* ((start_n (A249070 n))
                 (copy-of-prevec (vector-copy (A249070aux_digit_counts (- n 1))))
                 (newsize (max (vector-length copy-of-prevec) (+ 1 (A246359 start_n))))
                 (digcounts-for-n (vector-grow copy-of-prevec newsize))
                )
             (let loop ((n start_n)
                        (i 2)
                       )
                (cond ((zero? n) digcounts-for-n)
                      (else
                         (vector-set! digcounts-for-n (modulo n i)
                                      (+ 1 (or (vector-ref digcounts-for-n (modulo n i)) 0))
                         )
                         (loop (floor->exact (/ n i)) (+ i 1))
                      )
                )
            )
          )
        )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A249143-A249154 are now reserved for your use. 

(define (A249143 n) (A136480 (A249144 n))) ;; One-based.

;; One-based:
(definec (A249144 n) (if (< n 2) n (vector-ref (A249144aux_runlen_counts (- n 1)) (-1+ (A136480 (A249144 (- n 1)))))))

(definec (A249144aux_runlen_counts n)
  (cond ((zero? n) (vector 1))
        (else
          (let* ((a_n (A249144 n))
                 (copy-of-prevec (vector-copy (A249144aux_runlen_counts (- n 1))))
                 (newsize (max (vector-length copy-of-prevec) (A043276 a_n)))
                 (lencounts-for-n (vector-grow copy-of-prevec newsize))
                )
             (let loop ((runlens (binexp->runcount1list a_n)))
                (cond ((null? runlens) lencounts-for-n)
                      (else
                         (vector-set! lencounts-for-n (- (car runlens) 1)
                                      (+ 1 (or (vector-ref lencounts-for-n (- (car runlens) 1)) 0))
                         )
                         (loop (cdr runlens))
                      )
                )
            )
          )
        )
  )
)


;;;


(define (A249145 n) (A043276 (A249146 n))) ;; One-based.

;; One-based also:
(definec (A249146 n) (if (< n 2) n (vector-ref (A249146aux_runlen_counts (- n 1)) (-1+ (A043276 (A249146 (- n 1)))))))

(definec (A249146aux_runlen_counts n)
  (cond ((zero? n) (vector 1))
        (else
          (let* ((a_n (A249146 n))
                 (copy-of-prevec (vector-copy (A249146aux_runlen_counts (- n 1))))
                 (newsize (max (vector-length copy-of-prevec) (A043276 a_n)))
                 (lencounts-for-n (vector-grow copy-of-prevec newsize))
                )
             (let loop ((runlens (binexp->runcount1list a_n)))
                (cond ((null? runlens) lencounts-for-n)
                      (else
                         (vector-set! lencounts-for-n (- (car runlens) 1)
                                      (+ 1 (or (vector-ref lencounts-for-n (- (car runlens) 1)) 0))
                         )
                         (loop (cdr runlens))
                      )
                )
            )
          )
        )
  )
)

;;;;;;;;;;;;;;



(define (A249147 n) (A055396 (A249148 n)))


(definec (A249148 n) (if (= 1 n) 1 (vector-ref (A249148aux_primefactor_counts (- n 1)) (A055396 (A249148 (- n 1))))))

(definec (A249148aux_primefactor_counts n)
  (cond ((= 1 n) (vector 2))
        (else
          (let* ((a_n (A249148 n))
                 (copy-of-prevec (vector-copy (A249148aux_primefactor_counts (- n 1))))
                 (newsize (max (vector-length copy-of-prevec) (+ 1 (A061395 a_n))))
                 (pf_counts_vec (vector-grow copy-of-prevec newsize))
                )
             (let loop ((pf_indices (map A049084 (factor a_n)))) ;; 1 is the only number which gets index 0.
                (cond ((null? pf_indices) pf_counts_vec)
                      (else
                         (vector-set! pf_counts_vec
                                      (car pf_indices)
                                      (+ 1 (or (vector-ref pf_counts_vec (car pf_indices)) 0))
                         )
                         (loop (cdr pf_indices))
                      )
                )
            )
          )
        )
  )
)



;; A249336-A249347 are now reserved for your use.

(definec (A249336 n)
  (if (<= n 1)
      n
      (let ((s (A056239 (A249336 (- n 1)))))
        (let loop ((i (- n 1)) (k 0))
              (cond ((zero? i) k)
                    ((= (A056239 (A249336 i)) s) (loop (- i 1) (+ k 1)))
                    (else (loop (- i 1) k))
              )
        )
      )
  )
)


(define (A249338 n) (A056239 (A249336 n)))

(define A249339 (MATCHING-POS 1 1 (lambda (n) (= 1 (A249336 n)))))
(define A249339v2 (ZERO-POS 1 1 A249338))

(define A249340 (MATCHING-POS 1 1 (lambda (n) (and (= 1 (A249336 (+ 1 n))) (or (= 1 (A249336 n)) (prime? (A249336 n)))))))
(define A249340v2 (RECORD-POS 1 1 A249338))

(define (A249072 n) (A056239 (A249337 n)))

(define A249341 (MATCHING-POS 1 1 (lambda (n) (= 1 (A249337 n)))))
(define A249341v2 (ZERO-POS 1 1 A249072))

(define A249342 (MATCHING-POS 1 1 (lambda (n) (or (= 1 n) (and (= 1 (A249337 (+ 1 n))) (prime? (A249337 n)))))))
(define A249342v2 (RECORD-POS 1 1 A249072))



(definec (A249337 n)
  (if (<= n 2)
      n
      (let ((s (A056239 (A249337 (- n 1)))))
        (let loop ((i (- n 1)) (k 0))
              (cond ((zero? i) k)
                    ((= (A056239 (A249337 i)) s) (loop (- i 1) (+ k 1)))
                    (else (loop (- i 1) k))
              )
        )
      )
  )
)

;;;;;;;;;;;;;;;;;;;;;

(definec (Anewone1 n)
  (if (<= n 1)
      n
      (let ((s (A000010 (Anewone1 (- n 1)))))
        (let loop ((i (- n 1)) (k 0))
              (cond ((zero? i) k)
                    ((= (A000010 (Anewone1 i)) s) (loop (- i 1) (+ k 1)))
                    (else (loop (- i 1) k))
              )
        )
      )
  )
)

;;;;;;;;;;;;;;;;;;;;;

;; From the draft queue:
(definec (A248479 n) (cond ((= 1 n) 1) ((= 2 n) 3) ((odd? n) (- (A248479 (- n 1)) (A248479 (- n 2)))) (else (* (A248479 (- n 1)) (A248479 (- n 2))))))


(define (A249344bi row col) ;; row>=1 and col>=1
  (let ((p (A000040 row)))
    (let loop ((n col) (i 0))
          (cond ((not (zero? (modulo n p))) i)
                (else (loop (/ n p) (+ i 1)))
          )
    )
  )
)


(define (A249344 n) (A249344bi (A002260 n) (A004736 n)))
(define (A060175 n) (A249344bi (A004736 n) (A002260 n)))

;; A249421-A249440 are now reserved for your use.

(define (A249421bi row col) (A249344bi row (A001142 (- col 1))))

(define (A249421 n) (A249421bi (A002260 n) (A004736 n)))

(define (A249422 n) (A249421bi (A004736 n) (A002260 n)))

;; A249738-A249747 are now reserved for your use. 


(definec (A249739 n)
   (let loop ((n n) (p (A020639 n)))
      (cond ((= 1 n) n)
            ((zero? (modulo n (* p p))) p)
            (else (loop (/ n p) (A020639 (/ n p))))
      )
   )
)


(definec (A249740 n)
   (cond ((= n 1) n)
         ((zero? (A241917 n)) (A006530 n)) ;; If n is in A070003?
         (else (A249740 (A052126 n)))
   )
)

(define (A249740v2 n)
   (let loop ((n n) (p (A006530 n)))
      (cond ((= 1 n) n)
            ((zero? (modulo n (* p p))) p)
            (else (loop (/ n p) (A006530 (/ n p))))
      )
   )
)


(definec (A075167 n) (if (= 1 n) 0 (+ (A075167 (A071178 n)) (- (A061395 n) (A061395 (A051119 n))) (A253783 (A051119 n)))))

(definec (A253783 n) (if (= 1 n) 0 (+ (A075167 (+ 1 (A071178 n))) (- (A061395 n) (A061395 (A051119 n))) (A253783 (A051119 n)))))

(definec (A106490v2 n) (if (= 1 n) 0 (+ 1 (A106490v2 (A071178 n)) (A106490v2 (A051119 n)))))

(define A253781 (ZERO-POS 1 1 (lambda (n) (- (A075167 n) (A252464 n)))))

(define A253782 (NONZERO-POS 1 1 (lambda (n) (- (A075167 n) (A252464 n)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A254864-A254865 are now reserved for your use. 

(define (A254864 n) (A254864bi (A002024 n) (A002260 n)))
(define (A254864bi n k) (mul A000027 (+ 1 (- n (floor->exact (/ n (expt 3 k))))) n))
(define (A254864biv2 n k) (/ (A000142 n) (A000142 (- n (floor->exact (/ n (expt 3 k)))))))

(define (A254865 n) (mul A000027 (+ 1 (- n (floor->exact (/ n 3)))) n))

(define (A254865v2 n) (A254864bi n 1))

(define (A088487 n) (add (lambda (k) (floor->exact (/ (A254864bi n k) (A254864bi (- n 1) k)))) 1 8)) ;; Bagula

(define (A254876 n) (A254876bi (A002024 n) (A002260 n)))

(define (A254876bi n k) (/ (A000142 n) (mul A000027 (- n (floor->exact (/ (* 2 n) (expt 3 k)))) (- n (floor->exact (/ n (expt 3 k)))))))

(define (A088488 n) (add (lambda (k) (floor->exact (/ (A254876bi n k) (A254876bi (- n 1) k)))) 1 8)) ;; Bagula

