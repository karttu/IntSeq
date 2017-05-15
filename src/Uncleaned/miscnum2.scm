
;; Last edited 2017-05-14 by Antti Karttunen, added yet more unsorted functions to this old MIT/GNU-Scheme module.

(declare (usual-integrations))

(load "definech")

(load "c:\\program files (x86)\\slib\\mitscheme.init") ;; A. Jaffer's SLIB Scheme library.


(require 'factor) ;; This is for prime? from SLIB-library.

;; XXX - XFER: Make this a proper transform (of N -> Z functions and move to transforms.ss 

;; HUOM! ;; (map 1+ (ordinal-transform-of-list (ordinal-transform-of-list (map A278243 (iota 120)))))
;; Not the same as A286378 Restricted growth sequence computed for Stern-polynomial related filter-sequence A278243.


(define (ordinal-transform-of-list lista) ;; Not needed now: plusoffset
  (let ((resvec (make-vector (length lista)))
        (occurrences (make-equal-hash-table)) ;; Do NOT use: make-strong-eq-hash-table !!!
       )
    (let loop ((lista lista) (i 0))
        (cond ((null? lista)
;;                (map (lambda (n) (+ plusoffset n))
                      (vector->list resvec)
;;                )
              )
              ((hash-table/get occurrences (car lista) 0)
                 =>
                (lambda (n-times-so-far)
                   (begin
                      (hash-table/put! occurrences
                                       (car lista)
                                       (+ 1 n-times-so-far)
                      )
                      (vector-set! resvec i (+ 1 n-times-so-far))
                      (loop (cdr lista) (+ 1 i))
                   )
                )
              )
        )
    )
  )
)


(define (rgs-transform-of-list lista)
  (let ((resvec (make-vector (length lista)))
        (occurrences (make-equal-hash-table)) ;; Do NOT use: make-strong-eq-hash-table !!!
       )
    (begin
      (let loop ((lista lista) (i 0) (first-unused-number 1))
        (cond ((null? lista)
                      (vector->list resvec)
              )
              ((hash-table/get occurrences (car lista) #f)
                 =>
                (lambda (prev-position) ;; This (car lista) has been encountered before.
                   (begin
                      (vector-set! resvec i (vector-ref resvec prev-position))
                      (loop (cdr lista) (+ 1 i) first-unused-number)
                   )
                )
              )
              (else ;; Otherwise, a new fresh value.
                 (begin
                      (hash-table/put! occurrences
                                       (car lista)
                                       i
                      )
                      (vector-set! resvec i first-unused-number)
                      (loop (cdr lista) (+ 1 i) (+ 1 first-unused-number))
                 )
              )
        )
      )
    )
  )
)


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

(definec (A000010 n) (if (= 1 n) n (apply * (sub1from1st_nums (sort (factor n) <)))))

(define (A009195 n) (gcd n (A000010 n))) ;; [David W. Wilson] o=1: a(n) = gcd(n, phi(n)).

;; A109395 [Franz Vrabec] o=1: Denominator of phi(n)/n = Prod_{p|n} (1-1/p); phi(n)=A000010(n), the Euler totient function.
(define (A109395 n) (/ n (A009195 n)))


(define (A051953 n) (- n (A000010 n))) ;; [Labos Elemer] o=1: Cototient(n) := n - phi(n).


;; XFER: numtheory.jacobi.ss or such:

;; A112046 [AK] o=1: a(n) = first i >= 1 for which the Jacobi symbol J(i,2n+1) is not +1 (i.e. is either 0 or -1). 
(definec (A112046 n) (let loop ((i 1)) (if (not (= 1 (fix:jacobi-symbol i (+ n n 1)))) i (loop (+ 1 i)))))

(define (A112049 n) (A000720 (A112046 n)))

;; A053760 Smallest positive quadratic nonresidue modulo p, where p is the n-th prime.
(definec (A053760 n) (if (= 1 n) 2 (let ((p (A000040 n))) (let loop ((i 1)) (if (= -1 (fix:jacobi-symbol i p)) i (loop (+ 1 i)))))))


;; A053761 [Steven Finch] o=1: Least positive integer k for which the Jacobi symbol (k|2*n-1) is less than 1, where 2*n-1 is a nonsquare; a(n)=0 if 2*n-1 is a square.
;; Cf. A268829 and all that jazz...
(define (A053761 n) (if (= 1 n) 0 (* (- 1 (A010052 (+ n n -1))) (A112046 (- n 1)))))



;; XFER Numtheory.sigma.ss or such:

;; %F A000203 Multiplicative with a(p^e) = (p^(e+1)-1)/(p-1). - David W. Wilson, Aug 01, 2001.

(definec (A000203 n)
   (fold-left (lambda (prod p.e) (* prod (/ (- (expt (car p.e) (+ 1 (cdr p.e))) 1) (- (car p.e) 1))))
              1
              (if (= 1 n) (list) (elemcountpairs (ifactor n))) ;; (sort (factor n) <)
   )
)

(define (A000593 n) (A000203 (A000265 n))) ;; [NJAS] o=1: Sum of odd divisors of n.

(define (A161942 n) (A000265 (A000203 n))) ;; [Franklin T. Adams-Watters] o=1: Odd part of sum of divisors of n. 

;; A082903 [Labos Elemer] o=1: a(n) = gcd(2^n, sigma_1(n)) = gcd(A000079(n), A000203(n)) also a(n) = gcd(2^n, sigma_3(n)) = gcd(A000079(n), A001158(n)). 

(define (A082903 n) (gcd (A000079 n) (A000203 n)))

(define (A082903most_likely n) (/ (A000203 n) (A161942 n)))

(define (A286357 n)  (A001511 (A000203 n)))
(define (A286357v2 n) (A070939 (/ (A000203 n) (A161942 n))))


(define (A001065 n) (- (A000203 n) n)) ;; [NJAS, Guy] o=1: Sum of proper divisors (or aliquot parts) of n: sum of divisors of n that are less than n.

(define (A033879 n) (- (* 2 n) (A000203 n))) ;; [NJAS] o=1: Deficiency of n, or 2n - (sum of divisors of n). 

(define (A033880 n) (- (A000203 n) (* 2 n))) ;; [NJAS] o=1: Abundance of n, or (sum of divisors of n) - 2n. 

(define (A285702 n) (A000010 (A064216 n)))
(define (A285703 n) (A000203 (A064216 n)))
(define (A285704 n) (- (A285703 n) n))
(define (A285705 n) (- (* 2 n) (A285703 n)))


(define (A286385 n) (- (A003961 n) (A000203 n)))
(define (A286385v2 n) (+ -1 (A285705 (A048673 n))))
;; (define Akolmoset (MATCHING-POS 1 1 (lambda (n) (= 3 (A286385 n))))) ???
;; (define Aviitoset (MATCHING-POS 1 1 (lambda (n) (= 5 (A286385 n))))) ;; A031924


(define (A285700 n) (if (zero? (A010051 n)) 0 (+ 1 (A285700 (+ n n -1)))))

;; A181715 [M. F. Hasler] o=1: Length of the complete Cunningham chain of the second kind starting with prime(n).
(define (A181715 n) (A285700 (A000040 n)))

(definec (A285701 n)
   (cond ((zero? (A010051 n)) 0)
         ((or (= 2 n) (= 3 n)) 1)
         ((zero? (A010051 (+ n n -1))) 1)
         (else (+ 1 (A285701 (A000040 (+ -1 (A000720 (+ n n -1)))))))
   )
)

(definec (A285701v2 n)
   (cond ((zero? (A010051 n)) 0)
         ((or (= 2 n) (= 3 n)) 1)
         (else (+ 1 (A285701v2 (A064216 n))))
   )
)

(define (A285706 n) (A285701 (A000040 n)))


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

(define A085271 (MATCHING-POS 1 1 (lambda (n) (and (= 2 (A001222 n)) (= (A000523 (A020639 n)) (A000523 (A006530 n)))))))


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
(definec (A258851v0 n)
  (cond ((zero? n) 0)
        ((= 1 n) 0)
        ((prime? n) (A000720 n))
        (else
          (let* ((a (A020639 n))
                 (b (/ n a))
                )
             (+ (* a (A258851v0 b)) (* b (A258851v0 a)))
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


(define (A107078 n) (- 1 (A008966 n)))

(define (A107079 n) (+ 1 (A013928 n)))
(define (A107079v2 n) (+ (A013928 (+ 1 n)) (A107078 n)))

;; (same-intfuns1? A107079 A107079v2 16387) --> #t

;; (same-intfuns1? A000027 (COMPOSE A107079 A005117) 8192) --> #t


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

(define A285315 (MATCHING-POS 1 0 (lambda (n) (< (A019565 n) n))))
(define A285316 (MATCHING-POS 1 0 (lambda (n) (> (A019565 n) n))))

;; Squarefree terms in A285315:
(define A285317 (MATCHING-POS 1 0 (lambda (n) (and (< (A019565 n) n) (not (zero? (A008683 n)))))))

;; Their "A019565-inverses":
(define (A285318 n) (A048675 (A285317 n)))

(define A285319 (MATCHING-POS 1 0 (lambda (n) (and (< (A019565 n) n) (not (zero? (A008683 n))) (not (zero? (A008683 (A048675 n))))))))

(define (A285320 n) (if (or (zero? n) (zero? (A008683 n))) 0 (+ 1 (A285320 (A048675 n)))))

;; (definec (Ajoku n) (if (zero? n) n (+ 1 (Ajoku (A048675 n)))))

;; (map (COMPOSE Ajoku A000040) (iota 20))
;; (2 3 4 5 5 6 6 7 6 6 7 7 6 8 7 7 6 8 7 9 7 8 9 8 7 7 9 7 8 8)


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

(define (A283475 n) (A019565 (A005187 n)))

(define (A283980 n) (* (A006519 n) (A003961 n)))

(define (A283477 n) (A108951 (A019565 n)))

(definec (A283477r1 n)
   (cond ((zero? n) 1)
         ((even? n) (A283980 (A283477r1 (/ n 2))))
         (else (* 2 (A283980 (A283477r1 (/ (- n 1) 2)))))
   )
)

  
;; A283972-A284013 are now reserved for your use. 

(define (A284001 n) (A005361 (A283477 n)))
(define (A284002 n) (A072411 (A283477 n)))
(define (A284003 n) (A007913 (A283477 n))) ;; A001222(A284003(n)) = A209281(n) ???
(define (A284004 n) (A046523 (A284003 n)))

(define (A284005 n) (A000005 (A283477 n)))

(define (A283478 n) (A097248 (A108951 n)))

(define (A280700 n) (A000120 (A005187 n)))
(define (A280700v2 n) (A001221 (A283475 n)))
(define (A280700v3 n) (A001222 (A283475 n)))

(define (A280705 n) (A002110 (A280700 n)))
(define (A280705v2 n) (A046523 (A283475 n)))

(define (A283483 n) (A090880 (A283477 n)))

(define (A283984 n) (A276075 (A283477 n)))
(define (A283985 n) (A276085 (A283477 n)))

;; XFER: Base-2.core.ss A043545 [Clark Kimberling] o=0: (Maximal base 2 digit of n) - (minimal base 2 digit of n). 
(define (A043545 n) (- 1 (A036987 n)))

;; a(n) = if n=0 then 1 else A043545(n+1)*a(n+1-A053644(n+1)). - Reinhard Zumkeller, Aug 19 2006
(definec (A079559r1 n) (if (zero? n) 1 (* (A043545 (+ 1 n)) (A079559r1 (- (+ 1 n) (A053644 (+ 1 n)))))))

(define (A079559loop n)
  (let loop ((n n) (z 1))
     (if (zero? n)
         z
         (loop (- (+ 1 n) (A053644 (+ 1 n))) (* (A043545 (+ 1 n)) z))
     )
  )
)


(define A283476 (MATCHING-POS 1 1 (lambda (n) (and (= 1 (A008966 n)) (= 1 (A079559loop (A048675 n)))))))

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

;; A247180 [Zumkeller] o=1: Numbers with non repeating smallest prime factor. 
(define A247180 (ZERO-POS 1 1 (COMPOSE -1+ A067029)))


(define (A032742 n) (/ n (A020639 n))) ;; a(1) = 1; for n > 1, a(n) = largest proper divisor of n.

;; A054576 Largest proper factor of largest proper factor of n.
(define (A054576 n) (A032742 (A032742 n)))

;; A014673 [Zumkeller] o=1: Smallest prime factor of greatest proper divisor of n. 
(define (A014673 n) (A020639 (A032742 n)))
(define (A014673v2 n) (/ (A032742 n) (A054576 n)))


(define (A060681 n) (- n (A032742 n))) ;; Our version starts from n=1, with a(1) = 0.

(definec (A064097 n) (if (= 1 n) 0 (+ 1 (A064097 (A060681 n)))))


(define (A006530 n) (if (< n 2) n (last (ifactor n)))) ;; Gpf(n): greatest prime dividing n (with a(1)=1). 

(define (A066048 n) ( * (A020639 n) (A006530 n))) ;; Product of smallest and greatest prime factors of n.

(define (A052126 n)  (/ n (A006530 n)))

(definec (A076271 n) (if (= 1 n) n (+ (A076271 (- n 1)) (A006530 (A076271 (- n 1))))))

(define (A028233 n) (expt (A020639 n) (A067029 n)))

(define (A028234 n) (/ n (A028233 n)))

;; A005361 [Jeffrey Shallit, Olivier Gérard] Product of exponents of prime factorization of n.
(definec (A005361 n) (if (= 1 n) 1 (* (A067029 n) (A005361 (A028234 n)))))

;; A072411 [Labos Elemer] LCM of exponents in prime factorization of n. [XXX - Prepend! a(1) = 1]
(definec (A072411 n) (if (= 1 n) 1 (lcm (A067029 n) (A072411 (A028234 n)))))


;; A051903 [Labos Elemer] Maximal exponent in prime factorization of n. 
(definec (A051903 n) (if (= 1 n) 0 (max (A067029 n) (A051903 (A028234 n)))))

;; A056169 [Labos Elemer] o=1: Number of unitary prime divisors of n.
(definec (A056169 n) (if (= 1 n) 0 (+ (if (= 1 (A067029 n)) 1 0) (A056169 (A028234 n)))))

;; A056170 [Labos Elemer] o=1: Number of non-unitary prime divisors of n.
(define (A056170 n) (- (A001221 n) (A056169 n)))

(definec (A275812 n) (if (= 1 n) 0 (+ (if (> (A067029 n) 1) (A067029 n) 0) (A275812 (A028234 n)))))

(define (A275812v2 n) (- (A001222 n) (A056169 n)))

;; A046660 [NJAS] o=1: Excess of n = number of prime divisors (with multiplicity) - number of prime divisors (without multiplicity). 
(define (A046660 n) (- (A001222 n) (A001221 n)))

;; A048672 [AK] o=1: Binary encoding of squarefree numbers (A005117): A048640(n)/2. 
(define (A048672 n) (A048675 (A005117 n)))

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

;; Yet another:
(definec (A048675rec n) (cond ((= 1 n) 0) ((even? n) (+ 1 (A048675rec (/ n 2)))) (else (* 2 (A048675rec (A064989 n))))))



(definec (Aludicpredecessor n) (let ((v (A272565 n))) (if (= v n) 0 (let loop ((k (- n 1))) (if (= v (A272565 k)) k (loop (- k 1)))))))

(definec (Aludicsuccessor n) (if (= 1 n) 0 (let ((v (A272565 n))) (let loop ((k (+ 1 n))) (if (= v (A272565 k)) k (loop (+ 1 k)))))))


(definec (Asomething_else5 n)
  (cond ((<= n 2) (- n 1))
        ((= 1 (A192490 n)) (* 2 (Asomething_else5 (+ -1 (A192512 n)))))
        (else (+ 1 (* 2 (Asomething_else5 (Aludicpredecessor n)))))
  )
)

(definec (Asomething_else6 n)
  (cond ((<= n 1) (+ 1 n))
        ((even? n) (A003309 (+ 1 (Asomething_else6 (/ n 2)))))
        (else (Aludicsuccessor (Asomething_else6 (/ (- n 1) 2))))
  )
)

;; (same-intfuns0? A001477 (COMPOSE Asomething_else5 Asomething_else6) 55) --> #t
;; (same-intfuns1? A001477 (COMPOSE Asomething_else6 Asomething_else5) 55) --> #t


;; A079427 [Zumkeller] o=1: Least m > n having the same number of divisors as n, a(1)=1. 

(definec (A079427 n) (if (= 1 n) 1 (let ((v (A000005 n))) (let loop ((k (+ 1 n))) (if (= v (A000005 k)) k (loop (+ 1 k)))))))

(definec (Aprecu n) (let ((v (A000005 n))) (let loop ((k (- n 1))) (cond ((zero? k) k) ((= v (A000005 k)) k) (else (loop (- k 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;

(definec (A168081 n) (if (<= n 1) n (A003987bi (* 2 (A168081 (- n 1))) (A168081 (- n 2))))) ;; o=0: Lucas sequence U_n(x,1) over the field GF(2). Cf. also A000129.



;; A087207 [Mitch Cervinka] o=1: A binary representation of the primes that divide a number. 
(definec (A087207 n)
   (cond ((= 1 n) 0)
         ((= 1 (A010051 n)) (A000079 (- (A000720 n) 1)))
         (else (A003986bi (A087207 (A020639 n)) (A087207 (A032742 n))))
   )
)


(definec (A248663 n) ;; o=1: a(A000040(n)) = 2^(n-1), and a(n*m) = a(n) XOR a(m). [From Peter Kagey.]
  (cond ((= 1 n) 0)
        ((= 1 (A010051 n)) (A000079 (- (A000720 n) 1)))
        (else (A003987bi (A248663 (A020639 n)) (A248663 (A032742 n))))
  )
)

(definec (A248663v2 n) ;; o=1: a(A000040(n)) = 2^(n-1), and a(n*m) = a(n) XOR a(m). [From Peter Kagey.]
  (cond ((= 1 n) 0)
        (else (A003987bi (A000079 (- (A055396 n) 1)) (A248663v2 (A032742 n))))
  )
)


;; A257538 [Deutsch] o=1: The Matula number of the rooted tree obtained from the rooted tree T having Matula number n by replacing each edge of T by a path of length 2.
(definec (A257538 n) 
  (cond ((= 1 n) 1)
        (else (* (A000040 (A000040 (A257538 (A055396 n)))) (A257538 (A032742 n))))
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


(define A007774 (MATCHING-POS 1 1 (lambda (n) (= 2 (A001221 n))))) ;; o=1: Numbers that are divisible by exactly 2 different primes.
(define A030231 (MATCHING-POS 1 1 (lambda (n) (even? (A001221 n))))) ;; o=1: Number of distinct primes dividing n is even.

;; (same-intfuns1? A007774 (COMPOSE A030231 1+) 1200) --> 117 because 210 = 2*3*5*7 only in A030231.



(definec (A065338 n)
  (cond ((<= n 1) n)
        ((prime? n) (modulo n 4))
        (else (apply * (map A065338 (ifactor n))))
  )
)

(define (A065339v2 n) (A007949 (A065338 n))) ;; Number of primes congruent to 3 modulo 4 dividing n (with multiplicity).


(definec (A065339 n) (cond ((< n 3) 0) ((even? n) (A065339 (/ n 2))) (else (+ (/ (- (modulo (A020639 n) 4) 1) 2) (A065339 (A032742 n))))))

(definec (A083025 n) (cond ((< n 3) 0) ((even? n) (A083025 (/ n 2))) (else (+ (/ (- 3 (modulo (A020639 n) 4)) 2) (A083025 (A032742 n))))))

(define (A079635 n) (- (A083025 n) (A065339 n)))

(define A072202 (ZERO-POS 1 1 A079635))

(define A268379v2 (MATCHING-POS 1 1 (COMPOSE positive? A079635)))

(define A268379 (MATCHING-POS 1 1 (lambda (n) (> (A083025 n) (A065339 n))))) ;; o=1: Numbers having more prime factors of form 4*k+1 than of 4*k+3, when counted with multiplicity.

(define A268380 (MATCHING-POS 1 1 (lambda (n) (< (A083025 n) (A065339 n))))) ;; o=1: Numbers having less prime factors of form 4*k+1 than of 4*k+3, when counted with multiplicity.


(define A268380v2 (MATCHING-POS 1 1 (COMPOSE negative? A079635)))

(define A268381 (MATCHING-POS 1 1 (COMPOSE not negative? A079635)))



(definec (A065339r2 n) ;; Add exponents of 4k+3 primes (of prime factorization of n) together.
    (cond ((< n 3) 0)
          ((even? n) (A065339r2 (/ n 2)))
          ((= 1 (modulo (A020639 n) 4)) (A065339r2 (A032742 n)))
          (else (+ (A067029 n) (A065339r2 (A028234 n))))
    )
)

(define A004613 (MATCHING-POS 1 1 (lambda (k) (= 1 (A065338 k))))) ;; o=1: Numbers that are divisible only by primes congruent to 1 mod 4.

(define A004431 (MATCHING-POS 1 1 (lambda (n) (and (even? (A260728 n)) (not (zero? (A083025 n)))))))
;; o=1: Numbers that are the sum of 2 distinct nonzero squares.  (Subseq of A001481)
;; Numbers whose prime factorization includes at least one prime congruent to 1 mod 4 and any prime factor congruent to 3 mod 4 has even multiplicity. - Franklin T. Adams-Watters, May 03 2006


(definec (A260728 n) ;; (Bitwise-) Or exponents of 4k+3 primes (of prime factorization of n) together.
    (cond ((< n 3) 0)
          ((even? n) (A260728 (/ n 2)))
          ((= 1 (modulo (A020639 n) 4)) (A260728 (A032742 n)))
          (else (A003986bi (A067029 n) (A260728 (A028234 n))))
    )
)

(definec (A267113 n) ;; (Bitwise-) Or exponents of 4k+1 primes (of prime factorization of n) together.
    (cond ((< n 5) 0)
          ((even? n) (A267113 (/ n 2)))
          ((= 3 (modulo (A020639 n) 4)) (A267113 (A032742 n)))
          (else (A003986bi (A067029 n) (A267113 (A028234 n))))
    )
)

(define A004144 (ZERO-POS 1 1 A267113))
(define A009003 (NONZERO-POS 1 1 A267113))

(definec (A267115 n) ;; (Bitwise-) And exponents of primes in prime factorization of n together.
    (if (= 1 (A028234 n))
        (A067029 n)
        (A004198bi (A067029 n) (A267115 (A028234 n)))
    )
)

(define (A267115loop n)
  (let loop ((n (A028234 n)) (z (A067029 n)))
    (cond ((= 1 n) z)
          (else (loop (A028234 n) (A004198bi z (A067029 n))))
    )
  )
)

(define A002035 (MATCHING-POS 1 1 (COMPOSE odd? A267115))) ;; o=1: Numbers that contain primes to odd powers only. 
(define A072587 (MATCHING-POS 1 2 (COMPOSE even? A267115))) ;; o=1: Numbers having at least one prime factor with an even exponent. [1 not included].

(definec (A267116 n) ;; (Bitwise-) Or exponents of primes in prime factorization of n together.
    (cond ((= 1 n) 0)
          (else (A003986bi (A067029 n) (A267116 (A028234 n))))
    )
)

(define (A267116v2 n) (A003986bi (A007814 n) (A003986bi (A260728 n) (A267113 n))))

;; A124859 [Zumkeller] o=1: Multiplicative with p^e -> primorial(e), p prime and e>0. 
(definec (A124859 n) (cond ((= 1 n) 1) (else (* (A002110 (A067029 n)) (A124859 (A028234 n))))))

;; A046523 [NJAS] o=1: Smallest number with same prime signature as n. 
(define (A046523 n) (A124859 (A124859 n)))

;; A278216-A278248 are now reserved for your use

(define (A278217 n) (A046523 (+ 1 (A075157 n)))) ;; o=0: 
(define (A278217v2 n) (A046523 (A075159 (+ 1 n))))

(define (A278219 n) (A046523 (A243353 n)))

;; (same-intfuns0? A278219 (COMPOSE A278222 A003188) 1024) --> #t
;; (same-intfuns0? A278219 (COMPOSE A278220 1+ A075157) 512) --> #t



;; (same-intfuns1? A003991 (lambda (n) (* (A059895 n) (A059896 n))) 10440) --> #t

;; (same-intfuns1? A003991 (lambda (n) (* (A000290 (A059895 n)) (A059897 n))) 10440) --> #t


;; A059895 [MLB] o=1: Table a(i,j) = product prime[k]^(Ei[k] AND Ej[k]) where Ei and Ej are the vectors of exponents in the prime factorizations of i and j; AND is the bitwise operation on binary representation of the exponents. 

(define (A059895 n) (A059895bi (A002260 n) (A004736 n)))

;; Return a number in whose prime-factorization the exponents of primes in prime factorization of a and b have been bitwise-ored together.
(define (A059895bi a b)
  (let loop ((a a) (b b) (m 1))
    (cond ((= 1 a) m)
          ((= 1 b) m)
          ((equal? (A020639 a) (A020639 b))
;; The smallest prime factor of a is also the smallest prime factor of b? In that case:
;; divide it completely out of both, and multiply m with that spf rised to bitwise-and of those exponents:
              (loop (A028234 a) (A028234 b) (* m (expt (A020639 a) (A004198bi (A067029 a) (A067029 b)))))
          )
          ((< (A020639 a) (A020639 b))
;; Otherwise, eliminate the smallest prime factor of a (with multiplicity):
              (loop (A028234 a) b m)
          )
          (else
;; Otherwise, eliminate the smallest prime factor of b (with multiplicity):
              (loop a (A028234 b) m)
          )
    )
  )
)

;; A059896 [MLB] o=1: Table a(i,j) = product prime[k]^(Ei[k] OR Ej[k]) where Ei and Ej are the vectors of exponents in the prime factorizations of i and j; OR is the bitwise operation on binary representation of the exponents.

(define (A059896 n) (A059896bi (A002260 n) (A004736 n)))

;; Return a number in whose prime-factorization the exponents of primes in prime factorization of a and b have been bitwise-ored together.
(define (A059896bi a b)
  (let loop ((a a) (b b) (m 1))
    (cond ((= 1 a) (* m b))
          ((= 1 b) (* m a))
          ((equal? (A020639 a) (A020639 b))
;; The smallest prime factor of a is also the smallest prime factor of b? In that case:
;; divide it completely out of both, and multiply m with that spf rised to bitwise-or of those exponents:
              (loop (A028234 a) (A028234 b) (* m (expt (A020639 a) (A003986bi (A067029 a) (A067029 b)))))
          )
          ((< (A020639 a) (A020639 b))
;; Otherwise, "transfer" the smallest prime factor of a (with multiplicity) to the accumulated m:
              (loop (/ a (A028233 a)) b (* m (A028233 a)))
          )
          (else
;; Otherwise, "transfer" the smallest prime factor of b (with multiplicity) to the accumulated m:
              (loop a (/ b (A028233 b)) (* m (A028233 b)))
          )
    )
  )
)

;; A059897 [MLB] o=1: Square array read by antidiagonals: T(i,j) = product prime[k]^(Ei[k] XOR Ej[k]) where Ei and Ej are the vectors of exponents in the prime factorizations of i and j; XOR is the bitwise operation on binary representation of the exponents. 

(define (A059897 n) (A059897bi (A002260 n) (A004736 n)))

;; Return a number in whose prime-factorization the exponents of primes in prime factorization of a and b have been bitwise-ored together.
(define (A059897bi a b)
  (let loop ((a a) (b b) (m 1))
    (cond ((= 1 a) (* m b))
          ((= 1 b) (* m a))
          ((equal? (A020639 a) (A020639 b))
;; The smallest prime factor of a is also the smallest prime factor of b? In that case:
;; divide it completely out of both, and multiply m with that spf rised to bitwise-xor of those exponents:
              (loop (A028234 a) (A028234 b) (* m (expt (A020639 a) (A003987bi (A067029 a) (A067029 b)))))
          )
          ((< (A020639 a) (A020639 b))
;; Otherwise, "transfer" the smallest prime factor of a (with multiplicity) to the accumulated m:
              (loop (/ a (A028233 a)) b (* m (A028233 a)))
          )
          (else
;; Otherwise, "transfer" the smallest prime factor of b (with multiplicity) to the accumulated m:
              (loop a (/ b (A028233 b)) (* m (A028233 b)))
          )
    )
  )
)



(define (A284576 n) (A059896bi (A260443 n) (A260443 (+ 1 n))))
(define (A284577 n) (A059897bi (A260443 n) (A260443 (+ 1 n))))
(define (A284578 n) (A059895bi (A260443 n) (A260443 (+ 1 n))))

(define (A284573 n) (A046523 (A277324 n)))

(define (A284573v2 n) (A278243 (+ 1 n n)))


;; A285097-A285118 are now reserved for your use. 


(define (A285106 n) (apply + (bitwise_or_of_exp_lists (A260443as_coeff_list n) (A260443as_coeff_list (+ 1 n)))))
(define (A285107 n) (apply + (bitwise_xor_of_exp_lists (A260443as_coeff_list n) (A260443as_coeff_list (+ 1 n)))))
(define (A285108 n) (apply + (bitwise_and_of_exp_lists (A260443as_coeff_list n) (A260443as_coeff_list (+ 1 n)))))

(definec (A285106v2 n) (A001222 (A284576 n)))
(definec (A285107v2 n) (A001222 (A284577 n)))
(definec (A285108v2 n) (A001222 (A284578 n)))

;; (same-intfuns0? A285106 (lambda (n) (+ (A285107 n) (A285108 n))) 8192) --> #t

;; (same-intfuns0? (COMPOSE A007306 1+) (lambda (n) (+ (A285106 n) (A285108 n))) 27) --> #t
;; (same-intfuns0? (COMPOSE A007306 1+) (lambda (n) (+ (A285107 n) (* 2 (A285108 n)))) 27) --> #t

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Least number with prime signature - applied to various prime index bijections:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A278220 n) (A046523 (A241909 n)))

(definec (A278221 n) (A046523 (A122111 n)))


(define (A046523v2_apparently_check n) (A046523 (A242420 n))) ;; At least up to n=8192.

(define (A046523v3_apparently_check n) (A046523 (A242415 n))) ;; At least up to n=8192.

(define (A046523v4_apparently_check n) (A046523 (A242419 n)))

(define (A046523v5_apparently_check n) (A046523 (A069799 n)))

(define (A278221v2_apparently_check n) (A046523 (A153212 n))) ;; At least up to n=1024.


(define (A278523 n) (A046523 (A249818 n))) ;; A278523 [AK] o=1: a(n) = A046523(A249818(n)).
(define (A278524 n) (A046523 (A250246 n))) ;; A278523 [AK] o=1: a(n) = A046523(A250246(n)).

;; A278526 [AK] o=1: Filtering sequence (related to prime factorization): a(n) = A046523(A241916(n)).
(define (A278525 n) (A046523 (A241916 n)))

(define (A278526 n) (A046523 (A227413 n)))
(define (A278527 n) (A046523 (A246378 n)))

;; (same-intfuns1? A278526 (COMPOSE A278527 A054429) 511) --> #t
;; (same-intfuns1? A278527 (COMPOSE A278526 A054429) 511) --> #t



(define (A278531 n) (A046523 (A163511 n))) ;; [AK] o=0: a(n) = A046523(A163511(n)).
;; (same-intfuns0? A278531 (COMPOSE A278222 A054429) 2047) --> #t

(define (A278533 n) (A046523 (A253563 n)))
(define (A278535 n) (A046523 (A253565 n)))

;; (same-intfuns0? A278533 (COMPOSE A278535 A054429) 1023) --> #t
;; (same-intfuns0? A278535 (COMPOSE A278533 A054429) 1023) --> #t


(define (A278532 n) (A278219 (A255056 n))) ;; Cf. A255336, A278232, A278262, A278534. 
(define (A278534 n) (A278236 (A219666 n))) ;; Cf. A230406, A278232, A278262, A278534. 

(define (A278540 n) (A046523 (A235201 n)))

(define (A278541 n) (A046523 (A209636 n)))

(define (A278542 n) (A046523 (A209637 n)))

;; (same-intfuns0? A278541 (COMPOSE A278542 A054429) 511) --> #t
;; (same-intfuns0? A278542 (COMPOSE A278541 A054429) 511) --> #t

(define (A278543 n) (A046523 (A277198 n))) ;; Cf. A278243.

;;;;;;;;;;;;;;;;;;;;;;

(define (A278222 n) (A046523 (A005940 (+ 1 n))))

;; (same-intfuns0? A278222 (COMPOSE A124859 A278159) 1200) --> #t


(define (A278223 n) (A046523 (+ n n -1)))

(define (A278223v2 n) (A046523 (A064216 n)))

(define (A278224 n) (A046523 (A048673 n)))

(define (A278225 n) (A046523 (A275725 n)))

(define (A278226 n) (A046523 (A276086 n)))

(define (A278231 n) (A046523 (A193231 n)))

(define (A278233 n) (A046523 (A091203 n)))

;; Two derived seqs, apply to squares in GF(2)[X]:
(define (A278238 n) (A278233 (A000695 n)))

;; And to "almost squares":
(define (A278239 n) (A278233 (A277699 n)))

(define (A278234 n) (A046523 (A275734 n)))

(define (A278235 n) (A046523 (A275735 n)))

(define (A278236 n) (A046523 (A276076 n)))

(define (A278241 n) (A046523 (A000041 n)))

(define (A278245 n) (A046523 (A000045 n)))

;; A001608 [NJAS] o=0: Perrin sequence (or Ondrej Such sequence): a(n) = a(n-2) + a(n-3) with a(0) = 3, a(1) = 0, a(2) = 2.
(definec (A001608 n) (cond ((zero? n) 3) ((= 1 n) 0) ((= 2 n) 2) (else (+ (A001608 (- n 2)) (A001608 (- n 3))))))

(define (A278248 n) (if (= 1 n) 0 (A046523 (A001608 n))))

;; A069877 [Murthy] o=0: Smallest number with a prime signature whose indices are the decimal digits of n. 
(define (A069877 n) (A046523 (A054842 n)))

;; A276528 is now reserved for your use. 
;; A278249-A278263 are now reserved for your use. 
;; A278264-A278266 are now reserved for your use. 

(define (A278261 n) (A046523 (A273671 n)))

(define (A278263 n) (A046523 (A064413 n)))
(define (A278264 n) (A046523 (A064664 n)))

(define (A278266 n) (A046523 (A026477 n)))


(define (A278237 n) (A046523 (A263273 n)))

(define (A278243 n) (A046523 (A260443 n)))


;; Interlude, Vandermast:
(definec (A181819 n) (cond ((= 1 n) 1) (else (* (A000040 (A067029 n)) (A181819 (A028234 n)))))) ;; a(1) = 1; for n>1, if n = Product prime(i)^e(i), then a(n) = Product prime(e(i)).

(definec (A181819v1 n) (if (= 1 n) 1 (* (A008578 (A001511 n)) (A181819v1 (A064989 n)))))

(definec (A181819v2 n) (cond ((= 1 n) 1) ((even? n) (* (A000040 (A007814 n)) (A181819v2 (A000265 n)))) (else (A181819v2 (A064989 n)))))

(define (A238745 n) (A122111 (A181819 n))) ;; o=1: a(1) = 1; for n > 1, if the first integer with the same prime signature as n is factorized into primorials as Product A002110(i)^e(i), then a(n) = Product prime(i)^e(i). 


(define A238748 (MATCHING-POS 1 1 (lambda (n) (square? (A181819 n))))) ;; o=1: Numbers n such that each integer that appears in the prime signature of n appears an even number of times.

(definec (A182850 n) (if (<= n 2) 0 (+ 1 (A182850 (A181819 n))))) ;; o=1: a(n) = number of iterations that n requires to reach a fixed point under the x -> A181819(x) map. 

(define A182853 (MATCHING-POS 1 1 (lambda (n) (= 3 (A182850 n))))) ;; o=1: Squarefree composite integers and powers of squarefree composite integers.

(definec (A268385 n) (cond ((= 1 n) 1) (else (* (expt (A020639 n) (A193231 (A067029 n))) (A268385 (A028234 n))))))

(definec (A268385v1 n) (if (= 1 n) 1 (* (A000079 (A193231 (A007814 n))) (A003961 (A268385v1 (A064989 n))))))

(definec (A268674 n) (cond ((= 1 n) n) ((even? n) (A268674 (A000265 n))) (else (A083221bi (- (A055396 n) 1) (A078898 n)))))

;; (same-intfuns1? A000027  (COMPOSE A268674 A250469) 1200) --> #t
;; (same-intfuns1? A268674 (COMPOSE A268674 double) 1200) --> #t

(definec (A268675 n) (if (= 1 n) 1 (* (A000079 (A193231 (A007814 n))) (A250469 (A268675 (A268674 n))))))
(definec (A268675v2 n) (if (= 1 n) 1 (* (A000079 (A193231 (A007814 n))) (A250469 (A268675v2 (A250470 (A000265 n)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The same theme, slightly twisted:
;;
;;
;; This should not be same as A250246, although the first difference do not occur until at 2^21
;, where a(2097152) = a(2^21) = 2^27 = 134217728.
(definec (Aouto n) (if (<= n 1) n (* (A000079 (Aouto (A007814 n))) (A003961 (Aouto (A268674 n))))))

;; (same-intfuns1? A249818 Aouto 120) --> 42
;; 
;; (same-intfuns1? A249817 Aouto 120) --> 33
;; 
;; (same-intfuns1? A250245 Aouto 120) --> 33
;; 
;; (same-intfuns1? A250246 Aouto 4096) --> #t
;; 
;; (same-intfuns1? A250247 Aouto 120) --> 33
;; 
;; (same-intfuns1? A250248 Aouto 120) --> 42
;; 
;; (same-intfuns1? A250249 Aouto 120) --> 33
;; 
;; (same-intfuns1? A250250 Aouto 120) --> 73

;;;;;;;;;;;;;;;;;;;;;;;;;


(define A268391 (MATCHING-POS 1 1 (lambda (n) (and (= 1 (A001221 n)) (= 1 (A268384 (A067029 n)))))))

(define (A268392 n) (A268385 (A050376 n)))


(definec (A268387 n) ;; Bitwise-XOR exponents of primes in prime factorization of n together.
    (cond ((= 1 n) 0)
          (else (A003987bi (A067029 n) (A268387 (A028234 n))))
    )
)

(define A268390 (ZERO-POS 1 1 A268387)) ;; o=1: Positions of zeros in A268387.

(define (A268386 n) (A193231 (A268387 n)))

(definec (A064547 n) ;; o=1: Sum of binary digits (or count of 1-bits) in the exponents of the prime factorization of n.
    (cond ((= 1 n) 0)
          (else (+ (A000120 (A067029 n)) (A064547 (A028234 n))))
    )
)

(definec (A064547v1 n) (if (= 1 n) 0 (+ (A000120 (A007814 n)) (A064547v1 (A064989 n)))))


(define A000028 (MATCHING-POS 1 1 (COMPOSE odd? A064547)))

(define A000379 (MATCHING-POS 1 1 (COMPOSE even? A064547)))
(define A000379v2 (MATCHING-POS 1 1 (lambda (n) (even? (A000120 (A268387 n))))))
(define A000379v3 (ZERO-POS 1 1 (COMPOSE A010060 A268387)))

(define A050376 (MATCHING-POS 1 1 (lambda (n) (= 1 (A064547 n)))))
(define A268388 (MATCHING-POS 1 1 (lambda (n) (> (A064547 n) 1))))


;; (filter odd? (map (COMPOSE A064547 A268390) (iota 32769))) --> () ;; I.e. A268390 is a subsequence of A000379.


(define A267114 (ZERO-POS 1 1 (lambda (n) (- (A001222 n) (+ (A267115 n) (A267116 n))))))

(define A267117 (ZERO-POS 1 1 A267115)) ;; o=1: Indices of zeros in A267115. Subseq of A267114.

;; A268374-A268395 are now reserved for your use. 

(define (A268374 n) (- (A001222 n) (A267116 n)))

(define A268375 (ZERO-POS 1 1 A268374))

(define A268376 (NONZERO-POS 1 1 A268374))

;; Numbers n such that any prime factor congruent to 1 mod 4 has even multiplicity.
(define A268377 (MATCHING-POS 1 1 (COMPOSE even? A267113)))

;; Numbers whose prime factorization includes at least one prime congruent to 3 mod 4 and any prime factor congruent to 1 mod 4 has even multiplicity.
(define A268378 (MATCHING-POS 1 1 (lambda (n) (and (even? (A267113 n)) (not (zero? (A065339 n)))))))


;; A170818 a(n) = product of primes (with multiplicity) of form 4k+1 that divide n.
(definec (A170818 n) ;; Part of n composed of prime factors of form 4k+1.
    (cond ((= 1 n) 1)
          ((even? n) (A170818 (/ n 2)))
          ((= 3 (modulo (A020639 n) 4)) (A170818 (A032742 n)))
          (else (* (A028233 n) (A170818 (A028234 n))))
    )
)


(definec (A097706 n) ;; Part of n composed of prime factors of form 4k+3.
    (cond ((< n 3) 1)
          ((even? n) (A097706 (/ n 2)))
          ((= 1 (modulo (A020639 n) 4)) (A097706 (A032742 n)))
          (else (* (A028233 n) (A097706 (A028234 n))))
    )
)



(define A260730 (MATCHING-POS 1 1 (lambda (n) (> (A065339 n) (A260728 n)))))
;; (define A260730 (NONZERO-POS 1 1 (lambda (n) (- (A065339 n) (A260728 n)))))


(define (A229062 n) (- 1 (A000035 (A260728 n)))) ;; 1 if n is representable as sum of two nonnegative squares. 
;; Characteristic function of A001481.

;;;;;;;;;;;;

(definec (A270418perA270419 n)
    (cond ((= 1 n) 1)
          (else (* (expt (A020639 n) (A065620 (A067029 n))) (A270418perA270419 (A028234 n))))
    )
)

(definec (A270418 n)
    (cond ((= 1 n) 1)
          (else (* (expt (A020639 n) (* (A010060 (A067029 n)) (A065620 (A067029 n)))) (A270418 (A028234 n))))
    )
)

(definec (A270418v1 n)
    (cond ((= 1 n) 1)
          ((zero? (A010060 (A067029 n))) (A270418v1 (A028234 n)))
          (else (* (expt (A020639 n) (A065620 (A067029 n))) (A270418v1 (A028234 n))))
    )
)

(define (A270418v2 n) (numerator (A270418perA270419 n)))

(definec (A270419 n)
    (cond ((= 1 n) 1)
          (else (* (expt (A020639 n) (* (A010059 (A067029 n)) (- (A065620 (A067029 n))))) (A270419 (A028234 n))))
    )
)

(definec (A270419v1 n)
    (cond ((= 1 n) 1)
          ((zero? (A010059 (A067029 n))) (A270419v1 (A028234 n)))
          (else (* (expt (A020639 n) (- (A065620 (A067029 n)))) (A270419v1 (A028234 n))))
    )
)


(define (A270419v2 n) (denominator (A270418perA270419 n)))

(define A270420 (MATCHING-POS 1 1 (lambda (n) (> (A270418 n) (A270419 n)))))
(define A270421 (MATCHING-POS 1 1 (lambda (n) (< (A270418 n) (A270419 n)))))

(define sub1 -1+)

(define A270428 (ZERO-POS 1 1 (COMPOSE sub1 A270419)))

(definec (chA270428 n)
    (cond ((= 1 n) 1)
          (else (* (A010060 (A067029 n)) (chA270428 (A028234 n))))
    )
)

(define A270428v2 (NONZERO-POS 1 1 chA270428))


(define A262675 (ZERO-POS 1 1 (COMPOSE sub1 A270418)))

(definec (chA262675 n)
    (cond ((= 1 n) 1)
          (else (* (A010059 (A067029 n)) (chA262675 (A028234 n))))
    )
)

(define A262675v2 (NONZERO-POS 1 1 chA262675))


;; A138302 o=1: Exponentially 2^n-numbers: 1 together with positive integers n such that all exponents in prime factorization of n are powers of 2. 

(definec (chA138302 n) (if (= 1 n) 1 (* (A209229 (A067029 n)) (chA138302 (A028234 n)))))

(define A138302 (NONZERO-POS 1 1 chA138302))

;;;;;;;;;;;;
;; Interlude: Stern-Brocot tree. After Zumkeller's formulas, with some ad hoc corrections:

(define (A007305 n) (if (<= n 1) n (A007305shift1 (- n 1)))) ;; o=0: Numerators of Farey (or Stern-Brocot) tree fractions.

(definec (A007305shift1 n)
  (cond ((<= n 1) n)
        ((and (not (zero? (A025480 (- n 1)))) (not (zero? (A025480 n))))
           (+ (A007305shift1 (A025480 (- n 1))) (A007305shift1 (A025480 n)))
        )
        ((zero? (A025480 n)) (+ 1 (A007305shift1 (A025480 (- n 1)))))
        (else (+ 1 (A007305shift1 (A025480 (- n 1)))))
  )
)

(define (A047679 n) (A007305 (+ 2 (A054429 (+ 2 n))))) ;; o=0: Denominators in full Stern-Brocot tree.

;; (map (lambda (n) (/ (A007305 (+ 1 n)) (A047679 (- n 1)))) (iota 31))
;; --> (1 1/2 2 1/3 2/3 3/2 3 1/4 2/5 3/5 3/4 4/3 5/3 5/2 4 1/5 2/7 3/8 3/7 4/7 5/8 5/7 4/5 5/4 7/5 8/5 7/4 7/3 8/3 7/2 5)

(define (SBtree_index m n)
  (let loop ((m m) (n n) (z 1))
     (cond ((= m n) z)
           ((< m n) (loop m (- n m) (+ z z)))
           (else (loop (- m n) n (+ z z 1)))
     )
  )
)

;; (same-intfuns1? A000027 (lambda (n) (SBtree_index (A007305 (+ 1 n)) (A047679 (- n 1)))) 65537) --> #t

;; A065621 (plus-exponents, numerator)
;; A048724 (minus-exponents, denominator)

(definec (A270436 n) ;; Anumprimes_with_A065621
    (cond ((= 1 n) 1)
          (else (* (expt (A020639 n) (A065621 (A067029 n))) (A270436 (A028234 n))))
    )
)

(definec (A270437 n) ;; Adenomprimes_with_A048724
    (cond ((= 1 n) 1)
          (else (* (expt (A020639 n) (A048724 (A067029 n))) (A270437 (A028234 n))))
    )
)


;; A273662-A273673 are now reserved for your use. 
(define (A273671 n) (* (A270436 (A007305 (+ 1 n))) (A270437 (A047679 (- n 1)))))
;; And its inverse follows. Offsets? Both o=1.

(define (A273672 n) (SBtree_index (A270418 n) (A270419 n)))

;; (same-intfuns1? A000027 (COMPOSE A273671 A273672) 256) --> #t
;; (same-intfuns1? A000027 (COMPOSE A273672 A273671) 10000) --> #t

;;;;;;;;;;;;


(define (isA001481? n) (even? (A260728 n)))

(define A001481 (MATCHING-POS 1 0 isA001481?)) ;; Numbers that are the sum of 2 squares. 


(define (isA022544? n) (odd? (A260728 n)))

(define A022544 (MATCHING-POS 1 1 isA022544?)) ;; Numbers that are not the sum of 2 squares. 

(define (A072401 n) (cond ((zero? n) 0) ((and (even? (A007814 n)) (= 7 (modulo (A000265 n) 8))) 1) (else 0))) ;; 1 iff n is of the form 4^m*(8k+7). 


(define A004215 (ZERO-POS 1 1 (COMPOSE -1+ A072401)))

;; Least number of squares that add up to n. This formula after _Charles R Greathouse IV_'s Jul 19 2011 PARI-code:
(define (A002828 n) (cond ((zero? n) n) ((= 1 (A010052 n)) 1) ((= 1 (A229062 n)) 2) (else (+ 3 (A072401 n)))))

;; A062535  [Henry Bottomley] o=0: Don't be greedy! Difference between number of squares needed to sum to n using the greedy algorithm and using the best such sum.
(define (A062535 n) (- (A053610 n) (A002828 n)))


(define (A255131 n) (- n (A002828 n)))
(define (A255131v2 n) (- n (A002828v2 n))) ;; For cross-checking.

(definec (A278216 n) (let loop ((s 0) (k (+ 4 n))) (if (< k n) s (loop (+ s (if (= n (A255131 k)) 1 0)) (- k 1)))))

(define A278489 (NONZERO-POS 0 0 A278216))

(define A278490 (ZERO-POS 1 1 A278216))

(define A278494 (MATCHING-POS 1 1 (lambda (n) (and (not (zero? (A010051 n))) (zero? (A278216 n))))))


(definec (A278495 n)
   (let loop ((k (+ -1 (A000290 (+ 1 n)))) (s 0))
        (if (= 1 (A010052 k))
            s
            (loop (- k 1) (+ s (* (A010051 k) (if (zero? (A278216 k)) 1 0))))
        )
   )
)


(define (A278496 n) (A000196 (A278494 n)))

;; o=1: New experimental A004001-style recurrence, involving also A002828:
(definec (A284000 n) (if (<= n 3) 1 (+ (A284000 (A284000 (- n (A002828 n)))) (A284000 (- n (A284000 (- n (A002828 n))))))))


(definec (A284000r2 n) (if (<= n 3) 1 (+ (A284000r2 (A284000r2 (A255131 n))) (A284000r2 (- n (A284000r2 (A255131 n)))))))

(definec (A284006 n) (if (<= n 2) 1 (+ (A284006 (A284006 (- n (A001511 n)))) (A284006 (- n (A284006 (- n (A001511 n))))))))

(definec (A284006r2 n) (if (<= n 2) 1 (+ (A284006r2 (A284006r2 (A093049 n))) (A284006r2 (- n (A284006r2 (A093049 n)))))))

(definec (A284007 n) (if (<= n 2) 1 (+ (A284007 (A284007 (- n (A002487 n)))) (A284007 (- n (A284007 (- n (A002487 n))))))))

(definec (A284007r2 n) (if (<= n 2) 1 (+ (A284007r2 (A284007r2 (A284013 n))) (A284007r2 (- n (A284007r2 (A284013 n)))))))


(definec (A283474 n) (if (<= n 1) n (+ (A283474 (- n 1)) (A283474 (- n (A002487 n))))))

(define (A283479 n) (- (A283474 n) (A283474 (- n 1))))



;; A014085 [Jon Wild] o=0: Number of primes between n^2 and (n+1)^2. 
(definec (A014085 n)
   (let loop ((k (+ -1 (A000290 (+ 1 n)))) (s 0))
        (if (= 1 (A010052 k))
            s
            (loop (- k 1) (+ s (A010051 k)))
        )
   )
)


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

;; No any consistent bias between these, thus not interesting?

(definec (Acount_odd_numbers_encountered_with_A260734 n)
 (let ((end (- (A000290 n) 1)))
  (let loop ((k (- (A000290 (+ 1 n)) 1)) (s 0))
        (if (= k end) s (loop (A255131 k) (+ s (A000035 k))))
  )
 )
)

(definec (Acount_of_even_numbers_encountered_with_A260734 n)
 (let ((end (- (A000290 n) 1)))
  (let loop ((k (- (A000290 (+ 1 n)) 1)) (s 0))
        (if (= k end) s (loop (A255131 k) (+ s (- 1 (A000035 k)))))
  )
 )
)


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

(define A269860 (ZERO-POS 1 1 (lambda (n) (- (A000035 n) (A000035 (A048673 n))))))

(define A269861 (NONZERO-POS 1 1 (lambda (n) (- (A000035 n) (A000035 (A048673 n))))))

(definec (A269862 n) (if (<= n 1) 0 (+ (modulo (- (A048673 n) n) 2) (A269862 (- n 1)))))

(define A270430 (ZERO-POS 1 1 (lambda (n) (- (A000035 (A048673 n)) (A000035 (A064216 n))))))

(define A270431 (NONZERO-POS 1 1 (lambda (n) (- (A000035 (A048673 n)) (A000035 (A064216 n))))))

(definec (A270432 n) (if (<= n 1) n (+ (- 1 (modulo (- (A048673 n) (A064216 n)) 2)) (A270432 (- n 1)))))
(definec (A270433 n) (if (<= n 1) 0 (+ (modulo (- (A048673 n) (A064216 n)) 2) (A270433 (- n 1)))))

(define (A270434 n) (- (A270432 n) (A270433 n)))

(define A270435 (ZERO-POS 1 1 A270434))

(definec (A269853 n)
  (cond ((= 1 n) n)
        ((even? n) (* 2 (A269853 (/ n 2))))
        (else (+ 1 (* 2 (A269853 (A064216 (/ (- n 1) 2))))))
  )
)

(definec (A269854 n)
  (cond ((= 1 n) n)
        ((even? n) (* 2 (A269854 (/ n 2))))
        (else (+ 1 (A243501 (A269854 (/ (- n 1) 2)))))
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

;; A285699-A285736 are now reserved for your use.

(define (A285725 n) (if (= 1 n) 0 (- (A252735 n) (A000035 n))))

(define (A285726 n) (if (<= n 2) 0 (- (A252736 n) (- 1 (A000035 n)))))

(define (A285727 n) (if (<= n 2) 0 (A252463 (A252463 n))))
(define (A285727v2 n) (cond ((<= n 2) 0) ((even? n) (A252463 (/ n 2))) (else (A252463 (A064989 n)))))

(define (A285728 n) (cond ((<= n 1) 0) ((even? n) (A252463 (A000265 n))) (else (A285728 (A064989 n)))))

(define (A285712 n) (cond ((<= n 1) (- n 1)) ((= 2 (modulo n 3)) (A002264 (+ 1 n))) (else (/ (+ 1 (A064216 n)) 2))))
;; Parents for A245612

(define (A285713 n) (A046523 (A245612 n)))
(define (A285713v2 n) (A278224 (A163511 n)))

(definec (A285714 n) (if (<= n 1) 0 (+ 1 (A285714 (A285712 n)))))
(define (A285714v2 n) (A029837 (+ 1 (A245611 n))))

;; (same-intfuns1? A285714 A285714v2 1023) --> #

(definec (A285715 n) (if (<= n 2) (- n 1) (+ (if (= 2 (modulo n 3)) 0 1) (A285715 (A285712 n)))))
(define (A285715v2 n) (A000120 (A245611 n)))


;; (same-intfuns1? A285715 A285715v2 4096) --> #t

(definec (A285716 n) (if (<= n 2) 0 (+ (if (= 2 (modulo n 3)) 1 0) (A285716 (A285712 n)))))

(define (A285716v2 n) (A080791 (A245611 n)))

;; (same-intfuns1? A285714v2 (lambda (n) (+ (A285715 n) (A285716 n))) 1024) --> #t


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
(definec (A252738rec2 n) (if (<= n 1) (+ 1 n) (* (A267096 (- n 2)) (A000290 (A252738rec2 (- n 1))))))

(define (A252739 n) (/ (A252738 n) n))

(define (A007188 n) (mul (lambda (k) (expt (A000040 (+ 1 k)) (A007318tr n k))) 0 n)) ;; o=0: Multiplicative encoding of Pascal triangle: Product p(i+1)^C(n,i).

(definec (A007188r n) (if (zero? n) 2 (* (A007188r (- n 1)) (A003961 (A007188r (- n 1)))))) ;; As a recurrence.


(define (A267096 n) (mul (lambda (k) (expt (A000040 (+ 2 k)) (A007318tr n k))) 0 n)) ;; o=0: a(n) = Product_{i=0..n} prime(i+2)^binomial(n,i).

(definec (A191555 n) (if (= 1 n) 2 (* (A000040 n) (A000290 (A191555 (- n 1))))))
(definec (A191555slow n) (if (= 1 n) 2 (* (A000079 (A000079 (- n 1))) (A003961 (A191555slow (- n 1)))))) ;; o=1: prod(k=1..n, prime(k)^(2^(n-k))).

(define (A266639 n) (/ (A252738 n) (A191555 n)))


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

;; (same-intfuns0? A000045  (lambda (n) (reduce + 0 (A206296as_index_lists n))) 120)

(define (A276080 n) (sum_factorials_times_elements_in (A206296as_index_lists n)))


(definec (A206296as_index_lists n)
  (cond ((zero? n) (list))
        ((= 1 n) (list 1))
        (else (map +
                   (cons 0 (A206296as_index_lists (- n 1)))
                   (append  (A206296as_index_lists (- n 2)) (list 0 0))
              )
        )
  )
)

(define (sum_factorials_times_elements_in nums)
   (let loop ((s 0) (nums nums) (i 2) (f 1))
      (cond ((null? nums) s)
            (else (loop (+ s (* (car nums) f)) (cdr nums) (+ 1 i) (* i f)))
      )
   )
)


(define (sum_A000027_times_elements_in nums)
   (let loop ((s 0) (nums nums) (i 1))
      (cond ((null? nums) s)
            (else (loop (+ s (* (car nums) i)) (cdr nums) (+ 1 i)))
      )
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

(definec (A260443rec n)
  (cond ((<= n 1) (+ 1 n))
        ((even? n) (A003961 (A260443rec (/ n 2))))
        (else (* (A260443rec (/ (- n 1) 2)) (A260443rec (/ (+ n 1) 2))))
  )
)


(define (A260443 n) (product_primes_to_kth_powers (A260443as_coeff_list n)))

(define A260442 (FIXED-POINTS 0 1 (COMPOSE A260443 A048675)))

(define A277702 (RECORD-POS 1 0 A260443))
(define (A277703 n) (A260443 (A277702 n)))

;; After a(1)=0, a(2n)=a(n)+1, a(4n-1)=a(n)+1, a(4n+1)=a(n)+1 for n>=1 (Klavzar et al. Proposition 12). - Emeric Deutsch, Dec 04 2006
(definec (A057526 n)
  (cond ((= 1 n) 0)
        ((even? n) (+ 1 (A057526 (/ n 2))))
        ((= 3 (modulo n 4)) (+ 1 (A057526 (/ (+ 1 n) 4))))
        (else (+ 1 (A057526 (/ (+ -1 n) 4))))
  )
)

(define (A277329 n) (if (zero? n) n (+ 1 (A057526 n))))
(define (A277329v2 n) (A061395 (A260443 n)))

(definec (A277329rec n)
  (cond ((<= n 1) n)
        ((even? n) (+ 1 (A277329rec (/ n 2))))
        ((= 3 (modulo n 4)) (+ 1 (A277329rec (/ (+ 1 n) 4))))
        (else (+ 1 (A277329rec (/ (+ -1 n) 4))))
  )
)

(definec (A277330 n)
  (cond ((<= n 1) (+ 1 n))
        ((even? n) (A003961 (A277330 (/ n 2))))
        (else (/ (lcm (A277330 (/ (- n 1) 2)) (A277330 (/ (+ n 1) 2)))
                 (gcd (A277330 (/ (- n 1) 2)) (A277330 (/ (+ n 1) 2)))
              )
        )
  )
)

(definec (A277330v2 n) (A007913 (A260443 n)))

;; (same-intfuns0? A264977 (COMPOSE A048675 A277330) 255) --> #t

;; (same-intfuns0? A277330 (COMPOSE A019565 A264977) 255) --> #t

(define (A277700 n) (A000120 (A264977 n))) ;; XXX - Need also bisections of A264977 and A277700

(define (A283975 n) (A264977 (+ n n 1))) ;; o=0:
(define (A283975v2 n) (A248663 (A277324 n))) ;; o=0: 

(define (A283979 n) (/ (A003987bi n (A264977 n)) 4)) ;; Cf. A265397, A284013.

(define (A283983 n) (A000188 (A260443 n)))
(define (A283983v2 n) (A000196 (A283989 n)))

(define (A283989 n) (A008833 (A260443 n)))
(define (A283989v2 n) (/ (A260443 n) (A277330 n)))

(define (A284264 n) (/ (- (A002487 n) (A277700 n)) 2))
(define (A284264v2 n) (A001222 (A283983 n)))

(define (A283484 n) (A000188 (A277324 n)))
(define (A283484v2 n) (A283983 (+ n n 1)))

(define (A284265 n) (A284264 (+ n n 1))) ;; o=0: Odd bisection of A284264.
(define (A284265v2 n) (A001222 (A283484 n)))

(define (A284266 n) (A277700 (+ n n 1)))
(define (A284266v2 n) (A000120 (A283975 n))) ;; o=0: Odd bisection of A277700.

(define (A284267 n) (A284271 (+ n n 1)))
(define (A284267v2 n) (A056169 (A277324 n)))

(define (A284268 n) (A284272 (+ n n 1)))
(define (A284268v2 n) (A275812 (A277324 n)))

(definec (A284271 n) (A056169 (A260443 n)))
(definec (A284272 n) (A275812 (A260443 n)))


(define A260442fast (MATCHING-POS 0 1 (lambda (n) (or (= 1 (A010051 n)) (= n (A260443 (A048675 n)))))))
(define A260442faster (MATCHING-POS 0 1 (lambda (n) (or (= 1 (A010051 n)) (and (not (< (A001221 n) (+ 1 (A243055 n)))) (= n (A260443 (A048675 n))))))))


(define (A277415 n) (A048675 (A260442 n)))

(definec (A277416 n) (let ((b (A260443 n))) (let loop ((k 0)) (if (= (A260442 k) b) k (loop (+ 1 k))))))


(definec (A260443as_coeff_list n)
  (cond ((zero? n) (list))
        ((= 1 n) (list 1))
        ((even? n) (cons 0 (A260443as_coeff_list (/ n 2))))
        (else (add_two_lists
                   (A260443as_coeff_list (/ (- n 1) 2))
                   (A260443as_coeff_list (/ (+ n 1) 2))
              )
        )
  )
)

(define (add_two_lists nums1 nums2)
  (let ((len1 (length nums1))
        (len2 (length nums2))
       )
    (cond ((< len1 len2) (add_two_lists nums2 nums1)) ;; recursion is for lazy people.
          (else (map + nums1 (append nums2 (make-list (- len1 len2) 0))))
    )
  )
)

(define (gcd_of_exp_lists nums1 nums2)
  (let ((len1 (length nums1))
        (len2 (length nums2))
       )
    (cond ((< len1 len2) (gcd_of_exp_lists nums2 nums1))
          (else (map min nums1 (append nums2 (make-list (- len1 len2) 0))))
    )
  )
)

(define (bitwise_and_of_exp_lists nums1 nums2)
  (let ((len1 (length nums1))
        (len2 (length nums2))
       )
    (cond ((< len1 len2) (bitwise_and_of_exp_lists nums2 nums1))
          (else (map A004198bi nums1 (append nums2 (make-list (- len1 len2) 0))))
    )
  )
)


(define (bitwise_or_of_exp_lists nums1 nums2)
  (let ((len1 (length nums1))
        (len2 (length nums2))
       )
    (cond ((< len1 len2) (bitwise_or_of_exp_lists nums2 nums1))
          (else (map A003986bi nums1 (append nums2 (make-list (- len1 len2) 0))))
    )
  )
)

(define (bitwise_xor_of_exp_lists nums1 nums2)
  (let ((len1 (length nums1))
        (len2 (length nums2))
       )
    (cond ((< len1 len2) (bitwise_xor_of_exp_lists nums2 nums1))
          (else (map A003987bi nums1 (append nums2 (make-list (- len1 len2) 0))))
    )
  )
)


(define (product_primes_to_kth_powers nums)
   (let loop ((p 1) (nums nums) (i 1))
      (cond ((null? nums) p)
            (else (loop (* p (expt (A000040 i) (car nums))) (cdr nums) (+ 1 i)))
      )
   )
)

(define (A277333 n) (let ((k (A048675 n))) (if (= (A260443 k) n) k 0))) ;; o=1:
;; (same-intfuns0? A001477 (COMPOSE A277333 A260443) 1024) --> #t
;; (same-intfuns0?  (COMPOSE A277333 1+ double) (COMPOSE double A277333 A064989 1+ double)  120) --> #t

(define A277417 (ZERO-POS 1 1 (lambda (n) (- (A277333 n) (A248663 n)))))


(define (A277705 n) (fold-left (lambda (a e) (* (+ 1 e) a)) 1 (A260443as_coeff_list n)))
(define (A277705v2 n) (A000005 (A260443 n)))
(define (A277705v3 n) (A106737 (A277020 n)))

(define (A277197 n) (- (A260443 (+ 1 n)) (A260443 n)))

(define (A277198v2 n) (gcd (A260443 (+ 1 n)) (A260443 n)))

(define (A277198 n) (product_primes_to_kth_powers (gcd_of_exp_lists (A260443as_coeff_list n) (A260443as_coeff_list (+ 1 n)))))


(define (A277325 n) (reduce * 1 (filter positive? (A260443as_coeff_list n))))
(define (A277325v2 n) (A005361 (A260443 n)))

(define (A277326 n) (reduce lcm 1 (filter positive? (A260443as_coeff_list n))))
(define (A277326v2 n) (A072411 (A260443 n)))


(define (A277327v2 n) (A001221 (A277198 n)))

(define (A277327 n) (length (filter positive? (gcd_of_exp_lists (A260443as_coeff_list n) (A260443as_coeff_list (+ 1 n))))))


(define (A277328v2 n) (A001222 (A277198 n)))
(define (A277328 n) (reduce + 0 (gcd_of_exp_lists (A260443as_coeff_list n) (A260443as_coeff_list (+ 1 n)))))

(define (A277314v2 n) (A001221 (A260443 n))) ;; Number of nonzero terms in Stern polynomial B(n,t).
(define (A277314 n) (length (filter positive? (A260443as_coeff_list n)))) ;; o=0:

;; (same-intfuns0? A277314 (COMPOSE A069010 A277020) 120) --> #t


(define (A277315v2 n) (A051903 (A260443 n))) ;; Max exponent in A260443, max coefficient in Stern polynomial B(n,t).
(define (A277315 n) (reduce max 0 (A260443as_coeff_list n))) ;; o=0:

(define (A277316 n) (A260443 (A000040 n)))

(define A277317 (MATCHING-POS 1 1 (lambda (n) (= 1 (A010051 (A277333 n)))))) ;; o=1: Slow!
(define Ajotkut (MATCHING-POS 1 1 (lambda (n) (= 1 (A010051 (A048675 n)))))) ;; o=1, supersequence of above.


(define (A277323 n) (A260443 (* 2 n)))
(define (A277323v2 n) (if (zero? n) 1 (A003961 (A260443 n))))

(define (A277324 n) (A260443 (+ 1 n n)))
(define (A277324v2 n) (if (zero? n) 2 (* (A260443 n) (A260443 (+ 1 n)))))



(define (A276081 n) (sum_factorials_times_elements_in (A260443as_coeff_list n)))

(define (A276081v2 n) (A276075 (A260443 n)))


(define (A278530 n) (sum_A000027_times_elements_in (A260443as_coeff_list n)))

(define (A278530v2 n) (A056239 (A260443 n)))

(define (A278544v2 n) (A003415 (A260443 n)))

(define (A104244 n) (A104244bi (A002260 n) (A004736 n)))

(define (A104245 n) (A104244bi (A004736 n) (A002260 n)))

(define (A104244bi row col)
   (fold-left (lambda (sum p.e) (+ sum (* (cdr p.e) (expt row (- (A000720 (car p.e)) 1)))))
              0
              (if (= 1 col) (list) (elemcountpairs (ifactor col))) ;; (sort (factor n) <)
   )
)


;; Not a practical way to compute it, but just as a demonstration:
(define (A073133 n) (A073133bi (A004736 n) (A002260 n))) ;; o=1: (by ascending antidiagonals).
(define (A073133bi row col) (A104244bi row (A206296 col)))

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

;; Then insert that below, between A003961 or A064989: (is it involution or not?)

(define A080147 (MATCHING-POS 1 1 (lambda (n) (= 1 (modulo (A000040 n) 4))))) ;; o=1: Positions of 4k+1 primes A002144 among all primes A000040.

(define A080148 (MATCHING-POS 1 1 (lambda (n) (= 3 (modulo (A000040 n) 4))))) ;; o=1: Positions of 4k+3 primes A002145 among all primes A000040.

(define (A002144 n) (A000040 (A080147 n)))
(define (A002145 n) (A000040 (A080148 n)))

(define A267097 (LEFTINV-LEASTMONO 1 1 A080147))

(define A267098 (LEFTINV-LEASTMONO 1 1 A080148))


(define (A038698 n) (- (A267098 n) (A267097 n))) ;; [Havermann] o=1: Surfeit of 4k-1 primes over 4k+1 primes, beginning with prime 2.
 	

;; Instead of
;; A108546 Lexicographically earliest permutation of primes such that for n>1 forms 4*k+1 and 4*k+3 alternate. 
;; (2, 3, 5, 7, 13, 11, 17, 19, 29, 23, 37, ...)
;; we need 2, 5,3, 13,17, 7,11, 29,
;; Permutation of primes such that 4*k+1 prime is replaced
;; with smallest unused 4k+3, and vice versa: A002144, A002145.

(define (A267100 n)
  (cond ((<= n 1) n)
        ((= 1 (modulo (A000040 n) 4)) (A080148 (A267097 n)))
        (else (A080147 (A267098 n)))
  )
)

(define (A267100v2 n) (A000720 (A267101 n)))

(define (A267101 n) (cond ((= 1 n) 2) ((= 1 (modulo (A000040 n) 4)) (A002145 (A267097 n))) (else (A002144 (A267098 n)))))

;; (same-intfuns1? A000027 (COMPOSE A267100 A267100) 1200) --> #t

(definec (A267099 n)
  (cond ((<= n 1) n)
        ((= 1 (A010051 n)) (A267101 (A000720 n)))
        (else (* (A267099 (A020639 n)) (A267099 (A032742 n))))
  )
)

;; (same-intfuns1? A267101 (COMPOSE A267099 A000040) 1200) --> #t

(definec (A267105 n)
  (cond ((<= n 1) n)
        ((= 1 (modulo (A000040 n) 4)) (+ 1 (* 2 (A267105 (A267097 n)))))
        (else (* 2 (A267105 (A267098 n))))
  )
)


(definec (A267106 n)
  (cond ((<= n 1) n)
        ((odd? n) (A080147 (A267106 (/ (- n 1) 2))))
        (else (A080148 (A267106 (/ n 2))))
  )
)

;; (same-intfuns1? A000027 (COMPOSE A267105 A267106) 256) --> #t
;; (same-intfuns1? A000027 (COMPOSE A267106 A267105) 256) --> #t


(definec (A267107 n)
  (cond ((<= n 1) n)
        ((= 1 (modulo (A000040 n) 4)) (A080148 (A267107 (A267097 n))))
        (else (A080147 (A267107 (A267098 n))))
  )
)

;; (same-intfuns1? A000027 (COMPOSE A267107 A267107) 256) --> #t

(define A268393 (RECORD-POS 1 1 A267107)) ;; o=1: Positions of records in "Manta-ray permutation", A267107.
(define (A268394 n) (A267107 (A268393 n))) ;; o=1: Record values in "Manta-ray permutation", A267107.


;; Cf. A111745 		a(2k-1) = k-th prime congruent to 3 mod 4, a(2k) = k-th prime congruent to 1 mod 4. 
;; 3, 5, 7, 13, 11, 17, 19, 29, 23, 37, 31, 41,

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

(define (A275715 n) (A243071 (A249823 n)))

(define (A273664 n) (A249746 (A032766 n)))

(define (A273664v2 n) (A249824 (A254050 n)))


(define (A273669 n) (+ (* 10 (/ (+ (- n 2) (A000035 n)) 2)) 9 (* (A000035 n) -7)))
(define (A273669v2 n) (+ (* 10 (/ (+ (- n 2) (if (odd? n) 1 0)) 2)) (if (odd? n) 2 9)))


(definec (A275716 n)
  (cond ((<= n 1) (+ 1 n))
        ((even? n) (A273669 (A275716 (/ n 2))))
        (else (A273664 (A275716 (/ (- n 1) 2))))
  )
)


(define A275717 (MATCHING-POS 1 2 (lambda (n) (> (A003961 n) (A003961 (- n 1))))))
(define A275717v2 (MATCHING-POS 1 2 (lambda (n) (> (A048673 n) (A048673 (- n 1))))))

(define A275718 (MATCHING-POS 1 2 (lambda (n) (< (A003961 n) (A003961 (- n 1))))))
(define A275718v2 (MATCHING-POS 1 2 (lambda (n) (< (A048673 n) (A048673 (- n 1))))))

(define (A275719 n) (- (A275718 n) (A275717 n)))
(define (A275719v2 n) (- (A275722 n) (A275721 n)))

(definec (A275720 n) (if (= 1 n) 0 (+ (A275720 (- n 1)) (if (> (A003961 n) (A003961 (- n 1))) 1 -1))))

(define A275721 (MATCHING-POS 1 1 (lambda (n) (> (A003961 (+ 1 n)) (A003961 n)))))
(define A275722 (MATCHING-POS 1 1 (lambda (n) (< (A003961 (+ 1 n)) (A003961 n)))))

(definec (A249746v2 n)
  (cond ((= 1 n) n)
        ((= 2 (modulo n 3)) (A273669 (A249746v2 (/ (+ 1 n) 3))))
        (else (A273664 (A249746v2 (/ (+ 1 (A064216 n)) 2))))
  )
)

(definec (A249824v3 n)
  (cond ((= 1 n) n)
        ((even? n) (A273669 (A249824v3 (/ n 2))))
        (else (A273664 (A249824v3 (A064989 n))))
  )
)


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


(define (A286471 n) (if (or (= 1 n) (= 1 (A001222 n))) 0 (+ 1 (- (A055396 (A032742 n)) (A055396 n)))))

(definec (A286470 n)
  (cond ((or (= 1 n) (= 1 (A001221 n))) 0)
;;      ((zero? (modulo (A032742 n) (A020639 n))) (A286470 (A032742 n)))
        (else (max (- (A055396 (A032742 n)) (A055396 n)) (A286470 (A032742 n))))
  )
)

(define (A286469 n) (max (A055396 n) (A286470 n)))


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

;; A065371  [Zumkeller] o=1: a(1) = 1, a(prime(i)) = prime(i) - i for i > 0 and a(u * v) = a(u) * a(v) for u, v > 0. 
(definec (A065371 n) (cond ((= 1 n) 1) (else (* (- (A020639 n) (A055396 n)) (A065371 (A032742 n))))))

(define (A065372 n) (A065371 (A065371 n))) ;; [Zumkeller] o=1: a(n) = A065371(A065371(n)).


(define (A248692 n) (A000079 (A056239 n)))
(define (A248692v2 n) (apply * (map A000079 (map A049084 (ifactor n)))))

;; A258851: [Heinz] o=0: The pi-based arithmetic derivative of n: a(p) = pi(p) for p prime, a(u*v) = a(u)*v + u*a(v). 
(definec (A258851 n) (if (<= n 1) 0 (+ (* (A055396 n) (A032742 n)) (* (A020639 n) (A258851 (A032742 n))))))

(define (A278510 n) (- (A258851 n) (A056239 n)))

(definec (A278520 n) (- (A243503 n) (A056239 n)))


(define A242422 (NONZERO-POS 1 1 (COMPOSE A010054 A056239)))
(define A242422v2 (ZERO-POS 1 1 (COMPOSE A002262 A056239)))
(define A242423 (ZERO-POS 1 1 (COMPOSE  A010054 A056239)))
(define A242423v2 (NONZERO-POS 1 1 (COMPOSE A002262 A056239)))

(define (A065091 n) (A000040 (+ 1 n)))
(define A065090 (COMPLEMENT 1 A065091))

(define (A006254 n) (/ (+ 1 (A065091 n)) 2))

(define (A005097 n) (/ (- (A065091 n) 1) 2))

(define A005384 (MATCHING-POS 1 1 (lambda (n) (and (prime? n) (prime? (+ n n 1)))))) ;; o=1: Sophie Germain primes p: 2p+1 is also prime.

(define A266400 (MATCHING-POS 1 1 (lambda (n) (prime? (/ (- (A065091 n) 1) 2)))))

(define A266400v2 (MATCHING-POS 1 1 (COMPOSE prime? A005097)))


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

(define (A246200 n) (/ (A057889 (* 3 n)) 3)) ;; XXX - Base-3 analogue of A057889 and applied to 2n. ? is now A263272

;; XXX - XFER the following three squares-squarefree-etc.ss :

;; XXX - These implementations could be much more optimized (even without full factoring routines):

(definec (A019554 n) (let loop ((k 1)) (if (zero? (modulo (* k k) n)) k (loop (+ 1 k))))) ;; o=1: Smallest number whose square is divisible by n.

(define (A000188 n) (/ n (A019554 n))) ;; o=1: (1) Number of solutions to x^2 = 0 (mod n). (2) Also square root of largest square dividing n. (3) Also Max_{ d divides n } GCD(d, n/d). 

(definec (A000188loop n) (let loop ((k (A000196 n))) (if (zero? (modulo n (* k k))) k (loop (- k 1)))))

(define (A007913 n) (/ n (A000290 (A000188 n)))) ;; Squarefree part of n: a(n) = smallest positive number m such that n/m is a square.

;; A008833 [] o=1: Largest square dividing n. 
(define (A008833 n) (/ n (A007913 n)))
(define (A008833v2 n) (A000290 (A000188 n)))


(define (A240025 n) (if (zero? n) 1 (- (A000267 n) (A000267 (- n 1))))) ;; XFER: Squares/squares-quarter.ss together with A000267 from intfun_a.scm

(define (A002265 n) (floor->exact (/ n 4))) ;; o=0: Integers repeated 4 times. 

(define (A002620 n) (A002265 (A000290 n))) ;; o=0: XFER: Squares/squares-quarter.ss
(define (A033638 n) (+ 1 (A002620 n))) ;; o=0: XFER: Squares/squares-quarter.ss

;; A278259 [AK] o=1: Least number with the prime signature of the n-th quarter-square.
(define (A278259 n) (if (<= n 1) 0 (A046523 (A002620 n))))

;; A278260 [AK] o=1: Least number with the same prime signature as {the n-th quarter-square}+1 has.
(define (A278260 n) (A046523 (A033638 n)))

;; (same-intfuns1? A278254 (COMPOSE A278259 double) 1024) --> #t

;; (same-intfuns1? A278162 (COMPOSE A278260 double) 1024) --> #t

(define (A002378 n) (* (+ 1 n) n)) ;; A002378: [NJAS] o=0: Oblong (or promic, pronic, or heteromecic) numbers: a(n) = n*(n+1).

;; A278256 [AK] o=1: Least number with the same prime signature as the n-th oblong number (A002378).
(define (A278256 n) (A046523 (A002378 n)))

;; (define (A278256_not_so_easy n) (if (<= (modulo (- n 1) 4) 1) (* 2 (A003961 (A278253 n))) (* 2 (A278253 n))))

;; (same-intfuns1? A278256 (COMPOSE A278259 1+ double) 1200) --> #t

;; 

(definec (A265400 n) (cond ((<= n 3) 0) ((<= n 8) (- 1 (A000035 n))) ((= 1 (A240025 (- n 1))) 0) ((= 1 (A240025 (- n 2))) (A033638 (- (A000267 n) 4))) (else (+ 1 (A265400 (- n 1)))))) ;; XFER: Squares/squares-quarter.ss

;; (definec (A265410 n) (cond ((= 1 n) 0) ((<= n 7) 1) ((or (= 1 (A240025 (- n 1))) (= 1 (A240025 (- n 2)))) (A265410 (- n 1))) (else (+ 1 (A265410 (- n 1))))))
(definec (A265410 n) (cond ((= 1 n) 0) ((<= n 7) 1) (else (+ (A265410 (- n 1))  (* (- 1 (A240025 (- n 1))) (- 1 (A240025 (- n 2))))))))

;; (definec (A265410 n) (cond ((= 1 n) 0) ((<= n 7) 1) ((= 1 (A240025 (- n 1))) (A033638 (- (A000267 n) 4))) ((= 1 (A240025 (- n 2))) (A265410 (- n 1))) (else (+ 1 (A265410 (- n 1))))))

(define (A265410v2 n) (cond ((= 1 n) 0) ((<= n 7) 1) ((= 1 (A240025 (- n 1))) (A033638 (- (A000267 n) 4))) (else (A265400 n))))


(definec (A078510 n) (if (< n 2) n (+ (A078510 (- n 1)) (A078510 (A265409 n))))) ;; o=0: Spiro-Fibonacci numbers, a(n) = sum of two previous terms that are nearest when terms arranged in a spiral. 

(define (A265370 n) (if (zero? n) n (A070939 (A078510 n))))

(define A265404 (FUN_FOR_NUMBER_OF_GREEDY_SUMMANDS (lambda (n) (A078510 (+ n 6)))))


(definec (A265409 n) (cond ((<= n 7) 0) (else (+ (A265409 (- n 1))  (* (- 1 (A240025 n)) (- 1 (A240025 (- n 1))))))))

(define (A265409v2 n) (- (A265410 (+ 1 n)) 1))
(definec (A265409v3 n) (cond ((<= n 7) 0) ((or (= 1 (A240025 n)) (= 1 (A240025 (- n 1)))) (A265409v3 (- n 1))) (else (+ 1 (A265409v3 (- n 1))))))

(define (A265359v2 n) (- n (A265409 n)))
(definec (A265359 n) (cond ((<= n 7) n) (else (+ (A265359 (- n 1)) (A240025 n) (A240025 (- n 1))))))

(definec (A265407 n) (if (< n 2) n (A003987bi (* 2 (A265407 (- n 1))) (A265407 (A265409 n)))))

(definec (A265408 n) (cond ((<= n 1) (+ 1 n)) (else (* (A003961 (A265408 (- n 1))) (A265408 (A265409 n)))))) ;; o=0: Spironacci polynomials. Cf. A206296.



(define (A265411 n) (cond ((zero? n) 1) ((= 1 n) 7) ((= 1 (A240025 (- n 1))) 3) (else 1)))

(definec (A265412 n) (if (zero? n) 1 (+ (A265411 n) (A265412 (- n 1))))) ;; o=

(define (A265413 n) (if (zero? n) 1 (+ 1 (A265412 (- n 1))))) ;; o=0: Positions of records in A265410.
(define A265413v2 (RECORD-POS 0 1 A265410)) ;; 

(define A265410v3 (LEFTINV-LEASTMONO 1 0 A265413))

(define (A265400v2 n) (if (= 1 (A240025 (- n 1))) 0 (A265410v3 n)))

(define (u-pairs-same-no-zeros? a0 a1 b0 b1)
  (and (not (zero? a0)) (not (zero? a1)) (not (zero? b0)) (not (zero? b1))
       (or (and (= a0 b0) (= a1 b1)) (and (= a0 b1) (= a1 b0)))
  )
)


;; Here we tacitly assume that A260643(0) = 0, to avoid extra checks when A265400 returns zero.
;; Also, by using memoizing-macro definec, this is not so slow as otherwise one might think of.
;; But it's still quadratic, or sort of, and yes, it is so slow. But at least the terms match.
(definec (A260643 n)
  (if (<= n 1) n
     (let ((b0 (A260643 (- n 1))) (b1 (A260643 (A265400 n))))
;; Candidate k will be chosen if neither pair {k,b0} and {k,b1} occur anywhere in the spiral constructed so far.
        (let outerloop ((k 1))
          (if (or (= k b0) (= k b1))
              (outerloop (+ k 1)) ;; Would be equal to either neighbour, skip this k.
;; Otherwise start checking whether either {k,b0} or {k,b1} already occur somewhere:
              (let innerloop ((j (- n 1)))
                (let ((c0 (A260643 j))
                      (c1 (A260643 (- j 1)))
                      (c2 (A260643 (A265400 j)))
                     )
                   (cond ((= 1 j) k) ;; No conflicting pairs found, return the least conflict-free k found.
                         ((u-pairs-same-no-zeros? k b0 c0 c1) (outerloop (+ 1 k)))
                         ((u-pairs-same-no-zeros? k b0 c0 c2) (outerloop (+ 1 k)))
                         ((u-pairs-same-no-zeros? k b1 c0 c1) (outerloop (+ 1 k)))
                         ((u-pairs-same-no-zeros? k b1 c0 c2) (outerloop (+ 1 k)))
                         (else (innerloop (- j 1)))
                   )
                )
              )
          )
        )
     )
  )
)



(definec (A030102 n) ;; XFER: Base-3/base3-core.ss
  (let loop ((z 0) (n n))
    (if (zero? n)
        z
        (loop (+ (* 3 z) (modulo n 3))
              (floor->exact (/ n 3))
        )
    )
  )
)

(define (A051064 n) (+ 1 (A007949 n))) ;; o=1: 3^a(n) exactly divides 3n. Or, 3-adic valuation of 3n.


;; A030103 in intfun_a.scm XFER Base-2/base2-base4.ss

(define (A030104 n) ;; XFER: Base-5/base3-core.ss
  (let loop ((z 0) (n n))
    (if (zero? n)
        z
        (loop (+ (* 5 z) (modulo n 5))
              (floor->exact (/ n 5))
        )
    )
  )
)


(definec (A030108 n) ;; Base 9 reversal of n (written in base 10). ;; XFER: Base-3/base3-base9.ss ??? Should have b(n) = A263273 analogue, and then another sequence b(8n)/8.
  (let loop ((z 0) (n n))
    (if (zero? n)
        z
        (loop (+ (* 9 z) (modulo n 9))
              (floor->exact (/ n 9))
        )
    )
  )
)

(define (A038500 n) (A000244 (A007949 n))) ;; o=1: Highest power of 3 dividing n. ;; XFER: Base-3/base3-core.ss

(define (A038502 n) (/ n (A038500 n))) ;; o=1: Remove 3's from n. ;; XFER: Base-3/base3-core.ss

;; (define (A065331 n) (* (A038500 n) (A006519 n))) ;; o=1: Largest 3-smooth divisor of n. 
;; (define (A065330 n) (/ n (A065331 n))) ;; o=1: a(n) = Max { k | gcd(n, k) = k and gcd(k, 6) = 1 }. 
(define (A065330 n) (A038502 (A000265 n)))
(define (A065331 n) (/ n (A065330 n)))



(definec (A265398 n) (if (= 1 n) n (* (A065331 n) (A064989 (A065330 n)) (A064989 (A064989 (A065330 n)))))) ;; o=1: Reduce nonnegative integer polynomial once by x^2 -> x+1


(definec (A265399 n) (if (= (A065331 n) n) n (A265399 (A265398 n))))

(define (A265752 n) (A007814 (A265399 n))) ;; o=1: Exponent of 2 in A265399.
(define (A265753 n) (A007949 (A265399 n))) ;; o=1: Exponent of 3 in A265399.

(definec (A265750 n) (if (zero? n) 2 (* (A003961 (A265750 (- n 1))) (A000079 (+ 2 (* 4 n))))))

;; (map Apolyt (iota0 5))
;; --> (1 64 746496 15116544000000 1440541490895360000000000 4963168975122196702564377600000000000000)

;Value 104: (1 64 746496 15116544000000)

(define (A263272 n) (/ (A263273 (+ n n)) 2))

(define (A263273 n) (if (zero? n) n (* (A030102 (A038502 n)) (A038500 n)))) ;; o=0:  ;; XFER: Base-3/base3-core.ss


;;;;; Interlude: conjugates:

(define (A265329 n) (A263273 (A057889 (A263273 n))))

(define (A265369 n) (A057889 (A263273 (A057889 n))))

(define (A265902 n) (A263273 (A263272 (A263273 n))))

(define (A265904 n) (A263272 (A263273 (A263272 n))))

(define (A266189 n) (A263273 (A264985 (A263273 n))))

(define (A266190 n) (A264985 (A263273 (A264985 n))))

(define (A266401 n) (A064989 (A263273 (A003961 n))))

(define (A266402 n) (A064989 (A057889 (A003961 n))))

(define (A266403 n) (A250470 (A263273 (A250469 n))))

(define (A266404 n) (A250470 (A030101 (A250469 n))))
(define (A266404v2 n) (A250470 (A057889 (A250469 n))))

(define (A266407 n) (A064989 (A263273 (+ n n -1))))

(define (A266408 n) (/ (+ 1 (A263273 (A003961 n))) 2))

(define (A266415 n) (A250470 (A263273 (A003961 n))))

(define (A266416 n) (A064989 (A263273 (A250469 n))))

;;;;;;;;;;;;;;;

;; XFER Primeind/primeind-shifts-cross-permutations.ss (or something like that): ???

;; A266635-A266646 are now reserved for your use. 

(define (A266645 n) (A064989 (A250469 n)))
(define (A266645v2 n) (A064989 (A249817 (A003961 (A249818 n)))))

(define (A266646 n) (A250470 (A003961 n)))

;; A283463-A283484 are now reserved for your use. 

(define (A283463 n) (A032742 (A266645 n)))
(define (A283463v2 n) (/ (A266645 n) (A020639 n)))

(define (A283464 n) (A032742 (A266646 n)))
(define (A283464v2 n) (/ (A266646 n) (A020639 n)))

(define (A283465 n) (A046523 (A250469 n)))
(define (A283465v2 n) (A046523 (A266645 n)))
(define (A283466 n) (A046523 (A266646 n)))


;;
;; (define A250469old A250469)
;; (define vecA250469 (read-b-file-to-vector "seqs/b250469_upto20000.txt" 20001))
;; (define (A250469 n) (vector-ref vecA250469 n))
;; 
;; (define A250470old A250470)
;; (define vecA250472 (read-b-file-to-vector "seqs/b250472_upto10000.txt" 10001))
;; (define (A250470 n) (if (even? n) "ei oo" (vector-ref vecA250472 (/ (+ n 1) 2))))
;; 


;;;;;;;;;;;;;;;


(define (A234957 n) (let loop ((k 4)) (if (not (zero? (modulo n k))) (/ k 4) (loop (* 4 k))))) ;; o=1: Highest power of 4 dividing n.

;; A065883 Remove factors of 4 from n (i.e. write n in base 4, drop final zeros, then rewrite in decimal).
(define (A065883 n) (/ n (A234957 n))) ;; o=1 /XFER Base-2/base2-base4.ss

(define (A072400 n) (modulo (A065883 n) 8)) ;; [Zumkeller] o=1: (Factors of 4 removed from n) modulo 8.


;; [NJAS] o=0: A089309 Write n in binary; a(n) = number of 1's in rightmost block. 
(define (A089309 n) (if (zero? n) n (A007814 (+ 1 (A000265 n))))) ;; XFER: Base-2.core.ss or such.


;; A035263 [Karamanos Konstantinos, NJAS] o=1: Trajectory of 1 under the morphism 0 -> 11, 1 -> 10. 
(define (A035263 n) (A000035 (A001511 n))) ;; 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, ...

;; A089309 & A035263 together should be enough to define A072400.


(define (A060904 n) (expt 5 (A112765 n))) ;; o=1: Exact power of 5 that divides the n-th Fibonacci number (A000045), which is the same as the exact power of 5 that divides n. /XFER Base-5/base5-core.ss

(define (A264994 n) (if (zero? n) n (* (A030103 (A065883 n)) (A234957 n)))) ;; o=0: Bijective base-4 reverse.
(define (A264995 n) (if (zero? n) n (* (A030104 (A132739 n)) (A060904 n)))) ;; o=0: Bijective base-5 reverse.

(define (A264981 n) (let loop ((k 9)) (if (not (zero? (modulo n k))) (/ k 9) (loop (* 9 k))))) ;; o=1: Highest power of 9 dividing n. XFER Base-3/base3-base9.ss

(define (A264993 n) (/ (A264994 (* 3 n)) 3)) ;; o=0: XFER Base-2/base2-base4.ss
(define (A265335 n) (/ (A264994 (* 5 n)) 5)) ;; o=0: XFER Base-2/base2-base4.ss

(define (A264979 n) (if (zero? n) n (* (A030108 (/ n (A264981 n))) (A264981 n)))) ;; o=0: Bijective base-9 reverse.

;; A265329-A265370 are now reserved for your use. 

(define (A265338 n) (/ (A264979 (* 8 n)) 8))


(definec (A265330 n) (if (odd? n) 0 (+ 1 (A265330 (A265352 (/ n 2)))))) ;; o=1. Zero-based row index to A265345.

(definec (A265331 n) (if (odd? n) 1 (+ 1 (A265331 (A265352 (/ n 2)))))) ;; o=1. One-based row index to A265345.

;; (same-intfuns1? A265330 (COMPOSE A007814 A263273) 65537) --> #t ;; Because A263273 is an involution?
;; (same-intfuns1? A265331 (COMPOSE A001511 A263273) 65537) --> #t

;; (same-intfuns1?  (COMPOSE A007814 A265351)  (COMPOSE A007814 A265352) 128) --> 16

;; (map (lambda (n) (- (A007814 (A263272 n)) (A007814 (A263273 n)))) (iota 64))
;; --> (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 -3)

;; Compare to:
;; (same-intfuns1? A007814 (COMPOSE A007814 A057889) 1024) --> #t
;; (same-intfuns1? A057889  (lambda (n) (/ (A057889 (* 8 n)) 8)) 1024) --> #t

;;
(definec (A265910 n) (if (odd? n) (A265354 (/ (- n 1) 2)) (A265910 (A265352 (/ n 2)))))

(define (A265911 n) (+ 1 (A265910 n))) ;; o=1. One-based colum index to A265345.

(define (Amanhat n) (+ (A265330 n) (A265410 n))) ;; Manhattan distance to ? + 1 ? Same as A265340 ?

(define A264990 (ZERO-POS 1 1 A265410)) ;; o=1. A264980 sorted into ascending order.

;; A265404-A265415 are now reserved for your use. 

;; Cross-compose A263273 with A249813 / A249814 ? (or similar ones) or with A163511 (too much range there) etc.
;, Also with A178590 ? (although it's not a permutation)

(define (A265339 n) (A263273 (A004526 (A263273 n)))) ;; XXX- Submit!

(definec (A265340 n) (if (zero? n) 0 (+ 1 (A265340 (A265339 n)))))  ;; XXX- Submit!

(define (A265340v2 n) (if (zero? n) 0 (A070939 (A263273 n))))

(definec (A265336 n) (if (zero? n) 0 (+ (A265336 (A265339 n)) (if (even? n) 1 0))))
(define (A265336v2 n) (A080791 (A263273 n)))

(definec (A265337 n) (if (zero? n) 0 (+ (A265337 (A265339 n)) (if (odd? n) 1 0))))

(define (A265341 n) (+ 1 (* 2 (A265353 n)))) ;; o=0: 
(define (A265342 n) (* 2 (A265351 n))) ;; o=0: 

(define (A265345 n) (A265345bi (A002262 (+ -1 n)) (A025581 (+ -1 n)))) ;; o=1.
(define (A265345bi row col) (if (= 0 row) (A265341 col) (A265342 (A265345bi (- row 1) col)))) ;; row>=0, col>=0.

(define (A265345v2 n) (A265345biv2 (A002262 (+ -1 n)) (A025581 (+ -1 n)))) ;; o=1.
(define (A265345biv2 row col) (A263273 (* (A000079 row) (A263273 (A265341 col))))) ;; row>=0, col>=0.

(define (A265346 n) (let ((col (A265911 n)) (row (A265331 n))) (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2))))

(define (A265895 n) (A265895bi (A002262 (+ -1 n)) (A025581 (+ -1 n)))) ;; o=1.
(define (A265895bi row col) (A263273 (A265345bi row col)))

(define (A265895v2 n) (A265895biv2 (A002262 (+ -1 n)) (A025581 (+ -1 n)))) ;; o=1.
(define (A265895biv2 row col) (* (A000079 row) (A263273 (A265341 col)))) ;; row>=0, col>=0.

;; (define (A265895v2 n) (A263273 (A265345 n)))

(define (A265896 n) (A265346 (A263273 n)))


(define (A265347 n) (A265345bi (A025581 (+ -1 n)) (A002262 (+ -1 n)))) ;; o=1. Transpose of A265345.

(define (A265348 n) (let ((row (A265911 n)) (col (A265331 n))) (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2))))

(define (A265343 n) (A264978 (A263272 n))) ;; XXX - compose with A265363 and A265364 in some "twisted" order
(define (A265344 n) (A263272 (A264978 n)))

(define (A265351 n) (A263272 (A263273 n))) ;; o=0: Composition of A263273 with its even bisection. 
(define (A265351v2 n) (A264974 (A265367 n)))

(define (A265352 n) (A263273 (A263272 n))) ;; o=0: Composition of A263273 with its even bisection. 
(define (A265352v2 n) (A265368 (A264974 n)))

(define (A265353 n) (A264985 (A263273 n))) ;; o=0: Composition of A263273 with its odd bisection. 
(define (A265354 n) (A263273 (A264985 n))) ;; o=0: Composition of A263273 with its odd bisection. 

(define (A265355 n) (A263272 (A264985 n))) ;; o=0: Composition of bisections of A263273.
(define (A265356 n) (A264985 (A263272 n))) ;; o=0: Composition of bisections of A263273.

(define (A265357 n) (A264989 (A263272 n))) ;; o=0: Composition of A263272 with its odd bisection.
(define (A265358 n) (A263272 (A264989 n))) ;; o=0: Composition of A263272 with its odd bisection.

(define (A265361 n) (A264974 (A264989 n))) ;; o=0: Composition of bisections of A263272. 
(define (A265362 n) (A264989 (A264974 n))) ;; o=0: Composition of bisections of A263272. 

(define (A265363 n) (A264974 (A263273 n))) ;; o=0: Composition of A263273 with its quadrisection.
(define (A265364 n) (A263273 (A264974 n))) ;; o=0: Composition of A263273 with its quadrisection.

(define (A265365 n) (A264978 (A263273 n))) ;; o=0: Composition of A263273 with its octisection. 
(define (A265366 n) (A263273 (A264978 n))) ;; o=0: Composition of A263273 with its octisection. 

(define (A265367 n) (A264974 (A263272 (A263273 n))))
(define (A265367v2 n) (A264974 (A265351 n)))
(define (A265367v3 n) (A264975 (A263273 n)))

(define (A265368 n) (A263273 (A263272 (A264974 n))))
(define (A265368v2 n) (A265352 (A264974 n)))
(define (A265368v3 n) (A263273 (A264976 n)))

;;;;;;;;;;;;;;;;;


(define (A264965 n) (A263273 (A057889 n)))
(define (A264966 n) (A057889 (A263273 n)))

(define (A266641 n) (/ (A264965 (* 2 n)) 2))
(define (A266641v2 n) (A263272 (A057889 n)))

(define (A266642 n) (/ (A264966 (* 2 n)) 2))

(define (A266643 n) (/ (A264965 (* 3 n)) 3))
(define (A266643v2 n) (A263273 (A246200 n)))

(define (A266644 n) (/ (A264966 (* 3 n)) 3))

(define (A266635 n) (A126760 (A264965 (A007310 n)))) ;; o=1: Restriction to nonregular part.
(define (A266636 n) (A126760 (A264966 (A007310 n))))


(define (A264967 n) (A263272 (A246200 n)))
(define (A264968 n) (A246200 (A263272 n)))

(define (A264974 n) (/ (A263273 (* 4 n)) 4))
(define (A264974v2 n) (/ (A264986 n) 2))
(define (A264974v3 n) (/ (A263272 (* 2 n)) 2))

(define (A264978 n) (/ (A263273 (* 8 n)) 8))
(define (A264978v2 n) (/ (A263272 (* 4 n)) 4))
(define (A264978v3 n) (/ (A264974 (* 2 n)) 2))

(define (A264975 n) (A264974 (A263272 n)))
(define (A264975v2 n) (/ (A263273 (* 2 (A263273 (* 2 n)))) 4))
(define (A264975v3 n) (/ (A264984 (A264984 n)) 4))
(define (A264975v4 n) (/ (A263272 (A264984 n)) 2))

(define (A264976 n) (A263272 (A264974 n)))
(define (A264976v2 n) (/ (A263273 (/ (A263273 (* 4 n)) 2)) 2)) ;; a(n) = A263273(A263273(4*n) / 2)) / 2.

(define (A264983 n) (A263273 (+ 1 n n))) ;; Odd bisection of A263273.
(define (A264984 n) (A263273 (+ n n))) ;; Even bisection of A263273.

(define (A264985 n) (/ (- (A264983 n) 1) 2)) ;; o=0
(define (A264996 n) (/ (+ 1 (A263273 (+ n n -1))) 2)) ;; o=1:
(define (A264996v2 n) (+ 1 (A264985 (- n 1)))) ;; o=1

(define (A264986 n) (A263272 (+ n n)))
(define (A264987 n) (A263272 (+ 1 n n)))

(define (A264989 n) (/ (- (A264987 n) 1) 2))
(define (A264989v2 n) (/ (- (A263273 (A016825 n)) 2) 4))

(define (A264991 n) (A264989 (A264985 n)))
(define (A264992 n) (A264985 (A264989 n)))

;; Transfer to Base-3 somewhere, those above.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Where the parity stays same, or changes:
(define Akuulit (MATCHING-POS 1 1 (lambda (n) (= 0 (A000035 (+ n (A048673 n)))))))
(define Amuulit (MATCHING-POS 1 1 (lambda (n) (= 1 (A000035 (+ n (A048673 n)))))))

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


(definec (A250469 n) (if (= 1 n) n (A083221bi (+ (A055396 n) 1) (A078898 n))))
(define (A250469v2 n) (A249817 (A003961 (A249818 n))))

(define (A250470 n) (A249817 (A064989 (A249818 n))))
(define (A250471 n) (/ (+ 1 (A250469 n)) 2))
(define (A250472 n) (A250470 (+ n n -1)))
(define (A250472v2 n) (if (= 1 n) n (A083221bi (- (A055396 (+ n n -1)) 1) (A078898 (+ n n -1)))))

(define (A250479 n) (A250470 (+ n n)))


(definec (A280702 n) (gcd (A003961 n) (A250469 n)))

(define (A280703 n) (/ (A003961 n) (A280702 n)))

(define (A280704 n) (/ (A250469 n) (A280702 n)))

(define (A280701 n) (- n (A280704 n)))

(define (A280692 n) (- (A003961 n) (A250469 n)))

(define A280693 (ZERO-POS 1 1 A280692))
(define A280693v2 (FIXED-POINTS 1 1 A266645))

(define A280693v2 (ZERO-POS 1 1 (COMPOSE -1+ A280703)))
;; (define A280693v3_not_this (ZERO-POS 1 1 (COMPOSE -1+ A280704)))

;; (define Ajokutus (MATCHING-POS 1 1 (lambda (n) (and (zero? (modulo (A003961 n) (A250469 n))) (> (A003961 n) (A250469 n))))))

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

;; Note:
;; (definec (Ajokutus n) (if (zero? n) 24 (A064989 (* 3 (+ -1 (Ajokutus (- n 1)))))))

;; (map halve (map Ajokutus (iota0 20)))

;; 12, 19, 31, 59, 44, 46, 55, 107, 134, 166, 317, 398, 282, 557, 470, 622, 763, 531, 1051, 1267, 1807




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

;; A137288 [Ctibor O. Zizka] o=1: Numbers n such that 2*prime(n)-1 is prime.
(define (A137288 n) (A000720 (A005382 n)))




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

;; A277885 [AK] o=1: a(n) = index of the least non-unitary prime divisor of n or 0 if no such prime-divisor exists.
(definec (A277885 n) (cond ((= 1 n) 0) ((< 1 (A067029 n)) (A055396 n)) (else (A277885 (A028234 n)))))

(definec (A277885v2 n) (cond ((= 1 n) 0) ((< 1 (A067029 n)) (A055396 n)) (else (A277885v2 (A032742 n)))))

(definec (A249739v2 n) (cond ((= 1 n) 1) (else (let* ((spf (A020639 n)) (gpd (/ n spf))) (if (zero? (modulo gpd spf)) spf (A249739v2 gpd))))))
;; (same-intfuns1? A249739v2 (COMPOSE A008578 1+ A277885) 12000) --> #t


(define (A277886 n) (if (zero? (A277885 n)) n (* (A000040 (+ 1 (A277885 n))) (/ n (expt (A249739 n) 2)))))

;; (same-intfuns1? A048675  (COMPOSE A048675 A277886) 1200) --> #t

;; A097246 [Zumkeller] o=1: Replace factors of n that are squares of a prime by the prime succeeding this prime.

(definec (A097246 n) (if (= 1 n) 1 (* (A000244 (A004526 (A007814 n))) (A000079 (A000035 (A007814 n))) (A003961 (A097246 (A064989 n))))))
(define (A097246v2 n) (* (A003961 (A000188 n)) (A007913 n)))

(definec (A097248 n) (if (not (zero? (A008683 n))) n (A097248 (A097246 n))))

;; It is same:
(definec (A097248v2 n) (if (zero? (A277885 n)) n (A097248v2 (A277886 n))))
;; as: 
;; A097248 a(n)=r(n,m) with m such that r(n,m)=r(n,m+1), where r(n,k)=A097246(r(n,k-1)), r(n,0)=n. 
;; A097248(n)=r(n,a(n));

(definec (A097249 n) (if (not (zero? (A008683 n))) 0 (+ 1 (A097249 (A097246 n)))))

;; a(n) = number of times we must iterate A097246, starting at A260443(n), before the result is squarefree. 
(define (A277899slow n) (A097249 (A260443 n)))

(define (A277899 n) (A097249_for_coeff_list (A260443as_coeff_list n)))

(define (A097249_for_coeff_list nums)
   (let loop ((nums nums) (s 0))
      (if (<= (reduce max 0 nums) 1)
          s
          (loop (A097246_for_coeff_list nums) (+ 1 s))
      )
   )
)

(define (A097246_for_coeff_list nums) (add_two_lists (map A000035 nums) (cons 0 (map A004526 nums))))


;; (same-intfuns1? A048675  (COMPOSE A048675 A097248v2) 1200) --> #t


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Say first:
;; (define vecA265388 (read-b-file-to-vector "seqs/b265388_upto10000.txt" 10001))

(define (A265388fast n) (vector-ref vecA265388 n))

(define (A265388 n) ;; o=1: a(n) = gcd{k=1..n-1} binomial(2*n, 2*k).
 (let loop ((z 0) (k 1))
        (cond ((>= k n) z)
              ((= 1 z) z) ;; No need to continue.
              (else (loop (gcd z (A007318tr (* 2 n) (* 2 k))) (+ k 1)))
        )
 )
)

(define (range1-n n)
   (let loop ((n n) (result (list)))
      (cond ((zero? n) result)
            (else (loop (- n 1) (cons n result)))
      )
   )
)

(define (A265388with_fold n) (fold-left gcd 0 (map (lambda (k) (A007318tr (* 2 n) (* 2 k))) (range1-n (- n 1)))))

(define (A265388with_fold_v2 n) (fold-right gcd 0 (map (lambda (k) (A007318tr (* 2 n) (* 2 k))) (range1-n (- n 1)))))


;; A265394-A265403 are now reserved for your use. 

(define A265394 (RECORD-POS 1 1 A265388))

(define (A265395 n) (A265388 (A265394 n)))

(define (A265396 n) (numerator (/ (A265395 n) (A265394 n))))

(define A265401 (ZERO-POS 1 1 (COMPOSE -1+ A265388)))

(define A265402 (FIXED-POINTS 1 1 A265388))

(define A265403 (MATCHING-POS 1 1 (lambda (n) (= (+ n n -1) (A265388 n)))))

;;;; XXX - XFER Seqs/Factorial/factorial-core.ss

;; (define (A265609 n)  (A265609bi (A002262 n) (A025581 n))) ;; o=0: [Luschny] Array read by ascending antidiagonals: A(n,k) the rising factorial, also known as Pochhammer function, for n>=0 and k>=0.

;; (define (A265609bi col row) (if (zero? col) 1 (* (+ col row -1) (A265609bi (- col 1) row))))

(define (A265609 n) (A265609bi (A025581 n) (A002262 n)))

(define (A265609bi row col) (if (zero? col) 1 (* (+ row col -1) (A265609bi row (- col 1)))))

(define (A265609biv2 x n) (/ (A000142 (+ x n -1)) (A000142 (- x 1))))  ;; x^(n), i.e. (x+n-1)!/(x-1)!



(define (A265890 n) (A265890bi (A025581 n) (A002262 n)))

(define (A265890bi row col) (A099563 (A265609bi row col)))

(define (A000407 n) (/ (A000142 (+ n n 1)) (A000142 n))) ;; o=0: a(n) = (2n+1)! / n!. 

(define (A001813 n) (/ (A000142 (+ n n)) (A000142 n))) ;; o=0: a(n) = (2n)! / n!. 

;; XFER Factorial/factorial-base-core.ss
(define (A099563loop n)
   (let loop ((n n) (i 2))
     (let* ((dig (modulo n i)) (next-n (/ (- n dig) i)))
        (if (zero? next-n) dig (loop next-n (+ 1 i)))
     )
   )
)

(definec (A099563 n) (cond ((zero? n) n) ((= 1 (A265333 n)) 1) (else (+ 1 (A099563 (A257684 n))))))
;; XFER Factorial/factorial-base-core.ss

(define (A276153 n) (let loop ((n n) (i 1)) (let* ((p (A000040 i)) (dig (modulo n p)) (next-n (/ (- n dig) p))) (if (zero? next-n) dig (loop next-n (+ 1 i)))))) ;; XFER Primorial/primorial-base-core.ss

(define (A276153v2 n) (A071178 (A276086 n)))


;; A003266 [NJAS] o=1: Product of first n nonzero Fibonacci numbers F(1), ..., F(n).
(definec (A003266 n) (if (= 1 n) n (* (A000045 n) (A003266 (- n 1)))))

;; A099564 [John W. Layman] o=0 (should be!): Final nonzero number in the sequence n, f(n,2), f(f(n,2),3), f(f(f(n,2),3),4),..., where f(n,d)=Floor(n/F(n+1)), with F denoting the Fibonacci numbers (A000045). 

(define (A099564 n) (let loop ((n n) (i 3)) (let* ((f (A000045 i)) (dig (modulo n f)) (next-n (/ (- n dig) f))) (if (zero? next-n) dig (loop next-n (+ 1 i))))))

;; Standalone version:
(define (A099564v2 n) (let loop ((n n) (f1 1) (f2 2)) (let* ((dig (modulo n f2)) (next-n (/ (- n dig) f2))) (if (zero? next-n) dig (loop next-n f2 (+ f1 f2))))))



(define (A265333 n) (if (zero? n) n (let loop ((f 1) (i 2)) (let ((nextf (* i f))) (if (> nextf n) (if (< n (* 2 f)) 1 0) (loop nextf (+ 1 i)))))))


(define A265334 (NONZERO-POS 1 1 A265333))


(define (A265891 n) (A099563 (A000407 n))) ;; o=0: 

(define (A265891v2 n) (A265890bi (+ 1 n) (+ 1 n))) ;; o=0:

(define (A265893 n) (- (A084558 n) (A230403 n))) ;; o=0: 

(define (A265894 n) (A099563 (A001813 n))) ;; o=0:
(define (A265894v2 n) (A265890bi (+ 1 n) n)) ;; o=0:

(define (A265892 n) (A265892bi (A025581 n) (A002262 n)))

(define (A265892bi row col) (A265893 (A265609bi row col)))

(define A265897 (ZERO-POS 0 0 (COMPOSE -1+ A265891)))

(define A265898 (ZERO-POS 0 0 (COMPOSE -1+ A265894)))
(define A265899 (MATCHING-POS 1 1 (lambda (n) (<= (A265894 n) (A265894 (- n 1))))))

(define (A266119 n) (- (A265899 (+ 1 n)) (A265899 n))) ;; o=1:
(define (A266120 n) (A265894 (- (A265899 n) 1))) ;; o=1: 

(definec (A001710 n) (cond ((<= n 2) 1) (else (* n (A001710 (- n 1)))))) ;; o=0: [Sloane] a(0) = a(1) = a(2) = 1; a(n)=n*a(n-1) 

(define (A001710v2 n) (cond ((<= n 1) 1) ((even? n) (* (/ n 2) (A000142 (- n 1)))) (else (* (/ (- n 1) 2) (+ (A000142 (- n 1)) (A000142 (- n 2)))))))

(define (A001710Detlefs n) (if (<= n 1) 1 (* (A000142 (- n 2)) (A000217 (- n 1)))))

(define (A001710for_odd n) (+ (A153880 (A001710 (- n 1)))  (A001710 (- n 1))))

(define (A001710for_oddx n) (+ (A153880 (A001710 (+ n n)))  (A001710 (+ n n))))



(define (A026741 n) (if (even? n) (/ n 2) n)) ;; o=0: a(n) = n if n odd, n/2 if n even.


(define (A268389 n)
   (let loop ((n n) (s 0))
     (let ((k (A006068 n)))
       (if (odd? k) s (loop (/ k 2) (+ 1 s)))
     )
   )
)


(definec (A268389rec n) (if (odd? (A006068 n)) 0 (+ 1 (A268389rec (/ (A006068 n) 2)))))

(define (A277818 n) (+ 1 (A268389 n)))

(define (A268669 n)
   (let loop ((n n))
     (let ((k (A006068 n)))
       (if (odd? k) n (loop (/ k 2)))
     )
   )
)

(definec (A268669rec n) (if (odd? (A006068 n)) n (A268669rec (/ (A006068 n) 2))))

(define (A268389v2 n) (A007949 (A235042 n)))

(definec (A268395 n) (if (zero? n) n (+ (A268389 n) (A268395 (- n 1)))))

;; A268708-A268729 are now reserved for your use. 

(definec (A268708 n) (if (zero? n) 0 (+ 1 (A268708 (A268395 n)))))

(define (A268709 n) (A268708 (A000079 n)))

(define (A268710 n) (A268708 (+ 1 (A000079 n))))


(definec (A268711 n)
  (cond ((= 1 n) n)
        ((zero? (- (A268680 n) (A268680 (- n 1)))) (* 2 (A268711 (- n (A268680 n)))))
        (else (+ 1 (* 2 (A268711 (- (A268680 n) 1)))))
  )
)


(definec (A268712 n)
  (cond ((= 1 n) n)
        ((even? n) (A268677 (A268712 (/ n 2))))
        (else (A268678 (+ 1 (A268712 (/ (- n 1) 2)))))
  )
)

;; (same-intfuns1? A000027 (COMPOSE A268711 A268712) 1024) --> #t

;; (same-intfuns1? A000027 (COMPOSE A268712 A268711) 1024) --> #t


(definec (A268678 n) (if (zero? n) n (+ (A268679 n) (A268678 (- n 1)))))
(define (A268678v2 n) (A268395 (A001969 (+ 1 n))))

(define A268677 (COMPLEMENT 1 A268678))

(define (A268679 n) (A268389 (A001969 (+ 1 n))))

(define A268680 (LEFTINV-LEASTMONO 0 0 A268678))

(define (A268672 n) (- n (A268395 n)))

(define A268713 (RECORD-POS 0 0 A268672))


(define (A268670 n) (A006068 (A268669 n)))

(define (A268671 n) (/ (+ 1 (A268670 n)) 2))


(definec (A268384 n) (cond ((<= n 1) n) ((odd? (A006068 n)) 0) (else (A268384 (/ (A006068 n) 2)))))
(define (A268384v2 n) (A209229 (A193231 n))) ;; o=0: Characteristic function of A001317.

(definec (A001317v2 n) (if (zero? n) 1 (A048724 (A001317v2 (- n 1)))))

(define (A001317v3 n) (A048723bi 3 n))

(definec (A001317v5 n) (if (zero? n) 1 (A048720bi 3 (A001317v5 (- n 1)))))

(define A001317slow (RECORD-POS 0 1 A268389))

(definec (A001318 n) (if (zero? n) 0 (+ (if (even? n) (/ n 2) n) (A001318 (- n 1))))) ;; o=0: Generalized pentagonal numbers: n*(3*n-1)/2, n=0, +- 1, +- 2, +- 3,....

(definec (A001318v2 n) (if (zero? n) 0 (+ (A026741 n) (A001318 (- n 1))))) ;; o=0: Generalized pentagonal numbers: n*(3*n-1)/2, n=0, +- 1, +- 2, +- 3,....

(define (A262613 n) (A000203 (A001318 n))) ;; o=1: [Pol] Sum of divisors of n-th generalized pentagonal number.

;; XFER Divisors/divisors-tree.ss:

(define (A263271bi_simpler row col)
  (cond ((zero? col) row)
        ((and (zero? row) (= 1 col)) 2)
        ((zero? (A263271bi_simpler row (- col 1))) 0)
        (else (A262686 (A263271bi_simpler row (- col 1))))
  )
)

(define (A263271v2 n) (A263271bi_simpler (A002262 n) (A025581 n)))

(define (A265751bi row col)
  (cond ((zero? col) row)
        ((A082284 row) =>
           (lambda (lad)
             (if (zero? lad)
                 lad 
                 (A265751bi lad (- col 1))
             )
           )
        )
  )
)


(define (A265751v2bi row col)
  (cond ((zero? col) row)
        ((and (zero? row) (= 1 col)) 1)
        ((zero? (A265751v2bi row (- col 1))) 0)
        (else (A082284 (A265751v2bi row (- col 1))))
  )
)




(define (A265751 n) (A265751bi (A002262 n) (A025581 n)))

(define (A265751v2 n) (A265751v2bi (A002262 n) (A025581 n)))

(definec (A266110 n) (cond ((A082284 n) => (lambda (lad) (if (zero? lad) 0 (+ 1 (A266110 lad)))))))

(definec (A266111 n) (cond ((A082284 n) => (lambda (lad) (if (zero? lad) 1 (+ 1 (A266111 lad)))))))


(definec (A266112 n)
  (let ((parent (- n (A000005 n))))
    (let loop ((k (- n 1)))
       (cond ((<= k parent) 1)
             ((= (- k (A000005 k)) parent) 0) ;; A younger sibling found ?
             (else (loop (- k 1)))
       )
    )
  )
)

(definec (A266113 n) (if (= 1 n) n (+ (A266112 n) (A266113 (- n 1)))))

(define A266114 (NONZERO-POS 1 1 A266112))

(define A266115 (ZERO-POS 1 1 A266112))

(definec (A266116 n) (cond ((A082284 n) => (lambda (lad) (if (zero? lad) n (A266116 lad))))))

(define (A266116v2 n) (A265751bi n (A266110 n)))

(definec (A264990 n) (if (zero? n) n (max (A257511 n) (A264990 (A257684 n)))))

(define A265349 (MATCHING-POS 0 0 (lambda (n) (<= (A264990 n) 1))))
;; (define A265349v2 (ZERO-POS 0 0 A275949))

(define A265350 (MATCHING-POS 1 1 (lambda (n) (> (A264990 n) 1))))

(define (A264990old n) ;; Largest number of the same nonzero digits.
  (if (zero? n) n
   (let loop ((n n) (fex (if (zero? n) (list 0) (list))) (i 2))
      (cond ((zero? n)
                  (car (sort (multiplicities (sort (remove zero? fex) <)) >))
            )
            (else (loop (floor->exact (/ n i)) (cons (modulo n i) fex) (1+ i)))
      )
   )
  )
)

(defineperm1 (A266117 n)
  (if (= 1 n)
      n
      (let ((prev (A266117 (- n 1))))
         (let loop ((k 1))
             (cond ((and (not-lte? (A266118 k) (- n 1)) (= (A264990 (* k prev)) 1)) k)
                   (else (loop (+ 1 k)))
             )
         )
      )
  )
)

(define (A266118 n) (A266117 (- n)))



(define (A061858 n) (A061858bi (A002262 n) (A025581 n)))

(define (A061858bi x y) (- (A004247bi x y) (A048720bi x y)))


(defineperm1 (A266195 n) ;; o=1: Lexicographically earliest sequence of distinct positive integers such that for any n>0, A061858(a(n),a(n-1)) = 0.

  (cond ((= 1 n) n)
        (else
          (let ((prev (A266195 (- n 1))))
            (let loop ((k 1))
               (cond ((and (not-lte? (A266196 k) (- n 1)) (zero? (A061858bi k prev))) k)
                     (else (loop (+ 1 k)))
               )
            )
          )
        )
  )
)

(define (A266196 n) (A266195 (- n)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; XXX: Think about this also:
;; Yes, see: (A057889 10) = 10, (A057889 12) = 12, (A057889 14) = 14
;; Although (the real) A266195 satisfies the same condition as this one
;; (because products in A266194 can be formed with A048720 also)
;; it's not the lexicographically earliest sequence satisfying the condition
;; a(n)*a(n-1) = A057889(A057889(a(n)) * A057889(a(n-1))),
;; which the below would be: (Submit ???)
;;;;;;;;;;;;;;;;;;;;;;;;;

(defineperm1 (A266351 n)
  (cond ((= 1 n) n)
        (else
          (let ((prev (A266351 (- n 1))))
            (let loop ((k 1))
               (cond ((and (not-lte? (A266352 k) (- n 1)) (= (A057889 (* k prev)) (* (A057889 k) (A057889 prev)))) k)
                     (else (loop (+ 1 k)))
               )
            )
          )
        )
  )
)


(define (A266352 n) (A266351 (- n)))


;; And the same with Blue Code:

(defineperm1 (A265405 n)
  (cond ((= 1 n) n)
        (else
          (let ((prev (A265405 (- n 1))))
            (let loop ((k 1))
               (cond ((and (not-lte? (A265406 k) (- n 1)) (= (A193231 (* k prev)) (* (A193231 k) (A193231 prev)))) k)
                     (else (loop (+ 1 k)))
               )
            )
          )
        )
  )
)

(define (A265406 n) (A265405 (- n)))


;; And Stern's diatomic sequence, even though it is not a permutation:

(defineperm1 (A266405 n)
  (cond ((= 1 n) n)
        (else
          (let ((prev (A266405 (- n 1))))
            (let loop ((k 1))
               (cond ((and (not-lte? (A266406 k) (- n 1)) (= (A002487 (* k prev)) (* (A002487 k) (A002487 prev)))) k)
                     (else (loop (+ 1 k)))
               )
            )
          )
        )
  )
)

(define (A266406 n) (A266405 (- n)))


;;;;;;;;;;;;;;;;;;;;;;;;;


(define (A264982 n) (A070939 (A266195 n)))

(define (A266186 n) (A266196 (A000079 n)))
(define A266186v2 (RECORD-POS 0 1 A264982))

(define A266197 (MATCHING-POS 1 1 (lambda (n) (= (A070939 (A266195 n)) (A070939 (A266195 (+ 1 n)))))))

(define (A265748 n) (A266195 (A266197 n)))
(define (A265749 n) (A266195 (+ 1 (A266197 n))))

(define (A266194 n) (* (A266195 n) (A266195 (+ n 1))))
(define (A266194v2 n) (A048720bi (A266195 n) (A266195 (+ n 1))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a(1) = 1, then each a(n) = (A004074(n)+1)-th number selected from those not yet in the sequence.

(defineperm1 (A266411 n) ;; o=1: 
   (if (<= n 1)
       n
       (let loop ((i 1) ;; No optimization.
                  (the-nth-one (A004074 n))
                 )
             (cond ((not-lte? (A266412 i) n) ;; Found i such that A266412(i) > n, thus that i unused.
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

(define (A266412 n) (A266411 (- n)))

;;;;;;;;;;;


(define (A284013 n) (- n (A002487 n))) ;; XFER Base-2.A002487.etc.ss or such.

;; a(1) = 1, then each a(n) = (A002487(n))-th number selected from those not yet in the sequence.

(defineperm1 (A266413 n) ;; o=1: 
   (if (<= n 1)
       n
       (let loop ((i 1) ;; No optimization.
                  (the-nth-one (+ -1 (A002487 n)))
                 )
             (cond ((not-lte? (A266414 i) n) ;; Found i such that A266414(i) > n, thus that i unused.
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

(define (A266414 n) (A266413 (- n)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Related to A265901, A265903, etc. (XFER: Hofstadterian/Hofstadterian-A004001.ss or such)

;; A095901 [Robert G. Wilson v] o=1: A004001 (mod 2). 
(define (A095901 n) (A000035 (A004001 n)))

;; A095902 [Robert G. Wilson v] o=1: Number of odd entries in A004001 that are <= 2^n. 
(define (A095902 n) (A283480 (A000079 n)))

;; A283677 [Altug Alkan] o=1: a(n) = lcm(b(b(n)), b(n-b(n)+1)) where b(n) = A004001(n).

(define (A283677 n) (lcm (A004001 (A004001 n)) (A004001 (+ 1 (- n (A004001 n))))))
(define (A283677v2 n) (lcm (A004001 (A004001 n)) (A004001 (A080677 n))))


;; (same-intfuns1? A283677 (lambda (n) (lcm (A004001 (A004001 n)) (A004001 (A080677 n)))) 65537) --> #t

(definec (A004001v2 n) (if (<= n 2) 1 (+ (A004001v2 (A004001v2 (- n 1))) (A004001v2 (A080677 (- n 1))))))

(define (A283468 n) (if (<= n 2) 1 (- (A004001 (A004001 (- n 1))) (A004001 (- n (A004001 (- n 1)))))))

(define (A283469 n) (if (<= n 2) 1 (A003986bi (A004001 (A004001 (- n 1))) (A004001 (- n (A004001 (- n 1)))))))

;; (same-intfuns1? A283469 (lambda (n) (+ (A283470 n) (A283472 n))) 65537) --> #t


(define (A283470 n) (if (<= n 2) 1 (A003987bi (A004001 (A004001 (- n 1))) (A004001 (- n (A004001 (- n 1)))))))

(define (A283470v2 n) (if (<= n 2) 1 (A003987bi (A004001 (A004001 (- n 1))) (A004001 (A080677 (- n 1))))))

(define A283471 (ZERO-POS 1 1 A283470))
(define A283471v1 (ZERO-POS 1 1 A283468))
(define A283471v2 (MATCHING-POS 1 3 (lambda (n) (= (A004001 (A004001 (- n 1))) (A004001 (- n (A004001 (- n 1))))))))
(define A283471v3 (MATCHING-POS 1 3 (lambda (n) (= (A004001 (A004001 (- n 1))) (A004001 (A080677 (- n 1)))))))

(define (A283472 n) (if (<= n 2) 0 (A004198bi (A004001 (A004001 (- n 1))) (A004001 (- n (A004001 (- n 1)))))))

(define A283473 (ZERO-POS 1 1 A283472))

(define A283473v2 (MATCHING-POS 1 1 (lambda (n) (= (A004001 n) (A283470 n)))))

(define A283473v3 (MATCHING-POS 1 1 (lambda (n) (= (A004001 n) (A283469 n)))))

;; A283480 [AK] o=1: Partial sums of A095901
(definec (A283480 n) (if (= 1 n) n (+ (A095901 n) (A283480 (- n 1)))))

(define A283481 (NONZERO-POS 1 1 A095901))
(define A283482 (ZERO-POS 1 1 A095901))

(define (A093879 n) (- (A004001 (+ 1 n)) (A004001 n))) ;; o=1: First differences of A004001. 

(define A188163 (RECORD-POS 1 1 A004001)) ;; o=1: Smallest m such that A004001(m) = n.

(define (A188163v2_check n) (if (= 1 n) n (A088359 (- n 1)))) ;; Cf. Richard Mathar's question Jan 09 2013.


(define (A051135 n) (- (A188163 (+ 1 n)) (A188163 n))) ;; o=1: a(n) = number of times n appears in the Hofstadter-Conway $10000 sequence A004001.

;; Note that A004001(A088359(n))-1 = n for all n:
(define A088359 (ZERO-POS 1 1 (COMPOSE -1+ A051135))) ;; o=1: Numbers which occur only once in A004001.
(define A088359v2 (COMPOSE 1+ (NONZERO-POS 1 1 A093879))) ;; Without 1+ seems to give A087815.

(define A087686 (MATCHING-POS 1 1 (lambda (n) (> (A051135 n) 1)))) ;; o=1: Elements of A004001 that repeat consecutively.

(define (A266188 n) (A004001 (A087686 n)))

(define (A266399 n) (A188163 (A088359 n)))
(define (A266399v2 n) (A088359 (- (A088359 n) 1)))
(define A266399v3 (MATCHING-POS 1 2 (lambda (n) (< (A004001 (- n 1)) (A004001 n) (A004001 (+ n 1))))))

(define (A265900 n) (A265901bi n n))
(define (A265901 n) (A265901bi (A002260 n) (A004736 n)))
(define (A265901bi row col) (if (= 1 col) (A188163 row) (A087686 (+ 1 (A265901bi row (- col 1))))))


(define (A265903 n) (A265903bi (A002260 n) (A004736 n)))
(define (A265903bi row col) (if (= 1 row) (A188163 col) (A087686 (+ 1 (A265903bi (- row 1) col)))))

(define (A265909 n) (A265903bi (+ 1 n) n))

(define (A266109 n) (A087686 (+ 1 (A188163 n))))

(define (A267103 n) (A087686 (+ 1 (A266109 n))))
(define A267103v2 (MATCHING-POS 1 1 (lambda (n) (= 3 (A051135 n)))))

(define (A266109v2 n) (A265901bi n 2))

(define (A006127 n) (+ n (A000079 n))) ;; XFER: Base-2/base2-core.ss ???

(definec (A006127v2_check n) (if (zero? n) 1 (A088359 (A006127v2_check (- n 1)))))

;; XFER: Base-2/base2-core.ss or such?
(definec (A181988 n) (if (even? n) (+ (A003602 n) (A181988 (/ n 2))) (A003602 n))) ;; o=1: If n is odd, a(n) = (n+1)/2; if n is even, a(n) = a(n/2) + A003602(n).

(define (A266348 n) (if (= 1 n) 1 (- (A004001 (+ 1 n)) (A072376 n)))) ;; o=1:

(define (A266349 n) (+ 1 (- (A053644 n) (A004001 (+ 1 n))))) ;; o=1: 
(define (A266349v2 n) (+ 1 (- (A072376 n) (A266348 n)))) ;; o=1:

;; (same-intfuns1? (COMPOSE A266348 1+) (COMPOSE  (lambda (n) (- (A004001 (+ 1 n)) (A000079 (- (A000523 n) 1)))) 1+) 12000) --> #t

(define (A265332 n) (if (= 1 n) 1 (A051135 n))) ;; o=1: Column index of A265901, Row index of A265903.



(definec (A265754 n)
  (cond ((= 1 (A036987 n)) (A070939 n))
        ((> (A265754 (+ 1 n)) 1) (- (A265754 (+ 1 n)) 1))
        (else (A265754 (- (A000079 (A000523 n)) (A266349 n))))
  )
)

(define (A265754v2 n) (- (A265332 (+ 1 n)) (A036987 n))) ;; Kubo-Vakil R-sequence/tree.

(define A087815seems_to_be (ZERO-POS 1 1 (COMPOSE -1+ A265754)))

;; A087815 Terms in A087816 that occur in a run of length more than 1. 
(define A087815seems_to_be_from_2_onward (ZERO-POS 1 1 (COMPOSE -1+ A093879)))


(define (A080677 n) (- (+ 1 n) (A004001 n))) ;; o=1: a(n) = n + 1 - A004001(n). 
(define A080677v2_check (LEFTINV-LEASTMONO 1 1 A087686))

(definec (A162598 n) (if (= 1 (A265332 n)) (A004001 n) (A162598 (- (A080677 n) 1)))) ;; o=1: Ordinal transform of modified A051135.

;; A267096-A267117 are now reserved for your use. 
;; Inverse of A265901:
(define (A267102 n) (let ((col (A265332 n)) (row (A162598 n))) (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2))))

;; Inverse of A265903:
(define (A267104 n) (let ((col (A162598 n)) (row (A265332 n))) (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2))))


(definec (A267111 n)
   (cond ((< n 2) n)
         ((zero? (A093879 (- n 1))) (* 2 (A267111 (+ -1 (A080677 n))))) ;; I.e., if n is in A087686
         (else (+ 1 (* 2 (A267111 (+ -1 (A004001 n))))))
   )
)

(definec (A267111v2 n)
   (cond ((< n 2) n)
         ((zero? (A093879 (- n 1))) (* 2 (A267111v2 (- n (A004001 n)))))
         (else (+ 1 (* 2 (A267111v2 (+ -1 (A004001 n))))))
   )
)


(definec (A267112 n)
   (cond ((< n 2) n)
         ((even? n) (A087686 (+ 1 (A267112 (/ n 2)))))
         (else (A088359 (A267112 (/ (- n 1) 2))))
   )
)

;; A276437-A276448 are now reserved for your use. 

;; Mirror images of A267111 & A267112:

;; (same-intfuns1? A267111 (COMPOSE A054429 A276441) 16384) --> #t
;; (same-intfuns1? A276441 (COMPOSE A054429 A267111) 16384) --> #t
;; (same-intfuns1? A276441 (COMPOSE A233277 A276343) 32767) --> #t
;; (same-intfuns1? A276441 (COMPOSE A233275 A276345) 32767) --> #t
;; (same-intfuns1? A276441 (COMPOSE A006068 A276443) 32767) --> #t

;; Try also something like: A209862(A267111(A209861(n))).
;; and: A209862(A276441(A209861(n))).
;; Note that: A266341(n) = A209862(-1+A004001(1+A209861(n))).


(definec (A276441 n)
   (cond ((< n 2) n)
         ((zero? (A093879 (- n 1))) (+ 1 (* 2 (A276441 (+ -1 (A080677 n)))))) ;; I.e., if n is in A087686
         (else (* 2 (A276441 (+ -1 (A004001 n)))))
   )
)

;; (same-intfuns1? A267112 (COMPOSE A276442 A054429) 16384) --> #t
;; (same-intfuns1? A276442 (COMPOSE A276344 A233278) 32767) --> #t
;; (same-intfuns1? A276442 (COMPOSE A276346 A233276) 32767) --> #t
;; (same-intfuns1? A276442 (COMPOSE A276444 A003188) 32767) --> #t

(definec (A276442 n)
   (cond ((< n 2) n)
         ((even? n) (A088359 (A276442 (/ n 2))))
         (else (A087686 (+ 1 (A276442 (/ (- n 1) 2)))))
   )
)

;; Entangled with odious and evil numbers instead of odd and even:


(definec (A276443 n)
   (cond ((< n 2) n)
         ((zero? (A093879 (- n 1))) (A000069 (+ 1 (A276443 (+ -1 (A080677 n)))))) ;; I.e., if n is in A087686
         (else (A001969 (+ 1 (A276443 (+ -1 (A004001 n))))))
   )
)


(definec (A276444 n)
   (cond ((< n 2) n)
         ((zero? (A010060 n)) (A088359 (A276444 (A245710 n))))
         (else (A087686 (+ 1 (A276444 (- (A115384 n) 1)))))
   )
)


;; In another order:

(definec (A276445 n)
   (cond ((< n 2) n)
         ((zero? (A093879 (- n 1))) (A001969 (+ 1 (A276445 (+ -1 (A080677 n)))))) ;; I.e., if n is in A087686
         (else (A000069 (+ 1 (A276445 (+ -1 (A004001 n))))))
   )
)


(definec (A276446 n)
   (cond ((< n 2) n)
         ((zero? (A010060 n)) (A087686 (+ 1 (A276446 (A245710 n)))))
         (else (A088359 (A276446 (- (A115384 n) 1))))
   )
)

;; Entangled with A005187 and A055938 instead of odd and even numbers:

;; (same-intfuns1? A276343 (COMPOSE A233276 A267111) 8192) --> #t
;; (same-intfuns1? A276343 (COMPOSE A233278 A276441) 32767) --> #t

(definec (A276343 n)
   (cond ((< n 2) n)
         ((zero? (A093879 (- n 1))) (A005187 (+ 1 (A276343 (+ -1 (A080677 n)))))) ;; I.e., if n is in A087686
         (else (A055938 (A276343 (+ -1 (A004001 n)))))
   )
)

;; (same-intfuns1? A276344 (COMPOSE A267112 A233275) 8192) --> #t
;; (same-intfuns1? A276344 (COMPOSE A276442 A233277) 32767) --> #t

(definec (A276344 n)
   (cond ((< n 2) n)
         ((not (zero? (A079559 n))) (A087686 (+ 1 (A276344 (- (A213714 n) 1)))))
         (else (A088359 (A276344 (A234017 n))))
   )
)

;; Compose them in the other order:

;; (same-intfuns1? A276345 (COMPOSE A233276 A276441) 8192) --> #t
;; (same-intfuns1? A276345 (COMPOSE A233278 A267111) 32767) --> #t

(definec (A276345 n)
   (cond ((< n 2) n)
         ((zero? (A093879 (- n 1))) (A055938 (A276345 (+ -1 (A080677 n))))) ;; I.e., if n is in A087686
         (else (A005187 (+ 1 (A276345 (+ -1 (A004001 n))))))
   )
)


;; (same-intfuns1? A276346 (COMPOSE A276442 A233275) 32767) --> #t
;; (same-intfuns1? A276346 (COMPOSE A267112 A233277) 32767) --> #t


(definec (A276346 n)
   (cond ((< n 2) n)
         ((not (zero? (A079559 n))) (A088359 (A276346 (- (A213714 n) 1))))
         (else (A087686 (+ 1 (A276346 (A234017  n)))))
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A266341 n) (+ (- n (A053644 n)) (if (zero? (A063250 n)) 0 (A000079 (- (A063250 n) 1))))) ;; o=0: Cf. p. 240 in Kubo & Vakil paper.


(definec (A267108 n) (cond ((<= n 1) n) ((= 1 (A265332 n)) (+ 1 (A267108 (- (A004001 n) 1)))) (else (A267108 (- n (A004001 n))))))

(definec (A267109 n) (cond ((<= n 1) 0) ((= 1 (A265332 n)) (A267109 (- (A004001 n) 1))) (else (+ 1 (A267109 (- n (A004001 n)))))))

(define (A267110 n) (if (= 1 (A051135 n)) (- (A004001 n) 1) (- n (A004001 n))))
(define (A267110v2 n) (if (= 1 (A265332 n)) (- (A004001 n) 1) (- n (A004001 n))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; XFER: Sieves/Sieve-Lucky.scm
;;

(define A266420 (MATCHING-POS 1 1 (lambda (n) (and (odd? n) (zero? (A145649 n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; XFER: Sieves/Sieve-Ludic.scm
;;

(define A266419 (MATCHING-POS 1 1 (lambda (n) (and (odd? n) (zero? (A192490 n)))))) ;; o=1: Odd nonludic numbers.

(define (A266409 n) (/ (- (A003309 (+ 2 n)) 1) 2))

(define A266409v2 (NONZERO-POS 1 1 (lambda (n) (A192490 (+ n n 1))))) ;; o=1: Numbers n such that 2n+1 is a Ludic number (in A003309).


(define A266410 (ZERO-POS 1 1 (lambda (n) (A192490 (+ n n 1))))) ;; o=1: Numbers n such that 2n+1 is a nonludic number (in A192607).

(define (A266410v2 n) (/ (- (A266419 n) 1) 2))

(definec (A266350 n) (if (<= n 1) n (+ (A266350 (- n 1)) (A192490 (+ n n 1))))) ;; o=1: (Cf. A123579)

(define A266350v2 (LEFTINV-LEASTMONO 1 1 A266409))

;; (same-intfuns1? A000027 (COMPOSE A266350 A266409) 1200) --> #t

(definec (A266417 n)
   (cond ((<= n 1) n)
         ((zero? (A192490 (+ n n 1))) (* 2 (A266417 (- n (A266350 n)))))
         (else (+ 1 (* 2 (A266417 (+ -1 (A266350 n))))))
   )
)


(definec (A266418 n)
   (cond ((<= n 1) n)
         ((even? n) (A266410 (A266418 (/ n 2))))
         (else (A266409 (+ 1 (A266418 (/ (- n 1) 2)))))
   )
)

;; (same-intfuns1? A000027 (COMPOSE A266417 A266418) 128) --> #t
;; (same-intfuns1? A000027 (COMPOSE A266418 A266417) 128) --> #t

;; (same-intfuns1? A266417 (COMPOSE A237427 A266637) 128) --> #t

;; (same-intfuns1? A266418 (COMPOSE A266638 A237126) 128) --> #t


(definec (A266637 n)
   (cond ((<= n 1) n)
         ((zero? (A192490 (+ n n 1))) (A192607 (A266637 (- n (A266350 n)))))
         (else (A003309 (+ 1 (A266637 (+ -1 (A266350 n))))))
   )
)

;; (same-intfuns1? A266637 (COMPOSE A237126 A266417) 128) --> #t

(definec (A266638 n)
   (cond ((<= n 1) n)
         ((= 1 (A192490 n)) (A266409 (+ 1 (A266638 (- (A192512 n) 1)))))
         (else (A266410 (A266638 (A236863 n))))
   )
)

;; (same-intfuns1? A266638 (COMPOSE A266418 A237427) 128) --> #t

;; (same-intfuns1? A000027 (COMPOSE A266637 A266638) 120) --> #t
;; (same-intfuns1? A000027 (COMPOSE A266638 A266637) 120) --> #t

;; XFER: Bagula/Bagula-editions.ss

(definec (A142881 n)
  (cond ((<= n 1) n)
        ((= 0 (modulo n 3)) (- (* 2 (A142881 (- n 1))) (A142881 (- n 2))))
        ((= 1 (modulo n 3)) (+ (A142881 (- n 1)) (A142881 (- n 2))))
        (else (+ (* 2 (A142881 (- n 1))) (A142881 (- n 2))))
  )
)

(definec (A140948 n) (cond ((zero? n) 3) ((even? (A140948 (- n 1))) (/ (A140948 (- n 1)) 2)) (else (+ 1 (* (A065091 n) (A140948 (- n 1)))))))


;;;;;;

;; XFER: Base-2/Base-2.patterns.ss (together with the complement A003714): ?
(define A004780 (NONZERO-POS 1 0 (lambda (n) (- (A163617 n) (A048724 n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Already copied to ~/Appdata/Roaming/Racket/6.2.1/Collects/IntSeq/Seqs/Sieves/sieve-lucky.ss
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (A269369 n) (if (= 1 n) n (A255551bi (+ (A260438 n) 1) (A260439 n))))

(definec (A269370 n) (cond ((= 1 n) n) ((even? n) (A269370 (/ n 2))) (else (A255551bi (- (A260438 n) 1) (A260439 n)))))

(define (A269372 n) (- (A269369 (+ 1 n)) 1))

(definec (A269373 n)
   (cond ((<= n 1) n)
         (else (* (A000079 (- (A260438 (+ 1 n)) 1)) (+ -1 (* 2 (A269373 (A260439 (+ 1 n)))))))
   )
)

(definec (A269374 n)
   (cond ((<= n 1) n)
         ((even? n) (A269372 (A269374 (/ n 2))))
         (else (+ -1 (* 2 (A269374 (/ (+ n 1) 2)))))
   )
)

(definec (A269374v2 n)
   (cond ((<= n 1) n)
         (else (+ -1 (A255551bi (A001511 n) (A269374v2 (A003602 n)))))
   )
)



(definec (A269375 n)
  (cond ((<= n 1) (+ n 1))
        ((even? n) (* 2 (A269375 (/ n 2))))
        (else (A269369 (A269375 (/ (- n 1) 2))))
  )
)

(definec (A269376 n)
  (cond ((<= n 2) (- n 1))
        ((even? n) (* 2 (A269376 (/ n 2))))
        (else (+ 1 (* 2 (A269376 (A269370 n)))))
  )
)



(definec (A269377 n)
   (cond ((<= n 2) (+ 1 n))
         ((even? n) (A269369 (A269377 (/ n 2))))
         (else (* 2 (A269377 (/ (- n 1) 2))))
   )
)

(definec (A269378 n)
   (cond ((= 1 n) (- n 1))
         ((even? n) (+ 1 (* 2 (A269378 (/ n 2)))))
         (else (* 2 (A269378 (A269370 n))))
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Already copied to ~/Appdata/Roaming/Racket/6.2.1/Collects/IntSeq/Seqs/Sieves/sieve-ludic.ss
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (A269379 n) (if (= 1 n) n (A255127bi (+ (A260738 n) 1) (A260739 n))))

(definec (A269380 n) (cond ((= 1 n) n) ((even? n) (A269380 (/ n 2))) (else (A255127bi (- (A260738 n) 1) (A260739 n)))))

(define (A269382 n) (- (A269379 (+ 1 n)) 1))

(definec (A269383 n)
   (cond ((<= n 1) n)
         (else (* (A000079 (- (A260738 (+ 1 n)) 1)) (+ -1 (* 2 (A269383 (A260739 (+ 1 n)))))))
   )
)

(definec (A269384 n)
   (cond ((<= n 1) n)
         ((even? n) (A269382 (A269384 (/ n 2))))
         (else (+ -1 (* 2 (A269384 (/ (+ n 1) 2)))))
   )
)

(definec (A269384v2 n)
   (cond ((<= n 1) n)
         (else (+ -1 (A255127bi (A001511 n) (A269384v2 (A003602 n)))))
   )
)


(definec (A269385 n)
  (cond ((<= n 1) (+ n 1))
        ((even? n) (* 2 (A269385 (/ n 2))))
        (else (A269379 (A269385 (/ (- n 1) 2))))
  )
)

(definec (A269386 n)
  (cond ((<= n 2) (- n 1))
        ((even? n) (* 2 (A269386 (/ n 2))))
        (else (+ 1 (* 2 (A269386 (A269380 n)))))
  )
)



(definec (A269387 n)
   (cond ((<= n 2) (+ 1 n))
         ((even? n) (A269379 (A269387 (/ n 2))))
         (else (* 2 (A269387 (/ (- n 1) 2))))
   )
)

(definec (A269388 n)
   (cond ((= 1 n) (- n 1))
         ((even? n) (+ 1 (* 2 (A269388 (/ n 2)))))
         (else (* 2 (A269388 (A269380 n))))
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Already copied to ~/Appdata/Roaming/Racket/6.2.1/Collects/IntSeq/Seqs/Sieves/sieves-cross-permutations.ss
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (A269355 n) (A269380 (A250469 n)))

(define (A269356 n) (A268674 (A269379 n)))

(definec (A269357 n) (cond ((<= n 1) n) ((even? n) (* 2 (A269357 (/ n 2)))) (else (A269380 (A250469 n)))))

(definec (A269358 n) (cond ((<= n 1) n) ((even? n) (* 2 (A269358 (/ n 2)))) (else (A268674 (A269379 n)))))

(definec (A269171 n) (cond ((<= n 1) n) ((even? n) (* 2 (A269171 (/ n 2)))) (else (A269379 (A269171 (A268674 n))))))

(definec (A269172 n) (cond ((<= n 1) n) ((even? n) (* 2 (A269172 (/ n 2)))) (else (A250469 (A269172 (A269380 n))))))

(definec (A269171v2 n) (if (<= n 1) n (A255127bi (A055396 n) (A269171v2 (A078898 n)))))

(definec (A269172v2 n) (if (<= n 1) n (A083221bi (A260738 n) (A269172v2 (A260739 n)))))

(define (A269393 n) (/ (A269171 (* 3 n)) 3))

(define (A269394 n) (/ (A269172 (* 3 n)) 3))

(define (A269395 n) (A269171 (A269393 n)))

(define (A269396 n) (A269394 (A269172 n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XFER: ??? Together with A163511 and A071574 ? Primes/Prime.shifts.ss ???

;; Together with A010051 and A000720 to Primes/Primes.core.ss
(define (A026233 n) (if (= 1 (A010051 n)) (A000720 n) (- n (A000720 n)))) ;; o=1: a(n) = j if n is the j-th prime, else a(n) = k if n is the k-th nonprime. 


(definec (A269847 n) ;; o=1: Also a(n) = A163511(A071574(n)).
  (cond ((<= n 1) n)
        ((and (odd? n) (= 1 (A010051 n))) (A003961 (A269847 (A000720 n))))
        (else (* 2 (A269847 (- n (A000720 n)))))
  )
)

(definec (A269847v2 n) ;; o=1: Also a(n) = A163511(A071574(n)).
  (cond ((<= n 1) n)
        ((and (odd? n) (= 1 (A010051 n))) (A003961 (A269847v2 (A026233 n))))
        (else (* 2 (A269847v2 (A026233 n))))
  )
)

(definec (A269848 n) ;; o=1: Also a(n) = A237739(A243071(n)).
  (cond ((<= n 2) n)
        ((even? n) (A002808 (+ -1 (A269848 (/ n 2)))))
        (else (A000040 (A269848 (A064989 n))))
  )
)

(definec (A269848v2 n) ;; o=1: Also a(n) = A237739(A243071(n)).
  (cond ((<= n 1) n)
        ((even? n) (A065090 (+ 1 (A269848v2 (/ n 2)))))
        (else (A000040 (A269848v2 (A064989 n))))
  )
)


(definec (A269857 n) ;; o=1: Also a(n) = A252755(A071574(n)).
  (cond ((<= n 1) n)
        ((and (odd? n) (= 1 (A010051 n))) (A250469 (A269857 (A026233 n))))
        (else (A005843 (A269857 (A026233 n))))
  )
)

(definec (A269858 n) ;; o=1: Also a(n) = A237739(A252756(n)).
  (cond ((<= n 1) n)
        ((even? n) (A065090 (+ 1 (A269858 (/ n 2)))))
        (else (A000040 (A269858 (A268674 n))))
  )
)


;; XXX - Submit:

(definec (A269855 n) ;; o=1: Also a(n) = A252755(A006068(n)).
   (cond ((<= n 1) (+ n 1))
         ((= 1 (A010060 n)) (A250469 (A269855 (- (A115384 n) 1))))
         (else (* 2 (A269855 (A245710 n))))
   )
)

(definec (A269856 n) ;; o=1: Also a(n) = A003188(A252756(n)).
   (cond ((<= n 2) (- n 1))
         ((even? n) (A001969 (+ 1 (A269856 (/ n 2)))))
         (else (A000069 (+ 1 (A269856 (A268674 n)))))
   )
)

;; (same-intfuns0? A269855 (COMPOSE A252755 A006068) 2048) --> #t

;; (same-intfuns1? A269856 (COMPOSE A003188 A252756) 255) -> #t



(definec (A269851 n)
   (cond ((<= n 1) (+ 1 n))
         ((zero? (A093879 (- n 1))) (* 2 (A269851 (- n (A004001 n)))))
         (else (A250469 (A269851 (+ -1 (A004001 n)))))
   )
)

;; (same-intfuns0? A269851 (COMPOSE A252755 A267111) 2048) --> #t
;; (same-intfuns1? A269852 (COMPOSE A267112 A252756) 58) --> #t

(definec (A269852 n)
   (cond ((= n 1) 0)
         ((even? n) (A087686 (+ 1 (A269852 (/ n 2)))))
         (else (A088359 (A269852 (A268674 n))))
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XFER: Sieves/sieve-eratosthenes-permutations.ss

(define (A269360 n) (+ 1 (A250469 n)))

(definec (A269359 n) ;; A269360 -> A250469, A250469 -> A269360.
  (cond ((= 1 n) 1)
        ((even? n) (A250469 (+ 1 (A269359 (A268674 (- n 1))))))
;;      (else (A269360 (A269359 (+ -1 (A268674 n)))))
        (else (+ 1 (A250469 (A269359 (+ -1 (A268674 n))))))
  )
)

;; a(1) = 1, a(2n) = 2 * a(A268674(2n-1)), a(2n-1) = 1 + (2 * a(A268674(2n-1)-1)).
;; Cf. A245605.
(definec (A269863 n) ;; A269360 -> evens, A250469 -> odds.
   (cond ((= 1 n) 1)
         ((even? n) (* 2 (A269863 (A268674 (- n 1)))))
         (else (+ 1 (* 2 (A269863 (+ -1 (A268674 n))))))
   )
)


;; Cf. A245605.
(definec (A269864 n) ;; odds -> A250469.1+, evens -> 1+.A250469 
   (cond ((= 1 n) 1)
         ((even? n) (+ 1 (A250469 (A269864 (/ n 2)))))
         (else (A250469 (+ 1 (A269864 (/ (- n 1) 2)))))
   )
)


(definec (A269865 n) ;; Cf. A246375
  (cond ((<= n 1) n)
        ((even? n) (* 2 (A269865 (/ n 2))))
        (else (A250469 (+ 1 (A269865 (/ (- n 1) 2)))))
  )
)


(definec (A269866 n) ;; Cf. A246376
  (cond ((<= n 1) n)
        ((even? n) (* 2 (A269866 (/ n 2))))
        (else (+ 1 (* 2 (A269866 (- (A268674 n) 1)))))
  )
)

;; Entangle (even, A250469.1+) x (A250469.1+, even):

(definec (A269867 n)
  (cond ((<= n 1) n)
        ((even? n) (A250469 (+ 1 (A269867 (/ n 2)))))
        (else (* 2 (A269867 (- (A268674 n) 1))))
  )
)

;; A270189-A270200 are now reserved for your use. 
;; A270201-A270203 are now reserved for your use.

;; Analogues for Lucky sieve: (Should be XFER:red to Sieves/sieve-lucky-permutations.ss )

(definec (A270195 n) ;; Cf. A269865
  (cond ((<= n 1) n)
        ((even? n) (* 2 (A270195 (/ n 2))))
        (else (A269369 (+ 1 (A270195 (/ (- n 1) 2)))))
  )
)


(definec (A270196 n) ;; Cf. A269866
  (cond ((<= n 1) n)
        ((even? n) (* 2 (A270196 (/ n 2))))
        (else (+ 1 (* 2 (A270196 (- (A269370 n) 1)))))
  )
)

;; Entangle (even, A269369.1+) x (A269369.1+, even):

(definec (A270197 n) ;; Cf. A269867
  (cond ((<= n 1) n)
        ((even? n) (A269369 (+ 1 (A270197 (/ n 2)))))
        (else (* 2 (A270197 (- (A269370 n) 1))))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XFER: Primes/prime-gaps.ss etc.
;;


(define (A137264 n) (modulo (A001223 n) 3)) ;; [Noel H. Patson] o=1: Prime number gaps read modulo 3.

(define A270189 (NONZERO-POS 1 1 A137264))

(define A270190 (ZERO-POS 1 1 A137264))

(define A270191 (MATCHING-POS 1 1 (lambda (n) (= 1 (A137264 n)))))

(define A270192 (MATCHING-POS 1 1 (lambda (n) (= 2 (A137264 n)))))

(define A269849 (LEFTINV-LEASTMONO 1 1 A270189))

(define A269850 (LEFTINV-LEASTMONO 1 1 A270190))

(define (A269364 n) (- (A269849 n) (A269850 n)))


(define (A269389 n) (- (A270189 (+ n 6)) 6))

(define (A269399 n) (- (A270190 n) 6))


(define (A269362 n) (- (A269849 (+ n 6)) 6))

(define A269389v2 (MATCHING-POS 1 1 (lambda (n) (< 0 (A137264 (+ 6 n))))))

(define (A269399 n) (- (A270190 n) 6))

(define A269399v2 (MATCHING-POS 1 1 (lambda (n) (= 0 (A137264 (+ 6 n))))))

(define A269362v2 (LEFTINV-LEASTMONO 1 1 A269389))


(definec (A270193 n)
   (cond ((= 1 n) n)
         ((not (zero? (A137264 (+ 6 n)))) (* 2 (A270193 (+ -1 (A269362 n)))))
         (else (+ 1 (* 2 (A270193 (- n (A269362 n))))))
   )
)

(definec (A270194 n)
   (cond ((= 1 n) n)
         ((even? n) (A269389 (+ 1 (A270194 (/ n 2)))))
         (else (A269399 (A270194 (/ (- n 1) 2))))
   )
)


(definec (A270199 n)
   (cond ((= 1 n) n)
         ((not (zero? (A137264 (+ 6 n)))) (A269399 (A270199 (+ -1 (A269362 n)))))
         (else (A269389 (+ 1 (A270199 (- n (A269362 n))))))
   )
)


(definec (A270201 n)
   (cond ((= 1 n) n)
         ((not (zero? (A137264 n))) (* 2 (A270201 (+ -1 (A269849 n)))))
         (else (+ 1 (* 2 (A270201 (A269850 n)))))
   )
)

(definec (A270202 n)
   (cond ((= 1 n) n)
         ((even? n) (A270189 (+ 1 (A270202 (/ n 2)))))
         (else (A270190 (A270202 (/ (- n 1) 2))))
   )
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XXX - XFER INVERSE-BINOMIAL-TRANSFORM and BINOMIAL-TRANSFORM to transforms.ss:


(define (INVERSE-BINOMIAL-TRANSFORM Afun)
  (lambda (n) (add (lambda (k) (* (expt -1 (- n k)) (C n k) (Afun k))) 0 n))
)

(define (BINOMIAL-TRANSFORM Afun)
  (lambda (n) (add (lambda (k) (* (C n k) (Afun k))) 0 n))
)

;; A121572 [Franklin T. Adams-Watters] o=0: Subprimorials: inverse binomial transform of primorials (A002110). 
(define A121572 (INVERSE-BINOMIAL-TRANSFORM (lambda (n) (A002110 n))))

;; A121497 [T. D. Noe] binomial transform of the characteristic function of the prime numbers (A010051). 
(define A121497 (BINOMIAL-TRANSFORM (lambda (n) (A010051 n))))

(define A010051test (INVERSE-BINOMIAL-TRANSFORM A121497))


;;;;;;;;;;;;;;

;; A118777 [Zak Seidov] o=0: a(0) = 1; for n > 1: a(n) = a(n-1) + d, d = -/+1 if n is prime/nonprime
(definec (A118777 n) (if (zero? n) 1 (+ (A118777 (- n 1)) (expt -1 (A010051 n)))))

(define (A118777v2 n) (+ 1 (- n (* 2 (A000720 n))))) ;; After Zeidov's formula.

;; A257993 [Emeric Deutsch] o=0: Least gap in the partition having Heinz number n. 

(define (A257993 n) (let loop ((n n) (i 1)) (let* ((p (A000040 i)) (d (modulo n p))) (if (not (zero? d)) i (loop (/ (- n d) p) (+ 1 i))))))

(define (A276084 n) (let loop ((n n) (i 1)) (let* ((p (A000040 i)) (d (modulo n p))) (if (not (zero? d)) (- i 1) (loop (/ (- n d) p) (+ 1 i))))))

(define (A276088 n) (if (zero? n) n (let loop ((n n) (i 1)) (let* ((p (A000040 i)) (d (modulo n p))) (if (not (zero? d)) d (loop (/ (- n d) p) (+ 1 i)))))))

(define (A276088v2 n) (if (zero? n) n (/ (A276094 n) (A002110 (A276084 n)))))

;; A053589 [Frederick Magata] o=1: Greatest primorial number (A002110) which divides n.
(define (A053589 n) (A002110 (A276084 n)))

(define (A276151 n) (- n (A053589 n)))

;; Note: (same-intfuns1? A276151 (COMPOSE A276085 A032742 A276086) 30030) --> #t
;; XXX - We could conjugate many other multiplicative operations (A052126, A007947, A007913, lcm, gcd, etc) in primorial base.

(define (A276152 n) (* (A053669 n) (A053589 n)))
(define (A276152v2 n) (A002110 (A257993 n)))

(definec (A276154 n) (if (zero? n) n (+ (A276152 n) (A276154 (A276151 n)))))


(define A276155 (COMPLEMENT 1 A276154))

;; Numbers obtained by reinterpreting base-2 representation of n in primorial base: a(0) = 0, a(2n) = A276154(a(n)), a(2n+1) = 1+A276154(a(n)).
;; Cf. A059590.

(define (A276156 n)
   (let loop ((n n) (s 0) (pr 1) (i 1))
      (cond ((zero? n) s)
            ((even? n) (loop (/ n 2) s (* (A000040 i) pr) (+ 1 i)))
            (else (loop (/ (- n 1) 2) (+ s pr) (* (A000040 i) pr) (+ 1 i)))
      )
   )
)

(definec (A276156v1 n) ;; Recursive version.
  (cond ((zero? n) n)
        ((even? n) (A276154 (A276156v1 (/ n 2))))
        (else (+ 1 (A276154 (A276156v1 (/ (- n 1) 2)))))
  )
)


;; A260188 [Jean-Marc Rebert] o=1: Greatest primorial less than or equal to n. 
(define (A260188 n) (if (zero? n) n (let loop ((i 0)) (if (> (A002110 i) n) (A002110 (- i 1)) (loop (+ 1 i))))))

(define (A276157 n) (/ (A260188 n) (A053589 n)))


;; A111701 [Amarnath Murthy] o=1: Least integer obtained when n is divided by prime(1), then by prime(2), then by prime(3),..., stopping as soon as one of the primes does not divide it. In particular, a(2n-1) = 2n-1.
(define (A111701 n) (/ n (A053589 n)))

(definec (A007814yet_another-CHECK n) (if (odd? n) 0 (+ 1 (A007814yet_another-CHECK (A111701 n)))))


(definec (A276147 n) (if (odd? n) n (* (A053669 n) (A276147 (A111701 n)))))
(define (A276148 n) (A276147 (* 2 n)))




(define (A276094 n) (if (zero? n) n (let loop ((n n) (i 1) (pr 1)) (let* ((p (A000040 i)) (d (modulo n p))) (if (not (zero? d)) (* d pr) (loop (/ (- n d) p) (+ 1 i) (* pr p)))))))

(define (A276094v2 n) (if (zero? n) n (modulo n (A002110 (A257993 n)))))

(define (A276094v3 n) (if (zero? n) n (* (A276088 n) (A002110 (A276084 n)))))

(define (A276093 n) (- n (A276094 n)))

(definec (A276086v1 n) (if (zero? n) 1 (* (A053669 n) (A276086v1 (- n (A002110 (A276084 n)))))))
(definec (A276086v11 n) (if (zero? n) 1 (* (A053669 n) (A276086v11 (A276151 n)))))

(definec (A276086v2 n) (if (zero? n) 1 (* (expt (A053669 n) (A276088 n)) (A276086v2 (A276093 n)))))


(definec (A276150 n) (if (zero? n) 0 (+ 1 (A276150 (- n (A002110 (A276084 n)))))))
(definec (A276150v1 n) (if (zero? n) 0 (+ 1 (A276150v1 (A276151 n)))))
(definec (A276150v3 n) (if (zero? n) 0 (+ 1 (A276150v3 (- n (A260188 n))))))

(define (A276150v2 n) (A001222 (A276086 n)))


(definec (A276092 n)
  (let outloop ((i n) (t 1))
       (if (zero? i)
           t
           (let ((p (A000040 i)))
             (let inloop ((j (- p 1)) (t t))
                  (if (zero? j) (outloop (- i 1) t) (inloop (- j 1) (* t p)))
             )
           )
       )
  )
)

;; As a recurrence:
(definec (A276092v2 n) (if (zero? n) 1 (* (A276092v2 (- n 1)) (expt (A000040 n) (- (A000040 n) 1)))))



;; XFER: Misc, Anthony Hernandez:

(define A276305 (MATCHING-POS 1 1 (lambda (n) (and (= 1 (A010051 n)) (= 12 (A000005 (* n (+ n n 1))))))))





;; Random edit:

;; A276418 [Bhushan Bade] o=1: Sums of 25 consecutive primes. 

(definec (A276418 n) (if (= 1 n) (add A000040 1 25) (+ (A276418 (- n 1)) (- (A000040 (- n 1))) (A000040 (+ 24 n)))))



(define (A030301 n) (A000035 (A000523 n)))

;; A275973 [Stanislav Sykora] o=1: A binary sequence due to Sir Harold Jeffreys. 

;; a(n)={my(p=1, np=n-1); while(np, p++; np=np\2); return(bitand(p, 1)); }


(define (A275973 n) (if (<= n 2) (A000035 n) (A030301 (- n 1))))

(define (A275973_with_loop n) (let loop ((p 1) (np (- n 1))) (if (zero? np) (A000035 p) (loop (+ 1 p) (/ (- np (A000035 np)) 2)))))


;; XFER: Beanstalks
;; A276568-A276589 are now reserved for your use. 

;; With minimal number of squares, see: A002828 & A255131:

;; o=0: After zero, each n occurs A260734(n) times.
(define (A276571 n) (let loop ((k 0)) (if (>= (A260733 (+ 1 k)) n) k (loop (+ 1 k)))))

;; (define (A276572apu n) (- (A260733 (+ 1 (A276571 n))) n))

;; Simple self-inverse permutation of natural numbers: after a(0)=0, list each block of A260734(n) numbers in reverse order, from A260732(n) to A260733(1+n):
(define (A276572 n) (if (zero? n) n (+ (- (A260733 (+ 1 (A276571 n))) n) (A260732 (A276571 n)))))

;; The infinite trunk of minimal squares beanstalk: The only infinite sequence such that a(n-1) = a(n) - minimal number of squares (A002828) needed to sum to a(n).
(define (A276573 n) (A276574 (A276572 n)))


;; The infinite trunk of minimal squares beanstalk with reversed subsections.
(definec (A276574 n) (cond ((zero? n) n) ((= 1 n) 3) (else (let ((maybe_next (A255131 (A276574 (- n 1))))) (if (zero? (A010052 (+ 1 maybe_next))) maybe_next (+ -1 (A000290 (+ 2 (A000196 (+ 1 maybe_next))))))))))

(define (A276575 n) (A002828 (A276573 n)))
(define (A276575v2 n) (if (zero? n) n (- (A276573 n) (A276573 (- n 1)))))

(define (A278497 n) (A046523 (A276573 n)))

(define (A278498 n) (A065338 (A276573 n)))

(define (A278499 n) (modulo (A276573 n) 4))

(define A277887 (MATCHING-POS 1 1 (lambda (n) (= 1 (A010051 (A276573 n)))))) ;; o=1: Positions of primes in A276573.

(define (A277888 n) (A276573 (A277887 n))) ;; o=1: Primes in A276573.

(define A278485 (MATCHING-POS 1 1 (lambda (n) (= 1 (A010051 (+ -1 (A276573 n)))))))
(define (A278486 n) (A276573 (A278485 n)))
(define (A278487 n) (+ -1 (A276573 (A278485 n))))


;; 

(definec (A277890 n)
    (let ((org_k (- (A000290 (+ 1 n)) 1)))
       (let loop ((k org_k) (s 0))
            (if (and (< k org_k) (= 1 (A010052 (+ 1 k))))
                s
                (loop (- k (A002828 k)) (+ s (- 1 (A000035 k))))
            )
       )
    )
)

(definec (A277891 n)
    (let ((org_k (- (A000290 (+ 1 n)) 1)))
       (let loop ((k org_k) (s 0))
            (if (and (< k org_k) (= 1 (A010052 (+ 1 k))))
                s
                (loop (- k (A002828 k)) (+ s (A000035 k)))
            )
        )
    )
)

;; (same-intfuns1? A260734 (COMPOSE (lambda (n) (+ (A277891 n) (A277890 n))) 1+) 120) --> #t


;; A277486-A277488 are now reserved for your use. 

(definec (A277486 n)
     (let ((org_k (- (A000290 (+ 1 n)) 1)))
        (let loop ((k org_k) (s 0))
             (if (and (< k org_k) (= 1 (A010052 (+ 1 k))))
                 s
                 (loop (- k (A002828 k)) (+ s (A010051 (+ -1 k))))
             )
        )
     )
)


(definec (A277487 n)
    (let ((org_k (- (A000290 (+ 1 n)) 1)))
       (let loop ((k org_k) (s 0))
            (if (and (< k org_k) (= 1 (A010052 (+ 1 k))))
                s
                (loop (- k (A002828 k)) (+ s (A010051 k)))
            )
       )
    )
)

(definec (A277488 n)
    (let ((org_k (- (A000290 (+ 1 n)) 1)))
       (let loop ((k org_k) (s 0))
            (if (and (< k org_k) (= 1 (A010052 (+ 1 k))))
                s
                (loop (- k (A002828 k)) (+ s (A010051 (+ 1 k))))
            )
       )
    )
)

;; A278158-A278169 are now reserved for your use. 

;; Partial sums, seem to have a consistent (?) bias:
(definec (A278166 n) (if (= 1 n) (A277486 n) (+ (A277486 n) (A278166 (- n 1)))))
(definec (A278167 n) (if (= 1 n) (A277487 n) (+ (A277487 n) (A278167 (- n 1)))))
(definec (A278168 n) (if (= 1 n) (A277488 n) (+ (A277488 n) (A278168 (- n 1)))))


;; (reverse (map (lambda (n) ((COMPOSE-FUN-TO-NTH-POWER A255131 n) 100)) (iota 38)))
;; (0 3 6 8 11 15 16 18 21 24 27 30 32 35 38 40 43 45 48 51 53 56 59 63 64 67 70 72 75 78 80 83 85 88 90 93 96 99)

;; (equal? (map A276573 (iota0 1624)) (reverse (map (lambda (n) ((COMPOSE-FUN-TO-NTH-POWER A255131v2 n) 4624)) (iota 1625)))) -->  #t

(define A277014 (NONZERO-POS 0 0 (COMPOSE A010052 A276573)))
(define A277014v2 (ZERO-POS 1 1 (COMPOSE -1+ A276575)))


(define (A277015 n) (A000196 (A277016 n)))
(define (A277016 n) (A276573 (A277014 n)))

(define (A277023 n) (A276573 (A260732 n)))

(define (A277024 n) (- (A277023 n) (A000290 n)))

(define A277025 (ZERO-POS 0 0 (lambda (n) (A277024 (* 4 n)))))

(define A277026 (NONZERO-POS 1 1 (lambda (n) (A277024 (* 4 n)))))

;; A077773: [T. D. Noe] o=1 (should be prepended with a(0)=0 ?): Number of integers between n^2 and (n+1)^2 that are the sum of two squares; multiple representations are counted once. 
(define (A077773 n) (add (lambda (i) (* (- 1 (A010052 i)) (A229062 i))) (A000290 n) (+ -1 (A000290 (+ 1 n)))))

(define (A277193 n) (add (lambda (i) (* (- 1 (A010052 i)) (- 1 (A229062 i)) (- 1 (A072401 i)))) (A000290 n) (+ -1 (A000290 (+ 1 n)))))


(define (A277194 n) (add A072401 (A000290 n) (+ -1 (A000290 (+ 1 n)))))

(define (A277191 n) (add (lambda (i) (A000035 (A002828v2 i))) (A000290 n) (+ -1 (A000290 (+ 1 n)))))
(define (A277191v2 n) (if (zero? n) 0 (+ 1 (A277193 n))))

(define (A277192 n) (add (lambda (i) (- 1 (A000035 (A002828v2 i)))) (A000290 n) (+ -1 (A000290 (+ 1 n)))))
(define (A277192v2 n) (if (zero? n) 1 (+ (A077773 n) (A277194 n))))

;; (same-intfuns0? double (lambda (n) (+ (A077773 n) (A277193 n) (A277194 n))) 120) --> #t

;; (same-intfuns0? A005408 (lambda (n) (+ 1 (A077773 n) (A277193 n) (A277194 n))) 120) --> #t



;;;;;;;;;;;;;;;;;;
;; Interlude, XFER: linear recurrences with constant coefficients, all three:

;; A001541: [NJAS] o=0: a(0) = 1, a(1) = 3; for n > 1, a(n) = 6a(n-1) - a(n-2).
(definec (A001541 n) (cond ((zero? n) 1) ((= 1 n) 3) (else (- (* 6 (A001541 (- n 1))) (A001541 (- n 2))))))

;; A055792: [Henry Bottomley] o=0: a(n) and floor(a(n)/2) are both squares; i.e. squares which remain squares when written in base 2 and last digit is removed.
(define (A055792 n) (if (zero? n) n (A000290 (A001541 (- n 1)))))

;; A132592 [Mohamed Bouhamida] o=0: Sequence allows us to find X values of the equation: X(X + 1) - 8*Y^2 = 0.
(define (A132592 n) (+ -1 (A055792 (+ 1 n))))

;;;;;;;;;;;;;;;;;;;




;;;;;;

;; With greedy summation of squares, see: A053610 & A260740:

;; o=0: After zero, each n occurs A261224(n) times.
(define (A276581 n) (let loop ((k 0)) (if (>= (A261223 (+ 1 k)) n) k (loop (+ 1 k)))))

;; Simple self-inverse permutation of natural numbers: after a(0)=0, list each block of A261224(n) numbers in reverse order, from A261222(n) to A261223(1+n).
(define (A276582 n) (if (zero? n) n (+ (- (A261223 (+ 1 (A276581 n))) n) (A261222 (A276581 n)))))

;; o=0: The infinite trunk of greedy squares beanstalk: The only infinite sequence such that a(n-1) = a(n) - number of squares that sum to a(n) with greedy algorithm (A053610).
(define (A276583 n) (A276584 (A276582 n)))

(definec (A276584 n) (cond ((zero? n) n) ((= 1 n) 3) (else (let ((maybe_next (A260740 (A276584 (- n 1))))) (if (zero? (A010052 (+ 1 maybe_next))) maybe_next (+ -1 (A000290 (+ 2 (A000196 (+ 1 maybe_next))))))))))

(define (A276585 n) (A053610 (A276583 n)))
(define (A276585v2 n) (if (zero? n) n (- (A276583 n) (A276583 (- n 1)))))

;; (equal? (map A276583 (iota0 1009)) (reverse (map (lambda (n) ((COMPOSE-FUN-TO-NTH-POWER A260740 n) 3969)) (iota 1010)))) --> #t

;; (reverse (map (lambda (n) ((COMPOSE-FUN-TO-NTH-POWER A260740 n) 143)) (iota 40)))
;; (0 3 6 8 11 15 18 21 24 27 32 35 38 43 48 51 56 59 63 66 71 74 78 80 83 88 91 95 99 102 107 110 114 117 120 123 128 131 135 138)

;; (map A276583 (iota0 39))
;; (0 3 6 8 11 15 18 21 24 27 32 35 38 43 48 51 56 59 63 66 71 74 78 80 83 88 91 95 99 102 107 110 114 117 120 123 128 131 135 138)

;;;;;;


;;;;;;
;; A276603-A276624 are now reserved for your use. 

(define A048766 (LEFTINV-LEASTMONO-NC2NC 0 0 A000578)) ;; /XFER: core.cubes
(define (A010057 n) (if (= n (A000578 (A048766 n))) 1 0)) ;; /XFER: core.cubes


;; With greedy summation of cubes, see: A055401 & A261225:

;; o=0: After zero, each n occurs A261229(n) times.
(define (A276611 n) (let loop ((k 0)) (if (>= (A261228 (+ 1 k)) n) k (loop (+ 1 k)))))

;; Simple self-inverse permutation of natural numbers: after a(0)=0, list each block of A261229(n) numbers in reverse order, from A261227(n) to A261228(1+n).

(define (A276612 n) (if (zero? n) n (+ (- (A261228 (+ 1 (A276611 n))) n) (A261227 (A276611 n)))))

(define (A276613 n) (A276614 (A276612 n)))

(definec (A276614 n) (cond ((zero? n) n) ((= n 1) 7) (else (let ((maybe_next (A261225 (A276614 (- n 1))))) (if (zero? (A010057 (+ 1 maybe_next))) maybe_next (+ -1 (A000578 (+ 2 (A048766 (+ 1 maybe_next))))))))))

(define (A276615 n) (A055401 (A276613 n)))
(define (A276615v2 n) (if (zero? n) n (- (A276613 n) (A276613 (- n 1)))))

;; (equal? (map A276613 (iota0 1038)) (reverse (map (lambda (n) ((COMPOSE-FUN-TO-NTH-POWER A261225 n) 8000)) (iota 1039)))) --> #t


;; (reverse (map (lambda (n) ((COMPOSE-FUN-TO-NTH-POWER A261225 n) 216)) (iota 34)))
;; (0 7 14 21 26 33 40 47 52 59 63 70 77 84 89 96 103 110 115 124 131 138 145 150 157 164 171 176 183 187 194 201 208 215)

;; (map A276613 (iota0 33))
;; (0 7 14 21 26 33 40 47 52 59 63 70 77 84 89 96 103 110 115 124 131 138 145 150 157 164 171 176 183 187 194 201 208 215)


;;;;;;
;; o=0: After zero, each n occurs A261234(n-1) times.
(define (A276621 n) (let loop ((k 0)) (if (>= (A261233 k) n) k (loop (+ 1 k)))))

;; (map A276621 (iota0 50))
;; (0 1 2 2 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6)

;; Simple self-inverse permutation of natural numbers: after a(0)=0, list each block of A261234(n) numbers in reverse order, from A261232(n) to A261233(1+n).

(define (A276622 n) (if (zero? n) n (+ (A261232 (+ -1 (A276621 n))) (- (A261232 (A276621 n)) n 1))))

;; Formula: a(0) = 0; for n >= 1, a(n) = A261232(A276621(n)-1) + A261232(A276621(n)) - n - 1.


;; (map A276622 (iota0 49))
;; (0 1 3 2 8 7 6 5 4 20 19 18 17 16 15 14 13 12 11 10 9 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21)

;; The infinite trunk of ternary beanstalk: The only infinite sequence such that a(n-1) = a(n) - base-3 digit sum of a(n).
(define (A276623 n) (A276624 (A276622 n)))

(definec (A276624 n) (cond ((zero? n) n) ((= n 1) 2) (else (let ((maybe_next (* 2 (A054861 (A276624 (- n 1)))))) (if (not (= 1 (A053735 (+ 1 maybe_next)))) maybe_next (+ -1 (A000244 (+ 1 (A081604 (+ 1 maybe_next))))))))))

(define (A276603 n) (/ (A276623 n) 2))

(define (A276604 n) (A053735 (A276623 n)))
(define (A276604v2 n) (if (zero? n) n (- (A276623 n) (A276623 (- n 1)))))


(define (A276605 n) (/ (A276604 n) 2))
(define (A276605v2 n) (if (zero? n) n (- (A276603 n) (A276603 (- n 1)))))

;; [0] [1] [3,2] [8,7,6,5,4]        [20,9]
;;  0   2  8,4   26,20,16,12,10

;; (reverse (map (lambda (n) ((COMPOSE-FUN-TO-NTH-POWER (lambda (n) (- n (A053735 n))) n) 243)) (iota 50)))
;; (0 2 4 8 10 12 16 20 26 28 30 34 38 42 46 52 56 62 68 72 80 82 84 88 92 96 100 106 110 116 122 126 134 140 144 152 160 164 170 176 180 188 194 198 204 212 216 224 232 242)

;; (equal? (map A276623 (iota0 49)) (reverse (map (lambda (n) ((COMPOSE-FUN-TO-NTH-POWER (lambda (n) (- n (A053735 n))) n) 243)) (iota 50)))) --> #t

;; (equal? (map A276623 (iota0 6250)) (reverse (map (lambda (n) ((COMPOSE-FUN-TO-NTH-POWER (lambda (n) (- n (A053735 n))) n) 59049)) (iota 6251)))) --> #t


(define (A123098 n) (A019565 (A001317 n)))

;; A066117 o=1: [Henry Bottomley] Triangle read by rows: T(n,k)=T(n-1,k-1)*T(n,k-1) and T(n,1)=p(n).

(define (A066117 n) (A066117bi (A002260 n) (A004736 n)))

;; As a square array:
(define (A066117bi row col)
   (if (= 1 row)
       (A000040 col)
       (* (A066117bi (- row 1) col) (A066117bi (- row 1) (+ col 1)))
   )
)

(define (A066117v2 n) (A066117biv2 (A002260 n) (A004736 n)))

;; As a square array, alternative definition:
(define (A066117biv2 row col)
   (if (= 1 col)
       (A007188 (- row 1))
       (A003961 (A066117biv2 row (- col 1)))
   )
)

(define (A276586 n) (A276586bi (A002262 n) (A025581 n)))
(define (A276586bi row col) (A276085 (A066117bi (+ 1 row) (+ 1 col))))

(define (A276586as_sum n) (A276586bi_as_sum (A002262 n) (A025581 n)))
(define (A276586bi_as_sum row col) (add (lambda (i) (* (A007318tr row i) (A002110 (+ i col)))) 0 row))


(define (A276587 n) (A276586bi (A025581 n) (A002262 n)))

;;;

(define (A276588 n) (A276588bi (A002262 n) (A025581 n)))
(define (A276588bi row col) (A276075 (A066117bi (+ 1 row) (+ 1 col))))

(define (A276588as_sum n) (A276588bi_as_sum (A002262 n) (A025581 n)))
(define (A276588bi_as_sum row col) (add (lambda (i) (* (A007318tr row i) (A000142 (+ 1 i col)))) 0 row))

(define (A276589 n) (A276588bi (A025581 n) (A002262 n)))

;;;

(define (A136104 n) (A276085 (A007188 n)))

(define A136104v2 (BINOMIAL-TRANSFORM A002110))

(define (A136104as_sum n) (add (lambda (i) (* (A007318tr n i) (A002110 i))) 0 n))


(define (A255483 n) (A255483bi (A002262 n) (+ 1 (A025581 n))))
(define (A255483bi row col)
   (if (zero? row)
       (A000040 col)
       (let ((a (A255483bi (- row 1) col)) (b (A255483bi (- row 1) (+ col 1))))
         (/ (lcm a b) (gcd a b))
       )
   )
)


(define (A255483v2 n) (A255483biv2 (A002262 n) (+ 1 (A025581 n))))
(define (A255483biv2 row col) (if (= 1 col) (A123098 row) (A003961 (A255483biv2 row (- col 1)))))

(define (A276578 n) (A255483bi (A025581 n) (+ 1 (A002262 n)))) ;; o=0, tabl, transpose of A255483.

;; (define A276579 (MATCHING-POS 1 1 (lambda (n) (and (> n 1) (= 1 (A008966 n)) (= 1 (A209229 (A268669 (A048675 n))))))))
(define A276579 (MATCHING-POS 1 1 (lambda (n) (and (> n 1) (not (zero? (A008683 n))) (= 1 (A000120 (A268669 (A048675 n))))))))


(define (A051063 n) (* 9 (A001651 n))) ;; o=1: [NJAS, Gary W. Adamson] 27*n+9 or 27*n+18.




(define (A277006 n) (A005940 (+ 1 (A003714 n)))) ;; o=0.

;; (same-intfuns0? A048679 (COMPOSE A048675 A277006) 256) --> #t


(define (A277010 n) (A156552 (A005117 n))) ;; o=1.

(define (A277331 n) (A253563 (A003714 n)))

(define (A277332 n) (A253565 (A003714 n)))

(define (A022340 n) (* 2 (A003714 n)))


;; A091377 [Zumkeller] o=1: Numbers having fewer prime factors than the value of their smallest prime factor.

(define A091377 (MATCHING-POS 1 1 (lambda (n) (< (A001222 n) (A020639 n)))))
(define Amutkut (MATCHING-POS 1 1 (lambda (n) (<= (A001222 n) (A055396 n))))) ;; Differs from A091377 at n=256.

(define A277334 (MATCHING-POS 1 1  (lambda (n) (and (or (= 2 n) (not (even? n))) (not (zero? (A008683 (A052126 n))))))))



(define (A277020v2 n) (A156552 (A260443 n))) ;; o=0.

(define (A277189 n) (A277020 (+ n n 1))) ;; o=0: Odd bisection of A277020.

(define (A277020 n) (list_of_numbers_to_unary_binary_representation (A260443as_coeff_list n)))


(define (list_of_numbers_to_unary_binary_representation nums)
   (let loop ((s 0) (nums nums) (b 1))
      (cond ((null? nums) s)
;;          ((zero? (car nums)) (loop s (cdr nums) (* 2 b))) ;; Unnecessary clause.
            (else (loop (+ s (* (A000225 (car nums)) b)) (cdr nums) (* (A000079 (+ 1 (car nums))) b)))
      )
   )
)


;; (same-intfuns0? A001477 (COMPOSE A087808 A277020) 63) --> #t

(define (A277195 n) (A022290 (A277010 n))) ;; o=1.

(define (A277196 n) (A107079 (A277006 n))) ;; o=0:

;; (same-intfuns1? A001477 (COMPOSE A277196 A277195) 2048) --> #t
;; (same-intfuns0? A001477 (COMPOSE A277195 A277196) 353) --> #t

;; XFER: prime-indices-matula-goebel.ss :

(definec (A061775 n) (cond ((= 1 n) 1) ((= 1 (A010051 n)) (+ 1 (A061775 (A000720 n)))) (else (+ -1 (A061775 (A020639 n)) (A061775 (A032742 n))))))

(definec (A196050 n) (cond ((= 1 n) 0) ((= 1 (A010051 n)) (+ 1 (A196050 (A000720 n)))) (else (+ (A196050 (A020639 n)) (A196050 (A032742 n))))))


(definec (A277697 n)
  (cond ((= 1 n) 0)
        ((= 1 (A067029 n)) (A055396 n))
        (else (A277697 (A028234 n)))
  )
)

(definec (A277707 n)
  (cond ((= 1 n) 0)
        ((odd? (A067029 n)) (A055396 n))
        (else (A277707 (A028234 n)))
  )
)

(define (A277707v2 n) (A055396 (A007913 n)))

(define (A277708 n) (A008578 (+ 1 (A277707 n))))
(define (A277708v2 n) (A020639 (A007913 n)))


(define (A080368 n) (if (zero? (A277697 n)) 0 (A000040 (A277697 n))))
(define (A277698 n) (A008578 (+ 1 (A277697 n))))


;; Related to A115872, A065621, etc:

(define (A277320 n) (A277320bi (A002260 n) (A004736 n)))
(define (A277320bi row col) (A048720bi (A065621 row) col))


(define (A277199 n) (A277320bi (A004736 n) (A002260 n)))

(definec (A277699 n) (A277320bi n n))

(define (A284269 n) (A284270bi (A004736 n) (A002260 n)))

(define (A284270 n) (A284270bi (A002260 n) (A004736 n)))
(define (A284270bi row col) (modulo (A277320bi row col) row))

(define (A284552 n) (modulo (A065621 n) n))
(define (A284552v2 n) (A284270bi n 1))

(define (A284557 n) (modulo (A048727 n) 3))
(define (A284557v2 n) (A284270bi 3 n))


(define A284555 (ZERO-POS 0 0 A284557))

(definec (A284273 n) (A284270bi n n))
(define (A284273v2 n) (modulo (A277699 n) n))


(define (A284574 n) (modulo (A048724 n) 3))

(define (A284575 n) (modulo (A048725 n) 3))


(define A023758 (MATCHING-POS 1 0 (lambda (n) (or (<= n 4) (pow2? (+ 1 (A000265 n)))))))

(define A277704 (NONZERO-POS 1 1 (COMPOSE A010052 A277699)))

(define A277807 (MATCHING-POS 1 1 (lambda (n) (and (not (pow2? (+ 1 (A000265 n)))) (= 1 (A010052 (A277699 n)))))))

(define (A277806 n) (A000196 (A277699 (A277807 n))))


(define A277706 (ZERO-POS 1 1 (COMPOSE A010052 A277699)))

(define (A277805 n) (A277699 (A277706 n)))


(define (A277819 n) (A277820bi (A004736 n) (A002260 n)))
(define (A277820 n) (A277820bi (A002260 n) (A004736 n)))
(define (A277820bi row col) (if (= 1 col) (A065621 row) (A048724 (A277820bi row (- col 1)))))

(define (A277821 n) (let ((row (A268671 n)) (col (A277818 n))) (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2))))


(define (A277823 n) (A048724 (A065621 n)))
(define (A277823v2 n) (A048720bi (A065621 n) 3))
(define (A277823v3 n) (A277320bi n 3))

(define (A277825 n) (A048724 (A277823 n)))
(define (A277825v2 n) (A048720bi (A065621 n) 5))

(define (A277810bi row col) (A019565 (A277820bi row col)))
(define (A277810 n) (A277810bi (A002260 n) (A004736 n)))
(define (A277809 n) (A277810bi (A004736 n) (A002260 n)))

(define (A277811 n) (A019565 (A065621 n))) ;; Column 1 of A277810.


(definec (A277808 n) (if (= 1 (A010060 n)) 0 (+ 1 (A277808 (A003188 (/ (A006068 n) 2))))))
(define (A277808v2 n) (if (= 1 (A010060 n)) 0 (A001511 n)))

(define (A277808v3 n) (* (A010059 n) (A001511 n)))

(definec (A277822 n) (cond ((zero? n) n) ((= 1 (A010060 n)) 1) (else (+ 1 (A277822 (A003188 (/ (A006068 n) 2)))))))

(define (A277822v2 n) (+ (* (A010059 n) (A007814 n)) (A010060 n)))

(definec (A277822v3 n) (cond ((zero? n) n) ((= 1 (A010060 n)) 1) (else (+ 1 (A277822v3 (floor->exact (/ n 2)))))))

(definec (A277812 n) (if (= 1 (A010060 n)) n (A277812 (A003188 (/ (A006068 n) 2)))))
;; (definec (A277812v2 n) (if (= 1 (A010060 n)) n (A277812 (A000265 n))))


(define (A277813 n) (A115384 (A277812 n)))

;; A277880-A277911 are now reserved for your use.

(define (A129771 n) (+ 1 (* 2 (A000069 n))))

(define (A277880 n) (A277880bi (A002260 n) (A004736 n)))
(define (A277880bi row col) (if (= 1 col) (A000069 row) (A001969 (+ 1 (A277880bi row (- col 1))))))

(define (A277880v2 n) (A277880biv2 (A002260 n) (A004736 n)))
(define (A277880biv2 row col) (cond ((= 1 col) (A000069 row)) ((= 2 col) (A129771 row)) (else (* 2 (A277880biv2 row (- col 1))))))

(define (A277881 n) (let ((row (A277813 n)) (col (A277822 n))) (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2))))

(define (A277882 n) (A277880bi (A004736 n) (A002260 n)))
(define (A277883 n) (let ((col (A277813 n)) (row (A277822 n))) (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2))))


(define (A277901v2 n) (A277820 (A277881 n)))

(definec (A277901 n) (if (= 1 (A010060 n)) (A065621 (A115384 n)) (A048724 (A277901 (floor->exact (/ n 2))))))

(define (A277902v2 n) (A277880 (A277821 n)))

(definec (A277902 n) (if (= 1 (A010060 n)) (A000069 (A268671 n)) (A001969 (+ 1 (A277902 (/ (A006068 n) 2))))))


;; XFER: Transfer to triangles-core.ss or such:
(define (A048147 n) (A048147bi (A025581 n) (A002262 n)))
(define (A048147bi row col) (+ (* row row) (* col col)))

(define A277557 (MATCHING-POS 1 1 (lambda (n) (let ((x (A025581 n)) (y (A002262 n))) (and (not (zero? x)) (< x y) (odd? (+ x y)) (= 1 (gcd x y)))))))

;; A008834: [NJAS] o=1:	Largest cube dividing n. 
(definec (A008834 n) (let loop ((k 1) (m 1)) (let ((c (expt k 3))) (cond ((> c n) (expt m 3)) ((zero? (modulo n (expt k 3))) (loop (+ 1 k) k)) (else (loop (+ 1 k) m))))))

(define (A050985 n) (/ n (A008834 n))) ;; o=1: [Eric W. Weisstein] Cubefree part of n. 


;; A048766 [Charles T. Le] o=0: Integer part of cube root of n. Or, number of cubes <= n. Or, n appears 3n^2 + 3n + 1 times. 

(define A048766 (LEFTINV-LEASTMONO-NC2NC 0 0 A000578)) ;; /XFER: core.cubes

;; A277780 o=1 (or 0?): [Peter Kagey] a(n) is the least k > n such that n*k^2 is a cube. 
(define (A277780 n) (if (zero? n) 1 (* (A050985 n) (A000578 (+ 1 (A048766 (A008834 n)))))))


;; A118306 [Leroy Quet] o=1: If n = product(k>=1} p(k)^b(n,k), where p(k) is the k-th prime and where each b(n,k) is a nonnegative integer, then: If n occurs earlier in the sequence, then a(n) = product{k>=2} p(k-1)^b(n,k); If n does not occur earlier in the sequence, then a(n) = product{k>=1} p(k+1)^b(n,k). 

(defineperm1 (A118306p n)
  (cond ((= 1 n) n)
        ((not-lte? (A118306p (- n)) (- n 1)) (A003961 n))
        (else (A064989 n))
  )
)

(define (A118306 n) (cond ((= 1 n) n) ((odd? (A055396 n)) (A003961 n)) (else (A064989 n))))

;; [AK] o=1: Self-inverse permutation of natural numbers induced when A118306 is restricted to A007310:
(define (A277911 n) (A126760 (A118306 (A007310 n))))

(define A277908 (MATCHING-POS 1 1 (lambda (n) (= 1 (abs (- n (A277911 n)))))))

(define A277909 (MATCHING-POS 1 1 (lambda (n) (= (A277911 n) (+ 1 n)))))

(define A277910 (MATCHING-POS 1 1 (lambda (n) (= (A277911 n) (- n 1)))))

(define (A277907 n) (A007310 (A277908 n)))


;;;;;;;;;;;;;;;;;;;;;;





(define (A277892 n) (if (= 1 n) 0 (A001222 (A048675 n))))

;; A277893(n) = the least k > n for which A277892(k) = A277892(n), 0 if no such number exists.
(definec (A277893 n)
  (cond ((= 1 n) 2)
        ((= 2 n) 0)
        (else
           (let ((v (A277892 n)))
             (let loop ((k (+ 1 n)))
                (if (= (A277892 k) v) k (loop (+ 1 k)))
             )
           )
        )
  )
)

;; A277894(n) = the largest k < n for which A277892(k) = A277892(n), 0 if no such number exists.
(definec (A277894 n)
  (cond ((<= n 2) 0)
        (else
           (let ((v (A277892 n)))
             (let loop ((k (- n 1)))
                (cond ((= 1 k) 0)
                      ((= (A277892 k) v) k)
                      (else (loop (- k 1)))
                )
             )
           )
        )
  )
)

;; (same-intfuns1? (COMPOSE 1+ 1+) (COMPOSE A277894 A277893 1+ 1+) 100) --> #t


(definec (A277895 n) (cond ((<= n 2) 0) ((= 1 (A010051 n)) 1) (else (+ 1 (A277895 (A277894 n))))))


(define (A277897 n) (if (< n 3) n (A277898bi (A004736 (- n 2)) (A002260 (- n 2))))) ;; o=3:

(define (A277898 n) (if (< n 3) n (A277898bi (A002260 (- n 2)) (A004736 (- n 2))))) ;; o=3:

(define (A277898bi row col) (if (= 1 col) (A000040 (+ 1 row)) (A277893 (A277898bi row (- col 1)))))

(define (A277900 n) (A277898bi n 2))
(define (A277900v2 n) (A277893 (A065091 n)))


;; A046951 o=1: [Simon Colton] a(n) = number of squares dividing n. 

;; (definec (A046951 n) (cond ((= 1 n) 1) ((even? n) (* (A008619 (A007814 n)) (A046951 (A000265 n)))) (else (A046951 (A064989 n)))))

(definec (A046951 n) (if (= 1 n) 1 (* (A008619 (A007814 n)) (A046951 (A064989 n)))))

(define (A008619 n) (+ 1 (/ (- n (modulo n 2)) 2)))


;; A278159 o=0: [AK] Run length transform of primorials, A002110.

(define (A278159 n) (fold-left (lambda (a r) (* a (A002110 r))) 1 (bisect (reverse (binexp->runcount1list n)) (- 1 (modulo n 2)))))

;; (same-intfuns0? A278159 (COMPOSE A124859 A005940 1+) 1200) --> #t

;; A278161 o=0: [AK] Run length transform of A008619 (floor(n/2)+1). 

(define (A278161 n) (fold-left (lambda (a r) (* a (A008619 r))) 1 (bisect (reverse (binexp->runcount1list n)) (- 1 (modulo n 2)))))

;; (same-intfuns1? A046951 (COMPOSE A278161 A156552) 1200) --> #t
;; (same-intfuns0? A278161 (COMPOSE A046951 A005940 1+) 2048) --> #t

;; A000123 [NJAS] o=0: Number of binary partitions: number of partitions of 2n into powers of 2. 
(define (A000123 n) (A018819 (+ n n)))

;; A131205 [Zumkeller] o=1: a(n) = a(n-1) + a(floor(n/2)) + a(ceiling(n/2)). 
;; Partial sums of A000123: (1, 2, 4, 6, 10, 14, 20, 26,...). - Gary W. Adamson, Oct 26 2007 (but note the offset!)
(definec (A131205 n) (if (= 1 n) 1 (+ (A000123 (- n 1)) (A131205 (- n 1)))))

;; (definec (A278163 n) (let loop ((k 1)) (if (>= (A131205 k) n) (- k 1) (loop (+ 1 k)))))
(definec (A278163 n) (let loop ((k 1)) (if (>= (A131205 k) n) (- k 1) (loop (+ 1 k)))))
(define (A278164 n) (if (= 1 n) 0 (- n (A131205 (A278163 n)) 1)))

;; (same-intfuns1? A000027  (COMPOSE A278163 A131205 1+) 1024) --> #t


;; A018819 [DWW, NJAS, JHC] o=0: Binary partition function: number of partitions of n into powers of 2.
(definec (A018819 n) (cond ((zero? n) 1) ((even? n) (+ (A018819 (- n 1)) (A018819 (/ n 2)))) (else (A018819 (- n 1)))))

(definec (A277903 n) (let loop ((k 0)) (if (>= (A000123 k) n) k (loop (+ 1 k)))))

(define (A277904 n) (if (= 1 n) 0 (- n (A000123 (+ -1 (A277903 n))) 1)))

(definec (A277896 n) (if (= 1 (A209229 n)) 0 (let ((v (A048675 n))) (let loop ((k (+ 1 n))) (if (= (A048675 k) v) k (loop (+ 1 k)))))))


(definec (Anot_submitted n)
   (if (zero? (A278164 n))
       1
       (let ((prev (Anot_submitted (- n 1)))
             (row (A278163 n))
             (col (A278164 n))
            )
         (if (or
                (= (A018819 row) prev)
                (> (A003961 (A277905bi row prev))
                   (* 2 (A277905bi (+ row row -1) (- col prev)))
                )
             )
             prev
             (+ 1 prev)
         )
       )
   )
)


(definec (A277905 n) (A277905bi (A277903 n) (A277904 n)))
(define (A277905bi row col) (let outloop ((k (A019565 row)) (col col)) (if (zero? col) k (let inloop ((j (+ 1 k))) (if (= (A048675 j) row) (outloop j (- col 1)) (inloop (+ 1 j)))))))

(definec (A277905rec n) (if (= 1 n) n (let ((maybe_next (A277896 (A277905rec (- n 1))))) (if (not (zero? maybe_next)) maybe_next (A019565 (A277903 n))))))


(definec (A277905fast n)
   (cond ((= 1 n) n)
         ((odd? (A277903 n)) (* 2 (A277905fast (- n (A018819 (A277903 n))))))
         (else (A277905 n))
   )
)

;; A276528 [AK] o=1: Least number with same prime-signature as sigma(n), the sum of the divisors of n: a(n) = A046523(A000203(n)). 
(define (A276528 n) (A046523 (A000203 n)))

(define (A277906 n) (A046523 (A000010 n)))

;; A278227 [AK] o=1: Least number with same prime signature as prime(n)-1.
(define (A278227 n) (A046523 (+ -1 (A000040 n))))

;; A278228 [AK] o=1: Least number with same prime signature as prime(n)+1.
(define (A278228 n) (A046523 (+ 1 (A000040 n))))

;; A174801 [Gerasimov] o=1: Prime(n)+1 is prime or semiprime. 
(define A174801 (MATCHING-POS 1 1 (lambda (n) (<= (A278228 n) 6))))


;; A278229 [AK] o=1: Least number with same prime signature as 2*prime(n) - 1.
(define (A278229 n) (A046523 (+ -1 (* 2 (A000040 n)))))

;; A278230 [AK] o=1: Least number with same prime signature as 2*prime(n) + 1.
(define (A278230 n) (A046523 (+ 1 (* 2 (A000040 n)))))

(define (A278218 n) (A046523 (A007318 n))) ;; [AK] o=0: T(n,k) = Least number with same prime signature as binomial(n,k) has.

;; A000292 [NJAS] o=0: Tetrahedral (or triangular pyramidal) numbers: a(n) = C(n+2,3) = n*(n+1)*(n+2)/6. 
(define (A000292 n) (/ (* n (+ 1 n) (+ 2 n)) 6))

(define (A278252 n) (A046523 (A000292 n)))

(define (A278253 n) (A046523 (A000217 n))) ;; o=1:

;; A278257 [AK] o=1: Least number with the prime signature of A005187(n).
(define (A278257 n) (A046523 (A005187 n)))

;; A278258 [AK] o=0: Least number with the prime signature of the n-th Catalan number. 
(define (A278258 n) (A046523 (A000108 n)))

;; A005563 [NJAS] o=0: a(n) = n*(n+2) (or, (n+1)^2 - 1). 
(define (A005563 n) (* n (+ 2 n)))

;; A278160: [AK] o=1: Least number with same prime signature as (n+1)^2 - 1 has.
(define (A278160 n) (A046523 (A005563 n)))

;; A278162: [AK] o=0: Least number with same prime signature as n^2 + 1 has. 
(define (A278162 n) (A046523 (+ 1 (* n n))))

;; A278165 [AK] o=1: Least number with same prime signature as the n-th Jacobsthal number has.
(define (A278165 n) (A046523 (A001045 n)))


(define (A278254 n) (A000290 (A046523 n))) ;; o=1: Least number which has the same prime signature as the n^2. 
(define (A278254v2 n) (A046523 (A000290 n))) ;; o=1: 

(definec (A278240 n) (A046523 (A000225 n)))
(definec (A278242 n) (A046523 (A000051 n)))



         
(definec (A278246 n) (A046523 (A000096 n)))
(definec (A278247 n) (A046523 (A028387 n)))
(definec (A278251 n) (A046523 (A002061 n)))

;; A000326 [NJAS] o=0: Pentagonal numbers: a(n) = n*(3*n-1)/2. 
(define (A000326 n) (* (/ 1 2) n (+ n n n -1)))

(definec (A278255 n) (A046523 (A000326 n)))

;; A001844 [] o=0: Centered square numbers: 2n(n+1)+1. Sums of two consecutive squares. Also, consider all Pythagorean triples (X, Y, Z=Y+1) ordered by increasing Z; then sequence gives Z values.

(define (A001844 n) (+ 1 (* 2 (* n (+ 1 n))))) ;; a(n) = 2*n^2 + 2*n + 1.

;; A278244 [AK] o=0: Least number with the prime signature of the n-th centered square number (A001844(n)).
(define (A278244 n) (A046523 (A001844 n)))

(definec (A278249 n) (A046523 (A000123 n)))

(define (A278250 n) (A046523 (A001190 n)))

(define (A278232 n) (A046523 (A259934 n)))

(define (A278262 n) (A278222 (A179016 n)))


;; A278482-A278544 are now reserved for your use.

;; A278536 [AK] o=1: First differences of A273324.
(define (A278536 n) (- (A273324 (+ 1 n)) (A273324 n)))


(define (A010877 n) (modulo n 8)) ;; [NJAS] o=0: n mod 8. 

(define (A278488 n) (modulo (A276573 n) 8))

(define (A278265 n) (A000265 (A278498 n)))

;; (same-intfuns1? A278265 (COMPOSE A065338 A000265 A276573) 1200) --> #t

(define (A278509 n) (A065338 (A000265 n)))
(define (A278509v2 n) (A000265 (A065338 n)))

;; (same-intfuns1? A278509 A278509v2 1200) --> #t

;; (same-intfuns1? A278509 (COMPOSE A000244 A065339) 1024) --> #t

(definec (A278513 n) (A008683 (A276573 n)))

(definec (A278514 n) (if (zero? n) n (+ (A278513 n) (A278514 (- n 1)))))



(define (A278516 n) (A278216 (A276573 n)))

;; A278515 [AK] o=0: Number of steps required to iterate map k -> A255131(k) when starting from (A000196(n)+1)^2, before n is reached, or 0 if n is not reached.

(definec (A278515 n)
 (let* ((r (A000196 n))
        (end (A000290 r))
       )
  (let loop ((k (A000290 (+ 1 r))) (s 0))
        (cond ((= k n) s)
              ((<= k end) 0)
              (else (loop (A255131 k) (+ 1 s)))
        )
  )
 )
)

(definec (A278517 n) (if (zero? n) n (let loop ((k (A278517 (- n 1)))) (if (= (A260731 k) n) k (loop (+ 1 k))))))


(definec (A278518 n) (- (A278517 (+ 1 n)) (A278517 n)))
(define (A278518v2 n) (+ 1 (- (A278519 n) (A278517 n))))

;; (same-intfuns0? A278518 (lambda (n) (+ 1 (A278521 n) (A278522 n))) 256) --> #t

(define (A278519 n) (if (zero? n) n (+ -1 (A278517 (+ 1 n)))))

(define (A278521 n) (- (A276573 n) (A278517 n)))

(define (A278522 n) (- (A278519 n) (A276573 n)))


;;;;;;

;; A279336-A279357 are now reserved for your use. 



(definec (A279336 n) (cond ((= 1 n) n) ((zero? (A079559 n)) (* 2 (A234016 n))) (else (A003961 (A279336 (A213714 n))))))

(define (A279336v2 n) (A246278 (A256998 n)))

;; For n > 1, a(n) = the number which is in the same position of array A256997 as where n is in array A246278.

(definec (A279337 n) (cond ((= 1 n) n) ((even? n) (A055938 (/ n 2))) (else (A005187 (A279337 (A064989 n))))))

(define (A279337v2 n) (A256997 (A252752 n)))


(definec (A279338 n) (cond ((= 1 n) n) ((zero? (A079559 n)) (* 2 (A279338 (A256992 n)))) (else (A003961 (A279338 (A256992 n))))))

;; (same-intfuns1? A279338 (COMPOSE A005940 1+ A279343) 2048) --> #t

(definec (A279339 n) (cond ((= 1 n) n) ((even? n) (A055938 (A279339 (/ n 2)))) (else (A005187 (A279339 (A064989 n))))))

(definec (A279348 n) (cond ((= 1 n) n) ((zero? (A079559 n)) (* 2 (A279348 (A256992 n)))) (else (A250469 (A279348 (A256992 n))))))

(definec (A279349 n) (cond ((= 1 n) n) ((even? n) (A055938 (A279349 (/ n 2)))) (else (A005187 (A279349 (A268674 n))))))

;; (same-intfuns1? A000027 (COMPOSE A279349 A279348) 1024) --> #t

;; (same-intfuns1? A279348 (COMPOSE A250245 A279338) 1024) --> #t
;; (same-intfuns1? A279338 (COMPOSE A250246 A279348) 1024) --> #t

;; (same-intfuns1? A279348 (COMPOSE A252753 A279343) 1024) --> #t
;; (same-intfuns1? A279348 (COMPOSE A252755 A279341) 1024) --> #t

;; (same-intfuns1? A279349 (COMPOSE A279339 A250246) 85) --> #t
;; (same-intfuns1? A279349 (COMPOSE A279344 A252754) 55) --> #t
;; (same-intfuns1? A279349 (COMPOSE A279342 A252756) 55) --> #t


(definec (A279341 n) (cond ((<= n 2) (- n 1)) ((zero? (A079559 n)) (* 2 (A279341 (A256992 n)))) (else (+ 1 (* 2 (A279341 (A256992 n)))))))

(definec (A279342 n) (cond ((<= n 1) (+ 1 n)) ((even? n) (A055938 (A279342 (/ n 2)))) (else (A005187 (A279342 (/ (- n 1) 2))))))

;; (same-intfuns1? A279341 (COMPOSE A243071 A279338) 2048) --> #t
;; (same-intfuns0? A279342 (COMPOSE A279339 A163511) 2048) --> #t

(definec (A279343 n) (cond ((= 1 n) 0) ((zero? (A079559 n)) (+ 1 (* 2 (A279343 (A256992 n))))) (else (* 2 (A279343 (A256992 n))))))


(definec (A279344 n) (cond ((zero? n) 1) ((even? n) (A005187 (A279344 (/ n 2)))) (else (A055938 (A279344 (/ (- n 1) 2))))))

;; (same-intfuns1? A279341 (COMPOSE A279343 A279347) 2048) --> #t

;; (same-intfuns1? A279343 (COMPOSE A279341 A279347) 2048) --> #t

;; (same-intfuns1? A279343 (COMPOSE A156552 A279338) 1024) --> #t

;; (same-intfuns0? A279344 (COMPOSE A279339 A005940 1+) 511) --> #t

;; (same-intfuns0? A279344 (COMPOSE A279347 A279342) 1024) --> #t

;; (same-intfuns1? A256993 (COMPOSE 1+ A000523 A279341) 65537) --> #t
;; (same-intfuns1? (COMPOSE A070939 A279341 1+) (COMPOSE A256993 1+) 65537) --> #t

;; (same-intfuns1? A256993 (COMPOSE 1+ A000523 A279343) 65537) --> #t
;; (same-intfuns1? (COMPOSE A070939 A279343 1+) (COMPOSE A256993 1+) 65537) --> #t

(define (A279345 n) (A000120 (A279341 n)))
(define (A279346 n) (A000120 (A279343 n)))

(definec (A279347 n) (cond ((<= n 2) n) ((zero? (A079559 n)) (A005187 (A279347 (A256992 n)))) (else (A055938 (A279347 (A256992 n))))))

(define (A279347v2 n) (A279342 (A279343 n)))
(define (A279347v3 n) (A279344 (A279341 n)))

;; (same-intfuns1? (COMPOSE  (lambda (n) (+ -1 (A279345 n) (A279346 n))) 1+) (COMPOSE A256993 1+) 16387) --> #t

;; (same-intfuns1? (COMPOSE A279345 1+) (COMPOSE 1+ A080791 A279343 1+) 65537) --> #t
;; (same-intfuns1? (COMPOSE A279346 1+) (COMPOSE 1+ A080791 A279341 1+) 65537) --> #t


(define (A278501 n) (A046523 (A279336 n)))
(define (A278502 n) (A046523 (A279338 n)))


(define (A279351 n) (A249817 (A122111 n)))

(define (A279352 n) (A122111 (A249818 n)))

(define (A279350 n) (A046523 (A279352 n)))
(define (A279350v2 n) (A278221 (A249818 n)))

(define (A279354 n) (A046523 (A279356 n)))
(define (A279354v2 n) (A278220 (A249818 n)))

(define (A279355 n) (A249817 (A241909 n)))

(define (A279356 n) (A241909 (A249818 n)))


;; XXX - Submit:

(define (A279340 n) (- (A055938 (+ 1 n)) (A055938 n))) ;; o=1: First differences of A055938.

(define (A279353 n) (A003987bi (A055938 (+ 1 n)) (A055938 n)))

(define (A279357 n) (A003987bi (A005187 (+ 1 n)) (A005187 n))) ;; o=0.


;; A280488-A280509 are now reserved for your use. 

;; Note: In general, if a(n) = gcd(n,Ainvolution(n)), then a(Ainvolution(n)) = a(n).

;; a(n) = n (that is: n/a(n) = 1) if Ainvolution(n) = n, because then gcd(n,n) = n. (a sufficient condition).

;; Now, is it possible that gcd(Ainvolution(n),n) = n, even when Ainvolution(n) <> n ?
;; Say Ainvolution(n) = k <> n. For gcd(n,k) = n, k must be > n.
;;
;; Thus a(n) = gcd(n,Ainvolution(n)) = gcd(n,k) = n,
;; and a(Ainvolution(n)) = gcd(Ainvolution(n),Ainvolution(k)) = gcd(k,n) = n.
;;

;; If Ainvolution is A122111 then it certainly is not possible for gcd(k,n) = n and k > n to hold,
;; as then the Heinz-number k should encode a strictly larger partition that included a partition encoded by the
;; Heinz-number n in it (as Ferrers-diagram), and that is impossible because A122111 preserves
;; the size of the partition.

;; For A241909 we have no fixed points after a(1) = 1.

(define (A280488 n) (/ n (A280489 n)))
(definec (A280489 n) (gcd n (A241909 n)))

(define (A280490 n) (/ n (A280491 n))) ;; A088902 gives the positions of 1's.
(definec (A280491 n) (gcd n (A122111 n)))

;; (same-intfuns1? A280489 (COMPOSE A280489 A241909) 120) --> #t
;; (same-intfuns1? A280491 (COMPOSE A280491 A122111) 255) --> #t


;; (define (Amaybe_later2 n) (gcd (A122111 n) (A241909 n)))
;; (define (Amaybe_later3 n) (/ (A122111 n) (Amaybe_later2 n)))
;; (define (Amaybe_later4 n) (/ (A241909 n) (Amaybe_later2 n)))

(define (A280492 n) (if (= 1 n) 0 (- (A246277 n) (A078898 n))))

(define (A280495 n) (A032742 (A250245 n)))
(define (A280496 n) (A032742 (A250246 n)))

(define (A280497 n) (A032742 (A249817 n)))
(define (A280498 n) (A032742 (A249818 n)))

;;
;; (same-intfuns1? A280495 (lambda (n) (/ (A250245 n) (A020639 n))) 1200) --> #t
;; 
;; (same-intfuns1? A280496 (lambda (n) (/ (A250246 n) (A020639 n))) 1200) --> #t
;; 
;; (same-intfuns1? A280497 (lambda (n) (/ (A249817 n) (A020639 n))) 1200) --> #t
;; 
;; (same-intfuns1? A280498 (lambda (n) (/ (A249818 n) (A020639 n))) 1200) --> #t
;;

;; XFER: GF2X-something.ss:

;; A280493 [AK] o=1: Row sums of triangle A280499 (and A280494):

(definec (A091220 n) ;; Use GCD for deciding whether k is a divisor of n or not:
  (let loop ((k n) (s 0))
        (if (zero? k) s (loop (- k 1) (+ s (if (= k (A091255bi n k)) 1 0))))
  )
)

;; A280493 [AK] o=1: Sum of GF(2)[X] divisors of n: here the sum is ordinary sum of integers, but the divisors are all the numbers whose binary expansions encode such (0,1)-polynomials that divide the (0,1)-polynomial encoded by n, when the polynomial factorization is done over the field GF(2).

(definec (A280493 n) ;; Use GCD for deciding whether k is a divisor of n or not:
  (let loop ((k n) (s 0))
        (if (zero? k) s (loop (- k 1) (+ s (if (= k (A091255bi n k)) k 0))))
  )
)


;; A178908 [Franklin T. Adams-Watters] o=1: GF(2) sum of divisors of n. 
(definec (A178908 n)
  (let loop ((k n) (s 0))
        (if (zero? k) s (loop (- k 1) (A003987bi s (if (= k (A091255bi n k)) k 0))))
  )
)


;; Still a stupid way to compute this because we haven't implemented real GF(2)[X] division or remainder:
(definec (A280493v2 n)
  (let loop ((k n) (s 0))
        (if (zero? k) s (loop (- k 1) (+ s (A280500bi n k))))
  )
)

(definec (A280493v3 n) (add A280494 (+ 1 (A000217 (- n 1))) (A000217 n)))


(define (A280494 n) (A280500bi (A002024 n) (A004736 n))) ;; o=1: Transpose of triangular table A280499. (the first 128 rows)

(define (A280499 n) (A280500bi (A002024 n) (A002260 n))) ;; o=1: Lower triangular region of square array A280500. (the first 256 rows)

;; (same-intfuns1? A280494 (COMPOSE A280499 A038722) 120) --> #t
;; (same-intfuns1? A280499 (COMPOSE A280494 A038722) 120) --> #t

(define (A280500 n) (A280500bi (A002260 n) (A004736 n))) ;; the first 128 antidiagonals

(define (A280500bi row col) (let loop ((d row)) (cond ((zero? d) d) ((= (A048720bi d col) row) d) (else (loop (- d 1))))))

;; (same-intfuns1? A268669 (lambda (n) (A280500bi n (A001317 (A268389 n)))) 1024) --> #t

(definec (A280501 n) (A091255bi n (A193231 n)))
(definec (A280502 n) (A280500bi n (A280501 n))) ;; Positions of ones: A118666.

(definec (A280503 n) (A091255bi n (A056539 n)))
(definec (A280504 n) (A280500bi n (A280503 n))) ;; Positions of ones: A044918 (? Check the definition. The fixed points of A056539 ?).

(definec (A280505 n) (A091255bi n (A057889 n)))
(definec (A280506 n) (A280500bi n (A280505 n))) ;; Positions of ones: A057890.


;; (same-intfuns1? A280506 (COMPOSE A280506 double) 512) --> #t
;; (same-intfuns1? A280506 (COMPOSE A280506 A000265) 512) --> #t

;; (same-intfuns1? A280501 (COMPOSE A280501 A193231) 4096) --> #t
;; (same-intfuns1? A280503 (COMPOSE A280503 A056539) 4096) --> #t
;; (same-intfuns1? A280505 (COMPOSE A280505 A057889) 4096) --> #t

;; (same-intfuns1? A000027 (lambda (n) (A048720bi (A280501 n) (A280502 n))) 1024) --> #t
;; (same-intfuns1? A000027 (lambda (n) (A048720bi (A280503 n) (A280504 n))) 8192) --> #t
;; (same-intfuns1? A000027 (lambda (n) (A048720bi (A280505 n) (A280506 n))) 8192) --> #t


(define (A280507 n) (A003987bi n (A193231 n))) ;; o=0: Positions of zeros: A118666.
;; (same-intfuns0? A280507 (COMPOSE A280507 A193231) 4096) --> #t

(define (A280508 n) (A003987bi n (A057889 n))) ;; o=0: Positions of zeros: A057890
;; (same-intfuns0? A280508 (COMPOSE A280508 A057889) 4096) --> #t

(define (A280509 n) (A051064 (A246200 n))) ;; o=1: a(n) = A051064(A246200(n)); 3-adic valuation of A057889(3*n).

;; Note: A280509(23) = 4, while A051064(23) = 1.
;; (same-intfuns1? A280509 (lambda (n) (A007949 (A057889 (* 3 n)))) 4096) --> #t

;; (same-intfuns1? (COMPOSE A268389 A057889) A268389 4096) --> #t

;; If Ainvolution is A057889 then it certainly is not possible
;; for gcd(n,A057789(n)) = n and A057789(n) <> n to hold (Here we use GF(2)[X]-version of gcd, A091255)
;; because A057889 maps all irreducible GF(2)[X] factors to their mirror-images
;; and unless they are symmetric (or both an ir.factor and its mirror-image is present in equal amounts)
;; then gcd(factor,A057789(factor)) = 1, which contributes to the diminishing of whole gcd(n,A057789(n)).

;; gcd(ir1,ir2) = ir1 only if ir1 = ir2.

;; In general gcd(p_a*q_a*r_a, p_b*q_b*r_b) = gcd(p_a,p_b) * gcd(q_a,q_b) * gcd(r_a,r_b) (or something like that, use better notation).
;;

;; Thus, if an Ainvolution maps any irreducible factor to (the same or different) irreducible factor,
;; then it holds that gcd(n,Ainvolution(n)) = n if and only if Ainvolution(n) = n.
;;

;; Because A056539(2) = A057789(2) = 2 and because they get equal values on all odd n
;; (including all irreducible odd factors in GF(2)[X]):

;; (uniq (map (lambda (n) (- (A057889 n) (A056539 n))) (map A005408 (iota0 16384)))) --> (0)

;; it implies that A056539-based A280503 satisfies the same condition. (??? Not enough...)
;; For the even n for which A056539 also complements the bits
;;

;; A280686-A280707 are now reserved for your use. 

;;


;; /XFER: Fibonacci-core.ss or such...

;; A054494 Largest Fibonacci factor of n. Cf. A075850 ???

(definec (A054494 n) ;; Cf. A280686
   (let loop ((f1 1) (f2 1) (ld 1))
      (cond ((> f2 n) ld)
            ((zero? (modulo n f2)) (loop f2 (+ f1 f2) f2))
            (else (loop f2 (+ f1 f2) ld))
      )
   )
)

;; A054495 [Bottomley] o=1: Smallest k such that n/k is a Fibonacci number. 
(define (A054495 n) (/ n (A054494 n)))

;; A105800 o=1 [???] Greatest Fibonacci number that is a proper divisor of the n-th Fibonacci number; a(1) = a(2) = 1.

(define (A105800 n) (A280686 (A000045 n)))
(define (A105800v2 n) (A000045 (A032742 n))) ;; Of course, A000045 is a monotonic.

;; (same-intfuns1? A105800 A105800v2 233) --> #t

(define (A105800should_be_also n) (gcd (A000045 n) (A000045 (A032742 n))))

;; A280686 Largest Fibonacci proper divisor of n. Cf. A054494, A280687.
(definec (A280686 n)
   (let loop ((f1 1) (f2 1) (lpd 1))
      (cond ((>= f2 n) lpd)
            ((zero? (modulo n f2)) (loop f2 (+ f1 f2) f2))
            (else (loop f2 (+ f1 f2) lpd))
      )
   )
)

;; (same-intfuns1?  (COMPOSE A280686 A001690) (COMPOSE A054494 A001690) 69) --> #t

(define (A280687 n) (/ n (A280686 n))) ;; Cf. A280690

(define (A280688 n) (A280686 (A105800 n)))
(define (A280688v2 n) (A105800 (A032742 n)))

;; (same-intfuns1? A280687 A280687v2 987) --> #t

;; This should be in A032742's family (because A105800 is), also in A105800's family:
(define (A280689 n) (/ (A105800 n) (A280686 (A105800 n))))
(define (A280689v2 n) (/ (A105800 n) (A105800 (A032742 n))))
(define (A280689v3 n) (/ (A000045 (A032742 n)) (A000045 (A054576 n)))) ;; Cf. A280689.



;; (same-intfuns1? A280689 (lambda (n) (/ (A105800 n) (A280688 n))) 120) --> #t
;; (same-intfuns1? A280689 (COMPOSE A280690 A032742) 256) --> #t
;; (same-intfuns1? A280689 A280689v2 987) --> #t
;; (same-intfuns1? A280689 A280689v3 987) --> #t


(define (A280690 n) (/ (A000045 n) (A105800 n))) ;; Cf. A280689
(define (A280690v1 n) (/ (A000045 n) (A000045 (A032742 n))))
(define (A280690v2 n) (A280687 (A000045 n)))

;; (same-intfuns1? A280690 A280690v2 987) --> #t


;; (define (Amaybe_later n) (A046523 (A280690 n))) ;; Cf. A278245.

;; (same-intfuns1? Amaybe_later (lambda (n) (A046523 (A280687 (A000045 n)))) 42) --> #t (Essentially a function of A000045)

;; /XFER: Recurrences-XXX.ss ?

;; A000032 [NJAS] o=0: Lucas numbers (beginning at 2): L(n) = L(n-1) + L(n-2). (Cf. A000204.)
(definec (A000032 n) (cond ((zero? n) 2) ((= 1 n) 1) (else (+ (A000032 (- n 1)) (A000032 (- n 2))))))

;; A015518 [Olivier Gérard] o=0: a(n) = 2*a(n-1) + 3*a(n-2), with a(0)=0, a(1)=1. 
(definec (A015518 n) (if (<= n 1) n (+ (* 2 (A015518 (- n 1))) (* 3 (A015518 (- n 2))))))

(define (A280691 n) (/ (A015518 (A032742 n)) (A015518 (A054576 n))))


;; A280694 [AK] o=1: Largest Lucas number (A000032) dividing n.
;; Cf. A054494.
(definec (A280694 n)
   (let loop ((l1 1) (l2 3) (lpd 1))
      (cond ((> l1 n) (if (and (= 1 lpd) (even? n)) 2 lpd))
            ((zero? (modulo n l1)) (loop l2 (+ l1 l2) l1))
            (else (loop l2 (+ l1 l2) lpd))
      )
   )
)

(define (A280695 n) (/ n (A280694 n))) ;; n divided by the largest Lucas number (A000032) dividing n.

;; A280696 Largest Lucas proper divisor of n. Cf. A280686, A280697.
(definec (A280696 n)
   (let loop ((l1 1) (l2 3) (lpd 1))
      (cond ((>= l1 n) (if (and (= 1 lpd) (even? n) (> n 2)) 2 lpd))
            ((zero? (modulo n l1)) (loop l2 (+ l1 l2) l1))
            (else (loop l2 (+ l1 l2) lpd))
      )
   )
)

;; (same-intfuns1?  (COMPOSE A280696 A001690) (COMPOSE A054494 A001690) 69) --> #t

(define (A280697 n) (/ n (A280696 n))) ;; Cf. A280687

;; Greatest Lucas number that is a proper divisor of the n-th Lucas number, a(1) = 1.
(define (A280698 n) (A280696 (A000032 n)))

;; Greatest Lucas number that is a divisor of the n-th Fibonacci number, a(1) = a(2) = 1.
(define (A280699 n) (A280694 (A000045 n)))

;; A063041 [Zumkeller] o=2: Image of n under Collatz-2 map, a generalization of the classical '3x+1' - function: instead of dividing an even number by 2 a nonprime will be divided by its smallest prime factor and a prime will be multiplied not by 3 but by its prime-predecessor. 

(definec (A063041 n) (if (= 1 (A010051 n)) (+ 1 (* (A064989 n) n)) (A032742 n)))

;; A063045 [Zumkeller] o=2: Largest value in Collatz-2 (A063041) trajectory starting at n. 

(definec (A063045 n)
   (let loop ((n n) (m 0) (visited (list)))
      (if (memq n visited)
          m
          (loop (A063041 n) (max m (A063041 n)) (cons n visited))
      )
   )
)



;; XFER: Fibonacci-core.ss etc.

;; A283765 [CK] o=1: Numbers k such that L(k) is even, where L = A000201 = lower Wythoff sequence. 
(define A283765 (MATCHING-POS 1 1 (lambda (n) (even? (A105774 n)))))

;; A283766 [CK] o=1: Numbers k such that L(k) is odd, where L = A000201 = lower Wythoff sequence. 
(define A283766 (MATCHING-POS 1 1 (lambda (n) (odd? (A105774 n)))))

;; A105774 [Cloitre] o=1: A "fractal" transform of the Fibonacci's numbers : a(1)=1 then if F(n)<k<=F(n+1) a(k)=F(n+1)-a(k-F(n)) where F(n)=A000045(n). 

(definec (A105774 n) (if (= 1 n) n (- (A000045 (+ 2 (A072649 (- n 1)))) (A105774 (- n (A000045 (+ 1 (A072649 (- n 1)))))))))

;; A085002 [Cloitre] o=1: a(n) = floor(phi*n)-2*floor(phi*n/2) where phi is the golden ratio.

(define (A085002 n) (A000035 (A105774 n)))

;; A085003 [Cloitre] o=1: Partial sums of A085002. 

(definec (A085003 n) (if (= 1 n) n (+ (A085002 n) (A085003 (- n 1)))))

;; XFER: Base-2.core.ss or such?
(define (A029931 n)
  (let loop ((n n) (i 1) (s 0))
     (cond ((zero? n) s)
           ((odd? n) (loop (/ (- n 1) 2) (+ 1 i) (+ s i)))
           (else (loop (/ n 2) (+ 1 i) s))
     )
  )
)

;; A124757 [Franklin T. Adams-Watters] o=0: Zero-based weighted sum of compositions in standard order.
(define (A124757 n) (if (zero? n) n (- (A029931 n) (A070939 n))))

(define (A283981 n) (- (A029931 n) (A280700 n)))

(define (A283982 n) (if (zero? n) n (- (A070939 n) (A280700 n))))


(define (A283996 n) (A003986bi n (A005187 (floor->exact (/ n 2)))))

(define (A283997 n) (A003987bi n (A005187 (floor->exact (/ n 2)))))

;; (same-intfuns0? A000120 (lambda (n) (- n (A005187 (floor->exact (/ n 2))))) 65537) --> #t

(define (A283998 n) (A004198bi n (A005187 (floor->exact (/ n 2)))))

;; (same-intfuns0? A005187 (lambda (n) (+ (A283996 n) (A283998 n))) 1024) --> #t
;; (same-intfuns0? A005187 (lambda (n) (+ (A283997 n) (* 2 (A283998 n)))) 1024) --> #t
;; (same-intfuns0? A283996 (lambda (n) (+ (A283997 n) (A283998 n))) 65537) --> #t

;; (same-intfuns0? (lambda (n) (A003987bi (A006068 n) (A283997 n))) (lambda (n) (A003987bi (A006068 (floor->exact (/ n 2))) (A005187 (floor->exact (/ n 2))))) 65537) --> #t

(define (A283999 n) (A003987bi (A005187 n) (A006068 n)))

;; (same-intfuns0? A283999 (lambda (n) (A003987bi (A006068 (* 2 n)) (A283997 (* 2 n)))) 65537) --> #t

;; (same-intfuns0? A283997 (lambda (n) (A003987bi (A006068 n) (A283999 (floor->exact (/ n 2))))) 65537) --> #t

;; A000930 [NJAS] o=0: Narayana's cows sequence: a(0) = a(1) = a(2) = 1; thereafter a(n) = a(n-1) + a(n-3). 
(definec (A000930 n) (if (<= n 2) 1 (+ (A000930 (- n 1)) (A000930 (- n 3)))))

;; A000931 [NJAS] o=0: Padovan sequence: a(n) = a(n-2) + a(n-3) with a(0)=1, a(1)=a(2)=0. 
(definec (A000931 n) (cond ((zero? n) 1) ((<= n 2) 0) (else (+ (A000931 (- n 2)) (A000931 (- n 3))))))

;; A005185 [Plouffe & NJAS] o=1: Hofstadter Q-sequence: a(1) = a(2) = 1; a(n) = a(n-a(n-1)) + a(n-a(n-2)) for n > 2.
(definec (A005185 n) (if (<= n 2) 1 (+ (A005185 (- n (A005185 (- n 1)))) (A005185 (- n (A005185 (- n 2)))))))

(define (A283467 n) (A005185 (- (+ n 1) (A005185 n))))

(definec (A280706 n) (if (= 1 n) 1 (+ (A280706 (- n 1)) (A283467 n))))

;; A280706 [AK] o=1: a(n) = Sum_{k=1..n} q(k+1-q(k)), where q(k) = A005185(k).
(define (A280706v2 n) (add (lambda (k) (A005185 (- (+ k 1) (A005185 k)))) 1 n))

;; A284173 [Altug Alkan] o=1: a(n) = (Sum_{k=1..n} q(k+1-q(k))) mod n where q(k) = A005185(k). 

(define (A284173 n) (modulo (A280706 n) n))

;; A284019 [Altug Alkan] o=1: a(n) = A004001(n) - A005185(n).
(define (A284019 n) (- (A004001 n) (A005185 n)))


;; A007955 [R. Muller] o=1: Product of divisors of n. 

(define (A007955 n) (A000196 (expt n (A000005 n))))

;; A naive implementation:
(definec (A007955v2 n)
  (let loop ((d n) (m 1))
    (cond ((zero? d) m)
          ((zero? (modulo n d)) (loop (- d 1) (* m d)))
          (else (loop (- d 1) m))
    )
  )
)

(define (A283995 n) (A046523 (A007955 n)))

;; A007954 Product of proper divisors of n.

(define (A007956 n) (A000196 (expt n (- (A000005 n) 2))))
(define (A007956v2 n) (/ (A007955 n) n))

;; A123275 [Leroy Quet] o=1: Table (read by antidiagonals) where t(n,m) = largest divisor of m which is coprime to n.


(define (A123275 n) (A123275bi (A004736 n) (A002260 n)))


(define (A123275bi b a)
  (let loop ((a a) (m 1))
    (let ((s (A020639 a)))
       (cond ((= 1 a) m)
             ((zero? (modulo b s)) (loop (/ a s) m))
             (else (loop (/ a s) (* s m)))
       )
    )
  )
)


;; A243103 [Michael De Vlieger] o=1: Product of numbers m with 2 <= m <= n whose prime divisors all divide n. 

;; A naive implementation:
(definec (A243103 n)
  (let loop ((k n) (m 1))
    (cond ((= 1 k) m)
          ((= 1 (A123275bi n k)) (loop (- k 1) (* m k)))
          (else (loop (- k 1) m))
    )
  )
)

;; A283990: o=1: a(n) = A046523(A243103(n)).

(define (A283990 n) (A046523 (A243103 n)))

(define (A284008 n) (lcm (A260443 n) (A260443 (+ 1 n))))
(define (A284008v2 n) (/ (A277324 n) (A277198 n)))

(define (A284009 n) (A001222 (A284008 n)))
;; (same-intfuns1? A007306 (lambda (n) (+ (A277328 (- n 1)) (A284009 (- n 1)))) 120) --> #t


(definec (A106490 n) (if (= 1 n) 0 (+ 1 (A106490 (A067029 n)) (A106490 (A028234 n)))))


(definec (A106491 n) (cond ((= 1 n) n) ((= 1 (A028234 n)) (+ 1 (A106491 (A067029 n)))) (else (+ 1 (A106491 (A067029 n)) (A106491 (A028234 n))))))

;; XFER: Base-2.core.ss or such:
;; A093409 [Ralf Stephan] o=0: n-1 minus exponent of 2 in n, a(0) = 0.
(define (A093049 n) (if (zero? n) n (- n (A001511 n))))

;; A284252-A284273 are now reserved for your use. 

;; A284252 [AK] o=1: smallest prime dividing n which is > A020639(n)^2, or 1 if no such prime exists.

(definec (A284252 n)
   (let ((spf1 (A020639 n)))
     (let loop ((n (/ n spf1)))
        (let ((spf2 (A020639 n)))
           (cond ((= 1 spf2) 1)
                 ((> spf2 (* spf1 spf1)) spf2)
                 (else (loop (/ n spf2)))
           )
        )
     )
   )
)

(define (A284253 n) (/ n (A284252 n)))

(definec (A284254 n) (if (= 1 (A284252 n)) 1 (* (A284252 n) (A284254 (A284253 n)))))
(define (A284255 n) (/ n (A284254 n)))

(definec (A284256 n) (if (= 1 (A284252 n)) 0 (+ 1 (A284256 (A284253 n)))))
(define (A284256v2 n) (A001222 (A284254 n)))

(define (A284257 n) (A001222 (A284255 n)))
(define (A284257v2 n) (- (A001222 n) (A284256 n)))

(define (A284258 n) (A001221 (A284254 n)))
(define (A284259 n) (A001221 (A284255 n)))
(define (A284259v2 n) (- (A001221 n) (A284258 n)))

(define (A284260 n) (A006530 (A284255 n)))

;; A252459
(define (A284261 n) (- (A284258 n) (A284258 (A003961 n))))
(define (A284261v2 n) (- (A001221 (A284254 n)) (A001221 (A284254 (A003961 n)))))

;; A001747 [NJAS] o=1: 2 together with primes multiplied by 2. 
(define (A001747 n) (* 2 (A008578 n))) 

(define (A284262 n) (A242378bi (A284263 n) (A002110 n)))

(definec (A284263 n) (if (zero? n) n (A252459 (* 2 (A000040 n)))))
(define (A284263v2 n) (A252459 (A002110 n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A247503 [Tom Edgar] o=1: Completely multiplicative with a(prime(n)) = prime(n)^(n mod 2). 

(definec (A247503 n)
  (cond ((= 1 n) n)
        ((odd? (A055396 n)) (* (A020639 n) (A247503 (/ n (A020639 n)))))
        (else (A247503 (/ n (A020639 n))))
  )
)

;; A248101 [Tom Edgar] o=1: Completely multiplicative with a(prime(n)) = prime(n)^((n+1) mod 2). 

(definec (A248101 n)
  (cond ((= 1 n) n)
        ((even? (A055396 n)) (* (A020639 n) (A248101 (/ n (A020639 n)))))
        (else (A248101 (/ n (A020639 n))))
  )
)

;; A284552-A284584 are now reserved for your use. 

(define (A284553 n) (A247503 (A260443 n)))

(define (A284554 n) (A248101 (A260443 n)))

(definec (A000360with_prep_0 n) (cond ((<= n 1) n) ((even? n) (A284556 (/ n 2))) (else (+ (A000360with_prep_0 (/ (- n 1) 2)) (A000360with_prep_0 (/ (+ n 1) 2))))))

(define (A000360 n) (A000360with_prep_0 (+ 1 n)))

(definec (A284556 n) (cond ((<= n 1) 0) ((even? n) (A000360with_prep_0 (/ n 2))) (else (+ (A284556 (/ (- n 1) 2)) (A284556 (/ (+ n 1) 2))))))

;; b(0) = 0, b(1) = 1, b(2n) = c(n), b(2n+1) = b(n) + b(n+1).
;; c(0) = 0, c(1) = 0, c(2n) = b(n), c(2n+1) = c(n) + c(n+1).


(define (A284556v2 n) (A001222 (A284554 n)))


(define (A284563 n) (A247503 (A277324 n)))
(define (A284563v2 n) (A284553 (+ n n 1)))

(define (A284564 n) (A248101 (A277324 n)))
(define (A284564v2 n) (A284554 (+ n n 1)))

(define (A284565 n) (A000360with_prep_0 (+ n n 1)))
(define (A284565v2 n) (A001222 (A284563 n)))
(define (A284565v3 n) (A000360 (* 2 n)))

(define (A284566 n) (A284556 (+ n n 1)))
(define (A284566v2 n) (A001222 (A284564 n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (A284570 n) (- (A000005 (A000290 (+ 1 n))) (A000005 (* n n))))

;; A276553 [K. D. Bajpai] o=1: Numbers n such that n^2 and (n + 1)^2 have the same number of divisors.
(define A276553 (ZERO-POS 1 1 A284570))

;;;;

(define (A000265fast n) (let loop ((n n)) (if (odd? n) n (loop (/ n 2)))))

(definec (A285101 n) (if (zero? n) 2 (* (A285101 (- n 1)) (A242378bi n (A285101 (- n 1))))))

(definec (A285102 n) (if (zero? n) 2 (/ (lcm (A285102 (- n 1)) (A242378bi n (A285102 (- n 1)))) (gcd (A285102 (- n 1)) (A242378bi n (A285102 (- n 1)))))))

;; A068052 [AK] o=0: Start from 1, shift one left and sum mod 2 (bitwise-XOR) to get 3 (11 in binary), then shift two steps left and XOR to get 15 (1111 in binary), then three steps and XOR to get 119 (1110111 in binary), then four steps and so on.
(definec (A068052 n) (if (zero? n) 1 (A003987bi (A068052 (- n 1)) (* (A000079 n) (A068052 (- n 1))))))

;; A028362 [NJAS] o=1: Total number of self-dual binary codes of length 2n. Totally isotropic spaces of index n in symplectic geometry of dimension 2n.

(define (A028362 n) (A028362off0 (- n 1)))
(definec (A028362off0 n) (if (zero? n) 1 (+ (A028362off0 (- n 1)) (* (A000079 n) (A028362off0 (- n 1))))))


(definec (A285103 n) (A000120 (A068052 n)))

(define (A285104 n) (- (A000079 n) (A285103 n)))

(define (A285105 n) (A080791 (A068052 n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A065642 [Zumkeller] o=1: a(1) = 1; for n > 1, a(n) = Min {m > n | same prime factors as n, ignoring multiplicity}. 

(definec (A065642 n) (if (= 1 n) n (let ((k (A007947 n))) (let loop ((n (+ n k))) (if (= (A007947 n) k) n (loop (+ n k)))))))

(define (A285109 n) (* (A020639 n) n)) ;; [AK] o=1:  a(n) = n multiplied by its smallest prime factor; a(1) = 1.

;; (definec (A285328 n) (if (not (zero? (A008683 n))) 1 (let ((k (A007947 n))) (let loop ((n (- n k))) (if (= (A007947 n) k) n (loop (- n k)))))))

(definec (A285328 n)
    (cond ((= 1 n) n)
          ((pow2? n) (/ n 2))
          ((not (zero? (A008683 n))) 1)
          (else (let ((k (A007947 n))) (let loop ((n (- n k))) (if (= (A007947 n) k) n (loop (- n k))))))
    )
)



(definec (A008479 n) (if (not (zero? (A008683 n))) 1 (+ 1 (A008479 (A285328 n)))))

(define (A285329 n) (A013928 (A007947 n)))


;; A087207 [Mitch Cervinka] o=1: A binary representation of the primes that divide a number. 

(define (A087207 n) (A048675 (A007947 n)))

;; (definec (A285329 n) (if (not (zero? (A008683 n))) (A013928 n) (A285329 (A285328 n))))

;; (same-intfuns0? A064273 (COMPOSE A285329 A019565) 42) --> #t


;; A079277 [Istvan Beck] o=2: Largest integer k < n such that any prime factor of k is also a prime factor of n. 
;; Largest k < n with rad(kn) = rad(n), where rad = A007947.

;; This version has a(1) = 1:
(definec (A079277 n)
   (let ((r (A007947 n)))
      (let loop ((k (- n 1)))
         (cond ((<= n 1) 1)
               ((= (A007947 (* k n)) r) k)
               (else (loop (- k 1)))
         )
      )
   )
)

(define (A285699 n) (if (= 1 n) n (- n (A079277 n))))

(define (A285709 n) (- (A000010 n) (A285699 n)))
(define (A285709v2 n) (if (= 1 n) 0 (- (+ (A000010 n) (A079277 n)) n)))
(define (A285709v3 n) (if (= 1 n) 0 (- (A079277 n) (A051953 n))))


(define A285710 (ZERO-POS 1 1 A285709))
(define A285710v2 (MATCHING-POS 1 1 (lambda (n) (or (= 1 n) (= (A051953 n) (A079277 n))))))

(define (A285711 n) (if (= 1 n) n (gcd (A051953 n) (A079277 n))))

(define (A285707 n) (if (= 1 n) n (gcd n (A079277 n))))
(define (A285707v2 n) (if (= 1 n) n (gcd n (A285699 n))))

(define (A285708 n) (/ n (A285707 n)))

(define A208815 (MATCHING-POS 1 1 (lambda (n) (> (A051953 n) (A079277 n)))))

(define (A284584 n) (cond ((= 1 n) 0) ((zero? (A008683 n)) (A057627 n)) (else (A013928 n))))

;; Entanglement-permutations:

(definec (A284571 n)
   (cond ((= 1 n) n)
         ((not (zero? (A008683 n))) (* 2 (A284571 (A013928 n))))
         (else (+ 1 (* 2 (A284571 (+ -1 (A285328 n))))))
   )
)

(definec (A284572 n)
   (cond ((= 1 n) n)
         ((even? n) (A005117 (+ 1 (A284572 (/ n 2)))))
         (else (A065642 (+ 1 (A284572 (/ (- n 1) 2)))))
   )
)


;; (same-intfuns1? A000027 (COMPOSE A284572 A284571) 255) --> #t

;; (same-intfuns1? A000027 (COMPOSE A284571 A284572) 120) --> #t

;; A285111, A285112

(definec (A285111 n)
   (cond ((<= n 2) (- n 1))
         ((not (zero? (A008683 n))) (* 2 (A285111 (A013928 n))))
         (else (+ 1 (* 2 (A285111 (A285328 n)))))
   )
)

(definec (A285112 n)
   (cond ((<= n 1) (+ n 1))
         ((even? n) (A005117 (+ 1 (A285112 (/ n 2)))))
         (else (A065642 (A285112 (/ (- n 1) 2))))
   )
)

(definec (A285331 n)
   (cond ((<= n 2) (- n 1))
         ((not (zero? (A008683 n))) (* 2 (A285331 (A048675 n))))
         (else (+ 1 (* 2 (A285331 (A285328 n)))))
   )
)

(definec (A285332 n)
   (cond ((<= n 1) (+ n 1))
         ((even? n) (A019565 (A285332 (/ n 2))))
         (else (A065642 (A285332 (/ (- n 1) 2))))
   )
)

(define (A285333 n) (A048675 (A285332 n)))

(define (A285330 n) (if (not (zero? (A008683 n))) (A048675 n) (A285328 n)))

(define (A284311 n) (A284311bi (A002260 n) (A004736 n)))

(define (A284311bi row col) (if (= 1 row) (A005117 (+ 1 col)) (A065642 (A284311bi (- row 1) col))))

(define (A284457 n) (A284311bi (A004736 n) (A002260 n)))

(define (A285321 n) (A285321bi (A002260 n) (A004736 n)))
(define (A285321bi row col) (if (= 1 row) (A019565 col) (A065642 (A285321bi (- row 1) col))))

;; (first-dislocated (cons 0 (cons 1 (map (lambda (n) (A285321bi (A008479 n) (A087207 n))) (map 1+ (iota 55)))))) --> ()

(define (A285322 n) (A285321bi (A004736 n) (A002260 n)))


(define (A285325 n) (A285325bi (A002260 n) (A004736 n)))
(define (A285325bi row col) (A048675 (A285321bi row col)))

(define (A285326 n) (if (zero? n) n (+ n (A006519 n))))
(define (A285326v1 n) (A048675 (A065642 (A019565 n))))
(define (A285326v2 n) (A285325bi 2 n))
(define (A285326v3 n) (A048675 (A285109 (A019565 n))))

(define (A285327 n) (A048675 (A065642 (A065642 (A019565 n)))))
(define (A285327v2 n) (A285325bi 3 n))

(define (A285323 n) (cond ((zero? n) 1) ((or (= 1 (A000120 n)) (> (A000040 (+ 1 (A285099 n))) (A000290 (A000040 (+ 1 (A007814 n)))))) (A000290 (A000040 (+ 1 (A007814 n))))) (else  (A000040 (+ 1 (A285099 n))))))

(define (A285323v2 n) (/ (A065642 (A065642 (A019565 n))) (A019565 n)))

(define (A285323v3 n) (cond ((zero? n) 1) ((or (= 1 (A000120 n)) (> (A000040 (+ 1 (A285099 n))) (A000290 (A000040 (+ 1 (A007814 n)))))) (A000290 (A020639 (A019565 n)))) (else (A014673 (A019565 n)))))


(define (A285110 n) (A001222 (A285323 n)))
(define (A285110v2 n) (cond ((zero? n) n) ((or (= 1 (A000120 n)) (> (A000040 (+ 1 (A285099 n))) (A000290 (A000040 (+ 1 (A007814 n)))))) 2) (else 1)))
;; (define (A285110v2 n) (cond ((zero? n) n) ((= 1 (A000120 n)) 2) ((< (A000040 (+ 1 (A285099 n))) (A000290 (A000040 (+ 1 (A007814 n))))) 1) (else 2)))


(define (A285324 n) (A000523 (- (A285327 n) n))) ;; All terms A285327(n)-n are powers of 2.

(define (A285336 n) (numerator (/ (A065642 n) n)))
(define (A285337 n) (denominator (/ (A065642 n) n)))

(define A285100 (ZERO-POS 1 1 (COMPOSE -1+ A285337)))
(define A285100v2 (MATCHING-POS 1 1 (lambda (n) (= (A065642 n) (A285109 n)))))

;; A284342 [Gionata Neri] o=1: Numbers n that such A065642(n) < n*lpf(n), where lpf = least prime factor (A020639). 
(define A284342 (MATCHING-POS 1 1 (lambda (n) (< (A065642 n) (A285109 n)))))


;; A109168 [Paul D. Hanna] o=1: Continued fraction expansion of constant x (A109169) such that the continued fraction of 2*x yields the continued fraction of x interleaved with positive even numbers.
(definec (A109168 n) (if (zero? n) n (if (odd? n) (/ (+ 1 n) 2) (* 2 (A109168 (/ n 2))))))

;; (same-intfuns0? (COMPOSE A065642 A019565)  (COMPOSE A285109 A019565) 1023) --> #t

(define (A285327 n) (if (zero? n) n (/ (+ n (A006519 n)) 2)))

;; XXX - XFER: ???

(define (A036563 n) (- (expt 2 n) 3)) ;; [NJAS] o=0: 2^n-3.

(define (A014106 n) (* n (+ 3 n n))) ;; [NJAS] o=0: a(n) = n*(2*n+3). 


;; A055010 [Henry Bottomley] o=0: a(0) = 0; for n > 0, a(n) = 3*2^(n-1) - 1. 
;; A083329 [Paul Barry] o=0: a(0) = 1; for n > 0, a(n) = 3*2^(n-1) - 1.
;; A153893 [Vladimir Joseph Stephan Orlovsky] o=0: a(n) = 3*2^n - 1. 

(define (A055010 n) (if (zero? n) n (+ -1 (* 3 (expt 2 (- n 1))))))

(define (A083329 n) (if (zero? n) 1 (+ -1 (* 3 (expt 2 (- n 1))))))

(define (A153893 n) (+ -1 (* 3 (expt 2 n))))

;; A129760 [Russ Cox] o=1: Bitwise AND of binary representation of n-1 and n. 
(define (A129760 n) (A004198bi n (- n 1)))
(define (A129760v2 n) (- n (A006519 n)))

;; XFER: Base-2.core.ss or such:
;; A285099 [AK] o=0: Zero-based index of the second least significant 1-bit in base-2 representation of n, 0 if less than two 1-bits in n.
(define (A285099 n) (if (<= (A000120 n) 1) 0 (A007814 (A004198bi n (- n 1)))))

(define (A285097 n) (if (<= (A000120 n) 1) 0 (- (A285099 n) (A007814 n))))

(definec (A109162 n) (if (zero? n) n (A019565 (A109162 (- n 1))))) ;; [Quet] a(1)=1. a(n) = A019565(a(n-1)). 

;; XFER: Prime-factorization.something.ss ???

;; A014612 [Eric W. Weisstein] o=1: Numbers that are the product of exactly three (not necessarily distinct) primes.
(define A014612 (MATCHING-POS 1 1 (lambda (n) (= 3 (A001222 n)))))

;; A285508 [Kalle Siukola] o=1:  Numbers with exactly three prime factors, not all of them distinct. 
(define A285508 (MATCHING-POS 1 1 (lambda (n) (and (= 3 (A001222 n)) (< (A001221 n) 3)))))

;; A003159 [NJAS, Plouffe] o=1: Numbers n whose binary representation ends in an even number of zeros. 
(define A003159 (MATCHING-POS 1 1 (COMPOSE even? A007814)))

;; A036554 [Tom Verhoeff] o=1: Numbers whose binary representation ends in an odd number of zeros.
(define (A036554 n) (* 2 (A003159 n)))

(define A036554v2 (MATCHING-POS 1 1 (COMPOSE odd? A007814)))



(define (A285334 n) (A046523 (A243505 n)))


;; A285335: [AK] o=1: Odd bisection of A048675 divided by two.
(define (A285335 n) (A048675 (A064216 n)))

(define (A285335v2 n) (/ (A048675 (+ n n -1)) 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A000027v2 n) (A000027bi (A002260 n) (A004736 n)))


(define (A000027bi row col) (* (/ 1 2) (+ (expt (+ row col) 2) (- row) (- (* 3 col)) 2)))


;; XXX - Submit following arrays/triangles, also row sums, etc!

;; A072030 [Michael Somos] o=1: Array read by antidiagonals: T(n,k) = number of steps in simple Euclidean algorithm for gcd(n,k) where n >= 1, k >= 1.

(define (A072030 n) (A072030bi (A002260 n) (A004736 n)))
(define (A072030bi row col) (cond ((= row col) 1) ((< row col) (A072030bi col row)) (else (+ 1 (A072030bi col (- row col))))))

;; XFER: triangles-traces-and-relations.ss or such, many of the following ones:

(define (A285721 n) (A285721bi (A002260 n) (A004736 n)))
(define (A285721bi row col) (cond ((= row col) 0) ((> row col) (+ 1 (A285721bi (- row col) col))) (else (+ 1 (A285721bi row (- col row))))))



(define (A285721v2 n) (A285721biv2 (A002260 n) (A004736 n)))
(define (A285721biv2 row col) (if (= row col) 0 (+ 1 (A285721biv2 (abs (- row col)) (min col row)))))

(definec (A285721r1 n) (if (zero? (A285722 n)) 0 (+ 1 (A285721r1 (A285722 n)))))

(define (A285722 n) (A285722bi (A002260 n) (A004736 n)))
(define (A285722bi row col) (cond ((= row col) 0) ((> row col) (A000027bi (- row col) col)) (else (A000027bi row (- col row)))))

(define (A285723 n) (A285722bi (A004736 n) (A002260 n)))

(define (A285732 n) (A285732bi (A002260 n) (A004736 n)))
(define (A285732bi row col) (cond ((= row col) (- row)) ((> row col) (A000027bi (- row col) col)) (else (A000027bi row (- col row)))))

(define (A285733 n) (A285732bi (A004736 n) (A002260 n)))

(define (A286100 n) (A286100bi (A002260 n) (A004736 n)))
(define (A286100bi row col) (if (= row col) row 0))


;; Related to:
(define (A003989v2 n) (A003989biv2 (A002260 n) (A004736 n)))
(define (A003989biv2 row col) (if (= row col) row (A003989biv2 (abs (- row col)) (min col row))))


;;;

;; Cf. A003056, A003986, A004198.

;; [AK] o=o: tabl
(define (A286098 n) (A286098bi (A002262 n) (A025581 n)))
(define (A286098bi row col)
   (let ((a (A004198bi row col)) (b (A003986bi row col)))
     (/ (+ (expt (+ a b) 2) (* 3 a) b) 2)
   )
)

(define (A286099 n) (A286099bi (A002262 n) (A025581 n)))
(define (A286099bi row col)
   (let ((a (A003986bi row col)) (b (A004198bi row col)))
     (/ (+ (expt (+ a b) 2) (* 3 a) b) 2)
   )
)


(define (A286108 n) (A286108bi (A002262 n) (A025581 n)))
(define (A286108bi row col)
   (let ((a (* 2 (A004198bi row col))) (b (A003987bi row col)))
     (/ (+ (expt (+ a b) 2) (* 3 a) b) 2)
   )
)


(define (A286109 n) (A286109bi (A002262 n) (A025581 n)))
(define (A286109bi row col)
   (let ((a (A003987bi row col)) (b (* 2 (A004198bi row col))))
     (/ (+ (expt (+ a b) 2) (* 3 a) b) 2)
   )
)


;; A286142-A286164 are now reserved for your use. 



(define (A286145 n) (A286145bi (A002262 n) (A025581 n)))
(define (A286145bi row col)
   (let ((a (A003987bi row col)) (b col))
     (/ (+ (expt (+ a b) 2) (* 3 a) b) 2)
   )
)

(define (A286145v2 n) (A286145v2bi (A002262 n) (A025581 n)))
(define (A286145v2bi row col)
   (define (pairA001477 a b) (/ (+ (expt (+ a b) 2) (* 3 a) b) 2))
   (pairA001477 (A003987bi row col) col)
)

(define (A286147 n) (A286147bi (A002262 n) (A025581 n)))
(define (A286147bi row col)
   (let ((a (A003987bi row col)) (b row))
     (/ (+ (expt (+ a b) 2) (* 3 a) b) 2)
   )
)


(define (A286150 n) (A286150bi (A002262 n) (A025581 n)))
(define (A286150bi row col)
   (let ((a (A003987bi row col)) (b (min col row)))
     (/ (+ (expt (+ a b) 2) (* 3 a) b) 2)
   )
)

(define (A286151 n) (A286151bi (A002262 n) (A025581 n)))
(define (A286151bi row col)
   (define (pairA001477bi a b) (/ (+ (expt (+ a b) 2) (* 3 a) b) 2))
   (cond ((> row col) (pairA001477bi (A003987bi row col) col)) (else (pairA001477bi row (A003987bi col row))))
)

(define (A286153 n) (A286151bi (A002260 n) (A004736 n)))

(define (A286155 n) (A286155bi (A002260 n) (A004736 n)))
(define (A286155bi row col) (cond ((= row col) (- row)) ((> row col) (A000027bi (A003987bi row col) col)) (else (A000027bi row (A003987bi col row)))))


(define (A286156 n) (A286156bi (A002260 n) (A004736 n)))
(define (A286156bi row col)
   (if (zero? col)
       -1
       (let ((a (remainder row col)) (b (quotient row col)))
          (/ (+ (expt (+ a b) 2) (* 3 a) b) 2)
       )
   )
)

(define (A286157 n) (A286156bi (A004736 n) (A002260 n)))

(define (A286158 n) (A286156bi (A002024 n) (A002260 n)))

(define (A286159 n) (A286156bi (A002024 n) (A004736 n)))

;; XFER: Triangles-divisibility-or-such.ss ??=

;; A051731 [Klaus Strassburger] o=1: Triangle read by rows: T(n,k) = 1 if k divides n, T(n,k) = 0 otherwise. 
(define (A051731 n) (A051731bi (A002260 n) (A004736 n)))
(define (A051731bi row col) (if (not (zero? (modulo (+ row col -1) row))) 0 1))

;; A113998 [Paul Barry] o=1: Reverse of triangle A051731.

(define (A113998 n) (A113998bi (A002260 n) (A004736 n)))
(define (A113998bi row col) (if (not (zero? (modulo (+ row col -1) col))) 0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; phi & (n/k) packed triangles:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A286234 n) (A286234bi (A002260 n) (A004736 n)))

(define (A286234bi row col)
   (let ((a (A000010 col)) (b (quotient (+ row col -1) col)))
      (* (/ 1 2) (+ (expt (+ a b) 2) (- a) (- (* 3 b)) 2))
   )
)

(define (A286235 n) (A286235bi (A002260 n) (A004736 n)))

(define (A286235bi row col)
   (let ((a (A000010 row)) (b (quotient (+ row col -1) row)))
      (* (/ 1 2) (+ (expt (+ a b) 2) (- a) (- (* 3 b)) 2))
   )
)


(define (A286236 n) (A286236bi (A002260 n) (A004736 n)))

(define (A286236v2 n) (A286236tr (A002024 n) (A002260 n)))
(define (A286236tr n k) (A286236bi k (+ 1 (- n k))))

(define (A286236bi row col)
   (if (not (zero? (modulo (+ row col -1) col)))
       0
       (let ((a (A000010 col)) (b (/ (+ row col -1) col)))
          (* (/ 1 2) (+ (expt (+ a b) 2) (- a) (- (* 3 b)) 2))
       )
   )
)

(define (A286237 n) (A286237bi (A002260 n) (A004736 n)))

(define (A286237v2 n) (A286237tr (A002024 n) (A002260 n)))
;; (define (A286237tr n k) (A286237bi k (+ 1 (- n k))))

(define (A286237tr n k)
   (if (not (zero? (modulo n k)))
       0
       (let ((a (A000010 k)) (b (/ n k)))
          (* (/ 1 2) (+ (expt (+ a b) 2) (- a) (- (* 3 b)) 2))
       )
   )
)

;; (same-intfuns1? (COMPOSE A000217 -1+ A000040) (lambda (n) (A286237tr (A000040 n) (A000040 n))) 120) --> #t


(define (A286237bi row col)
   (if (not (zero? (modulo (+ row col -1) row)))
       0
       (let ((a (A000010 row)) (b (quotient (+ row col -1) row)))
          (* (/ 1 2) (+ (expt (+ a b) 2) (- a) (- (* 3 b)) 2))
       )
   )
)


;; A053635 [NJAS] o=0: Sum_{d|n} phi(d)*2^(n/d). 

(define (A053635clumsy_way n) (add (lambda (i) (if (zero? (A286236 i)) 0 (* (A002260 (A286236 i)) (A000079 (A004736 (A286236 i))))))  (+ 1 (A000217 (- n 1))) (A000217 n)))

(define (A053635clumsy_way2 n) (add (lambda (i) (if (zero? (A286237 i)) 0 (* (A002260 (A286237 i)) (A000079 (A004736 (A286237 i))))))  (+ 1 (A000217 (- n 1))) (A000217 n)))

(define (A053635clumsy_way3 n) (add (lambda (i) (if (zero? (A286239 i)) 0 (* (A002260 (A286239 i)) (A000079 (A004736 (A286239 i))))))  (+ 1 (A000217 (- n 1))) (A000217 n)))

;; A000031(n) = (1/n)*Sum_{ d divides n } phi(d)*2^(n/d), where phi is A000010.

(define (A000031clumsy_way n) (if (zero? n) 1 (* (/ 1 n) (A053635clumsy_way n))))

(define (A054610_clumsy_way n) (add (lambda (i) (if (zero? (A286237 i)) 0 (* (A002260 (A286237 i)) (A000244 (A004736 (A286237 i))))))  (+ 1 (A000217 (- n 1))) (A000217 n)))

(define (A054610_clumsy_way2 n) (add (lambda (i) (if (zero? (A286239 i)) 0 (* (A002260 (A286239 i)) (A000244 (A004736 (A286239 i))))))  (+ 1 (A000217 (- n 1))) (A000217 n)))

;; (same-intfuns1? A286235 (COMPOSE A286234 A038722) 7260) --> #t
;; (same-intfuns1? A286237 (COMPOSE A286236 A038722) 7260) --> #t
;; (same-intfuns1? A286239 (COMPOSE A286238 A038722) 7260) --> #t

;; (same-intfuns1? A286236 (lambda (n) (* (A113998 n) (A286234 n))) 7260) --> #t
;; (same-intfuns1? A286237 (lambda (n) (* (A051731 n) (A286235 n))) 7260) --> #t

(define (A286238 n) (A286239tr (A002024 n) (A004736 n)))

(define (A286239 n) (A286239tr (A002024 n) (A002260 n)))

(define (A286239tr n k)
   (if (not (zero? (modulo n k)))
       0
       (let ((a (A000010 (/ n k))) (b k))
          (* (/ 1 2) (+ (expt (+ a b) 2) (- a) (- (* 3 b)) 2))
       )
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A046523  & (n/k) packed triangles:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A286244 n) (A286244bi (A002260 n) (A004736 n)))

(define (A286244bi row col)
   (let ((a (A046523 col)) (b (quotient (+ row col -1) col)))
      (* (/ 1 2) (+ (expt (+ a b) 2) (- a) (- (* 3 b)) 2))
   )
)

(define (A286245 n) (A286245bi (A002260 n) (A004736 n)))

(define (A286245bi row col)
   (let ((a (A046523 row)) (b (quotient (+ row col -1) row)))
      (* (/ 1 2) (+ (expt (+ a b) 2) (- a) (- (* 3 b)) 2))
   )
)


(define (A286246 n) (A286246bi (A002260 n) (A004736 n)))

(define (A286246v2 n) (A286246tr (A002024 n) (A002260 n)))
(define (A286246tr n k) (A286246bi k (+ 1 (- n k))))

(define (A286246bi row col)
   (if (not (zero? (modulo (+ row col -1) col)))
       0
       (let ((a (A046523 col)) (b (/ (+ row col -1) col)))
          (* (/ 1 2) (+ (expt (+ a b) 2) (- a) (- (* 3 b)) 2))
       )
   )
)

(define (A286247 n) (A286247bi (A002260 n) (A004736 n)))

(define (A286247v2 n) (A286247tr (A002024 n) (A002260 n)))
;; (define (A286247tr n k) (A286247bi k (+ 1 (- n k))))

(define (A286247tr n k)
   (if (not (zero? (modulo n k)))
       0
       (let ((a (A046523 k)) (b (/ n k)))
          (* (/ 1 2) (+ (expt (+ a b) 2) (- a) (- (* 3 b)) 2))
       )
   )
)

;; (same-intfuns1? (COMPOSE A000217 -1+ A000040) (lambda (n) (A286247tr (A000040 n) (A000040 n))) 120) --> #t


(define (A286247bi row col)
   (if (not (zero? (modulo (+ row col -1) row)))
       0
       (let ((a (A046523 row)) (b (quotient (+ row col -1) row)))
          (* (/ 1 2) (+ (expt (+ a b) 2) (- a) (- (* 3 b)) 2))
       )
   )
)


;; A053635 [NJAS] o=0: Sum_{d|n} phi(d)*2^(n/d). 

(define (A027375clumsy_way n) (add (lambda (i) (if (zero? (A286246 i)) 0 (* (A008683 (A002260 (A286246 i))) (A000079 (A004736 (A286246 i))))))  (+ 1 (A000217 (- n 1))) (A000217 n)))

(define (A027375clumsy_way2 n) (add (lambda (i) (if (zero? (A286247 i)) 0 (* (A008683 (A002260 (A286247 i))) (A000079 (A004736 (A286247 i))))))  (+ 1 (A000217 (- n 1))) (A000217 n)))

(define (A027375clumsy_way3 n) (add (lambda (i) (if (zero? (A286249 i)) 0 (* (A008683 (A002260 (A286249 i))) (A000079 (A004736 (A286249 i))))))  (+ 1 (A000217 (- n 1))) (A000217 n)))

;; A001037(n) = (1/n)*Sum_{ d divides n } mu(d)*2^(n/d), where mu is A008683.

(define (A001037clumsy_way n) (if (zero? n) 1 (* (/ 1 n) (A027375clumsy_way n))))

(define (A054718_clumsy_way n) (if (zero? n) 1 (add (lambda (i) (if (zero? (A286247 i)) 0 (* (A008683 (A002260 (A286247 i))) (A000244 (A004736 (A286247 i))))))  (+ 1 (A000217 (- n 1))) (A000217 n))))

(define (A054718_clumsy_way2 n) (if (zero? n) 1 (add (lambda (i) (if (zero? (A286249 i)) 0 (* (A008683 (A002260 (A286249 i))) (A000244 (A004736 (A286249 i))))))  (+ 1 (A000217 (- n 1))) (A000217 n))))

;; A027376(n) = (1/n)*Sum_{d|n} mu(d)*3^(n/d). 

(define (A027376clumsy_way n) (if (zero? n) 1 (* (/ 1 n) (A054718_clumsy_way n))))

;; (same-intfuns1? A286245 (COMPOSE A286244 A038722) 7260) --> #t
;; (same-intfuns1? A286247 (COMPOSE A286246 A038722) 7260) --> #t
;; (same-intfuns1? A286249 (COMPOSE A286248 A038722) 7260) --> #t

;; (same-intfuns1? A286246 (lambda (n) (* (A113998 n) (A286244 n))) 7260) --> #t
;; (same-intfuns1? A286247 (lambda (n) (* (A051731 n) (A286245 n))) 7260) --> #t

(define (A286248 n) (A286249tr (A002024 n) (A004736 n)))

(define (A286249 n) (A286249tr (A002024 n) (A002260 n)))

(define (A286249tr n k)
   (if (not (zero? (modulo n k)))
       0
       (let ((a (A046523 (/ n k))) (b k))
          (* (/ 1 2) (+ (expt (+ a b) 2) (- a) (- (* 3 b)) 2))
       )
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A286379 n) (if (= 1 n) n (* (/ 1 2) (+ (expt (+ (A032742 n) (A278222 n)) 2) (- (A032742 n)) (- (* 3 (A278222 n))) 2))))

;; A286234-A286260 are now reserved for your use. 

(define (A286240 n) (* (/ 1 2) (+ (expt (+ (A278222 n) (A278222 (+ 1 n))) 2) (- (A278222 n)) (- (* 3 (A278222 (+ 1 n)))) 2))) ;; o=0 !!!

(define (A286241 n) (* (/ 1 2) (+ (expt (+ (A278219 n) (A278219 (+ 1 n))) 2) (- (A278219 n)) (- (* 3 (A278219 (+ 1 n)))) 2))) ;; o=0 !!!

(define (A286242 n) (* (/ 1 2) (+ (expt (+ (A278222 n) (A278219 n)) 2) (- (A278222 n)) (- (* 3 (A278219 n))) 2))) ;; o=0 !!!


(define (A286251 n) (* (/ 1 2) (+ (expt (+ (A001511 (+ 1 n)) (A046523 n)) 2) (- (A001511 (+ 1 n))) (- (* 3 (A046523 n))) 2)))

(define (A286252 n) (* (/ 1 2) (+ (expt (+ (A001511 (+ 1 n)) (A278222 n)) 2) (- (A001511 (+ 1 n))) (- (* 3 (A278222 n))) 2))) ;; o=0 !

(define (A286253 n) (* (/ 1 2) (+ (expt (+ (A055396 n) (A001511 (+ 1 n))) 2) (- (A055396 n)) (- (* 3 (A001511 (+ 1 n)))) 2)))

(define (A286254 n) (* (/ 1 2) (+ (expt (+ (A001511 n) (A055396 (+ 1 n))) 2) (- (A001511 n)) (- (* 3 (A055396 (+ 1 n)))) 2)))


(define (A286255 n) (* (/ 1 2) (+ (expt (+ (A046523 n) (A046523 (+ 1 n))) 2) (- (A046523 n)) (- (* 3 (A046523 (+ 1 n)))) 2)))

(define (A286256 n) (* (/ 1 2) (+ (expt (+ (A046523 n) (A046523 (+ 2 n))) 2) (- (A046523 n)) (- (* 3 (A046523 (+ 2 n)))) 2))) ;; 5's at lesser twin primes.

(define (A286257 n) (* (/ 1 2) (+ (expt (+ (A046523 n) (A046523 (+ -1 n n))) 2) (- (A046523 n)) (- (* 3 (A046523 (+ -1 n n)))) 2)))

(define (A286257v2 n) (* (/ 1 2) (+ (expt (+ (A046523 n) (A278223 n)) 2) (- (A046523 n)) (- (* 3 (A278223 n))) 2)))


(define (A286258 n) (* (/ 1 2) (+ (expt (+ (A046523 n) (A046523 (+ 1 n n))) 2) (- (A046523 n)) (- (* 3 (A046523 (+ 1 n n)))) 2)))

(define (A286259 n) (* (/ 1 2) (+ (expt (+ (A001511 n) (A049820 n)) 2) (- (A001511 n)) (- (* 3 (A049820 n))) 2)))

(define (A286260 n) (* (/ 1 2) (+ (expt (+ (A001511 n) (A161942 n)) 2) (- (A001511 n)) (- (* 3 (A161942 n))) 2)))


(define (A286451 n) (if (= 1 n) 0 (* (/ 1 2) (+ (expt (+ (A286357 n) (A001511 n)) 2) (- (A286357 n)) (- (* 3 (A001511 n))) 2))))

(define (A286452 n) (* (/ 1 2) (+ (expt (+ (A061395 n) (A278223 n)) 2) (- (A061395 n)) (- (* 3 (A278223 n))) 2)))
(define (A286452v2 n) (* (/ 1 2) (+ (expt (+ (A061395 n) (A046523 (+ n n -1))) 2) (- (A061395 n)) (- (* 3 (A046523 (+ n n -1)))) 2)))

(define (A286453 n) (* (/ 1 2) (+ (expt (+ (A061395 n) (A286465 n)) 2) (- (A061395 n)) (- (* 3 (A286465 n))) 2)))


(definec (A286454 n) (* (/ 1 2) (+ (expt (+ (A101296 n) (A286621 n)) 2) (- (A101296 n)) (- (* 3 (A286621 n))) 2)))

(definec (A286455 n) (* (/ 1 2) (+ (expt (+ (A055396 n) (A286621 n)) 2) (- (A055396 n)) (- (* 3 (A286621 n))) 2)))

(definec (A286456 n) (if (= 1 n) 0 (* (/ 1 2) (+ (expt (+ (A056239 n) (A243503 n)) 2) (- (A056239 n)) (- (* 3 (A243503 n))) 2))))


(definec (A286457 n) (if (= 1 n) 0 (* (/ 1 2) (+ (expt (+ (A078898 n) (A246277 n)) 2) (- (A078898 n)) (- (* 3 (A246277 n))) 2))))


;; E.g.:
;; (define vecA286448 (read-b-file-to-vector "seqs2/b286448_upto20000.txt" 20001))

;; (define vecA286449 (read-b-file-to-vector "seqs2/b286449_upto20000.txt" 20001))

;; (define (A286448 n) (vector-ref vecA286448 n))

;; (define (A286449 n) (vector-ref vecA286449 n))

(define (A286458 n) (* (/ 1 2) (+ (expt (+ (A286448 n) (A286449 n)) 2) (- (A286448 n)) (- (* 3 (A286449 n))) 2)))

(define (A286459 n) (A286458 (A064216 n)))



(define (A286243 n) (A278222 (A064216 n)))

(define (A286250 n) (A278223 (A064216 n)))
(define (A286250v2 n) (A046523 (+ -1 (* 2 (A064216 n)))))
(define (A286250v3 n) (A046523 (A245448 n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A286160 n) (* (/ 1 2) (+ (expt (+ (A000010 n) (A046523 n)) 2) (- (A000010 n)) (- (* 3 (A046523 n))) 2)))

(define (A286161 n) (* (/ 1 2) (+ (expt (+ (A001511 n) (A046523 n)) 2) (- (A001511 n)) (- (* 3 (A046523 n))) 2)))


(define (A286162 n) (* (/ 1 2) (+ (expt (+ (A001511 n) (A278222 n)) 2) (- (A001511 n)) (- (* 3 (A278222 n))) 2)))

(define (A286163 n) (* (/ 1 2) (+ (expt (+ (A046523 n) (A278222 n)) 2) (- (A046523 n)) (- (* 3 (A278222 n))) 2)))

(define (A286164 n) (* (/ 1 2) (+ (expt (+ (A055396 n) (A046523 n)) 2) (- (A055396 n)) (- (* 3 (A046523 n))) 2)))


(define (A286142 n) (* (/ 1 2) (+ (expt (+ (A257993 n) (A046523 n)) 2) (- (A257993 n)) (- (* 3 (A046523 n))) 2)))

(define (A286143 n) (* (/ 1 2) (+ (expt (+ (A055881 n) (A046523 n)) 2) (- (A055881 n)) (- (* 3 (A046523 n))) 2)))

(define (A286144 n) (* (/ 1 2) (+ (expt (+ (A000010 n) (A257993 n)) 2) (- (A000010 n)) (- (* 3 (A257993 n))) 2)))

(define (A286152 n) (* (/ 1 2) (+ (expt (+ (A051953 n) (A046523 n)) 2) (- (A051953 n)) (- (* 3 (A046523 n))) 2)))

(define (A286154 n) (* (/ 1 2) (+ (expt (+ (A055396 n) (A000010 n)) 2) (- (A055396 n)) (- (* 3 (A000010 n))) 2)))

(define (A286149 n) (* (/ 1 2) (+ (expt (+ (A046523 n) (A109395 n)) 2) (- (A046523 n)) (- (* 3 (A109395 n))) 2)))

(define (A285729 n) (* (/ 1 2) (+ (expt (+ (A032742 n) (A046523 n)) 2) (- (A032742 n)) (- (* 3 (A046523 n))) 2)))

(define (A286034 n) (* (/ 1 2) (+ (expt (+ (A046523 n) (A161942 n)) 2) (- (A046523 n)) (- (* 3 (A161942 n))) 2)))


;; Cf. A285729
(define (A286472 n) (if (= 1 n) n (+ (* 2 (A032742 n)) (if (> (A286471 n) 2) 1 0)))) ;; Count gaps.

(define (A286473 n) (if (= 1 n) n (+ (* 4 (A032742 n)) (modulo (A020639 n) 4)))) ;; Count 4k+1, 4k+3 primes. (Also 2 may occur)

(define (A286474 n) (if (= 1 n) n (+ (* 4 (A032742 n)) (modulo n 4)))) ;; Experimental.

(define (A286475 n) (if (= 1 n) n (+ (* 6 (A032742 n)) (modulo (A020639 n) 6)))) ;; Count 6k+1, 6k+5 primes. (Also 2 and 3 may occur)

(define (A286476 n) (if (= 1 n) n (+ (* 6 (A032742 n)) (modulo n 6)))) ;; Experimental.

;; (define (Anot_submitted n) (if (= 1 n) n (+ (* 2 (A032742 n)) (if (> (A286471 n) 1) 1 0))))


(define (A286386 n) (+ (* 2 (A286473 n)) (A010052 n)))

(define (A286388 n) (if (= 1 n) 0 (* (/ 1 2) (+ (expt (+ (A032742 n) (A001511 (+ 1 n))) 2) (- (A032742 n)) (- (* 3 (A001511 (+ 1 n)))) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (sort-dups lista)
  (sort lista (lambda (a b) (cond ((equal? a b) (format #t "Samat: ~a\n" a))) (< (car a) (car b))))
)

;; Use this:
;; (sort (map (lambda (n) (cons (A000203 n) (A051953 n))) (iota 120)) (lambda (a b) (cond ((equal? a b) (format #t "Samat: ~a\n" a))) (< (car a) (car b))))


(define (A286356 n) (* (/ 1 2) (+ (expt (+ (A061395 n) (A046523 n)) 2) (- (A061395 n)) (- (* 3 (A046523 n))) 2)))


(define (A286358 n) (* (/ 1 2) (+ (expt (+ (A286357 n) (A161942 n)) 2) (- (A286357 n)) (- (* 3 (A161942 n))) 2)))

(define (A286359 n) (* (/ 1 2) (+ (expt (+ (A000203 n) (A000203 (* 2 n))) 2) (- (A000203 n)) (- (* 3 (A000203 (* 2 n)))) 2)))

(define (A286360 n) (* (/ 1 2) (+ (expt (+ (A046523 n) (A000203 n)) 2) (- (A046523 n)) (- (* 3 (A000203 n))) 2)))

(define (A286460 n) (* (/ 1 2) (+ (expt (+ (A001511 n) (A000203 n)) 2) (- (A001511 n)) (- (* 3 (A000203 n))) 2))) ;; Cf. A286260, A286460.

;; Cf. A286161.
(define (A286461 n) (* (/ 1 2) (+ (expt (+ (A001511 n) (A286364 (+ n n -1))) 2) (- (A001511 n)) (- (* 3 (A286364 (+ n n -1)))) 2)))

;; Note: (same-intfuns1? A001511 (COMPOSE A051064 A003961) 1200) --> #t

(define (A286462 n) (* (/ 1 2) (+ (expt (+ (A051064 n) (A089309 n)) 2) (- (A051064 n)) (- (* 3 (A089309 n))) 2)))


(define (A286463 n) (* (/ 1 2) (+ (expt (+ (A051064 n) (A046523 n)) 2) (- (A051064 n)) (- (* 3 (A046523 n))) 2)))

(define (A286464 n) (* (/ 1 2) (+ (expt (+ (A051064 n) (A278222 n)) 2) (- (A051064 n)) (- (* 3 (A278222 n))) 2)))

(define (A286465 n) (if (= 1 n) n (* (/ 1 2) (+ (expt (+ (A112049 (- n 1)) (A046523 (+ -1 n n))) 2) (- (A112049 (- n 1))) (- (* 3 (A046523 (+ -1 n n)))) 2))))

(define (A286466 n) (* (/ 1 2) (+ (expt (+ (A112049 n) (A046523 n)) 2) (- (A112049 n)) (- (* 3 (A046523 n))) 2)))





(define (A286362 n) (* (/ 1 2) (+ (expt (+ (A089309 n) (A046523 n)) 2) (- (A089309 n)) (- (* 3 (A046523 n))) 2)))

;; (same-intfuns0? (COMPOSE (lambda (n) (let ((a (A002260 (A286251 n))) (b (A004736 (A286251 n)))) (A000027bi (- a 1) b))) A005408) (COMPOSE A286362 A005408) 1200) --> #t


(define (A286361 n) (A046523 (A170818 n)))

(define (A286363 n) (A046523 (A097706 n)))

(define (A286364 n) (* (/ 1 2) (+ (expt (+ (A286361 n) (A286363 n)) 2) (- (A286361 n)) (- (* 3 (A286363 n))) 2)))

;; (same-intfuns1? (COMPOSE (lambda (n) (cons (A002260 n) (A004736 n))) A286364) (lambda (n) (cons (A286361 n) (A286363 n))) 1200) --> #t

(define A004431v2 (MATCHING-POS 1 1 (lambda (n) (and (> (A002260 (A286364 n)) 1) (= 1 (A010052 (A004736 (A286364 n))))))))

;; (same-intfuns1? A004431 A004431v2 500) --> #t


(define (A286365 n) (+ (* 2 (A286364 n)) (A000035 (A007814 n))))

(define (A286366 n) (+ (* 2 (A286365 n)) (floor->exact (/ (A072400 n) 4))))

(define (A286367 n) (* (/ 1 2) (+ (expt (+ (A001511 n) (A286364 n)) 2) (- (A001511 n)) (- (* 3 (A286364 n))) 2)))


(define A000290v2 (MATCHING-POS 1 1 (lambda (n) (and (even? (A286365 n)) (= 1 (A010052 (A002260 (/ (A286365 n) 2)))) (= 1 (A010052 (A004736 (/ (A286365 n) 2))))))))

(define (A286368 n) (+ (* 4 (A072401 n)) (* 2 (A229062 n)) (A010052 n)))

(define (A286369 n) (+ (* 2 (A286364 n)) (floor->exact (/ (A072400 n) 4))))

(define (A286370 n) (A286369 (A139391 n)))

(define (A286371 n) (A286369 (A003961 n)))

(define (A286372 n) (A286366 (A064216 n)))

(define (A286373 n) (A286366 (A048673 n)))

(define (A286374 n) (A278222 (* n n))) ;; o=0: Cf. A159918.

(define (A286375 n) (A278222 (* n n n))) ;; o=0: Cf. A192085.

(define (A286376 n) (A278222 (A277699 n))) ;; o=1: Cf. A278239.

(define (A286377 n) (A278243 (* n n)))

(define (A286387 n) (A002487 (* n n)))


;; For factorial base:
(define (A286381 n) (* (/ 1 2) (+ (expt (+ (A055881 n) (A278236 n)) 2) (- (A055881 n)) (- (* 3 (A278236 n))) 2)))

;; For primorial base:
(define (A286382 n) (* (/ 1 2) (+ (expt (+ (A257993 n) (A278226 n)) 2) (- (A257993 n)) (- (* 3 (A278226 n))) 2)))

;; For GF(2)[X] factorization, Gray code, etc:
(define (A286383 n) (A278233 (A003188 n)))

(define (A286384 n) (A278233 (A006068 n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A286101 n) (A286101bi (A002260 n) (A004736 n)))
(define (A286101bi row col) (A000027bi (gcd row col) (lcm row col)))


(define (A286146 n) (A286101bi (A002024 n) (A002260 n))) ;; Lower triangular region of A286101.
(define (A286148 n) (A286101bi (A002024 n) (A004736 n))) ;; Lower triangular region of A286101, transposed.


(define (A286102 n) (A286102bi (A002260 n) (A004736 n)))
(define (A286102bi row col) (A000027bi (lcm row col) (gcd row col)))

(define (A285724 n) (A285724bi (A002260 n) (A004736 n)))
(define (A285724bi row col) (if (> row col) (A000027bi (lcm row col) (gcd row col)) (A000027bi (gcd row col) (lcm row col))))

(define (A285724v2 n) (A285724v2bi (A002260 n) (A004736 n)))
(define (A285724v2bi row col) (if (< row col) (A286101bi row col) (A286102bi row col)))

;; (same-intfuns1? A285724 A285724v2 10585) --> #t



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XFER: Collatz.ss
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A014682 [Mohammad K. Azarian] o=0: The Collatz or 3x+1 function: a(n) = n/2 if n is even, otherwise (3n+1)/2.
(define (A014682 n) (if (even? n) (/ n 2) (/ (+ n n n 1) 2)))

;; A078719 [Joseph L. Pe] o=1: Number of odd terms among n, f(n), f(f(n)), ...., 1 for the Collatz function (that is, until reaching "1" for the first time).
(define (A078719 n) (+ (A286380 n) (A000035 n)))

;; A075680 [Noe] o=1: For odd numbers 2n-1, the minimum number of iterations of the reduced Collatz function R required to yield 1. The function R is defined as R(k) = (3k+1)/2^r, with r as large as possible.
(define (A075680 n) (A286380 (+ n n -1)))

;; a(n) = the minimum number of iterations of the reduced Collatz function R required to yield 1. The function R (A139391) is defined as R(k) = (3k+1)/2^r, with r as large as possible.

(definec (A286380 n) (if (= 1 n) 0 (+ 1 (A286380 (A139391 n))))) ;; Bisection: A075680.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;

;; A201555 [Paul D. Hanna] o=0: a(n) = C(2*n^2,n^2) = A000984(n^2), where A000984 is the central binomial coefficients.

(define (A201555 n) (A000984 (* n n)))

;; A001790 [NJAS] o=0: Numerators in expansion of 1/sqrt(1-x). 
(define (A001790 n) (A000265 (A000984 n)))

;; A023816 [Kimberling] o=1: Sum of exponents in prime-power factorization of C(2n,n).  (XXX - Should have a(0) = 0 ?)
(define (A023816 n) (A001222 (A000984 n)))

;; A082481 [Cloitre] o=0: Number of 1's in binary representation of C(2n,n). 
(define (A082481 n) (A000120 (A000984 n)))



;; (define (Auusfiltteri n) (A286364 (A000984 n))) ;; o=0. (Has some duplicated values). Needs rgs-variant also.
;; (COMPOSE A278222 A000984) might have some duplicates as well.


;; A159918 [Reinhard Zumkeller] o=0: Number of ones in binary representation of n^2. 
(define (A159918 n) (A000120 (* n n)))


(define (A285717 n) (+ (A007814 n) (A159918 n)))
(define (A285717v2 n) (A007814 (* n (A201555 n))))

(define (A285388 n) (/ (* n (A201555 n)) (A000079 (A285717 n))))
(define (A285388v2 n) (numerator (/ (* n (A201555 n)) (/ (A060757 n) 2))))

(define (A285389 n) (denominator (/ (* n (A201555 n)) (/ (A060757 n) 2))))

(define (A285406 n) (A000523 (A285389 n)))
(define (A285406v2 n) (- (A056220 n) (A285717 n)))
(define (A285406v3 n) (- (* 2 n n) (A007814 n) (A000120 (* n n)) 1)) ;; (2*(n^2)) - A007814(n) - A000120(n^2) - 1



(define (A056220 n) (+ -1 (* 2 n n))) ;; [NJAS] o=0: a(n) = 2*n^2-1.

(define (A060757 n) (expt 4 (* n n))) ;; [Ahmed Fares] o=0: a(n) = 4^(n^2). 
(define (A060757v2 n) (A000079 (* 2 n n)))


;;;;;;

(define (A285730 n) (A285730bi (A002260 n) (A004736 n)))

(define (A285730bi row col) (let loop ((n row) (k col) (m 1)) (if (zero? k) m (loop (/ n (A006530 n)) (- k 1) (* m (A006530 n))))))

(define (A285731 n) (A285730bi (A004736 n) (A002260 n)))

(define (A285730v2 n) (A285730biv2 (A002260 n) (A004736 n)))
(define (A285730biv2 row col) (if (= 1 col) (A006530 row) (* (A006530 row) (A285730biv2 (A052126 row) (- col 1)))))

(define (A001105 n) (* 2 n n ))
(define (A285786 n) (if (= 1 n) n (- (A000720 (A001105 n)) (A000720 (A001105 (- n 1))))))

;; See https://math.stackexchange.com/questions/20564/sums-of-square-free-numbers-is-this-conjecture-equivalent-to-goldbachs-conjec?rq=1
;; ... this implies that n+1 can be written as the sum of exactly two squarefree numbers.

;; and http://www.jstor.org/stable/2040089

(definec (A285718 n)
   (if (= 1 n)
       0
       (let loop ((k 1))
          (if (not (zero? (A008683 (- n (A005117 k)))))
              (A005117 k)
              (loop (+ 1 k))
          )
       )
   )
)

;; A070321 [Benoit Cloitre] o=1: Greatest squarefree number <= n. 

(definec (A070321 n) (let loop ((k n)) (if (not (zero? (A008683 k))) k (loop (- k 1)))))

(define (A285719 n) (- n (A285718 n))) ;; Not the same as A070321. Not indeed.

(define (A285719v2 n)
   (if (= 1 n)
       n
       (let loop ((k (A013928 n)))
          (if (not (zero? (A008683 (- n (A005117 k)))))
              (A005117 k)
              (loop (- k 1))
          )
       )
   )
)


;; (same-intfuns1? A070321 (COMPOSE A285719 1+) 120) --> 50
;; (A285719 51) --> 46
;; (A070321 50) --> 47

(definec (A285734slow n)
   (if (= 1 n)
       0
       (let loop ((j 1) (k (- n 1)) (s 0))
          (if (> j k)
              s
              (loop (+ 1 j) (- k 1) (max s (* j (A008966 j) (A008966 k))))
          )
       )
   )
)

(definec (A285734 n)
   (if (= 1 n)
       0
       (let loop ((j (floor->exact (/ n 2))))
          (if (and (= 1 (A008966 j)) (= 1 (A008966 (- n j))))
              j
              (loop (- j 1))
          )
       )
   )
)

(define (A285735 n) (- n (A285734 n)))

(define (A285736 n) (- (A285735 n) (A285734 n)))
(define (A285736v2 n) (- n (* 2 (A285734 n))))

;; A286098-A286109 are now reserved for your use. 

(definec (A286103 n) (if (= 1 n) 0 (+ 1 (min (A286103 (A285734 n)) (A286103 (A285735 n))))))

;; If A286103(A285734(n)) < A286103(A285735(n)), a(n) = A285734(n), otherwise a(n) = A285735(n), a(1) = 0.

(definec (A286103v2 n) (if (= 1 n) 0 (+ 1 (A286103v2 (A286104 n)))))

(define (A286104 n) (cond ((= 1 n) 0) ((< (A286103 (A285734 n)) (A286103 (A285735 n))) (A285734 n)) (else (A285735 n))))

(definec (A286105 n) (if (= 1 n) 0 (+ 1 (max (A286105 (A285734 n)) (A286105 (A285735 n))))))

(definec (A286105v2 n) (if (= 1 n) 0 (+ 1 (A286105v2 (A286107 n)))))

;; (same-intfuns1? (COMPOSE A286103 double A005117) (COMPOSE 1+ A286103 A005117) 5000) --> #t

;; (same-intfuns1? (COMPOSE A286104 double A005117) (COMPOSE 1+ A286104 A005117) 3000) --> #t

;; (same-intfuns1? (COMPOSE A286105 double A005117) (COMPOSE 1+ A286105 A005117) 5000) --> #t


(define (A286106 n) (if (= 1 n) 0 (- (A286105 (A285735 n)) (A286105 (A285734 n)))))

(define (A286107 n) (cond ((= 1 n) 0) ((> (A286106 n) 0) (A285735 n)) (else (A285734 n))))


;; A071068 [Benoit Cloitre] o=1: Number of ways to write n as a sum of two unordered squarefree numbers.

(definec (A071068 n)
  (let loop ((k (A013928 n)) (s 0))
       (if (or (zero? k) (< (A005117 k) (- n (A005117 k))))
           s
           (loop (- k 1) (+ s (A008966 (- n (A005117 k)))))
       )
  )
)

(definec (A285720 n)
  (let loop ((k (A013928 n)) (s 0))
       (if (or (zero? k) (< (A005117 k) (- n (A005117 k))))
           s
           (loop (- k 1)
                 (+ s (if (and (= 1 (A008966 (- n (A005117 k)))) (zero? (A004198bi (A005117 k) (- n (A005117 k))))) 1 0))
           )
       )
  )
)



;; A088512 [Naohiro Nomoto] o=0: Number of partitions of n into two parts whose xor-sum is n. 

(define (A088512 n)
 (if (zero? n)
     n
     (let loop ((k (- n 1)) (s 0))
         (if (< k (- n k))
             s
             (loop (- k 1)
                   (+ s (if (zero? (A004198bi k (- n k))) 1 0))
             )
         )
     )
 )
)

;;;;;;;

(define (A286553 n) (A046523 (A252753 n)))

;; Note: A252753(n) = A250245(A005940(1+n)).
;; and A005940(1+n) = A250246(A252753(n-1)).
;; Also A005940(1+n) = A243353(A006068(n)).
;; And A278222(n) = A046523(A005940(1+n)).
;; A278219(n) = A278222(A003188(n)).
;; A278524(n) = A046523(A250246(n)). 

(define (A286555 n) (A252753 (A003188 n))) ;; o=0:
(define (A286556 n) (A006068 (A252754 n))) ;; o=1:

;; (same-intfuns0? A001477 (COMPOSE A286556 A286555) 512) --> #t

;; (same-intfuns1? A001477 (COMPOSE A286555 A286556) 65) --> #t

(define (A286557 n) (A046523 (A286555 n))) ;; o=0:


;; (same-intfuns0? A286553 (COMPOSE A286557 A006068) 4096) --> #t

;; (same-intfuns0? A286557 A286557v2 4096) --> #t
;; (same-intfuns0? A286557 (COMPOSE A286553 A003188) 1200) --> #t


;;;;;;;


;; A005070 [NJAS] o=1: Sum of primes = 1 (mod 3) dividing n. 
;; Additive with a(p^e) = p if p = 1 (mod 3), 0 otherwise. 

(definec (A005070 n) (if (= 1 n) 0 (+ (if (= 1 (modulo (A020639 n) 3)) (A020639 n) 0) (A005070 (A028234 n)))))

