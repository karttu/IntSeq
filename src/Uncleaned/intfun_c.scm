
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;; intfun_c.scm                                                           ;;
;;  - Some new integer functions, the rest from old intfun_b.scm          ;;
;;                                                                        ;;
;;  Coded by Antti Karttunen (my_firstname.my_surname@gmail.com),         ;;
;;   2002-2015                                                            ;;
;;  The permutation & partition generation functions should be cleanly    ;;
;;  divided into their own modules, and placed somewhere under            ;;
;;  https://github.com/karttu/IntSeq                                      ;;
;;                                                                        ;;
;;  Start with scheme --heap 13000                                        ;;
;;  if encountering "Out of memory" errors when compiling.                ;;
;;                                                                        ;;
;;  This is the most unclean of all intfun_?.scm RAW sources.             ;;
;;  (some sequences to be renumbered). Also, the current hasty            ;;
;;  implementation of A258012 fails with larger values of n.              ;;
;;                                                                        ;;
;;  Last edited 2017-06-28.                                               ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(declare (usual-integrations))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A220655-A220664 are now reserved for your use.

;; (define A220657 (PARTIALSUMS 0 0 (compose-funs 1+ A084558)))
;; (define A220658 (COMPOSE (LEAST-GTE-I 1 0 A220657) 1+))
(define A220657 (PARTIALSUMS 1 0 (compose-funs 1+ A084558)))
;; A220658: Each n occurs A084558(n)+1 times
;; Equally, for n>=1, each i in range [n!,(n+1)!-1] occurs n times.
;; 0,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,6,7,7,7,7,9,9,9,9
(define A220658 (COMPOSE (LEAST-GTE-I 1 0 (COMPOSE A220657 1+)) 1+))
(define (A220659 n) (- n (A220657 (A220658 n))))

;; Numbers 0..(n!-1) followed by numbers 0..((n+1)!-1).
(define (A220660 n) (- n (A007489 (-1+ (A084556 n))) 1)) ;; Offset=1.

;; Numbers 1..(n!) followed by numbers 1..(n+1)!.
(define (A220661 n) (- n (A007489 (-1+ (A084556 n))))) ;; Offset=1.

;; A084558+1:
;; 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,  2, 2, 2, 2, 2, 2,  2, 2, 2, 2, 2, 2, 3, 3, 3, ...
;; 0; 0,1; 0,1; 0,1,2; 0,1,2

;; The beginning position of each permutation in A030298: 1,2,2,4,4,6,6,6,9,9,9,12,12,12,15,15,15,18
(define (A220662 n) (1+ (A084555 (-1+ (A084557 n)))))

(define (A220663 n) (- n (A220662 n)))

(define (A130664 n) (+ 1 (A084555(- n 1))))


;; (define (A055089 n) (vector-ref (A055089permvec-short (A220658 n)) (A220659 n)))
(definec (A055089 n) (vector-ref (A055089permvec-short (A220658 n)) (A220659 n)))
(definec (A055089permvec-short rank) (A055089permvec  (+ 1 (A084558 rank)) rank))
;; (define (A055089 n) (vector-ref (A055089permvec (+ 1 (A084558 (A220658 n))) (A220658 n)) (A220659 n)))

(define (A060118 n) (vector-ref (permute-A060118 (make-initialized-vector (+ 1 (A084558 n)) 1+) (+ 1 (A084558 n)) (A220658 n)) (A220659 n)))

(define (A060118permvec-short rank) (permute-A060118 (make-initialized-vector (+ 1 (A084558 rank)) 1+) (+ 1 (A084558 rank)) rank))

(define (A060117permvec-short rank) (permvec1inverse-of (permute-A060118 (make-initialized-vector (+ 1 (A084558 rank)) 1+) (+ 1 (A084558 rank)) rank)))


(define (A030298 n) (vector-ref (A030298permvec (A084556 (A084557 n)) (A220660 (A084557 n))) (A220663 n)))

;; (A030299 409113) = 987654321 last valid term. 409113 = A007489(9)

(define (A030299 n) (vector->base-k (A030298permvec (A084556 n) (A220660 n)) 10))
(define (A220664 n) (- (A030299 (+ 1 n)) (A030299 n)))
;; (define (A107346 n) (A220664 (A220655 n))) ;; one-based.
(define (A219664 n) (A220664 (A220655 n))) ;; one-based.

(define (A217626 n) (/ (A107346 n) 9)) ;; As well.
(define A215940 (COMPOSE (PARTIALSUMS 1 1 A217626) -1+))

(define A090529 (LEAST-GTE-I 0 0 (lambda (n) (if (< n 1) -1 (A000142 n)))))
(define (A090529v2 n) (if (< n 2) 1 (A084556 (A220656 (- n 1)))))

(define (A051845 n) (vector->base-k (A030298permvec (A084556 n) (A220660 n)) (1+  (A084556 n))))
(define (A220689 n) (- (A051845 (+ 1 n)) (A051845 n)))
(define (A220690 n) (A220689 (A220655 n))) ;; one-based.

(define (A220691renumber n) (/ (A220690 n) (A090529 (1+ n))))
(define (A220691v2 n) ((lambda (y) (/ (A220689 y) (A084556 y))) (A220655 n)))

(define (A220692renumber n) (A220691 (A000142 n))) ;; 1,3,13,81,689,7547,101721,1632373, ... Records in A220691? check!

;; Triangle of subrecords...
;; E.g. row 9 begins as: 1,9,78,657,5336,41015,286694,1632373, (A217626 o A000142)


;; A212598: a(n) = n - m!, where m is the largest number such that m! <= n.
;; Numbers 0..A001563(n)-1 followed by numbers 0..A001563(n+1)-1, etc.
;; 0,0,1,2,3,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,0,1,2,3,4,5,6,7,8,9,10,11,
(define (A212598 n) (- n (A000142 (A084558 n)))) ;; One-based.


(define (A220655 n) (+ n (A007489 (A084558 n))))
;; Intermediate: (define (A220655 n) (+ n (A003422 (+ 1 (A084558 n))) -1))

(define (A220655v3 n) (+ (A003422 (+ 1 (A084558 n))) (A000142 (A084558 n)) (A212598 n) -1)) ;; Old, stupid way.
;; 2,5,6,7,8,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,...
;; Take first 1 from A003422(2) (i.e. 2)
;; Take next 4 from A003422(3)+1,...+4 (i.e. 4+1,...4+4, 5..8)
;; Take next 18 from A003422(4)+5,...,+22 (i.e. 10+5,...,10+22,, 15..32)


(define (A220655v2 n) ;; Rise up the factorial expansion
   (let loop ((n n) (z 0) (i 2) (f 1))
      (cond ((zero? n) z)
            (else (loop (quotient n i)
                        (+ (* f (+ 1 (remainder n i))) z)
                        (+ 1 i)
                        (* f i)
                  )
            )
      )
   )
)


;; Those i, for which the first digit of A030299(i) is  not 1.

(define (A220656 n) (+ n (A003422 (+ 1 (A084558 n)))))
(define (A220656v3 n) (+ (A003422 (+ 1 (A084558 n))) (A000142 (A084558 n)) (A212598 n))) ;; Old, stupid way.
(define (A220656v2 n) (+ 1 (A220655 n))) ;; 3,6,7,8,9,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,58,

(define A220695 (COMPLEMENT 1 A220655)) ;; 1,3,4,9,10,11,12,13,14,33,34,35,36,37,38,39,40,...
(define A220696 (COMPLEMENT 1 A220656)) ;; Those i, for which the first digit of A030299(i) is 1.
(define (A220696v2 n) (if (< n 2) n (+ 1 (A220695 (- n 1))))) ;; 1,2,4,5,10,11,12,13,14,15,34,35,36,37,38,39,...

;; A220689-A220698 are now reserved for your use.
;; 
(define (A220694 n) (+ 1 (A220663 n)))


(define (A055089permvec size rank)
  (let ((permvec (make-initialized-vector size 1+)))
   (let outloop ((rank rank)
                 (i 2)
                )
        (cond ((zero? rank) (permvec1inverse-of permvec))
              (else (let inloop ((k (- i 1))) ;; for k from i-1 by -1 to i-(r mod i) do p:= permul([[k,k+1]],p); od;
                         (cond ((< k (- i (remainder rank i)))
                                   (outloop (floor->exact (/ rank i)) (+ 1 i))
                               )
                               (else
                                  (begin
                                     (let ((tmp (vector-ref permvec (- k 1))))
                                        (vector-set! permvec (- k 1) (vector-ref permvec k))
                                        (vector-set! permvec k tmp)
                                     )
                                     (inloop (- k 1))
                                  )
                               )
                         )
                    )
              )
        )
   )
  )
)

(define (permvec1find-pos-of-i-from i permvec)
   (let loop ((k 0))
        (cond ((= k (vector-length permvec)) #f) ;; Not found.
              ((= i (vector-ref permvec k)) (+ 1 k)) ;; Found it.
              (else (loop (+ k 1)))
        )
   )
)

(define (permvec1inverse-of permvec)
    (make-initialized-vector (vector-length permvec) (lambda (i) (permvec1find-pos-of-i-from (+ 1 i) permvec)))
)

(define (vector-reverse vec)
   (make-initialized-vector (vector-length vec) (lambda (i) (vector-ref vec (- (vector-length vec) i 1))))
)

(define (vector0invert vec)
   (make-initialized-vector (vector-length vec) (lambda (i) (- (vector-length vec) (vector-ref vec i) 1)))
)

(define (vector1invert vec)
   (make-initialized-vector (vector-length vec) (lambda (i) (1+ (- (vector-length vec) (vector-ref vec i)))))
)

(define (vector->base-k vec k)
   (let loop ((i 0) (s 0))
      (cond ((= (vector-length vec) i) s)
            ((>= (vector-ref vec i) k)
                (error (format #f "Cannot interpret vector ~a in base ~a!" vec k))
            )
            (else (loop (+ i 1) (+ (* k s) (vector-ref vec i))))
      )
   )
)

(define (permvec1inverse-of-and-reverse permvec)
    (vector-reverse
      (make-initialized-vector (vector-length permvec) (lambda (i) (permvec1find-pos-of-i-from (+ 1 i) permvec)))
    )
)


(define (A030298permvec size rank) (vector-reverse (vector1invert (A055089permvec size rank))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (same-intfuns0? A001477 (COMPOSE permutation->a055089rank A055089permvec-short) 40320) --> #t

(define (permutation->A055089rank orgperm)
  (let ((perm (vector-copy orgperm)))
   (let outloop ((j (+ -1 (vector-length perm)))
                 (rank 0)
                )
        (cond ((zero? j) rank)
              (else (let inloop ((i 0))
                       (cond ((= i j) (outloop (- j 1) (+ rank (* (- (+ 1 j) (vector-ref perm j)) (A000142 j)))))
                             (else
                                (if (> (vector-ref perm i) (vector-ref perm j))
                                    (vector-set! perm i (- (vector-ref perm i) 1))
                                )
                                (inloop (+ 1 i))
                             )
                       )
                    )
              )
        )
   )
  )
)

(define (A261096 n) (A261096bi (A002262 n) (A025581 n)))
(define (A261097 n) (A261097bi (A002262 n) (A025581 n)))
(define (A261097v2 n) (A261096bi (A025581 n) (A002262 n)))

(define (A261096bi row col) ;; Other needed funs in permfuns.scm
   (let* ((a (A055089permvec-short row))
          (b (A055089permvec-short col))
          (c (permulvecs a b))
         )
     (permutation->a055089rank c)
   )
)

(define (A261097bi row col) ;; Other needed funs in permfuns.scm
   (let* ((a (A055089permvec-short row))
          (b (A055089permvec-short col))
          (c (permulvecs b a))
         )
     (permutation->a055089rank c)
   )
)

;; (same-intfuns0? A261096  (lambda (n) (A056019 (A261097bi (A056019 (A002262 n)) (A056019 (A025581 n))))) 10441) --> #t


(define (A261098 n) (A261096bi 1 n)) ;; Self-inverse.

(define (A004442v2 n) (A261096bi n 1))

(define (A261099 n) (A261096bi n n))

(define A014489 (ZERO-POS 0 0 A261099))

;;   0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23
;;   1,  0,  4,  5,  2,  3,  7,  6, 10, 11,  8,  9, 18, 19, 20, 21, 22, 23, 12, 13, 14, 15, 16, 17
;;   2,  3,  0,  1,  5,  4, 12, 13, 14, 15, 16, 17,  6,  7,  8,  9, 10, 11, 19, 18, 22, 23, 20, 21
;;   3,  2,  5,  4,  0,  1, 13, 12, 16, 17, 14, 15, 19, 18, 22, 23, 20, 21,  6,  7,  8,  9, 10, 11
;;   4,  5,  1,  0,  3,  2, 18, 19, 20, 21, 22, 23,  7,  6, 10, 11,  8,  9, 13, 12, 16, 17, 14, 15
;;   5,  4,  3,  2,  1,  0, 19, 18, 22, 23, 20, 21, 13, 12, 16, 17, 14, 15,  7,  6, 10, 11,  8,  9
;;   6,  7,  8,  9, 10, 11,  0,  1,  2,  3,  4,  5, 14, 15, 12, 13, 17, 16, 20, 21, 18, 19, 23, 22
;;   7,  6, 10, 11,  8,  9,  1,  0,  4,  5,  2,  3, 20, 21, 18, 19, 23, 22, 14, 15, 12, 13, 17, 16
;;   8,  9,  6,  7, 11, 10, 14, 15, 12, 13, 17, 16,  0,  1,  2,  3,  4,  5, 21, 20, 23, 22, 18, 19
;;   9,  8, 11, 10,  6,  7, 15, 14, 17, 16, 12, 13, 21, 20, 23, 22, 18, 19,  0,  1,  2,  3,  4,  5
;;  10, 11,  7,  6,  9,  8, 20, 21, 18, 19, 23, 22,  1,  0,  4,  5,  2,  3, 15, 14, 17, 16, 12, 13
;;  11, 10,  9,  8,  7,  6, 21, 20, 23, 22, 18, 19, 15, 14, 17, 16, 12, 13,  1,  0,  4,  5,  2,  3
;;  12, 13, 14, 15, 16, 17,  2,  3,  0,  1,  5,  4,  8,  9,  6,  7, 11, 10, 22, 23, 19, 18, 21, 20
;;  13, 12, 16, 17, 14, 15,  3,  2,  5,  4,  0,  1, 22, 23, 19, 18, 21, 20,  8,  9,  6,  7, 11, 10
;;  14, 15, 12, 13, 17, 16,  8,  9,  6,  7, 11, 10,  2,  3,  0,  1,  5,  4, 23, 22, 21, 20, 19, 18
;;  15, 14, 17, 16, 12, 13,  9,  8, 11, 10,  6,  7, 23, 22, 21, 20, 19, 18,  2,  3,  0,  1,  5,  4
;;  16, 17, 13, 12, 15, 14, 22, 23, 19, 18, 21, 20,  3,  2,  5,  4,  0,  1,  9,  8, 11, 10,  6,  7
;;  17, 16, 15, 14, 13, 12, 23, 22, 21, 20, 19, 18,  9,  8, 11, 10,  6,  7,  3,  2,  5,  4,  0,  1
;;  18, 19, 20, 21, 22, 23,  4,  5,  1,  0,  3,  2, 10, 11,  7,  6,  9,  8, 16, 17, 13, 12, 15, 14
;;  19, 18, 22, 23, 20, 21,  5,  4,  3,  2,  1,  0, 16, 17, 13, 12, 15, 14, 10, 11,  7,  6,  9,  8
;;  20, 21, 18, 19, 23, 22, 10, 11,  7,  6,  9,  8,  4,  5,  1,  0,  3,  2, 17, 16, 15, 14, 13, 12
;;  21, 20, 23, 22, 18, 19, 11, 10,  9,  8,  7,  6, 17, 16, 15, 14, 13, 12,  4,  5,  1,  0,  3,  2
;;  22, 23, 19, 18, 21, 20, 16, 17, 13, 12, 15, 14,  5,  4,  3,  2,  1,  0, 11, 10,  9,  8,  7,  6
;;  23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1,  0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; After the following Maple-code: (From: https://oeis.org/A060125 )
;; 
;; with(group); 
;; permul := (a, b) -> mulperms(b, a); 
;; 
;; swap := (p, i, j) -> convert(permul(convert(p, 'disjcyc'), [[i, j]]), 'permlist', nops(p));
;; 
;; PermRank3Aux := proc(n, p, q)
;;   if(1 = n) then RETURN(0);
;;     else
;;       RETURN((n-p[n])*((n-1)!) + PermRank3Aux(n-1, swap(p, n, q[n]), swap(q, n, p[n])));
;;   fi;
;; end;
;; 
;; PermRank3R := p -> PermRank3Aux(nops(p), p, convert(invperm(convert(p, 'disjcyc')), 'permlist', nops(p)));
;; 
;; PermRank3L := p -> PermRank3Aux(nops(p), convert(invperm(convert(p, 'disjcyc')), 'permlist', nops(p)), p); 
;; 

(define (permutation->A060117rank orgperm)
  (let ((p (vector-copy orgperm))
        (q (permvec1inverse-of orgperm))
       )
   (let loop ((j (+ -1 (vector-length p)))
              (rank 0)
             )
        (cond ((zero? j) rank)
              (else
                (let* ((old_p_j (vector-ref p j))
                       (old_q_j (vector-ref q j))
                       (old_p_q_j (vector-ref p (- old_q_j 1)))
                       (old_q_p_j (vector-ref q (- old_p_j 1)))
                      )
                   (vector-set! p j old_p_q_j)
                   (vector-set! p (- old_q_j 1) old_p_j)
                   (vector-set! q j old_q_p_j)
                   (vector-set! q (- old_p_j 1) old_q_j)
                   (loop (- j 1) (+ rank (* (- (+ 1 j) old_p_j) (A000142 j))))
                )
              )
        )
   )
  )
)

(define (permutation->A060118rank orgperm)
  (let ((q (vector-copy orgperm))
        (p (permvec1inverse-of orgperm))
       )
   (let loop ((j (+ -1 (vector-length p)))
              (rank 0)
             )
        (cond ((zero? j) rank)
              (else
                (let* ((old_p_j (vector-ref p j))
                       (old_q_j (vector-ref q j))
                       (old_p_q_j (vector-ref p (- old_q_j 1)))
                       (old_q_p_j (vector-ref q (- old_p_j 1)))
                      )
                   (vector-set! p j old_p_q_j)
                   (vector-set! p (- old_q_j 1) old_p_j)
                   (vector-set! q j old_q_p_j)
                   (vector-set! q (- old_p_j 1) old_q_j)
                   (loop (- j 1) (+ rank (* (- (+ 1 j) old_p_j) (A000142 j))))
                )
              )
        )
   )
  )
)


;; (same-intfuns0? A001477 (COMPOSE permutation->A060118rank A060118permvec-short) 40320) --> #t

(define (A056019 n) (permutation->A055089rank (permvec1inverse-of (A055089permvec-short n))))

(define (A060125 n) (permutation->A060117rank (A060118permvec-short n)))

(define A275843 (ZERO-POS 0 0 (lambda (n) (- (A060125 n) (A225901 n)))))


(define (A275957 n) (A225901 (A060125 n)))
(define (A275958 n) (A060125 (A225901 n)))

(define A275844 (NONZERO-POS 1 0 (lambda (n) (- (A060125 n) (A225901 n)))))
(define A275843v2 (FIXED-POINTS 0 0 A275957))

;; (same-intfuns0? A060125 (COMPOSE A060126 A056019 A060119) (! 8)) --> #t

(define (A261216 n) (A261216bi (A002262 n) (A025581 n)))
(define (A261217 n) (A261217bi (A002262 n) (A025581 n)))
(define (A261217v2 n) (A261216bi (A025581 n) (A002262 n)))

(define (A261216bi row col) ;; Other needed funs in permfuns.scm
   (let* ((a (A060117permvec-short row))
          (b (A060117permvec-short col))
          (c (permulvecs a b))
         )
     (permutation->a060117rank c)
   )
)

(define (A261217bi row col) ;; Other needed funs in permfuns.scm
   (let* ((a (A060118permvec-short row))
          (b (A060118permvec-short col))
          (c (permulvecs a b))
         )
     (permutation->a060118rank c)
   )
)

;; (same-intfuns0? A261216 (lambda (n) (A060125 (A261217bi (A060125 (A002262 n)) (A060125 (A025581 n))))) 10440) -> #t
;; (same-intfuns0? A261216 (lambda (n) (A060126 (A261096bi (A060119 (A002262 n)) (A060119 (A025581 n))))) 10440) -> #t
;; (same-intfuns0? A261216 (lambda (n) (A060127 (A261097bi (A060120 (A002262 n)) (A060120 (A025581 n))))) 10440) -> #t

;; (same-intfuns0? A261096 (lambda (n) (A060119 (A261216bi (A060126 (A002262 n)) (A060126 (A025581 n))))) 10440) -> #t
;; (same-intfuns0? A261096 (lambda (n) (A060120 (A261217bi (A060127 (A002262 n)) (A060127 (A025581 n))))) 10440) -> #t

;; (same-intfuns0? A261097 (lambda (n) (A060119 (A261217bi (A060126 (A002262 n)) (A060126 (A025581 n))))) 10440) -> #t
;; (same-intfuns0? A261097 (lambda (n) (A060120 (A261216bi (A060127 (A002262 n)) (A060127 (A025581 n))))) 10440) -> #t

;; (same-intfuns0? A261217 (lambda (n) (A060125 (A261216bi (A060125 (A002262 n)) (A060125 (A025581 n))))) 10440) -> #t
;; (same-intfuns0? A261217 (lambda (n) (A060126 (A261097bi (A060119 (A002262 n)) (A060119 (A025581 n))))) 10440) -> #t
;; (same-intfuns0? A261217 (lambda (n) (A060127 (A261096bi (A060120 (A002262 n)) (A060120 (A025581 n))))) 10440) -> #t

;; (same-intfuns0? A261098 (COMPOSE A060119 A261218 A060126) 40320) -> #t
;; (same-intfuns0? A261218 (COMPOSE A060126 A261098 A060119) 40320) -> #t

;; (same-intfuns0? A261099 (COMPOSE A060119 A261219 A060126) 40320) -> #t
;; (same-intfuns0? A261219 (COMPOSE A060126 A261099 A060119) 40320) -> #t


(define (A004442v3 n) (A261216bi n 1))

(define (A261218 n) (A261216bi 1 n))

(define (A261219 n) (A261216bi n n))

(define A261220v2 (ZERO-POS 0 0 A261219))

;; (same-intfuns0? A001477 (COMPOSE A261218 A261218) 5040) --> #t


(define (A060119 n) (permutation->A055089rank (A060117permvec-short n)))

(define (A060126 n) (permutation->A060117rank (A055089permvec-short n)))


(define (A060120 n) (permutation->A055089rank (A060118permvec-short n)))

(define (A060127 n) (permutation->A060118rank (A055089permvec-short n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (gen_partitions m colfun) ;; Adapted by AK from Kreher & Stinson, CAGES, p. 68, Algorithm 3.1.
   (let recurse ((m m) (b m) (n 0) (partition (list)))
        (cond ((zero? m) (colfun partition))
              (else
                 (let loop ((i 1))
                      (recurse (- m i) i (+ 1 n) (cons i partition))
                      (if (< i (min b m)) (loop (+ 1 i)))
                 )
              )
        )
   )
)

;; Further abstracted from previous.
(define (fold_over_partitions_of m initval addpartfun colfun)
   (let recurse ((m m) (b m) (n 0) (partition initval))
        (cond ((zero? m) (colfun partition))
              (else
                 (let loop ((i 1))
                      (recurse (- m i) i (+ 1 n) (addpartfun i partition))
                      (if (< i (min b m)) (loop (+ 1 i)))
                 )
              )
        )
   )
)


(define (count_number_of_distinct_lcms_of_partitions_until_fixed_point_met n initial_value)
   (let loop ((lcms (list initial_value initial_value)))
      (fold_over_partitions_of n 1 lcm
                       (lambda (p) (set-car! lcms (max (car lcms) (lcm (second lcms) p))))
      )
      (if (= (car lcms) (second lcms))
          (length (cdr lcms))
          (loop (cons (car lcms) lcms))
      )
   )
)



;; (map (lambda (n) (length (list_partitions n))) (iota 32)) --> A000041
(define (list_partitions n)
   (let ((results (list (list))))
;;    (gen_partitions n (lambda (p) (attach! p results)))
      (fold_over_partitions_of n (list) cons (lambda (p) (attach! p results)))
      (cdr (reverse! results))
   )
)

(define (A000792 n) ;; There are faster ways to compute this.
   (let ((maxprod (list 0)))
;;    (gen_partitions n (lambda (p) (set-car! maxprod (max (car maxprod) (apply * p)))))
      (fold_over_partitions_of n 1 * (lambda (p) (set-car! maxprod (max (car maxprod) p))))
      (car maxprod)
   )
)


(definec (A000793 n)
   (let ((maxlcm (list 0)))
;;    (gen_partitions n (lambda (p) (set-car! maxlcm (max (car maxlcm) (apply lcm p)))))
      (fold_over_partitions_of n 1 lcm (lambda (p) (set-car! maxlcm (max (car maxlcm) p))))
      (car maxlcm)
   )
)

;; A225627-A225646 are now reserved for your use.

(definec (A225627 n)
   (let ((maxlcm (list 0)))
;;    (gen_partitions n (lambda (p) (set-car! maxlcm (max (car maxlcm) (apply lcm (cons (A000793 n) p))))))
      (fold_over_partitions_of n (A000793 n) lcm (lambda (p) (set-car! maxlcm (max (car maxlcm) p))))
      (car maxlcm)
   )
)


(definec (A225628 n)
   (let ((maxlcm (list 0)))
;;    (gen_partitions n (lambda (p) (set-car! maxlcm (max (car maxlcm) (apply lcm (cons (A225627 n) p))))))
      (fold_over_partitions_of n (A225627 n) lcm (lambda (p) (set-car! maxlcm (max (car maxlcm) p))))
      (car maxlcm)
   )
)


(definec (A000793v2 n) (A225630bi n 1))
(definec (A225627v2 n) (A225630bi n 2))
(define (A225627v3 n) (* (A225636 n) (A000793 n)))

(definec (A225628v2 n) (A225630bi n 3))

(definec (A225629 n) (A225630bi n (max 0 (- (A225633 n) 1)))) ;; The last row before the value obtains fixed point.

(define (A225636 n) (/ (A225627 n) (A000793 n)))
(define (A225637 n) (/ (A003418 n) (A225629 n)))


(define (collect_lcms_of_partitions_until_fixed_point_met n initial_value)
   (let loop ((lcms (list initial_value initial_value)))
      (gen_partitions n (lambda (p) (set-car! lcms (max (car lcms) (apply lcm (cons (second lcms) p))))))
      (if (= (car lcms) (second lcms))
          (reverse! (cdr lcms))
          (loop (cons (car lcms) lcms))
      )
   )
)


;; The rows converge towards A003418, and the diagonal is also A003418. Because A001222(n) < n for all n (???)
;; The row zero: all-1 sequence, A000012,
;; the row 1: A000793,
;; the row 2: A225627
;; the row 3: A225628.

(define (A225630bi col row)
  (let ((maxlcm (list 0)))
   (let loop ((prevmaxlcm 1) (stepsleft row))
      (if (zero? stepsleft)
          prevmaxlcm
          (begin
            (gen_partitions col (lambda (p) (set-car! maxlcm (max (car maxlcm) (apply lcm (cons prevmaxlcm p))))))
            (loop (car maxlcm) (- stepsleft 1))
          )
      )
   )
  )
)

(define (A225630 n) (A225630bi (A025581 n) (A002262 n)))
(define (A225631 n) (A225630bi (A002262 n) (A025581 n))) ;; Transposed.

(definec (A225633 n) (- (A225634 n) 1)) ;; Number of steps to reach from 1 to A003418(n)
(definec (A225634v2 n) (length (collect_lcms_of_partitions_until_fixed_point_met n 1)))
(definec (A225634 n) (count_number_of_distinct_lcms_of_partitions_until_fixed_point_met n 1))

(define A225635 (PARTIALSUMS 1 0 A225634)) ;; Positions of ones in A225632.


;; A225632 = 1;1,2;1,3,6;1,4,12;1,6,30,60;

(definec (A225632 n) (A225630bi (Aux_for_225632 n) (- n (A225635 (Aux_for_225632 n)))))
(define Aux_for_225632 (COMPOSE -1+ (LEAST-GTE-I 1 1 A225635) 1+)) ;; On which column we are here?


;; The rows converge towards A003418, and the diagonal is also A003418 ??? Because A001222(n) < n for all n (???)
;; The row zero: A0001477,
;; the row 1: A225646

(define (A225640bi col row)
  (let ((maxlcm (list 0)))
   (let loop ((prevmaxlcm (max 1 col)) (stepsleft row))
      (if (zero? stepsleft)
          prevmaxlcm
          (begin
            (gen_partitions col (lambda (p) (set-car! maxlcm (max (car maxlcm) (apply lcm (cons prevmaxlcm p))))))
            (loop (car maxlcm) (- stepsleft 1))
          )
      )
   )
  )
)

(define (A225640 n) (A225640bi (A025581 n) (A002262 n)))
(define (A225641 n) (A225640bi (A002262 n) (A025581 n))) ;; Transposed.

(define (A225643 n) (-1+ (A225644 n))) ;; Number of steps to reach from n to A003418(n)
(definec (A225644v2 n) (length (collect_lcms_of_partitions_until_fixed_point_met n n)))
(definec (A225644 n) (count_number_of_distinct_lcms_of_partitions_until_fixed_point_met n n))

(define A225645 (PARTIALSUMS 1 0 A225644)) ;; A225642(a(n)) = n (possibly elsewhere also).

;; 1;2;3,6;4,12;5,30,60;6,30,60;7,84,420;8,120,840;9,180,1260,2520;10,210,840,2520;11,330,4620,13860,27720;...
(definec (A225642 n) (A225640bi (Aux_for_225642 n) (- n (A225645 (Aux_for_225642 n)))))
(define Aux_for_225642 (COMPOSE -1+ (LEAST-GTE-I 1 1 A225645) 1+)) ;; On which row we are here?

(definec (A225646old n) ;; The second row of A225640
   (let ((maxlcm (list 0)))
;;    (gen_partitions n (lambda (p) (set-car! maxlcm (max (car maxlcm) (apply lcm (cons n p))))))
      (fold_over_partitions_of n n lcm (lambda (p) (set-car! maxlcm (max (car maxlcm) p))))
      (car maxlcm)
   )
)

(definec (A225646 n) ;; The second row of A225640
   (let ((maxlcm (list 1)))
      (fold_over_partitions_of n n lcm (lambda (p) (set-car! maxlcm (max (car maxlcm) p))))
      (car maxlcm)
   )
)


;; A225648-A225657 are now reserved for your use.

;; Any other composites than 8 and 9 and 27 present?
;; (map A225648 (iota 25))
;; (1 5 7 8 9 11 13 17 19 23 27 29 31 37 41 43 47 53 59 61 67 71 73 79 83)
(define A225648 (MATCHING-POS 1 1 (lambda (i) (= 1 (gcd (A000793 i) i)))))

(define A225649 (MATCHING-POS 1 1 (lambda (i) (> (gcd (A000793 i) i) 1))))

(define (A225650 n) (gcd (A000793 n) n))

;; 1,2,3,4,6,10,12,14,15,20,21,24,30,35,36,39,40,42,44,52
(define A225651 (MATCHING-POS 1 1 (lambda (i) (zero? (modulo (A000793 i) i)))))

(define (A225652 n) (/ (A225646 n) (max 1 n)))


(define (inA225653? n)
 (or (zero? n)
  (let ((fun1 (lambda (seed)
                (let ((max1 (list 0)))
                  (fold_over_partitions_of n 1 lcm (lambda (p) (set-car! max1 (max (car max1) (lcm seed p)))))
                  (car max1)
                )
              )
        )
        (fun2 (lambda (seed)
                (let ((max2 (list 0)))
                  (fold_over_partitions_of n n lcm (lambda (p) (set-car! max2 (max (car max2) (lcm seed p)))))
                  (car max2)
                )
              )
        )
       )
    (equal-steps-to-convergence-nondecreasing? fun1 fun2 1 n)
  )
 )
)

(define A225653 (MATCHING-POS 0 0 inA225653?))
(define A225653v2 (MATCHING-POS 0 0 (lambda (i) (= (A225634 i) (A225644 i)))))

(definec (A225638 n)
 (if (zero? n) n
  (let ((fun1 (lambda (seed)
                (let ((max1 (list 0)))
                  (fold_over_partitions_of n 1 lcm (lambda (p) (set-car! max1 (max (car max1) (lcm seed p)))))
                  (car max1)
                )
              )
        )
        (fun2 (lambda (seed)
                (let ((max2 (list 0)))
                  (fold_over_partitions_of n (max 1 n) lcm (lambda (p) (set-car! max2 (max (car max2) (lcm seed p)))))
                  (car max2)
                )
              )
        )
       )
    (steps-to-convergence-nondecreasing fun1 fun2 1 n)
  )
 )
)

(define (A225638v2 n) (+ (A225639 n) (A225654 n)))

(definec (A225639 n)
 (if (zero? n) n
  (let ((fun1 (lambda (seed)
                (let ((max1 (list 0)))
                  (fold_over_partitions_of n 1 lcm (lambda (p) (set-car! max1 (max (car max1) (lcm seed p)))))
                  (car max1)
                )
              )
        )
        (fun2 (lambda (seed)
                (let ((max2 (list 0)))
                  (fold_over_partitions_of n (max 1 n) lcm (lambda (p) (set-car! max2 (max (car max2) (lcm seed p)))))
                  (car max2)
                )
              )
        )
       )
    (steps-to-convergence-nondecreasing fun2 fun1 n 1)
  )
 )
)


(definec (A226055 n) (A225630bi n (A225638 n)))
(definec (A226055v2 n) (A225640bi n (A225639 n)))
(definec (A226056 n) (- (A225634 n) (A225638 n)))
(definec (A226056v2 n) (- (A225644 n) (A225639 n)))

;; A226060-A226062 are now reserved for your use.

(define (A225654 n) (- (A225634 n) (A225644 n)))


(define (A225655 n) ;; Probably doesn't work with our definec. A pity! ("Can't bind name in null syntactic environment")
   (define (choose_y_if_larger_and_divisible_by_n x y) (if (and (> y x) (zero? (modulo y n))) y x))
   (let ((maxlcm (list 1)))
      (fold_over_partitions_of n 1 lcm (lambda (p) (set-car! maxlcm (choose_y_if_larger_and_divisible_by_n (car maxlcm) p))))
      (car maxlcm)
   )
)


(definec (A225655v2 n) (A225655 n)) ;; A kludge for convenience, so that we can have values cached.

(define (A225656 n) (/ (A225655v2 n) (max 1 n)))

(define A225657 (MATCHING-POS 1 0 (lambda (i) (= (A225655v2 i) i))))


(definec (A003418 n)
   (let loop ((n n) (acc 1))
         (if (zero? n)
             acc
             (loop (- n 1) (lcm acc n))
         )
   )
)

(define (A003418v2 n) (last (collect_lcms_of_partitions_until_fixed_point_met n 1)))

(define (A225558 n) (/ (A003418 n) (A000793 n)))

(define (A284567loop n)
   (let loop ((n n) (acc 1))
         (if (zero? n)
             acc
             (loop (- n 1) (A059897bi acc n))
         )
   )
)

(definec (A284567 n) (if (zero? n) 1 (A059897bi n (A284567 (- n 1)))))

(define (A284568 n) (/ (A000142 n) (A284567 n)))

(define (A001222easy n) (let loop ((n n) (z 0)) (if (= 1 n) z (loop (/ n (A020639 n)) (+ 1 z)))))

(define (A284561 n) (A001222easy (A284567 n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(definec (A254109toogreedy n)
  (cond ((<= n 63) n)
        ((= 14 (modulo n 32)) (+ 5 (* 8 (A254109toogreedy (floor->exact (/ n 32))))))
        ((= 30 (modulo n 64)) (+ 3 (* 4 (A254109toogreedy (floor->exact (/ n 64))))))
        (else (+ (modulo n 2) (* 2 (A254109toogreedy (floor->exact (/ n 2))))))
  )
)


(define (A254109toogreedyfast n) ;; (0 1 1 1 0) -> (1 0 1),  (0 1 1 1 1 0) -> (1 1). 
  (let loop ((n n) (s 0) (p2 1))
    (cond ((<= n 63) (+ (* p2 n) s))
          ((= 14 (modulo n 32)) (loop (floor->exact (/ n 32)) (+ s (* p2 5)) (* p2 8)))
          ((= 30 (modulo n 64)) (loop (floor->exact (/ n 64)) (+ s (* p2 3)) (* p2 4)))
          ((even? n) (loop (/ n 2) s (+ p2 p2)))
          (else (loop (/ (- n 1) 2) (+ s p2) (+ p2 p2)))
    )
  )
)


(definec (A254109 n)
  (cond ((<= n 63) n)
        ((= 14 (modulo n 32)) (+ 5 (* 8 (floor->exact (/ n 32)))))
        ((= 30 (modulo n 64)) (+ 3 (* 4 (floor->exact (/ n 64)))))
        (else (+ (modulo n 2) (* 2 (A254109 (floor->exact (/ n 2))))))
  )
)


(define (A254109fast n) ;; (0 1 1 1 0) -> (1 0 1),  (0 1 1 1 1 0) -> (1 1). 
  (let loop ((n n) (s 0) (p2 1))
    (cond ((<= n 63) (+ (* p2 n) s))
          ((= 14 (modulo n 32)) (+ (* p2 8 (floor->exact (/ n 32))) s (* p2 5)))
          ((= 30 (modulo n 64)) (+ (* p2 4 (floor->exact (/ n 64))) s (* p2 3)))
          ((even? n) (loop (/ n 2) s (+ p2 p2)))
          (else (loop (/ (- n 1) 2) (+ s p2) (+ p2 p2)))
    )
  )
)



(definec (A258009 n) ;; rewrite (0 1 1 0) -> (1 0 0 1), but apply only once, at pos. nearest to least significant end.
  (cond ((<= n 16) n)
        ((= 6 (modulo n 16)) (+ 9 (* 16 (floor->exact (/ n 16)))))
        (else (+ (modulo n 2) (* 2 (A258009 (floor->exact (/ n 2))))))
  )
)

(define (A258009fast n) ;; (0 1 1 0) -> (1 0 0 1)
  (let loop ((n n) (s 0) (p2 1))
    (cond ((<= n 16) (+ (* p2 n) s))
          ((= 6 (modulo n 16)) (+ (* p2 16 (floor->exact (/ n 16))) s (* p2 9)))
          ((even? n) (loop (/ n 2) s (+ p2 p2)))
          (else (loop (/ (- n 1) 2) (+ s p2) (+ p2 p2)))
    )
  )
)

(definec (Asimilar_but_applies_everywhere n) ;; (0 1 1 0) -> (1 0 0 1)
  (cond ((<= n 16) n)
        ((= 6 (modulo n 16)) (+ 9 (* 16 (Asimilar_but_applies_everywhere (floor->exact (/ n 16))))))
        (else (+ (modulo n 2) (* 2 (Asimilar_but_applies_everywhere (floor->exact (/ n 2))))))
  )
)

(define (Asimilar_but_applies_everywhere-fast n) ;; (0 1 1 0) -> (1 0 0 1)
  (let loop ((n n) (s 0) (p2 1))
    (cond ((<= n 16) (+ (* p2 n) s))
          ((= 6 (modulo n 16)) (loop (floor->exact (/ n 16)) (+ s (* p2 9)) (* p2 16)))
          ((even? n) (loop (/ n 2) s (+ p2 p2)))
          (else (loop (/ (- n 1) 2) (+ s p2) (+ p2 p2)))
    )
  )
)

(define (newphase p d) (modulo (+ p d d -1) 6))

(define (x-delta phase) (* (- (* 2 (floor->exact (/ phase 3))) 1) (- (modulo phase 3) 1)))

(define (y-delta phase) (* (- 1 (* 2 (floor->exact (/ phase 3)))) (floor->exact (/ (+ 1 (modulo phase 3)) 2))))

;; Numbers n such that when we start scanning bits in the binary expansion of n, from the least to the most significant end, and when we interpret them as to which direction to turn at each vertex (e.g., 0 = left, 1 = right) when traversing the edges of honeycomb lattice, then, when we have consumed all except the most significant 1-bit (which is ignored), we have eventually returned to the same vertex where we started from.


(define (isA255570? n)
  (let loop ((n n) (x 0) (y 0) (phase 0))
       (cond ((= 1 n) (and (zero? x) (zero? y)))
             (else
               (let* ((d (modulo n 2))
                      (newphase (modulo (+ phase d d -1) 6))
                     )
                 (loop (/ (- n d) 2)
                       (+ x (x-delta newphase))
                       (+ y (y-delta newphase))
                       newphase
                 )
               )
             )
       )
  )
)

(define A255570 (MATCHING-POS 0 1 isA255570?))


;; A subsequence of A255570 where also every A080541 (or A080542) rotation is also included.


;; Numbers n such that when we start scanning bits in the binary expansion of n, from the least to the most significant end, and when we interpret each bit as to a direction which to turn at each vertex (e.g., 0 = left, 1 = right) when traversing the edges of honeycomb lattice, then, when we have consumed all except the most significant 1-bit (which is ignored), we have eventually returned to the same vertex where we started from AND none of the other vertices have been visited twice. 

(define (isA255571? n)
  (let loop ((n n) (x 0) (y 0) (phase 0) (vv (list))) ;; vv = visited vertices
       (cond ((= 1 n) (and (zero? x) (zero? y)))
             ((member (cons x y) vv) #f)
             (else
               (let* ((d (modulo n 2))
                      (newphase (modulo (+ phase d d -1) 6))
                     )
                 (loop (/ (- n d) 2)
                       (+ x (x-delta newphase))
                       (+ y (y-delta newphase))
                       newphase
                       (cons (cons x y) vv)
                 )
               )
             )
       )
  )
)

(define A255571 (MATCHING-POS 0 1 isA255571?))


(define A258001 (MATCHING-POS 0 1 (lambda (n) (and (isA255571? n) (isA255571? (A080542 n))))))

(define A258001v2 (MATCHING-POS 0 1 (lambda (n) (and (isA255571? n) (for-all-nonmsb-bit-rotations? n isA255571?)))))

(define A258002 (MATCHING-POS 0 1 (lambda (n) (and (negative? (A037861 n)) (isA255571? n) (isA255571? (A080542 n))))))

(define (isA258003? n) (and (negative? (A037861 n)) (= (A256999 n) n) (isA255571? n) (isA255571? (A080542 n))))

(define A258003 (MATCHING-POS 0 1 isA258003?))

(define (A258004 n) (A053645 (A258003 n)))

(define (isA258005? n) (and (negative? (A037861 n)) (= n (A256999 (A059893 n))) (isA255571? n) (isA255571? (A080542 n))))

(define A258005 (MATCHING-POS 0 1 isA258005?))

;; Cf. A057779 Hexagonal polyominoes (or polyhexes, A000228) with perimeter 2n. a(1)=a(2)=0, a(3)=1, a(4)=0, a(5)=1

(definec (A258204 n)
   (let loop ((k (+ 1 (expt 2 (+ n n)))) (c 0))
     (cond ((pow2? k) c)
           (else (loop (+ 1 k) (+ c (if (isA258003? k) 1 0))))
     )
   )
)

(definec (A258205 n)
   (let loop ((k (+ 1 (expt 2 (+ n n)))) (c 0))
     (cond ((pow2? k) c)
           (else (loop (+ 1 k) (+ c (if (isA258005? k) 1 0))))
     )
   )
)


(define (A258206 n) (* (/ 1 2) (+ (A258204 n) (A258205 n))))

(define (isA258209? n) (= n (A256999 (A059893 n))))

(define A258209 (MATCHING-POS 0 0 isA258209?))


(definec (A258017 n)
   (let loop ((k (+ 1 (expt 2 (+ n n)))) (c 0))
     (cond ((pow2? k) c)
           (else (loop (+ 1 k) (+ c (if (isA258013? k) 1 0))))
     )
   )
)

(definec (A258018 n)
   (let loop ((k (+ 1 (expt 2 (+ n n)))) (c 0))
     (cond ((pow2? k) c)
           (else (loop (+ 1 k) (+ c (if (isA258015? k) 1 0))))
     )
   )
)


(define (A258019 n) (* (/ 1 2) (+ (A258017 n) (A258018 n))))



;; Renumber:
;; (define A258020-renumber (MATCHING-POS 0 1 (lambda (n) (and (isA255570? n) (isA255570? (A080542 n))))))
;; (define A258020v2-renumber (MATCHING-POS 0 1 (lambda (n) (and (isA255570? n) (for-all-nonmsb-bit-rotations? n isA255570?)))))

;; (define A258021-renumber (MATCHING-POS 0 1 (lambda (n) (and (isA255570? n) (not (isA255570? (A080541 n)))))))
;; (define A258021v2-renumber (MATCHING-POS 0 1 (lambda (n) (and (isA255570? n) (not (isA255570? (A080542 n)))))))

;;
;; (A007088 64712160) -> 11110110110110110111100000
;;                       11110110110110110111100000
;; (binwidth 64712160) -> 26
;; (+ (expt 2 26) 64712160) -> 131821024
;; (A007088 131821024) -> 111110110110110110111100000
;;
;; Cf. A258002(20622) = 131820496, the last 26-edge strictly selfavoiding holeless polyhex.

;;
;; From polyhexp.txt:
;; Neither this should match. However, cases like these prove that
;; the polyhex-language is NOT context-free: (CHECK YOUR TERMINOLOGY!)
;; ispolyhex([1,1,1,1,0,1,1,0,1,1,0,1,1,0,1,1,0,1,1,1,1,0,0,0,0,0],N).
;; (Currently this returns N=6.)
;; Note that it would not be enough to just filter off patterns with ,0,0,0,0,0, sub-pattern:
;; ispolyhex([1,1,1,1,0,1,1,0,1,0,1,1,0,1,1,0,1,1,0,1,0,1,1,1,1,0,1,0,0,0,0,1,0,0],N).
;; (Currently this returns N=8.)
;;

;; (A007088 1035393984) ;; The first really self-overlapping helicene (seven hexes).
;;  111101101101101101101111000000

;; (A007088 2109135808) ;; Same code 1-capped.
;; 1111101101101101101101111000000

;;
;; A 42-edge killer of a simple algorithm: (Two "crowns" connected by "snake" or "S"):
;; (A007088 3769691237784) =  110110110110110011000110110110110110011000
;; (A007088 8167737748888) = 1110110110110110011000110110110110110011000 (same code 1-capped).
;;
;; Note there is already a nine-hex, 26-edge killer of a very simple algorithm here: 130472806.
;; (is-n-rewritable-to-127-with-A254109-and-A258009? 130472806) --> #f
;; Trying every rotation helps:
;; (for-any-nonmsb-bit-rotation? 130472806 is-n-rewritable-to-127-with-A254109-and-A258009?) --> #t
;;
;; However, 42-edge killer fails also with that:
;; (for-any-nonmsb-bit-rotation? 8167737748888 is-n-rewritable-to-127-with-A254109-and-A258009?) --> #f
;;
;;



;;;;;;;;;;;;;;;;;;;;;;;

(define (bits-reversed->list n)
   (if (zero? n) (list) (cons (modulo n 2) (bits-reversed->list (floor->exact (/ n 2)))))
)

(define (bits->list n) (reverse! (bits-reversed->list n)))


(define (isA255561bitlist? bitlist)
  (let loop ((bitlist bitlist) (x 0) (y 0) (phase 0) (vv (list))) ;; vv = visited vertices
       (cond ((null? bitlist) (and (zero? x) (zero? y)))
             ((member (cons x y) vv) #f)
             (else
               (let* ((d (first bitlist))
                      (newphase (modulo (+ phase d d -1) 6))
                     )
                 (loop (cdr bitlist)
                       (+ x (x-delta newphase))
                       (+ y (y-delta newphase))
                       newphase
                       (cons (cons x y) vv)
                 )
               )
             )
       )
  )
)

(define (isA255561? n) (isA255561bitlist? (bits->list n)))

(define A255561 (MATCHING-POS 0 0 isA255561?))
(define A255561v2 (MATCHING-POS 0 0 (COMPOSE isA255561bitlist? bits->list)))



(define (is-bitlist-rewritable-to-111111? bits)
      (let loop ((bits bits) (len (length bits)))
           (let* ((newbits (apply-hex-rewrite-where-first-applicable-to-list bits))
                  (newlen (length newbits))
                 )
             (cond ((equal? newbits '(1 1 1 1 1 1)) #t) ;; Yes, reached the ur-hexagon!
                   ((equal? newbits bits) #f) ;; Didn't change at all? So we failed.
                   (else (loop newbits newlen)) ;; Otherwise, loop again with rewritten list.
             )
           )
      )
)


(define (apply-hex-rewrite-where-first-applicable-to-list bitlist)
  (let loop ((bits bitlist))
     (let ((len (length bits)))
       (cond ((< len 4) bits)
;; Don't use this one, it's harmful! (if applied naively, without backtracking...)
;; Solution (???): Do not apply if followed by 3 or 4 1's + one 0:
;; So out of 32 cases (5 bits following 0110), only cases 001110110, 101110110 and 011110110
;; should not be rewritten as xxxxx1001. Instead, they will be rewritten as 0101110, 1101110 and 11110
;;

;; XXX - Actually, should only be applied when everything else fails!
             ((and (>= len 4) (equal? (list-head bits 4) '(0 1 1 0))
                   (or (< len 9)
                       (and (not (equal? (list-head bits 9) '(0 1 1 0 1 1 1 0 0)))
                            (not (equal? (list-head bits 9) '(0 1 1 0 1 1 1 0 1)))
                            (not (equal? (list-head bits 9) '(0 1 1 0 1 1 1 1 0)))
                       )
                   )
              )
                (append '(1 0 0 1) (list-tail bits 4))
             )
             ((and (>= len 5) (equal? (list-head bits 5) '(0 1 1 1 0)))
                (append '(1 0 1) (list-tail bits 5))
             )
             ((and (>= len 6) (equal? (list-head bits 6) '(0 1 1 1 1 0)))
                (append '(1 1) (list-tail bits 6))
             )
             (else (cons (car bits) (loop (cdr bits))))
       )
     )
  )
)



;; (is-n-rewritable-to-63-with-A254109? 1035393984) --> #t

;; XXX: we DO need -0110- -> -1001- rewrite, because "obtuse" ("convex") polyhexes are missed otherwise:
;; (is-n-rewritable-to-63-with-A254109? 224694) --> #f
;; (is-n-rewritable-to-63-with-A254109? 3593946) --> #f


(define (is-n-rewritable-to-63-with-A254109? n)
   (let loop ((old-n 0) (n n))
        (cond ((= 63 n) #t) ;; Yes, reached the ur-hexagon!
              ((= n old-n) #f) ;; Didn't change at all? So we failed.
              (else (loop n (A254109 n)))
        )
   )
)


(define (is-n-rewritable-to-63-with-A254109-and-A258009? n)
   (let loop ((old-n 0) (n n))
        (cond ((= 63 n) #t) ;; Yes, reached the ur-hexagon!
              ((= n old-n)
                (let ((try2 (A258009fast n)))
                   (if (= try2 n) #f (loop n try2))
                )
              )
              (else (loop n (A254109fast n)))
        )
   )
)



;; (define A255562-renumber (MATCHING-POS 1 1 is-n-rewritable-to-63-with-A254109?))
;; (define A255562-renumberv1 (MATCHING-POS 1 1 (COMPOSE is-bitlist-rewritable-to-111111? bits->list)))
;; (define A255562-renumberv2 (MATCHING-POS 1 1 (COMPOSE is-bitlist-rewritable-to-111111? bits-reversed->list)))

(define (isA255563v1? n)
   (let ((bits (bits->list n)))
     (and (isA255561bitlist? bits) (is-bitlist-rewritable-to-111111? bits))
   )
)

(define A255563v1 (MATCHING-POS 1 1 isA255563v1?))

(define (isA255564v1? n)
   (let ((bits (bits->list n)))
     (and (not (isA255561bitlist? bits)) (is-bitlist-rewritable-to-111111? bits))
   )
)

(define A255564v1 (MATCHING-POS 1 1 isA255564v1?))


(define (is-n-rewritable-to-127-with-A254109? n)
   (let loop ((old-n 0) (n n))
        (cond ((= 127 n) #t) ;; Yes, reached the ur-hexagon (with msb-1 working as its "cap").
              ((= n old-n) #f) ;; Didn't change at all? So we failed.
              (else (loop n (A254109 n)))
        )
   )
)




(define (is-n-rewritable-to-127-with-A254109-and-A258009? n)
   (let loop ((prev-n 0) (n n))
        (cond ((= 127 n) #t) ;; Yes, reached the ur-hexagon!
              ((= n prev-n) ;; Reached a fixed point of repeated A254109 transformations?
                (let ((try2 (A258009fast n))) ;; Let's try another kind of reduction then...
                   (if (= try2 n) #f (loop n try2))
                )
              )
              (else (loop n (A254109fast n)))
        )
   )
)


;;
;; (is-n-rewritable-to-127-with-a254109-and-a258009-return-size-if-it-is? 127) --> 1
;; 
;; (is-n-rewritable-to-127-with-a254109-and-a258009-return-size-if-it-is? 486838) --> 7 (a simple crown)
;; 
;; (is-n-rewritable-to-127-with-a254109-and-a258009-return-size-if-it-is? 131821024) --> 6
;; 
;; (is-n-rewritable-to-127-with-a254109-and-a258009-return-size-if-it-is? 2109135808) --> 7
;; 
;; (is-n-rewritable-to-127-with-a254109-and-a258009-return-size-if-it-is? 130472806) --> #f
;; (for-any-nonmsb-bit-rotation? 130472806 is-n-rewritable-to-127-with-A254109-and-A258009-return-size-if-it-is?) = 9
;;
;;

(define (is-n-rewritable-to-127-with-A254109-and-A258009-return-size-if-it-is? n)
   (let loop ((prev-n 0) (n n) (size 1))
        (cond ((= 127 n) size) ;; Yes, reached the ur-hexagon!
              ((= n prev-n) ;; Reached a fixed point of repeated A254109 transformations?
                (let ((try2 (A258009fast n))) ;; Let's try another kind of reduction then...
                   (if (= try2 n) #f (loop n try2 size))
                )
              )
              (else (loop n (A254109fast n) (+ 1 size)))
        )
   )
)

;; I know this to be correct only up to size 6.
(define (Asize_in_hexes-naive n) (for-any-nonmsb-bit-rotation? n is-n-rewritable-to-127-with-A254109-and-A258009-return-size-if-it-is?))


(define (isA258012? n) (or (= 1 n) (and (= -7 (A037861 n)) (for-any-nonmsb-bit-rotation? n is-n-rewritable-to-127-with-A254109-and-A258009?))))

(define A258012 (MATCHING-POS 0 1 isA258012?))

(define (isA258013? n) (or (= 1 n) (and (= -7 (A037861 n)) (= (A256999 n) n) (for-any-nonmsb-bit-rotation? n is-n-rewritable-to-127-with-A254109-and-A258009?))))

(define A258013 (MATCHING-POS 0 1 isA258013?))


(define (isA258015? n) (or (= 1 n) (and (= -7 (A037861 n)) (= n (A256999 (A059893 n))) (for-any-nonmsb-bit-rotation? n is-n-rewritable-to-127-with-A254109-and-A258009?))))

(define A258015 (MATCHING-POS 0 1 isA258015?))


(define A255342reneimaa (MATCHING-POS 1 1 (lambda (n) (and (not (pow2? n)) (= (A053645 n) (A163380 (A053645 n))) (is-n-rewritable-to-127-with-A254109-and-A258009? n)))))

(define (A255342v2_reneimaa n) (A004755 (A255562-renumber n)))

(define A255562-renumbermaybe (MATCHING-POS 1 1 (lambda (n) (and  (= -6 (A037861 n)) (isA065609? n) (is-n-rewritable-to-63-with-A254109-and-A258009? n)))))

(define (A255562-renumberv2 n) (A053645 (A255342reneimaa n))) ;; Wrong?

;; (define A255562-renumbermaybe (MATCHING-POS 1 1 (lambda (n) (and  (= -6 (A037861 n)) (isA065609? n) (is-bitlist-rewritable-to-111111? (bits-reversed->list n))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Some new ones from Kimberling.
;; Rule 3 follows.  For k >= 1, let  A(k) = {a(1), …, a(k)} and D(k) = {d(1), …, d(k)}.  Begin with k = 1 and nonnegative integers a(1) and d(1).
;; 
;; Step 1:   If there is an integer h such that 1 - a(k) < h < 0 and h is not in D(k) and a(k) + h is not in A(k), let d(k+1) be the least such h, let a(k+1) = a(k) + h, replace k by k + 1, and repeat Step 1; otherwise do Step 2.
;; 
;; Step 2:  Let h be the least positive integer not in D(k) such that a(k) - h is not in A(k).  Let a(k+1) = a(k) + h and d(k+1) = h.  Replace k by k+1 and do Step 1. 
;; 

(define (not_in_range_of? k fun upto_n)
  (let loop ((i 1))
         (cond ((> i upto_n) #t)
               ((= (fun i) k) #f)
               (else (loop (+ i 1)))
         )
  )
)


(definec (A257906 n) (if (= 1 n) 0 (+ (A257906 (- n 1)) (A257907 n))))

(definec (A257907 n) 
 (cond ((= 1 n) 1)
       (else
         (let ((k (- n 1))
               (a_k (A257906 (- n 1)))
              )
           (let aloop ((h (+ 1 (- 1 a_k)))) ;; 1 - a(k) < h < 0
              (cond ((zero? h) ;; Time to employ plan B, I mean,... Step 2:
                       (let bloop ((h 1))
                          (cond ((and (not_in_range_of? (- a_k h) A257906 k)
                                      (not_in_range_of? h A257907 k)
                                 ) h
                                )
                                (else (bloop (+ 1 h)))
                          )
                       )
                    )
                    ((and (not_in_range_of? (+ a_k h) A257906 k) ;; Equal to: (not-lte? (A257906 (- (+ a_k h))) n) ???
                          (not_in_range_of? h A257907 k)
                     ) h
                    )
                    (else (aloop (+ 1 h)))
              )
           )
         )
       )
 )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some for V.J. Pohjola:

(define (A000503 n) (floor->exact (tan n)))


(define (upto_positive_side start)
  (let loop ((n start) (m #f))
     (let ((k (floor->exact (tan n))))
       (cond ((<= k 0) (loop (+ n 1) m))
             (else
                (let ((ratdiff (/ (abs (- k n)) n)))
                  (cond 
                     ((or (not m) (< ratdiff m))
                       (format #t "A000503(~a) = ~a, ratio = ~a\n" n k (/ (* 1.0 n) k))
                     )
                     ((>= k n) (format #t "!!! A000503(~a) = ~a, ~a >= ~a\n" n k k n))
                  )
                  (loop (+ n 1) (if (not m) ratdiff (min m ratdiff)))
                )
             )
       )
     )
  )
)

(define (A258020 n) (if (= n (floor->exact (tan n))) 0 (+ 1 (A258020 (floor->exact (tan n))))))

(define (A258021 n) (if (= n (floor->exact (tan n))) n (A258021 (floor->exact (tan n)))))

;; (define A258022 (ZERO-POS 0 0 A258021))
;; (define A258024 (NONZERO-POS 1 0 A258021))


(define A258022 (MATCHING-POS 1 0 (lambda (n) (<= (A258021 n) 0))))

(define A258024 (MATCHING-POS 1 0 (lambda (n) (> (A258021 n) 0))))

(define (A258200 n) (- (A258024 (+ 1 n)) (A258024 n)))

(define (A258201rec n) (if (= (floor->exact (tan n)) n) n (min n (A258201rec (floor->exact (tan n))))))

(define (A258201 n)
  (let loop ((n n) (m n))
    (let ((next (floor->exact (tan n))))
     (if (= n next)
         m
         (loop next (min m next))
     )
    )
  )
)

;; (define (A000027seemstobe n) (if (or (zero? n) (= 1 n)) n (max n (A000027seemstobe (floor->exact (tan n))))))

(define A258202 (MATCHING-POS 1 1 (lambda (n) (and (> (A258021 n) 0) (< (A258201 n) 0)))))


(define A258203 (MATCHING-POS 1 1 (lambda (n) (and (> (A258021 n) 0) (> (A258201 n) 0)))))

(define (A258006 n) (- (A258202 (+ 1 n)) (A258202 n)))
(define (A258007 n) (- (A258203 (+ 1 n)) (A258203 n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XFER: Base-factorial/base-factorial-core.ss, or such:

(definec (A257511v3 n) (if (zero? n) n (+ (A265333 n) (A257511v3 (A257687 n)))))

(define (A257511v4 n) (A260736 (A225901 n)))

(define (A257511v5 n) (A056169 (A276076 n)))

;; A273673(n,c): "Zero the exponent of prime(1+A084558(c)) and add it to the exponent of prime(1+A084558(c)-A099563(c)) in the prime factorization of n".


(define (A273673bi n c)
  (if (zero? c)
      n 
      (* (/ n (expt (A000040 (+ 1 (A084558 c))) (A249344bi (+ 1 (A084558 c)) n)))
         (expt (A000040 (+ 1 (- (A084558 c) (A099563 c)))) (A249344bi (+ 1 (A084558 c)) n))
      )
  )
)

(define (A273673 n) (A273673bi (A002260 n) (A004736 n))) ;; o=1 tabl.

;; A(n,0) = n; A(n,r) = A(A273673(n,r), A257687(r)).
;; It should be that A001222(A(n,k)) = A001222(n) for any k.
;; WAS: (define (A275723 n) (A275723bi (A002260 n) (A004736 n)))
(define (A275723 n) (A275723bi (A002260 n) (- (A004736 n) 1)))

;; WAS: (define (A275724 n) (A275723bi (A004736 n) (A002260 n)))
(define (A275724 n) (A275723bi (A004736 n) (- (A002260 n) 1)))

(define (A275723bi n fex)
  (let loop ((n n) (fex fex))
     (cond ((zero? fex) n)
           (else (loop (A273673bi n fex) (A257687 fex)))
     )
  )
)

;; Something like this:
(define (A275725 n) (A275723bi (A002110 (+ 1 (A084558 n))) n))

(define (A275807 n) (/ (A275725 n) 2))

;; And then we should have:

(define (A060131 n) (A072411 (A275725 n)))

(define (A060128 n) (A056170 (A275725 n)))

(define (A060129 n) (A275812 (A275725 n)))

(define (A060129v2 n) (- (+ 1 (A084558 n)) (A275851 n)))

(define (A275851 n) (A056169 (A275725 n)))

(define (A275851v2 n)  (- (+ 1 (A084558 n)) (A060129 n)))

(define A275852 (ZERO-POS 1 0 A275851))

;; A275832-A275853 are now reserved for your use. 

(define (A275832 n) (A007814 (A275725 n)))

(define A153880v3 (ZERO-POS 0 0 (COMPOSE -1+ A275832)))
(define A273670v3 (NONZERO-POS 0 0 (COMPOSE -1+ A275832)))

;; (same-intfuns0? A153880 A153880v3 5040) --> #t

(define A275833 (MATCHING-POS 0 0 (lambda (n) (odd? (A275832 n)))))
(define A275833v2 (MATCHING-POS 0 0 (lambda (n) (odd? (A275726 n)))))

(define A275834 (MATCHING-POS 1 1 (lambda (n) (even? (A275832 n)))))
(define A275834v2 (MATCHING-POS 1 1 (lambda (n) (even? (A275726 n)))))



(define A275813 (MATCHING-POS 0 0 (lambda (n) (odd? (A060131 n)))))

(define A275814 (MATCHING-POS 1 1 (lambda (n) (even? (A060131 n)))))


;; And A001221(A275725(n)) = ? A060128 A060129 ?
;; And A001222(A275725(n)) = ? A060128 A060129 ?


(define (A275730bi n c)
  (let loop ((z 0) (n n) (m 2) (f 1) (c c))
    (let ((d (modulo n m)))
     (cond ((zero? n) z)
           ((zero? c) (loop z (/ (- n d) m) (+ 1 m) (* f m) (- c 1)))
           (else (loop (+ z (* f d)) (/ (- n d) m) (+ 1 m) (* f m) (- c 1)))
     )
    )
  )
)

(define (A275730 n) (A275730bi (A002262 n) (A025581 n)))
(define (A275731 n) (A275730bi (A025581 n) (A002262 n)))


(definec (A275732 n)
  (cond ((zero? (A257261 n)) 1)
        (else (* (A000040 (A257261 n)) (A275732 (A275730bi n (- (A257261 n) 1)))))
  )
)

(definec (A275736 n)
  (cond ((zero? (A257261 n)) 0)
        (else (+ (A000079 (+ -1 (A257261 n))) (A275736 (A275730bi n (- (A257261 n) 1)))))
  )
)

(define (A275732loop n)
  (let loop ((z 1) (n n))
     (let ((y (A257261 n)))
        (cond ((zero? y) z)
              (else (loop (* z (A000040 y)) (A275730bi n (- y 1))))
        )
     )
  )
)

(definec (A275733 n) (if (zero? n) 1 (* (A275732 n) (A003961 (A275733 (A257684 n))))))

;; (same-intfuns0? A060130 (COMPOSE A001221 A275733) 40320) --> #t


(definec (A275734 n) (if (zero? n) 1 (* (A275732 n) (A275734 (A257684 n)))))


(definec (A275735 n) (if (zero? n) 1 (* (A000079 (A257511 n)) (A003961 (A275735 (A257684 n))))))

(define (A275726 n) (A048675 (A275725 n)))

(define (A275803 n) (A051903 (A275725 n))) ;; o=0: Maximal cycle length in A060117 / A060118.

(define A261220 (MATCHING-POS 0 0 (lambda (n) (>= 2 (A275803 n)))))
(define A261220v3 (MATCHING-POS 0 0 (lambda (n) (>= 2 (A060131 n)))))
(define A261220v1 (ZERO-POS 0 0 (lambda (n) (+ (A275947 n) (A276007 n)))))

(definec (A275727 n) (if (zero? n) n (+ (A275736 n) (* 2 (A275727 (A257684 n))))))
(define (A275727v2 n) (A048675 (A275733 n)))
;; (same-intfuns0? A275727 (COMPOSE A087207 A275733) 720) --> #t


(definec (A275728 n) (if (zero? n) n (+ (A275736 n) (A275728 (A257684 n)))))
(define (A275728v2 n) (A048675 (A275734 n)))

(definec (A276010 n) (if (zero? n) n (A003986bi (A275736 n) (A276010 (A257684 n)))))
(define (A276010v2 n) (A087207 (A275734 n)))


(define (A275946 n) (A056169 (A275734 n)))
(define (A275947 n) (A056170 (A275734 n)))

(define (A275948 n) (A056169 (A275735 n)))
(define (A275949 n) (A056170 (A275735 n)))

(define (A275962 n) (A275812 (A275734 n)))

(define (A275964 n) (A275812 (A275735 n)))

(define (A275849 n) (- (A084558 n) (A060502 n)))

(define (A275850 n) (- (A084558 n) (A275806 n)))

(define (A060500 n) (- (A060501 n) (A275851 n))) ;; XXX - Prove!
(define (A060500v2 n) (+ 1 (- (A275849 n) (A275851 n)))) ;; XXX - Actually, prove this!

(define (A060500check_it n) (avg (perm2siteswap-kuu (A060118permvec-short n))))

(define (A060500v3 n) ;; And also this, number of drops in A060118[n] !
  (let ((s (+ 1 (A084558 n))) (p (A060118permvec-short n)))
     (let loop ((d 0) (i 1))
           (if (> i s)
               d
               (loop (+ d (if (< (vector-ref p (- i 1)) i) 1 0)) (+ 1 i))
           )
     )
  )
)

(define (A060501 n) (- (+ 1 (A084558 n)) (A060502 n))) ;; XXX - Prove!

(define (A060501v2 n) (+ 1 (A275849 n)))

(define (A060502 n) (A001221 (A275734 n)))

(define (A060502v4 n) ;; Also: the number of drops (cases where p[i] < i) in the n-th permutation in list A060117.
  (let ((s (+ 1 (A084558 n))) (p (A060117permvec-short n)))
     (let loop ((d 0) (i 1))
           (if (> i s)
               d
               (loop (+ d (if (< (vector-ref p (- i 1)) i) 1 0)) (+ 1 i))
           )
     )
  )
)

(define A276001 (MATCHING-POS 0 0 (lambda (n) (>= 1 (A060502 n)))))
(define A276002 (MATCHING-POS 1 0 (lambda (n) (= 2 (A060502 n)))))
(define A276003 (MATCHING-POS 1 0 (lambda (n) (= 3 (A060502 n)))))

(define (A276004 n) (- (A060502 n) (A060128 n)))


(define (A276004v2 n)
  (let ((fv (list->vector (cons 0 (reverse (n->factbase n))))))
    (let loop ((i 1) (c 0))
         (if (>= i (vector-length fv))
             c
             (let ((d (vector-ref fv i)))
                (cond ((zero? d) (loop (+ 1 i) c))
                      ((zero? (vector-ref fv (- i d))) (loop (+ 1 i) c))
                      (else
                        (begin
                            (vector-set! fv (- i d) 0) ;; Count each falled upon digit only once!
                            (loop (+ 1 i) (+ 1 c))
                        )
                      )
                )
             )
         )
    )
  )
)

;; (same-intfuns0? A276004 (lambda (n) (A000120 (A004198bi (* 2 (A275727 n)) (A276010 n)))) 720) --> #t

(define (A276007 n) ;; Count the number of nonzero digits falling upon other nonzero digits.
  (let ((fv (list->vector (cons 0 (reverse (n->factbase n))))))
    (let loop ((i 1) (c 0))
         (if (>= i (vector-length fv))
             c
             (let ((d (vector-ref fv i)))
                (if (zero? d)
                    (loop (+ 1 i) c)
                    (loop (+ 1 i) (+ c (if (not (zero? (vector-ref fv (- i d)))) 1 0)))
                )
             )
         )
    )
  )
)


(define A276005 (ZERO-POS 0 0 A276004))
(define A276005v2 (ZERO-POS 0 0 A276007))

;; Note! (uniq (map (COMPOSE A275947 A060125 A276005) (iota0 4140))) --> (0)
;; Also: A275804(A000110(n)) = n! = A000142(n). [To be proved.]

(define A276006 (NONZERO-POS 1 0 A276004))
(define A276006v2 (NONZERO-POS 1 0 A276007))


(define A275956 (ZERO-POS 0 0 (lambda (n) (- (A275806 n) (A060502 n)))))

;; XXX - These should all be proved with any other formula-variants:

(define (A060500v1 n)
  (let ((s (+ 1 (A084558 n))) (p (A060118permvec-short n)))
     (let loop ((a 0) (i 1))
           (if (> i s)
               (/ a s)
               (loop (+ a (modulo (- (vector-ref p (- i 1))  i) s)) (+ 1 i))
           )
     )
  )
)

(define (A060500v11 n)
  (let ((s (+ 1 (A084558 n))) (p (A060117permvec-short n)))
     (let loop ((a 0) (i 1))
           (if (> i s)
               (/ a s)
               (loop (+ a (modulo (- i (vector-ref p (- i 1))) s)) (+ 1 i))
           )
     )
  )
)


(define (A060502v1 n)
  (let ((s (+ 1 (A084558 n))) (p (A060118permvec-short n)))
     (let loop ((a 0) (i 1))
           (if (> i s)
               (/ a s)
               (loop (+ a (modulo (- i (vector-ref p (- i 1))) s)) (+ 1 i))
           )
     )
  )
)


(define (A060502v11 n)
  (let ((s (+ 1 (A084558 n))) (p (A060117permvec-short n)))
     (let loop ((a 0) (i 1))
           (if (> i s)
               (/ a s)
               (loop (+ a (modulo (- (vector-ref p (- i 1))  i) s)) (+ 1 i))
           )
     )
  )
)

(define (A060502v2 n) (A275806 (A225901 n)))

(define (A060502v3 n) (avg (perm2siteswap3 (A060118permvec-short n))))

;; After similarly named Maple-function in A060502:
(define (perm2siteswap3 permvec)
  (let ((n (vector-length permvec)))
    (let loop ((s (list)) (i 1))
       (cond ((> i n) s)
             (else
               (let ((diff (modulo (- (vector-ref permvec (- i 1)) i) n)))
                 (loop (cons (if (zero? diff) 0 (- n diff)) s) (+ 1 i))
               )
             )
       )
    )
  )
)

(define (perm2siteswap-muu permvec)
  (let ((n (vector-length permvec)))
    (let loop ((s (list)) (i 1))
       (cond ((> i n) s)
             (else
               (let ((diff (modulo (- (vector-ref permvec (- i 1)) i) n)))
                 (loop (cons (- n diff) s) (+ 1 i))
               )
             )
       )
    )
  )
)


(define (perm2siteswap-muu-v2 permvec)
  (let ((n (vector-length permvec)))
    (let loop ((s (list)) (i 1))
       (cond ((> i n) s)
             (else
               (let ((diff (modulo (- i (vector-ref permvec (- i 1))) n)))
                 (loop (cons diff s) (+ 1 i))
               )
             )
       )
    )
  )
)


(define (perm2siteswap-kuu permvec)
  (let ((n (vector-length permvec)))
    (let loop ((s (list)) (i 1))
       (cond ((> i n) s)
             (else
               (let ((diff (modulo (- (vector-ref permvec (- i 1))  i) n)))
                 (loop (cons diff s) (+ 1 i))
               )
             )
       )
    )
  )
)



;; Not in OEIS yet: (a(n) = A060502(n) + A275851(n)):

(define (A275853 n) (+ (A060502 n) (A275851 n)))

;; (same-intfuns0? A275853 (lambda (n) (avg (perm2siteswap-muu (A060118permvec-short n)))) 40320) --> #t


;; (map (lambda (n)  (avg  (perm2siteswap-muu (A060118permvec-short n)))) (iota0 52))
;; (1 1 2 2 2 1 3 2 3 3 3 2 3 3 2 2 2 2 3 2 2 2 2 1 4 3 3 3 3 2 4 3 4 4 4 3 4 4 3 3 3 3 4 3 3 3 3 2 4 3 4 4 4)

;; (same-intfuns0? (lambda (n) (avg (perm2siteswap-muu (A060118permvec-short n)))) (lambda (n) (+ (A060502 n) (A275851 n))) 5040) --> #t

;; Neither is:
;; (map (COMPOSE (lambda (n) (avg (perm2siteswap-muu (A060118permvec-short n)))) A225901) (iota0 52))
;; 1, 1, 2, 1, 2, 2, 3, 2, 2, 1, 2, 2, 3, 3, 2, 2, 2, 2, 3, 2, 3, 2, 3, 3, 4, 3, 3, 2, 3, 3, 3


(define (A275729 n) (A048675 (A275735 n)))
(define (A275806 n) (A001221 (A275735 n)))


;; A275803-A275814 are now reserved for your use. 

(define A275804 (MATCHING-POS 0 0 (lambda (n) (>= 1 (A275811 n)))))
(define A275804v2 (NONZERO-POS 0 0 (lambda (n) (A008683 (A275734 n)))))
(define A275804v3 (MATCHING-POS 0 0 (lambda (n) (= (A060502 n) (A060130 n)))))
(define A275804v4 (ZERO-POS 0 0 A275947))


(define A275805 (MATCHING-POS 1 1 (lambda (n) (< 1 (A275811 n)))))
(define A275805v2 (ZERO-POS 1 1 (lambda (n) (A008683 (A275734 n)))))



(definec (A275808 n) (if (zero? n) n (A003987bi (A275736 n) (A275808 (A257684 n)))))

(define A275809 (ZERO-POS 0 0 A275808))

(definec (A275810 n) (- (A275809 n) (A275809 (- n 1)))) ;; o=1. First differences of A275809.

;; (same-intfuns0?  (COMPOSE A051903 A275734)  (COMPOSE A264990 A225901) 40320) --> #t

(define (A275811 n) (A051903 (A275734 n)))
(define (A275811v2 n) (A264990 (A225901 n)))



;; Submit these "impostors" (or false friends) later:
;; (same-intfuns1? A275811 (COMPOSE A072411 A275734) 5040) --> 287

;; (same-intfuns1? A264990 (COMPOSE A072411 A275735) 5040) --> 161

;; Not at all equal to these:

;; (definec (Asomenewone6 n) (if (zero? n) 1 (* (A257511 n) (Asomenewone6 (A257684 n)))))

;; (definec (Asomenewone9 n) (if (zero? n) 1 (lcm (A257511 n) (Asomenewone9 (A257684 n)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; XXX - XFER Seqs/Factorial/factorial-base.ss
;; Or: ;; /XFER: Base-F/Base.Factorial.core.ss or such...


(define (A009445 n) (A000142 (+ n n 1))) ;;  ;; o=0: (2n+1)! XFER: core-factorial or such ?

(define (A002674 n) (/ (A000142 (* 2 n)) 2)) ;; o=1: (2n)!/2.

(define (A002674v2 n) (* n (A009445 (- n 1)))) ;; As n*(2n-1)! = (2n)!/2, yes!


;; XFER: Factorial/factorial-base-core.ss

(definec (A060130v3 n) (if (zero? n) n (+ (A257511 n) (A060130v3 (A257684 n)))))


;; A059590 [Bottomley] o=0: Sum of distinct factorials (0! and 1! not treated as distinct). 

(define (A059590 n)
   (let loop ((n n) (s 0) (f 1) (i 2))
      (cond ((zero? n) s)
            ((even? n) (loop (/ n 2) s (* i f) (+ 1 i)))
            (else (loop (/ (- n 1) 2) (+ s f) (* i f) (+ 1 i)))
      )
   )
)

(definec (A059590v1 n) ;; Recursive version.
  (cond ((zero? n) n)
        ((even? n) (A153880 (A059590v1 (/ n 2))))
        (else (+ 1 (A153880 (A059590v1 (/ (- n 1) 2)))))
  )
)


;; 
(define (A276091v2 n) (A225901 (A059590 n)))


(define (A276091 n)
   (let loop ((n n) (s 0) (f 1) (i 2))
      (cond ((zero? n) s)
            ((even? n) (loop (/ n 2) s (* i f) (+ 1 i)))
            (else (loop (/ (- n 1) 2) (+ s (* (- i 1) f)) (* i f) (+ 1 i)))
      )
   )
)

(definec (A276091v1 n) ;; Recursive version.
  (cond ((zero? n) n)
        ((even? n) (A255411 (A276091v1 (/ n 2))))
        (else (+ 1 (A255411 (A276091v1 (/ (- n 1) 2)))))
  )
)


(definec (A276091v4 n) ;; Another recursive version.
  (cond ((zero? n) n)
        ((even? n) (A276340 (A276091v4 (/ n 2))))
        (else (+ 1 (A276340 (A276091v4 (/ (- n 1) 2)))))
  )
)

(definec (A275959 n) ;; Recursive version.
  (cond ((zero? n) n)
        ((even? n) (A255411 (A153880 (A275959 (/ n 2)))))
        (else (+ 1 (A255411 (A153880 (A275959 (/ (- n 1) 2))))))
  )
)


(define (A275959v2 n) (A276089 (A276091 n)))

(define A275959slow (FIXED-POINTS 0 0 (lambda (n) (A225901 n))))


(definec (A276082 n)
  (cond ((zero? n) n)
        ((even? n) (A153880 (A276082 (/ n 2))))
        (else (+ 1 (A255411 (A276082 (/ (- n 1) 2)))))
  )
)


(definec (A276083 n)
  (cond ((zero? n) n)
        ((even? n) (A255411 (A276083 (/ n 2))))
        (else (+ 1 (A153880 (A276083 (/ (- n 1) 2)))))
  )
)



(define (A276008 n) ;; o=0: Replace all nonzero digits by ones.
   (let loop ((n n) (s 0) (f 1) (i 2))
      (if (zero? n) 
         s
         (let ((d (modulo n i)))
            (if (zero? d)
                (loop (/ n i) s (* i f) (+ 1 i))
                (loop (/ (- n d) i) (+ s f) (* i f) (+ 1 i))
            )
         )
      )
   )
)

(define (A276008v2 n) (A059590 (A275727 n)))

;; Standalone version: ;; o=0: Decrement each nonzero digit by one in factorial base representation.
(define (A276009 n)
   (let loop ((n n) (s 0) (f 1) (i 2))
      (if (zero? n) 
         s
         (let ((d (modulo n i)))
            (if (zero? d)
                (loop (/ n i) s (* i f) (+ 1 i))
                (loop (/ (- n d) i) (+ s (* f (- d 1))) (* i f) (+ 1 i))
            )
         )
      )
   )
)

(define (A276009v2 n) (- n (A276008 n)))


(define (A276089 n) ;; Aerate the factorial base representation of n.
   (let loop ((n n) (s 0) (f 1) (i 2) (j 2))
      (if (zero? n) 
         s
         (let ((d (modulo n i)))
            (loop (/ (- n d) i) (+ s (* f d)) (* j (+ 1 j) f) (+ 1 i) (+ 2 j))
         )
      )
   )
)


(define (A276090 n) ;; Deaerate the factorial base representation of n, left inverse of A276089.
   (let loop ((n n) (s 0) (f 1) (i 2) (j 2))
      (if (zero? n) 
         s
         (let ((d (modulo n j)))
            (loop (floor->exact (/ (/ (- n d) j) (+ 1 j))) (+ s (* f d)) (* i f) (+ 1 i) (+ 2 j))
         )
      )
   )
)

;; Add to Examples and comments:
;; For n = 311 ("22321") we pick the digits at odd positions positions 1, 3 and 5, thus we get a(311) = 2*3! + 3*2! + 1*1! = 19.
;; For n = 373 ("30201") we pick the digits from those same positions and construct a(373) = 3*3! + 2*2! + 1*1! = 23.

;; This "deaerates" A276089(n) by picking only the digits from the odd positions of its factorial base representation. Of course, when computed for an arbitrary n, those digits, when  "compressed" into a(n) are not necessarily valid digits in standard factorial base representation (A007623), as the example given for n=311 shows.



(define (A266123 n) ;; Shift right & decrement by 1 all other digits except ones and zeros. Cf. A257684.
   (let loop ((n n) (z 0) (i 2) (f 0))
      (cond ((zero? n) z)
            (else
              (let ((d (remainder n i)))
                      (loop (quotient n i)
                            (+ z (* f (- d (if (<= d 1) 0 1))))
                            (+ 1 i)
                            (if (zero? f) 1 (* f (- i 1)))
                      )
              )
            )
      )
   )
)

;; A266186-A266197 are now reserved for your use. 

(define (A266193 n) ;; o=0: Shift right & decrement by 1 the maximal digits. Cf. A257684, A266123. Inverse of A153880.
   (let loop ((n n) (z 0) (i 2) (f 0))
      (cond ((zero? n) z)
            (else
              (let ((d (remainder n i)))
                      (loop (quotient n i)
                            (+ z (* f (- d (if (< d (- i 1)) 0 1))))
                            (+ 1 i)
                            (if (zero? f) 1 (* f (- i 1)))
                      )
              )
            )
      )
   )
)


(defineperm1 (A266191 n)
  (cond ((<= n 3) n)
        (else
          (let ((prev1 (A266191 (- n 1))) (prev2 (A266191 (- n 2))))
            (let loop ((k 1))
               (cond ((and (not-lte? (A266192 k) (- n 1)) (isA003714? (* k prev1 prev2))) k)
                     (else (loop (+ 1 k)))
               )
            )
          )
        )
  )
)

(define (A266192 n) (A266191 (- n)))

;;;;;;;;;;;;;;;;;;


(define (A007623 n) ;; New version! Use mod instead of modulo in R6RS
   (let loop ((n n) (s 0) (p 1) (i 2))
      (if (zero? n) s (let ((d (modulo n i))) (loop (/ (- n d) i) (+ (* p d) s) (* 10 p) (+ 1 i))))
   )
)

;;

;; XXX: Change all to starting offset=0: !
(definec (A265905 n) (if (= 1 n) n (+  (A265905 (- n 1)) (A153880 (A265905 (- n 1)))))) ;; o=1. Cf. A001710.

(define (A265906 n) (A153880 (A265905 n)))

(define (A265906v2 n) (- (A265905 (+ 1 n)) (A265905 n)))

(definec (A265907 n) (if (= 1 n) n (+  (A265907 (- n 1)) (A255411 (A265907 (- n 1))))))

(define (A265908 n) (A255411 (A265907 n)))

(define (A265908v2 n) (- (A265907 (+ 1 n)) (A265907 n)))

(define (A275940bi row col) (if (= 1 row) (A265905 col) (- (A275940bi (- row 1) (+ 1 col)) (A275940bi (- row 1) col))))

;; A275946-A275965 are now reserved for your use.

(define (A275950 n) (A275950bi (A002260 n) (A004736 n)))
(define (A275951 n) (A275950bi (A004736 n) (A002260 n)))

(define (A275950bi row col) (if (= 1 row) (A265905 col) (- (A275950bi (- row 1) (+ 1 col)) (A275950bi (- row 1) col))))

(define (A275953 n) (- (A265906 (+ 1 n)) (A265906 n)))

(definec (A275955 n) (A275950bi n 1))

(define (A275960 n) (A275960bi (A002260 n) (A004736 n)))
(define (A275961 n) (A275960bi (A004736 n) (A002260 n)))

(define (A275960bi row col) (if (= 1 row) (A265907 col) (- (A275960bi (- row 1) (+ 1 col)) (A275960bi (- row 1) col))))

(define (A275963 n) (- (A265908 (+ 1 n)) (A265908 n)))

(definec (A275965 n) (A275960bi n 1))

;; XXX: Uncomment when INVERSE-BINOMIAL-TRANSFORM and BINOMIAL-TRANSFORM have been transferred to transforms.ss 
;; (define A275955off0 (INVERSE-BINOMIAL-TRANSFORM (COMPOSE A265905 1+)))

;; (define A265905off0 (BINOMIAL-TRANSFORM A275955off0))


;; (define A275965off0 (INVERSE-BINOMIAL-TRANSFORM (COMPOSE A265907 1+)))

;; (define A265907off0 (BINOMIAL-TRANSFORM A275965off0))

;; (map A275955off0 (iota0 16))
;; (1 2 6 24 150 918 6876 63018 589518 6272712 76110150 950826294 13044895668 197045295354 3070861314894 51493780503216 931608094164870)
;; (map A275955 (iota 17))
;; (1 2 6 24 150 918 6876 63018 589518 6272712 76110150 950826294 13044895668 197045295354 3070861314894 51493780503216 931608094164870)



(define Afacbase_carryless (ZERO-POS 0 0 (lambda (n) (- (* 2 (A034968 n)) (A034968 (+ n (A153880 n))))))) ;; Not really, but a superset of carryless stuff (sum of nonconsecutive factorial numbers + * something?)

(define Afacbase_carryfull (COMPLEMENT 1 Afacbase_carryless))

;; Replace in factorial base representation of n each nonzero digit d with (m+1)-d, where m is the maximal digit allowed in that location, then convert back to decimal.

(define (A225901 n) ;; o=0: [Tek] Write n in factorial base, then replace each nonzero digit d of radix k by k-d.
   (let loop ((n n) (z 0) (m 2) (f 1))
      (cond ((zero? n) z)
            (else (loop (quotient n m)
                        (if (zero? (modulo n m)) z (+ z (* f (- m (modulo n m)))))
                        (+ 1 m)
                        (* f m)
                  )
            )
      )
   )
)

;; A276001-A276012 are now reserved for your use. 

(define (A276011 n) (A273662 (A225901 (A273670 n))))

(define (A276012 n) (A273663 (A225901 (A256450 n))))

(define (A275952 n) (A225901 (A256450 (A225901 n))))

(define (A275954 n) (A225901 (A273670 (A225901 n))))


(define (A276146 n) (A034968 (A225901 n)))

(define A153880v2 (ZERO-POS 0 0 A260736))

(definec0 (A273670 n) (if (zero? n) 1 (let ((prev (A273670 (- n 1)))) (cond ((even? prev) (+ 1 prev)) ((not (zero? (A260736 (+ 1 prev)))) (+ 1 prev)) (else (+ 2 prev))))))

(define A273670v2 (NONZERO-POS 0 0 A260736))

(define (A275840 n) (- (A273670 n) (A256450 n)))


(definec (A273662 n) (if (= 1 n) 0 (+ (A257680 n) (A273662 (- n 1)))))

(define (A273662v2 n) (- (A257682 n) 1))

(definec (A273663 n) (if (= 1 n) 0 (+ (A257680 (A225901 n)) (A273663 (- n 1)))))

;; (same-intfuns0? A001477 (COMPOSE A273663 A273670) 16387) --> #t


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New, o=0 version of A256450:
;;;;;;;;;;;;;;;;;;;;;;;;;;

(definec0 (A256450naive n) ;; o=0: 
   (if (zero? n) 1
       (let ((prev (A256450naive (- n 1))))
           (cond ((even? prev) (+ 1 prev)) ;; Actually unnecessary, but optimizes for the next clause:
                 ((> (A257511 (+ 1 prev)) 0) (+ 1 prev))
                 (else (+ 2 prev))
           )
       )
   )
)

(definec0 (A256450 n)
   (let* ((k (A258198 n))
          (d (- n (A258199 n)))
          (f (A000142 (+ 1 k)))
         )
      (cond ;; ((<= n 1) n)
            ((< d f) (+ f d))
            (else
               (+ (* f (+ 2 (floor->exact (/ (- d f) (A258199 n)))))
                  (A256450 (modulo (- d f) (A258199 n)))
               )
            )
      )
   )
)


(define (A257503bi row col) (if (= 1 row) (A256450 (- col 1)) (A255411 (A257503bi (- row 1) col))))
(define (A257503 n) (A257503bi (A002260 n) (A004736 n)))

(define (A257505bi row col) (if (= 1 col) (A256450 (- row 1)) (A255411 (A257505bi row (- col 1)))))
(define (A257505 n) (A257505bi (A002260 n) (A004736 n)))


;;;;;;;;;;;;;;;;;;

(define (A276931 n) (/ (A276932 n) 2))

(define (A276932 n) (A153880 (A273670 n)))

(define A276932v2 (MATCHING-POS 0 1 (lambda (n) (= 2 (A276949 n)))))

(define (A276933 n) (A153880 (A153880 (A273670 n))))
(define A276933v2 (MATCHING-POS 0 1 (lambda (n) (= 3 (A276949 n)))))

(define (A276934 n) (/ (A276933 n) 6))

(define (A276947 n) (- (A256450 n) (A256450 (- n 1)))) ;; o=1: First differences of A256450.

(define (A276948 n) (- (A273670 n) (A273670 (- n 1)))) ;; o=1: First differences of A273670.



;;
(definec (A276949 n) (cond ((zero? n) n) ((= 1 (A276950 n)) 1) (else (+ 1 (A276949 (A266193 n))))))
(define (A276949v2 n) (A257679 (A225901 n)))

(define (A276950 n) (if (zero? (A260736 n)) 0 1)) ;; o=0: Characteristic function for A273670: 1 if there is at least one maximal digit present in the factorial representation of n (A007623), otherwise 0. 
(define (A276950v2 n) (A257680 (A225901 n)))

(definec (A276951 n) (cond ((zero? n) n) ((not (zero? (A276950 n))) (A276952 n)) (else (A276951 (A266193 n)))))

;; Partial sums of A276950: a(0) = 0; for n >= 1, a(n) = A276950(n) + a(n-1). 
(definec (A276952 n) (if (zero? n) n (+ (A276950 n) (A276952 (- n 1)))))
(define (A276952v2 n) (+ 1 (A273663 n)))

(define (A276953bi row col) (if (= 1 row) (A273670 (- col 1)) (A153880 (A276953bi (- row 1) col))))
(define (A276953 n) (A276953bi (A002260 n) (A004736 n)))

;; (same-intfuns1? A000027 (lambda (n) (A276953bi (A276949 n) (A276951 n))) 40320) --> #t

(define (A276954 n) (let ((col (A276951 n)) (row (A276949 n))) (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2))))

;; (same-intfuns1? A000027 (COMPOSE A276953 A276954) 5050) --> #t
;; (same-intfuns1? A000027 (COMPOSE A276954 A276953) 1081) --> #t
;; (same-intfuns1? A276954 (COMPOSE A257504 A275847) 5040) --> #t


(define (A276955bi row col) (if (= 1 col) (A273670 (- row 1)) (A153880 (A276955bi row (- col 1)))))
(define (A276955 n) (A276955bi (A002260 n) (A004736 n)))

;; (same-intfuns1? A276955 (COMPOSE A275848v2 A257505) 5040) --> #t

(define (A276956 n) (let ((row (A276951 n)) (col (A276949 n))) (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2))))

;; (same-intfuns1? A000027 (COMPOSE A276955 A276956) 5050) --> #t
;; (same-intfuns1? A000027 (COMPOSE A276956 A276955) 1081) --> #t

;; (same-intfuns1? A276956 (COMPOSE A257506 A275847) 40320) --> #t

;; A108217: [Miklos Kristof] o=0: a(n) = n! + (n-2)!. (Row 5 of A276955).
(define (A108217 n) (if (<= n 1) 1 (* (A002061 n) (A000142 (- n 2)))))

;; A001344 [NJAS] o=-1: a(n) = sum_{k=0..2} (n+k)! * C(2,k). (from a(1) = 11 onward Row 2 of A276588, Row 8 of A276955).
(define (A001344 n) (cond ((= -1 n) 2) (else (* (A028387 (+ 1 n)) (A000142 n)))))

(define (A052649 n) (if (zero? n) 2 (+ (A000142 n) (* 2 (A000142 (+ 1 n)))))) ;; Row 4 of A276955.

(define (A054119 n) (if (<= n 1) (+ 1 n) (+ (A000142 n) (A000142 (- n 1)) (A000142 (- n 2))))) ;; Row 6 of A276955.

(define (A052572 n) (if (zero? n) 1 (* (+ 3 n) (A000142 n)))) ;; Row 7 of A276955, from a(2)=10 onward.
(define (A052572v2 n) (if (<= n 1) (* (+ 1 n) (+ 1 n)) (+ (A000142 (+ 1 n)) (* 2 (A000142 n)))))

(define (A225658 n) (+ (A000142 n) (A000142 (+ 1 n)) (* 3 (A000142 (+ 2 n))))) ;; [R. J. Mathar] o=0: a(n) = n! +(n+1)! +3*(n+2)!. 


(define (A276940 n) (if (= 1 n) 2 (* n n n (A000142 (- n 2))))) ;; From a(2)=27 onward row 20 of A276955.
(define (A276940v2 n) (* n (A054119 n)))
(define (A276940v3 n) (cond ((= 1 n) 2) ((= 2 n) 8) (else (+ (A000142 (+ 1 n)) (A000142 (- n 1)) (A000142 (- n 2))))))

;; 


(define (A276616 n) (A276616bi (A002260 n) (A004736 n)))
(define (A276616bi row col) (/ (A276953bi row col) (A000142 row)))

(define (A276617 n) (A276617bi (A002260 n) (A004736 n)))
(define (A276617bi row col) (/ (A276955bi row col) (A000142 col)))

(define (A276957 n) (cond ((zero? n) n) ((zero? (A276950 n)) (A255411 (A266193 n))) (else (A256450 (A273663 n)))))

(define (A276958 n) (cond ((zero? n) n) ((zero? (A257680 n)) (A153880 (A257684 n))) (else (A273670 (A273662 n)))))


;; (same-intfuns0? A001477 (COMPOSE A276957 A276958) 40320) --> #t
;; (same-intfuns0? A001477 (COMPOSE A276958 A276957) 40320) --> #t



;;;;;;;;;;;;;;;;;;;;
;; Entanglement-permutations, corrected because of changed offset of A256450:

(definec (A255565 n)
  (cond ((zero? n) n)
        ((zero? (A257680 n)) (* 2 (A255565 (A257685 n))))
        (else (+ 1 (* 2 (A255565 (A273662 n)))))
  )
)


(definec (A255566 n)
  (cond ((zero? n) n)
        ((even? n) (A255411 (A255566 (/ n 2))))
        (else (A256450 (A255566 (/ (- n 1) 2))))
  )
)


;; Now offset 1:
(definec (A255567 n) (cond ((<= n 2) n) ((odd? n) (+ 1 (A255567 (- n 1)))) (else (A255411 (A255567 (/ n 2))))))

;; At least same for terms a(2) - a(255):
(define A255567probably (MATCHING-POS 2 1 (lambda (n) (= (+ 1 (A256450 (A255411 n))) (A255411 (A256450 n))))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;


(definec (A273665 n)
  (cond ((zero? n) n)
        ((zero? (A257680 (A225901 n))) (* 2 (A273665 (A266193 n))))
        (else (+ 1 (* 2 (A273665 (A273663 n)))))
  )
)


(definec (A273666 n)
  (cond ((zero? n) n)
        ((even? n) (A153880 (A273666 (/ n 2))))
        (else (A273670 (A273666 (/ (- n 1) 2))))
  )
)

;; (same-intfuns1? A000027 (COMPOSE A273665 A273666) 255) --> #t

;; (same-intfuns1? A000027 (COMPOSE A273666 A273665) 5040) --> #t


;; And the cross-entanglements:

(definec0 (A273667 n)
  (cond ((zero? n) n)
        ((zero? (A257680 (A225901 n))) (A255411 (A273667 (A266193 n))))
        (else (A256450 (A273667 (A273663 n))))
  )
)

(definec0 (A273668 n)
  (cond ((zero? n) n)
        ((zero? (A257680 n)) (A153880 (A273668 (A257684 n))))
        (else (A273670 (A273668 (A273662 n))))
  )
)


;; (same-intfuns0? A001477 (COMPOSE A273667 A273668) 1200) --> #t

;; (same-intfuns0? A001477 (COMPOSE A273668 A273667) 1200) --> #t



;; And their compositions with A225901:

(define (A275835 n) (A273667 (A225901 n)))

(define (A275836 n) (A225901 (A273668 n)))

(define (A275837 n) (A225901 (A273667 n)))

(define (A275838 n) (A273668 (A225901 n)))

(define A275839 (FIXED-POINTS 0 0 A275837))

(define A275839v2 (MATCHING-POS 0 0 (lambda (n) (= (A225901 n) (A273667 n)))))

(define (A275841 n) (A273663 (A275837 (A273670 n))))

(define (A275842 n) (A273663 (A275838 (A273670 n))))

;; "Deficient (or simpler!) variants of A273667 & A273668":


(definec0 (A275845 n)
  (cond ((zero? n) n)
        ((zero? (A257680 (A225901 n))) (A255411 (A266193 n)))
        (else (A256450 (A275845 (A273663 n))))
  )
)

(definec0 (A275846 n)
  (cond ((zero? n) n)
        ((zero? (A257680 n)) (A153880 (A257684 n)))
        (else (A273670 (A275846 (A273662 n))))
  )
)



(definec0 (A275847 n)
  (cond ((zero? n) n)
        ((zero? (A257680 (A225901 n))) (A255411 (A275847 (A266193 n))))
        (else (A256450 (A273663 n)))
  )
)

(define (A275847v2 n) (A257503 (A276954 n)))
(define (A275847v3 n) (A257505 (A276956 n)))

;; (same-intfuns1? A275847 A275847v2 40320) --> #t
;; (same-intfuns1? A275847 A275847v3 40320) --> #t


(definec0 (A275848 n)
  (cond ((zero? n) n)
        ((zero? (A257680 n)) (A153880 (A275848 (A257684 n))))
        (else (A273670 (A273662 n)))
  )
)

(define (A275848v2 n) (A276953 (A257504 n)))
(define (A275848v3 n) (A276955 (A257506 n)))

;; (same-intfuns1? A275848 A275848v2 40320) --> #t
;; (same-intfuns1? A275848 A275848v3 40320) --> #t


;; (same-intfuns0? A273667 (COMPOSE A255566 A273665) 1200) --> #t

;; (same-intfuns0? A273668 (COMPOSE A273666 A255565) 1200) --> #t

(define (A276149 n) (if (zero? n) n (* (A048764 n) (+ 1 (- (A084558 n) (A099563 n))))))

(define (A225901v1 n) (if (zero? n) n (+ (A276091 (A275736 n)) (A153880 (A225901v1 (A257684 n)))))) ;; Elegant!

(define (A225901rec n) (if (zero? n) n (+ (A276149 n) (A225901rec (A257687 n))))) ;; Boring!


;; Why? Yes, of course:
(define (A225901v2 n) (A257684 (A225901 (A153880 n))))
(define (A225901v3 n) (A266193 (A225901 (A255411 n))))

(define (Ajoku1 n)  (A273662 (A225901 (A273670 n))))
(define (Ajoku2 n)  (A273663 (A225901 (A256450 n))))

;; (same-intfuns0? A001477 (COMPOSE Ajoku1 Ajoku2) 255) --> #t
;; (same-intfuns0? A001477 (COMPOSE Ajoku2 Ajoku1) 4096) --> #t


;; A276073-A276094 are now reserved for your use. 
;; A276075: Cf. A048675
(definec (A276075 n)
  (cond ((= 1 n) (- n 1))
        (else (+ (* (A067029 n) (A000142 (A055396 n)))
                 (A276075 (A028234 n))
              )
        )
  )
)

;;;;;;;;;;;;;;;;;;;
;; XFER: Primorials-core.ss
 
(definec (A002110 n) (if (zero? n) 1 (* (A000040 n) (A002110 (- n 1)))))


(define (A057588 n) (- (A002110 n) 1)) ;; o=1: [Mario Velucchi] Kummer numbers: -1 + product of first n consecutive primes.

;; A143293 [Gary W. Adamson] o=0: Partial sums of A002110, the primorial numbers. 
(definec (A143293 n) (if (zero? n) (A002110 n) (+ (A002110 n) (A143293 (- n 1)))))


(definec (A005867 n) (if (zero? n) 1 (* (- (A000040 n) 1) (A005867 (- n 1)))))

(define (A006094 n) (* (A000040 n) (A000040 (+ 1 n)))) ;; Products of 2 successive primes.

(define (A001248 n) (A000290 (A000040 n)))


(definec (A276085 n)
  (cond ((= 1 n) (- n 1))
        (else (+ (* (A067029 n) (A002110 (+ -1 (A055396 n))))
                 (A276085 (A028234 n))
              )
        )
  )
)

(define (A276086 n)
   (let loop ((n n) (t 1) (i 1))
      (if (zero? n) 
         t
         (let* ((p (A000040 i))
                (d (modulo n p))
               )
            (loop (/ (- n d) p) (* t (expt p d)) (+ 1 i))
         )
      )
   )
)

(define (A276087 n) (A276086 (A276086 n)))

;; A054842 [Bottomley] o=0: If n = a + 10 * b + 100 * c + 1000 * d + ... then a(n) = (2^a) * (3^b) * (5^c) * (7^d) * ... 
(define (A054842 n)
  (let ((b 10))
   (let loop ((n n) (t 1) (i 1))
      (if (zero? n) 
         t
         (let ((d (modulo n b)))
            (loop (/ (- n d) b) (* t (expt (A000040 i) d)) (+ 1 i))
         )
      )
   )
  )
)


;; A049345 [R. K. Guy] o=0: n written in primorial base. 

(define (A049345 n) ;; Use mod instead of modulo in R6RS
 (if (>= n 2100)
     (error "A049345: ambiguous primorial representation when n is larger than 2099:" n)
     (let loop ((n n) (s 0) (t 1) (i 1))
        (if (zero? n) s (let* ((p (A000040 i)) (d (modulo n p))) (loop (/ (- n d) p) (+ (* t d) s) (* 10 t) (+ 1 i))))
     )
 )
)

;; A267263 [Cade Brown] o=0: Number of nonzero digits in representation of n in primorial base.
(define (A267263 n) (A001221 (A276086 n)))

;; A060735 [Robert G. Wilson v] o=1: Where n / (phi(n) + 1) increases.
(define A060735 (ZERO-POS 1 1 (lambda (n) (+ -1 (A267263 n)))))


(define (A276080 n) (A276075 (A206296 n)))
;; (definec (A_is_not276080 n) (if (<= n 1) n (+ (A153880 (A_is_not276080 (- n 1))) (A_is_not276080 (- n 2)))))


(definec (A129251v2 n) (if (= 1 n) 0 (+ (A129251v2 (A028234 n)) (if (>= (A067029 n) (A020639 n)) 1 0))))


(definec (A276076 n) (if (zero? n) 1 (* (expt (A000040 (A084558 n)) (A099563 n)) (A276076 (A257687 n)))))

(definec (A276076v2 n) (if (zero? n) 1 (* (A275733 n) (A276076v2 (A276009 n)))))

(define (A276073 n) (A048675 (A276076 n)))

(define (A276074 n) (A248663 (A276076 n)))


;; XFER primorial-base-dispersions.ss or such

;; Just a quick work with defineperm1 now so that I get the inverses without too much pain:

(define (A276943bi row col) (if (= 1 row) (A276155 col) (A276154 (A276943bi (- row 1) col))))
(defineperm1 (A276943 n) (A276943bi (A002260 n) (A004736 n)))

(define (A276944 n) (A276943 (- n)))



(define (A276945bi row col) (if (= 1 col) (A276155 row) (A276154 (A276945bi row (- col 1)))))
(defineperm1 (A276945 n) (A276945bi (A002260 n) (A004736 n)))

(define (A276946 n) (A276945 (- n)))

(define (A276939 n) (+ (A002110 n) (A002110 (+ 1 n)))) ;; Row 2 of A276945.

(define (A286623 n) (A286623bi (A002260 n) (A004736 n)))
(define (A286623bi row col) (/ (A276943bi row col) (A002110 (- row 1))))

(define (A286625 n) (A286625bi (A002260 n) (A004736 n)))
(define (A286625bi row col) (/ (A276945bi row col) (A002110 (- col 1))))

;; XFER: These should be somewhere else, num-theory-misc.ss ?

;; A129251 [Zumkeller] o=1: Number of distinct prime factors p of n such that p^p is a divisor of n. 
(define (A129251 n) (if (= 1 n) 0 (+ (A129251 (A028234 n)) (if (zero? (modulo n (expt (A020639 n) (A020639 n)))) 1 0))))

(define A048103 (ZERO-POS 1 1 A129251))

(define A100716 (NONZERO-POS 1 1 A129251))


(definec (A276077 n) (if (= 1 n) 0 (+ (A276077 (A028234 n)) (if (> (A067029 n) (A055396 n)) 1 0))))

(define A276078 (ZERO-POS 1 1 A276077))

(define A276079 (NONZERO-POS 1 1 A276077))

;; Number of distinct prime factors p of n such that p^(A000720(p)) is a divisor of n, where A000720(p) gives the index of prime p, 1 for 2, 2 for 3, 3 for 5, and so on.

(definec (A276935 n) (if (= 1 n) 0 (+ (A276935 (A028234 n)) (if (= (A067029 n) (A055396 n)) 1 0))))

(define A276936 (NONZERO-POS 1 1 A276935)) ;; Numbers n with at least one distinct prime factor prime(k) such that prime(k)^k, but not prime(k)^(k+1) is a divisor of n.

(define A276937 (MATCHING-POS 1 1 (lambda (n) (and (not (zero? (A276935 n))) (zero? (A276077 n))))))

(define (A276938 n) (A003961 (A276937 n))) ;; Second row of A276941.

;; (define Amaybe_later (MATCHING-POS 1 1 (lambda (n) (= 1 (A001221 (A276937 n))))))

(define (A276941 n) (A276941bi (A002260 (- n 1)) (A004736 (- n 1)))) ;; o=2 (permutation of A276078 without 1).
(define (A276941bi row col) (if (= 1 row) (A276937 col) (A003961 (A276941bi (- row 1) col))))

(define (A276942 n) (A276941bi (A004736 (- n 1)) (A002260 (- n 1))))


;; A277006-A277027 are now reserved for your use. 

;; Convert the run length factorial representation to the prime exponents.
(define (A005940off0v2 n) ;; Should check?
   (let loop ((p 1) (n n) (r 0) (i 1))
        (cond ((zero? n) (* p (expt (A000040 i) r)))
              ((even? n) (loop (* p (expt (A000040 i) r)) (/ n 2) 0 (+ 1 i)))
              (else (loop p (/ (- n 1) 2) (+ 1 r) i))
        )
 
   )
)

;; Number of maximal runs of 1-bits (in binary expansion of n) such that the length of run > 1 + the total number of zeros to the right of that run:
(define (A277007 n)
   (let loop ((e 0) (n n) (z 0) (r 0))
        (cond ((zero? n) (+ e (if (> r (+ 1 z)) 1 0)))
              ((even? n) (loop (+ e (if (> r (+ 1 z)) 1 0)) (/ n 2) (+ 1 z) 0))
              (else (loop e (/ (- n 1) 2) z (+ 1 r)))
        )
 
   )
)

;; (same-intfuns0? A277007 (COMPOSE A276077 A005940 1+) 16384) --> #t


(define A277008 (ZERO-POS 0 0 A277007))
(define A277009 (NONZERO-POS 1 0 A277007))



;; Left inverse of A277012: (rewrite the run length factorial representation to the real one)
(define (A277011 n)
   (let loop ((s 0) (n n) (r 0) (i 2) (f 1))
        (cond ((zero? n) (+ s (* r f)))
              ((even? n) (loop (+ s (* r f)) (/ n 2) 0 (+ 1 i) (* i f)))
              (else (loop s (/ (- n 1) 2) (+ 1 r) i f))
        )
 
   )
)

;; (same-intfuns0? A276076 (COMPOSE A005940 1+ A277012) 5040) --> #t
;; (same-intfuns0? A277011 (COMPOSE A276075 A005940 1+) 16383) --> #t

;; (same-intfuns0? A001477 (COMPOSE A277011 A277012) 40320) --> #t

;; Does NOT work this way!: (definec (A277012 n) (if (zero? n) n (A085207bi (A000225 (A099563 n)) (A277012 (A257687 n)))))

;; factorial base representation of n (A007623) rewritten to base-2 number with 0's for 0's and any nonzero digits k --> run of k 1's with one trailing zero.

(define (A277012 n)
   (let loop ((n n) (z 0) (i 2) (j 0))
      (if (zero? n)
          z
          (let ((d (remainder n i)))
            (loop (quotient n i)
                  (+ z (* (A000225 d) (A000079 j)))
                  (+ 1 i)
                  (+ 1 j d)
            )
          )
      )
   )
)



(define (A277012v2 n) (A156552 (A276076 n)))


;; (equal? (map A277008 (iota0 119)) (list-head (sort (map A277012 (iota0 40320)) <) 120)) --> #t

;;;;;;;;;;;;;;;;;;;;
;; Primorial analogues:


;; Number of maximal runs of 1-bits (in binary expansion of n) such that the length of run >= A000040(1 + the total number of zeros to the right of that run):
(define (A277017 n)
   (let loop ((e 0) (n n) (z 0) (r 0))
        (cond ((zero? n) (+ e (if (>= r (A000040 (+ 1 z))) 1 0)))
              ((even? n) (loop (+ e (if (>= r (A000040 (+ 1 z))) 1 0)) (/ n 2) (+ 1 z) 0))
              (else (loop e (/ (- n 1) 2) z (+ 1 r)))
        )
 
   )
)

;; (same-intfuns0? A277017 (COMPOSE A129251 A005940 1+) 8191) --> #t

(define A277018 (ZERO-POS 0 0 A277017))
(define A277019 (NONZERO-POS 1 0 A277017))



;; Left inverse of A277022: (rewrite the run length primorial representation to the real one)
(define (A277021 n)
   (let loop ((s 0) (n n) (r 0) (i 1) (pr 1))
        (cond ((zero? n) (+ s (* r pr)))
              ((even? n) (loop (+ s (* r pr)) (/ n 2) 0 (+ 1 i) (* (A000040 i) pr)))
              (else (loop s (/ (- n 1) 2) (+ 1 r) i pr))
        )
 
   )
)

;; (same-intfuns0? A277021 (COMPOSE A276085 A005940 1+) 65537) --> #t

;; (same-intfuns0? A001477 (COMPOSE A277021 A277022) 30030) --> #t

(definec (A277022rec n) (if (zero? n) n (+ (* (A000225 (A276088 n)) (A000079 (A276084 n))) (* (A000079 (A276088 n)) (A277022rec (A276093 n))))))

(define (A277022v2 n) (A156552 (A276086 n)))

;; (same-intfuns0? A277022 A277022rec 30030) --> #t


;; primorial base representation of n (A049345) rewritten to base-2 number with 0's for 0's and any nonzero digits k --> run of k 1's with one trailing zero.

(define (A277022 n)
   (let loop ((n n) (z 0) (i 1) (j 0))
      (if (zero? n)
          z
          (let* ((p (A000040 i)) (d (remainder n p)))
            (loop (quotient n p)
                  (+ z (* (A000225 d) (A000079 j)))
                  (+ 1 i)
                  (+ 1 j d)
            )
          )
      )
   )
)


;; (define first_A002110_7_ones (sort (map A277022 (iota0 (A002110 7))) <))
;; (equal? (map A277018 (iota0 159)) (list-head first_A002110_7_ones 160)) --> #t

;;;;;;;;;;;;;;;;;;;;;;


;; /XFER: Base-2/Base2.???.ss ???

(define (A270198 n) (A054429 (A055938 (A054429 n))))

(define (A270200 n) (if (zero? n) n (A054429 (A005187 (+ 1 (A054429 (- n 1)))))))
(define A270200from1 (COMPLEMENT 1 A270198))

;; XFER: Base-factorial-derived-base-A001563.ss or such:

(define (A276326 n)
   (let loop ((n n) (s 0))
      (if (zero? n)
          s
          (let ((dig (A276333 n)))
             (if (> dig 9)
                 (error "A276326: ambiguous representation of n, digit > 9 would be needed: " n dig)
                 (loop (A276335 n) (+ s (* dig (expt 10 (- (A258198 n) 1)))))
             )
          )
      )
   )
)

(define (A276326no_error_checking n)
   (let loop ((n n) (s 0))
      (if (zero? n)
          s
          (loop (A276335 n) (+ s (* (A276333 n) (expt 10 (- (A258198 n) 1)))))
      )
   )
)


(define (A276327 n) (let loop ((n n)) (let ((next_n (A276335 n))) (if (zero? next_n) (A276333 n) (loop next_n)))))

(definec (A276327rec n) (if (zero? n) n (if (zero? (A276335 n)) (A276333 n) (A276327rec (A276335 n)))))

(definec (A276328 n) (if (zero? n) n (+ 1 (A276328 (- n (A258199 n))))))

(definec (A276328v2 n) (if (zero? n) n (+ (A276333 n) (A276328v2 (A276335 n)))))

(definec (A276328_by_minimization n) ;; XXX - Should be proved that is really A276328 !
   (if (zero? n)
       n
       (let loop ((i (A258198 n)) (m #f))
          (cond ((zero? i) m)
                ((not m) (loop (- i 1) (+ 1 (A276328_by_minimization (- n (A001563 i))))))
                (else (loop (- i 1) (min m (+ 1 (A276328_by_minimization (- n (A001563 i)))))))
          )
       )
   )
)


(define (A276329 n) (let loop ((i (A258198 n))) (cond ((zero? i) 1) ((zero? (modulo n (A001563 i))) i) (else (loop (- i 1))))))


(define (A276330 n) (if (zero? n) n (A001563 (A276329 n))))

(define (A276331 n) (- n (A276330 n)))

(definec (A276332 n) (if (zero? n) n (+ 1 (A276332 (A276331 n)))))

(define (A276333 n) (if (zero? n) n (floor->exact (/ n (A258199 n)))))

(define (A276334 n) (* (A258199 n) (A276333 n)))

(define (A276335 n) (- n (A276334 n)))

(define (A276336 n) (let loop ((n n) (m 0)) (if (zero? n) m (loop (A276335 n) (max m (A276333 n))))))

(definec (A276336rec n) (if (zero? n) n (max (A276333 n) (A276336rec (A276335 n)))))

(definec (A276337 n) (if (zero? n) n (+ 1 (A276337 (A276335 n)))))


(define (A276338 n) (* (A001563 (+ 1 (A258198 n))) (A276333 n)))

(define (A276339 n) (- (A255411 n) (A276340 n)))

(definec (A276340 n) (if (zero? n) n (+ (A276338 n) (A276340 (A276335 n)))))

(define A276341 (COMPLEMENT 1 A276340))



;; A274647 [Max Barrentine] o=0: A variation on Recamán's sequence

(defineperm1 (A274647 n)
   (if (<= n 1)
       n
       (let ((prev (A274647 (- n 1))))
         (let loop ((k n))
               (cond ((and (> (- prev k) 1)
                           (not-lte? (A276342 (- prev k)) n) ;; Found prev-k such that prev-k is free
                      )
                         (- prev k) ;;  thus that value was unused.
                     )
                     ((not-lte? (A276342 (+ prev k)) n) ;; Or found prev+k such that prev+k is free
                         (+ prev k)
                     )
                     (else (loop (+ k n)))
               )
         )
       )
   )
)

(define (A276342 n) (A274647 (- n)))

;; A274647 should have been computed before this one!
(define (A276438 n) (/ (- (A274647 n) (A274647 (- n 1))) n))

(definec (A276439 n) (if (zero? n) n (+ (A276438 n) (A276439 (- n 1)))))

;; XFER: sieves-lucky.ss

;; A265859 [Max Barrentine] o=1: Unluckiness of n
(define (A265859 n) (* (- 1 (A145649 n)) (A260438 n)))

;; A264940 [Max Barrentine] o=1: Lucky factor of n. 
(define (A264940 n) (if (= 1 (A145649 n)) 0 (if (even? n) 2 (A000959 (A265859 n)))))

(define (A264940v2 n) (if (= 1 (A145649 n)) 0 (if (even? n) 2 (A000959 (A260438 n)))))

(define A047229off1 (COMPLEMENT 1 A007310)) ;; This with o=1, should have a(0) = 0.


;; XFER: sieves-ludic.ss 

;; A272565 [Max Barrentine] o=1: Ludic factor of n. 
(define (A272565 n) (A003309 (+ 1 (A260738 n))))

;; A271419 [Max Barrentine] o=1: If n is a ludic number, a(n)=0; if n is not a ludic number, a(n) is the ludic number that rejects n from the ludic number sieve.
(define (A271419 n) (* (- 1 (A192490 n)) (A272565 n)))

;; A271420 [Max Barrentine] o=1: Nonludicity of n.
(define (A271419 n) (* (- 1 (A192490 n)) (A260738 n)))



(define A276347 (ZERO-POS 1 1 (lambda (n) (- (A272565 n) (A020639 n)))))

(define A276437 (NONZERO-POS 1 1 (lambda (n) (- (A272565 n) (A020639 n)))))

(define A276447 (MATCHING-POS 1 1 (lambda (n) (< (A272565 n) (A020639 n)))))

(define A276448 (MATCHING-POS 1 1 (lambda (n) (> (A272565 n) (A020639 n)))))

;; (define (Asomething n) (let loop ((k (A260738 n))) (if (zero? (modulo n (A003309 (+ 1 k)))) (A003309 (+ 1 k)) (loop (- k 1)))))

(define (A276440 n) (let loop ((k 1) (mt 1)) (let ((t (A003309 k))) (cond ((> t n) mt) ((zero? (modulo n t)) (loop (+ 1 k) t)) (else (loop (+ 1 k) mt))))))

(define A276568 (ZERO-POS 1 1 (lambda (n) (modulo n (A272565 n)))))

(define A276569 (NONZERO-POS 1 1 (lambda (n) (modulo n (A272565 n)))))

(define (A276570 n) (modulo n (A272565 n)))

;; (map (lambda (row) (cons (A003309 (+ 1 row)) (map (lambda (col) (A276570 (A255127bi row col))) (iota 55)))) (iota 15))

(define (A276580 n) (A276580bi (A002260 (- n 1)) (A004736 (- n 1)))) ;; o=2: 
(define (A276580bi row col) (A276570 (A255127bi row col)))

(define (A276576 n) (A276570 (A254100 n))) ;; o=1: Column 2 of A276580: a(n) = A276570(A254100(n)); Postludic numbers reduced by Ludic-numbers.

(define (A276576v2 n) (modulo (A254100 n) (A003309 (+ 1 n))))

(define (A276577 n) (modulo (A255415 n) 11))

;;;

(define (A276610 n) (A276610bi (A002260 n) (A004736 n)))

(define (A276610bi row col) (- (A255127bi (+ 1 row) col) (A255127bi row col)))

(define (A276610v2 n) (A276610biv2 (A002260 n) (A004736 n)))
(define (A276610biv2 row col) (- (A269379 (A255127bi row col)) (A255127bi row col)))

(define (A276609 n) (A276610bi (A004736 n) (A002260 n)))

(define (A276606 n) (- (A254100 (+ 1 n)) (A254100 n)))

(define (A276607 n) (- (A276606 n) (A260723 (+ 1 n))))

(define (A276608 n) (/ (A276607 n) 2))


(define (A276620 n) (A276620bi (A002260 n) (A004736 n)))
(define (A276620bi row col) (- (A276610bi row (+ 1 col)) (A276610bi row col)))

(define (A276619 n) (A276620bi (A004736 n) (A002260 n)))

;; (definec (A_maybe_later_6 n) (if (or (= 1 n) (not (zero? (A276570 n)))) 0 (+ 1 (A_maybe_later_6 (/ n (A272565 n))))))

;; (definec (A_maybe_later_7 n) (if (or (= 1 n) (not (zero? (A276570 n)))) n (A_maybe_later_7 (/ n (A272565 n)))))

;; (define A_maybe_later_8 (ZERO-POS 1 1 (COMPOSE -1+ A_maybe_later_7)))

;; (define A_maybe_later_9 (NONZERO-POS 1 1 (COMPOSE -1+ A_maybe_later_7)))


;; Random edit, XFER: triangulars.ss or something:

;; A276914 [Daniel Poveda Parrilla] o=0: Subsequence of triangular numbers obtained by adding a square and two smaller triangles, a(n) = n^2 + 2*A000217(A052928(n)). 

(define (A276914 n) (+ (A000290 n) (* 2 (A000217 (A052928 n)))))


;; A087808 [Ralf Stephan] o=0:	a(0) = 0; a(2n) = 2a(n), a(2n+1) = a(n) + 1

(definec (A087808 n) (cond ((zero? n) n) ((even? n) (* 2 (A087808 (/ n 2)))) (else (+ 1 (A087808 (/ (- n 1) 2))))))

;; (same-intfuns0? A001477 (COMPOSE A087808 A277020) 63) --> #t

;; (same-intfuns0? A087808 (COMPOSE A048675 A005940 1+) 16387) --> #t

;; (same-intfuns0? A048679 (COMPOSE A087808 A003714) 10946) --> #t

;; A085357 [Paul D. Hanna] o=0: Common residues of binomial(3n,n)/(2n+1) modulo 2: relates ternary trees (A001764) to the infinite Fibonacci word (A003849). (Characteristic function of A003714, Fibbinary numbers).

(define (A085357 n) (if (zero? (A004198bi n (+ n n))) 1 0))

(define (A063524 n) (if (= 1 n) n 0)) ;; o=0: Characteristic function of 1.

(define (A085357v2 n) (A008966 (A005940 (+ 1 n))))

(define (A085357v3 n) (fold-left (lambda (a r) (* a (A063524 r))) 1 (bisect (reverse (binexp->runcount1list n)) (- 1 (modulo n 2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;; XXX: The next one: XFER Base2/base2-recurrences.ss ?

(definec (A002487 n) (cond ((<= n 1) n) ((even? n) (A002487 (/ n 2))) (else (+ (A002487 (/ (- n 1) 2)) (A002487 (/ (+ n 1) 2))))))


(define (A007306 n) (if (zero? n) 1 (A002487 (+ n n -1))))

;; "Schroeppelian" bitwise-decompositions: http://www.inwap.com/pdp10/hbaker/hakmem/boolean.html#item23
;; of A002487:

(define (A283976 n) (if (even? n) (A002487 n) (A003986bi (A002487 (/ (- n 1) 2)) (A002487 (/ (+ n 1) 2)))))

(define (A283977 n) (if (even? n) (A002487 n) (A003987bi (A002487 (/ (- n 1) 2)) (A002487 (/ (+ n 1) 2)))))

(define (A283978 n) (if (even? n) 0 (A004198bi (A002487 (/ (- n 1) 2)) (A002487 (/ (+ n 1) 2)))))

;; And for A007306 from n>=1 onward:
(define (A283986 n) (A003986bi (A002487 (- n 1)) (A002487 n))) ;; o=1
(define (A283987 n) (A003987bi (A002487 (- n 1)) (A002487 n))) ;; o=1
(define (A283988 n) (A004198bi (A002487 (- n 1)) (A002487 n))) ;; o=1

(define (A283986v2 n) (A283976 (+ n n -1)))
(define (A283987v2 n) (A283977 (+ n n -1)))
(define (A283988v2 n) (A283978 (+ n n -1)))

;; (same-intfuns0? A283976 (lambda (n) (+ (A283977 n) (A283978 n))) 65537) --> #t
;; (same-intfuns0? A283976 (lambda (n) (- (A002487 n) (A283978 n))) 65537)

;; (same-intfuns1? A283986 (lambda (n) (+ (A283987 n) (A283988 n))) 65537) --> #t
;; (same-intfuns1? A283986 (lambda (n) (- (A007306 n) (A283988 n))) 65537) --> #t

(define A283973 (ZERO-POS 1 1 A283988))
(define A283973v2 (MATCHING-POS 1 1 (lambda (n) (= (A007306 n) (A283986 n)))))
(define A283973v3 (MATCHING-POS 1 1 (lambda (n) (= (A007306 n) (A283987 n)))))
(define A283973v4 (MATCHING-POS 1 1 (lambda (n) (= (A283986 n) (A283987 n)))))

(define A283974 (NONZERO-POS 1 1 A283988))

;; XXX: The next ones XFER Base2/base2-xor-recurrences.ss ?

(define (A099884 n) (A099884bi (A002262 n) (A025581 n)))
(define (A099884bi row col) (if (zero? row) (A000079 col) (A048724 (A099884bi (- row 1) col))))

(define (A276618 n) (A099884bi (A025581 n) (A002262 n)))

(define (A099884v2 n) (A099884biv2 (A002262 n) (A025581 n)))
(define (A099884biv2 row col) (if (zero? col) (A001317 row) (* 2 (A099884biv2 row (- col 1)))))


(definec (A264977 n) (cond ((<= n 1) n) ((even? n) (* 2 (A264977 (/ n 2)))) (else (A003987bi (A264977 (/ (- n 1) 2)) (A264977 (/ (+ n 1) 2)))))) ;; o=0:


(define (A265397 n) (- n (A264977 n))) ;; o=0: [Each seem to be a multiple of four].

(define A277701 (MATCHING-POS 1 1 (lambda (n) (= 1 (A264977 n)))))

(define A277701v2 (MATCHING-POS 1 1 (lambda (n) (= 2 (A277330 n)))))

(define A277712 (MATCHING-POS 1 1 (lambda (n) (= 2 (A264977 n)))))
(define A277712v2 (MATCHING-POS 1 1 (lambda (n) (= 3 (A277330 n)))))

(define A277713 (MATCHING-POS 1 1 (lambda (n) (= 3 (A264977 n)))))
(define A277713v2 (MATCHING-POS 1 1 (lambda (n) (= 6 (A277330 n)))))

(define (A277714 n) (/ (A277713 n) 3))

(define A277715 (MATCHING-POS 1 1 (lambda (n) (= 5 (A264977 n)))))

(definec (A277711 n) (let loop ((k 0)) (if (= (A264977 k) n) k (loop (+ 1 k))))) ;; Very crude.

(define (A277709 n) (A277710bi (A004736 n) (A002260 n)))

(define (A277710 n) (A277710bi (A002260 n) (A004736 n)))
(define (A277710bi row col) ((rowfun-for-A277710 row) col))
(definec (rowfun-for-A277710 n) (MATCHING-POS 1 0 (lambda (k) (= n (A264977 k)))))

(define (A277710v2 n) (A277710biv2 (A002260 n) (A004736 n)))
(define (A277710biv2 row col) (if (= 1 col) (A277711 row) (A277816 (A277710biv2 row (- col 1)))))

;;
(definec (A277824 n) (if (zero? n) n (+ 1 (A277824 (A277815 n)))))


;; "Ancestor": The least k for which A264977(k) = A264977(n).
(definec (A277826 n) (let ((v (A264977 n))) (let loop ((k 0)) (if (= v (A264977 k)) k (loop (+ 1 k))))))

(definec (A277826_slower n) (if (zero? n) n (let ((v (A264977 n))) (let loop ((k (- n 1)) (m n)) (cond ((zero? k) m) ((= v (A264977 k)) (loop (- k 1) k)) (else (loop (- k 1) m)))))))


(define (A277884 n) (A277814 (A277826 n)))

;; "Predecessor": The largest k < n for which A264977(k) = A264977(n), or 0 if no such k exists.
(definec (A277815 n) (if (zero? n) n (let ((v (A264977 n))) (let loop ((k (- n 1))) (cond ((zero? k) 0) ((= v (A264977 k)) k) (else (loop (- k 1))))))))

;; "Successor": The smallest k > n for which A264977(k) = A264977(n) (or bottom if no such k exists... ;-)
(define (A277816 n) (if (zero? n) n (let ((v (A264977 n))) (let loop ((k (+ 1 n))) (if (= v (A264977 k)) k (loop (+ 1 k)))))))

(define A277817 (ZERO-POS 0 0 A277815)) ;; Terms of A277711 sorted into ascending order, i.e. numbers n not in range of A277816.

;; 
(define (A000007 n) (if (zero? n) 1 0)) ;;  o=0: NJAS The characteristic function of 0: a(n) = 0^n.

(definec (A277814 n) (if (zero? n) n (+ (A277814 (- n 1)) (A000007 (A277815 n)))))

;; (same-intfuns1? A000027 (COMPOSE A277814 A277817) 120) --> #t

(definec (A277695 n)
  (cond ((= 1 n) n)
        ((zero? (A277815 n)) (* 2 (A277695 (+ -1 (A277814 n)))))
        (else (+ 1 (* 2 (A277695 (A277815 n)))))
  )
)

(definec (A277696 n)
  (cond ((= 1 n) n)
        ((even? n) (A277817 (+ 1 (A277696 (/ n 2)))))
        (else (A277816 (A277696 (/ (- n 1) 2))))
  )
)

;; (same-intfuns1? A000027 (COMPOSE A277696 A277695) 120) --> #t
;; (same-intfuns1? A000027 (COMPOSE A277695 A277696) 120) --> #t

(definec (Anot_this n)
  (cond ((= 1 n) n)
        ((zero? (A277815 n)) (* 2 (Anot_this (+ -1 (A277814 n)))))
        (else (+ 1 (* 2 (Anot_this (- n (A277814 n))))))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; XFER: sieve-flavius-josephus.ss
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; From David Wilson, Tue, 22 Nov 2016 on SeqFan-list:
;;
;; Let a(0) be the sequence of nonnegative integers, indexed starting at 0.
;; For k > 1 let a(k) be the sequence gotten by sieving out every nth element of a(k-1).
;; We then have:
;; 
;; a(0) = (0,   1,   2,   3,   4,   5,   6,   7,   8,   9, ...)
;; a(1) = (0,   2,   4,   6,   8,  10,  12,  14,  16,  18, ...)
;; a(2) = (0,   2,   6,   8,  12,  14,  18,  20,  24,  26, ...)
;; a(3) = (0,   2,   6,  12,  14,  18,  24,  26,  30,  36, ...)
;; a(4) = (0,   2,   6,  12,  18,  24,  26,  30,  38,  42, ...)
;; a(5) = (0,   2,   6,  12,  18,  26,  30,  38,  42,  48, ...)
;; a(6) = (0,   2,   6,  12,  18,  26,  38,  42,  48,  60, ...)
;; a(7) = (0,   2,   6,  12,  18,  26,  38,  48,  60,  62, ...)
;; a(8) = (0,   2,   6,  12,  18,  26,  38,  48,  62,  66, ...)
;; a(9) = (0,   2,   6,  12,  18,  26,  38,  48,  62,  78, ...)
;; ...
;; 
;; which is described by the following recursive definition:
;; 
;; a(0, n) = n for n >= 0.
;; a(k, n) = a(k - 1, [n *(k + 1)/k]) for k > 0 and n >= 0.
;; 


(define (A278482bi row col) (if (zero? row) col (A278482bi (- row 1) (floor->exact (* col (/ 1 row) (+ 1 row))))))

(define (A278482 n) (A278482bi (A002262 n) (A025581 n)))
(define (A278483 n) (A278482bi (A025581 n) (A002262 n)))

(define (A278484 n) (A278482bi n n))

(define (A000960 n) (+ 1 (A278484 (- n 1))))

;; A056526 [Bottomley] o=1: First differences of Flavius Josephus's sieve.
(define (A056526 n) (- (A000960 (+ 1 n)) (A000960 n)))

;; A100617 [NJAS] o=1: There are n people in a room. The first half (i.e., floor(n/2)) of them leave, then 1/3 (i.e., floor of 1/3) of those remaining leave, then 1/4, then 1/5, etc.; sequence gives number who remain at the end. 

(define A100617 (LEFTINV-LEASTMONO 1 1 A000960))

(define (A278169 n) (if (= 1 n) n (- (A100617 n) (A100617 (- n 1)))))


(define (A278492bi row col) (+ 1 (A278482bi row col)))

(define (A278492 n) (A278492bi (A002262 n) (A025581 n)))
(define (A278493 n) (A278492bi (A025581 n) (A002262 n)))


(define (A278503 n) (A278505bi (A004736 n) (A002260 n)))

(define (A278504 n) (let ((row (A278539 n)) (col (A278538 n))) (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2))))

(define (A278505 n) (A278505bi (A002260 n) (A004736 n)))

(define (A278506 n) (let ((row (A278538 n)) (col (A278539 n))) (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2))))

(define (A278505bi row col) (if (= 1 col) (A000960 row) (A278507bi row (- col 1))))


;; Flavius Josephus's sieve: Start with the natural numbers; at the k-th sieving step, remove every (k+1)-st term of the sequence remaining after the (k-1)-st sieving step; iterate. 

;; Flavius Josephus array: square array A(row,col), where row n lists the numbers removed at stage n in the sieve which produces A000960. Array is read by antidiagonals A(1,1), A(1,2), A(2,1), A(1,3), A(2,2), A(3,1), ... 

(define (A278507 n) (A278507bi (A002260 n) (A004736 n)))

(define (A278508 n) (A278507bi (A004736 n) (A002260 n)))

(define (A278507bi row col)
  (cond ((= 1 row) (* 2 col))
        (else (A278492bi (- row 1) (+ -1 (* col (+ 1 row)))))
  )
)


(define (A278511 n) (if (<= n 1) n (A278511bi (A002260 (- n 1)) (A004736 (- n 1)))))

(define (A278511bi row col) (cond ((= 1 row) (+ col col)) ((= 1 col) (A000960 row)) (else (A278507bi row (- col 1))))) 

(define (A278512 n) (if (= 1 n) n (let ((row (A278538 n)) (col (A278537 n))) (+ 1 (* (/ 1 2) (- (expt (+ row col) 2) row col col col -2))))))


;; Row index to A278507, is like A278538, apart from A000960 for which it gets zeros.
(definec (A278528 n) ;; Row index to A278507.
  (cond ((not (zero? (A278169 n))) 0) ;; If n is a Flavius number, then return zero.
        ((even? n) 1) ;; Optimization. All even numbers are on the row 1.
        (else ;; We have to search for it, in a two naive loops. (XXX - Could use a binary search in inner one?)
          (let searchrow ((row 2))
             (let searchcol ((col 1))
                (cond ((>= (A278507bi row col) n)
                         (if (= (A278507bi row col) n)
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

(definec (A278529 n) ;; Column index to A278507
  (cond ((not (zero? (A278169 n))) 0) ;; If n is a Flavius number, then return zero.
        ((even? n) (/ n 2)) ;; Optimization. All even numbers are on the row 1.
        (else ;; We have to search for it, in a two naive loops. (XXX - Could use a binary search in inner one?)
          (let searchrow ((row 2))
             (let searchcol ((col 1))
                (cond ((>= (A278507bi row col) n)
                         (if (= (A278507bi row col) n)
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



;; A278539: Column index to  A278511
(definec (A278537 n)
   (cond ((= 1 n) 0) ;; 1 is outside of A278511.
         ((not (zero? (A278169 n))) 1) ;; If n is a Flavius number > 1, then return 1
         ((even? n) (/ n 2)) ;; All even numbers are on the row 1.
         (else (+ 1 (A278529 n)))
   )
)

(define (A278538 n) (if (not (zero? (A278169 n))) (A100617 n) (A278528 n)))


(define (A278539 n) (+ 1 (A278529 n))) ;; [AK] o=1: Column index to A278505.

;; (same-intfuns1? A000027 (lambda (n) (A278505bi (A278538 n) (A278539 n))) 1275) --> #t

;; (same-intfuns1? (COMPOSE A000027 1+) (COMPOSE (lambda (n) (A278511bi (A278538 n) (A278537 n))) 1+) 1275) --> #t

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Three triangles derived from A007318 via Schroeppel's bitwise-AND-OR-XOR -decompositions:

;; (same-intfuns0? A007318 (lambda (n) (+ (A285116 n) (A285118 n))) 10440) --> #t
;; (same-intfuns0? A007318 (lambda (n) (+ (A285117 n) (* 2 (A285118 n)))) 10440) --> #t


(definec (A285116 n) (A285116tr (A003056 n) (A002262 n)))

(define (A285116tr n k)
  (cond ((zero? k) 1)
        ((= k n) 1)
        (else (A003986bi (A007318tr (- n 1) (- k 1)) (A007318tr (- n 1) k)))
  )
)



(definec (A285117 n) (A285117tr (A003056 n) (A002262 n)))

(define (A285117tr n k)
  (cond ((zero? k) 1)
        ((= k n) 1)
        (else (A003987bi (A007318tr (- n 1) (- k 1)) (A007318tr (- n 1) k)))
  )
)



(definec (A285118 n) (A285118tr (A003056 n) (A002262 n)))

(define (A285118tr n k)
  (cond ((zero? k) 0)
        ((= k n) 0)
        (else (A004198bi (A007318tr (- n 1) (- k 1)) (A007318tr (- n 1) k)))
  )
)

(define (A285113 n) (add A285116 (A000217 n) (+ -1 (A000217 (+ 1 n)))))
(define (A285114 n) (add A285117 (A000217 n) (+ -1 (A000217 (+ 1 n)))))
(define (A285115 n) (add A285118 (A000217 n) (+ -1 (A000217 (+ 1 n)))))

;; Mult.encoding of above: (define (Ajoku n) (if (zero? n) 1 (A059895bi (A007188 (- n 1)) (A003961 (A007188 (- n 1))))))


