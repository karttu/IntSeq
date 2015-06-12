
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
;;  Last edited June 11 2015.                                             ;;
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
