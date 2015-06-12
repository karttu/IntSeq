

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;  http://www.iki.fi/~kartturi/matikka/Schemuli/intfun_b.scm             ;;
;;  - Often needed integer functions, the rest from old intfuns1.scm      ;;
;;                                                                        ;;
;;  Coded by Antti Karttunen (my_firstname.my_surname@gmail.com),         ;;
;;   2002-2012                                                            ;;
;;  The rest now transferred to intfun_c.scm                              ;;
;;  Start with scheme --heap 13000                                        ;;
;;  if encountering "Out of memory" errors when compiling.                ;;
;;                                                                        ;;
;;  Last edited 2015-06-06.                                               ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(declare (usual-integrations))

;; The rest:

;; Here are the 10 A-numbers you requested: A163233 --- A163242.
(define (A163233bi x y) (+ (A000695 (A003188 x)) (* 2 (A000695 (A003188 y)))))
(define (A163233 n) (A163233bi (A025581 n) (A002262 n)))
;; Inverse:
(define (A163234 n) (packA001477 (A006068 (A059905 n)) (A006068 (A059906 n))))

;; Transpose of A163233:
(define (A163235 n) (A163233bi (A002262 n) (A025581 n)))
;; Inverse:
(define (A163236 n) (packA001477 (A006068 (A059906 n)) (A006068 (A059905 n))))

(define (A163237 n) (A163241 (A163233 n)))
(define (A163238 n) (A163234 (A163241 n)))

(define (A163239 n) (A163241 (A163235 n)))
(define (A163240 n) (A163236 (A163241 n)))

(define (A163241 n) (+ (A000695 (A003987bi (A059905 n) (A059906 n))) (* 2 (A000695 (A059906 n)))))

(define A163242 (ROWSUMS0 A163233))
;; (0 3 18 30 90 153 204 252 492 735 990 1242 1446 1653 1848 2040 3000 3963 4938 5910 6930 7953 8964 9972 10788 11607 12438 13266 14046 14829 15600 16368 20208 24051 27906 31758 35658 39561 43452 47340 51420 55503 59598)


;; lsbs = Least Significant Trigits

(define (complement-i-lsbs n i)
     (if (zero? i) n
         (+ (- 1 (modulo n 2))
            (* 2 (complement-i-lsbs (floor->exact (/ n 2)) (-1+ i)))
         )
     )
)

(define (complement-i-evenpos-lsbs n i)
     (if (zero? i) n
         (+ (- 1 (modulo n 2))
            (* 2 (complement-i-oddpos-lsbs (floor->exact (/ n 2)) (-1+ i)))
         )
     )
)

(define (complement-i-oddpos-lsbs n i)
  (+ (* 2 (complement-i-evenpos-lsbs (floor->exact (/ n 2)) i))
     (modulo n 2)
  )
)


;; Base 3 -> Base 9 conversion:
(define (A037314 n)
        (if (< n 3) n
            (+ (modulo n 3) (* 9 (A037314 (floor->exact (/ n 3)))))
        )
)

;; lsts = Least Significant Trigits

(define (complement-i-lsts n i)
     (if (zero? i) n
         (+ (- 2 (modulo n 3))
            (* 3 (complement-i-lsts (floor->exact (/ n 3)) (-1+ i)))
         )
     )
)

(define (complement-i-evenpos-lsts n i)
     (if (zero? i) n
         (+ (- 2 (modulo n 3))
            (* 3 (complement-i-oddpos-lsts (floor->exact (/ n 3)) (-1+ i)))
         )
     )
)

(define (complement-i-oddpos-lsts n i)
  (+ (* 3 (complement-i-evenpos-lsts (floor->exact (/ n 3)) i))
     (modulo n 3)
  )
)
    

;; Here are the 20 A-numbers you requested: A163325 --- A163344.

;; Cf. A059905
(define (A163325 n) ;; Take the even-positioned trigits and contract them.
  (if (zero? n)
      n
      (+ (modulo n 3) (* 3 (A163325 (floor->exact (/ n 9)))))
  )
)

;; Take the odd trigits and contract. Cf. A059906
(define (A163326 n) (A163325 (floor->exact (/ n 3))))

;; Gives id (define (id n) (+ (A037314 (A163325 n)) (* 3 (A037314 (A163326 n)))))
(define (A163327 n) (+ (A037314 (A163326 n)) (* 3 (A037314 (A163325 n))))) ;; Cf. A057300

(define (A163328 n) (+ (A037314 (A025581 n)) (* 3 (A037314 (A002262 n))))) ;; Cf. A054238

(define (A163329 n) (packA001477 (A163325 n) (A163326 n))) ;; Cf. A054239

;; Transposes of above:
(define (A163330 n) (+ (A037314 (A002262 n)) (* 3 (A037314 (A025581 n)))))

(define (A163331 n) (packA001477 (A163326 n) (A163325 n)))

(define (A163332 n)
  (let loop ((z 0)
             (n n)
             (i 0)
            )
     (let ((x (modulo n 3))
           (y (modulo (floor->exact (/ n 3)) 3))
          )
        (cond ((zero? n) z)
              ((and (= 1 x) (= 1 y)) ;; Central square, rotate everything 180
                  (loop (+ (* 4 (expt 3 i)) (complement-i-lsts z i))
                        (floor->exact (/ n 9))
                        (+ i 2)
                  )
              )
              ((= 1 x) ;; either 01 or 21, complement odd-positioned trigits
                  (loop (+ (* (+ (* y 3) 1) (expt 3 i))
                           (complement-i-oddpos-lsts z (/ i 2))
                        )
                        (floor->exact (/ n 9))
                        (+ i 2)
                  )
              )
              ((= 1 y) ;; either 10 or 12, complement even-positioned trigits
                  (loop (+ (* (+ 3 (- 2 x)) (expt 3 i)) ;; also x !
                           (complement-i-evenpos-lsts z (/ i 2))
                        )
                        (floor->exact (/ n 9))
                        (+ i 2)
                  )
              )
              (else 
                  (loop (+ (* (+ (* y 3) x) (expt 3 i)) z)
                        (floor->exact (/ n 9))
                        (+ i 2)
                  )
              )
        )
     )
  )
)

;; "Transposed", i.e. A163327-conjugate:
(define (A163333 n) (A163327 (A163332 (A163327 n))))

;; Hilbert II curve in a square array, zero-based
(define (A163334 n) (A163332 (A163328 n)))
;; Inverse:
(define (A163335 n) (A163329 (A163332 n)))

;; (map (lambda (n) (+ (abs (- (A025581 (A163358 (1+ n))) (A025581 (A163358 n))))
;;                    (abs (- (A002262 (A163358 (1+ n))) (A002262 (A163358 n))))))
;; (iota0 63))
;; (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)

;; (map (lambda (n) (+ (abs (- (A025581 (A163335 (1+ n))) (A025581 (A163335 n))))
;;                    (abs (- (A002262 (A163335 (1+ n))) (A002262 (A163335 n))))))
;; (same-intfuns? A000012 (lambda (n) (+ (abs (- (A025581 (A163335 (1+ n))) (A025581 (A163335 n))))
;;                    (abs (- (A002262 (A163335 (1+ n))) (A002262 (A163335 n))))))
;;   (expt 2 20))


;; Hilbert II curve in a square array, zero-based, transposed
(define (A163336 n) (A163332 (A163330 n)))
(define (A163336v2 n) (A163327 (A163333 (A163328 n))))
(define (A163336v3 n) (A163334 (A061579 n)))

;; Inverse:
(define (A163337 n) (A163331 (A163332 n)))
(define (A163337v2 n) (A061579 (A163335 n)))


;; Hilbert II curve, one-based:
(define (A163338 n) (+ 1 (A163334 (-1+ n))))
;; Inverse:
(define (A163339 n) (+ 1 (A163335 (-1+ n))))

;; Hilbert II curve transposed, one-based:
(define (A163340 n) (+ 1 (A163336 (-1+ n))))
;; Inverse A163341: 
(define (A163341 n) (+ 1 (A163337 (-1+ n))))

(define A163342 (ROWSUMS0 A163334))

(define (A163343 n) (A163334 (A046092 n)))
(define (A163343v2 n) (A163336 (A046092 n)))

(define (A163344 n) (/ (A163343 n) 4))

;; Here are the 11 A-numbers you requested: A163355 --- A163365.



(definec (A163355 n)
  (let* ((i (floor->exact (/ (A000523 n) 2)))
         (dd (modulo (floor->exact (/ n (expt 4 i))) 4))
         (r (if (zero? n) n (modulo n (expt 4 i))))
        )
     (cond ((zero? n) n)
;;         ((= 0 dd) ;; 00xy --> 00xy
;;               (format #t "I should not be here! Never executed!\n")
;;               (A163355 r)
;;         )
           ((= (+ 1 (modulo i 2)) dd) ;; 01xy --> 01yx or 10xy --> 01yx
                 (+ (expt 4 i) (A163355 (A057300 r)))
           )
           ((= 3 dd) ;; 11xy --> 10yx
                 (+ (* 2 (expt 4 i)) (A163355 (A057300 r)))
           )
           (else ;; 10xy --> 11c(xy) or 01xy --> 11c(xy)
                 (+ (* 3 (expt 4 i))
                    (A163355 (- (expt 4 i) 1 r))
                 )
           )
     )
  )
)


(definec (A163356 n)
  (if (zero? n) n
      (let* ((i (floor->exact (/ (A000523 n) 2)))
             (d (modulo (floor->exact (/ n (expt 4 i))) 4))
             (r (modulo n (expt 4 i)))
            )
         (+ (* (-1+ (modulo (expt (+ 2 (modulo i 2)) d) 5)) (expt 4 i))
            (cond ;; ((= 0 d) (A163356 r)) ;; Never encountered!
                  ((= 3 d) (- (expt 4 i) 1 (A163356 r)))
                  (else (A057300 (A163356 r)))
            )
         )
      )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A163355v2 n) ;; The older version.
  (let* ((i (A052928 (A000523 n)))
         (dd (modulo (floor->exact (/ n (expt 2 i))) 4))
         (rest (if (zero? n) n (modulo n (expt 2 i))))
        )
     (cond ((zero? n) n)
           ((= 0 dd) ;; 00xy --> 00xy
                 (A163355v2 rest)
           )
           ((= 3 dd) ;; 11xy --> 10yx
                 (+ (expt 2 (1+ i)) (A163355v2 (A057300 rest)))
           )
           ((= (+ 1 (floor->exact (/ (modulo i 4) 2))) dd)
;; 01xy --> 01yx or 10xy --> 01yx
                 (+ (expt 2 i) (A163355v2 (A057300 rest)))
           )
           (else ;; 10xy --> 11c(xy) or 01xy --> 11c(xy)
                 (+ (* 3 (expt 2 i))
                    (A163355v2 (- (expt 2 i) 1 rest))
                 )
           )
     )
  )
)


;; Older version:
(define (A163356v2 n)
  (let loop ((z 0)
             (n n)
             (i 0)
            )
     (let ((dd (modulo n 4)))
        (cond ((zero? n) z)
              ((= 0 dd) ;; 00xy --> 00xy
                 (loop z
                       (floor->exact (/ n 4))
                       (+ i 2)
                 )
              )
              ((= 2 dd) ;; 10xy --> 11yx
                 (loop (+ (* 3 (expt 2 i)) (A057300 z))
                       (floor->exact (/ n 4))
                       (+ i 2)
                 )
              )
              ((= 1 dd) ;; 01xy --> 01yx or 01xy --> 10yx
                 (loop (+ (expt 2 (+ i (floor->exact (/ (modulo i 4) 2))))
                          (A057300 z)
                       )
                       (floor->exact (/ n 4))
                       (+ i 2)
                 )
              )
              (else ;; 11xy --> 10c(xy) or 11xy --> 01c(xy)
                 (loop (+ (expt 2 (+ i (- 1 (floor->exact (/ (modulo i 4) 2)))))
                          (- (expt 2 i) z 1)
                       )
                       (floor->exact (/ n 4))
                       (+ i 2)
                 )
              )
        )
     )
  )
)



(define (range-of-nth-binary-forest n)
  (if (zero? n) (cons n n)
      (cons (A000079 (-1+ n)) (- (A000079 n) 1))
  )
)


(define (indices-of-nth-binary-forest n)
  (if (zero? n) (list n)
      (map (lambda (x) (+ (A000079 (-1+ n)) x)) (iota0 (-1+ (A000079 (-1+ n)))))
  )
)

;; From A000302(n) to A000302(n+1)-1, i.e. [1,3], [4,15], [16,63], [64,255], 
;; i.e. A002001 = 1,3,12,48,192,768,3072,12288,49152,196608,786432,3145728,
;; a(n) = 3*4^(n-1), n>0; a(0)=1.


(define (range-of-nth-quaternary-forest n)
  (if (zero? n) (cons n n)
      (cons (A000302 (-1+ n)) (+ (A000302 (-1+ n)) (-1+ (A002001 n))))
  )
)


(define (indices-of-nth-quaternary-forest n)
  (if (zero? n) (list n)
      (map (lambda (x) (+ (A000302 (-1+ n)) x)) (iota0 (-1+ (A002001 n))))
  )
)


;; ---
;; -----------------------------------------------------------------

(define (partition-by-intpermAfun size intpermutation indfun)
   (let ((src_set (indfun size)))
     (let loop ((cur (car src_set)) (src src_set) (res (list (list))))
        (cond ((null? src) (reverse! (map reverse! res)))
              ((member cur src)
                 (loop (intpermutation cur)
                       (delete! cur src)
                       (cons (cons cur (car res)) (cdr res))
                 )
              )
              (else
;; Completed one whole cycle, let's begin the next one with the first
;; parenthesization we have left:
                 (loop (car src) src (cons (list) res))
              )
        ) ; cond
     ) ; let loop
   )
)



(define (number-of-1-cycles cycles)
  (let ((fes 0))
    (for-each (lambda (c)
                (if (and (pair? c) (not (pair? (cdr c)))) (set! fes (1+ fes)))
              )
              cycles
    )
    fes
  )
)


(define (num-of-ones cycles)
  (let ((fes 0))
    (for-each (lambda (c) (if (= 1 c) (set! fes (1+ fes))))
              cycles
    )
    fes
  )
)

(define (cycsize-fun permu)
 (lambda (start)
  (let loop ((n (permu start)) (i 1))
         (cond ((= n start) i)
               (else (loop (permu n) (+ 1 i)))
         )
  )
 )
)


(define (fc-generic-Afun-very-inefficient Afun indfun)
  (lambda (n) (number-of-1-cycles (partition-by-intpermAfun n Afun indfun)))
)


(define (fc-generic-Afun Afun rangefun)
  (lambda (n)
    (let ((rp (rangefun n)))
      (let loop ((s 0) (i (car rp)))
          (cond ((> i (cdr rp)) s)
                ((= (Afun i) i) (loop (1+ s) (1+ i))) ;; Found a fixed point!
                (else (loop s (1+ i)))
          )
      )
    )
  )
)


(define (elements-on-even-orbits-generic-Afun Afun indfun)
  (lambda (n) (apply + (keep-matching-items (map length (partition-by-intpermAfun n Afun indfun)) even?)))
)

(define (elements-on-odd-orbits-generic-Afun Afun indfun)
  (lambda (n) (apply + (keep-matching-items (map length (partition-by-intpermAfun n Afun indfun)) odd?)))
)


(define (cc-generic-Afun Afun indfun)
  (lambda (n) (length (partition-by-intpermAfun n Afun indfun)))
)

(define (mc-generic-Afun Afun indfun)
 (lambda (n) (apply max (map length (partition-by-intpermAfun n Afun indfun))))
)


(define (lc-generic-Afun Afun indfun)
 (lambda (n) (apply lcm (map length (partition-by-intpermAfun n Afun indfun))))
)





(define (compute-and-print-generic-count-seqs Afun indfun outfile upto-n)
   (call-with-output-file outfile
     (lambda (outport)
       (let loop ((n 0)
                  (ccs (list 1))
                  (fcs (list 1))
                  (mcs (list 1))
                  (lcs (list 1))
                 )
          (format #t "n=~A: ccs=~A fcs=~A mcs=~A lcs=~A~%"
                      n ccs fcs mcs lcs
          )
          (format outport "n=~A: ccs=~A fcs=~A mcs=~A lcs=~A~%"
                      n ccs fcs mcs lcs
          )
          (flush-output outport)
          (cond
            ((< n upto-n)
                (let ((partlengths (map length (partition-by-intpermAfun (1+ n) Afun indfun))))
                   (loop (1+ n)
                         (append ccs (list (length partlengths)))
                         (append fcs (list (num-of-ones partlengths)))
                         (append mcs (list (fold-left max 0 partlengths)))
                         (append lcs (list (fold-left lcm 1 partlengths)))
                   )
                )
            )
          )
       )
     )
   )
)


(define (compute-and-print-cycle-vectors-for-A163355 outfile upto-n)
    (compute-and-print-generic-cycle-vectors A163355
                                     indices-of-nth-quaternary-forest
                                     outfile
                                     upto-n
    )
)


(define (compute-and-print-generic-cycle-vectors Afun indfun outfile upto-n)
   (call-with-output-file outfile
     (lambda (outport)
       (let loop ((n 0)
                  (partlengths (list 1))
                 )
          (let  ((ccs (list (length partlengths)))
                 (fcs (list (num-of-ones partlengths)))
                 (mcs (list (fold-left max 0 partlengths)))
                 (lcs (list (fold-left lcm 1 partlengths)))
                 (cpairs (multiset->countpairs partlengths))
                )
            (format #t "n=~A: ccs=~A fcs=~A mcs=~A lcs=~A, cycles, times*sizes=~A~%"
                        n ccs fcs mcs lcs cpairs
            )
            (format outport "n=~A: ccs=~A fcs=~A mcs=~A lcs=~A, cycles, times*sizes=~A~%"
                        n ccs fcs mcs lcs cpairs
            )
            (flush-output outport)
            (cond
              ((< n upto-n)
                 (loop (1+ n) (map length (partition-by-intpermAfun (1+ n) Afun indfun)))
              )
            )
          )
       )
     )
   )
)




(define (compute-b-file Afun outfile start upto-n)
   (call-with-output-file outfile
     (lambda (outport)
       (let loop ((n start) (z (Afun start)))
          (format #t "n=~A: ~A~%" n z)
          (format outport "~A ~A~%" n z)
          (flush-output outport)
          (cond ((< n upto-n) (loop (1+ n) (Afun (1+ n)))))
       )
     )
   )
)


;; Note that the input b-file should be ultra-clean, e.g. one generated by compute-b-file above.
(define (bisect-b-file-and-apply Afun infile outfile start even-or-odd-bit)
   (call-with-input-file infile
     (lambda (inport)
       (call-with-output-file outfile
         (lambda (outport)
            (let loop ((n_in_file (read inport)) (our_n start))
               (cond ((not (eof-object? n_in_file))
                        (let ((an_in_file (read inport)))
                          (cond ((= (modulo n_in_file 2) even-or-odd-bit)
                                    (format outport "~A ~A~%" our_n (Afun an_in_file))
                                    (flush-output outport)
                                    (loop (read inport) (+ 1 our_n))
                                )
                                (else (loop (read inport) our_n))
                          )
                        )
                     )
               )
            )
           )
         )
       )
   )
)


(define (read-b-file-to-vector infile size)
   (call-with-input-file infile
     (lambda (inport)
         (let ((intvec (make-vector size)))
            (let loop ((n_in_file (read inport)))
               (cond ((eof-object? n_in_file) intvec)
                     (else
                        (let ((an_in_file (read inport)))
                          (begin
                              (vector-set! intvec n_in_file an_in_file)
                              (loop (read inport))
                          )
                        )
                     )
               )
            )
         )
     )
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here are the 30 A-numbers you requested: A163890 --- A163919.

;; orbit-size-of-n-under-A163355
(definec (A163890 n)
   (let loop ((i 1) (nn (A163355 n)))
          (cond ((= nn n) i)
                (else (loop (1+ i) (A163355 nn)))
          )
   )
)


;; Up to n=21 at least.
(define A163891 (DISTINCT-POS 0 0 A163890))

;; Might be a permutation of A003586 (A036561), or at least a subset of it:
;; (Where are for example 27 and 81 and 243?)
;; (define A163892 (DISTINCT-VALS 0 0 A163890))
(define (A163892 n) (A163890 (A163891 n))) ;; Let's use the cache of A163891.


;; (map A163892 (iota0 21))
;; (1 2 6 3 4 8 12 18 9 16 32 24 108 36 48 54 72 64 96 192 216 324)

;; First differences of A163891, zero-based:
(define (A163893 n) (- (A163891 (1+ n)) (A163891 n)))

;; First differing element between A001477 and A163355^n:
;; (0 if they do not differ).
(definec (A163894 n)
   (if (zero? n) 0
       (let loop ((i 1) (nth_power (compose-fun-to-nth-power A163355 n)))
            (cond ((not (= i (nth_power i))) i)
                  (else (loop (1+ i) nth_power))
            )
       )
   )
)

;; At least up to n=8 or 12:
(define A163895 (RECORD-POS 0 0 A163894))
;; (define A163896 (RECORD-VALS 0 0 A163894))
(define (A163896 n) (A163894 (A163895 n))) ;; Let's use the same cache.


(define (A163897 n) (- (A163531 n) (A163547 n)))


;; Table, distance of number n in A163357 from
;; its position in the basic grid A054238:
;;

;; (define (A054238 n) (+ (A000695 (A025581 n)) (* 2 (A000695 (A002262 n)))))
;; (define (A054239 n) (packA001477 (A059905 n) (A059906 n)))


;; Two tables (squares of distances):
(define (A163898 n) (A163900 (A054238 n)))

(define (A163899 n) (A163900 (A163357 n)))

;; Based on this function: (zeros occur at A163901).
;; Squared distance between n's location in A054238 grid and A163357 grid.
(definec (A163900 n)
   (+ (A000290 (abs (- (A059906 n) (A059252 n))))
      (A000290 (abs (- (A059905 n) (A059253 n))))
   )
)

(define A163901 (MATCHING-POS 0 0 (lambda (i) (= i (A163355 i)))))

(define A163902 (MATCHING-POS 0 0
                  (lambda (i) (and (not  (= i (A163355 i)))
                                   (= i (A163355 (A163355 i)))
                              )
                  )
                )
)

(define A163903 (MATCHING-POS 0 0
                  (lambda (i) (and (not  (= i (A163355 i)))
                                   (= i (A163355 (A163355 (A163355 i))))
                              )
                  )
                )
)


;; Table, cycle size of each n:
;; (1's occur at the same positions as the zeros of A163898 and A163899, i.e. A165403):
(define (A163904 n) (A163890 (A054238 n)))
(define (A163904v2 n) (A163890 (A163357 n)))

;; (define A165403 (MATCHING-POS 0 0 (lambda (i) (= 0 (A163898 i)))))
;; (define A165403v2 (MATCHING-POS 0 0 (lambda (i) (= 0 (A163899 i)))))
(define A165403 (ZERO-POS 0 0 A163898))
(define A165403v2 (ZERO-POS 0 0 A163899))
(define A165403v3 (MATCHING-POS 0 0 (lambda (i) (= 1 (A163904 i)))))


(define (A165404 n) (A025581 (A165403 n)))

(define (A165406 n) (A007088 (A165404 n)))
(define (A165406v2 n) (A007090 (A163901 n)))

(define A163910 (cc-generic-Afun A163355 indices-of-nth-quaternary-forest))
(define A163911 (mc-generic-Afun A163355 indices-of-nth-quaternary-forest))
(define A163912 (lc-generic-Afun A163355 indices-of-nth-quaternary-forest))

;; (map A163913 (iota0 13))
;; (0 0 6 3 30 27 162 171 885 987 4839 5502 26436 30216)

(definec (A163913 n)
  (let ((rp (range-of-nth-quaternary-forest n)))
    (let loop ((s 0) (i (car rp)))
          (cond ((> i (cdr rp)) s)
                ((and (not (= (A163355 i) i)) (= (A163915 i) i))
                    (loop (1+ s) (1+ i))
                )
                (else (loop s (1+ i)))
          )
    )
  )
)

;; (0 0 2 1 10 9 54 57 295 329 1613 1834 8812 10072)
(define (A163914 n) (/ (A163913 n) 3))

;; Also two bisections, up to n=6:
(define (A163909 n) (A163914 (* 2 n)))
(define (A163919 n) (A163914 (1+ (* 2 n))))


(define (A163905 n) (A163355 (A163355 n)))
(define (A163906 n) (A163356 (A163356 n)))

(define (A163907 n) (A163905 (A054238 n)))
(define (A163908 n) (A054239 (A163906 n)))

(define (A163915 n) (A163355 (A163355 (A163355 n))))
(define (A163916 n) (A163356 (A163356 (A163356 n))))

(define (A163917 n) (A163915 (A054238 n)))
(define (A163918 n) (A054239 (A163916 n)))


;; Midpoint height of "Jacobi-bridge", computed for each odd integer
;; of the form 4n+3.
(definec (A165601 n)
  (let ((w (A004767 n))) ;; w = 4n+3
    (add (lambda (i) (jacobi-symbol i w)) 0 (/ (-1+ w) 2))
  )
)

(define A165602 (ZERO-POS 0 0 A165601)) ;; Positions of zeros in A165601.
;; Numbers of the form 4n+3 for which Sum_{i=0..(2n+1)} J(i/4n+3) = 0.
(define (A165603 n) (A004767 (A165602 n)))

(define (A165604 n) (A165601 (* 3 n))) ;; 12n+3
(define (A165605 n) (A165601 (+ 1 (* 3 n)))) ;; 12n+7 (A017605)
(define (A165606 n) (A165601 (+ 2 (* 3 n)))) ;; 12n+11

;; The height at the 1/3 point at "Jacobi-bridge",
;; computed for each odd integer of the form 12n+7
(definec (A165460 n)
  (let ((w (A017605 n))) ;; w = 12n+7
    (add (lambda (i) (jacobi-symbol i w)) 0 (/ (-1+ w) 3))
  )
)

(define A165461 (ZERO-POS 0 0 A165460)) ;; Positions of zeros in A165460.
(define (A165462 n) (/ (- (A165463 n) 3) 4)) ;; Subset of A165602 ?
;; Numbers of the form 12n+7 for which Sum_{i=0..(4n+2)} J(i/12n+7) = 0.
(define (A165463 n) (A017605 (A165461 n))) ;; Subset of A165603 ?


;; Here are the 10 A-numbers you requested: A165460 --- A165469.

;; Squared distance between n's location in A163334 grid and A163357 grid.
;; (equivalently: between n's location in A163336 grid and A163359 grid.)
(definec (A165464 n)
   (+ (A000290 (abs (- (A163529 n) (A059252 n))))
      (A000290 (abs (- (A163528 n) (A059253 n))))
   )
)

(define A165465 (ZERO-POS 0 0 A165464)) ;; Positions of zeros in A165464.

(definec (A165466 n)
   (+ (A000290 (abs (- (A163529 n) (A059253 n))))
      (A000290 (abs (- (A163528 n) (A059252 n))))
   )
)

(define A165467 (ZERO-POS 0 0 A165466)) ;; Positions of zeros in A165466.

(define A095274 (MATCHING-POS 1 0
  (lambda (n)
    (let ((w (A004767 n))  ;; w = 4n+3
          (hp (A005408 n)) ;; hp = 2n+1 = ((w-1)/2)
         )
      (let loop ((i 1) (s 1)) ;; s = sum of the first i Jacobi-symbols in [1,i]
            (cond ((< s 0) #f)
                  ((>= i hp) #t)
                  (else (loop (1+ i) (+ s (jacobi-symbol (1+ i) w))))
            )
      )
    )
  )
                )
)

(define (A095100 n) (A004767 (A095274 n)))


(define A095272 (MATCHING-POS 1 0
  (lambda (n)
    (let ((w (A004767 n))  ;; w = 4n+3
          (hp (A005408 n)) ;; hp = 2n+1 = ((w-1)/2)
         )
     (and (not (zero? (A010051 w)))
      (let loop ((i 1) (s 1)) ;; s = sum of the first i Jacobi-symbols in [1,i]
            (cond ((< s 0) #f)
                  ((>= i hp) #t)
                  (else (loop (1+ i) (+ s (jacobi-symbol (1+ i) w))))
            )
      )
     )
    )
  )
                )
)

(define (A095102 n) (A004767 (A095272 n)))


;; Subset of A095274.
(define A165468 (MATCHING-POS 1 0
  (lambda (n)
    (let ((w (A004767 n))  ;; w = 4n+3
          (hp (A005408 n)) ;; hp = 2n+1 = ((w-1)/2)
         )
      (let loop ((i 1) (s 1)) ;; s = sum of the first i Jacobi-symbols in [1,i]
            (cond ((<= s 0) #f)
                  ((>= i hp) #t)
                  (else (loop (1+ i) (+ s (jacobi-symbol (1+ i) w))))
            )
      )
    )
  )
                )
)

;; Subset of A095100.
(define (A165469 n) (A004767 (A165468 n)))


;; Subset of A095272.
(define index_fun_for_A165580
 (MATCHING-POS 1 0
  (lambda (n)
    (let ((w (A004767 n))  ;; w = 4n+3
          (hp (A005408 n)) ;; hp = 2n+1 = ((w-1)/2)
         )
     (and (not (zero? (A010051 w)))
      (let loop ((i 1) (s 1)) ;; s = sum of the first i Jacobi-symbols in [1,i]
            (cond ((<= s 0) #f)
                  ((>= i hp) #t)
                  (else (loop (1+ i) (+ s (jacobi-symbol (1+ i) w))))
            )
      )
     )
    )
  )
 )
)

;; Subset of A165468 and A095102.
(define (A165580 n) (A004767 (index_fun_for_A165580 n)))


;; Difference A095274 \ A165468:
;; (Those n, for which Jacobi-bridge of 4n+3 visits sea level, but doesn't dive)

(define A165607 (MATCHING-POS 1 0
  (lambda (n)
    (let ((w (A004767 n))  ;; w = 4n+3
          (hp (A005408 n)) ;; hp = 2n+1 = ((w-1)/2)
         )  ;; s = sum of the first i Jacobi-symbols in [1,i]
      (let loop ((i 1) (s 1) (zv 0)) ;; zv = # of sea level visits.
            (cond ((< s 0) #f) ;; Goes negative, fail.
                  ((>= i hp) (not (zero? zv)))
                  ((zero? s)
                       (loop (1+ i) (+ s (jacobi-symbol (1+ i) w)) (1+ zv))
                  )
                  (else (loop (1+ i) (+ s (jacobi-symbol (1+ i) w)) zv))
            )
      )
    )
  )
                )
)

;; A095100 \ A165469:
(define (A165608 n) (A004767 (A165607 n)))



(define index_fun_for_A165977
 (MATCHING-POS 1 0
  (lambda (n)
    (let ((w (A004767 n))  ;; w = 4n+3
          (hp (A005408 n)) ;; hp = 2n+1 = ((w-1)/2)
         )  ;; s = sum of the first i Jacobi-symbols in [1,i]
     (and (not (zero? (A010051 w)))
      (let loop ((i 1) (s 1) (zv 0)) ;; zv = # of sea level visits.
            (cond ((< s 0) #f) ;; Goes negative, fail.
                  ((>= i hp) (not (zero? zv)))
                  ((zero? s)
                       (loop (1+ i) (+ s (jacobi-symbol (1+ i) w)) (1+ zv))
                  )
                  (else (loop (1+ i) (+ s (jacobi-symbol (1+ i) w)) zv))
            )
      )
     )
    )
  )
 )
)



;; A095102 \ A165580:
(define (A165977 n) (A004767 (index_fun_for_A165977 n)))

;; Here are the 20 A-numbers you requested: A166040 --- A166059.

;; Fixed points: A165465
(define (A166041 n) (A163357 (A163335 n)))
(define (A166041v2 n) (A163359 (A163337 n)))

(define (A166042 n) (A163334 (A163358 n)))
(define (A166042v2 n) (A163336 (A163360 n)))

;; Fixed points: A165466
(define (A166043 n) (A163357 (A163337 n)))
(define (A166043v2 n) (A163359 (A163335 n)))

(define (A166044 n) (A163336 (A163358 n)))
(define (A166044v2 n) (A163334 (A163360 n)))

(define A166045 (RECORD-POS 0 0 A165601))
(define (A166046 n) (A004767 (A166045 n)))
(define A166047 (RECORD-VALS 0 0 A165601))

;; Cf. A112060, A112070

(definec (A166040 n)
    (let ((w (A005408 n)))
;; s = sum of the first i Jacobi-symbols in [1,i]
      (let loop ((i 1) (s 1) (zv 0)) ;; zv = # of sea level visits.
            (cond ((= i w) zv)
                  ((zero? s)
                       (loop (1+ i) (+ s (jacobi-symbol (1+ i) w)) (1+ zv))
                  )
                  (else (loop (1+ i) (+ s (jacobi-symbol (1+ i) w)) zv))
            )
      )
    )
)


(define A166048 (MATCHING-POS 1 0
  (lambda (n)
    (let ((w (A016813 n))  ;; w = 4n+1
          (hp (A005843 n)) ;; hp = 2n = ((w-1)/2)
         )  ;; s = sum of the first i Jacobi-symbols in [1,i]
      (let loop ((i 1) (s 1))
            (cond ((< s 0) #f) ;; Goes negative, fail.
                  ((>= i hp) #t)
                  (else (loop (1+ i) (+ s (jacobi-symbol (1+ i) w))))
            )
      )
    )
  )
                )
)

(define (A166049 n) (A016813 (A166048 n)))

;; Same without squares:
(define index_for_A166051 (MATCHING-POS 1 0
  (lambda (n)
    (let ((w (A016813c n))  ;; w = 4n+1
          (hp (A005843 n)) ;; hp = 2n = ((w-1)/2)
         )  ;; s = sum of the first i Jacobi-symbols in [1,i]
      (let loop ((i 1) (s 1))
            (cond ((< s 0) #f) ;; Goes negative, fail.
                  ((>= i hp) (zero? s))
                  (else (loop (1+ i) (+ s (jacobi-symbol (1+ i) w))))
            )
      )
    )
  )
                )
)

;; A166049 without squares (is this finite? Cf. A080114).
;; At least 7 terms.
(define (A166051 n) (A016813 (index_for_A166051 n)))

;; Here are the 20 A-numbers you requested: A166085 --- A166104.



;; Two bisections of A166040:
(define (A166085 n) (A166040 (A005843 n))) ;; For searching among 4n+1
(define (A166086 n) (A166040 (A005408 n))) ;; and for amongst 4n+3

(define A046092v2 (MATCHING-POS 1 0 (lambda (i) (= 0 (A166040 i)))))
(define (A016754v2 n) (A005408 (A046092v2 n))) ;; Odd squares.

(define A165468v2 (MATCHING-POS 1 0 (lambda (i) (= 1 (A166086 i)))))
(define (A165469v2 n) (A004767 (A165468v2 n))) ;; Yes.

(define A166052 (MATCHING-POS 1 0 (lambda (i) (= 3 (A166086 i)))))
(define (A166053 n) (A004767 (A166052 n)))

(define A166054 (MATCHING-POS 1 0 (lambda (i) (= 5 (A166086 i)))))
(define (A166055 n) (A004767 (A166054 n)))

(define A166056 (MATCHING-POS 1 0 (lambda (i) (= 7 (A166086 i)))))
(define (A166057 n) (A004767 (A166056 n)))

(define A166058 (MATCHING-POS 1 0 (lambda (i) (= 9 (A166086 i)))))
(define (A166059 n) (A004767 (A166058 n)))


;; At least 8 terms:
(define index_for_A166088 (MATCHING-POS 1 0 (lambda (i) (= 8 (A166040 i)))))
(define (A166088 n) (A005408 (index_for_A166088 n)))

;; First occurrence of n in A166040: Zero-based.
(define A166087 (LEAST-I-WITH-FUN-I-EQ-N 0 0 A166040))

;; Corresponding odd numbers: Zero-based.
(define (A166089 n) (A005408 (A166087 n)))



(define (A166091bi n k)
  (let ((m (A005408 k)))
   (let loop ((i 0) (n n))
        (cond ((= m (A166086 i))
                  (if (zero? n)
                      i
                      (loop (1+ i) (-1+ n))
                  )
              )
              (else (loop (1+ i) n))
        )
   )
  )
)

(definec (A166091 n) (A166091bi (A025581 n) (A002262 n)))

(define (A166092 n) (A004767 (A166091 n))) ;; Corresponding 4k+3 integers.
;; 
;; First 5 rows: A165469, A166053, A166055, A166057, A166059

(define A166093 (compose-funs (LEAST-I-WITH-FUN-I-EQ-N 0 0 A166085) A005843))
(define A166094 (compose-funs (LEAST-I-WITH-FUN-I-EQ-N 0 0 A166086) A005408)) ;; First column of A166091.

(define (A166090 n) (A016813 (A166093 n)))

;; Two bisections of A166089:
(define (A166095 n) (A166089 (A005843 n)))
(define (A166096 n) (A166089 (A005408 n))) ;; First column of A166092.

;; NOT!: A166095 = A016813  A166093 (instead, we have A166090.)
;; A166096 = A004767  A166094

(define A166097 (DISTINCT-POS 0 0 A166040))
(defineperm1 (A166098 n) (A166040 (A166097 n)))
(define A166098v2 (DISTINCT-VALS 0 0 A166040)) ;; Permutation of A001477?

(define (A166099 n) (A166098 (- n)))


;; Offset 0.


;; The height at the 1/6 point at "Jacobi-bridge",
;; computed for each odd integer of the form 12n+7
;; Compare to A165460.
(definec (A166050 n)
  (let ((w (A017605 n))) ;; w = 12n+7
    (add (lambda (i) (jacobi-symbol i w)) 0 (/ (-1+ w) 6))
  )
)

(definec (A166100 n)
    (let ((w (A005408 n)))  ;; w = 2n+1
      (let loop ((i 1) (s 1))
            (cond ((= i w) s)
               (else (loop (1+ i)
                           (+ s (if (= +1 (jacobi-symbol (1+ i) w)) (1+ i) 0))
                     )
               )
            )
      )
    )
)


;; Number of even numbers less than the n-th prime.
;;  Same as A005097 ((odd primes - 1)/2) with a leading zero.
;; Offset 1, starts as: 0, 1, 2, 3, 5, 6, 8, 9, 11, 14, 15, 18, 20, 21, 23, ...
(define (A102781 n) (if (= 1 n) 0 (/ (-1+ (A000040 n)) 2)))

;; Offset 1.
(define (A076409 n) (A166100 (A102781 n)))

;; Offset 3.
(define (A076410 n) (/ (A076409 n) (A000040 n)))

;; Offset 1. for A166101 - A166104.
(define A166101
   (MATCHING-POS 1 0 (lambda (n) (not (integer? (/ (A166100 n) (A005408 n))))))
)

;; Conjecture: a(n) = 3*A166103(n). Checked for terms a(1)-a(92).
(define (A166102 n) (A005408 (A166101 n)))

(define (A166103 n) (A000290 (A166104 n)))
(define (A166103v2 n) (/ (A166102 n) 3))

(define A166104
   (MATCHING-POS 1 1
     (lambda (n)
         (for-all? (factor n) 
                   (lambda (p) (or (= 1 p) (= 3 p) (= 5 (modulo p 6))))
         )
     )
   )
)

(define (A166104v2 n) (A000196 (A166103 n)))
;; (define (A166104v2 n) (sqrt (A166103 n)))

(definec (A166272 n) (numerator (/ (A166100 (A166101 n)) (A166102 n))))
(definec (A166272v2 n) (* 3 (/ (A166100 (A166101 n)) (A166102 n))))
(definec (A166272v3 n) (* 3 (/ (A166100 (A166101 n)) (A005408 (A166101 n)))))

(define A045410 ;; Primes congruent to {3, 5} mod 6. 3,5,11,17,23,29,41,47,...
 (compose-funs
   A000040
   (MATCHING-POS 1 1 (lambda (n) (or (= 2 n) (= 5 (modulo (A000040 n) 6)))))
 )
)


;; A125615(n)=a(A102781(n)).
(definec (A166405 n)
    (let ((w (A005408 n)))  ;; w = 2n+1
      (let loop ((i 1) (s 0))
            (cond ((= i w) s)
               (else (loop (1+ i)
                           (+ s (if (= -1 (jacobi-symbol (1+ i) w)) (1+ i) 0))
                     )
               )
            )
      )
    )
)

(define (A166406 n) (- (A166405 n) (A166100 n)))

(define (A166407 n) (* 3 (/ (A166406 n) (A005408 n))))

;; A165951(n)=A166408(A102781(n)) for n>=2.
(define (A166408 n) (floor->exact (/ (A166407 n) 3)))

;; Differs from A077425 for the first time at n=21, where a(n)=99, not 101.
;; I.e. seems to be union of A077425 & A165603:
(define A166409 (compose-funs A005408 (ZERO-POS 1 0 A166406)))

(define A165603v2 ;; Hmm... Prove it!
   (compose-funs A005408
                 (MATCHING-POS 1 0
                               (lambda (n) (and (zero? (A166406 n))
                                                (odd? n)
                                           )
                               )
                 )
   )
)



;; Here are the 10 A-numbers you requested: A166268 --- A166277.

(define (A166268 n) (A166050 (A005843 n)))
(define (A166268v2 n) (A165605 (A005843 n))) ;; ???

(define (A166269 n) (A166050 (A005408 n))) ;; -1 -1 -1 -2 -2 -3 -1

(define (A166270 n) (A165460 (A005843 n))) ;; = 2*A166268(n) ???
(define (A166271 n) (A165460 (A005408 n))) ;; = -2 * A166269(n). = (* 2 (A001489 (A166269 n))) ???

(define (A166273 n) (A165605 (A005408 n))) ;; = -3 * A166269(n). = (* 3 (A001489 (A166269 n))) ???

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here are the 20 A-numbers you requested: A166404 --- A166423.
;; Here are the 20 A-numbers you requested: A166424 --- A166443.


(define (A166436 n) (A102283 (A163536 n)))
(define (A166442 n) (A102283 (A163542 n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Here are the 18 A-numbers you requested: A165471 --- A165488.


;; A000668: [3,7,31,127,8191,131071,524287,2147483647,...]
;; A019434: [3,5,17,257,65537]
;; A005478: [2,3,5,13,89,233,1597,28657,514229,...]

;; 5: A080891
;; 7: 
;; 13: A011583
;; 17: A011584
;; 31: A011588
;; 89: A011601
;; 127: A011608
;;
;; 199: A011623
;; 211: A011624
;; 223: A011625
;; 227: A011626
;; 229: A011627
;; 233: A011628
;; 239: A011629
;; 241: A011630
;; 251: A011631
;; 257: Not present.
;; 263:
;; 269:
;; 271:
;; 277:
;; 281:
;; 283:
;; 293:
;; 307:

(define (A080891 n) (legendre-symbol n 5))
(define (A011583 n) (legendre-symbol n 13))
(define (A011584 n) (legendre-symbol n 17))
(define (A011588 n) (legendre-symbol n 31))
(define (A011601 n) (legendre-symbol n 89))
(define (A011608 n) (legendre-symbol n 127))

(define (A011623 n) (legendre-symbol n 199))
(define (A011624 n) (legendre-symbol n 211))
(define (A011625 n) (legendre-symbol n 223))
(define (A011626 n) (legendre-symbol n 227))
(define (A011627 n) (legendre-symbol n 229))
(define (A011628 n) (legendre-symbol n 233))
(define (A011629 n) (legendre-symbol n 239))
(define (A011630 n) (legendre-symbol n 241))
(define (A011631 n) (legendre-symbol n 251))


(define (A165471 n) (legendre-symbol n 65537)) ;; A019434(4)=65537.
(define A165472 (PARTIALSUMS 0 0 A165471))
(define A165473 (ZERO-POS 0 0 A165472))
;; (A165473 610) = 65536
;; (A165473 611) = 65537
;; (A165473 612) = 65543
(define A165474 (RECORD-ABSVALS-BETWEEN-ZEROS-POS 0 A165472 A165473))
(define (A165475 n) (A165472 (A165474 n)))

(define (1+halved n) (floor->exact (/ (1+ n) 2)))

;; Like A165471, but instead of values -1 and +1, we have 0 and 1,
;; with the zeros in original A165471(65537*k) discarded,
;; so that this one (A179416) is a period 65536 sequence
;; instead of period 65537 as A165471 is:
(define (A179416 n) (1+halved (A165471 (1+ (modulo n 65536)))))


(definec (A179417 n)
  (let ((ul (A005408 n)))
    (let loop ((i (A000290 n))
               (j 0)
               (s 0)
              )
        (cond ((= j ul) s)
              ((= 0 (1+halved (A165471 (1+ i)))) (loop (1+ i) (1+ j) s))
              (else (loop (1+ i) (1+ j) (+ s (expt 2 j))))
        )
    )
  )
)

(definec (A179417wrong_after_255 n)
  (let ((ul (A005408 n)))
    (let loop ((i (A000290 n))
               (j 0)
               (s 0)
              )
        (cond ((= j ul) s)
              ((= 0 (A179416 i)) (loop (1+ i) (1+ j) s))
              (else (loop (1+ i) (1+ j) (+ s (expt 2 j))))
        )
    )
  )
)

(define (A179418 n) (A000120 (A179417 n)))
;; Note: (apply + (map A179418 (iota0 255))) = 32768


(define (A165476 n) (legendre-symbol n 131071)) ;; A000668(6)=131071.
(define A165477 (PARTIALSUMS 0 0 A165476))
(define (A165478 n) (+ (* (floor->exact (/ n 2)) 131071) (* (modulo n 2) 131070)))
;; 0,131070,131071,262141,262142,

(define A165478v2 (ZERO-POS 0 0 A165477))
;; (map A165478 (iota0 4)) = (0 131070 131071 262141 262142)

(define (A165479 n) (+ (* (floor->exact (/ n 2)) 131071) (* (+ 1 (* 2 (modulo n 2))) 43690)))
(define A165479v2 (RECORD-ABSVALS-BETWEEN-ZEROS-POS 0 A165477 A165478))
;; Not this: (define (A165480 n) (A165477 (A165479 n))) ;; 570,0,570,0,570,0,570,0,...

(define A165480 (ZERO-POS 0 0 A163897))

;; (A000045 23) = 28657
(define (A165481 n) (legendre-symbol n 28657))
(define A165482 (PARTIALSUMS 0 0 A165481))
(define A165483 (ZERO-POS 0 0 A165482))
;; (A165483 232) = 28656
;; (A165483 233) = 28657
;; (A165483 234) = 28679
(define A165484 (RECORD-ABSVALS-BETWEEN-ZEROS-POS 0 A165482 A165483))
(define (A165485 n) (A165482 (A165484 n)))


(define (A165486 n) (* (sgn (A165472 n)) (A000290 (A165472 n))))
(define (A165487 n) (* (sgn (A165477 n)) (A000290 (A165477 n))))
(define (A165488 n) (* (sgn (A165482 n)) (A000290 (A165482 n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here are the 30 A-numbers you requested: A165569 --- A165598.
;; Here are the 10 A-numbers you requested: A165599 --- A165608.


;; for n=1, a(n)=1, and for n>1, return first such i>a(n-1) that
;; abs(*Phi* - A108539(i)/A000040(i))
;;   < abs(*Phi* - A108539(a(n-1))/A000040(a(n-1)))
;;

;; The indexing sequence for "Successively Better Golden Semiprimes."
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

(define (A165570 n) (* (A165571 n) (A165572 n))) ;; Cf. A108540
(define (A165571 n) (A000040 (A165569 n))) ;; Cf. A108541
(define (A165572 n) (A108539 (A165569 n))) ;; Cf. A108542


(define (A165573 n) (legendre-symbol n 257))
(define (A165574 n) (legendre-symbol n 263))

(define A165575 (PARTIALSUMS 0 0 A165573))
(define A165576 (PARTIALSUMS 0 0 A165574))

(define A165577 (PARTIALSUMS 0 0 A011626))
(define A165578 (PARTIALSUMS 0 0 A011627))
(define A165579 (PARTIALSUMS 0 0 A011628))


(define (A165581 n) (legendre-symbol n 524287)) ;; = A000668(7)=524287.
(define A165582 (PARTIALSUMS 0 0 A165581))
(define A165583 (ZERO-POS 0 0 A165582))
(define A165584 (RECORD-ABSVALS-BETWEEN-ZEROS-POS 0 A165582 A165583))
(define (A165585 n) (A165582 (A165584 n)))

(define (A165586 n) (legendre-symbol n 514229)) ;; = A005478(9) = A000045(29)
(define A165587 (PARTIALSUMS 0 0 A165586))
(define A165588 (ZERO-POS 0 0 A165587))
(define A165589 (RECORD-ABSVALS-BETWEEN-ZEROS-POS 0 A165587 A165588))
(define (A165590 n) (A165587 (A165589 n)))

(define (A165591 n) (jacobi-symbol n 59701)) ;; = A005385(11)*A005385(12) = 227*263.
(define (A165591v2 n) (* (A011626 n) (A165574 n)))
(define A165592 (PARTIALSUMS 0 0 A165591))
(define A165593 (ZERO-POS 0 0 A165592))
(define A165594 (RECORD-ABSVALS-BETWEEN-ZEROS-POS 0 A165592 A165593))
(define (A165595 n) (A165592 (A165594 n)))

(define (A165596 n) (jacobi-symbol n 59881)) ;; = A005478(6)*A019434(3) = 233*257 = A117879(11).
(define (A165596v2 n) (* (A011628 n) (A165573 n)))
(define A165597 (PARTIALSUMS 0 0 A165596))
(define A165598 (ZERO-POS 0 0 A165597))
(define A165599 (RECORD-ABSVALS-BETWEEN-ZEROS-POS 0 A165597 A165598))
(define (A165600 n) (A165597 (A165599 n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Hilbert curve in a square array, zero-based
(define (A163357 n) (A163355 (A054238 n)))
;; Inverse:
(define (A163358 n) (A054239 (A163356 n)))

;; Hilbert curve in a square array, transposed, zero-based
(define (A163359 n) (A163357 (A061579 n)))
;; Inverse:
(define (A163360 n) (A061579 (A163358 n)))

;; Hilbert curve in a square array, one-based
(define (A163361 n) (1+ (A163357 (-1+ n))))
;; Inverse:
(define (A163362 n) (1+ (A163358 (-1+ n))))

;; Hilbert curve in a square array, transposed, one-based
(define (A163363 n) (1+ (A163359 (-1+ n))))
;; Inverse:
(define (A163364 n) (1+ (A163360 (-1+ n))))

(define A163365 (ROWSUMS0 A163357))
(define A163365v2 (ROWSUMS0 A163359))

;; Here are the 10 A-numbers you requested: A163477 --- A163486.
(define (A163477 n) (/ (A163365 n) 4))

(define (A163478 n) (/ (A163242 n) 3))

(define (A163479 n) (/ (A163342 n) 6))

(define (A163480 n) (A163334 (A000217 n)))
(define (A163481 n) (A163334 (-1+ (A000217 (1+ n)))))

(define (A163482 n) (A163357 (A000217 n)))
(define (A163483 n) (A163357 (-1+ (A000217 (1+ n)))))


;; Cf. A163355
(defineperm1 (A163485 n)
  (let* ((i (floor->exact (/ (A000523 n) 2)))
         (dd (modulo (floor->exact (/ n (expt 4 i))) 4))
         (r (if (zero? n) n (modulo n (expt 4 i))))
        )
     (cond ((zero? n) n)
           ((= 0 dd) ;; 00xy --> 00xy
                 (A163485 r)
           )
           ((= 1 dd) ;; 01xy --> 11c(y)x
                 (+ (* 3 (expt 4 i))
                    (- (expt 4 i) 1 (A163485 (complement-i-oddpos-lsbs (A057300 r) i)))
                 )
           )
           ((= 2 dd) ;; 10xy --> 01c(y)x
                 (+ (expt 4 i)
                    (- (expt 4 i) 1 (A163485 (complement-i-oddpos-lsbs (A057300 r) i)))
                 )
           )
           (else ;; 11xy --> 10xy
                 (+ (* 2 (expt 4 i)) (A163485 r))
           )
     )
  )
)

;; Oh I'm so lazy now! Can't bother now to find the formula for the inverse.
;; We can do this with defineperm1 because A163485(0) = 0.
(define (A163486 n) (A163485 (- n)))

(define (A147995 n) (A163485 (A054238 (A061579 n))))


(define A163484 (ROWSUMS0 A147995))

;; Inverse of A147995
(define (A163544 n) (A061579 (A163546 n)))

;; Transpose of A147995:
(define (A163545 n) (A163485 (A054238 n)))
(define (A163546 n) (A054239 (A163486 n)))


;; -----------------------------------------------------------------


;; Here are the 20 A-numbers you requested: A163528 --- A163547.

(define (A163528 n) (A025581 (A163335 n)))
(define (A163528v2 n) (A002262 (A163337 n)))
(define (A163528v3 n) (A163325 (A163332 n)))

(define (A163529 n) (A002262 (A163335 n)))
(define (A163529v2 n) (A025581 (A163337 n)))
(define (A163529v3 n) (A163326 (A163332 n)))

(define (A163530 n) (+ (A163528 n) (A163529 n)))
(define (A163531 n) (+ (A000290 (A163528 n)) (A000290 (A163529 n))))

(define (A163532 n) (if (zero? n) n (- (A163528 n) (A163528 (- n 1)))))
(define (A163533 n) (if (zero? n) n (- (A163529 n) (A163529 (- n 1)))))


;; a(n) = (A163534 A008591) ;; * 9 n

(define (A163534 n) ;; One-based.
   (modulo (+ 3 (A163532 n) (A163533 n) (abs (A163533 n))) 4)
)


(define (A163534origdork n) ;; One-based.
   (+ (modulo (+ 3 (A163532 n) (A163533 n)) 4) (abs (A163533 n)))
)

(define (A163535 n) ;; One-based.
   (modulo (+ 3 (A163532 n) (A163533 n) (abs (A163532 n))) 4)
)

;; Also one-based sequences:
;; (same-intfuns? (compose-funs A163536 A008591) (compose-funs A163536 A008591 A008591) 59049) --> #t
(define (A163536 n) (A163241 (modulo (- (A163534 (1+ n)) (A163534 n)) 4)))
(define (A163537 n) (A163241 (modulo (- (A163535 (1+ n)) (A163535 n)) 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This should sort A163344 into ascending order,
;; which seems to be
;; A037314 Numbers n such that (sum of base 3 digits of n)=(sum of base 9 digits of n).
;;
;; Similarly, A163343(n)+1 (submit!) gives the central diagonal of
;; A163338/A163340, and when it is sorted, there are
;; both primes and squares in it.
;; Also A062880(n)+1 contains primes (is this A145812 ?)
;; Yes,  comment a(n)=2*A000695(n-1)+1 [From Vladimir Shevelev (shevelev(AT)bgu.ac.il), Nov 07 2008]

(define (A128173 n) ;; Numbers in ternary Gray code order.
   n ;; Not ready. Read Zunic's paper.
)


;; Here are the 20 A-numbers you requested: A166404 --- A166423.
;; Here are the 20 A-numbers you requested: A166424 --- A166443.

;; Do something to the starting offsets, e.g. change A163534 and A163540
;; to begin with offset 0? (In which case we define (PARTIALSUMS 1 0 ...) ?

;; One-based:
(define (A166436 n) (A102283 (A163536 n)))

;; Semantics: gives the number of quarter-turns the Hilbert walk traveler
;; has done with regards to the starting (default) direction,
;; since last "wounding back". (There exists a term for this,
;; something like a "winding number" or such.)
;; Compute up to some good value of A166434:
(define A166437 (PARTIALSUMS 2 1 A166436))
;; Note how this differs from "partial sums modulo 4", which gives the absolute direction:
(definec (A163534v2 n) (if (= 1 n) 0 (A010873 (+ (A163534v2 (-1+ n)) (A166436 (-1+ n))))))
;;  A163534(n) = A010873(A166437(n))


(define A166434 (ZERO-POS 1 1 A166437))

;; Begins as: 1,3,4,39,40,363,364,3279,3280,...
;; (take also bisections).
(define A166435 (RECORD-POS 1 1 A166437))

;;;;;;;;;;;;;;;;;;;;;

;; Sum of the winding numbers:
;; (Is it the same as partial sums of sums of relative directions? Of course.
;; Which one is better. Consider also negatives of the other.)
(define (A166439 n) (+ (A166437 n) (A166443 n)))


(define A166438 (ZERO-POS 1 1 A166439))

(define (A166442 n) (A102283 (A163542 n)))
(define A166443 (PARTIALSUMS 2 1 A166442))

(define A166440 (ZERO-POS 1 1 A166443))

;; Begins as: 1,2,3,29,51,461,819,7373,13107,
(define A166441 (RECORD-POS 1 1 A166443))

;; Note how this differs from "partial sums modulo 4", which gives the absolute direction:
(definec (A163540v2 n) (if (= 1 n) 0 (A010873 (+ (A163540v2 (-1+ n)) (A166442 (-1+ n))))))
;;  A163540(n) = A010873(A166443(n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---

;; These already given by Claude Lenormand:
(define (A059253 n) (A025581 (A163358 n)))
(define (A059253v2 n) (A002262 (A163360 n)))
(define (A059253v3 n) (A059905 (A163356 n)))

(define (A059252 n) (A002262 (A163358 n)))
(define (A059252v2 n) (A025581 (A163360 n)))
(define (A059252v3 n) (A059906 (A163356 n)))

(define (A059261 n) (+ (A059252 n) (A059253 n)))
(define (A059285 n) (- (A059253 n) (A059252 n)))

(define (A163538 n) (if (zero? n) n (- (A059253 n) (A059253 (- n 1)))))
(define (A163539 n) (if (zero? n) n (- (A059252 n) (A059252 (- n 1)))))

;; a(n) = (A163534 A008598) ;; * 16 n

(define (A163540 n) ;; One-based.
   (modulo (+ 3 (A163538 n) (A163539 n) (abs (A163539 n))) 4)
)

(define (A163540origdork n) ;; One-based.
   (+ (modulo (+ 3 (A163538 n) (A163539 n)) 4) (abs (A163539 n)))
)

(define (A163541 n) ;; One-based.
   (modulo (+ 3 (A163538 n) (A163539 n) (abs (A163538 n))) 4)
)

;; (same-intfuns? (compose-funs A163542 A008598) (compose-funs A163542 A008598 A008598) (expt 2 16)) --> #t
;; Also one-based sequences:
(define (A163542 n) (A163241 (modulo (- (A163540 (1+ n)) (A163540 n)) 4)))
(define (A163543 n) (A163241 (modulo (- (A163541 (1+ n)) (A163541 n)) 4)))

(define (A163547 n) (+ (A000290 (A059252 n)) (A000290 (A059253 n))))

;; Here are the 20 A-numbers you requested: A163528 --- A163547.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A072661 n) (A059905 (A048679 n)))
(define (A072662 n) (A059906 (A048679 n)))


(define (packA054238 x y) (+ (A000695 x) (* 2 (A000695 y))))
(define (packA054238tr x y) (+ (A000695 y) (* 2 (A000695 x))))

;; Old version: (define (pow2? n) (and (> n 0) (zero? (A004198bi n (- n 1)))))

;; This new one returns the exponent k, such that 2^k = n, in case n is
;; the power of 2, otherwise #f.
(define (pow2?v2 n) (and (> n 0) (zero? (A004198bi n (- n 1))) (A000523 n)))

(define (pow2? n)
  (let loop ((n n) (i 0))
       (cond ((zero? n) #f)
             ((odd? n) (and (= 1 n) i))
             (else (loop (/ n 2) (1+ i)))
       )
  )
)

;; "Binary asymmetry index", 0 if n is a binary palindrome, i.e. in A006995.
(define (A037888 n) (/ (A000120 (A003987bi n (A030101 n))) 2))

(define (A004442 n) (A003987bi n 1)) ;;  Natural numbers, pairs reversed: a(n) = n + (-1)^n; n XOR 1.

(define (A003188 n) (A003987bi n (floor->exact (/ n 2)))) ;;  Gray Code.

(define (A055975 n) (- (A003188 n) (A003188 (- n 1))))

(define (A005811 n) (A000120 (A003188 n))) ;; Number of runs in bin.exp of n (n>0); number of 1's in Gray code for n

(definec (A173318 n) (if (zero? n) n (+ (A173318 (- n 1)) (A005811 n)))) ;; Partial sums of A005811.

(definec (A233271 n) (if (zero? n) n (A233272 (A233271 (- n 1))))) ;; 0, 1, 2, 4, 7, 8, 12, 15, 16, ... ;; Cf. A216431
(define (A233271v2 n) (A054429 (A218616 n)))
(define (A233271v3 n) (A054429 (A179016 (A218602 n))))

;;;;;;;;;;;;;;;;;;;;
;; Interlude:


(define (A257799 n) (A010060 (A233271 n)))

(define (A257800 n) (A000035 (A233271 n)))

(define (A257800v2 n) (if (< n 2) (A000035 n) (- 1 (A213729 (A218602 n)))))

(define A257803 (MATCHING-POS 1 1 (lambda (n) (odd? (A233271 n)))))
(define A257803v2 (MATCHING-POS 1 1 (lambda (n) (odd? (A257800 n)))))

(define A257804 (MATCHING-POS 0 0 (lambda (n) (even? (A233271 n)))))
(define A257804v2 (MATCHING-POS 0 0 (lambda (n) (even? (A257800 n)))))


(define (A257806 n) (- (A257808 n) (A257807 n)))
(definec (A257806v2 n) (if (zero? n) n (+  (expt -1 (A233271 n)) (A257806v3 (- n 1)))))

(definec (A257807 n) (if (zero? n) n (+ (A257800 n) (A257807 (- n 1)))))
(definec (A257808 n) (if (zero? n) n (+ (- 1 (A257800 n)) (A257808 (- n 1)))))


;;;;;;;;;;;;;;;;;;;;

;; Ratushnyak's version of A233271 (submitted before that):
(definec (A216431 n) (if (< n 2) (+ n n) (A233272 (A216431 (- n 1))))) ;; 0, 2, 4, 7, 8, 12, 15, 16, 21, 24, 28, ...

(define (A233272 n) (+ 1 n (A080791 n)))
(define (A233272v2 n) (if (zero? n) 1 (+ n (A000120 (A054429 n)))))

(define (A120511 n) (+ (A005408 (- n 1)) (A080791 (- n 1))))
(define (A120511v2 n) (+ n n (A080791 n) (- (A007814 n)) (- (A036987 (- n 1)))))

(define (A233273 n) (A233272 (A005408 n))) ;; One more than A120511.
(define (A233273v2 n) (+ 1 (A005408 n) (A080791 n)))

(definec (A161511 n) ;; Number of 1...0 pairs in binary representation of 2n.
   (cond ((zero? n) n)
         ((even? n) (+ (A000120 n) (A161511 (/ n 2))))
         (else (+ 1 (A161511 (/ (- n 1) 2))))
   )
)

(define (A161511v0 n)
  (let loop ((n n) (i 1) (s 0))
    (cond ((zero? n) s)
          ((even? n) (loop (/ n 2) (+ i 1) s))
          (else (loop (/ (- n 1) 2) i (+ s i)))
    )
  )
)

(define (A161511v2 n) (A227183 (A006068 n)))
(define (A161511v3 n) (A056239 (A005940 (+ 1 n))))
(define (A161511v4 n) (A243503 (A163511 n)))


(define (A243499 n)
  (let loop ((n n) (i 1) (p 1))
    (cond ((zero? n) p)
          ((even? n) (loop (/ n 2) (+ i 1) p))
          (else (loop (/ (- n 1) 2) i (* p i)))
    )
  )
)

(define (A243499v2 n) (A227184 (A006068 n)))
(define (A243499v3 n) (A003963 (A005940 (+ 1 n))))
(define (A243499v4 n) (A243504 (A163511 n)))
;; (define (A243499v5 n) (A243504 (1+ (A075157 (A006068 n)))))


(define (A226060 n) (- (A213709 (+ n 1)) (A213709 n)))

;; Also relative position where A233271 and A218616 will cross:
;; See https://oeis.org/plot2a?name1=A233271&name2=A218616&tform1=untransformed&tform2=untransformed&shift=0&radiop1=matp&drawlines=true

(define (A226060v2 n)
  (cond ((zero? n) 0)
        (else
          (let loop ((u (- (A000079 (+ n 2)) 1)) (d (A000079 (+ n 1))) (steps 0))
            (cond ((< u d) steps)
                  (else (loop (A011371 u) (A233272 d) (+ steps 1)))
            )
          )
        )
  )
)


;; (0 -1 0 1 1 3 3 19 35 67 127 218 369 660 1267 2476 4863 9453 18078 34173 64374 121515 227965 426603 793638 1482307 2764957 5183333 9830514)
(definec (A234018v2 n)
  (cond ((zero? n) 0)
        ((< n 4) (A234018 n)) ;; Use the other version.
        (else
          (let* ((memosize (if (< n 8) 2 (+ 2 (expt 2 (- n 8)))))
                 (memo (make-vector memosize 0))
                )
            (let loop ((u (- (A000079 n) 1)) (d (A000079 (- n 1))) (i 0) (j #f) (du #f))
              (cond ((pow2? u)
                       (let ((offset (- (floor->exact (/ i 2)) du))) ;; Would give A233274.
                          (- (A054429 (vector-ref memo offset)) (vector-ref memo (+ offset (A000035 i))))
                       )
                    )
                    ((and (< u d) (not j))
                       (vector-set! memo 0 u)
                       (loop (A011371 u) (A233272 d) (+ i 1) 1 i)
                    )
                    (else
                       (if (and j (< j memosize)) (vector-set! memo j u))
                       (loop (A011371 u) (A233272 d) (+ i 1) (and j (+ 1 j)) du)
                    )
              )
            )
          )
        )
  )
)

(define (A048724 n) (A003987bi n (+ n n))) ;; Write n and 2n in binary and add them mod 2. Off=0.
(define (A048724v2 n) (A001969 (+ 1 (A003188 n))))

(define (A065621 n) (A003987bi (- n 1) (+ n n -1))) ;; Write n-1 and 2n-1 in binary and add them mod 2
(define (A065621v2 n) (A000069 (+ 1 (A003188 (- n 1)))))
(define (A065621v3 n) (- (A048724 (- n 1)) (expt -1 n))) ;; - Ralf Stephan, Sep 10 2003

(definec (A065620 n)
  (cond ((zero? n) n)
        ((even? n) (* 2 (A065620 (/ n 2))))
        (else (+ 1 (* -2 (A065620 (/ (- n 1) 2)))))
  )
)

(define (A065620v2 n) (- (A246160 n) (A246159 n)))

(definec (A245812 n) ;; Cf. A246211.
   (cond ((<= n 1) n)
         ((negative? (A065620 n)) (A065621 (+ 1 (A245812 (- (A065620 n))))))
         (else (A048724 (A245812 (- (A065620 n) 1))))
   )
)

(definec (A245812v2 n)
   (cond ((<= n 1) n)
         ((zero? (A010060 n)) (A065621 (+ 1 (A245812v2 (A246159 n)))))
         (else (A048724 (A245812v2 (- (A246160 n) 1))))
   )
)

(define (A193231 n)
  (let loop ((n n) (i 0) (s 0))
     (cond ((zero? n) s)
           ((even? n) (loop (/ n 2) (+ 1 i) s))
           (else (loop (/ (- n 1) 2) (+ 1 i) (A003987bi s (A001317 i))))
     )
  )
)

;; XXX - Check that this entanglement permutation is really equal to self-inverse A193231:
(definec (A193231v2 n)
   (cond ((< n 2) n)
         ((even? n) (A048724 (A193231v2 (/ n 2))))
         (else (A065621 (+ (A193231v2 (/ (- n 1) 2)) 1)))
   )
)

;; For reducing the formula to some other form:
(definec (A193231v3 n)
   (cond ((< n 2) n)
         ((even? n) (A048724 (A193231v3 (/ n 2))))
         (else (+ (A048724 (A193231v3 (/ (- n 1) 2))) (expt -1 (A193231v3 (/ (- n 1) 2)))))
   )
)

(definec (A193231v4 n)
   (cond ((< n 2) n)
         ((even? n) (A048724 (A193231v4 (/ n 2))))
         (else (+ (A048724 (A193231v4 (/ (- n 1) 2))) (expt -1 (A010060 (/ (- n 1) 2)))))
   )
)


(define (A234022 n) (A000120 (A193231 n)))
(define A234023 (MATCHING-POS 1 1 (lambda (n) (> (abs (- (A234022 n) (A234022 (+ n 1)))) 1))))

;; A few compositions & conjugates with other binexp related permutations:
(define (A234024 n) (A059893 (A193231 (A059893 n))))
(define (A234025 n) (A054429 (A193231 n)))
(define (A234026 n) (A193231 (A054429 n)))
(define (A234027 n) (A054429 (A193231 (A054429 n))))


(define (A234612 n) (A003188 (A193231 n)))
(define (A234612v2 n) (A193231 (A006068 n)))

(define (A234613 n) (A193231 (A003188 n)))
(define (A234613v2 n) (A006068 (A193231 n)))


(define (A213709nearby n)
  (let loop ((k (expt 2 n))
             (c 0)
            )
      (let ((zeros (A080791 k)))
         (if (zero? zeros)
             (+ 1 c)
             (loop (+ k zeros 1) (+ c 1))
         )
      )
  )
)


(define (A001787 n) (* n (expt 2 (-1+ n))))

;; A227736-A227745 are now reserved for your use.
;; Keep analogies: A227736 -> A227186, A227738 -> A227188, A227739 -> A227189. 
(define A227737 (LEAST-GTE-I 1 1 A173318)) ;; n occurs A005811(n) times. One based.

(definec (A000788 n) (if (zero? n) n (+ (A000788 (- n 1)) (A000120 n))))
(define (A000788check n) (* (/ 1 2) (add (lambda (k) (+ (A000120 (- n k)) (A000120 k))) 0 n))) ;; Cf. A187059

(define A100922 (COMPOSE (LEAST-GTE-I 1 1 A000788) 1+)) ;; n occurs A000120(n) times. Analogous to A227737, off=0

(define (A245788 n) (* n (A000120 n)))
(define (A249154 n) (* (+ n 1) (A000120 n)))
(define (A249154v2 n) (+ (A245788 n) (A000120 n)))

(define (A187059 n) (- (* 2 (A000788 n)) (A249154 n)))
(define (A187059slow n) (add A065040 (A000217 n) (A000096 n))) ;; Row sums of A065040.
(define (A187059veryslow n) (A007814 (A001142 n)))

(define (A227740 n) (- n (+ 1 (A173318 (- (A227737 n) 1)))))

(define (A243067 n) (- n (+ 1 (A000788 (- (A100922 (- n 1)) 1))))) ;; Analogous to A227740

;; Simple self-inverse permutation of natural numbers: List each block of A005811(n) numbers from A173318(n-1)+1 to A173318(n) in reverse order.
(define (A227741 n) (- (A173318 (A227737 n)) (A227740 n)))
(define (A227742 n) (+ (A173318 (* 2 (- n 1))) (/ (+ 1 (A005811 (- (* 2 n) 1))) 2)))
(define A227742v2 (FIXED-POINTS 1 1 A227741)) ;; Super-naive way.

(define A227743 (NONZERO-POS 1 0 (lambda (k) (A010052 (A173318 k))))) ;; Integers n for which A173318(n) is a square. 
(define (A227744 n) (A173318 (A227743 n))) ;; Squares in A173318.
(define (A227745 n) (A000196 (A227744 n))) ;; Nonnegative integers whose squares occur in A173318. 

(define (A227736 n) (A227186bi (A227737 n) (A227740 n)))
(define (A101211 n) (A227736 (A227741 n)))
(define (A101211v2 n) (A227186bi (A056539 (A227737 n)) (A227740 n)))

;; By Leroy Quet:
;; A163510: 0, 1, 0, 0, 2, 0, 1, 1, 0, 0, 0, 0, 3, 0, 2, 1, 1, 0, 0, 1, 2, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 4
(define (A163510 n) (- (A227186bi (A006068 (A100922 (- n 1))) (A243067 n)) 1))

;; A163511: 1, 2, 4, 3, 8, 9, 6, 5, 16, 27, 18, 25, 12, 15, 10, 7, 32

;; A163511 a(0)=1. a(n) = p(A000120(n)) * product{m=1 to A000120(n)} p(m)^A163510(n,m), where p(m) is the m-th prime.
(definec (A163511v2 n) ;; Based on formula given by Leroy Quet.
  (if (zero? n)
      1
      (let ((w (A000120 n)))
        (let loop ((p (A000040 w)) (m w))
           (cond ((zero? m) p)
                 (else
                    (loop (* p (expt (A000040 m) (A163510 (+ (A000788 (- n 1)) m))))
                          (- m 1)
                    )
                 )
           )
        )
      )
  )
)

(definec (A163511 n)
  (cond ((<= n 1) (+ n 1))
        ((even? n) (* 2 (A163511 (/ n 2))))
        (else (A003961 (A163511 (/ (- n 1) 2))))
  )
)

(define (A163511v3 n) (+ 1 (A075157 (A006068 n)))) ;; Why?

(definec (A243071 n)
  (cond ((<= n 2) (- n 1))
        ((even? n) (* 2 (A243071 (/ n 2))))
        (else (+ 1 (* 2 (A243071 (A064989 n)))))
  )
)

;; Inverse of A163511.
(definec (A243071not_ready n)
   (cond ((= 1 n) 0)
         ((zero? (A241917 n)) (A004754 (A243071not_ready (A052126 n)))) ;; n in A070003 ?
         (else
            (let ((x (A243071not_ready (A052126 n))))
               (+ x (* (A000079 (+ 1 (A000523 x))) (A004754 (A000225 (A241917 n)))))
            )
         )
   )
)


(define (A227738 n) (A227188bi (A227737 n) (A227740 n)))
(definec (A227738v2 n) (if (zero? (A227740 n)) (A227736 n) (+ (A227738v2 (- n 1)) (A227736 n))))

(define (A227739 n) (A227189bi (A227737 n) (A227740 n)))


;; Prove! (Defined with the help A054429 in analogous manner
;; as A122199 is defined as a "recursed variant" of A122155.)

(define (A003188v2 n) (if (< n 1) n (let ((m (A054429 n))) (+ (A053644 m) (A003188v2 (A053645 m))))))

;; Inverse then follows as:
(define (A006068 n) (if (< n 1) n (A054429 (+ (A053644 n) (A006068 (A053645 n))))))

;; Using the formula given by Benoit Cloitre:
;; a(0)=0, a(3k)=1-a(k); a(3k+1)=a(3k+2)=1.
(definec (A014578 n)
  (cond ((zero? n) n)
        ((zero? (modulo n 3)) (- 1 (A014578 (/ n 3))))
        (else 1)
  )
)


;; Fix 0; exchange even and odd numbers.
(define (A014681 n) (if (zero? n) 0 (+ n (- (* 2 (modulo n 2)) 1))))

;; Copied from gf2xfuns.scm:
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

(define (A048720bi-ei-ihan-toimi x y)
   (let loop ((p 0) (x x) (y y))
      (cond ((zero? x) y)
            ((odd? x) (loop (A003987bi p y) (/ (- x 1) 2) (* y 2)))
            (else (loop p (/ x 2) (* y 2)))
      )
   )
)



(define (A048735 n) (A004198bi n (>> n 1)))
(define (A014081 n) (A000120 (A048735 n)))

(define (packA048680oA054238 x y) (A048680 (packA054238 x y)))
(define (packA048680oA054238tr x y) (A048680 (packA054238tr x y)))

(define (A072793 n) (A048680 (A054238 n))) ;; EIS # reserved for packA048680oA054238
(define (A072794 n) (A054239 (A048679 n))) ;; inverse for above.

;; A072732 - A072741 reserved for us.

(define (A072732 n) (packA072732 (A025581 n) (A002262 n)))

(define (packA072732 x y)
   (let ((x-y (- x y)))
     (cond ((<= x-y 0) ;; i.e. (x <= y)
               (packA001477
                  (+ (* 2 x) (modulo x-y 2))
                  (+ (* 2 x) (floor->exact (/ (1+ (- x-y)) 2)))
               )
           )
;;         ((< x-y 2) ;; either x=y, or x=y+1.
;;             (packA001477 (+ (* 2 y) x-y) (* 2 y))
;;         )
           (else  ;; x > y. (floor->exact (/ -1 2)) has to be -1 !!!
               (packA001477
                  (+ (* 2 (1+ y)) (floor->exact (/ (- x-y 2) 2)))
                  (+ (* 2 y) (modulo (1+ x-y) 2))
               )
           )
     )
   )
)

(define (A072733 n) (packA072733 (A025581 n) (A002262 n)))

(define (packA072733 x y)
     (cond ((<= x y) ;; i.e. (x <= y)
             (let ((half-x (floor->exact (/ x 2))))
               (packA001477
                  half-x
                  (+ half-x
                     (* 2 (- y (* 2 half-x) (modulo x 2)))
                     (modulo x 2)
                  )
               )
             )
           )
           (else  ;; x > y
             (let ((half-y (floor->exact (/ y 2))))
               (packA001477
                  (+ 1 half-y
                     (* 2 (- (-1+ x) (* 2 half-y) (modulo y 2)))
                     (modulo y 2)
                  )
                  half-y
               )
             )
           )
     )
)


(define (A072734 n) (packA072734 (A025581 n) (A002262 n)))

;; And code this as an inverse of A072735 (the next one)...

;; Using this as a NxN->N packing bijection for a global arithmetic
;; unranking function for Catalan structures gives a better result
;; than A072642:
;; (define weird1tr (max-n-fun-with-arithrank-scheme packA072734))
;; (map binwidth (map weird1tr (iota0 10)))
;; (0 1 2 4 6  8 13 23 43 82 160)
;; And using it's "4th power" gives even better results in a certain range:
;; (0 1 2 8 9 11 15 23 39 71 134)
;;

(define (packA072734 x y)
   (let ((x-y (- x y)))
     (cond
           ((negative? x-y) ;; i.e. (y > x)
               (packA001477
                  (+ (* 2 x) (modulo (1+ x-y) 2))
                  (+ (* 2 x) (floor->exact (/ (+ (- x-y) (modulo x-y 2)) 2)))
               )
           )
           ((< x-y 3) ;; i.e. (x >= y and x <= y+2)
               (packA001477 (+ (* 2 y) x-y) (* 2 y))
           )
           (else  ;; x >= y+3
               (packA001477
                  (+ (* 2 y) (floor->exact (/ (1+ x-y) 2)) (modulo (1+ x-y) 2))
                  (+ (* 2 y) (modulo x-y 2))
               )
           )
     )
   )
)


(define (A072735 n) (packA072735 (A025581 n) (A002262 n)))

(define (packA072735 x y)
     (cond ((<= x y) ;; i.e. (x <= y)
             (let ((half-x (floor->exact (/ x 2))))
               (packA001477
                  half-x
                  (+ half-x
                     (* 2 (- y (* 2 half-x)))
                     (modulo x 2)
                     (if (and (eq? x y) (even? x)) 0 -1)
                  )
               )
             )
           )
           (else  ;; x > y
             (let ((half-y (floor->exact (/ y 2))))
               (packA001477
                  (+ half-y
                     (* 2 (- (-1+ x) (* 2 half-y)))
                     (modulo y 2)
                     (if (and (eq? x (1+ y)) (even? y)) 1 0)
                  )
                  half-y
               )
             )
           )
     )
)


(define (A072736 n) (A025581 (A072733 n))) ;; X-projection of A072732
(define (A072737 n) (A002262 (A072733 n))) ;; Y-projection of A072732

(define (A072738 n) (A025581 (A072732 n))) ;; X-projection of A072733
(define (A072739 n) (A002262 (A072732 n))) ;; Y-projection of A072733

(define (A072740 n) (A025581 (A072735 n))) ;; X-projection of A072734
(define (A072741 n) (A002262 (A072735 n))) ;; Y-projection of A072734

;; Also A072781 - A072800 reserved for us.

(define (A072781 n) (A025581 (A072734 n))) ;; X-projection of A072735
(define (A072782 n) (A002262 (A072734 n))) ;; Y-projection of A072735

(define (A072783 n) (- (A072740 n) (A072736 n)))
(define (A072784 n) (- (A072741 n) (A072737 n)))
(define (A072785 n) (- (A072781 n) (A072738 n)))
(define (A072786 n) (- (A072782 n) (A072739 n)))


(define (A075300bi x y) (-1+ (* (expt 2 x) (1+ (* 2 y)))))
(define (A075300 n) (A075300bi (A025581 n) (A002262 n)))

(define (A075302bi x y) (-1+ (* (1+ (* 2 x)) (expt 2 y))))
(define (A075302 n) (A075302bi (A025581 n) (A002262 n)))


;; (map binexp->runcount1list (iota0 16))
;; --> (() (1) (1 1) (2) (1 2) (1 1 1) (2 1) (3) (1 3)
;;      (1 2 1) (1 1 1 1) (1 1 2) (2 2) (2 1 1) (3 1) (4) (1 4))

(define (binexp->runcount1list n) ;; (length (binexp->runcount1list n)) gives A005811
   (if (zero? n)
       (list)
       (let loop ((n n) (rc (list)) (count 0) (prev-bit (modulo n 2)))
          (if (zero? n)
              (cons count rc)
              (if (eq? (modulo n 2) prev-bit) ;; If the lsb has not changed
                  (loop (floor->exact (/ n 2))
                        rc
                        (1+ count)
                        (modulo n 2)
                  )
                  (loop (floor->exact (/ n 2)) ;; If it has changed
                        (cons count rc)
                        1
                        (modulo n 2)
                  )
              )
          )
       )
  )
)


(define (runcount1list->binexp lista)
   (let loop ((lista lista) (s 0) (state 1))
     (cond ((null? lista) s)
           (else (loop (cdr lista)
                       (+ (* s (expt 2 (car lista)))
                          (* state (- (expt 2 (car lista)) 1))
                       )
                       (- 1 state)
                 )
           )
     )
   )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some lost code, resurrected from https://oeis.org/A129594
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A129594 n) (if (zero? n) n (ascpart_to_binexp (conjugate-partition (binexp_to_ascpart n)))))

(define (A005811v2 n) (length (binexp_to_ascpart n))) ;; Works from n>=1 onward.

(define (A043276 n) (if (zero? n) n (apply max (binexp->runcount1list n)))) ;; Zero for zero for convenience.

(define (A167489 n) (apply * (binexp->runcount1list n))) ;; Product of run lengths in binary representation of n

(define (A167489v2 n) (* (A227349 n) (A227350 n))) ;; Product of runlengths.

(define (A167489v3 n) (A227355 (A227352 (A005408 n))))

(define (A227349 n) (apply * (bisect (reverse (binexp->runcount1list n)) (- 1 (modulo n 2))))) ;; Product of lengths of 1-runs
(define (A227350 n) (apply * (bisect (reverse (binexp->runcount1list n)) (modulo n 2)))) ;; Prod. of lengths of 0-runs

(define (A246674 n) (fold-left (lambda (a r) (* a (A000225 r))) 1 (bisect (reverse (binexp->runcount1list n)) (- 1 (modulo n 2)))))

(define (A246685 n) (fold-left (lambda (a r) (if (= 1 r) a (* a (A000215 (- r 2))))) 1 (bisect (reverse (binexp->runcount1list n)) (- 1 (modulo n 2)))))

(define (A247282 n) (fold-left (lambda (a r) (* a (A001317 (- r 1)))) 1 (bisect (reverse (binexp->runcount1list n)) (- 1 (modulo n 2)))))

(define (A001316 n) (A000079 (A000120 n)))

(define (A001316v2 n) (fold-left (lambda (a r) (* a (A000079 r))) 1 (bisect (reverse (binexp->runcount1list n)) (- 1 (modulo n 2)))))


(define (A246595 n) (fold-left (lambda (a r) (* a r r)) 1 (bisect (reverse (binexp->runcount1list n)) (- 1 (modulo n 2)))))

(define (A246596 n) (fold-left (lambda (a r) (* a (A000108 r))) 1 (bisect (reverse (binexp->runcount1list n)) (- 1 (modulo n 2)))))


(define (A246660 n)
  (let loop ((n n) (i 0) (p 1))
     (cond ((zero? n) p)
           ((odd? n) (loop (/ (- n 1) 2) (+ i 1) (* p (+ 1 i))))
           (else (loop (/ n 2) 0 p))
     )
  )
)

(define (A246660v2 n) (fold-left (lambda (a r) (* a (A000142 r))) 1 (bisect (reverse (binexp->runcount1list n)) (- 1 (modulo n 2)))))

(definec (A246660v3 n)
  (cond ((zero? n) 1)
        ((even? n) (A246660v3 (/ n 2)))
        (else (* (A007814 (+ n 1)) (A246660v3 (/ (- n 1) 2))))
  )
)

(define (A227355 n) (A167489 (A003714 n)))
(define (A227355v2 n) (A227350 (A003714 n))) ;; Product of lengths of runs (or 0-runs only) in Z.E.
(define A003714v2 (MATCHING-POS 0 0 (lambda (i) (= (A167489 i) (A227350 i)))))
(define A003714v3 (MATCHING-POS 0 0 (lambda (i) (= 1 (A227349 i))))) ;; Quite obvious...

(define (A227193 n) (- (A227349 n) (A227350 n)))

(define (A227190 n) (- n (A167489 n)))
;; Cf. A227191. Are there points where beanstalk based on A227190 would always visit? E.g. A000975, A084639 ?

(definec (A227183 n)
   (let loop ((n n)
              (i (A005811 n))
              (d 0)
              (s 0)
             )
       (cond ((zero? n) s)
             (else (loop (A163575 n) ;; "n shifted one run right".
                         (- i 1)
                         (expt 1 d) ;; 0 changes to 1, 1 stays as such.
                         (+ s (* i (- (A136480 n) d)))
                   )
             )
       )     
   )
)

(define (A227183v2 n) (if (zero? n) n (apply + (binexp_to_ascpart n))))

(define (A227183v3 n)
  (let loop ((i (- (A005811 n) 1)) (s 0))
       (cond ((< i 0) s)
             (else (loop (- i 1) (+ s (A227189bi n i))))
       )
  )
)

(define (A227183v4 n) (- (A227192 n) (A000217 (- (A005811 n) 1))))

(define (A227183v5 n) (add A227739 (+ 1 (A173318 (- n 1))) (A173318 n)))

;; A227368 - A227370  are now reserved for your use.
(define A227368 (LEAST-I-WITH-FUN-I-EQ-N 0 0 A227183))
;; (define A227369list (sort (map A227368 (iota0 120)) <)) ;; A227368 same sorted into ascending order.
;; (define (A227369 n) (list-ref A227369list n))
(define A227369 (DISTINCT-POS 0 0 A227183))
(define (A227370 n) (A227183 (A227369 n))) ;; A227370 the permutation that maps between them. An involution?

;; A227761-A227762 are now reserved for your use.

(define (A227761 n) (if (< n 2) 0 (- (A043276 (A163575 (A227368 n))) 1)))
(define (A227761v2 n) (if (< n 2) 0 (apply max (DIFF (binexp_to_ascpart (A227368 n))))))
(define A227762 (ZERO-POS 1 0 A227761))

(define (A227184 n) (if (zero? n) 1 (apply * (binexp_to_ascpart n))))

(define (A227185 n) (if (zero? n) n (+ 1 (- (A029837 (+ 1 n)) (A005811 n)))))
(define (A227185v2 n) (if (zero? n) n (car (reverse (binexp_to_ascpart n)))))

;; The length of k:th (zero-based) run (from the lsb-end) of the bin.exp of n.
(define (A227186bi n k)
  (cond ((< (A005811 n) (+ 1 k)) 0) ;; Return zero if there are less runs than k+1.
        ((zero? k) (A136480 n))
        (else (A227186bi (A163575 n) (- k 1)))
  )
)

(define (A227188bi n k)
  (cond ((< (A005811 n) (+ 1 k)) 0) ;; Return zero if there are less runs than k+1.
        ((zero? k) (A136480 n))
        (else (+ (A136480 n) (A227188bi (A163575 n) (- k 1))))
  )
)


(define (A227189bi n k)
  (cond ((< (A005811 n) (+ 1 k)) 0) ;; Return zero if there are less runs than k+1.
        ((zero? k) (A136480 n))
        (else (+ (- (A136480 n) 1) (A227189bi (A163575 n) (- k 1))))
  )
)


(define (A227186 n) (A227186bi (A002262 n) (A025581 n)))

(define (A227188 n) (A227188bi (A002262 n) (A025581 n)))

(define (A227189 n) (A227189bi (A002262 n) (A025581 n)))

(define (A227192 n)
  (let loop ((i (- (A005811 n) 1)) (s 0))
       (cond ((< i 0) s)
             (else (loop (- i 1) (+ s (A227188bi n i))))
       )
  )
)

(define (A227192v2 n) (+ (A227183 n) (A000217 (- (A005811 n) 1))))

(define (A227192v3 n) (add A227738 (+ 1 (A173318 (- n 1))) (A173318 n)))

(define (A136480v2 n) (if (zero? n) 1 (car (binexp_to_ascpart n)))) ;; The smallest part in the partition.

(define (conjugate-partition ascpart)
   (let loop ((conjpart (list)) (ascpart ascpart))
        (cond ((null? ascpart) conjpart)
              (else (loop (cons (length ascpart) conjpart) (delete-matching-items! (map -1+ ascpart) zero?)))
        )
   )
)

(define (binexp_to_ascpart n)
   (let ((runlist (reverse! (binexp->runcount1list n)))) (PARTSUMS (cons (car runlist) (map -1+ (cdr runlist)))))
)

(define (ascpart_to_binexp ascpart) (runcount1list->binexp (reverse! (cons (car ascpart) (map 1+ (DIFF ascpart))))))



(define (bulgarian-operation ascpart)
   (let loop ((newpartition (list (length ascpart))) (ascpart ascpart))
        (cond ((null? ascpart) (sort newpartition <))
              (else
                 (loop (if (= 1 (car ascpart)) newpartition (cons (- (car ascpart) 1) newpartition))
                       (cdr ascpart)
                 )
              )
        )
   )
)


(define (bulgarian-operation-nth-order ascpart n)
  (if (or (zero? n) (null? ascpart)) ;; Zeroth order means identity function on partitions.
      ascpart
      (let ((newpart (length ascpart)))
        (let loop ((newpartition (list)) (ascpart ascpart))
             (cond ((null? ascpart)
                      (sort (cons newpart (bulgarian-operation-nth-order newpartition (- n 1))) <)
                   )
                   (else
                      (loop (if (= 1 (car ascpart)) newpartition (cons (- (car ascpart) 1) newpartition))
                            (cdr ascpart)
                      )
                   )
             )
        )
      )
  )
)



;; A037481 (cf. A037487) gives the fixed points:
(definec (A226062 n) (if (zero? n) n (ascpart_to_binexp (bulgarian-operation (binexp_to_ascpart n)))))

;; A227752-A227753 are now reserved for your use.

(definec (A227752 n) (add (lambda (k) (if (= n (A226062 k)) 1 0)) 0 (A000225 (A227183 n))))
(define A227753 (ZERO-POS 1 0 A227752))
(define (A225794 n) (A227183 (A227753 n)))

(definec (rowfun-for-A227141 n)
 (if (< n 2)
     (lambda (k) n) ;; Give A000012 for row n=1.
     (implement-cached-function 0 (rowfun-n k)
        (cond ((zero? k) 1)
              ((< k n) k)
              ((= k n) (- n 1))
              ((= k (+ n 1)) k)
              ((< k (* 2 n)) n)
              (else
                (let loop ((i 1))
                     (if (< (rowfun-n (- k i)) i)
                         (- i 1)
                         (loop (+ i 1))
                     )
                )
              )
        )
     )
 )
)

(define (A227141bi row col) ((rowfun-for-A227141 row) col))

(define (A227141 n) (A227141bi (+ 1 (A002262 n)) (A025581 n)))

;; Get terms in range [n,n^2] from each row:
;; [1]; [2,3,4]; [3,4,5,6,7,8,9]; [4,5,6,7,8,9,10,11,12,13,14,15,16]; ...
;; I.e.: 1; 1 3 1; 2 4 3 2 3 4 2; 3 5 4 4 3 4 5 4 3 4 4 5 3; 4 6 5 5 5 4 5 6 5 5 4 5 5 6 5 4 5 5 5 6 4;
(define (A227147 n) (A227141bi (A227177 n) (A227181 n)))
;; Turha: (define (A227147v2 n) (A227141bi (A227177 n) (+ (A227177 n) (A227179 n))))
(define (A227147v2 n) (- (A227185 (A227452 n)) (* (if (> n 1) 1 0) (- (A227177 (+ n 1)) (A227177 n)))))

(define (A227147v3 n) (if (< n 2) n (A005811 (A227452 (- n 1)))))


;; Compare with the other definition of A218616:
(definec (A227452 n)
   (cond ((< n 2) n)
         ((A226062 (A227452 (- n 1)))
              => (lambda (next)
                    (if (= next (A227452 (- n 1))) ;; We reached the fixed point, one of the terms of A037481
                        (A227451 (A227177 (+ 1 n))) ;; Get the top for the next larger triangular tree.
                        next ;; Else, use the value obtained.
                    )
                 )
         )
   )
)

(definec (compose-A226062-to-nth-power n)
 (cond ((zero? n) (lambda (x) x))
       (else (lambda (x) (A226062 ((compose-A226062-to-nth-power (- n 1)) x))))
 )
)

(define (A227452v2 n) ((compose-A226062-to-nth-power (A227179 n)) (A227451 (A227177 n))))






;; For each 0 in lista we generate a single 0 in binary expansion,
;; and for each non-zero natural number n, we generate n 1's followed
;; by one 0.
(define (n-tuple->bittuple lista)
   (let loop ((lista lista) (s 0))
     (cond ((null? lista) s)
           (else (loop (cdr lista)
                       (+ (* (expt 2 (+ 1 (car lista))) s)
                          (* 2 (- (expt 2 (car lista)) 1))
                       )
                 )
           )
     )
   )
)





;;

;; Replace in the binary expansion of n each 1-bit
;; with the distance to the next 1-bit:
;;
;; (binexp->siteswap-list 5)  --> (2 0 1)
;; (binexp->siteswap-list 6)  --> (1 2 0)
;; (binexp->siteswap-list 7)  --> (1 1 1)
;; (binexp->siteswap-list 8)  --> (4 0 0 0)
;; (binexp->siteswap-list 9)  --> (3 0 0 1)
;; (binexp->siteswap-list 10) --> (2 0 2 0)

(define (binexp->siteswap-list n)
   (if (zero? n)
       (list)
       (let loop ((n n) (rc (list)) (count 1))
          (if (zero? n)
              rc
              (if (zero? (modulo n 2))
                  (loop (/ n 2)
                        (cons 0 rc)
                        (1+ count)
                  )
                  (loop (floor->exact (/ n 2))
                        (cons count rc)
                        1
                  )
              )
          )
       )
  )
)



;; Pierre Lamothe, Fri, 21 May 2004:

(definec (A116623 n) ;; C.f. A001047(n) = A116623(A000225(n))
   (cond ((zero? n) 1)
         ((even? n) (+ (A116623 (/ n 2)) (expt 2 (A000523 n))))
         (else (+ (* 3 (A116623 (/ (- n 1) 2))) (expt 2 (+ 1 (A000523 n)))))
   )
)

(define (A116640 n) (A116623 (A059893 n)))

;; (define seqA116641 (list-head (uniq (sort (map A116623 (iota0 512)) <)) 162))

(define (A116641 n) (list-ref seqA116641 n))

(define (A116642 n) (A007088 (A116641 n)))

;; A055941 a(n) = sum(i[j]-j, j = 0..k-1) where n = sum( 2^i[j], j = 0 .. k-1).
;; As I interpret it: vector i gives the positions of 1-bits in n,
;;  and we take (positions 0-based from the least significant end):
;;   Sum (position of each 1-bit - how manyth 1-bit it is from the right
;;                                 (first = 0, second = 1, etc.)
;; over all 1-bits of n.
;;
;; This is equivalent of taking
;;   Sum (total number of zero-bits to the right of 1-bit)
;; over all 1-bits of n.

(define (A055941 n)
  (let loop ((n n) (ze 0) (s 0)) ;; ze = zeros encountered, s = sum.
    (cond ((zero? n) s)
          ((even? n) (loop (/ n 2) (1+ ze) s))
          (else (loop (/ (-1+ n) 2) ze (+ s ze)))
    )
  )
)

;; A161511 Number of 1...0 pairs in binary representation of 2n.
;; a(2n) = a(n) + A000120(n); a(2n+1) = a(n) + 1.
;; Note that A161511(n) = A055941(n)+A000120(n).

(define (A161511 n)
  (let loop ((n n) (ze 0) (s 0)) ;; ze = zeros encountered, s = sum.
    (cond ((zero? n) s)
          ((even? n) (loop (/ n 2) (1+ ze) s))
          (else (loop (/ (-1+ n) 2) ze (+ s (1+ ze))))
    )
  )
)

;; Square array A126441: (Alford Arnold)
;; a(n) occurs in the row A053645(n).
;; Each non-zero m occurs in row A053645(m) and column A161511(m).
;; (both zero-based.)
;; For each row r, the first non-zero term is A004760(r+1).
;; The least possible starting column for the row on which a(n)
;; is, is A000523(n+1).

;; The second value on each row is A004760(n+1) plus A062383(n);
;; subsequent values increase by ever enlarging powers of two. 
;;

(define (A126441 n) (A126441onebased (1+ n)))

(definec (A126441onebased n)
  (cond ((< n 2) n)
        (else
           (let ((prev (A126441onebased (- n (/ (A053644 n) 2)))))
               (if (or (= (A053644 n) (* 2 (A053644 (A053645 n)))) ;; n in A004755?
                          (zero? prev) ;; Row hasn't started yet?
                   )
                   (let ((starter (A004760 (1+ (A053645 n)))))      
                     (if (> (A161511 starter) (1+ (A000523 n)))
                         0
                         starter
                     )
                   )
                   (A004754 prev)
               )
           )
        )
  )
)



(define (A161920 n) (A161511 (A004760 n)))

;; Offset 1.
(define A166274 (NONZERO-POS 1 0 A126441))
(define A166275 (ZERO-POS 1 0 A126441))

(defineperm1 (A161924 n) (A126441 (A166274 n)))
(define (A166276 n) (A161924 (- n))) ;; upto 87.


;; A000070, off 0, starts as: 1,2,4,7,12,19,30,45,67,97,139,195,272,
;; A026905, off 1, starts as:   1,3,6,11,18,29,44,66,96,138,194,271,

(define seqA000070 '(1 2 4 7 12 19 30 45 67 97 139 195 272 373 508 
 684 915 1212 1597 2087 2714 3506 4508 5763 7338 
 9296 11732 14742 18460 23025 28629 35471 43820 
 53963 66273 81156 99133 120770 146785 177970 
 215308 259891 313065 376326 451501)
)

(define seqA026905 '(#f 1 3 6 11 18 29 44 66 96 138 194 271 372 507 683 
 914 1211 1596 2086 2713 3505 4507 5762 7337 9295 
 11731 14741 18459 23024 28628 35470 43819 53962 
 66272 81155 99132 120769 146784 177969 215307)
)

(define (A000070 n) (list-ref seqA000070 n)) ;; Lazy now!
(define (A026905 n) (list-ref seqA026905 n))

;; Sequence A161919: sort each subsequence A161924(A000070(k-1)..A026905(k))
;; into ascending order.

;; (define seqA161924 (cons #f (map A161924 (iota (A026905 19)))))
;; (define seqA161919 (cons #f (append-map! (lambda (n) (sort (sublist seqA161924 (A000070 (-1+ n)) (1+ (A026905 n))) <)) (iota 19))))

(defineperm1 (A161919 n) (list-ref seqA161919 n)) ;; Lazy now!
(define (A166277 n) (A161919 (- n)))

(definec (A001477hardway n) ;; Compressed version of A126441.
  (cond ((< n 2) n)
;; n+1 in A004755?
        ((= (A053644 n) (* 2 (A053644 (A053645 n))))
           (A004760 (1+ (A053645 n)))
        )
        (else ;; Otherwise, the previous term of the row + msb of that term.
           (A004754 (A001477hardway (- n (/ (A053644 n) 2))))
        )
  )
)


;; Here are the 6 A-numbers you requested: A116623 --- A116628.

;; From Paul D. Hanna <pauldhanna@juno.com> Feb 19 2006:

;; Set a(1)=1; for n>1, a(n) = least positive integer not appearing earlier
;; such that 
;; {a(k)|1=<k<=n} and {a(k) XOR a(k-1) |1=<k<=n} are disjoint sets of
;; distinct numbers. 
;;  
;; Define b(n) = a(n) XOR a(n+1); 
;; the sequences begin (unless I made a mistake): 
;; a(n): 1, 2, 4, 8, 5, 10, 16, 7, 9, 17, 32, 11, 18, 33, 19, 35, 20, 34,
;; 22, 40, 21, 41, 28, 36, 27, 64, ...
;; b(n):   3, 6, 12, 13, 15, 26, 23, 14, 24, 49, 43, 25, 51, 50, 48, 55, 54,
;; 52, 62, 61, 60, 53, 56, 63, 91,  ...
;;  
;; Q: Where do the powers of 2 appear in a()? 
;; 1,2,3,4,7,11,26, ...
;;
;; (map A116628 (iota 13)) --> (1 2 3 4 7 11 26 42 109 166 373 772 1532)
;; (map A116624 (map A116628 (iota 13))) --> (1 2 4 8 16 32 64 128 256 512 1024 2048 4096)


;; A116624(1) = 1; A116624(n) = the least positive integer i distinct from any of A116624(1..n-1)
;; and A116625(1..n-2), such that
;; also (i XOR A116624(n-1)) is not present in A116625(1..n-2) nor in A116624(1..n-1)
;; (Note that (i XOR A116624(n-1)) cannot be A116624(n-1), because i > 0)
;; Are any of these conditions superfluous?

(definec (A116624old n)
  (cond
    ((= 1 n) 1)
    (else
     (let outloop ((i 1)) ;; Play sure, and start always from the beginning, i is our candidate.
       (let ((k (A003987bi i (A116624old (- n 1))))) ;; Candidate for A116625(n-1)
         (let inloop ((j (- n 1))) ;; We compare i and k to all A116624old(n-1 .. 1) and A116625(n-1 .. 1)
           (cond ((zero? j) i) ;; No clashes found, 'i' is the man we were looking for!
                 ((= i (A116624old j)) (outloop (+ i 1))) ;; A116624 wouldn't be distinct, try next.
                 ((= i (A116625old (- j 1)))
                                    (outloop (+ i 1))) ;; A116624 wouldn't be disjoint from A116625
                 ((= k (A116625old (- j 1)))
                                    (outloop (+ i 1))) ;; A116625 wouldn't be distinct, try next.
                 ((= k (A116624old j))
                                    (outloop (+ i 1))) ;; A116625 wouldn't be disjoint from A116624
                 (else (inloop (- j 1))) ;; So far so good, check also smaller terms of A116624 & -25.
           )
         )
       )
     )
    )
  )
)

(define (A116625old n) (if (zero? n) n (A003987bi (A116624old n) (A116624old (+ n 1)))))

(definec (A116626old n) (if (odd? n) (A116624old (/ (+ n 1) 2)) (A116625old (/ n 2))))

(definec (A116627old n) (first-n-where-fun_n-is-i1 A116626old n))

(define (A116624 n) (A116626 (- (* 2 n) 1)))
(define (A116625 n) (A116626 (* 2 n)))

;; If n is odd, then A116626(n) = A116626(n-1) XOR A116626(n-2).
;; if n is even, then find the first i >= A116648(n-1)
;; such that neither i nor (i XOR A116626(n-1)) is present in A116626(1..n-1),
;; and return (i XOR A116626(n-1)). (What if we return i instead?)

(defineperm1 (A116626 n)
  (cond
    ((= 1 n) 1)
    ((odd? n) (A003987bi (A116626 (-1+ n)) (A116626 (- n 2))))
    (else
     (let outloop ((i (A116648 (-1+ n))))
       (let ((k (A003987bi i (A116626 (-1+ n)))))
         (let inloop ((j (- n 1))) ;; We compare i and k to all A116626(n-1 .. 1)
           (cond ((zero? j) k) ;; No clashes found, 'k' is the man we were looking for!
                 ((= i (A116626 j)) (outloop (+ i 1)))
                 ((= k (A116626 j)) (outloop (+ i 1)))
                 (else (inloop (- j 1))) ;; So far so good check also against smaller terms of A116626.
           )
         )
       )
     )
    )
  )
)

(define (A116627 n) (A116626 (- n))) ;; Take values from invcache. Must be computed after A116626.

(define A116628 (fun-succ-matching-is0 (lambda (i) (pow2? (A116624 i)))))

;; We consider a > b (i.e. not less than b) also in case a is nil.
;; (Because the stateful caching system used by defineperm1.)

(define (not-lte? a b) (cond ((not (number? a)) #t) (else (> a b))))

;; Here are the 4 A-numbers you requested: A116648 --- A116651.

;; (map A116648 (iota 128))
;; (2,2,4,4,5,5,5,5,7,7,7,7,7,7,9,9,11,11,11,11,11,11,18,18,19,19,19,19,20,20,20,20,21,21,21,21,21,21,21,21,27,27,27,27,27,27,27,27,29,29,29,29,30,30,30,30,30,30,30,30,30,30,39,39,39,39,42,42,42,42,44,44,44,44,45,45,45,45,46,46,46,46,46,46,47,47,47,47,71,71,71,71,71,71,73,73,73,73,74,74,74,74,75,75,75,75,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76)

;; (map A116649 (iota 32))
;; (1 3 5 9 15 17 23 25 29 33 41 49 53 63 67 71 75 79 85 89 95 99 103 107 139 143 163 167 175 179 199 219)

;; (map A116650 (iota 32))
;; (2 4 5 7 9 11 18 19 20 21 27 29 30 39 42 44 45 46 47 71 73 74 75 76 78 83 87 88 90 109 113 116)

;; (map A116651 (iota0 5))
;; (2 2 4 5 9 20)

(definec (A116648 n)
   (if (< n 2) (+ n 1) ;; A116626 begins as 1,3,2,6,4,12,8,13,5,15,...
       (let ((prev  (A116648 (- n 1))))
          (cond ((not (= (A116626 n) prev)) prev)
                (else (let loop ((i (+ 1 prev)))
                           (cond ((not-lte? (A116627 i) n) i)
                                 (else (loop (+ 1 i)))
                           )
                      )
                )
          )
       )
   )
)

(define A116649 (fun-succ-matching-is0 (lambda (i) (not (= (A116648 (- i 1)) (A116648 i))))))
(define (A116650 n) (A116648 (A116649 n)))
(define (A116651 n) (A116648 (A000079 n)))



;; The first differences of A005228:
;; (define (A030124 n) (cond ((= n 0) 2) ((= n 1) 4) (else (- (A005228 (+ n 2)) (A005228 (+ n 1))))))

;; Hofstadter's A005228 = 1,3,7,12,18,26,35,45,56,69,83,98,114,131,150,170,191,213,236,260,... (starting offset = 1).
;; A030124 2,4,5,6,8,9,10,11,13,14,15,16,17,19,20,...
;; A000027  1,2,3,4,5,6,7,8,9
;; pseuinv  0,1,1,2,3,4,4,5,...
;; (define A005228 (COMPLEMENT 1 (compose-funs A030124 -1+))) ;; Needs one-based seq.




;; Hofstadter's A005228 = 1,3,7,12,18,26,35,45,56,69,83,98,114,131,150,170,191,213,236,260,...
;; A030124 2,4,5,6,8,9,10,11,13,14,15,16,17,19,20,...
;; A000027  1,2,3,4,5,6,7,8,9

;; Defined as the partial sums of A030124
;; Note that to kick start this recursion, we need to specify the three initial values explicitly:
(definec (A005228 n) (cond ((= n 1) 1) ((= n 2) 3) ((= n 3) 7) (else (+ (A005228 (- n 1)) (A030124 (- n 1))))))

(define A030124 (COMPLEMENT 1 A005228)) ;; One-based.
(define A030124off0 (compose-funs A030124 1+)) ;; Zero-based.

;; A232739-A232753 are now reserved for your use.

;; (map A005228 (iota 29))
;; (1 3 7 12 18 26 35 45 56 69 83 98 114 131 150 170 191 213 236 260 285 312 340 369 399 430 462 495 529)

;; (map A030124 (iota 29))
;; (2 4 5 6 8 9 10 11 13 14 15 16 17 19 20 21 22 23 24 25 27 28 29 30 31 32 33 34 36)

(define A232746 (COMPOSE -1+ (LEAST-GTE-I 1 1 (COMPOSE -1+ A005228))))  ;; n occurs A030124 times.

;; (define A232746vanha_wrong (LEAST-GTE-I 1 1 (COMPOSE -1+ A005228)))



;; (map A232747 (iota 45)) ;; "Inverse function for Hofstadter's A005228"
;; 1,0,2,0,0,0,3,0,0, 0, 0, 4, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 6,

;; (map A232748 (iota 42))
;; 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
;; 0,1,1,2,3,4,4,5,6, 7, 8, 8, 9,10,11,12,13,13,14,15,16,17,18,19,20,20,21,22,23,24,25,...

(definec (A232747 n) ;; "Inverse function for Hofstadter's A005228"
   (cond ((< n 2) n)
         (else (* (A232746 n) (- (A232746 n) (A232746 (- n 1)))))
   )
)


(definec (A232748 n)
   (cond ((< n 2) (- n 1))
         (else (+ (A232748 (- n 1)) (- 1 (- (A232746 n) (A232746 (- n 1))))))
   )
)

;; Note that A232747(n)*A232749(n) = 0 for all n.
(definec (A232749 n) ;; "Inverse function for Hofstadter's A030124"
   (cond ((< n 2) 0)
         (else (* (A232748 n) (- (A232748 n) (A232748 (- n 1)))))
   )
)

;; (first-dislocated (map A232752 (map A232751 (iota0 1024)))) --> ()

(definec (A232751 n)
   (cond ((< n 2) n)
         ((not (zero? (A232747 n))) (* 2 (A232751 (- (A232747 n) 1))))
         (else (+ 1 (* 2 (A232751 (A232749 n)))))
   )
)

;; (map A232752 (iota0 63))
;; (0 1 3 2 12 5 7 4 114 16 26 8 45 10 18 6 7562 127 191 21 462 32 56 11 1285 53 83 14 236 23 35 9 29172079 7677 9314 141 20528 208 312 27 115291 489 679 39 1943 65 98 15 865555 1331 1751 62 4111 94 150 19 30983 255 369 29 802 42 69 13)

(definec (A232752 n)
   (cond ((< n 2) n)
         ((even? n) (A005228 (+ 1 (A232752 (/ n 2)))))
         (else (A030124 (A232752 (/ (- n 1) 2))))
   )
)


(definec (A232739 n) (if (= n 1) 2 (A030124 (A232739 (- n 1)))))

(define (A232750 n) (if (zero? n) 1 (- (A232746 (A232739 (+ n 1))) (A232746 (A232739 n)))))

(define (A232740 n) (- (A232753 (A005228 (+ n 1))) (A232753 (A005228 n))))
(define A232753 (LEAST-GTE-I 1 1 A232739)) ;; a(n) = largest k such that A232739(k) <= n.

(definec (A225850 n) (if (< n 3) n (- (* 2 (+ (A232747 n) (A232749 n))) (- (A232746 n) (A232746 (- n 1))))))


;; Few from Amarnath Murthy:

;; %S A096111 1,2,2,3,3,6,6,4,4,8,8,12,12,24,24,5,5,10,10,15,15,30,30,20,20,40,40,60,
;; %T A096111 60,120,120,6,6,12,12,18,18,36,36,24,24,48,48,72,72,144,144,30,30,60,60,
;; %U A096111 90,90

;; Zero-based.
(definec (A096111 n)
  (cond ;; ((zero? n) 1)
        ((pow2? (+ n 1)) (+ 2 (A000523 n)))
        (else (* (+ 1 (A000523 n)) (A096111 (A053645 n))))
  )
)

;; Bisection of above (zero-based):
(define (A121663v2 n) (A096111 (* 2 n)))


;; Zero-based.
(definec (A121663 n)
  (cond ((zero? n) 1)
        ((pow2? n) (+ 2 (A000523 n)))
        (else (* (+ 2 (A000523 n)) (A121663 (A053645 n))))
  )
)



;; One-based, permutation of natural numbers:
;;               v     v              v                                   v
;;           1 2 3 4 5 6  7  8 9 0 1 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
;;%S A096114 1,2,3,5,4,6,10,11,9,8,7,12,19,20,21,23,22,18,16,17,15,14,13,24,37,38,
;;%T A096114 39,41,40,42,46,47,45,44,43,36,31,32,33,35,34,30,28,29,27,26,25,48

;; C.f. A073080

(definec (A096114 n)
  (cond ((< n 3) n)
        ((and (zero? (modulo n 3)) (pow2? (/ n 3))) n)
        (else (let ((lastfix (* 3 (expt 2 (A000523 (floor->exact (/ n 3)))))))
                (+ lastfix (A096114 (- (* 2 lastfix) n)))
              )
        )
  )
)

;; Inverse of the above. One-based. Seems to be formed in a simple way:
;; 1 2 3 5 4 6 11 10 9 7 8 12 23 22 21 19 20 18 13 14 15 17 16 24
;; 47 46 45 43 44 42 37 38 39 41 40 36 25 26 27 29 28 30 35 34 33 31 32 48
(definec (A121664 n) (first-n-where-fun_n-is-i1 A096114 n))


;; One-based. Could be zero-based as well.
(definec (A096115 n)
  (cond
        ((pow2? (+ n 1)) (+ 1 (A000523 n)))
        ((pow2? n) (+ 1 (A096115 (- n 1))))
        (else (* (+ (A000523 n) 1) (A096115 (A035327 (- n 1)))))
  )
)

;; One-based. Could be zero-based as well.
(definec (A096115v2 n)
  (cond
        ((pow2? (+ n 1)) (+ 1 (A000523 n)))
        ((pow2? n) (+ 1 (A096115v2 (- n 1))))
        (else (* (+ (A000523 n) 1) (A096115v2 (- (expt 2 (A000523 (- n 1))) (A053645 (- n 1)) 1))))
  )
)

;; A122154 --- A122156.
;; (map (lambda (n) (count-the-occurrences A096115 n 1 (A000079 n))) (iota 16))
;; --> (1 2 2 2 2 4 2 4 2 4 2 6 2 4 4 4)
;; halved --> Is it A045778, yes, it is!

(define (A025147v2_slow n) (if (< n 2) n (count-the-occurrences A096116 n 1 (+ 1 (A000079 (- n 2))))))
(define (A045778v2_slow n) (count-the-occurrences A121663 n 0 (if (= 1 n) 0 (A000079 (- n 2)))))

(define (A122155 n)
   (cond ((< n 1) n)
         ((pow2? n) n)
         (else (- (* 2 (A053644 n)) (A053645 n)))
   )
)

(definec (A122198 n)
   (if (< n 1)
       n
       (A122155 (+ (A053644 n) (A122198 (A053645 n))))
   )
)

(definec (A122199 n)
   (if (< n 1)
       n
       (let ((m (A122155 n)))
          (+ (A053644 m) (A122199 (A053645 m)))
       )
   )
)


(definec (A096116 n)
  (cond ((= 1 n) 1)
        ((pow2? (- n 1)) (+ 2 (A000523 (- n 1))))
        (else (+ 2 (A000523 (- n 1)) (A096116 (+ 2 (A035327 (- n 1))))))
  )
)


;; One-based. There's opportunity for further cleaning of this formula: (Use A035327)
(definec (A096116v2 n)
  (cond ((= 1 n) 1)
        ((pow2? (- n 1)) (+ 2 (A000523 (- n 1))))
        (else (+ 2 (A000523 (- n 1)) (A096116v2 (+ 1 (- (expt 2 (A000523 (- n 1))) (A053645 (- n 1)))))))
  )
)




;; ----------------------

;; a(n)=a(n-1)+a(m), where m=2^(p+1)+2-n, and 2^p<n-1<=2^(p+1), for n >= 4.
(definec (A050049 n)
  (cond ((= 1 n) 1)
        (else (+ (A050049 (- n 1)) (A050049 (+ 1 (A035327 (- n 2))))))
  )
)

;; Murthy has supplied A096118 as a duplicate of Kimberling's A050029:
(definec (A050029 n)
  (cond ((< n 3) 1)
        (else (+ (A050029 (- n 1)) (A050029 (+ 1 (A035327 (- n 2))))))
  )
)

;; Zero-based: Compute up to n=14:
(define (A096119 n) (A050029 (+ 1 (expt 2 n))))

;; Murthy's A096120 is probably an erroneous duplicate of Kimberling's A050030 !

(definec (A050030 n)
  (cond ((< n 3) 1)
        (else (+ (A050030 (- n 1)) (A050030 (+ 1 (A053645 (- n 2))))))
  )
)

(define (A105996 n) (A050030 (+ 1 (expt 2 n))))

(defineperm1 (A076105 n)
   (cond ((< n 2) n)
         ((zero? (A025581 (- n 1))) ;; n is triangular, on the trailing edge
           (let* ((rowindex (A002024 n))
                  (rowsumsofar (add A076105 (+ (A000217 (- rowindex 1)) 1)
                                            (- (A000217 rowindex) 1)
                               )
                  )
                  (prevrowsum (A076103 (- rowindex 1)))
                 )
              (let loop ((i (modulo (- rowsumsofar) prevrowsum)))
                 (if (and (not (zero? i)) (not-lte? (A122154 i) (- n 1)))
                     i
                     (loop (+ i prevrowsum))
                 )
              )
           )
         )
         (else (luuA076105 (- n 1)))
   )
)


(define (A076101 n) (A076105 (+ 1 (A000217 (- n 1)))))

(define (A076102 n) (A076105 (A000217 n)))

(define (A076103 n) (add A076105 (+ (A000217 (- n 1)) 1) (A000217 n)))

(define (A076104 n) (/ (A076103 (+ n 1)) (A076103 n)))

(define (A122154 n) (A076105 (- n))) ;; Take values from invcache. Must be computed after A076105

;; luu stands for "least un-used". We don't want to submit these to OEIS, no inherent value!
(definec (luuA076105 n) ;; Least natural number which does not occur in A076105(1..n).
   (if (< n 2) (+ n 1) ;; A094280 begins as 1,2,3,4,5,6,...
       (let ((prev  (luuA076105 (- n 1))))
          (cond ((not (= (A076105 n) prev)) prev)
                (else (let loop ((i (+ 1 prev)))
                           (cond ((not-lte? (A122154 i) n) i)
                                 (else (loop (+ 1 i)))
                           )
                      )
                )
          )
       )
   )
)


;; 1,2,3,4,5,6,7,8,9,21,10,11,12,13,44,14,15,16,17,18,100,19,20,22,23,24,
(defineperm1 (A094280 n)
   (cond ((< n 2) n)
         ((zero? (A025581 (- n 1))) ;; n is triangular, on the trailing edge
           (let* ((rowindex (A002024 n))
                  (rowsumsofar (add A094280 (+ (A000217 (- rowindex 1)) 1)
                                            (- (A000217 rowindex) 1)
                               )
                  )
                  (prevrowsum (A094283 (- rowindex 1)))
                 )
;;            (format #t "rowindex=~a, rowsumsofar=~a, prevrowsum=~a\n"
;;                        rowindex rowsumsofar prevrowsum
;;            )
;; There are more intelligent ways to do this: (... ;-)
;;            (let loop ((i (luuA094280 (- n 1))))
;;               (if (and (> (+ rowsumsofar i) prevrowsum) ;; Differs from A076105.
;;                           (zero? (modulo (+ rowsumsofar i) prevrowsum))
;;                   )
;;                   i
;;                   (loop (+ i 1))
;;               )
;;            )
              (let loop ((i (modulo (- rowsumsofar) prevrowsum)))
                 (if (and (> (+ i rowsumsofar) prevrowsum) (not-lte? (A122156 i) (- n 1)))
                     i
                     (loop (+ i prevrowsum))
                 )
              )
           )
         )
         (else (luuA094280 (- n 1)))
   )
)


(define (A094281 n) (A094280 (+ 1 (A000217 (- n 1)))))

(define (A094282 n) (A094280 (A000217 n)))

(definec (A094283 n) (add A094280 (+ (A000217 (- n 1)) 1) (A000217 n)))

(define (A094284 n) (/ (A094283 (+ n 1)) (A094283 n)))

(define (A122156 n) (A094280 (- n))) ;; Take values from invcache. Must be computed after A094280

(definec (luuA094280 n) ;; Least natural number which does not occur in A094280(1..n).
   (if (< n 2) (+ n 1) ;; A094280 begins as 1,2,3,4,5,6,...
       (let ((prev  (luuA094280 (- n 1))))
          (cond ((not (= (A094280 n) prev)) prev)
                (else (let loop ((i (+ 1 prev)))
                           (cond ((not-lte? (A122156 i) n) i)
                                 (else (loop (+ 1 i)))
                           )
                      )
                )
          )
       )
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CatTrianglDirect := (r,m) -> `if`((m < 0),0,((r-m+1)*(r+m)!)/(r! * m! * (r+1)));
;; A009766 gives also:
;; a(n,m) = C(n+m,n)*(n-m+1)/(n+1), n >= m >= 0.

(define (A009766tr r m)
   (if (or (> m r) (< m 0))
       0 ;; Maybe we should raise an error instead?!
       (/ (* (1+ (- r m)) (! (+ r m)))
          (* (! r) (! m) (1+ r))
       )
   )
)

(define (A009766 n) (A009766tr (A003056 n) (A002262 n)))

;;
;; (definec (A0similarv2 n) (convolve (lambda (n) (if (zero? n) 1 (A000045 n))) A000108 n))
;; 
;; (definec (A0similarv3 n)
;;    (if (zero? n)
;;        1
;;        (convolve (compose-funs A000045 1+) (compose-funs A000108 1+) (- n 1))
;;    )
;; )
;; 

;; (map a0similarv2 (iota0 20))
;; (1 2 4 10 26 73 217 677 2192 7302 24860 86086 302162 1072362 3840952 13865259 50389441 184204645 676875116 2498698161 9262054997)
;; (map a0similarv3 (iota0 20))
;; (1 1 3 9 26 77 235 741 2406 8009 27211 94006 329229 1166135 4169804 15030784 54558258 199233832 731430790 2697927812 9993479022)


(define A014137 (PARTIALSUMS 0 0 A000108))

(define A014138 (PARTIALSUMS 1 1 A000108))

(definec (A014143 n) (if (zero? n) 1 (+ (A014138 (+ n 1)) (A014143 (- n 1))))) ;; Partial sums of A014138.
;; Starting offset 0: 1, 4, 12, 34, 98, 294, 919,

;; (definec (A014137 n) ;; Partial sums of A000108: 1,2,4,9,23,65,197,...
;;    (if (zero? n)
;;        1
;;        (+ (A014137 (-1+ n)) (A000108 n))
;;    )
;; )
;; 
;; This for the old starting offset:
;; (define (A014138 n) (-1+ (A014137 (1+ n))))





;; (map A072643 (iota0 23)) ;; n occurs (A000108 n) times.
;; --> (0 1 2 2 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5)

;; Was defined with definec, but without reason:

(define (A072643 n) (first_pos_with_funs_val_gte A014137 (1+ n))) ;; Was ranks_w/2

(define (A000245 n) (- (A000108 (1+ n)) (A000108 n)))

;; Most of these are "unnecessary", except A081288, A081291 and A081292.

(definec (A081288 n) (first_pos_with_funs_val_gte A000108 (1+ n)))

(define (A244160 n) (if (zero? n) n (- (A081288 n) 1))) ;; A more rational version of the above.

(define (A244215 n) (- (A244160 n) (A244160 (- n (A081290 n)))))
(define A244216 (ZERO-POS 0 0 A244215))
(define A244216v2 (MATCHING-POS 1 1 (lambda (k) (>= k (* 2 (A081290 k))))))
;; (define A244216v3 (MATCHING-POS 1 1 (lambda (k) (< 1 (car (A014418raw k)))))) ;; Yes.

(define A244217 (NONZERO-POS 1 1 A244215))
;; (define A244217v2 (MATCHING-POS 1 1 (lambda (k) (= 1 (car (A014418raw k))))))


(define (A081289 n) (if (zero? n) n (+ (A014137 (-1+ (A081288 n))) (A000108 (-1+ (A081288 n))))))
(define (A081290 n) (first_pos_with_funs_val_gte A081289 (A081289 n)))

(define (A081290v2 n) (if (zero? n) n (A000108 (-1+ (A081288 n)))))

(definec (A014420 n) (if (zero? n) n (+ 1 (A014420 (- n (A081290 n))))))

(define (A081291 n) (if (zero? n) n (+ (A014137 (-1+ (A081288 n))) n)))
(define (A081291v2 n) (+ (A081289 n) (- n (A081290 n))))

(define (A130380 n) (floor->exact (/ (+ (A000108 n) 1) 2)))

;; In gatomain.scm: (define (A081292 n) (A014486 (A081291 n)))

(define (A081293 n) (+ (A014137 n) (A000108 n)))

(define (A081289v2 n) (if (zero? n) n (A081293 (-1+ (A081288 n)))))


;; A081291 = 0 3 6 7 8 14 15 16 17 18 19 20 21 22 37 38 39 40 41 42 43

;; More. 82852 --- 82861 reserved.

(define (A082852 n) (if (zero? n) 0 (A014137 (-1+ (A072643 n)))))
(define (A082853 n) (- n (A082852 n)))
(define (A082854 n) (1+ (A082853 n)))
;; A082852 = (0 1 2 2 4 4 4 4 4 9 9 9 9 9 9 9 9 9 9 9 9 9 9 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65)
;; A082853 = (0 0 0 1 0 1 2 3 4 0 1 2 3 4 5 6 7 8 9 10 11 12 13 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55)

;; A014137(n) occurs A000245(n) times.
(define (A082855 n) (if (< n 2) n (A014137 (-1+ (A081288 (-1+ n))))))

(define (A081291v3 n) (if (zero? n) n (+ (A082855 (1+ n)) n)))

;; (define A001477v3 (compose-funs A082853 A001477 A081291))
;; (define A072619v2 (compose-funs A082853 A072088 A081291))
;; (define A038776v2 (compose-funs A082854 A057117 A081291))
;;
;; (define (A072088v2 n) (+ (A072619 (A082853 n)) (A082852 n)))

;; A081291 A081289  A081288   A081290
;;  0   0   0        0         0
;; 
;;  1   3   3  0     2         1
;; 
;;  2   6   6  0     3         2
;;  3   7   6  1     3         2
;;  4   8   6  2     3         2
;; 
;;  5  14  14  0     4         5
;;  6  15  14  1     4         5
;;  7  16  14  2     4         5
;;  8  17  14  3     4
;;  9  18  14  4     4   First pos i where (A081289(i) >= A081289(9)) = 5.
;; 10  19  14  5     4
;; 11  20  14  6     4
;; 12  21  14  7     4
;; 13  22  14  8     4
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note that: A153141(14)=10,  A006068(14)=11

(define (A153141 n)
  (if (< n 2)
      n
      (let loop ((maskbit (A072376 n)) (z n))
         (cond ((zero? maskbit) z)
               ((not (zero? (modulo (floor->exact (/ n maskbit)) 2)))
                   (- z maskbit) ;; Found first 1-bit, complement it, and return
               )
               (else (loop (floor->exact (/ maskbit 2)) (+ z maskbit)))
         )
      )
  )
)

;; Note that A153142(10)=14, while A003188(10)=15

(define (A153142 n)
  (if (< n 2)
      n
      (let loop ((maskbit (A072376 n)) (z n))
         (cond ((zero? maskbit) z)
               ((zero? (modulo (floor->exact (/ n maskbit)) 2))
                   (+ z maskbit) ;; Found first 0-bit, complement it, and return
               )
               (else (loop (floor->exact (/ maskbit 2)) (- z maskbit)))
         )
      )
  )
)

(define A153150 (compose-funs A059893 A056539 A059893))

;; Abridged/rotated binary decrementing!
(define (A153151 n) (cond ((< n 2) n) ((pow2? n) (- (* 2 n) 1)) (else (- n 1))))

;; Abridged/rotated binary incrementing!
(define (A153152 n) (cond ((< n 2) n) ((pow2? (1+ n)) (/ (1+ n) 2)) (else (1+ n))))

(define A153153 (compose-funs A059893 A003188 A059893))
(define A153154 (compose-funs A059893 A006068 A059893))


(define (A153151v2 n)
  (if (< n 2)
      n
      (let loop ((uplim (A053644 n)) (maskbit 1) (z n))
         (cond ((= uplim maskbit) z)
               ((not (zero? (modulo (floor->exact (/ n maskbit)) 2)))
                   (- z maskbit) ;; Found first 1-bit, complement it, and return
               )
               (else (loop uplim (* maskbit 2) (+ z maskbit)))
         )
      )
  )
)

(define (A153152v2 n)
  (if (< n 2)
      n
      (let loop ((uplim (A053644 n)) (maskbit 1) (z n))
         (cond ((= uplim maskbit) z)
               ((not (zero? (modulo (floor->exact (/ n maskbit)) 2)))
                   (+ z maskbit) ;; Found first 0-bit, complement it, and return
               )
               (else (loop uplim (* maskbit 2) (- z maskbit)))
         )
      )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New ones, Jan 10 2009 onward:

;; From:
;; Bondarenko, Grigorchuk, Kravchenko, Muntyan, Nekrashevych, Savchuk, Sunic,
;; Classification of groups generated by 3-state automata over
;; a 2-letter alphabet, pp. 8--9 & 103.
;; http://arxiv.org/abs/0803.3555

;; (find-matching-anum (catfun1 (psi Alamplighter1a)) 2055) --> "A122302"

;; (find-matching-anum (catfun1 (psi Alamplighter2a)) 2055) --> "A122301"
;;
;; (compose-funs A059893 Alamplighter1a A059893)
;;
;; (compose-funs A059893 Alamplighter2a A059893)
;; not present.

;; Here are the 50 A-numbers you requested: A154434 --- A154483.

;;
;; First one on page 104, inverse of the third one:
;; Also A054429-conjugate of the fourth one:
;; (compose-funs A054429 Alamplighter1a A054429) = Alamplighter4a
;; (A054429 A003188 A054429)
(define (A154436 n) ;; Alamplighter1a
  (if (< n 2)
      n
      (let loop ((maskbit (A072376 n)) (state 1) (z 1))
        (if (zero? maskbit)
            z
            (let ((dombit (modulo (floor->exact (/ n maskbit)) 2)))
              (cond ((= state dombit)
                        (loop (floor->exact (/ maskbit 2))
                              (- 1 state)
                              (+ z z (modulo (- state dombit) 2))
                        )
                    )
                    (else
                        (loop (floor->exact (/ maskbit 2))
                              state
                              (+ z z (modulo (- state dombit) 2))
                        )
                    )
              )
            )
        )
       
    )
  )
)


;; Third one on the page 104, inverse of the above one:
;; A054429-conjugate of the next one
;; (compose-funs A054429 Alamplighter3a A054429) = Alamplighter2a
;; (A054429 A006068 A054429)
(define (A154435 n) ;; Alamplighter2a
  (if (< n 2)
      n
      (let loop ((maskbit (A072376 n)) (state 1) (z 1))
        (if (zero? maskbit)
            z
            (let ((dombit (modulo (floor->exact (/ n maskbit)) 2)))
              (cond ((= 0 dombit)
                        (loop (floor->exact (/ maskbit 2))
                              (- 1 state)
                              (+ z z (modulo (- state dombit) 2))
                        )
                    )
                    (else
                        (loop (floor->exact (/ maskbit 2))
                              state
                              (+ z z (modulo (- state dombit) 2))
                        )
                    )
              )
            )
        )
       
    )
  )
)


(define A154438 (compose-funs A059893 A154436 A059893))
;; (A054429 A153153 A054429)

(define A154437 (compose-funs A059893 A154435 A059893))
;; (A054429 A153154 A054429)


;; Second one on the page 104. = A006068
(define (Alamplighter3a n)
  (if (< n 2)
      n
      (let loop ((maskbit (A072376 n)) (state 1) (z 1))
        (if (zero? maskbit)
            z
            (let ((dombit (modulo (floor->exact (/ n maskbit)) 2)))
              (cond ((= 1 dombit)
                        (loop (floor->exact (/ maskbit 2))
                              (- 1 state)
                              (+ z z (modulo (- state dombit) 2))
                        )
                    )
                    (else
                        (loop (floor->exact (/ maskbit 2))
                              state
                              (+ z z (modulo (- state dombit) 2))
                        )
                    )
              )
            )
        )
       
    )
  )
)

;; Fourth one on page 104, inverse of the second one: = A003188
;; It's clear that this is (n XOR (n>>1)).
(define (Alamplighter4a n)
  (if (< n 2)
      n
      (let loop ((maskbit (A072376 n)) (state 1) (z 1))
        (if (zero? maskbit)
            z
            (let ((dombit (modulo (floor->exact (/ n maskbit)) 2)))
              (cond ((not (= state dombit))
                        (loop (floor->exact (/ maskbit 2))
                              (- 1 state)
                              (+ z z (modulo (- state dombit) 2))
                        )
                    )
                    (else
                        (loop (floor->exact (/ maskbit 2))
                              state
                              (+ z z (modulo (- state dombit) 2))
                        )
                    )
              )
            )
        )
       
    )
  )
)


;;  A154439 =  (compose-funs A054429 A154443 A054429)

(defineperm1 (A154439 n) ;; Abasilica1a
  (if (< n 2)
      n
      (let loop ((maskbit (A072376 n)) (p 0) (z n))
         (cond ((zero? maskbit) z)
               ((zero? (modulo (floor->exact (/ n maskbit)) 2))
                   (+ z (* p maskbit)) ;; Found first 0-bit, complement it
;;                              if at even distance from msb, and return
               )
               (else (loop (floor->exact (/ maskbit 2))
                           (- 1 p)
                           (- z (* p maskbit))
                     )
               )
         )
      )
  )
)

;;  A154440 = (compose-funs A054429 A154446 A054429)
(define (A154440 n) (A154439 (- n))) ;; Abasilica1aINV


;;  A154441 =  (compose-funs A054429 A154445 A054429)
(defineperm1 (A154441 n) ;; Abasilica1b
  (if (< n 2)
      n
      (let loop ((maskbit (A072376 n)) (p 1) (z n))
         (cond ((zero? maskbit) z)
               ((zero? (modulo (floor->exact (/ n maskbit)) 2))
                   (+ z (* p maskbit)) ;; Found first 0-bit, complement it
;;                                if at odd distance from msb, and return
               )
               (else (loop (floor->exact (/ maskbit 2))
                           (- 1 p)
                           (- z (* p maskbit))
                     )
               )
         )
      )
  )
)

;;  A154442 =  (compose-funs A054429 A154446 A054429)
(define (A154442 n) (A154441 (- n))) ;; Abasilica1bINV


(defineperm1 (A154443 n) ;; Abasilica2a
  (if (< n 2)
      n
      (let loop ((maskbit (A072376 n)) (p 0) (z n))
         (cond ((zero? maskbit) z)
               ((not (zero? (modulo (floor->exact (/ n maskbit)) 2)))
                   (- z (* p maskbit)) ;; Found first 1-bit, complement it
;;                                        if at odd distance, and return
               )
               (else (loop (floor->exact (/ maskbit 2))
                           (- 1 p)
                           (+ z (* p maskbit))
                     )
               )
         )
      )
  )
)

(define (A154444 n) (A154443 (- n))) ;; Abasilica2aINV



(defineperm1 (A154445 n) ;; Abasilica2b
  (if (< n 2)
      n
      (let loop ((maskbit (A072376 n)) (p 1) (z n))
         (cond ((zero? maskbit) z)
               ((not (zero? (modulo (floor->exact (/ n maskbit)) 2)))
                   (- z (* p maskbit)) ;; Found first 1-bit, complement it
;;                                        if at odd distance, and return
               )
               (else (loop (floor->exact (/ maskbit 2))
                           (- 1 p)
                           (+ z (* p maskbit))
                     )
               )
         )
      )
  )
)

(define (A154446 n) (A154445 (- n))) ;;  Abasilica2bINV


;; These are inverses of each other:
(define (A154448 n)
  (if (< n 2)
      n
      (let loop ((maskbit (A072376 n)) (p 1) (z n))
         (cond ((zero? maskbit) z)
               ((= p (modulo (floor->exact (/ n maskbit)) 2))
                   (+ z (* (- 1 (* 2 p)) maskbit)) ;; Found first bit
;;                   equal to p, complement it if at odd distance, and return
               )
               (else (loop (floor->exact (/ maskbit 2))
                           (- 1 p)
                           (- z  (* (- 1 (* 2 p)) maskbit))
                     )
               )
         )
      )
  )
)


(define (A154447 n)
  (if (< n 2)
      n
      (let loop ((maskbit (A072376 n)) (p 0) (z n))
         (cond ((zero? maskbit) z)
               ((= p (modulo (floor->exact (/ n maskbit)) 2))
                   (+ z (* (- 1 (* 2 p)) maskbit)) ;; Found first bit
;;                   equal to p, complement it if at odd distance, and return
               )
               (else (loop (floor->exact (/ maskbit 2))
                           (- 1 p)
                           (- z  (* (- 1 (* 2 p)) maskbit))
                     )
               )
         )
      )
  )
)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lei Zhou's sequences:
;; Needs either GF2Xfuns from this same directory
;;  or (load "c:\\slib\\mitscheme.init")
;; and (require 'factor)
;; (for prime? function, and factor)

;; (Offset=4)
;; %S A103151 1,1,2,1,3,2,2,2,3,3,4,2,4,2,4,4,4,4,5,3,4,6,5,3,6,3,3,6,6,5,7,
; Number of decompositions of 2n+1 into ordered sums of one odd prime and two times another odd prime.
;; a(n) = count of all eligible p1 for 2n+1 = 2*p1 + p2; where p1 and p2 are prime numbers.

(definec (A103151 n)
   (let loop ((i 2) (z 0))
      (let ((p1 (A000040 i)))
         (cond ((>= p1 n) z)
               ((prime? (+ 1 (* 2 (- n p1)))) (loop (+ 1 i) (+ 1 z)))
               (else (loop (+ 1 i) z))
         )
      )
   )
)

;; Another variant:

(definec (A103151v2 n)
  (let ((o (+ (* 2 n) 1)))
   (let loop ((i 2) (z 0))
      (let ((p2 (A000040 i)))
         (cond ((> p2 (- o 6)) z)
               ((prime? (/ (- o p2) 2)) (loop (+ 1 i) (+ 1 z)))
               (else (loop (+ 1 i) z))
         )
      )
   )
  )
)


;; %N A103152 Smallest odd number which is the sum of an odd prime and twice another odd prime 
;; (can be equal to the first) in exactly n ways.

(definec (A103152 n) (+ 1 (* 2 (first-n-where-fun_n-is-i1 A103151 n))))


;; %N A103507 Index of the smallest prime in decomposition of 2n+1 into sum of twice of this odd 
;; prime and another odd prime.  (and 0 if no such prime exists).
;; %F A103507 PrimePi[A103153]

(definec (A103507 n)
   (let loop ((i 2))
      (let ((p1 (A000040 i)))
         (cond ((>= p1 n) 0)
               ((prime? (+ 1 (* 2 (- n p1)))) i)
               (else (loop (+ 1 i)))
         )
      )
   )
)


; %N A103153 Smallest prime in decomposition of 2n+1 into sum of twice of this prime and another prime.
; (and 0 if there are cases where the conjecture given in A103151 does not hold.)
; (and 0 if no such prime exists)

(define (A103153 n) (let ((ind (A103507 n))) (if (zero? ind) 0 (A000040 ind))))


(definec (A103509 n)
  (let ((o (+ (* 2 n) 1)))
   (let loop ((i 2))
      (let ((p2 (A000040 i)))
         (cond ((> p2 (- o 6)) 0)
               ((prime? (/ (- o p2) 2)) i)
               (else (loop (+ 1 i)))
         )
      )
   )
  )
)

;; %N A103509 Index of the smallest prime in decomposition of 2n+1 into sum of this odd prime 
;; and twice of another odd prime. (and 0 if no such prime exists).


(define (A103506 n) (let ((ind (A103509 n))) (if (zero? ind) 0 (A000040 ind))))


;; Minimum prime p2, such that |A000040(n)-p2| is of the form 2^k.
(definec (A130970 n)
  (let ((p1 (A000040 n)))
   (let loop ((i 1))
     (cond ((pow2? (abs (- p1 (A000040 i)))) (A000040 i))
           (else (loop (+ i 1)))
     )
   )
  )
)

;; Minimum prime p2, such that either |A000040(n)-p2| or A000040(n)+p2 is of the form 2^k.
(definec (A130971 n)
  (let ((p1 (A000040 n)))
   (let loop ((i 1) (p2 2))
     (cond ((pow2? (abs (- p1 p2))) p2)
           ((pow2? (+ p1 p2)) p2)
           (else (loop (+ i 1) (A000040 (+ i 1))))
     )
   )
  )
)

(define A130972 (RECORD-POS 1 1 A130971))

(define A103150 (RECORD-VALS 1 1 A130971))

(define (A103149 n)
  (if (= n 1) 3
      (A000040 (A130972 n))
  )
)

(define A103149v2 (compose-funs A000040 (RECORD-POS 2 2 A130971) 1+))

(definec (A103508 n) (+ 1 (* 2 (first-n-where-fun_n-is-i1 A103507 (+ 1 n)))))

(definec (A103510 n) (+ 1 (* 2 (first-n-where-fun_n-is-i1 A103509 (+ 1 n)))))

;; A103149 & A103150: starting offset changed to o=1.


;; Here are the 10 A-numbers you requested: A140259 --- A140268.

(definec (A140259 n)
   (cond ((zero? n) 3)
         ((= 1 (A140259 (- n 1))) (A002264 (+ n 11)))
         ((not (zero? (modulo (- n 1) 3))) (- (A140259 (- n 1)) 1))
         (else (A140259 (- n 1)))
   )
)

(define A140260 (MATCHING-POS 0 0 (lambda (i) (= (A140259 i) (A002264 (+ i 11))))))

(define (A140261 n) (A140259 (A140260 n)))

(define (A140262 n) (modulo (A140260 n) 9))


(define (A117966 n) ;; 0,1,-1,3,4,2,-3,-2,-4,9,10,8,12,...
  (let loop ((z 0) (i 0) (n n))
    (if (zero? n)
        z
        (loop (+ z (* (expt 3 i) (if (= 2 (modulo n 3)) -1 (modulo n 3))))
              (1+ i)
              (floor->exact (/ n 3))
        )
    )
  )
)


;; When applied to natural numbers >= 1, gives:
;; i.e. a bisection of the next sequence.

;; Inverse of A117966:
;; (map A117967 (map A117966 (iota0 729))) = (iota0 729)

;; Gives a non-negative integer, which when converted to ternary
;; and interpreted as _balanced ternary_, is equal to argument z.
;; When restricted to non-negative integers, gives A117967.
(define (A117967 z)
   (cond ((zero? z) 0)
         ((negative? z) (A004488 (A117967 (- z))))
         (else ;; z is positive.
            (let* ((lp3 (expt 3 (A062153 z))) ;; largest power of three of z.
                   (np3 (* 3 lp3)) ;; Next Power of Three.
                  )
                (if (< (* 2 z) np3) ;; z less than np3/2
                    (+ lp3 (A117967 (- z lp3)))
                    (+ np3 (A117967 (- z np3)))
                )
            )
         )
   )
)

;; Based on Frank's:
;; a(0) = 0, a(3n) = 3a(n), a(3n+1) = 3a(n)+1, a(3n-1) = 3a(n)+2.
(define (A117967v2 z)
   (cond ((zero? z) 0)
         ((negative? z) (A004488 (A117967v2 (- z))))
         ((zero? (modulo z 3)) (* 3 (A117967v2 (/ z 3))))
         ((= 1 (modulo z 3)) (+ (* 3 (A117967v2 (/ (- z 1) 3))) 1))
         (else (+ (* 3 (A117967v2 (/ (+ z 1) 3))) 2)) ;; z = 2 (= -1) modulo 3.
   )
)



(define (A117968 n) (A117967 (- n)))
;; (define (A117968v2 n) (A004488 (A117967 n)))


(define (A140263 n) (A117967 (A001057 n))) ;; Zero-based.
(define (A140264 n) (- (A140266 (+ 1 n)) 1))

(define (A140265 n) (+ 1 (A140263 (- n 1)))) ;; One-based.
(define (A140266 n) (Z->N (A117966 (- n 1))))


(define (A140267 n) (A007089 (A117967 n))) ;;  1,12,10,11,122,120,121,102,100

;; 2,21,20,22,211,210,212,201,200,202,221,220,222,2111,2110,2112,2101,2100,...
(define (A140268 n) (A007089 (A117968 n)))

;; Added 2012-03-03:

;; Essentially rewrites in binary expansion of n each 0 -> 01, 1X -> 1(rewrite X)0,
;; where X is the maximal suffix after the 1-bit.
(definec (A071162 n)
   (let loop ((n n) (s 0) (i 1))
        (cond ((zero? n) s)
              ((even? n) (loop (/ n 2) (+ s i) (* i 4)))
              (else (loop (/ (- n 1) 2) (* 2 (+ s i)) (* i 4)))
        )
   )
)


;; Some kind of "inverse" of A071162:
;; Rewrite from the msb end, 1 -> 1, 0+1 -> 0, stop when only zeros left.
(definec (A209859 n)
   (let loop ((n n) (s 0) (i (A053644 n)))
        (cond ((zero? n) s)
              ((> i n) ;; Found zero-bit (from the left)
                (if (> (/ i 2) n) ;; Is also the next bit to the right zero?
                    (loop n s (/ i 2)) ;; Then keep the s still same.
                    (loop (- n (/ i 2)) (* 2 s) (/ i 4)) ;; Otherwise skip the 1, and double the s.
                )
              )
              (else (loop (- n i) (+ (* 2 s) 1) (/ i 2)))
        )
   )
)


(define A209639 (compose-funs A209859 A005408))

;; As A014486 but also bit pattern 10+11 is forbidden.
;; (That is, if there are any adjacent 1's, they must all be located
;;  as part of the leftmost prefix of n.)
(define (member_of_A209641? n) ;; and also of A209642
   (let loop ((n n) (lev 0))
        (cond ((zero? n) (zero? lev))
              ((< lev 0) #f)
              ((even? n) (loop (/ n 2) (+ lev 1)))
;; So now we now we have found 1-bit.
              ((and (odd? (/ (-1+ n) 2)) ;; Is the next bit (to the left) also 1?
                    (even? (/ (-1+ (/ (-1+ n) 2)) 2)) ;; and the next after that is 0?
                    (not (zero? (/ (-1+ (/ (-1+ n) 2)) 2))) ;; but not past the msb yet?
               ) #f ;; Our forbidden pattern found.
              )
              (else (loop (/ (- n 1) 2) (- lev 1)))
        )
   )
)

(define *MAX-CACHE-SIZE-FOR-DEFINEC* (expt 2 23)) ;; (expt 2 20) ;; 290512 ;; Was 131072

(define A209641 (MATCHING-POS 0 0 member_of_A209641?))

;; A209635-A209644 are now reserved for your use.
;; A209859-A209868 

;; Modified from the CatalanRank in gatorank.scm:
;; Hypothesis: If we have w upward slopes, of which u start a leftmost upward prefix,
;; then we have altogether C(w-1,u) = C(w-1,w-1-u) ways to arrange the other w-u upward slopes
;; in such a way that no forbidden
;;           /
;; pattern: /
;; occurs anywhere.
;; Proof: from the Comments of A007318:
;; C(n+k-1,n-1) is the number of ways of placing k indistinguishable balls
;; into n boxes (the "bars and stars" argument - see Feller).
;; I.e. as k=w-u and n=u, we have C(w-1,u-1)

;;
;; After the the first \ (from left), when we first encounter a first /\
;; at level lev (= [1,u]) it means there are C(lev-1,w-u-1) ways to arrange
;; lexicographically less paths (with downward slope \ instead of upward slope /)
;; "between" (zero is lev = 1).

;;
;; w=4, u=1, C(3,0) = 1
;; /\/\/\/\
;;
;;
;; w=4, u=2, C(3,1) = 3
;;  /\        /\/\      /\/\/\
;; /  \/\/\  /    \/\  /      \
;; 0+2       1+1       2+0
;;
;;
;; w=4, u=3, C(3,2) = 3
;; (3 1) = 3
;;
;;   /\        /\        /\/\
;;  /  \      /  \/\    /    \
;; /    \/\  /      \  /      \
;;
;;
;; (2 1) + (2 2) = (3 2) = 3 = (3 1) = 3 (ways to insert + among " 1 1 ").
;;
;; 2^(4-1) = 8.
;;
;; (4 0) = 1
;; w=4, u=4, C(3,3) = 1
;;
;;    /\
;;   /  \
;;  /    \
;; /      \
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2^(4-1) = 8.
;;
;; (5 0) = (4 0) = 1
;;
;;     /\
;;    /  \
;;   /    \
;;  /      \
;; /        \
;;
;; (4 1) = 4
;;
;;     /\          /\          /\          /\/\
;;    /  \        /  \        /  \/\      /    \
;;   /    \      /    \/\    /      \    /      \
;;  /      \/\  /        \  /        \  /        \
;;
;;
;; (3 1) + (3 2) = (4 2) = 6
;; That is, 3 cases where the remaining /\'s come at the same level,
;; and 3 cases where they come at different levels:
;;
;;    /\          /\          /\          /\/\        /\/\        /\/\/\
;;   /  \        /  \/\      /  \/\/\    /    \      /    \/\    /      \
;;  /    \/\/\  /      \/\  /        \  /      \/\  /        \  /        \
;;  0+0+2       0+1+1       0+2+0       1+0+1       1+1+0       2+0+0
;;
;;
;; (4 1) = 4 (ways to insert + among " 1 1 1 ")= (4 3)
;;                     
;;   /\          /\/\        /\/\/\      /\/\/\/\
;;  /  \/\/\/\  /    \/\/\  /      \/\  /        \
;;  0+3         1+2         2+1         3+0
;;
;;  (1 1) = 1 = (4 4)
;;
;;  /\/\/\/\/\
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2^(5-1) = 16.
;;
;; (6 0) = (5 0) = 1
;; 
;;      /\
;;     /  \
;;    /    \
;;   /      \
;;  /        \
;; /          \
;;
;; (5 1) = 5
;; 
;;      /\            /\            /\             /\            /\/\
;;     /  \          /  \          /  \           /  \/\        /    \
;;    /    \        /    \        /    \/\       /      \      /      \
;;   /      \      /      \/\    /        \     /        \    /        \
;;  /        \/\  /          \  /          \   /          \  /          \
;;
;;
;; (4 1) + (4 2) = 4 + 6 = 10 = (5 2)
;;
;;     /\            /\             /\            /\            /\
;;    /  \          /  \           /  \          /  \/\        /  \/\
;;   /    \        /    \/\       /    \/\/\    /      \      /      \/\
;;  /      \/\/\  /        \/\   /          \  /        \/\  /          \
;;                       ^                ^          ^                ^
;;                    (sr += 1)     (ssr += 1)    (sr += 3)     (ssr += 1)
;;                                                  (3 1) ( 0+2, 1+1, 2+0 )
;;                                                  = (2 1) + (2 2) = (3 2)
;;
;;
;;     /\            /\/\           /\/\          /\/\          /\/\/\
;;    /  \/\/\      /    \         /    \        /    \/\      /      \
;;   /        \    /      \       /      \/\    /        \    /        \
;;  /          \  /        \/\   /          \  /          \  /          \
;;          ^          ^                  ^            ^            ^
;;     (ssr += 2)  (sr += 6)        (ssr += 1)   (ssr += 2)   (ssr += 3)
;;                    (4 2)
;;                   (from the previous the peak /\ can be cut off, and then we have
;;                    recursively regressed to one smaller case).
;;
;;
;; (3 1) + (3 3) + 2*(3 2) = 3 + 1 + 6 = 10 = (5 2) = (5 3) = (4 2) + (4 1)
;; That is, 3 cases where the remaining /\'s come at the same level,
;; and 1 case where they all go to the different levels,
;; and 6 cases where they are divided to two different levels:
;; 
;;   0+0+3         0+1+2         0+2+1         0+3+0         1+0+2
;;     /\            /\            /\            /\            /\/\
;;    /  \          /  \/\        /  \/\/\      /  \/\/\/\    /    \
;;   /    \/\/\/\  /      \/\/\  /        \/\  /          \  /      \/\/\
;;                   +1 ^            +1 ^            +1 ^     +4 ^ = (4 1) ( 0+3, 1+2, 2+1, 3+0 )
;; 
;;   1+1+1         1+2+0         2+0+1         2+1+0         3+0+0
;;     /\/\          /\/\          /\/\/\        /\/\/\        /\/\/\/\
;;    /    \/\      /    \/\/\    /      \      /      \/\    /        \
;;   /        \/\  /          \  /        \/\  /          \  /          \
;;       +1 ^            +1 ^        4 3              +1^        4 3 2
;; 
;;
;; (5 1) = 5 (ways to insert + among " 1 1 1 1 ") = (5 4).
;;                      
;;    /\            /\/\          /\/\/\        /\/\/\/\      /\/\/\/\/\
;;   /  \/\/\/\/\  /    \/\/\/\  /      \/\/\  /        \/\  /          \
;;   0+4           1+3           2+2           3+1           4+0
;;
;;
;;  (1 1) = 1 = (5 5)
;;   /\/\/\/\/\/\
;;

;; Hypothesis: If we have w upward slopes, of which u start a leftmost upward prefix,
;; C(w-1,u-1)
;; then we have altogether C(w-1,u-1) = C(w-1,w-1-(u-1)) = C(w-1,w-u) ways to arrange the other w-u upward slopes
;; in such a way that no forbidden
;;           /
;; pattern: /
;; occurs anywhere.
;; Proof: from the Comments of A007318:
;; C(n+k-1,n-1) is the number of ways of placing k indistinguishable balls
;; into n boxes (the "bars and stars" argument - see Feller).
;; My note: C(n+k-1,n-1) = C(n+k-1,(n+k-1)-(n-1)) = C(n+k-1,k)
;; Thus, when we set w = n+k and u=k we get C(n+k,k) from above.
;;

;;
;; After the the first \ (from left), when we first encounter a first /\
;; at level lev (= [1,u]) it means there are C(lev-1,w-u-1) ways to arrange
;; lexicographically less paths (with downward slope \ instead of upward slope /)
;; "between" (zero is lev = 1).
;;
;; Seems to work now...
;;
(define (A209640 n) ;; The totally balanced binary expansion with restrictions
  (if (or (zero? n) (not (member_of_A209641? n))) ;; Check the restrictions.
      0
      (let* ((w (/ (binwidth n) 2)))
        (let loop ((rank 0)
                   (row 1)
                   (u (- w 1))
                   (n (- n (A053644 n)))
                   (i (/ (A053644 n) 2)) ;; Start scanning from the second most significant bit.
                   (first_0_found? #f)
                  )
;;        (format #t "n=~a i=~a row=~a u=~a first_0_found?=~a rank=~a\n" n i row u first_0_found? rank)
          (cond ((or (zero? row) (zero? u) (zero? n)) (+ (expt 2 (-1+ w)) rank))
                ((> i n) ;; (zero? (modulo a 2))
                  (loop ;; Down, down, down...
                        rank ;; stays the same.
                        (- row 1) ;; We go one row topwards in Khayyam's triangle.
                        u         ;; We have the same number of upward steps after this downward step.
                        n
                        (/ i 2)
                        #t ;; At least now we have found the first or some subsequent zero.
                  )
                )
                (else ;; At this point both row and u and n must be >= 1.
                  (loop ;; Up the mountain high.
                        (+ rank (if first_0_found?
                                    (A007318tr (- (+ row u) 1) (- row 1))
                                    (A007318tr (- w 1) (- row 1))
                                )
                        )
                        (+ row 1)
                        (- u 1) ;; One up-step less left to use after this.
                        (- n i)
                        (/ i 2)
                        first_0_found?
                  )
                )
          )
        )
      )
  )
)


(define (A209861 n) (A209640 (A209642 n)))
(define (A209862 n) (A209859 (A036044 (A209641 n))))


(define (compute-and-print-cycle-vectors-for-A209861 outfile upto-n)
    (compute-and-print-generic-cycle-vectors A209861
                                     indices-of-nth-binary-forest
                                     outfile
                                     upto-n
    )
)


;; Conjecture: for every A209860(n), also A054429(A209860(n)) is in the sequence.
(define A209860 (MATCHING-POS 0 0 (lambda (i) (= (A209861 i) i))))

(define A209863 (fc-generic-Afun A209861 range-of-nth-binary-forest))
(define A209864 (cc-generic-Afun A209861 indices-of-nth-binary-forest))
(define A209865 (mc-generic-Afun A209861 indices-of-nth-binary-forest))
(define A209866 (lc-generic-Afun A209861 indices-of-nth-binary-forest))
(define A209867 (elements-on-odd-orbits-generic-Afun A209861 indices-of-nth-binary-forest))
(define A209868 (elements-on-even-orbits-generic-Afun A209861 indices-of-nth-binary-forest))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A227413 transferred to miscnum2.scm

(definec (A003188v3 n) ;; Seems to be, check!
   (cond ((< n 2) n)
         ((even? n) (A001969off0 (A003188v3 (/ n 2))))
         (else (A000069off0 (A003188v3 (/ (- n 1) 2))))
   )
)

(define (A233279 n) (A054429 (A006068 n)))
(define (A233280 n) (A003188 (A054429 n)))

(define (A233279v2 n) (A006068 (A063946 n)))
(define (A233280v2 n) (A063946 (A003188 n)))

(define (A233279v3 n) (A233280v3 (- n))) ;; Our hack with defineperm1's. RENUMBER!
;; (same-intfuns? A063946 (compose A003188 A233279) 256) ;; XXX -- Check!
;; (same-intfuns? A054429 (compose A233279 A003188) 256)
;; (same-intfuns? A054429 (compose A006068 A233280) 256)
;; (same-intfuns? A063946 (compose A233280 A006068) 256)


(defineperm1 (A233280v3 n) ;; Swap A000069 and A001969 and we seem to get A003188.
   (cond ((< n 2) n)
         ((even? n) (A000069off0 (A233280v3 (/ n 2))))
         (else (A001969off0 (A233280v3 (/ (- n 1) 2))))
   )
)

;; A234016-A234027 are now reserved for your use.

(definec (A234016 n) (if (< n 2) 0 (+ (A234016 (- n 1)) (- 1 (A079559 n)))))
(define (A234016v2 n) (- n (- (A046699off0 (+ n 1)) 1)))
(define (A234016v3 n) (- n (- (A046699 (+ n 2)) 1)))

(define (A234017 n) (* (- 1 (A079559 n)) (A234016 n)))


;; A233275-A233278

;; (same-intfuns? A054429 (COMPOSE A233275 A233278) 256)
;; (same-intfuns? A054429 (COMPOSE A233277 A233276) 256)


(definec (A233275 n)
  (cond ((< n 2) n)
        ((not (zero? (A079559 n))) (* 2 (A233275 (- (A213714 n) 1))))
        (else (+ 1 (* 2 (A233275 (A234017 n)))))
  )
)

;; (define (A233275 n) (A233276 (- n))) ;; Our hack with defineperm1's.

(definec (A233276 n) ;; Was defineperm1
   (cond ((< n 2) n)
         ((even? n) (A005187 (+ 1 (A233276 (/ n 2)))))
         (else (A055938 (A233276 (/ (- n 1) 2))))
   )
)

;; (define (A233277 n) (A233278 (- n))) ;; Our hack with defineperm1's.

(definec (A233277 n)
  (cond ((< n 2) n)
        ((zero? (A079559 n)) (* 2 (A233277 (A234017 n))))
        (else (+ 1 (* 2 (A233277 (- (A213714 n) 1)))))
  )
)


(definec (A233278 n) ;; Was defineperm1
   (cond ((< n 2) n)
         ((even? n) (A055938 (A233278 (/ n 2))))
         (else (A005187 (+ 1 (A233278 (/ (- n 1) 2)))))
   )
)

(define (A233275v2 n) (A054429 (A233277 n)))
(define (A233276v2 n) (A233278 (A054429 n)))

(define (A233277v2 n) (A054429 (A233275 n)))
(define (A233278v2 n) (A233276 (A054429 n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A209636 n)
   (let loop ((n (* 2 n)) (m 1))
        (cond ((< n 2) m)
              ((even? n) (loop (/ n 2) (* m 2)))
              (else (loop (/ (- n 1) 2) (A000040 m)))
        )
   )
)


(define (A209636v2 n) (A127301 (A071163 n)))
(define (A209636v3 n) (A209637 (A054429 n)))


(define (A209637 n) (A209636 (A054429 n)))
(define (A209637v2 n) (A127301 (A057505 (A071163 n))))
(define (A209637v3 n) (A127301 (A057163 (A071163 n))))

;; Compare with A071162:
(define (A209642 n)
   (let loop ((n n) (s 0) (i 1))
        (cond ((zero? n) s)
              ((even? n) (loop (/ n 2) (+ (* 4 s) 1) (* i 4)))
              (else (loop (/ (- n 1) 2) (* 2 (+ s i)) (* i 4)))
        )
   )
)

(define A209642v3 (compose-funs A056539 A071162))
(define A209642v4 (compose-funs A036044 A071162))


(define Anewperm1 (compose-funs A209861 A054429))
(define Anewperm1v2 (compose-funs A054429 A209861))

(define Anewperm2 (compose-funs A209862 A054429))
(define Anewperm2v2 (compose-funs A054429 A209862))


(define (A209635 n) (A008683 (A000265 n))) ;; Multiplicative.
(define A122132 (NONZERO-POS 1 1 A209635)) ;; Complement to next.
(define A038838 (ZERO-POS 1 1 A209635))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; From polyhexp.txt:
;;
;; %% 011110B -> 11B (perimeter contracted by four edges)
;; hexrewrite([0,1,1,1,1,0|X],[1,1|X]).
;; 
;; %% 01110B  -> 101B (perimeter contracted by two edges)
;; hexrewrite([0,1,1,1,0|X],[1,0,1|X]).
;; 
;; %% 0110B  -> 1001B (perimeter stays the same, one hex lopped off)
;; hexrewrite([0,1,1,0|X],[1,0,0,1|X]).
;; 

;; The most significant 1-bit works as a "sentinel", and is never rewritten:
;; (Could be defined recursively as well?)
;; Does this kind of "greedy rewriting" (without backtracking) always give
;; correct results, without auxiliary rotations of binary string?
;; (Rotate non-MSB-bits until even? Or is it enough just to always start
;; rewriting-processing from the lsb-end when any new rewrite has been made?)

(define (An_rewrites_possible_with_one_pass n)
    (let loop ((n n) (d 0))
         (cond ((< n 2) d)
               ((odd? n) (loop (/ (- n 1) 2) d)) ;; No rewrite possible this time: x1
               ((and (= (modulo n 64) 30) (> n 30)) ;; Largest rewriteable binary substring: x011110 -> x11
                   (loop (+ (* (floor->exact (/ n 64)) 4) 3) (+ d 1))
               )
               ((and (= (modulo n 32) 14) (> n 14)) ;; Second largest rewriteable binary substring: x01110 -> x101
                   (loop (+ (* (floor->exact (/ n 32)) 8) 5) (+ d 1))
               )
               ((and (= (modulo n 16) 6) (> n 6)) ;; First rewriteable binary substring: x0110 -> x1001
                   (loop (+ n 3) (+ d 1)) ;; 6 + 3 = 9.
               )
               (else (loop (/ n 2) d)) ;; Else it was some other even number. Shift once right.
         )
    )
)


;; First where differs from above one is at n=90, as:
;; (A007088 90) -> 1011010
;; (A007088 102) -> 1100110
;; thus:
;; (an_rewrites_possible_multiples_passes 90) --> 2

(define (An_rewrites_possible_multiples_passes n)
    (let loop ((n n) (rest 0) (i 0) (d 0))
         (cond ((< n 2) d)
               ((odd? n) (loop (/ (- n 1) 2) (+ rest (expt 2 i)) (1+ i) d)) ;; No rewrite possible this time: x1
               ((and (= (modulo n 64) 30) (> n 30)) ;; Largest rewriteable binary substring: x011110 -> x11
                   (loop (+ rest (* (expt 2 i) (+ (* (floor->exact (/ n 64)) 4) 3))) 0 0 (+ d 1))
               )
               ((and (= (modulo n 32) 14) (> n 14)) ;; Second largest rewriteable binary substring: x01110 -> x101
                   (loop (+ rest (* (expt 2 i) (+ (* (floor->exact (/ n 32)) 8) 5))) 0 0 (+ d 1))
               )
               ((and (= (modulo n 16) 6) (> n 6)) ;; First rewriteable binary substring: x0110 -> x1001
                   (loop (+ rest (* (expt 2 i) (+ n 3))) 0 0 (+ d 1)) ;; 6 + 3 = 9.
               )
               (else (loop (/ n 2) rest (1+ i) d)) ;; Else it was some other even number. Shift once right.
         )
    )
)



(define (An_rewrites_to_127?-ei-tarpeen n)
    (let loop ((n n) (rest 0) (i 0) (d 0))
         (cond ((< n 2) #f)
               ((= 127 (+ rest (* n (expt 2 i)))) #t)
               ((odd? n) (loop (/ (- n 1) 2) (+ rest (expt 2 i)) (1+ i) d)) ;; No rewrite possible this time: x1
               ((and (= (modulo n 64) 30) (> n 30)) ;; Largest rewriteable binary substring: x011110 -> x11
                   (loop (+ rest (* (expt 2 i) (+ (* (floor->exact (/ n 64)) 4) 3))) 0 0 (+ d 1))
               )
               ((and (= (modulo n 32) 14) (> n 14)) ;; Second largest rewriteable binary substring: x01110 -> x101
                   (loop (+ rest (* (expt 2 i) (+ (* (floor->exact (/ n 32)) 8) 5))) 0 0 (+ d 1))
               )
               ((and (= (modulo n 16) 6) (> n 6)) ;; First rewriteable binary substring: x0110 -> x1001
                   (loop (+ rest (* (expt 2 i) (+ n 3))) 0 0 (+ d 1)) ;; 6 + 3 = 9.
               )
               (else (loop (/ n 2) rest (1+ i) d)) ;; Else it was some other even number. Shift once right.
         )
    )
)

;; 1519 = 10111101111 should be first that rewrites to 127.

;; 2014 = 11111011110 in binary rewrites to 127.

(define (Apolyhexsize n)
    (let loop ((n n) (rest 0) (i 0) (d 0))
         (cond ((< n 2) 0) ;; Return zero to mark that n was not a valid polyhex perimeter code.
               ((= 127 (+ rest (* n (expt 2 i)))) (+ 1 d)) ;; Size of polyhex is # of rewrites + 1.
               ((odd? n) (loop (/ (- n 1) 2) (+ rest (expt 2 i)) (1+ i) d)) ;; No rewrite possible this time: x1
               ((and (= (modulo n 64) 30) (> n 30)) ;; Largest rewriteable binary substring: x011110 -> x11
                   (loop (+ rest (* (expt 2 i) (+ (* (floor->exact (/ n 64)) 4) 3))) 0 0 (+ d 1))
               )
               ((and (= (modulo n 32) 14) (> n 14)) ;; Second largest rewriteable binary substring: x01110 -> x101
                   (loop (+ rest (* (expt 2 i) (+ (* (floor->exact (/ n 32)) 8) 5))) 0 0 (+ d 1))
               )
               ((and (= (modulo n 16) 6) (> n 6)) ;; First rewriteable binary substring: x0110 -> x1001
                   (loop (+ rest (* (expt 2 i) (+ n 3))) 0 0 (+ d 1)) ;; 6 + 3 = 9.
               )
               (else (loop (/ n 2) rest (1+ i) d)) ;; Else it was some other even number. Shift once right.
         )
    )
)

(define Ahexuus1 (NONZERO-POS 0 0 Apolyhexsize))
(define Ahexuus2 (compose-funs Apolyhexsize Ahexuus1))
(define Ahexuus3 (RECORD-POS 0 0 Apolyhexsize)) ;; 127,1519,6007,23479,89527,356023,374491,...

;; Also this:
;; (map An_rewrites_possible_multiples_passes (map Ahexuus1 (iota0 128)))
;; (0 1 1 1 1 1 2 2 2 2 2 3 2 3 2 2 2 2 3 2 3 2 2 2 3 2 3 2 2 3 2 2 2 4 3 3 4 4 3 4 3 4 3 3 3 3 4 3 3 4 4 3 4 3 4 3 3 4 3 3 4 4 3 4 3 4 3 3 4 3 3 3 3 5 4 3 5 5 4 3 3 3 5 4 3 6 5 5 3 5 4 5 4 4 3 3 3 5 4 4 5 5 4 5 4 5 4 4 3 4 3 4 3 3 4 3 3 3 3 5 4 3 5 5 4 3 3 5 4)

;; Plus: How many in each range, sum of sizes (from above), averages?, etc.


;; Number of steps to reach 0 starting with n and using the iterated process:
;; x -> x - ( number of 1's in binary representation of x).
;; 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 7, 7, 8, 8, 9, 9, 10, 10, 10,

(definec (A071542 n) (if (zero? n) n (+ 1 (A071542 (- n (A000120 n))))))

;; (1 2 3 5 7 9 11 13 17 19 21 25 29 33 35 ...)

(define A213706 (PARTIALSUMS 0 0 A071542)) ;; The partial sums of the above.

(define (A213707 n) (+ n (A213706 n)))

(define A213708 (MATCHING-POS 0 0 (lambda (i) (or (< i 1) (not (= (A071542 (- i 1)) (A071542 i)))))))
(define A213708v2 (LEAST-I-WITH-FUN-I-EQ-N 0 0 A071542)) ;; Slow!
(define A213708v3 (RECORD-POS 0 0 A071542)) ;; Was: (RECORD-POS 0 0 A071542)

(define A213708_ind1 (RECORD-POS 1 0 A071542)) ;; Was: (RECORD-POS 0 0 A071542)
(define A213708_ind1v2 (MATCHING-POS 1 1 (lambda (i) (or (< i 2) (not (= (A071542 (- i 2)) (A071542 (-1+ i))))))))

(define (A086876 n) (- (A213708 (1+ n)) (A213708 n))) ;; With prepended 1.
(define (A086876v2 n) (1+ (- (A173601 n) (A213708 n))))


(define A173601 (PARTIALSUMS 1 0 (compose-funs A086876 1+)))
(define A173601slow (compose-funs -1+ (LEAST-I-WITH-FUN-I-EQ-N 0 0 A071542) 1+)) ;; Slow, for checking!
(define (A173601v2 n) (+ (A213708 n) (A086876 n) -1))

;; 1,1,2,3,5,9,17,30,54,98,179,330,614,1150,2162,4072,... (zero-based).
;; First differences of A213710.

(definec (A213709 n)
  (let loop ((i (-1+ (expt 2 (1+ n)))) (steps 1))
     (cond ((pow2? (1+ (- i (A000120 i)))) steps)
           (else (loop (- i (A000120 i)) (1+ steps)))
     )
  )
)

(define (A213709v2 n) (- (A071542 (-1+ (expt 2 (1+ n)))) (A071542 (-1+ (expt 2 n)))))

;; (map A213710 (iota0 10)) --> (1 2 3 5 8 13 22 39 69 123 221)
;; Note another false friend of Fibonaccis....
;; Gives the positions of (2^n)-1 in A179016, i.e. for all n>=0, A179016(A213710(n)) = (2^n)-1
;; (define A213710 (compose-funs 1+ (PARTIALSUMS 1 0 A213709)))
(define (A213710 n) (1+ (A218600 n)))
(define (A213710v2 n) (A071542 (A000079 n)))

;;  1 2 3 4 5 6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
;; [0,1,3,4,7,8,11,15,16,19,23,26,31,32,35,39,42,46,49,53,57,63,64,
;;  1 2 3   5       8             13                         22     = A213710
;;  1   3 4      7  8       11          15
;(0 0 1 2 3 3 4  4  4  5  5  5  5  5  6  6  6  6  6  6  6  6  6 7) = A213711
;; n+1 occurs A213709(n) times
(define A213711with_off1 (LEAST-GTE-I 0 0 A213710))

(definec (A179016with_off1 n)  ;; For the reference, the old version with starting offset=1.
  (cond ((< n 3) (- n 1))
        ((= (A213710 (A213711 n)) n) (- (expt 2 (A213711 n)) 1))
        (else (- (A179016 (+ n 1)) (A213712 (+ n 1))))
  )
)


(define (A218600 n) (if (zero? n) n (+ (A213709 (- n 1)) (A218600 (- n 1)))))
;; (define A218600 (PARTIALSUMS 1 0 A213709)) ;; a(n) = A213710(n)-1


(define (A218599 n) (- (A218600 (A213711 n)) n))
(define (A218601 n) (if (zero? n) n (-1+ (- n (A218600 (-1+ (A213711 n)))))))
(define (A218602 n) (- (A218600 (A213711 n)) (A218601 n)))

;; A new quick recurrence 11.11.2012:
(definec (A218616 n)
  (cond ((< n 4) (-1+ (expt 2 n)))
        ((A011371 (A218616 (-1+ n)))
          => (lambda (next) (if (pow2? (1+ next)) (A004755 (A004755 next)) next))
        )
  )
)

(definec (A218616v2 n)
  (cond ((< n 2) n)
        ((= (A213710 (-1+ (A213711 n))) n) (- (expt 2 (A213711 n)) 1))
        (else (A011371 (A218616v2 (-1+ n))))
  )
)

(define (A218616v3 n) (A179016 (A218602 n)))

(define (A179016v2 n) (A218616 (A218602 n)))

(define (A179016v3 n) (A054429 (A233271 (A218602 n))))

(define A213711 (LEAST-GTE-I 0 0 A218600)) ;; Offset changed from 1 to 0.

(definec (A179016 n) 
  (cond ((< n 2) n)
        ((= (A218600 (A213711 n)) n) (- (expt 2 (A213711 n)) 1))
        (else (- (A179016 (+ n 1)) (A213712 (+ n 1))))
  )
)

;; Also the first differences of A179016, shifted once right and prepended with zero:
(define (A213712 n) (A000120 (A179016 n)))

(define A213713 (COMPLEMENT 1 A179016))

;; (define A213714 (LEAST-I-WITH-FUN-I-EQ-N 1 1 A179016)
;; (define A213714 (LEAST-GTE-I 0 0 A005187))

;; 0,1,0,2,3,0,0,4,5,0,6,7,0,0,0,8,9,0,10,11,0,0,12,13,0,14,15,
;; A213714(A005187(n)) = n for all n.
(define (A213714 n) (if (< n 1) 0 (* (A079559 n) (- (A046699 (+ n 2)) 1))))
(define (A213714v2 n) (* (A079559 n) (-1+ (A046699off0 (1+ n)))))

(define (A213715 n) (A213714 (A179016 n))) ;; Is now zero-based. Positions of A179016 in A005187.

(define A213716 (COMPLEMENT 1 A213715))

(define (A213717 n) (A005187 (A213716 n))) ;; Is now zero based. Only finite branches.


;; (map A213718 (iota 27))
;; (1 2 2 3 4 4 4 5 6 6 6 7 7 7 7 8 9 9 9 10 10 10 10 11 11 11 12)
;; Old (define A213718 (LEAST-GTE-I 1 1 (compose-funs A179016 1+))) ;; n occurs A213712(n+1) times.
(define A213718 (LEAST-GTE-I 1 1 A179016)) ;; n occurs A213712(n) times. Still one-based.

(define (A213719 n) (if (zero? n) 1 (- (A213718 (1+ n)) (A213718 n)))) ;; A characteristic function for A179016
;; (define (A213720 n) (- 1 (A079559 n))) ;; A characteristic function for A055938. Deleted, A-number recycled.
(define (A000079_seems_to_be_yet_another_version n) (add A213723 (expt 2 n) (-1+ (expt 2 (1+ n)))))
;; (define (A213721 n) (- (A079559 n) (A213719 n))) ;; Characteristic function for A213717. Deleted, A-number recycl.

(define (A213731 n) (+ (A079559 n) (- (A079559 n) (A213719 n)))) ;; It's this, I think!


(define (A213722 n) (add (lambda (n) (- (A079559 n) (A213719 n))) (expt 2 n) (- (expt 2 (1+ n)) 1)))
(define (A213722v2 n) (add  (lambda (n) (- (A079559 n) (A213719 n))) (-1+ (expt 2 n)) (- (expt 2 (1+ n)) 2))) ;; This matches more with A213709.
;; A213722(n)+A213709(n)=A011782(n).

;; It seems that for n>1, A055938(2^n) = (2^(n+1)) + 1,
;; and that in each range [2^n,2^(n+1)-1], there are
;; 2^(n-1) terms. (Similarly for A005187?)

;; This gives offset +k to the less ("left") branch in beanstalk, if there are any k, such that A000120(n+k)=k,
;; otherwise zero.

;; a(A005187(n)) = A005843(n) = 2n (???)
(define (A213723 n) (A005843 (A213714 n)))
(define (A213723v2 n) (if (zero? (A079559 n)) 0 (+ n (A000120 (A213714 n)))))

;; A dumb version for the reference:
(define (A213723v3 n)
   (let loop ((k n))
        (cond ((= (A011371 k) n) k)
              ((> k (+ n n)) 0)
              (else (loop (1+ k)))
        )
   )
)

;; a(A005187(n)) = A005408(n) = 2n+1 (???)
(define (A213724 n) (if (zero? n) 1 (let ((v (A213723 n))) (if (zero? v) v (+ 1 v)))))


;; Index also with A213717:
;; Also, for each n, either zero if belongs to A055938, or the (maximum) length of finite branch
;; (or the number of vertices in that finite binary tree)
;; (because each number in A005187 branches in two, i.e. we have a binary tree,
;; Oriented or unoriented? The left-right orientation can be assigned by the numerical magnitude, for example.
;; Also, if oriented, form the A014486-index for each n, by cutting the infinite trunk
;; from the point of A179016(n).
;; Also, 0 for A055938 (leaves), 1 for A179016 (the other branch leads to finite tree), 2 for those in A213717.


(definec (A213725 n)
  (cond ;; ((< n 2) 0)
        ;; ((pow2? n) 0)
        ((zero? (A079559 n)) 1) ;; Leafs.
        ((not (zero? (A213719 n))) 0) ;; Nodes in infinite trunk.
        (else (1+ (max (A213725 (A213723 n)) (A213725 (A213724 n)))))
  )
)


(definec (A213726 n)
  (cond ((zero? (A079559 n)) 1) ;; Leafs.
        ((not (zero? (A213719 n))) 0) ;; Nodes in infinite trunk.
        (else (+ (A213726 (A213723 n)) (A213726 (A213724 n))))
  )
)

(definec (A213727 n)
  (cond ((zero? (A079559 n)) 1) ;; Leafs.
        ((not (zero? (A213719 n))) 0) ;; Nodes in infinite trunk.
        (else (+ 1 (A213727 (A213723 n)) (A213727 (A213724 n))))
  )
)


;; Breadth-first search for the shortest distance to the leaf. Cf. A213725
(definec (A257265 n)
  (let loop ((descendants (list (cons 0 n))))
    (let ((dist (caar descendants))
          (node (cdar descendants))
         )
       (cond ((zero? (A079559 node)) dist)
             (else
                (loop (sort ;; Discard the current node, add two children and then sort by the increasing distance.
                         (append (list (cons (+ 1 dist) (A213724 node))
                                       (cons (+ 1 dist) (A213723 node)) ;; This way we don't get problems with 0
                                 )
                                 (cdr descendants)
                         )
                         (lambda (a b) (< (car a) (car b)))
                      )
                )
             )
       )
    )
  )
)

(define A257508 (MATCHING-POS 1 0 (lambda (n) (= 1 (A257265 n)))))
(define A257508v2 (MATCHING-POS 1 0 (lambda (n) (or (zero? (A079559 (A213723 n))) (zero? (A079559 (A213724 n)))))))

(define A257509 (MATCHING-POS 1 0 (lambda (n) (= 2 (A257265 n)))))

(define (A256489 n) (- (A257509 (+ n 1)) (A257509 n)))

(define A257512 (MATCHING-POS 1 0 (lambda (n) (and (zero? (A079559 (A213723 n))) (zero? (A079559 (A213724 n)))))))

(define (A256490 n) (- (A257512 (+ n 1)) (A257512 n)))


;;;;;;;;;;;;;;;;;;;;;;


(defineperm1 (A257676 n)
  (if (<= n 1)
      n
      (let ((prev (A257676 (- n 1))))
         (cond ((= 1 (A213719 prev)) ;; If the previous term is in the infinite trunk of binary beanstalk?
                 (if (zero? (A213719 (A213723 prev))) ;; And the node to the left is not...
                     (A213723 prev) ;; Then choose it.
                     (A213724 prev) ;; Otherwise choose the right hand child.
                 )
               )
               ((not (zero? (A213723 prev))) (A213723 prev)) ;; If we can go left in this tendril, then let's go...
               ((not (zero? (A213724 prev))) (A213724 prev)) ;; If we can go right in this tendril, then let's go...
               (else ;; Otherwise, we have to backtrack in the beanstalk-tree.
                  (let loop ((prev prev) (back (A011371 prev)))
                         (cond ((= 1 (A213719 back)) ;; We got all the way back to the trunk?
                                  (if (zero? (A213719 (A213723 back))) ;; And the node to the left of it is not...
                                      (A213724 back) ;; Then choose the other (rhs) branch, to proceed up the trunk
                                      (A213723 back) ;; Otherwise the trunk proceeds to the left.
                                  )
                               )
                               ((and (even? prev) (not (zero? (A213724 back)))) ;; Climbing down from even...
                                      (A213724 back) ;; and there's a way to the right, so let's go there!
                               )
                               (else (loop back (A011371 back))) ;; Otherwise, keep on climbing down towards the root.
                         )
                  )
               )
         )
      )
  )
)

(define (A257677 n) (A257676 (- n)))

(define A257678 (FIXED-POINTS 1 0 A257676))

;;;;;;;;;;;;;;;;;;;;;;

(define (A213729_old n) (if (= (A213724 (A179016 n)) (A179016 (1+ n))) 1 0))
(define (A213729_oldv2 n) (- (A179016 (1+ n)) (A213723 (A179016 n))))
(define (A213729_oldv3 n) (A000035 (A179016 (1+ n))))

(define (A213728_old n) (if (= (A213723 (A179016 n)) (A179016 (1+ n))) 1 0))
(define (A213728_oldv2 n) (- (A213724 (A179016 n)) (A179016 (1+ n))))

;; Changed the indexing next day:
(define (A213729 n) (A000035 (A179016 n))) ;; New version starts with 0 for A179016(0)=0.

(define (A213729v2 n) (if (zero? n) n (- (A179016 n) (A213723 (A179016 (-1+ n))))))

(define (A213728 n) (- 1 (A213729 n))) ;; And its complement!
(define (A213728v2 n) (if (zero? n) 1 (- (A213724 (A179016 (-1+ n))) (A179016 n))))

;; (define (A213730old n) (+ (A213723 (A179016 n)) (A213728 n)))
(define (A213730 n) (+ (A213723 (A179016 n)) (A213728 (1+ n))))

;; A218541 - A218550

(define A213732old (NONZERO-POS 1 1 A213728))
(define A213733old (NONZERO-POS 1 1 A213729))


(define A218772 (NONZERO-POS 1 0 (compose-funs A213728 1+)))
(define A218773 (NONZERO-POS 1 0 (compose-funs A213729 1+)))

(define (A218774 n) (- (A218772 (1+ n)) (A218772 n)))
(define (A218775 n) (- (A218773 (1+ n)) (A218773 n)))

;; (define A213732old (NONZERO-POS 1 1 (compose-funs A213728 1+)))
;; (define A213733old (NONZERO-POS 1 1 (compose-funs A213729 1+)))
(define (A213732 n) (1+ (A218772 n)))
(define (A213733 n) (1+ (A218773 n)))

(define (A218541 n) (- (A213715 (1+ n)) (A213715 n)))


;; A213710		 a(n) = position of (2^n)-1 in A179016.	(Zero-based)
;; 1, 2, 3, 5, 8, 13, 22, 39, 69, 123, 221, 400, 730, 1344, 2494, 4656, 8728, 16406, 30902, 58320, 110299, 209099
;; 0, 1, 3, 4, 7, 8, 11, 15,

;; A213729 1-based, 
;;    1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1


;; Note, the examined ranges are [(2^n)-1,(2^(n+1))-2], or equally: [(2^n)-1,2*((2^n)-1)]
;; i.e. [0,0], [1,2], [3,6], [7,14], [15,30], [31,62], ...)

(definec (A218542 n)
  (if (zero? n) 1 ;; There's one even number in range [0,0], and that's 0 itself.
      (let loop ((i (- (expt 2 (1+ n)) n 2)) ;; i starts from ((2^(n+1))-1)-A000120((2^(n+1))-1) = (2^(n+1))-n-2
                 (s 0)
                )
          (cond ((pow2? (1+ i)) (+ s (- 1 (modulo i 2))))
                (else (loop (- i (A000120 i)) (+ s (- 1 (modulo i 2)))))
          )
      )
  )
)


(definec (A218543 n)
  (if (zero? n) 0 ;; There's no odd numbers in range [0,0]
      (let loop ((i (- (expt 2 (1+ n)) n 2)) ;; i starts from ((2^(n+1))-1)-A000120((2^(n+1))-1) = (2^(n+1))-n-2
                 (s 0)
                )
          (cond ((pow2? (1+ i)) (+ s (modulo i 2)))
                (else (loop (- i (A000120 i)) (+ s (modulo i 2))))
          )
      )
  )
)

;; Now corrected indexing:
(define (A218542v2 n) (add A213728 (A218600 n) (-1+ (A218600 (1+ n)))))
(define (A218543v2 n) (add A213729 (A218600 n) (-1+ (A218600 (1+ n)))))

(define (A257259 n) (- (A218542 n) (A218543 n)))

(define (A257805 n) (if (zero? n) 1 (+ (A257259 n) (A257805 (- n 1)))))
(define (A257805v2 n) (- (A257806 (A218600 (+ 1 n)))))



;; (map A218616 (iota0 21))
;; --> (0 1 3 7 4 15 11 8 31 26 23 19 16 63 57 53 49 46 42 39 35 32)
;; (map A218542analogue_for_evil_numbers (iota0 20))
;; --> (0 1 0 1 1 5 7 16 30 58 101 178 308 548 1000 1888 3614 6954 13346 25493 48416)

;; (map A218543analogue_for_odious_numbers (iota0 20))
;; --> (1 0 2 2 4 4 10 14 24 40 78 152 306 602 1162 2184 4064 7542 14072 26486 50384)

;; (map (lambda (n) (+ (A218542analogue_for_evil_numbers n)  (A218543analogue_for_odious_numbers n))) (iota0 20))
;; (1 1 2 3 5 9 17 30 54 98 179 330 614 1150 2162 4072 7678 14496 27418 51979 98800)

;; (map A213709 (iota0 20))
;; (1 1 2 3 5 9 17 30 54 98 179 330 614 1150 2162 4072 7678 14496 27418 51979 98800)

;; Note, the examined ranges are  [(2^(n+1))-1 downto 2^n],
;;       i.e. [1,1], [3,2], [7,4], [15,8], [31,16], ...
;; Compare to [0,0], [2,1], [6,3], [14,7], [30,15], [62,31], for A218542 & A218543.

(definec (A218542analogue_for_evil_numbers n)
  (if (< n 2) n ;; There are no evil numbers in range [1,1] of terms of A179016 , and there is one in range [3,2]
      (let loop ((i (- (expt 2 (1+ n)) 1)) ;; i starts from ((2^(n+1))-1)
                 (s 0)
                )
          (cond ((pow2? i) (+ s (A010059 i)))
                (else (loop (- i (A000120 i)) (+ s (A010059 i))))
          )
      )
  )
)


(definec (A218543analogue_for_odious_numbers n)
  (if (< n 2) (- 1 n) ;; There is one odious number in range [1,1] of terms of A179016, and none in range [3,2]
      (let loop ((i (- (expt 2 (1+ n)) 1)) ;; i starts from ((2^(n+1))-1)
                 (s 0)
                )
          (cond ((pow2? i) (+ s (A010060 i)))
                (else (loop (- i (A000120 i)) (+ s (A010060 i))))
          )
      )
  )
)



(define A218784 (MATCHING-POS 0 0 (lambda (i) (or (< i 1) (not (= (A213729 i) (A213729 (-1+ i))))))))

;; Where the direction of the infinite trunk of beanstalk changes?
;; (define A218544off1 (MATCHING-POS 1 1 (lambda (i) (or (< i 2) (not (= (A213729 i) (A213729 (-1+ i))))))))
(define (A218544 n) (1+ (A218784 n))) ;; Now zero-based indexing.
;; (define A218544v2 (MATCHING-POS 0 0 (lambda (i) (not (= (A213729 i) (A213729 (1+ i))))))) ;; Wrong!


;; (define (A218545 n) (- (A218544 (1+ n)) (A218544 n))) ;; Run-lengths
;; (define (A218545 n) (- (A218544 n) (A218544 (-1+ n)))) ;; This is one-based, A218544 zero-based.
(define (A218545 n) (- (A218784 n) (A218784 (-1+ n)))) ;; This is one-based, A218544 zero-based.

(define A218546 (RECORD-POS 1 1 A218545))

(define (A218617 n) (A218784 (-1+ (A218546 n))))

(define (A218547 n) (1+ (A218617 n)))
;; Was: (define (A218547 n) (A218544 (A218546 n))) ;; Indices to A213728 and A213729 where the next record run starts.



(define A218548 (RECORD-POS 1 0 A213726))

(define (A218549 n) (A213726 (A218548 n))) ;; One-based.
(define (A218550 n) (A213725 (A218548 n))) ;; One-based. Where does it differ?

;; (map A218546 (iota 5)) --> (1 2 14 36 4114)
;; (map A218547 (iota 5)) --> (1 2 19 60 7815)
;; (map A218548 (iota 5)) --> (1 2 4 6 8)


;; A218599-A218618 are now reserved for your use.

;; Both are one-based:
(define (A218603off1 n) (- (A179016 n) (A213708 (-1+ n)))) ;; Distance of the infinite trunk from the least edge.
(define (A218603off1v2 n) (-1+ (- (A086876 (-1+ n)) (A218604off1 n))))
(define (A218604off1 n) (- (A173601 (-1+ n)) (A179016 n))) ;; Distance of the infinite trunk from the other edge.
(define (A218604off1v2 n) (-1+ (- (A086876 (-1+ n)) (A218603off1 n))))

;; Now zero-based:
(define (A218603 n) (- (A179016 n) (A213708 n))) ;; Distance of the infinite trunk from the least edge.
(define (A218603v2 n) (-1+ (- (A086876 n) (A218604 n))))
(define (A218604 n) (- (A173601 n) (A179016 n))) ;; Distance of the infinite trunk from the other edge.
(define (A218604v2 n) (-1+ (- (A086876 n) (A218603 n))))

(define (A218605 n) (1+ (A218607 (-1+ n))))
(define (A218606 n) (1+ (A218608 (-1+ n))))

(define A218607 (ZERO-POS 0 0 A218603)) ;; We recompute new variants for new offsets!
(define A218608 (ZERO-POS 0 0 A218604)) ;; Note that these are zero-based both way.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XXX - Todo - Runlength Beanstalk:
(define (A236840 n)  (- n (A005811 n)))
(define (A255070 n) (/ (A236840 n) 2))

(definec (A255072 n) (if (zero? n) 0 (+ 1 (A255072 (A236840 n)))))
(define (A255071 n) (- (A255072 (- (expt 2 (+ n 1)) 2)) (A255072 (- (expt 2 n) 2))))

(define (A254119 n) (- (A213709 n) (A255071 n)))

(define (A255061 n) (A255072 (A000918 n)))
;; (definec (A255061fast n) (if (= 1 n) 0 (+ (A255061fast (- n 1)) (A255071fast (- n 1)))))

(define (A255062 n) (A255072 (A000225 n)))
;; (definec (A255062fast n) (if (<= n 1) n (+ (A255062fast (- n 1)) (A255071fast (- n 1)))))

(define A255053 (MATCHING-POS 0 0 (lambda (i) (or (< i 1) (not (= (A255072 (- i 1)) (A255072 i)))))))
(definec (A255053v1 n) (if (zero? n) n (+ (A255053v1 (- n 1)) (A255054 (- n 1)))))
(define A255053v2 (LEAST-I-WITH-FUN-I-EQ-N 0 0 A255072)) ;; Slow!
(define A255053v3 (RECORD-POS 0 0 A255072))

(define (A255054 n) (- (A255053 (1+ n)) (A255053 n))) ;; Cf. A086876, A255044.
(define (A255054v2 n) (+ 1 (- (A255055 n) (A255053 n))))
(define (A255054v3 n) (+ (A255123 n) (A255124 n) 1))

(define A255059 (MATCHING-POS 1 0 (lambda (n) (odd? (A255054 n)))))
(define A255060 (MATCHING-POS 1 0 (lambda (n) (even? (A255054 n)))))


;; (define A255055 (PARTIALSUMS 1 0 (COMPOSE A255054 1+)))
(definec (A255055 n) (if (zero? n) n (+ (A255054 n) (A255055 (- n 1)))))

(define A255055slow (COMPOSE -1+ (LEAST-I-WITH-FUN-I-EQ-N 0 0 A255072) 1+)) ;; Slow, for checking!
(define (A255055v2 n) (+ (A255053 n) (A255054 n) -1))


;; Runlength beanstalk with reversed subsections:
(definec (A255066 n)
  (cond ((< n 2) (+ n n))
        ((= n 2) 6)
;;      ((= n 3) 4) ;; Unnecessary.
        ((A236840 (A255066 (- n 1)))
          => (lambda (next) (if (pow2? (+ 2 next)) (A004755 (A004755 next)) next))
        )
  )
)

(define (A255067 n) (/ (A255066 n) 2)) ;; Divided by 2.

(define A255121 (COMPOSE -1+ (LEAST-GTE-I 0 1 A255061)))
(define (A255120 n) (if (zero? n) n (- n (A255062 (A255121 n)))))
(define (A255122 n) (- (A255061 (+ 1 (A255121 n))) (A255120 n)))

(define (A255056 n) (A255066 (A255122 n)))
(define (A255057 n) (A255067 (A255122 n)))
(define (A255057v2 n) (/ (A255056 n) 2))

(define (A255336 n) (A005811 (A255056 n)))
(define (A255336v2 n) (if (zero? n) n (- (A255056 n) (A255056 (- n 1)))))

(define (A255337 n) (/ (A005811 (A255056 n)) 2))
(define (A255337v2 n) (if (zero? n) n (- (A255057 n) (A255057 (- n 1)))))
(define A255338 (LEAST-GTE-I 0 0 A255057)) ;; n occurs A255337(n) times. Zero-based.
(define (A255339 n) (- (A255338 (1+ n)) (A255338 n))) ;; Characteristic function for A255057.


(definec (A255327 n)
  (cond ((odd? n) 1)
        ((= 1 (A255339 (/ n 2))) 0)
        (else (+ 1 (add A255327 (A091067 (/ n 2)) (A255068 (/ n 2)))))
  )
)

(definec (A255328 n) (if (zero? n) 1 (add A255327 (A091067 (A255057 n)) (A255056 (+ n 1)))))

(definec (A255329 n) (add A255327 (A255056 (+ n 1)) (A255068 (A255057 n))))

(definec (A255330 n) (if (zero? n) 1 (let ((k (A255057 n))) (add A255327 (A091067 k) (A255068 k)))))
(define (A255330v2 n) (+ (A255328 n) (A255329 n)))

(definec (A255331 n) (- (A255329 n) (A255328 n)))

(definec (A255332 n) (if (zero? n) -1 (+ (A255332 (- n 1)) (A255331 n))))

(definec (A255333 n) (if (zero? n) 1 (+ (A255333 (- n 1)) (A255330 n))))

(define (A255058 n) (A106836 (+ 1 (A255057 n))))

(define (A255123 n) (- (A255056 n) (A255053 n)))
(define (A255124 n) (- (A255055 n) (A255056 n)))

(define (A254113 n) (A010059 (A255056 n)))
(define (A254114 n) (A010060 (A255056 n)))


(definec (A255063 n)
  (if (zero? n) 1 ;; There's one evil number in range [0,0], and that's 0 itself.
      (let loop ((i (- (expt 2 (+ 1 n)) 4)) ;; i starts from ((2^(n+1))-2)-2
                 (s (modulo (+ 1 n) 2))
                )
          (cond ((pow2? (+ 2 i)) s)
                (else (loop (- i (A005811 i)) (+ s (A010059 i))))
          )
      )
  )
)


(definec (A255064 n)
  (if (zero? n) n ;; There's no odious number in range [0,0], so return 0.
      (let loop ((i (- (expt 2 (+ 1 n)) 4)) ;; i starts from ((2^(n+1))-2)-2
                 (s (modulo n 2))
                )
          (cond ((pow2? (+ 2 i)) s)
                (else (loop (- i (A005811 i)) (+ s (A010060 i))))
          )
      )
  )
)

(define (A255063v2 n) (add A254113 (A255062 n) (A255061 (+ 1 n)))) ;; Zero-based.
(define (A255063v3 n) (add (COMPOSE A010059 A255066) (A255062 n) (A255061 (+ 1 n))))

(define (A255064v2 n) (add A254114 (A255062 n) (A255061 (+ 1 n)))) ;; Zero-based.
(define (A255064v3 n) (add (COMPOSE A010060 A255066) (A255062 n) (A255061 (+ 1 n))))


(definec (A255125 n)
  (if (zero? n) 1 ;; There's one even number in range [0,0] of A255057, and that's 0 itself.
      (let loop ((i (- (expt 2 (+ 1 n)) 4)) ;; i starts from ((2^(n+1))-2)-2
                 (s 0)
                )
          (cond ((pow2? (+ 2 i)) s)
                (else (loop (- i (A005811 i)) (+ s (A133872 i))))
          )
      )
  )
)


(definec (A255126 n)
  (if (zero? n) n ;; There's no odd number in range [0,0] of A255057, so return 0.
      (let loop ((i (- (expt 2 (+ 1 n)) 4)) ;; i starts from ((2^(n+1))-2)-2
                 (s 1)
                )
          (cond ((pow2? (+ 2 i)) s)
                (else (loop (- i (A005811 i)) (+ s (A021913 i))))
          )
      )
  )
)

(define (A255125v2 n) (add (COMPOSE A059841 A255057) (A255062 n) (A255061 (+ 1 n)))) ;; Zero-based.
(define (A255125v3 n) (add (COMPOSE A059841 A255067) (A255062 n) (A255061 (+ 1 n))))

(define (A255126v2 n) (add (COMPOSE A000035 A255057) (A255062 n) (A255061 (+ 1 n)))) ;; Zero-based.
(define (A255126v3 n) (add (COMPOSE A000035 A255067) (A255062 n) (A255061 (+ 1 n))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A233270 n) (- (A233271 n) (A179016 n)))

(define (A233268off0 n) (floor->exact (/ (+ (A213710 n) (A218600 (+ n 1))) 2)))
(define (A233268 n) (floor->exact (/ (+ (A213710 (- n 1)) (A218600 n)) 2)))
(define (A233268v2 n) (+ (A218600 (- n 1)) (ceiling->exact (/ (A213709 (- n 1)) 2))))

(define (A233274 n) (if (< n 3) 0 (- (+ -1 (ceiling->exact (/ (A213709 (- n 1)) 2))) (A226060 (- n 2)))))
(define (A233274v2 n) (if (< n 2) 0 (- (A233268 n) (+ (A218600 (- n 1)) (A226060 (- n 2))) 1)))

(define (A234018 n) (A233270 (A233268 n))) ;; Values at the middle points.

(define A234019pos (RECORD-ABSVALS-BETWEEN-ZEROS-POS 0 A233270 A213710))

(define (A234019 n) (A233270 (- (A233268 n) (A234020 n))))
(define (A234019v2 n) (A233270 (A234019pos (- n 1))))

(define (A234020 n) ;; The difference from the middle point to the first maximum.
 (let ((middle (A233268 n)))
  (let loop ((i middle) ;; Start from the middle.
             (m 0)
             (maxp middle)
            )
     (cond ((zero? (A233270 i)) (- middle maxp))
           ((> (abs (A233270 i)) m) (loop (- i 1) (abs (A233270 i)) i))
           (else (loop (- i 1) m maxp))
     )
  )
 )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(definec (Aux_for218787 n)
  (cond ((zero? (A079559 n)) 0) ;; Leafs.
        ((not (zero? (A213719 n))) -1) ;; Nodes in infinite trunk.
        (else (A072764bi (Aux_for218787 (A213723 n)) (Aux_for218787 (A213724 n))))
  )
)


(definec (Aux_for218788 n) ;; Reflected.
  (cond ((zero? (A079559 n)) 0) ;; Leafs.
        ((not (zero? (A213719 n))) -1) ;; Nodes in infinite trunk.
        (else (A072764bi (Aux_for218788 (A213724 n)) (Aux_for218788 (A213723 n))))
  )
)

(define (A218786 n) (A072643 (A218787 n))) ;; The sizes of all side-trees.
(define (A218786v2 n) (-1+ (A213726 (A213730 n))))

(define (A218787 n) (Aux_for218787 (A213730 n))) ;; These are one-based, because
(define (A218788 n) (Aux_for218788 (A213730 n))) ;; we don't want the initial zero from A213730.

(define (A218787v2 n) (A057163 (A218788 n)))
(define (A218788v2 n) (A057163 (A218787 n)))

(define (A218609 n) (A218787 (A218611 n)))
(define (A218610 n) (A218788 (A218611 n)))

(define A218611 (DISTINCT-POS 1 1 A218787))
(define (A218612 n) (A213730 (A218611 n))) ;; ??

(define (A218613 n) (A218786 (A218611 n))) ;; The sizes of distinct ones.
(define (A218613v2 n) (-1+ (A213726 (A218612 n)))) ;; Alternative definition.

;; Also a sequence which tells on which side they are...

(definec (A218614 n) ;; Construct a binary code for n's place in the beanstalk-tree. Variant A.
   (cond ((< n 2) n) ;; Zero is here a special case, let's keep it zero. a(1)=1.
         ((even? n) (A004754 (A218614 (A011371 n)))) ;; Insert 0 before MSB.
         (else (A004755 (A218614 (A011371 n)))) ;; Insert 1 before MSB.
   )
)

(define (A218614v2 n) (A054429 (A218615 n)))

(definec (A218615 n) ;; Construct a binary code for n's place in the beanstalk-tree. Variant B.
   (cond ((< n 2) n) ;; Zero is here a special case, let's keep it zero. a(1)=1.
         ((odd? n) (A004754 (A218615 (A011371 n)))) ;; Insert 0 before MSB.
         (else (A004755 (A218615 (A011371 n)))) ;; Insert 1 before MSB.
   )
)

(define (A218615v2 n) (A054429 (A218614 n)))

(definec (A218790 n) ;; Construct a binary code for nth \/ in the beanstalk-tree. Variant A.
   (cond ((< n 2) n) ;; Zero is here a special case, let's keep it zero. a(1)=1.
         ((A011371 (* 2 n))
            => (lambda (bp)
                 (if (even? bp)
                     (A004754 (A218790 (/ bp 2)))
                     (A004755 (A218790 (/ (-1+ bp) 2)))
                 )
               )
         )
   )
)

(define (A218790v2 n) (A218614 (A005187 n)))
(define (A218790v3 n) (A054429 (A218791 n)))


(definec (A218791 n) ;; Construct a binary code for nth \/ in the beanstalk-tree. Variant A.
   (cond ((< n 2) n) ;; Zero is here a special case, let's keep it zero. a(1)=1.
         ((A011371 (* 2 n))
            => (lambda (bp)
                 (if (odd? bp)
                     (A004754 (A218791 (/ (-1+ bp) 2)))
                     (A004755 (A218791 (/ bp 2)))
                 )
               )
         )
   )
)

(define (A218791v2 n) (A218615 (A005187 n)))
(define (A218791v3 n) (A054429 (A218790 n)))



(definec (tree_for_A218780 n) ;; Needs functions from gato-package.
  (cond ((zero? n) (list))
        ((= 1 n) (list (list)))
        (else
          (let ((new-tree (copy-tree (tree_for_A218780 (-1+ n)))))
            (add-bud-for-the-nth-unbranching-tree-with-car-cdr-code! new-tree (A218791 n))
          )
        )
  )
)


(definec (tree_for_A218780! n) ;; Fast but dangerous version without copy-tree.
  (cond ((zero? n) (list))
        ((= 1 n) (list (list)))
        (else
          (let ((new-tree (tree_for_A218780! (-1+ n))))
            (add-bud-for-the-nth-unbranching-tree-with-car-cdr-code! new-tree (A218791 n))
          )
        )
  )
)


(definec (tree_for_A218782 n) ;; Needs functions from gato-package.
  (cond ((zero? n) (list))
        ((= 1 n) (list (list)))
        (else
          (let ((new-tree (copy-tree (tree_for_A218782 (-1+ n)))))
            (add-bud-for-the-nth-unbranching-tree-with-car-cdr-code! new-tree (A218790 n))
          )
        )
  )
)


(definec (tree_for_A218782! n) ;; Fast but dangerous version without copy-tree.
  (cond ((zero? n) (list))
        ((= 1 n) (list (list)))
        (else
          (let ((new-tree (tree_for_A218782! (-1+ n))))
            (add-bud-for-the-nth-unbranching-tree-with-car-cdr-code! new-tree (A218790 n))
          )
        )
  )
)

;;;;;;;;;;;;;;;;;;;;;


(definec (tree_for_A218776 n) ;; Needs functions from gato-package.
  (cond ((zero? n) (list))
        ((= 1 n) (list (list)))
        (else
          (let ((new-tree (copy-tree (tree_for_A218776 (-1+ n)))))
            (add-bud-for-the-nth-unbranching-tree-with-car-cdr-code! new-tree (A218615 n))
          )
        )
  )
)


(definec (tree_for_A218776! n) ;; Fast but dangerous version without copy-tree.
  (cond ((zero? n) (list))
        ((= 1 n) (list (list)))
        (else
          (let ((new-tree (tree_for_A218776! (-1+ n))))
            (add-bud-for-the-nth-unbranching-tree-with-car-cdr-code! new-tree (A218615 n))
          )
        )
  )
)


(definec (tree_for_A218778 n) ;; Needs functions from gato-package.
  (cond ((zero? n) (list))
        ((= 1 n) (list (list)))
        (else
          (let ((new-tree (copy-tree (tree_for_A218778 (-1+ n)))))
            (add-bud-for-the-nth-unbranching-tree-with-car-cdr-code! new-tree (A218614 n))
          )
        )
  )
)


(definec (tree_for_A218778! n) ;; Fast but dangerous version without copy-tree.
  (cond ((zero? n) (list))
        ((= 1 n) (list (list)))
        (else
          (let ((new-tree (tree_for_A218778! (-1+ n))))
            (add-bud-for-the-nth-unbranching-tree-with-car-cdr-code! new-tree (A218614 n))
          )
        )
  )
)


(definec (A218776 n) (parenthesization->A014486 (tree_for_A218776 n)))
(definec (A218776! n) (parenthesization->A014486 (tree_for_A218776! n)))

(define (A218777 n) (A080300 (A218776 n)))
(define (A218777! n) (A080300 (A218776! n)))
(define (A218777v2 n) (A057163 (A218779 n)))

(definec (A218778 n) (parenthesization->A014486 (tree_for_A218778 n)))
(definec (A218778! n) (parenthesization->A014486 (tree_for_A218778! n)))

(define (A218779 n) (A080300 (A218778 n)))
(define (A218779! n) (A080300 (A218778! n)))
(define (A218779v2 n) (A057163 (A218777 n)))

(definec (A218780 n) (parenthesization->A014486 (tree_for_A218780 n)))
(definec (A218780! n) (parenthesization->A014486 (tree_for_A218780! n)))

(define (A218781 n) (A080300 (A218780 n)))
(define (A218781! n) (A080300 (A218780! n)))
(define (A218781v2 n) (A057163 (A218783 n)))

(definec (A218782 n) (parenthesization->A014486 (tree_for_A218782 n)))
(definec (A218782! n) (parenthesization->A014486 (tree_for_A218782! n)))

(define (A218783 n) (A080300 (A218782 n)))
(define (A218783! n) (A080300 (A218782! n)))
(define (A218783v2 n) (A057163 (A218781 n)))

;; A218772-A218791 are now reserved for your use.

(define (A218618 n) (* (expt -1 (A213730 n)) (A213727 (A213730 n))))

(define A218785 (PARTIALSUMS 0 0 (compose-funs abs A218618)))

(define A218789 (PARTIALSUMS 0 0 A218618))


;; A218618, A218785, A218789

;; A219636-A219666 are now reserved for your use.


(define (A219641 n) (- n (A007895 n)))

(define A022342off1 (DISTINCT-POS 1 0 A219641)) ;; Integers with "even" Zeckendorf-expansion.

(define A035336v2off1 (MATCHING-POS 1 0 (lambda (i) (and (> i 0) (not (= (A219641 i) (A219641 (-1+ i)))) (not (= (A219641 i) (A219641 (1+ i)))))))) ;; Also Positions of integers which occur only once in A219641 ?


(define (A219639 n) (A219641 (A035336v2off1 n))) ;; Numbers that occur only once in A219641. 1-based.

(define (A219640 n) (A219641 (A022342off1 n))) ;; Distinct integers in A219641. One-based.


(define Aux_for219636 (COMPLEMENT 1 A035336v2off1))
(define (A219636 n) (if (= 1 n) 0 (Aux_for219636 (-1+ n))))
(define (Aux_for219637 n) (A219641 (A219636 n)))
(define A219637 (DISTINCT-VALS 1 1 Aux_for219637)) ;; Numbers that occur more than once in A219641? One-based.

(define A219638 (COMPLEMENT 1 A219640)) ;; Numbers that do not occur in A219641. 1-based. Cf. A055938, A219658.

(definec (A219642 n) (if (zero? n) n (+ 1 (A219642 (A219641 n))))) ;; Zero-based.


(define A219643 (MATCHING-POS 0 0 (lambda (i) (or (< i 1) (not (= (A219642 (- i 1)) (A219642 i)))))))
(define A219643v2 (LEAST-I-WITH-FUN-I-EQ-N 0 0 A219642)) ;; Slow!
(define A219643v3 (RECORD-POS 0 0 A219642)) ;;

(define (A219644 n) (- (A219643 (1+ n)) (A219643 n))) ;; Cf. A086876, A219654.
(define (A219644v2 n) (1+ (- (A219645 n) (A219643 n))))

(define A219645 (PARTIALSUMS 1 0 (compose-funs A219644 1+))) ;; Cf. A173601.
(define A219645slow (compose-funs -1+ (LEAST-I-WITH-FUN-I-EQ-N 0 0 A219642) 1+)) ;; Slow, for checking!
(define (A219645v2 n) (+ (A219643 n) (A219644 n) -1))

(define A219646 (PARTIALSUMS 0 0 A219642)) ;; Cf. A213706, A219656
(define (A219647 n) (+ n (A219646 n))) ;; Cf. A213707, A219657

(define (Aux_219648_from987_reversed n) (if (zero? n) 987 (A219641 (Aux_219648_from987_reversed (-1+ n)))))
(define A219648lista (reverse (map Aux_219648_from987_reversed (iota0 255))))

(define (Aux_219648_from1597_reversed n) (if (zero? n) 1597 (A219641 (Aux_219648_from1597_reversed (-1+ n)))))
(define A219648lista2 (reverse (map Aux_219648_from1597_reversed (iota0 385))))


;; Cf. A218254, A219659
(definec (A219649 n)
  (cond ((< n 2) n)
        ((not (zero? (A219649 (- n 1)))) (A219641 (A219649 (- n 1))))
        (else (+ 1 (A219649 (+ 1 (Aux_for_219649 (- n 1))))))
  )
)

(define Aux_for_219649 (compose-funs A219647 -1+ (LEAST-GTE-I 0 0 A219647))) ;; Gives the position of previous zero.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A230423 n) (* 2 (A230414 n)))

;; A dumb version for the reference (cf. A213723v3):
(definec (A230423v2 n)
   (let loop ((k n))
        (cond ((= (A219651 k) n) k)
              ((> k (+ n n)) 0)
              (else (loop (1+ k)))
        )
   )
)

(define (A230424v2 n) (* (A230412 n) (+ 1 (A230423 n))))
(definec (A230424 n) (if (zero? n) 1 (let ((v (A230423 n))) (if (zero? v) v (+ 1 v)))))

;; Not this way:
;; (definec (A230412 n)
;;   (if (< n 3)
;;       1
;;       (let ((k (- (A230413 (- n 1)) 1)))
;;          (if (= (+ (A219650 k) (A230405 k)) n) 1 0)
;;       )
;;   )
;; )
;; 
;; (definec (A230413 n) (if (zero? n) (A230412 n) (+ (A230413 (- n 1)) (A230412 n))))
;; 


;; Characteristic function for A219650
(definec (A230412 n)
  (if (zero? n)
      1
      (let ((k (A230413 (- n 1))))
         (if (= (+ (A219650 k) (A230405 k)) n) 1 0)
      )
  )
)


(definec (A230413 n) (if (zero? n) 0 (+ (A230413 (- n 1)) (A230412 n))))

(define (A230414 n) (* (A230412 n) (A230413 n)))

(define (A230414v2 n) (/ (A230423 n) 2)) ;; Was: A230423halved. Inverse function for A219650. Analogous binary: A213714.

;; (map A230423halved (iota0 45))
;; --> (0 1 2 0 0 3 4 5 0 0 6 7 8 0 0 9 10 11 0 0 0 0 0 12 13 14 0 0 15 16 17 0 0 18 19 20 0 0 21 22 23 0 0 0 0 0)
;; (define A219650v2 (LEAST-GTE-I 0 0 A230414)) ;; Temp name was: Apos_of_n_in_previous
;; (map A219650v2 (iota0 35))
;; --> (0 1 2 5 6 7 10 11 12 15 16 17 23 24 25 28 29 30 33 34 35 38 39 40 46 47 48 51 52 53 56 57 58 61 62 63)

(define (A230405 n) (A000217 (A230404 (+ 1 n)))) ;; XXX -- Check it!

(define (A230405v2 n) (- (A219650 (+ n 1)) (A219650 n)))
;; Only triangular terms occur: 1, 3, 6, 10, 15, 21, 28, ???

(definec (A219650 n) (if (zero? n) n (+ (A219650 (- n 1)) (A230405 (- n 1)))))

(define (Anon_ones_of_prev n) (A230405 (+ 2 (* 3 n))))

(define (Arecords_of_diffs n) (A230405 (-1+ (/ (A000142 (+ n 1)) 2)))) ;; Apparently A000217

(definec (A230425 n) ;; Cf. A213725
  (cond ((zero? (A230412 n)) 1) ;; Leafs.
        ((inA219666? n) 0) ;; Nodes in infinite trunk.
        (else (1+ (max (A230425 (A230423 n)) (A230425 (A230424 n)))))
  )
)


(definec (A230426 n)
  (cond ((zero? (A230412 n)) 1) ;; Leafs.
        ((inA219666? n) 0) ;; Nodes in infinite trunk.
        (else (+ (A230426 (A230423 n)) (A230426 (A230424 n))))
  )
)

(definec (A230427 n) ;; Cf. A213727
  (cond ((zero? (A230412 n)) 1) ;; Leafs.
        ((inA219666? n) 0) ;; Nodes in infinite trunk.
        (else (+ 1 (A230427 (A230423 n)) (A230427 (A230424 n))))
  )
)

(define (A230427v2 n) (if (zero? (A230426 n)) 0 (- (* 2 (A230426 n)) 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (A219650v2 n) (A219651 (* 2 n))) ;; Zero-based. Cf. A219640 and A005187.
(define (A219651 n) (- n (A034968 n))) ;; Zero-based.
(definec (A219652 n) (if (zero? n) n (+ 1 (A219652 (A219651 n))))) ;; Zero-based.

;;
;; The ranges go as:
;;
;; ]1,0] ](1+1)!-1,(1!-1)]
;; 
;; ]5,1] (21 - 3 => 2 - 1 => 1, 2 steps)
;; 
;; ]23,5 (321 - 6 => 221 - 5 (17-5 = 12) => 200 - 2 (12-2 = 10) => 120 - 3 (10 - 3 = 7) => 101 - 2 => 5

;; Can be computed as one-based sequence.
(definec (A219661 n)
  (if (zero? n) n
      (let loop ((i (-1+ (A000142 (1+ n)))) (steps 1))
           (cond ((isA000142? (1+ (A219651 i))) steps)
                 (else (loop (A219651 i) (1+ steps)))
           )
      )
  )
)

(definec (A219662 n) ;;  Cf. A218542
  (if (< n 2) n ;; There's one even number in range [0,0], and that's 0 itself.
      (let loop ((i (- (A000142 (1+ n)) (A000217 n) 1)) (s 0))
           (cond ((isA000142? (1+ i)) (+ s (- 1 (modulo i 2))))
                 (else (loop (A219651 i) (+ s (- 1 (modulo i 2)))))
           )
      )
  )
)


(definec (A219663 n) ;;  Cf. A218543
  (if (< n 2) 0 ;; There's no odd numbers in range [0,0].
      (let loop ((i (- (A000142 (1+ n)) (A000217 n) 1)) (s 0))
           (cond ((isA000142? (1+ i)) (+ s (modulo i 2)))
                 (else (loop (A219651 i) (+ s (modulo i 2))))
           )
      )
  )
)


;; (define Aevifactorial (MATCHING-POS 1 0 (lambda (i) (even? (A060130 i))))) ;; XXX -- Allocate A-numbers!
;; --> (0 3 5 7 8 10 13 14 16 19 20 22 25)
;; A230416: 0; 1; 5, 2; 23, 17, 12, 10, 7; 119, 109, 102, 97, 92, ..., 30, 28, 25;

;; (map A219666 (iota0 28))
;; --> (0 1 2 5 7 10 12 17 23 25 28 30 35 40 46 48 52 57 63 70 74 79 85 92 97 102 109 119 121)

;; (map A219662analogue_for_evifactorial_numbers (iota0 10))
;; --> (1 0 1 2 11 40 195 1350 9068 74332 684118)
;; (map A219663analogue_for_odifactorial_numbers (iota0 10))
;; --> (0 1 1 3 8 43 233 1261 9405 76394 693430)
;; (map (lambda (n) (+  (A219662analogue_for_evifactorial_numbers n) (A219663analogue_for_odifactorial_numbers n)))
;;   (iota0 10))
;; --> (1 1 2 5 19 83 428 2611 18473 150726 1377548)
;; (map A219661 (iota0 9)) --> (0 1 2 5 19 83 428 2611 18473 150726)

;; Ranges are [(n+1)!-1,n!] for n>=2: [5,2], [23,6]
(definec (A219662analogue_for_evifactorial_numbers n) ;;  Cf. A218542, A218542analogue_for_evil_numbers
  (if (< n 2) (- 1 n) ;; There are no evifactorial number in range [1,1] and one in [0,0].
      (let loop ((i (- (A000142 (1+ n)) 1)) (s 0))
           (cond ((isA000142? (1+ (A219651 i))) (+ s (if (even? (A060130 i)) 1 0)))
                 (else (loop (A219651 i) (+ s (if (even? (A060130 i)) 1 0))))
           )
      )
  )
)

(definec (A219663analogue_for_odifactorial_numbers n) ;;  Cf. A218543, A218543analogue_for_odious_numbers
  (if (< n 2) n ;; There's one odifactorial number in range [1,1] and none in [0,0].
      (let loop ((i (- (A000142 (1+ n)) 1)) (s 0))
           (cond ((isA000142? (1+ (A219651 i))) (+ s (if (odd? (A060130 i)) 1 0)))
                 (else (loop (A219651 i) (+ s (if (odd? (A060130 i)) 1 0))))
           )
      )
  )
)

;;
;; (map A007623 (map A219666 (iota0 27)))
;; (0 1 10 21 101 120 200 221 321 1001 1020 1100 1121 1220 1320 2000 2020 2111 2211 2320 3010 3101 3201 3310 4001 4100 4201 4321)

;; (map A230428 (iota 21))
;; (1 2 5 7 12 23 25 48 74 97 121 240 362 481 605 721 1440 2162 2881 3605 4326)

(definec (A230428 n)
  (if (< n 3) n
      (let ((k (A002260 n)))
          (let loop ((i (A230429 n)) (prev_i 0))
               (cond ((not (= (A099563 i) k)) prev_i) ;; 1st digit changed, take the prev_i
                     (else (loop (A219651 i) i))
               )
          )
      )
  )
)



;; (map A230429 (iota 28))
;; (1 2 5 10 17 23 46 70 92 119 238 358 476 597 719 1438 2158 2876 3597 4319 5039 10078 15118 20156 25197 30239 35279 40319)

;; (map A007623 (map A230429 (iota 28)))
;; (1 10 21 120 221 321 1320 2320 3310 4321 14320 24320 34310 44311 54321 154320 254320 354310 454311 554321 654321 1654320 2654320 3654310 4654311 5654321 6654321 7654321)

(definec (A230429 n)
  (if (= (A002024 n) (A002260 n))
      (- (A000142 (+ (A002024 n) 1)) 1) ;; (n+1)!-1, the right edge. 1,5,23,119,...
      (A219651 (A230428 (+ 1 n)))
  )
)

(definec (A230429-v2 n)
  (if (= (A002024 n) (A002260 n))
      (- (A000142 (+ (A002024 n) 1)) 1) ;; (n+1)!-1, the right edge. 1,5,23,119,...
      (let loop ((i (A230429-v2 (+ 1 n))))
           (cond ((= (A099563 i) (A002260 n)) i) ;; 1st digit has decr:ed to k, so we found the next to the left.
                 (else (loop (A219651 i)))
           )
      )
  )
)


(define (A230429_v3 n) (A2304_beanstalk_triangle_of_max_term_with_fe_of_length_n_and_first_digit_k (A002024 n) (A002260 n)))

;;
(define (A2304_beanstalk_triangle_of_max_term_with_fe_of_length_n_and_first_digit_k n k) ;; 1 <= k <= n
  (if (= n k)
      (- (A000142 (+ n 1)) 1) ;; (n+1)!-1, the right edge. 1,5,23,119,...
      (let loop ((i (A2304_beanstalk_triangle_of_max_term_with_fe_of_length_n_and_first_digit_k n (1+ k))))
           (cond ((= (A099563 i) k) i) ;; The first digit has been decremented to k, so we found the next to the left.
                 (else (loop (A219651 i)))
           )
      )
  )
)

;;
;;
;; 1,
;; 1,1,
;; 2,2,1,
;; 6,5,4,4,
;; 22,19,16,14,12,


;; (map A230420 (iota 45))
;; The triangle begins as: (the row sums give A219661,  (1 2 5 19 83 428 2611 18473 150726 1377548 ...)
;; 
;;     1
;;     1     1
;;     2     2     1
;;     6     5     4     4
;;    22    19    16    14    12
;;    94    82    73    65    59    55
;;   479   432   395   362   336   314   293
;;  2886  2667  2482  2324  2189  2073  1971  1881
;; 20276 19123 18124 17249 16473 15775 15140 14555 14011


(definec (A230420 n) ;; Offset = 1
  (if (<= n 3) 1
      (let loop ((i (A230429 n)) (s 0))
           (cond ((not (= (A099563 i) (A002260 n))) s)
                 (else (loop (A219651 i) (+ 1 s)))
           )
      )
  )
)

(define (A230421 n) (A230420 (A038722 n)))

(define (A230420-leftedge n) (A230420 (+ 1 (A000217 (- n 1))))) ;; I.e. the leftmost column.
;; (1 1 2 6 22 94 479 2886 20276)

(define (A230420-2nd-leftmost-col n) (A230420 (+ 2 (A000217 n))))
;; (1 2 5 19 82 432 2667 19123 156961)

(define (A230420-rightedge n) (A230420 (A000217 n)))
;; (1 1 1 4 12 55 293 1881 14011)


;; (define A226061 (PARTIALSUMS 2 1 A219661)) ;; Cf. A218600
(definec (A226061 n)
   (cond ((= 1 n) 0)
         (else (+ (A226061 (- n 1)) (A219661 (- n 1))))
   )
)

(definec (A219665 n) (1+ (A226061 n))) ;; Cf. A213710. a(n) = A219652(A000142(n..x))


(define A219653 (MATCHING-POS 0 0 (lambda (i) (or (< i 1) (not (= (A219652 (- i 1)) (A219652 i)))))))
(define A219653v2 (LEAST-I-WITH-FUN-I-EQ-N 0 0 A219652)) ;; Slow!
(define A219653v3 (RECORD-POS 0 0 A219652)) ;;

(define (A219654 n) (- (A219653 (1+ n)) (A219653 n))) ;; Cf. A086876, A219644.
(define (A219654v2 n) (1+ (- (A219655 n) (A219653 n))))
(define (A219654v3 n) (+ (A231723 n) (A231724 n) 1))


(define A219655 (PARTIALSUMS 1 0 (compose-funs A219654 1+))) ;; Cf. A173601.
(define A219655slow (compose-funs -1+ (LEAST-I-WITH-FUN-I-EQ-N 0 0 A219652) 1+)) ;; Slow, for checking!
(define (A219655v2 n) (+ (A219653 n) (A219654 n) -1))

(define A219656 (PARTIALSUMS 0 0 A219652)) ;; Cf. A213706, A219646
(define (A219657 n) (+ n (A219656 n))) ;; Cf. A213707, A219647

(define A219658 (COMPLEMENT 1 A219650)) ;; Numbers that do not occur in A219651. 1-based. Cf. A055938, A219638.

;; Cf. A218254, A219649
(definec (A219659 n)
  (cond ((< n 2) n)
        ((not (zero? (A219659 (- n 1)))) (A219651 (A219659 (- n 1))))
        (else (+ 1 (A219659 (+ 1 (Aux_for_219659 (- n 1))))))
  )
)

(define Aux_for_219659 (compose-funs A219657 -1+ (LEAST-GTE-I 0 0 A219657))) ;; Gives the position of previous zero.

;; A219666
;; So far, a fast and dirty implementation:
(define (Aux_219666_from720_reversed n) (if (zero? n) 720 (A219651 (Aux_219666_from720_reversed (-1+ n)))))

;; (define A219666lista (reverse (map Aux_219666_from720_reversed (iota0 111))))


(define A230411 (LEAST-GTE-I 1 1 A219665)) ;; Not A226061.

(define (A219666 n) (A230416 (A230432 n)))

(definec (A219666v2 n) ;; Starting offset is 0: 0, 1, 2, 5, 7, ...
  (cond ((<= n 2) n)
        ((= (A226061 (A230411 n)) n) (- (A000142 (A230411 n)) 1))
        (else (- (A219666v2 (+ n 1)) (A034968 (A219666v2 (+ n 1)))))
  )
)

;; Also the first differences of A219666, shifted once right and prepended with zero:
(define (A230406 n) (A034968 (A219666 n)))

(definec (A230406diffs n) (- (A230406 (+ 2 n)) (A230406 (+ 1 n))))

(define A230406diffs_pos_of_zeros (ZERO-POS 1 1 A230406diffs))

(define A230406diffs_pos_of_ones (MATCHING-POS 1 1 (lambda (i) (= 1 (A230406diffs i)))))
;; (map A230406diffs_pos_of_ones (iota 12))
;; (3 6 8 12 15 16 17 19 20 21 27 31)
;; 3 is present, because (A219666 3) = 5 = 21 in fact.exp, thus at next step we have 5-3 = 2, 10 in fact.exp.
;; 8 is present, because (A219666 8) = 23 = 321 in fact.exp, and 3+2+1 = 6, thus at next step we have 221 in fact.exp
;; x (= A226061(16)) is present, because (A219666 x) = ... = FEDCBA987654321 in fact.exp, and A000217(15)=120, which is also 5!
;; and thus in next step we have fact.exp FEDCBA987644321 whose digit sum is one less.

(define (A051683 n) (* (A000142 (A002024 n)) (A002260 n)))
(define A051683v2 (MATCHING-POS 1 1 (lambda (i) (= (A034968 i) (A099563 i)))))

;; Positions where terms of A051683 occur:
(define A230406pos_of_factmultiples (MATCHING-POS 1 1 (lambda (n) (let ((i (A230406 n))) (= (A034968 i) (A099563 i))))))



(define A230406vals_of_factmultiples (COMPOSE A230406 A230406pos_of_factmultiples))

(define A2304_recordpos_of_previous (RECORD-POS 1 1 A230406vals_of_factmultiples))
(define A2304_recordvals_of_previous (RECORD-VALS 1 1 A230406vals_of_factmultiples))

;; (map A2304_recordpos_of_previous (iota 6)) --> (1 3 5 34 112 510) ;; Not this!
;; Maybe this instead (infinite (?), but grows fast):
;; (map A230406pos_of_factmultiples (map A2304_recordpos_of_previous (iota 6))) --> (1 4 8 82 424 2206)
;; (map A2304_recordvals_of_previous (iota 6)) --> (1 2 6 12 18 24) ;; Possible terms: 48, 72, 96, 120, ?


;; (define A230418 (LEAST-GTE-I 1 1 A219666)) ;; n occurs A230406(n) times. Still one-based.
(define A230418 (LEAST-GTE-I 0 0 A219666)) ;; After zero, n occurs A230406(n) times. Now zero-based.

(define (inA219666? n) (or (zero? n) (= 1 (- (A230418 (1+ n)) (A230418 n)))))

;; Cf. A213730
(define (A230430 n) (+ (A230423 (A219666 n)) (- 1 (modulo (A219666 (1+ n)) 2))))
(define (A230430v2 n) (+ (A219666 (+ 1 n)) (expt -1 (modulo (A219666 (+ 1 n)) 2))))

;; Cf. A218601, A218602:
;; A227182-A230431 are now reserved for your use.
;; Rename A2304 -> A2304
;; A230403-A230432 are now reserved for your use.


(define (A230431 n) (if (< n 2) 0 (- n (A219665 (-1+ (A230411 (+ n 1)))))))
;; (define (A230431 n) (if (< n 2) 0 (- (+ n 1) (+ 1 (A219665 (-1+ (A230411 (+ n 1))))))))
(definec (A230432 n) (if (zero? n) n (- (A219665 (A230411 (+ 1 n))) (A230431 n) 1)))

;; A recurrence for A219666 with reversed subsections: (Cf. A218616)
;; 0; 1; 5, 2; 23, 17, 12, 10, 7; 119, 109, 102, 97, 92, ..., 30, 28, 25; - XXX --- Really like 5,2, yes!
(definec (A230416 n)
  (cond ((< n 3) (- (A000142 (+ 1 n)) 1))
        ((A219651 (A230416 (-1+ n)))
          => (lambda (next)
               (cond ((which_in_A000142? (+ 1 next)) => (lambda (k) (- (A000142 (+ k 2)) 1)))
                     (else next)
               )
             )
        )
  )
)

(define (A230416v2 n) (A219666 (A230432 n)))



(definec (A230407 n) (* (expt -1 (A230430 n)) (A230427 (A230430 n)))) ;; Cf. A218618.

(define A230408 (PARTIALSUMS 0 0 (COMPOSE abs A230407))) ;; Cf. A218785

(define A230409 (PARTIALSUMS 0 0 A230407)) ;; Cf. A218789

(definec (A230408v2 n) (if (zero? n) n (+ (abs (A230407 n)) (A230408v2 (- n 1)))))
(definec (A230409v2 n) (if (zero? n) n (+ (A230407 n) (A230409v2 (- n 1)))))


(define (A230410 n) (if (zero? n) n (A230415bi (A219666 n) (A219666 (- n 1)))))

(define A230422 (MATCHING-POS 1 1 (lambda (i) (= 1 (A230410 i)))))

(define (A231717 n) (if (zero? n) n (A231713bi (A219666 n) (A219666 (- n 1)))))

(define A231718 (MATCHING-POS 1 1 (lambda (i) (= 1 (A231717 i)))))

(define (A231719 n) (if (zero? n) n (A055881 (A230406 n))))
(define (A231719v2 n) (if (zero? n) n (A231715bi (A219666 n) (A219666 (- n 1)))))

(define (A231723 n) (- (A219666 n) (A219653 n))) ;; Cf. A218603
(define (A231724 n) (- (A219655 n) (A219666 n))) ;; Cf. A218604

(define (A231723v2 n) (- (A219654 n) (A231724 n) 1))
(define (A231724v2 n) (- (A219654 n) (A231723 n) 1))


(define (A258008 n) (- (A231718 (+ 1 n)) (A231718 n)))
(define (A258010 n) (- (A230422 (+ 1 n)) (A230422 n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reallocate these numbers:

(definec (uusixA213727 n)
  (cond ((zero? (A079559 n)) 0) ;; Leafs.
        ((not (zero? (A213719 n))) -1) ;; Nodes in infinite trunk.
        (else (1+ (packA001477 (A213727 (A213723 n)) (A213727 (A213724 n)))))
  )
)


(definec (uusixA213728 n)
  (cond ((zero? (A079559 n)) 0) ;; Leafs.
        ((not (zero? (A213719 n))) -1) ;; Nodes in infinite trunk.
        (else (1+ (packA061579 (A213728 (A213723 n)) (A213728 (A213724 n)))))
  )
)




(define (A213725list from-nth-A055938)
  (let* ((start-value (A055938 from-nth-A055938))
         (resvec (make-vector (1+ start-value) #f))
         (find-first-unused-from-the-end
            (lambda ()
                (let loopy ((i start-value))
                      (cond ((not (vector-ref resvec i)) i) ;; Yes, found a first free slot from the end!
                            ((zero? i) #f) ;; It's full already, let's exit.
                            (else (loopy (-1+ i)))
                      )
                )
            )
         )
        )
   (let loop ((n start-value) (steps 1))
      (cond ((not n) resvec) ;; find-first-unused-from-the-end returns #f when the vector is full.
            ((not (zero? (A213719 n)))
                (vector-set! resvec n 0) ;; Mark the nodes in infinite trunk with zero.
                (loop (find-first-unused-from-the-end) 1)
            )
            ((not (vector-ref resvec n)) ;; Ended first time in this value?
                (vector-set! resvec n steps) ;; How many steps was required to reach it this time?
                (loop (- n (A000120 n)) (1+ steps))
            )
            ((> steps (vector-ref resvec n)) ;; There was a previous number value for it, let's overwrite.
                (format #t "Overwriting resvec[~a]=~a with the new value ~a\n" n (vector-ref resvec n) steps)
                (vector-set! resvec n steps)
                (loop (- n (A000120 n)) (1+ steps))
            )
            (else
                (format #t "Not overwriting resvec[~a]=~a with the new value ~a\n" n (vector-ref resvec n) steps)
                (loop (- n (A000120 n)) (1+ steps))
            )
      )
   )
  )
)



;; A few functions for the sequences of Nico Brown:
            
;; Starting with 0, repeatedly subtract the number of 1's in the previous term's binary expansion,
;; then when you reach zero, increase the starting number's value by 1
;; 0, 1, 0, 2, 1, 0, 3, 1, 0, 4, 3, 1, 0, 5, 3, 1, 0, 6, 4, 3, 1, 0, 7, 4, 3, 1, 0, 8, 7, 4, 3, 1, 0, 9, 7, 4, 3, 1, 0, 10, 8, 7, 4, 3, 1, 0, 11, 8, 7, 4, 3, 1, 0, 12, 10, 8, 7, 4, 3, 1, 0

(definec (A218254 n)
  (cond ((< n 2) n)
        ((not (zero? (A218254 (- n 1)))) (- (A218254 (- n 1)) (A000120 (A218254 (- n 1)))))
        (else (+ 1 (A218254 (+ 1 (Apos_of_prev_zero (- n 1))))))
  )
)

(define Apos_of_prev_zero (compose-funs A213707 -1+ (LEAST-GTE-I 0 0 A213707)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (A173185 n) (if (< n 1) 1 (+ (A173185 (- n 1)) (A003418 n)))) ;; Cf. A231721
(define (A236856 n) (if (< n 2) n (+ (A236856 (- n 1)) (A003418 n)))) ;; Cf. A231722
(define A236857 (LEAST-GTE-I 0 0 A236856)) ;; Zero-based.
(define (A236858 n) (- n (A236856 (- (A236857 n) 1))))

;; Irregular triangle read by rows, T(n,k) = Sum_{i = 1..n} k mod i, k = LCM(1,2,...,n).
;; Kival Ngaokrajang, Feb 22 2014
(define (A238280tabf n k) (add (lambda (i) (modulo k i)) 1 n))

(define (A238280 n) (A238280tabf (A236857 n) (A236858 n))) ;; off=1.
