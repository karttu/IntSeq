
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;;  http://www.iki.fi/~kartturi/matikka/Nekomorphisms/gatorank.scm      ;;
;;  - Functions for ranking & unranking objects in Catalan families,    ;;
;;    in the standard lexicographical order (A014486)                   ;;
;;                                                                      ;;
;;  This Scheme-code is Copyright (C) 2002-2008 by Antti Karttunen      ;;
;;                                                                      ;;
;;  Last edited 2012-11-12.                                             ;;
;;  (E-mail: my_firstname.my_surname@gmail.com) and is placed under     ;;
;;  the GPL (Gnu Public License), so you are free to copy it.           ;;
;;                                                                      ;;
;;  Runs at least in MIT Scheme Release 7.6.0, for which one can find   ;;
;;  documentation and the pre-compiled binaries (for various OS's       ;;
;;  running in Intel x86 architecture) under the URL:                   ;;
;;                                                                      ;;
;;  http://www.swiss.ai.mit.edu/projects/scheme/                        ;;
;;                                                                      ;;
;;  The main pointer for this code collection is:                       ;;
;;  http://www.iki.fi/~kartturi/matikka/Nekomorphisms/gatomorf.htm      ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;;   (cf "./gatorank" "./")
;; )
;; 

;; From http://www.swiss.ai.mit.edu/projects/scheme/documentation/user_5.html
;;
;; If you redefine some global name in your code, for example, car, cdr
;; and cons, you should indicate it in the declaration as:
;; (declare (usual-integrations car cdr cons))

;; (Beware of using an argument name like list in the function definitions?)

(declare (usual-integrations))



(define (A213704bi row col)
  (cond ((zero? row) 0) ;; The top row all zeros.
        ((>= col (A000108 row)) 0) ;; On other rows, give zeros after the C(n) totally balanced binary seqs.
        (else (CatalanUnrank row col)) ;; But before that, give the totally balanced binary seqs.
  )
)
  	
(define (A213704 n) (A213704bi (A002262 n) (A025581 n))) ;; Zero-based


;; (map A014486 (cons 0 (iota 23)))
;; --> (0 2 10 12 42 44 50 52 56 170 172 178 180 184 202 204 210 212 216 226 228 232 240 682)

(definec (A014486 n)
   (let ((w/2 (A072643 n)))
      (CatalanUnrank w/2 (if (zero? n) 0 (- n (A014137 (-1+ w/2)))))
   )
)

(definec (A014486v2 n)
   (if (zero? n)
       0
       (A213704bi (A072643 n) (- n (A014137 (- (A072643 n) 1))))
   )
)


;; This is A081288(n) = minimal i such that A000108(i) > n.
;; (map A081288nc (cons 0 (iota 42)))
;; --> (0 2 3 3 3 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6)
(define (A081288nc n) (first_pos_with_funs_val_gte A000108 (1+ n)))


(define (CatalanRankLocal a)
   (if (zero? a)
       0
       (+ (CatalanRank (/ (binwidth a) 2) a))
   )
)

(define (CatalanRankGlobal a)
   (if (zero? a)
       0
       (let ((w/2 (/ (binwidth a) 2)))
          (+ (A014137 (-1+ w/2))
             (CatalanRank w/2 a)
          )
       )
   )
)

;; This should produce same as (cons 0 (iota 6919)):
;;
;; (map CatalanRankGlobal
;;      (map parenthesization->A014486
;;           (map A014486->parenthesization
;;                (map A014486 (cons 0 (iota 6919))))))
;; 

;; See http://www.iki.fi/~kartturi/matikka/tab9766.htm


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
             (c (A009766tr (-1+ size) size))
            )
      (if (negative? m) a
          (if (>= rank c)
              (loop (1+ (* 2 a))   ;; Up the mountain high
                    m
                    (-1+ y)
                    (- rank c)
                    (A009766tr m (-1+ y))
              )
              (loop (* 2 a)        ;; Down to the valley low
                    (-1+ m)
                    y
                    rank
                    (A009766tr (-1+ m) y)
              )
          )
      )
  )
)

;; Here is the old variant of the same algorithm, based on
;; one given in Stinson's & Kreher's book.
;; Note that this is almost identical to the one given above.

(define (CatalanUnrankOld size orank)
  (let ((rank (- (/ (C (* 2 size) size) (1+ size))
                 (1+ orank)
              )
        )
       )
    (let loop ((a 0)         ;; Constructed bit-string
               (m size)       ;; The row on A009766
               (y (-1+ size)) ;; The position on row m of A009766
               (lo 0)
               (c (A009766tr size (-1+ size)))
              )
      (if (zero? m) a
          (if (> (+ lo c) rank)
              (loop (1+ (* 2 a)) ;; Up the mountain high
                    m
                    (-1+ y)
                    lo
                    (A009766tr m (-1+ y))
              )
              (loop (* 2 a)      ;; Down to the valley low
                    (-1+ m)
                    y
                    (+ lo c)
                    (A009766tr (-1+ m) y)
              )
          )
      )
    )
  )
)

(define (CatalanRank w/2 a)
    (let loop ((a a) ;; The totally balanced binary expansion
               (r 0)
               (lo 0)
               (y -1)
              )
      (if (zero? a)
          (- (/ (C (* 2 w/2) w/2) (1+ w/2))
             (1+ lo)
          )
          (if (zero? (modulo a 2))
              (loop ;; Down to the valley
                    (floor->exact (/ a 2)) ;; Was: (fix:lsh a -1) ;; a >>= 1
                    (1+ r)
                    (+ lo (A009766tr (1+ r) y))
                    y
              )
              (loop ;; Upto the mountain high.
                    (floor->exact (/ a 2)) ;; Was: (fix:lsh a -1)
                    r lo (1+ y)
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
;; This one added 14. October 2003.
;; Thanks Frank, this makes it all much leaner!

(define (CatalanRankSexpAux size node)
  (let ((m (-1+ size))        ;; The row on A009766
        (y size)              ;; The position on row m of A009766
        (rank 0)
       )
    (let TreeRank ((node node))
      (cond ((not (pair? node)) (set! m (-1+ m)))
            (else
                  (set! rank (+ rank (A009766tr m y)))
                  (set! y (-1+ y))
                  (TreeRank (car node))
                  (TreeRank (cdr node))
            )
      )
    )
    rank
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



;; Unrank from a rank (an integer) to a symbolless S-expression directly.
;; See Frank Ruskey's thesis at:
;; http://www.cs.uvic.ca/~fruskey/Publications/Thesis/Thesis.html
;; especially the page 24:
;; http://www.cs.uvic.ca/~fruskey/Publications/Thesis/ThesisPage24.png
;; Note that also there the loop condition is given erroneously as m > 0,
;; while it should be m >= 0, i.e. we stop only when m goes negative.


(define (CatalanUnrankSexpAux size rank)
 (let ((sonstack (make-vector size))
       (root (list))
      )
   (let loop ((m (-1+ size))        ;; The row on A009766
              (y size)              ;; The position on row m of A009766
              (rank rank)
              (c (A009766tr (-1+ size) size))
              (rightson? #f)
              (sp 0)
             )
       (if (negative? m) root
           (cond
             ((>= rank c)
                (let ((newbranch (cons (list) (list))))
                   (cond
                     ((null? root) (set! root newbranch))
                     (rightson? (set-cdr! (vector-ref sonstack sp) newbranch))
                     (else (set-car! (vector-ref sonstack sp) newbranch)
                           (set! sp (1+ sp))
                     )
                   ) ;; cond
                   (vector-set! sonstack sp newbranch)
                   (loop m
                         (-1+ y)
                         (- rank c)
                         (A009766tr m (-1+ y))
                         #f     ;; Next time we have a left son.
                         sp     ;; sp already incremented above if needed.
                   )
                ) ;; let
             )
             (else
               (loop (-1+ m)
                     y
                     rank
                     (A009766tr (-1+ m) y)
                     #t         ;; Next time we have a right son.
                     (- sp (if rightson? 1 0))
               )
             )
           )
       )
   )
 )
)


(define (CatalanUnrankSexp n)
   (let ((size (A072643 n)))
      (CatalanUnrankSexpAux size (if (zero? n) 0 (- n (A014137 (-1+ size)))))
   )
)

;; It would be instructive to define CatalanRankSexp and CatalanUnrankSexp in Prolog,
;; but now we take an easier route, and create a massive precomputed table:

(define (prepare-prolog-table upto_n)
  (let loop ((n 0))
     (write-string "n2s(")
     (write n)
     (write-string ",")
     (print_prolog_sexp (CatalanUnrankSexp n)) ;; print_prolog_sexp defined in ../Schemuli/outfuns1.scm
     (write-string ").")
     (newline)
     (if (< n upto_n) (loop (1+ n)))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;; Function parenthesization->A014486 and two versions of             ;;
;;  A014486->parenthesization, first the                              ;;
;;   straightforward version converted from Maple code,               ;;
;;    and then the more enlightened "Forth"-inspired version.         ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (parenthesization->A014486 '())      --> 0
;; (parenthesization->A014486 '(()))    --> 2
;; (parenthesization->A014486 '(() ())) --> 10
;; (parenthesization->A014486 '((())))  --> 12

(define (parenthesization->A014486 p) ;; Renamed from parenthesization->binexp Jan 14 2007.
   (let loop ((s 0) (p p))
      (if (null? p)
          s
          (let* ((x (parenthesization->A014486 (car p)))
                 (w (binwidth x))
                )
             (loop
;; Do not use!     (+ (fix:lsh s (+ w 2)) (fix:lsh 1 (1+ w)) (* 2 x))
                   (+ (* s (expt 2 (+ w 2))) (expt 2 (1+ w)) (* 2 x))
                   (cdr p)
             )
          )
      )
   )
)


;; PeelNextBalSubSeq and RestBalSubSeq expect their
;; integer argument nn to contain the binary expansion
;; of the (sub-)parenthesization in reverse order,
;; with the least significant bit being always 1.


(define (PeelNextBalSubSeq nn) ;; We assume that given nn is odd.
  (let loop ((z 0) (level -1) (n (fix:lsh nn -1)))
      (cond ((zero? level) (/ z 2)) ;; n on prev. iteration must has been even.
            (else (loop (+ (fix:lsh z 1) (modulo n 2)) ;; z <<= 1, z += n % 2
                        (+ level (expt -1 (modulo n 2)))
                        (fix:lsh n -1) ;; n >>= 1
                  )
            )
      )
  )
)


(define (RestBalSubSeq nn) ;; We assume that given nn is odd.
  (let loop ((level -1) (n (fix:lsh nn -1)))
      (cond ((zero? level) (PeelNextBalSubSeq (1+ (* 2 n))))
            (else (loop (+ level (expt -1 (modulo n 2)))
                        (fix:lsh n -1) ;; n >>= 1
                  )
            )
      )
  )
)


(define (ConsTBBS a b) ;; "cons" two totally balanced binary sequences
   (let ((aw (binwidth a))
         (bw (binwidth b))
        )
     (+ (expt 2 (+ 1 aw bw)) (* a (expt 2 (1+ bw))) b)
   )
)


(define (reversed_A014486->parenthesization n)
   (cons (A014486->parenthesization_in_dumb_way (PeelNextBalSubSeq n))
         (A014486->parenthesization_in_dumb_way (RestBalSubSeq n))
   )
)

(define (A014486->parenthesization_in_dumb_way n)
   (if (zero? n)
       (list)
       (reversed_A014486->parenthesization (A030101 n)) ;; Was binrev
   )
)


;; Now, if we remember that "Lisp" spelled backwards is "Forth",
;; and the parenthesizations have another form as Dyck paths,
;; it's much easier to implement this by scanning the totally
;; balanced binary string from the end (the rightmost = the least
;; significant bit) to the beginning (to the leftmost = the most
;; significant bit). Note how we don't need double-forked
;; recursion anymore, but just simple tail-recursion is enough.
;;


;; Sep 06 2002:  (fix:lsh n -1) changed to (floor->exact (/ n 2))

(define (A014486->parenthesization n) ;; Renamed from binexp->parenthesization in Jan 14 2007.
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


;; Experimental, quaternary zigzag-tree code, variant A:
;; Differs from A057117 first time at position 56,
;; whence this has 42 while it has 44.
;; This is now stored in OEIS as A082356.
;; (map CatalanRankGlobal (map parenthesization->A014486 (map quatexpA->parenthesization (map A014486 (iota0 64)))))
;; (0 1 2 3 4 5 7 8 6 9 10 12 13 11 17 18 21 22 20 14 15 16 19 23 24 26 27 25 31 32 35 36 34 28 29 30 33 45 46 49 50 48 58 59 63 64 62 54 55 57 61 37 38 40 41 39 42 43 44 47 51 52 56 60 53)

(define (quatexpA->parenthesization n)
   (if (zero? n) (list)
       (let loop ((n (floor->exact (/ n 2)))
                  (stack (list (cons (list) (list))))
                 )
           (cond ((< n 2) (car stack))
                 (else
                   (case (modulo n 4)
                     ((0) ;; 00
                        (loop (floor->exact (/ n 4))
                              (cons (cons (list) (list)) stack)
                        )
                     )
                     ((1) ;; 01
                        (loop (floor->exact (/ n 4))
                              (cons2top! (cons (list) stack))
                        )
                     )
                     ((2) ;; 10
                        (loop (floor->exact (/ n 4))
                              (flip!topmost (cons2top! (cons (list) stack)))
                        )
                     )
                     ((3) ;; 11
                        (loop (floor->exact (/ n 4))
                              (cons2top! stack)
                        )
                     )
                   )
                 )
           )
       )
   )
)

;; (map CatalanRankGlobal (map parenthesization->A014486 (map quatexpB->parenthesization (map A014486 (iota0 64)))))
;; (0 1 3 2 8 7 5 4 6 22 21 18 17 20 13 12 10 9 11 15 14 19 16 64 63 59 58 62 50 49 46 45 48 55 54 61 57 36 35 32 31 34 27 26 24 23 25 29 28 33 30 41 40 38 37 39 52 51 60 56 43 42 47 44 53)
;; Is equal to A074684.
;; Variant B, with the roles of 01 and 10 swapped: (but 11 is not flipped!)
(define (quatexpB->parenthesization n)
   (if (zero? n) (list)
       (let loop ((n (floor->exact (/ n 2)))
                  (stack (list (cons (list) (list))))
                 )
           (cond ((< n 2) (car stack))
                 (else
                   (case (modulo n 4)
                     ((0) ;; 00
                        (loop (floor->exact (/ n 4))
                              (cons (cons (list) (list)) stack)
                        )
                     )
                     ((1) ;; 01
                        (loop (floor->exact (/ n 4))
                              (flip!topmost (cons2top! (cons (list) stack)))
                        )
                     )
                     ((2) ;; 10
                        (loop (floor->exact (/ n 4))
                              (cons2top! (cons (list) stack))
                        )
                     )
                     ((3) ;; 11
                        (loop (floor->exact (/ n 4))
                              (cons2top! stack)
                        )
                     )
                   )
                 )
           )
       )
   )
)

;; Variant C, with the roles of 01 and 10 swapped, AS WELL AS 11 flipped!
;; (map CatalanRankGlobal (map parenthesization->A014486 (map quatexpC->parenthesization (map A014486 (iota0 64)))))
;; (0 1 3 2 8 7 5 4 6 22 21 18 17 20 13 12 10 9 11 19 16 15 14 64 63 59 58 62 50 49 46 45 48 61 57 55 54 36 35 32 31 34 27 26 24 23 25 33 30 29 28 60 56 47 44 53 52 43 41 40 51 42 38 37 39)
;; This is now stored in OEIS as A082358.
;; This is of course the composition A057163 o A082356.

(define (quatexpC->parenthesization n)
   (if (zero? n) (list)
       (let loop ((n (floor->exact (/ n 2)))
                  (stack (list (cons (list) (list))))
                 )
           (cond ((< n 2) (car stack))
                 (else
                   (case (modulo n 4)
                     ((0) ;; 00
                        (loop (floor->exact (/ n 4))
                              (cons (cons (list) (list)) stack)
                        )
                     )
                     ((1) ;; 01
                        (loop (floor->exact (/ n 4))
                              (flip!topmost (cons2top! (cons (list) stack)))
                        )
                     )
                     ((2) ;; 10
                        (loop (floor->exact (/ n 4))
                              (cons2top! (cons (list) stack))
                        )
                     )
                     ((3) ;; 11
                        (loop (floor->exact (/ n 4))
                              (flip!topmost (cons2top! stack))
                        )
                     )
                   )
                 )
           )
       )
   )
)

;; Variant D is like the variant A, except the
;; double case (3) is flipped.
;; (map CatalanRankGlobal (map parenthesization->A014486 (map quatexpD->parenthesization (map A014486 (iota0 64)))))
;; (0 1 2 3 4 5 7 8 6 9 10 12 13 11 17 18 21 22 20 16 19 14 15 23 24 26 27 25 31 32 35 36 34 30 33 28 29 45 46 49 50 48 58 59 63 64 62 57 61 54 55 44 47 56 60 53 42 51 37 38 43 52 40 41 39)
;; This is of course equal to A057163 o A074684, and is stored as A082360 in OEIS.

(define (quatexpD->parenthesization n)
   (if (zero? n) (list)
       (let loop ((n (floor->exact (/ n 2)))
                  (stack (list (cons (list) (list))))
                 )
           (cond ((< n 2) (car stack))
                 (else
                   (case (modulo n 4)
                     ((0) ;; 00
                        (loop (floor->exact (/ n 4))
                              (cons (cons (list) (list)) stack)
                        )
                     )
                     ((1) ;; 01
                        (loop (floor->exact (/ n 4))
                              (cons2top! (cons (list) stack))
                        )
                     )
                     ((2) ;; 10
                        (loop (floor->exact (/ n 4))
                              (flip!topmost (cons2top! (cons (list) stack)))
                        )
                     )
                     ((3) ;; 11
                        (loop (floor->exact (/ n 4))
                              (flip!topmost (cons2top! stack))
                        )
                     )
                   )
                 )
           )
       )
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; New ideas March 9, 2012, based on some inspiring links sent by
;; Emeric Deutsch.
;; We can apply the Bit-tuple Notation for Trees (rooted, but non-oriented)
;; as described by Mueller, Szymanski, Trinajstic and Knop
;; also to our dear rooted and oriented (plane) trees.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parenthesization->bittuple p) (n-tuple->bittuple (p->Lw p)))

;; And what we seem to get? Some notorious old friends:
(define (A057506_yet_another_way-conjectural n) (A080300 (parenthesization->bittuple (A014486->parenthesization (A014486 n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Our own copy of p->Lw, copied from gatocout.scm:

(define (p->Lw p)
  (reverse! (cdr (reverse
    (let recurse ((p p))
           (cond ((null? p) (list 0))
                 (else ;; it is a list.
                    (append! (list (length p))
                             (apply append! (map recurse p))
                    )
                 )
           )
    )
  )))
)

;; The Lw->parenthesization and p->Lw (transferred to gatocout.scm)
;; are inverses of each other:
;; (first-dislocated (apply_upto_n 2056 (compose-funlist (list Lw->parenthesization p->Lw))))
;; --> ()

(define (Lw->parenthesization L)
   (let loop ((L (reverse L)) (stack (list (list)))) ;; The last leaf is implicit
       (cond ((null? L) (car stack))
             (else
                (loop (cdr L) (list-n-from-top (car L) stack))
             )
       )
   )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;;           Transferred from gatomain.scm                            ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (BinTree2Tree '(() . (() . ()))) --> (() (() ()))
;; (BinTree2Tree '((() . ()) . ())) --> ((() ()) ())


(define (BinTree2Tree bt) ;; Not a bijection. (A057123)
  (cond ((not (pair? bt)) bt)
        (else (list (BinTree2Tree (car bt))
                    (BinTree2Tree (cdr bt)))
        )
  )
)

;; Same with fold-right:
(define (*A057123 s) (fold-right (lambda (x y) (list (*A057123 x) y)) '() s))
(define (*A057123v2 s) (fold-right (lambda (x y) (cons (*A057123 x) (cons y '()))) '() s))

(define (Tree2BinTree_if_possible gt) ;; See A083927
  (call-with-current-continuation
    (lambda (e)
     (let recurse ((gt gt))
        (cond ((not (pair? gt)) gt)
              ((eq? 2 (length gt))
                    (cons (recurse (car gt))
                          (recurse (cadr gt))
                    )
              )
              (else (e '()))
        )
     ) ;; let
    )
  )
)



(define (bud! s i) (replace-nth-leaf! s i (list (list))))

(define (replace-nth-leaf! s i scion)
  (cond
    ((> i (count-pars s)) '()) ;; Index too big, return ().
    ((null? s) scion) ;; Note: not a destructive operation.
    (else
       (let ((leafs-to-visit i))
         (call-with-current-continuation
          (lambda (exit)
           (let fork ((s s))
              (cond ((null? (cdr s))
                       (if (zero? leafs-to-visit)
                           (exit (set-cdr! s scion))
                           (set! leafs-to-visit (-1+ leafs-to-visit))
                       )
                    )
                    (else (fork (cdr s)))
              )
              (cond ((null? (car s))
                       (if (zero? leafs-to-visit)
                           (exit (set-car! s scion))
                           (set! leafs-to-visit (-1+ leafs-to-visit))
                       )
                    )
                    (else (fork (car s)))
              )
           )
          )
         )
         s
       )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "Sort-swap" a binary tree, in a such a way, that in every branch
;; the right-hand-side tree is equivalent or "greater than" the
;; left hand side tree, in A014486 ordering:

(define (*A153835! s)
  (cond ((pair? s) 
             (*A153835! (car s)) ;; Sort the left branch
             (*A153835! (cdr s)) ;; and the right branch
             (if (= 1 (A154103bi (parenthesization->A014486 (car s))
                                 (parenthesization->A014486 (cdr s))
                      )
                 )
                 (*A069770! s)
             )
        )
  )
  s
)


;; This would give (A153835old 1245) = 1526
;;
;; (define (*A153835old_def! s)
;;   (cond ((pair? s) 
;;              (*A153835! (car s)) ;; Sort the left branch
;;              (*A153835! (cdr s)) ;; and the right branch
;;              (if (< (CatalanRankGlobal (parenthesization->A014486 (cdr s)))
;;                     (CatalanRankGlobal (parenthesization->A014486 (car s)))
;;                  )
;;                  (*A069770! s)
;;              )
;;         )
;;   )
;;   s
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Contract ((A) . B) -> (A . B) and (B . (() . A)) -> (B . A)
;; when A is not ().
;; Note the perfect symmetry.
(define (BranchReducedZigzagTree! bt) ;; As defined by Donaghey
  (cond
    ((pair? bt)
      (cond ((and (pair? (car bt)) (not (null? (caar bt))) (null? (cdar bt)))
               (set-car! bt (caar bt))
               (BranchReducedZigzagTree! bt)
            )
            ((and (pair? (cdr bt)) (not (null? (cddr bt))) (null? (cadr bt)))
               (set-cdr! bt (cddr bt))
               (BranchReducedZigzagTree! bt)
            )
            (else
               (BranchReducedZigzagTree! (car bt))
               (BranchReducedZigzagTree! (cdr bt))
            )
      )
    )
  )
  bt
)

;; Safer version of it:
(define (BranchReducedZigzagTree bt) (BranchReducedZigzagTree! (copy-tree bt)))

;; Note that we get (nth-complete-binary-tree n)
;; if we iterate this n times starting from ():
(define (BinTree2ZigzagTree bt) ;, cf. A080298.
  (cond ((not (pair? bt)) (cons bt bt))
        (else (cons (BinTree2ZigzagTree (car bt))
                    (BinTree2ZigzagTree (cdr bt)))
        )
  )
)

;; Inverse for above: A083928
(define (ZigzagTree2BinTree_if_possible gt)
  (call-with-current-continuation
    (lambda (e)
     (let recurse ((gt gt))
        (cond ((equal? gt '(() . ())) (list))
              ((not (pair? gt)) (e '()))
              (else
                    (cons (recurse (car gt))
                          (recurse (cdr gt))
                    )
              )
        )
     ) ;; let
    )
  )
)

;; These two transferred 2012-11-12 from gatomain.scm:
;;

(define (nth-unbranching-tree-of-size n size)
  (let ((root (list (list))))
    (let loop ((n n) (size size) (t root))
       (cond ((zero? size) root)
             ((even? n)
                (set-cdr! t (list (list)))
                (loop (/ n 2) (- size 1) (cdr t))
             )
             (else ;; n is odd.
                (set-car! t (list (list)))
                (loop (/ (- n 1) 2) (- size 1) (car t))
             )
       )
    )
  )
)


(define (add-bud-for-the-nth-unbranching-tree-with-car-cdr-code! tree n)
    (let loop ((n n) (t tree))
       (cond ((zero? n) (list))
             ((= n 1) (list (list)))
             ((= n 2) (set-cdr! t (list (list))))
             ((= n 3) (set-car! t (list (list))))
             ((even? n)
                (loop (/ n 2) (cdr t))
             )
             (else ;; n is odd.
                (loop (/ (- n 1) 2) (car t))
             )
       )
    )
    tree
)


;; Here are the 10 A-numbers you requested: A146888 --- A146897.

;; See http://www.research.att.com/~njas/sequences/A111713

;; Cannon, Floyd, and Parry. Notes on Richard Thompson's Groups F and T:
;; http://www.geom.uiuc.edu/docs/preprints/lib/GCG63/thompson.ps

;; And:
;; J.W. Cannon, W.J. Floyd, and W.R. Parry, Introductory Notes on Richard Thompson's Groups, L'Enseignement Mathematique, 42 (1996), pp. 215-256.

;; This is not ready at all!

(define (Bintree2Exponents bt)
     (let fork ((bt bt) (e -1))
        (cond ((puuttuu !) ())
              ((not (pair? gt)) (e '()))
              (else
                    (cons (fork (car bt) (+ e 1))
                          (fork (cdr bt) e)
                    )
              )
        )
     ) ;; let

)


;; The definition of A072764bi transferred from gatoaltr.scm 2012-11-11:

(define (A072764bi x y)
   (CatalanRankGlobal
       (parenthesization->A014486
            (cons (A014486->parenthesization (A014486 x))
                  (A014486->parenthesization (A014486 y))
            )
       )
   )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Only the (define A057548 (catfun1 list)) will then erroneously
;; give 0, instead of 1, if we initialize the cache like this:

(define (new-cat-cache)
;; No insidious hidden zeroes!
;; (vector 0 #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
   (vector #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
)

(define *MAX-CACHE-SIZE-FOR-CATMORFS* 131072) ;; 524292) ;; Was: 290512


(define-syntax grow-cache
  (syntax-rules ()
   ((grow-cache cachename arg) ;; No maxsize specified.
      (vector-grow cachename (max (1+ arg) (* 2 (vector-length cachename))))
   )
   ((grow-cache cachename arg 0) ;; Or specified as zero.
      (vector-grow cachename (max (1+ arg) (* 2 (vector-length cachename))))
   )
   ((grow-cache cachename arg maxsize)
      (vector-grow cachename (min maxsize (max (1+ arg) (* 2 (vector-length cachename)))))
   )
  )
)

(define-syntax implement-cached-function
  (syntax-rules ()
   ((implement-cached-function maxcachesize (funname argname) e0 ...)
        (letrec
           ((_cache_ (new-cat-cache))
            (funname
             (lambda (argname)
               (cond ((null? argname) _cache_) ;; For debugging.
                     ((vector? argname) argname) ;; As well as this: Caches for caches!
                     ((and (not (= 0 maxcachesize)) (>= argname maxcachesize))
                          e0 ...
                     )
                     (else
                         (if (>= argname (vector-length _cache_))
                             (set! _cache_ (grow-cache _cache_ argname maxcachesize))
                         )
                         (or (vector-ref _cache_ argname)
                             ((lambda (res)
                                (vector-set! _cache_ argname res)
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
          funname
        ) ; letrec
   )
  )
)


(define (catfun1 morphism)
  (implement-cached-function *MAX-CACHE-SIZE-FOR-CATMORFS* (fun_name n)
        (CatalanRankSexp (morphism (CatalanUnrankSexp n)))
  )
)

(define (catfun0 morphism) ;; only for telescoping (self-embeddable) automorphisms,
;; like df->bf or deeprevoncarside...
     (letrec ((cache (new-cat-cache))
              (fun (lambda (y)
                 (cond ((null? y) cache) ; For debugging, reveal our cache for nothing.
                       (else
                           (if (>= y (vector-length cache))
                               (set! cache (vector-grow cache
                                             (max (1+ y)
                                                  (* 2 (vector-length cache))
                                             )
                                           )
                               )
                           )
                           (or (vector-ref cache y)
                               (vector-set-and-return-value! cache y
                                     (CatalanRankLocal
                                       (parenthesization->A014486
                                         (morphism
                                           (A014486->parenthesization
                                             (CatalanUnrank (A081288nc y) y)
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
             )
         fun
     )
)


(define (catfun1restricted_to_bt_subset morphism)
     (letrec ((morphism_restricted_to_bt_subset
                (lambda (s)
                    (Tree2BinTree_if_possible (morphism (BinTree2Tree s)))
                )
              )
              (cache (new-cat-cache))
              (fun (lambda (y)
                 (cond ((null? y) cache) ; For debugging, reveal our cache for nothing.
                       (else
                           (if (>= y (vector-length cache))
                               (set! cache (vector-grow cache
                                             (max (1+ y)
                                                  (* 2 (vector-length cache))
                                             )
                                           )
                               )
                           )
                           (or (vector-ref cache y)
                               (vector-set-and-return-value! cache y
                                     (CatalanRankGlobal
                                       (parenthesization->A014486
                                         (morphism_restricted_to_bt_subset
                                           (A014486->parenthesization
                                             (A014486 y)))))
                               )
                           )
                       )
                 )
                )
              )
             )
         fun
     )
)

;; Like previous, but there's an extra stem beneath the binary tree,
;; i.e. it's a proper trivalent-plane-tree:
(define (catfun1restricted_to_tpt_subset morphism)
     (letrec ((morphism_restricted_to_tpt_subset
                (lambda (s)
                  (Tree2BinTree_if_possible (car (morphism (list (BinTree2Tree s)))))
                )
              )
              (cache (new-cat-cache))
              (fun (lambda (y)
                 (cond ((null? y) cache) ; For debugging, reveal our cache for nothing.
                       (else
                           (if (>= y (vector-length cache))
                               (set! cache (vector-grow cache
                                             (max (1+ y)
                                                  (* 2 (vector-length cache))
                                             )
                                           )
                               )
                           )
                           (or (vector-ref cache y)
                               (vector-set-and-return-value! cache y
                                     (CatalanRankGlobal
                                       (parenthesization->A014486
                                         (morphism_restricted_to_tpt_subset
                                           (A014486->parenthesization
                                             (A014486 y)))))
                               )
                           )
                       )
                 )
                )
              )
             )
         fun
     )
)

(define (catfun2 par2int-fun)
     (letrec ((cache (new-cat-cache))
              (fun (lambda (y)
                 (cond ((null? y) cache) ; For debugging, reveal our cache for nothing.
                       (else
                           (if (>= y (vector-length cache))
                               (set! cache (vector-grow cache
                                             (max (1+ y)
                                                  (* 2 (vector-length cache))
                                             )
                                           )
                               )
                           )
                           (or (vector-ref cache y)
                               (vector-set-and-return-value! cache y
                                         (par2int-fun
                                           (A014486->parenthesization
                                             (A014486 y)
                                           )
                                         )
                               )
                           )
                       )
                 )
                )
              )
             )
         fun
     )
)

(define (apply_upto_n upto_n morphism) ;; For testing.
   (map CatalanRankGlobal
       (map parenthesization->A014486
            (map morphism
                 (map A014486->parenthesization
                      (map A014486 (iota0 upto_n))))))
)


(define (binseqs_of_size size)
    (map (lambda (r) (CatalanUnrank size r))
         (iota0 (-1+ (A000108 size)))
    )
)

(define (partition_by_gatomorphism size gatomorphism)
   (let ((src_set (map A014486->parenthesization (binseqs_of_size size)))
        )
     (let loop ((cur (car src_set)) (src src_set) (res (list (list))))
        (cond ((null? src) (reverse! (map reverse! res)))
              ((member cur src)
                 (loop (gatomorphism cur)
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


;; Return a Cycle-Count function for a particular gatomorphism.
;; (This one is quite ineffective. See the module gatosiga.scm for a better one.)

(define (cc-fun gatomorphism)
  (lambda (n)
    (length (partition_by_gatomorphism n gatomorphism))
  )
)


(define (fixed-by-gatomorphism-slow size gatomorphism)
   (keep-matching-items
        (map A014486->parenthesization (binseqs_of_size size))
        (lambda (p) (equal? (gatomorphism p) p))
   )
)

(define (fixed-by-gatomorphism size gatomorphism)
  (let ((r (- (A000108 size) 1)))
   (let loop ((r r)
              (fps (list))
             )
      (let ((p (A014486->parenthesization (CatalanUnrank size r))))
       (cond ((< r 0) fps)
             ((equal? (gatomorphism p) p) (loop (- r 1) (cons p fps)))
             (else                        (loop (- r 1) fps))
       )
      )
   )
  )
)


(define (indices-of-nth-forest n)
  (if (zero? n) (list n)
      (map (lambda (x) (+ (A014137 (-1+ n)) x)) (iota0 (-1+ (A000108 n))))
  )
)

(define (partition-by-gatoAfun size gatomorphism)
   (let ((src_set (indices-of-nth-forest size)))
     (let loop ((cur (car src_set)) (src src_set) (res (list (list))))
        (cond ((null? src) (reverse! (map reverse! res)))
              ((member cur src)
                 (loop (gatomorphism cur)
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

(define (fc-Afun Afun) (lambda (n) (number-of-1-cycles (partition-by-gatoAfun n Afun))))

(define (cc-Afun Afun) (lambda (n) (length (partition-by-gatoAfun n Afun))))

(define (mc-Afun Afun)
  (lambda (n) (apply max (map length (partition-by-gatoAfun n Afun))))
)


(define (lc-Afun Afun)
  (lambda (n) (apply lcm (map length (partition-by-gatoAfun n Afun))))
)





(define (compute-and-print-count-seqs Afun outfile upto-n)
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
                (let ((partlengths (map length (partition-by-gatoAfun (1+ n) Afun))))
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


(define (compute-and-print-cycle-vectors Afun outfile upto-n)
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
                 (loop (1+ n) (map length (partition-by-gatoAfun (1+ n) Afun)))
              )
            )
          )
       )
     )
   )
)

