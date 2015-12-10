
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;;  http://www.iki.fi/~kartturi/matikka/Nekomorphisms/gatogenp.scm      ;;
;;                                                                      ;;
;;  Generate Prolog-clauses that are candidates for non-recursive       ;;
;;  gatomorphisms.                                                      ;;
;;                                                                      ;;
;;  This Scheme-code is coded 2003 by Antti Karttunen,                  ;;
;;  (E-mail: <my_firstname>.<my_surname>@iki.fi) and is placed in       ;;
;;  Public Domain.                                                      ;;
;;                                                                      ;;
;;  All the examples run at least in MIT Scheme Release 7.7.1, for      ;;
;;  which one can find documentation and the pre-compiled binaries      ;;
;;  (for various OS's running in Intel x86 architecture) under the URL: ;;
;;                                                                      ;;
;;  http://www.swiss.ai.mit.edu/projects/scheme/                        ;;
;;                                                                      ;;
;;  The main pointer for this code collection is:                       ;;
;;  http://www.iki.fi/~kartturi/matikka/Nekomorphisms/gatomorf.htm      ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;;
;; Gatomorphism (noun) = any bijection from a set of parenthesizations   ;;
;; of size n to the same set (of size n), which is well-defined for      ;;
;; all the sizes n (for sizes n=0 and 1 we have an identity mapping).    ;;
;; In place of parenthesizations we can assume any other manifestation   ;;
;; of the exercise 19 by Stanley.                                        ;;
;;                                                                       ;;
;; See R. P. Stanley, Exercises on Catalan and Related Numbers,          ;;
;; located at: http://www-math.mit.edu/~rstan/ec/catalan.pdf             ;;
;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; From http://www.swiss.ai.mit.edu/projects/scheme/documentation/user_5.html
;;
;; If you redefine some global name in your code, for example, car, cdr
;; and cons, you should indicate it in the declaration as:
;; (declare (usual-integrations car cdr cons))

;; (Beware of using an argument name like list in the function definitions?)

(declare (usual-integrations))

;; Compile as:
;; (cf "../Nekomorphisms/gatogenp" "../Nekomorphisms/")
;;


;; A082858 and A082860 copied from gatoaltr.scm:

;; Construct the greatest common subtree (intersect) of two binary trees.
(define (GCSB t1 t2)
   (cond ((or (not (pair? t1)) (not (pair? t2))) (list))
         (else (cons (GCSB (car t1) (car t2))
                     (GCSB (cdr t1) (cdr t2))
               )
         )
   )
)

;; Construct the least common supertree (union) of two binary trees.
(define (LCSB t1 t2)
   (cond ((and (not (pair? t1)) (not (pair? t2))) (list))
         (else (cons (LCSB (car* t1) (car* t2))
                     (LCSB (cdr* t1) (cdr* t2))
               )
         )
   )
)


;; Construct the list of subtrees where the binary tree t1 grows
;; past the tips of the tree t2:
;; The positions in the constructed list correspond to the
;; tips (leaves) of t2 in preorder.
;; I.e. the length of the resulting list is the number of
;; leaves (NILs) in t2.

;; (bintree-difference () ())                               --> (())
;; (bintree-difference '(() . ()) '(()))                    --> (() ())
;; (bintree-difference '(() . (() . ())) '(() . ()))        --> (() (()))
;; (bintree-difference '((() . ()) . ()) '(() . ()))        --> ((()) ())
;; (bintree-difference '(() . (() . ())) '(() . (() . ()))) --> (() () ())
;; (bintree-difference '(() . (() . ())) '((() . ()) . ())) --> (() () (()))

(define (bintree-difference t1 t2)
  (let ((res (list)))
    (let recurse ((t1 t1) (t2 t2))
       (cond ((not (pair? t2))
                 (set! res (cons t1 res))
             )
             (else
                    (recurse (if (pair? t1) (car t1) (list)) (car t2))
                    (recurse (if (pair? t1) (cdr t1) (list)) (cdr t2))
             )
       )
    )
    (reverse! res)
  )
)


(define (bintree-diff t1 t2)
    (map CatalanRankSexp
         (bintree-difference (CatalanUnrankSexp t1)
                             (CatalanUnrankSexp t2)
         )
    )
)

;; Like above, but use global ranks of the corresponding S-exps,
;; with negative ranks for those cases where the tree t2 grows
;; past the tree t1: (with respect to their greatest common subtree.)

;; (bintree-sym-diff () ())    --> (0)
;; (bintree-sym-diff '(() . ()) '(()))  --> (0 0)
;; (bintree-sym-diff '(() . (() . ())) '(() . ())) --> (0 1)
;; (bintree-sym-diff '(() . ()) '(() . (() . ()))) --> (0 -1)
;; (bintree-sym-diff '((() . ()) . ()) '(() . ())) --> (1 0)
;; (bintree-sym-diff '(() . (() . ())) '(() . (() . ()))) --> (0 0 0)
;; (bintree-sym-diff '(() . (() . ())) '((() . ()) . ())) --> (-1 1)
;; (bintree-sym-diff '((() . ()) . ()) '(() . (() . ()))) --> (1 -1)
;; (bintree-sym-diff '((). (() . (() . (() . ()))) . ())
;;                    '(((()(())()) . (() . ()))))
;;   --> (-137 4)


(define (bintree-sym-diff t1 t2)
  (let ((res (list)))
    (let recurse ((t1 t1) (t2 t2))
       (cond ((not (pair? t2))
                 (set! res (cons (CatalanRankSexp t1) res))
             )
             ((not (pair? t1))
                 (set! res (cons (- (CatalanRankSexp t2)) res))
             )
             (else
                    (recurse (if (pair? t1) (car t1) (list)) (car t2))
                    (recurse (if (pair? t1) (cdr t1) (list)) (cdr t2))
             )
       )
    )
    (reverse! res)
  )
)

(define (A082858bi x y)
   (CatalanRankGlobal
      (parenthesization->A014486
          (GCSB (A014486->parenthesization (A014486 x))
                (A014486->parenthesization (A014486 y))
          )
      )
   )
)

(define (A082858 n)
   (CatalanRankGlobal
      (parenthesization->A014486
          (GCSB (A014486->parenthesization (A014486 (A025581 n)))
                (A014486->parenthesization (A014486 (A002262 n)))
          )
      )
   )
)


(define (A082860bi x y)
   (CatalanRankGlobal
      (parenthesization->A014486
          (LCSB (A014486->parenthesization (A014486 x))
                (A014486->parenthesization (A014486 y))
          )
      )
   )
)

(define (A082860 n)
   (CatalanRankGlobal
      (parenthesization->A014486
          (LCSB (A014486->parenthesization (A014486 (A025581 n)))
                (A014486->parenthesization (A014486 (A002262 n)))
          )
      )
   )
)




;; In ../Schemuli/intfuns1.scm :
;;
;; (map binexp->runcount1list (iota0 16))
;; --> (() (1) (1 1) (2) (1 2) (1 1 1) (2 1) (3) (1 3)
;;      (1 2 1) (1 1 1 1) (1 1 2) (2 2) (2 1 1) (3 1) (4) (1 4))

;; s = rank of the source tree from 0 to Cat(r)
;; d = rank of the destin tree from 0 to Cat(r)
;; p = rank of the permutation from 0 to (n+1)!-1
;;
;;      r r   s d p   s d p
;; (24 (2 3) (0 0 0) (0 0 0))
;; (24 (2 3) (0 0 0) (0 0 1))
;; ...
;; (24 (2 3) (0 0 0) (0 0 23))
;; (24 (2 3) (0 0 0) (0 1 0))
;; ...
;; (24 (2 3) (0 0 0) (0 1 23))
;; ...
;; (24 (2 3) (0 0 0) (0 4 0))
;; ...
;; (24 (2 3) (0 0 0) (0 4 23))
;; (24 (2 3) (0 0 0) (1 0 0))
;; ...
;; ...
;; (24 (2 3) (0 0 0) (4 4 23))
;; (24 (2 3) (0 0 1) (0 0 0))
;; .
;; .
;; (24 (2 3) (0 0 1) (4 4 23))
;; .
;; .
;; .
;; (24 (2 3) (1 1 5) (4 4 23))
;; (25 (2 2 1) (0 0 0) (0 0 0) (0 0 0))
;; (25 (2 2 1) (1 1 5) (1 1 5) (0 0 1))
;;

;; Test-structure is of this format:
;; (n <runcountlist> <triplet1> <triplet2> ...)

(define (triplet-wrapped-around!? triplet size)
  (let* ((s-loc triplet)
          (d-loc (cdr s-loc))
          (p-loc (cdr d-loc))
         )
   (cond ((< (car p-loc) (-1+ (! (1+ size))))
            (set-car! p-loc (1+ (car p-loc)))
            #f
         )
         ((< (car d-loc) (-1+ (A000108 size)))
            (set-car! d-loc (1+ (car d-loc)))
            (set-car! p-loc 0)
            #f
         )
         ((< (car s-loc) (-1+ (A000108 size)))
            (set-car! s-loc (1+ (car s-loc)))
            (set-car! d-loc 0)
            (set-car! p-loc 0)
            #f
         )
         (else ;; Wasn't incrementable anymore, zero everything.
            (set-car! s-loc 0)
            (set-car! d-loc 0)
            (set-car! p-loc 0)
            #t ;; And return #t for the caller as a sign that we wrapped over.
         )
   )
  )
)

;; Runs as:
;;  (0 ())
;;  (1 (1) (0 0 0))
;;  (1 (1) (0 0 1))
;;  (2 (1 1) (0 0 0) (0 0 0))
;;  (2 (1 1) (0 0 1) (0 0 0))
;;  (2 (1 1) (0 0 0) (0 0 1))
;;  (2 (1 1) (0 0 1) (0 0 1))
;;  (3 (2) (0 0 0))
;;  (3 (2) (0 0 1))
;;  (3 (2) (0 0 2))
;;  (3 (2) (0 0 3))
;;  (3 (2) (0 0 4))
;;  (3 (2) (0 0 5))
;;  (3 (2) (0 1 0))
;;  ...
;;  ...
;;  ...
;;  (3 (2) (1 1 4))
;;  (3 (2) (1 1 5))
;;  (4 (1 2) (0 0 0) (0 0 0))
;;  (4 (1 2) (0 0 1) (0 0 0))
;;  (4 (1 2) (0 0 0) (0 0 1))
;;  (4 (1 2) (0 0 1) (0 0 1))
;;  ...
;;  ...
;;  (4 (1 2) (0 0 0) (0 0 5))
;;  (4 (1 2) (0 0 1) (0 0 5))
;;  (4 (1 2) (0 0 0) (0 1 0))
;;  (4 (1 2) (0 0 1) (0 1 0))
;;  ...

(define (next-triplet-structure! ts)
  (let ((n-triplets-org (length (cddr ts)))
       )
;; Increment triplets with an odometer-principle, from left to right:
          (let loop ((sizes (cadr ts))
                     (triplets (cddr ts))
                    )
              (cond ((null? triplets)
                       (set-car! ts (1+ (car ts)))
                       (set-car! (cdr ts) (binexp->runcount1list (car ts)))
                       (let ((n-triplets-now (length (cadr ts))))
                          (cond ((<= n-triplets-now n-triplets-org) ;; We shorten them...
                                   (set-cdr! (cdr ts) (nthcdr (- n-triplets-org n-triplets-now) (cddr ts)))
                                )
                                (else ;; we have to add more zero-triplets there...
                                   (let insloop ((how-many (- n-triplets-now n-triplets-org)))
                                         (cond ((> how-many 0)
                                                  (set-cdr! (cdr ts)
                                                            (cons (list 0 0 0)
                                                                  (cddr ts)
                                                            )
                                                  )
                                                  (insloop (-1+ how-many))
                                               )
                                         )
                                   )
                                )
                          )
                       )
                       ts
                    )
                    ((triplet-wrapped-around!? (car triplets) (car sizes))
                       (loop (cdr sizes) (cdr triplets))
                    )
                    (else ts)
              )
          )
  )
)


(define (global-ranks-of-source-trees sizes triplets)
    (map (lambda (size tr) (+ (A014137 (-1+ size)) (car tr)))
         sizes
         triplets
    )
)


;; If the source tree of a later clause is a super-tree of the
;; source-tree of an earlier clause (here in the cdr-direction of globranks)
;; then it is shadowed by that earlier clause, and is thus no-op.

(define (shadowed-clauses? globranks)
    (let loop ((g globranks))
       (cond ((null? g) #f)
             ((find-matching-item (cdr g) (lambda (r) (= (A082858bi (car g) r) r))))
;;           ((there-exists? (cdr g) (lambda (r) (= (A082858bi (car g) r) r))))
             (else (loop (cdr g)))
       )
    )
)

(define (nothing-filtered outport candname ts) #f)

;; Just by skipping over identity stand-alone definitions,
;; single non-stand-alone definitions, and
;; definitions with non-terminal single-node clause(s),
;; we had the following reduction of candidates
;; when we computed upto n=31:
;; %% Iterated over 1431951 candidates, of which 1321144 were filtered and 110807 did remain.
;; When we add the filter-all-loners? condition, we get:
;; %% Iterated over 1431951 candidates, of which 1353134 were filtered and 78817 did remain.
;; (Now it's enough to check only up to n=28).
;; When we also add the shadowed-claused filtering, and computed up to 28, we get:
;; %% Iterated over 112431 candidates, of which 43118 were filtered and 69313 did remain.
;;
;; Then for multiclause gatomorphisms, one clause can be constructed
;; in Cat(n)*Cat(n)*(n+1)! (= A001246[n]*A000142[n+1] = A001813[n]*A000108[n])
;; ways: 1,2,24,600,23520,1270080,87816960,7420533120,742053312000 (not in EIS)
;; and taking Cameron's inverse of this sequence (from a(n>=1) = 2 onward):
;; INVERT([seq(Cat(n)*Cat(n)*(n+1)!,n=1..9)]);
;; --> [2,28,704,26800,1404416,94890112,7887853568,779773444864,89407927009280,...]
;; we get an absolute upper bound for number of (distinct) simple nonrecursive
;; gatomorphisms of total n opening (closing) conses.



(define (filter0! outport candname ts) (filter-gen! outport candname ts #f))

(define (filter1! outport candname ts) (filter-gen! outport candname ts #t))

;; This one filters everything which is not a stand-alone, non-identity loner:
(define (filter2! outport candname ts)
   (let* ((n (car ts))
          (sizes (cadr ts))
          (triplets (cddr ts))
          (n-nodes (fold-left + 0 sizes))
         )
      (cond ((zero? n) #f) ;; Do not filter!
            ((= 1 (length sizes))
               (let* ((only-tr (car triplets))
                      (s-rank (car only-tr))
                      (d-rank (cadr only-tr))
                      (p-rank (caddr only-tr))
                     )
                  (cond ((not (= s-rank d-rank))
                           (format outport "% Skipping single ~A node non-stand-alone definition ~A.~%"
                                   n-nodes candname
                           )
                           (next-triplet-structure! ts)
                        )
                        ((zero? p-rank)
                           (format outport "% Skipping ~A node identity stand-alone definition ~A.~%"
                                   n-nodes candname
                           )
                           (next-triplet-structure! ts)
                        )
                        (else #f) ;; Otherwise, keep it.
                  )
               )
            )
            (else
              (format outport "% Skipping ~A node multi-clause definition ~A.~%"
                     n-nodes candname
              )
              (next-triplet-structure! ts)
            )
      )
   )
)


(define (filter-gen! outport candname ts filter-all-loners?)
   (let* ((n (car ts))
          (sizes (cadr ts))
          (triplets (cddr ts))
          (n-nodes (fold-left + 0 sizes))
         )
      (cond ((zero? n) #f) ;; Do not filter!
            ((= 1 (length sizes))
               (let* ((only-tr (car triplets))
                      (s-rank (car only-tr))
                      (d-rank (cadr only-tr))
                      (p-rank (caddr only-tr))
                     )
                  (cond (filter-all-loners?
                           (format outport "% Skipping ~A node single-clause definition ~A.~%"
                                   n-nodes candname
                           )
                           (next-triplet-structure! ts)
                        )
                        ((not (= s-rank d-rank))
                           (format outport "% Skipping single ~A node non-stand-alone definition ~A.~%"
                                   n-nodes candname
                           )
                           (next-triplet-structure! ts)
                        )
                        ((zero? p-rank)
                           (format outport "% Skipping ~A node identity stand-alone definition ~A.~%"
                                   n-nodes candname
                           )
                           (next-triplet-structure! ts)
                        )
                        (else #f)
                  )
               )
            )
            ((nthmemq 1 (cdr sizes))
               =>
                 (lambda (pos)
                     (format outport "% Skipping ~A node definition ~A because of non-terminal single-node clause(s).~%"
                             n-nodes candname
                     )
                     (next-triplet-structure! ts)
                 )
            )
            (else
               (let ((last-triplet (car triplets)) ;; Is first in our list.
                     (src-glob-ranks (global-ranks-of-source-trees sizes triplets))
                    )
                  (cond
                    ((and (= (car last-triplet) (cadr last-triplet))
                          (zero? (caddr last-triplet))
                     )
                       (format outport "% Skipping ~A node definition ~A because its last clause is stand-alone no-op (identity).~%"
                               n-nodes candname
                       )
                       (next-triplet-structure! ts)
                    )
                    ((shadowed-clauses? src-glob-ranks)
                       (format outport "% Skipping ~A node definition ~A because of shadowed clause(s).~%"
                               n-nodes candname
                       )
                       (next-triplet-structure! ts)
                    )
                    ((= 2 (length sizes))
                       (let* ((tr1 (cadr triplets))
                              (s1 (car tr1))
                              (d1 (cadr tr1))
                              (p1 (caddr tr1))
                              (tr2 (car triplets))
                              (s2 (car tr2))
                              (d2 (cadr tr2))
                              (p2 (caddr tr2))
                             )
                          (cond ((and (= s1 d1) (= s2 d2)
                                      (not (homogeneous-cycles?
                                              p2
                                              (apply bintree-diff
                                                     (reverse src-glob-ranks)
                                              )
                                           )
                                      )
                                 )
                                   (format outport "% Skipping ~A node two-stand-alone-clause definition ~A because it fails the simple injectivity check #1.~%"
                                           n-nodes candname
                                   )
                                   (next-triplet-structure! ts)
                                )
                                (else #f)
                          )
                       )
                    )
                    (else #f) ;; Do not filter!
                  )
               )
            )
      )
   )
)


;; Theorem:
;; If we have a two form-preserving clauses (i.e. s = d in both)
;; then it is a valid gatomorphism (a bijection) if and only if
;; in (bintree-diff t1 t2) each partition induced by the cycles of
;; the permutation of the second clause contains only identical
;; ranks. (i.e. 0's, 1's, etc.)
;;
;; (Here t1 is the source and dest tree of the first clause,
;;  and t2 is the s- and d-tree of the 2nd clause),
;;
;; For cases with more than two form-preserving clauses
;; it gets more complicated, as then for example the third
;; clause must be compared both to the second and the first.

;; Project: Extend this idea to other kind of clauses also...



(define (generate-prolog-code outfile upto_n filter!)
   (call-with-output-file outfile
     (lambda (outport)
       (let loop ((ts (list 0 (list)))
                  (total 0)
                  (names (list))
                 )
          (cond ((> (car ts) upto_n)
                    (format outport "~%~%%% Iterated over ~A candidates, of which ~A were filtered and ~A did remain.~%"
                             total (- total (length names)) (length names)
                    )
                    (for-each (lambda (candname) (output-prolog-applygat-clause outport candname))
                              (reverse names)
                    )
                    (for-each (lambda (candname) (output-prolog-testgat-clause outport candname 8))
                              (reverse names)
                    )
                )
                (else ;; i.e. (<= (car ts) upto_n)
                  (let ((candname (form-clause-name ts)))
                     (cond ((not (filter! outport candname ts))
                               (output-prolog-clauses
                                       outport
                                       candname
                                       (map form-clause (cadr ts) (cddr ts))
                               )
                               (loop (next-triplet-structure! ts)
                                     (1+ total)
                                     (cons candname names)
                               )
                            )
                            (else (loop ts ;; filter! has called (next-triplet-structure! ts)
                                        (1+ total)
                                        names
                                  )
                            )
                     )
                  )
                )
          )
       ) ;; let loop
     ) ;; lambda
   )
)

;; form-half-of-clause modified from (CatalanUnrankSexpAux size rank) present
;; in ./Nekomorphisms/gatorank.scm
;; See Frank Ruskey's thesis at:
;; http://www.cs.uvic.ca/~fruskey/Publications/Thesis/Thesis.html
;; especially the page 24:
;; http://www.cs.uvic.ca/~fruskey/Publications/Thesis/ThesisPage24.png
;; Note that also there the loop condition is given erroneously as m > 0,
;; while it should be m >= 0, i.e. we stop only when m goes negative.


(define (form-clause-name ts)
  (cond ((null? (cadr ts)) (format #f "c~A" (car ts)))
        (else
          (string-append
             (format #f "c~A" (car ts))
             (fold-left (lambda (prev n) (string-append prev (format #f "_~A" n))) "" (reverse (cadr ts)))
             (fold-left (lambda (prev tr)
                          (string-append prev
                                         (format #f "__~A_~A_~A" (car tr) (cadr tr) (caddr tr))
                          )
                        )
                        "_t"
                        (reverse (cddr ts))
             )
          )
        )
  )
)


(define *LEAVES* (list->vector '("L0" "L1" "L2" "L3" "L4" "L5" "L6" "L7" "L8" "L9" "L10" "L11" "L12" "L13" "L14" "L15" "L16")))
(define *XS* (list->vector '("X0" "X1" "X2" "X3" "X4" "X5" "X6" "X7" "X8" "X9" "X10" "X11" "X12" "X13" "X14" "X15")))
(define *YS* (list->vector '("Y0" "Y1" "Y2" "Y3" "Y4" "Y5" "Y6" "Y7" "Y8" "Y9" "Y10" "Y11" "Y12" "Y13" "Y14" "Y15")))

(define (form-clause size triplet)
  (let ((s-rank (car triplet))
        (d-rank (cadr triplet))
        (permrank (car (cddr triplet)))
       )
    (append!
        (reverse! (form-half-of-clause size s-rank *XS* *LEAVES*))
        (form-half-of-clause
                size d-rank *YS* (permute-A060118 *LEAVES* (1+ size) permrank)
        )
    )
  )
)


(define (form-half-of-clause size rank inodes leaves)
 (let ((sonstack (make-vector size))
       (clause (list))
      )
   (let loop ((m (-1+ size))        ;; The row on A009766
              (y size)              ;; The position on row m of A009766
              (rank rank)
              (c (CatTriangle (-1+ size) size))
              (rightson? #f)
              (sp 0)
              (ip 0)
              (lp 0)
             )
       (cond 
         ((negative? m)
           (cond ((not (null? clause)) ;; Add the last leaf in its place:
                      (set-car! (cddr (vector-ref sonstack sp)) (vector-ref leaves lp))
                 )
           )
           clause
         )
         (else
           (cond
             ((>= rank c)
                (let ((newbranch (list (vector-ref inodes ip) (list) (list))))
                   (cond
                     ((null? clause))
                     (rightson? (set-car! (cddr (vector-ref sonstack sp)) (vector-ref inodes ip)))
                     (else (set-car! (cdr (vector-ref sonstack sp)) (vector-ref inodes ip))
                           (set! sp (1+ sp))
                     )
                   ) ;; cond
                   (vector-set! sonstack sp newbranch)
                   (set! clause (cons newbranch clause))
                   (loop m
                         (-1+ y)
                         (- rank c)
                         (CatTriangle m (-1+ y))
                         #f      ;; Next time we have a left son.
                         sp      ;; sp already incremented above if needed.
                         (1+ ip) ;; Used one internal node name
                         lp      ;; but leaves are intact.
                   )
                ) ;; let
             )
             (else ;; It's the time for a leaf.
               (cond (rightson? (set-car! (cddr (vector-ref sonstack sp)) (vector-ref leaves lp)))
                     (else (set-car! (cdr (vector-ref sonstack sp)) (vector-ref leaves lp)))
               )
               (loop (-1+ m)
                     y
                     rank
                     (CatTriangle (-1+ m) y)
                     #t          ;; Next time we have a right son.
                     (- sp (if rightson? 1 0))
                     ip          ;; internal node labels not used
                     (1+ lp)     ;; but one leaf label was consumed.
               )
             )
           ) ;; cond
         ) ;; else
       ) ;; cond
   ) ;; let loop
 )
)



;; The following algorithm is a slight modification of unrank1
;; algorithm as presented by W. Myrvold and F. Ruskey, in
;; Ranking and Unranking Permutations in Linear Time,
;; Inform. Process. Lett. 79 (2001), no. 6, 281-284.
;; Available on-line: http://www.cs.uvic.ca/~fruskey/Publications/RankPerm.html

;; Same algorithm implemented in Haskell:

;; unrankA060118 n = unrankA060118x n 1 (idperm (1 + (factlen n)))

;; unrankA060118x :: Int -> Int -> Perm0Vec -> Perm0Vec
;; unrankA060118x 0 i p = p
;; unrankA060118x r i p = unrankA060118x (div r (i+1)) (i+1) (swapels p i (i-(mod r (i+1))))

;; (permute-a060118 (vector 'a 'b 'c 'd 'e 'f 'g) 0 0)  -->  #()
;; (permute-a060118 (vector 'a 'b 'c 'd 'e 'f 'g) 1 0)  -->  #(a)
;; (permute-a060118 (vector 'a 'b 'c 'd 'e 'f 'g) 2 0)  -->  #(a b)
;; (permute-a060118 (vector 'a 'b 'c 'd 'e 'f 'g) 2 1)  -->  #(b a)
;; (permute-a060118 (vector 'a 'b 'c 'd 'e 'f 'g) 3 0)  -->  #(a b c)
;; (permute-a060118 (vector 'a 'b 'c 'd 'e 'f 'g) 3 1)  -->  #(b a c)
;; (permute-a060118 (vector 'a 'b 'c 'd 'e 'f 'g) 3 2)  -->  #(a c b)
;; (permute-a060118 (vector 'a 'b 'c 'd 'e 'f 'g) 3 3)  -->  #(b c a)
;; (permute-a060118 (vector 'a 'b 'c 'd 'e 'f 'g) 3 4)  -->  #(c b a)
;; (permute-a060118 (vector 'a 'b 'c 'd 'e 'f 'g) 3 5)  -->  #(c a b)
;; (permute-a060118 (vector 'a 'b 'c 'd 'e 'f 'g) 4 0)  -->  #(a b c d)

;; And similarly:
;;   0  --> #(a b c d)
;;   1  --> #(b a c d)
;;   2  --> #(a c b d)
;;   3  --> #(b c a d)
;;   4  --> #(c b a d)
;;   5  --> #(c a b d)
;;   6  --> #(a b d c)
;;   7  --> #(b a d c)
;;   8  --> #(a c d b)
;;   9  --> #(b c d a)
;;   10  --> #(c b d a)
;;   11  --> #(c a d b)
;;   12  --> #(a d c b)
;;   13  --> #(b d c a)
;;   14  --> #(a d b c)
;;   15  --> #(b d a c)
;;   16  --> #(c d a b)
;;   17  --> #(c d b a)
;;   18  --> #(d b c a)
;;   19  --> #(d a c b)
;;   20  --> #(d c b a)
;;   21  --> #(d c a b)
;;   22  --> #(d b a c)
;;   23  --> #(d a b c)


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


(define (permute-A060118-lists elems size permrank)
  (vector->list (permute-A060118 (list->vector elems)
                                 size
                                 permrank
                )
  )
)


(define (pA060118 r)
  (let ((s (A084558 r)))
    (list->vector (permute-A060118-lists (iota0 s) (1+ s) r))
  )
)


(define (vector-swap! v i j)
  (let ((org-i (vector-ref v i)))
     (vector-set! v i (vector-ref v j))
     (vector-set! v j org-i)
  )
  v
)


;; Adapted from ../Schemuli/permfuns.scm

;; Finds the position of the last x in p, and #f if not found:
(define (vecnthmemq x p)
   (let loop ((i (vector-length p)))
          (cond ((zero? i) #f)
                ((= (vector-ref p (-1+ i)) x) (-1+ i))
                (else (loop (-1+ i)))
          )
   )
)

(define (perminv0 p)
    (let ((q (make-vector (vector-length p))))
       (let loop ((i (vector-length p)))
             (cond ((zero? i) q)
                   (else
                      (vector-set! q (-1+ i) (vecnthmemq (-1+ i) p))
                      (loop (-1+ i))
                   )
             )
       )
    )
)

;; # Modification of rank2 in Myrvold-Ruskey paper:
;; # q is inverse of p, both are given as permutation lists:
;; PermRank3Aux := proc(n, p, q)
;;  if(1 = n) then RETURN(0);
;;  else RETURN((n-p[n])*((n-1)!) + PermRank3Aux(n-1,swap(p,n,q[n]),swap(q,n,p[n]))); fi;
;; end;
;;
;; p must be a vector composed of integers 0 .. n-1:
(define (rank-by-A060117 p)
   (let permrank ((n (vector-length p))
                  (p p)
                  (q (perminv0 p))
                  (r 0)
                 )
          (cond ((<= n 1) r)
                (else
                  (let* ((n-1 (-1+ n))
                         (org-p-n (vector-ref p n-1))
                         (org-q-n (vector-ref q n-1))
                        )
                     (permrank n-1
                               (vector-swap! p n-1 org-q-n)
                               (vector-swap! q n-1 org-p-n)
                               (+ r (* (- n-1 org-p-n) (! n-1)))
                     )
                  )
                )
          )
   )
)

(define (rank-by-A060118 p)
   (let permrank ((n (vector-length p))
                  (p (perminv0 p))
                  (q p)
                  (r 0)
                 )
          (cond ((<= n 1) r)
                (else
                  (let* ((n-1 (-1+ n))
                         (org-p-n (vector-ref p n-1))
                         (org-q-n (vector-ref q n-1))
                        )
                     (permrank n-1
                               (vector-swap! p n-1 org-q-n)
                               (vector-swap! q n-1 org-p-n)
                               (+ r (* (- n-1 org-p-n) (! n-1)))
                     )
                  )
                )
          )
   )
)

;; (first-dislocated (map rank-by-A060118 (map pA060118 (iota0 625)))) --> ()
;; (map rank-by-A060117 (map pA060118 (iota0 25)))
;; --> (0 1 2 5 4 3 6 7 14 23 22 15 12 19 8 11 16 21 18 13 20 17 10 9 24 25) (= A060125)



;; (permute-a060118-lists (list 0 1 2 3 4) 5 12) --> (0 3 2 1 4)

;; (homogeneous-cycles? 12 (list 1 2 3 4 5)) --> (0 -2 0 2 0) --> #f
;; (homogeneous-cycles? 12 (list 1 7 3 7 5)) --> (0 0 0 0 0)  --> #t

(define (homogeneous-cycles? perm-rank items)
   (equal?
           items
           (permute-A060118-lists items (length items) perm-rank)
   )
)


(define (homogeneous-cycles-variant-coded-at-2-am? perm-rank items)
  (not
    (there-exists?
      (map (lambda (x y) (- x y))
           items
           (permute-A060118-lists items (length items) perm-rank)
      )
      (lambda (n) (not (zero? n)))
    )
  )
)



;; applygat(c1_1_t__0_0_1,X,Y) :-
;;   c1_1_t__0_0_1(X,Y).
(define (output-prolog-applygat-clause outport candname)
   (format outport "applygat(~A,X,Y) :-~%  ~A(X,Y).~%" candname candname)
)

;; E.g. like:
;; testgat(c1_1_t__0_0_1) :-
;;   checkUptoN(c1_1_t__0_0_1,8).

(define (output-prolog-testgat-clause outport candname upto_n)
   (format outport "testgat(~A) :-~%  checkUptoN(~A,~A).~%" candname candname upto_n)
)

(define (output-one-goal outport goal)
   (let ((c (car goal))
         (a (cadr goal))
         (b (caddr goal))
        )
     (format outport "  cons(~A,~A,~A),~%" a b c)
   )
)

(define (output-prolog-default-clause outport candname)
    (format outport "~%~A(X,X).~%~%~%" candname)
)

(define (output-prolog-clause outport candname clause)
  (let ((X-name (caar clause))
        (Y-name (caar (last-pair clause)))
        (clause-len-per-2 (/ (length clause) 2))
       )
    (format outport "~%~A(~A,~A) :-~%"
            candname
            X-name
            Y-name
    )
    (for-each (lambda (goal) (output-one-goal outport goal))
              (list-head clause clause-len-per-2)
    )
    (format outport "% --~%") ;; Midway separator, just for a nicer output.
    (for-each (lambda (goal) (output-one-goal outport goal))
              (nthcdr clause-len-per-2 clause)
    )
    (format outport "  !.~%") ;; Red cut in the end.
  )
)


(define (output-prolog-clauses outport candname clauses)
    (format outport "%%~%%%~%")
    (for-each (lambda (clause) (output-prolog-clause outport candname clause))
              (reverse clauses)
    )
    (output-prolog-default-clause outport candname)
)


;;
;; There are also other ways to prune the search space.
;; For example, it doesn't make sense to examine sequences
;; (if we are looking only for distinct, new gatomorphisms that
;; have not appeared for any smaller value of n)
;; where a later triplet has a (source) binary tree X_j
;; which is a super-tree of an earlier (source) binary tree X_i
;; (in other words X_i is a subtree of X_j, i.e. X_i = A082858(X_i,X_j)),
;; because then the latter clause is no-op, as the first clause
;; will always succeed for any tree which would succeed for the latter clause.
;; 
;; %             A   D
;; %              \ /
;; % A  D B  C     Q   B       A   B       []  A
;; %  \ / \ /       \ /         \ /         \ /           and by default:
;; %   P   M    -->  N   C       M  []  -->  N   B       []  A       []  A
;; %    \ /           \ /         \ /         \ /         \ /   -->   \ /
;; %     X             Y           X           Y           X           Y
;; 
;; 
;; %% Two non-default clauses with 3 & 2 = 5 opening (closing) conses:
;;
;; Generated names:
;;   XI1, XI2, XIn = Source Internal Node 1,2,...,n
;;   YI1, YI2, YIn = Destination Internal Node 1,2,...,n
;;   L1, L2, Ln    = Leaf 1,2,...,n
;;
;; In the first clause of a082351 we have of size 3
;;   a source tree of shape ((())())      (rank 1)
;;   a destination tree of shape (((()))) (= rank 4)
;;   and an identity permutation.
;;
;; In the second clause of a082351 we have of size 2
;;   a source tree of shape ((()))        (rank 1)
;;   a destination tree of the same shape (rank 1)
;;   and a permutation [3,1,2] on leaves.
;; 
;; a082351(X,Y) :-
;;   cons(XI1,XI2,X),
;;   cons(L1,L2,XI1),
;;   cons(L3,L4,XI2),
;; % --
;;   cons(L1,L2,YI2), %% Note: DI2 is equal to SI1.
;;   cons(YI2,L3,YI1),
;;   cons(YI1,L4,Y),
;;   !.
;; 
;; 
;; % Above clause implies that L3=[].
;;
;; a082351(X,Y) :-
;;   cons(X1,L3,X),
;;   cons(L1,L2,X1),
;; % --
;;   cons(L3,L1,Y1),
;;   cons(Y1,L2,Y),
;;   !.
;; 
;; ((cand_n X Y) (cons XI1 XI2 X) (cons L1 L2 XI1) (cons L3 L4 XI2)
;;               (cons L1 L2 YI2) (cons YI2 L3 YI1) (cons YI1 L4 Y)
;; )
;;
;; ((cand_n X Y) (cons X1 L3 X) (cons L1 L2 X1)   (cons L3 L1 Y1) (cons Y1 L2 Y))
;;


