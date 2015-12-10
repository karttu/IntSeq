
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;;                          gatocout.scm                              ;;
;;                                                                    ;;
;; Gatomorphism conversion functions (for output).                    ;;
;; Written by Antti Karttunen (firstname.surname@iki.fi) April, 2002  ;;
;; Last modified 22. Jun 2003.                                        ;;
;;                                                                    ;;
;; The functions convert s-expressions (which can be viewed either as ;;
;; parenthesizations or (planar & rooted) car/cdr-binary trees)       ;;
;; to structures better suited for the output of the certain          ;;
;; manifestations of Stanley's exercise 19.                           ;;
;; This module is independent of the actual graphics library/output   ;;
;; device used.                                                       ;;
;;                                                                    ;;
;; This file is located under:                                        ;;
;; http://www.iki.fi/~kartturi/matikka/Nekomorphisms/gatomorf.htm     ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; From http://www.swiss.ai.mit.edu/projects/scheme/documentation/user_5.html
;;
;; If you redefine some global name in your code, for example, car, cdr
;; and cons, you should indicate it in the declaration as:
;; (declare (usual-integrations car cdr cons))

;; (Beware of using an argument name like list in the function definitions?)

;; (declare (usual-integrations))  ;; For compilation in MIT Scheme.


;; Transferred from gatorank.scm:
;; (p->Lw p) converts a parenthesization 'p'to Lukasiewicz word
;; (in car-branch first, depth-first order, discarding the last zero).
;;

;; (output_seq (map (lambda (L) (baselist->n 10 L)) (map p->Lw (map BinTree2Tree (map A014486->parenthesization (map A014486 (iota0 64)))))))
;; --> 0,20,2020,2200,202020,202200,220020,220200,222000,... gives A071152.

;; (output_seq (map (lambda (L) (baselist->n 10 L)) (map p->Lw (map A014486->parenthesization (map A014486 (iota0 64))))))
;; --> 0,1,20,11,300,201,210,120,111,... gives A071153.

;; (output_seq (sort (map (lambda (L) (baselist->n 10 L)) (map p->Lw (map A014486->parenthesization (map A014486 (iota0 64))))) <))
;; -> 0,1,11,20,111,120,201,210,300,... gives A071154.

;; Calling this for the Figure 1: (A plane tree) shown in Stanley's
;; Hipparchus, Plutarch, Schröder and Hough, Am. Math. Monthly, Vol. 104,
;; No. 4, p. 344, 1997. (See http://www-math.mit.edu/~rstan/papers.html)
;; we obtain:
;;
;; (p->Lw '((() ()) ( (()()()()()) () (()()) ) (() () (() ()))))
;; (3 2 0 0 3 5 0 0 0 0 0 0 2 0 0 3 0 0 2 0)

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



;; From the interpretation d (plane binary trees with 2n+1 vertices (i.e. n+1 endpoints))
;; to the interpretation a (triangularizations of a convex (n + 2)-gon
;; into n triangles by n-1 diagonals that do not intersect in their interiors).
;;
;; (A014486->parenthesization (A014486 1)) --> (())
;; (bt->pt '(()))       --> ((1 . 2) (2 . 3) (1 . 3))
;; (A014486->parenthesization (A014486 2)) --> (() ())
;; (bt->pt '(() ()))    --> ((1 . 2) (2 . 3) (3 . 4) (2 . 4) (1 . 4))
;; (A014486->parenthesization (A014486 3)) --> ((()))
;; (bt->pt '((())))     --> ((1 . 2) (2 . 3) (1 . 3) (3 . 4) (1 . 4))
;; (A014486->parenthesization (A014486 4)) --> (() () ())
;; (bt->pt '(() () ())) --> ((1 . 2) (2 . 3) (3 . 4) (4 . 5) (3 . 5) (2 . 5) (1 . 5))

;; Note that the recursion order is important
;; (car-branch before the cdr-branch) because
;; of the side-effects of c we are playing with.
;; The largest n upto which we have drawn the edge segments
;; can always be found from the cdr-part of the first
;; pair in c, i.e. with (cdar c)
;;
;; The corners of the polygon are numbered clockwise
;; from 1 to n (where n = the number of leaves (here: ()'s)
;; in the binary tree + 1 for the root fork), so that
;; the corner 1 is at the left end and the corner n is
;; at the right end of the bottom ('root') edge
;; of the polygon.

(define (bt->pt bt)
  (let ((c (list (cons 0 1))))
     (let recurse ((bt bt) (sel +)) ;; + is playing the role of id. Not used.
        (cond ((not (pair? bt)) ;; A leaf -> The next onto edge.
                 (attach! (cons (cdar c) (1+ (cdar c))) c)
              )
              (else ;; It's a fork, so we need a diameter.
                 (let ((left-min (recurse (car bt) min)))
                    (attach! (cons left-min (recurse (cdr bt) max)) c)
                 )
              )
        )
        (sel (caar c) (cdar c))
     )
     (cdr (reverse! c))
  )
)

;; From the interpretation n (non-crossing handshakes, i.e. nonintersecting
;; chords joining 2n points on the circumference of a circle) to the
;; interpretation kk (fixed-point free and non-crossing involutions of [2n]):

;; (sexp->hs '())             -> ()
;; (sexp->hs '(()))           -> ((1 . 2))
;; (sexp->hs '(() ()))        -> ((1 . 2) (3 . 4))
;; (sexp->hs '((())))         -> ((1 . 4) (2 . 3))
;; (sexp->hs '(() () ()))     -> ((1 . 2) (3 . 4) (5 . 6))
;; (sexp->hs '(() (())))      -> ((1 . 2) (3 . 6) (4 . 5))
;; (sexp->hs '((()) ()))      -> ((1 . 4) (2 . 3) (5 . 6))
;; (sexp->hs '((() ())))      -> ((1 . 6) (2 . 3) (4 . 5))
;; (sexp->hs '(((()))))       -> ((1 . 6) (2 . 5) (3 . 4))

;; Could be cleaner, probably:

(define (sexp->hs p)
  (let ((c (list (cons 0 0)))
        (maxnode (list 0))
       )
     (let recurse ((p p))
        (cond ((pair? p)
                 (let ((this-trans (cons (1+ (car maxnode)) 0)))
                    (set-car! maxnode (1+ (car maxnode)))
                    (attach! this-trans c)
                    (recurse (car p))
                    (set-car! maxnode (1+ (car maxnode)))
                    (set-cdr! this-trans (car maxnode))
                    (recurse (cdr p))
                 )
              )
        )
     ) ; let recurse
     (cdr (reverse! c))
  ) ; let
)

;; From John Fiorillo's http://spectacle.berkeley.edu/~fiorillo/7genjimon.html

;; Genji crests ('genji-mon') or Genji incense ('genji-kô')
;; were emblems associated with the 54 chapters of the
;; 'Genji monogatari' ("The Tale of Genji"), written by
;; Murasaki Shikibu in the first quarter of the eleventh century.

;; See also: http://plaza27.mbn.or.jp/~921/kumiko/genjiko/genjikou.html
;; by Kazuhiro Kunii.

;; Both linked from http://www.research.att.com/projects/OEIS?Anum=A000110

;; From Stanley's http://www-math.mit.edu/~rstan/ec/catsol.ps.gz page 7:
;; (Solutions to the 27 page excerpt from his "Enumerative Combinatorics",
;; see: http://www-math.mit.edu/~rstan/ec/catalan.ps.gz)

;; (rr) Obvious bijection with (pp). (Vertical lines are in the same block
;; if they are connected by a horizontal line.) As mentioned in the Notes
;; to Chapter 1, Murasaki diagrams were used in The Tale of Genji
;; to represent the 52 partitions of a five-element set.
;; The noncrossing Murasaki diagrams correspond exactly to the
;; noncrossing partitions. The statement that noncrossing Murasaki diagrams
;; are enumerated by Catalan numbers seems first to have been observed
;; by H. W. Gould, who pointed it out to M. Gardner, leading to its
;; mention in [27]. Murasaki diagrams were not actually used by
;; Lady Murasaki herself. It wasn't until the Wasan period of old
;; Japanese mathematics, from the late 1600s well into the 1700s,
;; that the Wasanists started attaching the Murasaki diagrams
;; (which were actually incense diagrams) to illustrated editions
;; of The Tale of Genji.

;; Works like this:
;; Old order was like this:
;; (sexp->pp-qq-rr-cycles '())         --> ()
;; (sexp->pp-qq-rr-cycles '(()))       --> ((1))
;; (sexp->pp-qq-rr-cycles '(()()))     --> ((1 2))
;; (sexp->pp-qq-rr-cycles '((())))     --> ((1) (2))
;; (sexp->pp-qq-rr-cycles '(() () ())) --> ((1 2 3))
;; (sexp->pp-qq-rr-cycles '(() (())))  --> ((1 2) (3))
;; (sexp->pp-qq-rr-cycles '((()) ()))  --> ((1 3) (2))
;; (sexp->pp-qq-rr-cycles '((()())))   --> ((1) (2 3))
;; (sexp->pp-qq-rr-cycles '(((()))))   --> ((1) (2) (3))

(define (sexp->pp-qq-rr-cycles-old-order s)
  (let ((res (list (list (list))))
        (pos 0)
       )
    (let recurse ((s s) (depth 0) (newstick? #t))
          (cond ((pair? s)
                   (cond (newstick?
                            (set! pos (1+ pos))
                            (attach! (list depth pos) res)
                         )
                         ((assoc depth res) ;; Always found!
                            => (lambda (c) (set! pos (1+ pos)) (attach! pos (cdr c)))
                         )
                         (else (error "sexp->pp-qq-rr-cycles: no list beginning with " depth
                                      " found from " res " pos=" pos)
                         )
                   )
                   (cond ((pair? (car s))
                            (recurse (car s) (1+ depth) #t)
                         )
                   )
                   (recurse (cdr s) depth #f)
                )
          )
    )
    (map reverse! (map cdr (cdr (reverse! res))))
  )
)


;;
;; (sexp->pp-qq-rr-cycles '(()()))     --> ((1 2))
;; (sexp->pp-qq-rr-cycles '((())))     --> ((2) (1))
;; (sexp->pp-qq-rr-cycles '(()()()))   --> ((1 2 3))
;; (sexp->pp-qq-rr-cycles '(()(())))   --> ((1 3) (2))
;; (sexp->pp-qq-rr-cycles '((())()))   --> ((2 3) (1))
;; (sexp->pp-qq-rr-cycles '((()())))   --> ((3) (1 2))
;; (sexp->pp-qq-rr-cycles '(((()))))   --> ((3) (2) (1))
;; And:
;; (sexp->pp-qq-rr-cycles '((() (() (()()()) (())))))   --> ((10) (1 9) (2 6 8) (7) (3 4 5))
;; (sexp->pp-qq-rr-cycles '((() (() (()(())) (())))))   --> ((10) (1 9) (2 6 8) (7) (3 5) (4))
;; (sexp->pp-qq-rr-cycles '((() (() ((())()) (())))))   --> ((10) (1 9) (2 6 8) (7) (4 5) (3))
;;

;; Could be more elegant & symmetric, I think.
;; Map according to the descending (right-side) edge, so thus call DeepReverse first:

;; Borrowed from gatomorf.scm:
(define (gmA057164 s) ;; Was: DeepRev
   (cond ((not (pair? s)) s)
         ((null? (cdr s)) (cons (gmA057164 (car s)) (list)))
         (else (append (gmA057164 (cdr s))
                       (gmA057164 (cons (car s) (list))))
         )
   )
)

(define (sexp->pp-qq-rr-cycles s)
  (let ((res (list (list (list))))
        (pos (1+ (count-pars s)))
       )
    (let recurse ((s (gmA057164 s)) (depth 0) (newstick? #t))
          (cond ((pair? s)
                   (cond (newstick?
                            (set! pos (-1+ pos))
                            (attach! (list depth pos) res)
                         )
                         ((assoc depth res) ;; Always found!
                            => (lambda (c) (set! pos (-1+ pos)) (attach! pos (cdr c)))
                         )
                         (else (error "sexp->pp-qq-rr-cycles: no list beginning with " depth
                                      " found from " res " pos=" pos)
                         )
                   )
                   (cond ((pair? (car s))
                            (recurse (car s) (1+ depth) #t)
                         )
                   )
                   (recurse (cdr s) depth #f)
                )
          )
    )
    (map cdr (cdr (reverse! res)))
  )
)


(define (cycle->pairs cycle)
   (let loop ((res (list (last-pair cycle))) (cycle cycle))
       (cond ((null? cycle) (cdr (reverse! res)))
             (else (loop (cons (cons (car cycle) (caar res)) res) (cdr cycle)))
       )
   )
)

;; Map according to the descending (right-side) edge, so thus call DeepReverse first:
(define (sexp->pp-qq-rr s) (append-map! cycle->pairs (sexp->pp-qq-rr-cycles s)))

;; Find the first "hole" in the ascending list of integers, #f is there is no hole:
;; (next-hole '(3 4 5)) -> #f
;; (next-hole '(3 4 7)) -> 5
;;

(define (next-hole lista)
   (cond ((null? lista) #f)
         ((null? (cdr lista)) #f)
         ((> (cadr lista) (1+ (car lista))) (1+ (car lista)))
         (else (next-hole (cdr lista)))
   )
)


;; (find-all-holes '(1 3 5 10 12)) --> '(2 4 6 7 8 9 11)

(define (find-all-holes lista)
 (if (pair? lista)
     (let loop ((res (list)) (lista (cdr lista)) (a (1+ (car lista))))
        (cond ((null? lista) (reverse! res))
              ((> (car lista) a) (loop (cons a res) lista (1+ a)))
              (else (loop res (cdr lista) (1+ a))) ;; Otherwise a = (car lista)
        )
     )
     lista
 )
)

;; It's mandatory that each partition in partitions is sorted in ascending order,
;; from the least to the largest! partitions list itself don't need to be sorted.

;; (add-depths-to-rr-parts! (sexp->pp-qq-rr-cycles '((() (() (()()()) (())))))) --> '((0 1 9) (1 2 6 8) (2 3 4 5) (2 7) (0 10))
;; (add-depths-to-rr-parts! (sexp->pp-qq-rr-cycles '((() (() (()(())) (())))))) --> '((0 1 9) (1 2 6 8) (2 3 5) (3 4) (2 7) (0 10))
;; (add-depths-to-rr-parts! (sexp->pp-qq-rr-cycles '((() (() ((())()) (())))))) --> '((0 1 9) (1 2 6 8) (2 3) (2 4 5) (2 7) (0 10))

(define (add-depths-to-rr-parts! partitions)
  (let ((res (list (list))))
    (let aihvus ((pos 1) (depth 0))
          (cond ((assoc pos partitions)
                  => (lambda (c)
                       (attach! (attach! depth c) res)           ;; Transfer depth-prefixed partition c to the front of res
                       (set! partitions (delete! c partitions))  ;; and delete from partitions.
                       (let ((positions-inside (find-all-holes c)))
                          (for-each (lambda (pos)
                                      (aihvus pos (1+ depth))
                                    )
                                    positions-inside
                          )
                          (aihvus (1+ (car (last-pair c))) depth)
                       )
                     )
                )
          )
    )
    (cdr (reverse! res))
  )
)

;; Borrowed from gatoaltr.scm:


(define (p->code1 p)
  (let ((cs (list (list))))
     (let recurse ((p p) (level 0))
        (cond
          ((pair? p)
              (cond ((pair? (car p))
                       (attach! (1+ level) cs)
                       (recurse (car p) (1+ level))
                    )
              )
              (cond ((pair? (cdr p))
                       (attach! level cs)
                       (recurse (cdr p) level)
                    )
              )
          )
        )
     ) ;; let recurse
     (cdr (reverse! cs))
  )
)

;; I.e. add one to each, and concatenate 1 to the front: (I have forgotten why...)
(define (p->zerofree-code1 p)
   (p->code1 (cons p '()))
)

(define (p->factbase p)
   (factbaseR->n (p->zerofree-code1 p))
)

(define (sexp->A071157 s) (reverse! (p->zerofree-code1 s)))

(define (sexp->A071155 s) (p->factbase s))

(define (sexp->A071158 s) (reverse! (p->zerofree-code1 (gmA057164 s))))
(define (sexp->A071156 s) (sexp->A071155 (gmA057164 s)))

;; 
;; (sexp->A071158 '(()()())) --> '(1 1 1)  (sexp->A071156 '(()()())) --> 9
;; (sexp->A071158 '(()(()))) --> '(1 2 1)  (sexp->A071156 '(()(()))) --> 11
;; (sexp->A071158 '((())())) --> '(2 1 1)  (sexp->A071156 '((())())) --> 15
;; (sexp->A071158 '((()()))) --> '(2 2 1)  (sexp->A071156 '((()()))) --> 17
;; (sexp->A071158 '(((())))) --> '(3 2 1)  (sexp->A071156 '(((())))) --> 23
;;

;; Borrowed and modified from http://www.iki.fi/~kartturi/Schemuli/intfuns1.scm
(define (factbaseR->n rex) ;; Convert the reversed factorial expansion list to an integer.
   (let loop ((rex rex) (n 0) (i 1) (fn 1))
      (cond ((null? rex) n)
            (else (loop (cdr rex) (+ n (* fn (car rex))) (+ 1 i) (* fn (+ 1 i))))
      )
   )
)



;; Does either of these implement (accidentally) any of the
;; algorithms mentioned in "Drawing trees nicely with TeX",
;; by A. Brüggemann-Klein and D. Wood, available at:
;; http://cajun.cs.nott.ac.uk/compsci/epo/papers/volume2/issue2/epabk022.pdf ??
;;
;; See also: Jeremy Gibbons, Deriving Tidy Drawings of Trees.
;; Journal of Functional Programming, 6(3) p535-562, 1996.
;; http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/drawing.ps.gz
;;

;; For the "optimized" output of the binary trees we first construct
;; a breadth-first-wise "spread-tree" (cf. spreadsheet) of the
;; car/cdr-tree to be output (with the function construct-coordinate-tree),
;; and then "instantiate" its nodes with clash-free X-coordinates
;; using the function fill-cordtree-x-coordinates!

;;
;; Construct a list structure like:
;; (construct-coordinate-tree '(a . (b . c)))
;; --> (((0 . 0)) (((0 . 0)) ((0 . 0))) ((((0 . 0))) (((0 . 0)))))
;;
;; (construct-coordinate-tree '((a . b) . c))
;; --> (((0 . 0)) (((0 . 0)) ((0 . 0))) ((((0 . 0)) ((0 . 0))) (((0 . 0)) ((0 . 0)))))
;;

(define (construct-coordinate-tree bt)
  (let ((cs (list (list (cons 0 0))))) ;; The root at x-position 0.
   ((lambda (recfun)
      (cond ((pair? bt)
        (recfun (cdr bt) 1)
        (recfun (car bt) 1)
      ))
      cs
    )
     (letrec ((recurse (lambda (bt depth)
                (let ((this-level (nthcdr depth cs))
                      (prev-level (list-ref cs (- depth 1)))
                     )

                 (if (pair? this-level) ;; Not the first of this level.
                     (set-car! this-level (cons prev-level (car this-level)))
                     (append! cs (list (list prev-level)))
                 )
                 (cond ((pair? bt)
                          (recurse (cdr bt) (1+ depth))
                          (recurse (car bt) (1+ depth))
                       )
                 ) ; cond
                ) ; let
             )))
           recurse
     ) ; letrec
   ) ; lambda
  ) ; let
)


(define (repl-parent-pointers-with-coord-pairs! lista x-displ)
   (cond ((pair? lista)
            (set-car! lista (cons (+ (caaar lista) x-displ) (caaar lista)))
            (repl-parent-pointers-with-coord-pairs! (cdr lista) (- x-displ))
         )
   )
)

(define (add-displ-to-each-child-x! lista x-displ)
   (cond ((pair? lista)
            (set-car! (car lista) (+ (caar lista) x-displ))
            (add-displ-to-each-child-x! (cdr lista) (- x-displ))
         )
   )
)

(define (clash-free? level)
   (or (null? level)
       (null? (cdr level))
       (apply < (map car level))
   )
)


;; (fill-cordtree-x-coordinates! (construct-coordinate-tree '()) 1)
;; --> (((0 . 0)))
;; (fill-cordtree-x-coordinates! (construct-coordinate-tree '(a . b)) 1)
;; --> (((0 . 0)) ((-1 . 0) (1 . 0)))
;; (fill-cordtree-x-coordinates! (construct-coordinate-tree '(a . (b . c))) 1)
;; --> (((0 . 0)) ((-1 . 0) (1 . 0)) ((0 . 1) (2 . 1)))
;; (fill-cordtree-x-coordinates! (construct-coordinate-tree '((a . b) . c)) 1)
;; --> (((0 . 0)) ((-1 . 0) (1 . 0)) ((-2 . -1) (0 . -1)))
;; (fill-cordtree-x-coordinates! (construct-coordinate-tree '((a . b) . (c . d))) 1)
;; --> (((0 . 0)) ((-1 . 0) (1 . 0)) ((-3/2 . -1) (-1/2 . -1) (1/2 . 1) (3/2 . 1)))
;;  (fill-cordtree-x-coordinates! (construct-coordinate-tree '((a . (b . c)) . ((d . e) . f))) 1)
;; --> (((0 . 0)) ((-1 . 0) (1 . 0)) ((-3/2 . -1) (-1/2 . -1) (1/2 . 1) (3/2 . 1)) ((-3/4 . -1/2) (-1/4 . -1/2) (1/4 . 1/2) (3/4 . 1/2)))
;; 


;; Call as (fill-cordtree-x-coordinates! (construct-coordinate-tree bt) x-displ #f)
(define (fill-cordtree-x-coordinates! ct x-displ never-contract?) ;; New argument added 12. Feb 2003.
  (cond ((pair? ct)
           (cond ((not (number? (caaar ct))) ;; Still pointers to parents at this level.
                   (repl-parent-pointers-with-coord-pairs! (car ct) (- x-displ))
                   (fill-cordtree-x-coordinates! ct x-displ never-contract?) ;; And check again.
                 )
                 ((or never-contract?
                      (clash-free? (car ct)) ;; If this level is clash-free, then continue
                  )
                   (fill-cordtree-x-coordinates! (cdr ct) x-displ never-contract?)
                 )
                 (else ;; We have to contract the branches at this level.
                   (add-displ-to-each-child-x! (car ct) (/ x-displ 2))
                   (fill-cordtree-x-coordinates! ct (/ x-displ 2) never-contract?) ;; And try again.
                 )
           )
        )
  )
  ct
)



;; (p->tree-x-coordinates '())       --> (1)
;; (p->tree-x-coordinates '(()))     --> (1 (1))
;; (p->tree-x-coordinates '((())))   --> (1 (1 (1)))
;; (p->tree-x-coordinates '(()()))   --> (3/2 (1) (2))
;; (p->tree-x-coordinates '(()()())) --> (2 (1) (2) (3))
;; (p->tree-x-coordinates '((())())) --> (3/2 (1 (1)) (2))
;; (p->tree-x-coordinates '(()(()))) --> (3/2 (1) (2 (2)))
;; (p->tree-x-coordinates '((()()))) --> (3/2 (3/2 (1) (2)))
;; (p->tree-x-coordinates '(((())))) --> (1 (1 (1 (1))))


(define (incr x) (set-car! x (+ (car x) 1)) (car x))


(define (average-x-of branches)
   (/ (apply + (map car branches)) (length branches))
)

(define (p->tree-x-coordinates p)
  (let ((max_x (list 0)))
    (let recurse ((p p))
       (cond ((not (pair? p)) (list (incr max_x)))
             (else
               (let ((branches (map recurse p)))
                  (cons (average-x-of branches) branches)
               )
             )
       )
    )
  )
)



;; (normalize-root-to-zero-and-scale! (p->tree-x-coordinates '(()((())()))) 12)
;; --> (0 (-9) (9 (3 (3)) (15)))

(define (normalize-root-to-zero-and-scale! tx x-scale)
   (let ((off (car tx)))
      (let recurse ((tx tx))
         (cond ((pair? tx)
                    (if (number? (car tx))
                        (set-car! tx (* (- (car tx) off) x-scale))
                        (recurse (car tx))
                    )
                    (recurse (cdr tx))
               )
         )
      )
      tx
   )
)




