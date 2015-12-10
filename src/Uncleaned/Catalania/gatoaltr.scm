
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;;  http://www.megabaud.fi/~karttu/matikka/Nekomorphisms/gatoaltr.scm   ;;
;;  - Functions for ranking & unranking objects in Catalan families,    ;;
;;  in some of the alternative orders.                                  ;;
;;                                                                      ;;
;;  This Scheme-code is Copyright (C) 2002--2007 by Antti Karttunen     ;;
;;  (E-mail: my_firstname.my_surname@gmail.com) and is placed under     ;;
;;  the GPL (Gnu Public License), so you are free to copy it.           ;;
;;                                                                      ;;
;;  Last edited 2015-01-16.                                             ;;
;;                                                                      ;;
;;  Runs at least in MIT Scheme Release 7.6.0, for which one can find   ;;
;;  documentation and the pre-compiled binaries (for various OS's       ;;
;;  running in Intel x86 architecture) under the URL:                   ;;
;;                                                                      ;;
;;  http://www.swiss.ai.mit.edu/projects/scheme/                        ;;
;;                                                                      ;;
;;  Aubrey Jaffer's SLIB Scheme library is available at:                ;;
;;  http://www.swiss.ai.mit.edu/~jaffer/SLIB.html                       ;;
;;                                                                      ;;
;;  The main pointer for this code collection is:                       ;;
;;  http://www.megabaud.fi/~karttu/matikka/Nekomorphisms/gatomorf.htm   ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(declare (usual-integrations))

;; (load "c:\\slib\\mitscheme.init") ;; Aubrey Jaffer's SLIB Scheme library.
;; (require 'factor)                 ;; Currently works only with MIT Scheme 7.6

;; (load "c:\\matikka\\Schemuli\\definecd.scm") ;; Use the dirty version.
;; (load "c:\\matikka\\Schemuli\\intfuns1.scm")
;; (load "c:\\matikka\\Schemuli\\lstfuns1.scm")

;; (load "c:\\matikka\\Nekomorphisms\\gatochek.scm")
;; (load "c:\\matikka\\Nekomorphisms\\gatorank.scm")

(define (load-altr) (load "c:\\matikka\\Nekomorphisms\\gatoaltr.scm"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;; A few alternative ranking & unranking functions (in development)   ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (rank-bintree bt packfun)
    (cond ((not (pair? bt)) 0)
          (else (1+ (packfun (rank-bintree (car bt) packfun)
                             (rank-bintree (cdr bt) packfun)
                    )
                )
          )
    )
)

(define (unrank-bintree rank pr1 pr2)
    (cond ((zero? rank) (list))
          (else (cons (unrank-bintree (pr1 (-1+ rank)) pr1 pr2)
                      (unrank-bintree (pr2 (-1+ rank)) pr1 pr2)
                )
          )
    )
)



(define (lexrank->arithrank-bijection packfun)
    (lambda (n) (rank-bintree (A014486->parenthesization (A014486 n)) packfun))
)


(define (arithrank->lexrank-bijection pr1 pr2)
    (lambda (n)
     (CatalanRankGlobal (parenthesization->A014486 (unrank-bintree n pr1 pr2)))
    )
)

(define (size-fun-with-arithrank-scheme pr1 pr2)
    (lambda (n)
;;   (/ (binwidth (parenthesization->A014486 (unrank-bintree n pr1 pr2))) 2)
     (count-pars (unrank-bintree n pr1 pr2))
    )
)

;; (define A071673 (size-fun-with-arithrank-scheme A025581 A002262))
;; or: (define A071673 (size-fun-with-arithrank-scheme A002262 A025581))


(definec (A071673 n) ;; Cf. comment at A072768. Was definec
    (cond ((zero? n) n)
          (else (+ 1 (A071673 (A025581 (-1+ n)))
                     (A071673 (A002262 (-1+ n)))
                )
          )
    )
)

(define A071674 (compose-funs lsb A071673))

(define (max-n-fun-with-arithrank-scheme packfun)
    (lambda (size)
      (let loop ((max-n 0) (rank (A000108 size)))
        (cond
           ((zero? rank) max-n)
           (else (loop (max max-n
                            (rank-bintree
                                (A014486->parenthesization
                                     (CatalanUnrank size (-1+ rank)))
                                packfun
                            )
                       )
                       (-1+ rank)
                 )
           )
        ) ;; cond
      ) ;; let loop
    )
)


(define (max-n-fun-with-arithrank-scheme-v2 packfun corrfun)
    (lambda (size)
      (let loop ((max-n 0) (rank (A000108 size)))
        (cond
           ((zero? rank) max-n)
           (else (loop (max max-n
                            (corrfun 
                              (rank-bintree
                                (A014486->parenthesization
                                     (CatalanUnrank size (-1+ rank)))
                                packfun
                              )
                            )
                       )
                       (-1+ rank)
                 )
           )
        ) ;; cond
      ) ;; let loop
    )
)

(define lexrank->arithrankA061579 (lexrank->arithrank-bijection packA061579))

(define arithrankA061579->lexrank (arithrank->lexrank-bijection A002262 A025581))

(define lexrank->arithrankA001477 (lexrank->arithrank-bijection packA001477))

(define arithrankA001477->lexrank (arithrank->lexrank-bijection A025581 A002262))


(define lexrank->arithrankA054238 (lexrank->arithrank-bijection packA054238))
(define arithrankA054238->lexrank (arithrank->lexrank-bijection A059905 A059906))

(define lexrank->arithrankA054238tr (lexrank->arithrank-bijection packA054238tr))
(define arithrankA054238tr->lexrank (arithrank->lexrank-bijection A059906 A059905))


(define A072638 (max-n-fun-with-arithrank-scheme packA001477))
;; Or: (define A072638 (max-n-fun-with-arithrank-scheme packA061579))

(define A072639 (max-n-fun-with-arithrank-scheme packA054238))
;; Or: (define A072639 (max-n-fun-with-arithrank-scheme packA054238tr))

(define A072640 (max-n-fun-with-arithrank-scheme (lambda (x y) (A048680 (packA054238 x y)))))

(define A072654 (max-n-fun-with-arithrank-scheme-v2 packA054238tr A048680))
(define A072655 (compose-funs binwidth A072654))

;; (map lexrank->arithrankA061579 (cons 0 (iota 120)))
;; --> (0 1 2 3 4 7 5 6 10 11 29 16 22 56 8 12 9 15 36 14 21 28 66 67 436 137 254 1597 37 79 46 121 667 106 232 407 2212 17 38 23 30 68 13 18 20 78 465 44 153 276 1653 19 25 27 45 91 35 55 136 703 77 120 253 435 2278 2279 95267 9454 32386 1276004 704 3161 1082 7382 222779 5672 27029 83029 2447579 154 742 277 466 2347 92 172 211 3082 108346 991 11782 38227 1367032 191 326 379 1036 4187 631 1541 9317 247457 3004 7261 32132 94831 2595782 80 467 155 278 1655 47 93 57 138 705 122 255 437 2280)

;; (map arithrankA061579->lexrank (cons 0 (iota 120)))
;; --> (0 1 2 3 4 6 7 5 14 16 8 9 15 42 19 17 11 37 43 51 44 20 12 39 121 52 126 53 21 10 40 123 149 127 154 56 18 28 38 124 151 385 155 163 47 54 30 112 122 152 387 475 164 135 156 57 13 114 376 150 388 477 503 136 480 165 22 23 41 378 466 386 478 505 413 481 508 60 45 29 107 125 468 1234 476 506 415 1540 509 177 128 55 84 113 371 153 1236 1531 504 416 1542 1630 178 390 159 166 33 348 377 461 389 1533 1621 414 1543 1632 551 391 489 510 61)

;; (first-dislocated (map lexrank->arithrankA061579 (map btrank0->lexrank (cons 0 (iota 999))))) --> ()


;; (map lexrank->btrank1 (cons 0 (iota 120)))
;; --> (0 1 3 2 10 6 5 7 4 66 28 21 36 15 14 9 12 56 22 8 16 29 11 2278 435 253 703 136 120 55 91 1653 276 45 153 465 78 77 35 27 44 20 25 18 68 2212 407 30 232 667 121 19 13 23 106 46 38 79 1597 254 17 37 137 436 67 2598060 95266 32385 248160 9453 7381 1596 4278 1368685 38503 1081 11935 108811 3160 3081 666 406 1035 231 351 190 2415 2449791 83436 496 27261 223446 7503 210 105 300 5778 1128 780 3240 1277601 32640 171 741 9591 95703 2346 2345 464 275 740 152 135 65 104 1710 299 54 170 495 90)

;; (map btrank1->lexrank (cons 0 (iota 120)))
;; --> (0 1 3 2 8 6 5 7 19 15 4 22 16 52 14 13 20 60 43 51 41 11 18 53 178 42 153 39 10 21 47 155 177 125 151 38 12 61 56 136 154 555 123 150 40 33 55 179 164 135 479 553 122 152 117 29 17 159 557 163 417 477 552 124 471 113 9 64 44 490 556 507 415 476 554 381 467 37 36 57 191 127 489 1799 505 414 478 1791 377 149 120 30 181 165 600 126 1572 1797 504 416 1536 1787 121 474 114 94 54 561 509 599 389 1570 1796 506 1329 1532 551 384 468 358 28)

;; A072764 - A072773 reserved for us.

;; The definition of A072764bi transferred to gatorank.scm 2012-11-11.

(define (A072764 n) ;; Was definec
   (cond ((zero? n) n)
         (else (A072764bi (A025581 (-1+ n)) (A002262 (-1+ n))))
   )
)

(define (A072765 n) ;; Inverse function of A072764. Was definec
   (cond ((zero? n) n)
         (else (1+ (packA001477 (A072771 n) (A072772 n))))
   )
)


(define (A072766 n) ;; Was definec
   (cond ((zero? n) n)
         (else
           (let ((y (A025581 (-1+ n))) (x (A002262 (-1+ n))))
              (CatalanRankGlobal
                 (parenthesization->A014486
                    (cons (A014486->parenthesization (A014486 x))
                          (A014486->parenthesization (A014486 y))
                    )
                 )
              )
           )
         )
   )
)


(define (A072767 n) ;; Inverse function of A072766. Was definec
   (cond ((zero? n) n)
         (else (1+ (packA001477 (A072772 n) (A072771 n))))
   )
)

;; A072764 & A072767 for the inverses of the above two.

;;
;; Naive way:
;;
;; (define (A072768 n) ;; The sizes of the parenthesizations produced by A072764 & -6.
;;    (cond ((zero? n) n)
;;          (else
;;            (let ((x (A025581 (-1+ n))) (y (A002262 (-1+ n))))
;;               (count-pars
;;                     (cons (A014486->parenthesization (A014486 x))
;;                           (A014486->parenthesization (A014486 y))
;;                     )
;;               )
;;            )
;;          )
;;    )
;; )
;; 


;; Nice way. We should code a general transformation of the integer
;; sequences based on the same idea. The set of sequences which
;; begin with 0, and then contain A000108(n) copies of the value n
;; (in any order) is then closed under that transformation ???
;; (The only non-trivial subset of all the sequence N^N that is closed
;;  by that transformation?)

;; Similarly for other such transformations based on more exotic
;; NxN bijections, like A054238 (bit-interleaving).

;; Is there a fixed point? (i.e. eigen-sequences) Isn't A071673 just the one?
;; Do (any of) the other sequences belong into the set converge towards it?

;; Note that A072768 differs from A071673 first time at position n=37,
;; where (A072768 37) = 4, while (A071673 37) = 5.

;; Was defined with definec:

(define (A072768 n) ;; The sizes of the parenthesizations produced by A072764 & -6.
   (cond ((zero? n) n)
         (else
           (+ 1 (A072643 (A025581 (-1+ n))) (A072643 (A002262 (-1+ n))))
         )
   )
)


;; Also interesting... The total average of all the terms (if that makes
;; any sense when talking about an infinite sequence) must be zero!???
(define (A072769 n) (- (A071673 n) (A072768 n)))

(define (A072770 n) (modulo (A072768 n) 2)) ;; Cf. A071674
(define (A072770v2 n) (modulo (+ (A072769 n) (A071674 n)) 2))



(define (A072773 n) ;; upper triangular region of A072764, zero-based.
    (CatalanRankGlobal
       (parenthesization->A014486
          (cons (A014486->parenthesization (A014486 (A003056 n)))
                (A014486->parenthesization (A014486 (A002262 n)))
          )
       )
    )
)



(define A071651 lexrank->arithrankA061579)
(define A071652 arithrankA061579->lexrank)

(define A071653 lexrank->arithrankA001477)
(define A071654 arithrankA001477->lexrank)

(define (A071654bi x y) (A071654 (packA001477 x y)))
(define (A071654ib x y) (A071654 (packA001477 y x)))
(define (A071654v2 n) (A071654bi (A025581 n) (A002262 n)))

;; produces: 0,1,4,2,15,7,9,5,3,113,39,118,42,16,52,20,21,29,11,8,34,14,6,12951,363,...
(define Aweird1 (lexrank->arithrank-bijection A071654bi))
;; produces: 0,1,2,4,3,5,9,7,15,6,14,34,11,29,8,21,20,16,42,52,118,39,113,12,95,258,10,25,...
(define Aweird2 (lexrank->arithrank-bijection A071654ib))

(define (A072766bi x y) (A072766 (1+ (packA001477 x y))))

(define A001477_v5_check (lexrank->arithrank-bijection (lambda (x y) (-1+ (A072764bi x y)))))
(define A057163_v5_check (lexrank->arithrank-bijection (lambda (x y) (-1+ (A072764bi y x)))))



(define A071671huu (compose-funs halve A071152 A071652 1+))
(define A071671 (compose-funs halve baselist-as-decimal p->Lw BinTree2Tree (lambda (n) (unrank-bintree n A002262 A025581)) 1+))
(define A071671haa (compose-funs halve baselist-as-decimal p->Lw BinTree2Tree *A057163 (lambda (n) (unrank-bintree n A025581 A002262)) 1+))

(define A071672huu (compose-funs halve A071152 A071654 1+))
(define A071672 (compose-funs halve baselist-as-decimal p->Lw BinTree2Tree (lambda (n) (unrank-bintree n A025581 A002262)) 1+))
(define A071672haa (compose-funs halve baselist-as-decimal p->Lw BinTree2Tree *A057163 (lambda (n) (unrank-bintree n A002262 A025581)) 1+))


(define A072634 (lexrank->arithrank-bijection packA054238))
(define A072635 (arithrank->lexrank-bijection A059905 A059906))

(define A072636 (lexrank->arithrank-bijection packA054238tr))
(define A072637 (arithrank->lexrank-bijection A059906 A059905))


(define A072656 (lexrank->arithrank-bijection packA048680oA054238))
(define A072657 (arithrank->lexrank-bijection A072661 A072662))

(define A072658 (lexrank->arithrank-bijection packA048680oA054238tr))
(define A072659 (arithrank->lexrank-bijection A072662 A072661))


(define A072644   (size-fun-with-arithrank-scheme A059905 A059906))
(define A072644v2 (size-fun-with-arithrank-scheme A059906 A059905))
(define A072644v3 (compose-funs halve binwidth A014486 A072635))

(define A072646 (compose-funs A048680 A072636))
(define A072647 (compose-funs A072637 A048679))

(define A072645 (compose-funs halve binwidth A014486 A072647))

(define A072645v2 (compose-funs halve binwidth A014486 A072635 A048679))

(define A072660 (compose-funs halve binwidth A014486 A072657))
(define A072660v2 (compose-funs halve binwidth A014486 A072659))
(define A072660v3 (size-fun-with-arithrank-scheme A072661 A072662))
(define A072660v4 (size-fun-with-arithrank-scheme A072662 A072661))


(define A072787 (lexrank->arithrank-bijection packA072734))
(define A072788 (arithrank->lexrank-bijection A072740 A072741))
(define A072789 (size-fun-with-arithrank-scheme A072740 A072741))
(define A072789v2 (size-fun-with-arithrank-scheme A072741 A072740))
(define A072789v3 (compose-funs halve binwidth A014486 A072788))
(define A072790 (max-n-fun-with-arithrank-scheme packA072734))
(define A072791 (compose-funs binwidth A072790))
(define A072792 (compose-funs lsb A072789)) ;; Just for the sake of abstract art!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; In similar vein:
;; The AND- & OR-operations on binary trees (forms a lattice, I think)
;; a "tip-product" of binary trees and general trees
;; (is associative, but not commutative). Also 2-ary append.

;; (bin-interleave '(() . (() . (() . ())))) --> 11
;; (bin-interleave '(() . ((() . ()) . ()))) --> 35
;; (bin-interleave '((() . ()) . (() . ()))) --> 7
;; (bin-interleave '((() . (() . ())) . ())) --> 21
;; (bin-interleave '(((() . ()) . ()) . ())) --> 69
;; (bin-interleave '(() () () ()))           --> 139


(define (bin-interleave bt)
    (cond ((not (pair? bt)) 0)
          (else (1+ (* 2 (+ (* 2 (A000695 (bin-interleave (car bt))))
                            (A000695 (bin-interleave (cdr bt)))
                         )
                    )
                )
          )
    )
)

(define (decode-A082856-code code) ;; Inverse function of bin-interleave
  (call-with-current-continuation
    (lambda (exit)
     (let recurse ((code code))
        (cond ((zero? code) (list))
              ((even? code) (exit '()))
              (else
                 (let ((even-bits (A059905 (floor->exact (/ code 2))))
                       (odd-bits  (A059906 (floor->exact (/ code 2))))
                      )
                   (cons (recurse odd-bits)
                         (recurse even-bits)
                   ) ;; Do this in different order and induce A057163.
                 )
              )
        )
     ) ;; let recurse
    )
  )
)

(define A082856 (compose-funs bin-interleave A014486->parenthesization A014486))
;; (map A082856 (iota0 23))
;; (0 1 3 5 11 35 7 21 69 139 2059 43 547 8227 15 39 23 277 4117 71 85 1093 16453 32907)

(define A082857 (compose-funs A080300 parenthesization->A014486 decode-A082856-code))

;; (first-dislocated (map A082857 (map A082856 (iota0 6918)))) --> ()
;; (map A082857 (iota0 69))
;; -> (0 1 0 2 0 3 0 6 0 0 0 4 0 0 0 14 0 0 0 0 0 7 0 16 0 0 0 0 0 0 0 42
;;  0 0 0 5 0 0 0 15 0 0 0 11 0 0 0 39 0 0 0 0 0 0 0 43 0 0 0 0 0 0 0 123 0 0 0 0 0 8)
;; Cf. A075173 & A075174.

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

(define (A082858v2 n)
   (let ((x (A025581 n))
         (y (A002262 n))
        )
      (A082857 (A004198bi (A082856 x) (A082856 y)))
   )
)

(define (A082859 n) ;; only lower/upper triangular region of A082858, zero-based.
   (CatalanRankGlobal
      (parenthesization->A014486
          (GCSB (A014486->parenthesization (A014486 (A003056 n)))
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


(define (A082860v2 n)
   (let ((x (A025581 n))
         (y (A002262 n))
        )
      (A082857 (A003986bi (A082856 x) (A082856 y)))
   )
)


(define (A082861 n) ;; only lower/upper triangular region of A082860, zero-based.
   (CatalanRankGlobal
      (parenthesization->A014486
          (LCSB (A014486->parenthesization (A014486 (A003056 n)))
                (A014486->parenthesization (A014486 (A002262 n)))
          )
      )
   )
)


;; (output-check-html "C:\\matikka\\nekomorphisms\\seqs\\a82856-61.htm" check-A082856-61 119 45)
(define check-A082856-61
 (list
       (list 119 0 82856 A082856)
       (list 119 0 82857 A082857 A082856)
       (list 119 0 82858 A082858)
       (list 119 0 82859 A082859)
       (list 119 0 82860 A082860)
       (list 119 0 82861 A082861)
 )
)


(define (A085201bi x y)
   (A080300
       (parenthesization->A014486
            (append (A014486->parenthesization (A014486 x))
                    (A014486->parenthesization (A014486 y))
            )
       )
   )
)


(define (A085201 n) (A085201bi (A025581 n) (A002262 n)))
(define (A085202 n) (A085201bi (A002262 n) (A025581 n)))

(define (A085201biv2 x y) (A080300 (A085207bi (A014486 x) (A014486 y))))
(define (A085201v2 n) (A085201biv2 (A025581 n) (A002262 n)))
(define (A085202v2 n) (A085201biv2 (A002262 n) (A025581 n)))

(define (A085201biv3 x y) (A085200 (A085215bi (A071155 y) (A071155 x))))
(define (A085201v3 n) (A085201biv3 (A025581 n) (A002262 n)))
(define (A085202v3 n) (A085201biv3 (A002262 n) (A025581 n)))

(define (A085201biv4 x y)
   (cond ((zero? x) y)
         (else (A072764bi (A072771 x) (A085201biv4 (A072772 x) y)))
   )
)
(define (A085201v4 n) (A085201biv4 (A025581 n) (A002262 n)))
(define (A085202v4 n) (A085201biv4 (A002262 n) (A025581 n)))


(define (A085203bi x y)
   (A080300
       (parenthesization->A014486
            (app-to-xrt  (A014486->parenthesization (A014486 x))
                         (A014486->parenthesization (A014486 y))
            )
       )
   )
)


(define (A085203bi! x y)
   (A080300
       (parenthesization->A014486
            (app-to-xrt! (A014486->parenthesization (A014486 x))
                         (A014486->parenthesization (A014486 y))
            )
       )
   )
)

(define (A085203 n) (A085203bi (A025581 n) (A002262 n)))
(define (A085204 n) (A085203bi (A002262 n) (A025581 n)))

(define (A085203! n) (A085203bi! (A025581 n) (A002262 n)))
(define (A085204! n) (A085203bi! (A002262 n) (A025581 n)))


(define (A085203biv2 x y) (A080300 (A085211bi (A014486 x) (A014486 y))))
(define (A085203v2 n) (A085203biv2 (A025581 n) (A002262 n)))
(define (A085204v2 n) (A085203biv2 (A002262 n) (A025581 n)))

(define (A085203biv3 x y) (A085200 (A085219bi (A071155 y) (A071155 x))))
(define (A085203v3 n) (A085203biv3 (A025581 n) (A002262 n)))
(define (A085204v3 n) (A085203biv3 (A002262 n) (A025581 n)))

;; Check that A057548(0) returns 1, not 0.
(define (A085203biv4 x y)
  (cond ((zero? x) y)
        ((zero? (A072772 x)) (A057548 (A085203biv4 (A072771 x) y)))
        (else (A072764bi (A072771 x) (A085203biv4 (A072772 x) y)))
  )
)

(define (A085203v4 n) (A085203biv4 (A025581 n) (A002262 n)))
(define (A085204v4 n) (A085203biv4 (A002262 n) (A025581 n)))

;; Defined in gatomorf.scm:
;;
;; (define (app-to-xrt a b) ;; Append 'b' to the eXtreme Rightmost Tip of 'a'.
;;   (cond ((null? a) b)
;;         ((pair? (cdr a)) (cons (car a) (app-to-xrt (cdr a) b)))
;;         (else (cons (app-to-xrt (car a) b) (cdr a)))
;;   )
;; )
;; 

(define (A085205bi x y)
   (A080300
       (parenthesization->A014486
            (list (A014486->parenthesization (A014486 x))
                  (A014486->parenthesization (A014486 y))
            )
       )
   )
)

(define (A085205 n) (A085205bi (A025581 n) (A002262 n)))
(define (A085206 n) (A085205bi (A002262 n) (A025581 n)))

(define (A085205biv2 x y) (A072764bi x (A057548v2 y)))
(define (A085205v2 n) (A085205biv2 (A025581 n) (A002262 n)))
(define (A085206v2 n) (A085205biv2 (A002262 n) (A025581 n)))

;; 85159 --- 85228
(define (A085223 n) (A085201bi n 1))
(define A085223v2 (compose-funs A057508 A072795 A057508))
(define A085223v3 (compose-funs A057164 A072795 A057164))
(define A085224 (compose-funs A014486 A085223))

(define (A085224v2 n) (+ (* 4 (A014486 n)) 2))
(define A085223v4 (compose-funs A080300 A085224v2))

(define (A085225 n) (A085203bi n 1))

(define (A085226 n) (A085205bi n 0))
(define (A085227 n) (A085205bi 0 n))
(define (A085228 n) (A085205bi n n))

(define (A057520 n) (/ (A014486 n) 2))

;; Each n occurs A076050(n) times. One-based.
(definec (A085182 n) (first_pos_with_funs_val_gte (compose-funs A085197v3 1+) (1+ n)))

(define A085183 (compose-funs A053645 A057520))
(define A085184 (compose-funs A007090 A085183))
(define A085185 (compose-funs A007090 A014486))

(define (A085191 n) (- (A071156 (1+ n)) (A071156 n)))
(define A085190 (compose-funs A085191 -1+ A081291 1+))
(define A085189 (compose-funs halve A085190))
(definec (A085188 n) (cond ((zero? n) n) (else (+ (A085188 (-1+ n)) (A085189 (-1+ n))))))
(define A077134 (compose-funs A085188 A000108)) ;; Check this!
(define A085187 (compose-funs A007623 A085188))

(define (A085192 n) (- (A014486 (1+ n)) (A014486 n)))
(define A085193 (compose-funs A085192 -1+ A081291 1+))

;; Note: A007001 and A085182 are one-based, but A085193 is zero-based:
(definec (A085193v2 nn)
 (let ((n (1+ nn)))
   (cond ((= 1 (A007001 (+ n 1)))
             (- (* 4 (A085193v2 (-1+ (A085182 n))))
                (- (expt 2 (A007001 n)) 2)
             )
         )
         (else (expt 2 (A007001 n)))
   )
 )
)

(definec (A085193v3 n)
   (cond ((= 1 (A007001 (+ n 2)))
             (- (+ 2 (* 4 (A085193v3 (-1+ (A085182 (1+ n))))))
                (expt 2 (A007001 (1+ n)))
             )
         )
         (else (expt 2 (A007001 (1+ n))))
   )
)

(define A085194 (compose-funs halve A085193))
(define A085194v2 (compose-funs (lambda (n) (- (A057520 (1+ n)) (A057520 n))) -1+ A081291 1+))

(definec (A085195 n) (cond ((zero? n) n) (else (+ (A085195 (-1+ n)) (A085194 (-1+ n))))))
(define A085186 (compose-funs A007090 A085195)) ;; Same in base-4.

(define A000302v2 (compose-funs A085195 A000108 1+))

(define A079319 (compose-funs A085194 -1+ A000108 1+)) ;; Records in A085194, Check: 1,3,9,29,101,373,1429,... yes!

(definec (A085196 n) (- (A085223 n) (A072795 n)))
(define A080336 (compose-funs A085196 A081291)) ;; Not A014419...
(define (A085197 n) (+ (A080336 (-1+ n)) n)) ;; One-based.
(define A085197v2 (compose-funs A082854 A082315 A072795 A081291 -1+))

;; One-based:
(definec (A007001 n) (- (A080336 n) (A080336 (-1+ n))))
(define A076050 (compose-funs 1+ A007001))
(define (A076050v2 n) (- (A085197 (1+ n)) (A085197 n)))
(definec (A085197v3 n) (cond ((< n 2) n) (else (+ (A085197v3 (-1+ n)) (A076050 (-1+ n))))))

(define A080237 (compose-funs A007814 A014486))
(definec (A007001v2 n) (cond ((< n 2) n) (else (A080237 (A081291 (-1+ n))))))


;; A085178bi(x>=1,y>=1) gives the position of the yth x in A080237
(define (A085178bi x y)
   (let loop ((i 1) (y (-1+ y)))
       (cond ((= (A080237 i) x)
                 (if (zero? y) i (loop (1+ i) (-1+ y)))
             )
             (else (loop (1+ i) y))
       )
   )
)

;; One-based:
(definec (A085178 n) (A085178bi (1+ (A025581 (-1+ n))) (1+ (A002262 (-1+ n)))))

;; Inverse of the above, as a permutation sequence:
(define (A085179 n) (let loop ((i 1)) (cond ((= n (A085178 i)) i) (else (loop (1+ i))))))

;; Transpose and its inverse:
(definec (A085176 n) (A085178bi (1+ (A002262 (-1+ n))) (1+ (A025581 (-1+ n)))))
(define (A085177 n) (let loop ((i 1)) (cond ((= n (A085176 i)) i) (else (loop (1+ i))))))


;; A085180bi(x>=1,y>=1) gives the position of the yth x in A007001.
(define (A085180bi x y)
   (let loop ((i 1) (y (-1+ y)))
       (cond ((= (A007001v2 i) x)
                 (if (zero? y) i (loop (1+ i) (-1+ y)))
             )
             (else (loop (1+ i) y))
       )
   )
)

;; One-based:
(definec (A085180 n) (A085180bi (1+ (A025581 (-1+ n))) (1+ (A002262 (-1+ n)))))

;; Inverse of the above, as a permutation sequence:
(define (A085181 n) (let loop ((i 1)) (cond ((= n (A085180 i)) i) (else (loop (1+ i))))))

;; A085198 defined in intfuns1.scm



;; (definec (A008578 n) ;; A008578 (non-composite numbers) ;; Was ithprime
;;   (cond ((< n 3) (1+ n)) ;; 0 -> 1, 1 -> 2, 2 -> 3,
;;         (else (list-ref (primes> 0 n) (-1+ n)))
;;   )
;; )

;; The offset was changed from 0 to 1 at Tue Jun 01 03:00:00 EDT 2010
(definec (A008578off0 n) ;; A008578 (non-composite numbers) ;; Was ithprime
   (cond ((< n 3) (1+ n)) ;; 0 -> 1, 1 -> 2, 2 -> 3,
         (else (A000040 n))
   )
)

(define (A008578 n) (if (< n 2) n (A000040 (- n 1)))) ;; Correct offset. One in GF2Xfuns.scm has off=0.

(definec (primes-index n) ;; A049084
   (cond ((not (prime? n)) 0)
         (else
            (let loop ((i 1))
                  (cond ((= (A000040 i) n) i) ;; Was using A008578 erroneously!
                        ((> i n)
                          (error "Error detected in primes-index, index i " i
                                 "larger than n: " n)
                        )
                        (else (loop (1+ i)))
                  )
            )
         )
   )
)


;;
;;  0     0 ->  1
;;  1     1 ->  2
;;  2    10 ->  3
;;  3    11 ->  4 (2^2)
;;  4   100 ->  6 (2^1 * 3^1)
;;  5   101 ->  5
;;  6   110 ->  9 (3^2)
;;  7   111 ->  8 (2^3)
;;  8  1000 -> 12 (2^2 * 3^1)
;;  9  1001 -> 15 (3^1 * 5^1)
;; 10  1010 ->  7
;; 11  1011 -> 10 (2^1 * 5^1)
;; 12  1100 -> 18 (2^1 * 3^2)

(define (binruns->primefactorization n)
  (let loop ((n n) (i 0) (p (modulo (1+ n) 2)) (z 1))
     (cond ((zero? n) (* z (A008578off0 i)))
           ((= (modulo n 2) p)
               (loop (floor->exact (/ n 2)) i (modulo n 2) (* z (A008578off0 i)))
           )
           (else
               (loop (floor->exact (/ n 2)) (1+ i) (modulo n 2) z)
           )
     )
  )
)


(define (cons-n-times n what lista)
   (cond ((zero? n) lista)
         (else (cons-n-times (-1+ n) what (cons what lista)))
   )
)


;;  1 -> ()        -> 0
;;  2 -> (1)       -> 1
;;  3 -> (1 0)     -> 2
;;  4 -> (2)       -> 3
;;  5 -> (1 0 0)   -> 5
;;  6 -> (1 1)     -> 4
;;  7 -> (1 0 0 0)
;;  8 -> (3)
;;  9 -> (2 0)
;; 10 -> (1 0 1)

;; (sort (factor 264) <) --> (2 2 2 3 11)

(define (Nvector->binruns el)
   (let loop ((el el) (n 0) (p 1))
      (cond ((null? el) n)
            (else
              (loop (cdr el)
                    (+ (* n (expt 2 (car el)))
                       (* p (-1+ (expt 2 (car el))))
                    )
                    (- 1 p)
              )
            )
      )
   )
)

;; (primefactorization->explist 1) -->  ()
;; (primefactorization->explist 2) -->  (1)
;; (primefactorization->explist 3) -->  (1 0)
;; (primefactorization->explist 4) -->  (2)
;; (primefactorization->explist 5) -->  (1 0 0)
;; (primefactorization->explist 6) -->  (1 1)
;; (primefactorization->explist 7) -->  (1 0 0 0)
;; (primefactorization->explist 8) -->  (3)
;; (primefactorization->explist 9) -->  (2 0)
;; (primefactorization->explist 10) --> (1 0 1)
;; (primefactorization->explist 11) --> (1 0 0 0 0)
;; (primefactorization->explist 12) --> (1 2)


(define (primefactorization->explist n)
  (if (= 1 n) (list)
      (let loop ((factors (sort (factor n) <)) (pf 1) (el (list)))
         (cond ((null? factors) el)
               ((= (car factors) pf)
                   (set-car! el (1+ (car el)))
                   (loop (cdr factors) (car factors) el)
               )
               (else
                   (loop (cdr factors)
                         (car factors)
                         (cons 1
                               (cons-n-times
                                         (-1+ (- (A049084 (car factors))
                                                 (A049084 pf)
                                              )
                                         )
                                         0
                                         el
                               )
                         )
                   )
               )
         )
      )
  )
)

(define (primefacs->explist n) (reverse! (primefactorization->explist n)))

(define (explist->Nvector! el) ;; Just increment the tail elems by +1.
   (cond
     ((pair? el)
       (let loop ((el (cdr el)))
          (cond ((pair? el)
                   (set-car! el (1+ (car el)))
                   (loop (cdr el))
                )
          )
       )
     )
   )
   el
)


(define (Nvector->parenthesization n n->vec)
  (letrec ((recurse (lambda (e) (map recurse (n->vec e)))))
     (recurse n)
  )
)

(define (wr x) (write x) (newline) x)

(define (primefactorization->parenthesization2 n)
   (Nvector->parenthesization n
        (lambda (e) (wr (explist->Nvector! (primefactorization->explist n))))
   )
)

;; The next one works, the above one doesn't!


;; Mapping from exponent lists to list structures:
;;
;;  1 -> ()        -> ()       -> ()
;;  2 -> (1)       -> (1)      -> (())
;;  3 -> (1 0)     -> (1 1)    -> (() ())
;;  4 -> (2)       -> (2)      -> ((()))
;;  5 -> (1 0 0)   -> (1 1 1)  -> (() () ()) 
;;  6 -> (1 1)     -> (1 2)    -> (() (()))
;;  7 -> (1 0 0 0) -> (1 1 1 1)-> (() () () ())
;;  8 -> (3)       -> (3)      -> ((() ()))
;;  9 -> (2 0)     -> (2 1)    -> ((()) ())
;; 10 -> (1 0 1)   -> (1 1 2)  -> (() () (()))

(define (primefactorization->parenthesization n)
  (map primefactorization->parenthesization
       (explist->Nvector! (primefactorization->explist n))
  )
)


(define (parenthesization->primefactorization p)
  (Nvector->primefactorization! (map parenthesization->primefactorization p))
)

(define (Nvector->primefactorization! el)
  (let loop ((el (reverse! el)) (i 1) (z 1))
    (cond ((null? el) z)
          ((null? (cdr el)) (* (expt (A008578off0 i) (car el)) z))
          (else
            (loop (cdr el) (1+ i) (* (expt (A008578off0 i) (-1+ (car el))) z))
          )
    )
  )
)


(define (binruns->parenthesization n)
  (map binruns->parenthesization
       (map -1+ (binexp->runcount1list n))
  )
)


(define (parenthesization->binruns p)
  (Nvector->binruns (map 1+ (map parenthesization->binruns p)))
)


(define (A075157 n) (-1+ (binruns->primefactorization n))) ;; Starts with offset 0.
(define (A075158 n) (Nvector->binruns (explist->Nvector! (primefactorization->explist (1+ n)))))

(define (A075159 n) (binruns->primefactorization (-1+ n))) ;; Starts with offset 1.
(define (A075160 n) (1+ (A075158 (-1+ n))))

;; Starts with offset 0.
(define (A075161 n)
   (CatalanRankGlobal
        (parenthesization->A014486 (primefactorization->parenthesization (1+ n)))
   )
)

(define (A075162 n)
   (-1+ (parenthesization->primefactorization
          (A014486->parenthesization (A014486 n))
        )
   )
)

;; Starts with offset 1
(define (A075163 n) (1+ (A075161 (-1+ n))))
(define (A075164 n) (1+ (A075162 (-1+ n))))

;; Starts with offset 1
(define (A075165 n)
   (parenthesization->A014486 (primefactorization->parenthesization n))
)

;; O=1
(define (A075166 n)
   (A007088 (parenthesization->A014486 (primefactorization->parenthesization n)))
)


;; O=1
(define (A075167v2 n) ;; New version now at miscnum2.scm
 (halve (binwidth (parenthesization->A014486 (primefactorization->parenthesization n))))
)


;; Starts with offset = 0.
(define (A075168 n)
   (CatalanRankGlobal
        (parenthesization->A014486 (binruns->parenthesization n))
   )
)

;; Offset = 0.
(define (A075169 n)
   (parenthesization->binruns (A014486->parenthesization (A014486 n)))
)

;; Starts with offset = 0.
(define (A075170 n) (parenthesization->A014486 (binruns->parenthesization n)))

(define (A075171 n)
   (A007088
        (parenthesization->A014486 (binruns->parenthesization n))
   )
)

(define (A075172 n)
   (halve (binwidth
            (parenthesization->A014486 (binruns->parenthesization n))
          )
   )
)



(define (prime-exponents->binary-interleaved-by NxN->N)
  (lambda (n)
    (let loop ((s 0) (i 0) (p-exps (reverse! (primefactorization->explist n))))
      (cond ((null? p-exps) s)
            (else (loop (+ s
                           (store-n-to-bits-given-by-column-x-of-table
                                      (car p-exps) i NxN->N
                           )
                        )
                        (1+ i)
                        (cdr p-exps)
                  )
            )
      )
    )
  )
)

;; --> (0 1 2 4 8 3 128 5 32 9 32768 6 ...)
(define A059884 (prime-exponents->binary-interleaved-by A075300bi))

(define (A059884v2 n)
   (let loop ((s 0) (i 0) (p-exps (reverse! (primefactorization->explist n))))
     (cond ((null? p-exps) s)
           (else (loop (+ s
                         (* (expt 2 (-1+ (expt 2 i)))
                            (expand-n-x-fold (car p-exps) (expt 2 (1+ i)))
                         )
                       )
                       (1+ i)
                       (cdr p-exps)
                 )
           )
     )
   )
)

(define (binary-interleaved-by->prime-exponents NxN->N)
  (lambda (n)
    (let loop ((p 1)
               (i 0)
               (n n)
               (m (fetch-from-bits-of-n-given-by-column-x-of-table n 0 NxN->N))
              )
      (cond
        ((zero? n) p)
        (else
          (loop (* p (expt (A008578off0 (1+ i)) m))
                (1+ i)
                (- n (store-n-to-bits-given-by-column-x-of-table m i NxN->N))
                (fetch-from-bits-of-n-given-by-column-x-of-table
                                       n (1+ i) NxN->N
                )
          )
        )
      )
    )
  )
)


;; Inverse of A059884: 1,2,3,6,4,8,12,24,5,10,15,30,20,40,60,120,16,32,48,96,64,128,192,
(define A059900 (binary-interleaved-by->prime-exponents A075300bi))


;; As A059884, but use an interleaved unary coding of exponents,
;; instead of binary.
;; This maps between the lattice (poset) defined by the ordinary
;; divisibility relation (x|y) and the Boolean lattice, thus it's
;; possible to implement all such operations as gcd, lcm, Moebius mu,
;; division test, etc. with a simple bitwise boolean operations
;; AND, OR, etc.
;; (Compute also the "inverse", with 0 inserted to those positions
;;  to which this N -> N injection doesn't map any value.)

;; Of course there are myriad of other variants, most of which
;; do not grow so steeply. (We can use row x of any NxN <-> N bijection
;; to select the bit-positions where to insert the unary coding
;; of the exponent of p_x.)



(define (prime-exponents->unary-interleaved-by NxN->N)
  (lambda (n)
    (let loop ((s 0) (i 0) (p-exps (reverse! (primefactorization->explist n))))
      (cond ((null? p-exps) s)
            (else (loop (+ s
                           (store-n-to-bits-given-by-column-x-of-table
                                    (-1+ (expt 2 (car p-exps))) i NxN->N
                           )
                        )
                        (1+ i)
                        (cdr p-exps)
                  )
            )
      )
    )
  )
)



(define (unary-interleaved-by->prime-exponents NxN->N)
  (lambda (n)
   (call-with-current-continuation 
    (lambda (exit-prematurely)
      (let loop ((p 1)
                 (i 0)
                 (n n)
                 (m (fetch-from-bits-of-n-given-by-column-x-of-table n 0 NxN->N))
                )
        (cond
          ((zero? n) p)
          ((not (= (1+ m) (expt 2 (binwidth m)))) (exit-prematurely 0))
          (else
            (loop (* p (expt (A008578off0 (1+ i)) (binwidth m)))
                  (1+ i)
                  (- n (store-n-to-bits-given-by-column-x-of-table m i NxN->N))
                  (fetch-from-bits-of-n-given-by-column-x-of-table
                                         n (1+ i) NxN->N
                  )
            )
          )
        )
      )
    )
   )
  )
)

;; (map A075173 (iota 21)) --> (0 1 2 5 8 3 128 21 34 9 32768 7 ...)
(define A075173 (prime-exponents->unary-interleaved-by A075300bi))

;; (map A075174 (iota0 21)) --> (1 2 3 6 0 4 0 12 5 10 15 30 0 20 0 60 0 0 0 0 0 8 ...)
(define A075174 (unary-interleaved-by->prime-exponents A075300bi))

(define A075175 (prime-exponents->unary-interleaved-by A001477bi))
(define A075176 (unary-interleaved-by->prime-exponents A001477bi))

(define (A003989biv2 x y) (A075174 (A004198bi (A075173 x) (A075173 y))))
(define (A003989biv3 x y) (A075176 (A004198bi (A075175 x) (A075175 y))))

(define (A003990biv2 x y) (A075174 (A003986bi (A075173 x) (A075173 y))))
(define (A003990biv3 x y) (A075176 (A003986bi (A075175 x) (A075175 y))))

(define (does_it_divide_v1? x y)
  (if (zero? (modulo y x))
      1
      0
  )
)

(define (does_it_divide_v2? x y)
  (if (zero? (A003987bi (A003986bi (A075173 x) (A075173 y)) (A075173 y)))
      1
      0
  )
)

(define (does_it_divide_v3? x y)
  (if (zero? (A003987bi (A003986bi (A075175 x) (A075175 y)) (A075175 y)))
      1
      0
  )
)


(define (A075173v2 n)
   (let loop ((s 0) (i 0) (p-exps (reverse! (primefactorization->explist n))))
     (cond ((null? p-exps) s)
           (else (loop (+ s
                         (* (expt 2 (-1+ (expt 2 i)))
                            (expand-n-x-fold (-1+ (expt 2 (car p-exps)))
                                             (expt 2 (1+ i))
                            )
                         )
                       )
                       (1+ i)
                       (cdr p-exps)
                 )
           )
     )
   )
)


;; Invoke like:
;;
;; (output-check-html "c:\\matikka\\Nekomorphisms\\chek2-55.htm" check-these2 55 52)
;;

;; The compositions between A075157, A075162 and A075168, etc.
;; were not correct. (It's more complicated than that...)


(define check-these2
 (list
       (list 0 75157 A075157 A075158 (list -1+ A075159 1+)) ; Not (list A075162 A075168)
       (list 0 75158 A075158 A075157 (list -1+ A075160 1+)) ; Not (list A075169 A075161)

       (list 1 75159 A075159 A075160 (list 1+ A075157 -1+))
       (list 1 75160 A075160 A075159 (list 1+ A075158 -1+))

       (list 0 75161 A075161 A075162 (list -1+ A075163 1+)) ; Not (list A075168 A075158)
       (list 0 75162 A075162 A075161 (list -1+ A075164 1+)) ; Not (list A075157 A075169)

       (list 1 75163 A075163 A075164 (list 1+ A075161 -1+))
       (list 1 75164 A075164 A075163 (list 1+ A075162 -1+))

       (list 1 75165 A075165)
       (list 1 75166 A075166)
       (list 1 75167 A075167v2)

       (list 0 75168 A075168 A075169) ;; Not! (list A075161 A075157)
       (list 0 75169 A075169 A075168) ;; Not! (list A075158 A075162)

       (list 0 75170 A075170)
       (list 0 75171 A075171)
       (list 0 75172 A075172)       
 )
)

(define check-these3
 (list
;;     (list 1 75173 A075173)
       (list 0 75174 A075174) ;; Can't check this inverse even upto n=40: A075173
       (list 1 75175 A075175)
       (list 0 75176 A075176) ;; Neither this: A075175 (offsets differ)
 )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;  Miscellaneous ideas.                                                  ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (next-left-branch L)
   (*A057163 (list (gmA057164 L)))
)

(define (nth-branch n) ((compose-fun-to-nth-power next-left-branch n) '()))

(define (next-branch! branches)
  (attach! (next-left-branch (car branches)) branches)
)


(define (iterate-and-print-left-branches outfile upto-n)
   (call-with-output-file outfile
     (lambda (outport)
       (let loop ((n 0)
                  (branch (list))
                  (br-reversed (list))
                 )
          (format outport "n=~A: (length branch)=~A" n (length branch))
          (cond ((or (not (pair? branch)) (null? (car br-reversed)))
                  (format outport " ends with ()")
                  (cond ((equal? br-reversed branch)
                           (format outport " and is symmetric: ~A" branch)
                        )
                  )
                )
          )
          (newline outport)
          (flush-output outport)
          (cond
            ((< n upto-n)
                (let ((next-branch (*A057163! (list br-reversed))))
                   (loop (1+ n)
                         next-branch
                         (gmA057164 next-branch)
                   )
                )
            )
          )
       )
     )
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;  Code based on the "generating trees approach"                         ;;
;;                                                                        ;;
;;  See for example:                                                      ;;
;;  Julian West: Permutation trees and the Catalan and Schröder numbers   ;;
;;  Discrete Math., 146: 247-262 (1995).                                  ;;
;;  URL: http://www.mala.bc.ca/~westj/papers/catsch.ps                    ;;
;;                                                                        ;;
;;  and:                                                                  ;;
;;  Banderier, Bousquet-Mélou, Denise, Flajolet, Gardy, Goyou-Beauchamps: ;;
;;  Generating Functions for Generating Trees                             ;;
;;  Discrete Mathematics 246(1-3), March 2002, pp. 29-55                  ;;
;;  URL: http://pauillac.inria.fr/algo/banderier/Papers/DiscMath99.ps     ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (writeln x) (write x) (newline))
(define (writerank x)
   (write (CatalanRankGlobal (parenthesization->A014486 x)))
   (write-string " ")
)


;; (expand-branch-Catalan '(2 1 0))
;; --> ((0 2 1 0) (1 2 1 0) (2 2 1 0) (3 2 1 0))


;; This conses a lots of common list structure. Maybe that
;; could be utilized when converting the code lists to
;; parenthesizations, so that the latter would also be composed
;; of commonly consed sub-expressions. (Yet another application
;; of the "spread-trees" idea?)

;; The rule for Catalan numbers is (k) ~> (0)(1)...(k-1)(k)(k+1)

(define (expand-branch-Catalan cr) ;; cr = code reversed.
  (let loop ((res (list (cons (1+ (if (pair? cr) (car cr) 0)) cr))))
     (cond ((zero? (caar res)) res)
           (else
              (loop (cons (cons (-1+ (caar res)) (cdar res)) res))
           )
     )
  )
)


;; The rule for Motzkin numbers is (k) ~> (0)(1)...(k-1)(k+1)
;; (With (0) ~> (1), and (1) ~> (0)(2))

;; Hmm, I have lost my track here... Yes, we have to use
;; Donaghey's M (A057505/A057506) here.

;; I guess we should have the following bijection:
;; ()      -> an empty tree; Motzkin path of the length 0.
;; (1)     -> a tree of one edge |,  Motzkin path of the length 1: - 1
;; (1 0)   -> a tree of two edges positioned serially, Motzkin path: -- 11
;; (1 2)   -> a tree of two branches ("parallel") \/, Motzkin path: /\ 20
;; (1 0 1) ->
;; (1 2 0)
;; (1 2 1)
;; (1 2 3)

(define (expand-branch-Motzkin cr) ;; cr = code reversed.
  (let loop ((res (list (cons (1+ (if (pair? cr) (car cr) 0)) cr))))
     (cond ((or (zero? (caar res))
                (and (eq? (caar res) 1)
                     (or (null? cr) (zero? (car cr)))
                )
            ) res)
           (else
              (if (and (pair? cr)
                       (eq? (caar res) (1+ (car cr)))
                       (not (zero? (car cr)))
                  )
                  (loop (cons (cons (- (caar res) 2) (cdar res)) res))
                  (loop (cons (cons (-1+ (caar res)) (cdar res)) res))
              )
           )
     )
  )
)


(define (expand-tree-upto-v1 n expand-branch foo code->p-fun)
  (let loop ((explist (list (list))))
    (cond ((not (null? explist))
             (foo (code->p-fun (reverse (car explist))))
             (loop (if (eq? (length (car explist)) n)
                       (cdr explist) ;; discard, don't expand this one.
                       (append! (cdr explist) (expand-branch (car explist)))
                   )
             )
          )
    )
  )
)


;; Implement a more elegant lazy-evaluation version later.

;; (Swap the argument order of append! and the order is screwed completely,
;;  and, furthermore, is dependent of n.)
(define (expand-tree-upto-v2 n expand-branch foo code->p-fun)
  (let loop ((explist (list (list))))
    (cond ((not (null? explist))
             (foo (code->p-fun (reverse (car explist))))
             (loop (if (eq? (length (car explist)) n)
                       (cdr explist) ;; discard, don't expand this one.
                       (append! (cdr explist) (expand-branch (car explist)))
                   )
             )
          )
    )
  )
)

;; This gives the parentheiszations in the lexicographic order (as in A014486):
;; (first-dislocated (cons 0 (collect-Catalan-tree-v1 7 (compose-funlist (list CatalanRankGlobal parenthesization->A014486)))))
;; --> ()
(define (collect-Catalan-tree-v1 upto_n fun)
   (let ((res (list (list))))
     (expand-tree-upto-v1
         upto_n
         expand-branch-Catalan
         (lambda (p) (attach! (fun p) res))
         code->p1
     )
     (cdr (reverse! res))
   )
)

;; This seems to give A057163: (1 3 2 8 7 6 5 4 22 21 20 18 17 19 16 15 13 12 14 11 10 9 ...)
;; (collect-Catalan-tree-v2 4 (compose-funlist (list CatalanRankGlobal parenthesization->A014486)))

(define (collect-Catalan-tree-v2 upto_n fun)
   (let ((res (list (list))))
     (expand-tree-upto-v2
         upto_n
         expand-branch-Catalan
         (lambda (p) (attach! (fun p) res))
         code->p2
     )
     (cdr (reverse! res))
   )
)

(define (collect-Motzkin-tree-v1 upto_n fun)
   (let ((res (list (list))))
     (expand-tree-upto-v1
         upto_n
         expand-branch-Motzkin
         (lambda (p) (attach! (fun p) res))
         code->p1
     )
     (cdr (reverse! res))
   )
)

(define (collect-Motzkin-tree-v2 upto_n fun)
   (let ((res (list (list))))
     (expand-tree-upto-v2
         upto_n
         expand-branch-Motzkin
         (lambda (p) (attach! (fun p) res))
         code->p2
     )
     (cdr (reverse! res))
   )
)

;; silmup is a pointer to a cons cell containing (()) in its car-part,
;; i.e. ((()) .... )

(define (grow-car-side! silmup update?)
  (let ((nb (list (list))))
    (set-car! (car silmup) nb) ;; i.e. (list (caar silmup))
    (if update? (set-car! silmup nb)) ;; i.e. (caar silmup)
    nb
  )
)

;; This grows the current branch one longer, i.e. adds a new () next
;; to the other:
(define (grow-cdr-side! silmup update?)
  (let ((nb (list (list))))
    (set-cdr! (car silmup) nb) ;; i.e. (list (cdar silmup))
    (if update? (set-car! silmup nb)) ;; i.e. (cdar silmup)
    nb
  )
)


(define (insert-after! nth lista item)
   (let ((ip (nthcdr nth lista)))
      (set-cdr! ip (cons item (cdr ip)))
   )
)

(define (code->p1 cs)
 (let* ((p (list (list))) ;; = (())
        (silmut (list p))
       )
    (let loop ((cs cs) (prev-c 0))
      (cond ((null? cs) p)
            ((> (car cs) prev-c) ;; New branch?
               (insert-after! prev-c
                              silmut
                              (grow-car-side! (nthcdr prev-c silmut) #f)
               )
               (loop (cdr cs) (car cs))
            )
            (else
               (grow-cdr-side! (nthcdr (car cs) silmut) #t)
               (loop (cdr cs) (car cs))
            )
      ) ;; cond
    ) ;; let
 ) ;; let*
)

;; The car/cdr-flipped variant of code->p1 given above.
(define (code->p2 cs)
 (let* ((p (list (list))) ;; = (())
        (silmut (list p))
       )
    (let loop ((cs cs) (prev-c 0))
      (cond ((null? cs) p)
            ((> (car cs) prev-c) ;; New branch?
               (insert-after! prev-c
                              silmut
                              (grow-cdr-side! (nthcdr prev-c silmut) #f)
               )
               (loop (cdr cs) (car cs))
            )
            (else
               (grow-car-side! (nthcdr (car cs) silmut) #t)
               (loop (cdr cs) (car cs))
            )
      ) ;; cond
    ) ;; let
 ) ;; let*
)



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

(define (p->code2 p)
  (let ((cs (list (list))))
     (let recurse ((p p) (level 0))
        (cond
          ((pair? p)
              (cond ((pair? (cdr p))
                       (attach! (1+ level) cs)
                       (recurse (cdr p) (1+ level))
                    )
              )
              (cond ((pair? (car p))
                       (attach! level cs)
                       (recurse (car p) level)
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

(define (max-one-step-rises-and-no-zeros? a)
   (cond ((null? a) #t)
         ((zero? (car a)) #f)
         ((null? (cdr a)) #t)
         ((> (cadr a) (1+ (car a))) #f)
         (else (max-one-step-rises-and-no-zeros? (cdr a)))
   )
)


(define (A071155->p1 c)
  (cond ((zero? c) (list))
        (else
          (let ((rfex (reverse! (n->factbase c))))
             (cond ((and (= (car rfex) 1)
                         (max-one-step-rises-and-no-zeros? rfex)
                    )
                      (code->p1 (map -1+ (cdr rfex)))
                   )
                   (else (list)) ;; c is not in A071155
             )
          )
        )
  )
)

(define A071155 (catfun2 p->factbase))
(define A071156 (compose-funs A085198 A014486))
(define A071156v2 (compose-funs A071155 A057164))
(define A071155v2 (compose-funs A071156 A057164))
(define A071157 (catfun2 (compose-funs baselist-as-decimal n->factbase p->factbase)))
(define A071158 (compose-funs A007623 A071156))
(define A071159 (catfun2 (compose-funs baselist-as-decimal p->zerofree-code1)))

(define (A071159raw n) (p->zerofree-code1 (A014486->parenthesization (A014486 n))))

(define (ind-of-first-non-rising lista)
  (let loop ((lista lista) (i 0) (prev 0))
        (cond ((null? lista) i)
              ((> (car lista) prev) (loop (cdr lista) (+ i 1) (car lista)))
              (else i)
        )
  )
)

(define (A239903_only_upto_16794 n) (if (zero? n) n (A235049 (A071159 (A081291 n)))))

(definec (A239903raw n)
  (if (zero? n)
      (list)
      (let loop ((n n) ;; First to be subtracted is A081290(n)
                 (row (A244160 n))
                 (col (- (A244160 n) 1))
                 (srow (- (A244160 n) 1))
                 (catstring (list 0))
                )
;;       (format #t "n=~a, row=~a, col=~a, catstring=~a\n" n row col catstring)
         (cond ((or (zero? row) (negative? col)) (reverse! (cdr catstring)))
               ((> (A009766tr row col) n) (loop n srow (- col 1) (- srow 1) (cons 0 catstring))) ;; One row up...
               (else (loop (- n (A009766tr row col)) (+ row 1) col srow (cons (+ 1 (car catstring)) (cdr catstring))))
         )
      )
  )
)

(define (A239903 n) (baselist-as-decimal (A239903raw n)))
(define (A239903v2 n) (if (zero? n) n (baselist-as-decimal (map -1+ (A071159raw (A081291 n)))))) ;; Good results up to n=58784.

(define (A236855 n) (apply + (A239903raw n)))
;;(define (A236855v2 n) (let ((x (A071155 (A081291 n)))) (+ (- (A034968 x) (A084558 x)) (- (A084558 x) (A060130 x)))))
(define (A236855v2 n) (let ((x (A071155 (A081291 n)))) (- (A034968 x) (A060130 x)))) ;; Reduces to this.

(define (A236859 n) (if (zero? n) n (- (A126307 (A081291 n)) 1)))

(definec (A237449 n) (- n (A236855 n)))

(define (CatBaseSum lista)
   (let loop ((digits (reverse lista)) (i 1) (s 0))
       (if (null? digits) s (loop (cdr digits) (+ i 1) (+ s (* (car digits) (A000108 i)))))
   )
)

(define (CatBaseSumVec digits)
  (let ((size (vector-length digits)))
    (let loop ((i size) (s 0))
        (if (zero? i) s (loop (- i 1) (+ s (* (vector-ref digits (- size i)) (A000108 i)))))
    )
  )
)

(define A244155 (FIXED-POINTS 0 0 (COMPOSE CatBaseSum A239903raw)))

(define A244156 (MATCHING-POS 1 1 (lambda (k) (not (= k (CatBaseSum (A239903raw k)))))))

(define (A244157 n) (- n (CatBaseSum (A239903raw n))))
(define (A244157v2 n) (- n (A244158 (A239903 n))))



(define (A014418raw_uncached n)
    (define (add-intermediate-zeros-into-base-list catbaserepr ind_of_previous_largest ind_of_largest)
        (if (zero? ind_of_previous_largest) ;; Except do not add any zeros at the first time...
            catbaserepr
            (append! (make-list (- ind_of_previous_largest ind_of_largest 1) 0)
                     catbaserepr
            )
        )
    ) ;; An internal function twice used.

    (let loop ((n n) ;; First to be subtracted is A081290(n)
               (catbaserepr (list))
               (ind_of_previous_largest 0) ;; Surely different at first time from ind_of_largest for all nonzero n.
              )
      (let* ((largest_cat_that_still_fits_into_the_bag (A081290 n))
             (ind_of_largest (- (A081288 largest_cat_that_still_fits_into_the_bag) 1))
            )
         (cond ((zero? n)
                 (reverse! (add-intermediate-zeros-into-base-list catbaserepr ind_of_previous_largest 0))
               )
               (else
                   (cond ((= ind_of_largest ind_of_previous_largest) ;; The same Cat fits one more time.
                            (loop (- n largest_cat_that_still_fits_into_the_bag)
                                  (cons (+ 1 (car catbaserepr)) (cdr catbaserepr)) ;; So increment the top digit.
                                  ind_of_largest
                            )
                         )
                         (else
                            (loop (- n largest_cat_that_still_fits_into_the_bag)
                                  (cons 1 ;; Add 1 to the front, and as many zeros between as necessary:
                                        (add-intermediate-zeros-into-base-list catbaserepr
                                                                               ind_of_previous_largest
                                                                               ind_of_largest
                                        )
                                  )
                                  ind_of_largest
                            )
                         )
                   )
               )
         )
      )
   )
)


(define (A014418raw_vector n)
  (if (zero? n)
      (make-vector 0)
      (let ((catbasevec (make-vector (A244160 n) 0)))
        (let loop ((n n))
           (cond ((zero? n) (vector-reverse catbasevec))
                 (else
                   (let ((k (A244160 n)))
                     (vector-set! catbasevec (- k 1) (+ 1 (vector-ref catbasevec (- k 1))))
                     (loop (- n (A000108 k)))
                   )
                 )
           )
        )
      )
  )
)


(definec (A014418raw n) (vector->list (A014418raw_vector n)))

(definec (A014418raw2 n) (A014418raw_uncached n)) ;; definec doesn't allow internal defines. A work-around.

(define (A014418v2 n) (baselist-as-decimal (A014418raw n))) ;; Representation of n in base of Catalan numbers.

(definec (A014418 n)
   (if (zero? n)
       n
       (+ (expt 10 (- (A244160 n) 1)) (A014418 (- n (A000108 (A244160 n)))))
   )
)

(define (A244161v2 n) (baselist-as-quaternary (A014418raw n))) ;; Base-4 version.

(definec (A244161 n)
   (if (zero? n)
       n
       (+ (expt 4 (- (A244160 n) 1)) (A244161 (- n (A000108 (A244160 n)))))
   )
)

(define (A244221 n) (A000035 (A244161 n)))

(define (A244220 n) (- 1 (A244221 n)))

(define A244222 (ZERO-POS 0 0 A244221)) ;; With "even" repr.
(define A244223 (NONZERO-POS 1 1 A244221)) ;; With "odd" repr.

(definec (A244224 n) (if (zero? n) 1 (+ (A244220 n) (A244224 (- n 1)))))

(definec (A244225 n) (if (<= n 1) n (+ (A244221 n) (A244225 (- n 1)))))

(define (A244229 n) (- (A244224 n) 1))
(define (A244229v2 n) (- n (A244225 n)))

(definec (A244226 n)
  (if (zero? n)
      1
      (let* ((prev_run_ends_at (A244218 (- n 1)))
             (prevpar (A244221 prev_run_ends_at))
            )
        (let loop ((i (+ 1 prev_run_ends_at)))
           (cond ((= (A244221 (+ i 1)) prevpar) (- i prev_run_ends_at))
                 (else (loop (+ i 1)))
           )
        )
      )
  )
)

(define (A244227 n) (A244226 (+ n n)))
(define (A244228 n) (+ 1 (A244227 n)))
(define (A244228v2 n) (- (A244223 (+ n 1)) (A244223 n))) ;; Works from n=1 onward.

(define (A131718 n) (cond ((gcd (modulo n 6) 6) => (lambda (m) (if (= 2 m) m 1))))) ;; Period 6: repeat 1, 1, 2, 1, 2, 1.

(definec (A244218 n) (if (zero? n) 0 (+ (A244226 n) (A244218 (- n 1)))))
(define A244218v2 (NONZERO-POS 0 0 (lambda (n) (modulo (- (A014418 (+ n 1)) (A014418 n)) 2))))

(define (A244219 n) (if (zero? n) 0 (+ 1 (A244218 (- n 1)))))
;; (define A244219v2 (NONZERO-POS 1 1 (lambda (n) (modulo (- (A014418 (- n 1)) (A014418 n)) 2)))) ;; For n>=1



(define (A014420 n) (apply + (A014418raw n)))

(define (A244318 n) (apply * (map 1+ (A014418raw n))))

(define (A244320 n) (- n (A014420 n)))


;; (define A197433maybe_check (MATCHING-POS 0 0 (lambda (k) (equal? (A014418raw k) (A239903raw k))))) ;; Yes it is!

(define (A244159raw n)
  (if (zero? n)
      (make-vector 0)
      (let* ((maxsize (A244160 n))
             (catbasevec (make-vector maxsize 0))
            )
          (let outer_loop ((n n))
              (let inner_loop ((n n) (i (A244160 n)))
                 (cond ((zero? n) (vector-reverse catbasevec))
                       ((zero? i) (outer_loop n))
                       ((<= (A000108 i) n)
                           (begin
                              (vector-set! catbasevec (- i 1) (+ 1 (vector-ref catbasevec (- i 1))))
                              (inner_loop (- n (A000108 i)) (- i 1))
                           )
                       )
                       (else (inner_loop n (- i 1)))
                 )
              )
          )
      )
  )
)


(define (A244315 n)
  (let outer_loop ((n n))
     (let inner_loop ((n n) (i (A244160 n)))
        (cond ((zero? n) i)
              ((zero? i) (outer_loop n))
              ((<= (A000108 i) n) (inner_loop (- n (A000108 i)) (- i 1)))
              (else (inner_loop n (- i 1)))
        )
     )
  )
)

(define (A244316 n) (if (zero? n) n (+ (A244315 n) 1)))


(definec (A244159 n) (basevec-as-decimal (A244159raw n)))

(define (A244231 n) (if (zero? n) 0 (apply max (vector->list (A244159raw n)))))

(define (A244232v2 n) (apply + (vector->list (A244159raw n))))

(define (A244233 n) (apply * (vector->list (A244159raw n))))

(define (A244234v2 n) (- n (A244232 n)))

;; A244314-A244323 are now reserved for your use.

(define A244314v2 (ZERO-POS 1 1 A244233))

;; Inverse of A071155, i.e. A085200(A071155(n)) = n for all n.
(define A085200 (compose-funs A080300 parenthesization->A014486 A071155->p1))

;; The inverse function of A071156, i.e. A085199(A071156(n)) = n for all n.
(define A085199 (compose-funs A057164 A085200))


;; (output-check-html "C:\\matikka\\nekomorphisms\\seqs\\a85183-99.htm" check-A085183-99 119 30 #f)
(define check-A085183-99
 (list
       (list 119 1 85183 A085183)
       (list 119 1 85184 A085184)
       (list 119 0 85185 A085185)
       (list 119 0 85186 A085186)
       (list 119 0 85187 A085187)
       (list 119 0 85188 A085188)
       (list 119 0 85189 A085189)
       (list 119 0 85190 A085190)
       (list 119 0 85191 A085191)
       (list 119 0 85192 A085192)
       (list 119 0 85193 A085193)
       (list 119 0 85194 A085194)
       (list 119 0 85195 A085195)
       (list 119 0 85196 A085196)
       (list 119 1 85197 A085197)
       (list 119 0 85198 A085198)
       (list 119 0 85199 A085199)
 )
)

;; (output-check-html "C:\\matikka\\nekomorphisms\\seqs\\a85200-8.htm" check-A085200-8 119 30 #f)
(define check-A085200-8
 (list
       (list 119 0 84558 A084558)
       (list 119 0 85200 A085200)
       (list 119 0 85201 A085201)
       (list 119 0 85202 A085202)
       (list 119 0 85203 A085203)
       (list 119 0 85204 A085204)
       (list 119 0 85205 A085205)
       (list 119 0 85206 A085206)
       (list 119 0 85207 A085207)
       (list 119 0 85208 A085208)
       (list 119 0 85209 A085209)
       (list 119 0 85210 A085210)
       (list 119 0 85211 A085211)
       (list 119 0 85212 A085212)
       (list 119 0 85213 A085213)
       (list 119 0 85214 A085214)
       (list 119 0 85215 A085215)
       (list 119 0 85216 A085216)
       (list 119 0 85217 A085217)
       (list 119 0 85218 A085218)
       (list 119 0 85219 A085219)
       (list 119 0 85220 A085220)
       (list 119 0 85221 A085221)
       (list 119 0 85222 A085222)
       (list 119 0 85223 A085223)
       (list 119 0 85224 A085224)
       (list 119 0 85225 A085225)
       (list 119 0 85226 A085226)
       (list 119 0 85227 A085227)
       (list 119 0 85228 A085228)
 )
)


;; (output_seq (map p->factbase (map A014486->parenthesization (map A014486 (iota0 196)))))
;; --> 0,1,3,5,9,15,11,17,23,33,57,39,63,87,35,59,41,65,89,47,71,95,119,153,273,177,297,417,159,279,183,303,423,207,327,447,567,155,275,179,299,419,161,281,185,305,425,209,329,449,569,167,287,191,311,431,215,335,455,575,239,359,479,599,719,873,1593,993,1713,2433,897,1617,1017,1737,2457,1137,1857,2577,3297,879,1599,999,1719,2439,903,1623,1023,1743,2463,1143,1863,2583,3303,927,1647,1047,1767,2487,1167,1887,2607,3327,1287,2007,2727,3447,4167,875,1595,995,1715,2435,899,1619,1019,1739,2459,1139,1859,2579,3299,881,1601,1001,1721,2441,905,1625,1025,1745,2465,1145,1865,2585,3305,929,1649,1049,1769,2489,1169,1889,2609,3329,1289,2009,2729,3449,4169,887,1607,1007,1727,2447,911,1631,1031,1751,2471,1151,1871,2591,3311,935,1655,1055,1775,2495,1175,1895,2615,3335,1295,2015,2735,3455,4175,959,1679,1079,1799,2519,1199,1919,2639,3359,1319,2039,2759,3479,4199,1439,2159,2879,3599,4319,5039

;; (output_seq (sort (map p->factbase (map A014486->parenthesization (map A014486 (iota0 196)))) <))
;; --> 0,1,3,5,9,11,15,17,23,33,35,39,41,47,57,59,63,65,71,87,89,95,119,153,155,159,161,167,177,179,183,185,191,207,209,215,239,273,275,279,281,287,297,299,303,305,311,327,329,335,359,417,419,423,425,431,447,449,455,479,567,569,575,599,719,873,875,879,881,887,897,899,903,905,911,927,929,935,959,993,995,999,1001,1007,1017,1019,1023,1025,1031,1047,1049,1055,1079,1137,1139,1143,1145,1151,1167,1169,1175,1199,1287,1289,1295,1319,1439,1593,1595,1599,1601,1607,1617,1619,1623,1625,1631,1647,1649,1655,1679,1713,1715,1719,1721,1727,1737,1739,1743,1745,1751,1767,1769,1775,1799,1857,1859,1863,1865,1871,1887,1889,1895,1919,2007,2009,2015,2039,2159,2433,2435,2439,2441,2447,2457,2459,2463,2465,2471,2487,2489,2495,2519,2577,2579,2583,2585,2591,2607,2609,2615,2639,2727,2729,2735,2759,2879,3297,3299,3303,3305,3311,3327,3329,3335,3359,3447,3449,3455,3479,3599,4167,4169,4175,4199,4319,5039

;; (output_seq (map (lambda (bl) (baselist->n 10 bl)) (map n->factbase (map p->factbase (map A014486->parenthesization (map A014486 (iota0 196)))))))
;; --> 0,1,11,21,111,211,121,221,321,1111,2111,1211,2211,3211,1121,2121,1221,2221,3221,1321,2321,3321,4321,11111,21111,12111,22111,32111,11211,21211,12211,22211,32211,13211,23211,33211,43211,11121,21121,12121,22121,32121,11221,21221,12221,22221,32221,13221,23221,33221,43221,11321,21321,12321,22321,32321,13321,23321,33321,43321,14321,24321,34321,44321,54321,111111,211111,121111,221111,321111,112111,212111,122111,222111,322111,132111,232111,332111,432111,111211,211211,121211,221211,321211,112211,212211,122211,222211,322211,132211,232211,332211,432211,113211,213211,123211,223211,323211,133211,233211,333211,433211,143211,243211,343211,443211,543211,111121,211121,121121,221121,321121,112121,212121,122121,222121,322121,132121,232121,332121,432121,111221,211221,121221,221221,321221,112221,212221,122221,222221,322221,132221,232221,332221,432221,113221,213221,123221,223221,323221,133221,233221,333221,433221,143221,243221,343221,443221,543221,111321,211321,121321,221321,321321,112321,212321,122321,222321,322321,132321,232321,332321,432321,113321,213321,123321,223321,323321,133321,233321,333321,433321,143321,243321,343321,443321,543321,114321,214321,124321,224321,324321,134321,234321,334321,434321,144321,244321,344321,444321,544321,154321,254321,354321,454321,554321,654321

;; (output_seq (map (lambda (bl) (baselist->n 10 bl)) (map n->factbase (sort (map p->factbase (map A014486->parenthesization (map A014486 (iota0 196)))) <))))
;; --> 0,1,11,21,111,121,211,221,321,1111,1121,1211,1221,1321,2111,2121,2211,2221,2321,3211,3221,3321,4321,11111,11121,11211,11221,11321,12111,12121,12211,12221,12321,13211,13221,13321,14321,21111,21121,21211,21221,21321,22111,22121,22211,22221,22321,23211,23221,23321,24321,32111,32121,32211,32221,32321,33211,33221,33321,34321,43211,43221,43321,44321,54321,111111,111121,111211,111221,111321,112111,112121,112211,112221,112321,113211,113221,113321,114321,121111,121121,121211,121221,121321,122111,122121,122211,122221,122321,123211,123221,123321,124321,132111,132121,132211,132221,132321,133211,133221,133321,134321,143211,143221,143321,144321,154321,211111,211121,211211,211221,211321,212111,212121,212211,212221,212321,213211,213221,213321,214321,221111,221121,221211,221221,221321,222111,222121,222211,222221,222321,223211,223221,223321,224321,232111,232121,232211,232221,232321,233211,233221,233321,234321,243211,243221,243321,244321,254321,321111,321121,321211,321221,321321,322111,322121,322211,322221,322321,323211,323221,323321,324321,332111,332121,332211,332221,332321,333211,333221,333321,334321,343211,343221,343321,344321,354321,432111,432121,432211,432221,432321,433211,433221,433321,434321,443211,443221,443321,444321,454321,543211,543221,543321,544321,554321,654321

;; (output_seq (map  (lambda (bl) (baselist->n 10 bl)) (map p->zerofree-code1 (map A014486->parenthesization (map A014486 (iota0 196))))))
;; --> 0,1,11,12,111,112,121,122,123,1111,1112,1121,1122,1123,1211,1212,1221,1222,1223,1231,1232,1233,1234,11111,11112,11121,11122,11123,11211,11212,11221,11222,11223,11231,11232,11233,11234,12111,12112,12121,12122,12123,12211,12212,12221,12222,12223,12231,12232,12233,12234,12311,12312,12321,12322,12323,12331,12332,12333,12334,12341,12342,12343,12344,12345,111111,111112,111121,111122,111123,111211,111212,111221,111222,111223,111231,111232,111233,111234,112111,112112,112121,112122,112123,112211,112212,112221,112222,112223,112231,112232,112233,112234,112311,112312,112321,112322,112323,112331,112332,112333,112334,112341,112342,112343,112344,112345,121111,121112,121121,121122,121123,121211,121212,121221,121222,121223,121231,121232,121233,121234,122111,122112,122121,122122,122123,122211,122212,122221,122222,122223,122231,122232,122233,122234,122311,122312,122321,122322,122323,122331,122332,122333,122334,122341,122342,122343,122344,122345,123111,123112,123121,123122,123123,123211,123212,123221,123222,123223,123231,123232,123233,123234,123311,123312,123321,123322,123323,123331,123332,123333,123334,123341,123342,123343,123344,123345,123411,123412,123421,123422,123423,123431,123432,123433,123434,123441,123442,123443,123444,123445,123451,123452,123453,123454,123455,123456


;; (expand-tree-upto-v1 3 (lambda (x) (writeln (p->code1 x))))
;; or:
;; (expand-tree-upto-v2 3 (lambda (x) (writeln (p->code2 x))))
;; ()
;; (0)
;; (1)
;; (0 0)
;; (0 1)
;; (1 0)
;; (1 1)
;; (1 2)
;; (0 0 0)
;; (0 0 1)
;; (0 1 0)
;; (0 1 1)
;; (0 1 2)
;; (1 0 0)
;; (1 0 1)
;; (1 1 0)
;; (1 1 1)
;; (1 1 2)
;; (1 2 0)
;; (1 2 1)
;; (1 2 2)
;; (1 2 3)



;; (expand-tree-upto-v1 3 (lambda (x) (writeln (p->zerofree-code1 x))))
;; (1)
;; (1 1)
;; (1 2)
;; (1 1 1)
;; (1 1 2)
;; (1 2 1)
;; (1 2 2)
;; (1 2 3)
;; (1 1 1 1)
;; (1 1 1 2)
;; (1 1 2 1)
;; (1 1 2 2)
;; (1 1 2 3)
;; (1 2 1 1)
;; (1 2 1 2)
;; (1 2 2 1)
;; (1 2 2 2)
;; (1 2 2 3)
;; (1 2 3 1)
;; (1 2 3 2)
;; (1 2 3 3)
;; (1 2 3 4)

