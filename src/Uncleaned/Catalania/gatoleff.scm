
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;;  http://www.megabaud.fi/~karttu/matikka/Nekomorphisms/gatoleff.scm   ;;
;;  - Functions for implementing "LEPREFF"'s,                           ;;
;;  i.e. "Leaves Preserving Forth-Forms"                                ;;
;;                                                                      ;;
;;  This Scheme-code is Copyright (C) 2002 by Antti Karttunen           ;;
;;  (E-mail: my_firstname.my_surname@iki.fi) and is placed under        ;;
;;  the GPL (Gnu Public License), so you are free to copy it.           ;;
;;                                                                      ;;
;;  Runs at least in MIT Scheme Release 7.6.0, for which one can find   ;;
;;  documentation and the pre-compiled binaries (for various OS's       ;;
;;  running in Intel x86 architecture) under the URL:                   ;;
;;                                                                      ;;
;;  http://www.swiss.ai.mit.edu/projects/scheme/                        ;;
;;                                                                      ;;
;;  The main pointer for this code collection is:                       ;;
;;  http://www.megabaud.fi/~karttu/matikka/Nekomorphisms/gatomorf.htm   ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; From http://www.swiss.ai.mit.edu/projects/scheme/documentation/user_5.html
;;
;; If you redefine some global name in your code, for example, car, cdr
;; and cons, you should indicate it in the declaration as:
;; (declare (usual-integrations car cdr cons))

;; (Beware of using an argument name like list in the function definitions?)

(declare (usual-integrations))


;; Some inspiration from: http://www.its.caltech.edu/~boozer/symbols/pr.html
;;
;; and from the papers of Henry G. Baker:
;; "Linear Logic and Permutation Stacks -- The Forth Shall Be First"
;; at http://linux.rice.edu/~rahul/hbaker/ForthStack.html
;; "NREVERSAL of Fortune"
;; at http://linux.rice.edu/~rahul/hbaker/ReverseGC.html
;; and "Lively Linear Lisp"
;; at http://linux.rice.edu/~rahul/hbaker/LinearLisp.html

;; At the left (car) side map even integers to all integers,
;; and odd integers to sublists,
;; and at the right (cdr) side map all integers to sublists,
;; with 0 mapped to the terminating ().

;; How to generate a subset that begins and ends always
;; with two parentheses? (i.e. with a sublist)?
;; (or is always a list of one sublist. Easy.)

(define (unrank-intsexp rank pr1 pr2)
  (let recurse ((rank rank) (side 'oikea))
    (cond ((eq? side 'oikea) ;; At the right side.
             (cond ((zero? rank) (list)) ;; 0 marks the terminating nil.
                   (else (cons (recurse (pr1 (-1+ rank)) 'vasen)
                               (recurse (pr2 (-1+ rank)) 'oikea)
                         )
                   )
             )
          )
          (else ;; At the left side
             (cond ((zero? (modulo rank 2)) (/ rank 2)) ;; 2n -> n
                   (else (cons (recurse (pr1 (/ (-1+ rank) 2)) 'vasen)
                               (recurse (pr2 (/ (-1+ rank) 2)) 'oikea)
                         )
                   )
             )
          )
    )
  )
)


(define (rank-intsexp is packfun)
  (let recurse ((is is) (side 'oikea))
    (cond ((integer? is) (* is 2)) ;; n -> 2n (integers only on the car-side)
          ((null? is) 0) ;; nils should occur only on the cdr-side.
          (else (1+ (* (if (eq? side 'vasen) 2 1)
                       (packfun (recurse (car is) 'vasen)
                                (recurse (cdr is) 'oikea)
                       )
                    )
                )
          )
    )
  )
)

(define (unrank-intsexp-tr rank) (unrank-intsexp rank A025581 A002262))
(define (rank-intsexp-tr sexp)   (rank-intsexp sexp packA001477))

;; lepreff stands for "Leaves Preserving Forth-Form"

(define (map-int-to-prim-at-level n depth)
    (cond ((eq? 1 depth) (list 'L n)) ;; Only calls to other lepreff's allowed.
          ((eq? 2 depth)                    ;; Also SWAPs & recursion calls allowed.
              (cond ((zero? n) 'SWAP)
                    ((eq? 1 n) 'RECURSE)
                    (else (list 'L (- n 2)))
              )
          )
          (else ;; Stack depth 3 or more. Also ROTation of the whole stack allowed.
              (cond ((zero? n) 'SWAP)
                    ((eq? 1 n) 'ROTN)
                    ((eq? 2 n) 'RECURSE)
                    (else (list 'L (- n 3)))
              )
          )
    )
)


;; Increment all integers that occur either as in
;;
;; a) any non-initial or non-terminal position of
;;    a top-level or any level list.
;;    (i x x x x x t)
;;
;; b) the initial position of the sublist which
;;    itself is not an initial element of its
;;    parent list, and is preceded by another sublist
;;    (not an integer).
;;    (( ... ) (X ...))
;;
;; c) the terminal position of the sublist which
;;    itself is not an terminal element of its
;;    parent list, and is followed by another sublist
;;    (not an integer).
;;    (( ... X) (...))

;; I.e., do not increment an integer if it occurs
;; as the initial element of a top-level list,
;; or of a sublist which is in the initial position
;; of its parent-list, or is preceded by an integer,
;; or if it occurs
;; as the terminal element of a top-level list,
;; or of a sublist which is in the terminal position
;; of its parent-list, or is followed by an integer.
;; or both (is the only element of a sublist which
;; is neither followed nor preceded by another sublist)

;; (0) --> (0)
;; ((0)) --> ((0))
;; (0 0) --> (0 0)
;; (1) --> (1)
;; ((0) 0) --> ((0) 0)
;; (0 (0)) --> (0 (0))
;; (((0))) --> (((0)))
;; (1 0) --> (1 0)
;; ((0) (0)) --> ((1) (1))
;; (0 0 0) --> (0 1 0)
;; (2) --> (2)
;; (((0)) 0) --> (((0)) 0)
;; (1 (0)) --> (1 (0))
;; ((0) 0 0) --> ((0) 1 0)
;; (0 1) --> (0 1)
;; ((0 0)) --> ((0 0))
;; (2 0) --> (2 0)
;; (((0)) (0)) --> (((0)) (1))
;; (1 0 0) --> (1 1 0)
;; ((0) 1) --> ((0) 1)
;; (0 (0) 0) --> (0 (0) 0)
;; (3) --> (3)
;; ((0 0) 0) --> ((0 0) 0)
;; (2 (0)) --> (2 (0))
;; (((0)) 0 0) --> (((0)) 1 0)
;; (1 1) --> (1 1)
;; ((0) (0) 0) --> ((1) (1) 0)
;; (0 0 (0)) --> (0 1 (0))
;; ((1)) --> ((1))
;; (3 0) --> (3 0)
;; ((0 0) (0)) --> ((0 1) (1))
;; (2 0 0) --> (2 1 0)
;; (((0)) 1) --> (((0)) 1)
;; (1 (0) 0) --> (1 (0) 0)
;; ((0) 0 (0)) --> ((0) 1 (0))
;; (0 ((0))) --> (0 ((0)))
;; (4) --> (4)

;; (intsexp-increment-selectively! (copy-tree '(0 (0 (0) 0) (0 0) (0 ((0)) 0) 0)))
;; --> (0 (0 (0) 1) (1 1) (1 ((0)) 0) 0)

;; (1 2 3 (0) 0 4)       --> (1 3 4 (0) 1 4)
;; ((1 2 3 (0)) (3))     --> ((1 3 4 (0)) (4))
;;
;; ((0) 0 4)             --> ((0) 1 4)
;; (((0)) (3))           --> (((0)) (4))
;;
;; (((0)) 0 4)           --> (((0)) 1 4)
;; ((((0))) (3))         --> ((((0))) (4))
;;
;; (1 2 3 ((0)) 0 4)     --> (1 3 4 ((0)) 1 4)
;; ((1 2 3 ((0))) (3))   --> ((1 3 4 ((0))) (4))
;;
;; (2 (3 4 ((0)) 0 5))   --> (2 (3 5 ((0)) 1 5))
;; (2 (3 4 ((0))) (4))   --> (2 (3 5 ((0))) (5))
;; ((2 (3 4 ((0))) (4))) --> ((2 (3 5 ((0))) (5)))

;; ((2 3 ((0)) 0 4) 5 6) --> ((2 4 ((0)) 1 4) 6 6)
;; ((2 3 ((0))) (3 5 6)) --> ((2 4 ((0))) (4 6 6))

(define (intsexp-increment-selectively! is) (intsexp-change-selectively! is 1+))


(define (intsexp-change-selectively! is how)
  (let recurse ((is is) (pre '()) (depth 0))
    (cond
      ((pair? is)
         (cond ((pair? (car is)) ;; We have a sublist.
                  (cond ((and (integer? (caar is)) ;; Whose first elem is integer.
                              (pair? pre) ;; Sublist preceded by another sublist
                         )
                           (set-car! (car is) (how (caar is))) ;; Incr first
                        )
                  )
                  (cond ((and (or (not (pair? pre)) ;; Make sure that didn't match above
                                  (pair? (cdar is)) ;; or the length of sublist > 1
                              )                     ;; (to avoid double-change)
                              (integer? (car (last-pair (car is)))) ;; last elem = integer
                              (pair? (cdr is))  ;; And sublist is followed by ...
                              (pair? (cadr is)) ;; ...  another sublist?
                         )
                           (set-car! (last-pair (car is))
                                     (how (car (last-pair (car is))))
                           )
                        )
                  )
                  (recurse (car is) '() (1+ depth))
               )
               (else ;; We have an integer at this position.
                     ;; Change if we are in the middle of the sublist:
                   (cond ((and (> depth 0) (not (null? pre)) (not (null? (cdr is))))
                            (set-car! is (how (car is)))
                         )
                   )
               )
         )
         (recurse (cdr is) (car is) depth) ;; Handle the rest.
      )
    ) ;; cond
  ) ;; let
  is
)




(define (intsexp->lepreff-clause is)
  (let recurse ((is is) (depth 1))
    (cond ((null? is) is)
          ((list? (car is))
             (append! (list '<SNOC>) ;; SNOC
                      (recurse (car is) (1+ depth))
                      (list '<CONS>) ;; CONS
                      (recurse (cdr is) depth)
             )
          )
          (else ;; An integer.
             (cons (map-int-to-prim-at-level (car is) depth)
                   (recurse (cdr is) depth)
             )
          )
    ) ;; cond
  ) ;; let
)


(define (cons-snocs->swaps fl)
  (let loop ((fl fl) (res (list)))
    (cond ((null? fl) (reverse! res))
          ((and (pair? (cdr fl))
                (eq? '<CONS> (car fl))  ;; level -1
                (eq? '<SNOC> (cadr fl)) ;; level +1
           )
             (loop (cddr fl) (cons 'SWAP res))
          )
          (else (loop (cdr fl) (cons (car fl) res)))
    )
  )
)

(define (lepreff0 n) (intsexp->lepreff-clause (unrank-intsexp-tr n)))
(define (lepreff1 n)
   (cons-snocs->swaps
       (intsexp->lepreff-clause
           (intsexp-increment-selectively!
               (unrank-intsexp-tr n)
           )
       )
   )
)



