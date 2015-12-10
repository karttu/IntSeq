;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;;
;;                          gatomain.scm                                 ;;
;;                                                                       ;;
;;  Antti Karttunen's collection of rotation-automorphisms and other     ;;
;;             mappings of Catalan-enumerated objects                    ;;
;;     "Catalan automorphisms", previously known as 'Gatomorphisms'      ;;
;; ('Catamorphism' is already reserved by Constructive Algorithmics)     ;;
;;  But... an interesting result is that ...                             ;;
;;   every Catalan automorphism indeed is a Catamorphism!                ;;
;;  This is based on the following paper:                                ;;
;;  Jeremy Gibbons, Graham Hutton and Thorsten Altenkirch,               ;;
;;  "When is a function a fold or an unfold?",                           ;;
;;  Electronic Notes in Theoretical Computer Science, 44 (2001), no. 1.  ;;
;;                                                                       ;;
;;  This code runs at least in MIT Scheme Release 7.7.1, for             ;;
;;  which one can find documentation and the pre-compiled binaries       ;;
;;  (for various OS's running in Intel x86 architecture) under the URL:  ;;
;;                                                                       ;;
;;  http://www.swiss.ai.mit.edu/projects/scheme/                         ;;
;;                                                                       ;;
;;  Aubrey Jaffer's SLIB Scheme library is available at:                 ;;
;;  http://www.swiss.ai.mit.edu/~jaffer/SLIB.html                        ;;
;;                                                                       ;;
;;  The main pointer for this code collection is:                        ;;
;;  http://www.iki.fi/~kartturi/matikka/Nekomorphisms/gatomorf.htm       ;;
;;                                                                       ;;
;;  Last edited 2014-06-07                                               ;;
;;                                                                       ;;
;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;;
;; "Catalan automorphism" (noun) = any bijection from a set of           ;;
;; parenthesizations of size n to the same set (of size n),              ;;
;; which is well-defined for all the sizes n (for sizes n=0 and 1 we     ;;
;; have an identity mapping).                                            ;;
;; In place of parenthesizations we can assume any other manifestation   ;;
;; of the exercise 19 by Stanley.                                        ;;
;;                                                                       ;;
;; See R. P. Stanley, Exercises on Catalan and Related Numbers,          ;;
;; located at: http://www-math.mit.edu/~rstan/ec/catalan.pdf             ;;
;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Porting from Maple to Scheme started by Antti Karttunen
;; (E-mail: my_firstname.my_surname@gmail.com)
;; in March 2002. These functions are in public domain.

;; Works in MIT Scheme, release 7.6.0. And 7.7.? as well.

;; To be done: output of the objects with their rotations
;; Use either MIT Scheme native Win32 graphics routines and/or
;; FPS (functional PostScript) library at
;; ftp://ftp.scsh.net/pub/scsh/contrib/fps/doc/fps.html
;; (and maybe write an article about the whole project)


;; From http://www.swiss.ai.mit.edu/projects/scheme/documentation/user_5.html
;;
;; If you redefine some global name in your code, for example, car, cdr
;; and cons, you should indicate it in the declaration as:
;; (declare (usual-integrations car cdr cons))

;; (Beware of using an argument name like 'list' in the function definitions?)

(declare (usual-integrations))


;; (load "c:/slib/mitscheme.init") ;; Aubrey Jaffer's SLIB Scheme library.
;; (require 'factor)

(define (load-me) (load "/users/karttu/A/matikka/Nekomorphisms/gatomain.scm"))
;; (set-working-directory-pathname! "/users/karttu/A/matikka/Nekomorphisms")

(load "../Schemuli/definech.com") ;; Use hygienic version now.
;; (load "../Schemuli/lstfuns1")
(load "../Schemuli/intfuns1")

(load "./gatomorf") ;; .com
(load "./gatochek")
;; (load "./gatoleff")
(load "./gatorank.com")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;;           Miscellaneous left-over stuff follow...                  ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Don't use destructive version here!
;; (output-gatomorphism-partitions "A057161.sxp" "RotateTriangularization" RotateTriangularization 5)
;; (output-gatomorphism-partitions "A057501.sxp" "RotateHandshakes" RotateHandshakes 5)

;; To be read in with gato-fps.scm's function:
;; (output-part-file-as-ps-file "a057161.sxp" "a057161.ps" 8)
;;

(define (output-gatomorphism-partitions filename gatoname gatomorphism upto_n)
   (call-with-output-file filename
     (lambda (outport)
        (write (list gatoname upto_n) outport)
        (newline outport)
        (let loop ((i 0))
           (cond ((<= i upto_n)
                    (write (partition_by_gatomorphism i gatomorphism) outport)
                    (newline outport)
                    (flush-output outport)
                    (loop (1+ i))
                 )
           )
        )
     )
   )
)


;; (output-gatomorphism-fixpoints "c:/matikka/Nekomorphisms/A057546.sxp" "A057546 = Fixpoints of A057511/A057512" DeepRol 10)
(define (fixes_A071661)
 (output-gatomorphism-fixpoints "c:/matikka/Nekomorphisms/A071661.sxp" "A?????? = Fixpoints of A071661/A071662" *A071661 14)
)

(define (fixed_by_A057118)
 (output-gatomorphism-fixpoints-opl "A102241.sxp" "A102241 = Fixpoints of A057117/A057118" *A057118 14)
)

(define (fixed_by_A072089)
 (output-gatomorphism-fixpoints-opl "A102244.sxp" "A102244 = Fixpoints of A072088/A072089" *A072089 7)
)

(define (fixed_by_A057511)
 (output-gatomorphism-fixpoints-opl "A102247.sxp" "A102247 = Fixpoints of A057511/A057512"
    (lambda (p) (*A057511! (copy-tree p))) 12
 )
)

(define (output-gatomorphism-fixpoints filename gatoname gatomorphism upto_n)
   (call-with-output-file filename
     (lambda (outport)
        (write (list gatoname upto_n) outport)
        (newline outport)
        (let loop ((i 0))
           (cond ((<= i upto_n)
                    (write (list (fixed-by-gatomorphism i gatomorphism)) outport)
                    (newline outport)
                    (flush-output outport)
                    (loop (1+ i))
                 )
           )
        )
     )
   )
)

;; opl = One Per Line.
(define (output-gatomorphism-fixpoints-opl filename gatoname gatomorphism upto_n)
   (call-with-output-file filename
     (lambda (outport)
        (write (list gatoname upto_n) outport)
        (newline outport)
        (let loop ((i 0))
           (cond ((<= i upto_n)
                    (for-each
                        (lambda (fp)
                            (write (list (list fp)) outport)
                            (newline outport)
                            (flush-output outport)
                        )
                        (fixed-by-gatomorphism i gatomorphism)
                    )
                    (loop (1+ i))
                 )
           )
        )
     )
   )
)


(define (two-cycles-of-A057505)
 (output-gatomorphism-x-cycles "c:/matikka/Nekomorphisms/A57505_2.lst" "c:/matikka/Nekomorphisms/A079438.sxp" DeepRotateTriangularization "A079438 = Fixpoints of A071661/A071662")
)

(define (three-cycles-of-A057505)
 (output-gatomorphism-x-cycles "c:/matikka/Nekomorphisms/A57505_3.lst" "c:/matikka/Nekomorphisms/A079442.sxp" DeepRotateTriangularization "A079442 = Fixpoints of A071663/A071664")
)

(define (read-lists-from infile) ;; Borrowed from gato-fps.scm
   (call-with-input-file infile
      (lambda (inport)
           (let loop ((sexp (read inport)) (res (list)))
                 (cond ((eof-object? sexp) (reverse! res))
                       (else (loop (read inport) (cons sexp res)))
                 )
           )
      )
   )
)


(define (cycle-size-under t1 gatomorphism)
   (let loop ((t (gatomorphism (copy-tree t1))) (s 1))
          (cond ((equal? t t1) s)
                (else (loop (gatomorphism t) (1+ s)))
          )
   )
)

(define (expand-cycle-representative cr gatomorphism)
   (let ((eka (A014486->parenthesization (A014486 (car cr)))))
     (let loop ((s (gatomorphism eka)) (res (list eka)))
           (cond ((equal? s eka) (reverse! res))
                 (else (loop (gatomorphism s) (cons s res)))
           )
     )
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-psi-eqs from upto size)
   (for-each (lambda (a) (format #t "~a --> ~a ?\n" a
                            (find-matching-anum (catfun1 (psi (Anum->Afun a)))
                                                (-1+ (A014137 size)) ;; 2055
                            )
                         )
             )
             (map (lambda (n) (+ n from)) (iota0 (- upto from)))
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (output-gatomorphism-x-cycles infile outfile gatomorphism title)
   (let* ((cycl-representatives (read-lists-from infile))
          (upto_n (-1+ (length cycl-representatives)))
         )
        (call-with-output-file outfile
         (lambda (outport)
            (write (list title upto_n) outport)
            (newline outport)
            (let loop ((crs cycl-representatives))
               (cond ((pair? crs)
                        (write
                            (map (lambda (cr)
                                   (expand-cycle-representative cr gatomorphism)
                                 )
                                 (car crs)
                            )
                            outport
                        )
                        (newline outport)
                        (flush-output outport)
                        (loop (cdr crs))
                     )
               )
            )
         )
        )
   ) ;; let*
)


;; Use as:
;;
;; (output-sexp-sequence
;;    (map A014486->parenthesization (map A014486 (iota0 (-1+ (A014137 7)))))
;;    "C:/matikka/Nekomorphisms/a014486.sxp" "A014486 upto the size 7")

;; To be read in with gato-fps.scm's function:
;; (output-sexp-file-as-ps-file "a014486.sxp" "a014486.ps" 12)
;;

(define (output-sexp-sequence sexps filename comment-line)
   (call-with-output-file filename
     (lambda (outport)
        (write (list comment-line) outport)
        (newline outport)
        (let loop ((sexps sexps))
           (cond ((pair? sexps)
                    (write (car sexps) outport)
                    (newline outport)
                    (flush-output outport)
                    (loop (cdr sexps))
                 )
           )
        )
     )
   )
)

;; E.g. use as:
;; (for-each (lambda (Anum) (output-bfile (+ 129604 Anum) 0 2055 #t)) (iota0 8))

(define (output-bfile Anum offset upto-n create-edit-file?)
  (let* ((filename (format #f "b~a.txt" (string-tail (Anum->str Anum) 1)))
         (editfilename (format #f "~a.edit" (Anum->str Anum)))
         (Afun (Anum->Afun Anum))
         (editblurp (format #f "%H ~a A. Karttunen, <a href=\"http://www.research.att.com/~~njas/sequences/~a\">Table of n, a(n) for n = ~a..~a</a>"
                            (Anum->str Anum) filename offset upto-n
                    )
         )
        )
     (if create-edit-file?
         (call-with-output-file editfilename
            (lambda (out) (format out "~a\n" editblurp))
         )
     )
     (call-with-output-file filename
       (lambda (outport)
          (let loop ((i offset))
            (cond ((<= i upto-n)
;;                  (format #t "~a ~a\n" i (Afun i)) ;; Too much clutter on our screens!
;;                  (format outport "~a ~a\n" i (Afun i))
;; Replaced with the following four lines:
;; (As we want to print out very BIG numbers in some cases):
                    (write i outport)
                    (format outport " ")
                    (write (Afun i) outport)
                    (newline outport)
                    (flush-output outport)
                    (loop (+ 1 i))
                  )
            )
          )
       )
     )
     editblurp
  )
)



;; Each integer n appears first at position given by A014137
(define (A179751 n) (*A179751 (A014486->parenthesization (A014486 n))))

;; Each integer n appears first at position given by A014138. Cf. A080237
;; (and also A085197 ?)
(define (A179752 n) (*A179752 (A014486->parenthesization (A014486 n))))

(define (CompleteBinTree? s) ;; The binary tree s has its leaves all on the same level?
   (cond ((not (pair? s)) #t) ;; () (i.e., an empty tree) is a complete binary tree.
         ((equal? (car s) (cdr s)) (CompleteBinTree? (car s)))
         (else #f)
   )
)


(define (MaxBTDepthDownto1stCBT s)
   (cond ((CompleteBinTree? s) 0)
         (else (1+ (max (MaxBTDepthDownto1stCBT (car s))
                        (MaxBTDepthDownto1stCBT (cdr s))
                   )
               )
         )
   )
)



(define (MaxBTDepthVector n) ;; gives the columns of A073345
  (let ((resvec (make-vector (1+ n))))
    (vector-fill! resvec 0)
    (let loop ((src_set (map A014486->parenthesization (binseqs_of_size n)))
              )
       (cond ((not (pair? src_set)) resvec)
             (else (incr-num-vector! resvec (*A179751 (car src_set)))
                   (loop (cdr src_set))
             )
       )
    )
  )
)


(define (MaxBTDepthDownto1stCBTVector n) ;; gives the columns of A073346
  (let ((resvec (make-vector (1+ n))))
    (vector-fill! resvec 0)
    (let loop ((src_set (map A014486->parenthesization (binseqs_of_size n)))
              )
       (cond ((not (pair? src_set)) resvec)
             (else (incr-num-vector! resvec (MaxBTDepthDownto1stCBT (car src_set)))
                   (loop (cdr src_set))
             )
       )
    )
  )
)

;; Use as:
;; (define Tamari (map (lambda (s) (cons (foo s) (sort (map foo (find-Tamari-successors s)) <))) (map poo (iota0 196))))

(define (find-Tamari-successors s)
  (cond ((null? s) s)
        ((null? (cdr s)) (map list (find-Tamari-successors (car s))))
        (else ;; We can do robl here.
           (cons (robl! (copy-tree s))
                 (append
                         (map (lambda (t) (cons (car s) t))
                              (find-Tamari-successors (cdr s))
                         )
                         (map (lambda (t) (cons t (cdr s)))
                              (find-Tamari-successors (car s))
                         )
                 )
           )
        )
  )
)


;; See M. C. Er,
;; Classes of admissible permutations that are generatable by depth-first traversals of ordered trees,
;; The Computer Journal, volume 32 (1989), issue 1, pages 76--85.

;; Use zero-based permutations, instead of one-based used in Er's article:

;; Note that the resulting permutation can be decomposed to (length s) parts.

;; Use this function to inductively prove that in/post(*A057505(s)) = pre/in(s),
;; (based on the fact that *A057505 = KROF(*A057501).)

(define (pre/in-permutation s)
  (if (null? s) s
      (let ((car-side (pre/in-permutation (car s)))
            (cdr-side (pre/in-permutation (cdr s)))
           )
        (append (map 1+ car-side)
                (list 0)
                (map (lambda (n) (+ 1 n (length car-side))) cdr-side)
        )
      )
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;  More miscellaneous ideas.                                             ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This version is extremely dangerous with destructive gatomorphisms,
;; when prophylactic copy-tree is omitted!
;; (definec (nth-complete-binary-tree n)
;;  (cond ((zero? n) (list))
;;        (else ((lambda (x) (cons x x)) (nth-complete-binary-tree (-1+ n))))
;;  )
;; )

;; Let's play on the safe side, on the expense of memory:

(definec (nth-complete-binary-tree n)
  (cond ((zero? n) (list))
        (else ((lambda (x) (cons (copy-tree x) (copy-tree x)))
                (nth-complete-binary-tree (-1+ n))
              )
        )
  )
)


;; (map (lambda (x) (write x) (newline)) (map sym-antipodal-tree-of-nth-complete-bin-tree (iota0 5)))
;; These seem to be isomorphic to tpt's, constructed from
;; complete binary trees with an extra stem is added beneath
;; the root:
;; ()
;; (())
;; ((() ()))
;; (((() ()) (() ())))
;; ((((() ()) (() ())) ((() ()) (() ()))))
;; (((((() ()) (() ())) ((() ()) (() ()))) (((() ()) (() ())) ((() ()) (() ())))))


(define (sym-antipodal-tree-of-nth-complete-bin-tree n)
  (cond
    ((zero? n) (nth-complete-binary-tree n))
    (else
       (let loop ((t (copy-tree (nth-complete-binary-tree n)))
                  (i (/ (1+ (expt 3 (-1+ n))) 2))
                 )
              (cond ((zero? i) t)
                    (else (loop (*A057505! t) (-1+ i)))
              )
       )
    )
  )
)


(definec (nth-stunted-zigzag-tree n)
  (let ((marker (list)))
    (cond ((zero? n) (cons marker marker)) ;; (() . ())
          (else (next-stunted-zigzag-tree (nth-stunted-zigzag-tree (-1+ n))))
    )
  )
)

;; Probably there is a more elegant way to construct these,
;; without explicitly referring to the corresponding 1D-CA-bits:
;; (E.g. utilize the fractality (self-similarity) of the tree.)

(define (next-stunted-zigzag-tree t)
   (let* ((n (*A179751 t)) ;; Computed over and over again...
          (mask-bits (nth-row-of-A047999 n))
          (marker (list)) ;; Use 'A when debugging.
          (t2 (copy-tree t))
         )
     (let fork ((bt t2) (d 1) (p 0))
;;      (format #t "d=~A p=~A bt=~A~%" d p bt)
        (cond ((pair? bt)
                 (cond ((= d n) ;; Maximum depth?
;;                       (format #t "d=~A (bit-string-ref ~S ~A)=~A~%"
;;                            d mask-bits p (bit-string-ref mask-bits p)
;;                       )
                         (cond ((bit-string-ref mask-bits p)
                                   (set-car! bt (cons marker marker))
                               )
                         )
                         (cond ((bit-string-ref mask-bits (1+ p))
                                   (set-cdr! bt (cons marker marker))
                               )
                         )
                       )
                       (else
                         (fork (car bt) (1+ d) p)
                         (fork (cdr bt) (1+ d) (1+ p))
                       )
                 )
              )
        )
     )
     t2 ;; Return the modified copy.
   )
)

(definec (nth-primitive-stunted-zigzag-tree n) ;; Beware with destructive functions!
   (cond ((zero? n) (cons (list) (list)))
         (else (BinTree2ZigZagTree (nth-stunted-zigzag-tree (-1+ n))))
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New ideas, 2011-10-13:

(define (straight-tailed? s) (and (pair? s) (null? (car (last-pair s)))))

(define (char_A085223 n)
   (if (straight-tailed? (A014486->parenthesization (A014486 n))) 1 0)
)

;; (define A085223v5 (NONZERO-POS 0 0 char_A085223)) ;; Seems to work.

;; nth-unbranching-tree-of-size transferred to gatorank.scm
;; add-bud-for-the-nth-unbranching-tree-with-car-cdr-code! to gatorank.scm

(define (A071163v2 n)
   (if (zero? n)
       n
       (let ((size (- (binwidth n) 1)))
         (A080300 (parenthesization->A014486
                   (nth-unbranching-tree-of-size (A053645 (A059893 n)) size)
;;                 (nth-unbranching-tree-of-size (- (A059893 n) (expt 2 size)) size)
                  )
         )
       )
   )
)

;;
;; In this way:
;; 
;;  1
;;  .
;; 
;; 3  2
;;  \/
;; 
;;  6  4
;; 3 \/
;;  \/
;; 
;; 7  5
;;  \/ 2
;;   \/
;; 
;;
;;   12 8
;;  6 \/
;; 3 \/
;;  \/
;; 
;;
;; 15 11
;;   \/ 5
;;    \/ 2
;;     \/
;;


(define (bintree2indices t)
  (let ((is (list (list))))
    (let fork ((t t) (s 0) (i 1))
       (cond ((not (pair? t)) (attach! (+ i s) is))
             (else
                (fork (cdr t) s (* 2 i))
                (fork (car t) (+ s i) (* 2 i))
             )
       )
    )
    (cdr (reverse! is))
  )
)



(define (indices2bintree indices)
  (ZigzagTree2BinTree_if_possible
    (fold-right (lambda (x y)
                  (let ((size (- (binwidth x) 1)))
                           (LCSB (nth-unbranching-tree-of-size (A053645 x) size) y)
                  )
                )
                '()
                indices ;; Indices should be "capped" with extra msb-1.
    )
  )
)


;; (first-dislocated (map A080300 (map parenthesization->A014486 (map indices2bintree (map bintree2indices (map A014486->parenthesization (map A014486 (iota0 2059))))))))
;; --> ()

;; Demonstrating "Super-posited incrementing/decrementing in action":
;; (first-dislocated (map A069768 (map A080300 (map parenthesization->A014486 (map indices2bintree (map (lambda (luu) (map A153152 luu)) (map bintree2indices (map A014486->parenthesization (map A014486 (iota0 2059))))))))))
;; --> ()


;; What this should do? And how? Some of my (march) hare-brained ideas...
;; It's probably impossible to get the same indices as what bintree2indices gives with this kind of method.
;; (map length (map give-some-numbers-by-A069767-walking (map A014486->parenthesization (map A014486 (iota0 65)))))
;; (0 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 3 1 1 1 1 1 1 1 1 3 3 3 1 1 3 1 1 1 5 5 2 5 5 2 2 5 1 1 5 1 1 1 2 2 2 3 3 5 3 1 1 5 3 1 1 1 1)
;; Any significance?

(define (give-some-numbers-by-A069767-walking orgt)
  (let* (;; (size (A072643 (A080300 (parenthesization->A014486 t))))
         ;; (uplim (expt 2 size)) ;; An upper bound, maybe? (Should use the actual orbit size for this tree.)
         
        )
    (let loop ((t orgt) (i 0) (is (list)))
         (cond ;; ((= i uplim) (reverse! is))
               ((and (not (zero? i)) (equal? t orgt)) (reverse! is))
               ((straight-tailed? t)
                  (loop (*A069767 t) (1+ i) (cons i is))
               )
               (else
                  (loop (*A069767 t) (1+ i) is)
               )
         )
    )
  )
)


;; Contract by replacing any subtree which is a complete binary tree with ().
;; Should hold: (*A179751 (ContractBinaryTree bt)) = (MaxBTDepthDownto1stCBT s)
;; (same-intfuns? (compose-funs MaxBTDepthDownto1stCBT A014486->parenthesization A014486) (compose-funs *A179751 ContractBinaryTree A014486->parenthesization A014486) 23714) --> #t

(define (ContractBinaryTree bt)
  (cond ((CompleteBinTree? bt) (list))
        (else (cons (ContractBinaryTree (car bt))
                    (ContractBinaryTree (cdr bt))
              )
        )
  )
)




(define (bintree2indices-by-walking-org t size)
  (let ((uplim (expt 2 size)))
    (let loop ((t t) (i 0) (is (list)))
         (cond ((= i uplim) (reverse! is))
               ((straight-tailed? t)
                  (loop (*A069767 t) (1+ i) (cons i is))
               )
               (else
                  (loop (*A069767 t) (1+ i) is)
               )
         )
    )
  )
)

(define (indices2bintree-org indices-zb size)
    (fold-right (lambda (x y) (LCSB (nth-unbranching-tree-of-size x size) y))
                '()
                indices-zb ;; Indices should be zero-based.
    )
)


(define (bintree2indices-org t)
  (let ((is (list (list))))
    (let fork ((t t) (d 1) (i 0))
       (cond ((not (pair? t)) (attach! i is))
             (else
                (fork (cdr t) (* 2 d) i)
                (fork (car t) (* 2 d) (+ i d))
             )
       )
    )
    (cdr (reverse! is))
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Inverse function for A014486:
(define (A080300 n) (cond ((zero? (A080116 n)) 0) (else (CatalanRankGlobal n))))
(define (A080301 n) (cond ((zero? (A080116 n)) -1) (else (CatalanRankLocal n))))

;; Reserved:
;; A080263 = same in decimal, 12,...
;; A080264 = bin-form of the tree: 1100,11100100,...
;; A080265 = pos in A014486 (CatalanGlobalRank)
;; A080266 = pos in each Cat(n) subset (CatalanRank)

(define A080263 (compose-funs parenthesization->A014486 nth-stunted-zigzag-tree))
(define A080264 (compose-funs A007088 A080263))
(define A080265 (compose-funs A080300 A080263))
(define A080266 (compose-funs A080301 A080263))

(define A080268 (compose-funs parenthesization->A014486 *A057118 nth-stunted-zigzag-tree))
(define A080269 (compose-funs A007088 A080268))
(define A080270 (compose-funs A080300 A080268))
(define A080271 (compose-funs A080301 A080268))

(definec (A080272 n) (cycle-size-under (nth-stunted-zigzag-tree n) *A057505!))
;; --> (1 3 3 27 54 54 18 1134 1134 1134 1134 1782 1782 594 594 30618 30618)
(definec (A080273 n) (cycle-size-under (nth-stunted-zigzag-tree n) *A057118))
;; --> 1,3,16,1441,41888,3376173
(definec (A080274 n) (cycle-size-under (nth-complete-binary-tree n) *A057118))
;; --> 1,1,3,202,1582608


;; If the following is true, then how do the corresponding
;; almost-antipodal symmetric general trees look like?
;; Are they beautiful?
;; (definec (A000244-shifted-right-prep-with-1? n) (cycle-size-under (nth-complete-binary-tree n) *A057505!))
;; --> (1 1 3 9 27 81 243 729 2187 6561 19683)

;; A080292 --- 80313.

(definec (A080292 n) (cycle-size-under (nth-primitive-stunted-zigzag-tree n) *A057505!))
;; -->

(define A080293 (compose-funs parenthesization->A014486 nth-primitive-stunted-zigzag-tree))
(define A080293v2 (compose-funs parenthesization->A014486 BranchReducedZigzagTree nth-stunted-zigzag-tree (lambda (n) (* 2 n))))
(define A080294 (compose-funs A007088 A080293))
(define A080295 (compose-funs A080300 A080293))
(define A080296 (compose-funs A080301 A080293))
(definec (A080297 n) (cycle-size-under (nth-primitive-stunted-zigzag-tree n) *A057118))
;; -->


(definec (A080311 n) (cycle-size-under (A014486->parenthesization (A014486 n)) *A057118))
(definec (A080967 n) (cycle-size-under (A014486->parenthesization (A014486 n)) *A057505!))


(definec (A080298 n)
  (cond ((zero? n)
           (A080300
              (parenthesization->A014486
                  (BinTree2ZigZagTree (A014486->parenthesization (A014486 n)))
              )
           )
        )
        (else ((catfun1 BinTree2ZigzagTree) n))
  )
)

(define A080298v2 (compose-funs A080300 A080310 A014486))

(define A080299   (compose-funs A014486 A080298))
(define A080299v2 (compose-funs A080310 A014486))

(define (A080302 n) (/ (A080292 (1+ n)) 3))


;;
;; Moved the range A080303 - A080311 to range A080973 - A080981.
;; A-numbers A080311 & A080972
;; are still free.
;; 
;; sed -e 's/80303/80973/g' -e 's/80304/80974/g' -e 's/80305/80975/g'
;;     -e 's/80306/80976/g' -e 's/80307/80977/g' -e 's/80308/80978/g'
;;     -e 's/80309/80979/g' -e 's/80310/80980/g' -e 's/80311/80981/g'
;;
;;

(definec (nth-antipodal-of-primitive-stunted-zigzag-tree n) (apply-n-times *A057505! (/ (1+ (A080292 n)) 2) (copy-tree (nth-primitive-stunted-zigzag-tree n))))

(define A080973 (compose-funs parenthesization->A014486 nth-antipodal-of-primitive-stunted-zigzag-tree))
(define A080974 (compose-funs A007088 A080973))
(define A080975 (compose-funs A080300 A080973))
;; (1 7 515 73211 2249220471 431283926958 18838905762720934 896134321804401371660)
;; Should be invariant under A057164!
(define A080976 (compose-funs A080301 A080973))

(define (A080977 n) (/ (A080272 (* 2 n)) (A080292 n)))
;; Gives: 1,1,6,2,14,14,22,22,18,18,46,46,62,62,26,26

;; (define (A080977vvv n) (/ (A080272 (1+ (* 2 n))) (A080292 (1+ n))))
;; Gives: 1,3,6,14,14,22,22,18,18,46,46 which is not exactly same.

(define A080978v2 (compose-funs count-pars nth-antipodal-of-primitive-stunted-zigzag-tree))
;; (1 3 7 11 19 23 31 39 55 59 67)
;; is A006046 0,1,3,5,9,11,15,19,27,29,33,37,.... * 2 + 1

(define A080979direct (catfun1 BranchReducedZigzagTree))

(definec (A080979 n)
  (cond ((zero? n)
           (A080300
              (parenthesization->A014486
                  (BranchReducedZigzagTree (A014486->parenthesization (A014486 n)))
              )
           )
        )
        (else (A080979direct n))
  )
)

;; (define seqA080980 (uniq (sort (map A080979 (iota0 6918)) <)))
;; (definec (A080980v2 n) (list-ref seqA080980 n))

(definec (A080980 n)
   (cond
      ((zero? n) 0) ;; The first primitive tree is A014486(0) = 0.
      (else
         (let loop ((i (1+ (A080980 (-1+ n)))))
             (cond ((= (A080979 i) i) i)
                   (else (loop (1+ i)))
             )
         )
      )
   )
)

(define  A080981 (compose-funs A014486 A080980))

(definec (A080970 n)
   (cond
      ((zero? n) 4) ;; The first non-primitive tree is A014486(4) = 42. = 101010
      (else
         (let loop ((i (1+ (A080970 (-1+ n)))))
             (cond ((not (= (A080979 i) i)) i)
                   (else (loop (1+ i)))
             )
         )
      )
   )
)


(define  A080971 (compose-funs A014486 A080970))


(definec (A080968v2 n)
   (cycle-size-under (A014486->parenthesization (A014486 (A080980 n))) *A057505!)
)

(define A080968 (compose-funs A080967 A080980)) ;; cycle-size for primitive trees

(define A080969 (compose-funs A080967 A080970)) ;; cycle-size for non-primitive trees

(define (A080972 n)
   (let ((nonprim (A080970 n)))
      (/ (A080967 nonprim) ;; I.e. (A080969 n)
         (A080967 (A080979 nonprim))
      )
   )
)


(definec (A080312 n) (A057118 (A080298 n)))
(define A080313 (compose-funs A014486 A080312)) ;; dec. bf-readings of zigzag-bin-trees
(define A080314 (compose-funs A007088 A080313)) ;; bin. of the same.
(definec (A080315 n) (/ (A080313 n) 2)) ;; Discard one zero.
(define (A080316 n) (- (A080315 n) (expt 2 (floor-log-2 (A080315 n)))))
(define A080317 (compose-funs A007088 A080316))

(define A080318 (compose-funs parenthesization->A014486 *A057118 nth-primitive-stunted-zigzag-tree))
(define A080319 (compose-funs A007088 A080318))
(define A080320 (compose-funs A080300 A080318))
(define A080321 (compose-funs A080301 A080318))

;; Call as:
;; (output-check-html2 "C:/matikka/nekomorphisms/a80263-.htm" check-these-too 100 9)
;; Function output-check-html is located in gatochek.scm
;;

(define check-these-too
 (list
       (list 100 0 80311 A080311)
       (list 100 0 80967 A080967)
       (list 100 0 80968 A080968)
       (list 100 0 80969 A080969)
       (list 100 0 80970 A080970)
       (list 100 0 80971 A080971)
       (list 100 0 80972 A080972)
       (list 100 0 80978 A080978)


       (list 15 0 80263 A080263)
       (list 15 0 80264 A080264)
       (list 15 0 80265 A080265)
       (list 15 0 80266 A080266)

       (list 15 0 80268 A080268)
       (list 15 0 80269 A080269)
       (list  5 0 80270 A080270) ;; () (list A057118 A080265)
       (list 15 0 80271 A080271)

       (list 24 0 80272 A080272)
       (list  3 0 80273 A080273)
       (list  3 0 80274 A080274)

       (list 15 0 80292 A080292)

       (list 15 0 80293 A080293)
       (list 15 0 80294 A080294)
       (list  7 0 80295 A080295) ;; () (list A080298 A080265)
       (list 15 0 80296 A080296)

       (list  3 0 80297 A080297)

       (list 120 0 80298 A080298 () (list A080300 A080310 A014486))

       (list 60  0 80299 A080299 () (list A080310 A014486))

       (list 60 0 80300 A080300)
       (list 60 0 80301 A080301)

       (list 14 0 80302 A080302)

       (list 120 0 80303 A080303)
       (list 120 0 80310 A080310)

       (list 15 0 80973 A080973)
       (list 15 0 80974 A080974)
       (list 15 0 80975 A080975)
       (list 15 0 80976 A080976)

       (list 15 0 80977 A080977)
    
       (list 100 0 80979 A080979)
       (list 100 0 80980 A080980)
       (list 100 0 80981 A080981)

       (list 50 0 80312 A080312)
       (list 50 0 80313 A080313)
       (list 50 0 80314 A080314)
       (list 50 0 80315 A080315)
       (list 50 0 80316 A080316)
       (list 50 0 80317 A080317)

       (list 15 0 80318 A080318)
       (list 15 0 80319 A080319)
       (list 15 0 80320 A080320) ;; () (list A057118 A080295)
       (list 15 0 80321 A080321)

 )
)

(define (A081292 n) (A014486 (A081291 n)))

;; (output-check-html2 "C:/matikka/nekomorphisms/a81288-92.htm" check-these-as-well 100 20)

(define check-these-as-well
 (list
       (list 100 0 81288 A081288)
       (list 100 0 81289 A081289)
       (list 100 0 81290 A081290)
       (list 100 0 81291 A081291)
       (list 100 0 81292 A081292)
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;; Define the corresponding induced integer sequences (which most are       ;;
;; permutations of the natural non-negative integers) from the              ;;
;; gatomorphisms given in the file:                                         ;;
;;                                                                          ;;
;;  http://www.iki.fi/~kartturi/matikka/Nekomorphisms/gatomorf.scm          ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define A057117   (catfun1restricted_to_bt_subset *A072088))
(define A057118   (catfun1 *A057118)) ;; Was: df->bf
;; (define A057118v2 (catfun1restricted_to_bt_subset *A072089))

;; Something wrong with the indices of this version:
(define A070041v2   (compose-funlist (list 1+ (catfun0 *A057118)))) ;; inverse of A038776.
(define A070041 (compose-funs 1+ A082853 A057118 A081291 -1+))
(define A038776 (compose-funs 1+ A082853 A057117 A081291 -1+))

(define A072088 (catfun1 *A072088)) ;; Was: gt-bf->df
(define A072089 (catfun1 *A072089)) ;; Was: gt-df->bf

(define A072619 (catfun0 *A072088)) ;; Yes, self-embeddable like its bt-variant.
(define A072619v2 (compose-funs A082853 A072088 A081291))

(define A072620 (catfun0 *A072089))
(define A072620v2 (compose-funs A082853 A072089 A081291))

(define A072621 (compose-funs 1+ A072619))
(define A072622 (compose-funs 1+ A072620))


(define A057161   (catfun1 *A057161)) ;; RotateTriangularization
(define A057161!  (catfun1 *A057161!)) ;; RotateTriangularization!
(define A057161v2 (catfun1 *A057161v2)) ;; Was: RotateHandshakesD1
(define A057162   (catfun1 *A057162)) ;; RotateTriangularizationInv
(define A057162!  (catfun1 *A057162!)) ;; RotateTriangularizationInv!
(define A057163   (catfun1 *A057163))
(define A057163!  (catfun1 *A057163!))
(define A057164   (catfun1 *A057164)) ;; Was DeepRev
(define A057164!  (catfun1 *A057164!)) ;; Was DeepRev1!
(define A057164v2 (compose-funs A080300 A036044 A014486)) ;; Fast versions
(define A057164v3 (compose-funs A080300 A056539 A014486)) ;; working on binary exp.
(define A057501   (catfun1 *A057501)) ;; Was RotateHandshakes
(define A057501!  (catfun1 *A057501!))
(definec (A057501v2 n) (if (zero? n) n (A085201bi (A072771 n) (A057548 (A072772 n))))) ;;; Yes!
(definec (A057161rec n) (if (zero? n) n (A085201bi (A057161rec (A072771 n)) (A057548 (A072772 n)))))
(definec (A057503rec n) (if (zero? n) n (A085201bi (A072771 n) (A057548 (A057503rec (A072772 n))))))
(definec (A057505rec n) (if (zero? n) n (A085201bi (A057505rec (A072771 n)) (A057548 (A057505rec (A072772 n))))))


(define A057502   (catfun1 (lambda (p) (*A057502! (copy-tree p)))))
(define A057502!  (catfun1 *A057502!))
(define A057503   (catfun1 *A057503))
(define A057503!  (catfun1 *A057503!))
(define A057504   (catfun1 *A057504!))
(define A057504!  (catfun1 *A057504!))
(define A057505   (catfun1 DeepRotateTriangularization))
(define A057505v1 (catfun1 DonagheysM))
(define A057505!  (catfun1 *A057505!))
(define A057506   (catfun1 DeepRotateTriangularizationInv))
(define A057506!  (catfun1 *A057506!))
(define A057508   (catfun1 reverse))
(define A057508!  (catfun1 *A057508!)) ;; Was Rev1!
(define A057508!! (catfun1 Rev2!))
(define A057509   (catfun1 *A057509)) ;; Was Rol
(define A057509!  (catfun1 *A057509!)) ;; Rol!
(define A057510!  (catfun1 *A057510!)) ;; Ror!
(define A057510   (catfun1 (lambda (p) (*A057510! (copy-tree p)))))
(define A057511   (catfun1 A057511)) ;; Was DeepRol
(define A057512   (catfun1 (lambda (p) (*A057512! (copy-tree p)))))

;; injections, not bijections
(define A057123old (catfun1 BinTree2Tree))
(define A057123 (catfun1 *A057123))

;; Reserve permanent A-numbers later:
;; Also "inverse" for this injection:
(define A900001 (MATCHING-POS 0 0
                   (lambda (n) (not (find-matching-item (p->Lw (A014486->parenthesization (A014486 n)))
                                                        (lambda (d) (> d 2))
                                    )
                               )
                   )
                )
)

;; (map A900001 (iota0 37))
;; ;; (0 1 2 3 5 6 7 8 12 13 15 16 18 19 20 21 22 32 34 35 36 40 41 43 47 49 50 52 53 55 56 57 59 60 61 62 63 64)
;; (map A085159 (map A900001 (iota0 37)))
;; ;; (0 1 2 3 6 7 5 8 16 19 20 12 13 21 15 18 22 47 53 56 60 57 61 34 35 32 36 62 40 41 49 43 50 63 52 55 59 64)
;; 
;; (map A085160 (map A900001 (iota0 37)))
;; (0 1 2 3 7 5 6 8 16 18 20 12 21 13 15 19 22 49 43 47 50 53 55 57 32 56 59 61 34 62 35 40 63 36 41 52 60 64)

;; ;; And the inverse:
;; (map (lambda (i) (pos-of-first-matching (map A900001 (iota0 37)) (lambda (el) (eq? el (A085159 (A900001 i)))))) (iota0 37))
;; ;; (0 1 2 3 5 6 4 7 11 13 14 8 9 15 10 12 16 24 28 30 33 31 34 18 19 17 20 35 21 22 25 23 26 36 27 29 32 37)
;; 

(define A900002
          (MATCHING-POS 0 0
              (lambda (n) (and (not (find-matching-item (p->Lw (A014486->parenthesization (A014486 n)))
                                                        (lambda (d) (> d 2))
                                    )
                               )
                               (not (= (A069787 n) ((LeBorgne-catsigperm 12) n)))
                          )
              )
          )
)


(define A057547v2 (compose-funs A079946 A014486))
(define A057548 (compose-funs A080300 A079946 A014486))
;; (define A057548v3 (compose-funs A069770 A072795))
(define A057548v2 (catfun1 list))

(define A057518 (catfun1 AllTrees2DoubleTrunked))
;; --> (0 2 5 6 12 13 15 16 19 31 32 34 35 36 40 41 43 44 47 52 53 56 60 87 88 90 91 92 96 97 99 100 101 103 104 105 106 115 116 118 119 120 124 125 127 128 131 136 137 140 144 152 153 155 156 159 164 165 168 172 178 179 182 186 191 261 262 264 265 266 270 271 273 274 275 277 278 279 280 289 290 292 293 294 298 299 301 302 303 305 306 307 308 312 313 315 316 317 319 320 321 322 324 325 326 327 328 351 352 354 355 356 360 361 363 364 365 367 368 369 370)
;; (map A014486 (map Argh (cons 0 (iota 30)))) gives A057517: (Change to offset=0)
; --> (0 10 44 50 180 184 204 210 226 724 728 740 744 752 820 824 844 850 866 908 914 930 962 2900 2904 2916 2920 2928 2964 2968 2980)

(define A072771 (catfun1 car)) ;; Projection functions for the tabular
(define A072772 (catfun1 cdr)) ;; bijections A072764 & A072766.

;; New ones:



(define A069767! (catfun1 *A069767!)) ;; Inverses of each ...
(define A069768! (catfun1 *A069768!)) ;;   ... other
(define A069767  (catfun1 *A069767))
(define A069768  (catfun1 *A069768))
(define A069769! (catfun1 *A069769!)) ;; Was: Rev1CarSide!, Self-inverse
(define A069769  (catfun1 *A069769)) ;; Was: CarReverse
(define A069770  (catfun1 *A069770)) ;; Was: SwapBinTree
(define A069770! (catfun1 *A069770!)) ;; Self-inverse
(define A069771  (catfun1 *A069771)) ;; Self-inverse. Was: RotateHandshakes180
(define A069772  (catfun1 *A069772)) ;; Self-inverse. Was: xReflectHandshakes
(define A069773! (catfun1 *A069773!))
(define A069774! (catfun1 *A069774!))
(define A069775! (catfun1 *A069775!)) ;; Was: RolCarSide!
(define A069776! (catfun1 *A069776!)) ;; Was: RorCarSide!
(define A069787! (catfun1 *A069787!)) ;; Was: DeepRev1CarSide!

(define A069888! (catfun1 *A069888!)) ;; Was DeepReverse_et_RotateHandshakes!
(define A069889! (catfun1 *A069889!)) ;; Was RotateHandshakes_et_DeepReverse!

;; And the non-destructive variants:
(define A069773  (catfun1 (lambda (p) (*A069773! (copy-tree p)))))
(define A069774  (catfun1 (lambda (p) (*A069774! (copy-tree p)))))
(define A069775  (catfun1 (lambda (p) (*A069775! (copy-tree p)))))
(define A069776  (catfun1 (lambda (p) (*A069776! (copy-tree p)))))
(define A069787  (catfun1 (lambda (p) (*A069787! (copy-tree p)))))
(define A069888  (catfun1 *A069888))
(define A069889  (catfun1 *A069889))

(define A071152 (catfun2 (compose-funs baselist-as-decimal p->Lw BinTree2Tree)))
(define A063171 (compose-funs A007088 A014486)) ;; Wrong: (compose-funs halve A071152 1+))
(define A071153 (catfun2 (compose-funs baselist-as-decimal p->Lw)))
(define (A079436 n) (* 10 (A071153 n)))
(define A071160 (compose-funs baselist-as-decimal binexp->siteswap-list))
(define A071161 (compose-funs A071160 A054429))
;; We have now a direct version in intfuns1.scm (intfun_b.scm):
(define A071162v2 (compose-funs parenthesization->A014486 Lw->parenthesization binexp->siteswap-list))
(define A071163 (compose-funs A080300 A071162))

;; A209635-A209644 are now reserved for your use.

(define A209643 (compose-funs A080300 A209641))
(define A209644 (compose-funs baselist-as-decimal p->Lw A014486->parenthesization A209641))

;; (map A127301 (map A071163 (iota0 32)))
;; 1,2,4,3,8,6,7,5,16,12,14,10,19,13,17,11,32,24,28,20,38,26,34,22,53,37,43,29,67,41,59,31,64,

;; Seems to be reflected by A054429:
;; (map A127301 (map A057163 (map A071163 (iota0 32))))
;; 1,2,3,4,5,7,6,8,11,17,13,19,10,14,12,16,31,59,41,67,29,43,37,53,22,34,26,38,20,28,24,32,127,
;; = (map A127301 (map A071163 (map A054429 (iota0 32))))
;; Can we extend this to all non-rooted general trees? (To create another (?) perm. of nat.numbers via Matula-codes?)

;; (map A127301 (map A057505! (map A071163 (iota0 32))))
;; is same, as A057505 = A057164 o A057163, and A057164 doesn't affect the Matula-signature:
;; 1,2,3,4,5,7,6,8,11,17,13,19,10,14,12,16,31,59,41,67,29,43,37,53,22,34,26,38,20,28,24,32,127,

;; Submit the same sequence as sorted.
;; ("Natural numbers which do not contain any odd squares as divisors, nor which contain any prime A000040(i),
;;   where i would contain odd squares or any other such forbidden primes as divisors, recursively.")
;; Comment: these are Matula-numbers for the rooted trees where no vertices with more than one non-leaf branch
;; occur.
;; Create also a super-sequence: "Natural numbers which do not contain any odd squares as divisors, apart from 1".
;; if doesn't exist already. (or: "Natural numbers for which A008683(A000263(n)) <> 0"
;; and its complement: "Natural numbers for which A008683(A000263(n)) = 0" (contains an odd non-unit square)
;;
;; and the two induced permutations.


(define A089850 (catfun1 *A089850!))
(define A089851 (catfun1 *A089851!))
(define A089852 (catfun1 *A089852!))
(define A089853 (catfun1 *A089853!))
(define A089854 (catfun1 *A089854!))
(define A089855 (catfun1 *A089855!))
(define A089856 (catfun1 *A089856!))
(define A089857 (catfun1 *A089857!))
(define A089858 (catfun1 *A089858!))
(define A089859 (catfun1 *A089859!))
(define A089860 (catfun1 *A089860!))
(define A089861 (catfun1 *A089861!))
(define A089862 (catfun1 *A089862!))
(define A089863 (catfun1 *A089863!))
(define A089864 (catfun1 *A089864!))
(define A089865 (catfun1 *A089865!))
(define A089866 (catfun1 *A089866!))
(define A089867 (catfun1 *A089867!))
(define A089868 (catfun1 *A089868!))
(define A089869 (catfun1 *A089869!))
(define A089870 (catfun1 *A089870!))

(define (A089849 n) ;; Fix-counts for A069772
    (if (even? n) (A000984 (/ n 2)) (A000108 (/ (- n 1) 2)))
)

(define (A089880 n) (/ (+ (A000108 n) (A089849 n)) 2)) ;; Cycle-counts for A069772


(define (A089848 n) ;; Fix-counts for A089851/A089853.
     (cond ((zero? n) 1)
           (else (+ (A000108 (-1+ n))
                    (if (= 2 (modulo n 3))
                        (A000108 (/ (- n 2) 3))
                        0
                    )
                 )
           )
     )
)

;; Orbit-counts for A089851/A089853.
(define (A089847 n) (/ (+ (A000108 n) (* 2 (A089848 n))) 3))

;; Orbit-counts for A089859/A089863.
(define (A089407 n)
   (if (< n 2) 1
       (* (/ 1 4) (+ (A000108 n)
                     (A089408 n)
                     (if (= 3 (modulo n 4)) (* 2 (A000108 (/ (- n 3) 4))) 0)
                  )
       )
   )
)

;; Fix-point counts for A089864. (i.e. A089859/A089863 squared)
;; (map A089408 (iota0 22))
;; --> (1 1 2 1 2 2 4 5 10 14 28 42 84 132 264 429 858 1430 2860 4862 9724 16796 33592)

(define (A089408 n)
   (cond ((zero? n) 1)
         ((even? n) (* 2 (A000108 (-1+ (/ n 2)))))
         (else (A000108 (/ (-1+ n) 2)))
   )
)

;; Orbit-counts for A089864.
(define (A089402 n) (* (/ 1 2) (+ (A000108 n) (A089408 n))))


;; %S A089407 1,1,1,2,4,11,34,109,360,1219,4206,14708,52024,185758,668676,2423821,8839632,32411555,119410390,441817020,1641032536
;; %N A089407 Number of cycles in range [A014137(n-1)..A014138(n-1)] of permutation A089859/A089863.

;; A089848 := n -> Cat(n-1)+`if`((2 = (n mod 3)),Cat((n-2)/3),0);
;; 
;; > [seq(A089848(n),n=1..25)];
;; 
;; [1, 2, 2, 5, 15, 42, 132, 431, 1430, 4862, 16801, 58786, 208012,
;; 
;;     742914, 2674440, 9694845, 35357712, 129644790, 477638700,
;; 
;;     1767263322, 6564120420, 24466267020, 91482564069, 343059613650,
;; 
;;     1289904147324]
;; 
;; > A089847 := n -> (1/3)*(Cat(n)+2*A089848(n));
;; 
;;               A089847 := n -> 1/3 Cat(n) + 2/3 A089848(n)
;; 
;; > [seq(A089847(n),n=1..25)];
;; 
;; [1, 2, 3, 8, 24, 72, 231, 764, 2574, 8840, 30796, 108528, 386308,
;; 
;;     1386756, 5014575, 18249120, 66786738, 245642760, 907513530,
;; 
;;     3366215688, 12531502620, 46805032560, 175341580596, 658674458208,
;; 
;;     2480584898700]
;;

(define (A089833 n) (* (A000108 n) (-1+ (A000142 (1+ n))))) ;; The first column of A089831.

(define (A089835 n) (* (A000108 n) (A000108 n) (A000142 (1+ n))))
;;     A089836 = INVERT(A089835) = INVERT([seq(Cat(n)*Cat(n)*(n+1)!,n=1..9)]);
;;     [2,28,704,26800,1404416,94890112,7887853568,779773444864,89407927009280,...]


;; Now defined directly in gatorank.scm
;; (define A071673 (compose-funs halve length p->Lw BinTree2Tree (lambda (n) (unrank-bintree n A002262 A025581)) 1+))
;; Or: (define A071673 (compose-funs halve length p->Lw BinTree2Tree (lambda (n) (unrank-bintree n A025581 A002262)) 1+))
;; (define A071673 (size-fun-with-arithrank-scheme A025581 A002262))
;; or: (define A071673 (size-fun-with-arithrank-scheme A002262 A025581))


;; (define A057515v2 (catfun2 length))

(define (A057514 n) (halve (A000120 (A003188 (A014486 n)))))
(define (A057514v2 n) (A014081 (A014486)))
(define (A057515 n) (length (A014486->parenthesization (A014486 n))))

(define (A127284 n) (if (zero? n) 0 (- (A057514 n) 1))) ;; Number of valleys

(define (A153239 n)
  (let ((s (A014486->parenthesization (A014486 n))))
     (if (null? s) 0 (- (count-pars (cdr s)) (count-pars (car s))))
  )
)

(define (A153240 n) (gentree-deep-balance (A014486->parenthesization (A014486 n))))

(define (gentree-deep-balance l)
  (let ((r (reverse l))) ;; Same list reversed, scanned from right.
     (let loop ((i 0) (j (- (length l) 1)) (l l) (r r) (z 0))
            (cond ((= i j) (+ z (gentree-deep-balance (car l))))
                  ((> i j) z) ;; Even number of top-lev elements? Return z
                  (else (loop (+ i 1) (- j 1) (cdr l) (cdr r)
                              (+ z (- (count-pars (car r))
                                      (count-pars (car l))
                                   )
                              )
                        )
                  )
           )
     )
  )
)




;; Differs from previous for the first time at n=268, where:
;; A153241(268) = 1, while A153240(268)=2
;; (A014486->parenthesization (A014486 268)) = (() (() (())) (()))

;; Both A153240(A061856(n)) and A153241(A061856(n)) = 0 for all n.

(define (A153241 n) (gentree-balance (A014486->parenthesization (A014486 n))))

;; Like previous, but examine the center-element (from the odd-length lists)
;; only if the other elements were balanced (or the length of list is 1):

(define (gentree-balance l)
  (let ((r (reverse l))) ;; Same list reversed, scanned from right.
     (let loop ((i 0) (j (- (length l) 1)) (l l) (r r) (z 0))
            (cond ((= i j) (+ z (if (zero? z) (gentree-balance (car l)) 0)))
                  ((> i j) z) ;; Even number of top-lev elements? Return z
                  (else (loop (+ i 1) (- j 1) (cdr l) (cdr r)
                              (+ z (- (count-pars (car r))
                                      (count-pars (car l))
                                   )
                              )
                        )
                  )
           )
     )
  )
)

;; Complement of A057548:
(define A153242 (MATCHING-POS 0 0 (lambda (i) (not (= 1 (A057515 i))))))

(define A153243 (MATCHING-POS 0 0 (lambda (i) (zero? (A153239 i)))))

(define A061856 (MATCHING-POS 0 0 (lambda (i) (= i (A057164 i)))))

;; Added Jun 7 2014:

(define (A243491 n) (A127301 (A069787 n)))
(define (A243492 n) (- (A243491 n) (A127301 n)))

(define A243490 (FIXED-POINTS 0 0 A069787)) ;; Convention is to start them from zero.
(define A243490v2 (ZERO-POS 0 0 A243492))
(define A243489 (MATCHING-POS 1 0 (lambda (k) (not (= k (A069787 k)))))) ;; This starts with one.

(define (A243493 n) (A127301 (A243490 n)))
(define (A243493v2 n) (A243491 (A243490 n)))

(define A243495 (FIXED-POINTS 0 0 A057512))
(define (A243496 n) (A127301 (A243495 n)))

;; A153239 --- A153241.

(define (A126302 n) (A125989 (A014486 n))) ;; Sum of peak heights.

(define (A126303 n) (*A126303 (A014486->parenthesization (A014486 n)))) ;; Anew_oddlevnodes
(define (A126304 n) (*A126304 (A014486->parenthesization (A014486 n)))) ;; Anew_evenlevnodes
(define (A126305 n) (1+ (A126304 n))) ;; Number of even-level nodes, including the root.
;; A072643(n) = A126303(n) + A126305(n)


(define (A126306 n) (A000120 (A048735 (A014486 n)))) ;; Double rises.
(define (A126306v2 n) (A014081 (A014486 n)))

(define (A126307 n) (A090996 (A014486 n))) ;; Anew_len_rising_slope
(define (A126307v2 n) (A099563 (A071156 n)))

(define (A126307v3 n) (ind-of-first-non-rising (A071159raw n)))

(define (A126309 n) (A080300 (A126308 (A014486 n)))) ;; Anewcompressed


(define (A080067 n) (if (zero? n) 1 (A057163 (A057548 (A057164 n)))))
(define (A080067v2 n) (A072795 (A057506 n)))

(definec (A080068 n) (if (zero? n) 0 (A080067 (A080068 (-1+ n)))))
(definec (A080069 n) (A014486 (A080068 n)))
;; (define A080069 (compose-funs A014486 A080068))
(define A080070 (compose-funs A007088 A080069))
(define A080070v2 (compose-funs A063171 A080068))
(define A080071v1 (compose-funs length A014486->parenthesization A080069))
(define A080071 (compose-funs A057515 A080068))


;; Here are the 30 A-numbers you requested: A179751 --- A179780.
;; (Remember A179753, in intfun_b.scm !
;; Here are the 30 A-numbers you requested: A179827 --- A179856.

(definec (A179754 n) (if (= 1 n) 6 (A122237 (A179754 (-1+ n)))))
(definec (A179755 n) (A014486 (A179754 n)))
;; (define A179755 (compose-funs A014486 A179754))


(definec (A179756 n) (if (= 1 n) 8 (A122237 (A179756 (-1+ n)))))
(definec (A179757 n) (A014486 (A179756 n)))
;; (define A179757 (compose-funs A014486 A179756))

;; Prove that A179755(n+1) = A004758(A179757(n)).
;; equally, that A179754(n+1) = A127307(A179756(n)).


;; A057515 applied to A179764-A179766 seems to
;; give all-1 seqs after the initial term.

;; Highest binary tree sequences:
(define (A179840 n) (A179751 (A080068 n)))
(define (A179841 n) (A179751 (A122241 n)))
(define (A179842 n) (A179751 (A122244 n)))
(define (A179843 n) (A179751 (A179756 n)))

;; Highest general tree sequences:
;; Also the local maximums in A179759, A179764-A179766:
(define (A179844 n) (A179752 (A080068 n)))
(define (A179845 n) (A179752 (A122241 n)))
(define (A179846 n) (A179752 (A122244 n)))
(define (A179847 n) (A179752 (A179756 n)))



;; Bit-sequences. (4) A179758, A179761, A179762, A179763
;; Their partial sums. (4)
;; Their partial sums^2 (4)

;; Bits of terms of A080069 (A080070) from the most significant to the least siginificant bits, concatenated to one bit sequence:
;;
(definec (A179758 n) (modulo (floor->exact (/ (A080069 (A000194 n)) (expt 2 (A179753 n)))) 2))

;; Number of zeros in range ..,.. is given by A080071()+-...
;; Maximum in range ... is given by A179752(A080069(n..))
(define A179759 (PARTIALSUMS 1 1 (lambda (n) (- (A033999 (A179758 n))))))
(define A179759v2 (PARTIALSUMS 1 1 (compose-funs - A033999 A179758)))


(definec (A179761 n) (modulo (floor->exact (/ (A122242 (- (A000194 (+ n 6)) 2)) (expt 2 (A179753 (+ n 6))))) 2))

(definec (A179762 n) (modulo (floor->exact (/ (A122245 (- (A000194 (+ n 6)) 2)) (expt 2 (A179753 (+ n 6))))) 2))

(definec (A179763 n) (modulo (floor->exact (/ (A179757 (- (A000194 (+ n 6)) 2)) (expt 2 (A179753 (+ n 6))))) 2))

(define (xorfuns a b) (lambda (n) (modulo (+ (a n) (b n)) 2)))

(define A179827 (xorfuns A179761 A179762))
(define A179828 (xorfuns A179761 A179763))
(define A179829 (xorfuns A179762 A179763))

;;;

(define A179764 (PARTIALSUMS 1 1 (lambda (n) (- (A033999 (A179761 n))))))
(define A179765 (PARTIALSUMS 1 1 (lambda (n) (- (A033999 (A179762 n))))))
(define A179766 (PARTIALSUMS 1 1 (lambda (n) (- (A033999 (A179763 n))))))

;; A179760 & A179767-A179769 free.

;;;

;; For A179761, A179762 and A179763 (the bit-sequences)
;; we need: column-bit-sequence, quadrisection
;; positions of 1's in that latter, and the first differences,
;; complement of positions of ones.  (3*5 = 15)

;; (map (lambda (n) (A179761 (A028347 (+ n 2)))) (iota 21))
;; --> (1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1)


(definec (A179770 n) (A179761 (A028875 (+ n 2))))

(define (A179771 n) (A179770 (* 4 n)))
;; (map A179771 (iota 61))
;; Conjecture: the last zero (107th) occurs at n=166, after which only ones occur:
;; (1 1 0 0 0 0 1 1 1 0 0 0 1 0 1 0 0 0 0 1 1 0 0 1 0 1 1 0 0 1 1 0 0 0 0 1 0 1 1 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 1 0 0 0 0 1)

;; Conjecture, from a(60)=167 onward, all integers >= 167 occur.
;; Positions of "L"'s on the central diagonal of A122242:
(define A179772 (MATCHING-POS 1 1 (lambda (i) (= 1 (A179771 i)))))

;; (map A179772 (iota 30))
;; (1 2 7 8 9 13 15 20 21 24 26 27 30 31 36 38 39 47 52 56 61 63 64 65 68 69 71 73 76 81)

;; First differences, one-based:
(define (A179773 n) (- (A179772 (+ n 1)) (A179772 n)))

;; Complement, probably finite, only terms 1-107 exist:
(define A179774 (COMPLEMENT 1 A179772))

;;;;;

(definec (A179775 n) (A179762 (A028875 (+ n 2))))
(define (A179776 n) (A179775 (- (* 4 n) 2))) ;; Quadrisection....

;; Conjecture, from a(56)=164 onward, all integers >= 164 occur.
(define A179777 (MATCHING-POS 1 1 (lambda (i) (= 1 (A179776 i)))))

;; First differences, one-based:
(define (A179778 n) (- (A179777 (+ n 1)) (A179777 n)))

;; Complement, probably finite, only terms 1-108 exist:
(define A179779 (COMPLEMENT 1 A179777))

;;;;;

(define (A179830 n) (A179763 (A028884 (+ n 2))))

(define (A179831 n) (A179830 (1+ (* 4 n))))

(define A179832 (MATCHING-POS 1 1 (lambda (i) (= 1 (A179831 i)))))

;; First differences, one-based:
(define (A179833 n) (- (A179832 (+ n 1)) (A179832 n)))

;; Complement, probably finite, only terms 1-119 exist (1,2,3,4,6,...,163,164)
(define A179834 (COMPLEMENT 1 A179832))


;; Here are the 30 A-numbers you requested: A179827 --- A179856.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here are the 20 A-numbers you requested: A122227 --- A122246.

(define (A122227 n) (A057548 (A057117 n)))
(definec (A122228 n) (if (zero? n) 0 (A122227 (A122228 (-1+ n)))))
(define A122229 (compose-funs A014486 A122228))
(define A122230 (compose-funs A007088 A122229))

(definec (A122231 n) (if (= 1 n) 4 (A122227 (A122231 (-1+ n)))))
(define A122232 (compose-funs A014486 A122231))
(define A122233 (compose-funs A007088 A122232))

(definec (A122234 n) (if (= 1 n) 5 (A122227 (A122234 (-1+ n)))))
(define A122235 (compose-funs A014486 A122234))
(define A122236 (compose-funs A007088 A122235))

(definec (A122238 n) (if (= 1 n) 7 (A122227 (A122238 (-1+ n)))))
(define A122239 (compose-funs A014486 A122238))
(define A122240 (compose-funs A007088 A122239))

(define (A122237 n) (A057548 (A082358 n))) ;; (A057548 (A082356 n))

(definec (A122241 n) (if (= 1 n) 4 (A122237 (A122241 (-1+ n)))))
(definec (A122242 n) (A014486 (A122241 n)))
;; (define A122242 (compose-funs A014486 A122241))
(define A122243 (compose-funs A007088 A122242))

(definec (A122244 n) (if (= 1 n) 5 (A122237 (A122244 (-1+ n)))))
(definec (A122245 n) (A014486 (A122244 n)))
;; (define A122245 (compose-funs A014486 A122244))
(define A122246 (compose-funs A007088 A122245))


;; Here are the 40 A-numbers you requested: A125974 --- A126013.

(define A125976 (compose-funs A080300 A125974 A014486)) ;; Kreweras

(define A125977 (compose-funs A057163 A125976))
(define A125978 (compose-funs A125976 A057163))

(define A125979 (compose-funs A057164 A125976))


;; (equal? (map A125979 (iota0 42)) (map A071663 (iota0 42))) --> #t
;; (equal? (map A125979 (iota0 43)) (map A071663 (iota0 43))) --> ()

(define A125980 (compose-funs A057163 A125976 A057163)) ;; A057163-conjugate of Kreweras.
(define A126290 (compose-funs A069772 A125976 A069772)) ;; A069772-conjugate of Kreweras.
;; Because A069772 = A057164 o A069771 = A069771 o A057164, it follows that
;; A126290 = A069771 o (A057164 o A125976 o A057164) o A069771 = A069771 o A125976 o A069771.



(define A125981 (catfun1 *A125981)) ;; Deutsch's bijection from 2000.
(define A125982 (catfun1 *A125982)) ;; Inverse.

(define A125983 (compose-funs A057163 A125981 A057163)) ;; A057163-conjugates.
(define A125984 (compose-funs A057163 A125982 A057163))

(define A126291 (cc-Afun A125986))
(define A126292 (mc-Afun A125986))
(define A126293 (lc-Afun A125986))

(define A126294 (cc-Afun A125988))
(define A126295 (fc-Afun A125988))
(define A126296 (mc-Afun A125988))
(define A126297 (lc-Afun A125988))


(define A126298 (fun-succ-matching-is0 (lambda (i) (= i (A125988 i)))))
(define A126298v2 (fun-succ-matching-is0 (lambda (i) (= i (A125986 (A125986 i))))))

(define (A126299 n) (A071158 (A126298 n)))
(define A126300 (fun-succ-matching-is0 (lambda (i) (= i (A125986 i)))))
;; Wrongly: (define A126300 (fun-succ-matching-is0 (lambda (i) (= i (A125988 (A125988 i))))))
(define (A126301 n) (A071158 (A126300 n)))



(define A126312 (MATCHING-POS 0 0 (lambda (i) (= i (A071661 i)))))
(define A126312_prove_that_same (MATCHING-POS 0 0 (lambda (i) (= i (A125977 i)))))
(define A126313 (compose-funs A069772 A125976)) ;; = (A126290 A069772) = (A126315 A057164)
(define A126314 (compose-funs A125976 A069772)) ;; = (A069772 A126290) = (A057164 A126316)

(define A126315 (compose-funs A069771 A125976)) ;; = (A126290 A069771) = (A126313 A057164)
(define A126316 (compose-funs A125976 A069771)) ;; = (A069771 A126290) = (A057164 A126314)

(define A126317 (cc-Afun A125977))
(define A126318 (mc-Afun A125977))
(define A126319 (lc-Afun A125977))

(define A126320 (compose-funs A057164 A057163 A057164))

;; Here are the 31 A-numbers you requested: A127277 --- A127307.

(define A127277 (cc-Afun A126313))
(define A127278 (fc-Afun A126313))
(define A127279 (mc-Afun A126313))
(define A127280 (lc-Afun A126313))

(define A127281 (cc-Afun A126315))
(define A127282 (fc-Afun A126315))
(define A127283 (mc-Afun A126315))

(define A127285 (catfun1 *A127285!))
(define A127286 (catfun1 *A127286!))

(define A127287 (catfun1 *A127287!))
(define A127288 (catfun1 *A127288!))

(define A127293 (cc-Afun A127291))
(define A127294 (mc-Afun A127291))
(define A127295 (lc-Afun A127291))

(define A127296 (cc-Afun A127289))
(define A127297 (mc-Afun A127289))
(define A127298 (lc-Afun A127289))

;; Do this if you want to compute a bit more terms in a reasonable time:
;; (define vecA000040 (fasload "/home/karttu/Schemuli/primes_up_to_65537nth.vec"))
;; (define (A000040 n) (vector-ref vecA000040 (- n 1)))

(define (*A127301 s)
  (if (null? s) 1 (fold-left (lambda (m t) (* m (A000040 (*A127301 t)))) 1 s))
)
(define (*A127301v2 s) (fold-right (lambda (t m) (* (A000040 (*A127301 t)) m)) 1 s))

;; (define A127301 (compose-funs *A127301 A014486->parenthesization A014486))
(definec (A127301 n) (*A127301 (A014486->parenthesization (A014486 n))))
(definec (A127301v2 n) (*A127301v2 (A014486->parenthesization (A014486 n))))

;; (define (*A057123 s) (fold-right (lambda (x y) (list (*A057123 x) y)) '() s))
;; (define (*A057123v2 s) (fold-right (lambda (x y) (cons (*A057123 x) (cons y '()))) '() s))

(define (*A127302 s) (fold-right (lambda (t m) (* (A000040 (*A127302 t)) (A000040 m))) 1 s))
(define (*A127302v2 s) (*A127301 (*A057123 s)))

(definec (A127302old n) (A127301 (A057123 n)))
(definec (A127302 n) (*A127302 (A014486->parenthesization (A014486 n))))

;; Encode the Lukasiewicz-word of the general tree encoded by A014486(n)
;; using the partition encoding scheme described by Marc LeBrun on SeqFan mailing
;; list on Wednesday, 11 January 2006:
;;
;;   For example here's a way to map integers 1-to-1 to partitions in a "crazy" order:
;;   factor n, take the (finite) tuple of exponents, add 1 to the first,
;;   use the rest as successive differences between parts,
;;   and finally subtract 1 from the last part:

;;    
;;     2 -> [1]           -> 1
;;     3 -> [0,1]         -> 11
;;     4 -> [2]           -> 2
;;     5 -> [0,0,1]       -> 111
;;     6 -> [1,1]         -> 22
;;     7 -> [0,0,0,1]     -> 1111
;;     8 -> [3]           -> 3
;;     9 -> [0,2]         -> 12
;;     10 -> [1,0,1]       -> 222
;;     11 -> [0,0,0,0,1]   -> 11111
;;     12 -> [2,1]         -> 33
;;     13 -> [0,0,0,0,0,1] -> 111111
;;     14 -> [1,0,0,1]     -> 2222
;;     15 -> [0,1,0,1]     -> 1222
;;     16 -> [4]           -> 4
;;    
;;   and so on.  The powers of 2 map to the singleton partitions, primes to all ones, etc.

;; Inverse process: from a sorted (ascending) partition of n,
;; subtract 1 from the first part, then take the first differences
;; of parts, and add 1 to the last (of differences or the first part if only one),
;; and use them as the exponents of p_1, p_2, etc.

;;   L-word    L-word+1      sort <
;;        ()         ()           ()                  1.
;;       (1)        (2)  2       (2)       [2]  2^2 = 4.
;;     (2 0)      (3 1)  4     (1 3)     [0 3]  3^3 = 27.
;;     (1 1)      (2 2)  4     (2 2)     [1 1]  2*3 = 6
;;   (3 0 0)    (4 1 1)  6   (1 1 4)   [0 0 4]  5^4 = 625
;;   (2 0 1)    (3 1 2)  6   (1 2 3)   [0 1 2]  3*25 = 75
;;   (2 1 0)    (3 2 1)  6   (1 2 3)   -- "" --        75
;;   (1 2 0)    (2 3 1)  6   (1 2 3)   -- "" --        75
;;   (1 1 1)    (2 2 2)  6   (2 2 2)   [1 0 1]  2*5  = 10
;; (4 0 0 0)  (5 1 1 1)  8 (1 1 1 5)   [0 0 0 5] 7^5 = 16807
;; (3 0 0 1)  (4 1 1 2)  8 (1 1 2 4)   [0 0 1 3] 5*343 = 1715


(define (add1to_the_last lista)
  (let ((rev (reverse lista)))
     (reverse! (cons (+ 1 (car rev)) (cdr rev)))
  )
)

(define (ascpart_to_prime-exps partlist)
   (if (null? partlist) partlist
       (add1to_the_last (cons (- (car partlist) 1) (DIFF partlist)))
   )
)

(define (sub1from_the_last lista)
  (let ((rev (reverse lista)))
     (reverse! (cons (- (car rev) 1) (cdr rev)))
  )
)

(define (prime-exps_to_ascpart explist)
   (if (null? explist) explist
       (sub1from_the_last (PARTSUMS (cons (+ (car explist) 1) (cdr explist))))
   )
)


(define (binexp_to_ascpart n)
  (let ((runlist (reverse! (binexp->runcount1list n))))
    (PARTSUMS (cons (car runlist) (map -1+ (cdr runlist))))
  )
)

(define (ascpart_to_binexp ascpart)
 (runcount1list->binexp (reverse! (cons (car ascpart) (map 1+ (DIFF ascpart)))))
)

(define (A000027yav2 n) (ascpart_to_binexp (binexp_to_ascpart n)))

(define (explist->n explist)
  (if (null? explist) 1
      (mul (lambda (i) (expt (A000040 i) (list-ref explist (-1+ i))))
           1 (length explist)
      )
  )
)

;; (define (A129593_the_old_definition n) ;; L-word to prime-factorization encoded partition code.
;;   (if (zero? n) 1
;;       (let* ((partlist (sort (p->Lw (A014486->parenthesization (A014486 n))) <))
;;              (explist (add1to_the_last (cons (car partlist) (DIFF partlist))))
;;             )
;;         (explist->n explist)
;;       )
;;   )
;; )


(define (A129593 n) ;; Lukasiewicz-word to prime-factorization encoded partition code.
  (if (zero? n) 1
      (let* ((partlist (sort (delete-matching-items!
                                 (p->Lw (A014486->parenthesization (A014486 n)))
                                 zero?
                             )
                             <
                       )
             )
             (explist (ascpart_to_prime-exps partlist))
            )
        (explist->n explist)
      )
  )
)


;; Like A129593, but add 1 to all leaves (including the last leaf!) except the root,
;; and then consider as a partition.
(define (A129599 n) ;; Lukasiewicz-word to prime-factorization encoded partition code, variant.
  (if (zero? n) 1
      (let* ((Lword (p->Lw (A014486->parenthesization (A014486 n)))) ;; Without the last leaf.
             (partlist (sort (cons 1 (cons (car Lword) (map 1+ (cdr Lword)))) <))
             (explist (ascpart_to_prime-exps partlist))
            )
        (explist->n explist)
      )
  )
)


;; For A129593 begins as 1,1,2,3,5,7,11,15,22,30,42,56,77,101,... (like A000041, proved.)
;; For A129599 begins as 1,1,1,2,3,5,7,11,15,22,30,42,56,77,... (like A000041)
(definec (Alnumofdistincts n)
  (if (zero? n) 1
      (length
        (uniq
          (sort (map (lambda (x) (A129599 (+ (A014137 (-1+ n)) x)))
                     (iota0 (-1+ (A000108 n))))
                <
          )
        )
      )
  )
)

(define (A000027yav n) ;; An identity-function on n >= 1
  (if (< n 2) n
      (let ((explist (ascpart_to_prime-exps (prime-exps_to_ascpart (primefacs->explist n)))))
        (explist->n explist)
      )
  )
)

(define (conjugate-partition ascpart)
  (let loop ((conjpart (list))
             (ascpart ascpart)
            )
    (cond ((null? ascpart) conjpart)
          (else (loop (cons (length ascpart) conjpart)
                      (delete-matching-items! (map -1+ ascpart) zero?)
                )
          )
    )
  )
)


(define (add-ascpartitions ap1 ap2)
  (let loop ((sums (list))
             (dp1 (reverse! ap1))
             (dp2 (reverse! ap2))
            )
     (cond ((and (null? dp1) (null? dp2)) sums)
           ((null? dp1) (loop (cons (car dp2) sums) dp1 (cdr dp2)))
           ((null? dp2) (loop (cons (car dp1) sums) (cdr dp1) dp2))
           (else (loop (cons (+ (car dp1) (car dp2)) sums) (cdr dp1) (cdr dp2)))
     )
  )
)

(define (A122111v5 n) (apply * (map A000040 (conjugate-partition (A112798row n)))))

(define (A122111maybe n) ;; 1,2,4,3,8,6,16,5,9,12,32,10,64,24,18,7,128,15,256,20,36,48,512,14,27,96,25,... 
  (if (< n 2) n
      (let ((explist (ascpart_to_prime-exps (conjugate-partition (prime-exps_to_ascpart (primefacs->explist n))))))
        (explist->n explist)
      )
  )
)


;; A241909 etc transferred to miscnum2.scm
;; Cross-map:
(definec (A241909v3 n) (apply * (map A000040 (prime-exps_to_ascpart (primefacs->explist n)))))

(definec (A241909maybe n) (explist->n (ascpart_to_prime-exps (A112798row n)))) ;; XXX - Prove that it is involution!

(define (A241914 n) (if (= n 1) 0 (- n (+ 2 (A203623 (- (A241920 n) 1)))))) ;; Cf. A241910
(define (A241915 n) (if (= n 1) 1 (- n (A203623 (- (A241920 n) 1)) 1))) ;; Cf. A241911

(define (A241916v3 n) (apply * (map A000040 (conjugate-partition (prime-exps_to_ascpart (primefacs->explist n))))))

(definec (A241918row n) (prime-exps_to_ascpart (primefacs->explist n)))

(definec (A241918 n) ;; Cf. A112798
   (cond ((zero? (A241914 n))
             (if (zero? (A241914 (+ n 1)))
                 (A067255 n)
                 (+ 1 (A067255 n))
             )
         )
         ((zero? (A241914 (+ 1 n))) (+ (A241918 (- n 1)) (- (A067255 n) 1)))
         (else (+ (A241918 (- n 1)) (A067255 n)))
   )
)

(define (A241918v2 n) (if (= n 1) 0 (list-ref (A241918row (A241920 n)) (A241914 n))))


(define (A061395v2 n) (length (A241918row n)))

(define (A067255row n) (if (= 1 n) (list 0) (primefacs->explist n))) ;; Should be cross-linked with A241918 and ...
(define (A067255 n) (list-ref (A067255row (A241920 n)) (A241914 n))) ;; offset=1.

(define (A129595bi col row)
  (let ((explist (ascpart_to_prime-exps
                        (add-ascpartitions (prime-exps_to_ascpart (primefacs->explist col))
                                           (prime-exps_to_ascpart (primefacs->explist row))
                        )
                 )
        )
       )
    (explist->n explist)
  )
)

(define (A129595 n) (A129595bi (1+ (A025581 (-1+ n))) (1+ (A002262 (-1+ n))))) ;; One-based
(define (A129596 n) (A129595bi (A002024 n) (1+ (A002262 (-1+ n))))) ;; One-based, upper-triangle.
(define (A129597 n) (A129595bi n n)) ;; Central Diagonal
(define (A129598 n) (A129595bi n 2)) ;; Row 2. Note the similarity with A050399 & A072995, except that a(15)=75 and a(30)=150 (instead of ...) Formula: a(n) = n * A111089(n)

(define (A129594 n) (if (zero? n) n (ascpart_to_binexp (conjugate-partition (binexp_to_ascpart n)))))

(define (A129594v2 n) (A075158 (-1+ (A122111 (1+ (A075157 n))))))

(define (A122111v2 n) (1+ (A075157 (A129594 (A075158 (- n 1))))))

(define (A243353v2 n) (A241909 (+ 1 (A075157 n))))  ;; Offset 0, results start from 1. (Might be slow?)

(define (A243354v2 n) (A075158 (- (A241909 n) 1))) ;; Offset 1, results start from zero. (Slow!)

(define (A242424v2 n) (A243353 (A226062 (A243354 n))))

;; A243488-A243506 are now reserved for your use.

(define (A243503v3 n) (apply + (prime-exps_to_ascpart (primefacs->explist n))))
(define (A243504v3 n) (apply * (prime-exps_to_ascpart (primefacs->explist n))))

;; Transfer to miscnum2.scm
(define (A243503v2 n) (A227183 (A075158 (- n 1))))

(define (A243504v2 n) (A227184 (A075158 (- n 1))))



(define (A243051v0 n)
  (explist->n (ascpart_to_prime-exps (bulgarian-operation (prime-exps_to_ascpart (primefacs->explist n)))))
)

(define (A243051v2 n) (1+ (A075157 (A226062 (A075158 (- n 1))))))

(define (A243052v0 n)
 (explist->n (ascpart_to_prime-exps (bulgarian-operation-nth-order (prime-exps_to_ascpart (primefacs->explist n)) 2)))
)

(define (A243053v0 n)
 (explist->n (ascpart_to_prime-exps (bulgarian-operation-nth-order (prime-exps_to_ascpart (primefacs->explist n)) 3)))
)

(define (A243060bi row col)
 (explist->n
   (ascpart_to_prime-exps
     (bulgarian-operation-nth-order (prime-exps_to_ascpart (primefacs->explist col)) row)
   )
 )
)

(define (A243060 n) (A243060bi (A002260 n) (A004736 n)))

;; Seems to give primes, from 5 onward:
(definec (Afirst_differing_point_of_A243060_and_A243070 n)
  (let loop ((i 1))
       (if (not (= (A243060bi n i) (A243070bi n i))) i (loop (+ 1 i)))
  )
)


;; Convert col and row from binexp's to prime-exps, multiply, and convert the result back to binexp:
(define (A129600bi col row) (A075158 (-1+ (* (1+ (A075157 col)) (1+ (A075157 row))))))
(define (A129600 n) (A129600bi (A025581 n) (A002262 n))) ;; Zero-based
(define (A129601 n) (A129600bi (A003056 n) (A002262 n))) ;; Zero-based, upper-triangle.
(define (A129602 n) (A129600bi n n)) ;; Central diagonal.

(define (A129602v2 n)
  (if (zero? n) n
      (let ((rl (binexp->runcount1list n)))
        (runcount1list->binexp (cons (* 2 (car rl)) (map (lambda (i) (- (* 2 i) 1)) (cdr rl))))
      )
  )
)

(define (A014601_almost n) (A129600bi 1 n)) ;; Except a(0), is same: Congruent to 0 or 3 mod 4.

(define (A129603 n) (runcount1list->binexp (map (lambda (i) (- (* 2 i) 1)) (binexp->runcount1list n))))


(define (A075157v2 n) ;; Zero-based.
  (if (< n 1) n
      (let ((explist (ascpart_to_prime-exps (binexp_to_ascpart n))))
        (-1+ (explist->n explist))
      )
  )
)

(define (Asomething n)
  (if (< n 2) n
      (let ((explist (reverse! (primefacs->explist n))))
        (explist->n explist)
      )
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define A127306 (MATCHING-POS 0 0 (lambda (i) (= i (A126313 i)))))

(define (A127307 n) (A072764bi (A072795 (A072771 n)) (A072772 n))) ;; One-based
(define A127307v2 (compose-funs A080300 A004758 A014486)) ;; Also.


(define A127377 (catfun1 *A127377!))
(define A127378 (catfun1 *A127378!))
(define A127379 (catfun1 *A127379!))
(define A127380 (catfun1 *A127380!))
(define A127381 (catfun1 *A127381!))
(define A127382 (catfun1 *A127382!))

(define A127387 (catfun1 *A127387!))
(define A127388 (catfun1 *A127388!))

(define A127383 (cc-Afun A127377))
(define A127384 (cc-Afun A127379))
(define A127385 (cc-Afun A127387))
(define A127386 (cc-Afun A127388))

(define A127389 (fc-Afun A127377))

(define A129604  (catfun1 *A129604))
(define A129604! (catfun1 *A129604!))

(define A129605  (catfun1 *A129605))
(define A129605! (catfun1 *A129605!))

(define A129606  (catfun1 *A129606))
(define A129606! (catfun1 *A129606!))

(define A129607  (catfun1 *A129607))
(define A129607! (catfun1 *A129607!))

(define A129608    (catfun1 *A129608))
(define A129608v2  (catfun1 *A129608v2))
(define A129608!   (catfun1 *A129608!))
(define A129608v2! (catfun1 *A129608v2!))

(define A129609  (catfun1 *A129609))
(define A129609! (catfun1 *A129609!))

(define A129610  (catfun1 *A129610))
(define A129610! (catfun1 *A129610!))

(define A129611  (catfun1 *A129611))
(define A129611! (catfun1 *A129611!))

(define A129612  (catfun1 *A129612))
(define A129612! (catfun1 *A129612!))



(define (compcounts)
  (let ((u 13))
    (compute-and-print-count-seqs A127378 "./nextbatch/A127378.counts.upto13.txt" u)
;;n=13: ccs=(1 1 1 2 6 15 46 141 446 1427 4722 15884 54224 187380) fcs=(1 1 0 1 2 4 10 23 56 138 344 870 2220 5716) mcs=(1 1 2 4 4 8 8 8 8 16 16 16 16 16) lcs=(1 1 2 4 4 8 8 8 8 16 16 16 16 16)
;; INVERT([1,1,0,1,2,4,10,23,56,138,344,870,2220,5716]) gives: 1,2,3,6,12,26,59,138,332,814,2028,5118 (A086625)


    (compute-and-print-count-seqs A127380 "./nextbatch/A127380.counts.upto13.txt" u)
;;n=13: ccs=(1 1 2 4 9 23 62 179 543 1705 5482 18056 60634 206824) fcs=(1 1 2 3 6 12 26 59 138 332 814 2028 5118 13054) mcs=(1 1 1 2 4 4 8 8 8 8 16 16 16 16) lcs=(1 1 1 2 4 4 8 8 8 8 16 16 16 16)
;; A086625 begins from zero as: [1,2,3,6,12,26,59,138,332,814,2028,5118,13054,...]

    (compute-and-print-count-seqs A127387 "./nextbatch/A127387.counts.upto13.txt" u)
;;n=13: ccs=(1 1 1 3 8 23 71 226 743 2500 8570 29828 105116 374308) fcs=(1 1 0 1 2 4 10 23 56 138 344 870 2220 5716) mcs=(1 1 2 2 2 2 2 2 2 2 2 2 2 2) lcs=(1 1 2 2 2 2 2 2 2 2 2 2 2 2)

    (compute-and-print-count-seqs A127388 "./nextbatch/A127388.counts.upto13.txt" u)
;;n=13: ccs=(1 1 2 4 10 27 79 244 784 2597 8805 30407 106565 377977) fcs=(1 1 2 3 6 12 26 59 138 332 814 2028 5118 13054) mcs=(1 1 1 2 2 2 2 2 2 2 2 2 2 2) lcs=(1 1 1 2 2 2 2 2 2 2 2 2 2 2)

    (compute-and-print-count-seqs A127300 "./nextbatch/A127300.counts.upto13.txt" u)
  )
)

 
(define A071655 (catfun1 *A071655!)) ;; robr_car_cdr!
(define A071656 (catfun1 *A071656!)) ;; car_cdr_robl!
(define A071657 (catfun1 *A071657!)) ;; car_robr_cdr!
(define A071658 (catfun1 *A071658!)) ;; cdr_robl_car!
(define A071659 (catfun1 *A071659!)) ;; car_cdr_robr!
(define A071660 (catfun1 *A071660!)) ;; robl_car_cdr!

(define A071661 (compose-fun-to-nth-power A057505 2))
(define A071662 (compose-fun-to-nth-power A057506 2))
(define A071663 (compose-fun-to-nth-power A057505 3))
(define A071664 (compose-fun-to-nth-power A057506 3))
(define A071665 (compose-fun-to-nth-power A057505 4)) ;; Not A071662 !
(define A071666 (compose-fun-to-nth-power A057506 4)) ;; Not A071661 !
(define A071667 (compose-fun-to-nth-power A057505 5)) ;; Not A057506 !
(define A071668 (compose-fun-to-nth-power A057506 5)) ;; Not A057505 !
(define A071669 (compose-fun-to-nth-power A057505 6)) ;; Not A000027 !
(define A071670 (compose-fun-to-nth-power A057506 6)) ;; Not A000027 !


;; (define A057161vx? (catfun1restricted_to_tpt_subset gm_what??)) ;; Is it possible?
;; (define A057162vx? (catfun1restricted_to_tpt_subset gm_what??))

(define A072090 (catfun1 cdr_robr_car!))
(define A072091 (catfun1 car_robl_cdr!))
(define A072092 (catfun1 car_robr_car!))
(define A072093 (catfun1 car_robl_car!))
(define A072094 (catfun1 cdr_robr_cdr!))
(define A072095 (catfun1 cdr_robl_cdr!))

;; Note: the first term should be 1, not 0!
(define (A072795 n) (+ n (A000108 (A072643 n))))
(define A072795v2 (compose-funs A069770 A057548))
(define A072795v3 (catfun1 (lambda (s) (cons '() s)))) ;; i.e. A069770 o A057548

(define (char_A057548 n)
  (cond ((zero? n) 0)
        ((null? (cdr (A014486->parenthesization (A014486 n)))) 1)
        (else 0)
  )
)


(define (char_A072795 n)
  (cond ((zero? n) 0)
        ((null? (car (A014486->parenthesization (A014486 n)))) 1)
        (else 0)
  )
)

(define (*char_A072795 s) (fold-right (lambda (x y) (if (null? x) 1 0)) 0 s))
(define (char_A072795v2 n) (*char_A072795 (A014486->parenthesization (A014486 n))))


(define A072796 (catfun1 *A072796!))
(define A072797 (catfun1 *A072797!))

(define A072798 (catfun0 *A069787!)) ;; Yes, it's telescoping one.
(define A072799 (compose-funs 1+ (catfun0 *A069787!)))
(define A072800 (compose-funs A030101 A014486))

(define A073194 (catfun1 *A073194!))
(define A073195 (catfun1 *A073195!))
(define A073196 (catfun1 *A073196!))
(define A073197 (catfun1 *A073197!))
(define A073198 (catfun1 *A073198!))
(define A073199 (catfun1 *A073199!))

(define A073205 (catfun1 *A073205!))
(define A073206 (catfun1 *A073206!))
(define A073207 (catfun1 *A073207!))
(define A073208 (catfun1 *A073208!))
(define A073209 (catfun1 *A073209!))
(define A073210 (catfun1 *A073210!))

(define A073269 (catfun1 *A073269!))
(define A073270 (catfun1 *A073270!))

(define A073280 (catfun1 *A073280!))
(define A073281 (catfun1 *A073281!))
(define A073282 (catfun1 *A073282!))
(define A073283 (catfun1 *A073283!))
(define A073284 (catfun1 *A073284!))
(define A073285 (catfun1 *A073285!))
(define A073286 (catfun1 *A073286!))
(define A073287 (catfun1 *A073287!))
(define A073288 (catfun1 *A073288!))
(define A073289 (catfun1 *A073289!))

(define A073290 (compose-fun-to-nth-power A069767 2)) ;; Row 105.
(define A073291 (compose-fun-to-nth-power A069768 2)) ;; Row 197.

(define A073292 (compose-fun-to-nth-power A069767 3)) ;; Row ...
(define A073293 (compose-fun-to-nth-power A069768 3)) ;; Row ...

(define A073294 (compose-fun-to-nth-power A069767 4)) ;; Row ...
(define A073295 (compose-fun-to-nth-power A069768 4)) ;; Row ...

(define A073296 (compose-fun-to-nth-power A069767 5)) ;; Row ...
(define A073297 (compose-fun-to-nth-power A069768 5)) ;; Row ...

(define A073298 (compose-fun-to-nth-power A069767 6)) ;; Row ...
(define A073299 (compose-fun-to-nth-power A069768 6)) ;; Row ...


(define A074679 (catfun1 *A074679!)) ;; was gmrobl!
(define A074680 (catfun1 *A074680!)) ;; was gmrobr!
(define A074679v2 (catfun1 *A074679v2!))
(define A074680v2 (catfun1 *A074680v2!))

(define A074681 (catfun1 *A074681!))
(define A074682 (catfun1 *A074682!))
(define A074683 (catfun1 *A074683!))
(define A074684 (catfun1 *A074684!))
(define A074685 (catfun1 *A074685!))
(define A074686 (catfun1 *A074686!))
(define A074687 (catfun1 *A074687!))
(define A074688 (catfun1 *A074688!))
(define A074689 (catfun1 *A074689!))
(define A074690 (catfun1 *A074690!))


(define check-these
 (list
;;     (list 0 73265 A073265)
;;     (list 0 73266 A073266)
;;     (list 0 73267 A073267)
;;     (list 0 73268 A073268)

;;     (list 0 74679 A074679 A074680 (list A057163 A074680 A057163) (list A073283 A073280 A072796))
;;     (list 0 74680 A074680 A074679 (list A057163 A074679 A057163) (list A072796 A073280 A073282))

;;     (list 0 74681 A074681 A074682 (list A057163 A074684 A057163))
;;     (list 0 74682 A074682 A074681 (list A057163 A074683 A057163))

;;     (list 0 74683 A074683 A074684 (list A057163 A074682 A057163))
;;     (list 0 74684 A074684 A074683 (list A057163 A074681 A057163))

;;     (list 0 74685 A074685 A074686 (list A057163 A074689 A057163))
;;     (list 0 74686 A074686 A074685 (list A057163 A074690 A057163))

;;     (list 0 74687 A074687 A074688 (list A057163 A074688 A057163))
;;     (list 0 74688 A074688 A074687 (list A057163 A074687 A057163))

;;     (list 0 74689 A074689 A074690 (list A057163 A074685 A057163))
;;     (list 0 74690 A074690 A074689 (list A057163 A074686 A057163))
;;

;;     (list 0 73269 A073269 A073270)
;;     (list 0 73270 A073270 A073269)

;;     (list 0 73280 A073280 A073280)
;;     (list 0 73281 A073281 A073281)

;;     (list 0 73282 A073282 A073283)
;;     (list 0 73283 A073283 A073282)

;;     (list 0 73284 A073284 A073285)
;;     (list 0 73285 A073285 A073284)

;;     (list 0 73286 A073286 A073287)
;;     (list 0 73287 A073287 A073286)

;;     (list 0 73288 A073288 A073289)
;;     (list 0 73289 A073289 A073288)

;;     (list 0 73290 A073290 A073291 (list A057163 A073291 A057163))
;;     (list 0 73291 A073291 A073290 (list A057163 A073290 A057163))

;;     (list 0 73292 A073292 A073293 (list A057163 A073293 A057163))
;;     (list 0 73293 A073293 A073292 (list A057163 A073292 A057163))

;;     (list 0 73294 A073294 A073295 (list A057163 A073295 A057163))
;;     (list 0 73295 A073295 A073294 (list A057163 A073294 A057163))

;;     (list 0 73296 A073296 A073297 (list A057163 A073297 A057163))
;;     (list 0 73297 A073297 A073296 (list A057163 A073296 A057163))

;;     (list 0 73298 A073298 A073299 (list A057163 A073299 A057163))
;;     (list 0 73299 A073299 A073298 (list A057163 A073298 A057163))

;;     (list 0 73194 A073194 A073195 (list A057163 A073205 A057163))
;;     (list 0 73195 A073195 A073194 (list A057163 A073206 A057163))

;;     (list 0 73196 A073196 A073197 (list A057163 A073207 A057163))
;;     (list 0 73197 A073197 A073196 (list A057163 A073208 A057163))

;;     (list 0 73198 A073198 A073199 (list A057163 A073209 A057163))
;;     (list 0 73199 A073199 A073198 (list A057163 A073210 A057163))

;;     (list 0 73205 A073205 A073206 (list A057163 A073194 A057163))
;;     (list 0 73206 A073206 A073205 (list A057163 A073195 A057163))

;;     (list 0 73207 A073207 A073208 (list A057163 A073196 A057163))
;;     (list 0 73208 A073208 A073207 (list A057163 A073197 A057163))

;;     (list 0 73209 A073209 A073210 (list A057163 A073198 A057163))
;;     (list 0 73210 A073210 A073209 (list A057163 A073199 A057163))

;;     (list 0 72732 A072732 A072733)
;;     (list 0 72733 A072733 A072732)
;;     (list 0 72734 A072734 A072735)
;;     (list 0 72735 A072735 A072734)

;;     (list 0 72736 A072736)
;;     (list 0 72737 A072737)
;;     (list 0 72738 A072738)
;;     (list 0 72739 A072739)
;;     (list 0 72740 A072740)
;;     (list 0 72741 A072741)

;;     (list 0 72781 A072781)
;;     (list 0 72782 A072782)
;;     (list 0 72783 A072783)
;;     (list 0 72784 A072784)
;;     (list 0 72785 A072785)
;;     (list 0 72786 A072786)

;;     (list 0 72787 A072787 A072788)
;;     (list 0 72788 A072788 A072787)

;;     (list 0 72789 A072789)
;;     (list 0 72792 A072792)

;;     (list 0 72793 A072793 A072794 (list A048680 A054238))
;;     (list 0 72794 A072794 A072793 (list A054239 A048679))

;;     (list 0 72795 A072795 A072795 (list A069770 A057548))

;;     (list 0 72796 A072796 A072796 (list A057163 A072797 A057163))
;;     (list 0 72797 A072797 A072797 (list A057163 A072796 A057163))

;;     (list 0 72798 A072798 A072798)
;;     (list 0 72799 A072799 A072799)

;;     (list 0 72800 A072800)


;;     (list 0 72764 A072764 A072765 (list A072766 A038722) (list A069770 A072766))
;;     (list 0 72765 A072765 A072764)
;;     (list 0 72766 A072766 A072767 (list A072764 A038722) (list A069770 A072764))
;;     (list 0 72767 A072767 A072766)

;;     (list 0 72768 A072768)
;;     (list 0 72769 A072769)
;;     (list 0 72770 A072770)
;;     (list 0 72771 A072771)
;;     (list 0 72772 A072772)
;;     (list 0 72773 A072773)


;;     (list 0 72634 A072634 A072635)
;;     (list 0 72635 A072635 A072634 (list A057163 A072637))
;;     (list 0 72636 A072636 A072637)
;;     (list 0 72637 A072637 A072636 (list A057163 A072635))

;;     (list 0 72644 A072644)

;;     (list 0 72645 A072645)

;;     (list 0 72646 A072646 A072647 (list A048680 A072636))
;;     (list 0 72647 A072647 A072646 (list A072637 A048679))

;;     (list 0 72648 A072648)
;;     (list 0 72649 A072649)
;;     (list 0 72650 A072650)

;;       (list 0 72656 A072656 A072657)
;;       (list 0 72657 A072657 A072656 (list A057163 A072659))
;;       (list 0 72658 A072658 A072659)
;;       (list 0 72659 A072659 A072658 (list A057163 A072657))

;;        (list 0 72660 A072660)
;;        (list 0 72661 A072661)
;;        (list 0 72662 A072662)
;; 
;;        (list 0 71655 A071655  A071656 (list A057163 A071660 A057163))
;;        (list 0 71656 A071656  A071655 (list A057163 A071659 A057163))
;;        (list 0 71657 A071657  A071658 (list A057163 A071658 A057163))
;;        (list 0 71658 A071658  A071657 (list A057163 A071657 A057163))
;;        (list 0 71659 A071659  A071660 (list A057163 A071656 A057163))
;;        (list 0 71660 A071660  A071659 (list A057163 A071655 A057163))
;; 
;;        (list 0 72088 A072088  A072089)
;;        (list 0 72089 A072089  A072088)
;; 
;;        (list 0 72090 A072090  A072091 (list A057163 A072091 A057163))
;;        (list 0 72091 A072091  A072090 (list A057163 A072090 A057163))
;; 
;;        (list 0 72092 A072092  A072093 (list A057163 A072095 A057163))
;;        (list 0 72093 A072093  A072092 (list A057163 A072094 A057163))
;;        (list 0 72094 A072094  A072095 (list A057163 A072093 A057163))
;;        (list 0 72095 A072095  A072094 (list A057163 A072092 A057163))
;; 
;;        (list 0 72619 A072619  A072620)
;;        (list 0 72620 A072620  A072619)
;;        (list 0 72621 A072621  A072622)
;;        (list 0 72622 A072622  A072621)
;; 
;;        (list 0 71661 A071661  A071662 (list A057163 A071662 A057163))
;;        (list 0 71662 A071662  A071661 (list A057163 A071661 A057163))
;;        (list 0 71663 A071663  A071664 (list A057163 A071664 A057163))
;;        (list 0 71664 A071664  A071663 (list A057163 A071663 A057163))
;;        (list 0 71665 A071665  A071666 (list A057163 A071666 A057163))
;;        (list 0 71666 A071666  A071665 (list A057163 A071665 A057163))
;;        (list 0 71667 A071667  A071668 (list A057163 A071668 A057163))
;;        (list 0 71668 A071668  A071667 (list A057163 A071667 A057163))
;;        (list 0 71669 A071669  A071670 (list A057163 A071670 A057163))
;;        (list 0 71670 A071670  A071669 (list A057163 A071669 A057163))
;; 
;;     (list 0 57161 A057161! A057162 (list A057163 A057162 A057163) (list A057508 A069767) (list A069767 A069769))
;;     (list 0 57162 A057162! A057161 (list A057163 A057161 A057163) (list A069768 A057508) (list A069769 A069768))
;;     (list 0 57163 A057163! A057163)
;;     (list 0 57164 A057164! A057164 (list A057163 A069787 A057163))
;;     (list 0 57501 A057501! A057502 (list A057163 A069773 A057163))
;;     (list 0 57502 A057502  A057501 (list A057163 A069774 A057163))
;;     (list 0 57503 A057503! A057504!) ;; (list A057163 A0xxxx A057163)
;;     (list 0 57504 A057504! A057503) ;; (list A057163 A0xxxx A057163)

;;     (list 0 57505 A057505  A057506 (list A057163 A057506 A057163) (list A057164 A057163))
;;     (list 0 57506 A057506  A057505 (list A057163 A057505 A057163) (list A057163 A057164))
;;     (list 0 57508 A057508! A057508 (list A057163 A069769 A057163))
;;     (list 0 57509 A057509! A057510 (list A057163 A069775 A057163) (list A057501 A069770))
;;     (list 0 57510 A057510! A057509 (list A057163 A069776 A057163) (list A069770 A057502))
;;     (list 0 69767 A069767! A069768 (list A057163 A069768 A057163))
;;     (list 0 69768 A069768! A069767 (list A057163 A069767 A057163))
;;     (list 0 69769 A069769! A069769 (list A057163 A057508 A057163))
;;     (list 0 69770 A069770  A069770 (list A057163 A069770 A057163))
;;     (list 0 69771 A069771  A069771)
;;     (list 0 69772 A069772  A069772 (list A057164 A069771) (list A069771 A057164))
;;     (list 0 69773 A069773! A069774 (list A057163 A057501 A057163))
;;     (list 0 69774 A069774! A069773 (list A057163 A057502 A057163))
;;     (list 0 69775 A069775! A069776 (list A057163 A057509 A057163) (list A069773 A069770))
;;     (list 0 69776 A069776! A069775 (list A057163 A057510 A057163) (list A069770 A069774))
;;     (list 0 69787 A069787! A069787 (list A057163 A057164 A057163))
;;     (list 0 69888 A069888! A069888 (list A057501 A057164))
;;     (list 0 69889 A069889! A069889 (list A057164 A057501))
;;     (list 0 57117 A057117 A057118)
;;     (list 0 57118 A057118 A057117)
;;     (list 1 70041 A070041 A070041) ;; Not the correct inverse, but just something.... This one is 1-based.

;;     (list 0 71651 A071651) ;;  A071652  These perms fluctuate so wildly,
;;     (list 0 71652 A071652 A071651 (list A057163 A071654)) ;; that the checking of inverses
;;     (list 0 71653 A071653) ;;  A071654  is out of question after 63 terms...
;;     (list 0 71654 A071654 A071653 (list A057163 A071652)) ;;
;;     (list 0 71671 A071671)
;;     (list 0 71672 A071672)
;;     
;;     (list 0 63171 A063171)
;;     (list 0 71152 A071152)
;;     (list 0 71153 A071153)
;;     (list 0 71155 A071155)
;;     (list 0 71157 A071157)
;;     (list 0 71159 A071159)
;;     (list 0 71160 A071160)
;;     (list 0 71161 A071161)
;;     (list 0 71162 A071162)
;;     (list 0 71163 A071163)
 )
)


;; A082313 - A082342 + 82345 --- 82364

(define A082313 (compose-funs A057501 A057164 A057502))
(define A082314 (compose-funs A057502 A057164 A057501))
(define A082315 (compose-funs A057501 A057501)) ;; ^2
(define A082316 (compose-funs A057502 A057502)) 
(define A082317 (compose-funs A057501 A057501 A057501)) ;; ^3
(define A082318 (compose-funs A057502 A057502 A057502))
(define A082319 (compose-funs A082315 A082315)) ;; ^4
(define A082320 (compose-funs A082316 A082316))
(define A082321 (compose-funs A082315 A082317)) ;; ^5
(define A082322 (compose-funs A082316 A082318))
(define A082323 (compose-funs A082317 A082317)) ;; ^6
(define A082324 (compose-funs A082318 A082318))

(define A082325 (compose-funs A057163 A057511 A057163))
(define A082326 (compose-funs A057163 A057512 A057163))

(define A082325 (catfun1 *A082325!))
(define A082326 (catfun1 *A082326!))

(define A082327 (catfun0 *A082325!))
(define A082328 (catfun0 *A082326!))

(define A082329 (compose-funs 1+ (catfun0 *A082325!)))
(define A082330 (compose-funs 1+ (catfun0 *A082326!)))


(define A082331 (compose-funs A057163 A069888))
(define A082332 (compose-funs A069888 A057163))

(define A082333 (compose-funs A057163 A082313))
(define A082334 (compose-funs A082313 A057163))

(define A082335 (catfun1 *A082335!))
(define A082336 (catfun1 *A082336!))

;; Checked upto n=12000:
(define A069773v2? (compose-funs A057163 A057501 A057163)) ;; A069787 o A069774 o A069787
(define A069774v2? (compose-funs A057163 A057502 A057163)) ;; A069787 o A069773 o A069787

;; (define A082337 (catfun1 *A082337!)) duplicate of A069775, obsolete!
;; (define A082338 (catfun1 *A082338!)) duplicate of A069776, obsolete!

(define A082339 (catfun1 *A082339!))
(define A082340 (catfun1 *A082340!))

(define A082341 (catfun1 *A082341!))
(define A082342 (catfun1 *A082342!))


(define A082345 (catfun1 *A082345!))
(define A082346 (catfun1 *A082346!))

(define A082347 (catfun1 *A082347!))
(define A082348 (catfun1 *A082348!))

(define A082349 (catfun1 *A082349!))
(define A082350 (catfun1 *A082350!))

(define A082351 (catfun1 *A082351!))
(define A082352 (catfun1 *A082352!))
(define A082353 (catfun1 *A082353!))
(define A082354 (catfun1 *A082354!))

(define A082355 (catfun1 *A082355!))
(define A082356 (catfun1 *A082356!))

(define A082357 (catfun1 *A082357!))
(define A082358 (catfun1 *A082358v2!)) ;; Faster than *A082358!

(define A082359 (catfun1 *A082359!))
(define A082360 (catfun1 *A082360!))

(define A082361 (catfun0 *A082351!))
(define A082362 (catfun0 *A082352!))
(define A082363 (catfun0 *A082355!))
(define A082364 (catfun0 *A082356!))


;; (output-check-html "C:/matikka/nekomorphisms/a82313-64.htm" check-these-82313- 100 45)

(define check-these-82313-
 (list
       (list 100 0 57502 A057502 A057501 (list A069888 A082313))
       (list 100 0 82313 A082313 A082313 (list A057501 A057164 A057502))
       (list 100 0 82314 A082314 A082314 (list A057502 A057164 A057501))
       (list 100 0 82315 A082315 A082316 (list A057501 A057501) (list A082313 A057164))
       (list 100 0 82316 A082316 A082315 (list A057502 A057502) (list A057164 A082313))
       (list 100 0 82317 A082317 A082318 (list A057501 A082315))
       (list 100 0 82318 A082318 A082317 (list A057502 A082316))
       (list 100 0 82319 A082319 A082320 (list A082315 A082315) (list A057501 A082317))
       (list 100 0 82320 A082320 A082319 (list A082316 A082316) (list A057502 A082318))
       (list 100 0 82321 A082321 A082322 (list A057501 A082319))
       (list 100 0 82322 A082322 A082321 (list A057502 A082320))
       (list 100 0 82323 A082323 A082324 (list A057501 A082321) (list A082317 A082317) (list A082315 A082315 A082315))
       (list 100 0 82324 A082324 A082323 (list A057502 A082322) (list A082318 A082318) (list A082316 A082316 A082316))

       (list 100 0 82325 A082325 A082326 (list A057163 A057511 A057163) (list A069787 A082326 A069787))
       (list 100 0 82326 A082326 A082325 (list A057163 A057512 A057163) (list A069787 A082325 A069787))

       (list 100 0 82327 A082327 A082328 (list A072798 A082328 A072798) (list A082853 A082325 A081291))

       (list 100 0 82328 A082328 A082327 (list A072798 A082327 A072798) (list A082853 A082326 A081291))

       (list 100 0 82329 A082329 A082330 (list A082854 A082325 A081291))
       (list 100 0 82330 A082330 A082329 (list A082854 A082326 A081291))

       (list 100 0 82331 A082331 A082332 (list A057163 A069888))
       (list 100 0 82332 A082332 A082331 (list A069888 A057163))

       (list 100 0 82333 A082333 A082334 (list A057163 A082313))
       (list 100 0 82334 A082334 A082333 (list A082313 A057163))

       (list 100 0 82335 A082335 A082336) ;; Cf. A074679
       (list 100 0 82336 A082336 A082335) ;; Cf. A074680

       (list 100 0 82339 A082339 A082340)
       (list 100 0 82340 A082340 A082339)

       (list 100 0 82341 A082341 A082342 (list A057163 A073285 A057163)) ;; Differs from A082326 at term n=39.
       (list 100 0 82342 A082342 A082341 (list A057163 A073284 A057163)) ;; Differs from A082325 at term n=39.

       (list 100 0 82345 A082345 A082346)
       (list 100 0 82346 A082346 A082345)

       (list 100 0 82347 A082347 A082348)
       (list 100 0 82348 A082348 A082347)

       (list 100 0 82349 A082349 A082350)
       (list 100 0 82350 A082350 A082349)

       (list 100 0 82351 A082351 A082352 (list A057163 A082353 A057163))
       (list 100 0 82352 A082352 A082351 (list A057163 A082354 A057163))

       (list 100 0 82353 A082353 A082354 (list A057163 A082351 A057163))
       (list 100 0 82354 A082354 A082353 (list A057163 A082352 A057163))

       (list 100 0 82355 A082355 A082356 (list A082357 A057163))
       (list 100 0 82356 A082356 A082355 (list A057163 A082358))

       (list 100 0 82357 A082357 A082358 (list A082355 A057163))
       (list 100 0 82358 A082358 A082357 (list A057163 A082356))

       (list 100 0 82359 A082359 A082360 (list A074683 A057163))
       (list 100 0 82360 A082360 A082359 (list A057163 A074684))

       (list 100 0 82361 A082361 A082362 (list A082853 A082351 A081291))
       (list 100 0 82362 A082362 A082361 (list A082853 A082352 A081291))

       (list 100 0 82363 A082363 A082364 (list A082853 A082355 A081291))
       (list 100 0 82364 A082364 A082363 (list A082853 A082356 A081291))

 )
)

;; (output-check-html2 "C:/matikka/nekomorphisms/a82852-.htm" check-these-82852- 100 25)

(define check-these-82852-
 (list
       (list 120 0 82852 A082852)
       (list 120 0 82853 A082853)
       (list 120 0 82854 A082854)
       (list 120 0 82855 A082855)
 )
)


(define (A083923 n) (if (= 1 (A057515 n)) 1 0)) ;; Characteristic function for A057548.
(define (A083923v3 n) (if (zero? (A072772 n)) 1 0))

;; Char func. for A072795:
(define (A083924 n) (cond ((zero? n) 0) ((zero? (A072771 n)) 1) (else 0)))
(define (A083924v3 n) (cond ((zero? n) 0) ((zero? (A057515 (A072771 n))) 1) (else 0)))
(define A083923v2 (compose-funs A083924 A069770))
(define A083924v2 (compose-funs A083923 A069770))
(define (A083925 n) (* (A083923 n) (A072771 n))) ;; Inverse fun for A057548.
(define (A083926 n) (* (A083924 n) (A072772 n))) ;; Inverse fun for A072795.
(define A083927 (catfun1 Tree2BinTree_if_possible)) ;; Inverse for A057123.
(define A083928 (catfun1 ZigzagTree2BinTree_if_possible)) ;; Inverse for A080298

(define A069775v2? (compose-funs A083927 A069775 A057123))
(define A069776v2? (compose-funs A083927 A069776 A057123)) 

(define A083929 (compose-funs A083927 A083925)) ;; Inverse for the next.
(define A083930 (compose-funs A057548 A057123)) ;; From gen-trees to tpt.

(define A083931 (compose-funs A000695 A014486))
(define (A083932 n) (* 3 (A083931 n))) ;; A014486 -> double-width. -> A080316
(define A083933 (compose-funs A007088 A083932)) ;; Same sequence in binary. -> A080317
(define A083934 (compose-funs A080300 A083932)) ;; A014486-indices.

(define (A083935 n) ;; Inverse for above.
   (let* ((b (A014486 n))
          (c (if (zero? (modulo b 3)) (A059905 (/ b 3)) 0))
         )
    (if (zero? (A059906 (/ b 3)))
        (A080300 c)
        0
    )
   )
)


(define A083936 (compose-funs A014486 A083930))

(define A083937 (compose-funs A014486 A072795))

;; (define A083938 (catfun1 (lambda (x) (cons x x))))
;; After the first term, diagonal of A072764 (and A072766 also).
(define (A083938 n)
   (cond ((zero? n) 0)
         (else (A080300
                  (parenthesization->A014486
                    ((lambda (s) (cons s s))
                       (A014486->parenthesization (A014486 (-1+ n)))
                    )
                  )
               )
         )
   )
)

(define (A083938v2 n) (if (zero? n) 0 (A072764bi (-1+ n) (-1+ n))))
(define A083939 (compose-funs A014486 A083938))

(define (A083940 n)
   (cond ((zero? n) 0)
         (else (A080300
                  (parenthesization->A014486
                    ((lambda (s) (cons s (*A057163 s)))
                       (A014486->parenthesization (A014486 (-1+ n)))
                    )
                  )
               )
         )
   )
)

(define (A083940v2 n) (if (zero? n) 0 (A072764bi (-1+ n) (A057163 (-1+ n)))))

(define A083941 (compose-funs A014486 A083940))

(define A083942 (compose-funs A080300 A002542))

(define A084107 (compose-funs parenthesization->A014486 nth-complete-binary-tree))
(define A084108 (compose-funs A080300 A084107))
(define A084108v2 (compose-funs A057117 A083942)) ;; Don't use!
;; Also iterates of A080298:
(definec (A084108v3 n) (if (zero? n) 0 (A080298 (A084108v3 (-1+ n)))))

(define A083942v2 (compose-funs A057118 A084108))

(define A0002542v2 (compose-funs A014486 A057118 A084108))

;; Note:
;; (map A083938 (iota0 9))
;; = (keep-matching-items (iota0 2055) (lambda (x) (= x (A069770 x))))
;; --> (0 1 6 42 52 385 414 477 506 555)
;;
;; (map A083940 (iota0 9))
;; = (keep-matching-items (iota0 2055) (lambda (x) (= x (A057163 x))))
;; --> (0 1 6 43 51 389 416 477 504 551)
;;
;; (map A084108 (iota0 3))
;; = (keep-matching-items (iota0 625) (lambda (x) (= x (A069767 x))))
;; = (keep-matching-items (iota0 625) (lambda (x) (= x (A069768 x))))
;; --> (0 1 6 477)


;; Checked upto 23714, they match:
(define A071661v2? (compose-funs A083929 A071663 A083930))
(define A071662v2? (compose-funs A083929 A071664 A083930))




;; (define A057164v2 (compose-funs A080300 A036044 A014486)) ;; Fast versions
;; (define A057164v3 (compose-funs A080300 A056539 A014486)) ;; working on binary exp.

(define A057163v2 (compose-funs A083927 A057164v2 A057123))
(define A057117v2 (compose-funs A083927 A072088 A057123))
(define A057118v2 (compose-funs A083927 A072089 A057123))

(define A069770v2 (compose-funs A083927 A072796 A057123)) ;; Obvious.
(define A072797v2 (compose-funs A083927 A072797 A057123))

(define A057163v3 (compose-funs A083928 A057163 A080298))

;; (output-check-html "C:/matikka/nekomorphisms/seqs/a83923-84108.htm" check-A083923- 119 45 8)
(define check-A083923-
 (list
       (list 100 0 83923 A083923 #f (list A083924 A069770))
       (list 100 0 83924 A083924 #f (list A083923 A069770))

       (list 100 0 83925 A083925 A057548)
       (list 100 0 83926 A083926 A072795)

       (list 100 0 83927 A083927 A057123)
       (list 100 0 83928 A083928 A080298)
       (list 100 0 83929 A083929 A083930 (list A083927 A083925))

       (list 100 0 83930 A083930 #f (list A057548 A057123))

       (list 100 0 83931 A083931 #f (list A000695 A014486))
       (list 100 0 83932 A083932 #f (list A008585 A083931))
       (list 100 0 83933 A083933 #f (list A007088 A083932))
       (list 100 0 83934 A083934 #f (list A080300 A083932))

       (list 100 0 83935 A083935 A083934)
       (list 100 0 83936 A083936)
       (list 100 0 83937 A083937)

       (list 100 0 83938 A083938)
       (list 100 0 83939 A083939)

       (list 100 0 83940 A083940)
       (list 100 0 83941 A083941)

       (list   7 0 83942 A083942)

       (list   7 0 84107 A084107)
       (list   7 0 84108 A084108)

 )
)


(define A085159  (catfun1 *A085159))
(define A085159! (catfun1 *A085159!))
(define A085161  (catfun1 *A085161))
(define A085161! (catfun1 *A085161!))

(define A085160  (compose-funs A085161 A085159 A085161))
(define A085160! (compose-funs A085161! A085159! A085161!))
(define A085162 (compose-funs A057163! A085161! A057163!)) ;; A057163-conjugate
(define A085163 (catfun1 *A085163!))

(define A085161!v2 (compose-funs A085163 A057508))
(define A085163!v2 (compose-funs A085161! A057508))
(define A085164 (compose-funs A057508 A085161!)) ;; Inverse of A085163.

(define A085161v2 (compose-funs A074684 A057164 A074683)) ;; Should be?

;; A057163-conjugates of the qq-rr rotations: (cf. A086429 & 86430)
(define A085165 (compose-funs A057163! A085159! A057163!))
(define A085166 (compose-funs A057163! A085160! A057163!))

(define A085167 (catfun1 *A085167!))
(define A085167!v2 (compose-funs A085159! A069770!))
(define A085168 (compose-funs A069770! A085160!)) ;; Inverse of A085167

(define A085169 (catfun1 *A085169))
(define A085169! (catfun1 *A085169!))
(define A085169!v2 (catfun1 *A085169v2!))
(define A085170! (catfun1 *A085170!))
(define A085170 A085170!)

(define A085159!v2 (compose-funs A085169! A082315 A085170!))
(define A085160!v2 (compose-funs A085169! A082316 A085170!))
(define A085159v3 (compose-funs A074684 A082315 A074683)) ;; Firmly in A073200 !
(define A085160v3 (compose-funs A074684 A082316 A074683))

(define A074684v2 (compose-funs A083925 A085169 A057548))

(define A085171 (catfun1 *A085171!))
;; Define the inverse for above in dummy way, before we realize how the
;; S-expressions should be manipulated:
(definec (A085172 n) (let loop ((i 0)) (cond ((= n (A085171 i)) i) (else (loop (1+ i))))))

(define A085173 (compose-funs A074684 A057501 A074683)) ;; "Half-rotation" of (qq)-(rr)
(define A085174 (compose-funs A074684 A057502 A074683))
(define A085159v4 (compose-funs A085173 A085173)) ;; Half+half = whole
(define A085160v4 (compose-funs A085174 A085174))

;; Left-over, an involution:
(define A085175 (compose-funs A085169! A057164 A085170!)) ;; Why not A085161 ?

;; 86425 --- 86434
(define A086425 (compose-funs A057164 A074684)) ;; Design conjugators.
(define A086426 (compose-funs A074683 A057164))

(define A086427 (compose-funs A057164 A085173 A057164)) ;; Half-steps
(define A086427v2 (compose-funs A086425 A057501 A086426))
(define A086428 (compose-funs A057164 A085174 A057164))
(define A086428v2 (compose-funs A086425 A057502 A086426))

(define A086429 (compose-funs A057164 A085159 A057164)) ;; Whole-steps
(define A086429v2 (compose-funs A086425 A082315 A086426))
(define A086429v3 (compose-funs A086427 A086427))
(define A086430 (compose-funs A057164 A085160 A057164))
(define A086430v2 (compose-funs A086425 A082316 A086426))
(define A086430v3 (compose-funs A086428 A086428))

(define A086431 (compose-funs A057164 A085161 A057164)) ;; Reflection of (pp)-(rr)
(define A086431v2 (compose-funs A086425 A057164 A086426)) ;; Reflection of (pp)-(rr)

;; A086432 ?

(define A086433 (compose-funs A082853 A085169 A081291)) ;; Contractions.
(define A086434 (compose-funs A082853 A085170 A081291))


;; (output-check-html "C:/matikka/nekomorphisms/seqs/a85159-.htm" check-A085159- 119 45 #f)
(define check-A085159-
 (list
       (list 119 0 85159 A085159 A085160)
       (list 119 0 85160 A085160 A085159)

       (list 119 0 85161 A085161 A085161)
       (list 119 0 85162 A085162 A085162)

       (list 119 0 85163 A085163 A085164)
       (list 119 0 85164 A085164 A085163)

       (list 119 0 85165 A085165 A085166)
       (list 119 0 85166 A085166 A085165)

       (list 119 0 85167 A085167 A085168)
       (list 119 0 85168 A085168 A085167)

       (list 119 0 85169 A085169 A085170)
       (list 119 0 85170 A085170 A085169)

       (list 119 0 85171 A085171 A085172)
       (list 119 0 85172 A085172 A085171)

       (list 119 0 85173 A085173 A085174)
       (list 119 0 85174 A085174 A085173)

       (list 119 0 85175 A085175 A085175)

       (list 119 0 86425 A086425 A086426)
       (list 119 0 86426 A086426 A086425)

       (list 119 0 86427 A086427 A086428)
       (list 119 0 86428 A086428 A086427)

       (list 119 0 86429 A086429 A086430)
       (list 119 0 86430 A086430 A086429)

       (list 119 0 86431 A086431 A086431)

;;     (list 119 0 86432 A086432 A086432)

       (list 119 0 86433 A086433 A086434)
       (list 119 0 86434 A086434 A086433)
 )
)

;; Here are the 5 A-numbers you requested: A120705 --- A120709.

(define A120705 (catfun1 *A120705!))
(define A120706 (catfun1 *A120706!))
(define A120707 (cc-Afun A120705))
(define A120708 (mc-Afun A120705))
(define A120709 (lc-Afun A120705))

;; (output-check-html "C:/karttu/nekomorphisms/seqs/a120705-9.htm" check-A120705-9 119 45 #f)
(define check-A120705-9
 (list
       (list 119 0 120705 A120705 A120706)
       (list 119 0 120706 A120706 A120705)
       (list 9 0 120707 A120707)
       (list 9 0 120708 A120708)
       (list 9 0 120709 A120709)
 )
)


(definec (?*A089840 n) (catfun1 (**A089840 n)))
(define (A089840 n) ((?*A089840 (A002262 n)) (A025581 n)))

(definec (?*A122200 n) (catfun1 (**A122200 n)))
(define (A122200 n) ((?*A122200 (A002262 n)) (A025581 n)))

(definec (?*A122201 n) (catfun1 (**A122201 n)))
(define (A122201 n) ((?*A122201 (A002262 n)) (A025581 n)))
(definec (?*A122202 n) (catfun1 (**A122202 n)))
(define (A122202 n) ((?*A122202 (A002262 n)) (A025581 n)))
(definec (?*A122203 n) (catfun1 (**A122203 n)))
(define (A122203 n) ((?*A122203 (A002262 n)) (A025581 n)))
(definec (?*A122204 n) (catfun1 (**A122204 n)))
(define (A122204 n) ((?*A122204 (A002262 n)) (A025581 n)))

(definec (?*A122283 n) (catfun1 (**A122283 n)))
(define (A122283 n) ((?*A122283 (A002262 n)) (A025581 n)))
(definec (?*A122284 n) (catfun1 (**A122284 n)))
(define (A122284 n) ((?*A122284 (A002262 n)) (A025581 n)))

(definec (?*A122285 n) (catfun1 (**A122285 n)))
(define (A122285 n) ((?*A122285 (A002262 n)) (A025581 n)))
(definec (?*A122286 n) (catfun1 (**A122286 n)))
(define (A122286 n) ((?*A122286 (A002262 n)) (A025581 n)))

(definec (?*A122287 n) (catfun1 (**A122287 n)))
(define (A122287 n) ((?*A122287 (A002262 n)) (A025581 n)))
(definec (?*A122288 n) (catfun1 (**A122288 n)))
(define (A122288 n) ((?*A122288 (A002262 n)) (A025581 n)))

(definec (?*A122289 n) (catfun1 (**A122289 n)))
(define (A122289 n) ((?*A122289 (A002262 n)) (A025581 n)))
(definec (?*A122290 n) (catfun1 (**A122290 n)))
(define (A122290 n) ((?*A122290 (A002262 n)) (A025581 n)))

(definec (?*A130400 n) (catfun1 (**A130400 n)))
(define (A130400 n) ((?*A130400 (A002262 n)) (A025581 n)))

(definec (?*A130401 n) (catfun1 (**A130401 n)))
(define (A130401 n) ((?*A130401 (A002262 n)) (A025581 n)))

(definec (?*A130402 n) (catfun1 (**A130402 n)))
(define (A130402 n) ((?*A130402 (A002262 n)) (A025581 n)))

(definec (?*A130403 n) (catfun1 (**A130403 n)))
(define (A130403 n) ((?*A130403 (A002262 n)) (A025581 n)))

(define A122282 (catfun1 *A122282!))

(define A122291 (catfun1 *A122291!))
(define A122292 (catfun1 *A122292!))
(define A122293 (catfun1 *A122293!))
(define A122294 (catfun1 *A122294!))
(define A122295 (catfun1 *A122295!))
(define A122296 (catfun1 *A122296!))
(define A122297 (catfun1 *A122297!))
(define A122298 (catfun1 *A122298!))

(define A122300 (catfun1 *A122300!))
(define A122301 (catfun1 *A122301uus!))
(define A122302 (catfun1 *A122302uus!))
(define A122303 (catfun1 *A122303!))
(define A122304 (catfun1 *A122304!))
(define A122305 (catfun1 *A122305!))
(define A122306 (catfun1 *A122306!))
(define A122307 (catfun1 *A122307!))
(define A122308 (catfun1 *A122308!))
(define A122309 (catfun1 *A122309!))
(define A122310 (catfun1 *A122310!))
(define A122311 (catfun1 *A122311!))
(define A122312 (catfun1 *A122312!))
(define A122313 (catfun1 *A122313!))
(define A122314 (catfun1 *A122314!))
(define A122315 (catfun1 *A122315!))
(define A122316 (catfun1 *A122316!))
(define A122317 (catfun1 *A122317!))
(define A122318 (catfun1 *A122318!))
(define A122319 (catfun1 *A122319!))
(define A122320 (catfun1 *A122320!))
(define A122321 (catfun1 *A122321!))
(define A122322 (catfun1 *A122322!))
(define A122323 (catfun1 *A122323!))
(define A122324 (catfun1 *A122324!))
(define A122325 (catfun1 *A122325!))
(define A122326 (catfun1 *A122326!))
(define A122327 (catfun1 *A122327!))
(define A122328 (catfun1 *A122328!))
(define A122329 (catfun1 *A122329!))
(define A122330 (catfun1 *A122330!))
(define A122331 (catfun1 *A122331!))
(define A122332 (catfun1 *A122332!))
(define A122333 (catfun1 *A122333!))
(define A122334 (catfun1 *A122334!))
(define A122335 (catfun1 *A122335!))
(define A122336 (catfun1 *A122336!))
(define A122337 (catfun1 *A122337!))
(define A122338 (catfun1 *A122338!))
(define A122339 (catfun1 *A122339!))
(define A122340 (catfun1 *A122340!))
(define A122341 (catfun1 *A122341!))
(define A122342 (catfun1 *A122342!))
(define A122343 (catfun1 *A122343!))
(define A122344 (catfun1 *A122344!))
(define A122345 (catfun1 *A122345!))
(define A122346 (catfun1 *A122346!))
(define A122347 (catfun1 *A122347!))
(define A122348 (catfun1 *A122348!))
(define A122349 (catfun1 *A122349!))
(define A122350 (catfun1 *A122350!))
(define A122351 (catfun1 *A122351!))

(define A122353 (catfun1 *A122353uus!))
(define A122354 (catfun1 *A122354!))
(define A122355 (catfun1 *A122355!))
(define A122356 (catfun1 *A122356!))
(define A122357 (catfun1 *A122357!))
(define A122358 (catfun1 *A122358!))
(define A122359 (catfun1 *A122359!))

(define A122360 (catfun1 *A122360!))
(define A122361 (catfun1 *A122361!))
(define A122362 (catfun1 *A122362!))
(define A122363 (catfun1 *A122363!))
(define A122364 (catfun1 *A122364!))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A153246 --- A153250.

(define (A153250bi x y) (A080300 (parenthesization->A014486 (bud! (A014486->parenthesization (A014486 x)) y))))

  	
(define (A153250 n) (A153250bi (A002262 n) (A025581 n))) ;; Zero-based

;; Row n contains A072643(n)+1 non-zero terms.

;; I'm lazy now.
(define (A153250nth-row-terms n)
   (collect-intfun-values-to-list (lambda (i) (A153250bi n i)) 0 (A072643 n))
)

(define (A153250-up-to-row n)
   (fold-right append! '()
           (collect-intfun-values-to-list A153250nth-row-terms 0 n)
   )
)

(define A153249seq (A153250-up-to-row 35))

(define (A153249 n) (list-ref A153249seq n)) ;; "tabf" Row n contains A072643(n)+1 terms.

(define (A153246 n) (count-fleeing-trees n A057164))
(define (A153247 n) (count-fleeing-trees n A123493))
(define (A153248 n) (count-fleeing-trees n A123494))

;; (map (lambda (n) (count-fleeing-trees n A127387)) (iota0 201))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (load "/home/karttu/Schemuli/GF2Xfuns.com")
;; and say: (output-entries-to-file120_45 seqs-derived-gms "./seqs/A122282-seqs.txt" "Sep 01 2006")

(define seqs-derived-gms
 (list 
  (list 122282 "Row 1 of A122200, row 7 of A122203 and A122204. An involution of non-negative integers."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the first non-recursive gatomorphism *A069770 with the recursion schema RIBS (see A122200), or alternatively, derived from"
" the seventh non-recursive gatomorphism *A089854 with recursion scheme SPINE or ENIPS. (c.f. A122203, A122204 for their definitions).")
        '(y: "The number of fixed points in range [A014137(n-1)..A014138(n-1)] of this permutation is given by INVERT transform of \"aerated\" Catalans [1,1,0,1,0,2,0,5,0,14,0,42,...].")
        '(indentries: Catsigperm)
  )

  (list 122351 "Row 1 of A122289 and A122290. An involution of non-negative integers."
        '(off: 0)
        (list 'c: (string-append "The signature-permutation of the gatomorphism which is derived from the gatomorphism *A057163"
" with the recursion schema FORK (see A122201),"
" that is, from the first non-recursive gatomorphism *A069770 with"
" FORK(FORK(*A069770)) or equivalently,"
" with KROF(KROF(*A069770)) (see A122202)."
            )
        )
        '(y: "A007595 gives the number of orbits in range [A014137(n-1)..A014138(n-1)] of this permutation.") ;; Weird, prove!
        '(indentries: Catsigperm)
  )

  (list 122363 "Row 2 of A122289."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the second non-recursive gatomorphism *A072796 with FORK(FORK(*A072796)) = FORK(*A057511). (c.f. A122201 for the definition of FORK).")
        '(indentries: Catsigperm)
        '(inv: 122364)
  )

  (list 122364 "Row 2 of A122290."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the second non-recursive gatomorphism *A072796 with KROF(KROF(*A072796)) = KROF(*A057512). (c.f. A122202 for the definition of KROF).")
        '(indentries: Catsigperm)
        '(inv: 122363)
  )


  (list 122300 "Row 2 of A122283 and A122284. An involution of non-negative integers."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the second non-recursive gatomorphism *A072796 either with recursion schema DEEPEN or NEPEED. (c.f. A122283, A122284 for their definitions).")
        '(y: "A057163(n) = A083927(A122300(A057123(n))).")
        '(indentries: Catsigperm)
  )

  (list 122301 "Row 1 of A122283, row 21 of A122201."
        '(off: 0)
        (list 'c: (string-append
  "The signature-permutation of the gatomorphism which is derived"
  " from the first non-recursive gatomorphism *A069770 with recursion schema DEEPEN (c.f. A122283 for the definition),"
  " or equivalently, derived from the 21st non-recursive gatomorphism"
  " *A089863 with recursion schema FORK (c.f. A122201 for the definition)."
                  )
        )
        '(indentries: Catsigperm)
        '(inv: 122302)
  )

  (list 122302 "Row 1 of A122284, row 15 of A122202."
        '(off: 0)
        (list 'c: (string-append
  "The signature-permutation of the gatomorphism which is derived"
  " from the first non-recursive gatomorphism *A069770 with recursion schema NEPEED (c.f. A122284 for the definition),"
  " or equivalently, derived from the fifteenth non-recursive gatomorphism"
  " *A089859 with recursion schema KROF (c.f. A122202 for the definition)."
                  )
        )
        '(indentries: Catsigperm)
        '(inv: 122301)
  )

  (list 122303 "Row 3 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the third non-recursive gatomorphism *A089850 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122304)
  )

  (list 122304 "Row 3 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the third non-recursive gatomorphism *A089850 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122303)
  )


  (list 122305 "Row 4 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the fourth non-recursive gatomorphism *A089851 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122306)
  )

  (list 122306 "Row 6 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the sixth non-recursive gatomorphism *A089853 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122305)
  )


  (list 122307 "Row 5 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the fifth non-recursive gatomorphism *A089852 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122308)
  )

  (list 122308 "Row 5 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the fifth non-recursive gatomorphism *A089852 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122307)
  )

  (list 122309 "Row 6 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the sixth non-recursive gatomorphism *A089853 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122310)
  )

  (list 122310 "Row 4 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the fourth non-recursive gatomorphism *A089851 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122309)
  )


  (list 122311 "Row 7 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the seventh non-recursive gatomorphism *A089854 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122312)
        '(y: "Differs from A073287 for the first time at n=35, where a(n) = 36, while A073287(n) = 35.")
  )

  (list 122312 "Row 7 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the seventh non-recursive gatomorphism *A089854 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122311)
        '(y: "Differs from A073286 for the first time at n=35, where a(n) = 36, while A073286(n) = 35.")
  )

  (list 122313 "Row 8 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the eighth non-recursive gatomorphism *A072797 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122314)
        '(y: "A082325(n) = A083927(A122313(A057123(n))). Differs from A069775 for the first time at n=34, where a(n) = 35, while A069775(n) = 34.")
  )

  (list 122314 "Row 8 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the eighth non-recursive gatomorphism *A072797 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122313)
        '(y: "A082326(n) = A083927(A122314(A057123(n))). Differs from A069776 for the first time at n=34, where a(n) = 35, while A069776(n) = 34.")
  )

  (list 122315 "Row 9 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the ninth non-recursive gatomorphism *A089855 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122316)
  )

  (list 122316 "Row 11 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the eleventh non-recursive gatomorphism *A089857 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122315)
  )

  (list 122317 "Row 10 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the tenth non-recursive gatomorphism *A089856 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122318)
  )

  (list 122318 "Row 10 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the tenth non-recursive gatomorphism *A089856 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122317)
  )

  (list 122319 "Row 11 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the eleventh non-recursive gatomorphism *A089857 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122320)
  )

  (list 122320 "Row 9 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the ninth non-recursive gatomorphism *A089855 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122319)
  )

  (list 122321 "Row 12 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the twelfth non-recursive gatomorphism *A074679 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122322)
  )

  (list 122322 "Row 17 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the seventeenth non-recursive gatomorphism *A074680 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122321)
  )

  (list 122323 "Row 13 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the thirteenth non-recursive gatomorphism *A089858 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122324)
  )

  (list 122324 "Row 18 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the eighteenth non-recursive gatomorphism *A089861 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122323)
  )

  (list 122325 "Row 14 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the fourteenth non-recursive gatomorphism *A073269 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122326)
  )

  (list 122326 "Row 19 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the nineteenth non-recursive gatomorphism *A073270 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122325)
  )

  (list 122327 "Row 15 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the fifteenth non-recursive gatomorphism *A089859 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122328)
  )

  (list 122328 "Row 21 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the 21st non-recursive gatomorphism *A089863 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122327)
  )

  (list 122329 "Row 16 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the sixteenth non-recursive gatomorphism *A089860 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122330)
  )

  (list 122330 "Row 20 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the twentieth non-recursive gatomorphism *A089862 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122329)
  )

  (list 122331 "Row 17 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the seventeenth non-recursive gatomorphism *A074680 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122332)
  )

  (list 122332 "Row 12 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the twelfth non-recursive gatomorphism *A074679 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122331)
  )

  (list 122333 "Row 18 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the eighteenth non-recursive gatomorphism *A089861 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122334)
  )

  (list 122334 "Row 13 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the thirteenth non-recursive gatomorphism *A089858 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122333)
  )

  (list 122335 "Row 19 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the nineteenth non-recursive gatomorphism *A073270 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122336)
  )

  (list 122336 "Row 14 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the fourteenth non-recursive gatomorphism *A073269 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122335)
  )

  (list 122337 "Row 20 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the twentieth non-recursive gatomorphism *A089862 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122338)
  )

  (list 122338 "Row 16 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the sixteenth non-recursive gatomorphism *A089860 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122337)
  )

  (list 122339 "Row 21 of A122283."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the 21st non-recursive gatomorphism *A089863 with recursion schema DEEPEN (c.f. A122283 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122340)
  )

  (list 122340 "Row 15 of A122284."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the fifteenth non-recursive gatomorphism *A089859 with recursion schema NEPEED (c.f. A122284 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122339)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (list 122341 "Row 3 of A122201."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the third non-recursive gatomorphism *A089850 with recursion schema FORK (c.f. A122201 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122342)
  )

  (list 122342 "Row 3 of A122202."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the third non-recursive gatomorphism *A089850 with recursion schema KROF (c.f. A122202 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122341)
  )

  (list 122343 "Row 4 of A122201."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the fourth non-recursive gatomorphism *A089851 with recursion schema FORK (c.f. A122201 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122344)
  )

  (list 122344 "Row 6 of A122202."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the sixth non-recursive gatomorphism *A089853 with recursion schema KROF (c.f. A122202 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122343)
  )

  (list 122345 "Row 5 of A122201."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the fifth non-recursive gatomorphism *A089852 with recursion schema FORK (c.f. A122201 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122346)
  )

  (list 122346 "Row 5 of A122202."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the fifth non-recursive gatomorphism *A089852 with recursion schema KROF (c.f. A122202 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122345)
  )

  (list 122347 "Row 6 of A122201."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the sixth non-recursive gatomorphism *A089853 with recursion schema FORK (c.f. A122201 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122348)
  )

  (list 122348 "Row 4 of A122202."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the fourth non-recursive gatomorphism *A089851 with recursion schema KROF (c.f. A122202 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122347)
  )

  (list 122349 "Row 7 of A122201."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the seventh non-recursive gatomorphism *A089854 with recursion schema FORK (c.f. A122201 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122350)
        '(y: "Differs from A073289 for the first time at n=63, where a(n) = 50, while A073289(n) = 49.")
  )

  (list 122350 "Row 7 of A122202."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the seventh non-recursive gatomorphism *A089854 with recursion schema KROF (c.f. A122202 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122349)
        '(y: "Differs from A073288 for the first time at n=49, where a(n) = 64, while A073288(n) = 63.")
  )

  (list 122291 "Row 10 of A122201."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the tenth non-recursive gatomorphism *A089856 with recursion schema FORK (c.f. A122201 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122292)
  )

  (list 122292 "Row 10 of A122202."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the tenth non-recursive gatomorphism *A089856 with recursion schema KROF (c.f. A122202 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122291)
  )


  (list 122293 "Row 11 of A122201."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the eleventh non-recursive gatomorphism *A089857 with recursion schema FORK (c.f. A122201 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122294)
  )

  (list 122294 "Row 9 of A122202."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the ninth non-recursive gatomorphism *A089855 with recursion schema KROF (c.f. A122202 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122293)
  )

  (list 122295 "Row 13 of A122201."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the thirteenth non-recursive gatomorphism *A089858 with recursion schema FORK (c.f. A122201 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122296)
  )

  (list 122296 "Row 18 of A122202."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the eighteenth non-recursive gatomorphism *A089861 with recursion schema KROF (c.f. A122202 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122295)
  )

  (list 122297 "Row 14 of A122201."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the fourteenth non-recursive gatomorphism *A073269 with recursion schema FORK (c.f. A122201 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122298)
  )

  (list 122298 "Row 19 of A122202."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the nineteenth non-recursive gatomorphism *A073270 with recursion schema KROF (c.f. A122202 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122297)
  )


  (list 122353 "Row 15 of A122201."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the fifteenth non-recursive gatomorphism *A089859 with recursion schema FORK (c.f. A122201 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122354)
  )

  (list 122354 "Row 21 of A122202."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the 21st non-recursive gatomorphism *A089863 with recursion schema KROF (c.f. A122202 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122353)
  )


  (list 122355 "Row 16 of A122201."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the sixteenth non-recursive gatomorphism *A089860 with recursion schema FORK (c.f. A122201 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122356)
  )

  (list 122356 "Row 20 of A122202."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the twentieth non-recursive gatomorphism *A089862 with recursion schema KROF (c.f. A122202 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122355)
  )

  (list 122357 "Row 18 of A122201."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the eighteenth non-recursive gatomorphism *A089861 with recursion schema FORK (c.f. A122201 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122358)
  )

  (list 122358 "Row 13 of A122202."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the thirteenth non-recursive gatomorphism *A089858 with recursion schema KROF (c.f. A122202 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122357)
  )

  (list 122359 "Row 19 of A122201."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the nineteenth non-recursive gatomorphism *A073270 with recursion schema FORK (c.f. A122201 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122360)
  )

  (list 122360 "Row 14 of A122202."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the fourteenth non-recursive gatomorphism *A073269 with recursion schema KROF (c.f. A122202 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122359)
  )

  (list 122361 "Row 20 of A122201."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the twentieth non-recursive gatomorphism *A089862 with recursion schema FORK (c.f. A122201 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122362)
  )

  (list 122362 "Row 16 of A122202."
        '(off: 0)
        '(c: "The signature-permutation of the gatomorphism which is derived from the sixteenth non-recursive gatomorphism *A089860 with recursion schema KROF (c.f. A122202 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 122361)
  )


 )
)


(define check-A122200-
 (list
       (list 119 0 122282 A122282 A122282) ;; A122200[1] = A122203[7] = A122204[7]

       (list 119 0 122351 A122351 A122351) ;; A122289[1] = A122290[1]

       (list 119 0 122363 A122363 A122364) ;; A122289[2]
       (list 119 0 122364 A122364 A122363) ;; A122290[2]

       (list 119 0 122341 A122341 A122342) ;; A122201[3]
       (list 119 0 122342 A122342 A122341) ;; A122202[3]

       (list 119 0 122343 A122343 A122344) ;; A122201[4]
       (list 119 0 122344 A122344 A122343) ;; A122202[6]

       (list 119 0 122345 A122345 A122346) ;; A122201[5]
       (list 119 0 122346 A122346 A122345) ;; A122202[5]

       (list 119 0 122347 A122347 A122348) ;; A122201[6]
       (list 119 0 122348 A122348 A122347) ;; A122202[4]

       (list 119 0 122349 A122349 A122350) ;; A122201[7]
       (list 119 0 122350 A122350 A122349) ;; A122202[7]

       (list 119 0 122291 A122291 A122292) ;; A122201[10]
       (list 119 0 122292 A122292 A122291) ;; A122202[10]

       (list 119 0 122293 A122293 A122294) ;; A122201[11]
       (list 119 0 122294 A122294 A122293) ;; A122202[9]

       (list 119 0 122295 A122295 A122296) ;; A122201[13]
       (list 119 0 122296 A122296 A122295) ;; A122202[18]

       (list 119 0 122297 A122297 A122298) ;; A122201[14]
       (list 119 0 122298 A122298 A122297) ;; A122202[19]

       (list 119 0 122353 A122353 A122354) ;; A122201[15]
       (list 119 0 122354 A122354 A122353) ;; A122202[21]

       (list 119 0 122355 A122355 A122356) ;; A122201[16]
       (list 119 0 122356 A122356 A122355) ;; A122202[20]

       (list 119 0 122357 A122357 A122358) ;; A122201[18]
       (list 119 0 122358 A122358 A122357) ;; A122202[13]

       (list 119 0 122359 A122359 A122360) ;; A122201[19]
       (list 119 0 122360 A122360 A122359) ;; A122202[14]

       (list 119 0 122361 A122361 A122362) ;; A122201[20]
       (list 119 0 122362 A122362 A122361) ;; A122202[16]

;;;;;;;;;;;;;;;

       (list 119 0 122300 A122300 A122300) ;; A122283[2] =  A122284[2]

       (list 119 0 122301 A122301 A122302) ;; A122283[1]
       (list 119 0 122302 A122302 A122301) ;; A122284[1]

       (list 119 0 122303 A122303 A122304) ;; A122283[3]
       (list 119 0 122304 A122304 A122303) ;; A122284[3]

       (list 119 0 122305 A122305 A122306) ;; A122283[4]
       (list 119 0 122306 A122306 A122305) ;; A122284[6]

       (list 119 0 122307 A122307 A122308) ;; A122283[5]
       (list 119 0 122308 A122308 A122307) ;; A122284[5]

       (list 119 0 122309 A122309 A122310) ;; A122283[6]
       (list 119 0 122310 A122310 A122309) ;; A122284[4]

       (list 119 0 122311 A122311 A122312) ;; A122283[7]
       (list 119 0 122312 A122312 A122311) ;; A122284[7]

       (list 119 0 122313 A122313 A122314) ;; A122283[8]
       (list 119 0 122314 A122314 A122313) ;; A122284[8]

       (list 119 0 122315 A122315 A122316) ;; A122283[9]
       (list 119 0 122316 A122316 A122315) ;; A122284[11]

       (list 119 0 122317 A122317 A122318) ;; A122283[10]
       (list 119 0 122318 A122318 A122317) ;; A122284[10]

       (list 119 0 122319 A122319 A122320) ;; A122283[11]
       (list 119 0 122320 A122320 A122319) ;; A122284[9]

       (list 119 0 122321 A122321 A122322) ;; A122283[12]
       (list 119 0 122322 A122322 A122321) ;; A122284[17]

       (list 119 0 122323 A122323 A122324) ;; A122283[13]
       (list 119 0 122324 A122324 A122323) ;; A122284[18]

       (list 119 0 122325 A122325 A122326) ;; A122283[14]
       (list 119 0 122326 A122326 A122325) ;; A122284[19]

       (list 119 0 122327 A122327 A122328) ;; A122283[15]
       (list 119 0 122328 A122328 A122327) ;; A122284[21]

       (list 119 0 122329 A122329 A122330) ;; A122283[16]
       (list 119 0 122330 A122330 A122329) ;; A122284[20]

       (list 119 0 122331 A122331 A122332) ;; A122283[17]
       (list 119 0 122332 A122332 A122331) ;; A122284[12]

       (list 119 0 122333 A122333 A122334) ;; A122283[18]
       (list 119 0 122334 A122334 A122333) ;; A122284[13]

       (list 119 0 122335 A122335 A122336) ;; A122283[19]
       (list 119 0 122336 A122336 A122335) ;; A122284[14]

       (list 119 0 122337 A122337 A122338) ;; A122283[20]
       (list 119 0 122338 A122338 A122337) ;; A122284[16]

       (list 119 0 122339 A122339 A122340) ;; A122283[21]
       (list 119 0 122340 A122340 A122339) ;; A122284[15]

 )
)

;; (output-entries-to-file120_45 tables-A122200- "./seqs/A122200-tables.txt" "Sep 01 2006")
(define tables-A122200-
 (list
  (list 122200 "Signature permutations of RIBS-transformations of non-recursive gatomorphisms in table A089840."
        '(off: 0)
        '(keywords: "tabl")
        '(indentries: Catsigperm)
        (list 'c: (string-append "Row n is the signature permutation of the Catalan automorphism"
" which is obtained from the nth nonrecursive automorphism in the table A089840 with"
" the recursion scheme \"RIBS\". In this recursion scheme the given automorphism is applied"
" to all (toplevel) subtrees of the Catalan structure, when it is interpreted as a general tree."
" Permutations in this table form a countable group, which is isomorphic with the group in A089840."
" (The RIBS transformation gives the group isomorphism, mapping row-to-row in order.)"
" Furthermore, row n of this table is also found as the row b(n) in tables A122203 and A122204,"
" where the sequence b(n) (to be computed, begins as 0,7,...) gives the"
" A089840-index of the non-recursive gatomorphism"
" which is formed from A089840[n] by applying it to the left subtree of a binary tree,"
" and leaving the right-hand side subtree intact."
" (E.g. from A089840[1] = A069770 we get A089840[7] = A089854.)"
" If the count of fixed points of the gatomorphism"
" A089840[n] is given by sequence f, then the count of fixed points of the gatomorphism A089840[b(n)]"
" is given by CONV(f,A000108) (where CONV stands for convolution), and the count of fixed points of"
" the gatomorphism A122200[n] by INVERT(RIGHT(f))."
" The associated Scheme-procedures RIBS and !RIBS can be used"
" to obtain such a transformed gatomorphism from any constructively or destructively implemented"
" gatomorphism."
                  )
        )
        (list 'y: (string-append
      " Row 0 (identity permutation): A001477, row 1: A122282."
      " C.f. also tables A089840, A122201-A122204, A122283-A12284, A122285-A122288, A122289-A122290."
                  )
        )
        '(scheme: "(define (RIBS foo) (lambda (s) (map foo s) s))\n(define (!RIBS foo!) (letrec ((bar! (lambda (s) (cond ((pair? s) (foo! (car s)) (bar! (cdr s)))) s))) bar!))"

         )
  )

  (list 122201 "Signature permutations of FORK-transformations of non-recursive gatomorphisms in table A089840."
        '(off: 0)
        '(keywords: "tabl")
        '(indentries: Catsigperm)
        (list 'c: (string-append "Row n is the signature permutation of the Catalan automorphism"
" which is obtained from the nth nonrecursive automorphism in the table A089840 with"
" the recursion scheme \"FORK\". In this recursion scheme the given automorphism is first applied"
" at the root of binary tree, before the algorithm recurses down to the both branches."
" I.e. this corresponds to the pre-order (prefix) traversal of a Catalan structure,"
" when it is interpreted as a binary tree. The associated Scheme-procedure !FORK can be used"
" to obtain such a transformed gatomorphism from any destructively implemented"
" gatomorphism."
" Each row occurs only once in this table."
" Inverses of these permutations can be found in table A122202."
                  )
        )
        (list 'y: (string-append "The first 22 rows of this table:"
           " Row 0 (identity permutation): A001477, 1: A057163, 2: A057511,"
           " 3: A122341, 4: A122343, 5: A122345, 6: A122347, 7: A122349,"
           " 8: A082325, 9: A082360, 10: A122291, 11: A122293,"
           " 12: A074681, 13: A122295, 14: A122297, 15: A122353, 16: A122355,"
           " 17: A074684, 18: A122357, 19: A122359, 20: A122361, 21: A122301."
           " Other rows: row 4253: A082356, row ??? (index to be computed): A082358."
" C.f. also tables A089840, A122200, A122202-A122204, A122283-A12284, A122285-A122288, A122289-A122290."
                  )
        )
        '(scheme: "(define (!FORK foo!) (letrec ((bar! (lambda (s) (cond ((pair? s) (foo! s) (bar! (car s)) (bar! (cdr s)))) s))) bar!))"
         )
  )

  (list 122202 "Signature permutations of KROF-transformations of non-recursive gatomorphisms in table A089840."
        '(off: 0)
        '(keywords: "tabl")
        '(indentries: Catsigperm)
        (list 'c: (string-append "Row n is the signature permutation of the Catalan automorphism"
" which is obtained from the nth nonrecursive automorphism in the table A089840 with"
" the recursion scheme \"KROF\". In this recursion scheme the algorithm first recurses"
" down to the both branches, before the given automorphism is applied at the root of binary tree."
" I.e. this corresponds to the post-order (postfix) traversal of a Catalan structure,"
" when it is interpreted as a binary tree. The associated Scheme-procedure KROF and !KROF can be used"
" to obtain such a transformed gatomorphism from any constructively or destructively implemented"
" gatomorphism."
" Each row occurs only once in this table."
" Inverses of these permutations can be found in table A122201."
                  )
        )
        (list 'y: (string-append "The first 22 rows of this table:"
           " Row 0 (identity permutation): A001477, 1: A057163, 2: A057512,"
           " 3: A122342, 4: A122348, 5: A122346, 6: A122344, 7: A122350,"
           " 8: A082326, 9: A122294, 10: A122292, 11: A082359, 12: A074683,"
           " 13: A122358, 14: A122360, 15: A122302, 16: A122362,"
           " 17: A074682, 18: A122296, 19: A122298, 20: A122356, 21: A122354."
           " Other rows: row 4069: A082355, row ??? (index to be computed): A082357."
 " C.f. also tables A089840, A122200, A122201-A122204, A122283-A12284, A122285-A122288, A122289-A122290."
                  )
        )
        '(scheme: "(define (KROF foo) (letrec ((bar (lambda (s) (fold-right (lambda (x y) (foo (cons (bar x) y))) '() s)))) bar))\n(define (!KROF foo!) (letrec ((bar! (lambda (s) (cond ((pair? s) (bar! (car s)) (bar! (cdr s)) (foo! s))) s))) bar!))"
         )
  )

  (list 122203 "Signature permutations of SPINE-transformations of non-recursive gatomorphisms in table A089840."
        '(off: 0)
        '(keywords: "tabl")
        '(indentries: Catsigperm)
        (list 'c: (string-append "Row n is the signature permutation of the Catalan automorphism"
" which is obtained from the nth nonrecursive automorphism in the table A089840 with"
" the recursion scheme \"SPINE\". In this recursion scheme the given automorphism is first applied"
" at the root of binary tree, before the algorithm recurses down to the right-hand side branch."
" This corresponds to the fold-left traversal (with the given automorphism working as an"
" unary function which is applied) of the Catalan structure, interpreted e.g. as"
" a parenthesization or a Lisp-like list."
" The associated Scheme-procedures SPINE and !SPINE can be used to obtain such a"
" transformed gatomorphism from any constructively or destructively implemented gatomorphism."
" Each row occurs only once in this table."
" Inverses of these permutations can be found in table A122204."
                  )
        )
        (list 'y: (string-append "The first 22 rows of this table:"
           " Row 0 (identity permutation): A001477, 1: A069767, 2: A057509, "
           " 7: A122282, 8: A082339, 17: A057501."
 " C.f. also tables A089840, A122200, A122201-A122204, A122283-A12284, A122285-A122288, A122289-A122290."
                  )
        )
        '(scheme: "(define (SPINE foo) (lambda (s) (fold-left (lambda (x y) (foo (cons x y))) '() s)))\n(define (!SPINE foo!) (letrec ((bar! (lambda (s) (cond ((pair? s) (foo! s) (bar! (cdr s)))) s))) bar!))"
         )
  )

  (list 122204 "Signature permutations of ENIPS-transformations of non-recursive gatomorphisms in table A089840."
        '(off: 0)
        '(keywords: "tabl")
        '(indentries: Catsigperm)
        (list 'c: (string-append "Row n is the signature permutation of the Catalan automorphism"
" which is obtained from the nth nonrecursive automorphism in the table A089840 with"
" the recursion scheme \"ENIPS\". In this recursion scheme the algorithm first recurses down to"
" the right-hand side branch of the binary tree, before the given automorphism is applied at its root."
" This corresponds to the fold-right traversal (with the given automorphism working as an"
" unary function which is applied) of the Catalan structure, interpreted e.g. as"
" a parenthesization or a Lisp-like list."
" The associated Scheme-procedures ENIPS and !ENIPS can be used to obtain such a"
" transformed gatomorphism from any constructively or destructively implemented gatomorphism."
" Each row occurs only once in this table."
" Inverses of these permutations can be found in table A122203."
                  )
        )
        (list 'y: (string-append "The first 22 rows of this table:"
           " Row 0 (identity permutation): A001477, 1: A069768, 2: A057510,"
           " 7: A122282, 8: A082340, 12: A057502."
  " C.f. also tables A089840, A122200, A122201-A122203, A122283-A12284, A122285-A122288, A122289-A122290."
                  )
        )
        '(scheme: "(define (ENIPS foo) (lambda (s) (fold-right (lambda (x y) (foo (cons x y))) '() s)))\n(define (!ENIPS foo!) (letrec ((bar! (lambda (s) (cond ((pair? s) (bar! (cdr s)) (foo! s))) s))) bar!))"
         )
  )

  (list 122283 "Signature permutations of DEEPEN-transformations of non-recursive gatomorphisms in table A089840."
        '(off: 0)
        '(keywords: "tabl")
        '(indentries: Catsigperm)
        (list 'c: (string-append "Row n is the signature permutation of the Catalan automorphism"
" which is obtained from the nth nonrecursive automorphism in the table A089840 with"
" the recursion scheme \"DEEPEN\". In this recursion scheme the given automorphism is first applied"
" at the root of general tree, before the algorithm recurses down to all subtrees."
" I.e. this corresponds to the pre-order (prefix) traversal of a Catalan structure, when it is"
" interpreted as a general tree. The associated Scheme-procedures DEEPEN and !DEEPEN can be used"
" to obtain such a transformed gatomorphism from any constructively or destructively implemented"
" gatomorphism."
" Each row occurs only once in this table."
" Inverses of these permutations can be found in table A122284."
                  )
        )
        (list 'y: (string-append "The first 22 rows of this table:"
           " Row 0 (identity permutation): A001477, 1: A122301, 2: A122300, 3: A122303,"
           " 4: A122305, 5: A122307, 6: A122309, 7: A122311, 8: A122313, 9: A122315, 10: A122317,"
           " 11: A122319, 12: A122321, 13: A122323, 14: A122325, 15: A122327, 16: A122329,"
           " 17: A122331, 18: A122333, 19: A122335, 20: A122337, 21: A122339."
           " C.f. also tables A089840, A122200, A122201-A122204, A122285-A122288, A122289-A122290."
                  )
        )
        '(scheme: "(define (!DEEPEN foo!) (letrec ((bar! (lambda (s) (cond ((pair? s) (foo! s) (for-each bar! s))) s))) bar!))"
         )
 )

  (list 122284 "Signature permutations of NEPEED-transformations of non-recursive gatomorphisms in table A089840."
        '(off: 0)
        '(keywords: "tabl")
        '(indentries: Catsigperm)
        (list 'c: (string-append "Row n is the signature permutation of the Catalan automorphism"
" which is obtained from the nth nonrecursive automorphism in the table A089840 with"
" the recursion scheme \"NEPEED\". In this recursion scheme the algorithm first recurses"
" down to all subtrees, before the given automorphism is applied at the root of general tree."
" I.e. this corresponds to the post-order (postfix) traversal of a Catalan structure,"
" when it is interpreted as a general tree. The associated Scheme-procedure NEPEED and !NEPEED can be used"
" to obtain such a transformed gatomorphism from any constructively or destructively implemented"
" gatomorphism."
" Each row occurs only once in this table."
" Inverses of these permutations can be found in table A122283."
                  )
        )
        (list 'y: (string-append "The first 22 rows of this table:"
           " Row 0 (identity permutation): A001477, 1: A122302, 2: A122300, 3: A122304,"
           " 4: A122310, 5: A122308, 6: A122306, 7: A122312, 8: A122314, 9: A122320, 10: A122318,"
           " 11: A122316, 12: A122332, 13: A122334, 14: A122336, 15: A122340, 16: A122338,"
           " 17: A122322, 18: A122324, 19: A122326, 20: A122330, 21: A122328."
           " C.f. also tables A089840, A122200, A122201-A122204, A122285-A122288, A122289-A122290."
                  )
        )
        '(scheme: "(define (!NEPEED foo!) (letrec ((bar! (lambda (s) (cond ((pair? s) (for-each bar! s) (foo! s))) s))) bar!))"
         )
  )

  (list 122285 "Signature permutations of ENIPS-transformations of gatomorphisms in table A122203."
        '(off: 0)
        '(keywords: "tabl")
        '(indentries: Catsigperm)
        (list 'c: (string-append "Row n is the signature permutation of the Catalan automorphism"
" which is obtained from the nth automorphism in the table A122203 with the recursion scheme"
" \"ENIPS\", or equivalently row n is obtained as ENIPS(SPINE(nth row of A089840))."
" See A122203 and A122204 for the description of SPINE and ENIPS."
" Each row occurs only once in this table."
" Inverses of these permutations can be found in table A122286."
                  )
        )
        (list 'y: (string-append "The known rows of this table:"
           " Row 0 (identity permutation): A001477, Row 2: A057508, "
           " Row 17: A057503."
           " C.f. also tables A089840, A122200, A122201-A122204, A122283-A12284, A122286-A122288, A122289-A122290."
 " As a sequence differs from A122286 for the first time at n=92, where a(n)=18, while A122286(n)=17."
                  )
        )
  )

  (list 122286 "Signature permutations of SPINE-transformations of gatomorphisms in table A122204."
        '(off: 0)
        '(keywords: "tabl")
        '(indentries: Catsigperm)
        (list 'c: (string-append "Row n is the signature permutation of the Catalan automorphism"
" which is obtained from the nth automorphism in the table A122204 with the recursion scheme"
" \"SPINE\", or equivalently row n is obtained as SPINE(ENIPS(nth row of A089840))."
" See A122203 and A122204 for the description of SPINE and ENIPS."
" Each row occurs only once in this table."
" Inverses of these permutations can be found in table A122285."
                  )
        )
        (list 'y: (string-append "The known rows of this table:"
           " Row 0 (identity permutation): A001477, Row 2: A057508, "
           " Row 12: A057504."
 " C.f. also tables A089840, A122200, A122201-A122204, A122283-A12284, A122285-A122288, A122289-A122290."
 " As a sequence differs from A122285 for the first time at n=92, where a(n)=17, while A122285(n)=18."
                  )
        )
  )

  (list 122287 "Signature permutations of FORK-transformations of gatomorphisms in table A122204."
        '(off: 0)
        '(keywords: "tabl")
        '(indentries: Catsigperm)
        (list 'c: (string-append "Row n is the signature permutation of the Catalan automorphism"
" which is obtained from the nth automorphism in the table A122204 with the recursion scheme"
" \"FORK\", or equivalently row n is obtained as FORK(ENIPS(nth row of A089840))."
" See A122201 and A122204 for the description of FORK and ENIPS."
" Each row occurs only once in this table."
" Inverses of these permutations can be found in table A122288."
                  )
        )
        (list 'y: (string-append "The known rows of this table:"
           " Row 0 (identity permutation): A001477, row 1: A069767 (check!), Row 2: A057164, "
           " Row 12: A057506."
 " C.f. also tables A089840, A122200, A122201-A122204, A122283-A12284, A122285-A122288, A122289-A122290."
                  )
        )
  )

  (list 122288 "Signature permutations of KROF-transformations of gatomorphisms in table A122203."
        '(off: 0)
        '(keywords: "tabl")
        '(indentries: Catsigperm)
        (list 'c: (string-append "Row n is the signature permutation of the Catalan automorphism"
" which is obtained from the nth automorphism in the table A122203 with the recursion scheme"
" \"KROF\", or equivalently row n is obtained as KROF(SPINE(nth row of A089840))."
" See A122202 and A122203 for the description of KROF and SPINE."
" Each row occurs only once in this table."
" Inverses of these permutations can be found in table A122287."
                  )
        )
        (list 'y: (string-append "The known rows of this table:"
           " Row 0 (identity permutation): A001477, row 1: A069768 (check!), Row 2: A057164, "
           " Row 17: A057505."
 " C.f. also tables A089840, A122200, A122201-A122204, A122283-A12284, A122285-A122286, A122289-A122290."
                  )
        )
  )

  (list 122289 "Signature permutations of FORK-transformations of gatomorphisms in table A122201."
        '(off: 0)
        '(keywords: "tabl")
        '(indentries: Catsigperm)
        (list 'c: (string-append "Row n is the signature permutation of the Catalan automorphism"
" which is obtained from the nth automorphism in the table A122201 with the recursion scheme"
" \"FORK\", or equivalently row n is obtained as FORK(FORK(nth row of A089840))."
" See A122201 for the description of FORK."
" Each row occurs only once in this table."
" Inverses of these permutations can be found in table A122290."
                  )
        )
        (list 'y: (string-append "The known rows of this table:"
           " Row 0 (identity permutation): A001477, row 1: A122351, row 2: A122363."
           " C.f. also tables A089840, A122200, A122201-A122204, A122283-A12284, A122285-A122288."
                  )
        )
  )

  (list 122290 "Signature permutations of KROF-transformations of gatomorphisms in table A122202."
        '(off: 0)
        '(keywords: "tabl")
        '(indentries: Catsigperm)
        (list 'c: (string-append "Row n is the signature permutation of the Catalan automorphism"
" which is obtained from the nth automorphism in the table A122202 with the recursion scheme"
" \"KROF\", or equivalently row n is obtained as KROF(KROF(nth row of A089840))."
" See A122202 for the description of KROF."
" Each row occurs only once in this table."
" Inverses of these permutations can be found in table A122289."
                  )
        )
        (list 'y: (string-append "The known rows of this table:"
           " Row 0 (identity permutation): A001477, row 1: A122351, row 2: A122364."
           " C.f. also tables A089840, A122200, A122201-A122204, A122283-A12284, A122285-A122288."
                  )
        )
  )


 )
)

(define uudet-nr (list 123492 123495 123496 123497 123498 123499 123500 123503 123695 123696 123713 123714))

(define A123492 (catfun1 *A123492!))
(define A123493 (catfun1 *A123493!))
(define A123494 (catfun1 *A123494!))
(define A123495 (catfun1 *A123495!))
(define A123496 (catfun1 *A123496!))
(define A123497 (catfun1 *A123497!))
(define A123498 (catfun1 *A123498!))
(define A123499 (catfun1 *A123499!))
(define A123500 (catfun1 *A123500!))
(define A123501 (catfun1 *A123501!))
(define A123502 (catfun1 *A123502!))
(define A123503 (catfun1 *A123503!))

(define A123695 (catfun1 *A123695!))
(define A123696 (catfun1 *A123696!))

(define A123713 (catfun1 *A123713!))
(define A123714 (catfun1 *A123714!))
(define A123715 (catfun1 *A123715!))
(define A123716 (catfun1 *A123716!))
(define A123717 (catfun1 *A123717!))
(define A123718 (catfun1 *A123718!))
(define A123719 (catfun1 *A123719))


;; (load "/home/karttu/Schemuli/GF2Xfuns.com")
;; (output-entries-to-file120_45 seqs-A123XXX "./seqs/A123XXX.txt" "Oct 10 2006")

(define seqs-A123XXX
 (list 
  (list 123492 "An involution of non-negative integers: signature permutation of a nonrecursive Catalan automorphism which swaps the sides of a binary tree if the left subtree of either the left or right hand side toplevel subtree is not empty, and otherwise keeps the binary tree intact."
        '(off: 0)
        '(c: "ADD the example lines!")
        '(y: "Row 79361 of A089840. Used to construct A123493, A123494, A123715 and A123716.")
        '(inv: 123492)
        '(indentries: Catsigperm)
        '(scheme: "(define (*A123492! s) (cond ((null? s) s) ((and (pair? (cdr s)) (pair? (cadr s))) (*A069770! s)) ((and (pair? (car s)) (pair? (caar s))) (*A069770! s))) s)" )

  )

  (list 123493 "Signature permutation of a Catalan automorphism: Row 79361 of table A122201."
        '(off: 0)
        (list 'c: (string-append "Signature-permutation of the Catalan automorphism which"
" is derived from nonrecursive Catalan automorphism *A123492"
" with the recursion schema FORK (defined in A122201). See further comments at A123494."
                  )
        )
        '(inv: 123494)
        '(y: "Row 79361 of A122201.")
        '(indentries: Catsigperm)
        '(scheme: "(define *A123493! (!FORK *A123492!))")
  )

  (list 123494 "Signature permutation of a Catalan automorphism: Row 79361 of table A122202."
        '(off: 0)
        (list 'c: (string-append "Signature-permutation of the Catalan automorphism which"
" is derived from the automorphism *A123492"
" with the recursion schema KROF (defined in A122202)."
" Like automorphisms *A057163 and *A069767/*A069768 these automorphisms keep closed"
" the subset of \"zigzagging\" binary trees (i.e. those binary trees where there are no nodes with"
" two non-empty branches, or equivalently, those ones for which Stanley's interpretation (c)"
" forms a non-branching line),"
" and thus induce a permutation of binary strings, when, starting from the root of such"
" a binary tree, the turns taken by non-empty branches are interpreted as a 0 or 1 (or vice versa)"
" depending on whether the tree grows to the left or right."
" In this manner, the Catalan automorphisms *A123494 and *A123493 induce"
" the Binary Reflected Gray Code (see A003188 and A006068)."
                  )
        )
        '(inv: 123493)
        '(y: "Row 79361 of A122201. See also A123715 and A123716.")
        '(indentries: Catsigperm)
        '(scheme: "(define *A123494! (!KROF *A123492!))")
  )

  (list 123495 "Signature permutation of a nonrecursive Catalan automorphism: Row 65518 of table A089840."
        '(off: 0)
        '(c: "ADD the example lines!")
        '(comps: (082351 069770))
        '(inv: 123496)
        '(y: "Row 65518 of A089840. Used to construct automorphism *A082357.")
        '(indentries: Catsigperm)
        '(scheme: "(define (*A123495! s) (cond ((null? s) s) ((and (pair? (car s)) (pair? (cdr s))) (*A069770! s) (*A074679! s)) (else (*A074679! s))) s)")
  )

  (list 123496 "Signature permutation of a nonrecursive Catalan automorphism: Row 65796 of table A089840."
        '(off: 0)
        '(c: "ADD the example lines!")
        '(comps: (069770 082352))
        '(inv: 123495)
        '(y: "Row 65796 of A089840. Used to construct automorphism *A082358.")
        '(indentries: Catsigperm)
        '(scheme: "(define (*A123496! s) (cond ((null? s) s) ((and (pair? (car s)) (pair? (caar s))) (*A074680! s) (*A069770! s)) (else (*A074680! s))) s)")
  )

  (list 123497 "Signature permutation of a nonrecursive Catalan automorphism: Row 1655089 of table A089840."
        '(off: 0)
        '(c: "ADD the example lines!")
        '(inv: 123498)
        '(y: "Row 1655089 of A089840. Used to construct automorphism *A123501. A074680(n) = A083927(a(A057123(n))).")
        '(indentries: Catsigperm)
        '(scheme: "(define (*A123497! s) (cond ((null? s) s) ((and (pair? (car s)) (pair? (cdar s))) (*A074680! s) (let ((old-cddr-s (cddr s))) (set-cdr! (cdr s) (cdadr s)) (set-cdr! (cadr s) old-cddr-s))) ((pair? (car s)) (*A072797! s)) ((pair? (cdr s)) (*A072796! s))) s)")
  )

  (list 123498 "Signature permutation of a nonrecursive Catalan automorphism: Row 1654249 of table A089840."
        '(off: 0)
        '(c: "ADD the example lines!")
        '(inv: 123497)
        '(y: "Row 1654249 of A089840. Used to construct automorphism *A123502. A074679(n) = A083927(a(A057123(n))).")
        '(indentries: Catsigperm)
        '(scheme: "(define (*A123498! s) (cond ((null? s) s) ((and (pair? (cdr s)) (pair? (cadr s))) (let ((old-cddr-s (cddr s))) (set-cdr! (cdr s) (cdadr s)) (set-cdr! (cadr s) old-cddr-s)) (*A074679! s)) ((pair? (cdr s)) (*A072796! s)) ((pair? (car s)) (*A072797! s))) s)")
  )

  (list 123499 "Signature permutation of a nonrecursive Catalan automorphism: rotate a binary tree left if possible, otherwise apply *A089863."
        '(off: 0)
        (list 'c: (string-append
"This automorphism cannot be represented"
" as a composition of two smaller nonrecursive automorphisms. See the comments at A123503."
                 )
        )
        '(inv: 123500)
        '(y: "Row 258 of A089840. Variant of A074679.")
        '(indentries: Catsigperm)
        '(scheme: "(define (*A123499! s) (cond ((null? s) s) ((pair? (cdr s)) (*A074679! s)) (else (*A089863! s))) s)")
  )

  (list 123500 "Signature permutation of a nonrecursive Catalan automorphism: rotate a binary tree right if possible, otherwise apply *A089859."
        '(off: 0)
        (list 'c: (string-append
"This automorphism cannot be represented"
" as a composition of two smaller nonrecursive automorphisms. See the comments at A123503."
                 )
        )
        '(inv: 123499)
        '(y: "Row 264 of A089840. Variant of A074680.")
        '(indentries: Catsigperm)
        '(scheme: "(define (*A123500! s) (cond ((null? s) s) ((pair? (car s)) (*A074680! s)) (else (*A089859! s))) s)")
  )

  (list 123501 "Signature permutation of a Catalan automorphism: apply *A123497 at the root, then recurse into the left subtree of the right hand side subtree of a binary tree."
        '(off: 0)
        '(inv: 123502)
        '(y: "A057501(n) = A083927(a(A057123(n))) = A083927(A085159(A057123(n))).")
        '(indentries: Catsigperm)
        '(scheme: "(define (*A123501! s) (*A123497! s) (cond ((and (pair? s) (pair? (cdr s))) (*A123501! (cadr s)))) s)")
  )

  (list 123502 "Signature permutation of a Catalan automorphism: first recurse into the left subtree of the right hand side subtree of a binary tree, and after that apply *A123498 at the root."
        '(off: 0)
        '(inv: 123501)
        '(y: "A057502(n) = A083927(a(A057123(n))) = A083927(A085160(A057123(n))).")
        '(indentries: Catsigperm)
        '(scheme: "(define (*A123502! s) (cond ((and (pair? s) (pair? (cdr s))) (*A123502! (cadr s)))) (*A123498! s) s)")
  )

  (list 123503 "An involution of non-negative integers: signature permutation of a nonrecursive Catalan automorphism, row 253 of table A089840."
        (list 'c: (string-append "This automorphism either swaps (if A057515(n) > 1)"
" the first two toplevel elements (of a general plane tree, like *A072796 does),"
" and otherwise (if n > 1, A057515(n)=1) swaps the sides of the left hand side subtree"
" of the S-expression (when viewed as a binary tree, like *A089854 does)."
" This is the first multiclause automorphism in table A089840 which cannot be represented"
" as a composition of two smaller nonrecursive automorphisms,"
" the property which is also shared by *A123499 and *A123500."
" ADD the example lines!"
                  )
        )
        '(off: 0)
        '(y: "Row 253 of A089840. Used to construct A123717 and A123718.")
        '(inv: 123503)
        '(indentries: Catsigperm)
        '(scheme: "(define (*A123503! s) (cond ((null? s) s) ((pair? (cdr s)) (*A072796! s)) (else (*A089854! s))) s)")
  )

  (list 123695 "Signature permutation of a nonrecursive Catalan automorphism: Row 1653002 of table A089840."
        '(off: 0)
        (list 'c: (string-append
"It is possible to recursively construct more of these kind of nonrecursive automorphisms,"
" where by default (if A057515(n) > 1) *A074679 is applied, and otherwise the previous"
" automorphism of this construction process (here *A074679 itself) is applied"
" to the left subtree of a binary tree, before the whole tree is swapped with *A069770."
" Question then remains whether the associated cycle-count sequences converge to anything interesting."
" ADD the example lines!"
                  )
        )
        '(inv: 123696)
        '(y: "Row 1653002 of A089840. Variant of A074679.")
        '(indentries: Catsigperm)
        '(scheme: "(define (*A123695! s) (cond ((null? s) s) ((pair? (cdr s)) (*A074679! s)) ((pair? (car s)) (*A074679! (car s)) (*A069770! s))) s)")
  )

  (list 123696 "Signature permutation of a nonrecursive Catalan automorphism: Row 1653063 of table A089840."
        '(off: 0)
        '(c: "See the comments at A123695. ADD the example lines!")
        '(inv: 123695)
        '(y: "Row 1653063 of A089840. Variant of A074680.")
        '(indentries: Catsigperm)
        '(scheme: "(define (*A123696! s) (cond ((null? s) s) ((pair? (car s)) (*A074680! s)) ((pair? (cdr s)) (*A074680! (cdr s)) (*A069770! s))) s)")
  )


  (list 123713 "Signature permutation of a nonrecursive Catalan automorphism: Row 1783367 of table A089840."
        '(off: 0)
        (list 'c: (string-append
""
                  )
        )
        '(inv: 123714)
        '(y: "Row 1783367 of A089840. Differs from A089855 for the first time at n=102, where a(n)=103, while A089855(n)=102.")
        '(indentries: Catsigperm)
        '(scheme: "(define (*A123713! s) (cond ((not (pair? s)) s) ((pair? (car s)) (let ((org_cdar (cdar s))) (set-cdr! (car s) (cdr s)) (set-cdr! s (caar s)) (set-car! (car s) org_cdar) s)) ((and (pair? (cdr s)) (pair? (cadr s)) (pair? (caadr s)) (pair? (caaadr s))) (let ((org_b (car (caaadr s)))) (set-car! (caaadr s) (cdr (caaadr s))) (set-cdr! (caaadr s) (cdaadr s)) (set-cdr! (caadr s) (cdadr s)) (set-cdr! (cadr s) (cddr s)) (set-cdr! (cdr s) org_b) s)) (else s)))")
  )

  (list 123714 "Signature permutation of a nonrecursive Catalan automorphism: Row 1786785 of table A089840."
        '(off: 0)
        (list 'c: (string-append
"This is the last multiclause automorphism of total seven opened conses in the table A089840."
" The next nonrecursive automorphism, A089840[1786786], which consists of a single seven-node clause,"
" swaps the first two toplevel elements (of a general plane tree, like *A072796 does),"
" but only if A057515(n) > 6, and otherwise keeps the tree intact."
                  )
        )
        '(inv: 123713)
        '(y: "Row 1786785 of A089840. Differs from A089857 for the first time at n=102, where a(n)=106, while A089857(n)=102.")
        '(indentries: Catsigperm)
        '(scheme: "(define (*A123714! s) (cond ((not (pair? s)) s) ((pair? (car s)) (let ((org_a (caar s))) (set-car! (car s) (cdr s)) (set-cdr! s (cdar s)) (set-cdr! (car s) org_a) s)) ((and (pair? (cdr s)) (pair? (cadr s)) (pair? (caadr s)) (pair? (caaadr s))) (let ((org_f (cddr s))) (set-cdr! (cdr s) (cdadr s)) (set-cdr! (cadr s) (cdaadr s)) (set-cdr! (caadr s) (cdr (caaadr s))) (set-cdr! (caaadr s) (car (caaadr s))) (set-car! (caaadr s) org_f) s)) (else s)))")
  )


  (list 123715 "Signature permutation of a Catalan automorphism: Row 79361 of table A122203."
        '(off: 0)
        (list 'c: (string-append "Signature-permutation of the Catalan automorphism which"
" is derived from nonrecursive Catalan automorphism *A123492"
" with the recursion schema SPINE (defined in A122203)."
                  )
        )
        '(inv: 123716)
        '(y: "Row 79361 of A122203.")
        '(indentries: Catsigperm)
        '(scheme: "(define *A123715! (!SPINE *A123492!))")
  )

  (list 123716 "Signature permutation of a Catalan automorphism: Row 79361 of table A122204."
        '(off: 0)
        (list 'c: (string-append "Signature-permutation of the Catalan automorphism which"
" is derived from nonrecursive Catalan automorphism *A123492"
" with the recursion schema ENIPS (defined in A122204)."
                  )
        )
        '(inv: 123715)
        '(y: "Row 79361 of A122204.")
        '(indentries: Catsigperm)
        '(scheme: "(define *A123716! (!ENIPS *A123492!))")
  )

  (list 123717 "Signature permutation of a Catalan automorphism: Row 253 of table A122203."
        '(off: 0)
        (list 'c: (string-append "Signature-permutation of the Catalan automorphism which"
" is derived from nonrecursive Catalan automorphism *A123503"
" with the recursion schema SPINE (defined in A122203)."
" Deriving a formula for this automorphism's fixed-point count and cycle-count sequences"
" might be an interesting exercise."
                  )
        )
        '(comps: (057509 089854))
        '(inv: 123718)
        '(y: "Row 253 of A122203.")
        '(indentries: Catsigperm)
        '(scheme: "(define *A123717! (!SPINE *A123503!))")
  )

  (list 123718 "Signature permutation of a Catalan automorphism: Row 253 of table A122204."
        '(off: 0)
        (list 'c: (string-append "Signature-permutation of the Catalan automorphism which"
" is derived from nonrecursive Catalan automorphism *A123503"
" with the recursion schema ENIPS (defined in A122204)."
                  )
        )
        '(comps: (089854 057510))
        '(inv: 123717)
        '(y: "Row 253 of A122204.")
        '(indentries: Catsigperm)
        '(scheme: "(define *A123718! (!ENIPS *A123503!))")
  )

  (list 123719 "An involution of non-negative integers: signature permutation of the Catalan automorphism which is obtained with recursion schema RIBS from automorphism *A085161"
        '(off: 0)
        (list 'c: (string-append
             "Recursion schema RIBS is defined in A122200."
" Number of fixed points in range [A014137(n-1)..A014138(n-1)] of this permutation"
" is given by INVERT transform of A001405, appropriately shifted."
                  )
        )
        '(inv: 123719)
        '(comps: (085160 085163))
        '(y: "A085163(n) = A085159(a(n)).")
        '(indentries: Catsigperm)
        '(scheme: "(define *A123719 (RIBS *A085161))")
  )
 )
)


;; Iterates of A122237, starting from 0.
(define (A106191v1 n) (if (< n 2) n (A080300 (A079946 (A020988 (- n 2))))))
;; Seems to be A106191, apart from the zeroth term:
(define (A106191v2 n) (if (< n 2) 1 (+ (A014137 (- n 1)) (A014138 (- n 2)))))
(define (A106191v3 n) (if (< n 2) 1 (- (* 2 (A014137 (- n 1))) 1)))

(define (A106191_signed_v n) (if (< n 2) 1 (- 1 (* 2 (A014137 (- n 1))))))

;; A lot's of comments to A106191:
;;
;; (map A075164 (map A106191v3 (iota0 12)))
;; (1,1,3,9,27,243,2187,177147,1594323,129140163,1162261467,94143178827,68630377364883)
;; (map (lambda (n) (floor->exact (round (/ (log n) (log 3))))) (map A075164 (map A106191v3 (iota0 15))))
;; (0 0 1 2 3 5 7 11 13 17 19 23 29 31 37 41)
;; i.e. A106191(n) = A075163(A000244(A000040(n-2))), for n>=3. Why 3^p_{n-2}, why not 2^p_{n-2} ?
;; = A106453(....) Weird! Not at all, quite clear!

;; (map A075163 (map (lambda (n) (expt 3 n)) (list 1 2 3 5 7 11 13 17 19)))
;; (3 7 17 45 129 393 1251 4111 13835)

;; (output-entries-to-file120_45 seqs-A122227-46 "./seqs/A122227-46.txt" "Sep 01 2006")

(define seqs-A122227-46
 (list 
  (list 122227 "a(n) = A057548(A057117(n))"
        '(off: 0)
        '(y: "Iterates: A122228, A122231, A122234, A122238. C.f. A080067, A122237.")
  )

  (list 122228 "Iterates of A122227, starting from 0."
        '(off: 0)
        '(upto: 15)
        '(y: "C.f. A122229, A122230, A106191.")
        '(scheme: "(define (A122228 n) (if (zero? n) 0 (A122227 (A122228 (-1+ n)))))")
  )

  (list 122229 "a(n) = A014486(A122228(n))."
        '(off: 0)
        '(upto: 15)
        '(f: "A simple formula exists, C.f. A106191.")
        '(c: "A122230 shows the same sequence in binary.")
        '(y: "Compare to the plots given in A080070, A122232, A122235, A122239, A122242, A122245.")
  )

  (list 122230 "a(n) = A007088(A122229(n))."
        '(off: 0)
        '(upto: 15)
        '(keywords: "base")
        '(y: "C.f. A080070, A122233, A122236, A122240, A122243, A122246.")
  )

  (list 122237 "a(n) = A057548(A082358(n))"
        '(off: 0)
        '(y: "Iterates: A106191, A122241, A122244. C.f. also A122227, A080067.")
  )

;; Add as comments to A106191:
  (list 106191 "Iterates of A122237, starting from 0."
        '(off: 0)
        '(f: "a(0)=0, a(1)=1, a(n) = A080300(A079946(A020988(n-2))) or A080300(A080675(n)) from n>=1 onwards.")
        (list 'c: (string-append
     "A080675 gives the A014486-codes of these iterates. They have a simple binary pattern:"
     " 10,1100,110100,11010100,1101010100, ... i.e. the nth one has a binary expansion 1(10){n-1}0,"
     " i.e. there are n-1 \"10\"'s between the most significant 1 and the least significant 0."
                  )
        )
        '(y: "C.f. A122228.")
        (list 'scheme: (string-append
   "(define (A106191 n) (if (< n 2) n (A080300 (A079946 (A020988 (- n 2))))))"
   "\n(define (A106191v2 n) (if (< n 2) 1 (+ (A014137 (- n 1)) (A014138 (- n 2)))))"
   "\n(define (A106191v3 n) (if (< n 2) 1 (- (* 2 (A014137 (- n 1))) 1)))"
   "\n(define (A106191_signed_v n) (if (< n 2) 1 (- 1 (* 2 (A014137 (- n 1))))))"
                       )
        )
  )

  (list 122231 "Iterates of A122227, starting from 4."
        '(off: 1)
        '(upto: 12)
        '(y: "C.f. A122232, A122233.")
        '(scheme: "(define (A122231 n) (if (= 1 n) 4 (A122227 (A122231 (-1+ n)))))")
  )

  (list 122232 "a(n) = A014486(A122231(n))."
        '(off: 1)
        '(c: "A122233 shows the same sequence in binary.")
        '(upto: 12)
        '(y: "Compare to the plots given in A080070, A122229, A122235, A122239, A122242, A122245.")
  )

  (list 122233 "a(n) = A007088(A122232(n))."
        '(off: 1)
        '(upto: 12)
        '(keywords: "base")
        '(y: "C.f. A080070, A122230, A122236, A122240, A122243, A122246.")
  )


  (list 122234 "Iterates of A122227, starting from 5."
        '(off: 1)
        '(upto: 12)
        '(y: "C.f. A122236, A122237.")
  )

  (list 122235 "a(n) = A014486(A122234(n))."
        '(off: 1)
        '(c: "A122236 shows the same sequence in binary.")
        '(upto: 12)
        '(y: "Compare to the plots given in A080070, A122229, A122232, A122239, A122242, A122245.")
  )

  (list 122236 "a(n) = A007088(A122235(n))."
        '(off: 1)
        '(upto: 12)
        '(keywords: "base")
        '(y: "C.f. A080070, A122230, A122233, A122240, A122243, A122246.")
  )


  (list 122238 "Iterates of A122227, starting from 7."
        '(off: 1)
        '(upto: 12)
        '(y: "C.f. A122239, A122240.")
  )

  (list 122239 "a(n) = A014486(A122238(n))."
        '(off: 1)
        '(c: "A122240 shows the same sequence in binary.")
        '(upto: 12)
        '(y: "Compare to the plots given in A080070, A122229, A122232, A122235, A122242, A122245.")
  )

  (list 122240 "a(n) = A007088(A122239(n))."
        '(off: 1)
        '(upto: 12)
        '(keywords: "base")
        '(y: "C.f. A080070, A122230, A122233, A122236, A122243, A122246.")
  )


  (list 122241 "Iterates of A122237, starting from 4."
        '(off: 1)
        '(upto: 12)
        '(y: "C.f. A122242, A122243.")
        '(scheme: "(define (A122241 n) (if (= 1 n) 4 (A122237 (A122241 (-1+ n)))))")
  )

  (list 122242 "a(n) = A014486(A122241(n))."
        '(off: 1)
        '(c: "A122243 shows the same sequence in binary.")
        '(upto: 12)
        '(y: "Compare to the plots given in A080070, A122229, A122232, A122235, A122239, A122245.")
  )

  (list 122243 "a(n) = A007088(A122242(n))."
        '(off: 1)
        '(upto: 12)
        '(keywords: "base")
        '(y: "C.f. A080070, A122230, A122233, A122236, A122240, A122246.")
  )


  (list 122244 "Iterates of A122237, starting from 5."
        '(off: 1)
        '(upto: 12)
        '(y: "C.f. A122245, A122246.")
        '(scheme: "(define (A122244 n) (if (= 1 n) 5 (A122237 (A122244 (-1+ n)))))")
  )

  (list 122245 "a(n) = A014486(A122244(n))."
        '(off: 1)
        '(c: "A122246 shows the same sequence in binary.")
        '(upto: 12)
        '(y: "Compare to the plots given in A080070, A122229, A122232, A122235, A122239, A122242.")
  )

  (list 122246 "a(n) = A007088(A122245(n))."
        '(off: 1)
        '(upto: 12)
        '(keywords: "base")
        '(y: "C.f. A080070, A122230, A122233, A122236, A122240, A122243.")
  )
 )
)

;; (output-sequence-file gat-list3 2056 "Jun 28 2006" "/home/karttu/Nekomorphisms/seqs/A120705-9.txt")
(define gat-list3
 (list
  (list A120705 A120706 120705 120706  "Gatomorphism A120705" '() - -
       '(SoS: 4)
       '(CC: 120707) '(FC: 19590) '(MC: 120708) '(LC: 120709)
       '(SCHEME:
          ("Destructive version"

              (define (*A120705! s)
                (cond ((pair? s)
                         (*A074680! s) ;; i.e. gmrobr!
                         (*A120705! (car s))
                         (cond ((pair? (cdr s))
                                  (*A120705! (cddr s))
                                  (*A120706! (cadr s))
                               )
                         )
                      )
                )
                s
              )
          )
        )
  )

  (list A120706 A120705 120706 120705  "Gatomorphism A120706" '() - -
       '(SoS: 4)
       '(CC: 120707) '(FC: 19590) '(MC: 120708) '(LC: 120709)
       '(SCHEME:
          ("Destructive version"

              (define (*A120706! s) ;; inverse of *A120705!
                (cond ((pair? s)
                         (*A074679! s) ;; i.e. gmrobl!
                         (*A120706! (cdr s))
                         (cond ((pair? (car s))
                                  (*A120706! (caar s))
                                  (*A120705! (cdar s))
                               )
                         )
                      )
                )
                s
              )
          )
        )
  )

 )
)



;; (output-entries-to-file120_45 seqs-December2006 "./seqs/December2006.txt" "Dec XX 2006")

(define seqs-December2006
 (list 
  (list 125989 "Sum of heights of 10's in binary expansion of n."
        '(off: 0)
        '(keywords: "base")
        (list 'c: (string-append
           "The 'height' of the digits in the binary expansion of n is here defined by the"
           " procedure where, starting from the least significant bit and the height=0,"
           " and proceeding leftwards, all encountered 1-bits"
           " decrease the height by one, and all 0-bits increase it by one."
           " The sequence gives the sums of local maximums of heights"
           " (i.e. at the positions where the height decreases when going either left or right)."
           " This sequence is used for computing A126302."
                  )
        )
        (list 'e: (string-append
           "10 is 1010 in binary, and both peaks occur at height=1, thus a(10)=2."
           " 11 is 1011 in binary, so the only peak '10' occurs at height -1, thus a(11)=-1."
                  )
        )
;; Not even this holds A125989(A125975(n)) = A125989(A125974(A125975(n))) (but maybe for some subset?)
        '(y: "A126302 = a(A014486(n)). Cf. A085198.")
        '(scheme: "(define (A125989 n) (let loop ((n n) (s 0) (h 0)) (cond ((zero? n) s) ((= 2 (modulo n 4)) (loop (/ (- n 2) 4) (+ s h 1) h)) ((odd? n) (loop (/ (- n 1) 2) s (- h 1))) (else (loop (/ n 2) s (+ 1 h))))))")
  )

  (list 126302 "a(n) = Sum of peak heights of the nth Dyck-path encoded by A014486(n)."
        '(off: 0)
        '(f: "a(n) = A125989(A014486(n)).")
        (list 'e: (string-append
           "A014486(2) = 10 (1010 in binary), encodes Dyck path /\\/\\, with two peaks at height=1, thus a(10)=2."
                  )
        )
        '(comps: (126302 125976))
  )

  (list 126303 "a(n) = Number of nodes with odd distance to the root in the nth plane general tree encoded by A014486(n). Both internal and terminal nodes (leaves) are counted."
        '(off: 0)
        (list 'e: (string-append
           "A014486(2) = 10 (1010 in binary), encodes the general tree ..."
                  )
        )
        (list 'y: "a(n) = A072643(n)-A126305(n). Cf. A126304.")
        (list 'scheme: (string-append
              "(define (A126303 n) (*A126303 (A014486->parenthesization (A014486 n))))"
              "\n(define (*A126303 s) (cond ((null? s) 0) (else (fold-left (lambda (x y) (+ x 1 (*A126304 y))) 0 s))))"
                  )
        )
  )

  (list 126304 "a(n) = Number of nodes with nonzero even distance to the root in the nth plane general tree encoded by A014486(n)."
        '(off: 0)
        (list 'e: (string-append
           "A014486(2) = 10 (1010 in binary), encodes the general tree ..."
                  )
        )
        (list 'y: "a(n) = A126305(n)-1. Cf. A126303.")
        (list 'scheme: (string-append
              "(define (A126304 n) (*A126304 (A014486->parenthesization (A014486 n))))"
              "\n(define (*A126304 s) (cond ((null? s) 0) (else (apply + (map *A126303 s)))))"
                  )
        )
  )

  (list 126305 "a(n) = Number of nodes with even distance to the root in the nth plane general tree encoded by A014486(n). The root node itself is also included."
        '(off: 0)
        (list 'y: "a(n) = A126304(n)+1 = A072643(n)-A126303(n).")
  )

  (list 126306 "a(n) = Number of double-rises in the nth Dyck path encoded by A014486(n)."
        '(off: 0)
        (list 'e: (string-append
           "A014486(8) = 56 (111000 in binary), encodes the Dyck path /..."
                  )
        )
        '(comps: (000120 48735 14486) (14081 14486))
        (list 'y: "a(A125976(n)) = A057514(n)-1, for all n >= 1.")
  )

  (list 126307 "a(n) = Length of the leftmost ascent (i.e. height of the first peak) in the nth Dyck path encoded by A014486(n)."
        '(off: 0)
        (list 'c: "In other words, this sequence gives the number of leading 1's in the terms of A063171.")
        (list 'e: (string-append
           "A014486(8) = 56 (111000 in binary), encodes the Dyck path /..., thus a(8)=3."
                  )
        )
        '(comps: (90996 14486) (99563 71156) (57515 125985) (80237 57164) (57515 57504 57164))
  )


  (list 126308 "Delete '10'-substrings in the binary expansion of n."
        '(off: 0)
        '(keywords: "base")
        (list 'e: (string-append
          "10 is 1010 in binary, thus it is rewritten as '', meaning a(10)=0."
          " 27 is 11011 in binary, and when '10' is deleted, results 111, 7 in decimal, thus a(27)=7."
                  )
        )
        '(y: "A126309 = A080300(a(A014486(n))). Cf. A080303.")
        '(scheme: "(define (A126308 n) (cond ((zero? n) 0) ((= 2 (modulo n 4)) (A126308 (/ (- n 2) 4))) (else (+ (modulo n 2) (* 2 (A126308 (floor->exact (/ n 2))))))))")
  )

  (list 126309 "A014486-index for the Dyck path compressed from the nth Dyck path encoded by A014486(n)."
        '(off: 0)
        '(f: "a(n) = A080300(A126308(A014486(n))).")
        (list 'e: (string-append
          "A014486(4) encodes the Dyck path /\\/\\/\\, of which, when all the peaks are removed, nothing remains, thus a(4)=0. A014486(18) encodes the Dyck path:"
      "\n....../\\"
      "\n.../\\/..\\"
      "\n../......\\,"
      "\nwhich, after the peaks are removed, results"
      "\n.../\\,"
      "\n../..\\ encoded by A014486(3), thus a(18)=3."
                  )
        )
        '(comps: (125985 126310 125986))
;; (map (lambda (n) (- (A126309 n) (A125985 (A126310 (A125986 n))))) (iota0 2055))

        (list 'c: (string-append
             "According to Vaille, the concept of \"compression d'un pont\" was introduced"
             " by Poupard, in \"Sur les quasi-points\" paper."
             " In effect, the operation removes all the peaks /\\ from the Dyck path."
                  )
        )
;; comps: A125985(A126310(n)) = A126309(A125985(n))
;; Check also: A057514(A125985(n)) = A072643(n) - A057514(n) + 1.
  )

  (list 126310 "A014486-index for the Dyck path derived from the nth Dyck path encoded by A014486(n)."
        '(off: 0)
        '(comps: (125986 126309 125985))
;; (map (lambda (n) (- (A126310 n) (A125986 (A126309 (A125985 n))))) (iota0 2055))
        (list 'c: (string-append
             "According to Vaill\\'{e}, the concept of \"d\\'{e}rivation des ponts\" is defined"
             " by Kreweras, in \"Sur les \\'{e}ventails de segments\" paper."
                  )
        )
        (list 'scheme: (string-append " rising-list->binexp given in A125985)."
                "\n(define (A126310 n) (A080300 (rising-list->binexp (reverse! (map -1+ (map length (A126310-aux1 (A036044 (A014486 n)))))))))"
                "\n(define (A126310-aux1 n) (let loop ((n n) (vs (list)) (u 0) (d 0)) (cond ((zero? n) (if (null? vs) vs (reverse! (cdr vs)))) ((= 2 (modulo n 4)) (loop (/ n 2) (cons (list (+ 1 u)) vs) (+ u 1) d)) ((= 1 (modulo n 4)) (add-valley-abscisses! (+ d 1) vs) (loop (/ (- n 1) 2) vs u (+ d 1))) ((odd? n) (loop (/ (- n 1) 2) vs u (+ d 1))) (else (loop (/ n 2) vs (+ u 1) d)))))"
                "\n(define (add-valley-abscisses! valley-abscisse peak-ordonnees) (for-each (lambda (s) (append! s (list valley-abscisse))) (keep-matching-items peak-ordonnees (lambda (po) (>= (car po) valley-abscisse)))))"
                       )
        )
  )

  (list 90996 "Number of leading 1's in binary expansion of n."
        '(off: 0)
        '(f: "a(n) = A007814(1+A030101(n)).")
  )

  (list 126006 "Involution of nonnegative integers: Swap the positions of digits q0 <-> q1, q2 <-> q3, q4 <-> q5, etc. in the base-4 expansion of n (where n = ... + q4*256 + q3*64 + q2*16 + q1*4 + q0)"
        '(off: 0)
        '(indentries: Nperm)
        '(keywords: "base")
        '(inv: 126006)
        '(e: "29 = 0*64 + 1*16 + 3*4 + 1, i.e. 131 in quaternary, and when digits are swapped in pairs, results 1013 in quaternary (1*64 + 0*16 + 1*4 + 3 = 71 in decimal), thus a(29)=71.")
        '(y: "Cf. A126007. A057300 is the analogous permutation based on swapping the binary digits of n.")
        '(scheme: "(define (A126006 n) (let loop ((n n) (s 0) (p 1)) (cond ((zero? n) s) (else (loop (floor->exact (/ n 16)) (+ s (* p (+ (* 4 (modulo n 4)) (modulo (floor->exact (/ n 4)) 4)))) (* p 16))))))")
  )

  (list 126007 "Involution of nonnegative integers: Keep the least significant quaternary digit q0 of n fixed, but swap the positions of digits q1 <-> q2, q3 <-> q4, ..., etc. in the base-4 expansion of n (where n = ... + q4*256 + q3*64 + q2*16 + q1*4 + q0)"
        '(off: 0)
        '(keywords: "base")
        '(indentries: Nperm)
        '(f: "a(n) = (n mod 4) + 4*A126006([n/4]), where brackets stand for the floor function.")
        '(inv: 126007)
        '(scheme: "(define (A126007 n) (+ (modulo n 4) (* 4 (A126006 (floor->exact (/ n 4))))))")
  )

  (list 126008 "Involution of nonnegative integers: composition of involutions A057300 and A126007."
        '(off: 0)
        '(keywords: "base")
        '(indentries: Nperm)
        '(comps: (57300 126007) (126007 57300))
        '(inv: 126008)
        '(y: "The first 64 terms are identical with A106485.")
  )

  (list 125974 "Function whose restriction to A014486 induces signature-permutation A125976."
        '(off: 0)
        '(keywords: "base")
        '(y: "A125975 gives the terms i, for which a(a(i)) = i. Question: would it be possible to construct an \"elegant and natural variant\" which were an involution for all the natural numbers? (and acting in the same way on the set A125975, or at least on the set A014486.)")
        '(scheme: "Scheme-code and Python-code follows. A lot of it.")
  )

  (list 125975 "Integers i, for which A125974(A125974(i)) = i."
        '(off: 0)
        '(c: "The even terms seem to be the even terms of A031443, and some of the odd terms are produced by the composition A030101 o A014486.")
  )

  (list 125976 "Signature-permutation of Kreweras 1970 involution on Dyck paths."
        '(off: 0)
        '(indentries: Catsigperm)
        '(comps: (80300 125974 14486))
        '(inv: 125976)
        (list 'c: (string-append "Lalanne shows in 1992 paper that this automorphism preserves the quantity HP_v(w), defined as HP(w) + v*DL(w), where HP(w) is the sum of the heights of the peaks in the Dyck path w, and DL(w) is the semilength of w, and v is an arbitrary real > -1. In OEIS-terms, AnewHP(a(n)) + v * A072643(a(n)) = AnewHP(n) + v * A072643(n), for all n."
          "Like A069772, this involution keeps symmetric Dyck paths symmetric, but not necessarily same."
             )
        )
        '(y: "The number of cycles and fixed points in range [A014137(n-1)..A014138(n-1)] of this involution seem to be given by A007595 and the \"aerated\" Catalans [1,1,0,1,0,2,0,5,0,14,0,42,...], thus this is probably a conjugate of A069770 (as well as of A057163). Compositions and conjugations with other automorphisms: A125977-A125979, A125980, A126290.")
  )

  (list 125977 "Signature-permutation of a Catalan automorphism: composition of A057163 and A125976."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 125978)
        '(comps: (57163 125976))
  )

  (list 125978 "Signature-permutation of a Catalan automorphism: composition of A125976 and A057163."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 125977)
        '(comps: (125976 57163))
  )

  (list 125979 "Signature-permutation of a Catalan automorphism: composition of A125976 and A057164."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 125979)
        '(comps: (125976 57164) (57164 125976))
        '(y: "Differs from A071663 for the first time at n=43, where a(n)=34, while A071663(n)=48.")
  )

  (list 125980 "Signature-permutation of a Catalan automorphism: A057163-conjugate of A125976."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 125980)
        '(comps: (57163 125976 57163) (125977 57163) (57163 125978))
  )

  (list 126290 "Signature-permutation of a Catalan automorphism: A069772-conjugate of A125976."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 126290)
        '(comps: (69772 125976 69772) (69771 125976 69771))
  )

  (list 125981 "Signature-permutation of Deutsch's 2000 bijection on ordered trees."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 125982)
        (list 'c: (string-append "Deutsch shows in his 2000 paper that this automorphism converts"
" any ordered tree with the number of nodes having degree q, to a tree with an equal"
" amount of odd-level nodes having degree q-1, from which it follows that, "
" for each positive integer q, the parameters \"number of nodes of degree q\" and \"number of odd-level nodes of degree q-1\" are equidistributed."
" He also shows that this automorphism converts any tree with k leaves to a tree with k"
" even-level nodes, i.e. in OEIS-terms A057514(n) = Anew_evenlevnodes(A125981(n)), for all n."
                  )
        )
        '(y: "The number of cycles, maximum cycle sizes and LCM's of all cycle sizes in range [A014137(n-1)..A014138(n-1)] of this permutation seem to be given by A089411, A086586 and A089412, thus this is probably a conjugate of A074683/A074684. A125983 gives the A057163-conjugate.")
  )

  (list 125982 "Signature-permutation of the inverse of Deutsch's 2000 bijection on ordered trees."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 125981)
        '(y: "A125984 gives the A057163-conjugate.")
  )

  (list 125983 "Signature-permutation of a Catalan automorphism: A057163-conjugate of A125981."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 125984)
        '(comps: (57163 125981 57163))
  )

  (list 125984 "Signature-permutation of a Catalan automorphism: A057163-conjugate of A125982."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 125983)
        '(comps: (57163 125982 57163))
  )

  (list 125985 "Signature-permutation of Vaille's 1997 bijection on 'bridges' (Dyck paths)."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 125986)

        (list 'c: (string-append "Vaille shows in 1997 paper that this automorphism transforms"
" a 'derivation' of a Dyck path to its 'compression', i.e. in OEIS-terms"
" A125985(Anewderived(n)) = Anewcompressed(A125985(n)) holds for all n."
" He also proves that for all n, it holds that A057515(A125985(n)) = Anew_len_rising_slope(n)"
" and A057514(A125985(n)) = A072643(n) - A057514(n) + 1."
                  )
        )
        '(y: "The number of cycles, maximum cycle sizes and LCM's of all cycle sizes in range [A014137(n-1)..A014138(n-1)] of this permutation are given by A126291, A126292 and A126293. The fixed points are given by A126300/A126301.")
  )

  (list 125986 "Signature-permutation of the inverse of Vaille's 1997 bijection on Dyck paths."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 125985)
  )

  (list 125987 "Signature-permutation of the square of Vaille's 1997 bijection on Dyck paths."
        '(off: 0)
        '(indentries: Catsigperm)
        '(comps: (125985 125985))
        '(inv: 125988)
        '(y: "The number of cycles, fixed points, maximum cycle sizes and LCM's of all cycle sizes in range [A014137(n-1)..A014138(n-1)] of this permutation are given by A126294, A126295, A126296 and A126297. The fixed points are given by A126298/A126299.")
  )

  (list 125988 "Signature-permutation of the square of A125986."
        '(off: 0)
        '(indentries: Catsigperm)
        '(comps: (125986 125986))
        '(inv: 125987)
  )


  (list 126291 "Number of cycles in range [A014137(n-1)..A014138(n-1)] of permutation A125985/A125986."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
        '(c: "The number of orbits (equivalence classes) to which Vaille's automorphism partitions the set of A000108(n) Dyck paths of semilength n.")
        '(y: "A126294(2n) = 2*a(2n) for n>0.")
  )

  (list 126292 "Maximum cycle size in range [A014137(n-1)..A014138(n-1)] of permutation A125985/A125986."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
        '(c: "The size of a largest orbit to which Vaille's automorphism partitions the set of A000108(n) Dyck paths of semilength n.") 
        '(y: "For n>0, it seems that a(2n) = 2*A126296(2n).")
  )

  (list 126293 "Least common multiple of all cycle sizes in range [A014137(n-1)..A014138(n-1)] of permutation A125985/A125986."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
        '(y: "Cf. A126297.")
  )

  (list 126294 "Number of cycles in range [A014137(n-1)..A014138(n-1)] of permutation A125987/A125988."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
        '(c: "The number of orbits (equivalence classes) to which the square of Vaille's automorphism partitions the set of A000108(n) Dyck paths of semilength n.")
        '(y: "a(2n) = 2*A126291(2n) for n>0.")
  )

  (list 126295 "Number of fixed points in range [A014137(n-1)..A014138(n-1)] of permutation A125987/A125988."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
        '(y: "Fixed points themselves are given in A126298/A126299.")
  )

  (list 126296 "Maximum cycle size in range [A014137(n-1)..A014138(n-1)] of permutation A125987/A125988."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
        '(c: "The size of a largest orbit to which Vaille's automorphism partitions the set of A000108(n) Dyck paths of semilength n.") 
        '(y: "For n>0, it seems that A126292(2n) = 2*a(2n).")
  )

  (list 126297 "Least common multiple of all cycle sizes in range [A014137(n-1)..A014138(n-1)] of permutation A125987/A125988."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
        '(y: "Cf. A126293.")
  )

  (list 126298 "Fixed points of the permutation A125987/A125988."
        '(off: 0)
        '(upto: 10) ;; We add the rest manually.
        '(keywords: "more")
        '(c: "Those i, for which A125988(i)=i. A126299 shows the same fixed points using factorial code as employed in Vaille's paper.")
        '(y: "Superset of A126300.")
  )

  (list 126299 "Factorial code for the fixed points of the square of Vaille's 1997 bijection on Dyck paths."
        '(off: 0)
        '(upto: 20) ;; We add the rest manually.
        '(keywords: "base")
        '(c: "After n=... the terms can not anymore be presented with decimal numbers.")
        '(comps: (71158 126298))
        '(y: "Superset of A126301.")
  )

  (list 126300 "Fixed points of the permutation A125985/A125986."
        '(off: 0)
        '(upto: 3) ;; We add the rest manually.
        '(c: "Those i, for which A125987(i)=i. A126301 shows the same fixed points using factorial code as employed in Vaille's paper.")
        '(y: "Subset of A126298. Cf. A126295.")
  )

  (list 126301 "Factorial code for the fixed points of Vaille's 1997 bijection on Dyck paths."
        '(off: 0)
        '(upto: 3) ;; We add the rest manually, if we have time.
        '(keywords: "base")
        '(c: "Vaille gives the terms 23211, 2432211 and 2354543212221 on the last page of the 1997 paper.")
        '(comps: (71158 126300))
        '(y: "Subset of A126299. Cf. A126295.")
  )

 )
)


;; (output-entries-to-file120_45 seqs-January2007 "./seqs/January2007.txt" "Jan XX 2007")


(define seqs-January2007
 (list 


  (list 126312 "Fixed points of the permutation A071661/A071662."
        '(off: 0)
        '(upto: 55)
        '(c: "Those i, for which A071661(i)=i, i.e. for which A057163(A057164(i)) = A057164(A057163(i)). These seem to consist of just those general plane trees which are symmetric and will stay symmetric also after the underlying plane binary tree has been reflected, i.e. for which A057164(i)=i and A057164(A057163(i)) = A057163(i). Cf. comments at A123050 and A080070. The sequence seems to give also the fixed points of the permutation A125977/A125978, although the latter are not conjugates of A071661/A071662."
         )
  )

  (list 126313 "Signature-permutation of a Catalan automorphism: composition of A069772 and A125976."
        '(off: 0)
        '(indentries: Catsigperm)
        '(c: "Like A069771, A069772, A125976 and A126315/A126316, this automorphism keeps symmetric Dyck paths symmetric, but not necessarily same.")
        '(inv: 126314)
        '(comps: (69772 125976) (126290 69772) (126315 57164))
        '(y: "The number of cycles, number of fixed points, maximum cycle sizes and LCM's of all cycle sizes in range [A014137(n-1)..A014138(n-1)] of this permutation are given by A127277, A127278, A127279 and A127280. The fixed points are given by A127306. Note the curiosity: this automorphism partitions the A000108(8) = 1430 Catalan structures of size eight (e.g. Dyck paths of length 16) into 79 equivalence classes, of which the largest contains 79 members.")
  )

  (list 126314 "Signature-permutation of a Catalan automorphism: composition of A125976 and A069772."
        '(off: 0)
        '(indentries: Catsigperm)
        '(c: "Like A069771, A069772, A125976 and A126315/A126316, this automorphism keeps symmetric Dyck paths symmetric, but not necessarily same.")
        '(inv: 126313)
        '(comps: (125976 69772) (69772 126290) (57164 126316))
  )

  (list 126315 "Signature-permutation of a Catalan automorphism: composition of A069771 and A125976."
        '(off: 0)
        '(indentries: Catsigperm)
        '(c: "Like A069771, A069772, A125976 and A126313/A126314, this automorphism keeps symmetric Dyck paths symmetric, but not necessarily same.")
        '(inv: 126316)
        '(comps: (69771 125976) (126290 69771) (126313 57164))
        '(y: "The number of cycles, number of fixed points and maximum cycle sizes in range [A014137(n-1)..A014138(n-1)] of this permutation are given by A127281, A127282 and A127283. See also the comment at A127280.")

  )

  (list 126316 "Signature-permutation of a Catalan automorphism: composition of A125976 and A069771."
        '(off: 0)
        '(indentries: Catsigperm)
        '(c: "Like A069771, A069772, A125976 and A126313/A126314, this automorphism keeps symmetric Dyck paths symmetric, but not necessarily same.")
        '(inv: 126315)
        '(comps: (125976 69771) (69771 126290) (57164 126314))
  )

  (list 126317 "Number of cycles in range [A014137(n-1)..A014138(n-1)] of permutation A125977/A125978."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
  )

  (list 126318 "Maximum cycle size in range [A014137(n-1)..A014138(n-1)] of permutation A125977/A125978."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
  )

  (list 126319 "Least common multiple of all cycle sizes in range [A014137(n-1)..A014138(n-1)] of permutation A125977/A125978."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
  )

  (list 126320 "Signature-permutation of a Catalan automorphism: A057163 conjugated by A057164."
        '(off: 0)
        '(indentries: Catsigperm)
        '(comps: (57164 57163 57164))
        '(y: "A069787.")
        '(inv: 126320)
  )

  (list 127277 "Number of cycles in range [A014137(n-1)..A014138(n-1)] of permutation A126313/A126314."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
  )

  (list 127278 "Number of fixed points in range [A014137(n-1)..A014138(n-1)] of permutation A126313/A126314."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
        '(y: "Fixed points themselves are given in A127306.")
  )

  (list 127279 "Maximum cycle size in range [A014137(n-1)..A014138(n-1)] of permutation A126313/A126314."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
  )

  (list 127280 "Least common multiple of all cycle sizes in range [A014137(n-1)..A014138(n-1)] of permutation A126313/A126314."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
        '(c: "The sequence seems to give the least common multiples also for the permutation A126315/A126316, but with a(3)=6 instead of 3.")
  )

  (list 127281 "Number of cycles in range [A014137(n-1)..A014138(n-1)] of permutation A126315/A126316."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
  )

  (list 127282 "Number of fixed points in range [A014137(n-1)..A014138(n-1)] of permutation A126315/A126316."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
  )

  (list 127283 "Maximum cycle size in range [A014137(n-1)..A014138(n-1)] of permutation A126315/A126316."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
  )

  (list 127285 "Signature-permutation of a Catalan automorphism: SPINE-transformation of *A057508."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append "SPINE-transformation is explained in A122203."
 " Selitä Church-numeraaleista, ym. Toplevel of the list is permuted as follows:"
 " When applied to a list of integers (1 ... 2n), this automorphism produces permutation"
 " (2n 1 2n-1 2 2n-3 3 ... n+1 n), and when applied to a list (1 .. 2n+1), produces permutation"
 " (2n+1 1 2n 2 2n-1 3 ... n n+1). For example,"
 " the lists of integers (1), (1,2), ..., (1,2,3,4,5,6,7,8) are permuted as follows:"
 " (1) (2,1) (3,1,2) (4,1,3,2) (5,1,4,2,3) (6,1,5,2,4,3) (7,1,6,2,5,3,4) (8,1,7,2,6,3,5,4)."
 " Used to construct A127287 and A127289."
                  )
        )
        '(inv: 127286)
        '(comps: (127287 57508))
  )

  (list 127286 "Signature-permutation of a Catalan automorphism: ENIPS-transformation of *A057508."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append "ENIPS-transformation is explained in A122204."
 " When applied to a list of integers (1 ... 2n), this automorphism produces permutation"
 " (2 4 6 ... 2n-2 2n 2n-1 2n-3 ... 5 3 1),"
 " and when applied to a list (1 .. 2n+1), produces permutation"
 " (2 4 6 ... 2n-2 2n 2n+1 2n-1 2n-3 ... 5 3 1)."
 " For example, the lists of integers (1), (1,2), ..., (1,2,3,4,5,6,7,8) are permuted as follows:"
 " (1) (2,1) (2,3,1) (2,4,3,1) (2,4,5,3,1) (2,4,6,5,3,1) (2,4,6,7,5,3,1) (2,4,6,8,7,5,3,1)."
 " Used to construct A127288."
                  )
        )
        '(inv: 127285)
        '(comps: (57508 127288))
  )

  (list 127287 "Signature-permutation of a Catalan automorphism: composition of A127285 and A057508."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append
 "When applied to a list of integers (1 ... 2n), this automorphism produces permutation"
 " (1 2n 2 2n-1 3 2n-3  ... n n+1), and when applied to a list (1 .. 2n+1), produces permutation"
 " (1 2n+1 2 2n 3 2n-1 ... n n+1). For example,"
 " the lists of integers (1), (1,2), ..., (1,2,3,4,5,6,7,8) are permuted as follows:"
 " (1) (1,2) (1,3,2) (1,4,2,3) (1,5,2,4,3) (1,6,2,5,3,4) (1,7,2,6,3,5,4) (1,8,2,7,3,6,4,5)."
 " Used to construct A127291."
                  )
        )
        '(inv: 127288)
        '(comps: (127285 57508))
  )

  (list 127288 "Signature-permutation of a Catalan automorphism: composition of A057508 and A127286."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append
 "When applied to a list of integers (1 ... 2n), this automorphism produces permutation"
 " (1 3 5 2n-1 ... 2n 2n-2 ... 4 2), and when applied to a list (1 .. 2n+1), produces permutation"
 " (1 3 5 2n-1 2n+1 2n 2n-2 ... 4 2). For example,"
 " the lists of integers (1), (1,2), ..., (1,2,3,4,5,6,7,8) are permuted as follows:"
 " (1) (1,2) (1,3,2) (1,3,4,2) (1,3,5,4,2) (1,3,5,6,4,2) (1,3,5,7,6,4,2) (1,3,5,7,8,6,4,2)."
                  )
        )
        '(inv: 127287)
        '(comps: (57508 127286))
  )

  (list 127289 "Signature-permutation of a Catalan automorphism: composition of A127291 and A057164."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append "This is otherwise like A127291, but uses *A127285 instead of *A127287"
                    " as the picker permutation for the function \"tau\", which can be found"
                    " in the entry A127291. A014486->parenthesization is given in A014486."
                  )
        )
        (list 'y: (string-append
" The number of cycles, maximum cycle sizes and LCM's of all cycle sizes"
" in range [A014137(n-1)..A014138(n-1)] of this permutation are given by"
" A127296, A127297 and A127298."
                  )
        )
        '(inv: 127290)
        '(comps: (127291 57164) (57164 127299))
        '(scheme: "(define (A127289 n) (tau (A014486->parenthesization (A014486 n)) *A127285!))")
  )

  (list 127290 "Signature-permutation of a Catalan automorphism: composition of A057164 and A127292."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 127289)
        '(comps: (57164 127292) (127300 57164))
  )

  (list 127291 "Signature-permutation of Elizalde's and Deutsch's 2003 bijection for Dyck paths."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append
         "Deutsch and Elizalde show in their paper that this automorphism converts"
         " certain properties concerning \"tunnels\" of Dyck path, to another"
         " set of properties concerning the number of hills, even and odd rises, as well as"
         " the number of returns (A057515), thus proving the equidistribution"
         " of the said parameters."
         "\n%C A127291 This automorphism is implemented with function \"tau\""
         " (Scheme code given below) that takes as its arguments an S-expression and"
         " a Catalan automorphism that permutes only the top level of the list"
         " (i.e. the toplevel branches of a general tree, or the whole arches of a Dyck path)"
         " and thus, when the permuting automorphism is applied to a list (parenthesization)"
         " of length n, it induces some permutation of [1..n]."
;;       " (The integers could also be represented in some notation resembling Church numerals,"
;;       " e.g. as () = 0, (()) = 1, ((())) = 2, etc.)"
         " This automorphism is induced in that manner by the automorphism *A127287,"
         " and likewise, *A127289 is induced by *A127285, *A057164 by *A057508,"
         " *A057501 by *A057509 and *A057502 by *A057510."
         " Note that so far these examples satisfy the homomorphism condition, e.g."
         " as *A127287 = *A127285 o *A057508 so is *A127291 = *A127289 o *A057164."
         " and likewise, as *A057510 = *A057508 o *A057509 o *A057508,"
         " so is *A057502 = *A057164 o *A057501 o *A057164."
         " However, it remains open what is the exact criteria of the \"picking automorphism\""
         " and the corresponding permutation, that this method would induce a bijection."
         " For example, if we give *A127288 (the inverse of *A127287) to function \"tau\""
         " it will not induce *A127292, and actually not a bijection at all."
         " Instead, we have to compute the inverse of this"
         " automorphism with another, more specific algorithm given in A127300."
                  )
        )
        '(inv: 127292)
        (list 'y: (string-append "A127291(A057548(n)) = A072795(A127291(n))"
" A127291(A072795(n)) = A127307(A127291(A057502(n))) for all n >= 1."
" The number of cycles, maximum cycle sizes and LCM's of all cycle sizes"
" in range [A014137(n-1)..A014138(n-1)] of this permutation are given by"
" A127293, A127294 and A127295. Number of fixed ponts begins as 1,1,0,0,0,1,0,0,0,0,0,1,0,0,..."
                  )
        )
        '(comps: (127289 57164) (57164 127299 57164))
        (list 'scheme: (string-append
"(define (A127291 n) (tau (A014486->parenthesization (A014486 n)) *A127287!))"
" ;;  A014486->parenthesization is given in A014486."
"\n%o A127291 (define (tau s permuter) (let* ((sper (transpos-list->permvec (sexp->kk s))) (visivec (make-vector (vector-length sper) ()))) (let loop ((tper (if (null? s) s (permuter (iota0 (-1+ (vector-length sper)))))) (s 0)) (cond ((null? tper) (A080300 s)) (else (let ((x (vector-ref sper (car tper)))) (cond ((not (vector-ref visivec x)) (vector-set! visivec (car tper) #t) (loop (cdr tper) (+ s s 1))) (else (loop (cdr tper) (+ s s))))))))))"
"\n%o A127291 (define (transpos-list->permvec tplist) (let ((permvec (make-initialized-vector (* 2 (length tplist)) (lambda (i) i)))) (let loop ((tplist tplist)) (cond ((null? tplist) permvec) (else (let* ((tp (car tplist)) (x (car tp)) (y (cdr tp)) (temp (vector-ref permvec x))) (vector-set! permvec x (vector-ref permvec y)) (vector-set! permvec y temp) (loop (cdr tplist)))))))) ;; Converts a list of transpositions to a permutation vector [0..(n-1)]"
"\n%o A127291 (define (iota0 upto_n) (let loop ((n upto_n) (result (list))) (cond ((zero? n) (cons 0 result)) (else (loop (- n 1) (cons n result)))))) ;; (iota0 5) gives (0 1 2 3 4 5)"
"\n%o A127291 (define (sexp->kk p) (let ((c (list (list))) (maxnode (list -1))) (let recurse ((p p)) (cond ((pair? p) (let ((this-trans (cons (1+ (car maxnode)) 0))) (set-car! maxnode (1+ (car maxnode))) (attach! this-trans c) (recurse (car p)) (set-car! maxnode (1+ (car maxnode))) (set-cdr! this-trans (car maxnode)) (recurse (cdr p)))))) (cdr (reverse! c)))) ;; Converts a symbolless S-expression to a list of noncrossing transpositions in a standard way."
"\n%o A127291 (define (attach! elem lista) (set-cdr! lista (cons (car lista) (cdr lista))) (set-car! lista elem) lista) ;; Attaches an element physically to the front of non-empty list."
                       )
        )
  )

  (list 127292 "Signature-permutation of the inverse of Elizalde's and Deutsch's 2003 bijection for Dyck paths."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append "Note that this automorphism cannot be produced just by giving"
" A127288 (the inverse of A127287) to function \"tau\" given in A127291."
" Instead, we have to use another algorithm given in A127300."
                  )
        )
        '(inv: 127291)
        '(comps: (57164 127290) (57164 127300 57164))
  )

;; n=13: ccs=(1 1 1 2 3 6 8 8 9 10 8 14 18 10) fcs=() mcs=(1 1 2 3 6 21 80 255 965 3349 9366 30793 80798 396492) lcs=(1 1 2 6 30 1260 1680 19825740 10028280 182416547040 271239404020200 219240769050559711332360 6467876945923041743744426827092900 27867228909478820644943875389480)

  (list 127293 "Number of cycles in range [A014137(n-1)..A014138(n-1)] of permutation A127291/A127292."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
  )

  (list 127294 "Maximum cycle size in range [A014137(n-1)..A014138(n-1)] of permutation A127291/A127292."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
  )

  (list 127295 "Least common multiple of all cycle sizes in range [A014137(n-1)..A014138(n-1)] of permutation A127291/A127292."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
  )

  (list 127296 "Number of cycles in range [A014137(n-1)..A014138(n-1)] of permutation A127289/A127290."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
  )

  (list 127297 "Maximum cycle size in range [A014137(n-1)..A014138(n-1)] of permutation A127289/A127290."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
  )

  (list 127298 "Least common multiple of all cycle sizes in range [A014137(n-1)..A014138(n-1)] of permutation A127289/A127290."
        '(off: 0)
        '(upto: 8) ;; We add the rest manually.
  )

  (list 127299 "Signature-permutation of A057164-conjugate of Elizalde's and Deutsch's 2003 bijection for Dyck paths."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 127300)
        '(comps: (57164 127291 57164) (57164 127289))
        '(scheme: "(define (A127299 n) (A057164 (A127291 (A057164 n))))")
  )

  (list 127300 "Signature-permutation of A057164-conjugate of the inverse of Elizalde's and Deutsch's 2003 bijection for Dyck paths."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append "Used to construct the inverse for A127291."
                  )
        )
        '(inv: 127299)
        '(comps: (57164 127292 57164) (127290 57164))
        (list 'scheme: (string-append
"(define (A127300 n) (A080300 (transpos-list->A014486 (A127300-aux1 (A014486 n)))))"
"\n%o A127300 (define (transpos-list->A014486 tplist) (fold-left (lambda (s p) (+ s (expt 2 (max (car p) (cdr p))))) 0 tplist))"
"\n%o A127300 (define (a127300-aux1 n) (if (zero? n) (list) (let loop ((n n) (tplist1 (list)) (tplist2 (list)) (i 0) (j (A000523 n)) (b 1)) (cond ((zero? n) (append tplist1 tplist2)) ((even? n) (loop (/ n 2) tplist2 (cons (cons '() i) tplist1) j (+ i b) (- b))) ((assq '() tplist1) => (lambda (p) (set-car! p i) (loop (/ (- n 1) 2) tplist2 tplist1 j (+ i b) (- b)))) ((rassq '() tplist2) => (lambda (p) (set-car! p i) (loop (/ (- n 1) 2) tplist2 tplist1 j (+ i b) (- b)))) (else (error \"n not in A014486!\"))))))"
" ;; Returns a list of non-crossing transpositions."
"\n%o A127300 (define (rassq key al) (let loop ((al al) (last-found #f)) (cond ((null? al) last-found) ((eq? (caar al) key) (loop (cdr al) (car al))) (else (loop (cdr al) last-found)))))"
" ;; (rassq key al) is essentially the same as: (assq key (reverse al))"
                       )
        )
  )


  (list 127301 "Matula-Goebel signature for plane general trees encoded by A014486."
        '(off: 0)
        '(upto: 196)
        (list 'c: (string-append "This sequence hides a morphism that converts"
 " A000108(n) oriented (plane) rooted general trees encoded in range [A014137(n-1)..A014138(n-1)]"
 " of A014486 "
 " to A000081(n+1) non-oriented rooted general trees, encoded by their Matula-Goebel numbers."
 " The latter encoding is explained in A061773."
 "\n%C A127301 If the signature-permutation of a Catalan automorphism SP satisfies"
 " the condition a(SP(n)) = a(n) for all n, then it"
 " preserves the non-oriented form of a general tree, which implies also"
 " that it is Lukasiewicz-word permuting."
;; " The cycle-count sequence of such an automorphism is guaranteed to be"
;; " genuinely monotone from n >= 2 onward,"
;; " because A000081 is genuinely monotone from n >= 3 onward."
;; Would be true if A000081(n+2) > A000081(n+1), AND A000081(n+2) > A000108(n)
;; which is not, as already A000081(8)-A000108(6) = 115-132 = -17

 " Examples of such automorphisms include"
 " *A072796, *A057508 *A057509/*A057510, *A057511/*A057512, *A057164, *A127285/*A127286"
 " and *A127287/*A127288."
                  )
        )
       '(comps: (127301 072796) (127301 057508) (127301 057509) (127301 057510) (127301 057511)
                (127301 057512) (127301 057164) (127301 127285) (127301 127286) (127301 127287)
                (127301 127288)
;;              (127301 127388) ;; For checking... should fail.
        )
        (list 'e: (string-append
          "A000081(n+1) distinct values occur each range [A014137(n-1)..A014138(n-1)]."
" As an example, A014486(5) = 44 (= 101100 in binary = A063171(5)), encodes the following plane tree:"
"\n%e A127301 .....o"
"\n%e A127301 .....|"
"\n%e A127301 .o...o"
"\n%e A127301 ..\\./."
"\n%e A127301 ...*.."
"\n%e A127301 Matula-Goebel encoding for this tree gives a code number A000040(1) * A000040(A000040(1)) = 2*3 = 6, thus a(5)=6."
"\n%e A127301 Likewise, A014486(6) = 50 (= 110010 in binary = A063171(6)) encodes the plane tree:"
"\n%e A127301 .o"
"\n%e A127301 .|"
"\n%e A127301 .o...o"
"\n%e A127301 ..\\./."
"\n%e A127301 ...*.."
"\n%e A127301 Matula-Goebel encoding for this tree gives a code number A000040(A000040(1)) * A000040(1)"
" = 3*2 = 6, thus a(6) is also 6, which shows these two trees are identical if one ignores their"
" orientation."
                  )
        )

        (list 'y: (string-append
          "a(A014138(n)) = A007097(n+1), a(A014137(n)) = A000079(n+1). Cf. A127302."
                  )
        )

;; Numbers that occur only once in this sequence are given by the
;; sequence with the following definition:
;; 1 occurs. if the term t occurs, then also all powers k>=1 of A000040(t)^k occur as well.

;; If we take distinct values, in the order of their appearance, we get some
;; permutation of natural numbers. Already in OEIS? No (-1+ from each) Inverse?

        (list 'scheme: (string-append
;;  "(define A127301 (compose-funs *A127301 A014486->parenthesization A014486)) ;; A014486->parenthesization given in A014486."
  "(define (A127301 n) (*A127301 (A014486->parenthesization (A014486 n)))) ;; A014486->parenthesization given in A014486."
  "\n%o A127301 (define (*A127301 s) (if (null? s) 1 (fold-left (lambda (m t) (* m (A000040 (*A127301 t)))) 1 s)))"
                       )
        )
  )

  (list 127302 "Matula-Goebel signature for plane binary trees encoded by A014486."
        '(off: 0)
        '(upto: 64)
        '(f: "a(n) = A127301(A057123(n)).")
        (list 'c: (string-append "This sequence hides a morphism that converts"
 " A000108(n) oriented (plane) rooted binary trees encoded in range [A014137(n-1)..A014138(n-1)]"
 " of A014486 "
 " to A001190(n+1) non-oriented rooted binary trees, encoded by their Matula-Goebel numbers"
 " (when viewed as a subset of non-oriented rooted general trees)."
 " See comments as A127301."
 "\n%C A127301 If the signature-permutation of a Catalan automorphism SP satisfies"
 " the condition a(SP(n)) = a(n) for all n, then it"
 " preserves the non-oriented form of a binary tree."
;; Bullshit:
;; " The cycle-count sequence of such an automorphism is guaranteed to be"
;; " genuinely monotone from n >= 3 onward,"
;; " because A001190 is genuinely monotone from n >= 4 onward."
 " Examples of such automorphisms include"
 " *A069770, *A057163, *A122351, *A069767/*A069768, *A073286-*A073289, *A089854, *A089859/*A089863, *A089864, *A122282, *A127377-*A127382, *A127387, *A127388."
                  )
        )
       '(comps: (127302 069770) (127302 057163) (127302 122351) (127302 069767) (127302 069768)
                (127302 073286) (127302 073287) (127302 073288) (127302 073289) (127302 089854)
                (127302 089859) (127302 089863)
                (127302 089864) (127302 122282) (127302 127377) (127302 127378) (127302 127379)
                (127302 127380) (127302 127387) (127302 127388)
        )

        (list 'e: (string-append
          "A001190(n+1) distinct values occur each range [A014137(n-1)..A014138(n-1)]."
" As an example, terms 014486(4..8) encode the following five plane binary trees:"
"\n%e A127302 ........\\/.....\\/.................\\/.....\\/..."
"\n%e A127302 .......\\/.......\\/.....\\/.\\/.....\\/.......\\/.."
"\n%e A127302 ......\\/.......\\/.......\\_/.......\\/.......\\/."
"\n%e A127302 n=.....4........5........6........7........8.."
"\n%e A127302 The trees in positions 4, 5, 7 and 8 all produce Matula-Goebel number"
" A000040(1)*A000040(A000040(1)*A000040(A000040(1)*A000040(1)))"
" = 2*A000040(2*A000040(2*2)) = 2*A000040(14) = 2*43 = 86, as they are just"
" different planar representations of the one and same non-oriented tree."
" The tree in position 6 produces Matula-Goebel number"
" A000040(A000040(1)*A000040(1)) * A000040(A000040(1)*A000040(1))"
" = A000040(2*2) * A000040(2*2) = 7*7 = 49. Thus a(4..8) = 86,86,49,86,86."

                  )
        )
  )

;; 0, 1, 1, 1, 2,
;; 1; 4; 14 14; 86 86 49 86 86;

  (list 127306 "Fixed points of the permutation A126313/A126314."
        '(off: 0)
        '(upto: 14)
        '(c: "Those i, for which A126313(i)=i. Cf. A126312, A127278.")
  )

  (list 127307 "Positions of Dyck words beginning as UUD (110) in A014486/A063171."
        '(off: 1)
        '(f: "a(n) = A072764bi(A072795(A072771(n)),A072772(n)).")
        (list 'scheme: (string-append
            "(define (A127307 n) (A072764bi (A072795 (A072771 n)) (A072772 n)))"
            "\n%o A127307 (define (*A127307 s) (cons (cons '() (car s)) (cdr s))) ;; Corresponding function acting on S-expressions."
                       )
        )
  )


  (list 127377 "Signature-permutation of a Catalan automorphism, auxiliary bijection for Callan's 2006 bijection on Dyck Paths."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append "Used to construct *A127379."
                  )
        )
        (list 'y: (string-append
" The number of cycles and the number of fixed points"
" in range [A014137(n-1)..A014138(n-1)] of this permutation are given by"
" A127383 and A127389."
" The maximum cycles and LCM's of cycle sizes begin as 1,1,2,4,4,8,8,8,8,16,16,16,16,16,..."
" A127387 shows a variant which is an involution."
" A127302(a(n)) = A127302(n) holds for all n."
                  )
        )
        '(inv: 127378)
        '(scheme: "(define (*A127377! s) (cond ((null? s)) ((null? (car s)) (*A069770! s) (*A127377! (car s))) ((null? (cdr s)) (*A069770! s) (*A127379! (cdr s))) (else (*A127379! s))) s)")
  )

  (list 127378 "Signature-permutation of a Catalan automorphism, inverse of A127377."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append "Used to construct *A127380."
                  )
        )
        (list 'y: (string-append "A127302(a(n)) = A127302(n) holds for all n."
                  )
        )
        '(inv: 127377)
        '(scheme: "(define (*A127378! s) (cond ((null? s)) ((null? (cdr s)) (*A069770! s) (*A127378! (cdr s))) ((null? (car s)) (*A069770! s) (*A127380! (car s))) (else (*A127380! s))) s)")
  )

  (list 127379 "Signature-permutation of Callan's 2006 bijection on Dyck Paths, mirrored version (A057164-conjugate)."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append
 "It's much easier to implement Callan's 2006 bijection for S-expressions if the graphical description"
 " given by Callan is first mirrored."
 " Then this automorphism is just RIBS-transformation (explained in A122200) of the automorphism"
 " A127377, and Callan's original variant A127381 is obtained as A057164(A127379(A057164(n)))."
                  )
        )
        (list 'y: (string-append
" The number of cycles and the number of fixed points"
" in range [A014137(n-1)..A014138(n-1)] of this permutation are given by"
" A127384 and A086625 shifted once right (as shown by Callan)."
" The maximum cycles and LCM's of cycle sizes begin as 1,1,1,2,4,4,8,8,8,8,16,16,16,16,..."
" A127302(a(n)) = A127302(n) holds for all n."
" A127388 shows a variant which is an involution."
                  )
        )
        '(inv: 127380)
        '(comps: (57164 127381 57164))
        '(scheme: "(define (*A127379! s) (for-each *A127377! s) s)")
  )

  (list 127380 "Signature-permutation of the inverse of Callan's 2006 bijection on Dyck Paths, mirrored version (A057164-conjugate)."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append
 "This automorphism is RIBS-transformation (explained in A122200) of the automorphism"
 " A127378, and Callan's original variant A127382 is obtained as A057164(A127380(A057164(n)))."
                  )
        )
        '(inv: 127379)
        '(comps: (57164 127382 57164))
        '(scheme: "(define (*A127380! s) (for-each *A127378! s) s)")
  )

  (list 127381 "Signature-permutation of Callan's 2006 bijection on Dyck Paths."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append
 "This is easiest to implement for S-expressions as A057164-conjugate of A127379."
                  )
        )
        (list 'y: (string-append
" The number of cycles and the number of fixed points"
" in range [A014137(n-1)..A014138(n-1)] of this permutation are given by"
" A127384 and A086625 shifted once right (as shown by Callan in his paper)."
" The maximum cycles and LCM's of cycle sizes begin as 1,1,1,2,4,4,8,8,8,8,16,16,16,16,..."
" A127302(a(n)) = A127302(n) holds for all n."
                  )
        )
        '(inv: 127382)
        '(comps: (57164 127379 57164))
  )

  (list 127382 "Signature-permutation of the inverse of Callan's 2006 bijection on Dyck Paths."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append "A127302(a(n)) = A127302(n) holds for all n."

                  )
        )
        '(inv: 127381)
        '(comps: (57164 127380 57164))
  )

  (list 127387 "Signature-permutation of a Catalan automorphism, a variant of A127377."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append "Used to construct *A127388."
                  )
        )
        (list 'y: (string-append
" The number of cycles and the number of fixed points"
" in range [A014137(n-1)..A014138(n-1)] of this involution are given by"
" A127385 and A127389. (This automorphism has the same fixed points as A127377/A127378.)"
" A127302(a(n)) = A127302(n) holds for all n."
                  )
        )
        '(inv: 127387)
        '(scheme: "(define (*A127387! s) (cond ((null? s)) ((null? (car s)) (*A069770! s) (*A127387! (car s))) ((null? (cdr s)) (*A069770! s) (*A127387! (cdr s))) (else (*A127388! s))) s)")
  )

  (list 127388 "Signature-permutation of a Catalan automorphism, a variant of A127379."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append
 "This automorphism is RIBS-transformation (explained in A122200) of the automorphism"
 " A127387."
                  )
        )
        (list 'y: (string-append
" The number of cycles and the number of fixed points"
" in range [A014137(n-1)..A014138(n-1)] of this involution are given by A127386 and"
" A086625 shifted once right (this automorphism has the same fixed points as A127379 and A127381)."
" A127302(a(n)) = A127302(n) holds for all n."
                  )
        )
        '(inv: 127388)
        '(scheme: "(define (*A127388! s) (for-each *A127387! s) s)")
  )

  (list 127383 "Number of cycles in range [A014137(n-1)..A014138(n-1)] of permutation A127377/A127378."
        '(off: 0)
        '(upto: 9) ;; We add the rest manually.
  )

  (list 127384 "Number of cycles in range [A014137(n-1)..A014138(n-1)] of permutations A127379/A127380 and A127381/A127382."
        '(off: 0)
        '(upto: 9) ;; We add the rest manually.
  )

  (list 127385 "Number of cycles in range [A014137(n-1)..A014138(n-1)] of permutations A127387."
        '(off: 0)
        '(upto: 9) ;; We add the rest manually.
  )

  (list 127386 "Number of cycles in range [A014137(n-1)..A014138(n-1)] of permutations A127388."
        '(off: 0)
        '(upto: 9) ;; We add the rest manually.
  )

  (list 127389 "Number of fixed points in range [A014137(n-1)..A014138(n-1)] of permutations A127377/A127378 and A127387."
        '(off: 0)
        '(upto: 9) ;; We add the rest manually.
        (list 'y: (string-append "This is INVERTi transform of A086625 (appropriately shifted)."
" E.g. INVERT([1,1,0,1,2,4,10,23,56,138,344,870,2220,5716]) gives:"
" 1,2,3,6,12,26,59,138,332,814,2028,5118,... (beginning of A086625)"
                  )
        )
  )


 )
)


;; (output-entries-to-file120_45 seqs-2007May1 "./seqs/2007May1.txt" "May 01 2007")

(define seqs-2007May1
 (list 

  (list 129594 "Involution of nonnegative integers induced by the conjugation of the partition encoded in the run lengths of binary expansion of n."
        '(off: 0)
        '(indentries: Nperm)
        '(inv: 129594)
        '(y: "a(n) = A075158(A122111(1+A075157(n)) - 1)")
        (list 'scheme: (string-append
            "(define (A129594 n) (if (zero? n) n (ascpart_to_binexp (conjugate-partition (binexp_to_ascpart n)))))"
            "\n%o A129594 (define (conjugate-partition ascpart) etc..."
                       )
        )        

  )

  (list 129595 "Array A(i,j): A(1,1), A(2,1), A(1,2), A(3,1), A(2,2), A(1,3), ... of elementwise sums of partitions encoded in the prime-factorizations of i and j."
        '(off: 1)
        '(keywords: "tabl")
        '(c: "Marc LeBrun describes this encoding of partitions in his January 11 2006 message on SeqFan mailing list. A122111 gives the involution of natural numbers induced when partition conjugation is applied to the same encoding.")
        '(y: "A129593, A129596, A129597, A129598.")
  )

  (list 129596 "Table T(i,j): T(1,1), T(2,1), T(2,2), T(3,1), T(3,2), T(3,3), ... of elementwise sums of partitions encoded in the prime-factorizations of i and j."
        '(off: 1)
        '(keywords: "tabl")
        '(c: "This is the upper triangular region of array A129595 (or equally, the lower triangular region, because A129595 is symmetric).")
  )

  (list 129597 "Central diagonal of array A129595."
        '(off: 1)
        '(y: "a(n) = A129595bi(n,n)")
  )

  (list 129598 "a(n) = n * A111089(n)."
        '(off: 1)
        '(y: "Row 2 of A129595. Note the similarity with A050399 & A072995, except that a(15)=75 and a(30)=150 (instead of ...)")
  )

  (list 129593 "Prime-factorization encoded partition code for the Lukasiewicz-word."
        '(off: 0)
        '(comps: (129593 072797)
                (129593 072796) (129593 057508) (129593 057509) (129593 057510) (129593 057511)
                (129593 057512) (129593 057164) (129593 127285) (129593 127286) (129593 127287)
                (129593 127288)
                (129593 127388) ;; For checking... should fail.
         )
        (list 'c: (string-append
           "One is added to each element of the Lukasiewicz-word of a general plane tree encoded"
           " by A014486(n) (with the last leaf not marked, as shown in A071153)"
           " which is then sorted, and viewed as a partition of a natural number, is encoded"
           " in the manner explained in A129595."
           " 1,20,11,300,201,210,120,111,4000,3001,3010, etc."
           " blaa blaa: A072797, A127301"
           "A000041(n) distinct values occur in each range [A014137(n-1)..A014138(n-1)]."
                  )
        )
        '(y: "A129599")
  )

  (list 129599 "Prime-factorization encoded partition code for the Lukasiewicz-word."
        '(off: 0)
        '(comps: (129599 072797) (129599 057501) (129599 057502) (129599 069771) (129599 069772)
                (129599 072796) (129599 057508) (129599 057509) (129599 057510) (129599 057511)
                (129599 057512) (129599 057164) (129599 127285) (129599 127286) (129599 127287)
                (129599 127288)
                (129599 127388) ;; For checking... should fail.
         )

        (list 'c: (string-append
           "One is added to each element, except to the first (root) one, of the Lukasiewicz-word of a general plane tree encoded"
           " by A014486(n) (with the last leaf also explicitly marked, as shown in A079436)"
           " which is then sorted, and viewed as a partition of a natural number, is encoded"
           " in the manner explained in A129595."
           " 10,200,110,3000,2010,2100,1200,1110,40000,30010,30100, etc."
           " blaa blaa: A072797, A127301"
           "A000041(n) distinct values seem to occur in each range [A014137(n)..A014138(n)]."
                  )
        )
        '(y: "A129593")
  )


;; %N A003991 Multiplication table read by antidiagonals: T(i,j) = ij.
  (list 129600 "Array A(i,j): A(0,0), A(1,0), A(0,1), A(2,0), A(1,1), A(0,2), ... of binary runlength encoded products of i and j."
        '(off: 0)
        '(keywords: "tabl")
        '(c: ".")
        '(y: "A003991, A129601. Center diagonal: A129602. Row 1: apart from the first term, equal to A014601.")
        '(scheme: "(define (A129600bi col row) (A075158 (-1+ (* (1+ (A075157 col)) (1+ (A075157 row))))))\n%o A129600 (define (A129600 n) (A129600bi (A025581 n) (A002262 n)))")
  )

  (list 129601 "Table T(i,j): T(0,0), T(1,0), T(1,1), T(2,0), T(2,1), T(2,2), ... of binary runlength encoded products of i and j."
        '(off: 0)
        '(keywords: "tabl")
        '(c: "This is the upper triangular region of array A129600 (or equally, the lower triangular region, because A129600 is symmetric).")
        '(scheme: "(define (A129601 n) (A129600bi (A003056 n) (A002262 n)))")
  )

  (list 129602 "Replace in the binary expansion each run of k 0's (or 1's) with 2k-1 0's (or 1's), except in the most significant run, double the number of 0's (or 1's)."
        '(off: 0)
        '(c: "Central diagonal of array A129600.")
        '(y: "a(n) = A129600bi(n,n)")
        '(scheme: "(define (A129602 n) (if (zero? n) n (let ((rl (binexp->runcount1list n))) (runcount1list->binexp (cons (* 2 (car rl)) (map (lambda (i) (- (* 2 i) 1)) (cdr rl)))))))")
  )

  (list 129603 "Replace in the binary expansion each run of k 0's (or 1's) with 2k-1 0's (or 1's)."
        '(off: 0)
        '(y: "Variant: A129602.")
        '(scheme: "(define (A129603 n) (runcount1list->binexp (map (lambda (i) (- (* 2 i) 1)) (binexp->runcount1list n))))")
  )



 )
)


;; (output-entries-to-file120_45 seqs-2007May22 "./seqs/batch22may/2007May22.scm" "May 22 2007")

(define seqs-2007May22
 (list 
  (list 129604 "Signature-permutation of a Catalan automorphism, row 1654720 of A089840."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append "Automorphism *A069770 = FORK(*A129604) = KROF(*A129604)."
                     " See the definitions given in A122201 and A122202."
                  )
        )
        (list 'y: (string-append
" The number of cycles and the number of fixed points"
" in range [A014137(n-1)..A014138(n-1)] of this involution are given by"
" the same sequences as is the case for example with"
" A069770, A057163 and A122351,"
" that is, A007595 and zero-interspersed A000108."
                  )
        )
        '(inv: 129604)
        '(comps: (069770 089864) (089864 069770))
        (list 'scheme: (string-append 
"(define (*A129604 s) (cond ((pair? s) (cons (*A069770 (cdr s)) (*A069770 (car s)))) (else s))"
"\n(define (*A129604! s) (cond ((pair? s) (*A069770! (car s)) (*A069770! (cdr s)) (*A069770! s))) s))"
                       )
        )
  )

  (list 129605 "Signature-permutation of a Catalan automorphism, row 3613 of A089840."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append "Automorphism *A072796 = SPINE(*A129605)."
                     " See the definition given in A122203."
                  )
        )
        '(inv: 129606)
        (list 'scheme: (string-append 
"(define (*A129605 s) (cond ((> (length s) 2) (cons (cadr s) (cons (caddr s) (cons (car s) (cdddr s))))) (else (*A072796 s))))"
"\n(define (*A129605! s) (cond ((< (length s) 3) (*A072796! s)) (else (let ((org_car (car s))) (set-car! s (cadr s)) (set-car! (cdr s) (caddr s)) (set-car! (cddr s) org_car) s))))"
                       )
        )
  )

  (list 129606 "Signature-permutation of a Catalan automorphism, row 3613 of A089840."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append "Automorphism *A072796 = ENIPS(*A129606)."
                     " See the definition given in A122204."
                  )
        )
        '(inv: 129605)
        (list 'scheme: (string-append 
"(define (*A129606 s) (cond ((> (length s) 2) (cons (caddr s) (cons (car s) (cons (cadr s) (cdddr s))))) (else (*A072796 s))))"
"\n(define (*A129606! s) (cond ((< (length s) 3) (*A072796! s)) (else (let ((org_car (car s))) (set-car! s (caddr s)) (set-car! (cddr s) (cadr s)) (set-car! (cdr s) org_car) s))))"
                       )
        )
  )

  (list 129607 "Signature-permutation of a Catalan automorphism: swap the left and right subtree of degree 2 general trees"
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 129607)
        (list 'c: (string-append
                     "This is otherwise like automorphism *A072796, except that this involution"
                     " exchanges the two leftmost subtrees of a general tree ONLY when the degree"
                     " of the tree is EXACTLY two."
                     " Automorphism *A129608 = SPINE(*A129607) = ENIPS(*A129607)."
                     " See the definitions given in A122203 and A122204."
                  )
        )
        (list 'y: (string-append "Row 3608 of A089840."
                  )
        )
        (list 'scheme: (string-append 
"(define (*A129607 s) (if (= 2 (length s)) (*A072796 s) s))"
"\n(define (*A129607! s) (if (= 2 (length s)) (*A072796! s)) s)"
                       )
        )
  )

  (list 129608 "Signature-permutation of a Catalan automorphism, swap the two rightmost subtrees of general trees"
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 129608)
        (list 'c: (string-append "This self-inverse automorphism is obtained as either SPINE(*A129607) or ENIPS(*A129607)."
                     " See the definitions given in A122203 and A122204."
                  )
        )
        '(comps: (057508 72796 057508) (057164 072796 057164))
        '(y: "Row 3608 of A122203 and A122204.")
  )

  (list 129609 "Signature-permutation of a Catalan automorphism, row 65167 of A089840."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 129610)
        (list 'c: (string-append "Automorphism *A074679 = ENIPS(*A129609)."
                     " See the definition given in A122204."
                  )
        )
        (list 'scheme: (string-append 
"(define (*A129609 s) (cond ((pair? s) (*A074679 (cons (car s) (*A074680 (cdr s))))) (else s)))"
"\n(define (*A129609! s) (cond ((pair? s) (*A074680! (cdr s)) (*A074679! s))) s)"
                       )
        )
  )

  (list 129610 "Signature-permutation of a Catalan automorphism, row 65352 of A089840."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 129609)
        (list 'c: (string-append "Automorphism *A074680 = SPINE(*A129610)."
                     " See the definition given in A122203."
                  )
        )
        (list 'scheme: (string-append 
"(define (*A129610 s) (cond ((pair? s) (let ((t (*A074680 s))) (cons (car t) (*A074679 (cdr t))))) (else s)))"
"\n(define (*A129610! s) (cond ((pair? s) (*A074680! s) (*A074679! (cdr s)))) s)"
                       )
        )
  )

  (list 129611 "Signature-permutation of a Catalan automorphism, row 169 of A089840."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 129612)
        (list 'c: (string-append "Automorphism *A089859 = ENIPS(*A129611)."
                     " See the definition given in A122204."
                  )
        )
        (list 'scheme: (string-append 
"(define (*A129611 s) (cond ((pair? s) (*A089859 (cons (car s) (*A089863 (cdr s))))) (else s)))"
"\n(define (*A129611! s) (cond ((pair? s) (*A089863! (cdr s)) (*A089859! s))) s)"
                       )
        )
  )

  (list 129612 "Signature-permutation of a Catalan automorphism, row 251 of A089840."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 129611)
        (list 'c: (string-append "Automorphism *A089863 = SPINE(*A129612)."
                     " See the definition given in A122203."
                  )
        )
        (list 'scheme: (string-append 
"(define (*A129612 s) (cond ((pair? s) (let ((t (*A089863 s))) (cons (car t) (*A089859 (cdr t))))) (else s)))"
"\n(define (*A129612! s) (cond ((pair? s) (*A089863! s) (*A089859! (cdr s)))) s)"
                       )
        )
  )
 )
)


(define A130339 (catfun1 *A130339!))
(define A130340 (catfun1 *A130340!))
(define A130341 (catfun1 *A130341!))
(define A130342 (catfun1 *A130342!))
(define A130343 (catfun1 *A130343!))
(define A130344 (catfun1 *A130344!))
(define A130345 (catfun1 *A130345!))
(define A130346 (catfun1 *A130346!))
(define A130347 (catfun1 *A130347!))
(define A130348 (catfun1 *A130348!))
(define A130349 (catfun1 *A130349!))
(define A130350 (catfun1 *A130350!))
(define A130351 (catfun1 *A130351!))
(define A130352 (catfun1 *A130352!))
(define A130353 (catfun1 *A130353!))
(define A130354 (catfun1 *A130354!))
(define A130355 (catfun1 *A130355!))
(define A130356 (catfun1 *A130356!))
(define A130357 (catfun1 *A130357!))
(define A130358 (catfun1 *A130358!))
(define A130359 (catfun1 *A130359!))
(define A130360 (catfun1 *A130360!))
(define A130361 (catfun1 *A130361!))
(define A130362 (catfun1 *A130362!))
(define A130363 (catfun1 *A130363!))
(define A130364 (catfun1 *A130364!))
(define A130365 (catfun1 *A130365!))
(define A130366 (catfun1 *A130366!))
(define A130367 (catfun1 *A130367!))
(define A130368 (catfun1 *A130368!))
(define A130369 (catfun1 *A130369!))
(define A130370 (catfun1 *A130370!))
(define A130371 (catfun1 *A130371!))
(define A130372 (catfun1 *A130372!))
(define A130373 (catfun1 *A130373!))
(define A130374 (catfun1 *A130374!))
(define A130375 (catfun1 *A130375!))
(define A130376 (catfun1 *A130376!))

;; (output-entries-to-file120_45 seqs-2007Jun05 "./seqs/batch5jun/2007Jun05.scm" "Jun 05 2007")

(define seqs-2007Jun05
 (list
  (list 130339 "Signature permutation of a Catalan automorphism: swap the two rightmost subtrees of general trees, if the root degree (A057515(n)) is even."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append
           "This self-inverse automorphism is obtained as either SPINE(*A129608) or ENIPS(*A129608)."
           " See the definitions given in A122203 and A122204."
                  )
        )
        '(comps: (057508 130340 057508) (057164 130340 057164))
        '(y: "Row 3608 of A122285 and A122286. a(n) = A129608(n), if A057515(n) mod 2 = 0, otherwise a(n)=n.")
        '(inv: 130339)
        (list 'scheme: (string-append 
"(define (*A130339! s) (if (even? (length s)) (*A129608! s)) s)"
                       )
        )
  )

  (list 130340 "Signature permutation of a Catalan automorphism: swap the two leftmost subtrees of general trees, if the root degree (A057515(n)) is even."
        '(off: 0)
        '(indentries: Catsigperm)
        '(comps: (057508 130339 057508) (057164 130339 057164))
        '(y: "a(n) = A072796(n), if A057515(n) mod 2 = 0, otherwise a(n)=n.")
        '(inv: 130340)
        (list 'scheme: (string-append 
"(define (*A130340! s) (if (even? (length s)) (*A072796! s)) s)"
                       )
        )
  )

  (list 130341 "Row 3 of A122203."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the third non-recursive Catalan automorphism *A089850 with recursion schema SPINE (see A122203 for the definition). This automorphism is also produced when automorphism *A069767 is applied to the right-hand side subtree of the given binary tree, with the left side left intact.")
        '(indentries: Catsigperm)
        '(inv: 130342)
        '(comps: (069767 069770))
        (list 'y: (string-append " The number of cycles and the number of fixed points"
" in range [A014137(n-1)..A014138(n-1)] of this permutation are given by A089404 and A073268."
" Cf. also A073286."
                  )
        )
  )

  (list 130342 "Row 3 of A122204."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the third non-recursive Catalan automorphism *A089850 with recursion schema ENIPS (see A122204 for the definition). This automorphism is also produced when automorphism *A069768 is applied to the right-hand side subtree of the given binary tree, with the left side left intact.")
        '(indentries: Catsigperm)
        '(inv: 130341)
        '(comps: (069770 069768))
        (list 'y: (string-append " The number of cycles and the number of fixed points"
" in range [A014137(n-1)..A014138(n-1)] of this permutation are given by A089404 and A073268."
" Cf. also A073287."
                  )
        )
  )

  (list 130343 "Row 4 of A122203."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the fourth non-recursive Catalan automorphism *A089851 with recursion schema SPINE (see A122203 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130344)
  )

  (list 130344 "Row 6 of A122204."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the sixth non-recursive Catalan automorphism *A089853 with recursion schema ENIPS (see A122204 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130343)
  )

  (list 130345 "Row 5 of A122203."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the fifth non-recursive Catalan automorphism *A089852 with recursion schema SPINE (see A122203 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130346)
  )

  (list 130346 "Row 5 of A122204."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the fifth non-recursive Catalan automorphism *A089852 with recursion schema ENIPS (see A122204 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130345)
  )

  (list 130347 "Row 6 of A122203."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the sixth non-recursive Catalan automorphism *A089853 with recursion schema SPINE (see A122203 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130348)
  )

  (list 130348 "Row 4 of A122204."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the fourth non-recursive Catalan automorphism *A089851 with recursion schema ENIPS (see A122204 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130347)
  )

  (list 130349 "Row 9 of A122203."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the ninth non-recursive Catalan automorphism *A089855 with recursion schema SPINE (see A122203 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130350)
  )

  (list 130350 "Row 11 of A122204."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the eleventh non-recursive Catalan automorphism *A089857 with recursion schema ENIPS (see A122204 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130349)
  )

  (list 130351 "Row 10 of A122203."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the tenth non-recursive Catalan automorphism *A089856 with recursion schema SPINE (see A122203 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130352)
  )

  (list 130352 "Row 10 of A122204."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the tenth non-recursive Catalan automorphism *A089856 with recursion schema ENIPS (see A122204 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130351)
  )

  (list 130353 "Row 11 of A122203."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the eleventh non-recursive Catalan automorphism *A089857 with recursion schema SPINE (see A122203 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130354)
  )

  (list 130354 "Row 9 of A122204."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the ninth non-recursive Catalan automorphism *A089855 with recursion schema ENIPS (see A122204 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130353)
  )

  (list 130355 "Row 13 of A122203."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the thirteenth non-recursive Catalan automorphism *A089858 with recursion schema SPINE (see A122203 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130356)
  )

  (list 130356 "Row 18 of A122204."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the eighteenth non-recursive Catalan automorphism *A089861 with recursion schema ENIPS (see A122204 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130355)
  )

  (list 130357 "Row 14 of A122203."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the fourteenth non-recursive Catalan automorphism *A073269 with recursion schema SPINE (see A122203 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130358)
  )

  (list 130358 "Row 19 of A122204."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the nineteenth non-recursive Catalan automorphism *A073270 with recursion schema ENIPS (see A122204 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130357)
  )

  (list 130359 "Row 15 of A122203."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the fifteenth non-recursive Catalan automorphism *A089859 with recursion schema SPINE (see A122203 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130360)
  )

  (list 130360 "Row 21 of A122204."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the 21st non-recursive Catalan automorphism *A089863 with recursion schema ENIPS (see A122204 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130359)
  )

  (list 130361 "Row 16 of A122203."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the sixteenth non-recursive Catalan automorphism *A089860 with recursion schema SPINE (see A122203 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130362)
  )

  (list 130362 "Row 20 of A122204."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the twentieth non-recursive Catalan automorphism *A089862 with recursion schema ENIPS (see A122204 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130361)
  )

  (list 130363 "Row 18 of A122203."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the eighteenth non-recursive Catalan automorphism *A089861 with recursion schema SPINE (see A122203 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130364)
  )

  (list 130364 "Row 13 of A122204."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the thirteenth non-recursive Catalan automorphism *A089858 with recursion schema ENIPS (see A122204 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130363)
  )

  (list 130365 "Row 19 of A122203."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the nineteenth non-recursive Catalan automorphism *A073270 with recursion schema SPINE (see A122203 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130366)
  )

  (list 130366 "Row 14 of A122204."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the fourteenth non-recursive Catalan automorphism *A073269 with recursion schema ENIPS (see A122204 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130365)
  )

  (list 130367 "Row 20 of A122203."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the twentieth non-recursive Catalan automorphism *A089862 with recursion schema SPINE (see A122203 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130368)
  )

  (list 130368 "Row 16 of A122204."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the sixteenth non-recursive Catalan automorphism *A089860 with recursion schema ENIPS (see A122204 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130367)
  )

  (list 130369 "Signature permutation of a Catalan automorphism: apply *A074679 to the root, and recurse down the cdr-spine (the right-hand side edge of a binary tree) as long as the binary tree rotation is possible, and if the top-level length (A057515(n)) is odd, then apply *A069770 to the last branch-point."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append
 "This automorphism converts lists of even length (1 2 3 4 ... 2n-1 2n) to the form"
 " ((1 . 2) (3 . 4) ... (2n-1 . 2n)),"
 " and when applied to lists of odd length, like (1 2 3 4 5), i.e. (1 . (2 . (3 . (4 . (5 . ()))))),"
 " converts them as ((1 . 2) . ((3 . 4) . (() . 5)))."
                  )
        )
        '(comps: (074685 130372) (130376 074685))
        '(inv: 130370)
        (list 'y: (string-append
" The number of cycles, number of fixed points, maximum cycle sizes and LCM's of all cycle sizes"
" in range [A014137(n-1)..A014138(n-1)] of this permutation are given by"
" A130377, LEFT(A019590), A130378 and A130379."
                  )
        )

        (list 'scheme: (string-append
"(define (*A130369! s) (cond ((not (pair? s))) ((not (pair? (cdr s))) (*A069770! s)) (else (*A074679! s) (*A130369! (cdr s)))) s)"
                       )
        )
  )

  (list 130370 "Signature permutation of a Catalan automorphism: inverse of *A130369."
        '(off: 0)
        '(indentries: Catsigperm)
        '(c: "This Catalan automorphism converts even-length lists like (a b c d e f) to ((a . b) (c . d) (e . f)), and odd-length lists like ...")
        '(comps: (130371 074686) (074686 130375))
        '(inv: 130369)
        (list 'y: (string-append
" The number of cycles, number of fixed points, maximum cycle sizes and LCM's of all cycle sizes"
" in range [A014137(n-1)..A014138(n-1)] of this permutation are given by"
" A130377, LEFT(A019590), A130378 and A130379."
                  )
        )
        (list 'scheme: (string-append
"(define (*A130370! s) (cond ((not (pair? s))) ((not (pair? (car s))) (*A069770! s)) (else (*A130370! (cdr s)) (*A074680! s))) s)"
                       )
        )
  )

  (list 130371 "Signature permutation of a Catalan automorphism: apply *A074685 to the last subtree, if the root degree (A057515(n)) is odd."
        '(off: 0)
        '(indentries: Catsigperm)
        '(comps: (130370 074685) (074686 130375 074685) (130370 130375 130369))
        '(inv: 130372)
        (list 'scheme: (string-append
"(define (*A130371! s) (if (odd? (length s)) (*A074685! (car (last-pair s)))) s)"
                       )
        )
  )

  (list 130372 "Signature permutation of a Catalan automorphism: apply *A074686 to the last subtree, if the root degree (A057515(n)) is odd."
        '(off: 0)
        '(indentries: Catsigperm)
        '(comps: (074686 130369) (074686 130376 074685) (130370 130376 130369))
        '(inv: 130371)
        (list 'scheme: (string-append
"(define (*A130372! s) (if (odd? (length s)) (*A074686! (car (last-pair s)))) s)"
                       )
        )
  )

  (list 130373 "Signature permutation of a Catalan automorphism: flip the positions of even and odd-indiced elements at the toplevel of list, leaving the first element in place if the length (A057515(n)) is odd."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append
 "This automorphism permutes the top-level of a list of even length (1 2 3 4 ... 2n-1 2n) as"
 " (2 1 4 3 ... 2n 2n-1), and when applied to a list of odd length (1 2 3 4 5 ... 2n 2n+1),"
" permutes it as (1 3 2 5 4 ... 2n+1 2n)."
                  )
        )
        '(comps: (057508 130374 057508) (057164 130374 057164))
        (list 'y: (string-append
"SPINE and ENIPS transform of *A130340 (transformations explained in A122203 and A122204)."
" The number of cycles and the number of fixed points"
" in range [A014137(n-1)..A014138(n-1)] of this permutation are given by A073193 and A073192."
                  )
        )
        '(inv: 130373)
  )

  (list 130374 "Signature permutation of a Catalan automorphism: flip the positions of even and odd-indiced elements at the toplevel of list, leaving the last element in place if the length (A057515(n)) is odd."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append
 "This automorphism permutes the top-level of a list of even length (1 2 3 4 ... 2n-1 2n) as"
 " (2 1 4 3 ... 2n 2n-1), and when applied to a list of odd length (1 2 3 4 ... 2n-1 2n 2n+1),"
 " permutes it as (2 1 4 3 ... 2n 2n-1 2n+1)."
                  )
        )
        '(comps: (057508 130373 057508) (057164 130373 057164) (127285 127288) (127287 127286))
        (list 'y: (string-append "a(A085223(n)) = A130370(A122282(A130369(A085223(n))))."
" The number of cycles and the number of fixed points"
" in range [A014137(n-1)..A014138(n-1)] of this permutation are given by A073193 and A073192."
                  )
        )
        '(inv: 130374)
        (list 'scheme: (string-append
"(define (*A130374! s) (cond ((pair? s) (*A072796! s) (if (pair? (cdr s)) (*A130374! (cddr s))))) s)"
                       )
        )
  )

  (list 130375 "Signature permutation of a Catalan automorphism: apply *A074685 after the first nil on the top-level of list, if any present, otherwise leave the structure intact."
        '(off: 0)
        '(indentries: Catsigperm)
        '(comps: (074685 130370) (074685 130371 074686) (130369 130371 130370))
        '(inv: 130376)
        (list 'scheme: (string-append
"(define *A130375! (!APPLY-AFTER-FIRST-NIL *A074685!))"
"\n%o A130375 (define (!APPLY-AFTER-FIRST-NIL f!) (letrec ((g! (lambda (s) (cond ((pair? s) (if (pair? (car s)) (g! (cdr s)) (f! (cdr s))))) s))) g!))"
                       )
        )
  )

  (list 130376 "Signature permutation of a Catalan automorphism: apply *A074686 after the first nil on the top-level of list, if any present, otherwise leave the structure intact."
        '(off: 0)
        '(indentries: Catsigperm)
        '(comps: (130369 074686) (074685 130372 074686) (130369 130372 130370))
        '(inv: 130375)
        (list 'scheme: (string-append
"(define *A130376! (!APPLY-AFTER-FIRST-NIL *A074686!))"
"\n%o A130375 (define (!APPLY-AFTER-FIRST-NIL f!) (letrec ((g! (lambda (s) (cond ((pair? s) (if (pair? (car s)) (g! (cdr s)) (f! (cdr s))))) s))) g!))"
                       )
        )
  )

 )
)



(define A130919   (catfun1 *A130919!))
(define A130919v2 (catfun1 *A130919v2!))
(define A130920   (catfun1 *A130920!))
(define A130920v2 (catfun1 *A130920v2!))

(define A130921 (catfun1 *A130921!))
(define A130922 (catfun1 *A130922!))

(define A130923 (catfun1 *A130923!))
(define A130924 (catfun1 *A130924!))
(define A130925 (catfun1 *A130925!))
(define A130926 (catfun1 *A130926!))

(define A130381 (catfun1 *A130381!))
(define A130382 (catfun1 *A130382!))
(define A130383 (catfun1 *A130383!))
(define A130384 (catfun1 *A130384!))
(define A130385 (catfun1 *A130385!))
(define A130386 (catfun1 *A130386!))
(define A130387 (catfun1 *A130387!))
(define A130388 (catfun1 *A130388!))
(define A130389 (catfun1 *A130389!))
(define A130390 (catfun1 *A130390!))
(define A130391 (catfun1 *A130391!))
(define A130392 (catfun1 *A130392!))
(define A130393 (catfun1 *A130393!))
(define A130394 (catfun1 *A130394!))
(define A130395 (catfun1 *A130395!))
(define A130396 (catfun1 *A130396!))
(define A130397 (catfun1 *A130397!))
(define A130398 (catfun1 *A130398!))

(define A130927 (catfun1 *A130927!))
(define A130928 (catfun1 *A130928!))
(define A130929 (catfun1 *A130929!))
(define A130930 (catfun1 *A130930!))
(define A130931 (catfun1 *A130931!))
(define A130932 (catfun1 *A130932!))
(define A130933 (catfun1 *A130933!))
(define A130934 (catfun1 *A130934!))
(define A130935 (catfun1 *A130935!))
(define A130936 (catfun1 *A130936!))
(define A130937 (catfun1 *A130937!))
(define A130938 (catfun1 *A130938!))
(define A130939 (catfun1 *A130939!))
(define A130940 (catfun1 *A130940!))
(define A130941 (catfun1 *A130941!))
(define A130942 (catfun1 *A130942!))
(define A130943 (catfun1 *A130943!))
(define A130944 (catfun1 *A130944!))
(define A130945 (catfun1 *A130945!))
(define A130946 (catfun1 *A130946!))
(define A130947 (catfun1 *A130947!))
(define A130948 (catfun1 *A130948!))
(define A130949 (catfun1 *A130949!))
(define A130950 (catfun1 *A130950!))
(define A130951 (catfun1 *A130951!))
(define A130952 (catfun1 *A130952!))
(define A130953 (catfun1 *A130953!))
(define A130954 (catfun1 *A130954!))
(define A130955 (catfun1 *A130955!))
(define A130956 (catfun1 *A130956!))
(define A130957 (catfun1 *A130957!))
(define A130958 (catfun1 *A130958!))
(define A130959 (catfun1 *A130959!))
(define A130960 (catfun1 *A130960!))
(define A130961 (catfun1 *A130961!))
(define A130962 (catfun1 *A130962!))
(define A130963 (catfun1 *A130963!))
(define A130964 (catfun1 *A130964!))
(define A130965 (catfun1 *A130965!))
(define A130966 (catfun1 *A130966!))

(define A130967 (cc-Afun A130919))
(define A130968 (fc-Afun A130920))

(define A130969 (cc-Afun A130935))

;; (load "../Schemuli/GF2Xfuns")
;; (cd "seqs/batch11jun")
;; (output-entries-to-file120_45 seqs-2007Jun11 "2007Jun11.scm" "Jun 11 2007")

(define seqs-2007Jun11
 (list

  (list 130400 "Signature permutations of INORDER-transformations of non-recursive Catalan automorphisms in table A089840."
        '(off: 0)
        '(keywords: "tabl")
        (list 'c: (string-append "Row n is the signature permutation of the Catalan automorphism"
" which is obtained from the nth nonrecursive automorphism in the table A089840 with"
" the recursion scheme \"INORDER\". In this recursion scheme the given automorphism is applied"
" at the root of binary tree after the algorithm has recursed down the car-branch"
" (left-hand-side tree in the context of binary trees), but before the algorithm recurses down"
" to the cdr-branch (the right-hand-side of the binary tree, with respect to the new orientation"
" of branches, possibly changed by the applied automorphism)."
" I.e. this corresponds to the depth-first in-order traversal of a Catalan structure,"
" when it is interpreted as a binary tree. The associated Scheme-procedures INORDER and !INORDER"
" can be used to obtain such a transformed automorphism from any constructively"
" (or respectively: destructively) implemented automorphism."
" Each row occurs only once in this table."
" This transformation has many fixed points besides the trivial identity automorphism *A001477:"
" at least *A069770, *A089863 and *A129604 stay as they are."
" Inverses of these permutations can be found in table A130401."
                  )
        )
        (list 'y: (string-append "The first 22 rows of this table:"
           " Row 0 (identity permutation): A001477,"
 " 1: A069770, 2: A073284, 3: A122341, 4: A130381, 5: A130383, 6: A130385, 7: A122350,"
 " 8: A082341, 9: A130387, 10: A130389, 11: A130391, 13: A130393, 14: A130395, 15: A130397,"
 " 16: A130927, 17: A071657, 18: A130929, 19: A130931, 20: A130933, 21: A089863."
 " Other rows: row 1654694: A073280, row 1654720: A129604."
 " C.f. also tables A089840, A122201-A122204, A130402-A130403."
                  )
        )

        (list 'scheme: (string-append
"(define (INORDER f) (letrec ((g (lambda (s) (cond ((not (pair? s)) s) (else (let ((t (f (cons (g (car s)) (cdr s))))) (cons (car t) (g (cdr t))))))))) g))"
"(define (!INORDER f!) (letrec ((g! (lambda (s) (cond ((pair? s) (g! (car s)) (f! s) (g! (cdr s)))) s))) g!))"
                       )
        )
  )

  (list 130401 "Signature permutations of REDRONI-transformations of non-recursive Catalan automorphisms in table A089840."
        '(off: 0)
        '(keywords: "tabl")
        (list 'c: (string-append "Row n is the signature permutation of the Catalan automorphism"
" which is obtained from the nth nonrecursive automorphism in the table A089840 with"
" the recursion scheme \"REDRONI\". In this recursion scheme the given automorphism is applied"
" at the root of binary tree after the algorithm has recursed down the cdr-branch"
" (right-hand-side tree in the context of binary trees), but before the algorithm recurses down"
" to the car-branch (the left-hand-side of the binary tree, with respect to the new orientation"
" of branches, possibly changed by the applied automorphism)."
" I.e. this corresponds to the reversed depth-first in-order traversal of a Catalan structure,"
" when it is interpreted as a binary tree. The associated Scheme-procedures REDRONI and !REDRONI"
" can be used to obtain such a transformed automorphism from any constructively"
" (or respectively: destructively) implemented automorphism."
" Each row occurs only once in this table."
" This transformation has many fixed points besides the trivial identity automorphism *A001477:"
" at least *A069770, *A089859 and *A129604 stay as they are."
" Inverses of these permutations can be found in table A130400."
                  )
        )
        (list 'y: (string-append "The first 22 rows of this table:"
           " Row 0 (identity permutation): A001477,"
 " 1: A069770, 2: A073285, 3: A122342, 4: A130386, 5: A130384, 6: A130382, 7: A122349,"
 " 8: A082342, 9: A130392, 10: A130390, 11: A130388, 12: A071658, 13: A130930, 14: A130932,"
 " 15: A089859, 16: A130934, 18: A130394, 19: A130396, 20: A130928, 21: A130398."
 " Other rows: row 1654694: A073280, row 1654720: A129604."
 " C.f. also tables A089840, A122201-A122204, A130402-A130403."
                  )
        )
        (list 'scheme: (string-append
"(define (REDRONI f) (letrec ((g (lambda (s) (fold-right (lambda (x y) (let ((t (f (cons x y)))) (cons (g (car t)) (cdr t)))) '() s)))) g))"
"(define (!REDRONI f!) (letrec ((g! (lambda (s) (cond ((pair? s) (g! (cdr s)) (f! s) (g! (car s)))) s))) g!))"
                       )
        )
  )

  (list 130402 "Signature permutations of ENIPS-transformations of A057163-conjugates of Catalan automorphisms in table A122203."
        '(off: 0)
        '(keywords: "tabl")
        (list 'c: (string-append "Row n is the signature permutation of the Catalan automorphism"
" which is obtained from A057163-conjugate of the nth automorphism in the table A122203 with"
" the recursion scheme \"ENIPS\", i.e. row n is obtained as"
" ENIPS(A057163 o SPINE(A089840[n]) o A057163)."
" See A122203 and A122204 for the description of SPINE and ENIPS."
" Each row occurs only once in this table."
" Inverses of these permutations can be found in table A130403."
" This table contains also all the rows of A122204 and A089840."
                  )
        )
        (list 'y: (string-append "The known rows of this table:"
           " Row 0 (identity permutation): A001477,"
 " 1: A082346, 2: A130935, 3: A073289, 4: A130937, 5: A130939, 6: A130941, 7: A130943,"
 " 8: A130945, 9: A130947, 10: A130949, 11: A130951, 12: A074687, 13: A130953, 14: A130955,"
 " 15: A130957, 16: A130959, 17: A057162, 18: A130961, 19: A130963, 20: A130965, 21: A069768."
 " Other rows: 251: A069770, 3613: A082340, 65352: A057502."
           " C.f. also tables A089840, A122201-A122204, A122285-A122286, A130400-A130401."
                  )
        )
  )

  (list 130403 "Signature permutations of SPINE-transformations of A057163-conjugates of Catalan automorphisms in table A122204."
        '(off: 0)
        '(keywords: "tabl")
        (list 'c: (string-append "Row n is the signature permutation of the Catalan automorphism"
" which is obtained from A057163-conjugate of the nth automorphism in the table A122204 with the"
" recursion scheme \"SPINE\", i.e. row n is obtained as"
" SPINE(A057163 o ENIPS(A089840[n]) o A057163)."
" See A122203 and A122204 for the description of SPINE and ENIPS."
" Each row occurs only once in this table."
" Inverses of these permutations can be found in table A130402."
" This table contains also all the rows of A122203 and A089840."
                  )
        )
        (list 'y: (string-append "The known rows of this table:"
           " Row 0 (identity permutation): A001477,"
" 1: A082345, 2: A130936, 3: A073288, 4: A130942, 5: A130940, 6: A130938, 7: A130944,"
" 8: A130946, 9: A130952, 10: A130950, 11: A130948, 12: A057161, 13: A130962, 14: A130964,"
" 15: A069767, 16: A130966, 17: A074688, 18: A130954, 19: A130956, 20: A130960, 21: A130958,"
" Other rows: 169: A069770, 3617: A082339, 65167: A057501."
           " C.f. also tables A089840, A122201-A122204, A122285-A122286, A130400-A130401."
                  )
        )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (list 130918 "Simple self-inverse permutation of natural numbers: List each block of A000108(n) numbers from A014137(n-1) to A014138(n-1) in reversed order."
        '(off: 0)
        '(indentries: Catsigperm)
;;      '(create-b-file: 2055)
        (list 'c: (string-append
           "In principle this involution is the signature permutation of"
           " yet another Catalan automorphism."
           " However, the question remains what is the most \"natural\" way to create such"
           " an automorphism acting e.g. on S-expressions (i.e. rooted plane binary trees),"
           " which would produce this sequence as its signature permutation."
                  )
        )
        '(f: "a(0)=0, a(n) = A014138(A072643(n)-1) - A082853(n).")
        (list 'y: (string-append "Cf. A054429, A057163."
" The number of cycles and the number of fixed points"
" in range [A014137(n-1)..A014138(n-1)] of this permutation are given by A130380 and A036987."
                  )
        )

        '(inv: 130918)
        (list 'scheme: (string-append 
"(define (A130918 n) (if (zero? n) n (- (A014138 (- (A072643 n) 1)) (A082853 n))))"
                       )
        )
  )

  (list 130919 "Signature permutation of a Catalan automorphism: DEEPEN-transform of automorphism *A057511."
        '(off: 0)
        '(indentries: Catsigperm)
;;      '(create-b-file: 2055)
        (list 'c: (string-append 
"*A130919 = DEEPEN(*A057511) = NEPEED(*A057511) = DEEPEN(DEEPEN(*A057509)) = NEPEED(NEPEED(*A057509))."
" See A122283, A122284 for the definitions of DEEPEN and NEPEED transforms."
                  )
        )
        '(inv: 130920)
        (list 'y: (string-append
"A122351(n) = A083927(A130919(A057123(n)))."
" The number of cycles and the number of fixed points"
" in range [A014137(n-1)..A014138(n-1)] of this permutation are given by A130967 and A130968."
" Maximum cycle sizes seems to be given by A000793 (shifted once right)."
                  )
        )
  )

  (list 130920 "Signature permutation of a Catalan automorphism: DEEPEN-transform of automorphism *A057512."
        '(off: 0)
        '(indentries: Catsigperm)
;;      '(create-b-file: 2055)
        (list 'c: (string-append 
"*A130920 = DEEPEN(*A057512) = NEPEED(*A057512) = DEEPEN(DEEPEN(*A057510)) = NEPEED(NEPEED(*A057510))."
" See A122283, A122284 for the definitions of DEEPEN and NEPEED transforms."
                  )
        )
        '(inv: 130919)
        '(y: "A122351(n) = A083927(A130920(A057123(n))).")
  )

  (list 130921 "Signature permutation of a Catalan automorphism: composition of automorphisms *A074684 and *A057164."
        '(off: 0)
        '(indentries: Catsigperm)
;;      '(create-b-file: 2055)
        '(inv: 130922)
        '(comps: (074684 057164))
        '(y: "Cf. A086425.")
        '(scheme: "(define (*A130921! s) (*A074684! (*A057164! s)))")
  )

  (list 130922 "Signature permutation of a Catalan automorphism: composition of automorphisms *A057164 and *A074683."
        '(off: 0)
        '(indentries: Catsigperm)
;;      '(create-b-file: 2055)
        '(inv: 130921)
        '(comps: (057164 074683))
        '(y: "Cf. A086426.")
        '(scheme: "(define (*A130922! s) (*A057164! (*A074683! s)))")
  )

  (list 130923 "Signature permutation of a Catalan automorphism: Inverse FORK-transform of automorphism *A120705."
        '(off: 0)
        '(indentries: Catsigperm)
;;      '(create-b-file: 2055)
        (list 'c: (string-append 
"This is the unique Catalan automorphism f, such that *A120705 = (FORK f)."
" See A122201 for the definition of FORK."
                  )
        )
        '(inv: 130924)
        '(y: "Cf. A130925 & A130926.")
        (list 'scheme: (string-append
"(define (*A130923! s) (cond ((pair? s) (*A120705! s) (*A120706! (car s)) (*A120706! (cdr s)))) s)"
"\n%o A130924 (Another version where the call to car-branch is eliminated, but cdr-branch gets just more complex:)"
"\n%o A130924 "
"(define (*A130923! s) (cond ((pair? s) (*A074680! s) (cond ((pair? (cdr s))"
" (*A120705! (cddr s))"
" (*A120706! (cadr s))"
" (*A120706! (cdr s)))))) s)"
                       )
        )
  )


  (list 130924 "Signature permutation of a Catalan automorphism: Inverse KROF-transform of automorphism *A120706."
        '(off: 0)
        '(indentries: Catsigperm)
;;      '(create-b-file: 2055)
        (list 'c: (string-append 
"This is the unique Catalan automorphism f, such that *A120706 = (KROF f)."
" See A122202 for the definition of KROF."
                  )
        )
        '(inv: 130923)
        '(y: "Cf. A130925 & A130926.")
        (list 'scheme: (string-append
"(define (*A130924! s) (cond ((pair? s) (*A120705! (cdr s)) (*A120705! (car s)) (*A120706! s))) s)"
                       )
        )
  )

  (list 130925 "Signature permutation of a Catalan automorphism: Inverse FORK-transform of automorphism *A120706."
        '(off: 0)
        '(indentries: Catsigperm)
;;      '(create-b-file: 2055)
        (list 'c: (string-append 
"This is the unique Catalan automorphism f, such that *A120706 = (FORK f)."
" See A122201 for the definition of FORK."
                  )
        )
        '(inv: 130926)
        '(y: "Cf. A130923 & A130924.")
        (list 'scheme: (string-append
"(define (*A130925! s) (cond ((pair? s) (*A074679! s) (cond ((pair? (car s)) (*A120706! (caar s)) (*A120705! (cdar s)) (*A120705! (car s)))))) s)"
                       )
        )
  )


  (list 130926 "Signature permutation of a Catalan automorphism: Inverse KROF-transform of automorphism *A120705."
        '(off: 0)
        '(indentries: Catsigperm)
;;      '(create-b-file: 2055)
        (list 'c: (string-append 
"This is the unique Catalan automorphism f, such that *A120705 = (KROF f)."
" See A122202 for the definition of KROF."
                  )
        )
        '(inv: 130925)
        '(y: "Cf. A130923 & A130924.")
        (list 'scheme: (string-append
"(define (*A130926! s) (cond ((pair? s) (*A120706! (cdr s)) (*A120706! (car s)) (*A120705! s))) s)"
                       )
        )
  )

  (list 130380 "Catalan numbers halved and rounded to the next integer."
        '(off: 0)
        '(f: "a(n) = ceiling(A000108(n)/2).")
        '(c: "Number of cycles in range [A014137(n-1)..A014138(n-1)] of permutation A130918.")
        '(y: "a(n) = (A000108(n)+A036987(n))/2.")
        (list 'scheme: (string-append
"(define (A130380 n) (floor->exact (/ (+ (A000108 n) 1) 2)))"
                       )
        )
  )

  (list 130381 "Signature permutation of a Catalan automorphism: Row 4 of A130400."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the fourth non-recursive Catalan automorphism *A089851 with recursion schema INORDER (see A130400 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130382)
  )

  (list 130382 "Signature permutation of a Catalan automorphism: Row 6 of A130401."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the sixth non-recursive Catalan automorphism *A089853 with recursion schema REDRONI (see A130401 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130381)
  )

  (list 130383 "Signature permutation of a Catalan automorphism: Row 5 of A130400."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the fifth non-recursive Catalan automorphism *A089852 with recursion schema INORDER (see A130400 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130384)
  )

  (list 130384 "Signature permutation of a Catalan automorphism: Row 5 of A130401."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the fifth non-recursive Catalan automorphism *A089852 with recursion schema REDRONI (see A130401 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130383)
  )

  (list 130385 "Signature permutation of a Catalan automorphism: Row 6 of A130400."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the sixth non-recursive Catalan automorphism *A089853 with recursion schema INORDER (see A130400 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130386)
  )

  (list 130386 "Signature permutation of a Catalan automorphism: Row 4 of A130401."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the fourth non-recursive Catalan automorphism *A089851 with recursion schema REDRONI (see A130401 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130385)
  )

  (list 130387 "Signature permutation of a Catalan automorphism: Row 9 of A130400."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the ninth non-recursive Catalan automorphism *A089855 with recursion schema INORDER (see A130400 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130388)
  )

  (list 130388 "Signature permutation of a Catalan automorphism: Row 11 of A130401."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the eleventh non-recursive Catalan automorphism *A089857 with recursion schema REDRONI (see A130401 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130387)
  )

  (list 130389 "Signature permutation of a Catalan automorphism: Row 10 of A130400."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the tenth non-recursive Catalan automorphism *A089856 with recursion schema INORDER (see A130400 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130390)
  )

  (list 130390 "Signature permutation of a Catalan automorphism: Row 10 of A130401."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the tenth non-recursive Catalan automorphism *A089856 with recursion schema REDRONI (see A130401 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130389)
  )

  (list 130391 "Signature permutation of a Catalan automorphism: Row 11 of A130400."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the eleventh non-recursive Catalan automorphism *A089857 with recursion schema INORDER (see A130400 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130392)
  )

  (list 130392 "Signature permutation of a Catalan automorphism: Row 9 of A130401."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the ninth non-recursive Catalan automorphism *A089855 with recursion schema REDRONI (see A130401 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130391)
  )

  (list 130393 "Signature permutation of a Catalan automorphism: Row 13 of A130400."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the thirteenth non-recursive Catalan automorphism *A089858 with recursion schema INORDER (see A130400 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130394)
  )

  (list 130394 "Signature permutation of a Catalan automorphism: Row 18 of A130401."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the eighteenth non-recursive Catalan automorphism *A089861 with recursion schema REDRONI (see A130401 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130393)
  )

  (list 130395 "Signature permutation of a Catalan automorphism: Row 14 of A130400."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the fourteenth non-recursive Catalan automorphism *A073269 with recursion schema INORDER (see A130400 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130396)
  )

  (list 130396 "Signature permutation of a Catalan automorphism: Row 19 of A130401."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the nineteenth non-recursive Catalan automorphism *A073270 with recursion schema REDRONI (see A130401 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130395)
  )

  (list 130397 "Signature permutation of a Catalan automorphism: Row 15 of A130400."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the 15th non-recursive Catalan automorphism *A089859 with recursion schema INORDER (see A130400 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130398)
  )

  (list 130398 "Signature permutation of a Catalan automorphism: Row 21 of A130401."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the 21th non-recursive Catalan automorphism *A089863 with recursion schema REDRONI (see A130401 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130397)
  )

  (list 130927 "Signature permutation of a Catalan automorphism: Row 16 of A130400."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the 16th non-recursive Catalan automorphism *A089860 with recursion schema INORDER (see A130400 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130928)
  )

  (list 130928 "Signature permutation of a Catalan automorphism: Row 20 of A130401."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the 20th non-recursive Catalan automorphism *A089862 with recursion schema REDRONI (see A130401 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130927)
  )


  (list 130929 "Signature permutation of a Catalan automorphism: Row 18 of A130400."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the 18th non-recursive Catalan automorphism *A089861 with recursion schema INORDER (see A130400 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130930)
  )

  (list 130930 "Signature permutation of a Catalan automorphism: Row 13 of A130401."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the 13th non-recursive Catalan automorphism *A089858 with recursion schema REDRONI (see A130401 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130929)
  )

  (list 130931 "Signature permutation of a Catalan automorphism: Row 19 of A130400."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the 19th non-recursive Catalan automorphism *A073270 with recursion schema INORDER (see A130400 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130932)
  )

  (list 130932 "Signature permutation of a Catalan automorphism: Row 14 of A130401."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the 14th non-recursive Catalan automorphism *A073269 with recursion schema REDRONI (see A130401 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130931)
  )

  (list 130933 "Signature permutation of a Catalan automorphism: Row 20 of A130400."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the 20th non-recursive Catalan automorphism *A089862 with recursion schema INORDER (see A130400 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130934)
  )

  (list 130934 "Signature permutation of a Catalan automorphism: Row 16 of A130401."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from the 16th non-recursive Catalan automorphism *A089860 with recursion schema REDRONI (see A130401 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130933)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (list 130935 "Signature permutation of a Catalan automorphism: Row 2 of A130402."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from *A069775 with recursion schema ENIPS (see A122204 for the definition).")
        '(indentries: Catsigperm)
        (list 'y: (string-append
" The number of cycles in range [A014137(n-1)..A014138(n-1)] of this permutation are given by"
" A130969. The number of fixed points begins like A003238."
" Maximum cycle sizes begins like A000792 (shifted once right)."
                  )
        )
        '(inv: 130936)
  )

  (list 130936 "Signature permutation of a Catalan automorphism: Row 2 of A130403."
        '(off: 0)
        '(c: "The signature-permutation of the Catalan automorphism which is derived from *A069776 with recursion schema SPINE (see A122203 for the definition).")
        '(indentries: Catsigperm)
        '(inv: 130935)
  )

  (list 130937 "Signature permutation of a Catalan automorphism: Row 4 of A130402."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130938)
  )

  (list 130938 "Signature permutation of a Catalan automorphism: Row 6 of A130403."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130937)
  )

  (list 130939 "Signature permutation of a Catalan automorphism: Row 5 of A130402."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130940)
  )

  (list 130940 "Signature permutation of a Catalan automorphism: Row 5 of A130403."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130939)
  )

  (list 130941 "Signature permutation of a Catalan automorphism: Row 6 of A130402."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130942)
  )

  (list 130942 "Signature permutation of a Catalan automorphism: Row 4 of A130403."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130941)
  )

  (list 130943 "Signature permutation of a Catalan automorphism: Row 7 of A130402."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130944)
  )

  (list 130944 "Signature permutation of a Catalan automorphism: Row 7 of A130403."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130943)
  )

  (list 130945 "Signature permutation of a Catalan automorphism: Row 8 of A130402."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130946)
  )

  (list 130946 "Signature permutation of a Catalan automorphism: Row 8 of A130403."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130945)
  )

  (list 130947 "Signature permutation of a Catalan automorphism: Row 9 of A130402."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130948)
  )

  (list 130948 "Signature permutation of a Catalan automorphism: Row 11 of A130403."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130947)
  )

  (list 130949 "Signature permutation of a Catalan automorphism: Row 10 of A130402."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130950)
  )

  (list 130950 "Signature permutation of a Catalan automorphism: Row 10 of A130403."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130949)
  )

  (list 130951 "Signature permutation of a Catalan automorphism: Row 11 of A130402."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130952)
  )

  (list 130952 "Signature permutation of a Catalan automorphism: Row 9 of A130403."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130951)
  )

  (list 130953 "Signature permutation of a Catalan automorphism: Row 13 of A130402."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130954)
  )

  (list 130954 "Signature permutation of a Catalan automorphism: Row 18 of A130403."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130953)
  )

  (list 130955 "Signature permutation of a Catalan automorphism: Row 14 of A130402."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130956)
  )

  (list 130956 "Signature permutation of a Catalan automorphism: Row 19 of A130403."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130955)
  )

  (list 130957 "Signature permutation of a Catalan automorphism: Row 15 of A130402."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130958)
  )

  (list 130958 "Signature permutation of a Catalan automorphism: Row 21 of A130403."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130957)
  )

  (list 130959 "Signature permutation of a Catalan automorphism: Row 16 of A130402."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130960)
  )

  (list 130960 "Signature permutation of a Catalan automorphism: Row 20 of A130403."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130959)
  )

  (list 130961 "Signature permutation of a Catalan automorphism: Row 18 of A130402."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130962)
  )

  (list 130962 "Signature permutation of a Catalan automorphism: Row 13 of A130403."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130961)
  )

  (list 130963 "Signature permutation of a Catalan automorphism: Row 19 of A130402."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130964)
  )

  (list 130964 "Signature permutation of a Catalan automorphism: Row 14 of A130403."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130963)
  )

  (list 130965 "Signature permutation of a Catalan automorphism: Row 20 of A130402."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130966)
  )

  (list 130966 "Signature permutation of a Catalan automorphism: Row 16 of A130403."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 130965)
  )

;;;;;


  (list 130967 "Number of cycles in range [A014137(n-1)..A014138(n-1)] of permutations A130919/A130920."
        '(off: 0)
        '(upto: 9) ;; We add the rest manually.
        (list 'y: (string-append "Cf. A130968, A057513."))
  )

  (list 130968 "Number of fixed points in range [A014137(n-1)..A014138(n-1)] of permutations A130919/A130920."
        '(off: 0)
        '(upto: 9) ;; We add the rest manually.
        (list 'y: (string-append "Cf. A130967, A057546."))
  )

  (list 130969 "Number of cycles in range [A014137(n-1)..A014138(n-1)] of permutations A130935/A130936."
        '(off: 0)
        '(upto: 9) ;; We add the rest manually.

  )

 )
)



(define A131141 (catfun1 *A131141!))
(define A131142 (catfun1 *A131142!))
(define A131143 (catfun1 *A131143!))
(define A131144 (catfun1 *A131144!))
(define A131145 (catfun1 *A131145!))
(define A131146 (catfun1 *A131146!))
(define A131147 (catfun1 *A131147!))
(define A131148 (catfun1 *A131148!))
(define A131149 (catfun1 *A131149!))
(define A131150 (catfun1 *A131150!))
(define A131151 (catfun1 *A131151!))
(define A131152 (catfun1 *A131152!))
(define A131153 (catfun1 *A131153!))
(define A131154 (catfun1 *A131154!))
(define A131155 (catfun1 *A131155!))
(define A131156 (catfun1 *A131156!))
(define A131157 (catfun1 *A131157!))
(define A131158 (catfun1 *A131158!))
(define A131159 (catfun1 *A131159!))
(define A131160 (catfun1 *A131160!))
(define A131161 (catfun1 *A131161!))
(define A131162 (catfun1 *A131162!))
(define A131163 (catfun1 *A131163!))
(define A131164 (catfun1 *A131164!))
(define A131165 (catfun1 *A131165!))
(define A131166 (catfun1 *A131166!))
(define A131167 (catfun1 *A131167!))
(define A131168 (catfun1 *A131168!))
(define A131169 (catfun1 *A131169!))
(define A131170 (catfun1 *A131170!))
(define A131171 (catfun1 *A131171!))
(define A131172 (catfun1 *A131172!))
(define A131173 (catfun1 *A131173!))

(define A130981 (catfun1 *A130981!))
(define A130982 (catfun1 *A130982!))
(define A130983 (catfun1 *A130983!))
(define A130984 (catfun1 *A130984!))
(define A130985 (catfun1 *A130985!))
(define A130986 (catfun1 *A130986!))
(define A130987 (catfun1 *A130987!))
(define A130988 (catfun1 *A130988!))
(define A130989 (catfun1 *A130989!))
(define A130990 (catfun1 *A130990!))
(define A130991 (catfun1 *A130991!))
(define A130992 (catfun1 *A130992!))
(define A130993 (catfun1 *A130993!))
(define A130994 (catfun1 *A130994!))
(define A130995 (catfun1 *A130995!))
(define A130996 (catfun1 *A130996!))
(define A130997 (catfun1 *A130997!))
(define A130998 (catfun1 *A130998!))
(define A130999 (catfun1 *A130999!))
(define A131000 (catfun1 *A131000!))
(define A131001 (catfun1 *A131001!))
(define A131002 (catfun1 *A131002!))
(define A131003 (catfun1 *A131003!))
(define A131004 (catfun1 *A131004!))
(define A131005 (catfun1 *A131005!))
(define A131006 (catfun1 *A131006!))
(define A131007 (catfun1 *A131007!))
(define A131008 (catfun1 *A131008!))
(define A131009 (catfun1 *A131009!))
(define A131010 (catfun1 *A131010!))

(define A153835 (catfun1 *A153835!))

(define A154121 (catfun1 *A154121!))
(define A154122 (catfun1 *A154122!))
(define A154123 (catfun1 *A154123!))
(define A154124 (catfun1 *A154124!))
(define A154125 (catfun1 *A154125!))
(define A154126 (catfun1 *A154126!))



(define A154470 (catfun1 *A154470))
(definec (A154471 n) (if (= 0 n)  31706 (A154470 (A154471 (-1+ n)))))
(define (A154472 n) (A126309 (A154471 n)))
(define (A154473 n) (A014486 (A154472 n)))
(define (A154474 n) (A007088 (A154473 n)))
(define (A154475 n) (A072643 (A154472 n)))
(define (A154476 n) (- (A072643 (A154471 n)) (A072643 (A154472 n))))

;; (define Auusbal (compose-funs A153240 A080068))
;; (map Auusbal (iota0 10))
;; --> (0 0 0 1 0 3 2 0 1 7 4)


;; (load "../Schemuli/GF2Xfuns")
;; (cd "seqs/batch20jun")
;; (output-entries-to-file120_45 seqs-2007Jun20 "2007Jun20.scm" "Jun 20 2007")

(define seqs-2007Jun20
 (list

  (list 131141 "Signature permutation of a Catalan automorphism: row 3 of A122285."
        '(off: 0)
        '(c: "Derived from *A130341 with recursion scheme ENIPS.")
        '(indentries: Catsigperm)
        '(inv: 131142)
  )

  (list 131142 "Signature permutation of a Catalan automorphism: row 3 of A122286."
        '(off: 0)
        '(c: "Derived from *A130342 with recursion scheme SPINE.")
        '(indentries: Catsigperm)
        '(inv: 131141)
  )

  (list 131143 "Signature permutation of a Catalan automorphism: row 4 of A122285."
        '(off: 0)
        '(c: "Derived from *A130343 with recursion scheme ENIPS.")
        '(indentries: Catsigperm)
        '(inv: 131144)
  )

  (list 131144 "Signature permutation of a Catalan automorphism: row 6 of A122286."
        '(off: 0)
        '(c: "Derived from *A130344 with recursion scheme SPINE.")
        '(indentries: Catsigperm)
        '(inv: 131143)
  )

  (list 131145 "Signature permutation of a Catalan automorphism: row 5 of A122285."
        '(off: 0)
        '(c: "Derived from *A130345 with recursion scheme ENIPS.")
        '(indentries: Catsigperm)
        '(inv: 131146)
  )

  (list 131146 "Signature permutation of a Catalan automorphism: row 5 of A122286."
        '(off: 0)
        '(c: "Derived from *A130346 with recursion scheme SPINE.")
        '(indentries: Catsigperm)
        '(inv: 131145)
  )

  (list 131147 "Signature permutation of a Catalan automorphism: row 6 of A122285."
        '(off: 0)
        '(c: "Derived from *A130347 with recursion scheme ENIPS.")
        '(indentries: Catsigperm)
        '(inv: 131148)
  )

  (list 131148 "Signature permutation of a Catalan automorphism: row 4 of A122286."
        '(off: 0)
        '(c: "Derived from *A130348 with recursion scheme SPINE.")
        '(indentries: Catsigperm)
        '(inv: 131147)
  )

  (list 131173 "Signature permutation of a Catalan automorphism: row 7 of A122285 and A122286."
        '(off: 0)
        '(c: "Derived from *A122282 with recursion scheme SPINE or ENIPS.")
        '(indentries: Catsigperm)
        '(inv: 131173)
        (list 'scheme: (string-append
"(define (*A131173! s) (cond ((pair? s) (*A069770! (car s)) (if (pair? (cdr s)) (*A131173! (cddr s))))) s)"
                       )
        )
  )

  (list 131169 "Signature permutation of a Catalan automorphism: row 8 of A122285."
        '(off: 0)
        '(c: "Derived from *A082339 with recursion scheme ENIPS.")
        '(indentries: Catsigperm)
        '(inv: 131170)
  )

  (list 131170 "Signature permutation of a Catalan automorphism: row 8 of A122286."
        '(off: 0)
        '(c: "Derived from *A082340 with recursion scheme SPINE.")
        '(indentries: Catsigperm)
        '(inv: 131169)
  )

  (list 131149 "Signature permutation of a Catalan automorphism: row 9 of A122285."
        '(off: 0)
        '(c: "Derived from *A130349 with recursion scheme ENIPS.")
        '(indentries: Catsigperm)
        '(inv: 131150)
  )

  (list 131150 "Signature permutation of a Catalan automorphism: row 11 of A122286."
        '(off: 0)
        '(c: "Derived from *A130350 with recursion scheme SPINE.")
        '(indentries: Catsigperm)
        '(inv: 131149)
  )

  (list 131151 "Signature permutation of a Catalan automorphism: row 10 of A122285."
        '(off: 0)
        '(c: "Derived from *A130351 with recursion scheme ENIPS.")
        '(indentries: Catsigperm)
        '(inv: 131152)
  )

  (list 131152 "Signature permutation of a Catalan automorphism: row 10 of A122286."
        '(off: 0)
        '(c: "Derived from *A130352 with recursion scheme SPINE.")
        '(indentries: Catsigperm)
        '(inv: 131151)
  )

  (list 131153 "Signature permutation of a Catalan automorphism: row 11 of A122285."
        '(off: 0)
        '(c: "Derived from *A130353 with recursion scheme ENIPS.")
        '(indentries: Catsigperm)
        '(inv: 131154)
  )

  (list 131154 "Signature permutation of a Catalan automorphism: row 9 of A122286."
        '(off: 0)
        '(c: "Derived from *A130354 with recursion scheme SPINE.")
        '(indentries: Catsigperm)
        '(inv: 131153)
  )


  (list 131171 "Signature permutation of a Catalan automorphism: row 12 of A122285."
        '(off: 0)
        '(c: "Derived from *A074685 with recursion scheme ENIPS.")
        '(indentries: Catsigperm)
        '(inv: 131172)
  )

  (list 131172 "Signature permutation of a Catalan automorphism: row 17 of A122286."
        '(off: 0)
        '(c: "Derived from *A074686 with recursion scheme SPINE.")
        '(indentries: Catsigperm)
        '(inv: 131171)
  )

  (list 131155 "Signature permutation of a Catalan automorphism: row 13 of A122285."
        '(off: 0)
        '(c: "Derived from *A130355 with recursion scheme ENIPS.")
        '(indentries: Catsigperm)
        '(inv: 131156)
  )

  (list 131156 "Signature permutation of a Catalan automorphism: row 18 of A122286."
        '(off: 0)
        '(c: "Derived from *A130356 with recursion scheme SPINE.")
        '(indentries: Catsigperm)
        '(inv: 131155)
  )

  (list 131157 "Signature permutation of a Catalan automorphism: row 14 of A122285."
        '(off: 0)
        '(c: "Derived from *A130357 with recursion scheme ENIPS.")
        '(indentries: Catsigperm)
        '(inv: 131158)
  )

  (list 131158 "Signature permutation of a Catalan automorphism: row 19 of A122286."
        '(off: 0)
        '(c: "Derived from *A130358 with recursion scheme SPINE.")
        '(indentries: Catsigperm)
        '(inv: 131157)
  )

  (list 131159 "Signature permutation of a Catalan automorphism: row 15 of A122285."
        '(off: 0)
        '(c: "Derived from *A130359 with recursion scheme ENIPS.")
        '(indentries: Catsigperm)
        '(inv: 131160)
  )

  (list 131160 "Signature permutation of a Catalan automorphism: row 21 of A122286."
        '(off: 0)
        '(c: "Derived from *A130360 with recursion scheme SPINE.")
        '(indentries: Catsigperm)
        '(inv: 131159)
  )

  (list 131161 "Signature permutation of a Catalan automorphism: row 16 of A122285."
        '(off: 0)
        '(c: "Derived from *A130361 with recursion scheme ENIPS.")
        '(indentries: Catsigperm)
        '(inv: 131162)
  )

  (list 131162 "Signature permutation of a Catalan automorphism: row 20 of A122286."
        '(off: 0)
        '(c: "Derived from *A130362 with recursion scheme SPINE.")
        '(indentries: Catsigperm)
        '(inv: 131161)
  )

  (list 131163 "Signature permutation of a Catalan automorphism: row 18 of A122285."
        '(off: 0)
        '(c: "Derived from *A130363 with recursion scheme ENIPS.")
        '(indentries: Catsigperm)
        '(inv: 131164)
  )

  (list 131164 "Signature permutation of a Catalan automorphism: row 13 of A122286."
        '(off: 0)
        '(c: "Derived from *A130364 with recursion scheme SPINE.")
        '(indentries: Catsigperm)
        '(inv: 131163)
  )

  (list 131165 "Signature permutation of a Catalan automorphism: row 19 of A122285."
        '(off: 0)
        '(c: "Derived from *A130365 with recursion scheme ENIPS.")
        '(indentries: Catsigperm)
        '(inv: 131166)
  )

  (list 131166 "Signature permutation of a Catalan automorphism: row 14 of A122286."
        '(off: 0)
        '(c: "Derived from *A130366 with recursion scheme SPINE.")
        '(indentries: Catsigperm)
        '(inv: 131165)
  )

  (list 131167 "Signature permutation of a Catalan automorphism: row 20 of A122285."
        '(off: 0)
        '(c: "Derived from *A130367 with recursion scheme ENIPS.")
        '(indentries: Catsigperm)
        '(inv: 131168)
  )

  (list 131168 "Signature permutation of a Catalan automorphism: row 16 of A122286."
        '(off: 0)
        '(c: "Derived from *A130368 with recursion scheme SPINE.")
        '(indentries: Catsigperm)
        '(inv: 131167)
  )

;;;;;;;;;;;;;;;;;;;;


  (list 130981 "Signature permutation of a Catalan automorphism: row 3 of A122287 and A122288."
        '(off: 0)
        '(c: "The signature-permutation of the self-inverse Catalan automorphism which is derived from *A130341 with recursion scheme FORK or KROF.")
        '(indentries: Catsigperm)
        '(inv: 130981)
  )

  (list 130983 "Signature permutation of a Catalan automorphism: row 4 of A122287 and A122288."
        '(off: 0)
        '(c: "Derived from *A130343 with recursion scheme KROF or from *A130348 with recursion scheme FORK.")
        '(indentries: Catsigperm)
        '(inv: 130984)
  )

  (list 130984 "Signature permutation of a Catalan automorphism: row 6 of A122287 and A122288."
        '(off: 0)
        '(c: "Derived from *A130344 with recursion scheme FORK or from *A130347 with recursion scheme KROF.")
        '(indentries: Catsigperm)
        '(inv: 130983)
  )

  (list 130982 "Signature permutation of a Catalan automorphism: row 5 of A122287 and A122288."
        '(off: 0)
        '(c: "The signature-permutation of the self-inverse Catalan automorphism which is derived from *A130345 with recursion scheme FORK or KROF.")
        '(indentries: Catsigperm)
        '(inv: 130982)
  )

  (list 130985 "Signature permutation of a Catalan automorphism: row 7 of A122288."
        '(off: 0)
        '(c: "The signature-permutation of the self-inverse Catalan automorphism which is derived from *A122282 with recursion scheme KROF.")
        '(indentries: Catsigperm)
        '(inv: 130986)
  )

  (list 130986 "Signature permutation of a Catalan automorphism: row 7 of A122287."
        '(off: 0)
        '(c: "The signature-permutation of the self-inverse Catalan automorphism which is derived from *A122282 with recursion scheme FORK.")
        '(indentries: Catsigperm)
        '(inv: 130985)
  )

  (list 130987 "Signature permutation of a Catalan automorphism: row 8 of A122288."
        '(off: 0)
        '(c: "Derived from *A082339 with recursion scheme KROF.")
        '(indentries: Catsigperm)
        '(inv: 130988)
  )

  (list 130988 "Signature permutation of a Catalan automorphism: row 8 of A122287."
        '(off: 0)
        '(c: "Derived from *A082340 with recursion scheme FORK.")
        '(indentries: Catsigperm)
        '(inv: 130987)
  )

  (list 130989 "Signature permutation of a Catalan automorphism: row 9 of A122288."
        '(off: 0)
        '(c: "Derived from *A130349 with recursion scheme KROF.")
        '(indentries: Catsigperm)
        '(inv: 130990)
  )

  (list 130990 "Signature permutation of a Catalan automorphism: row 11 of A122287."
        '(off: 0)
        '(c: "Derived from *A130350 with recursion scheme FORK.")
        '(indentries: Catsigperm)
        '(inv: 130989)
  )

  (list 130991 "Signature permutation of a Catalan automorphism: row 10 of A122288."
        '(off: 0)
        '(c: "Derived from *A130351 with recursion scheme KROF.")
        '(indentries: Catsigperm)
        '(inv: 130992)
  )

  (list 130992 "Signature permutation of a Catalan automorphism: row 10 of A122287."
        '(off: 0)
        '(c: "Derived from *A130352 with recursion scheme FORK.")
        '(indentries: Catsigperm)
        '(inv: 130991)
  )

  (list 130993 "Signature permutation of a Catalan automorphism: row 11 of A122288."
        '(off: 0)
        '(c: "Derived from *A130353 with recursion scheme KROF.")
        '(indentries: Catsigperm)
        '(inv: 130994)
  )

  (list 130994 "Signature permutation of a Catalan automorphism: row 9 of A122287."
        '(off: 0)
        '(c: "Derived from *A130354 with recursion scheme FORK.")
        '(indentries: Catsigperm)
        '(inv: 130993)
  )

  (list 131009 "Signature permutation of a Catalan automorphism: row 12 of A122288."
        '(off: 0)
        '(c: "Derived from *A074685 with recursion scheme KROF.")
        '(indentries: Catsigperm)
        '(inv: 131010)
  )

  (list 131010 "Signature permutation of a Catalan automorphism: row 17 of A122287."
        '(off: 0)
        '(c: "Derived from *A074686 with recursion scheme FORK.")
        '(indentries: Catsigperm)
        '(inv: 131009)
  )

  (list 130995 "Signature permutation of a Catalan automorphism: row 13 of A122288."
        '(off: 0)
        '(c: "Derived from *A130355 with recursion scheme KROF.")
        '(indentries: Catsigperm)
        '(inv: 130996)
  )

  (list 130996 "Signature permutation of a Catalan automorphism: row 18 of A122287."
        '(off: 0)
        '(c: "Derived from *A130356 with recursion scheme FORK.")
        '(indentries: Catsigperm)
        '(inv: 130995)
  )

  (list 130997 "Signature permutation of a Catalan automorphism: row 14 of A122288."
        '(off: 0)
        '(c: "Derived from *A130357 with recursion scheme KROF.")
        '(indentries: Catsigperm)
        '(inv: 130998)
  )

  (list 130998 "Signature permutation of a Catalan automorphism: row 19 of A122287."
        '(off: 0)
        '(c: "Derived from *A130358 with recursion scheme FORK.")
        '(indentries: Catsigperm)
        '(inv: 130997)
  )

  (list 130999 "Signature permutation of a Catalan automorphism: row 15 of A122288."
        '(off: 0)
        '(c: "Derived from *A130359 with recursion scheme KROF.")
        '(indentries: Catsigperm)
        '(inv: 131000)
  )

  (list 131000 "Signature permutation of a Catalan automorphism: row 21 of A122287."
        '(off: 0)
        '(c: "Derived from *A130360 with recursion scheme FORK.")
        '(indentries: Catsigperm)
        '(inv: 130999)
  )

  (list 131001 "Signature permutation of a Catalan automorphism: row 16 of A122288."
        '(off: 0)
        '(c: "Derived from *A130361 with recursion scheme KROF.")
        '(indentries: Catsigperm)
        '(inv: 131002)
  )

  (list 131002 "Signature permutation of a Catalan automorphism: row 20 of A122287."
        '(off: 0)
        '(c: "Derived from *A130362 with recursion scheme FORK.")
        '(indentries: Catsigperm)
        '(inv: 131001)
  )

  (list 131003 "Signature permutation of a Catalan automorphism: row 18 of A122288."
        '(off: 0)
        '(c: "Derived from *A130363 with recursion scheme KROF.")
        '(indentries: Catsigperm)
        '(inv: 131004)
  )

  (list 131004 "Signature permutation of a Catalan automorphism: row 13 of A122287."
        '(off: 0)
        '(c: "Derived from *A130364 with recursion scheme FORK.")
        '(indentries: Catsigperm)
        '(inv: 131003)
  )

  (list 131005 "Signature permutation of a Catalan automorphism: row 19 of A122288."
        '(off: 0)
        '(c: "Derived from *A130365 with recursion scheme KROF.")
        '(indentries: Catsigperm)
        '(inv: 131006)
  )

  (list 131006 "Signature permutation of a Catalan automorphism: row 14 of A122287."
        '(off: 0)
        '(c: "Derived from *A130366 with recursion scheme FORK.")
        '(indentries: Catsigperm)
        '(inv: 131005)
  )

  (list 131007 "Signature permutation of a Catalan automorphism: row 20 of A122288."
        '(off: 0)
        '(c: "Derived from *A130367 with recursion scheme KROF.")
        '(indentries: Catsigperm)
        '(inv: 131008)
  )

  (list 131008 "Signature permutation of a Catalan automorphism: row 16 of A122287."
        '(off: 0)
        '(c: "Derived from *A130368 with recursion scheme FORK.")
        '(indentries: Catsigperm)
        '(inv: 131007)
  )


 )
)



;; (load "../Schemuli/GF2Xfuns")
;; (cd "seqs/batch20jun")
;; (output-entries-to-file120_45 seqs-2007Jun20 "2007Jun20.scm" "Jun 20 2007")

(define seqs-2008Dec21
 (list

  (list 153239 "Balance of binary trees as ordered by A014486: number of vertices in the right subtree - number of vertices in the left subtree."
        '(off: 0)
        '(create-b-file: 2055)
        (list 'y: (string-append "A153243 gives the positions of zeros. Cf. A153240, A153241."))
        (list 'scheme:
          '(
(define (A153239 n)
  (let ((s (A014486->parenthesization (A014486 n))))
     (if (null? s) 0 (- (count-pars (cdr s)) (count-pars (car s))))
  )
)
(define (count-pars a)
    (cond ((not (pair? a)) 0)
          (else (+ 1 (count-pars (car a)) (count-pars (cdr a))))
    )
)
          )
        )
  )

  (list 153240 "Balance of general trees as ordered by A014486, variant A."
        '(off: 0)
        '(create-b-file: 2055)
        '(c: "This differs from variant A153241 only in that if the degree of the tree is odd (i.e. A057515(n) = 1 mod 2), then the balance of the center-subtree is always taken into account.")
        (list 'y: (string-append
"Differs from variant A153241 for the first time at n=268, where"
" A153241(268) = 1, while a(268)=2."
" Note that (A014486->parenthesization (A014486 268)) = (() (() (())) (()))."
" a(A061856(n)) = 0 for all n."
" Cf. also A153239."
                  )
        )

        (list 'scheme:
           '(
(define (A153240 n) (gentree-deep-balance (A014486->parenthesization (A014486 n))))

(define (gentree-deep-balance l)
  (let ((r (reverse l))) ;; Same list reversed, scanned from right.
     (let loop ((i 0) (j (- (length l) 1)) (l l) (r r) (z 0))
            (cond ((= i j) (+ z (gentree-deep-balance (car l))))
                  ((> i j) z) ;; Even number of top-lev elements? Return z
                  (else (loop (+ i 1) (- j 1) (cdr l) (cdr r)
                              (+ z (- (count-pars (car r))
                                      (count-pars (car l))
                                   )
                              )
                        )
                  )
           )
     )
  )
)
(define (count-pars a)
    (cond ((not (pair? a)) 0)
          (else (+ 1 (count-pars (car a)) (count-pars (cdr a))))
    )
)
          )
        )
  )

  (list 153241 "Balance of general trees as ordered by A014486, variant B."
        '(off: 0)
        '(create-b-file: 2055)
        '(c: "This differs from variant A153240 only in that if the degree of the tree is odd (i.e. A057515(n) = 1 mod 2), then the balance of the center-subtree is taken into account ONLY if the total weight of other subtrees at the left and the right hand side from the center were balanced against each other.")
        (list 'y: (string-append
"Differs from variant A153240 for the first time at n=268, where"
" A153240(268) = 2, while a(268)=1."
" Note that (A014486->parenthesization (A014486 268)) = (() (() (())) (()))."
" a(A061856(n)) = 0 for all n."
" Cf. also A153239."
                  )
        )

        (list 'scheme:
           '(

(define (A153241 n) (gentree-balance (A014486->parenthesization (A014486 n))))

;; Like previous, but examine the center-element (from the odd-length lists)
;; only if the other elements were balanced (or the length of list is 1):

(define (gentree-balance l)
  (let ((r (reverse l))) ;; Same list reversed, scanned from right.
     (let loop ((i 0) (j (- (length l) 1)) (l l) (r r) (z 0))
            (cond ((= i j) (+ z (if (zero? z) (gentree-balance (car l)) 0)))
                  ((> i j) z) ;; Even number of top-lev elements? Return z
                  (else (loop (+ i 1) (- j 1) (cdr l) (cdr r)
                              (+ z (- (count-pars (car r))
                                      (count-pars (car l))
                                   )
                              )
                        )
                  )
           )
     )
  )
)
(define (count-pars a)
    (cond ((not (pair? a)) 0)
          (else (+ 1 (count-pars (car a)) (count-pars (cdr a))))
    )
)
          )
        )
  )

  (list 153242 "Positions of general trees in A014486 whose degree is not one."
        '(off: 0)
        (list 'y: (string-append "I.e. such i that A057515(i) is not 1. Complement of A057548."))
  )

  (list 153243 "Positions of balanced binary trees in A014486, i.e. trees where number of vertices in the right subtree = number of vertices in the left subtree."
        '(off: 0)
        (list 'y: (string-append "Positions of zeros in A153239."))
  )

 )
)



(define seqs-2008Dec22
 (list

  (list 153246 "Number of fleeing trees computed for Catalan bijection A057164."
       '(off: 0)
        (list 'c: (string-append
"A \"fleeing tree\" sequence computed for Catalan bijection CatBij gives"
" for each binary tree A014486(n) the number of cases where, when a new V (bud)"
" is inserted into one of the A072643(n)+1 possible leaves of that tree,"
" it follows that (CatBij tree) is not a subtree of (CatBij tree-with-bud-inserted)."
" I.e. for each tree A014486(n), we compute Sum_{i=0}^A072643(n)"
" (1 if catbij(n) is a subtree of catbij(A153250bi(n,i)), 0 otherwise)."
" Here A153250 gives the bud-inserting operation."
" Note that for any Catalan Bijection, which is an image of \"psi\" isomorphism (see A153141)"
" from the Automorphism Group of infinite binary trees, the result will be"
" A000004, the zero-sequence. To satisfy that condition, CatBij should at"
" least satisfy A127302(CatBij(n)) = A127302(n) for all n (clearly A057164"
" does not satisfy that, so we got non-zero terms here). However, that"
" is just a necessary but not a sufficient condition."
" For example, A123493 & A123494 satisfy it, "
" but still they produce non-zero sequences: A153247, A153248."
                  )
        )
        (list 'y: (string-append "Cf. A082858, A153250, A153247, A153248."))
        (list 'scheme:
           '(
  (define (A153246 n) (count-fleeing-trees n A057164))
  (define (count-fleeing-trees n catbij)
    (add (lambda (i)
            (if (= (A082858bi (catbij (A153250bi n i)) (catbij n)) (catbij n))
                0
                1
            )
         )
         0
         (A072643 n)
    )
  )
(define (add intfun lowlim uplim)
   (let sumloop ((i lowlim) (res 0))
      (cond ((> i uplim) res)
            (else (sumloop (1+ i) (+ res (intfun i))))
      )
   )
)
            )
        )
  )

  (list 153247 "Number of fleeing trees computed for Catalan bijection A123493."
       '(off: 0)
        (list 'c: "See the comments at A153246. Essentially, A123493 does not extend uniquely to an automorphism of infinite binary tree, because its behaviour is dependent on whether certain vertices of a finite binary tree are leaves (terminal nodes) or not. Similarly for bijections like A127387 and A127379.")
        (list 'y: (string-append "Cf. A153248."))
        (list 'scheme: '(define (A153247 n) (count-fleeing-trees n A123493)))
  )


  (list 153248 "Number of fleeing trees computed for Catalan bijection A123494."
       '(off: 0)
        (list 'c: "See the comments at A153247.")
        (list 'y: (string-append "Cf. A153248."))
        (list 'scheme: '(define (A153248 n) (count-fleeing-trees n A123494)))
  )

  (list 153249 "Non-zero terms of table A153250 collected into one sequence row by row."
       '(off: 0)
       '(keywords: "tabf")
        (list 'c: "Row n (starting from row 0) contains A072643(n)+1 terms.")
  )

;; Top left corner of array: 
;;  1, 0, 0, 0, 0, ...
;;  2, 3, 0, 0, 0, ...
;;  4, 5, 6, 0, 0, ...
;;  6, 7, 8, 0, 0, ...
;;  9,10,11,14, 0, ...
;; 11,12,13,16, 0, ...
;; 14,15,16,22, 0, ...

  (list 153250 "Array A(x,y): A(0,0), A(1,0), A(0,1), A(2,0), A(1,1), A(0,2), ... formed by growing a bud (a single V-node) to the y:th leaf of the binary tree A014486(x)."
       '(off: 0)
       '(upto: 90)
       '(keywords: "tabl")
       '(create-b-file: 1274) ;; A000217(50)-1
        (list 'c: (string-append
"Note: the leaf-positions are indexed so that the rightmost one in the tree is the leaf 0, et cetera, up to the leftmost one, which is the leaf with index A072643(x)."
" In this manner, terms on each row stay in monotone order."
" Row n (starting from row 0) contains A072643(n)+1 non-zero terms, and then infinite number of zeroes after that. A153249 gives only the non-zero terms. Can be used to compute \"fleeing tree\" sequences for Catalan bijections. See comments at A153246."
                  )
        )
        (list 'y: "A002262, A025581.")
        (list 'scheme:
           '(
(define (A153250 n) (A153250bi (A002262 n) (A025581 n)))
(define (A153250bi x y) (A080300 (parenthesization->A014486 (bud! (A014486->parenthesization (A014486 x)) y))))

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
             )
        )
  	
  )
 )
)

;; Do this if you want to compute a bit more terms in a reasonable time:
;; (define vecA000040 (fasload "/home/karttu/Schemuli/primes_up_to_65537nth.vec"))
;; (define (A000040 n) (vector-ref vecA000040 (- n 1)))


(define seqs-2009Jan03
 (list

  (list 153835 "The first representative in A014486 for each equivalence class of non-oriented binary tree corresponding to the oriented (plane) binary tree encoded by A014486(n)."
       '(off: 0)
       '(create-b-file: 2055)
        (list 'c: (string-append
"Any n that occurs in the sequence, occurs for the first time at n, (a(n)=n),"
" i.e. there are no cases where a(n) > n."
" A001190(n+1) distinct values occur in each range [A014137(n-1)..A014138(n-1)]."
" This sequence is similar to A127302 in that it maps all the plane binary trees"
" which belong to the same equivalence class of non-oriented binary trees"
" to one and same integer, and likewise, every"
" Catalan bijection whose signature permutation SP satisfies the condition"
" mentioned in A127302, satisfies in a similar way A153835(SP(n)) = A153835(n)."
                  )
        )
        (list 'y: (string-append "A127302(a(n)) = A127302(n) holds for all n."
                                 " Cf. A154103, A069770."
                  )
        )

        (list 'scheme:

'(define (*A153835! s)
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

        )
  )

  (list 154103 "Auxiliary array for computing A153835."
       '(off: 0)
       '(upto: 104)
       '(keywords: "tabl")
       '(c: "A(x,y): A(0,0), A(1,0), A(0,1), A(2,0), A(1,1), A(0,2), ... is 1 if y=0 and x!=0, or if A085207bi(2*x, y) is greater than A085207bi(2*y, x), otherwise 0.")
       '(create-b-file: 1274) ;; A000217(50)-1
        (list 'y: "Transpose: A154104. Cf. A085207, A002262, A025581.")
        (list 'scheme:
'(
(define (A154103bi x y)
   (cond ((and (zero? y) (not (zero? x))) 1)
         ((> (A085207bi (* 2 x) y) (A085207bi (* 2 y) x)) 1)
         (else 0)
   )
)
(define (A154103 n) (A154103bi (A002262 n) (A025581 n)))
 )

        )
  )

  (list 154104 "Transpose of array A154103."
       '(off: 0)
       '(upto: 104)
       '(keywords: "tabl")
;;     '(create-b-file: 1274) ;; A000217(50)-1
       '(scheme: (define (A154104 n) (A154103bi (A025581 n) (A002262 n))))
  )

 )
)



;; (load "../Schemuli/GF2Xfuns")
;; (cd "seqs/batch2009jan06")
;; (output-entries-to-file120_45 seqs-2009Jan06 "2009Jan06.scm" "Jan 06 2009")

(define seqs-2009Jan06
 (list

  (list 154121 "Signature permutation of a Catalan bijection: row 3655 of A089840."
        '(off: 0)
        (list 'c: (string-append "This bijection of binary trees can be obtained"
" by applying bijection *A074679 to the right hand side subtree,"
" and leaving the left hand side subtree intact (Thompson's B!):"
 "\n%C A154121 ....C...D.......B...C"
 "\n%C A154121 .....\\./.........\\./"
 "\n%C A154121 ..B...x....-->....x...D.................B..().........()..B.."
 "\n%C A154121 ...\\./.............\\./...................\\./....-->....\\./..."
 "\n%C A154121 A...x...........A...x.................A...x.........A...x...."
 "\n%C A154121 .\\./.............\\./...................\\./...........\\./....."
 "\n%C A154121 ..x...............x.....................x.............x......"
 "\n%C A154121 ............................................................."
 "\n%C A154121 That is, (a . (b . (c . d))) -> (a . ((b . c) . d))"
 "\n%C A154121 or (a . (b . ())) --> (a . (() . b)) if the former is not possible."
                  )
        )
        '(indentries: Catsigperm)
        '(inv: 154122)
        '(comps: (069770 089865 069770))
        '(y: "Cf. A154123.")
        '(scheme: "(define (*A154121! s) (if (pair? s) (*A074679! (cdr s))) s)")
  )

  (list 154122 "Signature permutation of a Catalan bijection: row 3747 of A089840."
        '(off: 0)
        (list 'c: (string-append "This bijection of binary trees can be obtained"
" by applying bijection *A074680 to the right hand side subtree,"
" and leaving the left hand side subtree intact (Inverse of Thompson's B!):"

 "\n%C A154122 .B...C...............C...D"
 "\n%C A154122 ..\\./.................\\./"
 "\n%C A154122 ...x...D....-->....B...x.................()..C ........C...()"
 "\n%C A154122 ....\\./.............\\./...................\\./....-->....\\./..."
 "\n%C A154122 .A...x...........A...x.................A...x.........A...x...."
 "\n%C A154122 ..\\./.............\\./...................\\./...........\\./....."
 "\n%C A154122 ...x...............x.....................x.............x......"
 "\n%C A154122 .............................................................."
 "\n%C A154122 That is, (a . ((b . c) . d)) -> (a . (b . (c . d)))"
 "\n%C A154122 or (a . (() . c)) -> (a . (c . ())) if the former is not possible."
                  )
        )
        '(indentries: Catsigperm)
        '(inv: 154121)
        '(comps: (069770 089866 069770))
        '(y: "Cf. A154124.")
        '(scheme: "(define (*A154122! s) (if (pair? s) (*A074680! (cdr s))) s)")
  )

  (list 154123 "Signature permutation of a Catalan bijection: row 3656 of A089840."
        '(off: 0)
        (list 'c: (string-append "This bijection of binary trees is obtained"
" in the following way. (Thompson's B!):"
 "\n%C A154123 ....C...D.......B...C"
 "\n%C A154123 .....\\./.........\\./"
 "\n%C A154123 ..B...x....-->....x...D.................B..().........()..A.."
 "\n%C A154123 ...\\./.............\\./...................\\./....-->....\\./..."
 "\n%C A154123 A...x...........A...x.................A...x.........B...x...."
 "\n%C A154123 .\\./.............\\./...................\\./...........\\./....."
 "\n%C A154123 ..x...............x.....................x.............x......"
 "\n%C A154123 ............................................................."
 "\n%C A154123 That is, (a . (b . (c . d))) -> (a . ((b . c) . d))"
 "\n%C A154123 or (a . (b . ())) --> (b . (() . a)) if the former is not possible."
                  )
        )
        '(indentries: Catsigperm)
        '(inv: 154124)
        '(y: "Cf. A154121.")
        '(scheme: 
 (define (*A154123! s) ;; /* A089840[3656] */
  (if (and (pair? s) (pair? (cdr s)) (pair? (cddr s)))
      (*A074679! (cdr s))
      (*A089851! s)
  )
  s
 )
        )
  )


  (list 154124 "Signature permutation of a Catalan bijection: row 3748 of A089840."
        '(off: 0)
        (list 'c: (string-append "This bijection of binary trees is obtained"
" in the following way. (Inverse of Thompson's B!):"
 "\n%C A154124 .B...C...............C...D"
 "\n%C A154124 ..\\./.................\\./"
 "\n%C A154124 ...x...D....-->....B...x.................()..C ........A...()"
 "\n%C A154124 ....\\./.............\\./...................\\./....-->....\\./..."
 "\n%C A154124 .A...x...........A...x.................A...x.........C...x...."
 "\n%C A154124 ..\\./.............\\./...................\\./...........\\./....."
 "\n%C A154124 ...x...............x.....................x.............x......"
 "\n%C A154124 .............................................................."
 "\n%C A154124 That is, (a . ((b . c) . d)) -> (a . (b . (c . d)))"
 "\n%C A154124 or (a . (() . c)) -> (c . (a . ())) if the former is not possible."
                  )
        )
        '(indentries: Catsigperm)
        '(inv: 154123)
        '(y: "Cf. A154122.")
        '(scheme: 
(define (*A154124! s) ;; /* A089840[3748] */
  (if (and (pair? s) (pair? (cdr s)) (pair? (cadr s)))
      (*A074680! (cdr s))
      (*A089853! s)
  )
  s
)
        )
  )


  (list 154125 "Self-inverse signature permutation of a Catalan bijection: row 83 of A089840."
        '(off: 0)
        (list 'c: (string-append "This bijection of binary trees swaps"
" the left and right subtree of a binary tree, but ONLY if BOTH are non-empty."
" If either the left or right hand side tree is empty, fixes the tree."
 "\n%C A154125 .A...B.C...D.......C...D.A...B."
 "\n%C A154125 ..\\./...\\./.........\\./...\\./.."
 "\n%C A154125 ...x.....x...--->....x.....x..."
 "\n%C A154125 ....\\.../.............\\.../...."
 "\n%C A154125 ......x.................x......"
 "\n%C A154125 ..............................."
 "\n%C A154125 ((a . b) . (c . d)) -> ((c . d) . (a . b))"
 "\n%C A154125 or fix, if either the left or right hand side subtree is empty."
                  )
        )
        '(indentries: Catsigperm)
        '(inv: 154125)
        '(comps: (069770 154126) (154126 069770))
        '(scheme: "(define (*A154125! s) (if (and (pair? s) (pair? (car s)) (pair? (cdr s))) (*A069770! s)) s)")
  )

  (list 154126 "Self-inverse signature permutation of a Catalan bijection: row 183 of A089840."
        '(off: 0)
        (list 'c: (string-append "This bijection of binary trees swaps"
" the left and right subtree of a binary tree, but ONLY if either of them is empty."
" If both the left and right hand side tree is non-empty, fixes the tree."
 "\n%C A154126 .A...B.C...D.......A...B.C...D....."
 "\n%C A154126 ..\\./...\\./.........\\./...\\./........................"
 "\n%C A154126 ...x.....x...--->....x.....x.......A...B.......B...A."
 "\n%C A154126 ....\\.../.............\\.../.........\\./..--->...\\./.."
 "\n%C A154126 ......x.................x............x...........x..."
 "\n%C A154126 ..............................(where either A or B is (), a leaf)"
                  )
        )
        '(indentries: Catsigperm)
        '(inv: 154126)
        '(comps: (069770 154125) (154125 069770))
        '(scheme: "(define (*A154126! s) (if (and (pair? s) (or (not (pair? (car s))) (not (pair? (cdr s))))) (*A069770! s)) s)")
  )
 )
)




(define A154470 (catfun1 *A154470))
(definec (A154471 n) (if (= 0 n)  31706 (A154470 (A154471 (-1+ n)))))
(define (A154472 n) (A126309 (A154471 n)))
(define (A154473 n) (A014486 (A154472 n)))
(define (A154474 n) (A007088 (A154473 n)))
(define (A154475 n) (A072643 (A154472 n)))
(define (A154476 n) (- (A072643 (A154471 n)) (A072643 (A154472 n))))

(define A154477 (compose-funs A153240 A080068))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define A154449 (catfun1 *A154449!))
(define A154450 (catfun1 *A154450!))
(define A154451 (catfun1 *A154451!))
(define A154452 (catfun1 *A154452!))
(define A154453 (catfun1 *A154453!))
(define A154454 (catfun1 *A154454!))
(define A154455 (catfun1 *A154455!))
(define A154456 (catfun1 *A154456!))

(define A154457 (catfun1 *A154457!))
(define A154458 (catfun1 *A154458!))

;; Where are these? Were they duplicate of some other sign.perms?
;; (define A154461 (catfun1 *A154461!))
;; (define A154462 (catfun1 *A154462!))
;; (define A154463 (catfun1 *A154463!))
;; (define A154464 (catfun1 *A154464!))



;; (load "../Schemuli/GF2Xfuns")
;; (cd "seqs/batch20jun")
;; (output-entries-to-file120_45 seqs2009Jan11 "2007Jan11.scm" "Jan 11 2009")

(define seqs2009Jan11
 (list

  (list 154470 "Rewriting function for Wolfram's e[x_][y_] -> x[x[y]] symbolic system transformation, implemented on nihilistic S-expressions encoded by A014486."
        '(off: 0)
        (list 'c: (string-append
"Here we use the empty list () as a marker for symbol 'e'."
" Any sublist like (... ()(x1 x2 ... xn)(y1 y2 ... yn) ...) is transformed"
" into the form (... x1 x2 ... xn (x1 x2 ... xn (y1 y2 ... yn)) ...)."
" A54471 gives the iterations, starting from initial value 31706."
                  )
        )
        '(scheme:
(define (*A154470 s)
   (cond ((null? s) s)
         ((and (null? (car s))
               (>= (length s) 3)
               (pair? (second s))
               (pair? (third s))
          )
            (append (second s)
                    (cons (append (second s) (list (third s)))
                          (*A154470 (cdddr s))
                    )
            )
         )
         (else (cons (*A154470 (car s)) (*A154470 (cdr s))))
   )
)
        )

  )

  (list 154471 "Function A154470 iterated, starting from the initial value 31706."
        '(off: 0)
        '(create-b-file: 100)
        '(f: "a(0) = 31706, a(n) = A154470(a(n-1)).")
        (list 'c: (string-append
" Note how A014486(31706) = 2988236, and (A014486->parenthesization 2988236) = "
" (() (() (()) (())) (()) (())), from which, when after converting ()'s to e's"
" we get: (e (e (e) (e)) (e) (e)), corresponding to the initial state"
" e[e[e][e]][e][e] of Wolfram's system."
" A154472 gives the corresponding sequence with ()'s removed."
                  )
        )
        '(scheme: "(define (A154471 n) (if (= 0 n)  31706 (A154470 (A154471 (-1+ n)))))")
  )

  (list 154472 "a(n) = A126309(A154471(n)), A154471 with ()'s removed."
        '(off: 0)
        '(create-b-file: 100)
        (list 'c: (string-append
"This sequence essentially gives the iterated S-expressions (their A014486-indices)"
" of the sequence A154471, with ()'s removed. See A154473."
                  )
        )
        '(comps: (80300 154473))
  )

  (list 154473 "a(n) = A014486(A154472(n))"
       '(keywords: "base")
        '(off: 0)
        '(create-b-file: 100)
        (list 'c: (string-append
"This sequence gives the parenthesis expressions shown at the upper"
" right corner image of the page 103 of NKS, with the left brackets (black squares)"
" converted to 1's, and the right brackets (white squares) converted to 0's,"
" and then interpreting each such number as a binary number, and converted"
" to decimal. A154474 shows the corresponding binary representations."
                  )
        )
  )

  (list 154474 "a(n) = A007088(A154473(n))"
       '(keywords: "base")
        '(off: 0)
        (list 'c: (string-append
"This sequence gives the parenthesis expressions shown at the upper"
" right corner image of the page 103 of NKS, with left brackets (black squares)"
" converted to 1's, and right brackets (white squares) converted to 0's."
                  )
        )
  )

  (list 154475 "Number of opening (equally: closing) brackets in each term of A154472-A154474."
        '(off: 0)
        '(create-b-file: 100)
        '(f: "a(n) = A072643(A154472(n)).")
        (list 'c: (string-append
"2*a(n) gives the number of bits in A154474(n)."
                  )
        )
        '(y: "a(n) = A029837(1+A154473(n))/2. a(n) = A154476(n)-1.")
  )


  (list 154476 "Number of e's in each iteration of Wolfram's e[x_][y_] -> x[x[y]] symbolic rewriting system, starting from the initial state e[e[e][e]][e][e]"
        '(off: 0)
        '(create-b-file: 100)
        '(f: "a(n) = A072643(A154471(n)) - A072643(A154472(n)).")
        '(y: "a(n) = A154476(n)+1.")
  )

  (list 154477 "a(n) = A153240(A080068(n))"
        '(off: 0)
        '(create-b-file: 512)
        (list 'c: (string-append
"This sequence gives some indication how well the terms of A080068"
" are balanced as general trees, which has some implications"
" as to the correctness of A123050 (see comments at A080070)."
                  )
        )
        '(y: "See also A080071.")
  )

 )
)



;; (load "../Schemuli/GF2Xfuns")
;; (cd "seqs/batch2009jan06")
;; (output-entries-to-file120_45 seqs-2009Jan14 "2009Jan14.scm" "Jan 06 2009")

;; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

(define seqs-2009Jan17
 (list

;; These two just for checking:
  (list 069767 "Signature permutation of a Catalan bijection Knick."
        '(off: 0)
;;      '(indentries: Catsigperm)
        '(inv: 069768)
        '(comps: (154450 154455) (154452 154453))
  )

  (list 069768 "Signature permutation of a Catalan bijection Knick."
        '(off: 0)
;;      '(indentries: Catsigperm)
        '(inv: 069767)
        '(comps: (154454 154451) (154456 154449))
  )

  (list 154449 "Signature permutation of a Catalan bijection: Inverse of generator \"a\" of the rightward recursing instance of Basilica group: a = (1,b), b = s(1,a)."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 154450)
        '(comps: (154455 069768) (57163 154453 57163))
        (list 'c: (string-append
"This automorphism of rooted plane binary trees"
" switches the two descendant trees"
" for every other vertex"
" as it returns back toward the root,"
" after descending down to the rightmost tip of the tree along the 111... ray,"
" so that the last vertex whose descendants are swapped,"
" is the right-hand side child of the root,"
" and the root itself is fixed."
" Specifically, *A154449 = psi(A154439),"
" where the isomorphism psi is given in A153141 (see further comments there)."
                  )
        )
        '(y: "Cf. A154451.")
        '(scheme: "(define (*A154449! s) (if (pair? s) (*A154451! (cdr s))) s)")
  )

  (list 154450 "Signature permutation of a Catalan bijection: Generator \"a\" of the rightward recursing instance of Basilica group: a = (1,b), b = s(1,a)."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 154449)
        '(comps: (069767 154456) (57163 154454 57163))
        (list 'c: (string-append
"This automorphism of rooted plane binary trees"
" switches the two descendant trees"
" for every other vertex"
" as it descends"
" along the 111... ray,"
" but not starting swapping until at the right-hand side child of the root,"
" leaving the root itself fixed."
" Specifically, *A154450 = psi(A154440),"
" where the isomorphism psi is given in A153141 (see further comments there)."
                  )
        )
        '(y: "Cf. A069770, A154452.")
        '(scheme: "(define (*A154450! s) (if (pair? s) (*A154452! (cdr s))) s)")
  )

  (list 154451 "Signature permutation of a Catalan bijection: Inverse of generator \"b\" of the rightward recursing instance of Basilica group: a = (1,b), b = s(1,a)."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 154452)
        '(comps: (154453 069768) (57163 154455 57163))
        (list 'c: (string-append
"This automorphism of rooted plane binary trees"
" switches the two descendant trees"
" for every other vertex"
" as it returns back toward the root,"
" after descending down to the rightmost tip of the tree along the 111... ray,"
" so that the last vertex whose descendants are swapped"
" is the root node of the tree."
" Specifically, *A154451 = psi(A154441),"
" where the isomorphism psi is given in A153141 (see further comments there)."
                  )
        )
        '(y: "Cf. A069770, A154449.")
        '(scheme: "(define (*A154451! s) (cond ((pair? s) (*A154449! (cdr s))  (*A069770! s))) s)")
  )

  (list 154452 "Signature permutation of a Catalan bijection: Generator \"b\" of the rightward recursing instance of Basilica group: a = (1,b), b = s(1,a)."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 154451)
        '(comps: (069767 154454) (57163 154456 57163))
        (list 'c: (string-append
"This automorphism of rooted plane binary trees"
" switches the two descendant trees"
" for every other vertex"
" as it descends"
" along the 111... ray,"
" starting swapping already at the root."
" Specifically, *A154452 = psi(A154442),"
" where the isomorphism psi is given in A153141 (see further comments there)."
                  )
        )
        '(y: "Cf. A069770, A154450.")
        '(scheme: "(define (*A154452! s) (cond ((pair? s) (*A069770! s) (*A154450! (cdr s)))) s)")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;


  (list 154453 "Signature permutation of a Catalan bijection: Inverse of generator \"a\" of the leftward recursing instance of Basilica group: a = (b,1), b = s(a,1)."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 154454)
        '(comps: (154451 069767) (57163 154449 57163))
        (list 'c: (string-append
"This automorphism of rooted plane binary trees"
" switches the two descendant trees"
" for every other vertex"
" as it returns back toward the root,"
" after descending down to the leftmost tip of the tree along the 000... ray,"
" so that the last vertex whose descendants are swapped,"
" is the left-hand side child of the root,"
" and the root itself is fixed."
" Specifically, *A154453 = psi(A154443),"
" where the isomorphism psi is given in A153141 (see further comments there)."
                  )
        )
        '(y: "Cf. A154455.")
        '(scheme: "(define (*A154453! s) (if (pair? s) (*A154455! (car s))) s)")
  )

  (list 154454 "Signature permutation of a Catalan bijection: Generator \"a\" of the leftward recursing instance of Basilica group: a = (b,1), b = s(a,1)."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 154453)
        '(comps: (069768 154452) (57163 154450 57163))
        (list 'c: (string-append
"This automorphism of rooted plane binary trees"
" switches the two descendant trees"
" for every other vertex"
" as it descends"
" along the 000... ray,"
" but not starting swapping until at the left-hand side child of the root,"
" leaving the root itself fixed."
" Specifically, *A154454 = psi(A154444),"
" where the isomorphism psi is given in A153141 (see further comments there)."
                  )
        )
        '(y: "Cf. A069770, A154456.")
        '(scheme: "(define (*A154454! s) (if (pair? s) (*A154456! (car s))) s)")
  )

  (list 154455 "Signature permutation of a Catalan bijection: Inverse of generator \"b\" of the leftward recursing instance of Basilica group: a = (b,1), b = s(a,1)."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 154456)
        '(comps: (154449 069767) (57163 154451 57163))
        (list 'c: (string-append
"This automorphism of rooted plane binary trees"
" switches the two descendant trees"
" for every other vertex"
" as it returns back toward the root,"
" after descending down to the leftmost tip of the tree along the 000... ray,"
" so that the last vertex whose descendants are swapped"
" is the root node of the tree."
" Specifically, *A154455 = psi(A154445),"
" where the isomorphism psi is given in A153141 (see further comments there)."
                  )
        )
        '(y: "Cf. A069770, A154453.")
        '(scheme: "(define (*A154455! s) (cond ((pair? s) (*A154453! (car s))  (*A069770! s))) s)")
  )

  (list 154456 "Signature permutation of a Catalan bijection: Generator \"b\" of the leftward recursing instance of Basilica group: a = (b,1), b = s(a,1)."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 154455)
        '(comps: (069768 154450) (57163 154452 57163))
        (list 'c: (string-append
"This automorphism of rooted plane binary trees"
" switches the two descendant trees"
" for every other vertex"
" as it descends"
" along the 000... ray,"
" starting swapping already at the root."
" Specifically, *A154456 = psi(A154446),"
" where the isomorphism psi is given in A153141 (see further comments there)."
                  )
        )
        '(y: "Cf. A069770, A154454.")
        '(scheme: "(define (*A154456! s) (cond ((pair? s) (*A069770! s) (*A154454! (car s)))) s)")
  )

;; 154457 & 154458!!!

  (list 154457 "Signature-permutation of a Catalan bijection induced by wreath recursion a=s(b,c), b=s(c,a), c=(c,c), starting from state b."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 154458)
        '(comps: (57163 154458 57163))
        (list 'c: (string-append
"This Catalan bijection is induced by"
" the second generator of group 2861 mentioned on page 144 of"
" \"Classification of groups generated by 3-state automata over a 2-letter alphabet\" paper."
" Specifically, *A154457 = psi(A154447),"
" where the isomorphism psi is given in A153141 (see further comments there)."
                  )
        )
        '(y: "Cf. A072376, A153141-A153142, A154435-A154436, A154439-A154446. Corresponds to A154457 in the group of Catalan bijections.")
        '(scheme: "(define (*A154457! s) (cond ((pair? s) (*A069770! s) (*A154458! (car s)))) s)")
  )

  (list 154458 "Signature-permutation of a Catalan bijection induced by wreath recursion a=s(b,c), b=s(c,a), c=(c,c), starting from state a."
        '(off: 0)
        '(indentries: Catsigperm)
        '(inv: 154457)
        '(comps: (57163 154457 57163))
;; %H A154448 Bondarenko, Grigorchuk, Kravchenko, Muntyan, Nekrashevych, Savchuk, Sunic, <A HREF="http://arxiv.org/abs/0803.3555">Classification of groups generated by 3-state automata over a 2-letter alphabet</A>, p. 144.
        (list 'c: (string-append
"This Catalan bijection is induced by"
" the first generator of group 2861 mentioned on page 144 of"
" \"Classification of groups generated by 3-state automata over a 2-letter alphabet\" paper."
" Specifically, *A154458 = psi(A154448),"
" where the isomorphism psi is given in A153141 (see further comments there)."
                  )
        )
        '(y: "Cf. A072376, A153141-A153142, A154435-A154436, A154439-A154446. Corresponds to A154458 in the group of Catalan bijections.")
        '(scheme: "(define (*A154458! s) (cond ((pair? s) (*A069770! s) (*A154457! (cdr s)))) s)")
  )


  (list 154461 "Signature-permutation of a Catalan bijection induced by Lamplighter group generating wreath recursion, variant 3: a = s(a,b), b = (a,b), starting from the state b."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append
"This Catalan bijection is induced by  Lamplighter group generating wreath"
" recursion (i.e. binary transducer) a = s(a,b), b = (a,b)"
" (where s means swap)"
" listed as the third variant on page 104 of Bondarenko, Grigorchuk, et al. paper,"
" starting from the state b."
" This automorphism is RIBS-transformation (explained in A122200) of the automorphism *A122301."
                  )
        )
        '(inv: 154462)
        '(comps: (57163 154463 57163) (122301 069770) (057163 122301))
        '(scheme: "(define *A154461! (!RIBS *A122301!)) or equally: (define (*A154461! s) (for-each *A122301! s) s)")
  )

  (list 154462 "Signature-permutation of a Catalan bijection induced by Lamplighter group generating wreath recursion, variant 1: a = s(b,a), b = (a,b), starting from the state b."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append
"This Catalan bijection is induced by  Lamplighter group generating wreath"
" recursion (i.e. binary transducer) a = s(b,a), b = (a,b)"
" (where s means swap)"
" listed as the first variant on page 104 of Bondarenko, Grigorchuk, et al. paper,"
" starting from the state b."
" This automorphism is RIBS-transformation (explained in A122200) of the automorphism *A122302."
                  )
        )
        '(inv: 154461)
        '(comps: (57163 154464 57163) (069770 122302) (122302 057163))
        '(scheme: "(define *A154462! (!RIBS *A122302!)) or equally: (define (*A154462! s) (for-each *A122302! s) s)")
  )


  (list 154463 "Signature-permutation of a Catalan bijection induced by Lamplighter group generating wreath recursion, variant 4: a = s(b,a), b = (b,a), starting from the state b."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append
"This Catalan bijection is induced by  Lamplighter group generating wreath"
" recursion (i.e. binary transducer) a = s(b,a), b = (b,a)"
" (where s means swap)"
" listed as the fourth variant on page 104 of Bondarenko, Grigorchuk, et al. paper,"
" starting from the state b."
                  )
        )
        '(inv: 154464)
        '(comps: (57163 154461 57163) (057163 122353) (122353 069770))
        '(scheme: "(define (*A154463! s) (cond ((pair? s) (*A154463! (car s)) (*A122353! (cdr s)))) s)")
  )


  (list 154464 "Signature-permutation of a Catalan bijection induced by Lamplighter group generating wreath recursion, variant 2: a = s(a,b), b = (b,a), starting from the state b."
        '(off: 0)
        '(indentries: Catsigperm)
        (list 'c: (string-append
"This Catalan bijection is induced by  Lamplighter group generating wreath"
" recursion (i.e. binary transducer) a = s(a,b), b = (b,a)"
" (where s means swap)"
" listed as the second variant on page 104 of Bondarenko, Grigorchuk, et al. paper,"
" starting from the state b."
                  )
        )
        '(inv: 154463)
        '(comps: (57163 154462 57163) (122354 057163) (069770 122354))
        '(scheme: "(define (*A154464! s) (cond ((pair? s) (*A154464! (car s)) (*A122354! (cdr s)))) s)")
  )

 )
)




;; Here are the 30 A-numbers you requested: A179751 --- A179780.
;; (Remember A179753, in intfun_b.scm !
;; Here are the 30 A-numbers you requested: A179827 --- A179856.

;; (load "~/Schemuli/out1oeis")
;; (cd "seqs/batch2010jul31")
;; (define *MAX-CACHE-SIZE-FOR-DEFINEC* 593666)
;; (output-entries-to-file120_45 seqs2010Jul31 "2010Jul31.scm" "Jul 31 2010")

(define seqs2010Jul31
 (list

  (list 179751 "Maximum depth of each term of A014486 when interpreted as a binary tree."
       '(off: 0)
       '(create-b-file: 2055)
       '(c: "Each integer n appears first at position given by A014137.")
  )

  (list 179752 "Maximum depth of each term of A014486 when interpreted as a general tree."
       '(off: 0)
       '(create-b-file: 2055)
       '(c: "Each integer n appears first at position given by A014138.")
       '(y: "Cf. A080237, A085197. Compute also two variants working with all n, whose restriction to A014486 this is.")
  )

  (list 179753 "Integers (2k)-1..0 followed by integers (2k)+1..0 and so on."
;; 1,0,3,2,1,0,5,4,3,2,1,0,7,6,5,4,3,2,1,0,...
       '(off: 1)
       '(f: "a(n) =  (2*A000194(n)) - A074294(n).")
       '(scheme: "(define (A179753 n) (- (* 2 (A000194 n)) (A074294 n)))")
  )

  (list 179754 "Iterates of A122237 starting from the initial value 6."
        '(off: 1)
        '(create-b-file: 1024)
        '(f: "a(1) = 6, a(n) = A122237(A179754(a(n-1))).")
        '(y: "a(n+1) = A127307(A179756(n)).")
        '(scheme: "(define (A179754 n) (if (= 1 n) 6 (A122237 (A179754 (-1+ n)))))")
  )

  (list 179755 "a(n) = A014486(A179754(n))."
        '(off: 1)
        '(create-b-file: 1024)
        '(y: "a(n+1) = A004758(A179757(n)). Cf. also A122242, A122245.")
  )

  (list 179756 "Iterates of A122237 starting from the initial value 8."
        '(off: 1)
        '(create-b-file: 1024)
        '(f: "a(1) = 8, a(n) = A122237(A179756(a(n-1))).")
        '(scheme: "(define (A179756 n) (if (= 1 n) 8 (A122237 (A179756 (-1+ n)))))")
  )

  (list 179757 "a(n) = A014486(A179756(n))."
        '(off: 1)
        '(create-b-file: 1024)
        '(y: "A122242, A122245, A179755. Cf. also A179827, A179828, A179829.")
  )

  (list 179840 "a(n) = A179751(A080068(n))."
        '(off: 1)
        '(create-b-file: 1024)
        '(y: "Cf. also A179844 & A179841-A179843.")
  )

  (list 179841 "a(n) = A179751(A122241(n))."
        '(off: 1)
        '(create-b-file: 1024)
        '(y: "Cf. also A179845 & A179840-A179843.")
  )

  (list 179842 "a(n) = A179751(A122244(n))."
        '(off: 1)
        '(create-b-file: 1024)
        '(y: "Cf. also A179846 & A179840-A179843.")
  )

  (list 179843 "a(n) = A179751(A179756(n))."
        '(off: 1)
        '(create-b-file: 1024)
        '(y: "Cf. also A179847 & A179840-A179842.")
  )

  (list 179844 "a(n) = A179752(A080068(n))."
        '(off: 1)
        '(create-b-file: 1024)
        '(c: "Also the local maximum of A179759 in range X.")
        '(y: "Cf. also A179840 & A179845-A179847.")
  )

  (list 179845 "a(n) = A179752(A122241(n))."
        '(off: 1)
        '(create-b-file: 1024)
        '(c: "Also the local maximum of A179764 in range X.")
        '(y: "Cf. also A179841 & A179844-A179847.")
  )

  (list 179846 "a(n) = A179752(A122244(n))."
        '(off: 1)
        '(create-b-file: 1024)
        '(c: "Also the local maximum of A179765 in range X.")
        '(y: "Cf. also A179842 & A179844-A179847.")
  )

  (list 179847 "a(n) = A179752(A179756(n))."
        '(off: 1)
        '(create-b-file: 1024)
        '(c: "Also the local maximum of A179766 in range X.")
        '(y: "Cf. also A179843 & A179844-A179846.")
  )

  (list 179758 "Binary expansions of A080069 (A080070) concatenated together to a single binary sequence, so that from each term, the most significant bits come before the least significant bits."
        '(off: 1)
        '(create-b-file: 262656) ;; (* 2 (A000217 512))
        '(y: "Cf. also A179759 & A179761-A179763.")
        '(scheme: "(define (A179758 n) (modulo (floor->exact (/ (A080069 (A000194 n)) (expt 2 (A179753 n)))) 2))")
  )

  (list 179759 "Partial sums of -(A033999(A179758(n)))."
        '(off: 1)
        '(create-b-file: 262656) ;; (* 2 (A000217 512))
        '(c: "The local maximum in range X is given by A179844.")
        '(y: "Cf. also A179764-A179766.")
  )


  (list 179761 "Binary expansions of A122242 (A122243) concatenated together to a single binary sequence, so that from each term, the most significant bits come before the least significant bits."
        '(off: 1)
        '(create-b-file: 593664) ;; (+ (* 2 (A000217 768)) (* 4 768))
        '(y: "Cf. also A179764 and A179758, A179762 & A179763. A179827-A179828.")
        '(scheme: "(define (A179761 n) (modulo (floor->exact (/ (A122242 (- (A000194 (+ n 6)) 2)) (expt 2 (A179753 (+ n 6))))) 2))")
  )

  (list 179762 "Binary expansions of A122245 (A122246) concatenated together to a single binary sequence, so that from each term, the most significant bits come before the least significant bits."
        '(off: 1)
        '(create-b-file: 593664) ;; (+ (* 2 (A000217 768)) (* 4 768))
        '(y: "Cf. also A179765 and A179758, A179761 & A179763. A179827-A179829.")
        '(scheme: "(define (A179762 n) (modulo (floor->exact (/ (A122245 (- (A000194 (+ n 6)) 2)) (expt 2 (A179753 (+ n 6))))) 2))")
  )

  (list 179763 "Binary expansions of A179757 concatenated together to a single binary sequence, so that from each term, the most significant bits come before the least significant bits."
        '(off: 1)
        '(create-b-file: 593664) ;; (+ (* 2 (A000217 768)) (* 4 768))
        '(y: "Cf. also A179765 and A179758, A179761 & A179763. A179828-A179829.")
        '(scheme: "(define (A179763 n) (modulo (floor->exact (/ (A179757 (- (A000194 (+ n 6)) 2)) (expt 2 (A179753 (+ n 6))))) 2))")
  )

  (list 179764 "Partial sums of -(A033999(A179761(n)))."
        '(off: 1)
        '(create-b-file: 593664) ;; (+ (* 2 (A000217 768)) (* 4 768))
        '(c: "The local maximum in range X is given by A179845.")
        '(y: "Cf. also A179761 & A179759, A179765-A179766.")
  )

  (list 179765 "Partial sums of -(A033999(A179762(n)))."
        '(off: 1)
        '(create-b-file: 593664) ;; (+ (* 2 (A000217 768)) (* 4 768))
        '(c: "The local maximum in range X is given by A179846.")
        '(y: "Cf. also A179762 & A179759, A179764-A179766.")
  )

  (list 179766 "Partial sums of -(A033999(A179763(n)))."
        '(off: 1)
        '(create-b-file: 593664) ;; (+ (* 2 (A000217 768)) (* 4 768))
        '(c: "The local maximum in range X is given by A179847.")
        '(y: "Cf. also A179763 & A179759, A179764-A179765.")
  )

  (list 179827 "Modulo 2 sum of A179761 and A179762."
        '(off: 1)
        '(create-b-file: 593664) ;; (+ (* 2 (A000217 768)) (* 4 768))
        '(y: "Cf. also A179828 & A179829.")
  )

  (list 179828 "Modulo 2 sum of A179761 and A179763."
        '(off: 1)
        '(create-b-file: 593664) ;; (+ (* 2 (A000217 768)) (* 4 768))
        '(y: "Cf. also A179827 & A179829.")
  )

  (list 179829 "Modulo 2 sum of A179762 and A179763."
        '(off: 1)
        '(create-b-file: 593664) ;; (+ (* 2 (A000217 768)) (* 4 768))
        '(y: "Cf. also A179827 & A179828.")
  )

;;;

  (list 179770 "The fourth central column of triangle A122242, i.e. A179761(4), A179761(11), A179761(20), A179761(31), ..."
        '(off: 1)
        '(create-b-file: 1024)
        '(f: "a(n)=A179761(A028875(n+2)).")
        '(y: "Cf. A179771-A179774, and also A179775, A179830.")
  )

  (list 179771 "Quadrisection of the fourth central column of triangle A122242, a(n) = A179770(4*n)."
        '(off: 1)
        '(create-b-file: 256)
        '(f: "a(n) = A179770(4*n).")
        '(c: "Conjecture: the last zero (107th) occurs at n=166, after which only ones occur.")
        '(y: "Cf. A179772-A179774, and also A179776, A179831.")
  )

  (list 179772 "Position of ones in A179771."
        '(off: 1)
        '(create-b-file: 150)
        '(c: "This seems to give the positions where \"L\"'s occur in the central diagonal of A122242. Conjecture, from a(60)=167 onward, all integers >= 167 present.")
        '(y: "Cf. A179773-A179774, and also A179777, A179832.")
  )

  (list 179773 "First differences of A179772."
        '(off: 1)
        '(create-b-file: 150)
        '(f: "a(n) = A179772(n+1)-A179772(n).")
        '(y: "Cf. A179774, and also A179778, A179833.")
  )

  (list 179774 "Complement of A179772."
        '(off: 1)
        '(upto: 107)
        '(create-b-file: 107)
        '(c: "Probably finite, no more terms after a(107)=... exist.")
        '(y: "Cf. A179772, and also A179779, A179834.")
  )

;;;;;

  (list 179775 "The fourth central column of triangle A122245, i.e. A179762(4), A179762(11), A179762(20), A179762(31), ..."
        '(off: 1)
        '(create-b-file: 1024)
        '(f: "a(n)=A179762(A028875(n+2)).")
        '(y: "Cf. A179776-A179779, and also A179770, A179830.")
  )

  (list 179776 "Quadrisection of the fourth central column of triangle A122245, a(n) = A179775((4*n)-2)."
        '(off: 1)
        '(create-b-file: 256)
        '(f: "a(n) = A179775((4*n)-2).")
        '(c: "Conjecture: the last zero (108th) occurs at n=163, after which only ones occur.")
        '(y: "Cf. A179777-A179779, and also A179771, A179831.")
  )

  (list 179777 "Position of ones in A179776."
        '(off: 1)
        '(create-b-file: 150)
        '(c: "This seems to give the positions where \"L\"'s occur in the central diagonal of A122245. Conjecture, from a(56)=164 onward, all integers >= 164 present.")
        '(y: "Cf. A179778-A179779, and also A179772, A179832.")
  )

  (list 179778 "First differences of A179777."
        '(off: 1)
        '(create-b-file: 150)
        '(f: "a(n) = A179777(n+1)-A179777(n).")
        '(y: "Cf. A179779, and also A179773, A179833.")
  )

  (list 179779 "Complement of A179777."
        '(off: 1)
        '(upto: 108)
        '(create-b-file: 108)
        '(c: "Probably finite, no more terms after a(108)=... exist.")
        '(y: "Cf. A179777, and also A179774, A179834.")
  )

;;;;;

  (list 179830 "The first central column of triangle A179757, i.e. A179763(1), A179763(8), A179763(17), A179763(28), ..."
        '(off: 1)
        '(create-b-file: 1024)
        '(f: "a(n)=A179763(A028884(n+2)).")
        '(y: "Cf. A179831-A179834, and also A179770, A179775.")
  )

  (list 179831 "Quadrisection of the fourth central column of triangle A122242, a(n) = A179830((4*n)+1)."
        '(off: 1)
        '(create-b-file: 256)
        '(f: "a(n) = A179830((4*n)+1).")
        '(c: "Conjecture: the last zero (119th) occurs at n=164, after which only ones occur.")
        '(y: "Cf. A179832-A179834, and also A179771, A179776.")
  )

  (list 179832 "Position of ones in A179831."
        '(off: 1)
        '(create-b-file: 150)
        '(c: "This seems to give the positions where \"L\"'s occur in the central diagonal of A122242. Conjecture, from a(xx)=165 onward, all integers >= 165 present.")
        '(y: "Cf. A179833-A179834, and also A179772, A179777.")
  )

  (list 179833 "First differences of A179832."
        '(off: 1)
        '(create-b-file: 150)
        '(f: "a(n) = A179832(n+1)-A179832(n).")
        '(y: "Cf. A179834, and also A179773, A179778.")
  )

  (list 179834 "Complement of A179832."
        '(off: 1)
        '(upto: 119)
        '(create-b-file: 119)
        '(c: "Probably finite, no more terms after a(119)=... exist.")
        '(y: "Cf. A179832, and also A179774, A179779.")
  )
 )
)


;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(load "./gatosiga")
(load "./gato-out")
(load "./gatoaltr")

