
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;;  Schemuli/lstfuns1.scm                                               ;;
;;  - Often needed list functions and such.                             ;;
;;                                                                      ;;
;;  (Schemuli stands for the "Useful Library for Scheme" and "matikka"  ;;
;;   is a certain kind of fish, believe it or not).                     ;;
;;                                                                      ;;
;;  Coded by Antti Karttunen (my_firstname.my_surname@gmail.com), 2002- ;;
;;  Last edited Sep 26 2015.                                            ;;
;;                                                                      ;;
;;  This Scheme-code is in Public Domain and runs (at least)            ;;
;;  in MIT Scheme Release 7.6.0, for which one can find documentation   ;;
;;  and the pre-compiled binaries (for various OS's running in          ;;
;;  Intel x86 architecture) under the URL:                              ;;
;;  http://www.swiss.ai.mit.edu/projects/scheme/                        ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; From http://www.swiss.ai.mit.edu/projects/scheme/documentation/user_5.html
;;
;; If you redefine some global name in your code, for example, car, cdr
;; and cons, you should indicate it in the declaration as:
;; (declare (usual-integrations car cdr cons))

;; (Beware of using an argument name like list in the function definitions?)

(declare (usual-integrations))


;;
;; Compile as:
;;
;; (cf "c:\\matikka\\Schemuli\\lstfuns1" "c:\\matikka\\Schemuli\\")
;; 

(define (car* p) (if (pair? p) (car p) p))
(define (cdr* p) (if (pair? p) (cdr p) p))

(define vector-set-and-return-value!
        (lambda (vec ind val) (vector-set! vec ind val) val)
)


(define (add-to-vec! vec n what)
   (let ((newvec (if (>= n (vector-length vec)) (vector-grow vec (1+ n)) vec)))
      (vector-set! newvec n what)
      newvec
   )
)

(define (incr-num-vector! vec n)
   (let ((old (vector-ref vec n)))
      (cond ((not (number? old)) (vector-set! vec n 1))
            (else (vector-set! vec n (1+ old)))
      )
   )
)


(define (collect-non-zero-values-from-num-vector vec) ;; to an association list
   (let loop ((i (-1+ (vector-length vec))) (a (list)))
       (cond ((negative? i) a)
             (else
               (let ((n (vector-ref vec i)))
                  (cond ((or (not (number? n)) (zero? n)) (loop (-1+ i) a))
                        (else (loop (-1+ i) (cons (cons i n) a)))
                  )
               )
             )
       )
   )
)


;; Bit like vector-binary-search of MIT/GNU Scheme, but returns the index instead of element.
;; (This algorithm copied directly from Wikipedia...)
;; At least this works:
;; (same-intfuns? A001477 (lambda (n) (pos_of_k_in_vector (vector-ref vecA008578 n) vecA008578)) 100000)

(define (pos_of_k_in_vector k vec)
  (let ((minind (let miniloop ((i 0)) (if (vector-ref vec i) i (miniloop (+ i 1))))) ;; Find the pos of first non-#f
        (maxind (-1+ (vector-length vec)))
       )
    (if (< (vector-ref vec maxind) k)
        (error "The max. element in the given vector is less than what was searched for: "
               (vector-ref vec maxind) k
        )
        (let loop ((imin minind) (imax maxind))
             (cond ((> imin imax) #f) ;; Didn't found it.
                   (else
                      (let* ((imid (+ imin (floor->exact (/ (- imax imin) 2))))
                             (item (vector-ref vec imid)) ;; What's there?
                            )
                        (cond ((= item k) imid) ;; Found it?
                              ((< item k) (loop (+ imid 1) imax))
                              ((> item k) (loop imin (- imid 1)))
                        )
                      )
                   )
             )
        )
    )
  )
)


;; Unnecessary, use vector-fill!
;; (define (fill-vec-with! vec what)
;;   (let loop ((n (vector-length vec)))
;;      (cond ((zero? n) vec)
;;            (else (vector-set! vec (-1+ n) what)
;;                  (loop (-1+ n))
;;            )
;;      )
;;   )
;; )


(define (vector-grow-and-fill-the-rest-with-zeros! oldvec newsize)
  (let ((oldsize (vector-length oldvec))
        (newvec (vector-grow oldvec newsize))
       )
    (let loop ((i oldsize))
         (cond
            ((= i newsize) newvec)
            (else ;; I.e. (< i newsize)
               (vector-set! newvec i 0)
               (loop (+ i 1))
            )
         )
    )
  )
)



(define (bisect lista parity) ;; Parity is 0 or 1.
   (let loop ((lista lista) (i 0) (z (list)))
        (cond ((null? lista) (reverse! z))
              ((eq? i parity)
                    (loop (cdr lista) (modulo (1+ i) 2) (cons (car lista) z))
              )
              (else (loop (cdr lista) (modulo (1+ i) 2) z))
        )
   )
)

(define (rotleft a) (if (null? a) a (append (cdr a) (list (car a)))))
(define (rotright a) (reverse! (rotleft (reverse a))))


(define (distinct-elems lista)
   (if (not (pair? lista))
       0
       (let loop ((n 0)
                  (lista lista)
                  (prev (not (car lista)))
                 )
           (cond ((not (pair? lista)) n)
                 ((equal? (car lista) prev) (loop n (cdr lista) prev))
                 (else                      (loop (1+ n) (cdr lista) (car lista)))
           )
       )
   )
)


(define (multiplicities lista) ;; Of numeric elements.
       (let loop ((mults (list))
                  (lista lista)
                  (prev #f)
                 )
           (cond ((not (pair? lista)) (reverse! mults))
                 ((equal? (car lista) prev)
                     (set-car! mults (+ 1 (car mults)))
                     (loop mults (cdr lista) prev)
                 )
                 (else
                   (loop (cons 1 mults) (cdr lista) (car lista))
                 )
           )
       )
)


(define (uniq lista) ;; Assumed to be sorted already with (sort lista <)
   (let loop ((lista lista) (z (list)))
        (cond ((null? lista) (reverse! z))
              ((and (pair? z) (equal? (car z) (car lista)))
                    (loop (cdr lista) z)
              )
              (else (loop (cdr lista) (cons (car lista) z)))
        )
   )
)

(define (multiset->countpairs mset)
  (let ((sorted (sort mset <)))
    (map cons (multiplicities sorted) (uniq sorted))
  )
)


(define (elemcountpairs lista) ;; Of numeric elements, already sorted.
       (let loop ((pairs (list))
                  (lista lista)
                  (prev #f)
                 )
           (cond ((not (pair? lista)) (reverse! pairs))
                 ((equal? (car lista) prev)
                     (set-cdr! (car pairs) (+ 1 (cdar pairs)))
                     (loop pairs (cdr lista) prev)
                 )
                 (else ;; A new item?
                   (loop (cons (cons (car lista) 1) pairs) (cdr lista) (car lista))
                 )
           )
       )
)


(define reversed_iota
  (lambda (n)
      (if (zero? n) (list)
          (cons n (reversed_iota (- n 1)))
      )
  )
)

(define iota (lambda (n) (reverse! (reversed_iota n))))

(define (iota0 upto_n)
   (let loop ((n upto_n) (result (list)))
      (cond ((zero? n) (cons 0 result))
            (else (loop (- n 1) (cons n result)))
      )
   )
)

(define (pos-of-first-matching lista pred?)
   (let loop ((lista lista) (i 0))
     (cond ((null? lista) #f)
           ((pred? (car lista)) i)
           (else (loop (cdr lista) (1+ i)))
     )
   )
)

(define (nthmemq elem lista)
   (let loop ((lista lista) (i 0))
     (cond ((null? lista) #f)
           ((eq? (car lista) elem) i)
           (else (loop (cdr lista) (1+ i)))
     )
   )
)


(define (nthcdrmemq sublist lista)
   (let loop ((lista lista) (i 0))
     (cond ((null? lista) #f)
           ((eq? lista sublist) i)
           (else (loop (cdr lista) (1+ i)))
     )
   )
)


(define (positions n a)
  (let loop ((b (list)) (a a) (i 0))
    (cond ((null? a) (reverse! b))
          ((= (car a) n) (loop (cons i b) (cdr a) (1+ i)))
          (else (loop b (cdr a) (1+ i)))
    )
  )
)

(define (DIFF a)
  (map - (cdr a) (reverse! (cdr (reverse a))))
)

;; Like above, but keeps the first element of the original list, ints:
(define (diff1 ints) (reverse (fold-left (lambda (xs x) (cons (- x (apply + xs)) xs)) '() ints)))

(define (PARTSUMS a)
  (cdr (reverse! (fold-left (lambda (psums n) (cons (+ n (car psums)) psums)) (list 0) a)))
)

(define (revdeltas ints) (partsums (reverse (diff1 ints))))

(define attach! ; Borrowed from Franz lisp, is like destructive cons.
  (lambda (elem lista)
     (set-cdr! lista (cons (car lista) (cdr lista)))
     (set-car! lista elem)
     lista
  )
)


(define (pop! lista)
  (let ((topmost (car lista)))
    (cond ((pair? (cdr lista))
                (set-car! lista (cadr lista))
                (set-cdr! lista (cddr lista))
          )
    )
    topmost
  )
)

;; Convert (a . (b . rest)) --> ((a . b) . rest)
;; with no cons cells wasted.

(define (cons2top! stack)
  (let ((ex-cdr (cdr stack)))
      (set-cdr! stack (car ex-cdr))
      (set-car! ex-cdr stack)
      ex-cdr
  )
)


(define (flip!topmost stack)
  (let* ((topmost (car stack))
         (ex-cdr (cdr topmost))
        )
      (set-cdr! topmost (car topmost))
      (set-car! topmost ex-cdr)
      stack
  )
)

;; (list-n-from-top 0 stack) pushes () to top of stack.
;; (list-n-from-top 1 stack) replaces the top element with its list:ed version,
;; etc.
(define (list-n-from-top n stack)
  (cons (list-head stack n) (nthcdr n stack))
)

;; Returns the number of deleted items. Fails if all items of lista would be deleted.
(define (delete-matching! lista pred?)
   (let loop ((prevptr #f) (ptr lista) (n 0))
      (cond ((null? ptr) n)
            ((pred? (car ptr))
               (if prevptr
                   (begin
                     (set-cdr! prevptr (cdr ptr))
                     (loop prevptr (cdr ptr) (+ 1 n))
                   )
                   (begin
                     (set-car! ptr (cadr ptr))
                     (set-cdr! ptr (cddr ptr))
                     (loop prevptr ptr (+ 1 n))
                   )
               )
            )
            (else (loop ptr (cdr ptr) n))
       )
   )
)

(define (add-matching-numbers! lista pred? lowlim uplim)
   (let loop ((k lowlim) (added 0))
         (cond ((> k uplim) added)
               ((pred? k)
                  (begin
                     (attach! k lista)
                     (loop (+ 1 k) (+ 1 added))
                  )
               )
               (else (loop (+ 1 k) added))
         )
   )
)


(define (count-pars a)
    (cond ((not (pair? a)) 0)
          (else (+ 1 (count-pars (car a)) (count-pars (cdr a))))
    )
)

(define (max* a b)
  (cond ((and (number? a) (number? b)) (max a b))
        ((number? a) a)
        ((number? b) b)
        (else #f)
  )
)

(define (max-in-tree bt)
     (cond ((not (pair? bt)) bt)
           (else (max* (max-in-tree (car bt))
                       (max-in-tree (cdr bt))
                 )
           )
     )
)


(define (copy-tree bt)
  (cond ((not (pair? bt)) bt)
        (else (cons (copy-tree (car bt))
                    (copy-tree (cdr bt)))
        )
  )
)


(define (nthcdr n lista)
  (if (or (zero? n) (null? lista))
      lista
      (nthcdr (- n 1) (cdr lista))
  )
)


;; For testing whether we have an identity permutation or not.
(define (first-dislocated lista)
   (let loop ((lista lista) (i 0))
     (cond ((null? lista) lista)
           ((not (eq? (car lista) i)) lista)
           (else (loop (cdr lista) (1+ i)))
     )
   )
)

(define (non-false-positions lista)
  (let loop ((lista lista) (a (list)) (i 0))
     (cond ((null? lista) (reverse! a))
           ((car lista) (loop (cdr lista) (cons i a) (1+ i)))
           (else (loop (cdr lista) a (1+ i)))
     )
  )
)

;; See http://pobox.com/~oleg/ftp/papers/XML-parsing-talk.ps.gz, page 45/49.
;;
;;   Note that
;;
;;   foldr (x seed -> foldl (flip (:)) seed x) []
;;
;;   is an efficient implementation of
;;
;;   concat . map reverse
;;
;; i.e. (apply append! (map reverse s))

(define (reverse-sublists-and-concat s)
  (fold-right (lambda (a b)
                 (fold-left (lambda (x y) (cons y x)) b a)
              )
              '()
              s
  )
)

;; Why not use reduce or fold-left or fold-right here?
;; Because I didn't know about them at the time I wrote these.
;; And then I would need to generate the list of indices
;; first, which is wasteful. But for Sum over divisors
;; it is nice.

;; Implement sum_{i=lowlim..uplim} intfun(i)
(define (add intfun lowlim uplim)
   (let sumloop ((i lowlim) (res 0))
      (cond ((> i uplim) res)
            (else (sumloop (1+ i) (+ res (intfun i))))
      )
   )
)

(define (mul intfun lowlim uplim)
   (let multloop ((i lowlim) (res 1))
      (cond ((> i uplim) res)
            (else (multloop (1+ i) (* res (intfun i))))
      )
   )
)

(define (max_pt_in_range intfun lowlim uplim)
   (let loop ((i (+ 1 lowlim)) (maxnow (intfun lowlim)) (maxpt lowlim))
      (cond ((> i uplim) maxpt)
            (else (let ((v (intfun i)))
                     (if (> v maxnow)
                         (loop (+ 1 i) v i)
                         (loop (+ 1 i) maxnow maxpt)
                     )
                  )
            )
      )
   )
)


(define (collect-intfun-values-to-list intfun lowlim uplim)
  (let loop ((z (list)) (i uplim))
         (cond ((< i lowlim) z)
               (else (loop (cons (intfun i) z) (-1+ i)))
         )
  )
)

;; Stupid:
;; (define (same-intfuns? fun1 fun2 lowlim uplim) ;; Check superficially.
;;   (zero? (add (lambda (n) (abs (- (fun1 n) (fun2 n)))) lowlim uplim))
;; )

(define (same-intfuns0? fun1 fun2 uplim)
   (let checkloop ((i 0))
      (cond ((> i uplim) #t) ;; superficially, up to n uplim, yes.
            ((not (equal? (fun1 i) (fun2 i))) i) ;; return first i where they differ.
            ((= 0 (modulo i 16384)) (write i) (newline) (flush-output) (checkloop (1+ i)))
            (else (checkloop (1+ i)))
      )
   )
)

(define (same-intfuns1? fun1 fun2 uplim)
   (let checkloop ((i 1))
      (cond ((> i uplim) #t) ;; superficially, up to n uplim, yes.
            ((not (equal? (fun1 i) (fun2 i))) i) ;; return first i where they differ.
            ((= 0 (modulo i 16384)) (write i) (newline) (flush-output) (checkloop (1+ i)))
            (else (checkloop (1+ i)))
      )
   )
)

(define same-intfuns? same-intfuns1?)

(define (steps-to-convergence-nonincreasing fun1 fun2 initval1 initval2)
   (let loop ((steps 0) (a1 initval1) (a2 initval2))
      (cond ((equal? a1 a2) steps)
            ((> a1 a2) (loop (+ steps 1) (fun1 a1) a2))
            (else (loop steps a1 (fun2 a2)))
      )
   )
)

;; How many steps are needed to iterate fun1 starting from initval1,
;; before we get the same results as with fun2 starting from initval2?
;; Both functions are expected to be non-decreasing.
(define (steps-to-convergence-nondecreasing fun1 fun2 initval1 initval2)
   (let loop ((steps 0) (a1 initval1) (a2 initval2))
      (cond ((equal? a1 a2) steps)
            ((< a1 a2) (loop (+ steps 1) (fun1 a1) a2))
            (else (loop steps a1 (fun2 a2)))
      )
   )
)

(define (equal-steps-to-convergence-nondecreasing? fun1 fun2 initval1 initval2)
   (let loop ((steps 0) (a1 initval1) (a2 initval2))
      (cond ((equal? a1 a2) (zero? steps))
            ((< a1 a2) (loop (+ steps 1) (fun1 a1) a2))
            (else (loop (- steps 1) a1 (fun2 a2)))
      )
   )
)

(define (print-fixed-points outfile fun1 uplim)
  (call-with-output-file outfile
    (lambda (out)
      (let loop ((i 1))
        (cond ((> i uplim) #t) ;; superficially, up to n uplim, yes.
              ((= i (fun1 i))
                 (write 'fixed:) (write i) (newline) (flush-output)
                 (write i out) (newline out) (flush-output out) (loop (1+ i))
              )
              ((= 0 (modulo i 262144))
                 (write 'running:) (write i) (newline) (flush-output) (loop (1+ i)))
              (else (loop (1+ i)))
        )
      )
    )
  )
)

(define (count-the-occurrences intfun what lowlim uplim)
   (let cntloop ((i lowlim) (res 0))
      (cond ((> i uplim) res)
            (else (cntloop (1+ i) (+ res (if (= what (intfun i)) 1 0))))
      )
   )
)

(define (compose-funlist funlist)
 (cond ((null? funlist) (lambda (x) x))
       (else (lambda (x) ((car funlist) ((compose-funlist (cdr funlist)) x))))
 )
)

(define (compose-funs . funlist)
 (cond ((null? funlist) (lambda (x) x))
       (else (lambda (x) ((car funlist) ((apply compose-funs (cdr funlist)) x))))
 )
)


(define (compose-fun-to-nth-power fun n)
 (cond ((zero? n) (lambda (x) x))
       (else (lambda (x) (fun ((compose-fun-to-nth-power fun (- n 1)) x))))
 )
)

;; Semantically
;; ((compose-fun-to-nth-power fun n) x) =  (apply-n-times fun n x)
;; although this should be much more practical:
(define (apply-n-times fun n x)
    (cond ((zero? n) x)
          (else (apply-n-times fun (-1+ n) (fun x)))
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (filter-matching-terms-from-term-file matches? infile outfun)
   (call-with-input-file infile
     (lambda (inport)
            (let loop ((count 0) (term (read inport)))
               (cond ((eof-object? term) count)
                     ((matches? term)
                        (begin (outfun term) (loop (+ 1 count) (read inport)))
                     )
                     (else (loop count (read inport)))
               )
            )
     )
   )
)

(define (output-matching-terms-from-file matches? infile)
   (filter-matching-terms-from-term-file matches? infile (lambda (x) (display x) (newline)))
)



