
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;;                          permfuns.scm                              ;;
;;                                                                    ;;
;; Some permutation functions.                                        ;;
;; Written by Antti Karttunen (firstname.surname@iki.fi) March, 2003  ;;
;; Last modified 26. Aug 2015.                                        ;;
;;                                                                    ;;
;; This file is located under:                                        ;;
;; http://www.iki.fi/~kartturi/matikka/Schemuli/permfuns.scm          ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; From http://www.swiss.ai.mit.edu/projects/scheme/documentation/user_5.html
;;
;; If you redefine some global name in your code, for example, car, cdr
;; and cons, you should indicate it in the declaration as:
;; (declare (usual-integrations car cdr cons))

;; (Beware of using an argument name like list in the function definitions?)

(declare (usual-integrations))  ;; For compilation in MIT Scheme.

(define (permlist? p) (or (null? p) (integer? (car p))))
(define (permcycles? p) (or (null? p) (pair? (car p))))

(define (permdeg p)
   (cond ((permlist? p) (length p))
         (else ;; Find max. element of cycles.
           (let ((n (max-in-tree p)))
              (cond ((number? n) n)
                    (else 0)
              )
           )
         )
   )
)


;; Apply the permutation p to i, i.e. compute p(i). i in [1..permdeg(p)]
(define (permcall p i)
   (cond ((permlist? p) (list-ref p (-1+ i)))
         ((find-matching-item p (lambda (c) (memq i c)))
            =>
               (lambda (c) ;; The cycle containing i.
                  (let ((d (memq i c)))
                     (cond ((cdr d) => car)
                           (else (car c))
                     )
                  )
               )
         )
   )
)


;; (permlist->cycles '(1))        -->  ((1))
;; (permlist->cycles '(1 2))      -->  ((1) (2))
;; (permlist->cycles '(1 2 3))    -->  ((1) (2) (3))
;; (permlist->cycles '(2 1))      -->  ((1 2))
;; (permlist->cycles '(1 3 2))    -->  ((1) (2 3))
;; (permlist->cycles '(3 1 2))    -->  ((1 3 2))
;; (permlist->cycles '(2 3 1))    -->  ((1 2 3))
;; (permlist->cycles '(3 2 1))    -->  ((1 3) (2))
;; (permlist->cycles '(1 2 4 3))  -->  ((1) (2) (3 4))
;; (permlist->cycles '(2 1 4 3))  -->  ((1 2) (3 4))


(define (permlist->cycles pl)
  (if (null? pl)
      pl
      (let ((cs (list (list 1)))
            (p (cons #f (list-copy (cdr pl))))
           )
       (let loop ((i (car pl)))
             (cond ((list-ref p (-1+ i)) ;; I.e. (not (memq i (car cs))) holds.
                      => (lambda (next-i)
                            (set-car! (list-tail p (-1+ i)) #f)
                            (set-car! cs (cons i (car cs)))
                            (loop next-i)
                         )
                   )
                   ((pos-of-first-matching p (lambda (x) x)) ;, Returns zero-based i.
                      => (lambda (i)
                            (set-car! cs (reverse! (car cs)))
                            (attach! (list) cs)
                            (loop (1+ i))
                         )
                   )
                   (else
                         (set-car! cs (reverse! (car cs)))
                         (reverse! cs)
                   )
             )
       )
      )
  )
)


(define (perminv p)
    (cond ((permlist? p)
             (map (lambda (i) (1+ (nthmemq i p))) (iota (length p)))
          )
          (else ;; Cycles, just reverse them.
             (map reverse p)
          )
    )
)

(define (vector-grow-b-to-size-of-a a b)
    (let ((new-b (vector-grow b (vector-length a))))
      (let loop ((i (vector-length b)))
         (cond ((= i (vector-length a)) new-b)
               (else (vector-set! new-b i (+ 1 i))
                     (loop (+ 1 i))
               )
         )
      )
    )
)


;; Maple documentation for group[permgroup] says:
;
;; This package follows the convention that ``permutations act on the right''.
;; In other words, if p1 and p2 are permutations, then the product of p1 and p2 (p1 &* p2)
;; is defined such that (p1 &* p2)(i) = p2(p1(i)) for i=1..deg. 

;;
;; And that is just the opposite of what I'm used to, after
;; reading Rotman (Introduction to the Theory of Groups),
;; Metsäkylä-Näätänen (Algebra) and A.C. White (Ringing the Changes,
;; Ringing the Cosets)
;;
;; So with this we have: permul(a,b) = a*b(i) = a(b(i)).
;;
;; When we multiply from left, the left-hand-side operand a
;; permutes the ELEMENTS in permutation b (the right-hand-side operand),
;; and when we are multiplying from right, the right-hand-side
;; operand b permutes the POSITIONS in permutation a.
;;
;;  convert(permul([[5,4]],convert([5,4,3,2,1],'disjcyc')),'permlist',5);
;;    -> [4, 5, 3, 2, 1]
;;
;;  convert(permul(convert([5,4,3,2,1],'disjcyc'),[[5,4]]),'permlist',5);
;;    -> [5, 4, 3, 1, 2]
;;
;; (permul '(1 2 3 5 4) '(5 4 3 2 1))   --> (4 5 3 2 1)
;; (permul '(5 4 3 2 1) '(1 2 3 5 4))   --> (5 4 3 1 2)
;;
;;

(define (permul p q)
    (cond ((and (permlist? p) (permlist? q))
             (map (lambda (i) (list-ref p (-1+ i))) q)
          )
          (else ;; Cycles, do it like this. Would work for above case also.
             (permlist->cycles
                (map (lambda (i) (permcall p (permcall q i)))
                     (iota (permdeg q))
                )
             )
          )
    )
)

(define (permulvecs a b)
   (let ((aa (if (>= (vector-length a) (vector-length b)) a (vector-grow-b-to-size-of-a b a)))
         (bb (if (<= (vector-length a) (vector-length b)) b (vector-grow-b-to-size-of-a a b)))
        )
      (list->vector (permul (vector->list aa) (vector->list bb)))
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maple code from old findnext.txt, for computing A055089, etc:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; # My modification:
;; # We could use the ending condition if(0 = n), but if(0 = r) is faster, in case
;; # r's factorial expansion ends in one more zeros:
;; 
;; PermUnrank3Raux := proc(n,r,p) local s;
;;   if(0 = r) then RETURN(p);
;;   else
;;     s := floor(r/((n-1)!));
;;     RETURN(PermUnrank3Raux(n-1, r-(s*((n-1)!)), permul(p,[[n,n-s]])));
;;   fi;
;; end;
;; 
;; PermUnrank3R := proc(r) local n; n := nops(factorial_base(r)); convert(PermUnrank3Raux(n+1,r,[]),'permlist',1+(((r+2) mod (r+1))*n)); end;
;; 
;; # PermRevLexUnrank (A055089) can be computed also with PermRevLexUnrankA and PermRevLexUnrankAMSD
;; PermRevLexUnrankA := proc(rr) local r,i,p,k;
;;   r := rr;
;;   p := [];
;;   i := 2;
;;   while(r <> 0)
;;    do
;;     for k from i-1 by -1 to i-(r mod i) do p:= permul([[k,k+1]],p); od;
;;     r := floor(r/i);
;;     i := i+1;
;;    od;
;;   RETURN(convert(p,'permlist',1+(((rr+2) mod (rr+1))*nops(factorial_base(rr)))));
;; end;
;; 
;; # Compare this with PermUnrank3Raux, and you see why the identity
;; # given at A060112 holds:
;; 
;; PermRevLexUnrankAMSDaux := proc(n,r,pp) local s,p,k;
;;   p := pp;
;;   if(0 = r) then RETURN(p);
;;   else
;;     s := floor(r/((n-1)!));
;;     for k from n-s to n-1 do p:= permul(p,[[k,k+1]]); od;
;;     RETURN(PermRevLexUnrankAMSDaux(n-1, r-(s*((n-1)!)), p));
;;   fi;
;; end;
;; 
;; PermRevLexUnrankAMSD := proc(r) local n; n := nops(factorial_base(r)); convert(PermRevLexUnrankAMSDaux(n+1,r,[]),'permlist',1+(((r+2) mod (r+1))*n)); end;
;;

;; PermRevLexUnrankA := proc(rr) local r,i,p,k;
;;   r := rr;
;;   p := [];
;;   i := 2;
;;   while(r <> 0)
;;    do
;;     for k from i-1 by -1 to i-(r mod i) do p:= permul([[k,k+1]],p); od;
;;     r := floor(r/i);
;;     i := i+1;
;;    od;
;;   RETURN(convert(p,'permlist',1+(((rr+2) mod (rr+1))*nops(factorial_base(rr)))));
;; end;


;; A220655-A220664 are now reserved for your use.

;; Each n occurs A084558(n)+1 times
;; Equally, each n in range [n!,(n+1)!-1] occurs n times.
;; 0,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,6,7,7,7,7,9,9,9,9

(define (LexRankPerm rank) ;; Was incorrectly named A055089perm, but it doesn't generate them in same order!
  (let ((permvec (make-initialized-vector (+ 1 (A084558 rank)) 1+)))
   (let outloop ((rank rank)
                 (i 2)
                )
        (cond ((zero? rank) permvec)
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

(define (A055089aux1 rank)
   (let ((size (A084558 rank)))
      (A055089aux2 (+ 1 size) rank (make-initialized-vector size 1+))
   )
)

(define (A055089aux2 size rank permvec)
   (cond ((zero? rank) permvec)
         (else ;; Loop with k from n-floor(r/((n-1)!)) to n-1 swapping each (k-1,k) pair (zero-based) of permvec:
           (let loop ((k (- size (floor->exact (/ rank (A000142 (- size 1)))))))
                (cond ((= k size) (A055089aux2 (- size 1) (remainder rank (A000142 (- size 1))) permvec))
                      (else
                         (begin
                            (let ((tmp (vector-ref permvec (- k 1))))
                              (vector-set! permvec (- k 1) (vector-ref permvec k))
                              (vector-set! permvec k tmp)
                            )
                            (loop (+ 1 k))
                         )
                      )
                )
           )
         )
   )
)

;; PermRevLexUnrankAMSDaux := proc(n,r,pp) local s,p,k;
;;   p := pp;
;;   if(0 = r) then RETURN(p);
;;   else
;;     s := floor(r/((n-1)!));
;;     for k from n-s to n-1 do p:= permul(p,[[k,k+1]]); od;
;;     RETURN(PermRevLexUnrankAMSDaux(n-1, r-(s*((n-1)!)), p));
;;   fi;
;; end;
 
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (permcycles-skew p)
   (let ((deg (permdeg p)))
     (map (lambda (c) (map (lambda (i) (- deg (modulo i deg))) c))
          p
     )
   )
)

;; Note that in case dir = +1 the clause (1+ (modulo (+ (-1+ i) dir) deg))
;; is equal to (1+ (modulo i deg)).

(define (permcycles-rot p dir)
   (let ((deg (permdeg p)))
     (map (lambda (c) (map (lambda (i) (1+ (modulo (+ (-1+ i) dir) deg))) c))
          p
     )
   )
)

;; In a manner similar to A061417 (where dir is e.g. +1 or -1).
;; convert(SiteSwap2Perm1(rotateL(Perm2SiteSwap2([2,3,1,4]))),'disjcyc') --> [[1, 2, 4]]
;; (permrot '(2 3 1 4) -1)  --> ((4 1 2) (3))
;; convert(SiteSwap2Perm1(rotateL(Perm2SiteSwap2([7,6,2,3,1,4,5]))),'disjcyc');
;; --> [[1, 5, 3, 2], [4, 7, 6]]
;; (permrot '(7 6 2 3 1 4 5) -1) --> ((7 6 4) (1 5 3 2))
;;


(define (permrot p dir)
    (cond ((permcycles? p) (permcycles-rot p dir))
          (else (permcycles-rot (permlist->cycles p) dir))
    )
)


;; Note that:

;; (permul (perminv (list (iota 6))) (permul (sexp->kk-perm '(((())))) (list (iota 6))))
;; --> (4 3 2 1 6 5) =  ((1 4) (2 3) (5 6)) as cycles.


;; From the interpretation n (non-crossing handshakes, i.e. nonintersecting
;; chords joining 2n points on the circumference of a circle) to the
;; interpretation kk (fixed-point free and non-crossing involutions of [2n]),
;; i.e. straight to the cycle notation:

;; (sexp->kk-perm '())             -> ()
;; (sexp->kk-perm '(()))           -> ((1 2))
;; (sexp->kk-perm '(() ()))        -> ((1 2) (3 4))
;; (sexp->kk-perm '((())))         -> ((1 4) (2 3))
;; (sexp->kk-perm '(() () ()))     -> ((1 2) (3 4) (5 6))
;; (sexp->kk-perm '(() (())))      -> ((1 2) (3 6) (4 5))
;; (sexp->kk-perm '((()) ()))      -> ((1 4) (2 3) (5 6))
;; (sexp->kk-perm '((() ())))      -> ((1 6) (2 3) (4 5))
;; (sexp->kk-perm '(((()))))       -> ((1 6) (2 5) (3 4))

;; Could be cleaner, probably:

(define (sexp->kk-perm s)
  (let ((c (list (cons 0 0)))
        (maxnode (list 0))
       )
     (let recurse ((s s))
        (cond ((pair? s)
                 (let ((this-trans (list (1+ (car maxnode)) 0)))
                    (set-car! maxnode (1+ (car maxnode)))
                    (attach! this-trans c)
                    (recurse (car s))
                    (set-car! maxnode (1+ (car maxnode)))
                    (set-car! (cdr this-trans) (car maxnode))
                    (recurse (cdr s))
                 )
              )
        )
     ) ; let recurse
     (cdr (reverse! c))
  ) ; let
)

;; (kk-perm->binexp '((1 2)))             -> 2
;; (kk-perm->binexp '((1 2) (3 4)))       -> 10
;; (kk-perm->binexp '((1 4) (2 3)))       -> 12
;; (kk-perm->binexp '((1 2) (3 4) (5 6))) -> 42
;; (kk-perm->binexp '((1 2) (3 6) (4 5))) -> 44
;, (kk-perm->binexp '((1 4) (2 3) (5 6))) -> 50
;; (kk-perm->binexp '((1 6) (2 3) (4 5))) -> 52
;; (kk-perm->binexp '((1 6) (2 5) (3 4))) -> 56


;; (define (A001477v2 n)
;;  (A080300 (kk-perm->binexp (sexp->kk-perm (binexp->parenthesization (A014486 n)))))
;; )


;; (define (A001477v3 n)
;;   (let* ((p (sexp->kk-perm (binexp->parenthesization (A014486 n))))
;;          (d (permdeg p))
;;         )
;;     (A080300 (kk-perm->binexp (permul (permul p (list (iota d)))
;;                                       (perminv (list (iota d)))
;;                               )
;;              )
;;     )
;;   )
;; )


;; (define (A057164v2 n)
;;   (let* ((p (sexp->kk-perm (binexp->parenthesization (A014486 n))))
;;          (d (permdeg p))
;;         )
;;     (A080300 (kk-perm->binexp (permul
;;                                 (perminv (permcycles-skew (permul p (list (iota d)))))
;;                                 (perminv (list (iota d)))
;;                               )
;;              )
;;     )
;;   )
;; )


;; (define (A000001 n) ;; Wouter's sk(ew), at last I managed to reconstruct it!
;;   (let* ((p (sexp->kk-perm (binexp->parenthesization (A014486 n))))
;;          (d (permdeg p))
;;         )
;;     (A080300 (kk-perm->binexp (permul (perminv (list (iota d)))
;;                                 (perminv (permcycles-skew (permul (list (iota d)) p)))
;;                               )
;;              )
;;     )
;;   )
;; )


(define (kk-perm->binexp p)
  (let ((s 0)
        (d (permdeg p))
       )
     (for-each (lambda (tp)
                  (set! s (+ s (expt 2 (- d (apply min tp)))))
               )
               p
     )
     s
  )
)



;; Seems to be:

;; (define (A057501v2 n)
;;   (let* ((p (sexp->kk-perm (binexp->parenthesization (A014486 n))))
;;          (d (permdeg p))
;;         )
;;     (A080300 (kk-perm->binexp (permul (permul (perminv (list (iota d))) p)
;;                                       (list (iota d))
;;                               )
;;              )
;;     )
;;   )
;; )


;; (define (A057502v2 n)
;;   (let* ((p (sexp->kk-perm (binexp->parenthesization (A014486 n))))
;;          (d (permdeg p))
;;         )
;;     (A080300 (kk-perm->binexp (permul (permul (list (iota d)) p)
;;                                       (perminv (list (iota d)))
;;                               )
;;              )
;;     )
;;   )
;; )


;; Also:


;; (define (A057501v3 n)
;;   (let* ((p (sexp->kk-perm (binexp->parenthesization (A014486 n))))
;;          (d (permdeg p))
;;         )
;;     (A080300 (kk-perm->binexp (permul (permrot (permul p (perminv (list (iota d)))) -1)
;;                                       (list (iota d))
;;                               )
;;              )
;;     )
;;   )
;; )

;; (define (A057502v3 n)
;;   (let* ((p (sexp->kk-perm (binexp->parenthesization (A014486 n))))
;;          (d (permdeg p))
;;         )
;;     (A080300 (kk-perm->binexp (permul (permrot (permul p (perminv (list (iota d)))) +1)
;;                                       (list (iota d))
;;                               )
;;              )
;;     )
;;   )
;; )


