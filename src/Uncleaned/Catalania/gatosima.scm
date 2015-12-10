
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;;  http://www.megabaud.fi/~karttu/matikka/Nekomorphisms/gatosima.scm   ;;
;;  - System for automatically constructing simple recursive            ;;
;;    gatomorphisms of type A starting from the basic primitives        ;;
;;    swap!, exch2first-cdr!                                            ;;
;;                                                                      ;;
;;  This Scheme-code is coded 2002 by Antti Karttunen,                  ;;
;;  (E-mail: my_firstname.my_surname@iki.fi) and is placed in           ;;
;;  Public Domain.                                                      ;;
;;                                                                      ;;
;;  All the examples run at least in MIT Scheme Release 7.7.1, for      ;;
;;  which one can find documentation and the pre-compiled binaries      ;;
;;  (for various OS's running in Intel x86 architecture) under the URL: ;;
;;                                                                      ;;
;;  http://www.swiss.ai.mit.edu/projects/scheme/                        ;;
;;                                                                      ;;
;;  The main pointer for this code collection is:                       ;;
;;  http://www.megabaud.fi/~karttu/matikka/Nekomorphisms/gatomorf.htm   ;;
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


;; The types of recursion we use:

;;
;; 0: (foo! s), (recurse (car s)), (recurse (cdr s))
;;   Recursing both branches. Inverse is obtained by substituting foo!'s
;;   inverse into case 1.
;;
;; 1: (recurse (cdr s)), (recurse (car s)), (foo! s)
;;   See case 0.
;;
;; 2: (foo! s), (recurse (cdr s))
;;   Inverse is obtained by substituting foo!'s inverse into case 3.
;;   Form (foo! s), (recurse (car s)) is obtainable as the car/cdr-flipped
;;   conjugate of this case applied to foo!'s car/cdr-flipped conjugate.
;;
;; 3: (recurse (cdr s)), (foo! s)
;;   See case 2.
;;
;; Now we take also this case:
;; 4: (recurse (car s)), (foo! s), (recurse (cdr s))
;;   Recursing car-branch first, and then applying foo! before
;;   recursing the cdr-branch.
;;   Form (recurse (cdr s)), (foo! s), (recurse (car s))
;;   is obtainable as the car/cdr-flipped conjugate of this case
;;   applied to foo!'s car/cdr-flipped conjugate.
;;
;;   Inverse is obtained by applying that case to foo!'s inverse,
;;   i.e. as the car/cdr-flipped conjugate of the case
;;   (recurse (car s)), (foo! s), (recurse (cdr s))
;;   applied to the car/cdr-flipped conjugate of foo!'s inverse.
;;   (or the inverse of car/cdr-flipped conjugate of foo!, which
;;    is the same thing).

;;  Note that we still leave out some simple recursion cases like:
;;   (recurse (car s)), (foo! s), (recurse (car s))
;;   (foo! s), (recurse (car s)), (recurse (car s))
;;   (recurse (car s)), (recurse (car s)), (foo! s)
;; or even
;;   (foo! (car s)) (recurse (cdr s))
;;
;; etc, but having 5 means that we can see the recursion type of
;; recursively composed gatomorphisms (all with even n)
;; straight from the last decimal digit of n. (2 & 4 -> cases 0 & 1),
;; (6 & 8 -> cases 2 & 3), (0 -> case 4).

;; Permutation of non-negative integers:
;; The position of the "canonical inverse" in 73200.
;; I.e. for 0 & 1, i's 0 and 1, otherwise for odd n,
;; (A o B)'s inverse is (B' o A'), where A' and B' are
;; the inverses of A and B, respectively,
;; and for even n follow the practices
;; given above for forming inverse,
;; and do this recursively.
;;

;; The percentage of "obscure junk" grows steadily as the n grows.

;;   0: SwapBinTree
;;   1: exch2first-cdr


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;;
;; The basic composition logic, regardless of the implementation.        ;;
;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *HAND-CODED-GATOMORPHISMS*
  (list (cons 01477 A001477) ;; 7
        (cons 57117 A057117)
        (cons 57118 A057118)
        (cons 57161 A057161) ;;
        (cons 57162 A057162) ;;
        (cons 57163 A057163) ;; 2   ReflectBinTree
        (cons 57164 A057164) ;; 164 DeepRev
        (cons 57501 A057501) ;; 261 RotateHandshakes
        (cons 57502 A057502) ;;
        (cons 57503 A057503) ;;
;;      (cons 57504 A057504) ;; until coded.
        (cons 57505 A057505) ;;
        (cons 57506 A057506) ;;
        (cons 57508 A057508) ;; 168 Reverse
        (cons 57509 A057509) ;; 16  Rol
        (cons 57510 A057510) ;; 18  Ror
        (cons 57511 A057511) ;; 12  DeepRol
        (cons 57512 A057512) ;; 14  DeepRor
        (cons 69767 A069767) ;;
        (cons 69768 A069768) ;;
        (cons 69769 A069769)
        (cons 69770 A069770) ;; 0   Swap
        (cons 69771 A069771)
        (cons 69772 A069772)
        (cons 69773 A069773)
        (cons 69774 A069774)
        (cons 69775 A069775) ;;
        (cons 69776 A069776)
        (cons 69787 A069787)
        (cons 69888 A069888)
        (cons 69889 A069889) ;;
        (cons 71655 A071655)
        (cons 71656 A071656)
        (cons 71657 A071657)
        (cons 71658 A071658)
        (cons 71659 A071659)
        (cons 71660 A071660)
        (cons 71661 A071661)
        (cons 71662 A071662)
        (cons 71663 A071663)
        (cons 71664 A071664)
        (cons 71665 A071665)
        (cons 71666 A071666)
        (cons 71667 A071667)
        (cons 71668 A071668)
        (cons 71669 A071669)
        (cons 71670 A071670)
        (cons 72088 A072088)
        (cons 72089 A072089)
        (cons 72090 A072090)
        (cons 72091 A072091)
        (cons 72092 A072092)
        (cons 72093 A072093)
        (cons 72094 A072094)
        (cons 72095 A072095)
        (cons 72621 A072621)
        (cons 72622 A072622)
        (cons 72796 A072796) ;; 1   Exch2First-cdr
        (cons 72797 A072797) ;; 171 = 2o1o2 = Exch2First-car
        (cons 73194 A073194)
        (cons 73195 A073195)
        (cons 73196 A073196)
        (cons 73197 A073197)
        (cons 73198 A073198)
        (cons 73199 A073199)
        (cons 73205 A073205)
        (cons 73206 A073206)
        (cons 73207 A073207)
        (cons 73208 A073208)
        (cons 73209 A073209)
        (cons 73210 A073210)
        (cons 73269 A073269) ;; 3
        (cons 73270 A073270) ;; 5
        (cons 73280 A073280) ;; 9
        (cons 73281 A073281) ;; 15
        (cons 73282 A073282) ;; 13
        (cons 73283 A073283) ;; 19
        (cons 73284 A073284) ;; 20
        (cons 73285 A073285) ;; ...
        (cons 73286 A073286) ;; 41
        (cons 73287 A073287) ;; ...
        (cons 73288 A073288) ;; 416
        (cons 73289 A073289) ;; ...
        (cons 73290 A073290) ;; 105
        (cons 73291 A073291) ;; 197
        (cons 73292 A073292) ;; 
        (cons 73293 A073293) ;; 
        (cons 73294 A073294) ;; 
        (cons 73295 A073295) ;; 
        (cons 73296 A073296) ;; 
        (cons 73297 A073297) ;; 
        (cons 73298 A073298) ;; 
        (cons 73299 A073299) ;; 
  )
)



(define *SIMA-VEC* #f)

(define (add-to-sima-vec! n what)
   (if (>= n (vector-length *SIMA-VEC*))
       (set! *SIMA-VEC* (vector-grow *SIMA-VEC* (1+ n)))
   )
   (vector-set-and-return-value! *SIMA-VEC* n what)
)


(define (first-point-with-vector lista)
   (cond ((null? lista) #f)
         ((vector? (car lista)) lista)
         (else (first-point-with-vector (cdr lista)))
   )
)

(define *CC-COUNT-AUX-VEC* #f)
(define *CC-CYCLE-TYPES-VEC* #f)
(define *CACHE-LIST* (list))

;; This is updated in obtain-cached-sima-function!
(define *LATEST-SAFE-CACHE-ROBBING-POINT* *CACHE-LIST*)
(define *MAX-N-CACHES* 10) ;; Should be > some critical value to avoid deadlocking...

(define *CACHES-ALLOCATED-NOW* 0)


(define (rob-cache-vector)
  (let ((robbed-vec (car *LATEST-SAFE-CACHE-ROBBING-POINT*)))
          (format #t
"(rob-cache-vector): robbing the cache: ~A~%" (vector-head robbed-vec 23)
          )
;; Tell the old user that it has lost its treasure:
          (set-car! *LATEST-SAFE-CACHE-ROBBING-POINT* #f)
          (set! *LATEST-SAFE-CACHE-ROBBING-POINT*
               (first-point-with-vector (cdr *LATEST-SAFE-CACHE-ROBBING-POINT*))
          )
          (vector-fill! robbed-vec #f) ;; Clear it for our new sima function.
          robbed-vec
  )
)

;; This returns a pointer to the cache-list to the point where
;; the reserved vector is located, i.e., now always as the first element
;; of *CACHE-LIST*.

(define (reserve-indirect-vector size)
    (cond
       ((< *CACHES-ALLOCATED-NOW* *MAX-N-CACHES*)
         (set! *CACHE-LIST* (cons (make-vector size) *CACHE-LIST*))
         (set! *CACHES-ALLOCATED-NOW* (1+ *CACHES-ALLOCATED-NOW*))
         (vector-fill! (car *CACHE-LIST*) #f)
         *CACHE-LIST*
       )
       (else ;; We have to rob some other simaform's cache, and clear it for this one.
;; Take the last one allocated before this call to obtain-cached-sima-function!:
          (set! *CACHE-LIST* (cons (rob-cache-vector) *CACHE-LIST*))
          *CACHE-LIST*
       )
    )
)

(define (indirect-vector-ref vecptr ind)
   (cond ((vector? (car vecptr)) (vector-ref (car vecptr) ind))
         (else
               (format #t
"(indirect-vector-ref ... ~A): our cache has been robbed, robbing somebody's else...~%"
                       ind)
               (set-car! vecptr (rob-cache-vector))
               (indirect-vector-ref vecptr ind)
         )
   )
)

(define (indirect-vector-set-and-return-value! vecptr ind val)
   (cond ((vector? (car vecptr)) (vector-set-and-return-value! (car vecptr) ind val))
         (else
               (format #t
"(indirect-vector-set-and-return-value! ... ~A ~A): our cache has been robbed, robbing somebody's else...~%"
                       ind val)
               (set-car! vecptr (rob-cache-vector))
               (indirect-vector-set-and-return-value! vecptr ind val)
         )
   )
)


(define (compose-recursive-cached-sima-fun
              foo ;; Another such creature
              rectype ;; 0-3, what kind of recursion we want.
              $new-memo ;; E.g. make-vector
              $fetch-from-memo ;; E.g. vector-ref
              $store-to-memo! ;; E.g. vector-set-and-return-value!
              memosize        ;; E.g. the size of the vector. Could be ignored as well.
              always-fixed?   ;; Which objects are always fixed? () and (()) or 0 and 1.
              %cons %car %cdr  ;; Functions implementing cons, car & cdr primitives.
        )
  (letrec
    ((memo ($new-memo memosize))
     (recurse
       (lambda (s)
         (cond ((eq? #t s) memo) ; For debugging, reveal our memo.
               (($fetch-from-memo memo s))
               ((always-fixed? s) ($store-to-memo! memo s s))
               (else
                  ($store-to-memo! memo s
                     (case rectype
                        ((0) ;; (foo! s) (recurse (car s)) (recurse (cdr s))
                            (let ((t (foo s)))
                              (%cons (recurse (%car t)) (recurse (%cdr t)))
                            )
                        )
                        ((1) ;; (recurse (cdr s)) (recurse (car s)) (foo! s)
                             (foo (%cons (recurse (%car s)) (recurse (%cdr s))))
                        )
                        ((2) ;; (foo! s) (recurse (cdr s))
                            (let ((t (foo s)))
                              (%cons (%car t) (recurse (%cdr t)))
                            )
                        )
                        ((3) ;; (recurse (cdr s)) (foo! s)
                             (foo (%cons (%car s) (recurse (%cdr s))))
                        )
                        ((4) ;; (recurse (car s)) (foo! s) (recurse (cdr s))
                            (let ((t (foo (%cons (recurse (%car s)) (%cdr s)))))
                              (%cons (%car t) (recurse (%cdr t)))
                            )
                        )
                     )
                  )
               ) ;; else
         ) ;; cond
       ) ;; lambda
     ) ;; (recurse ..)
    ) ;; ((memo ...) ...)
    recurse ;; return our new recursive composition.
  ) ;; letrec
)



(define (compose-two-cached-sima-funs
              foo bar ;; Two such creatures
              $new-memo ;; E.g. make-vector
              $fetch-from-memo ;; E.g. vector-ref
              $store-to-memo! ;; E.g. vector-set-and-return-value!
              memosize        ;; E.g. the size of the vector. Could be ignored as well.
              always-fixed?   ;; Which objects are always fixed?
        )
  (letrec
    ((memo ($new-memo memosize))
     (composition
       (lambda (s)
         (cond ((eq? #t s) memo) ; For debugging, reveal our memo.
               (($fetch-from-memo memo s))
               ((always-fixed? s) ($store-to-memo! memo s s))
               (else ($store-to-memo! memo s (foo (bar s))))
         ) ;; cond
       ) ;; lambda
     ) ;; (composition ..)
    ) ;; ((memo ...) ...)
    composition ;; return our new composition.
  ) ;; letrec
)



(define (left-side-ref n)
  (let ((v (A059905 (fix:lsh n -2))))
     (cond ((< v 3) v) ;; 0->0, 1->1, 2->2, 3->4, 4->6, 5->8, 6->10, 7->12, etc.
           (else (* 2 (-1+ v)))
     )
  )
)

(define right-side-ref A059906) ;; The odd-positioned bits (bit-1, 3, 5, etc.)

(load-option 'format) ;; To use format, do this.


;; Returns the new gatosimas vector with the required function in position n:
(define (obtain-cached-sima-function! n
              $new-memo ;; E.g. make-vector
              $fetch-from-memo ;; E.g. vector-ref
              $store-to-memo! ;; E.g. vector-set-and-return-value!
              memosize        ;; E.g. the size of the vector. Could be ignored as well.
              always-fixed?
              %cons %car %cdr  ;; Functions implementing cons, car & cdr primitives.
        )
  (set! *LATEST-SAFE-CACHE-ROBBING-POINT* *CACHE-LIST*)
  (format #t
   "For simaform ~A. the (length  *LATEST-SAFE-CACHE-ROBBING-POINT*)=~A<BR>~%"
                      n
                      (length *LATEST-SAFE-CACHE-ROBBING-POINT*)
  )
  (let fetch ((n n)) ;; Fetch the function n.
    (cond ((and (< n (vector-length *SIMA-VEC*)) (vector-ref *SIMA-VEC* n)))
          ((zero? (modulo n 2))
  ;; Even n's reserved for recursively composed simaforms.
             (let* ((foo-index (floor->exact (/ (- n 2) 10))) ;; For rectypes 0 - 4.
                    (rectype (fix:lsh (modulo (- n 2) 10) -1)) ;;   --- "" ---
;;                  (foo-index (fix:lsh (- n 2) -3)) ;; if we had only rectypes 0-3.
;;                  (rectype (fix:lsh (fix:and (- n 2) 7) -1)) ;;  --- "" ---
                   )
                (format #t
      "The simaform ~A. is recursively (rectype ~A) composed from the simaform ~A.~%"
                      n rectype foo-index
                )
                (add-to-sima-vec!
                      n
                      (compose-recursive-cached-sima-fun
                                               (fetch foo-index)
                                               rectype
                                               $new-memo
                                               $fetch-from-memo
                                               $store-to-memo!
                                               memosize
                                               always-fixed?
                                               %cons %car %cdr
                      )
                )
             )
          )
          (else
  ;; Odd n's reserved for ordinarily composed simaforms.
  ;; At the left side we use only the primitives SwapBinTree (0), exch2first-cdr (1)
  ;; or any recursively composed simaform in even position (2,4,6,8,10,...)
  ;; At the right side we can use any simaform we like.
             (let ((foo-index (left-side-ref n))
                   (bar-index (right-side-ref n))
                  )
                (format #t
      "The simaform ~A. is an ordinary composition of the simaforms ~A. and ~A.~%"
                      n foo-index bar-index
                )
                (add-to-sima-vec!
                        n
                        (compose-two-cached-sima-funs
                                               (fetch foo-index)
                                               (fetch bar-index)
                                               $new-memo
                                               $fetch-from-memo
                                               $store-to-memo!
                                               memosize
                                               always-fixed?
                        )
                )
             ) ;; let
          ) ;; else
    )
  ) ;; let fetch
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;;
;; Consing implementation.                                               ;;
;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Later I will code this caching with a more efficient method,
;; but right now a simple association list, where the new stuff
;; is added to the END suffices to us.
;;
;; Here the car keeps the pointer to the beginning of the assoc list,
;; and cdr to the last pair of it, which is updated when
;; new stuff is added with store-to-memo! to the end:

(define (new-memo ignore)
  (let ((empty-a-list (list (cons (list) (list)))))
    (cons empty-a-list empty-a-list)
  )
)

(define (fetch-from-memo memo s)
   (cond ((assoc s (car memo)) => cdr)
         (else #f)
   )
)

(define (store-to-memo! memo s val)
  (let ((new-node  (list (cons s val))))
   (set-cdr! (cdr memo) new-node) ;; Add the new node to the end.
   (set-cdr! memo (cddr memo))    ;; and update the pointer to that end.
   val
  )
)


(define (obtain-consing-sima-function! n)
    (if (not *SIMA-VEC*) (set! *SIMA-VEC* (vector SwapBinTree exch2first-cdr)))
    (obtain-cached-sima-function! n
                                  new-memo
                                  fetch-from-memo
                                  store-to-memo!
                                  0
                                  (lambda (s) (or (null? s) (equal? '(()) s)))
                                  cons car cdr
    )
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;;
;;                                                                       ;;
;;                                                                       ;;
;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (load-precomputed-vec filename size max-elem)
   (format #t "Loading a vector of size ~A from the file ~A ..." size filename)
   (let ((vec (make-vector size))
        )
     (call-with-input-file filename
        (lambda (inport)
           (let loop ((nextnum (read inport)) (n 0) (inserted 0))
              (cond ((or (eq? n size) (eof-object? nextnum))
                       (format #t " inserted ~A items~%" inserted)
                       (flush-output (current-output-port))
                       vec
                    )
                    (else
                          (if (or (negative? max-elem) (<= nextnum max-elem))
                              (vector-set! vec n nextnum)
                          )
                          (loop (read inport)
                                (1+ n)
                                (if (or (negative? max-elem) (<= nextnum max-elem))
                                    (1+ inserted)
                                    inserted
                                )
                          )
                    )
              )
           )
        )
     )
   )
)


(define (load-vecs-and-dump upto-n)
   (let* ((vec-size (A014137 upto-n)) ;; I.e. 290512 for upto-n = 12.
          (constab-size (/ (* 2080 2081) 2))
          (VecA014486 (load-precomputed-vec "c:\\matikka\\Nekomorphisms\\vA014486.lst" vec-size -1))
          (VecA069770 (load-precomputed-vec "c:\\matikka\\Nekomorphisms\\vA069770.lst" vec-size -1))
          (VecA072771 (load-precomputed-vec "c:\\matikka\\Nekomorphisms\\vA072771.lst" vec-size -1))
          (VecA072772 (load-precomputed-vec "c:\\matikka\\Nekomorphisms\\vA072772.lst" vec-size -1))
          (VecA072796 (load-precomputed-vec "c:\\matikka\\Nekomorphisms\\vA072796.lst" vec-size -1))
          (VecA072764 (load-precomputed-vec "c:\\matikka\\Nekomorphisms\\vA072764.lst" constab-size (-1+ vec-size)))
         )
      (fasdump VecA014486 (format #f "VA014486.b~A" upto-n))
      (fasdump VecA069770 (format #f "VA069770.b~A" upto-n))
      (fasdump VecA072771 (format #f "VA072771.b~A" upto-n))
      (fasdump VecA072772 (format #f "VA072772.b~A" upto-n))
      (fasdump VecA072796 (format #f "VA072796.b~A" upto-n))
      (fasdump VecA072764 (format #f "VA072764.b8"))
   )
)

  
(define (fix_binwidth n) ;; A029837 (with a(0)=0 instead of 1)
  (let loop ((n n) (i 0)) ;;  or A036377 (with offset=0 instead of 1)
     (if (fix:zero? n)
         i
         (loop (fix:lsh n -1) (1+ i))
     )
  )
)

(define (fix_ConsTBBS a b) ;; "cons" two totally balanced binary sequences
   (let ((aw (fix_binwidth a))
         (bw (fix_binwidth b))
        )
     (+ (fix:lsh 1 (+ 1 aw bw)) (fix:lsh a (1+ bw)) b)
   )
)


(define (precompute_A072764 filename upto_diagonal)
   (with-output-to-file filename
     (lambda ()
        (let outloop ((n 0))
           (cond ((<= n upto_diagonal)
                    (let inloop ((x n) (y 0))
                       (write (CatalanRankGlobal (consTBBS (A014486 x) (A014486 y))))
                       (newline)
                       (if (zero? x)
                           (outloop (1+ n))
                           (inloop (-1+ x) (1+ y))
                       )
                    )
                 )
           )
        )
     )
   )
)

;; (define (packA001477 x y) (/ (+ (expt (+ x y) 2) x (* 3 y)) 2))

;; cons-cache-arg-limit should be 2079 if VecA072764 is computed
;; with (constab-size (/ (* 2080 2081) 2))

;; Let's forget the cons-caching for the moment. The naive limit
;; checking given below won't work...

(define (FixCons x y VecA014486) ;; Not needed: VecA072764 cons-cache-arg-limit 
;; (cond
;;   ((or (fix:> x cons-cache-arg-limit) (fix:> y cons-cache-arg-limit))
            (FixRank (fix_ConsTBBS (vector-ref VecA014486 x)
                                     (vector-ref VecA014486 y)
                       )
                       VecA014486
            )
;;   )
;;   (else
;;         (let ((ind (fix:lsh (fix:+ ((lambda (x+y) (fix:* x+y x+y)) (fix:+ x y))
;;                                    (fix:+ x (fix:* 3 y))
;;                             )
;;                             -1
;;                     )
;;              ))
;;           (vector-ref VecA072764 ind)
;;         )
;;   )
;; )
)


;; pref11 is 0 if tbs begins as 10.., 1 if it begins as 11...
;; Start searching from [lowlim,uplim[ range with a simple binary
;; search.

(define (FixRank tbs VecA014486)
   (if (zero? tbs)
       0
       (let* ((bw (fix_binwidth tbs))
              (size_1 (-1+ (fix:lsh bw -1)))
              (pref11 (fix:and (fix:lsh tbs (- 2 bw)) 1))
              (lowlim (+ (A014137 size_1) (* pref11 (A000108 size_1))))
              (uplim  (+ (A014137 (+ size_1 pref11))
                         (* (fix:and (1+ pref11) 1) (A000108 size_1))
                      )
              )
             )
         (fix_BinarySearchFromVector VecA014486 tbs lowlim uplim)
       )
   )
)

(define (fix_BinarySearchFromVector vec item incl_lowlim excl_uplim)
   (let loop ((incl_lowlim incl_lowlim) (excl_uplim excl_uplim))
     (let* ((between (fix:lsh (fix:+ incl_lowlim excl_uplim) -1))
            (numbet (vector-ref vec between))
           )
       (cond ((fix:= numbet item) between) ;; Return the pos. where the item was found
             ((fix:>= incl_lowlim between) #f) ;; We have failed.
             ((fix:> numbet item) (loop incl_lowlim between))
             (else (loop between excl_uplim))
       )
     )
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;;
;;                                                                       ;;
;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (elements-fixed-by simafun incl_lowlim excl_uplim)
   (let loop ((fixed 0) (n incl_lowlim))
         (cond ((fix:>= n excl_uplim) fixed)
               ((fix:= (simafun n) n) (loop (1+ fixed) (1+ n)))
               (else (loop fixed (1+ n)))
         )
   )
)


;; Note that for sizes 0 & 1 we know that there are only one cycle,
;; one fixed element, the max. cycle size is 1, and the lcm of all cycle sizes is 1.

(define (get-count-list count-fun simafun size)
   (let loop ((n 2) (fcs (list 1 1)))
      (cond ((> n size) (reverse! fcs))
            (else (loop (1+ n)
                        (cons (count-fun simafun
                                         (A014137 (-1+ n))
                                         (A014137 n)
                              )
                              fcs
                        )
                  )
            )
      )
   )
)



(define (find-next-nonzero vec incl_lowlim excl_uplim)
   (let loop ((n incl_lowlim))
         (cond ((fix:>= n excl_uplim) 0)
               ((not (fix:zero? (vector-ref vec n))) n)
               (else (loop (fix:1+ n)))
         )
   )
)


(define (count-cycles vec incl_lowlim excl_uplim cycle-types)
   (let loop ((ind incl_lowlim) (cyclen 0) (cycles 0) (fixedcycs 0) (maxcyclen 0) (lcm_cyclen 1) (sum_cyclens 0))
     (let ((next (vector-ref vec ind)))
;;       (format #t "(count-cycles ~S ~A ~A): cycles=~A ind=~A cyclen=~A next=~A~%"
;;                     vec incl_lowlim excl_uplim cycles ind cyclen next
;;       )
         (cond ((fix:zero? next) ;; This cycle finished?
                  (let ((cont-point (find-next-nonzero vec ind excl_uplim)))
                     (cond ((fix:zero? cont-point) ;; All overwritten in this range?
                               (if cycle-types
                                   (incr-num-vector! cycle-types cyclen)
                               )
                               (values ;; Incorporate the last cycle's information also!
                                       (1+ cycles)
                                       (if (fix:= 1 cyclen) (1+ fixedcycs) fixedcycs)
                                       (max cyclen maxcyclen)
                                       (lcm cyclen lcm_cyclen)
                                       (fix:+ sum_cyclens cyclen) ;; Should sum to Cat..
                               )
                           )
                           (else
                                 (if cycle-types (incr-num-vector! cycle-types cyclen))
                                 (loop cont-point
                                       0 ;; Initialize cycle length back to zero.
                                       (1+ cycles)
                                       (if (fix:= 1 cyclen) (1+ fixedcycs) fixedcycs)
                                       (max cyclen maxcyclen)
                                       (lcm cyclen lcm_cyclen)
                                       (fix:+ sum_cyclens cyclen)
                                 )
                           )
                     )
                  )
               )
               (else
                     (vector-set! vec ind 0) ;; Clear the visited points.
                     (loop next (1+ cyclen)
                           cycles fixedcycs maxcyclen lcm_cyclen sum_cyclens
                     )
               )
         )
     ) ;; let
   ) ;; let loop
)


;; Get our local copy of the simafun's cache (which we are then free to corrupt...)
(define (initialize-aux-vec-with-funs-values! auxvec simafun size)
   (let ((vecsize (vector-length auxvec)))
;; Not this easy, cannot just copy the cache in dirty way:
;;    (subvector-move-left! (car (simafun #t)) 0 vecsize auxvec 0)
;; Instead, we have to explicitly copy the values, as we might not
;; have computed yet this simafun for the whole range:
     (let copyloop ((n 0))
         (cond ((fix:< n vecsize)
                  (vector-set! auxvec n (simafun n))
                  (copyloop (fix:1+ n))
               )
         )
     )
     auxvec
   )
)

(define (get-cc-lists simafun size)
   (initialize-aux-vec-with-funs-values! *CC-COUNT-AUX-VEC* simafun size)
   (let loop ((n 2)
              (ccs (list 1 1))
              (fcs (list 1 1))
              (mcs (list 1 1))
              (lcs (list 1 1))
;;            (scs (list 1 1))
             )
      (cond ((> n size)
                (values (reverse! ccs)
                        (reverse! fcs)
                        (reverse! mcs)
                        (reverse! lcs)
;;                      (reverse! scs)
                )
            )
            (else
              (call-with-values
                      (lambda () (count-cycles *CC-COUNT-AUX-VEC*
                                         (A014137 (-1+ n))
                                         (A014137 n)
                                         #f ;; omitted now: *CC-CYCLE-TYPES-VEC*
                                 )
                      )
                      (lambda (cycles fixedcycs maxcyclen lcm_cyclen sum_cyclens)
                         (if (not (eq? sum_cyclens (A000108 n)))
                             (format #t
 "***ERROR: count-cycles FAILED MISERABLY, IT RETURNED sum_cyclens as ~A for ~A.~%"
                                     sum_cyclens n
                             )
                         )
;;            (format #t "Cycle-types for n=~A is ~A~%" n
;;                (collect-non-zero-values-from-num-vector *CC-CYCLE-TYPES-VEC*)
;;            )
;;            (vector-fill! *CC-CYCLE-TYPES-VEC* 0) ;; Clear the cycle-types vector.

                         (loop (1+ n)
                               (cons cycles      ccs)
                               (cons fixedcycs   fcs)
                               (cons maxcyclen   mcs)
                               (cons lcm_cyclen  lcs)
;;                             (cons sum_cyclens scs)
                         )
                      )
              ) ;; call-with-values
            ) ;; else
      ) ;; cond
   )
)

(define (pre-compute-gatomorphisms-on-list Anum-gatomorf-pairs upto-the-value)
 (let ((iota_upto_the_value (iota0 upto-the-value)))
  (let loop ((alist Anum-gatomorf-pairs))
        (cond ((pair? alist)
                 (for-each (cdar alist) iota_upto_the_value) ;; Will also cache it.
                 (loop (cdr alist))
              )
        )
  )
 )
)



(define (find-matching-gatomorphism-from-list simafun Anum-gatomorf-pairs upto-val)
  (let outloop ((alist Anum-gatomorf-pairs))
        (cond ((pair? alist)
                 (let inloop ((n upto-val)) ;; Probably differs in the high end
                    (cond ((fix:< n 2) (car alist)) ;; Found one!
                          ((fix:= ((cdar alist) n) (simafun n))
                               (inloop (fix:-1+ n))
                          ) ;; Equivalent so far, continue checking.
                          (else (outloop (cdr alist))) ;; First differing, try next one.
                    )
                 )
              )
              (else #f)
        )
  )
)


(define (compose-fun-wrapper-around-cache-vector vec)
  (lambda (x) (vector-ref vec x))
)

(define (form-obtain-fix-sima-function upto-n max-n-caches)
  (let* ((vec-size (A014137 upto-n)) ;; I.e. 290512 for upto-n = 12.
         (constab-size (/ (* 2080 2081) 2))
         (path "c:\\matikka\\Nekomorphisms\\")
         (load-size 12)
         (VecA014486 (fasload (format #f "~AVA014486.b~A" path load-size)))
         (VecA069770 (fasload (format #f "~AVA069770.b~A" path load-size)))
         (VecA072771 (fasload (format #f "~AVA072771.b~A" path load-size)))
         (VecA072772 (fasload (format #f "~AVA072772.b~A" path load-size)))
         (VecA072796 (fasload (format #f "~AVA072796.b~A" path load-size)))
;;       (VecA072764 (fasload (format #f "~AVA072764.b~A" path 8)))
         (%cons (lambda (x y) (FixCons x y VecA014486)))
         (%car  (compose-fun-wrapper-around-cache-vector VecA072771))
         (%cdr  (compose-fun-wrapper-around-cache-vector VecA072772))
         (%swap (compose-fun-wrapper-around-cache-vector VecA069770))
         (%exch2first (compose-fun-wrapper-around-cache-vector VecA072796))
        )
    (set! *SIMA-VEC* (vector %swap %exch2first))
    (set! *CACHE-LIST* (list))
    (set! *MAX-N-CACHES* max-n-caches)
    (set! *CC-COUNT-AUX-VEC* (make-vector vec-size))
    (set! *CC-CYCLE-TYPES-VEC* (make-vector (expt 2 12)))
    (lambda (n)
      (obtain-cached-sima-function! n
                                    reserve-indirect-vector
                                    indirect-vector-ref
                                    indirect-vector-set-and-return-value!
                                    vec-size
                                    (lambda (s) (fix:< s 2))
                                    %cons %car %cdr
      )
;;    (obtain-cached-sima-function! n
;;                                  make-vector
;;                                  vector-ref
;;                                  vector-set-and-return-value!
;;                                  vec-size
;;                                  (lambda (s) (fix:< s 2))
;;                                  %cons %car %cdr
;;    )
    )
  ) ;; let*
)




(define (output-all-5-tables filename size) ;; Should be 12
  (let ((ofsf (form-obtain-fix-sima-function size (1+ size))))
   (with-output-to-file filename
     (lambda ()
        (let loop ((what 0))
              (output_seq (collect-table ofsf size what))
              (newline)
              (if (< what 4) (loop (1+ what)))
        )
     )
   )
  )
)


(define (collect-table ofsf size what) ;; what: 0->perms, 1->ccs, 2->fix, 3->max, 4->lcm
  (let ((uplim (binomial_n_2 (+ 2 size)))) ;; 2 -> 6, 3 -> 10, etc.
    (let loop ((n 0) (res (list)))
       (cond
         ((eq? n uplim) (reverse! res))
         (else ;; i.e. (< n uplim)
           (let* ((x (A025581 n))
                  (y (A002262 n))
                  (simafun (ofsf y))
                  (val (cond ((zero? what) (simafun x))
                             (else
                               (call-with-values
                                  (lambda () (get-cc-lists simafun size))
                                  (lambda (ccs fcs mcs lcs)
                                     (case what
                                       ((1) (list-ref ccs x))
                                       ((2) (list-ref fcs x))
                                       ((3) (list-ref mcs x))
                                       ((4) (list-ref lcs x))
                                     )
                                  )
                               ) ;; call-with-values
                             )
                       ) ;; cond
                  )
                 )
             (loop (1+ n) (cons val res))
           ) ;; let*
         )
       ) ;; cond
    ) ;; let loop
  ) ;; outer let
)


(define (list-sima-forms size sima-upto-n list-upto-n search-upto-n max-n-caches) 
   (pre-compute-gatomorphisms-on-list *HAND-CODED-GATOMORPHISMS* (-1+ (A014137 8)))
   (let ((ofsf (form-obtain-fix-sima-function size max-n-caches))
         (first-of-N (iota0 (-1+ list-upto-n)))
        )
     (let loop ((n 0))
       (let* ((simafun (ofsf n))
              (already-in-EIS? (find-matching-gatomorphism-from-list simafun
                                   *HAND-CODED-GATOMORPHISMS* (-1+ (A014137 8))
                               )
              )
             )

             (cond (already-in-EIS?
                    (format #t "Seems that <A HREF=\"http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=A0~A\">A0~A</A> = " 
                            (car already-in-EIS?) (car already-in-EIS?)
                    )
                   )
             )

             (output-sima-link "Perm" n (map simafun first-of-N)
                               list-upto-n search-upto-n
             )

;;           (output-sima-link "FIX" n (get-count-list elements-fixed-by simafun size)
;;                             list-upto-n search-upto-n
;;           )

             (call-with-values
                 (lambda () (get-cc-lists simafun size))
                 (lambda (ccs fcs mcs lcs)
                    (output-sima-link "CC"  n ccs list-upto-n 7)
                    (output-sima-link "FIX" n fcs list-upto-n 7)
                    (output-sima-link "MAX" n mcs list-upto-n 7)
                    (output-sima-link "LCM" n lcs list-upto-n 7)
                 )
             )

             (cond ((< n sima-upto-n)
                      (write-string "\n<P><BR></P>\n")
                      (loop (1+ n))
                   )
             )
       )
     )
   )
)


;; Call for example as (output-sima-html "simato16.htm" 12 16 120 33 11)

(define (output-sima-html filename
                          size ;; Should be 12
                          sima-upto-n
                          list-upto-n ;; Should be 120
                          search-upto-n ;; should be 33 or such
                          max-n-caches ;; 10 or such.
        )
   (with-output-to-file filename
     (lambda ()
        (format #t "<HTML><HEAD><TITLE>Sima sequences 0 - ~A" sima-upto-n)
        (write-string "</TITLE></HEAD><BODY BGCOLOR=\"white\" TEXT=\"blue\" LINKS=\"red\" VLINKS=\"red\">\n")
        (list-sima-forms size sima-upto-n list-upto-n search-upto-n max-n-caches) 
        (write-string "</BODY></HTML>")
     )
   )
)

;; Quite ugly, disposal code:
(define (output-sima-link strid simanum seq upto_n search_upto_n)
   (let ((search_upto_n (min search_upto_n (length (cdr seq))))
         (upto_n (min upto_n (length seq)))
        )
      (format #t " <BR><B>~A~A</B> := " strid simanum)
      (write-string "<A HREF=\"http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eishis.cgi?sequence=")
      (output_seq (list-head (cdr seq) search_upto_n))
      (write-string "\">[")
      (output_seq (list-head seq upto_n))
      (write-string "];</A> ");
   )
)



