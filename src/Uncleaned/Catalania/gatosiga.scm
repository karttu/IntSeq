
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;;  http://www.megabaud.fi/~karttu/matikka/Nekomorphisms/gatosiga.scm   ;;
;;                System for automatically constructing                 ;;
;;                  "Simple Gatomorphisms of type B"                    ;;
;;                 (here called "sigamorfs" for short)                  ;;
;;      starting from the basic primitives A069770 and A072796          ;;
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
;;   1: *A072796


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
        (cons 57503 A057503) ;; 2618
        (cons 57504 A057504!) ;; 5216
        (cons 57505 A057505) ;; 2614
        (cons 57506 A057506) ;; 5212
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
        (cons 72796 A072796) ;; 1   *A072796
        (cons 72797 A072797) ;; 171 = 2o1o2 = *A072797
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

        (cons 74679 A074679)
        (cons 74680 A074680)
        (cons 74681 A074681)
        (cons 74682 A074682)
        (cons 74683 A074683)
        (cons 74684 A074684)
        (cons 74685 A074685)
        (cons 74686 A074686)
        (cons 74687 A074687)
        (cons 74688 A074688)
        (cons 74689 A074689)
        (cons 74690 A074690)
  )
)



(define *SIGA-VEC* #f)

(define (add-to-siga-vec! n what)
   (if (>= n (vector-length *SIGA-VEC*))
       (set! *SIGA-VEC* (vector-grow *SIGA-VEC* (1+ n)))
   )
   (vector-set-and-return-value! *SIGA-VEC* n what)
)


(define (first-point-with-vector lista)
   (cond ((null? lista) #f)
         ((vector? (car lista)) lista)
         (else (first-point-with-vector (cdr lista)))
   )
)


(define (nth-point-with-vector n lista) ;; zero-based.
   (cond ((null? lista) #f)
         ((vector? (car lista))
             (if (zero? n) lista (nth-point-with-vector (-1+ n) (cdr lista)))
         )
         (else (nth-point-with-vector n (cdr lista)))
   )
)


(define *CC-COUNT-AUX-VEC* #f)
(define *CC-CYCLE-TYPES-VEC* #f)
(define *CACHE-LIST* (list))

;; This is updated in obtain-cached-siga-function!
(define *LATEST-SAFE-CACHE-ROBBING-POINT* *CACHE-LIST*)
(define *MAX-N-CACHES* 10) ;; Should be > some critical value to avoid BIG trouble...

(define *CACHES-REQUESTED-NOW* 0) ;; Keeps the length of *CACHE-LIST*
(define *LATEST-SIGAMORF-REQUESTED* 0)

;; We would obtain somewhat better reuse of the cache vectors
;; (meaning avoiding the wasteful robbing of still useful vectors)
;; if we avoided robbing AT LEAST the second most recent,
;; possibly also the third and the fourth most recent caches
;; (but the latest cache reserved for the previous sigamorf
;; is free game of course). OK, we will try that now!

;; Nope, this wouldn't be good either (with any skip value
;; greater than 1, as the growing head
;; of the *CACHE-LIST* would be then cluttered with the unused
;; caches used in the previous recursive compositions.
;; We will probably needs some kind of hit-based ranking
;; of caches anyway... Now we just avoid robbing the
;; second most recent cache.

;; Solution: The two first elements of each cache vector
;; are actually unnecessary, as we know them to be always
;; fixed, 0 and 1, thus first of them could be used for
;; keeping the sgtb-number of the gatomorphism it is allocated
;; for, and the second the sgtb-number of the latest
;; "top-level" gatomorphism which needs it (possibly over
;; a hop or two of intermediate gatomorphisms), and the
;; number of the latest top-level gatomorphism is updated
;; by obtain-cached-siga-function!, and kept in the
;; global variable *LATEST-SIGAMORF-REQUESTED*
;; and when deciding whether to rob a cache vector, 
;; its "root sigamorf"'s value would be compared
;; *LATEST-SIGAMORF-REQUESTED*, and if too close to it,
;; wouldn't be robbed.


(define (rob-cache-vector)
  (let ((the-most-recent-robbed? (eq? *LATEST-SAFE-CACHE-ROBBING-POINT* *CACHE-LIST*))
        (robbed-vec (car *LATEST-SAFE-CACHE-ROBBING-POINT*))
       )
;;        (format #t
;; "(rob-cache-vector): robbing the cache~A: ~A~%"
;;             (if the-most-recent-robbed? ", the most recent one" "")
;;             (vector-head robbed-vec 23)
;;        )
;; Tell the old user that it has lost its treasure:
          (set-car! *LATEST-SAFE-CACHE-ROBBING-POINT* #f)
          (set! *LATEST-SAFE-CACHE-ROBBING-POINT*
               (nth-point-with-vector
                          (if the-most-recent-robbed?
                              1 ;; protect the second most recent cache allocated...
                              0 ;; Usually start searching from the next one.
                          )
                          (cdr *LATEST-SAFE-CACHE-ROBBING-POINT*)
               )
          )
;;        (format #t
;; "Now (length *LATEST-SAFE-CACHE-ROBBING-POINT*)=~A, robbed the ~Ath cache<BR>~%"
;;                    (length *LATEST-SAFE-CACHE-ROBBING-POINT*)
;;                    (nthcdrmemq *LATEST-SAFE-CACHE-ROBBING-POINT* *CACHE-LIST*)
;;        )

          (vector-fill! robbed-vec #f) ;; Clear it for our new siga function.
          robbed-vec
  )
)

;; This returns a pointer to the cache-list to the point where
;; the reserved vector is located, i.e., now always as the first element
;; of *CACHE-LIST*.

(define (reserve-indirect-vector size)
    (cond
       ((< *CACHES-REQUESTED-NOW* *MAX-N-CACHES*)
         (set! *CACHE-LIST* (cons (make-vector size) *CACHE-LIST*))
         (set! *CACHES-REQUESTED-NOW* (1+ *CACHES-REQUESTED-NOW*))
         (vector-fill! (car *CACHE-LIST*) #f)
         *CACHE-LIST*
       )
       (else ;; We have to rob some other sigamorf's cache, and clear it for this one.
         (set! *CACHES-REQUESTED-NOW* (1+ *CACHES-REQUESTED-NOW*)) ;; Keep on adding.
;; Take the last one allocated before this call to obtain-cached-siga-function!:
         (set! *CACHE-LIST* (cons (rob-cache-vector) *CACHE-LIST*))
         *CACHE-LIST*
       )
    )
)

(define (indirect-vector-ref vecptr ind)
   (cond ((vector? (car vecptr)) (vector-ref (car vecptr) ind))
         (else
;;              (format #t
;; "(indirect-vector-ref ... ~A): our cache has been robbed, robbing someone else's...~%"
;;                      ind)
                (set-car! vecptr (rob-cache-vector))
                (indirect-vector-ref vecptr ind)
         )
   )
)

(define (indirect-vector-set-and-return-value! vecptr ind val)
   (cond ((vector? (car vecptr)) (vector-set-and-return-value! (car vecptr) ind val))
         (else
;;               (format #t
;; "(indirect-vector-set-and-return-value! ... ~A ~A): our cache has been robbed, robbing someone else's...~%"
;;                       ind val)
               (set-car! vecptr (rob-cache-vector))
               (indirect-vector-set-and-return-value! vecptr ind val)
         )
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;;
;; The basic composition logic, regardless of the implementation.        ;;
;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; These three functions for testing fold-right:

(define (compose-rec-case-3-by-foldr foo)
  (lambda (s) (fold-right (lambda (x y) (foo (cons x y))) '() s))
)

;; No need to check whether pair, if we are using only parenthesizations
;; with ()'s as terminal nodes:
(define (compose-rec-case-1-by-foldr-simple foo)
  (letrec ((bar (lambda (s)
                  (fold-right (lambda (x y) (foo (cons (bar x) y))) '() s)
                )
          ))
     bar
  )
)

(define (compose-rec-case-1-by-foldr foo)
  (letrec ((bar (lambda (s)
                  (cond ((not (pair? s)) s)
                        (else (fold-right (lambda (x y) (foo (cons (bar x) y))) '() s))
                  )
                )
          ))
     bar
  )
)

;;
;; We could define rotate left as:
;;
;; (define (Rol s)
;;   (cond ((not (pair? s)) s)
;;         (else (fold-right cons (list (car s)) (cdr s)))
;;   )
;; )

;; (define revyy (compose-rec-case-3-by-foldr Rol))
;; (revyy '(a (b (c d e) f) d)) --> (d (b (c d e) f) a)
;;
;; (define deep-revyy (compose-rec-case-1-by-foldr Rol))
;; (deep-revyy '(a (b (c d e) f) d)) --> (d (f (e d c) b) a)
;;
;; (define deep-revyy-simple (compose-rec-case-1-by-foldr-simple Rol))
;; (deep-revyy-simple '(() ((())()()) () ()))  --> (() () (() () (())) ())


;; Here is the function we actually use:

(define (compose-recursive-cached-siga-fun
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



(define (compose-two-cached-siga-funs
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

(define (index-for-composed-sgtb lhs rhs)
  (let ((new-lhs (cond ((< lhs 2) lhs)
                       ((even? lhs) (1+ (/ lhs 2)))
                       (else (error "Only the primitive gatomorphisms A069770 (0) & A072796 (1) or one of the recursively composed gatomorphisms (even numbers >= 2) can occur at the left side of the composition. Odd number > 1 is not allowed at the left side: " lhs))
                 )
       ))
    (1+ (packA054238 (* 2 new-lhs) rhs))
  )
)

(define (index-for-recursive-sgtb foo rectype) (+ 2 (* 10 foo) (* 2 rectype)))


(load-option 'format) ;; To use format, do this.


;; Returns the new gatosigas vector with the required function in position n:
(define (obtain-cached-siga-function! n
              $new-memo ;; E.g. make-vector
              $fetch-from-memo ;; E.g. vector-ref
              $store-to-memo! ;; E.g. vector-set-and-return-value!
              memosize        ;; E.g. the size of the vector. Could be ignored as well.
              always-fixed?
              %cons %car %cdr  ;; Functions implementing cons, car & cdr primitives.
        )
  (set! *LATEST-SAFE-CACHE-ROBBING-POINT* *CACHE-LIST*)
  (set! *LATEST-SIGAMORF-REQUESTED* n)
  (let fetch ((n n)) ;; Fetch the function n.
    (cond ((and (< n (vector-length *SIGA-VEC*)) (vector-ref *SIGA-VEC* n)))
          ((zero? (modulo n 2))
  ;; Even n's reserved for recursively composed sigamorfs.
             (let* ((foo-index (floor->exact (/ (- n 2) 10))) ;; For rectypes 0 - 4.
                    (rectype (fix:lsh (modulo (- n 2) 10) -1)) ;;   --- "" ---
;;                  (foo-index (fix:lsh (- n 2) -3)) ;; if we had only rectypes 0-3.
;;                  (rectype (fix:lsh (fix:and (- n 2) 7) -1)) ;;  --- "" ---
                   )
;;              (format #t
;;      "The sigamorf ~A. is recursively (rectype ~A) composed from the sigamorf ~A.~%"
;;                      n rectype foo-index
;;              )
                (add-to-siga-vec!
                      n
                      (compose-recursive-cached-siga-fun
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
  ;; Odd n's reserved for ordinarily composed sigamorfs.
  ;; At the left side we use only the primitives SwapBinTree (0), *A072796 (1)
  ;; or any recursively composed sigamorf in even position (2,4,6,8,10,...)
  ;; At the right side we can use any sigamorf we like.
             (let ((foo-index (left-side-ref n))
                   (bar-index (right-side-ref n))
                  )
;;              (format #t
;;    "The sigamorf ~A. is an ordinary composition of the sigamorfs ~A. and ~A.~%"
;;                      n foo-index bar-index
;;              )
                (add-to-siga-vec!
                        n
                        (compose-two-cached-siga-funs
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


(define (obtain-consing-siga-function! n)
    (if (not *SIGA-VEC*) (set! *SIGA-VEC* (vector SwapBinTree *A072796)))
    (obtain-cached-siga-function! n
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

(define (elements-fixed-by sigamorf incl_lowlim excl_uplim)
   (let loop ((fixed 0) (n incl_lowlim))
         (cond ((fix:>= n excl_uplim) fixed)
               ((fix:= (sigamorf n) n) (loop (1+ fixed) (1+ n)))
               (else (loop fixed (1+ n)))
         )
   )
)


;; Note that for sizes 0 & 1 we know that there are only one cycle,
;; one fixed element, the max. cycle size is 1, and the lcm of all cycle sizes is 1.

(define (get-count-list count-fun sigamorf size)
   (let loop ((n 2) (fcs (list 1 1)))
      (cond ((> n size) (reverse! fcs))
            (else (loop (1+ n)
                        (cons (count-fun sigamorf
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


;; Get our local copy of the sigamorf's cache (which we are then free to corrupt...)
(define (initialize-aux-vec-with-funs-values! auxvec sigamorf size)
   (let ((vecsize (vector-length auxvec)))
;; Not this easy, cannot just copy the cache in dirty way:
;;    (subvector-move-left! (car (sigamorf #t)) 0 vecsize auxvec 0)
;; Instead, we have to explicitly copy the values, as we might not
;; have computed yet this sigamorf for the whole range:
     (let copyloop ((n 0))
         (cond ((fix:< n vecsize)
                  (vector-set! auxvec n (sigamorf n))
                  (copyloop (fix:1+ n))
               )
         )
     )
     auxvec
   )
)

(define (get-cc-lists sigamorf size)
   (initialize-aux-vec-with-funs-values! *CC-COUNT-AUX-VEC* sigamorf size)
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



(define (find-matching-gatomorphism-from-list sigamorf Anum-gatomorf-pairs upto-val)
  (let outloop ((alist Anum-gatomorf-pairs))
        (cond ((pair? alist)
                 (let inloop ((n upto-val)) ;; Probably differs in the high end
                    (cond ((fix:< n 2) (car alist)) ;; Found one!
                          ((fix:= ((cdar alist) n) (sigamorf n))
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

(define (form-obtain-fix-siga-function upto-n max-n-caches)
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
    (set! *SIGA-VEC* (vector %swap %exch2first))
    (set! *CACHE-LIST* (list))
    (set! *MAX-N-CACHES* max-n-caches)
    (set! *CC-COUNT-AUX-VEC* (make-vector vec-size))
    (set! *CC-CYCLE-TYPES-VEC* (make-vector (expt 2 12)))
    (lambda (n)
      (obtain-cached-siga-function! n
                                    reserve-indirect-vector
                                    indirect-vector-ref
                                    indirect-vector-set-and-return-value!
                                    vec-size
                                    (lambda (s) (fix:< s 2))
                                    %cons %car %cdr
      )
;;    (obtain-cached-siga-function! n
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
  (let ((ofsf (form-obtain-fix-siga-function size (1+ size))))
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
                  (sigamorf (ofsf y))
                  (val (cond ((zero? what) (sigamorf x))
                             (else
                               (call-with-values
                                  (lambda () (get-cc-lists sigamorf size))
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


(define (list-siga-forms size siga-upto-n list-upto-n search-upto-n max-n-caches only-EIS-ones?)
   (pre-compute-gatomorphisms-on-list *HAND-CODED-GATOMORPHISMS* (-1+ (A014137 (min size 8))))
   (let ((ofsf (form-obtain-fix-siga-function size max-n-caches))
         (first-of-N (iota0 (-1+ list-upto-n)))
        )
     (let loop ((n 0))
       (let* ((sigamorf (ofsf n))
              (already-in-EIS? (find-matching-gatomorphism-from-list sigamorf
                                   *HAND-CODED-GATOMORPHISMS* (-1+ (A014137 (min size 8)))
                               )
              )
             )

             (cond (already-in-EIS?
                    (format #t "Seems that <A HREF=\"http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=A0~A\">A0~A</A> = row ~A " 
                            (car already-in-EIS?) (car already-in-EIS?) n
                    )
                    (cond (only-EIS-ones? ;; Trust me, it will never exhaust completely!
                            (set! *HAND-CODED-GATOMORPHISMS*
                                  (delete! already-in-EIS? *HAND-CODED-GATOMORPHISMS*)
                            )
                            (format #t
                               "~%<BR>The length of *HAND-CODED-GATOMORPHISMS* now: ~A<BR>~%"
                                    (length *HAND-CODED-GATOMORPHISMS*)
                            )
                          )
                    )
                   )
                   ((and only-EIS-ones? (< n siga-upto-n))
                      (loop (1+ n))
                   )
             )

             (output-siga-link "Perm" n (map sigamorf first-of-N)
                               list-upto-n search-upto-n
             )

;;           (output-siga-link "FIX" n (get-count-list elements-fixed-by sigamorf size)
;;                             list-upto-n search-upto-n
;;           )

             (call-with-values
                 (lambda () (get-cc-lists sigamorf size))
                 (lambda (ccs fcs mcs lcs)
                    (output-siga-link "CC"  n ccs list-upto-n 7)
                    (output-siga-link "FIX" n fcs list-upto-n 7)
                    (output-siga-link "MAX" n mcs list-upto-n 7)
                    (output-siga-link "LCM" n lcs list-upto-n 7)
                 )
             )

             (cond ((< n siga-upto-n)
                      (write-string "\n<P><BR></P>\n")
                      (loop (1+ n))
                   )
             )
       )
     )
   )
)


;; Call for example as (output-siga-html "siga8192.htm" 12 8192 120 33 11 #f)

(define (output-siga-html filename
                          size ;; Should be 12
                          siga-upto-n ;; upto how many sequences?
                          list-upto-n ;; Should be 120
                          search-upto-n ;; should be 33 or such
                          max-n-caches ;; 10 or such.
                          only-EIS-ones?
        )
   (with-output-to-file filename
     (lambda ()
        (format #t "<HTML><HEAD><TITLE>Siga sequences 0 - ~A" siga-upto-n)
        (write-string "</TITLE></HEAD><BODY BGCOLOR=\"white\" TEXT=\"blue\" LINKS=\"red\" VLINKS=\"red\">\n")
        (list-siga-forms size siga-upto-n list-upto-n search-upto-n max-n-caches only-EIS-ones?)
        (write-string "</BODY></HTML>")
     )
   )
)

;; Quite ugly, disposal code:
(define (output-siga-link strid siganum seq upto_n search_upto_n)
   (let ((search_upto_n (min search_upto_n (length (cdr seq))))
         (upto_n (min upto_n (length seq)))
        )
      (format #t " <BR><B>~A~A</B> := " strid siganum)
      (write-string "<A HREF=\"http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eishis.cgi?sequence=")
      (output_seq (list-head (cdr seq) search_upto_n))
      (write-string "\">[")
      (output_seq (list-head seq upto_n))
      (write-string "];</A> ");
   )
)



