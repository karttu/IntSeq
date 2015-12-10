
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;;  http://www.iki.fi/~kartturi/matikka/Nekomorphisms/gatomorf.scm      ;;
;;  - "Hand-coded" functions for "Catalan Auomorphisms",                ;;
;;    first the consing, non-destructive versions,                      ;;
;;    and then the destructive versions composed of the basic           ;;
;;    nonrecursive automorphisms in A089840, and then                   ;;
;;    various recursive derivations of these, plus                      ;;
;;    various miscellaneous automorphisms.                              ;;
;;                                                                      ;;
;; "Catalan automorphisms" were previously known as "Gatomorphisms"     ;;
;; ("Catamorphism" is already reserved by Constructive Algorithmics)    ;;
;;  But... an interesting result is that ...                            ;;
;;   every Catalan automorphism indeed is a Catamorphism!               ;;
;;  This is based on the following paper:                               ;;
;;  Jeremy Gibbons, Graham Hutton and Thorsten Altenkirch,              ;;
;;  "When is a function a fold or an unfold?",                          ;;
;;  Electronic Notes in Theoretical Computer Science, 44 (2001), no. 1. ;;
;;  (i.e. every one can be presented as say KROF or ENIPS of some other ;;
;;   Catalan bijection.)                                                ;;
;;                                                                      ;;
;;  Actually, "Catalan bijections" or "binary tree bijections" might    ;;
;;  be better name, as the more arbitrary the Scheme-code gets, the     ;;
;;  harder it is to see which kind of "structure"'s automorphism it is. ;;
;;                                                                      ;;
;;  Note: I'm writing a paper about this subject.                       ;;
;;                                                                      ;;
;;  This Scheme-code is coded 2002-2007 by Antti Karttunen,             ;;
;;  (E-mail: my_firstname.my_surname@gmail.com) and is placed in        ;;
;;  Public Domain.                                                      ;;
;;  Last edited Jul 3 2011.                                             ;;
;;                                                                      ;;
;;  All the examples run at least in MIT Scheme Release 7.6.0, for      ;;
;;  which one can find documentation and the pre-compiled binaries      ;;
;;  (for various OS's running in Intel x86 architecture) under the URL: ;;
;;                                                                      ;;
;;  http://www.swiss.ai.mit.edu/projects/scheme/                        ;;
;;                                                                      ;;
;;  The main pointer for this code collection is:                       ;;
;;  http://www.iki.fi/~kartturi/matikka/Nekomorphisms/gatomorf.htm      ;;
;;                                                                      ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;;
;; Catalan Automorphism (noun) = any bijection from a set of             ;;
;; parenthesizations  of size n to the same set (of size n),             ;;
;; which is well-defined for                                             ;;
;; all the sizes n (for sizes n=0 and 1 we have an identity mapping).    ;;
;; In place of parenthesizations we can assume any other manifestation   ;;
;; of the exercise 19 by Stanley.                                        ;;
;;                                                                       ;;
;; See R. P. Stanley, Exercises on Catalan and Related Numbers,          ;;
;; located at: http://www-math.mit.edu/~rstan/ec/catalan.pdf             ;;
;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; WARNING, WARNING, WARNING!
;; We have to make sure that all destructive implementations (whose name ends with !)
;; are PROPERLY side-effective, i.e. that their argument is guaranteed
;; to be structurally modified to the intended shape! It is not
;; enought that just the result returned is! (In case any of the recursive
;; transformations !FORK, !SPINE, !DEEPEN, etc. are used.)
;;

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
;; (cf "../Schemuli/definech" "../Schemuli/")
;; 
;; (load  "../Schemuli/definech")
;; ;Loading "matikka\\schemuli\\definech.com" -- done
;; ;Value: definec
;; 
;; (fluid-let ((sf/default-syntax-table user-initial-environment))
;;   (cf "gatomorf")
;; )
;; 
;;

;; Keep this list UP-TO-DATE! Last updated June 11, 2007,
;; containing 435 members.
;; + 33 + 30. = 498 in June 20, 2007.

;; These two functions will help in that task:
(define (pr-catfun1-line n) (format #t "(define ~a (catfun1 *~a!))\n" (Anum->str n) (Anum->str n)))
(define (pr-inverse-line n) (format #t "     (~a ~a)\n" n (- n (- 1 (* 2 (modulo n 2))))))

;; Call e.g. as:
;; (for-each (lambda (n) (pr-inverse-line (+ n 130381))) (iota0 17))
;; (for-each (lambda (n) (pr-catfun1-line (+ n 130381))) (iota0 17))

(define inverse_Anums
  '(
     (001477 001477)
     (057117 057118)
     (057118 057117)
     (057161 057162)
     (057162 057161)
     (057163 057163)
     (057164 057164)
     (057501 057502)
     (057502 057501)
     (057503 057504)
     (057504 057503)
     (057505 057506)
     (057506 057505)
     (057508 057508)
     (057509 057510)
     (057510 057509)
     (057511 057512)
     (057512 057511)
     (069767 069768)
     (069768 069767)
     (069769 069769)
     (069770 069770)
     (069771 069771)
     (069772 069772)
     (069773 069774)
     (069774 069773)
     (069775 069776)
     (069776 069775)
     (069787 069787)
     (069888 069888)
     (069889 069889)
     (071655 071656)
     (071656 071655)
     (071657 071658)
     (071658 071657)
     (071659 071660)
     (071660 071659)
     (071661 071662)
     (071662 071661)
     (071663 071664)
     (071664 071663)
     (071665 071666)
     (071666 071665)
     (071667 071668)
     (071668 071667)
     (071669 071670)
     (071670 071669)
     (072088 072089)
     (072089 072088)
     (072090 072091)
     (072091 072090)
     (072092 072093)
     (072093 072092)
     (072094 072095)
     (072095 072094)
     (072796 072796)
     (072797 072797)
     (073194 073195)
     (073195 073194)
     (073196 073197)
     (073197 073196)
     (073198 073199)
     (073199 073198)
     (073205 073206)
     (073206 073205)
     (073207 073208)
     (073208 073207)
     (073209 073210)
     (073210 073209)
     (073269 073270)
     (073270 073269)
     (073280 073280)
     (073281 073281)
     (073282 073283)
     (073283 073282)
     (073284 073285)
     (073285 073284)
     (073286 073287)
     (073287 073286)
     (073288 073289)
     (073289 073288)
     (073290 073291)
     (073291 073290)
     (073292 073293)
     (073293 073292)
     (073294 073295)
     (073295 073294)
     (073296 073297)
     (073297 073296)
     (073298 073299)
     (073299 073298)
     (074679 074680)
     (074680 074679)
     (074681 074682)
     (074682 074681)
     (074683 074684)
     (074684 074683)
     (074685 074686)
     (074686 074685)
     (074687 074688)
     (074688 074687)
     (074689 074690)
     (074690 074689)
     (082313 082313)
     (082314 082314)
     (082315 082316)
     (082316 082315)
     (082317 082318)
     (082318 082317)
     (082319 082320)
     (082320 082319)
     (082321 082322)
     (082322 082321)
     (082323 082324)
     (082324 082323)
     (082325 082326)
     (082326 082325)
     (082331 082332)
     (082332 082331)
     (082333 082334)
     (082334 082333)
     (082335 082336)
     (082336 082335)
     (082339 082340)
     (082340 082339)
     (082341 082342)
     (082342 082341)
     (082345 082346)
     (082346 082345)
     (082347 082348)
     (082348 082347)
     (082349 082350)
     (082350 082349)
     (082351 082352)
     (082352 082351)
     (082353 082354)
     (082354 082353)
     (082355 082356)
     (082356 082355)
     (082357 082358)
     (082358 082357)
     (082359 082360)
     (082360 082359)
     (085159 085160)
     (085160 085159)
     (085161 085161)
     (085162 085162)
     (085163 085164)
     (085164 085163)
     (085165 085166)
     (085166 085165)
     (085167 085168)
     (085168 085167)
     (085169 085170)
     (085170 085169)
     (085171 085172)
     (085172 085171)
     (085173 085174)
     (085174 085173)
     (085175 085175)
     (086425 086426)
     (086426 086425)
     (086427 086428)
     (086428 086427)
     (086429 086430)
     (086430 086429)
     (086431 086431)

     (089850 089850)
     (089851 089853)
     (089852 089852)
     (089853 089851)
     (089854 089854)
     (089855 089857)
     (089856 089856)
     (089857 089855)
     (089858 089861)
     (089859 089863)
     (089860 089862)
     (089861 089858)
     (089862 089860)
     (089863 089859)
     (089864 089864)
     (089865 089866)
     (089866 089865)
     (089867 089868)
     (089868 089867)
     (089869 089870)
     (089870 089869)
     (120705 120706)
     (120706 120705)
     (122282 122282)
     (122291 122292)
     (122292 122291)
     (122293 122294)
     (122294 122293)
     (122295 122296)
     (122296 122295)
     (122297 122298)
     (122298 122297)
     (122300 122300)
     (122301 122302)
     (122302 122301)
     (122303 122304)
     (122304 122303)
     (122305 122306)
     (122306 122305)
     (122307 122308)
     (122308 122307)
     (122309 122310)
     (122310 122309)
     (122311 122312)
     (122312 122311)
     (122313 122314)
     (122314 122313)
     (122315 122316)
     (122316 122315)
     (122317 122318)
     (122318 122317)
     (122319 122320)
     (122320 122319)
     (122321 122322)
     (122322 122321)
     (122323 122324)
     (122324 122323)
     (122325 122326)
     (122326 122325)
     (122327 122328)
     (122328 122327)
     (122329 122330)
     (122330 122329)
     (122331 122332)
     (122332 122331)
     (122333 122334)
     (122334 122333)
     (122335 122336)
     (122336 122335)
     (122337 122338)
     (122338 122337)
     (122339 122340)
     (122340 122339)
     (122341 122342)
     (122342 122341)
     (122343 122344)
     (122344 122343)
     (122345 122346)
     (122346 122345)
     (122347 122348)
     (122348 122347)
     (122349 122350)
     (122350 122349)
     (122351 122351)
     (122353 122354)
     (122354 122353)
     (122355 122356)
     (122356 122355)
     (122357 122358)
     (122358 122357)
     (122359 122360)
     (122360 122359)
     (122361 122362)
     (122362 122361)
     (122363 122364)
     (122364 122363)
     (123492 123492)
     (123493 123494)
     (123494 123493)
     (123495 123496)
     (123496 123495)
     (123497 123498)
     (123498 123497)
     (123499 123500)
     (123500 123499)
     (123501 123502)
     (123502 123501)
     (123503 123503)
     (123695 123696)
     (123696 123695)
     (123713 123714)
     (123714 123713)
     (123715 123716)
     (123716 123715)
     (123717 123718)
     (123718 123717)
     (123719 123719)
     (125976 125976)
     (125977 125978)
     (125978 125977)
     (125979 125979)
     (125980 125980)
     (125981 125982)
     (125982 125981)
     (125983 125984)
     (125984 125983)
     (125985 125986)
     (125986 125985)
     (125987 125988)
     (125988 125987)
     (126290 126290)
     (126313 126314)
     (126314 126313)
     (126315 126316)
     (126316 126315)
     (126320 126320)
     (127285 127286)
     (127286 127285)
     (127287 127288)
     (127288 127287)
     (127289 127290)
     (127290 127289)
     (127291 127292)
     (127292 127291)
     (127299 127300)
     (127300 127299)
     (127377 127378)
     (127378 127377)
     (127379 127380)
     (127380 127379)
     (127381 127382)
     (127382 127381)
     (127387 127387)
     (127388 127388)
     (129604 129604)
     (129605 129606)
     (129606 129605)
     (129607 129607)
     (129608 129608)
     (129609 129610)
     (129610 129609)
     (129611 129612)
     (129612 129611)
     (130339 130339)
     (130340 130340)
     (130341 130342)
     (130342 130341)
     (130343 130344)
     (130344 130343)
     (130345 130346)
     (130346 130345)
     (130347 130348)
     (130348 130347)
     (130349 130350)
     (130350 130349)
     (130351 130352)
     (130352 130351)
     (130353 130354)
     (130354 130353)
     (130355 130356)
     (130356 130355)
     (130357 130358)
     (130358 130357)
     (130359 130360)
     (130360 130359)
     (130361 130362)
     (130362 130361)
     (130363 130364)
     (130364 130363)
     (130365 130366)
     (130366 130365)
     (130367 130368)
     (130368 130367)
     (130369 130370)
     (130370 130369)
     (130371 130372)
     (130372 130371)
     (130373 130373)
     (130374 130374)
     (130375 130376)
     (130376 130375)

     (130918 130918)

     (130919 130920)
     (130920 130919)

     (130921 130922)
     (130922 130921)

     (130923 130924)
     (130924 130923)

     (130925 130926)
     (130926 130925)

     (130381 130382)
     (130382 130381)
     (130383 130384)
     (130384 130383)
     (130385 130386)
     (130386 130385)
     (130387 130388)
     (130388 130387)
     (130389 130390)
     (130390 130389)
     (130391 130392)
     (130392 130391)
     (130393 130394)
     (130394 130393)
     (130395 130396)
     (130396 130395)
     (130397 130398)
     (130398 130397)

     (130927 130928)
     (130928 130927)
     (130929 130930)
     (130930 130929)
     (130931 130932)
     (130932 130931)
     (130933 130934)
     (130934 130933)
     (130935 130936)
     (130936 130935)
     (130937 130938)
     (130938 130937)
     (130939 130940)
     (130940 130939)
     (130941 130942)
     (130942 130941)
     (130943 130944)
     (130944 130943)
     (130945 130946)
     (130946 130945)
     (130947 130948)
     (130948 130947)
     (130949 130950)
     (130950 130949)
     (130951 130952)
     (130952 130951)
     (130953 130954)
     (130954 130953)
     (130955 130956)
     (130956 130955)
     (130957 130958)
     (130958 130957)
     (130959 130960)
     (130960 130959)
     (130961 130962)
     (130962 130961)
     (130963 130964)
     (130964 130963)
     (130965 130966)
     (130966 130965)

     (131141 131142)
     (131142 131141)
     (131143 131144)
     (131144 131143)
     (131145 131146)
     (131146 131145)
     (131147 131148)
     (131148 131147)
     (131149 131150)
     (131150 131149)
     (131151 131152)
     (131152 131151)
     (131153 131154)
     (131154 131153)
     (131155 131156)
     (131156 131155)
     (131157 131158)
     (131158 131157)
     (131159 131160)
     (131160 131159)
     (131161 131162)
     (131162 131161)
     (131163 131164)
     (131164 131163)
     (131165 131166)
     (131166 131165)
     (131167 131168)
     (131168 131167)
     (131169 131170)
     (131170 131169)
     (131171 131172)
     (131172 131171)
     (131173 131173)

     (130981 130981)
     (130982 130982)
     (130983 130984)
     (130984 130983)
     (130985 130986)
     (130986 130985)
     (130987 130988)
     (130988 130987)
     (130989 130990)
     (130990 130989)
     (130991 130992)
     (130992 130991)
     (130993 130994)
     (130994 130993)
     (130995 130996)
     (130996 130995)
     (130997 130998)
     (130998 130997)
     (130999 131000)
     (131000 130999)
     (131001 131002)
     (131002 131001)
     (131003 131004)
     (131004 131003)
     (131005 131006)
     (131006 131005)
     (131007 131008)
     (131008 131007)
     (131009 131010)
     (131010 131009)

     (154121 154122)
     (154122 154121)

     (154123 154124)
     (154124 154123)

     (154125 154125)
     (154126 154126)

     (154449 154450)
     (154450 154449)
     (154451 154452)
     (154452 154451)
     (154453 154454)
     (154454 154453)
     (154455 154456)
     (154456 154455)

     (154457 154458)
     (154458 154457)

  )
)

(definec (find-invAnum Anum) ;; Cached, so reasonably fast.
   (cond ((find-matching-item inverse_Anums (lambda (s) (eq? (car s) Anum)))
           => (lambda (s) (second s))
         )
         (else #f)
   )
)

(definec (Anum->Afun-checked Anum)
  (let ((Afun (string->symbol (string-downcase (Anum->str Anum)))))
    (and (environment-bound? user-initial-environment Afun)
         (eval Afun user-initial-environment)
    )
  )
)

(define (find-matching-anum fun-searched upto-n)
   (cond ((find-matching-item inverse_Anums
             (lambda (e)
                (let* ((Anum (first e))
                       (Afun (Anum->Afun-checked Anum))
                      )
                   (cond ((integer? (same-intfuns? Afun fun-searched upto-n) ) #f)
                         (else #t)
                   )
                )
             )
          )
           => (lambda (s) (Anum->str (first s)))
         )
         (else #f)
   )
)

(define (search-for-all-conjugates fun-conjugated upto-n)
  (let ((n 0))
   (for-each
      (lambda (fpair)
         (let* ((Anum (first fpair))
                (invAnum (second fpair)) ;; Slower: (find-invAnum Anum)
                (Afun (Anum->Afun-checked Anum))
                (invAfun (Anum->Afun-checked invAnum))
                (conj1 (find-matching-anum
                            (compose-funs Afun fun-conjugated invAfun) upto-n
                       )
                )
               )
             (begin
                (format #t " ~a" n)
                (set! n (1+ n))
                (cond (conj1
                          (format #t "\n(~a f ~a) = ~a?\n" Anum invAnum conj1)
                          (flush-output (current-output-port))
                      )
                )
             )
          )
      )
      inverse_Anums
   )
  )
)


(define (check-inverse-Anums upto-n)
 (let ((involutions 0))
   (for-each (lambda (e)
                (let* ((Anum (first e))
                       (invAnum (find-invAnum Anum))
                       (Afun (Anum->Afun-checked Anum))
                       (invAfun (Anum->Afun-checked invAnum))
                      )
                   (cond ((eq? Anum invAnum) (set! involutions (+ 1 involutions)))
                         ((not (eq? (find-invAnum invAnum) Anum))
                            (format #t "**Error: ~a: (find-invAnum ~a)=~a\n" 
                                       e invAnum (find-invAnum invAnum)
                            )
                         )
                   )
                   (cond ((not Afun)
                            (format #t "**Error: No ~a defined!\n"
                                       (string->symbol (string-downcase (Anum->str Anum)))
                            )
                         )
                         ((not invAfun)
                            (format #t "**Error: No ~a defined!\n"
                                       (string->symbol (string-downcase (Anum->str invAnum)))
                            )
                         )
                         ((and (> upto-n 0)
                               (same-intfuns? (compose-funs Afun invAfun) A001477 upto-n)
                          )
                            => (lambda (true_or_num)
                                  (if (integer? true_or_num)
                                      (format #t "**Error: ~a is not the inverse of ~a !\n"
                                          (string->symbol (string-downcase (Anum->str Anum)))
                                          (string->symbol (string-downcase (Anum->str invAnum)))
                                      )
                                  )
                               )
                         )
                   )
                )
             )
             inverse_Anums
   )
   involutions
 )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In this section no automorphisms, but
;; instead some "metric functions" from the set of
;; S-expressions to the a set of integers:
;; (Where should these be located, another source-file
;;  of their own?)

(define (*Anumber_of_oddlevel_leaves s) ;; Something likes this.
   (cond ((null? s) 0)
         (else (apply + (map *Anumber_of_evenlevel_leaves s)))
   )
)

(define (*Anumber_of_evenlevel_leaves s)
   (cond ((null? s) 1)
         (else (apply + (map *Anumber_of_oddlevel_leaves s)))
   )
)

(define (*A126303 s) ;; Number of odd-level nodes.
   (cond ((null? s) 0)
         (else (fold-left (lambda (x y) (+ x 1 (*A126304 y))) 0 s))
   )
)

(define (*A126304 s) ;; Number of even-level nodes excluding the root.
   (cond ((null? s) 0)
         (else (apply + (map *A126303 s)))
   )
)



(define (*A179751 s) ;; Was: MaxBTDepth
   (cond ((not (pair? s)) 0)
         (else (1+ (max (*A179751 (car s)) (*A179751 (cdr s)))))
   )
)


(define (*A179752 s) ;; Was: MaxGTDepth
   (cond ((not (pair? s)) 0)
         (else (max (1+ (*A179752 (car s))) (*A179752 (cdr s))))
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ENIPS foo) (lambda (s) (fold-right (lambda (x y) (foo (cons x y))) '() s)))

(define (KROF foo) (letrec ((bar (lambda (s) (fold-right (lambda (x y) (foo (cons (bar x) y))) '() s)))) bar))

(define (FORK foo)
  (letrec ((bar (lambda (s)
                  (let ((t (foo s)))
                    (if (pair? t)
                        (cons (bar (car t)) (bar (cdr t)))
                        t
                    )
                  )
                )
          ))
     bar
  )
)

(define (!FORK foo!)
  (letrec ((bar! (lambda (s)
                    (cond ((pair? s)
                            (foo! s)
                            (bar! (car s))
                            (bar! (cdr s))
                          )
                    )
                    s
                 )
          ))
     bar!
  )
)


(define (!KROF foo!)
  (letrec ((bar! (lambda (s)
                    (cond ((pair? s)
                            (bar! (car s))
                            (bar! (cdr s))
                            (foo! s)
                          )
                    )
                    s
                 )
          ))
     bar!
  )
)


(define (SPINE foo)
  (letrec ((bar (lambda (s)
                  (let ((t (foo s)))
                    (if (pair? t)
                        (cons (car t) (bar (cdr t)))
                        t
                    )
                  )
                )
          ))
     bar
  )
)

(define (!SPINE foo!)
  (letrec ((bar! (lambda (s)
                    (cond ((pair? s)
                            (foo! s)
                            (bar! (cdr s))
                          )
                    )
                    s
                 )
          ))
     bar!
  )
)


(define (!ENIPS foo!)
  (letrec ((bar! (lambda (s)
                    (cond ((pair? s)
                            (bar! (cdr s))
                            (foo! s)
                          )
                    )
                    s
                 )
          ))
     bar!
  )
)

(define (RIBS foo) (lambda (s) (map foo s)))


(define (!RIBSv2 foo!)
  (letrec ((bar! (lambda (s)
                    (cond ((pair? s)
                            (foo! (car s))
                            (bar! (cdr s))
                          )
                    )
                    s
                 )
          ))
     bar!
  )
)

;; (define (!RIBS foo!) (letrec ((bar! (lambda (s) (for-each foo! s)))) bar!))
(define (!RIBS foo!) (lambda (s) (for-each foo! s) s))

(define (DEEPEN foo) (letrec ((bar (lambda (s) (map bar (foo s))))) bar))


;; This form needed for one proof in my paper:
(define (DEEPENv2 f)
 (letrec ((g (lambda (s)
                (cond ((null? s) s)
                      (else (let ((t (f s)))
                               (cons (g (car t)) (map g (cdr t)))
                            )
                      )
                )
             )
         ))
     g
 )
)


(define (!DEEPEN foo!) (letrec ((bar! (lambda (s) (foo! s) (for-each bar! s) s))) bar!))

(define (!DEEPENorg foo!)
  (letrec ((bar! (lambda (s)
                    (cond ((pair? s)
                            (foo! s)
                            (for-each bar! s)
                          )
                    )
                    s
                 )
          ))
     bar!
  )
)

(define (!NEPEED foo!) (letrec ((bar! (lambda (s) (for-each bar! s) (foo! s) s))) bar!))

(define (NEPEED foo) (letrec ((bar (lambda (s) (foo (map bar s))))) bar))

(define (!NEPEED_org foo!)
  (letrec ((bar! (lambda (s)
                    (cond ((pair? s)
                            (for-each bar! s)
                            (foo! s)
                          )
                    )
                    s
                 )
          ))
     bar!
  )
)


;; "New" recursion schemes added June 9, 2007, for inorder depth-first traversal of binary trees:


;; This is the recursion type 4 in A073200.
(define (INORDER f)
  (letrec ((g (lambda (s)
                (cond ((not (pair? s)) s)
                      (else (let ((t (f (cons (g (car s)) (cdr s)))))
                              (cons (car t) (g (cdr t)))
                            )
                      )
                )
              )
          ))
     g
  )
)

(define (!INORDER f!)
  (letrec ((g! (lambda (s)
                    (cond ((pair? s)
                            (g! (car s))
                            (f! s)
                            (g! (cdr s))
                          )
                    )
                    s
                 )
          ))
     g!
  )
)

;; Same reversed:
(define (REDRONI f)
   (letrec ((g (lambda (s)
                  (fold-right (lambda (x y) (let ((t (f (cons x y)))) (cons (g (car t)) (cdr t))))
                              '()
                              s
                  )
               )
           ))
           g
   )
)


(define (!REDRONI f!)
  (letrec ((g! (lambda (s)
                    (cond ((pair? s)
                            (g! (cdr s))
                            (f! s)
                            (g! (car s))
                          )
                    )
                    s
                 )
          ))
     g!
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Experimental "recursion schemes":

(define (!APPLY-AT-FIRST-NIL f!)
  (letrec ((g! (lambda (s)
                    (cond ((pair? s)
                             (if (pair? (car s))
                                 (g! (cdr s))
                                 (f! s)
                             )
                          )
                    )
                    s
               )
          ))
     g!
  )
)

(define (!APPLY-AFTER-FIRST-NIL f!)
  (letrec ((g! (lambda (s)
                    (cond ((pair? s)
                             (if (pair? (car s))
                                 (g! (cdr s)) ;; If no nil, then cont. cdr-wise
                                 (f! (cdr s)) ;; if nil, then apply f here.
                             )
                          )
                    )
                    s
               )
          ))
     g!
  )
)



(define (WRAPPER-APPLY-ONLY-IF-UNIQ-NIL-AT-CAR f)
  (lambda (s)
      (cond ((and (null? (car s)) (not (memq? '() (cdr s))))
               (cons (car s) (f (cdr s)))
            )
            (else s)
      )
  )
)

;; (APPLY-AFTER-LAST-NIL f)=(ENIPS (WRAPPER-APPLY-ONLY-IF-UNIQ-NIL-AT-CAR f))

(define (APPLY-AFTER-LAST-NIL f)
  (lambda (s)
    (fold-right
       (lambda (x y)
           (cond ((and (null? x) (not (memq? '() y)))
                    (cons x (f y))
                 )
                 (else (cons x y))
           )
       )
       '()
       s
    )
  )
)


;; We could also define the constructive version of APPLY-AFTER-FIRST-NIL
;; with fold-right:
;; (We have to undo the previous (f y) with f^{-1} when the next nil at
;; the lower level is detected, and then do (f y) after _this_ nil.):
;; We need a function like (INV f), which returns inverse of f.

;; Note that there is no need to test for the presence of previous ():

;; (define (APPLY-AFTER-FIRST-NIL f)
;;  (let ((app-inv-f-after-first-nil (APPLY-AFTER-FIRST-NIL (INV f))))
;;   (lambda (s)
;;     (fold-right
;;        (lambda (x y)
;;            (cond ((null? x) ;; NIL encountered at car.
;;                     (cons x (f (app-inv-f-after-first-nil y)))
;;                  )
;;                  (else (cons x y))
;;            )
;;        )
;;        '()
;;        s
;;     )
;;   )
;;  )
;; )


;; (define (WRAPPER-APPLY-ONLY-ONCE-AFTER-CAR-NIL f)
;;   (let ((f_only_here
;;           (compose-funs
;;                  f
;;                  (app-inv-f-after-first-nil (APPLY-AFTER-FIRST-NIL (INV f)))
;;           )
;;        ))
;;     (lambda (s)
;;         (cond ((null? (car s)) (cons (car s) (f_only_here (cdr s))))
;;               (else s) ;; If car-side is non-nil, then act as identity.
;;         )
;;     )
;;   )
;; )

;; (APPLY-AFTER-FIRST-NIL f)=(ENIPS (WRAPPER-APPLY-ONLY-ONCE-AFTER-CAR-NIL f))


;;
;; A wacky idea: ()'s as <>'s, i.e. "Nils as Diamonds"?
;; E.g. Apply some automorphism, but only between the first
;; and second nil, if the left and right sides satisfy some condition.
;; (apply how? Like with "partial" RIBS?)
;; Any application to TAG's, etc.?
;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,

(define (F_CAR f) (lambda (s) (cons (f (car s)) (cdr s))))
(define (F_CDR f) (lambda (s) (cons (car s) (f (cdr s)))))

(define (!F_CAR f!) (lambda (s) (if (pair? s) (f! (car s))) s))
(define (!F_CDR f!) (lambda (s) (if (pair? s) (f! (cdr s))) s))


(define (KROFINV f)
   (let ((f-inv (INV f)))
      (lambda (s) (if (null? s) s (f (cons (f-inv (car s)) (f-inv (cdr s))))))
   )
)

(define (!KROFINV f!)
   (let ((f-inv! (!INV f!)))
      (lambda (s)
         (cond ((pair? s)
                   (f-inv! (car s))
                   (f-inv! (cdr s))
                   (f! s)
               )
         )
         s
      )
   )
)


(define (ENIPSINV f)
   (let ((f-inv (INV f)))
      (lambda (s) (if (null? s) s (f (cons (car s) (f-inv (cdr s))))))
   )
)


(define (!ENIPSINV f!)
   (let ((f-inv! (!INV f!)))
      (lambda (s)
         (cond ((pair? s)
                   (f-inv! (cdr s))
                   (f! s)
               )
         )
         s
      )
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,


(define-syntax swap2!
  (syntax-rules ()
   ((swap2! x y)
      (let ((old-y y)) (set! y x) (set! x old-y))
   )
  )
)

;; A089840[1], (a . b) --> (b . a)
(define (*A069770 s) (if (pair? s) (cons (cdr s) (car s)) s)) ;; Previously known as SwapBinTree

;; -> (0 1 2 3 4 6 5 7 8 9 10 14 16 19 11 15 12 17 18 13 20 21 22 23 24 25 26 27 37 ...)
(define (*A072796 s) ;; A089840[2], (a . (b . c)) --> (b . (a . c))
  (if (and (pair? s) (pair? (cdr s))) (cons (cadr s) (cons (car s) (cddr s))) s)
)

(define (*A089850 s) ;; A089840[3], (a . (b . c)) --> (a . (c . b))
  (if (and (pair? s) (pair? (cdr s))) (cons (car s) (cons (cddr s) (cadr s))) s)
)

(define (*A089851 s) ;; A089840[4], (a . (b . c)) --> (b . (c . a))
  (if (and (pair? s) (pair? (cdr s))) (cons (cadr s) (cons (cddr s) (car s))) s)
)

(define (*A089852 s) ;; A089840[5], (a . (b . c)) --> (c . (b . a))
  (if (and (pair? s) (pair? (cdr s))) (cons (cddr s) (cons (cadr s) (car s))) s)
)

(define (*A089853 s) ;; A089840[6], (a . (b . c)) --> (c . (a . b))
  (if (and (pair? s) (pair? (cdr s))) (cons (cddr s) (cons (car s) (cadr s))) s)
)

(define (*A089854 s) ;; A089840[7], ((a . b) . c)) --> ((b . a) . c)
  (if (and (pair? s) (pair? (car s))) (cons (cons (cdar s) (caar s)) (cdr s)) s)
)

(define (*A072797 s) ;; A089840[8], ((a . b) . c) --> ((a . c) . b)
  (if (and (pair? s) (pair? (car s))) (cons (cons (caar s) (cdr s)) (cdar s)) s)
)

(define (*A089855 s) ;; A089840[9], ((a . b) . c)) --> ((b . c) . a)
  (if (and (pair? s) (pair? (car s))) (cons (cons (cdar s) (cdr s)) (caar s)) s)
)

(define (*A089856 s) ;; A089840[10], ((a . b) . c)) --> ((c . b) . a)
  (if (and (pair? s) (pair? (car s))) (cons (cons (cdr s) (cdar s)) (caar s)) s)
)

(define (*A089857 s) ;; A089840[11], ((a . b) . c)) --> ((c . a) . b)
  (if (and (pair? s) (pair? (car s))) (cons (cons (cdr s) (caar s)) (cdar s)) s)
)


(define (*A074679 s) ;; A089840[12], (a . (b . c)) --> ((a . b) . c), (a . ()) --> (() . a)
  (cond ((not (pair? s)) s)
        ((not (pair? (cdr s))) (cons (cdr s) (car s))) ;; i.e. (*A069770 s)
        (else (cons (cons (car s) (cadr s)) (cddr s)))
  )
)

(define (*A089858 s) ;; A089840[13], (a . (b . c)) --> ((b . a) . c), (a . ()) --> (() . a)
  (cond ((not (pair? s)) s)
        ((not (pair? (cdr s))) (cons (cdr s) (car s))) ;; i.e. (*A069770 s)
        (else (cons (cons (cadr s) (car s)) (cddr s)))
  )
)

(define (*A073269 s) ;; A089840[14], (a . (b . c)) --> ((a . c) . b), (a . ()) --> (() . a)
  (cond ((not (pair? s)) s)
        ((not (pair? (cdr s))) (cons (cdr s) (car s))) ;; i.e. (*A069770 s)
        (else (cons (cons (car s) (cddr s)) (cadr s)))
  )
)

(define (*A089859 s) ;; A089840[15], (a . (b . c)) --> ((c . b) . a), (a . ()) --> (() . a)
  (cond ((not (pair? s)) s)
        ((not (pair? (cdr s))) (cons (cdr s) (car s))) ;; i.e. (*A069770 s)
        (else (cons (cons (cddr s) (cadr s)) (car s)))
  )
)

(define (*A089860 s) ;; A089840[16], (a . (b . c)) --> ((c . a) . b), (a . ()) --> (() . a)
  (cond ((not (pair? s)) s)
        ((not (pair? (cdr s))) (cons (cdr s) (car s))) ;; i.e. (*A069770 s)
        (else (cons (cons (cddr s) (car s)) (cadr s)))
  )
)

(define (*A074680 s) ;; A089840[17], ((a . b) . c) --> (a . (b . c)), (() . b) --> (b . ())
  (cond ((not (pair? s)) s)
        ((not (pair? (car s))) (cons (cdr s) (car s))) ;; i.e. (*A069770 s)
        (else (cons (caar s) (cons (cdar s) (cdr s))))
  )
)

(define (*A089861 s) ;; A089840[18], ((a . b) . c) --> (b . (a . c)), (() . b) --> (b . ())
  (cond ((not (pair? s)) s)
        ((not (pair? (car s))) (cons (cdr s) (car s))) ;; i.e. (*A069770 s)
        (else (cons (cdar s) (cons (caar s) (cdr s))))
  )
)

(define (*A073270 s) ;; A089840[19], ((a . b) . c) --> (a . (c . b)), (() . b) --> (b . ())
  (cond ((not (pair? s)) s)
        ((not (pair? (car s))) (cons (cdr s) (car s))) ;; i.e. (*A069770 s)
        (else (cons (caar s) (cons (cdr s) (cdar s))))
  )
)

(define (*A089862 s) ;; A089840[20], ((a . b) . c) --> (b . (c . a)), (() . b) --> (b . ())
  (cond ((not (pair? s)) s)
        ((not (pair? (car s))) (cons (cdr s) (car s))) ;; i.e. (*A069770 s)
        (else (cons (cdar s) (cons (cdr s) (caar s))))
  )
)

(define (*A089863 s) ;; A089840[21], ((a . b) . c) --> (c . (b . a)), (() . b) --> (b . ())
  (cond ((not (pair? s)) s)
        ((not (pair? (car s))) (cons (cdr s) (car s))) ;; i.e. (*A069770 s)
        (else (cons (cdr s) (cons (cdar s) (caar s))))
  )
)


(define (swap! s)
  (let ((ex-car (car s)))
     (set-car! s (cdr s))
     (set-cdr! s ex-car)
     s
  )
)

;; robl! -- Rotate Binary tree Left. Inverse of robr!
;; Convert (a . (b . rest)) to ((a . b) . rest) destructively
;; (with no cons cells wasted).
;; Like cons2top! but keeps the "point(er) of reference" same:

(define (robl! s)
  (let ((ex-car (car s)))         ;; <- a
      (set-car! s (cddr s))       ;; (a . (b . rest)) -> (rest . (b . rest))
      (set-cdr! (cdr s) ex-car)   ;; -> (rest . (b . a))
      (swap! (cdr s))             ;; -> (rest . (a . b))
      (swap! s)                   ;; -> ((a . b) . rest)
      s
  )
)

;; robr! -- Rotate Binary tree Right. Inverse of robl!
;; Convert ((a . b) . rest) to (a . (b . rest)) destructively
;; (with no cons cells wasted).

(define (robr! s)
  (let ((ex-cdr (cdr s)))         ;; <- rest
      (set-cdr! s (caar s))       ;; ((a . b) . rest) -> ((a . b) . a)
      (set-car! (car s) ex-cdr)   ;; -> ((rest . b) . a)
      (swap! (car s))             ;; -> ((b . rest) . a)
      (swap! s)                   ;; -> (a . (b . rest))
      s
  )
)


;; (permute-a060118 (vector 'a 'b 'c 'd 'e 'f 'g) 3 0)  -->  #(a b c)
;; (permute-a060118 (vector 'a 'b 'c 'd 'e 'f 'g) 3 1)  -->  #(b a c)
;; (permute-a060118 (vector 'a 'b 'c 'd 'e 'f 'g) 3 2)  -->  #(a c b)
;; (permute-a060118 (vector 'a 'b 'c 'd 'e 'f 'g) 3 3)  -->  #(b c a)
;; (permute-a060118 (vector 'a 'b 'c 'd 'e 'f 'g) 3 4)  -->  #(c b a)
;; (permute-a060118 (vector 'a 'b 'c 'd 'e 'f 'g) 3 5)  -->  #(c a b)

;; From gatonore.c:
;;
;; CLAUSE gmA001477[] = { CLAUSESEQ_begin(0,0) };                /* A089840[0] */
;; CLAUSE gmA069770[] = { CLAUSESEQ_begin(1,1), { 1, 0, 0, 1 } };/* A089840[1] */
;; CLAUSE gmA072796[] = { CLAUSESEQ_begin(3,1), { 2, 0, 0, 1 } };/* A089840[2] */
;; CLAUSE gmA089850[] = { CLAUSESEQ_begin(3,1), { 2, 0, 0, 2 } };/* A089840[3] */
;; CLAUSE gmA089851[] = { CLAUSESEQ_begin(3,1), { 2, 0, 0, 3 } };/* A089840[4] */
;; CLAUSE gmA089852[] = { CLAUSESEQ_begin(3,1), { 2, 0, 0, 4 } };/* A089840[5] */
;; CLAUSE gmA089853[] = { CLAUSESEQ_begin(3,1), { 2, 0, 0, 5 } };/* A089840[6] */
;; CLAUSE gmA089854[] = { CLAUSESEQ_begin(3,1), { 2, 1, 1, 1 } };/* A089840[7] */
;; CLAUSE gmA072797[] = { CLAUSESEQ_begin(3,1), { 2, 1, 1, 2 } };/* A089840[8] */
;; CLAUSE gmA089855[] = { CLAUSESEQ_begin(3,1), { 2, 1, 1, 3 } };/* A089840[9] */
;; CLAUSE gmA089856[] = { CLAUSESEQ_begin(3,1), { 2, 1, 1, 4 } };/* A089840[10] */
;; CLAUSE gmA089857[] = { CLAUSESEQ_begin(3,1), { 2, 1, 1, 5 } };/* A089840[11] */
;; CLAUSE gmA074679[] = { CLAUSESEQ_begin(4,2), { 2, 0, 1, 0,}, { 1, 0, 0, 1 } }; /* A089840[12] */
;; CLAUSE gmA089858[] = { CLAUSESEQ_begin(4,2), { 2, 0, 1, 1,}, { 1, 0, 0, 1 } }; /* A089840[13] */
;; CLAUSE gmA073269[] = { CLAUSESEQ_begin(4,2), { 2, 0, 1, 2,}, { 1, 0, 0, 1 } }; /* A089840[14] */
;; CLAUSE gmA089859[] = { CLAUSESEQ_begin(4,2), { 2, 0, 1, 4,}, { 1, 0, 0, 1 } }; /* A089840[15] */
;; CLAUSE gmA089860[] = { CLAUSESEQ_begin(4,2), { 2, 0, 1, 5,}, { 1, 0, 0, 1 } }; /* A089840[16] */
;; CLAUSE gmA074680[] = { CLAUSESEQ_begin(4,2), { 2, 1, 0, 0 }, { 1, 0, 0, 1 } }; /* A089840[17] */
;; CLAUSE gmA089861[] = { CLAUSESEQ_begin(4,2), { 2, 1, 0, 1,}, { 1, 0, 0, 1 } }; /* A089840[18] */
;; CLAUSE gmA073270[] = { CLAUSESEQ_begin(4,2), { 2, 1, 0, 2,}, { 1, 0, 0, 1 } }; /* A089840[19] */
;; CLAUSE gmA089862[] = { CLAUSESEQ_begin(4,2), { 2, 1, 0, 3,}, { 1, 0, 0, 1 } }; /* A089840[20] */
;; CLAUSE gmA089863[] = { CLAUSESEQ_begin(4,2), { 2, 1, 0, 4,}, { 1, 0, 0, 1 } }; /* A089840[21] */


(define (*A069770! s) ;; Name was: SwapBinTree!
  (cond ((not (pair? s)))
        (else
              (swap! s)
        )
  )
  s
)





;; -> (0 1 2 3 4 6 5 7 8 9 10 14 16 19 11 15 12 17 18 13 20 21 22 23 24 25 26 27 37 ...)
(define (*A072796! s)
  (cond ((not (pair? s)) s)
        ((not (pair? (cdr s))) s)
        (else (swap! s)
              (robr! s)
              (swap! (cdr s))
              s
        )
  )
)

;; -> (0 1 2 3 4 5 7 6 8 9 10 11 12 13 17 18 16 14 15 20 19 21 22 23 24 25 26 27 28 ...)
(define (*A072797! s)
  (cond ((not (pair? s)) s)
        ((not (pair? (car s))) s)
        (else (swap! s)
              (robl! s)
              (swap! (car s))
              s
        )
  )
)


(define (*A089850! s) ;; (a . (b . c)) --> (a . (c . b))
  (cond ((not (pair? s)) s)
        ((not (pair? (cdr s))) s)
        (else (swap! (cdr s)) s)
  )
)



;; Simple elementary, non-recursive automorphism A089851:
;;
;;  b  c         c  a
;; a \/   --->  b \/
;;  \/           \/
;;
;; If the tree is not of this form, then fix it.
;;
;; It is very easy to count all the involved sequences:
;;
;;
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
;; Can this be formed with the help of A003441 ???
;; 1,1,3,10,30,99,335,1144,3978,14000,...


(define (*A089851! s) ;; (a . (b . c)) --> (b . (c . a))
  (cond ((not (pair? s)) s)
        ((not (pair? (cdr s))) s)
        (else (swap! s)
              (robr! s)
              s
        )
  )
)


(define (*A089852! s) ;; (a . (b . c)) --> (c . (b . a))
  (cond ((not (pair? s)) s)
        ((not (pair? (cdr s))) s)
        (else
           (let ((org_cddr (cddr s)))
              (set-cdr! (cdr s) (car s))
              (set-car! s org_cddr)
              s
           )
        )
  )
)



(define (*A089853! s) ;; (a . (b . c)) --> (c . (a . b))
  (cond ((not (pair? s)) s)
        ((not (pair? (cdr s))) s)
        (else
           (let ((org_cadr (cadr s)))
              (set-car! (cdr s) (car s))
              (set-car! s (cddr s))
              (set-cdr! (cdr s) org_cadr)
              s
           )
        )
  )
)


(define (*A089854! s) ;; ((a . b) . c) --> ((b . a) . c)
  (cond ((not (pair? s)) s)
        ((not (pair? (car s))) s)
        (else (swap! (car s)) s)
  )
)


(define (*A089855! s) ;; ((a . b) . c) --> ((b . c) . a)
  (cond ((not (pair? s)) s)
        ((not (pair? (car s))) s)
        (else
           (let ((org_cdar (cdar s)))     ;; save orig. b
              (set-cdr! (car s) (cdr s))  ;; c -> b
              (set-cdr! s (caar s))       ;; a -> c
              (set-car! (car s) org_cdar) ;; b -> a
              s
           )
        )
  )
)

(define (*A089856! s) ;; ((a . b) . c) --> ((c . b) . a)
  (cond ((not (pair? s)) s)
        ((not (pair? (car s))) s)
        (else
           (let ((org_caar (caar s)))
              (set-car! (car s) (cdr s))
              (set-cdr! s org_caar)
              s
           )
        )
  )
)

(define (*A089857! s) ;; ((a . b) . c) --> ((c . a) . b)
  (cond ((not (pair? s)) s)
        ((not (pair? (car s))) s)
        (else (swap! s)
              (robl! s)
              s
        )
  )
)

;; CLAUSE gmA074679[] = { CLAUSESEQ_begin(4,2), { 2, 0, 1, 0,}, { 1, 0, 0, 1 } }; /* A089840[12] */

;;  B  C        A  B
;; A \/    -->   \/ C        and  A ()        () A
;;  \/            \/               \/    -->   \/
;;

(define (*A074679! s)
  (cond ((pair? s)
           (cond ((pair? (cdr s)) (robl! s))
                 (else (swap! s))
           )
        )
  )
  s
)


;; Composite versions of the above ones, proving
;; that they occur also in A073200.

(define (*A074679v2! s) ;; was gmRobl!
    (*A072796! s)
    (*A069770! s)
    (*A057163! s)
    (*A072796! s)
    (*A057163! s)
    s
)

;;  B  C        B  A
;; A \/    -->   \/ C        and  A ()        () A
;;  \/            \/               \/    -->   \/
;;

(define (*A089858! s)
  (cond ((pair? s)
           (cond ((pair? (cdr s)) (robl! s) (swap! (car s)))
                 (else (swap! s))
           )
        )
  )
  s
)


;;  B  C        A  C
;; A \/    -->   \/ B        and  A ()        () A
;;  \/            \/               \/    -->   \/
;;

(define (*A073269! s) (*A069770! (*A072796! s))) ;; inv. of *A073270. Row 3 in A073200, row 14 in A089840.

(define (*A073281! s) (*A072796! (*A073269! s))) ;; involution. Row 15.

;; %S A089407 1,1,1,2,4,11,34,109,360,1219,4206,14708,52024,185758,668676,2423821,8839632,32411555,119410390,441817020,1641032536
;; %N A089407 Number of cycles in range [A014137(n-1)..A014138(n-1)] of permutation A089859/A089863.

;; And the number of fixed points in range [A014137(n-1)..A014138(n-1)] of permutation A089859/A089863
;; is:
;;     0 1   3       7       11     15       19
;;     1,1,0,1,0,0,0,1,0,0,0,2,0,0,0,5,0,0,0,14,0

;; In addition to the . and \/, only trees of the form:
;;
;; T  T T  T
;;  \/   \/
;;   \   /
;;    \./
;;
;; are fixed, where T is any tree, thus this is a fourthfold aeration of the Catalan numbers.

;; For two-fold application, in addition to the trees shown above, also
;; trees of the form:
;;                          A  A B  B
;; T  T           T  T       \/   \/
;;  \/ ()   and () \/   and   \   /
;;   \/           \/           \./
;; are fixed.

;; %S A040002 1,1,2,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
;; %N A040002 Maximum cycle size in range [A014137(n-1)..A014138(n-1)] of permutation A089859/A089863.

;;  B  C        C  B
;; A \/    -->   \/ A        and  A ()        () A
;;  \/            \/               \/    -->   \/
;;

(define (*A089859! s)
  (cond ((pair? s)
           (cond ((pair? (cdr s)) (swap! (cdr s)) (swap! s))
                 (else (swap! s))
           )
        )
  )
  s
)


;;  B  C        C  A
;; A \/    -->   \/ B        and  A ()        () A
;;  \/            \/               \/    -->   \/
;;

(define (*A089860! s)
  (cond ((pair? s)
           (cond ((pair? (cdr s)) (swap! (cdr s)) (robl! s) (swap! (car s)))
                 (else (swap! s))
           )
        )
  )
  s
)

;; A  B            B  C
;;  \/ C   -->    A \/        and  () B        B ()
;;   \/            \/               \/    -->   \/
;;

(define (*A074680! s)
  (cond ((pair? s)
           (cond ((pair? (car s)) (robr! s))
                 (else (swap! s))
           )
        )
  )
  s
)


(define (*A074680v2! s) ;; was gmRobr!, i.e. gmRobl!'s inverse, do everything in reverse order
    (*A057163! s) ;; as all the constituents are involutions...
    (*A072796! s)
    (*A057163! s)
    (*A069770! s)
    (*A072796! s)
    s
)


;; A  B            A  C
;;  \/ C   -->    B \/        and  () B        B ()
;;   \/            \/               \/    -->   \/
;;

(define (*A089861! s)
  (cond ((pair? s)
           (cond ((pair? (car s)) (swap! (car s)) (robr! s))
                 (else (swap! s))
           )
        )
  )
  s
)


;; A  B            C  B
;;  \/ C   -->    A \/        and  () B        B ()
;;   \/            \/               \/    -->   \/
;;

(define (*A073270! s) (*A072796! (*A069770! s))) ;; inv. of *A073269. Row 5 in A073200, row 19 in A089840.
  



;; A  B            C  A
;;  \/ C   -->    B \/        and  () B        B ()
;;   \/            \/               \/    -->   \/
;;

(define (*A089862! s)
  (cond ((pair? s)
           (cond ((pair? (car s)) (swap! (car s)) (robr! s) (swap! (cdr s)))
                 (else (swap! s))
           )
        )
  )
  s
)


;; A  B            B  A
;;  \/ C   -->    C \/        and  () B        B ()
;;   \/            \/               \/    -->   \/
;;

(define (*A089863! s)
  (cond ((pair? s)
           (cond ((pair? (car s)) (swap! (car s)) (swap! s))
                 (else (swap! s))
           )
        )
  )
  s
)

;; The "square" of A089859/A089863.
;; This involution effects the following transformation:
;;
;;  A  B C  D     B  A D  C       B   C       C   B       A   B        B   A
;;   \ / \ /       \ / \ /         \ /         \ /         \ /          \ /
;;    .   .    -->  .   .       ()  .  -->  ()  .           .  ()   -->  .  ()
;;     \ /           \ /         \ /         \ /             \ /          \ /
;;      .             .           .           .               .            .

;; Note that the last two-clauses can be encountered in either order,
;; as well as those two independent if-forms inside cond below:

(define (*A089864! s)
  (cond ((pair? s)
           (if (pair? (car s)) (swap! (car s)))
           (if (pair? (cdr s)) (swap! (cdr s)))
        )
  )
  s
)


(define (*A089865! s)
  (if (pair? s) (*A074679! (car s)))
  s
)

(define (*A089866! s)
  (if (pair? s) (*A074680! (car s)))
  s
)


;; %C A154121 ....C...D.......B...C
;; %C A154121 .....\./.........\./
;; %C A154121 ..B...x....-->....x...D.................B..().........()..B..
;; %C A154121 ...\./.............\./...................\./....-->....\./...
;; %C A154121 A...x...........A...x.................A...x.........A...x....
;; %C A154121 .\./.............\./...................\./...........\./.....
;; %C A154121 ..x...............x.....................x.............x......
;; %C A154121 .............................................................
;; %C A154121 That is, (a . (b . (c . d))) -> (a . ((b . c) . d))
;; %C A154121 or (a . (b . ())) --> (a . (() . b)) if the former is not possible.


;; %C A154122 .B...C...............C...D
;; %C A154122 ..\./.................\./
;; %C A154122 ...x...D....-->....B...x.................()..C ........C...()
;; %C A154122 ....\./.............\./...................\./....-->....\./...
;; %C A154122 .A...x...........A...x.................A...x.........A...x....
;; %C A154122 ..\./.............\./...................\./...........\./.....
;; %C A154122 ...x...............x.....................x.............x......
;; %C A154122 ..............................................................
;; %C A154122 That is, (a . ((b . c) . d)) -> (a . (b . (c . d)))
;; %C A154122 or (a . (() . c)) -> (a . (c . ())) if the former is not possible.

(define (*A154121! s) ;; /* A089840[3655] */
  (if (pair? s) (*A074679! (cdr s)))
  s
)

(define (*A154122! s) ;; /* A089840[3747] */
  (if (pair? s) (*A074680! (cdr s)))
  s
)


;; %C A154123 ....C...D.......B...C
;; %C A154123 .....\./.........\./
;; %C A154123 ..B...x....-->....x...D.................B..().........()..A..
;; %C A154123 ...\./.............\./...................\./....-->....\./...
;; %C A154123 A...x...........A...x.................A...x.........B...x....
;; %C A154123 .\./.............\./...................\./...........\./.....
;; %C A154123 ..x...............x.....................x.............x......
;; %C A154123 .............................................................
;; %C A154123 That is, (a . (b . (c . d))) -> (a . ((b . c) . d))
;; %C A154123 or (a . (b . ())) --> (b . (() . a)) if the former is not possible.


;; %C A154124 .B...C...............C...D
;; %C A154124 ..\./.................\./
;; %C A154124 ...x...D....-->....B...x.................()..C ........A...()
;; %C A154124 ....\./.............\./...................\./....-->....\./...
;; %C A154124 .A...x...........A...x.................A...x.........C...x....
;; %C A154124 ..\./.............\./...................\./...........\./.....
;; %C A154124 ...x...............x.....................x.............x......
;; %C A154124 ..............................................................
;; %C A154124 That is, (a . ((b . c) . d)) -> (a . (b . (c . d)))
;; %C A154124 or (a . (() . c)) -> (c . (a . ())) if the former is not possible.


(define (*A154123! s) ;; /* A089840[3656] */
  (if (and (pair? s) (pair? (cdr s)) (pair? (cddr s)))
      (*A074679! (cdr s))
      (*A089851! s)
  )
  s
)


(define (*A154124! s) ;; /* A089840[3748] */
  (if (and (pair? s) (pair? (cdr s)) (pair? (cadr s)))
      (*A074680! (cdr s))
      (*A089853! s)
  )
  s
)


;; %C A154125 .A...B.C...D.......C...D.A...B.
;; %C A154125 ..\./...\./.........\./...\./..
;; %C A154125 ...x.....x...--->....x.....x...
;; %C A154125 ....\.../.............\.../....
;; %C A154125 ......x.................x......
;; %C A154125 ...............................
;; %C A154125 ((a . b) . (c . d)) -> ((c . d) . (a . b))
;; %C A154125 or fix, if either the left or right hand side subtree is empty.


(define (*A154125! s) ;; /* A089840[83] */
  (if (and (pair? s) (pair? (car s)) (pair? (cdr s))) (*A069770! s))
  s
)

;; %C A154126 .A...B.C...D.......A...B.C...D.....
;; %C A154126 ..\./...\./.........\./...\./........................
;; %C A154126 ...x.....x...--->....x.....x.......A...B.......B...A.
;; %C A154126 ....\.../.............\.../.........\./..--->...\./..
;; %C A154126 ......x.................x............x...........x...
;; %C A154126 ..............................(where either A or B is (), a leaf)

(define (*A154126! s) ;; /* A089840[183] */
  (if (and (pair? s) (or (not (pair? (car s))) (not (pair? (cdr s))))) (*A069770! s))
  s
)


; INVKROF(*A069770)
; CLAUSE gmA129604[] = { CLAUSESEQ_begin(103,3), { 3, 2, 2, 20,}, { 2, 0, 1, 4 }, { 2, 1, 0, 4 } }; /* A089840[1654720] */
; ((a . b) . (c . d)) --> ((d . c) . (b . a))
; (a . (b . c))       --> ((c . b) . a)   [a implied ()]
; ((a . b) . c)       --> (c . (b . a))   [c implied ()]
;
; This involution effects the following transformation:
;
;  A  B C  D     D  C B  A       B   C   C   B           A   B            B   A
;   \ / \ /       \ / \ /         \ /     \ /             \ /              \ /
;    X1  X2   -->  Y1  Y2      A   X1  --> Y1  A           X1  C  -->   C   Y1
;     \ /           \ /         \ /         \ /             \ /          \ /
;      X0            Y0          X0          Y0              X0           Y0
;

; I.e. *A069770 = FORK(*A129604) = KROF(*A129604)
; A129604 = A069770 o A089864 = A089864 o A069770


(define (*A129604 s)
  (cond ((pair? s) (cons (*A069770 (cdr s)) (*A069770 (car s))))
        (else s)
  )
)

(define (*A129604! s)
  (cond ((pair? s)
           (*A069770! (car s))
           (*A069770! (cdr s))
           (*A069770! s)
        )
  )
  s
)


; CLAUSE gmA129607[] = { CLAUSESEQ_begin(24,2), { 3, 0, 0, 0,}, { 2, 0, 0, 1 } }; /* A089840[3608] */
; (a . (b . (c . d))) --> (a . (b . (c . d)))
; (a . (b . c))       --> (b . (a . c))   [c implied ()]
;
;       C   D         C   D
;        \ /           \ /
;     B   X2        B   Y2      B   C       A   C
;      \ /           \ /         \ /         \ /
;   A   X1    --> A   Y1      A   X1  --> B   Y1  (C is [])
;    \ /           \ /         \ /         \ /
;     X0            Y0          X0          Y0


(define (*A129607 s)
  (if (= 2 (length s)) (*A072796 s) s)
)

(define (*A129607! s)
  (if (= 2 (length s)) (*A072796! s))
  s
)

(define *A129608   (ENIPS *A129607))
(define *A129608v2 (SPINE *A129607))

(define *A129608!   (!ENIPS *A129607!))
(define *A129608v2! (!SPINE *A129607!))

; A129608 = A057508 o A072796 o A057508 = A057164 o A072796 o A057164
; A072796 = (!SPINE *A129605!) = (!ENIPS *A129606!)

; CLAUSE gmA129605[] = { CLAUSESEQ_begin(24,2), { 3, 0, 0, 3,}, { 2, 0, 0, 1 } }; /* A089840[3613] */
; (a . (b . (c . d))) --> (b . (c . (a . d)))
; (a . (b . c))       --> (b . (a . c))   [c implied ()]
;
;       C   D         A   D
;        \ /           \ /
;     B   X2        C   Y2      B   C       A   C
;      \ /           \ /         \ /         \ /
;   A   X1    --> B   Y1      A   X1  --> B   Y1  (C is [])
;    \ /           \ /         \ /         \ /
;     X0            Y0          X0          Y0

(define (*A129605 s)
  (cond ((> (length s) 2)
           (cons (cadr s) (cons (caddr s) (cons (car s) (cdddr s))))
        )
        (else (*A072796 s))
  )
)

(define (*A129605! s)
  (cond ((< (length s) 3) (*A072796! s))
        (else
           (let ((org_car (car s)))
              (set-car! s (cadr s))
              (set-car! (cdr s) (caddr s))
              (set-car! (cddr s) org_car)
              s
           )
        )
  )
)

; CLAUSE gmA129606[] = { CLAUSESEQ_begin(24,2), { 3, 0, 0, 5,}, { 2, 0, 0, 1 } }; /* A089840[3617] */
; (a . (b . (c . d))) --> (c . (a . (b . d)))
; (a . (b . c))       --> (b . (a . c))   [c implied ()]
;
;       C   D         B   D
;        \ /           \ /
;     B   X2        A   Y2      B   C       A   C
;      \ /           \ /         \ /         \ /
;   A   X1    --> C   Y1      A   X1  --> B   Y1  (C is [])
;    \ /           \ /         \ /         \ /
;     X0            Y0          X0          Y0


(define (*A129606 s)
  (cond ((> (length s) 2)
           (cons (caddr s) (cons (car s) (cons (cadr s) (cdddr s))))
        )
        (else (*A072796 s))
  )
)

(define (*A129606! s)
  (cond ((< (length s) 3) (*A072796! s))
        (else
           (let ((org_car (car s)))
              (set-car! s (caddr s))
              (set-car! (cddr s) (cadr s))
              (set-car! (cdr s) org_car)
              s
           )
        )
  )
)

; A074679 = (!ENIPS *A129609!)
; A074680 = (!SPINE *A129610!)
;% CLAUSE gmA129609[] = { CLAUSESEQ_begin(39,3), { 3, 1, 2, 0,}, { 2, 0, 1, 2 }, { 1, 0, 0, 1 } }; /* A089840[65167] */

; (a . ((b . c) . d)) --> ((a . b) . (c . d))
; (a . (b . c))       --> ((a . c) . b) [b implied ()]
; (a . b)             --> (b . a)       [b implied ()]

(define (*A129609 s)
  (cond ((pair? s) (*A074679 (cons (car s) (*A074680 (cdr s)))))
        (else s)
  )
)

(define (*A129609! s)
  (cond ((pair? s)
           (*A074680! (cdr s))
           (*A074679! s)
        )
  )
  s
)

;% CLAUSE gmA129610[] = { CLAUSESEQ_begin(39,3), { 3, 2, 1, 0,}, { 2, 1, 0, 2 }, { 1, 0, 0, 1 } }; /* A089840[65352] */

; ((a . b) . (c . d)) --> (a . ((b . c) . d))
; ((a . b) . c)       --> (a . (c . b))   [c implied ()]
; (a . b)             --> (b . a)         [a implied ()]

(define (*A129610 s)
  (cond ((pair? s) (let ((t (*A074680 s))) (cons (car t) (*A074679 (cdr t)))))
        (else s)
  )
)

(define (*A129610! s)
  (cond ((pair? s)
           (*A074680! s)
           (*A074679! (cdr s))
        )
  )
  s
)

; *A089859! = (!ENIPS *A129611!)
; *A089863! = (!SPINE *A129612!)

;% CLAUSE gmA129611[] = { CLAUSESEQ_begin(8,2), { 3, 1, 4, 10,}, { 1, 0, 0, 1 } }; /* A089840[169] */
; (a . ((b . c) . d)) --> (((c . b) . d) . a)
; (a . b)             --> (b . a)   [b implied () or (() . X)]


(define (*A129611 s)
  (cond ((pair? s) (*A089859 (cons (car s) (*A089863 (cdr s)))))
        (else s)
  )
)

(define (*A129611! s)
  (cond ((pair? s)
           (*A089863! (cdr s))
           (*A089859! s)
        )
  )
  s
)

;% CLAUSE gmA129612[] = { CLAUSESEQ_begin(8,2), { 3, 4, 1, 22,}, { 1, 0, 0, 1 } }; /* A089840[251] */
; (((a . b) . c) . d) --> (d . ((b . a) . c))
; (a . b)             --> (b . a)   [a implied () or (() . X)]

(define (*A129612 s)
  (cond ((pair? s) (let ((t (*A089863 s))) (cons (car t) (*A089859 (cdr t)))))
        (else s)
  )
)

(define (*A129612! s)
  (cond ((pair? s)
           (*A089863! s)
           (*A089859! (cdr s))
        )
  )
  s
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (*A057161 bt) ;; Was: RotateTriangularization
  (let loop ((lt bt) (nt (list)))
     (cond ((not (pair? lt)) nt)
           (else (loop (car lt)
                       (cons (cdr lt) nt)
                 )
           )
     )
  )
)



(define (*A057162 bt) ;; Was: RotateTriangularizationInv
  (let loop ((lt bt) (nt (list)))
     (cond ((not (pair? lt)) nt)
           (else (loop (cdr lt)
                       (cons nt (car lt))
                 )
           )
     )
  )
)



(define (DeepRotateTriangularization bt) ;; -> A057505
  (let loop ((lt bt) (nt (list)))
     (cond ((not (pair? lt)) nt)
           (else (loop (car lt)
                       (cons (DeepRotateTriangularization (cdr lt)) nt)
                 )
           )
     )
  )
)



(define (DeepRotateTriangularizationInv bt) ;; -> A057506
  (let loop ((lt bt) (nt (list)))
     (cond ((not (pair? lt)) nt)
           (else (loop (cdr lt)
                       (cons nt (DeepRotateTriangularizationInv (car lt)))
                 )
           )
     )
  )
)


; ((()((()())()))()())
;
;       /\/\
;      /    \/\
;   /\/        \
;  /            \/\/\
; /                  \
;
;->(define vv (list '((()((()())()))()())))
; (((() ((() ()) ())) () ()))
; ->(*A069771 vv)
; ((() ((() () ()) ())) () ())
; ->(*A069772 vv)
; (() () ((() (() () ())) ()))
;
;          /\/\/\
;       /\/      \
;      /          \/\
; /\/\/              \
;

(define (*A057501 s) ;; Was RotateHandshakes
  (cond ((null? s) (list))
        (else (append (car s) (list (cdr s))))
  )
)

(define (RotateHandshakes_n_steps s n)
  (cond ((zero? n) s)
        (else (RotateHandshakes_n_steps (*A057501 s) (- n 1)))
  )
)


(define (*A069771 s) (RotateHandshakes_n_steps s (count-pars s))) ;; Was RotateHandshakes180

; Reflect handshakes over x-axis. (*A057164 reflects over y-axis)
; This transformation keeps palindromic parenthesizations/mountain ranges/
; rooted planar trees palindromic, but not necessarily same.

(define (*A069772 s) (*A057164 (*A069771 s))) ;; Was xReflectHandshakes


(define (*A057161v2 s) ;; Was RotateHandshakesD1
   (cond ((null? s) s)
         (else (append (*A057161v2 (car s)) (list (cdr s))))
   )
)

;; This is Emeric Deutsch's bijection "Gamma" on Dyck paths:
;; A bijection on Dyck paths and its consequences, Discrete mathematics 179 (1998) 252-256.

(define (*A057503 a)
  (cond ((null? a) a)
        (else (append (car a) (list (*A057503 (cdr a)))))
  )
)

;; Another way to define it:
(define (*A057503v2 s) (fold-right (lambda (x y) (append x (list y))) '() s))

(define (*A057503! s)
  (cond ((pair? s)
           (*A057503! (cdr s))
           (*A057501! s)
        )
  )
  s
)

(define (*A057504! s)
  (cond ((pair? s)
           (*A057502! s)
           (*A057504! (cdr s))
        )
  )
  s
)


(define (*A057505! s)
  (cond ((pair? s)
           (*A057505! (car s))
           (*A057505! (cdr s))
           (*A057501! s)
        )
  )
  s
)

(define (*A057506! s)
  (cond ((pair? s)
           (*A057502! s)
           (*A057506! (car s))
           (*A057506! (cdr s))
        )
  )
  s
)


(define (*A071661 s) (*A057505! (DeepRotateTriangularization s)))

;; Yields the same as DeepRotateTriangularization
(define (DonagheysM a) ;; --> A057505 (was known as RotateHandshakesD3)
  (cond ((null? a) a)
        (else (append (DonagheysM (car a)) (list (DonagheysM (cdr a)))))
  )
)

(define (list->string-strange lista)
  (string-append
    "("
    (with-output-to-string
      (lambda ()
        (let recurse ((lista lista))
          (cond ((pair? lista)
                   (recurse (car lista))
                   (write-string "(")
                   (recurse (cdr lista))
                   (write-string ")")
                )
          )
        )
      )
    )
    ")"
  )
)

(define (DonagheysMv2 s)
   (with-input-from-string (list->string-strange s) read)
)

;; Whose signature-permutation this is?
;; --> (0 1 2 3 6 4 5 8 7 19 16 11 14 9 15 10 13 22 21 12 18 20 17 60 56 47 53 44 33 ...)

(define (*A057164v3 a)
  (cond ((null? a) a)
        (else (append (*A057164v3 (cdr a))
                      (list (*A057164v3 (car a)))
              )
        )
  )
)


(define (*A057163 bt) ;; Was known as ReflectBinTree
  (cond ((not (pair? bt)) bt)
        (else (cons (*A057163 (cdr bt))
                    (*A057163 (car bt)))
        )
  )
)


(define (AllTrees2DoubleTrunked bt)
  (cond ((not (pair? bt)) bt)
        (else (list (car bt) (cdr bt)))
  )
)


(define (app-to-xrt a b) ;; Append 'b' to the eXtreme Rightmost Tip of 'a'.
  (cond ((null? a) b)
        ((pair? (cdr a)) (cons (car a) (app-to-xrt (cdr a) b)))
        (else (cons (app-to-xrt (car a) b) (cdr a)))
  )
)

(define (app-to-xrt! a b) ;; Append 'b' to the eXtreme Rightmost Tip of 'a', physically.
  (cond
    ((null? a) b) ;; Only the ethereal () cannot be touched.
    (else
      (let recurse ((a a) (b b))
         (cond ((and (not (pair? (car a))) (not (pair? (cdr a))))
                   (set-car! a b)
               )
               ((pair? (cdr a)) (recurse (cdr a) b))
               (else (recurse (car a) b))
         )
      )
      a ;; Return the modified a.
    )
  )
)

;; Rotate interpretations (pp), (qq) & (rr) of Stanley
;; (i.e. non-crossing partitions and Murasaki diagrams)
;; (cf. 42 non-crossing ones of 52 "genji-mon" in "Genji monogatari")
;; Compare to the definition of *A057501 (RotateHandshakes)
;; Is this a conjugate of A082315 (= A057501^2) as all
;; the count sequences appear to be same?
;; What is the conjugating automorphism?
;; Hint: use Meeussen's "non-neighbouring alliances"
;; and remove every second point from them (plus the
;; associated chord(s), if any.)

(define (*A085159 s)
  (cond ((null? s) s)
        (else (app-to-xrt (car s) (append (cdr s) (list (list)))))
  )
)

(define (*A085159!org s)
  (cond ((null? s) s)
        (else (app-to-xrt! (car s) (append! (cdr s) (list (list)))))
  )
)

;; This version is properly side-effective, i.e. s is guaranteed
;; to be structurally modified to the intended shape:
;; (not just the result returned):
(define (*A085159! s)
  (cond ((null? s) s)
        ((null? (car s)) (*A057509! s))
        (else
           (let* ((car-s (car s))
                  (org-caar (car car-s))
                  (org-cdar (cdr car-s))
                 )
             (set-car! car-s (list))
             (set-cdr! car-s (cdr s))
             (*A057509! car-s)
             (set-car! s org-caar)
             (set-cdr! s org-cdar)
             (app-to-xrt! s car-s)
           )
        )
  )
)

(define (*A085160 s)  (*A085161 (*A085159 (*A085161 s))))
(define (*A085160! s) (*A085161! (*A085159! (*A085161! s))))


;; Here the orbit counts is A007123, same as for A057164.
;; (and fixed counts is A001405 = C(n,[n/2]).)
(define (*A085161 s)
  (cond ((null? s) s)
        (else
          (let ((u (reverse s)))
             (app-to-xrt (*A085161 (car u))
                         (append (map *A085161 (cdr u)) (list (list)))
             )
          )
        )
  )
)

(define (*A085161! s)
  (cond ((null? s) s)
        (else
          (let ((u (reverse! s)))
             (app-to-xrt! (*A085161! (car u))
                          (append! (map *A085161! (cdr u)) (list (list)))
             )
          )
        )
  )
)

(define (*A085162! s) (*A057163! (*A085161! (*A057163! s))))


;; The fixed counts seem to be 1 1 2 2 3 4 7 11 21 36 71 ...,
;; i.e. A051920 C(n,[n/2])+1 = A001405(n)+1 shifted right by 2 ?


;; A085163(n) = A085159(A123719(n)) (also A085161(A057508(n))), A123719(n) = A085160(A085163(n)).

(define (*A085163! s)
  (cond ((null? s) s)
        (else (app-to-xrt! (*A085161! (car s))
                           (append! (map *A085161! (cdr s)) (list (list)))
              )
        )
  )
)


;; Orbit count sequence = Once right shifted version of A001683?
;; Is this a conjugate of A074679/A074680 ? Find a conjugating
;; permutation... (should be weird?)

(define (*A085167! s)
  (cond ((null? s) s)
        (else (app-to-xrt! (cdr s) (append! (car s) (list (list)))))
  )
)



;; This one is horizontally telescoping:
;; 0 1 2 3 4 5 6 8 7 9 10 11 13 12 14 15 19 22 21 ...
;; Conjugate A082315 with this, and voila, you should get A085159

(define (*A085169 s)
  (letrec ((evenlev (lambda (s)
                      (cond ((not (pair? s)) s)
                            (else (cons (oddlev (car s)) (evenlev (cdr s))))
                      )
                    )
           )
           (oddlev (lambda (s)
                      (cond ((not (pair? s)) s)
                            (else (append (evenlev (car s)) (list (oddlev (cdr s)))))
                      )
                    )
           )
          )
     (evenlev s)
  )
)

;; Corresponds to evenlev in above definition.
(define (*A085169! s)
   (cond ((pair? s)
            (*A074684!v2 (car s))
            (*A085169! (cdr s))
         )
   )
   s
)


;; Corresponds to oddlev above. Check that really equals A074684 ! (Shouldn't be hard...)
(define (*A074684!v2 s)
   (cond ((pair? s)
            (*A085169! (car s))
            (*A074684!v2 (cdr s))
            (*A057501! s)
         )
   )
   s
)

(define (*A085169v2! s) (map *A074684! s))
(define (*A085170! s) (map *A074683! s))

;; Variant of *A085163!
(define (*A085171! s)
  (cond ((null? s) s)
        (else (app-to-xrt! (*A085171! (car s))
                           (append! (map *A085171! (cdr s)) (list (list)))
              )
        )
  )
)


(define (*A085171!v2 s)
  (cond ((null? s) s)
        (else
           (for-each *A085171!v2 s)
           (*A085159! s)
        )
  )
  s
)

;; Similarly, we can define:

(define (*A057511v2! s)
  (cond ((null? s) s)
        (else
           (for-each *A057511v2! s)
           (*A057509! s)
        )
  )
  s
)

(define (*A057164!v2 s)
  (cond ((null? s) s)
        (else
           (for-each *A057164!v2 s)
           (*A057508! s)
        )
  )
  s
)

;; Few fresh applications of "RECURSE_DEEPLY" transformation:
;; Nine new ones, waiting for their submissions to OEIS:

(define (*Anew1! s)
  (cond ((null? s) s)
        (else
           (for-each *Anew1! s)
           (*A069770! s)
        )
  )
  s
)


(define (*Anew2! s) ;; Inverse of the above.
  (cond ((null? s) s)
        (else
           (*A069770! s)
           (for-each *Anew2! s)
        )
  )
  s
)


(define (*Anew3! s) ;; An involution. I think A057163 embeds here nicely.
  (cond ((null? s) s)
        (else
           (for-each *Anew3! s)
           (*A072796! s)
        )
  )
  s
)

(define (*Anew3andhalf! s) ;; This should be nice, A082325 should embed here in scale n:2n.
  (cond ((null? s) s)
        (else
           (for-each *Anew3! s)
           (*A072797! s)
        )
  )
  s
)


(define (*Anew4! s)
  (cond ((null? s) s)
        (else
           (for-each *Anew4! s)
           (*A074679! s)
        )
  )
  s
)

(define (*Anew5! s) ;; Inverse of the above.
  (cond ((null? s) s)
        (else
           (*A074680! s)
           (for-each *Anew5! s)
        )
  )
  s
)

(define (*Anew6! s)
  (cond ((null? s) s)
        (else
           (for-each *Anew6! s)
           (*A074680! s)
        )
  )
  s
)

(define (*Anew7! s) ;; Inverse of the above.
  (cond ((null? s) s)
        (else
           (*A074679! s)
           (for-each *Anew7! s)
        )
  )
  s
)

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



(define (*A086425 s) (*A057164 (*A074684! (copy-tree s))))
(define (*A086426 s) (*A074683! (*A057164 s)))

(define (*A086429 s) (*A057164 (*A085159 (*A057164 s))))
(define (*A086430 s) (*A057164 (*A085160 (*A057164 s))))


;; Differs from A122326 at n=19, where a(19)=10, while A122326(19)=11.
;; See Emeric Deutsch: A Bijection on Ordered Trees and Its Consequences,
;; Journal of Combinatorial Theory, Series A 90, 210-215 (2000)


(define (*A125981 s) ;; Not yet in OEIS, as of DEC 12 2006.
  (cond ((null? s) s)
        (else (append (*A125981 (car s)) (list (map *A125981 (cdr s)))))
  )
)

(define (*A125982 s) ;; Inverse of the above.
  (cond ((null? s) s)
        (else (map *A125982 (cons (except-last-pair s) (car (last-pair s)))))
  )
)

;; keep-matching-items should copy the toplevel of its list argument,
;; but _NOT_ any of the deeper levels!

(define (add-valley-abscisses! valley-abscisse peak-ordonnees)
   (for-each (lambda (s) (append! s (list valley-abscisse)))
             (keep-matching-items peak-ordonnees (lambda (po) (>= (car po) valley-abscisse)))
   )
)

;; There is probably much nicer implementation for this:
(define (A126310-aux1 n) ;; Construct a rising list for derivation.
  (let loop ((n n) (vs (list)) (u 0) (d 0))
     (cond ((zero? n) (if (null? vs) vs (reverse! (cdr vs)))) ;; Discard the last peak-valley pair.
           ((= 2 (modulo n 4)) ;; Found a peak, add its "ordonnée" (number of u-steps so far) into vs list.
                (loop (/ n 2) (cons (list (+ 1 u)) vs) (+ u 1) d)
           )
           ((= 1 (modulo n 4)) ;; Found a valley, append its abscisse to vs-list to those ordonnees for which are greater than or equal to this "abscisse" (number of d-steps so far).
                (add-valley-abscisses! (+ d 1) vs)
                (loop (/ (- n 1) 2) vs u (+ d 1))
           )
           ((odd? n) (loop (/ (- n 1) 2) vs u (+ d 1)))
           (else (loop (/ n 2) vs (+ u 1) d))
     )
  )
)


(define (A126310 n)
 (A080300 (rising-list->binexp (reverse! (map -1+ (map length (A126310-aux1 (A036044 (A014486 n))))))))
)


;; J. Vaillé, Une bijection explicative de plusieurs propriétés remarquables des ponts, 
;; European J. Combin., vol 18 (1997), no. 1, 117--124. 

;; n is one of A014486 (a totally balanced binary word).
;; This functions returns the list of lists, of the form
;; (([i/i+1]+) ([i+1/i+2]+) ([i+2/i+3]+) ...)
;; where in the first list i's correspond to binary digits
;; of n, starting _after_ the first 0 encountered when scanning
;; right from the msb-1 of n. The first list continues as
;; long as there are as many 1's (corresponding to 0-bits in n)
;; as is the length of maximal 1-prefix of the bin.exp. of n.
;; Et cetera...


(define (A125985-aux1 n)
  (if (zero? n) (list)
   (let ((begin_from (<< 1 (- (- (A000523 n) (A090996 n)) 1)))) ;; pointer after the most signif. 0.
    (let loop ((s (A090996 n)) ;; Number of U's from previous batch, to be matched with D's.
               (t 0)           ;; Number of U's encountered in this batch so far.
               (nth_list 1)
               (p begin_from)
               (b (if (= 0 (A004198bi n begin_from)) 0 1)) ;; The bit under scrutiny. 1 if U, 0 if D.
               (lists (list (list)))
;;             (q 50)
              )
;;     (format #t "s=~a t=~a nth_list=~a p=~a b=~a lists=~a\n" s t nth_list p b lists)
       (cond
;;           ((zero? q) #f)
             ((< s 1) ;; The D-slopes have "consumed" the U-slopes of the previous list.
                (cond ((< p 1) (reverse! lists)) ;; (reverse! (map reverse! lists))) ;; Ready!
                      (else ;; start the next list, s will be t, t will be zero.
                         (loop (- t (- 1 b)) b (+ 1 nth_list)
                               (>> p 1) (if (= 0 (A004198bi n (>> p 1))) 0 1)
                               (cons (list (+ b 1 nth_list)) lists)
;;                             (- q 1)
                         )
                      )
                )
             )
             (else
                (loop (- s (- 1 b)) (+ t b) nth_list
                      (>> p 1) (if (= 0 (A004198bi n (>> p 1))) 0 1)
                      (cons (cons (+ b nth_list) (car lists)) (cdr lists))
;;                    (- q 1)
                )
             )
       )
    )
   )
  )
)

;; two multisets of numeric elements, a consisting of integers 1-m, b consting of integers m and m+1,
;; the m's should be matched, and in the result's m+1's should always follow after some m or m+1.
(define (m-join a b m)
    (let loop ((a a)
               (b b)
               (c (list))
              )
        (cond ((and (not (pair? a)) (not (pair? b))) (reverse! c))
              ((not (pair? a)) (loop a (cdr b) (cons (car b) c)))
              ((not (pair? b)) (loop (cdr a) b (cons (car a) c)))
              ((equal? (car a) (car b)) ;; it should be m
                  (loop (cdr a) (cdr b) (cons (car a) c))
              )
              ((> (car b) m)
                  (loop a (cdr b) (cons (car b) c))
              )
              (else ;; I.e. (< (car a) m)
                  (loop (cdr a) b (cons (car a) c))
              )
        )
    )
)

(define (A125985-aux2 n)
   (let loop ((lists (A125985-aux1 n))
              (z (list))
              (m 1)
             )
       (if (null? lists) z (loop (cdr lists) (m-join z (car lists) m) (+ m 1)))
   )
)

;; (first-dislocated (map A080300 (map rising-list->binexp (map reverse! (map n->factbase (map A071156 (iota0 6919))))))) --> ()
;; i.e. A080300 o rising-list->binexp o reverse! o n->factbase o A071156 = id.

(define (rising-list->binexp rising-list)
   (let loop ((s 0)
              (i 0)
              (h 0)
              (fs rising-list)
             )
       (cond ((null? fs) (+ s (<< (- (<< 1 h) 1) i)))
             ((> (car fs) h) ;; We are rising. (- (car fs) h) should be 1.
                (loop s (+ i 1) (car fs) (cdr fs))
             )
             (else ;; add (- h (car fs))+1 1's to the front.
                (loop
                      (+ s (<< (- (<< 1 (+ 1 (- h (car fs)))) 1) i))
                      (+ i 2 (- h (car fs)))
                      (car fs)
                      (cdr fs)
                )
             )
       )
   )
)

;; If we were clever, we could probably rank the "rising list" directly,
;; instead of taking it through rising-list->binexp and A080300:

(definec (A125985 n) ;; J. Vaillé's intricate bijection.
   (A080300 (rising-list->binexp (A125985-aux2 (A014486 n))))
)

(define (descending-list->bin-lists rl)
   (let loop ((z (list))
              (m 1)
             )
     (let ((sl (map (lambda (n) (- n m))
                    (keep-matching-items rl (lambda (n) (or (= n m) (= n (+ 1 m)))))
               )
           )
          )
        (cond ((null? sl) z)
              (else (loop (cons sl z) (+ m 1)))
        )
     )
   )
)

(define (binlist->n binlist)
   (let loop ((s 0) (bl binlist))
       (if (null? bl) s (loop (+ s s (car bl)) (cdr bl)))
   )
)

(definec (A125986 n) ;; And its inverse.
   (let ((z (reduce append! '()
                    (reverse! (descending-list->bin-lists (binexp->A071158-list (A014486 n))))
            )
         )
         (tl (A057515 n))
        )
     (A080300 (/ (+ (<< (- (<< 1 tl) 1) (+ (length z) 1)) (binlist->n z)) 2))
   )
)

(define (A125987 n) (A125985 (A125985 n)))
(define (A125988 n) (A125986 (A125986 n)))


;; Here are the 40 A-numbers you requested: A125974 --- A126013.
;; (A125985 - A125989 still free)
;; Here are the 31 A-numbers you requested: A126290 --- A126320.


(define (*A057509 s) ;; Was Rol
  (cond ((not (pair? s)) s) (else (append (cdr s) (list (car s)))))
)


(define (*A057508 s)
  (if (null? s) s (append (*A057508 (cdr s)) (list (car s))))
)


(define (RolRecursive s)
  (cond ((not (pair? s)) s)
        ((null? (cdr s)) s)
        (else (cons (car (cdr s))
                    (RolRecursive (cons (car s) (cdr (cdr s))))
              )
        )
  )
)


(define (A057511 s) ;; Was DeepRol
  (cond ((not (pair? s)) s)
        ((null? (cdr s)) (list (A057511 (car s))))
        (else (cons (A057511 (car (cdr s)))
                    (A057511 (cons (car s) (cdr (cdr s))))
              )
        )
  )
)


(define (*A069769 bt) ;; Was: CarReverse
  (let loop ((lt bt) (nt (list)))
     (cond ((not (pair? lt)) nt)
           (else (loop (car lt)
                       (cons nt (cdr lt))
                 )
           )
     )
  )
)

(define (AnotherReverse bt) ;; --> A057508
  (let loop ((lt bt) (nt (list)))
     (cond ((not (pair? lt)) nt)
           (else (loop (cdr lt)
                       (cons (car lt) nt)
                 )
           )
     )
  )
)



(define (*reverse lista)
       (cond ((null? lista) (list))
              (else (append (*reverse (cdr lista))
                           (cons (car lista) (list))))))

 
(define (rewerse lista)
   (cond ((null? lista) lista)
         ((null? (cdr lista)) lista)
         (else (cons (car (rewerse (cdr lista)))
                     (rewerse (cons (car lista)
                                    (rewerse (cdr (rewerse (cdr lista))))
                              )
                     )
               )
         )
   )
)


;; Compute the similar sequence as A033538 for this function:
(define (deep*reverse lista)
   (cond ((not (pair? lista)) lista)
         ((null? (cdr lista)) (list (deep*reverse (car lista))))
         (else (cons (deep*reverse (car (deep*reverse (cdr lista))))
                     (deep*reverse (cons (car lista)
                                         (deep*reverse
                                             (cdr (deep*reverse (cdr lista)))
                                         )
                                   )
                     )
               )
         )
   )
)

;; This is much shorter:
(define (*A057164 s)
  (if (null? s) s (append (*A057164 (cdr s)) (list (*A057164 (car s)))))
)

;; This was geared for reversing lists with also non-() symbols.
;; From an old Seppänen & Hyvönen Lisp-kurssi Interlisp code, I think:
(define (*A057164old s) ;; Was: DeepRev
   (cond ((not (pair? s)) s)
         ((null? (cdr s)) (cons (*A057164old (car s)) (list)))
         (else (append (*A057164old (cdr s))
                       (*A057164old (cons (car s) (list))))
         )
   )
)



;; df->bf is the inverse of Wouter Meeussen's breadth-first to depth-first
;; automorphism of plane binary trees. Cf. A038776, A057117, A057118

;; (df->bf '(a (b (c) d) e))  -->  (a ((b e) (c) . d))
;; I.e. (df->bf '(a . ((b . ((c . ()) . (d . ()))) . (e . ()))))
;; --> (a . (((b . (e . ())) . ((c . ()) . d))))

;; Note: it's essential that pass-left is "frozen" (i.e. bound
;; lexically as usual) before it's called from newcont closure
;; (i.e. when this was not the leftmost branch (= first branch traversed)
;; of the level depth,
;; and likewise, it's essential that when we come to
;; the first branch (= leftmost) of each level (and pass-left
;; is set to false), we call the next "continuation" through
;; conts-list, which MUST be physically modified, because we might
;; not yet have traversed to the rightmost branch of the next
;; shallower level of bt.

(define (*A057118 bt) ;; Was: df->bf
  (let ((conts (list car))) ;; The last thing we do is take car
    (let recurse ((bt bt) (depth 0))
      (let* ((plp (nthcdr depth conts))
             (pass-left (and (pair? plp) (car plp)))
             (newcont (lambda (stack)
                       ((or pass-left (list-ref conts (-1+ depth)))
                         (if (pair? bt) (cons2top! stack) (cons bt stack))
                       )
                      )
             )
            )
     
        (if pass-left
            (set-car! plp newcont)
            (append! conts (list newcont))
        )

        (cond ((pair? bt)
                (recurse (car bt) (1+ depth))
                (recurse (cdr bt) (1+ depth))
              )
        )
      ) ; let*
    ) ;; let recurse
    ((car (last-pair conts)) (list)) ;; Now, apply the last of closures to ()
  )
)

;; --> (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 19 16 17 18 15 20 21 22 23 24 25 26 27 28 ...)

(define (*A072089 p) ;; The same idea for general trees. Was: gt-df->bf
  (let ((conts (list car))) ;; The last thing we do is take car
    (let recurse ((p p) (depth 0))
      (let* ((plp (nthcdr depth conts))
             (pass-left (and (pair? plp) (car plp)))
             (newcont (lambda (stack)
                       ((or pass-left (list-ref conts (-1+ depth)))
                         (list-n-from-top (length p) stack)
                       )
                      )
             )
            )
     
        (if pass-left
            (set-car! plp newcont)
            (append! conts (list newcont))
        )

        (for-each
           (lambda (branch) (recurse branch (1+ depth)))
           p
        )
      ) ; let*
    ) ;; let recurse
    ((car (last-pair conts)) (list)) ;; Now, apply the last of closures to ()
  )
)



;; For popping Lukasiewicz-word lists with the final (implicit) zero missing:
(define (pop-with-trailing-zeros! lista)
  (let ((topmost (car lista)))
    (cond ((pair? (cdr lista))
                (set-car! lista (cadr lista))
                (set-cdr! lista (cddr lista))
          )
          (else (set-car! lista 0)) ;; Next time, return this zero.
    )
    topmost
  )
)

;; If we use this instead of force-bf-wise in Lw-bf->p!, we get yet another automorphism.
;; (How well defined it is?)
(define (force-sexp s)
   (cond ((promise? s) (force-sexp (force s)))
         ((not (pair? s)) s)
         (else
             (map force-sexp s)
         )
   )
)

;; What first came to my mind... Probably we can improve this...
(define (force-bf-wise! s)
  (letrec ((clear? #t)
           (force-next-level!
             (lambda (s)
                (cond
                   ((not (null? s))
                      (cond ((promise? (car s))
                               (set-car! s (force (car s)))
                               (cond ((not (null? (car s))) (set! clear? #f)))
                            )
                            ((pair? (car s)) (force-next-level! (car s)))
                      )
                      (force-next-level! (cdr s))
                   )
                )
             )
           )
          )
     (let loop ()
           (set! clear? #t)
           (force-next-level! s)
           (if (not clear?) (loop))
     )
     s
  )
)


(define (Lw-bf->p! L)
  (letrec ((lazy-liz
             (lambda (n)
              (cond ((zero? n) (list)) ;; No empty promises, just an empty list.
                    (else
                      (let ((d (pop-with-trailing-zeros! L)))
                         (cons (delay (lazy-liz d))
                               (lazy-liz (- n 1))
                         )
                      )
                    )
              )
             )
          ))
    (force-bf-wise! (lazy-liz (pop-with-trailing-zeros! L)))
  )
)

;; Was. gt-bf->df
(define (*A072088 s) (cond ((null? s) s) (else (Lw-bf->p! (p->Lw s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;; Destructive versions of some of the above automorphisms,           ;;
;; implemented by using only physical primitives                      ;;
;; swap!, robl! and robr! and simple recursions on car and/or cdr     ;;
;; branch of the tree-structure.                                      ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 
;; (define (AnotherReverse! s) ;; --> A057508
;;   (if (null? s)
;;       s
;;       (let loop ((c s) (ex-cdr (cdr s)) (prev (list)))
;;            (set-cdr! c prev)
;;            (cond ((not (pair? ex-cdr)) c)
;;                  (else (loop ex-cdr (cdr ex-cdr) c))
;;            )
;;       )
;;   )
;; )
;; 
;; (define (*A057501! a) ;; This version not purely physical!
;;   (if (null? a)
;;       a
;;       (let ((ex-car (car a)))
;;         (set-car! a (cdr a))
;;         (set-cdr! a (list)) ;; () <- (cdr (last-pair ex-car)) (one cell)
;;         (cond ((pair? ex-car)
;;                  (set-cdr! (last-pair ex-car) a)
;;                  ex-car
;;               )
;;               (else a)
;;         )
;;       )
;;   )
;; )
;; 


(define (*A082335! s)
  (cond ((pair? s)
           (cond ((pair? (cdr s)) (robl! s))
                 (else (*A057163! s))
           )
        )
  )
  s
)


(define (*A082336! s)
  (cond ((pair? s)
           (cond ((pair? (car s)) (robr! s))
                 (else (*A057163! s))
           )
        )
  )
  s
)



(define (*A082349! s)
  (cond ((pair? s)
           (cond ((pair? (cdr s)) (robl! s))
                 (else (*A069767! s))
           )
        )
  )
  s
)


(define (*A082350! s)
  (cond ((pair? s)
           (cond ((pair? (car s)) (robr! s))
                 (else (*A069768! s))
           )
        )
  )
  s
)


;;             A   D
;;              \ /
;; A  D B  C     Q   B       A   B       []  A
;;  \ / \ /       \ /         \ /         \ /           and by default:
;;   P   M    -->  N   C       M  []  -->  N   B       []  A       []  A
;;    \ /           \ /         \ /         \ /         \ /   -->   \ /
;;     X             Y           X           Y           X           Y


(define (*A082351! s)
  (cond ((not (pair? s)) s)
        ((not (pair? (car s))) s)
        ((not (pair? (cdr s))) (robl! (swap! s)))
        (else (robl! s))
  )
)

;; D   C
;;  \ /
;;   P   B         D  C B  A  []  B        B   A
;;    \ /           \ / \ /    \ /          \ /         and by default:
;;     M   A   -->   Q   N      M   A   -->  N  []     []  A       []  A
;;      \ /           \ /        \ /          \ /       \ /   -->   \ /
;;       X             Y          X            Y         X           Y


(define (*A082352! s)
  (cond ((not (pair? s)) s)
        ((not (pair? (car s))) s)
        ((not (pair? (caar s))) (swap! (robr! s)))
        (else (robr! s))
  )
)

;; This effects the following transformation:
;;
;;                      C   D
;;                       \ /
;;  A  B C  D         B   .       B   C       C   ()
;;   \ / \ /           \ /         \ /         \ / 
;;    .   .    -->  A   .       ()  .  -->  B   .           A  ()   -->  A  ()
;;     \ /           \ /         \ /         \ /             \ /          \ /
;;      .             .           .           .               .            .

;; A057163-conjugates of the above ones:
(define (*A082353! s)
  (cond ((not (pair? s)) s)
        ((not (pair? (cdr s))) s)
        ((not (pair? (car s))) (robr! (swap! s)))
        (else (robr! s))
  )
)

(define (*A082354! s)
  (cond ((not (pair? s)) s)
        ((not (pair? (cdr s))) s)
        ((not (pair? (cddr s))) (swap! (robl! s)))
        (else (robl! s))
  )
)


(define (*A082355! s) ;; *A082351! applied to recursion case 1.
  (cond ((pair? s) (*A082355! (car s)) (*A082355! (cdr s)) (*A082351! s)))
  s
)


(define (*A082356! s) ;; *A082352! applied to recursion case 0.
  (cond ((pair? s) (*A082352! s) (*A082356! (car s)) (*A082356! (cdr s))))
  s
)

(define (*A082357! s) (*A082355! (*A057163! s)))
(define (*A082358! s) (*A057163! (*A082356! s)))

(define (*A082359! s) (*A074683! (*A057163! s)))
(define (*A082360! s) (*A057163! (*A074684! s)))




(define (*A074681! s) ;; *A074679! applied to recursion case 0.
  (cond ((pair? s) (*A074679! s) (*A074681! (car s)) (*A074681! (cdr s))))
  s
)

(define (*A074682! s) ;; *A074680! applied to recursion case 1.
  (cond ((pair? s) (*A074682! (car s)) (*A074682! (cdr s)) (*A074680! s)))
  s
)

(define (*A074683! s) ;; *A074679! applied to recursion case 1.
  (cond ((pair? s) (*A074683! (car s)) (*A074683! (cdr s)) (*A074679! s)))
  s
)


(define (*A074684! s) ;; *A074680! applied to recursion case 0.
  (cond ((pair? s) (*A074680! s) (*A074684! (car s)) (*A074684! (cdr s))))
  s
)


(define (*A074685! s) ;; *A074679! applied to recursion case 2.
  (cond ((pair? s) (*A074679! s) (*A074685! (cdr s))))
  s
)

(define (*A074686! s) ;; *A074680! applied to recursion case 3.
  (cond ((pair? s) (*A074686! (cdr s)) (*A074680! s)))
  s
)


(define (*A074687! s) ;; *A074679! applied to recursion case 4.
  (cond ((pair? s) (*A074687! (car s)) (*A074679! s) (*A074687! (cdr s))))
  s
)

(define (*A074688! s) ;; The inverse of *A074687!
  (cond ((pair? s) (*A074688! (cdr s)) (*A074680! s) (*A074688! (car s))))
  s
)


;; These seem to be duplicates of A071657 and A071658:

;; (define (gmrobr-recu-4! s) ;; *A074680! applied to recursion case 4.
;;   (cond ((pair? s) (gmrobr-recu-4! (car s)) (*A074680! s) (gmrobr-recu-4! (cdr s))))
;;   s
;; )

;; (define (gmrobr-recu-4-inv! s) ;; The inverse of the above one
;;   (cond ((pair? s) (gmrobr-recu-4-inv! (cdr s)) (*A074679! s) (gmrobr-recu-4-inv! (car s))))
;;   s
;; )


(define (*A074689! s) ;; i.e. "gmrobr-down-car"
  (cond ((pair? s) (*A074680! s) (*A074689! (car s))))
  s
)

(define (*A074690! s) ;; i.e. "down-car-gmrobl" 
  (cond ((pair? s) (*A074690! (car s)) (*A074679! s)))
  s
)

(define (*A069767 s)
  (cond ((not (pair? s)) s)
        (else (cons (cdr s) (*A069767 (car s))))
  )
)

;; I.e. *A069767! = (!SPINE *A069770!)
(define (*A069767! s)
  (cond ((pair? s)
              (*A069770! s)
              (*A069767! (cdr s))
        )
  )
  s
)


(define (*A069768 s)
  (cond ((not (pair? s)) s)
        (else (cons (*A069768 (cdr s)) (car s)))
  )
)

;; I.e. *A069768! = (!ENIPS *A069770!)
(define (*A069768! s)
  (cond ((pair? s)
              (*A069768! (cdr s))
              (*A069770! s)
        )
  )
  s
)

(define (*A069767old! s) ;; Was: SwapDownCar!
  (cond ((not (pair? s)))
        (else
              (swap! s)
              (*A069767! (cdr s)) ;; Really down car...
;; Or:
;;            (*A069767! (car s))
;;            (swap! s)
        )
  )
  s
)


(define (*A069768old! s) ;; Was: SwapDownCdr!
  (cond ((not (pair? s)))
        (else
              (swap! s)
              (*A069768! (car s)) ;; Really down cdr...
;; Or:
;;            (*A069768! (cdr s))
;;            (swap! s)
        )
  )
  s
)


(define (*A057163! s) ;; Was known as ReflectBinTree!
  (cond ((pair? s) (*A069770! s) (*A057163! (car s)) (*A057163! (cdr s))))
  s
)

(define (*A057501! s)
  (cond ((not (pair? s)))
        ((not (pair? (car s))) (swap! s))
        (else
              (robr! s)
              (*A057501! (cdr s))
        )
  )
  s
)

(define (*A057502! s)
  (cond ((not (pair? s)))
        ((not (pair? (cdr s))) (swap! s))
        (else
              (*A057502! (cdr s))
              (robl! s)
        )
  )
  s
)


(define (*A057501v2! s)
  (cond ((pair? s) (*A074680! s) (*A057501v2! (cdr s))))
  s
)

(define (*A057502v2! s)
  (cond ((pair? s) (*A057502v2! (cdr s)) (*A074679! s)))
  s
)


;; (0 1 3 2 7 8 4 5 6 17 18 20 21 22 9 10 11 12 13 14 15 19 16 45 46 48 49 50 54 55 ...)
(define (RobrDownCar! s)
  (cond ((not (pair? s)))
        ((not (pair? (car s))) (swap! s))
        (else
              (robr! s)
              (RobrDownCar! (car s))
        )
  )
  s
)

;; (0 1 3 2 6 7 8 4 5 14 15 16 17 18 19 20 22 9 10 21 11 12 13 37 38 39 40 41 42 43 ...)
(define (RobrDownCarInv! s)
  (cond ((not (pair? s)))
        ((not (pair? (cdr s))) (swap! s))
        (else
              (RobrDownCarInv! (car s))
              (robl! s)
        )
  )
  s
)


;; (0 1 3 2 6 7 8 4 5 15 14 16 17 18 19 20 21 9 10 22 11 12 13 39 40 41 37 38 43 ...)
(define (RoblDownCdr_et_Swap! s)
  (cond ((not (pair? s)))
        ((not (pair? (cdr s))) (swap! s))
        (else
              (robl! s)
              (RoblDownCdr_et_Swap! (cdr s))
        )
  )
  s
)

;; (0 1 3 2 7 8 4 5 6 17 18 20 21 22 10 9 11 12 13 14 15 16 19 45 46 48 49 50 54 55 ...)
(define (RoblDownCdr_et_SwapInv! s)
  (cond ((not (pair? s)))
        ((not (pair? (car s))) (swap! s))
        (else
              (RoblDownCdr_et_SwapInv! (cdr s))
              (robr! s)
        )
  )
  s
)


(define (*A069773! s) ;; Was RoblDownCar_et_Swap!
  (cond ((not (pair? s)))
        ((not (pair? (cdr s))) (swap! s))
        (else
              (robl! s)
              (*A069773! (car s))
        )
  )
  s
)

(define (*A069774! s) ;; Was RoblDownCar_et_SwapInv!
  (cond ((not (pair? s)))
        ((not (pair? (car s))) (swap! s))
        (else
              (*A069774! (car s))
              (robr! s)
        )
  )
  s
)


(define (*A069773v2! s)
  (cond ((pair? s) (*A074679! s) (*A069773v2! (car s))))
  s
)

(define (*A069774v2! s)
  (cond ((pair? s) (*A069774v2! (car s)) (*A074680! s)))
  s
)


(define (*A069773 s)
  (cond ((null? s) s)
        (else
          ((lambda (x y) (*A057163 (append (*A057163 y) (list (*A057163 x)))))
             (car s)
             (cdr s)
          )
        )
  )
)


(define (*A069775 s) ;; Check that this holds!
  (cond ((null? s) s)
        (else
          ((lambda (x y) (*A057163 (append (*A057163 y) (list (*A057163 x)))))
             (cdr s)
             (car s)
          )
        )
  )
)




;; --> (0 1 3 2 7 8 5 4 6 17 18 20 21 22 12 13 10 9 11 15 14 19 16 45 46 48 49 50 54 55 ...)
(define (*A071655! s)
  (cond ((not (pair? s)))
        ((not (pair? (car s))) (swap! s))
        (else
              (robr! s)
              (*A071655! (car s))
              (*A071655! (cdr s))
        )
  )
  s
)

;; --> (0 1 3 2 7 6 8 4 5 17 16 18 14 15 20 19 22 9 10 21 11 12 13 45 44 46 42 43 48 47 ...)
(define (*A071656! s)
  (cond ((not (pair? s)))
        ((not (pair? (cdr s))) (swap! s))
        (else
              (*A071656! (car s))
              (*A071656! (cdr s))
              (robl! s)
        )
  )
  s
)

;; --> (0 1 3 2 7 8 5 6 4 17 18 20 21 22 12 13 15 16 19 10 9 14 11 45 46 48 49 50 54 55 ...)
(define (*A071657! s)
  (cond ((not (pair? s)))
        ((not (pair? (car s))) (swap! s))
        (else
              (*A071657! (car s))
              (robr! s)
              (*A071657! (cdr s))
        )
  )
  s
)


(define (*A071657v2! s)
  (cond ((not (pair? s)))
        (else
              (*A071657v2! (car s))
              (cond ((not (pair? (car s))) (swap! s))
                    (else (robr! s)
                          (*A071657v2! (cdr s))
                    )
              )
        )
  )
  s
)

;; --> (0 1 3 2 8 6 7 4 5 20 19 22 14 15 21 16 17 9 10 18 11 12 13 64 53 55 51 52 54 60 ...)
(define (*A071658! s)
  (cond ((not (pair? s)))
        ((not (pair? (cdr s))) (swap! s))
        (else
              (*A071658! (cdr s))
              (robl! s)
              (*A071658! (car s))
        )
  )
  s
)


;; --> (0 1 3 2 7 8 4 5 6 17 18 20 21 22 10 9 11 12 13 14 15 19 16 45 46 48 49 50 54 55 ...)
(define (cdr_robr_car! s)
  (cond ((not (pair? s)))
        ((not (pair? (car s))) (swap! s))
        (else
              (cdr_robr_car! (cdr s))
              (robr! s)
              (cdr_robr_car! (car s))
        )
  )
  s
)

;; --> (0 1 3 2 6 7 8 4 5 15 14 16 17 18 19 20 22 9 10 21 11 12 13 39 40 41 37 38 43 42 ...)
(define (car_robl_cdr! s)
  (cond ((not (pair? s)))
        ((not (pair? (cdr s))) (swap! s))
        (else
              (car_robl_cdr! (car s))
              (robl! s)
              (car_robl_cdr! (cdr s))
        )
  )
  s
)

;; --> (0 1 3 2 7 8 4 6 5 17 18 20 21 22 9 10 14 19 16 11 12 15 13 45 46 48 49 50 54 55 ...)
(define (car_robr_car! s)
  (cond ((not (pair? s)))
        ((not (pair? (car s))) (swap! s))
        (else
              (car_robr_car! (car s))
              (robr! s)
              (car_robr_car! (car s))
        )
  )
  s
)

;; --> (0 1 3 2 6 8 7 4 5 14 15 19 20 22 16 21 18 9 10 17 11 12 13 37 38 39 40 41 51 ...)
(define (car_robl_car! s)
  (cond ((not (pair? s)))
        ((not (pair? (cdr s))) (swap! s))
        (else
              (car_robl_car! (car s))
              (robl! s)
              (car_robl_car! (car s))
        )
  )
  s
)


;; --> (0 1 3 2 7 8 5 4 6 17 18 20 21 22 13 12 10 9 11 15 14 16 19 45 46 48 49 50 54 55 ...)
(define (cdr_robr_cdr! s)
  (cond ((not (pair? s)))
        ((not (pair? (car s))) (swap! s))
        (else
              (cdr_robr_cdr! (cdr s))
              (robr! s)
              (cdr_robr_cdr! (cdr s))
        )
  )
  s
)

;; --> (0 1 3 2 7 6 8 4 5 17 16 18 15 14 20 19 21 9 10 22 11 12 13 45 44 46 42 43 48 47 ...)
(define (cdr_robl_cdr! s)
  (cond ((not (pair? s)))
        ((not (pair? (cdr s))) (swap! s))
        (else
              (cdr_robl_cdr! (cdr s))
              (robl! s)
              (cdr_robl_cdr! (cdr s))
        )
  )
  s
)

;; (0 1 3 2 7 8 4 6 5 17 18 20 21 22 10 9 14 16 19 11 12 15 13 45 46 48 49 50 54 55 ...)
(define (*A071659! s)
  (cond ((not (pair? s)))
        ((not (pair? (car s))) (swap! s))
        (else
              (*A071659! (car s))
              (*A071659! (cdr s))
              (robr! s)
        )
  )
  s
)


;; (0 1 3 2 6 8 7 4 5 15 14 19 20 22 16 21 17 9 10 18 11 12 13 39 41 40 37 38 52 ...)
(define (*A071660! s)
  (cond ((not (pair? s)))
        ((not (pair? (cdr s))) (swap! s))
        (else
              (robl! s)
              (*A071660! (car s))
              (*A071660! (cdr s))
        )
  )
  s
)


;; Not a bijection: (0 1 2 2 4 5 4 4 6 9 10 11 12 13 9 10 9 9 11 14 14 16 19 23 ...)
(define (robr_down_cdr! s)
  (cond ((not (pair? s)))
        ((not (pair? (car s))))
        (else
              (robr! s)
              (robr_down_cdr! (cdr s))
        )
  )
  s
)

(define (*A057509! s) ;; Was: Rol!
  (cond ((pair? s)
              (swap! s)
              (*A057501! s)
        )
  )
  s
)

(define (*A057510! s) ;; Was: Ror!
  (cond ((pair? s)
              (*A057502! s)
              (swap! s)
        )
  )
  s
)


;;  RECURSION_SCHEMA_2(gms_A057509,gms_A072796);
;;  RECURSION_SCHEMA_3(gms_A057510,gms_A072796);

(define (*A057509v2! s)
  (cond ((pair? s)
            (*A072796! s)
            (*A057509v2! (cdr s))
        )
  )
  s
)

(define (*A057510v2! s)
  (cond ((pair? s)
            (*A057510v2! (cdr s))
            (*A072796! s)
        )
  )
  s
)

;; Leaves the first car-branch intact.
(define (*Ajoku1! s)
  (cond ((pair? s)
            (*A072796! s)
            (*A057509! s)
        )
  )
  s
)

;; Leaves the first car-branch intact. Inverse of *Ajoku1!
(define (*Ajoku2! s)
  (cond ((pair? s)
            (*A057510! s)
            (*A072796! s)
        )
  )
  s
)

;; These correspondingly leave the main-cdr stem intact, i.e.
;; they affect only the car-side:
(define *Ajoku3! (compose-funs *A057163! *Ajoku1! *A057163!))
(define *Ajoku4! (compose-funs *A057163! *Ajoku2! *A057163!))

;; Let's continue tomorrow...

(define (*Ajoku5! s)
  (cond ((pair? s)
              (*Ajoku3! s)
              (*Ajoku5! (cdr s))
        )
  )
  s
)

(define (*Ajoku5v2! s)
  (for-each *A072797! s)
  s
)


(define (*A089867! s)
  (if (pair? s) (*A085169! (car s)))
  s
)

(define (*A089868! s)
  (if (pair? s) (*A085170! (car s)))
  s
)

(define (*A089869! s)
  (for-each *A085169! s)
  s
)

(define (*A089870! s)
  (for-each *A085170! s)
  s
)

(define (A057511_sans_top! s) ;; How to implement like the others?
  (cond ((pair? s)
              (*A057509! (car s))
              (A057511_sans_top! (car s))
              (A057511_sans_top! (cdr s))
        )
  )
  s
)

(define (A057511v2! s) (*A057509! (A057511_sans_top! s)))


(define (DeepRor_sans_top! s) ;; How to implement like the others?
  (cond ((pair? s)
              (*A057510! (car s))
              (DeepRor_sans_top! (car s))
              (DeepRor_sans_top! (cdr s))
        )
  )
  s
)

(define (DeepRorv2! s) (*A057510! (DeepRor_sans_top! s)))



(define (*A057511! s)
  (cond ((pair? s)
              (*A072796! s)
              (*A057511! (car s))
              (*A057511! (cdr s))
        )
  )
  s
)


(define (*A057512! s)
  (cond ((pair? s)
              (*A057512! (car s))
              (*A057512! (cdr s))
              (*A072796! s)
        )
  )
  s
)

;; (define DeepRol! *A057511!)
;; (define DeepRor! *A057512!)
 
;; See http://www.iki.fi/~kartturi/software/stacks.lsp

(define (*A057508! s)
  (cond ((pair? s)
              (*A057508! (cdr s))
              (*A057509! s)
        )
  )
  s
)

(define (Rev2! s)
  (cond ((pair? s)
              (*A057510! s)
              (Rev2! (cdr s))
        )
  )
  s
)



(define (*A057164! s) ;; Was DeepRev1!
  (cond ((pair? s)
              (*A057164! (car s))
              (*A057164! (cdr s))
              (*A057509! s)
        )
  )
  s
)



(define (*A069775! s)  ;; Was: RolCarSide!
  (cond ((not (pair? s)))       ;; Do nothing.
        (else
              (swap! s)
              (*A069773! s)
        )
  )
  s
)

(define (*A069776! s) ;; Was: RorCarSide!
  (cond ((not (pair? s)))       ;; Do nothing.
        (else
              (*A069774! s)
              (swap! s)
        )
  )
  s
)



(define (*A069769! s)          ;; Was: Rev1CarSide!
  (cond ((not (pair? s)))       ;; Do nothing
        (else
              (*A069769! (car s))
              (*A069775! s)
        )
  )
  s
)


(define (*A069787! s) ;; Was: DeepRev1CarSide!
  (cond ((not (pair? s)))       ;; Do nothing
        (else
              (*A069787! (car s))
              (*A069787! (cdr s))
              (*A069775! s)
        )
  )
  s
)

(define (*A057161! s) ;; Was: RotateTriangularization!
  (*A069769! s)
  (*A069767! s)
  s
)

(define (*A057162! s) ;; Was: RotateTriangularizationInv!
  (*A057508! s)
  (*A069768! s)
  s
)


(define (append-A057163-conjugated a b)
  (*A057163! (append (*A057163 a) (*A057163 b)))
)

(define (F_for_A057162 x y) (append-A057163-conjugated y (cons '() x)))

(define (*A057162v2 s) (fold-right F_for_A057162 '() s))
;; Implies that:
(define *A057162v2! (!ENIPS *A069773!))
(define *A057161v2! (!SPINE *A069774!))

;; Or equally:
(define (*A057162v3 s)
  (fold-right
    (lambda (x y) (*A057163 (append (*A057163 y) (list (*A057163 x)))))
    '()
    s
  )
)


(define (*A069888 s) (*A057501 (*A057164 s)))
(define (*A069889 s) (*A057164 (*A057501 s)))

(define (*A069888! s) ;; Was DeepReverse_et_RotateHandshakes!
  (*A057164! s)
  (*A057501! s)
  s
)

(define (*A069889! s) ;; Was RotateHandshakes_et_DeepReverse!
  (*A057501! s)
  (*A057164! s)
  s
)


;; Not a bijection!
;; -> (0 1 2 2 4 4 4 4 5 9 9 9 9 9 9 9 9 9 10 10 11 12 13 23 23 ...)

(define (robl-swap-robr! s)
  (cond ((not (pair? s)))
        ((not (pair? (cdr s))) (swap! s))
        (else
              (robl! s)
              (robl-swap-robr! (car s))
              (robr! s)
              (robl-swap-robr! (cdr s))
        )
  )
  s
)


;; But these are:


(define (*A073194! s) ;; inverse of *A073195
  (cond ((pair? s)
               (*A072797! s)
               (*A073194! (car s))
               (*A072796! s)
               (*A073194! (cdr s))
        )
  )
  s
)


(define (*A073195! s) ;; inverse of *A073194
  (cond ((pair? s)
               (*A073195! (cdr s))
               (*A072796! s)
               (*A073195! (car s))
               (*A072797! s)
        )
  )
  s
)


(define (*A073196! s) ;; inverse of *A073197
  (cond ((pair? s)
               (*A072797! s)
               (*A073196! (car s))
               (*A072796! s)
               (*A073196! (car s))
        )
  )
  s
)



(define (*A073197! s) ;; inverse of *A073196
  (cond ((pair? s)
               (*A073197! (car s))
               (*A072796! s)
               (*A073197! (car s))
               (*A072797! s)
        )
  )
  s
)



(define (*A073198! s) ;; inverse of *A073199
  (cond ((pair? s)
               (*A072797! s)
               (*A073198! (cdr s))
               (*A072796! s)
               (*A073198! (cdr s))
        )
  )
  s
)



(define (*A073199! s) ;; inverse of *A073198
  (cond ((pair? s)
               (*A073199! (cdr s))
               (*A072796! s)
               (*A073199! (cdr s))
               (*A072797! s)
        )
  )
  s
)


;; car/cdr-flipped conjugates for above ones:

(define (*A073205! s) ;; inverse of *A073206
  (cond ((pair? s)
               (*A072796! s)
               (*A073205! (cdr s))
               (*A072797! s)
               (*A073205! (car s))
        )
  )
  s
)


(define (*A073206! s) ;; inverse of *A073205
  (cond ((pair? s)
               (*A073206! (car s))
               (*A072797! s)
               (*A073206! (cdr s))
               (*A072796! s)
        )
  )
  s
)


(define (*A073207! s) ;; inverse of *A073208
  (cond ((pair? s)
               (*A072796! s)
               (*A073207! (cdr s))
               (*A072797! s)
               (*A073207! (cdr s))
        )
  )
  s
)



(define (*A073208! s) ;; inverse of *A073207
  (cond ((pair? s)
               (*A073208! (cdr s))
               (*A072797! s)
               (*A073208! (cdr s))
               (*A072796! s)
        )
  )
  s
)



(define (*A073209! s) ;; inverse of *A073210
  (cond ((pair? s)
               (*A072796! s)
               (*A073209! (car s))
               (*A072797! s)
               (*A073209! (car s))
        )
  )
  s
)


(define (*A073210! s) ;; inverse of *A073209
  (cond ((pair? s)
               (*A073210! (car s))
               (*A072797! s)
               (*A073210! (car s))
               (*A072796! s)
        )
  )
  s
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                 ;;
;;  Some missing or otherwise interesting automorphisms from the   ;;
;;  beginning of the table A073200.                                ;;
;;                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (*A073280! s) (*A069770! (*A057163! s))) ;; involution. Row 9.

(define (*A073282! s) (*A072796! (*A057163! s))) ;; inv of A073283. Row 13

(define (*A073283! s) (*A057163! (*A072796! s))) ;; inv of A073282. Row 19

(define (*A073284! s) ;; inverse of A073285. Row 20.
   (cond ((pair? s)
               (*A073284! (car s))
               (*A072796! s)
               (*A073284! (cdr s))
         )
   )
   s
)


(define (*A073285! s) ;; inverse of A073284. Row ???
   (cond ((pair? s)
               (*A073285! (cdr s))
               (*A072796! s)
               (*A073285! (car s))
         )
   )
   s
)


(define (*A073286! s) (*A069770! (*A069767! s))) ;; inverse of A073287. Row 41.

(define (*A073287! s) (*A069768! (*A069770! s))) ;; inverse of A073286. Row 69.


(define (*A073288! s) ;; inverse of A073289. Row 416
   (cond ((pair? s)
             (*A073286! s)
             (*A073288! (cdr s))
         )
   )
   s
)


(define (*A073289! s) ;; inverse of A073288. Row 696.
   (cond ((pair? s)
             (*A073289! (cdr s))
             (*A073287! s)
         )
   )
   s
)

;; These correspondences explain why A073288/9 has A023359
;; as its fix count sequence:
;; Similarly, when we do (map foo! s) with any Catalan automorphism
;; that has A019590 as its fix count sequence (i.e. that fixes
;; only () and (()) ) then we obtain A000045, the Fibonacci numbers.
;; (In general, INVERT transform of the fix-counts of that gm).

(define (*A073288v2! s) (map *A069767! s))
(define (*A073289v2! s) (map *A069768! s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define *A082325v2! (compose-funs *A057163! *A057511! *A057163!))
(define *A082326v2! (compose-funs *A057163! *A057512! *A057163!))

(define (*A082325! s) ;; inverse of A082326. Row 1712
   (cond ((pair? s)
               (*A072797! s)
               (*A082325! (car s))
               (*A082325! (cdr s))
         )
   )
   s
)



(define (*A082326! s) ;; inverse of A082325. Row 1714
   (cond ((pair? s)
               (*A082326! (car s))
               (*A082326! (cdr s))
               (*A072797! s)
         )
   )
   s
)


(define (*A082337! s)
  (cond ((pair? s)
              (*A072797! s)
              (*A082337! (car s))
        )
  )
  s
)


(define (*A082338! s)
  (cond ((pair? s)
              (*A082338! (car s))
              (*A072797! s)
        )
  )
  s
)

(define (*A082339! s)
  (cond ((pair? s)
              (*A072797! s)
              (*A082339! (cdr s))
        )
  )
  s
)


(define (*A082340! s)
  (cond ((pair? s)
              (*A082340! (cdr s))
              (*A072797! s)
        )
  )
  s
)


(define (*A082341! s) ;; inverse of A082342. Row ???
   (cond ((pair? s)
               (*A082341! (car s))
               (*A072797! s)
               (*A082341! (cdr s))
         )
   )
   s
)


(define (*A082342! s) ;; inverse of A082341. Row ???
   (cond ((pair? s)
               (*A082342! (cdr s))
               (*A072797! s)
               (*A082342! (car s))
         )
   )
   s
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (*A069768v2! s) ;; Check!
   (cond ((pair? s)
               (*A069767! s)
               (*A069768v2! (car s))
               (*A069768v2! (cdr s))
         )
   )
   s
)



(define (*A069767v2! s) ;; Check!
   (cond ((pair? s)
               (*A069767v2! (car s))
               (*A069767v2! (cdr s))
               (*A069768! s)
         )
   )
   s
)



(define (*A069767v3! s) ;; Check!
   (cond ((pair? s)
               (*A069768! s)
               (*A069767v3! (car s))
               (*A069767v3! (cdr s))
         )
   )
   s
)



(define (*A069768v3! s) ;; Check!
   (cond ((pair? s)
               (*A069768v3! (car s))
               (*A069768v3! (cdr s))
               (*A069767! s)
         )
   )
   s
)

;;;;


(define (*A082345! s) ;; inverse of A082346. Row 66.
   (cond ((pair? s)
               (*A069767! s)
               (*A082345! (cdr s))
         )
   )
   s
)



(define (*A082346! s) ;; inverse of A082345. Row 88.
   (cond ((pair? s)
               (*A082346! (cdr s))
               (*A069768! s)
         )
   )
   s
)



(define (*A082347! s) ;; inverse of A082348. Row ???
   (cond ((pair? s)
               (*A069768! s)
               (*A082347! (cdr s))
         )
   )
   s
)



(define (*A082348! s) ;; inverse of A082347. Row ???
   (cond ((pair? s)
               (*A082348! (cdr s))
               (*A069767! s)
         )
   )
   s
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *A125981v2  (NEPEED *A057501)) ;; Check! Strange! = (KROF (ENIPS^-1 *A057501)) ???
(define *A125982v2! (!DEEPEN *A057502!))

(define *A074684v3! (!DEEPEN *A057501!)) ;; As it should be.
(define *A074683v3! (!NEPEED *A057502!))


;; Compute cycles, fixed points, etc.
;; Find KROF^-1 and FORK^-1, i.e. SPINE^-1(*A057511) and ENIPS^-1(*A057512)
(define *A130919! (!NEPEED *A057511!))
(define *A130919v2! (!DEEPEN *A057511!))
(define *A130920! (!DEEPEN *A057512!))
(define *A130920v2! (!NEPEED *A057512!))

(define (*A130921! s) (*A074684! (*A057164! s)))
(define (*A130922! s) (*A057164! (*A074683! s)))

;; Justification: *A120705 = (FORK *A130923)
(define (*A130923v2! s)
  (cond ((pair? s)
           (*A120705! s)
           (*A120706! (car s))
           (*A120706! (cdr s))
        )
  )
  s
)
;; converts to: (note that the car-branch is eliminated, but cdr-branch gets just more complex:)
;; Or? Is this in turn ENIPS or SPINE of something?
;; How with the infinite labeled trees? (then we can ignore the possibility of
;; executing the last clause of *A074679/*A074680.)
(define (*A130923! s)
  (cond ((pair? s)
           (*A074680! s) ;; From *A120705!
           (cond ((pair? (cdr s))
                    (*A120705! (cddr s))
                    (*A120706! (cadr s))
                    (*A120706! (cdr s))
                 )
           )
        )
  )
  s
)


;; *A120706 = (KROF *A130924)
(define (*A130924! s)
  (cond ((pair? s)
           (*A120705! (cdr s))
           (*A120705! (car s))
           (*A120706! s)
        )
  )
  s
)

;; Justification: *A120706 = (FORK *A130925)
;; Check A057163-conjugateness...
(define (*A130925! s)
  (cond ((pair? s)
           (*A074679! s) ;; From *A120705!
           (cond ((pair? (car s))
                    (*A120706! (caar s))
                    (*A120705! (cdar s))
                    (*A120705! (car s))
                 )
           )
        )
  )
  s
)

;; *A120705 = (KROF *A130926)
(define (*A130926! s)
  (cond ((pair? s)
           (*A120706! (cdr s))
           (*A120706! (car s))
           (*A120705! s)
        )
  )
  s
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definec (**A089840c n) ;; c for constructive variants, for checking.
   (case n
     ((0)  (lambda (s) s)) ;; *A001477! (identity permutation)
     ((1)  *A069770)      ;; involution
     ((2)  *A072796)      ;; involution
     ((3)  *A089850)      ;; involution
     ((4)  *A089851)      ;; 3-cycles
     ((5)  *A089852)      ;; involution
     ((6)  *A089853)      ;; 3-cycles
     ((7)  *A089854)      ;; involution
     ((8)  *A072797)      ;; involution
     ((9)  *A089855)      ;; 3-cycles
     ((10) *A089856)      ;; involution
     ((11) *A089857)      ;; 3-cycles
     ((12) *A074679)      ;; cycles not limited
     ((13) *A089858)      ;; cycles not limited
     ((14) *A073269)      ;; cycles not limited
     ((15) *A089859)      ;; 4-cycles
     ((16) *A089860)      ;; cycles not limited
     ((17) *A074680)      ;; cycles not limited
     ((18) *A089861)      ;; cycles not limited
     ((19) *A073270)      ;; cycles not limited
     ((20) *A089862)      ;; cycles not limited
     ((21) *A089863)      ;; 4-cycles
     ((3608) *A129607)
     ((3613) *A129605)
     ((3617) *A129606)
     ((3702) *A082354)
     ((3886) *A082353)
     ((4069) *A082351)
     ((4207) *A089865)
     ((4253) *A082352)
     ((4299) *A089866)
;;   ((1654023) *A073281)
;;   ((1654694) *A089864) ;; involution
;;   ((1654720) *A129604)
     (else (error "Non-recursive automorphism A089840[" n "] not supported!"))
   )
)

;; Yes, we can cache functions (and lambda-forms), one of the reasons Scheme is so good!
(definec (**A089840 n)
   (case n
     ((0)  (lambda (s) s)) ;; *A001477! (identity permutation)
     ((1)  *A069770!)      ;; involution
     ((2)  *A072796!)      ;; involution
     ((3)  *A089850!)      ;; involution
     ((4)  *A089851!)      ;; 3-cycles
     ((5)  *A089852!)      ;; involution
     ((6)  *A089853!)      ;; 3-cycles
     ((7)  *A089854!)      ;; involution
     ((8)  *A072797!)      ;; involution
     ((9)  *A089855!)      ;; 3-cycles
     ((10) *A089856!)      ;; involution
     ((11) *A089857!)      ;; 3-cycles
     ((12) *A074679!)      ;; cycles not limited
     ((13) *A089858!)      ;; cycles not limited
     ((14) *A073269!)      ;; cycles not limited
     ((15) *A089859!)      ;; 4-cycles
     ((16) *A089860!)      ;; cycles not limited
     ((17) *A074680!)      ;; cycles not limited
     ((18) *A089861!)      ;; cycles not limited
     ((19) *A073270!)      ;; cycles not limited
     ((20) *A089862!)      ;; cycles not limited
     ((21) *A089863!)      ;; 4-cycles
     ((169) *A129611!)
     ((251) *A129612!)
     ((253) *A123503!)
     ((258) *A123499!)
     ((264) *A123500!)
     ((3608) *A129607!)
     ((3613) *A129605!)
     ((3617) *A129606!)
     ((3702) *A082354!)
     ((3886) *A082353!)
     ((4069) *A082351!)
     ((4207) *A089865!)
     ((4253) *A082352!)
     ((4299) *A089866!)
     ((65167) *A129609!)
     ((65352) *A129610!)
     ((65518) *A123495!)
     ((65796) *A123496!)
     ((79361) *A123492!)
     ((1654023) *A073281!) ;; involution
     ((1654694) *A089864!) ;; involution
     ((1654720) *A129604!) ;; involution
     ((1655089) *A123497!)
     ((1654249) *A123498!)
     ((1653002) *A123695!)
     ((1653063) *A123696!)
     ((1783367) *A123713!)
     ((1786785) *A123714!)
     (else (error "Destructive version of non-recursive automorphism A089840[" n "] not implemented!"))
   )
)

;; Here are the 5 A-numbers you requested: A122200 --- A122204.

;; Here are the 50 A-numbers you requested: A122282 --- A122331.
;; Here are the  2 A-numbers you requested: A122332 --- A122333.
;; Here are the 10 A-numbers you requested: A122334 --- A122343.
;; Here are the  8 A-numbers you requested: A122344 --- A122351.
;; Here are the 12 A-numbers you requested: A122353 --- A122364.

;; A122299 is free.

;; I.e. 70 A-numbers: A122282 --- A122351.


;; Yes, we can cache functions (and lambda-forms), which is one of the reasons why Scheme is so good!

;; First-order recursion schemes:
(definec (**A122200 n) (!RIBS   (**A089840 n))) ;; Give row 1, i.e. row 7 of A122203 and A122204.

(definec (**A122201 n) (!FORK   (**A089840 n)))
(definec (**A122202 n) (!KROF   (**A089840 n)))
(definec (**A122203 n) (!SPINE  (**A089840 n)))
(definec (**A122204 n) (!ENIPS  (**A089840 n)))

(definec (**A122283 n) (!DEEPEN (**A089840 n)))
(definec (**A122284 n) (!NEPEED (**A089840 n)))

;; Second-order recursion schemes:
(definec (**A122285 n) (!ENIPS (**A122203 n)))
(definec (**A122286 n) (!SPINE (**A122204 n)))
(definec (**A122287 n) (!FORK  (**A122204 n)))
(definec (**A122288 n) (!KROF  (**A122203 n)))

;; As well. Give the examples for rows 1 (A122351, an involution) and 2: A122363 and A122364.
(definec (**A122289 n) (!FORK  (**A122201 n)))
(definec (**A122290 n) (!KROF  (**A122202 n)))

;; Constructive variants, for checking that we got everything all right:
(definec (**A122200c n) (RIBS   (**A089840c n))) ;; Give row 1, i.e. row 7 of A122203 and A122204.

(definec (**A122201c n) (FORK   (**A089840c n)))
(definec (**A122202c n) (KROF   (**A089840c n)))
(definec (**A122203c n) (SPINE  (**A089840c n)))
(definec (**A122204c n) (ENIPS  (**A089840c n)))

(definec (**A122283c n) (DEEPEN (**A089840c n)))
(definec (**A122284c n) (NEPEED (**A089840c n)))

;; Second-order recursion schemes:
(definec (**A122285c n) (ENIPS (**A122203c n)))
(definec (**A122286c n) (SPINE (**A122204c n)))
(definec (**A122287c n) (FORK  (**A122204c n)))
(definec (**A122288c n) (KROF  (**A122203c n)))

;; As well. Give the examples for rows 1 (A122351, an involution) and 2: A122363 and A122364.
(definec (**A122289c n) (FORK  (**A122201c n)))
(definec (**A122290c n) (KROF  (**A122202c n)))

;; "New" recursion schemes, June 9, 2007:

(definec (**A130400 n) (!INORDER (**A089840 n)))
(definec (**A130401 n) (!REDRONI (**A089840 n)))
(definec (**A130402 n) (!ENIPS (compose-funs *A057163! (**A122203 n) *A057163!)))
(definec (**A130403 n) (!SPINE (compose-funs *A057163! (**A122204 n) *A057163!)))

(definec (**A130400c n) (INORDER (**A089840c n)))
(definec (**A130401c n) (REDRONI (**A089840c n)))
(definec (**A130402c n) (ENIPS (compose-funs *A057163 (**A122203c n) *A057163)))
(definec (**A130403c n) (SPINE (compose-funs *A057163 (**A122204c n) *A057163)))

;; A130918 - A131017 reserved for us!

;; Simple self-inverse permutation of natural numbers: List each block of A000108(n) numbers,
;; from A014137(n-1) .. to A014138(n-1) in reversed order. Cf. A054429
(define (A130918 n) (if (zero? n) n (- (A014138 (- (A072643 n) 1)) (A082853 n))))

;; Call e.g. as:
;; (with-output-to-file "./seqs/check-tables.txt" (lambda () (check-them-all 23714)))

(define (scan-them-all upto-n)
 (let ((rows (append! (iota0 21)
                      (list 169 251 253 258 264 3608 3613 3617 3702 3886 4069 4207 4253 4299
                            65167 65352 65518 65796 79361
                            1653002 1653063 1654023 1654249 1654694 1654720 1655089 1783367 1786785
                      )
             )
      ))

   (format #t "Scanning table A089840:\n")
   (print-matching-anums "A089840" **A089840 rows upto-n)

   (format #t "Scanning table A122200:\n")
   (print-matching-anums "A122200" **A122200 rows upto-n)

   (format #t "Scanning table A122201:\n")
   (print-matching-anums "A122201" **A122201 rows upto-n)

   (format #t "Scanning table A122202:\n")
   (print-matching-anums "A122202" **A122202 rows upto-n)

   (format #t "Scanning table A122203:\n")
   (print-matching-anums "A122203" **A122203 rows upto-n)

   (format #t "Scanning table A122204:\n")
   (print-matching-anums "A122204" **A122204 rows upto-n)

   (format #t "Scanning table A122283:\n")
   (print-matching-anums "A122283" **A122283 rows upto-n)

   (format #t "Scanning table A122284:\n")
   (print-matching-anums "A122284" **A122284 rows upto-n)

   (format #t "Scanning table A122285:\n")
   (print-matching-anums "A122285" **A122285 rows upto-n)

   (format #t "Scanning table A122286:\n")
   (print-matching-anums "A122286" **A122286 rows upto-n)

   (format #t "Scanning table A122287:\n")
   (print-matching-anums "A122287" **A122287 rows upto-n)

   (format #t "Scanning table A122288:\n")
   (print-matching-anums "A122288" **A122288 rows upto-n)

   (format #t "Scanning table A122289:\n")
   (print-matching-anums "A122289" **A122289 rows upto-n)

   (format #t "Scanning table A122290:\n")
   (print-matching-anums "A122290" **A122290 rows upto-n)

   (format #t "Scanning table A130400:\n")
   (print-matching-anums "A130400" **A130400 rows upto-n)

   (format #t "Scanning table A130401:\n")
   (print-matching-anums "A130401" **A130401 rows upto-n)

   (format #t "Scanning table A130402:\n")
   (print-matching-anums "A130402" **A130402 rows upto-n)

   (format #t "Scanning table A130403:\n")
   (print-matching-anums "A130403" **A130403 rows upto-n)

 )
)


(define (check-them-all upto_n)
 (let ((upto_row 21))
  (format #t "Comparing A122201 against A122202, should produce differences.\n")
  (check-equiv **A122201 **A122202 upto_row upto_n)

  (format #t "Comparing A122289 against A122290, should produce differences.\n")
  (check-equiv **A122289c **A122290c upto_row upto_n)

  (format #t "Checking A089840.\n")
  (check-equiv **A089840 **A089840c upto_row upto_n)

  (format #t "Checking A122200.\n")
  (check-equiv **A122200 **A122200c upto_row upto_n)

  (format #t "Checking A122201.\n")
  (check-equiv **A122201 **A122201c upto_row upto_n)

  (format #t "Checking A122202.\n")
  (check-equiv **A122202 **A122202c upto_row upto_n)

  (format #t "Checking A122203.\n")
  (check-equiv **A122203 **A122203c upto_row upto_n)

  (format #t "Checking A122204.\n")
  (check-equiv **A122204 **A122204c upto_row upto_n)

  (format #t "Checking A122283.\n")
  (check-equiv **A122283 **A122283c upto_row upto_n)

  (format #t "Checking A122284.\n")
  (check-equiv **A122284 **A122284c upto_row upto_n)

  (format #t "Checking A122285.\n")
  (check-equiv **A122285 **A122285c upto_row upto_n)

  (format #t "Checking A122286.\n")
  (check-equiv **A122286 **A122286c upto_row upto_n)

  (format #t "Checking A122287.\n")
  (check-equiv **A122287 **A122287c upto_row upto_n)

  (format #t "Checking A122288.\n")
  (check-equiv **A122288 **A122288c upto_row upto_n)

  (format #t "Checking A122289.\n")
  (check-equiv **A122289 **A122289c upto_row upto_n)

  (format #t "Checking A122290.\n")
  (check-equiv **A122290 **A122290c upto_row upto_n)

  (format #t "Checking A130400.\n")
  (check-equiv **A130400 **A130400c upto_row upto_n)

  (format #t "Checking A130401.\n")
  (check-equiv **A130401 **A130401c upto_row upto_n)

  (format #t "Checking A130402.\n")
  (check-equiv **A130402 **A130402c upto_row upto_n)

  (format #t "Checking A130403.\n")
  (check-equiv **A130403 **A130403c upto_row upto_n)

  (format #t "Comparing A130400 against A130401, should produce differences.\n")
  (check-equiv **A130400 **A130401 upto_row upto_n)

  (format #t "Comparing A130400 against A122201, should produce differences.\n")
  (check-equiv **A130400 **A122201 upto_row upto_n)

  (format #t "Comparing A130400 against A122202, should produce differences.\n")
  (check-equiv **A130400 **A122202 upto_row upto_n)

  (format #t "Comparing A130401 against A122201, should produce differences.\n")
  (check-equiv **A130401 **A122201 upto_row upto_n)

  (format #t "Comparing A130401 against A122202, should produce differences.\n")
  (check-equiv **A130401 **A122202 upto_row upto_n)

;; Find involutions, if any:
  (format #t "Comparing A130400 against A130401, should produce differences.\n")
  (check-equiv **A130400 **A130401 upto_row upto_n)

  (format #t "Comparing A130402 against A130403, should produce differences.\n")
  (check-equiv **A130402 **A130403 upto_row upto_n)

  (format #t "Comparing A122285 against A122286, should produce differences.\n")
  (check-equiv **A122285 **A122286 upto_row upto_n)

  (format #t "Comparing A122287 against A122288, should produce differences.\n")
  (check-equiv **A122287 **A122288 upto_row upto_n)

 )
)


(define (check-equiv tab1fun tab2fun row_upto n_upto)
   (let outloop ((row 0))
       (cond ((<= row row_upto)
                (let inloop ((fun1 (catfun1 (tab1fun row)))
                             (fun2 (catfun1 (tab2fun row)))
                             (n 0)
                            )
                    (cond ((> n n_upto)
                              (format #t "Row ~a checked up to ~a, OK.\n" row n_upto)
                              (outloop (+ 1 row))
                          )
                          ((= (fun1 n) (fun2 n)) (inloop fun1 fun2 (+ 1 n))) ;; So far OK.
                          (else
                              (format #t
 "Row ~a, (fun1 ~a)=~a different from (fun2 ~a)=~a. Skipping the rest.\n"
                                    row n (fun1 n) n (fun2 n)
                              )
                              (outloop (+ 1 row))
                          )
                    )
                )
             )
       )
   )
)


(define (print-matching-anums tab1name tab1fun rows upto-n)
   (let outloop ((rows rows))
       (cond ((null? rows) #f)
             (else ;; i.e. (not (null? rows))
                (let ((fun-searched (catfun1 (tab1fun (car rows)))))
                   (cond ((find-matching-anum fun-searched upto-n)
                           =>
                              (lambda (Asym)
                                (format #t "~a[~a] = ~a ?\n"
                                        tab1name (car rows) Asym
                                )
                              )
                         )
                   )
                   (outloop (cdr rows))
                )
             )
       )
   )
)





(define *A122351! (**A122289 1))
(define *A122351!v2 (**A122290 1))
(define *A122363! (**A122289 2))
(define *A122364! (**A122290 2))

(define *A122282!   (**A122200 1))

;; A130339 - A130403

(define *A069767v4! (**A122203 1))
(define *A057509v3! (**A122203 2))
(define *A122282v2! (**A122203 7))
(define *A082339v2! (**A122203 8))
(define *A074685v2! (**A122203 12))
(define *A057501v3! (**A122203 17))
(define *A069770v2! (**A122203 21)) ;; (!SPINE *A089863!)


(define (*A130339! s) ;; Swap the two rightmost if the length is even.
  (if (even? (length s)) (*A129608! s))
  s
)
(define *A130339v2! (!SPINE *A129608!))
(define *A130339v3! (!ENIPS *A129608!))

;; *A130340 = ENIPS what?
(define (*A130340! s) ;; Swap the two leftmost if the length is even. (A057508 A130339 A057508) (A057164 A130339 A057164)
  (if (even? (length s)) (*A072796! s))
  s
)




(define *A130341! (**A122203 3))
(define *A130342! (**A122204 3))

(define *A130343! (**A122203 4))
(define *A130344! (**A122204 6))

(define *A130345! (**A122203 5))
(define *A130346! (**A122204 5))

(define *A130347! (**A122203 6))
(define *A130348! (**A122204 4))

(define *A130349! (**A122203 9))
(define *A130350! (**A122204 11))

(define *A130351! (**A122203 10))
(define *A130352! (**A122204 10))

(define *A130353! (**A122203 11))
(define *A130354! (**A122204 9))

(define *A130355! (**A122203 13))
(define *A130356! (**A122204 18))

(define *A130357! (**A122203 14))
(define *A130358! (**A122204 19))

(define *A130359! (**A122203 15))
(define *A130360! (**A122204 21))

(define *A130361! (**A122203 16))
(define *A130362! (**A122204 20))

(define *A130363! (**A122203 18))
(define *A130364! (**A122204 13))

(define *A130365! (**A122203 19))
(define *A130366! (**A122204 14))

(define *A130367! (**A122203 20))
(define *A130368! (**A122204 16))


(define (*A130369! s) ;; Cf. A074685
  (cond ((not (pair? s)))
        ((not (pair? (cdr s))) (*A069770! s))
        (else
              (*A074679! s)
              (*A130369! (cdr s))
        )
  )
  s
)

(define (*A130370! s) ;; Cf. A074686.
  (cond ((not (pair? s)))
        ((not (pair? (car s))) (*A069770! s))
        (else
              (*A130370! (cdr s))
              (*A074680! s)
        )
  )
  s
)

(define (*A130371! s)
  (if (odd? (length s)) (*A074685! (car (last-pair s))))
  s
)

(define (*A130372! s)
  (if (odd? (length s)) (*A074686! (car (last-pair s))))
  s
)

(define *A130373! (!SPINE *A130340!))
(define *A130373v2! (!ENIPS *A130340!))

(define (*A130374! s) ;; Cf. A127285 & A127287 (A130370 A122282 A130369) (A057508 A130373 A057508) (A057164 A130373 A057164)
  (cond ((pair? s)
              (*A072796! s)
              (if (pair? (cdr s)) (*A130374! (cddr s)))
        )
  )
  s
)

(define *A130375! (!APPLY-AFTER-FIRST-NIL *A074685!))
(define *A130376! (!APPLY-AFTER-FIRST-NIL *A074686!))


(define *A069768v4! (**A122204 1))
(define *A057510v3! (**A122204 2))
(define *A122282v3! (**A122204 7))
(define *A082340v2! (**A122204 8))
(define *A057502v3! (**A122204 12))
(define *A069770v3! (**A122204 15)) ;; (!ENIPS *A089859!)
(define *A074686v2! (**A122204 17))

;; I.e. 70 A-numbers: A122282 --- A122351.


(define *A122341! (**A122201 3))
(define *A122342! (**A122202 3))

(define *A122343! (**A122201 4))
(define *A122344! (**A122202 6))

(define *A122345! (**A122201 5))
(define *A122346! (**A122202 5))

(define *A122347! (**A122201 6))
(define *A122348! (**A122202 4))

(define *A122349! (**A122201 7)) ;; (equal? (map A073289 (iota0 63)) (map A122349 (iota0 63))) differs!
(define *A122350! (**A122202 7)) ;; c.f. likewise A073288


(define *A122291! (**A122201 10))
(define *A122292! (**A122202 10))

(define *A122293! (**A122201 11))
(define *A122294! (**A122202 9))

(define *A122295! (**A122201 13))
(define *A122296! (**A122202 18))

(define *A122297! (**A122201 14))
(define *A122298! (**A122202 19))

;; Here are the 12 A-numbers you requested: A122353 --- A122364.

(define *A122353! (**A122201 15))
(define *A122354! (**A122202 21))

(define *A122355! (**A122201 16))
(define *A122356! (**A122202 20))

(define *A122357! (**A122201 18))
(define *A122358! (**A122202 13))

(define *A122359! (**A122201 19))
(define *A122360! (**A122202 14))

(define *A122361! (**A122201 20))
(define *A122362! (**A122202 16))



(define *A122300! (**A122283 2)) ;; involution
(define *A122300!v2 (**A122284 2))

(define *A122301! (**A122283 1)) ;; Inverse = *A122302. Also FORK(A089840[21]) = FORK(A089863)
(define *A122302! (**A122284 1)) ;; inverse = *A122301. Also KROF(A089840[15]) = KROF(A089859)

(define *A122303! (**A122283 3)) ;; inverse = *A122304.
(define *A122304! (**A122284 3)) ;; inverse = *A122303.

(define *A122305! (**A122283 4)) ;; inverse = *A122306.
(define *A122306! (**A122284 6)) ;; inverse = *A122305.

(define *A122307! (**A122283 5)) ;; inverse = *A122308.
(define *A122308! (**A122284 5)) ;; inverse = *A122307.

(define *A122309! (**A122283 6)) ;; inverse = *A122310.
(define *A122310! (**A122284 4)) ;; inverse = *A122309.

(define *A122311! (**A122283 7)) ;; inverse = *A122312. Diffs for the first time from A073287 at n=35.
(define *A122312! (**A122284 7)) ;; inverse = *A122311.

(define *A122313! (**A122283 8)) ;; inverse = *A122314. Diffs for the first time from A069775 at n=34.
(define *A122314! (**A122284 8)) ;; inverse = *A122313.

(define *A122315! (**A122283 9)) ;; inverse = *A122316.
(define *A122316! (**A122284 11)) ;; inverse = *A122315.

(define *A122317! (**A122283 10)) ;; inverse = *A122318.
(define *A122318! (**A122284 10)) ;; inverse = *A122317.

(define *A122319! (**A122283 11)) ;; inverse = *A122320.
(define *A122320! (**A122284 9))  ;; inverse = *A122319.

(define *A122321! (**A122283 12))
(define *A122322! (**A122284 17))

(define *A122323! (**A122283 13))
(define *A122324! (**A122284 18))

(define *A122325! (**A122283 14))
(define *A122326! (**A122284 19))

(define *A122327! (**A122283 15))
(define *A122328! (**A122284 21))

(define *A122329! (**A122283 16))
(define *A122330! (**A122284 20))

(define *A122331! (**A122283 17))
(define *A122332! (**A122284 12))

(define *A122333! (**A122283 18))
(define *A122334! (**A122284 13))

(define *A122335! (**A122283 19))
(define *A122336! (**A122284 14))

(define *A122337! (**A122283 20))
(define *A122338! (**A122284 16))

(define *A122339! (**A122283 21))
(define *A122340! (**A122284 15))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Row 0 and 1 are involutions, but none of 2-21:

;; (define *A069770! (**A130400 1)) ;; A fixed point of both INORDER
;; (define *A069770! (**A130401 1)) ;;  and REDRONI. As well as A129604

;; (define *A073284! (**A130400 2))
;; (define *A073285! (**A130401 2))

;; (define *A122341! (**A130400 3))
;; (define *A122342! (**A130401 3))

(define *A130381! (**A130400 4))
(define *A130382! (**A130401 6))

(define *A130383! (**A130400 5))
(define *A130384! (**A130401 5))

(define *A130385! (**A130400 6))
(define *A130386! (**A130401 4))

;; (define *A122350! (**A130400 7))
;; (define *A122349! (**A130401 7))

;; (define *A082341! (**A130400 8))
;; (define *A082342! (**A130401 8))

(define *A130387! (**A130400 9))
(define *A130388! (**A130401 11))

(define *A130389! (**A130400 10))
(define *A130390! (**A130401 10))

(define *A130391! (**A130400 11))
(define *A130392! (**A130401 9))

;; (define *A074687! (**A130400 12))
;; (define *A074688! (**A130401 17))

(define *A130393! (**A130400 13))
(define *A130394! (**A130401 18))

(define *A130395! (**A130400 14))
(define *A130396! (**A130401 19))

(define *A130397! (**A130400 15))
(define *A130398! (**A130401 21))

(define *A130927! (**A130400 16))
(define *A130928! (**A130401 20))

;; (define *A071657! (**A130400 17))
;; (define *A071658! (**A130401 12))

(define *A130929! (**A130400 18))
(define *A130930! (**A130401 13))

(define *A130931! (**A130400 19))
(define *A130932! (**A130401 14))

(define *A130933! (**A130400 20))
(define *A130934! (**A130401 16))

;; (define *A089863! (**A130400 21)) ;; i.e. a fixed point of INORDER: *A089863 = INORDER(*A089863)
;; (define *A089859! (**A130401 15)) ;; i.e. a fixed point of REDRONI: *A089859 = REDRONI(*A089859)


;; (define *A082346! (**A130402 1))
;; (define *A082345! (**A130403 1))

(define *A130935! (**A130402 2))
(define *A130936! (**A130403 2))

;; (define *A073289! (**A130402 3)) ;; Check!
;; (define *A073288! (**A130403 3)) ;; !!!

(define *A130937! (**A130402 4))
(define *A130938! (**A130403 6))

(define *A130939! (**A130402 5))
(define *A130940! (**A130403 5))

(define *A130941! (**A130402 6))
(define *A130942! (**A130403 4))

(define *A130943! (**A130402 7))
(define *A130944! (**A130403 7))

(define *A130945! (**A130402 8))
(define *A130946! (**A130403 8))

(define *A130947! (**A130402 9))
(define *A130948! (**A130403 11))

(define *A130949! (**A130402 10))
(define *A130950! (**A130403 10))

(define *A130951! (**A130402 11))
(define *A130952! (**A130403 9))

(define *A074687! (**A130402 12))
(define *A074688! (**A130403 17))

(define *A130953! (**A130402 13))
(define *A130954! (**A130403 18))

(define *A130955! (**A130402 14))
(define *A130956! (**A130403 19))

(define *A130957! (**A130402 15))
(define *A130958! (**A130403 21))

(define *A130959! (**A130402 16))
(define *A130960! (**A130403 20))

;; (define *A057162! (**A130402 17))
;; (define *A057161! (**A130403 12))

(define *A130961! (**A130402 18))
(define *A130962! (**A130403 13))

(define *A130963! (**A130402 19))
(define *A130964! (**A130403 14))

(define *A130965! (**A130402 20))
(define *A130966! (**A130403 16))

;; (define *A069768! (**A130402 21))
;; (define *A069767! (**A130403 15))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Comparing A122285 against A122286, should produce differences.
;; Row 0 checked up to 23714, OK.
;; Row 1, (fun1 12)=18 different from (fun2 12)=17. Skipping the rest.
;; Row 2 checked up to 23714, OK.
;; Row 3, (fun1 26)=32 different from (fun2 26)=31. Skipping the rest.
;; Row 4, (fun1 28)=31 different from (fun2 28)=34. Skipping the rest.
;; Row 5, (fun1 29)=44 different from (fun2 29)=53. Skipping the rest.
;; Row 6, (fun1 26)=53 different from (fun2 26)=44. Skipping the rest.
;; Row 7 checked up to 23714, OK.

;; Rows 0, 2 (A057508) and 7 (new one, ENIPS(A122282) = SPINE(A122282)) are involutions.

;; of 21 rows, 4 are present already. Of the 21-4=17 new, one is involution, thus
;; we have 2*17=34 - 1 = 33 new sequences for A122285 & A122286.

;; A130973 - A131017 (45) + A131141 - A131220 (80 free)
;; A130973-A130980 free.

;; Scanning table A122285:
;; A122285[0] = A001477 ?
;; A122285[1] = A082348 ?
;; A122285[2] = A057508 ?
;; A122285[17] = A057503 ?
;; A122285[21] = A069768 ?
;; A122285[251] = A130360 ?
;; A122285[3608] = A130339 ?
;; A122285[3613] = A057510 ?
;; A122285[65352] = A074686 ?
;; Scanning table A122286:
;; A122286[0] = A001477 ?
;; A122286[1] = A082347 ?
;; A122286[2] = A057508 ?
;; A122286[12] = A057504 ?
;; A122286[15] = A069767 ?
;; A122286[169] = A130359 ?
;; A122286[3608] = A130339 ?
;; A122286[3617] = A057509 ?
;; A122286[65167] = A074685 ?
;; 


(define *A131141! (**A122285 3))
(define *A131142! (**A122286 3))

(define *A131143! (**A122285 4))
(define *A131144! (**A122286 6))

(define *A131145! (**A122285 5))
(define *A131146! (**A122286 5))

(define *A131147! (**A122285 6))
(define *A131148! (**A122286 4))

(define (*A131173! s)
  (cond ((pair? s)
           (*A069770! (car s))
           (if (pair? (cdr s)) (*A131173! (cddr s)))
        )
  )
  s
)

(define *A131173v2! (**A122285 7))
(define *A131173v3! (**A122286 7))

(define *A131169! (**A122285 8))
(define *A131170! (**A122286 8))


(define *A131149! (**A122285 9))
(define *A131150! (**A122286 11))

(define *A131151! (**A122285 10))
(define *A131152! (**A122286 10))

(define *A131153! (**A122285 11))
(define *A131154! (**A122286 9))

(define *A131171! (**A122285 12))
(define *A131172! (**A122286 17))

(define *A131155! (**A122285 13))
(define *A131156! (**A122286 18))

(define *A131157! (**A122285 14))
(define *A131158! (**A122286 19))

(define *A131159! (**A122285 15))
(define *A131160! (**A122286 21))

(define *A131161! (**A122285 16))
(define *A131162! (**A122286 20))

(define *A131163! (**A122285 18))
(define *A131164! (**A122286 13))

(define *A131165! (**A122285 19))
(define *A131166! (**A122286 14))

(define *A131167! (**A122285 20))
(define *A131168! (**A122286 16))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Comparing A122287 against A122288, should produce differences.
;; Row 0 checked up to 23714, OK.
;; Row 1, (fun1 4)=7 different from (fun2 4)=8. Skipping the rest.
;; Row 2 checked up to 23714, OK.
;; Row 3 checked up to 23714, OK.
;; Row 4 checked up to 23714, OK.
;; Row 5 checked up to 23714, OK.
;; Row 6 checked up to 23714, OK.
;; 
;; Rows 0, 2 (A057164), 3, and 5 are involutions.
;; Rows 4 and 6 are same in tables A122287 and A122288.
;; (same-intfuns? A122287_4 A122288_4 82500) --> #t
;; (same-intfuns? A122287_6 A122288_6 82500) --> #t

;; of 21 rows, 4 are present already. Of the 21-4=17 new ones, two are involutions
;; and two are located for some other reason in both tables,
;; thus we have 2*17=34 - 4 = 30 new sequences for A122287 & A122288.

;; Exceptions to numbering:
;; (define *A131009! (**A122288 12))
;; (define *A131010! (**A122287 17))
;; plus the ones in the beginning, up to row 8:

(define *A130981! (**A122288 3))
(define *A130981v2! (**A122287 3))

(define *A130983! (**A122288 4))
(define *A130984! (**A122287 6))

(define *A130983v2! (**A122287 4))
(define *A130984v2! (**A122288 6))

(define *A130982! (**A122288 5))
(define *A130982v2! (**A122287 5))

(define *A130985! (**A122288 7))
(define *A130986! (**A122287 7))

(define *A130987! (**A122288 8))
(define *A130988! (**A122287 8))

(define *A130989! (**A122288 9))
(define *A130990! (**A122287 11))

(define *A130991! (**A122288 10))
(define *A130992! (**A122287 10))

(define *A130993! (**A122288 11))
(define *A130994! (**A122287 9))

(define *A131009! (**A122288 12))
(define *A131010! (**A122287 17))

(define *A130995! (**A122288 13))
(define *A130996! (**A122287 18))

(define *A130997! (**A122288 14))
(define *A130998! (**A122287 19))

(define *A130999! (**A122288 15))
(define *A131000! (**A122287 21))

(define *A131001! (**A122288 16))
(define *A131002! (**A122287 20))

(define *A131003! (**A122288 18))
(define *A131004! (**A122287 13))

(define *A131005! (**A122288 19))
(define *A131006! (**A122287 14))

(define *A131007! (**A122288 20))
(define *A131008! (**A122287 16))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Scanning table A122287:
;; A122287[0] = A001477 ?
;; A122287[1] = A069767 ?
;; A122287[2] = A057164 ?
;; A122287[12] = A057506 ?
;; A122287[15] = A057163 ?
;; A122287[169] = A122353 ?
;; A122287[3617] = A057511 ?
;; A122287[65167] = A074681 ?
;; Scanning table A122288:
;; A122288[0] = A001477 ?
;; A122288[1] = A069768 ?
;; A122288[2] = A057164 ?
;; A122288[17] = A057505 ?
;; A122288[21] = A057163 ?
;; A122288[251] = A122354 ?
;; A122288[3613] = A057512 ?
;; A122288[65352] = A074682 ?
;; 

;; Here are the 12 A-numbers you requested: A123492 --- A123503.
;; Here are the  3 A-numbers you requested: A123694 --- A123696.
;; Here are the  7 A-numbers you requested: A123713 --- A123719.

(define (*A123492! s)
  (cond ((null? s) s)
        ((and (pair? (cdr s)) (pair? (cadr s))) (*A069770! s))
        ((and (pair? (car s)) (pair? (caar s))) (*A069770! s))
  )
  s
)

(define (*A123492v2! s)
  (cond ((null? s) s)
        ((and (pair? (car s)) (pair? (caar s))) (*A069770! s))
        ((and (pair? (cdr s)) (pair? (cadr s))) (*A069770! s))
  )
  s
)


(define *A123493! (!FORK *A123492!)) ;; Induces A006068
(define *A123494! (!KROF *A123492!)) ;; Incuces A003188

(define *A123715! (!SPINE *A123492!)) ;; Something interesting?
(define *A123716! (!ENIPS *A123492!)) ;; as well?

;; ((a . b) . (c . d)) --> (((c . d) . a) . b)
;; (a . (b . c))       --> ((a . b) . c)   [a implied ()]
;; (a . b)             --> (b . a)         [b implied ()]
;;
;;             C   D
;;              \ /
;; A  B C  D     Y2   A          B   C   []  B
;;  \ / \ /       \ /             \ /     \ /
;;   X1  X2    --> Y1  B       []  X1  --> Y1  C       A  []       []  A
;;    \ /           \ /         \ /         \ /         \ /   -->   \ /
;;     X0            Y0          X0          Y0          X0          Y0


(define (*A123495! s) ;; = A082351 o A069770
  (cond ((null? s) s)
        ((and (pair? (car s)) (pair? (cdr s)))
            (*A069770! s)
            (*A074679! s)
        )
        (else (*A074679! s))
  )
  s
)

;; (((a . b) . c) . d) --> ((c . d) . (a . b))
;; ((a . b) . c)       --> (a . (b . c))   [a implied ()]
;; (a . b)             --> (b . a)         [a implied ()]
;;
;; A   B
;;  \ /
;;   X2  C         C  D A  B  []  B            B   C
;;    \ /           \ / \ /    \ /              \ /
;;     X1  D   -->   Y1  Y2     X1  C   -->  []  Y1    []  B       B  []
;;      \ /           \ /        \ /          \ /       \ /   -->   \ /
;;       X0            Y0         X0           Y0        X0          Y0


(define (*A123496! s) ;; = A069770 o A082352
  (cond ((null? s) s)
        ((and (pair? (car s)) (pair? (caar s)))
            (*A074680! s)
            (*A069770! s)
        )
        (else (*A074680! s))
  )
  s
)


(define *A082357v2! (!KROF *A123495!))
(define *A082358v2! (!FORK *A123496!))

;; A074680 = A083927 A123497 A057123

(define (*A123497! s)
  (cond ((null? s) s)
        ((and (pair? (car s)) (pair? (cdar s)))
            (*A074680! s)
            (let ((old-cddr-s (cddr s)))
               (set-cdr! (cdr s) (cdadr s))
               (set-cdr! (cadr s) old-cddr-s)
            )
        )
        ((pair? (car s)) (*A072797! s))
        ((pair? (cdr s)) (*A072796! s))
  )
  s
)

;; A074679 = A083927 A123498 A057123

(define (*A123498! s)
  (cond ((null? s) s)
        ((and (pair? (cdr s)) (pair? (cadr s)))
            (let ((old-cddr-s (cddr s)))
               (set-cdr! (cdr s) (cdadr s))
               (set-cdr! (cadr s) old-cddr-s)
            )
            (*A074679! s)
        )
        ((pair? (cdr s)) (*A072796! s))
        ((pair? (car s)) (*A072797! s))
  )
  s
)


;; A057501 = A083927 A123501 A057123 = A083927 A085159 A057123

(define (*A123501! s)
  (*A123497! s)
  (cond ((and (pair? s) (pair? (cdr s))) (*A123501! (cadr s))))
  s
)

;; A057502 = A083927 A123502 A057123 = A083927 A085160 A057123

(define (*A123502! s)
  (cond ((and (pair? s) (pair? (cdr s))) (*A123502! (cadr s))))
  (*A123498! s)
  s
)


;; CLAUSE gmA123499[] = { CLAUSESEQ_begin(12,2), { 2, 0, 1, 0,}, { 2, 1, 0, 4 } }; /* A089840[258] */
; (a . (b . c)) --> ((a . b) . c)
; ((a . b) . c) --> (c . (b . a))

(define (*A123499! s)
  (cond ((null? s) s)
        ((pair? (cdr s)) (*A074679! s))
        (else (*A089863! s))
  )
  s
)


;; CLAUSE gmA123500[] = { CLAUSESEQ_begin(12,2), { 2, 1, 0, 0,}, { 2, 0, 1, 4 } }; /* A089840[264] */
; ((a . b) . c) --> (a . (b . c))
; (a . (b . c)) --> ((c . b) . a)

(define (*A123500! s)
  (cond ((null? s) s)
        ((pair? (car s)) (*A074680! s))
        (else (*A089859! s))
  )
  s
)

;; CLAUSE gmA123503[] = { CLAUSESEQ_begin(12,2), { 2, 0, 0, 1,}, { 2, 1, 1, 1 } }; /* A089840[253] */
; (a . (b . c)) --> (b . (a . c))
; ((a . b) . c) --> ((b . a) . c)
;
;   B   C         A   C    A   B           B   A
;    \ /           \ /      \ /             \ /
; A   X1   -->  B   Y1       X1 []    -->    Y1 []
;  \ /           \ /          \ /             \ /
;   X0            Y0           X0              Y0

; Take SPINE & ENIPS transform of this. The fpc and cc sequences should have "interesting" formulae.

(define (*A123503! s)
  (cond ((null? s) s)
        ((pair? (cdr s)) (*A072796! s))
        (else (*A089854! s))
  )
  s
)

(define *A123717! (!SPINE *A123503!)) ;; Something interesting?
(define *A123718! (!ENIPS *A123503!)) ;; and its inverse.

(define *A123719 (RIBS *A085161))

; CLAUSE gmA123695[] =  { CLAUSESEQ_begin(99,3), { 2, 0, 1, 0,}, { 3, 3, 1, 23,}, { 2, 1, 0, 3 } }; /* A089840[1653002] */
; (a . (b . c)) --> ((a . b) . c)
; ((a . (b . c)) . d) --> (d . ((a . b) . c))
; ((a . b) . c) --> (c . (b . a))
;
;                            B   C        A   B
;                             \ /          \ /
;   B   C     A   B        A   X2           Y2  C   A  []               []  A
;    \ /       \ /          \ /              \ /     \ /                 \ /
; A   X1   -->  Y1  C        X1 []   -->  []  Y1      X1 []    -->    []  Y1
;  \ /           \ /          \ /          \ /         \ /             \ /
;   X0            Y0           X0           Y0          X0              Y0

(define (*A123695! s)
  (cond ((null? s) s)
        ((pair? (cdr s)) (*A074679! s))
        ((pair? (car s)) (*A074679! (car s)) (*A069770! s))
  )
  s
)



; CLAUSE gmA123696[] = { CLAUSESEQ_begin(99,3), { 2, 1, 0, 0,}, { 3, 1, 3, 9,}, { 2, 0, 1, 4 } }; /* A089840[1653063] */
; ((a . b) . c)       --> (a . (b . c))
; (a . ((b . c) . d)) --> ((b . (c . d)) . a)
; (a . (b . c))       --> ((c . b) . a)
;
;                            B   C       C   D
;                             \ /         \ /
; A   B             B   C      X2  D    B  Y2           []   C      C   []
;  \ /               \ /        \ /      \ /             \ /         \ /
;   X1  C  -->    A   Y1     []  X1  -->  Y1  []      []  X1   -->    Y1  []
;    \ /           \ /        \ /          \ /         \ /             \ /
;     X0            Y0         X0           Y0          X0              Y0

(define (*A123696! s)
  (cond ((null? s) s)
        ((pair? (car s)) (*A074680! s))
        ((pair? (cdr s)) (*A074680! (cdr s)) (*A069770! s))
  )
  s
)


; CLAUSE gmA123713[] = { CLAUSESEQ_begin(124,2), { 2, 1, 1, 3,}, { 5, 13, 13, 152 } }; /* A089840[1783367]] */

; ((a . b) . c)                   --> ((b . c) . a)  (c.f. A089855)
; (a . ((((b . c) . d) . e) . f)) --> (a . ((((c . d) . e) . f) . b))
;
;
;  A   B         B   C
;   \ /           \ /
;    X1  C   -->   Y1  A
;     \ /           \ /
;      X0            Y0
;
;  B   C            C   D
;   \ /              \ /
;    X4  D            Y4  E
;     \ /              \ /
;      X3  E            Y3  F
;       \ /     -->      \ /
;        X2  F            Y2  B
;         \ /              \ /
;      []  X1           []  Y1
;       \ /              \ /
;        X0               Y0
;

;; Differs from A089855 for the first time at n=102, where a(n)=103, while A089855(n)=102.

(define (*A123713! s)
  (cond ((not (pair? s)) s)
        ((pair? (car s))
           (let ((org_cdar (cdar s)))     ;; save orig. b
              (set-cdr! (car s) (cdr s))  ;; c -> b
              (set-cdr! s (caar s))       ;; a -> c
              (set-car! (car s) org_cdar) ;; b -> a
              s
           )
        )
        ((and (pair? (cdr s))
              (pair? (cadr s))
              (pair? (caadr s))
              (pair? (caaadr s))
         )
           (let ((org_b (car (caaadr s))))
              (set-car! (caaadr s) (cdr (caaadr s))) ;; b <- c
              (set-cdr! (caaadr s) (cdaadr s))       ;; c <- d
              (set-cdr! (caadr s) (cdadr s))         ;; d <- e
              (set-cdr! (cadr s) (cddr s))           ;; e <- f
              (set-cdr! (cdr s) org_b)               ;; f <- b
              s
           )

        ) ;; and
        (else s)
  )
)



; CLAUSE gmA123714[] = { CLAUSESEQ_begin(124,2), { 2, 1, 1, 5,}, { 5, 13, 13, 566 } }; /* A089840[1786785] */
; ((a . b) . c) --> ((c . a) . b)  (c.f. A089857)
; (a . ((((b . c) . d) . e) . f)) --> (a . ((((f . b) . c) . d) . e))
; 
;
;  A   B         C   A
;   \ /           \ /
;    X1  C   -->   Y1  B
;     \ /           \ /
;      X0            Y0
;
;
;  B   C            F   B
;   \ /              \ /
;    X4  D            Y4  C
;     \ /              \ /
;      X3  E            Y3  D
;       \ /     -->      \ /
;        X2  F            Y2  E
;         \ /              \ /
;      []  X1           []  Y1
;       \ /              \ /
;        X0               Y0
;

;; Differs from A089857 for the first time at n=102, where a(n)=106, while A089857(n)=102.

(define (*A123714! s)
  (cond ((not (pair? s)) s)
        ((pair? (car s))
           (let ((org_a (caar s))) ;; save orig a.
              (set-car! (car s) (cdr s))  ;; a <- c
              (set-cdr! s (cdar s))       ;; c <- b
              (set-cdr! (car s) org_a)    ;; b <- a
              s
           )
        )
        ((and (pair? (cdr s))
              (pair? (cadr s))
              (pair? (caadr s))
              (pair? (caaadr s))
         )
           (let ((org_f (cddr s)))
              (set-cdr! (cdr s) (cdadr s))           ;; f <- e
              (set-cdr! (cadr s) (cdaadr s))         ;; e <- d
              (set-cdr! (caadr s) (cdr (caaadr s)))  ;; d <- c
              (set-cdr! (caaadr s) (car (caaadr s))) ;; c <- b
              (set-car! (caaadr s) org_f)            ;; b <- f
              s
           )

        ) ;; and
        (else s)
  )
)



(define *A127285v2! (!SPINE *A057508!))
(define *A127286v2! (!ENIPS *A057508!))
(define *A127287v2! (compose-funs *A127285v2! *A057508!))
(define *A127288v2! (compose-funs *A057508! *A127286v2!))

(define (*A127285! s)
   (cond ((pair? s) (*A057508! s) (*A127285! (cdr s))))
   s
)

(define (*A127286! s)
   (cond ((pair? s) (*A127286! (cdr s)) (*A057508! s)))
   s
)

(define (*A127287! s) (*A127285! (*A057508! s)))
(define (*A127288! s) (*A057508! (*A127286! s)))


;; (define (A_is_not_bijective n) (tau (A014486->parenthesization (A014486 n)) *A130374!))


(define (A057164v5 n) (tau (A014486->parenthesization (A014486 n)) *A057508!))
(define (A057501v5 n) (tau (A014486->parenthesization (A014486 n)) *A057509!))
(define (A057502v5 n) (tau (A014486->parenthesization (A014486 n)) *A057510!))

;; See  Emeric Deutsch and Sergi Elizalde, A simple and unusual bijection for Dyck paths
;; and its consequences, Ann. Comb. vol. 7 (2003), no. 3, 281--297.

(define (A127289 n) (tau (A014486->parenthesization (A014486 n)) *A127285!))
(define (A127290 n) (A127300 (A057164 n)))

(define (A127291 n) (tau (A014486->parenthesization (A014486 n)) *A127287!))

(define (A127292 n) (A057164 (A127300 (A057164 n))))


(define (tau s permuter)
  (let* ((sper (transpos-list->permvec (sexp->kk s)))
         (visivec (make-vector (vector-length sper) #f))
        )
   (let loop ((tper (if (null? s) s (permuter (iota0 (-1+ (vector-length sper))))))
              (s 0)
             )
;;    (format #t "sper=~a, visivec=~a, tper=~a, s=~a\n" sper visivec tper s)
      (cond ((null? tper) (A080300 s)) ;; Ready?
            (else
               (let ((x (vector-ref sper (car tper))))
                  (cond ((not (vector-ref visivec x)) ;; First time for this pair?
                           (vector-set! visivec (car tper) #t) ;; Mark the other element.
                           (loop (cdr tper) (+ s s 1))
                        )
                        (else (loop (cdr tper) (+ s s))) ;; The second elem of pair.
                  )
               )
            )
      )
   )
  )
)


;; Borrowed and modified from sexp->hs in http://www.iki.fi/kartturi/matikka/Nekomorphisms/gatocout.scm

;; From the interpretation n (non-crossing handshakes, i.e. nonintersecting
;; chords joining 2n points on the circumference of a circle) to the
;; interpretation kk (fixed-point free and non-crossing involutions of [2n]):

;; (sexp->kk '())       --> ()
;; (sexp->kk '(()))     --> ((0 . 1))
;; (sexp->kk '(()()))   --> ((0 . 1) (2 . 3))
;; (sexp->kk '((())))   --> ((0 . 3) (1 . 2))
;; (sexp->kk '(()()())) --> ((0 . 1) (2 . 3) (4 . 5))
;; (sexp->kk '(()(()))) --> ((0 . 1) (2 . 5) (3 . 4))
;; (sexp->kk '((())())) --> ((0 . 3) (1 . 2) (4 . 5))
;; (sexp->kk '((()()))) --> ((0 . 5) (1 . 2) (3 . 4))
;; (sexp->kk '(((())))) --> ((0 . 5) (1 . 4) (2 . 3))

;; Could be cleaner, probably:

(define (sexp->kk p)
  (let ((c (list (list))) ;; Just something that attach! will work.
        (maxnode (list -1))
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



(define (transpos-list->permvec tplist)
  (let ((permvec (make-initialized-vector (* 2 (length tplist)) (lambda (i) i)))) ;; id-perm.
   (let loop ((tplist tplist))
      (cond ((null? tplist) permvec) ;; Ready?
            (else
               (let* ((tp (car tplist))
                      (x-point (car tp))
                      (y-point (cdr tp))
                      (temp (vector-ref permvec x-point))
                     )
                 (vector-set! permvec x-point (vector-ref permvec y-point))
                 (vector-set! permvec y-point temp)
                 (loop (cdr tplist))
               )
            )
      )
   )
  )
)


;; Essentially the same as (except faster): (define (rassq key alist) (assq key (reverse alist)))
(define (rassq key al)
  (let loop ((al al) (last-found #f))
        (cond ((null? al) last-found)
              ((eq? (caar al) key) (loop (cdr al) (car al)))
              (else (loop (cdr al) last-found))
        )
  )
)


;; Returns a list of non-crossing transpositions
(define (A127300-aux1 n)
  (if (zero? n) (list)
    (let loop ((n n)
               (tplist1 (list))
               (tplist2 (list))
               (i 0) ;; index to the side we are in, initially the left side.
               (j (A000523 n)) ;; index to the other side, initially the right side.
               (b +1) ;; On which side we currently are? +1 = left, -1 = right.
              )
       (cond ((zero? n) (append tplist1 tplist2))
             ((even? n) (loop (/ n 2) tplist2 (cons (cons '() i) tplist1) j (+ i b) (- b)))
             ((assq '() tplist1) =>
                (lambda (p)
                   (set-car! p i)
                   (loop (/ (- n 1) 2) tplist2 tplist1 j (+ i b) (- b))
                )
             )
             ((rassq '() tplist2) =>
                (lambda (p)
                   (set-car! p i)
                   (loop (/ (- n 1) 2) tplist2 tplist1 j (+ i b) (- b))
                )
             )
             (else ;; Should not happen if n is a member of A014486.
                (error "Unmatched closing slope with n, tplist1, tplist2, i, j and b="
                       n tplist1 tplist2 i j b
                )
             )
       )
    )
  )
)

(define (transpos-list->A014486 tplist)
   (fold-left (lambda (s p) (+ s (expt 2 (max (car p) (cdr p))))) 0 tplist)
)



(define (A127299 n) (A057164 (A127291 (A057164 n))))
(define (A127300 n) (A080300 (transpos-list->A014486 (A127300-aux1 (A014486 n)))))

;; Here are the 20 A-numbers you requested: A127376 --- A127395.

;; See D. Callan, A Bijection on Dyck Paths and Its Cycle Structure, 2006, 17pp, at:
;; http://www.stat.wisc.edu/~callan/papers/Motzkin_manifestation/


(define (*A127377! s)
   (cond ((null? s))
         ((null? (car s)) (*A069770! s) (*A127377! (car s)))
         ((null? (cdr s)) (*A069770! s) (*A127379! (cdr s)))
         (else (*A127379! s))
   )
   s
)

(define (*A127378! s)
   (cond ((null? s))
         ((null? (cdr s)) (*A069770! s) (*A127378! (cdr s)))
         ((null? (car s)) (*A069770! s) (*A127380! (car s)))
         (else (*A127380! s))
   )
   s
)

(define *A127379! (!RIBS *A127377!))
(define *A127380! (!RIBS *A127378!))

(define *A127381! (compose-funs *A057164! *A127379! *A057164!))
(define *A127382! (compose-funs *A057164! *A127380! *A057164!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A variant of Callan's 2006 bijection. An involution.

(define (*A127387! s)
   (cond ((null? s))
         ((null? (car s)) (*A069770! s) (*A127387! (car s)))
         ((null? (cdr s)) (*A069770! s) (*A127387! (cdr s)))
         (else (*A127388! s))
   )
   s
)

(define *A127388! (!RIBS *A127387!))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Left-over stuff:
(define (transpos-list->permlist tplist)
  (let ((permlist (if (null? tplist) '() (iota0 (-1+ (* 2 (length tplist))))))) ;; Start from id-perm (1 .. 2n)
   (let loop ((tplist tplist))
      (cond ((null? tplist) permlist) ;; Ready?
            (else
               (let* ((tp (car tplist))
                      (x-point (list-tail permlist (car tp)))
                      (y-point (list-tail permlist (cdr tp)))
                      (temp (car x-point)) 
                     )
                 (set-car! x-point (car y-point))
                 (set-car! y-point temp)
                 (loop (cdr tplist))
               )
            )
      )
   )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;; Isomorphism from the automorphism group of infinite binary trees     ;;
;; (i.e. the infinite iterated wreath product of S_2's)                 ;;
;; to the group of bijections of finite binary trees.                   ;;
;; AK, Dec 19 2008.                                                     ;;
;;                                                                      ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here the fundamental observation is that all "eligible" permutations
;; like A063946, A054429, A153141-A153142, A003188-A006068, etc.
;; should begin as a(1)=1, and then they should always map 2n and 2n+1
;; to adjacent positions. (If 2n is before 2n+1, then no swap,
;; and if 2n is after 2n+1, then we do swap, at the level one nearer
;; to root.)
;;
;; What about A059893, A059894 and A056539 as they don't conform
;; to that condition? (So they don't extend uniquely to all finite
;; binary trees. Maybe I should invent a more lenient isomorphism,
;; which accepts a superset of N-permutations as what psi accepts).


;;  (psi A063946) = A069770
;;  (psi A054429) = A057163
;;  (psi A153141) = A069767
;;  (psi A153142) = A069768

;;  (psi A006068) = A122353
;;  (psi A003188) = A122354


;; This gives the isomorphism: (wrap with catfun1)
(define (psi inftreeperm)
   (lambda (s) (swap-binary-tree-according-to-infbintree-permutation s inftreeperm))
)

;; We should check that inftreeperm is really an element of the automorphism
;; group of the infinite binary tree (the infinitely iterated wreath product
;; of S_2's), which means that each node should end under its original
;; parent, in the image as well.
;; I.e. the children should not flee from their parents, no runaway children!

;; And also, it should be in principle checked that (inftreeperm 1) = 1.

(define (swap-binary-tree-according-to-infbintree-permutation s inftreeperm)
 (cond
   ((not (= 1 (inftreeperm 1)))
     (error ;; Rotten already at the root
        "Function inftreeperm should return 1 for 1, and it should be one-to-one and onto!"
     )
   )
   (else ;; we can proceed:
     (let fork ((s s) (nod 1))
       (cond
        ((pair? s)
           (fork (car s) (* 2 nod))
           (fork (cdr s) (+ (* 2 nod) 1))
           (let ((node-dest (inftreeperm nod))
                 (left-dest (inftreeperm (* 2 nod)))
                 (right-dest (inftreeperm (1+ (* 2 nod))))
                )
              (cond ((or (not (= (floor->exact (/ left-dest 2)) node-dest))
                         (not (= (floor->exact (/ right-dest 2)) node-dest))
                     )
                       (error ;; Runaway child(ren)!
                           (format
                              #t
"Function inftreeperm is not an automorphism of an infinite binary tree. Either the left or right child flees from its parent: (inftreeperm ~a)=~a. Left: (inftreeperm ~a)=~a, Right: (inftreeperm ~a)=~a.\n"
                              nod node-dest
                              (* 2 nod) left-dest
                              (1+ (* 2 nod)) right-dest
                           )
                       )
                    )
                    ((= (1+ left-dest) right-dest)
                         ;; images in order, so do nothing, no swap!
                    )
                    (else ;; I.e. (= left-dest (1+ right-dest))
                        (*A069770! s)
                    )
              )
           )
        )
       )
     )
    s
   )
 )
)


;; This version is too cavalier, as this treats A065190 equally to A054429:
;; (A065190 should not be allowed)
(define (swap-binary-tree-according-to-infbintree-permutation-old-version-really-old
                                            s inftreeperm)
 (let fork ((s s) (nod 1))
   (cond ((pair? s)
           (fork (car s) (* 2 nod))
           (fork (cdr s) (+ (* 2 nod) 1))
           (cond ((= (1+ (inftreeperm (* 2 nod))) (inftreeperm (1+ (* 2 nod))))
                      ;; images in order, so do nothing, no swap!
                 )
                 ((= (inftreeperm (* 2 nod)) (1+ (inftreeperm (1+ (* 2 nod)))))
                     (*A069770! s)
                 )
                 (else (error
                        (format
                           #t
"Function inftreeperm is not a valid sort of permutation. No consecutive values: (inftreeperm ~a)=~a and (inftreeperm ~a)=~a.\n"
                           (* 2 nod) (inftreeperm (* 2 nod))
                           (1+ (* 2 nod)) (inftreeperm (1+ (* 2 nod)))
                        )
                       )
                 )
           )
         )
   )
 )
 s
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;; Converse stuff, check whether we have a Catalan bijection which      ;;
;; can be obtained with psi.                                            ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; n is the A014486-index of the tree, catbij is e.g. A001477, A069767, etc.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;; More images of Aut(T^(2)) automorphisms.                             ;;
;; Generators for Basilica & Lamplighter group, etc.                    ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define A154449v2 (catfun1 (psi (Anum->Afun 154439))))
;; (define A154450v2 (catfun1 (psi (Anum->Afun 154440))))
;; 
;; (define A154451v2 (catfun1 (psi (Anum->Afun 154441))))
;; (define A154452v2 (catfun1 (psi (Anum->Afun 154442))))
;; 
;; (define A154453v2 (catfun1 (psi (Anum->Afun 154443))))
;; (define A154454v2 (catfun1 (psi (Anum->Afun 154444))))
;; 
;; (define A154455v2 (catfun1 (psi (Anum->Afun 154445))))
;; (define A154456v2 (catfun1 (psi (Anum->Afun 154446))))
;; 
;; (define A154457v2 (catfun1 (psi (Anum->Afun 154447))))
;; (define A154458v2 (catfun1 (psi (Anum->Afun 154448))))

;; A154459 - 154469 free and A154478 - 154483 free

;; (init-them 131072)
;; (map (lambda (a) (format #t "~a --> ~a ?\n" a (find-matching-anum (catfun1 (psi (Anum->Afun a))) 2055))) (map (lambda (n) (+ n 154439)) (iota0 9)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (*A154449! s) ;; Inverse of the next
    (if (pair? s) (*A154451! (cdr s)))
    s
)

(define (*A154450! s)
    (if (pair? s) (*A154452! (cdr s)))
    s
)

(define (*A154451! s) ;; Inverse of the next
    (cond ((pair? s) (*A154449! (cdr s))  (*A069770! s)))
    s
)

(define (*A154452! s)
    (cond ((pair? s) (*A069770! s) (*A154450! (cdr s))))
    s
)


(define (*A154453! s) ;; Inverse of the next
    (if (pair? s) (*A154455! (car s)))
    s
)

(define (*A154454! s)
    (if (pair? s) (*A154456! (car s)))
    s
)

(define (*A154455! s) ;; Inverse of the next
    (cond ((pair? s) (*A154453! (car s))  (*A069770! s)))
    s
)

(define (*A154456! s)
    (cond ((pair? s) (*A069770! s) (*A154454! (car s))))
    s
)

(define (*A154457! s)
    (cond ((pair? s) (*A069770! s) (*A154458! (car s))))
    s
)

(define (*A154458! s)
    (cond ((pair? s) (*A069770! s) (*A154457! (cdr s))))
    s
)


;; (list 154435 "Permutation of non-negative integers induced by Lamplighter group generating wreath recursion, variant 3: a = s(a,b), b = (a,b), starting from the state a."

(define (*A122301uus! s)
   (cond ((pair? s) (*A069770! s) (*A122301uus! (car s)) (*A122349uus! (cdr s))))
   s
)

(define *A122349uus! (!RIBS *A122301uus!))


;; (list 154436 "Permutation of non-negative integers induced by Lamplighter group generating wreath recursion, variant 1: a = s(b,a), b = (a,b), starting from the state a."

(define (*A122302uus! s)
   (cond ((pair? s) (*A069770! s) (*A122350uus! (car s)) (*A122302uus! (cdr s))))
   s
)

(define *A122350uus! (!RIBS *A122302uus!))


(define (*A122353uus! s)
   (cond ((pair? s) (*A069770! s) (*A122341uus! (car s)) (*A122353uus! (cdr s))))
   s
)

(define (*A122341uus! s)
   (cond ((pair? s) (*A122341uus! (car s)) (*A122353uus! (cdr s))))
   s
)


(define (*A122354uus! s)
   (cond ((pair? s) (*A069770! s) (*A122354uus! (car s)) (*A122342uus! (cdr s))))
   s
)

(define (*A122342uus! s)
   (cond ((pair? s) (*A122342uus! (car s)) (*A122354uus! (cdr s))))
   s
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;; See "Symbolic Systems" in chapter 3 "The World of Simple Programs"   ;;
;; in Stephen Wolfram, A New Kind of Science, Wolfram Media, 2002       ;;
;; pp. 102-104, 896-898                                                 ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the rewriting system shown on the page 102:
;; See also: page-896f-text.html

;; Starting from e[e[e][e]][e][e]
;; with rewrite-system e[x_][y_] -> x[x[y]]

;; Here we use () as a marker for e.
;; (And in another sequence we eradicate them with A126308/A126309).

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

;;  (()(()(())(()))(())(()))
;; (*A154470 ' (()(()(())(()))(())(())))

;; (A014486->parenthesization (A126308 (parenthesization->A014486 g1)))
;; ;Value 55: ( (() ()) () () )
;; (A014486->parenthesization (A126308 (parenthesization->A014486 g2)))
;; ;Value 56: ( () () (() () ()) () )
;; 
;; (A014486->parenthesization (A126308 (parenthesization->A014486 g3)))
;; ;Value 57: ( (()) ((()) ()) () )
;; 
;; (A014486->parenthesization (A126308 (parenthesization->A014486 g4)))
;; ;Value 58: ( () (() ((()) ())) () )
;; 
;; (A014486->parenthesization (A126308 (parenthesization->A014486 g5)))
;; ;Value 59: ( ((() ((()) ()))) () )
;; 
;; (map (lambda (x) (A126308 (parenthesization->A014486 x))) (list g1 g2 g3 g4 g5))
;; (842 11090 13202 46882 60994)
;; (map A007088 ' (842 11090 13202 46882 60994))
;; (1101001010 10101101010010 11001110010010 1011011100100010 1110111001000010)
;; 

;; Apply also A126309 to A014486-indices of ...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;;                                                                      ;;
;; Code adapted from:                                                   ;;
;; Yvan Le Borgne,                                                      ;;
;; An algorithm to describe bijections involving Dyck paths, FPSAC 06,  ;;
;; http://garsia.math.yorku.ca/fpsac06/papers/46.pdf                    ;;
;; 2006, 12pp.                                                          ;;
;;                                                                      ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We implement "Dyck building" as a list of u's and d's
;; ("ups" and "downs") and labels as integers 0..
;; We don't distinguish the falling labels from the rising labels
;; in any special way, as for the insertion modes which we are concerned
;; with, we can always be sure that the falling label
;; always occurs to the right of the corresponding rising label.
;; Same is true of the letters A, B and x in the insertion mode.


;; p->zerofree-code1 given in gatoaltr.scm. Essentially it gives
;; the digits of A071158 from the least significant end.

(define (catsigperm-by-insertion-mode insmode)
  (lambda (n)
     (A080300 (ads->A014486-by-insertion-mode
                   insmode
                   (map -1+ (p->zerofree-code1 (A014486->parenthesization (A014486 n))))
              )
     )
  )
)


(define (deb-catsigperm-by-insertion-mode insmode)
  (lambda (n)
     (A080300 (deb-ads->A014486-by-insertion-mode
                   insmode
                   (map -1+ (p->zerofree-code1 (A014486->parenthesization (A014486 n))))
              )
     )
  )
)


;; Our insertion mode is one of the:

(define INSERTION-MODES
 (list
   '((u B B) (d A A)) ;; = ((u B) (B d A A))
   '((u A A) (d B B))
   '((A A u) (B B d))
   '((B B u) (A A d))

   '((A B B u) (d A))
   '((B A A u) (d B))
   '((A u) (d B B A))
   '((B u) (d A A B))

   '((B A B u) (A d))
   '((A B A u) (B d))
   '((u A) (d B A B))
   '((B u) (d A B A))
 )
)


;; (for-each (lambda (i) (format #t "LeBorgne[~a] = ~a ?\n"
;;                               i (find-matching-anum (LeBorgne-catsigperm i) 2055)))
;;           (iota (length INSERTION-MODES))
;; )
;; LeBorgne[1] = A001477
;; LeBorgne[2] = A057163
;; LeBorgne[3] = A057164
;; LeBorgne[4] = A057505
;; LeBorgne[5] = A126320
;; LeBorgne[6] = A071661
;; LeBorgne[7] = A057506
;; LeBorgne[8] = A069787
;; LeBorgne[9] = () ?
;; LeBorgne[10] = () ?
;; LeBorgne[11] = () ?
;; LeBorgne[12] = () ? fcs differs from A024791 for the first time at n=???
;; 

(definec (LeBorgne-catsigperm n) (catsigperm-by-insertion-mode (list-ref INSERTION-MODES (- n 1))))
(definec (deb-LeBorgne-catsigperm n) (deb-catsigperm-by-insertion-mode (list-ref INSERTION-MODES (- n 1))))

(define (up-and-downs->A014486 uds)
   (let loop ((uds uds) (n 0))
      (cond ((null? uds) n)
            ((eq? 'u (car uds)) (loop (cdr uds) (+ 1 (* 2 n))))
            (else (loop (cdr uds) (* 2 n)))
      )
   )
)


(define (resplice-multiple lista elem items)
   (let loop ((z (list))
              (lista lista)
              (items items)
             )
       (cond ((null? lista) (reverse! z))
             ((equal? (car lista) elem)
               (loop (append! (reverse (car items)) z) (cdr lista) (cdr items))
             )
             (else (loop (cons (car lista) z) (cdr lista) items))
       )
   )
)

;; There is a more intelligent way of doing these substitutions.
;; This is straight from the paper:
(define (insert-by-mode ins-mode sk dyb)
  (resplice-multiple
     (resplice-multiple
        (resplice-multiple dyb sk ins-mode)
        'A (list (list sk) (list sk))
     )
     'B (list (list (+ 1 sk)) (list (+ 1 sk)))
  )
)



(define (ads->A014486-by-insertion-mode ins-mode ads)
   (let loop ((ads ads) (dyb (list 0 0)))
      (cond ((null? ads)
               (up-and-downs->A014486
                   (delete-matching-items! dyb integer?)
               )
            )
            (else
              (loop (cdr ads)
                    (insert-by-mode
                          ins-mode
                          (car ads)
                          (delete-matching-items! dyb
                               (lambda (n) (and (integer? n) (> n (car ads))))
                          )
                    )
              )
            )
      )
   )
)


(define (deb-ads->A014486-by-insertion-mode ins-mode ads)
   (let loop ((ads ads) (dyb (list 0 0)))
      (format #t "almost decr. seq.=~a Dyck building=~a\n" ads dyb)
      (cond ((null? ads)
               (up-and-downs->A014486
                   (delete-matching-items! dyb integer?)
               )
            )
            (else
              (loop (cdr ads)
                    (insert-by-mode
                          ins-mode
                          (car ads)
                          (delete-matching-items! dyb
                               (lambda (n) (and (integer? n) (> n (car ads))))
                          )
                    )
              )
            )
      )
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Related to LeBorgne[12], but not quite there yet!

;; Induces also a permutation of compositions
;; (thus also a permutation of binary strings (run lengths)
;;  and a permutation of N to OEIS.)
;; What is the easiest way to compute the inverse?


(define (*A140587 s) ;; These A-numbers have grown stale, allocate new ones!
  (if (null? s) s
      (let loop ((z (list)) (heads (car s)) (s (cdr s)))
        (cond ((null? s) (append z (list heads)))
              ((null? (car s))
                 (loop (append z (list heads))
                       (list)
                       (cdr s)
                 )
              )
              (else (loop (cons (cdar s) z)
                          (cons (caar s) heads)
                          (cdr s)
                    )
              )
        )
      )
  )
)


(define (*A140588! s) ;; Not purely side-effective!
  (if (< (length s) 2) s
    (let ((h (car (snip-the-last-pair! s))))
      (let loop ((z (list)) (heads h) (s s))
        (cond ((null? s) (cons heads z))
              ((null? heads)
                 (if (null? (cdr s))
                     (loop (cons '() z) (car s) (cdr s))
                     (let ((h (car (snip-the-last-pair! s))))
                        (loop (cons '() z) h s)
                     )
                 )
              )
              (else (loop (cons (cons (car heads) (car s)) z)
                          (cdr heads)
                          (cdr s)
                    )
              )
        )
      )
    )
  )
)

;; Returns the last pair, which is also physically removed from the list.
(define (snip-the-last-pair! s) ;; We must have (length s) > 1 !
   (let loop ((p s) (s (cdr s)))
        (cond ((not (pair? (cdr s)))
                (set-cdr! p '())
                s
              )
              (else (loop s (cdr s)))
        )
   )
)


(define (shove-to-top a b) ;; b is inserted to the top of a.
   (cond ((null? a) b)
         (else (cons (shove-to-top (car a) b) (cdr a)))
   )
)

(define (*Astrange2 s)
  (if (null? s) s
      (let loop ((z (list)) (heads (cdr s)) (s (car s)))
        (cond ((null? s) (shove-to-top z (cons '() heads)))
              ((null? (cdr s))
                 (loop (shove-to-top z (cons '() heads))
                       (list)
                       (car s)
                 )
              )
              (else (loop (cons z (cddr s))
                          (cons (cadr s) heads)
                          (car s)
                    )
              )
        )
      )
  )
)

(define (*Astrange3 s) (*Astrange2 (*A069787! (tree-copy s))))


(define (*Astrange4 s)
  (if (null? s) s
      (let ((s2 (*A069787! (tree-copy s))))
        (let loop ((z (list)) (heads (cdr s2)) (s (car s2)))
          (cond ((null? s) (shove-to-top z (cons '() heads)))
                ((null? (cdr s))
                   (loop (shove-to-top z (cons '() heads))
                         (list)
                         (car s)
                   )
                )
                (else (loop (cons z (*Astrange4 (cddr s)))
                            (cons (cadr s) heads)
                            (car s)
                      )
                )
          )
        )
      )
  )
)


(define (*Astrange5 s)
  (if (null? s) s
      (let ((s2 (*A069769! (tree-copy s)))) ;; *A069787!
        (let loop ((z (list)) (heads (*Astrange5 (cdr s2))) (s (car s2)))
          (cond ((null? s) (shove-to-top z (cons '() heads)))
                ((null? (cdr s))
                   (loop (shove-to-top z (cons '() heads))
                         (list)
                         (car s)
                   )
                )
                (else (loop (cons z (*Astrange5 (cddr s)))
                            (shove-to-top (*Astrange5 (cadr s)) (cons '() heads))
                            (car s)
                      )
                )
          )
        )
      )
  )
)


(define (s-vert->composition s)
  (let loop ((cl (list)) (s s))
     (cond ((null? s) cl)
           (else (loop (cons (1+ (count-pars (cdr s))) cl) (car s)))
     )
  )
)



;; (map (LeBorgne-catsigperm 12) (iota0 64))
;; ;Value 84: (0 1 2 3 4 5 7 6 8 9 10 12 11 13 16 18 17 14 15 21 20 19 22 23 24 26 25 27 30 32 31 28 29 35 34 33 36 42 43 48 47 50 44 49 45 37 38 46 40 39 41 56 59 58 53 55 57 54 51 52 63 62 61 60 64)
;; 
;; (map (catfun1 *Astrange3) (iota0 64))
;; ;Value 85: (0 1 2 3 4 5 7 6 8 9 10 12 11 13 16 18 17 14 15 21 20 19 22 23 24 26 25 27 31 32 30 28 29 35 34 33 36 42 43 49 47 50 44 48 45 37 38 46 40 39 41 56 59 58 53 55 57 54 51 52 63 62 61 60 64)
;; 
;; (map (catfun1 *Astrange4) (iota0 64))
;; ;Value 86: (0 1 2 3 4 5 7 6 8 9 10 12 11 13 16 18 17 14 15 21 20 19 22 23 24 26 25 27 31 32 30 28 29 35 34 33 36 42 43 49 47 50 44 48 45 37 38 46 40 39 41 56 59 58 53 55 57 54 51 52 63 62 61 60 64)
;; 
;; 
;; (same-intfuns? (catfun1 *Astrange3) (catfun1 *Astrange4) 197)
;; ;Value: 109
;; 
;; ((catfun1 *Astrange3) 109)
;; ;Value: 124
;; ((catfun1 *Astrange4) 109)
;; ;Value: 123
;; ((LeBorgne-catsigperm 12) 109) --> 124
;; 
;; (define Astrange1 (catfun1 *Astrange1))

;;(map Astrange1 (iota0 64))
;; (0 1 2 3 4 5 6 7 8 9 10 11 15 13 14 12 16 17 18 19 20 21 22 23 24 25 38 27 28 26 39 43 52 33 41 35 36 37 29 30 40 34 42 31 44 45 46 47 48 49 50 51 32 53 54 55 56 57 58 59 60 61 62 63 64)

;; (define tenner (list (list (list))  (list (list))  (list (list))  (list (list))  (list (list))))
;; tenner
;; ((()) (()) (()) (()) (()))
;; (count-pars tenner) --> 10
;; 
;; (map 1+ (map count-pars tenner)) --> (2 2 2 2 2)
;; 
;; (define t (*Astrange1 tenner)) --> (() () () () (() () () () ()))
;; 
;; (map 1+ (map count-pars t)) --> (1 1 1 1 6)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (5 1 1 1 2)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (1 5 1 1 2)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (1 4 2 1 2)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (1 1 3 3 2)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (1 2 2 1 4)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (3 1 1 3 2)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (1 2 3 1 3)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (2 2 1 3 2)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (1 2 1 3 3)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (2 2 1 2 3)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (2 1 1 3 3)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (2 2 2 1 3)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (2 1 1 4 2)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (1 3 2 1 3)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (2 1 2 3 2)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (1 2 1 2 4)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (3 1 1 2 3)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (2 1 3 1 3)
;; (begin (set! t (*Astrange1 t)) (map 1+ (map count-pars t))) --> (2 2 2 2 2)
;; 


;; (define tenner2 (list (list (list (list (list (list) (list)) (list)) (list)) (list)) (list)))
;; tenner2 --> (((((() ()) ()) ()) ()) ())
;; (s-vert->composition tenner2) --> (2 2 2 2 2)
;; (count-pars tenner2) --> 10
;; 
;; (define t2 (*Astrange3 tenner2)) --> (((((() () () () () ())))))
;; (s-vert->composition t2) --> (6 1 1 1 1)
;; (begin (set! t2 (*Astrange3 t2)) (s-vert->composition t2)) --> (1 1 1 1 6)
;; (begin (set! t2 (*Astrange3 t2)) (s-vert->composition t2)) --> (2 1 1 1 5)
;; (begin (set! t2 (*Astrange3 t2)) (s-vert->composition t2)) --> (2 1 1 2 4)
;; (begin (set! t2 (*Astrange3 t2)) (s-vert->composition t2)) --> (3 1 2 1 3)
;; (begin (set! t2 (*Astrange3 t2)) (s-vert->composition t2)) --> (2 2 3 1 2)
;; (begin (set! t2 (*Astrange3 t2)) (s-vert->composition t2)) --> (2 4 1 2 1)

