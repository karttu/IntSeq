
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;;  http://www.megabaud.fi/~karttu/matikka/Nekomorphisms/gatochek.scm   ;;
;;  - Functions for checking gatomorphism-induced permutations          ;;
;;    against Neil Sloane's OEIS                                        ;;
;;      http://www.research.att.com/~njas/sequences/                    ;;
;;    and also for cursorily checking that the various composition      ;;
;;    identities hold.                                                  ;;
;;                                                                      ;;
;;  THIS IS THE UGLIEST MODULE OF THEM ALL...                           ;;
;;  (dispensable code...)                                               ;;
;;                                                                      ;;
;;  This Scheme-code is coded 2002 by Antti Karttunen,                  ;;
;;  (E-mail: my_firstname.my_surname@iki.fi) and is placed in           ;;
;;  Public Domain.                                                      ;;
;;                                                                      ;;
;;  This should run at least in MIT Scheme Release 7.6.0, for           ;;
;;  which one can find documentation and the pre-compiled binaries      ;;
;;  (for various OS's running in Intel x86 architecture) under the URL: ;;
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

;; Compile as:
;; (cf "c:\\matikka\\Nekomorphisms\\gatochek" "c:\\matikka\\Nekomorphisms\\")
;;

(define output_seq
  (lambda (seq)
      (cond ((null? seq)) ;; No (newline) this time!
            (else (write (car seq))
                  (if (not (null? (cdr seq))) (write-string ","))
                  (output_seq (cdr seq))
            )
      )
  )
)

;; Quite ugly, disposal code:
;; The worst kludge yet: There can be a maximum upper limit
;; overwrite-number in the beginning of the list:
;; Another horrendous kludge added 9. May 2003: If dont-check-inverse? is 
;; an integer instead of #f or #t, then we check inverses and
;; compositions only to that far.

(define (output-seq-link x upto_n upto2n dont-check-inverse?)
  (let* ((third-elem (list-ref x 2))
         (upto_n (if (number? third-elem) (car x) upto_n))
         (upto2n (if (number? third-elem) (min (car x) upto2n) upto2n))
         (x      (if (number? third-elem) (cdr x) x))
         (test_upto_n (if (number? dont-check-inverse?) dont-check-inverse? upto_n))
        )
   (let* ((offset (car x)) ;; Addition to format at Sep 06 2002
          (Anum (cadr x))
          (fun  (caddr x))
          (fun_et_inv_comp
              (and (or (number? dont-check-inverse?) (not dont-check-inverse?))
                   (> (length x) 3)
                   (cadddr x) ;; User can specify it as #f, then it's not checked.
                   (not (null? (cadddr x)))
                   (compose-funlist (list fun (cadddr x)))
              )
          )
          (compositions (and (> (length x) 3) (cddddr x)))
          (iota-used (if (zero? offset) iota0 iota))
          (res-seq (map fun (iota-used upto_n)))
         )
      (write-string "<BR><A HREF=\"http://www.research.att.com/~njas/sequences/A")
      (write Anum)
      (write-string "\">A")
      (write Anum)
      (write-string "</A> := <A HREF=\"http://www.research.att.com/~njas/sequences/?q=")
      (output_seq (list-head (cdr res-seq) upto2n))
      (write-string "\">[")
      (output_seq res-seq)
      (write-string "];</A>\n");
      (cond
        ((> (length x) 3)
          (let ((test-seq (iota-used test_upto_n))
                (nth-comp 1)
               )
             (cond ((not fun_et_inv_comp))
                   ((not (null? (first-dislocated
                                  (append (if (zero? offset) (list) (list 0))
                                      (map fun_et_inv_comp test-seq)))))
                     (write-string "<B>The inverse is not correct!</B>\n")
                   )
             )
             (for-each (lambda (complist)
                         (let ((comp (compose-funlist complist))
                               (test-seq (iota-used upto_n))
                              )
                            (cond ((not (equal? (map comp test-seq) res-seq))
                                    (write-string "<B>The ") (write nth-comp)
                                    (write-string ". composition is not correct!</B>")
                                  )
                            )
                         )
                         (set! nth-comp (1+ nth-comp))
                       )
                       compositions
             ) ; for-each
          ) ; let
        )
      ) ; cond
   ) ; let*
  ) ;; let*
)

;; Call-as
;; (output-check-html "C:\\matikka\\nekomorphisms\\test6918.htm" check-these 6918 32 #f)
;; or:
;; (output-check-html "C:\\matikka\\nekomorphisms\\test2056.htm" check-these 2056 45 #t)


(define (output-check-html filename seqfuns upto_n upto2n do-not-check-invs?)
   (with-output-to-file filename
     (lambda ()
        (write-string
         "<HTML><HEAD><TITLE>Check-up of sequences:")
        (for-each (lambda (x) (write-string " A0") (write (cadr x))) seqfuns)
        (write-string "</TITLE></HEAD><BODY BGCOLOR=\"white\" TEXT=\"blue\" LINKS=\"red\" VLINKS=\"red\">\n")
        (for-each (lambda (x) (output-seq-link x upto_n upto2n do-not-check-invs?)) seqfuns)
        (write-string "</BODY></HTML>")
     )
   )
)


(define (output-check-html2 filename seqfuns upto_n upto2n)
   (with-output-to-file filename
     (lambda ()
        (write-string
         "<HTML><HEAD><TITLE>Check-up of sequences:")
        (for-each (lambda (x) (write-string " A0") (write (cadr x))) seqfuns)
        (write-string "</TITLE></HEAD><BODY BGCOLOR=\"white\" TEXT=\"blue\" LINKS=\"red\" VLINKS=\"red\">\n")
        (for-each (lambda (x) (output-seq-link x upto_n upto2n #t)) seqfuns)
        (write-string "</BODY></HTML>")
     )
   )
)


(define (output-fun-vals filename fun upto_n)
   (with-output-to-file filename
     (lambda ()
        (let loop ((n 0))
           (cond ((< n upto_n)
                    (write (fun n)) (newline)
                    (loop (1+ n))
                 )
           )
        )
     )
   )
)

(define (precompute-some-sequences)
  (begin
    (output-fun-vals "c:\\matikka\\nekomorphisms\\vA069770.lst" A069770! 290512)
    (output-fun-vals "c:\\matikka\\nekomorphisms\\vA072796.lst" A072796  290512)
    (output-fun-vals "c:\\matikka\\nekomorphisms\\vA072771.lst" A072771  290512)
    (output-fun-vals "c:\\matikka\\nekomorphisms\\vA072772.lst" A072772  290512)
    (output-fun-vals "c:\\matikka\\nekomorphisms\\vA072800.lst" A072800  290512)
    (output-fun-vals "c:\\matikka\\nekomorphisms\\vA072764.lst" A072764  3600586)
  )
)


