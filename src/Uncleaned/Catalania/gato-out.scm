
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;;  http://www.megabaud.fi/~karttu/matikka/Nekomorphisms/gato-out.scm   ;;
;;  - Functions for outputting the effects of gatomorphisms             ;;
;;    as a HTML file.                                                   ;;
;;                                                                      ;;
;;  This Scheme module is coded 2002 by Antti Karttunen,                ;;
;;  (E-mail: my_firstname.my_surname@gmail.com) and is placed in        ;;
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



;; Call as:
;; (output-sequence-file gat-list 197 "Apr 11 2003" "./seqs/gatseqs1.txt")

(define (output-sequence-file listlets upto-n subm-date outfile)
   (call-with-output-file outfile
     (lambda (outport)

       (for-each (lambda (listlet)
               (output-gatomorphism-entry listlet upto-n subm-date outport)
            )
            listlets
       )
     )
   )
)


(define (output-gatomorphism-entry listlet upto-n subm-date out)
 (call-with-current-continuation
   (lambda (exit)
     (let* ((Afun0          (list-ref listlet 0))
            (Afun1          (list-ref listlet 1))
            (Anum0          (list-ref listlet 2))
            (Anum1          (cond ((number? (list-ref listlet 3)) (list-ref listlet 3))
                                  (else Anum0)
                            )
            )
            (descr          (list-ref listlet 4))
            (interpret      (list-ref listlet 5))
            (BT-restriction (list-ref listlet 6))
            (T-contraction  (list-ref listlet 7))
            (rest-of        (nthcdr 8 listlet))
            (cc             (cond ((assoc 'CC: rest-of) => cadr) (else #f)))
            (fc             (cond ((assoc 'FC: rest-of) => cadr) (else #f)))
            (mc             (cond ((assoc 'MC: rest-of) => cadr) (else #f)))
            (lc             (cond ((assoc 'LC: rest-of) => cadr) (else #f)))
            (mlc            (cond ((assoc 'MLC: rest-of) => cadr) (else #f)))
            (fixes          (assoc 'FIXES: rest-of))
            (cycles         (assoc 'CYCLES: rest-of))
            (compositions   (assoc 'COMPS: rest-of))
            (sigatbs        (assoc 'SigatB: rest-of))
            (A089840ind     (assoc 'A089840index: rest-of))
            (horcontr       (assoc 'HORCONTR: rest-of))
            (cfs            (assoc 'CF: rest-of))
            (Scheme-funs    (assoc 'SCHEME: rest-of))
            (notes          (assoc 'NOTES: rest-of))
            (SKIP-OUTPUT    (assoc 'SKIP-OUTPUT: rest-of))
            (show-on-sizes  (cond ((assoc 'SoS: rest-of) => cdr)
                                  (else (list 4)))
            )
           )

       (if SKIP-OUTPUT (exit 'now))

       (output-gatomorphism-entry-aux
           out upto-n subm-date
           Afun0 Afun1 Anum0 Anum1
           descr
           notes
           compositions
           sigatbs
           A089840ind
           horcontr
           cfs
           cc fc (or mc mlc) (or lc mlc)
           Scheme-funs
       )

;; And the inverse, if this is not an involution:
       (cond ((not (eq? Afun0 Afun1))
               (output-gatomorphism-entry-aux
                   out upto-n subm-date
                   Afun1 Afun0 Anum1 Anum0
                   descr
                   notes
                   compositions
                   sigatbs
                   A089840ind
                   horcontr
                   cfs
                   cc fc (or mc mlc) (or lc mlc)
                   Scheme-funs
               )
             )
       )

     ) ;; let*
   )
 )
)


;; Works in MIT Scheme:
(define (Anum->Afun Anum)
  (eval (string->symbol (string-downcase (Anum->str Anum))) user-initial-environment)
)

(define (check-composition outport comp inv-perm-seq)
   (let ((Acomp (compose-funlist (map Anum->Afun comp))))
     (cond ((not (null? (first-dislocated (map Acomp inv-perm-seq))))
              (format outport " (this composition is NOT correct!)")
           )
     )
   )
)

;; Arbitrary upper limit needed until the cache-system in gatosiga.scm is rationalized!
(define (check-sigatb outport siganum inv-perm-seq)
  (if (and (number? siganum) (< siganum 600000))
      (let ((Asiga (catfun1 (obtain-consing-siga-function! siganum))))
         (cond ((not (null? (first-dislocated (map Asiga inv-perm-seq))))
                  (format outport " (this siga-index ~A is NOT correct!)" siganum)
               )
         )
      )
  )
)

(define (check-horcontr outport Afun0 Ahorcor id-seq)
  (let ((Azero (lambda (n) (- (Afun0 n) (+ (Ahorcor (A082853 n)) (A082852 n))))))
    (cond ((pos-of-first-matching (map Azero id-seq) (lambda (x) (not (zero? x))))
             (format outport " (this is NOT correct!)")
          )
    )
  )
)


(define (output-gatomorphism-entry-aux
            outport upto_n subm-date
            Afun0 Afun1 Anum0 Anum1
            descr
            notes
            compositions
            sigatbs
            A089840ind
            horcontr
            cfs
            cc fc mc lc
            Scheme-funs
        )
  (let* ((Astr (Anum->str Anum0))
         (Astrinv (Anum->str Anum1))
         (id_seq (iota0 upto_n))
         (perm-seq (map Afun0 id_seq))
         (inv-perm-seq (map Afun1 id_seq))
         (one-based-pos-of-first-term-gte-2
               (1+ (pos-of-first-matching perm-seq (lambda (x) (>= x 2))))
         )
         (part1 (sublist perm-seq 0 26))
         (part2 (sublist perm-seq 26 49))
         (part3 (sublist perm-seq 49 70))
;; '(COMPS: (57161 (57508 69767) (69767 69769) (57163 57162 57163))
;;          (57162 (69768 57508) (69769 69768) (57163 57161 57163))
;;  )
         (comps (and compositions
                     (cond ((assoc Anum0 (cdr compositions)) => cdr))
                )
         )
         (in-A073200 (and sigatbs (cond ((assoc Anum0 (cdr sigatbs)) => cadr))))
         (in-A089840 (and A089840ind (cond ((assoc Anum0 (cdr A089840ind)) => cadr))))
         (horcontr (and horcontr (cond ((assoc Anum0 (cdr horcontr)) => cadr))))
         (cfs (and cfs (cdr cfs)))
         (gatodefs (and (pair? Scheme-funs) (cdr (cadr Scheme-funs))))
        )

   (cond ;; (dont-check-inverse?)
         ((not (null? (first-dislocated (map Afun1 perm-seq))))
            (format outport "!!! The inverse ~A for ~A is not correct\n"
                    Astrinv Astr
            )
         )
   )
   (format outport "%I ~A\n" Astr)
   (with-output-to-port outport
     (lambda ()
       (format outport "%S ~A " Astr) (output_seq part1) (format outport ",\n")
       (format outport "%T ~A " Astr) (output_seq part2) (format outport ",\n")
       (format outport "%U ~A " Astr) (output_seq part3) (newline outport)
     )
   )
   (format outport "%N ~A ~A of natural numbers induced by the gatomorphism ~A acting on the parenthesizations encoded by A014486/A063171.\n"
       Astr
       (if (eq? Afun0 Afun1) "Involution" "Permutation")
       (format #f "gm~A" (string-downcase Astr))
   )
   (if descr
       (format outport "%C ~A ~A.\n" Astr descr)
   )

   (if notes
       (format outport "%C ~A ~A\n" Astr notes)
   )

   (format outport "%H ~A A. Karttunen, <A HREF=\"http://www.iki.fi/kartturi/matikka/Nekomorphisms/gatomorf.htm\">Gatomorphisms</A> <I>(With the complete Scheme source)</I>\n" Astr)
   (format outport "%H ~A <A HREF=\"http://www.research.att.com/~~njas/sequences/Sindx_Per.html#IntegerPermutationCatAuto\">Index entries for signature-permutations induced by Catalan automorphisms</A>\n" Astr)

   (format outport "%Y ~A" Astr)
   (if (not (eq? Afun0 Afun1)) (format outport " Inverse of ~A." Astrinv))

;; We should also check them, but can't do this with just A-numbers, as
;; we need the function definitions. (Here the old-fashioned Lisp would beat Scheme.)
   (cond (comps
           (format outport " a(n)")
           (for-each (lambda (comp)
                        (format outport " = ")
                        (for-each
                            (lambda (fun)
                               (format outport "~A(" (Anum->str fun))
                            )
                            comp
                        )
                        (format outport "n")
                        (for-each (lambda (x) (format outport ")")) comp)
                        (check-composition outport comp inv-perm-seq)
                     )
                     comps
           )
           (format outport ".")
         )
   )

   (cond (horcontr
           (format outport " a(n) = ~A(A082853(n))+A082852(n)." (Anum->str horcontr))
           (check-horcontr outport Afun0 (Anum->Afun horcontr) id_seq)
         )
   )

   (cond (in-A089840
           (format outport " Row ~A of A089840." in-A089840)
         )
   )
   (cond (in-A073200
           (format outport " Occurs in A073200 as row ~A." in-A073200)
           (check-sigatb outport in-A073200 inv-perm-seq)
         )
   )
   (cond (cfs
           (format outport " Cf. also ")
           (for-each (lambda (Anum)
                        (cond ((not (eq? Anum (car cfs))) (format outport ", ")))
                        (format outport "~A" (Anum->str Anum))
                     )
                     cfs
           )
           (format outport ".")
         )
   )

   (newline outport)

   (cond ((or cc fc mc lc)
            (format outport "%Y ~A" Astr)
            (if cc (format outport " Number of cycles: ~A." (Anum->str cc)))
            (if fc (format outport " Number of fixpoints: ~A." (Anum->str fc)))
            (if mc (format outport " Max. cycle size: ~A." (Anum->str mc)))
            (if lc (format outport " LCM of cycle sizes: ~A." (Anum->str lc)))
            (format outport " (In range [A014137(n-1)..A014138(n-1)] of this permutation, possibly shifted one term left or right).\n")
         )
   )

   (format outport "%K ~A nonn\n%O ~A 0,~A\n%A ~A Antti Karttunen (Firstname.Surname@gmail.com), ~A\n"
           Astr Astr one-based-pos-of-first-term-gte-2 Astr subm-date
   )

   (cond (gatodefs
           (format outport "%o ~A (Scheme function~A implementing this automorphism on list-structures:)\n" Astr (if (not (null? (cdr gatodefs))) "s" ""))
           (for-each (lambda (gd) (format outport "%o ~A ~A\n" Astr gd)) gatodefs)
         )
   )

   (newline outport)

  ) ;; let*
)




;; Call as:
;; (html-gatomorf-list gat-list 7 "c:\\matikka\\Nekomorphisms\\gatlist1.htm")

(define (html-gatomorf-list listlets upto-what outfile)
   (call-with-output-file outfile
     (lambda (outport)
       (format outport
            "<HTML><HEAD><TITLE>An Introduction to Gatomorphisms</TITLE></HEAD>\n"
       )
       (format outport
            "<BODY BGCOLOR=\"white\">\n"
       )
       (write-string "<DIV ALIGN=CENTER><H3>Antti Karttunen</H3>\n" outport)
       (write-string "<H1>A Brief Survey on Gatomorphisms.</H1></DIV>\n" outport)

       (write-string
          "<I>(This is a paper in preparation, please don't redistribute" outport)
       (write-string
          " this document or its URL without asking me first.)</I>\n" outport)
       (write-string
          "<P><BR></P>We survey the following gatomorphisms in this document:\n" outport)
       (write-string
          "<OL TYPE=\"i\">\n" outport)

       (for-each (lambda (listlet)
              (cond ((not (assoc 'SKIP-OUTPUT: (nthcdr 8 listlet)))
                       (write-string "<LI>" outport)
                       (html-out-seq-A-links (caddr listlet) (cadddr listlet) #t outport)
                       (format outport " - ~A~%" (list-ref listlet 4))
                    )
              )
            )
            listlets
       )

       (write-string
          "</OL><P><BR></P><HR>\n" outport)

       (for-each (lambda (listlet)
               (html-out-gatomorf-pair-description listlet upto-what outport)
            )
            listlets
       )
       (write-string "<HR><P>This HTML-document generated with <A HREF=\"http://www.iki.fi/~kartturi/matikka/Nekomorphisms/gato-out.scm\">http://www.iki.fi/~kartturi/matikka/Nekomorphisms/gato-out.scm</A>.<BR>\n" outport)
       (write-string "For other needed sources, see <A HREF=\"http://www.iki.fi/~kartturi/matikka/Nekomorphisms/gatomorf.htm\">http://www.iki.fi/~kartturi/matikka/Nekomorphisms/gatomorf.htm</A> </P>" outport)
       (write-string "</BODY></HTML>\n" outport)
     )
   )
)

(define (html-out-sequence-search-link2 seq out)
  (with-output-to-port out
   (lambda ()
      (write-string "<A HREF=\"http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eishis.cgi?sequence=")
      (output_seq (list-head (cdr seq) (- (length seq) 2)))
      (write-string "\">")
      (output_seq seq)
      (write-string ",...</A>\n")
   )
  )
)

(define (html-out-Anchor Anum out)
  (format out "<A NAME=\"~A\"></A>" (Anum->str Anum))
)

(define (Anum->str Anum)
  (string-append "A"
      (string-pad-left (if (string? Anum) Anum (number->string Anum)) 6 #\0)
  )
)

(define (html-out-sequence-A-link3 Anum inner? out)
  (let ((Astr (Anum->str Anum)))
    (if inner?
       (format out "<A HREF=\"#~A\">~A</A>" Astr Astr)
       (format out "<A HREF=\"http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=~A\">~A</A>" Astr Astr)
    )
  )
)

(define (html-out-seq-A-links Anum Anum-inv inner? out)
    (if (not inner?) (html-out-Anchor Anum out))
    (html-out-sequence-A-link3 Anum inner? out)
    (cond ((integer? Anum-inv)
             (format out "/")
             (if (not inner?) (html-out-Anchor Anum-inv out))
             (html-out-sequence-A-link3 Anum-inv inner? out)
          )
    )
)

(define (html-out-compositions compositions out)
  (let loopout ((compositions compositions))
    (cond
       ((pair? compositions)
         (write-string " <B>=</B> " out)
         (let loopin ((cs (car compositions)))
            (cond ((null? cs))
                  (else
                      (html-out-sequence-A-link3 (car cs) #t out)
                      (if (pair? (cdr cs))
                          (write-string " o " out)
                      )
                      (loopin (cdr cs))
                  )
            )
         )
         (loopout (cdr compositions))
       )
    )
  )  
)


(define (latex-out-compositions compositions out)
  (let loopout ((compositions compositions))
    (cond
       ((pair? compositions)
         (write-string " = " out)
         (let loopin ((cs (car compositions)))
            (cond ((null? cs))
                  (else
                      (format out "\\autname{A~a}" (car cs))
                      (if (pair? (cdr cs))
                          (write-string " $\\circ$ " out)
                      )
                      (loopin (cdr cs))
                  )
            )
         )
         (loopout (cdr compositions))
       )
    )
  )  
)



;;    '(RCOMPS: (57163 (FORK 69770)))   
;;  Recursive composition: & \autname{A057163} = (FORK \autname{A069770}). \\

(define (latex-out-rcompositions compositions out)
  (let loopout ((compositions compositions))
    (cond
       ((pair? compositions)
         (write-string " = " out)
         (let ((cs (car compositions)))
            (format out "(~a \\autname{A~a})"
                    (string-upcase (symbol->string (car cs)))
                    (cadr cs)
            )
         )
         (loopout (cdr compositions))
       )
    )
  )
)

(define (output-n-times out n what)
  (let loop ((n n))
     (cond ((not (zero? n))
              (write-string what out)
              (loop (- n 1))
           )
     )
  )
)


;; Keep on loopin'

(define (latex-out-seqinfo out title seqinfolist rowend)
  (if seqinfolist (format out "~A: & " title))
  (let loopin ((cs seqinfolist) (i 0))
       (cond ((null? cs)
                 (output-n-times out i ")")
                 (format out ".~A" rowend)
             )
             ((symbol? (car cs))
                 (format out "$~A$(" (string-upcase (symbol->string (car cs))))
                 (loopin (cdr cs) (+ 1 i))
             )
             ((string? (car cs))
                 (format out "~A(" (car cs))
                 (loopin (cdr cs) (+ 1 i))
             )
             (else ;; It should be a fixnum, presumably the last one!
                 (format out "~A" (latex-eisseq (car cs)))
                 (loopin (cdr cs) i)
             )
       )
  )
)



;; (define (latex-out-all-compositions out title comps latex-out-comps-fun rowend)
;;     (format out "~A: & " title)
;;     (for-each (lambda (c)
;;                  (format out "~A " (latex-gatname (car c)))
;;                  (latex-out-comps-fun (cdr c) out)
;;                  (write-string "\n" out)
;;               )
;;               comps
;;     )
;;     (format out rowend)
;; )


(define (latex-out-all-compositions out title compositions latex-out-comps-fun rowend)
    (format out "~A: & " title)
    (let loop ((comps compositions) (i 0))
      (cond ((pair? comps)
               (if (not (eq? comps compositions)) (write-string " & " out))
               (format out "~A " (latex-gatname (car (car comps))))
               (latex-out-comps-fun (cdr (car comps)) out)
               (write-string rowend out)
               (loop (cdr comps) (+ i 1))
            )
      )
    )
)


(define (html-out-gatomorf-pair-description listlet upto-what out)
 (call-with-current-continuation
   (lambda (exit)
     (let* ((Afun0          (list-ref listlet 0))
            (Afun1          (list-ref listlet 1))
            (Anum           (list-ref listlet 2))
            (Anum-inv       (list-ref listlet 3))
            (descr          (list-ref listlet 4))
            (interpret      (list-ref listlet 5))
            (BT-restriction (list-ref listlet 6))
            (T-contraction  (list-ref listlet 7))
            (rest-of        (nthcdr 8 listlet))
            (partition-lengths (map (lambda (n)
                                      (map length (partition-by-gatoAfun n Afun0))
                                    )
                                    (iota upto-what)
                               )
            )
            (cc-seq         (cons 1 (map length partition-lengths)))
            (fc-seq         (cons 1 (map num-of-ones partition-lengths)))
            (mc-seq         (cons 1 (map (lambda (s) (fold-left max 0 s))
                                         partition-lengths
                                    )
                            )
            )
            (lc-seq         (cons 1 (map (lambda (s) (fold-left lcm 1 s))
                                         partition-lengths
                                    )
                            )
            )
   
            (cc             (assoc 'CC: rest-of))
            (fc             (assoc 'FC: rest-of))
            (mc             (assoc 'MC: rest-of))
            (lc             (assoc 'LC: rest-of))
            (mlc            (assoc 'MLC: rest-of))
            (fixes          (assoc 'FIXES: rest-of))
            (cycles         (assoc 'CYCLES: rest-of))
            (compositions   (assoc 'COMPS: rest-of))
            (SigatBs        (assoc 'SigatB: rest-of))
            (A089840ind     (assoc 'A089840index: rest-of))
            (Scheme-funs    (assoc 'SCHEME: rest-of))
            (notes          (assoc 'NOTES: rest-of))
            (SKIP-OUTPUT    (assoc 'SKIP-OUTPUT: rest-of))
            (show-on-sizes  (cond ((assoc 'SoS: rest-of) => cdr)
                                  (else (list 4)))
            )
           )
 
       (if SKIP-OUTPUT (exit 'now))
  
       (format out "<P><BR></P><H3>")
       (html-out-seq-A-links Anum Anum-inv #f out)
   
       (format out "</H3>\n<DL>\n")
   
       (format out "<BR><BR><DT><B>Description:</B><DD>~A.\n<BR>\n" descr)
   
       (flush-output out)
   
       (cond ((or fixes fc fc-seq)
               (format out "<BR><BR><DT><B>Fixes:</B>~%"
               )
               (cond ((not fixes) "?")
                     (else
                        (for-each (lambda (text)
                                    (format out "<DD>~A.~%" text)
                                  )
                                  (cdr fixes)
                        )
                     )
               )
   
               (format out "<BR><BR><DD><B>Counted by ~A:</B> = "
                         (html-out-sequence-A-link3 (if fc (cadr fc) "?????") #f #f)
               )
               (html-out-sequence-search-link2 fc-seq out)
             )
       )
   
   
   
       (cond ((or cycles cc cc-seq)
               (format out "<BR><BR><DT><B>Cycles correspond to:</B>~%"
               )
               (cond ((not cycles) "?")
                     (else
                        (for-each (lambda (text)
                                    (format out "<DD>~A.~%" text)
                                  )
                                  (cdr cycles)
                        )
                     )
               )
   
               (format out "<BR><BR><DD><B>Counted by ~A:</B> = "
                         (html-out-sequence-A-link3 (if cc (cadr cc) "?????") #f #f)
               )
               (html-out-sequence-search-link2 cc-seq out)
             )
       )
   
       (cond (mlc
              (format out "<BR><BR><DT><B>Max. cycle lengths and L.C.M.s of all cycle lengths given by:</B><DD>~A = \n"
                  (html-out-sequence-A-link3 (if mlc (cadr mlc) "?????") #f #f)
              )
              (html-out-sequence-search-link2 mc-seq out)
             )
             (else
               (format out "<BR><BR><DT><B>Max. cycle lengths given by:</B><DD>~A = \n"
                       (html-out-sequence-A-link3 (if mc (cadr mc) "?????") #f #f)
               )
               (html-out-sequence-search-link2 mc-seq out)
           
               (format out "<BR><BR><DT><B>L.C.M.s of cycle lengths given by:</B><DD>~A = \n"
                       (html-out-sequence-A-link3 (if lc (cadr lc) "?????") #f #f)
               )
               (html-out-sequence-search-link2 lc-seq out)
             )
       )
   
   
       (format out "<BR><BR><DT><B>L-word permuting:</B><DD>~A\n"
               (cond ((not (integer? BT-restriction)) "No.")
                     (else
                       (format #f
                          "Yes, the restriction to binary trees induces the gatomorphism ~A."
                          (html-out-sequence-A-link3 BT-restriction #t #f)
                       )
                     )
               )
       )
   
       (format out "<BR><BR><DT><B>Telescoping:</B><DD>~A\n"
               (cond ((not (integer? T-contraction)) "No.")
                     (else
                       (format #f
                          "Yes, contraction gives the permutation ~A."
                            (html-out-sequence-A-link3 T-contraction #f #f)
                       )
                     )
               )
       )

       (cond (SigatBs
                (format out "<BR><BR><DT><B>Occurs in <A HREF=\"http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=A073200\">A073200</A></B>:")
                (for-each
                   (lambda (sb)
                      (format out "<DD>~A: ~A\n"
                              (Anum->str (car sb))
                              (cond ((integer? (cadr sb))
                                        (format #f "as the row ~A." (cadr sb))
                                    )
                                    (else (cadr sb))
                              )
                      )
                   )
                   (cdr SigatBs)
                )
             )
       )
   
   ;; '(COMPS: (57509 (57501 69770)) (57510 (69770 57502)))
       (cond (compositions
               (format out "<BR><BR><DT><B>Composed of:</B>\n")
               (for-each (lambda (c)
                           (format out "<DD>~A "  (Anum->str (car c)))
                           (html-out-compositions (cdr c) out)
                           (write-string "\n" out)
                         )
                         (cdr compositions)
               )
             )
       )
   
       (cond (notes
               (format out "<BR><BR><DT><B>Notes:</B><DD>\n")
               (for-each (lambda (s) (write-string s out)) (cdr notes))
             )
       )
   
       (cond (Scheme-funs
               (format out "<BR><BR><DT><B>Scheme functions implementing this gatomorphism on S-expressions:</B>\n<P>")
               (for-each
                  (lambda (sublist)
                     (format out "<DD>~A:<BR>\n<PRE>\n" (car sublist)) ;; The title (a string)
                     (for-each
                        (lambda (fun)
                           (pretty-print fun out)
                           (newline out) (newline out)
                        )
                        (cdr sublist)
                     )
                     (format out "</PRE>\n")
                  )
                  (cdr Scheme-funs)
               )
             )
       )

   
       (for-each
          (lambda (nth-forest)
            (format out "<BR><BR><DT><B>The effect of this gatomorphism on the forest Cat[~A] viewed as ~A.</B>\n"
                    nth-forest (map-letterlist-to-words interpret)
            )
            (format out "</DL><BR>\n")
   
            (html-show-cycle-partitions out (partition-by-gatoAfun nth-forest Afun0)
                interpret 8 nth-forest)
   
            (flush-output out)
          )
          show-on-sizes
       )
     )
   )
 )
)



(define (latex-eisseq Anum)
  (let ((Astr (Anum->str Anum)))
    (format #f "\\EISseq{~A}" Astr)
  )
)

(define (latex-gatname Anum)
  (let ((Astr (Anum->str Anum)))
    (format #f "\\autname{~A}" (string-upcase Astr))
  )
)

(define (latex-str-gatomtitle Anum Anum-inv)
        (gen-latex-str-gatomtitle Anum Anum-inv "/" latex-gatname)
)


(define (latex-str-gatotexfilename Anum Anum-inv)
        (string-append (gen-latex-str-gatomtitle Anum Anum-inv "-" Anum->str) ".tex")
)

(define (gen-latex-str-gatomtitle Anum Anum-inv sepchar Anum2str-fun)
    (string-append (Anum2str-fun Anum)
                   (if (integer? Anum-inv)
                       (string-append sepchar (Anum2str-fun Anum-inv))
                       ""
                   )
    )
)

;; Call as (latex-gatomorf-list gat-list 7 "c:/karttu/Nekomorphisms/gatopape")

(define (latex-gatomorf-list listlets upto-what directory)
  (for-each
    (lambda (listlet)
      (let* ((Anum     (list-ref listlet 2))
             (Anum-inv (list-ref listlet 3))
             (filename (string-append directory "/" (latex-str-gatotexfilename Anum Anum-inv)))
            )
        (latex-gatomorf-list-all-to-one-file (list listlet) upto-what filename)
      )
    )
    listlets
  )
)

(define (latex-gatomorf-list-all-to-one-file listlets upto-what outfile)
   (call-with-output-file outfile
     (lambda (outport)
       (for-each (lambda (listlet)
               (latex-out-gatomorf-pair-description listlet upto-what outport)
            )
            listlets
       )
     )
   )
)


(define (latex-out-gatomorf-pair-description listlet upto-what out)
 (call-with-current-continuation
   (lambda (exit)
     (let* ((Afun0          (list-ref listlet 0))
            (Afun1          (list-ref listlet 1))
            (Anum           (list-ref listlet 2))
            (Anum-inv       (list-ref listlet 3))
            (descr          (list-ref listlet 4))
            (interpret      (list-ref listlet 5))
            (BT-restriction (list-ref listlet 6))
            (T-contraction  (list-ref listlet 7))
            (rest-of        (nthcdr 8 listlet))
            (partition-lengths (map (lambda (n)
                                      (map length (partition-by-gatoAfun n Afun0))
                                    )
                                    (iota upto-what)
                               )
            )
            (cc-seq         (cons 1 (map length partition-lengths)))
            (fc-seq         (cons 1 (map num-of-ones partition-lengths)))
            (mc-seq         (cons 1 (map (lambda (s) (fold-left max 0 s))
                                         partition-lengths
                                    )
                            )
            )
            (lc-seq         (cons 1 (map (lambda (s) (fold-left lcm 1 s))
                                         partition-lengths
                                    )
                            )
            )
   
            (cc             (cond ((assoc 'cc: rest-of) => cdr) (else #f)))
            (fc             (cond ((assoc 'fc: rest-of) => cdr) (else #f)))
            (mlc            (cond ((assoc 'mlc: rest-of) => cdr) (else #f)))
            (mc             (or mlc (cond ((assoc 'mc: rest-of) => cdr) (else #f))))
            (lc             (or mlc (cond ((assoc 'lc: rest-of) => cdr) (else #f))))
            (fixes          (assoc 'FIXES: rest-of))
            (cycles         (assoc 'CYCLES: rest-of))
            (compositions   (assoc 'COMPS: rest-of))
            (rcompositions  (assoc 'RCOMPS: rest-of))
            (SigatBs        (assoc 'SigatB: rest-of))
            (A089840ind     (assoc 'A089840index: rest-of))
            (Scheme-funs    (assoc 'SCHEME: rest-of))
            (notes          (assoc 'NOTES: rest-of))
            (SKIP-OUTPUT    (assoc 'SKIP-OUTPUT: rest-of))
            (show-on-sizes  (cond ((assoc 'SoS: rest-of) => cdr)
                                  (else (list 4)))
            )
            (rowend         "\\\\\n")
           )
 
       (if SKIP-OUTPUT (exit 'now))

       (format out "\\beginautdesc{~a}\n" (latex-str-gatomtitle Anum Anum-inv))

;; Fixed points counted by: & $AERATED$(\EISseq{A000108}). \\
;; Cycles counted by: & \EISseq{A007595}. \\
;; Max. cycle lengths given by: & $LEFT$(\EISseq{A046698}). \\ 
;; LCM's of cycle lengths given by: & $LEFT$(\EISseq{A046698}). \\

       (latex-out-seqinfo out "Fixed points counted by" fc rowend)
       (latex-out-seqinfo out "Cycles counted by" cc rowend)
       (latex-out-seqinfo out "Max. cycle lengths given by" mc rowend)
       (latex-out-seqinfo out "LCM's of cycle lengths given by" lc rowend)

;; Lukasiewicz-word permuting: & No. \\
;; Telescoping: & No. \\
   
       (format out "Lukasiewicz-word permuting: & ~A~A"
               (cond ((not (integer? BT-restriction)) "No.")
                     (else
                       (format #f
                          "Yes, the restriction to binary trees induces ~A."
                          (latex-gatname BT-restriction)
                       )
                     )
               )
               rowend
       )
   
       (format out "Telescoping: & ~A~A"
               (cond ((not (integer? T-contraction)) "No.")
                     (else
                       (format #f
                          "Yes, contraction gives the permutation ~A."
                          (latex-eisseq T-contraction)
                       )
                     )
               )
               rowend
       )


;;    '(RCOMPS: (57163 (FORK 69770)))   
;; Recursive composition: & \autname{A057163} = (FORK \autname{A069770}). \\
       (cond (rcompositions
               (latex-out-all-compositions out
                                           "Recursive compositions"
                                           (cdr rcompositions)
                                           latex-out-rcompositions
                                           rowend
               )
             )
       )

;; '(COMPS: (57509 (57501 69770)) (57510 (69770 57502)))

       (cond (compositions
               (latex-out-all-compositions out
                                           "Compositions"
                                           (cdr compositions)
                                           latex-out-compositions
                                           rowend
               )
             )
       )

   
;;     (cond (notes
;;             (format out "<BR><BR><DT><B>Notes:</B><DD>\n")
;;             (for-each (lambda (s) (write-string s out)) (cdr notes))
;;           )
;;     )

       (cond (Scheme-funs
;;             (format out "<BR><BR><DT><B>Scheme functions implementing this gatomorphism on S-expressions:</B>\n<P>")
               (for-each
                  (lambda (sublist)
                     (format out "~A: &\n\\beginppschemecode\n" (car sublist)) ;; The title (a string)
                     (for-each
                        (lambda (fun)
                           (pretty-print fun out)
                           (newline out) ;; (newline out)
                        )
                        (cdr sublist)
                     )
                     (format out "\\end{verbatim}\n\\endppschemecode\n~A" rowend)
                  )
                  (cdr Scheme-funs)
               )
             )
       )

       (format out "\\finautdesc\n")

   
       (format out "\n\n~A.\n" descr)
   
       (flush-output out)
   
   
;;        (for-each
;;           (lambda (nth-forest)
;;             (format out "<BR><BR><DT><B>The effect of this gatomorphism on the forest Cat[~A] viewed as ~A.</B>\n"
;;                     nth-forest (map-letterlist-to-words interpret)
;;             )
;;             (format out "</DL><BR>\n")
;;    
;;             (html-show-cycle-partitions out (partition-by-gatoAfun nth-forest Afun0)
;;                 interpret 8 nth-forest)
;;    
;;             (flush-output out)
;;           )
;;           show-on-sizes
;;        )

     )
   )
 )
)


;; (insert-after-every-nth-element! (iota 10) 3 '+)
;; --> (1 2 3 + 4 5 6 + 7 8 9 + 10)
;;
;; (insert-after-every-nth-element! (iota 5) 1 '+) --> (1 + 2 + 3 + 4 + 5)

(define (insert-after-every-nth-element! lista n item)
  (let loop ((p lista) (i 0))
    (cond ((null? p) lista)
          ((= i n)
                (attach! item p)
                (loop (cdr p) 0)
          )
          (else (loop (cdr p) (1+ i)))
    )
  )
)


;; (reversed-cut-after-every-nth-element!  (iota 3) 3 '+) --> ((1 2 3))
;; (reversed-cut-after-every-nth-element!  (iota 9) 3 '+)
;; --> ((+ 8 9) (+ 6 7) (+ 4 5) (1 2 3))
;; (reversed-cut-after-every-nth-element!  (iota 10) 3 '+)
;; --> ((+ 10) (+ 8 9) (+ 6 7) (+ 4 5) (1 2 3))
;; (reversed-cut-after-every-nth-element!  (iota 10) 2 '+)
;; --> ((+ 10) (+ 9) (+ 8) (+ 7) (+ 6) (+ 5) (+ 4) (+ 3) (1 2))

;; (reversed-cut-after-every-nth-element!  (iota 3) 3 '+) --> (((1 2 3)))
;;(reversed-cut-after-every-nth-element!  (iota 9) 3 '+)
;; --> (((+ 8 9)) ((+ 6 7)) ((+ 4 5)) ((1 2 3)))
;; (reversed-cut-after-every-nth-element!  (iota 10) 3 '+)
;; --> (((+ 10)) ((+ 8 9)) ((+ 6 7)) ((+ 4 5)) ((1 2 3)))
;; (reversed-cut-after-every-nth-element!  (iota 10) 2 '+)
;; --> (((+ 10)) ((+ 9)) ((+ 8)) ((+ 7)) ((+ 6)) ((+ 5)) ((+ 4)) ((+ 3)) ((1 2)))

(define (reversed-cut-after-every-nth-element! lista n item)
  (let loop ((p lista) (res (list (list lista))) (i 0))
    (cond ((null? p) res)
          ((and (pair? (cdr p)) (= i (-1+ n)))
              (let ((ex-cdr-p (cdr p)))
                (set-cdr! p (list)) ;; Cut at this point.
                (loop ex-cdr-p (cons (list (cons item ex-cdr-p)) res) 1)
              )
          )
          (else (loop (cdr p) res (1+ i)))
    )
  )
)


(define (choose-max-cycle-lte-from lte-to-this src-cycles)
  (cond ((null? src-cycles) src-cycles)
        ((<= (length (car src-cycles)) lte-to-this) (car src-cycles))
        (else (choose-max-cycle-lte-from lte-to-this (cdr src-cycles)))
  )
)



;; (partition-cycle-partition
;;   '((a b c d e f g h i j k) (1 2 3 4 5 6 7 8) (1 2 3 4 5) (1 2 3 4) (11) (1 2 3) (4 5 6 7))
;;   5
;; )
;; --> (((a b c d e)) ((+ f g h i)) ((+ j k)) ((1 2 3 4 5)) ((+ 6 7 8)) ((1 2 3 4 5)) ((1 2 3 4)) ((4 5 6 7)) ((1 2 3) (11)))



(define (partition-cycle-partition src-cycles max-per-line)
   (let loop ((src-cycles (sort src-cycles (lambda (a b) (> (length a) (length b)))))
              (still-fits-n max-per-line)
              (dst (list (list)))
             )
       (cond ((null? src-cycles)
                (cond ((null? (car dst)) (loop src-cycles still-fits-n (cdr dst)))
                      (else (reverse! (map reverse! dst)))
                )
             )
             ((> (length (car src-cycles)) (- max-per-line 2))
                (loop (cdr src-cycles)
                      max-per-line
                      (cons (list)
                         (append!
                               (reversed-cut-after-every-nth-element!
                                                    (car src-cycles)
                                                    max-per-line
                                                    '+
                               )
                               (cdr dst)
                         )
                      )
                )
             )
             (else ;; past the overlong cycles.
               (let ((cycle-chosen (choose-max-cycle-lte-from still-fits-n src-cycles)))
                 (cond ((null? cycle-chosen)  ;; No more fits to this row.
                          (loop src-cycles   ;; Start the next one.
                                max-per-line
                                (cons (list) dst)
                          )
                       )
                       (else ;; Still fits onto this row.
                          (loop (delete cycle-chosen src-cycles)
                                (- still-fits-n (1+ (length cycle-chosen)))
                                (cons (cons cycle-chosen (car dst)) (cdr dst))
                          )
                       )
                 ) ;; cond
               ) ;; let
             ) ;; else, past the overlong cycles.
       ) ;; cond
   ) ;; let loop
)


(define (html-show-cycle-partitions out cycles interpret max-per-line size)
  (format out "<TABLE>\n")
  (let loop ((cycpart (partition-cycle-partition cycles max-per-line)))
       (cond ((pair? cycpart)
                (html-show-row-or-more
                   out
                   (reduce (lambda (x y) (append x (list (list)) y))
                           (list)
                           (car cycpart)
                   )
                   interpret
                   max-per-line
                   size
                )
                (format out "<TR><TH VALIGN=BOTTOM><P></P></TH></TR>")
                (loop (cdr cycpart))
             )
       )
  )
  (format out "</TABLE>\n")
)


(define (html-show-row-or-more out cycles interpret max-per-line size)
  (for-each
       (lambda (i)
         (html-show-row-for-one-interpretation out i cycles max-per-line size)
       )
       interpret
  )
)

(define (html-show-row-for-one-interpretation out il cycle max-per-line size)
  (format out "<TR>")
  (let loop ((cycle cycle))
     (cond ((pair? cycle)
              (format out "<TH VALIGN=BOTTOM>~A</TH>" (form-html-interpretation il (car cycle) size))
              (loop (cdr cycle))
           )
     )
  )
  (format out "</TR>\n")
)


(define (form-html-interpretation letra n size)
 (cond
   ((eq? n '+) ;; "Continued on the next line" marker.
     (string-append "<LISTING>" (make-string (1+ (* 2 size)) #\-) "&gt;</LISTING>")
   )
   ((not (integer? n)) ;; A delimiter between different cycles output on the same row.
     (let loop ((s "") (n (* 2 (1+ size))))
            (cond ((zero? n) (string-append "<LISTING>" s "</LISTING>"))
                  (else (loop (string-append s "&nbsp;") (-1+ n)))
            )
     )
   )
   (else
    (case letra
     ((a d e n qq rr) (format #f "<IMG SRC=\"kuvat/~A~A.gif\">" letra n))
     ((i) ;; "Dyck paths (mountain ranges)"
          (string-append
            "<LISTING>"
            (html-form-ascii-Dyck-path (A014486->parenthesization (A014486 n))
                                       "/" "\\" " "  "<BR>"
            )
            "</LISTING>"
          )
     )
     ((L) ;; "Lukasiewicz-words"
          (string-append
            "<LISTING>"
            (fold-right string-append
                        ""
                        (map (lambda (x) (format #f "~A" x))
                             (append! (p->Lw (A014486->parenthesization (A014486 n)))
                                      (list 0)
                             )
                        )
            )
            "</LISTING>"
          )
     )
     ((P) (list->string-dense (A014486->parenthesization (A014486 n))
                   "<LISTING><FONT COLOR=\"red\" FACE=\"Times New Roman,Times\">(</FONT>"
                   "<FONT COLOR=\"red\" FACE=\"Times New Roman,Times\">)</FONT></LISTING>"
          )
     )
    ) ;; case
   ) ;; else
 ) ;; cond
)


(define (list->string-dense lista begtok endtok)
  (string-append
    begtok
    (with-output-to-string
      (lambda ()
        (let recurse ((lista lista))
          (cond ((pair? lista)
                   (write-string "(")
                   (recurse (car lista))
                   (write-string ")")
                   (recurse (cdr lista))
                )
          )
        )
      )
    )
    endtok
  )
)

(define (add-chars-to-string slopestr space x str)
    (string-append str
                   (string-pad-left slopestr
                                    (1+ (- x (string-length str)))
                                    (string-ref space 0)
                   )
    )
)


(define (add-chars-to-x-y-string! slopestr space x y list-of-strings)
  (let* ((len (length list-of-strings))
         (yth-string-pos (and (< y len) (nthcdr (- (-1+ len) y) list-of-strings)))
        )
   (cond (yth-string-pos
           (set-car! yth-string-pos
                     (add-chars-to-string slopestr space x (car yth-string-pos))
           )
         )
         (else
           (attach! (add-chars-to-string slopestr space x "")
                    list-of-strings
           )
         )
   )
  )
)



(define (html-form-ascii-Dyck-path s upchar downchar space nl)
   (let ((list-of-strings (list "")) (x-now (list 0)))
      (let recurse ((s s) (y 0))
         (cond ((pair? s)
                   (add-chars-to-x-y-string! upchar   space (car x-now) y list-of-strings)
                   (set-car! x-now (1+ (car x-now)))
                   (recurse (car s) (1+ y))
                   (add-chars-to-x-y-string! downchar space (car x-now) y list-of-strings)
                   (set-car! x-now (1+ (car x-now)))
                   (recurse (cdr s) y)
               )
         )
      )
      (reduce (lambda (x y) (string-append x nl y))
              (list)
              (map (lambda (s) (string-pad-right s (car x-now)))
                   list-of-strings
              )
      )
   )
)


(define (map-intletter-to-word let)
   (case let
     ((a)  "polygon triangulations")
     ((d)  "binary trees")
     ((e)  "general trees")
     ((i)  "Dyck paths (mountain ranges)")
     ((n)  "noncrossing handshakes")
     ((qq) "noncrossing partitions")
     ((rr) "noncrossing Murasaki-diagrams")
     ((L)  "Lukasiewicz-words")
     ((P)  "parenthesizations")
   )
)

(define (map-letterlist-to-words letlist)
   (join-words-with-commas-et-and (map map-intletter-to-word letlist))
)

(define (join-words-with-commas-et-and orgwords)
   (let loop ((s "") (words orgwords))
      (cond ((not (pair? words)) s)
            (else (loop (string-append
                             s
                             (car words)
                             (cond ((null? (cdr words)) "")
                                   ((null? (cddr words)) " and ")
                                   (else ", ")
                             )
                        )
                        (cdr words)
                  )
            )
      )
   )
)


;; The most interesting ones:


(define gat-list-nonrecursives
 (list
  (list A069770 A069770 69770 * "Swap binary tree sides"
         '(d a P L) - -
         '(A089840index: (69770 1))
         '(SigatB: (69770 0))
         '(SoS: 3)
         '(CC: 07595) '(FC: AERATED 000108) '(MLC: LEFT 46698)
         '(FIXES: "binary trees whose both sides are identical")
         '(CYCLES: "rooted planar binary trees upto left-right swap"
                   "necklaces of n+1 white beads and n-1 black beads. [Correspondence with above requires Raney's lemma.]"
          )
         '(SCHEME:
            ("Constructive variant"
               (define (*A069770 s)
                  (cond ((not (pair? s)) s)
                        (else (cons (cdr s) (car s)))
                  )
               )
            )
            ("Destructive variant"
               (define (*A069770! s)
                  (if (pair? s) (swap! s))
                  s
               )
             )
          )
  )
  (list A072796 A072796 72796 * "Exchange the two leftmost branches of general trees if the degree of root &gt; 1, otherwise keep the tree intact"
         '(e i P L)   '????? -
         '(A089840index: (72796 2))
         '(SigatB: (72796 1))
         '(SoS: 4)
         '(CC: 73191) '(FC: 73190) '(MLC: 46698)
         '(FIXES: "general plane trees whose root degree is either 1, or the two leftmost branches of the root are identical")
         '(SCHEME:
            ("Constructive variant"
               (define (*A072796 s)
                  (cond ((not (pair? s)) s)
                        ((not (pair? (cdr s))) s)
                        (else (cons (cadr s) (cons (car s) (cddr s))))
                  )
               )
            )
            ("Destructive variant"
               (define (*A072796! s)
                  (cond ((not (pair? s)) s)
                        ((not (pair? (cdr s))) s)
                        (else (swap! s) (robr! s) (swap! (cdr s)) s)
                  )
               )
            )
          )
  )

  (list A089850 A089850 89850 * "Swap cadr and cddr of an S-exp if its length > 1, i.e. transform (a . (b . c)) to (a . (c . b)) if possible"
         '(e i P L)   '????? -
         '(SoS: 4)
         '(A089840index: (89850 3))
         '(CC: 73191) '(FC: 73190) '(MLC: 46698)
         '(COMPS: (89850 (69770 89859) (89863 69770) (57163 89854 57163))
          )
;;       '(FIXES: "general plane trees whose root degree is either 1, or the ...")
         '(SCHEME:
            ("Destructive variant"
               (define (*A089850! s) ;; (a . (b . c)) --> (a . (c . b))
                 (cond ((not (pair? s)) s)
                       ((not (pair? (cdr s))) s)
                       (else (swap! (cdr s)) s)
                 )
               )

               (define (swap! s)
                 (let ((ex-car (car s)))
                    (set-car! s (cdr s))
                    (set-cdr! s ex-car)
                    s
                 )
               )

            )
          )
  )

  (list A089851 A089853 89851 89853 "Rotate car, cadr and cddr of an S-exp if its length > 1"
         '(e i P L)   '????? -
         '(SoS: 4)
         '(A089840index: (89851 4) (89853 6))
         '(COMPS: (89851 (89850 72796) (57163 89857 57163))
                  (89853 (72796 89850) (57163 89855 57163))
          )
         '(CC: 89847) '(FC: 89848)
;;       '(FIXES: "general plane trees whose root degree is either 1, or the ...")
         '(SCHEME:
            ("Destructive variants"
               (define (*A089851! s) ;; (a . (b . c)) --> (b . (c . a))
                 (cond ((not (pair? s)) s)
                       ((not (pair? (cdr s))) s)
                       (else (swap! s)
                             (robr! s)
                             s
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

               (define (swap! s)
                 (let ((ex-car (car s)))
                    (set-car! s (cdr s))
                    (set-cdr! s ex-car)
                    s
                 )
               )

               (define (robr! s)
                 (let ((ex-cdr (cdr s)))         ;; <- rest
                     (set-cdr! s (caar s))       ;; ((a . b) . rest) -> ((a . b) . a)
                     (set-car! (car s) ex-cdr)   ;; -> ((rest . b) . a)
                     (swap! (car s))             ;; -> ((b . rest) . a)
                     (swap! s)                   ;; -> (a . (b . rest))
                     s
                 )
               )

            )
          )
  )
               
  (list A089852 A089852 89852 * "Swap car and cddr of an S-exp if its length > 1, i.e. transform (a . (b . c)) to (c . (b . a)) if possible"
         '(e i P L)   '????? -
         '(SoS: 4)
         '(A089840index: (89852 5))
         '(CC: 73191) '(FC: 73190) '(MLC: 46698)
         '(COMPS: (89852 (69770 89858) (89861 69770) (57163 89856 57163))
          )
;;       '(FIXES: "general plane trees whose root degree is either 1, or the ...")
         '(SCHEME:
            ("Destructive variant"
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

            )
          )
  )


  (list A089854 A089854 89854 * "Transform ((a . b) . c) --> ((b . a) . c) if possible"
         '(e i P L)   '????? -
         '(SoS: 4)
         '(A089840index: (89854 7))
         '(CC: 73191) '(FC: 73190) '(MLC: 46698)
         '(COMPS: (89854 (89859 69770) (69770 89863) (57163 89850 57163))
          )

;;       '(FIXES: "general plane trees whose root degree is either 1, or the ...")
         '(SCHEME:
            ("Destructive variant"
               (define (*A089854! s) ;; ((a . b) . c) --> ((b . a) . c)
                 (cond ((not (pair? s)) s)
                       ((not (pair? (car s))) s)
                       (else (swap! (car s)) s)
                 )
               )

               (define (swap! s)
                 (let ((ex-car (car s)))
                    (set-car! s (cdr s))
                    (set-cdr! s ex-car)
                    s
                 )
               )

            )
          )
  )


  (list A089855 A089857 89855 89857 "Rotate caar, cdar and cdr of an S-exp if possible"
         '(e i P L)   '????? -
         '(SoS: 4)
         '(A089840index: (89855 9) (89857 11))
         '(CC: 89847) '(FC: 89848)
;;       '(FIXES: "general plane trees whose root degree is either 1, or the ...")
         '(COMPS: (89855 (89860 69770) (69770 74680) (57163 89853 57163))
                  (89857 (74679 69770) (69770 89862) (57163 89851 57163))
          )
         '(SCHEME:
            ("Destructive variants"

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

               
               (define (*A089857! s) ;; ((a . b) . c) --> ((c . a) . b)
                 (cond ((not (pair? s)) s)
                       ((not (pair? (car s))) s)
                       (else (swap! s)
                             (robl! s)
                             s
                       )
                 )
               )
               
               (define (swap! s)
                 (let ((ex-car (car s)))
                    (set-car! s (cdr s))
                    (set-cdr! s ex-car)
                    s
                 )
               )

               
               (define (robl! s)
                 (let ((ex-car (car s)))         ;; <- a
                     (set-car! s (cddr s))       ;; (a . (b . rest)) -> (rest . (b . rest))
                     (set-cdr! (cdr s) ex-car)   ;; -> (rest . (b . a))
                     (swap! (cdr s))             ;; -> (rest . (a . b))
                     (swap! s)                   ;; -> ((a . b) . rest)
                     s
                 )
               )
               

            )
          )
  )

  (list A089856 A089856 89856 * "Swap caar and cdr of an S-exp if possible."
         '(e i P L)   '????? -
         '(SoS: 4)
         '(A089840index: (89856 10))
         '(CC: 73191) '(FC: 73190) '(MLC: 46698)
         '(COMPS: (89856 (73269 69770) (69770 73270) (57163 89852 57163))
          )
;;       '(FIXES: "general plane trees whose root degree is either 1, or the ...")
         '(SCHEME:
            ("Destructive variant"
               
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
            )
          )
  )

  (list A089858 A089861 89858 89861 "Gatomorphism A089858/A089861"
         '(e i P L)   '????? -
         '(SoS: 4)
         '(A089840index: (89858 13) (89861 18))
         '(CC: 73193) '(FC: 19590) '(MC: 89422) '(LC: 89423)
;;       '(FIXES: "general plane trees whose root degree is either 1, or the ...")
         '(COMPS: (89858 (72797 69770) (69770 89852) (57163 73270 57163))
                  (89861 (89852 69770) (69770 72797) (57163 73269 57163))
          )
         '(SCHEME:
            ("Destructive variants"
               
               (define (*A089858! s)
                 (cond ((pair? s)
                          (cond ((pair? (cdr s)) (robl! s) (swap! (car s)))
                                (else (swap! s))
                          )
                       )
                 )
                 s
               )
               
               
               (define (*A089861! s)
                 (cond ((pair? s)
                          (cond ((pair? (car s)) (swap! (car s)) (robr! s))
                                (else (swap! s))
                          )
                       )
                 )
                 s
               )
               
               
               (define (swap! s)
                 (let ((ex-car (car s)))
                    (set-car! s (cdr s))
                    (set-cdr! s ex-car)
                    s
                 )
               )

               
               (define (robl! s)
                 (let ((ex-car (car s)))         ;; <- a
                     (set-car! s (cddr s))       ;; (a . (b . rest)) -> (rest . (b . rest))
                     (set-cdr! (cdr s) ex-car)   ;; -> (rest . (b . a))
                     (swap! (cdr s))             ;; -> (rest . (a . b))
                     (swap! s)                   ;; -> ((a . b) . rest)
                     s
                 )
               )
               
               (define (robr! s)
                 (let ((ex-cdr (cdr s)))         ;; <- rest
                     (set-cdr! s (caar s))       ;; ((a . b) . rest) -> ((a . b) . a)
                     (set-car! (car s) ex-cdr)   ;; -> ((rest . b) . a)
                     (swap! (car s))             ;; -> ((b . rest) . a)
                     (swap! s)                   ;; -> (a . (b . rest))
                     s
                 )
               )

            )
          )
  )
 
  (list A089859 A089863 89859 89863 "Gatomorphism A089859/A089863"
         '(e i P L)   '????? -
         '(SoS: 4)
         '(A089840index: (89859 15) (89863 21))
         '(CC: 89407) '(MLC: 40002)
;;       '(FIXES: "general plane trees whose root degree is either 1, or the ...")
         '(COMPS: (89859 (89854 69770) (69770 89850))
                  (89863 (89850 69770) (69770 89854))
          )
         '(SCHEME:
            ("Destructive variants"
               (define (*A089859! s)
                 (cond ((pair? s)
                          (cond ((pair? (cdr s)) (swap! (cdr s)) (swap! s))
                                (else (swap! s))
                          )
                       )
                 )
                 s
               )

               
               (define (*A089863! s)
                 (cond ((pair? s)
                          (cond ((pair? (car s)) (swap! (car s)) (swap! s))
                                (else (swap! s))
                          )
                       )
                 )
                 s
               )

               (define (swap! s)
                 (let ((ex-car (car s)))
                    (set-car! s (cdr s))
                    (set-cdr! s ex-car)
                    s
                 )
               )
               
            )
          )
  )

  (list A089860 A089862 89860 89862 "Gatomorphism A089859/A089862"
        '(a d e n i P L) - -
        '(SoS: 4 5)
        '(A089840index: (89860 16) (89862 20))
        '(CC: 01683) '(FC: LEFT 019590) '(MLC: 89410)
        '(COMPS: (89860 (89855 69770) (69770 89851) (69770 74680 69770) (57163 89862 57163))
                 (89862 (89853 69770) (69770 89857) (69770 74679 69770) (57163 89860 57163))
         )
         '(FIXES: "nothing after size n &gt; 1")
         '(NOTES: "The cycle-count sequence seems to be the same as for polygon triangulations (see above), except shifted right by one. [I'm working on this.]"
          )
         '(SCHEME:
            ("Destructive variants using primitives swap! and robl!/robr!"
               
               (define (*A089860! s)
                 (cond ((pair? s)
                          (cond ((pair? (cdr s)) (swap! (cdr s)) (robl! s) (swap! (car s)))
                                (else (swap! s))
                          )
                       )
                 )
                 s
               )
               
               (define (*A089862! s)
                 (cond ((pair? s)
                          (cond ((pair? (car s)) (swap! (car s)) (robr! s) (swap! (cdr s)))
                                (else (swap! s))
                          )
                       )
                 )
                 s
               )
               
               (define (swap! s)
                 (let ((ex-car (car s)))
                    (set-car! s (cdr s))
                    (set-cdr! s ex-car)
                    s
                 )
               )

               
               (define (robl! s)
                 (let ((ex-car (car s)))         ;; <- a
                     (set-car! s (cddr s))       ;; (a . (b . rest)) -> (rest . (b . rest))
                     (set-cdr! (cdr s) ex-car)   ;; -> (rest . (b . a))
                     (swap! (cdr s))             ;; -> (rest . (a . b))
                     (swap! s)                   ;; -> ((a . b) . rest)
                     s
                 )
               )
               
               (define (robr! s)
                 (let ((ex-cdr (cdr s)))         ;; <- rest
                     (set-cdr! s (caar s))       ;; ((a . b) . rest) -> ((a . b) . a)
                     (set-car! (car s) ex-cdr)   ;; -> ((rest . b) . a)
                     (swap! (car s))             ;; -> ((b . rest) . a)
                     (swap! s)                   ;; -> (a . (b . rest))
                     s
                 )
               )

            )
          )
  )

 
  (list A089864 A089864 89864 89864 "Gatomorphism A089864 (Square of A089859/A089863)"
         '(e i P L)   '????? -
         '(SoS: 4)
         '(A089840index: (89864 1654694))
         '(CC: 89402) '(FC: 89408) '(MLC: 46698)
;;       '(FIXES: "general plane trees whose root degree is either 1, or the ...")
         '(COMPS: (89864 (89859 89859) (89863 89863))
          )
         '(SCHEME:
            ("Destructive variants"
               (define (*A089864! s)
                 (cond ((pair? s)
                          (if (pair? (car s)) (swap! (car s)))
                          (if (pair? (cdr s)) (swap! (cdr s)))
                       )
                 )
                 s
               )

               (define (swap! s)
                 (let ((ex-car (car s)))
                    (set-car! s (cdr s))
                    (set-cdr! s ex-car)
                    s
                 )
               )
               
            )
          )
  )

  (list A082351 A082352 82351 82352 #f '(a d e n i P L) - -
       '(SoS: 4)
       '(CF: 74679 74680 82355 82356 82361 82362)
       '(A089840index: (82351 4069) (82352 4253))
       '(COMPS: (82351 (57163 82353 57163)) (82352 (57163 82354 57163)))
       '(HORCONTR: (82351 82361) (82352 82362))
       '(SCHEME:
          ("Destructive variants"

             (define (*A082351! s)
               (cond ((not (pair? s)) s)
                     ((not (pair? (car s))) s)
                     ((not (pair? (cdr s))) (robl! (swap! s)))
                     (else (robl! s))
               )
             )
             
             (define (*A082352! s)
               (cond ((not (pair? s)) s)
                     ((not (pair? (car s))) s)
                     ((not (pair? (caar s))) (swap! (robr! s)))
                     (else (robr! s))
               )
             )
          )
        )
  )


  (list A082353 A082354 82353 82354 #f '(a d e n i P L) - -
       '(SoS: 4)
       '(A089840index: (82353 3886) (82354 3702))
       '(COMPS: (82353 (57163 82351 57163)) (82354 (57163 82352 57163)))
       '(SCHEME:
          ("Destructive variants"
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
          )
        )
  )

 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gat-list
 (list
  (list A069770 A069770 69770 * "Swap binary tree sides"
         '(d a P L) - -
         '(A089840index: (69770 1))
         '(SigatB: (69770 0))
         '(SoS: 3)
         '(CC: 07595) '(FC: AERATED 000108) '(MLC: LEFT 46698)
         '(FIXES: "binary trees whose both sides are identical")
         '(CYCLES: "rooted planar binary trees upto left-right swap"
                   "necklaces of n+1 white beads and n-1 black beads. [Correspondence with above requires Raney's lemma.]"
          )
         '(SCHEME:
            ("Constructive variant"
               (define (*A069770 s)
                  (cond ((null? s) s)
                        (else (cons (cdr s) (car s)))
                  )
               )
            )
            ("Destructive variant"
               (define (*A069770! s)
                  (if (pair? s) (swap! s))
                  s
               )
             )
          )
  )

               
  (list A057163 A057163 57163 * "Reflect binary trees and polygon triangulations"
         '(d a P L) - -
         '(SigatB: (57163 2))
         '(SoS: 3)
         '(CC: 07595) '(FC: AERATED 000108) '(MLC: LEFT 46698)
         '(FIXES: "binary trees whose left and right side are mirror images of each other")
         '(CYCLES: "rooted planar binary trees upto reflection"
                   "necklaces of n+1 white beads and n-1 black beads. [Correspondence with above requires Raney's lemma.]"
          )
         '(RCOMPS: (57163 (FORK 69770)))

         '(SCHEME:
            ("Constructive variant"
               (define (*A057163 s)
                  (cond ((null? s) s)
                        (else (cons (*A057163 (cdr s)) (*A057163 (car s))))
                  )
               )
            )
            ("Destructive variant"
               (define (*A057163! s)
                  (cond ((pair? s)
                              (swap! s)
                              (*A057163! (car s))
                              (*A057163! (cdr s))
                        )
                  )
                  s
               )
            )
          )
  )

  (list A072796 A072796 72796 * "Exchange the two leftmost branches of general trees if the degree of root larger than 1, otherwise keep the tree intact"
         '(e i P L)   '????? -
         '(A089840index: (72796 2))
         '(SigatB: (72796 1))
         '(SoS: 4)
         '(CC: 73191) '(FC: 73190) '(MLC: 46698) ;; No need to shift A046698, as begins as 1,1,1,2,2,2,2,2,...
         '(FIXES: "general plane trees whose root degree is either 1, or the two leftmost branches of the root are identical")
         '(SCHEME:
            ("Constructive variant"
               (define (*A072796 s)
                  (cond ((null? s) s)
                        ((not (pair? (cdr s))) s)
                        (else (cons (cadr s) (cons (car s) (cddr s))))
                  )
               )
            )
            ("Destructive variant"
               (define (*A072796! s)
                  (cond ((null? s) s)
                        ((not (pair? (cdr s))) s)
                        (else (swap! s) (robr! s) (swap! (cdr s)) s)
                  )
               )
            )
          )
  )

  (list A057509 A057510 57509 57510 "Shallow Rotate general trees and parenthesizations"
         '(e i P L)   69770 -
         '(SigatB: (57509 16) (57510 18))
         '(SoS: 4)
         '(CC: 03239) '(FC: 34731) '(MC: RIGHT 28310) '(LC: RIGHT 03418) ;; No need to shift A003239 or A034731.
         '(COMPS: (57509 (57501 69770)) (57510 (69770 57502)))
         '(RCOMPS: (57509 (SPINE 72796)) (57510 (ENIPS 72796)))
         '(FIXES: "general trees where all sub-branches of the root are identical")
         '(CYCLES: "rooted planar trees with n non-root nodes (???)"
                   "necklaces with n black beads and n white beads"
          )
         '(SCHEME:
            ("Constructive variant for rotate left using Lisp/Scheme built-in function append"
               (define (*A057509 s)
                  (cond ((null? s) s)
                        (else (append (cdr s) (list (car s))))
                  )
               )
            )

            ("Constructive, recursive variant for rotate left"
               (define (*A057509v2 s)
                  (cond ((null? s) s)
                        ((null? (cdr s)) s)
                        (else (cons (car (cdr s))
                                    (*A057509v2 (cons (car s) (cdr (cdr s))))
                              )
                        )
                  )
               )
            )
            ("Destructive variants, for both rotate left and right, composed of swap! and handshake rotates"
               (define (*A057509! s)
                  (cond ((pair? s) (swap! s) (*A057501! s)))
                  s
               )

               (define (*A057510! s)
                  (cond ((pair? s) (*A057502! s) (swap! s)))
                  s
               )
            )
          )

  )

  (list A057508 A057508 57508 * "Shallow Reverse general trees and parenthesizations"
         '(e n i P L) 69770 -
         '(SigatB: (57508 168))
         '(SoS: 4)
         '(CC: 73193) '(FC: 73192)
         '(RCOMPS: (57508 (ENIPS 57509) (SPINE 57510)))
         '(FIXES: "general plane trees whose n-th subtree from the left is equal with the n-th subtree from the right, for all its subtrees (i.e. are palindromic in the shallow sense)")
         '(CYCLES: "rooted planar general trees upto reversal of the order of subtrees"
          )
         '(SCHEME:
            ("Equivalent to Lisp/Scheme built-in function reverse"
               (define *A057508 reverse)
            )
            ("Constructive, recursive variant using Lisp/Scheme built-in function append"
               (define (*A057508v2 s)
                  (cond ((null? s) (list))
                        (else (append (*A057508v2 (cdr s)) (list (car s))))
                  )
               )
            )
            ("Constructive, deeply recursive variant. [See <A HREF=\"http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=A033538\">A033538</A>]"
               (define (*A057508v3 s)
                  (cond ((null? s) s)
                        ((null? (cdr s)) s)
                        (else (cons (car (*A057508v3 (cdr s)))
                                    (*A057508v3 (cons (car s)
                                                   (*A057508v3 (cdr (*A057508v3 (cdr s))))
                                             )
                                    )
                              )
                        )
                  )
               )
            )
            ("Constructive, tail-recursive variant"
               (define (*A057508v4 a)
                 (let loop ((a a) (b (list)))
                    (cond ((not (pair? a)) b)
                          (else (loop (cdr a) (cons (car a) b)))
                    )
                 )
               )
            )
            ("Two destructive variants"
               (define (*A057508! s)
                  (cond ((pair? s) (*A057508! (cdr s)) (*A057509! s)))
                  s
               )

               (define (*A057508v2! s)
                  (cond ((pair? s) (*A057510! s) (*A057508v2! (cdr s))))
                  s
               )

            )
          )
  )


  (list A057164 A057164 57164 * "Deep Reverse general trees and parenthesizations"
         '(e n i P L) 57163 -
         '(SigatB: (57164 164))
         '(SoS: 4)
         '(CC: LEFT 07123) '(FC: 01405) '(MLC: 46698)
         '(RCOMPS: (57164 (FORK 57510) (KROF 57509) (DEEP 57508)))
         '(FIXES: "general trees, parenthesizations and Dyck paths which are symmetric")
         '(CYCLES: "rooted planar general trees upto reflection"
                   "bracelets with n black beads and n-1 white beads [Correspondence with above requires Raney's lemma.]"
          )
         '(SCHEME:
            ("Constructive, recursive variant using Lisp/Scheme built-in function append"
               (define (*A057164 s)
                  (cond ((null? s) s)
                        ((null? (cdr s)) (cons (*A057164 (car s)) (list)))
                        (else (append (*A057164 (cdr s))
                                      (*A057164 (cons (car s) (list))))
                        )
                  )
               )
            )
            ("Constructive, deeply recursive variant"
               (define (*A057164v2 s)
                  (cond ((null? s) s)
                        ((null? (cdr s)) (list (*A057164v2 (car s))))
                        (else
                          (cons (*A057164v2 (car (*A057164v2 (cdr s))))
                                (*A057164v2 (cons (car s)
                                                    (*A057164v2
                                                      (cdr (*A057164v2 (cdr s)))
                                                    )
                                              )
                                )
                          )
                        )
                  )
               )
            )
            ("Destructive variant"
               (define (*A057164! s)
                  (cond ((pair? s)
                           (*A057164! (car s))
                           (*A057164! (cdr s))
                           (*A057509! s)
                        )
                  )
                  s
               )
            )
          )
  )


  (list A057511 A057512 57511 57512 "Deep Rotate general trees and parenthesizations"
         '(e i P L)   57163 -
         '(SigatB: (57511 12) (57512 14))
         '(SoS: 4)
         '(CC: 57513) '(FC: 57546) '(MC: RIGHT 00793) '(LC: RIGHT 03418)
         '(RCOMPS: (57511 (FORK 72796) (DEEP 57509)) (57512 (KROF 72796) (DEEP 57510)))
         '(FIXES: "[I'm working on this]")
         '(SCHEME:
            ("Constructive, recursive variant for deep rotate left"
               (define (*A057511 s)
                  (cond ((null? s) s)
                        ((null? (cdr s)) (list (*A057511 (car s))))
                        (else (cons (*A057511 (car (cdr s)))
                                    (*A057511 (cons (car s) (cdr (cdr s))))
                              )
                        )
                  )
               )
            )
            ("Destructive, recursive variants for both, using exch2first (A072796)"

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
            )
          )
  )


  (list A069767 A069768 69767 69768 "Swap recursively the other side of binary tree"
         '(d a P L) - -
         '(SigatB: (69767 6) (69768 8))
         '(SoS: 4)
         '(CC: 73431) '(FC: 36987) '(MLC: 11782) ;; No need to shift anything.
         '(RCOMPS: (69767 (SPINE 69770)) (69768 (ENIPS 69770)))
         '(FIXES: "complete binary trees [with leaves all on the same level]")
         '(SCHEME:
            ("Destructive variants"
               (define (*A069767! s)
                  (cond ((pair? s)
                              (swap! s)
                              (*A069767! (cdr s))
                        )
                  )
                  s
               )

               (define (*A069768! s)
                  (cond ((pair? s)
                              (*A069768! (cdr s))
                              (swap! s)
                        )
                  )
                  s
               )
            )
          )
         '(NOTES:
"In each forest of Cat[n] binary trees of n internal (branching nodes),\
there is a subset of 2<SUP>n-1</SUP> binary trees whose height\
(i.e. max depth) is equal to their size.\
This gatomorphism keeps that subset closed, and furthermore,\
it acts transitively on it, i.e. those trees form a single\
cycle of their own, as can be seen below.\
If we let the root node stand for the least significant bit,\
and the next-to-top node on those trees stand the most\
significant bit, and mark <B>0</B> when the next node upwards\
is at the right, and <B>1</B> when it is at left, we get\
the sequence of binary words (in this case, of three bits)\
shown below on the top of the eight binary trees belonging\
to that closed cycle.\
<BR>It is easy to see that this gatomorphism induces\
the well-known binary wrap-around (\"odometer\") increment/decrement algorithm\
on the binary strings that are in bijective correspondence\
with such binary trees."

          )
  )

  (list A057161 A057162 57161 57162 "Rotate polygon triangulations" '(d a P L) - -
         '(SigatB: (57161 17517) (57162 35013))
         '(SoS: 4)
         '(CC: LEFT LEFT 01683) '(FC: LEFT 019590) '(MLC: 57544) ;; A001683  1,1,1,1,4,6,19,... has offset = 2 in OEIS!
         '(COMPS: (57161 (57508 69767) (69767 69769) (57163 57162 57163))
                  (57162 (69768 57508) (69769 69768) (57163 57161 57163))
          )
         '(FIXES: "nothing after size n &gt; 1")
         '(CYCLES: "one-sided triangulations of the polygon (upto rotations), i.e. flexagons of order n"
                   "unlabeled plane boron trees with n leaves, i.e. rootless plane trivalent trees"
          )
         '(SCHEME:
            ("Constructive, tail-recursive variants"
               (define (*A057161 a)
                  (let loop ((a a) (b (list)))
                    (cond ((not (pair? a)) b)
                          (else (loop (car a) (cons (cdr a) b)))
                    )
                  )
               )

               (define (*A057162 a)
                  (let loop ((a a) (b (list)))
                    (cond ((not (pair? a)) b)
                          (else (loop (cdr a) (cons b (car a))))
                    )
                  )
               )
            )

            ("Constructive, recursive variant for A057161 using Lisp/Scheme built-in function append"
               (define (*A057161v2 s)
                  (cond ((null? s) s)
                        (else (append (*A057161v2 (car s)) (list (cdr s))))
                  )
               )
            )

            ("Destructive variants, composed of other destructively implemented gatomorphisms"

               (define (*A057161! s)
                  (*A069769! s)
                  (*A069767! s)
                  s
               )

               (define (*A057162! s)
                  (*A057508! s)
                  (*A069768! s)
                  s
               )
            )
          )
  )

;; (index-for-composed-sgtb 2  (index-for-composed-sgtb 1  (index-for-composed-sgtb 2  (index-for-composed-sgtb 0 1))))
;;Value: 557243
;; (index-for-composed-sgtb 2  (index-for-composed-sgtb 1  (index-for-composed-sgtb 0  (index-for-composed-sgtb 2 1))))
;;Value: 549755846843

  (list A074679 A074680 74679 74680 "Rotate binary tree, if possible, otherwise swap its sides"
        '(a d e n i P L) - -
        '(SigatB: (74679 557243 549755846843) (74680 2155872911 2156396687))
        '(SoS: 4 5)
        '(CC: LEFT 01683) '(FC: LEFT 019590) '(MLC: 89410)
        '(COMPS: (74679 (57163 72796 69770 57163 72796) (69770 89862 69770))
                 (74680 (72796 57163 69770 72796 57163) (69770 89860 69770))
         )
         '(FIXES: "nothing after size n &gt; 1")
         '(NOTES: "The cycle-count sequence seems to be the same as for polygon triangulations (see above), except shifted right by one. [I'm working on this.]"
          )
         '(SCHEME:
            ("Destructive variants using primitives swap! and robl!/robr!"
               (define (*A074679! s)
                  (cond ((pair? s)
                           (cond ((pair? (cdr s)) (robl! s))
                                 (else (swap! s))
                           )
                        )
                  )
                  s
               )

               (define (*A074680! s)
                  (cond ((pair? s)
                           (cond ((pair? (car s)) (robr! s))
                                 (else (swap! s))
                           )
                        )
                  )
                  s
               )
            )
          )
  )


  (list A057501 A057502 57501 57502 "Rotate non-crossing chords (handshake) arrangements; Rotate root position of the general trees"
         '(e n i P L) - -
         '(SigatB: (57501 261) (57502 521))
         '(SoS: 4)
         '(CC: LEFT 02995) '(FC: LEFT 019590) '(MC: 57543)
         '(RCOMPS: (57501 (SPINE 74680)) (57502 (ENIPS 74679)))
         '(COMPS: (57501 (57509 69770)) (57502 (69770 57510) (69888 82313)))
         '(FIXES: "nothing after size n &gt; 1")
         '(CYCLES: "non-crossing handshakes upto rotations"
                   "non-rooted planar trees"
          )
         '(SCHEME:
            ("Constructive variant for A057501 using Lisp/Scheme built-in function append"
               (define (*A057501 s)
                  (cond ((null? s) s)
                        (else (append (car s) (list (cdr s))))
                  )
               )
            )
            ("Destructive variants using primitives swap! and robl!/robr!"
               (define (*A057501! s)
                  (cond ((null? s))
                        ((not (pair? (car s))) (swap! s))
                        (else (robr! s) (*A057501! (cdr s)))
                  )
                  s
               )

               (define (*A057502! s)
                  (cond ((null? s))
                        ((not (pair? (cdr s))) (swap! s))
                        (else (*A057502! (cdr s)) (robl! s))
                  )
                  s
               )
            )
            ("Destructive variants using gatomorphisms A074680 and A074679"
               (define (*A057501v2! s)
                 (cond ((pair? s) (*A074680! s) (*A057501v2! (cdr s))))
                 s
               )
               
               (define (*A057502v2! s)
                 (cond ((pair? s) (*A057502v2! (cdr s)) (*A074679! s)))
                 s
               )
            )
          )
  )


  (list A057505 A057506 57505 57506 "Donaghey's M" '(a d e n i P L) - -
         '(SigatB: (57505 2614) (57506 5212))
         '(SoS: 4)
         '(CC: 57507) '(FC: LEFT 019590) '(MC: 57545) '(LC: 60114)
         '(COMPS: (57505 (57164 57163)) (57506 (57163 57164)))
         '(RCOMPS: (57505 (KROF 57501)) (57506 (FORK 57502)))
         '(FIXES: "nothing after size n &gt; 1")
         '(NOTES: "See: R. Donaghey, <B>Automorphisms on Catalan trees and bracketing, J. Combin. Theory, Series B, 29 (1980), 75-90.</B>")
         '(SCHEME:
            ("Constructive, tail-recursive variants"
               (define (*A057505 a)
                  (let loop ((a a) (b (list)))
                    (cond ((not (pair? a)) b)
                          (else (loop (car a) (cons (*A057505 (cdr a)) b)))
                    )
                  )
               )

               (define (*A057506 a)
                  (let loop ((a a) (b (list)))
                    (cond ((not (pair? a)) b)
                          (else (loop (cdr a) (cons b (*A057506 (car a)))))
                    )
                  )
               )
            )
            ("Constructive, recursive variant for *A057505 using Lisp/Scheme built-in function append"
               (define (*A057505v2 s)
                  (cond ((null? s) s)
                        (else (append (*A057505v2 (car s))
                                      (list (*A057505v2 (cdr s)))
                              )
                        )
                  )
               )
            )

            ("The variant based on the transformation explained in Donaghey's paper"
               (define (*A057505v3 s)
                  (with-input-from-string (list->string-strange s) read)
               )

               (define (list->string-strange s)
                 (string-append
                   "("
                   (with-output-to-string
                     (lambda ()
                       (let recurse ((s s))
                         (cond ((pair? s)
                                  (recurse (car s))
                                  (write-string "(")
                                  (recurse (cdr s))
                                  (write-string ")")
                               )
                         )
                       )
                     )
                   )
                   ")"
                 )
               )
            )

            ("Destructive variants, built on handshake rotates"

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
            )
          )
  )

  (list A057503 A057504 57503 57504 "Gatomorphism A057503/A057504"
         '(a d e n i P L) - -
         '(SigatB: (57503 2618) (57504 5216))
         '(SoS: 4)
         '(CC: LEFT LEFT 01683) '(FC: LEFT 019590) '(MLC: 57544)
         '(RCOMPS: (57503 (ENIPS 57501)) (57504 (SPINE 57502)))
         '(NOTES: "The count sequences seem to be the same as for polygon triangulations. [I'm working on this.]"
          )
         '(SCHEME:
            ("Constructive, recursive variant for A057503 using Lisp/Scheme built-in function append"
               (define (*A057503 s)
                  (cond ((null? s) s)
                        (else (append (car s) (list (*A057503 (cdr s)))))
                  )
               )
            )

            ("Destructive variants, built on handshake rotates"
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
            )
          )
  )

  (list A071661 A071662 71661 71662 "Donaghey's M<SUP>2</SUP>" '(a d e n i P L) - -
         '(SigatB: (71661 13373289) (71662 53490677))
         '(SKIP-OUTPUT: YES)
         '(SoS: 4)
         '(COMPS: (71661 (57505 57505)) (71662 (57506 57506)))
  )
  (list A071663 A071664 71663 71664 "Donaghey's M<SUP>3</SUP>" '(a d e n i P L) - -
         '(SigatB: (71663 "yes") (71664 "yes"))
         '(SKIP-OUTPUT: YES)
         '(SoS: 4)
         '(COMPS: (71663 (57505 71661)) (71664 (57506 71662)))
  )
  (list A071665 A071666 71665 71666 "Donaghey's M<SUP>4</SUP>" '(a d e n i P L) - -
         '(SigatB: (71665 "yes") (71666 "yes"))
         '(SKIP-OUTPUT: YES)
         '(SoS: 4)
         '(COMPS: (71665 (71661 71661)) (71666 (71662 71662)))
  )
  (list A071667 A071668 71667 71668 "Donaghey's M<SUP>5</SUP>" '(a d e n i P L) - -
         '(SigatB: (71667 "yes") (71667 "yes"))
         '(SKIP-OUTPUT: YES)
         '(SoS: 4)
         '(COMPS: (71667 (57505 71665)) (71668 (57506 71666)))
  )
  (list A071669 A071670 71669 71670 "Donaghey's M<SUP>6</SUP>" '(a d e n i P L) - -
         '(SigatB: (71669 "yes") (71669 "yes"))
         '(SKIP-OUTPUT: YES)
         '(SoS: 4)
         '(COMPS: (71669 (71663 71663)) (71670 (71664 71664)))
  )

  (list A073286 A073287 73286 73287 "Swap recursively the other side of binary tree, but excluding the root node"
         '(d a P L) - -
         '(SKIP-OUTPUT: YES)
         '(SigatB: (73286 41) (73287 69))
         '(SoS: 4)
         '(FC: 73268) '(MLC: 11782)
         '(COMPS: (73286 (69770 69767)) (73287 (69768 69770)))
  )
  (list A073288 A073289 73288 73289 "Gatomorphism A073288/A073289"
         '(d a P L) - -
         '(SKIP-OUTPUT: YES)
         '(SigatB: (73288 416) (73289 696))
         '(SoS: 4)
         '(FC: 23359) '(MLC: 11782)
  )

  (list A071655 A071656 71655 71656 "Rotate binary tree if possible and recurse down both branches, otherwise apply swap and terminate"
         '(a d e n i P L) - -
         '(SigatB: (71655 "???") (71656 "???"))
         '(SoS: 4)
         '(SCHEME:
            ("Destructive variants"
               (define (*A071655! s)
                 (cond ((null? s))
                       ((not (pair? (car s))) (swap! s))
                       (else
                             (robr! s)
                             (*A071655! (car s))
                             (*A071655! (cdr s))
                       )
                 )
                 s
               )
               
               (define (*A071656! s)
                 (cond ((null? s))
                       ((not (pair? (cdr s))) (swap! s))
                       (else
                             (*A071656! (car s))
                             (*A071656! (cdr s))
                             (robl! s)
                       )
                 )
                 s
               )
            )
          )
  )

  (list A071659 A071660 71659 71660 "Gatomorphism A071659: If robr not possible, apply swap, otherwise recurse down on both branches and after that rotate binary tree right"
         '(a d e n i P L) - -
         '(SKIP-OUTPUT: YES)
         '(SigatB: (71655 "???") (71656 "???"))
         '(SoS: 4)
         '(SCHEME:
            ("Destructive variants"

               (define (*A071659! s)
                 (cond ((null? s))
                       ((not (pair? (car s))) (swap! s))
                       (else
                             (*A071659! (car s))
                             (*A071659! (cdr s))
                             (robr! s)
                       )
                 )
                 s
               )
               
               (define (*A071660! s)
                 (cond ((null? s))
                       ((not (pair? (cdr s))) (swap! s))
                       (else
                             (robl! s)
                             (*A071660! (car s))
                             (*A071660! (cdr s))
                       )
                 )
                 s
               )

            )
          )
  )

  (list A074681 A074682 74681 74682 "Gatomorphism A074681/A074682"
         '(a d e n i P L) - -
         '(SigatB: (74681 5572432) (74682 "yes"))
         '(SoS: 4)
         '(NOTES: "The count sequence is not monotone"
          )
         '(SCHEME:
            ("Destructive variants"
               (define (*A074681! s)               
                 (cond ((pair? s) (*A074679! s) (*A074681! (car s)) (*A074681! (cdr s))))             s
               )
               
               (define (*A074682! s)
                 (cond ((pair? s) (*A074682! (car s)) (*A074682! (cdr s)) (*A074680! s)))
                 s
               )
            )
          )
  )

  (list A074683 A074684 74683 74684 "Gatomorphism A074683/A074684"
         '(a d e n i P L) - -
         '(SoS: 4)
         '(SigatB: (74683 5572434) (74684 "yes"))
         '(COMPS: (74684 (83925 85169 57548)))
         '(NOTES: "The count sequence is not monotone"
          )
         '(SCHEME:
            ("Destructive variants"
               (define (*A074683! s)
                 (cond ((pair? s) (*A074683! (car s)) (*A074683! (cdr s)) (*A074679! s)))
                 s
               )
               
               
               (define (*A074684! s)
                 (cond ((pair? s) (*A074680! s) (*A074684! (car s)) (*A074684! (cdr s))))
                 s
               )
            )
          )
  )

  (list A069773 A069774 69773 69774 "The car/cdr-flipped conjugate of handshake rotate"
         '(a d e n i P L) - -
         '(SoS: 4)
         '(SigatB: (69773 163899) (69774 164379))
         '(CC: LEFT 02995) '(FC: LEFT 019590) '(MC: 57543)
         '(FIXES: "nothing after size n &gt; 1")
         '(COMPS: (69773 (57163 57501 57163)) (69774 (57163 57502 57163)))
         '(SCHEME:
            ("Destructive variants using primitives swap! and robl!/robr!"
               (define (*A069773! s)
                 (cond ((null? s))
                       ((not (pair? (cdr s))) (swap! s))
                       (else
                             (robl! s)
                             (*A069773! (car s))
                       )
                 )
                 s
               )
               
               (define (*A069774! s)
                 (cond ((null? s))
                       ((not (pair? (car s))) (swap! s))
                       (else
                             (*A069774! (car s))
                             (robr! s)
                       )
                 )
                 s
               )
            )               
            ("Destructive variants using gatomorphisms A074680 and A074679"
               (define (*A069773v2! s)
                 (cond ((pair? s) (*A074679! s) (*A069773v2! (car s))))
                 s
               )
               
               (define (*A069774v2! s)
                 (cond ((pair? s) (*A069774v2! (car s)) (*A074680! s)))
                 s
               )
            )
          )
  )
  (list A069787 A069787 69787 * "The car/cdr-flipped conjugate of deep reverse"
         '(a d e n i P L) - 72799
         '(SigatB: (69787 538968755))
         '(SoS: 4)
         '(CC: LEFT 07123) '(FC: 01405) '(MLC: 46698)
         '(COMPS: (69787 (57163 57164 57163)))
  )

  (list A069769 A069769 69769 * "The car/cdr-flipped conjugate of shallow reverse"
         '(a d e n i P L) - -
         '(SigatB: (69769 538976435))
         '(SoS: 4)
         '(CC: 73193) '(FC: 73192)
         '(COMPS: (69769 (57163 57508 57163)))
  )


  (list A069888 A069888 69888 * "Reflect non-crossing chords by the axis through corners (WM)"
         '(a d e n i P L) - -
         '(SigatB: (69888 2155874567))
         '(SoS: 4)
         '(CC: 07595) '(FC: 00108)
         '(COMPS: (69888 (57501 57164)))
         '(SCHEME:
            ("Constructive variant, composed of gatomorphisms DeepReverse (*A057164) & RotateHandshakes (*A057501)"
               (define (*A069888 s) (*A057501 (*A057164 s)))
            )
            ("Destructive variant, composed of gatomorphisms DeepReverse (*A057164) & RotateHandshakes (*A057501)"
               (define (*A069888! s) ;; Was DeepReverse_et_RotateHandshakes!
                 (*A057164! s)
                 (*A057501! s)
                 s
               )
            )
          )    
  )

  (list A069889 A069889 69889 * "Reflect non-crossing chords by the axis through corners"
         '(a d e n i P L) - -
         '(SoS: 4)
         '(SKIP-OUTPUT: YES)
         '(SigatB: (69889 148535))
         '(CC: 07595) '(FC: 00108)
         '(COMPS: (69889 (57164 57501)))
         '(SCHEME:
            ("Constructive variant, composed of gatomorphisms RotateHandshakes (*A057501) & DeepReverse (*A057164)"

               (define (*A069889 s) (*A057164 (*A057501 s)))
            )               
            ("Destructive variant, composed of gatomorphisms RotateHandshakes (*A057501) & DeepReverse (*A057164)"
               
               (define (*A069889! s)
                 (*A057501! s)
                 (*A057164! s)
                 s
               )
            )
          )
  )

  (list A069771 A069771 69771 * "Rotate non-crossing chords by 180 degrees" '(a d e n i P L) - -
         '(SoS: 3 4)
         '(SigatB: (69771 "no?"))
         '(CC: LEFT 07123) '(FC: 01405) '(MLC: 46698)
         '(COMPS: (69771 (57164 69772)))
         '(SCHEME:
            ("Constructive variant, which calls RotateHandshakes (*A057501) as many times as there are left or right parentheses in the S-expression"
               (define (*A069771 s) (RotateHandshakes_n_steps s (count-pars s)))

               (define (RotateHandshakes_n_steps s n)
                 (cond ((zero? n) s)
                       (else (RotateHandshakes_n_steps (*A057501 s) (- n 1)))
                 )
               )

               (define (count-pars s)
                   (cond ((null? s) 0)
                         (else (+ 1 (count-pars (car s)) (count-pars (cdr s))))
                   )
               )
            )
          )
  )

  (list A069772 A069772 69772 * "Reflect non-crossing chords by X-axis" '(a d e n i P L) - -
         '(SoS: 3 4)
         '(SigatB: (69772 "no?"))
         '(COMPS: (69772 (57164 69771)))
         '(SCHEME:
            ("Constructive variant, composed of gatomorphisms RotateHandshakes 180 degrees (*A069771) & DeepReverse (*A057164)"
               (define (*A069772 s) (*A057164 (*A069771 s)))
            )
          )
  )

  (list A072088 A072089 72088 72089 "Meeussen's breadth-first &lt;-&gt; depth-first conversion for general trees"
         '(a d e n i P L) 57117 72619
         '(SoS: 3 4 5)
         '(SigatB: (72088 "no!?") (72089 "no!?"))
         '(SCHEME:
            ("Lazily evaluating variant for A072808"
               (define (*A072088 s) (cond ((null? s) s) (else (Lw-bf->p! (p->Lw s)))))


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

               (define (Lw-bf->p! L)
                 (letrec ((lazy-liz
                            (lambda (n)
                             (cond ((zero? n) (list))
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
               
               
               (define (pop-with-trailing-zeros! lista)
                 (let ((topmost (car lista)))
                   (cond ((pair? (cdr lista))
                               (set-car! lista (cadr lista))
                               (set-cdr! lista (cddr lista))
                         )
                         (else (set-car! lista 0))
                   )
                   topmost
                 )
               )

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
            )
            ("Continuation-passing variant for A072809"
               
               (define (*A072089 p)
                 (let ((conts (list car)))
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
                     )
                   )
                   ((car (last-pair conts)) (list))
                 )
               )


               (define (list-n-from-top n stack)
                 (cons (list-head stack n) (nthcdr n stack))
               )

               (define (nthcdr n lista)
                 (if (or (zero? n) (null? lista))
                     lista
                     (nthcdr (- n 1) (cdr lista))
                 )
               )

            )
          )
  )

  (list A057117 A057118 57117 57118 "Meeussen's breadth-first &lt;-&gt; depth-first conversion for binary trees"
         '(a d e n i P L) - 38776
         '(SoS: 3 4 5)
         '(SigatB: (57117 "no!?") (57118 "no!?"))
         '(CC: 38775) '(MC: 57542) '(LC: 60113)
         '(SCHEME:
               
            ("Continuation-passing variant for A057118"
               
               (define (*A057118 bt)
                 (let ((conts (list car)))
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
                     )
                   )
                   ((car (last-pair conts)) (list))
                 )
               )


               (define (nthcdr n lista)
                 (if (or (zero? n) (null? lista))
                     lista
                     (nthcdr (- n 1) (cdr lista))
                 )
               )


               (define (cons2top! stack)
                 (let ((ex-cdr (cdr stack)))
                     (set-cdr! stack (car ex-cdr))
                     (set-car! ex-cdr stack)
                     ex-cdr
                 )
               )


            )
          )
  )
 )
)


;; Call as:
;; (html-gatomorf-list gat-list2 10 "./gatlist2.htm")
;; Or as:
;; (output-sequence-file gat-list2 2056 "Jun 19 2003" "c:\\matikka\\Nekomorphisms\\seqs\\A85159-.txt")

;; Note: At least gatomorphisms A085159/60, A085161, A085163/4, A085171/2, A085175
;; and A086429/30, A086431 seem to be Lukasiewicz-word permuting ones.
;; (it's clear that the last three are if the first three are.)
;;


(define gat-list2
 (list
  (list A089865 A089866 89865 89866  "Apply A074679/A074680 to the left subtree" '(d qq rr e n i P L) - -
       '(SoS: 4)
       '(A089840index: (89865 4207) (89866 4299))
;; The max-count seq is actually RIGHT(A089410). The fix counts = 1,1,Cat(n-1)+Cat(n), i.e. RIGHT(RIGHT(A005807))
       '(CC: 89844) '(FC: 5807) '(MC: 89410) '(LC: 89845)
       '(SCHEME:
          ("Destructive variants"

              (define (*A089865! s)
                (if (pair? s) (*A074679! (car s)))
                s
              )
              
              (define (*A089866! s)
                (if (pair? s) (*A074680! (car s)))
                s
              )
          )
        )
  )

  (list A089867 A089868 89867 89868  "Apply A085169/A085170 to the left subtree" '(d qq rr e n i P L) - -
       '(SoS: 4)
       '(CC: 89846) '(FC: 90826) '(MC: 86586) '(LC: 86587)
       '(SCHEME:
          ("Destructive variants"
              (define (*A089867! s)
                (if (pair? s) (*A085169! (car s)))
                s
              )
              
              (define (*A089868! s)
                (if (pair? s) (*A085170! (car s)))
                s
              )
          )
        )
  )
  (list A089869 A089870 89869 89870  "Apply A085169/A085170 to each top-level element" '(d qq rr e n i P L) - -
       '(SoS: 4)
       '(CC: 90827) '(FC: 000129) '(MC: 86586) '(LC: 86587)
       '(SCHEME:
          ("Destructive variants"
              (define (*A089869! s)
                (for-each *A085169! s)
                s
              )
              
              (define (*A089870! s)
                (for-each *A085170! s)
                s
              )
          )
        )
  )

  (list A085159 A085160 85159 85160  "Rotate rising slope variants of (pp)-(rr)" '(qq rr e n i P L) - -
       '(SigatB: (85159 "yes") (85160 "yes"))
       '(SoS: 4)
       '(FC: 46698) '(CC: 54357)
       '(CF: 85165 85166 85167 85168 86429 86430 85203)
       '(COMPS: (85159 (85161 85160 85161)
                       (85169 82315 85170)
                       (74684 82315 74683)
                       (85173 85173)
                )
                (85160 (85161 85159 85161)
                       (85169 82316 85170)
                       (74684 82316 74683)
                       (85174 85174)
                )
        )
       '(SCHEME:
          ("Constructive variants"

              (define (*A085159 s)
                (cond ((null? s) s)
                      (else (app-to-xrt (car s) (append (cdr s) (list (list)))))
                )
              )
              (define (*A085160 s)  (*A085161 (*A085159 (*A085161 s))))

          )
          ("Destructive variants"
              
              (define (*A085159! s)
                (cond ((null? s) s)
                      (else (app-to-xrt! (car s) (append! (cdr s) (list (list)))))
                )
              )
              (define (*A085160! s) (*A085161! (*A085159! (*A085161! s))))
          )
        )
  )

  (list A085161 A085161 85161 * "Reflect rising slope variants of (pp)-(rr)" '(qq rr e n i P L) - -
       '(SigatB: (85161 "yes"))
       '(SoS: 4)
       '(CC: LEFT 07123) '(FC: 01405) '(MLC: 46698)
       '(CF: 85159 85160 85162 85175 86431 85203)
       '(COMPS: (85161 (85163 57508)
                       (74684 57164 74683)
                )
        )
       '(SCHEME:
          ("Constructive variant"
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
          )

          ("Destructive variant"
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
          )
        )
  )

  (list A085162 A085162 85162 * "A057163-conjugate of A085161" '(qq rr e n i P L) - -
       '(SigatB: (85162 "yes"))
       '(SoS: 4)
       '(CC: LEFT 07123) '(FC: 01405) '(MLC: 46698)
       '(CF: 85165 85166 86431)
       '(COMPS: (85162 (57163 85161 57163)))
  )

  (list A085163 A085164 85163 85164 "Variants of A085161" '(qq rr e n i P L) - -
       '(SigatB: (85163 "yes") (85164 "yes"))
       '(SoS: 4)
       '(FC: 51920)
       '(CF: 85171 85172)
       '(COMPS: (85163 (85161 57508))
                (85164 (57508 85161))
        )
       '(SCHEME:
          ("Destructive variant"
              
              
              (define (*A085163! s)
                (cond ((null? s) s)
                      (else (app-to-xrt! (*A085161! (car s))
                                         (append! (map *A085161! (cdr s)) (list (list)))
                            )
                      )
                )
              )
          )
        )              
  )

  (list A085165 A085166 85165 85166  "A057163-conjugates of A085159 & A085160" '(qq rr e n i P L) - -
       '(SigatB: (85165 "yes") (85166 "yes"))
       '(SoS: 4)
       '(FC: 46698) '(CC: 54357)
       '(CF: 85162 86429 86430)
       '(COMPS: (85165 (85162 85166 85162)
                )
                (85166 (85162 85165 85162)
                )
        )
  )


  (list A085167 A085168 85167 85168 "Gatomorphisms A085167/A085168" '(qq rr e n i P L) - -
       '(SigatB: (85167 "yes") (85168 "yes"))
       '(SoS: 4)
       '(CC: 01683)
       '(CF: 74679 74680 85203)
       '(COMPS: (85167 (85159 69770))
                (85168 (69770 85160))
        )
       '(SCHEME:
          ("Destructive variant"

              (define (*A085167! s)
                (cond ((null? s) s)
                      (else (app-to-xrt! (cdr s) (append! (car s) (list (list)))))
                )
              )
          )
        )
  )

  (list A085169 A085170 85169 85170 "Gatomorphisms A085169/A085170" '(d qq rr e n i P L) - -
       '(SigatB: (85169 "???") (85170 "???"))
       '(SoS: 4)
       '(CC: 86585) '(FC: 00045) '(MC: 86586) '(LC: 86587)
       '(CF: 74683 74684 85159 85160 85175)
       '(HORCONTR: (85169 86433) (85170 86434))
       '(SCHEME:
          ("Constructive variant"
              
              (define (*A085169 s)
                (letrec ((evenlev (lambda (s)
                                    (cond ((null? s) s)
                                          (else (cons (oddlev (car s)) (evenlev (cdr s))))
                                    )
                                  )
                         )
                         (oddlev (lambda (s)
                                    (cond ((null? s) s)
                                          (else (append (evenlev (car s)) (list (oddlev (cdr s)))))
                                    )
                                  )
                         )
                        )
                   (evenlev s)
                )
              )
          )
          ("Destructive variant"

              (define (*A085169! s)
                 (cond ((pair? s)
                          (*A074684 (car s))
                          (*A085169! (cdr s))
                       )
                 )
                 s
              )

              (define (*A085169v2! s) (map *A074684! s))
              (define (*A085170! s) (map *A074683! s))
          )
        )
  )

  (list A085171 A085172 85171 85172 "Gatomorphisms A085171/A085172" '(qq rr e n i P L) - -
       '(SigatB: (85171 "???") (85172 "???"))
       '(SoS: 4)
       '(CF: 85163 85164 85203)
       '(SCHEME:
          ("Destructive variant"
              
              (define (*A085171! s)
                (cond ((null? s) s)
                      (else (app-to-xrt! (*A085171! (car s))
                                         (append! (map *A085171! (cdr s)) (list (list)))
                            )
                      )
                )
              )

          )
        )
  )

  (list A085173 A085174 85173 85174  "Half-step rotate rising slope variants of (pp)-(rr)" '(qq rr e n i P L) - -
       '(SigatB: (85173 "yes") (85174 "yes"))
       '(SoS: 4)
       '(CC: LEFT 02995) '(FC: LEFT 019590) '(MC: 57543)
       '(COMPS: (85173 (85161 85174 85161)
                       (85169 57501 85170)
                       (74684 57501 74683)
                )
                (85174 (85161 85173 85161)
                       (85169 57502 85170)
                       (74684 57502 74683)
                )
        )
       '(CF: 85159 85160 86427 86428)
  )


  (list A085175 A085175 85175 * "A085170-conjugate of A057164" '(qq rr e n i P L) - -
       '(SigatB: (85175 "???"))
       '(SoS: 4)
       '(CC: LEFT 07123) '(FC: 01405) '(MLC: 46698)
       '(CF: 85161)
       '(COMPS: (85175 (85169 57164 85170)))
  )

  (list A086425 A086426 86425 86426 "Gatomorphisms A086425/A086426" '(qq rr e n i P L) - -
       '(SigatB: (86425 "yes") (86426 "yes"))
       '(SoS: 4)
       '(CF: 86427 86428 86429 86430 86431)
       '(COMPS: (86425 (57164 74684)) (86426 (74683 57164)))
  )


  (list A086427 A086428 86427 86428 "Half-step rotate of interpretations (pp)-(rr)" '(qq rr e n i P L) - -
       '(SigatB: (86427 "yes") (86428 "yes"))
       '(SoS: 4 5)
       '(CC: LEFT 02995) '(FC: LEFT 019590) '(MC: 57543)
       '(COMPS: (86427 (86431 86428 86431)
                       (57164 85173 57164)
                       (86425 57501 86426)
                )
                (86428 (86431 86427 86431)
                       (57164 85174 57164)
                       (86425 57502 86426)
                )
        )
       '(CF: 86429 86430)
  )

  (list A086429 A086430 86429 86430 "Whole-step rotate of interpretations (pp)-(rr)" '(qq rr e n i P L) - -
       '(SigatB: (86429 "yes") (86430 "yes"))
       '(SoS: 4 5)
       '(FC: 46698) '(CC: 54357)
       '(COMPS: (86429 (86427 86427)
                       (86431 86430 86431)
                       (57164 85159 57164)
                       (86425 82315 86426)
                )
                (86430 (86428 86428)
                       (86431 86429 86431)
                       (57164 85160 57164)
                       (86425 82316 86426)
                )
        )
  )

  (list A086431 A086431 86431 * "Reflect interpretations (pp)-(rr)" '(qq rr e n i P L) - -
       '(SigatB: (86431 "yes"))
       '(SoS: 4 5)
       '(CC: LEFT 07123) '(FC: 01405) '(MLC: 46698)
       '(CF: 86427 86430)
       '(COMPS: (86431 (57164 85161 57164) (86425 57164 86426)))
  )

  (list A086433 A086434 86433 86434 "Contraction-permutations of A085169/A085170" '(a d e n i P L) - -
       '(COMPS: (86433 (82853 85169 81291))
                (86434 (82853 85170 81291))
        )
  )

  (list A082313 A082313 82313 * "Meeussen's skew catacycles"
       '(e n i d P L) - -
       '(COMPS: (82313 (57501 57164 57502) (69888 57502)))
       '(SigatB: (82313 604463486276865131809167))
       '(SoS: 4)
       '(CC: LEFT 07123) '(FC: 01405) '(MLC: 46698)
       '(CF: 82314 82315 82333 82334)
;;     '(FIXES: "???")
;;     '(CYCLES: "???")
  )
  (list A082314 A082314 82314 * #f
       '(e n i d P L) - -
       '(COMPS: (82314 (57502 57164 57501) (57502 69889)))
       '(SigatB: (82314 2361759710983228099211))
       '(SoS: 4)
       '(CC: LEFT 07123) '(FC: 01405) '(MLC: 46698)
       '(CF: 82313)
;;     '(FIXES: "???")
;;     '(CYCLES: "???")
  )
  (list A082315 A082316 82315 82316 "Rotate handshakes two steps" '(a d e n i P L) - -
       '(SigatB: (82315 34359740687) (82316 549755978251))
       '(SoS: 4)
       '(CC: 54357)
       '(COMPS: (82315 (57501 57501) (82313 57164)) (82316 (57502 57502) (57164 82313)))
       '(CF: 82317 82324)
  )
  (list A082317 A082318 82317 82318 "Rotate handshakes three steps" '(a d e n i P L) - -
       '(SigatB: (82317 11150372599265311570767859136464952601119119)
                 (82318 730750818665451459101842418728548123849399632523)
        )
       '(SoS: 4)
       '(COMPS: (82317 (57501 82315)) (82318 (57502 82316)))
       '(CF: 82319 82324)
  )
  (list A082319 A082320 82319 82320 "Rotate handshakes four steps" '(a d e n i P L) - -
       '(SigatB: (82319 "yes") (82320 "yes"))
       '(SoS: 4)
       '(COMPS: (82319 (82315 82315)) (82320 (82316 82316)))
       '(CF: 82321 82324)
  )
  (list A082321 A082322 82321 82322 "Rotate handshakes five steps" '(a d e n i P L) - -
       '(SigatB: (82321 "yes") (82322 "yes"))
       '(SoS: 4)
       '(COMPS: (82321 (82315 82317)) (82322 (82316 82318)))
       '(CF: 82323 82324)
  )

  (list A082323 A082324 82323 82324 "Rotate handshakes six steps" '(a d e n i P L) - -
       '(SigatB: (82323 "yes") (82324 "yes"))
       '(SoS: 4)
       '(COMPS: (82323 (57501 82321) (82317 82317) (82315 82315 82315))
                (82324 (57502 82322) (82318 82318) (82316 82316 82316))
        )
  )

  (list A082325 A082326 82325 82326 "A057163-conjugate of Deep Rotate" '(a d e n i P L) - -
       '(SigatB: (82325 1792 8883) (82326 1794 131219))
       '(SoS: 4)
       '(RCOMPS: (82325 FORK 72797) (82326 KROF 72797))
       '(COMPS: (82325 (57163 57511 57163) (69787 82326 69787))
                (82326 (57163 57512 57163) (69787 82325 69787))
        )
       '(CC: 57513) '(FC: 57546) '(MC: RIGHT 00793) '(LC: RIGHT 03418)
       '(HORCONTR: (82325 82327) (82326 82328))
       '(CF: 82337 82338)
  )

  (list A082327 A082328 82327 82328 #f '() - -
       '(SoS: )
       '(CF: 82329 82330)
       '(COMPS: (82327 (82853 82325 81291) (72798 82328 72798))
                (82328 (82853 82326 81291) (72798 82327 72798))
        )
  )

  (list A082329 A082330 82329 82330 #f '() - -
       '(SoS: )
       '(CF: 82327 82338)
       '(COMPS: (82329 (82854 82325 81291))
                (82330 (82854 82326 81291))
        )
  )

  (list A082331 A082332 82331 82332 #f '(a d e n i P L) - -
       '(SigatB: (82331 9223512774351650875) (82332 576469548397005071))
       '(SoS: 4)
       '(COMPS: (82331 (57163 69888)) (82332 (69888 57163)))
       '(CF: 82333 82334)
  )

  (list A082333 A082334 82333 82334 #f '(a d e n i P L) - -
       '(SigatB: (82333 730750818666116073099889616808727458479087976635)
                 (82334 "yes")
        )
       '(SoS: 4)
       '(COMPS: (82333 (57163 82313)) (82334 (82313 57163)))
       '(CF: 82331 82332)
  )

  (list A082335 A082336 82335 82336  "Rotate binary tree, if possible, otherwise reflect it" '(a d e n i P L) - -
       '(SigatB: (82335 "???") (82336 "???"))
       '(SoS: 4)
       '(FC: 19590)
       '(CF: 74679 74680 82349 82350)
       '(SCHEME:
          ("Destructive variants"

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

          )
        )
  )


;;        (list 100 0 82337 A082337 A082338 (list A057163 A057509 A057163))
;;        (list 100 0 82338 A082338 A082337 (list A057163 A057510 A057163))

;; Duplicates of A069775 and A069776:
;;(list A082337 A082338 82337 82338 "A057163-conjugate of shallow rotate" '(a d e n i P L) - -
;;     '(SigatB: (82337 131251) (82338 131731))
;;     '(SoS: 4)
;;     '(CC: 03239) '(FC: 34731) '(MC: RIGHT 28310) '(LC: RIGHT 03418)
;;     '(COMPS: (82337 (57163 57509 57163)) (82338 (57163 57510 57163)))
;;     '(CF: 82325 82326 82339 82341)
;;     '(SCHEME:
;;        ("Destructive variants"

;;           (define (*A082337! s)
;;             (cond ((pair? s)
;;                         (*A072797! s)
;;                         (*A082337! (car s))
;;                   )
;;             )
;;             s
;;           )
;;           
;;           
;;           (define (*A082338! s)
;;             (cond ((pair? s)
;;                         (*A082338! (car s))
;;                         (*A072797! s)
;;                   )
;;             )
;;             s
;;           )
;;           
;;        )
;;      )
;;)

;;     (list 100 0 82339 A082339 A082340)
;;     (list 100 0 82340 A082340 A082339)

  (list A082339 A082340 82339 82340 #f '(a d e n i P L) - -
       '(SoS: 4)
       '(SigatB: (82339 1796) (82340 1798))
       '(MLC: 16116) ;; Check! 1,1,2,2,4,4,8,8,16,16,...
       '(RCOMPS: (82339 SPINE 72797) (82340 ENIPS 72797))
       '(CF: 82337 82341)
       '(SCHEME:
          ("Destructive variants"
             
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
             
             
          )
        )
  )

;;     (list 100 0 82341 A082341 A082342 (list A057163 A073285 A057163)) ;; Differs from A082326 at term n=39.
;;     (list 100 0 82342 A082342 A082341 (list A057163 A073284 A057163)) ;; Differs from A082325 at term n=39.

  (list A082341 A082342 82341 82342 "A057163-conjugate of A073285/A073284" '(a d e n i P L) - -
       '(SigatB: (82341 1800) (82342 "yes"))
       '(SoS: 4)
       '(CC: 57513) '(FC: 57546) '(MC: RIGHT 00793) '(LC: RIGHT 03418) ;; Check!
       '(COMPS: (82341 (57163 73285 57163)) (82342 (57163 73284 57163)))
       '(CF: 82337 82339)
       '(SCHEME:
          ("Destructive variants"
             
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
          )
        )
  )



  (list A082345 A082346 82345 82346 #f '(a d e n i P L) - -
       '(SigatB: (82345 66) (82346 88))
       '(SoS: 4)
       '(CF: 73288 73289 82347 82348)
       '(SCHEME:
          ("Destructive variants"
             
             (define (*A082345! s)
                (cond ((pair? s)
                            (*A069767! s)
                            (*A082345! (cdr s))
                      )
                )
                s
             )

             (define (*A082346! s)
                (cond ((pair? s)
                            (*A082346! (cdr s))
                            (*A069768! s)
                      )
                )
                s
             )
             

          )
        )
  )

  (list A082347 A082348 82347 82348 #f '(a d e n i P L) - -
       '(SigatB: (82347 86) (82348 68))
       '(SoS: 4)
       '(CF: 73288 73289 82345 82346)
       '(SCHEME:
          ("Destructive variants"

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
          )
        )
  )


  (list A082349 A082350 82349 82350  "Rotate binary tree, if possible, otherwise apply gatomorphism *A069767!/*A069768!" '(a d e n i P L) - -
       '(SigatB: (82349 "???") (82350 "???"))
       '(SoS: 4)
       '(CF: 74679 74680 82335 82336)
       '(CC: 73193) ;; Check! Surprising...
       '(FC: 19590)
       '(SCHEME:
          ("Destructive variants"

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

          )
        )
  )

  (list A082355 A082356 82355 82356 #f '(a d e n i P L) - -
       '(SoS: 4)
       '(COMPS: (82355 (82357 57163)) (82356 (57163 82358)))
       '(HORCONTR: (82355 82363) (82356 82364))
       '(CF: 57117 57118 82351 82352)
       '(SCHEME:
          ("Destructive variants"
             
             (define (*A082355! s)
               (cond ((pair? s) (*A082355! (car s)) (*A082355! (cdr s)) (*A082351! s)))
               s
             )
             
             (define (*A082356! s)
               (cond ((pair? s) (*A082352! s) (*A082356! (car s)) (*A082356! (cdr s))))
               s
             )


          )
        )
  )

  (list A082357 A082358 82357 82358 #f '(a d e n i P L) - -
       '(SoS: 4)
       '(CF: 82359 82360)
       '(COMPS: (82357 (82355 57163)) (82358 (57163 82356)))
  )

  (list A082359 A082360 82359 82360 #f '(a d e n i P L) - -
       '(SoS: 4)
       '(SigatB: (82359 18764713496857) (82360 "yes"))
       '(CF: 82357 82358)
       '(COMPS: (82359 (74683 57163)) (82360 (57163 74684)))
  )

  (list A082361 A082362 82361 82362 #f '() - -
       '(SoS: )
       '(CF: 82363 82364)
       '(COMPS: (82361 (82853 82351 81291)) (82362 (82853 82352 81291)))
  )

  (list A082363 A082364 82363 82364 #f '() - -
       '(SoS: )
       '(CF: 82361 82362)
       '(COMPS: (82363 (82853 82355 81291)) (82364 (82853 82356 81291)))
  )


 )
)

