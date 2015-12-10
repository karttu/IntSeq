
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;;
;;                          gato-fps.scm                                 ;;
;;                                                                       ;;
;;  Gatomorphism output functions for FPS package.                       ;;
;;                                                                       ;;
;;  This Scheme-code is Copyright (C) 2002-2003 by Antti Karttunen       ;;
;;  (E-mail: my_firstname.my_surname@iki.fi) and is placed under         ;;
;;  the GPL (Gnu Public License), so you are free to copy it.            ;;
;;                                                                       ;;
;;  Note: this module runs only in scsh (Scheme Shell) and uses          ;;
;;  the FPS (functional PostScript) library by Wandy Sae-Tan and         ;;
;;  Olin Shivers, located at http://www.scsh.net/resources/fps.html or   ;;
;;  ftp://ftp.scsh.net/pub/scsh/contrib/fps/doc/fps.html                 ;;
;;                                                                       ;;
;;  The latest scsh (Scheme Shell) can be found at http://www.scsh.net/  ;;
;;                                                                       ;;
;;  The main pointer for this code collection is:                        ;;
;;  http://www.iki.fi/~kartturi/matikka/Nekomorphisms/gatomorf.htm       ;;
;;                                                                       ;;
;;  Last edited 20. December 2004.                                       ;;
;;                                                                       ;;
;;  Start as:                                                            ;;
;;  % scsh                                                               ;;
;;  Welcome to scsh 0.6.1 (Combinatorial Algorithms)                     ;;
;;  Type ,? for help.                                                    ;;
;;  > ,config ,load fps-package.scm                                      ;;
;;  fps-package.scm                                                      ;;
;;  > ,open fps                                                          ;;
;;  Load structure fps (y/n)? y                                          ;;
;;  > ,load gato-fps.scm                                                 ;;
;;  gato-fps.scm                                                         ;;
;;  gatocout.scm                                                         ;;
;;  >                                                                    ;;
;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following utility functions are from lstfuns1.scm module:
;; (load "../Schemuli/lstfuns1.scm") ;; 


(define attach! ; Borrowed from Franz lisp, is like destructive cons.
  (lambda (elem lista)
     (set-cdr! lista (cons (car lista) (cdr lista)))
     (set-car! lista elem)
     lista
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


(define (count-pars a)
    (cond ((not (pair? a)) 0)
          (else (+ 1 (count-pars (car a)) (count-pars (cdr a))))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the conversion routines from parenthesizations (sexp's)
;; to appropriate manifestations of the Catalan's family.

(load "gatocout.scm")

(define (1+ n) (+ 1 n))
(define (-1+ n) (- n 1))

(define (post-incr! n-list)
   (let ((z (car n-list)))
      (set-car! n-list (1+ z))
      z
   )
)

(define (monus1 n) (if (> n 0) (-1+ n) n))


(define (find-max a)
    (cond ((not (pair? a))
               (if (number? a) a 0)
          )
          (else (max (find-max (car a)) (find-max (cdr a))))
    )
)

(define (pin-headed-line pt1 pt2)
   (compose (line pt1 pt2)
            (arc pt1 0.5 0 2pi)
            (arc pt2 0.5 0 2pi)
   )
)

(define (pin-headed-pict-line pt1 pt2)
  (stroke
      (compose (line pt1 pt2)
               (arc pt1 0.5 0 2pi)
;;             (arc pt2 0.5 0 2pi)  ;; Don't draw the lower bullet, as it has been already drawn!
      )
  )
)

(define (pin-headed-pict-line-red pt1 pt2)
  (stroke
      (compose (line pt1 pt2)
               (arc pt1 0.5 0 2pi)
;;             (arc pt2 0.5 0 2pi)  ;; Don't draw the lower bullet, as it has been already drawn!
      )
      (:color (rgb 1 0 0))
  )
)

;; Test:
;; (define pentagon (draw-polygon-triangularization '((() ())) 24 100 300 #f))
;; (define handshakes (draw-n-chords '((() ())) 24 200 300 #f #f))
;; (define both (compose pentagon handshakes))
;; (out both "esim1.ps")
;; Or:
;; (define hexadecigon (draw-polygon-triangularization '((() ((()) ()) ((() ()))) (() (()))) 48 100 300 #f))
;; (define handshakes2 (draw-n-chords '((() ((()) ()) ((() ()))) (() (()))) 48 200 300 #f #f))
;; (define both2 (compose hexadecigon handshakes2))
;; (out both2 "esim2.ps")
;;



(define (draw-binary-tree sexp ox oy x_displ y_displ diamwidth)
    (with-attrib ((:line-width (or diamwidth 0.6)) (:line-cap 'round))
      (translate ox oy
             (compose
                (draw-bincordtree
                    (fill-cordtree-x-coordinates! (construct-coordinate-tree sexp) x_displ #f)
                    0 0 y_displ
                )
             )
       )
    )
)


(define (draw-uncontracted-binary-tree sexp ox oy x_displ y_displ diamwidth)
    (with-attrib ((:line-width (or diamwidth 0.6)) (:line-cap 'round))
      (translate ox oy
             (compose
                (draw-bincordtree
                    (fill-cordtree-x-coordinates! (construct-coordinate-tree sexp) x_displ #t)
                    0 0 y_displ
                )
             )
       )
    )
)


(define (draw-n-ary-tree sexp ox oy x_displ y_displ diamwidth)
    (with-attrib ((:line-width (or diamwidth 0.6)) (:line-cap 'round))
      (translate ox oy
             (compose
                (draw-cordtree
                    (normalize-root-to-zero-and-scale! (p->tree-x-coordinates sexp) x_displ)
                    0 0 y_displ
                )
             )
       )
    )
)


;;  (fill-cordtree-x-coordinates! (construct-coordinate-tree '((a . (b . c)) . ((d . e) . f))) 1)
;; --> (((0 . 0)) ((-1 . 0) (1 . 0)) ((-3/2 . -1) (-1/2 . -1) (1/2 . 1) (3/2 . 1)) ((-3/4 . -1/2) (-1/4 . -1/2) (1/4 . 1/2) (3/4 . 1/2)))

;; If the car-side of pair (caar level) can be found at
;; cdr-side of some pair of the nextlev, then this is an
;; "internal" edge colored black, otherwise it's a "leaf-edge"
;; colored red.

(define (draw-one-level-of-bincordtree level nextlev ox oy ny)
      (let loop ((level level))
            (cond ((pair? level)
                    (compose
                      (cond
                        ((find (lambda (x) (= (cdr x) (caar level))) nextlev)
                          (pin-headed-pict-line
                               (pt (+ ox (caar level)) ny)
                               (pt (+ ox (cdar level)) oy)
                          )
                        )
                        (else
                          (pin-headed-pict-line-red
                               (pt (+ ox (caar level)) ny)
                               (pt (+ ox (cdar level)) oy)
                          )
                        )
                      )
                      (loop (cdr level))
                    )
                  )
                  (else the-empty-pict)
            )
      )
)

;; Draw from top to bottom, because the result is nicer.

(define (draw-bincordtree cot ox oy y_displ)
  (let ((revcot (reverse! (cdr cot))))
    (compose
      (let loop ((ct revcot) (nextlev '())
                 (y (+ oy (* (length revcot) y_displ)))
                )
             (cond ((pair? ct)
                     (compose
                       (draw-one-level-of-bincordtree
                                      (car ct)
                                      nextlev
                                      ox (- y y_displ) y
                       )
                       (loop (cdr ct) (car ct) (- y y_displ))
                     )
                   )
                   (else the-empty-pict)
             )
      )
      ((if (eq? (length cot) 1)
           pin-headed-pict-line-red ;; For empty trees a red root dot
           pin-headed-pict-line     ;; For others a black one.
       )
           (pt (+ ox (caaar cot)) (+ oy (cdaar cot)))
           (pt (+ ox (caaar cot)) (+ oy (cdaar cot)))
      )
    ) ;; compose
  )
)



(define (draw-bincordtree-bit-older cot ox oy y_displ)
;;(stroke  ;; Already stroked, now composing pict's, not paths!
    (compose
      ((if (eq? (length cot) 1)
           pin-headed-pict-line ;; For empty trees a black root dot
           pin-headed-pict-line-red ;; For others a red one.
       )
           (pt (+ ox (caaar cot)) (+ oy (cdaar cot)))
           (pt (+ ox (caaar cot)) (+ oy (cdaar cot)))
      )
      (let loop ((ct (cdr cot)) (y oy))
             (cond ((pair? ct)
                     (compose
                       (draw-one-level-of-bincordtree
                                      (car ct)
                                      (if (pair? (cdr ct)) (cadr ct) '())
                                      ox y (+ y y_displ)
                       )
                       (loop (cdr ct) (+ y y_displ))
                     )
                   )
                   (else the-empty-pict)
             )
       )
    ) ;; compose
;;)
)

;; (normalize-root-to-zero-and-scale! (p->tree-x-coordinates '(()((())()))) 12)
;; --> (0 (-9) (9 (3 (3)) (15)))

(define (draw-cordtree ct ox oy y_displ)
  (stroke
    (let recurse ((ct ct) (y oy) (prev_y oy) (prev_x (car ct)))
       (cond ((pair? ct)
               (compose
                 (cond ((number? (car ct)) ;; At the beginning of branch-list?
                         (compose
                            (pin-headed-line
                               (pt (+ ox prev_x) prev_y)
                               (pt (+ ox (car ct)) y)
                            )
                            (recurse (cdr ct) (+ y y_displ) y (car ct)) ;; Start scanning sub-branches.
                         )
                       )
                       (else ;; Still scanning the sub-branches.
                         (compose
                            (recurse (car ct) y prev_y prev_x) ;; Draw this branch.
                            (recurse (cdr ct) y prev_y prev_x) ;; Continue sub-branches.
                         )
                       )
                  ) ;; cond
               ) ;; compose
             )
             (else the-empty-path)
       )
    )
  )
)


(define (width-for-digit-list digit-list scale)
    (let* ((font-used (font "Courier" scale))
           (str-glyph (simple-string->glyphpath font-used
                        (string-concatenate (map (lambda (x)
                                                   (format #f "~A"
                                                        (cond ((< x 10) x)
                                                              (else (ascii->char (+ x 55))) ;; 'A' = 65.
                                                        )
                                                   )
                                                 )
                                                 digit-list
                                            )
                        )
                      )
           )
           (max-pt (bounding-box:max (bounding-box str-glyph)))
           (str-width (pt:x max-pt))
          )
       str-width
    )
)


(define (draw-digit-list digit-list ox oy scale)
    (let* ((font-used (font "Courier" scale))
           (Lword (simple-string->glyphpath font-used
                    (string-concatenate (map (lambda (x)
                                                (format #f "~A"
                                                        (cond ((< x 10) x)
                                                              (else (ascii->char (+ x 55))) ;; 'A' = 65.
                                                        )
                                                )
                                             )
                                             digit-list
                                        )
                    )
                  )
           )
          )
       (translate ox oy (stroke Lword))
    )
)



;; Show also the last zero explicitly!
(define (draw-Lukasiewicz-word sexp ox oy scale)
        (draw-digit-list (append! (p->Lw sexp) (list 0)) ox oy scale)
)

(define (draw-A071158-word sexp ox oy scale)
        (draw-digit-list (if (null? sexp) (list 0) (sexp->A071158 sexp)) ox oy scale)
)


(define (draw-cardinal-number cardno ox oy scale)
    (let* ((font-used (font "Courier" scale))
           (ostr (simple-string->glyphpath font-used (format #f "~A" cardno)))
          )
       (translate ox oy (stroke ostr))
    )
)

(define (draw-cardinal-number-right cardno ox oy scale)
    (let* ((font-used (font "Courier" scale))
           (ostr (simple-string->glyphpath font-used (format #f "~A" cardno)))
           (max-pt (bounding-box:max (bounding-box ostr)))
           (str-width (pt:x max-pt))
          )
       (translate (- ox str-width) oy (stroke ostr))
    )
)

(define (draw-ordinal-number ordno ox oy scale)
    (let* ((font-used (font "Courier" scale))
           (ostr (simple-string->glyphpath font-used (format #f "~A." ordno)))
          )
       (translate ox oy (stroke ostr))
    )
)


(define (draw-parenthesization-ugly sexp ox oy scale)
  (with-attrib ((:line-width 0.6) (:line-cap 'round))
    (let* ((x_now (list ox))
           (font-used (font "Courier" scale))
           (pars (simple-string->glyphpath font-used (format #f "~S" sexp)))
          )
      (translate ox oy (stroke pars))
    )
  )
)


;; Well, we do violence to the kerning here, but we want
;; the parentheses to be exactly in same x-positions as
;; the corresponding digits of TBBS, and slopes of the Dyck path.
(define (draw-parenthesization sexp ox oy scale binexp-also?)
  (with-attrib ((:line-width 0.6) (:line-cap 'round))
    (let* ((x_now (list ox)) ;; Perfaito?
           (s 0)
           (letrsize (* 2 scale))
           (num-y-level oy)
           (par-y-level (if binexp-also? (- oy letrsize) oy))
           (font-used (font "Courier" letrsize))
           (lpar (char->glyphpath font-used #\())
           (rpar (char->glyphpath font-used #\)))
           (zero (char->glyphpath font-used #\0))
           (one  (char->glyphpath font-used #\1))
          )
     (compose
      (let recurse ((p sexp))
        (cond ((not (null? p))
                  (compose (translate (car x_now) par-y-level (stroke lpar))
                           (if binexp-also?
                               (translate (car x_now) num-y-level (stroke one))
                               the-empty-pict
                           )
                           (begin
                               (set! s (+ (* 2 s) 1))
                               (set-car! x_now (+ (car x_now) scale))
                               (recurse (car p)) ;; Recurse between.
                           )
                           (translate (car x_now) par-y-level (stroke rpar))
                           (if binexp-also?
                               (translate (car x_now) num-y-level (stroke zero))
                               the-empty-pict
                           )
                           (begin
                               (set! s (+ (* 2 s) 0))
                               (set-car! x_now (+ (car x_now) scale))
                               (recurse (cdr p)) ;; Recurse the rest.
                           )
                  )
              )
              (else the-empty-pict)
        )
      ) ;; let recurse
      (cond (binexp-also?
              (let* ((str (format #f "~A" s))
                     (str-glyph (simple-string->glyphpath font-used str))
                     (max-pt (bounding-box:max (bounding-box str-glyph)))
                     (str-width  (pt:x max-pt))
                    )
                 (translate
                            (- ox (+ (* 2 scale) str-width)) ;; Was: (- ox (* scale (+ 2 (string-length str))))
                            num-y-level
                            (stroke str-glyph)
                 )
              )
            )
            (else the-empty-pict)
      ) ;; let recurse
     ) ;; compose
    ) ;; let*
  ) ;;
)

(define (parenthesization->binexp sexp)
  (let ((s 0))
    (let recurse ((p sexp))
       (cond ((not (null? p))
                 (set! s (+ (* 2 s) 1))
                 (recurse (car p)) ;; Recurse between.
                 (set! s (+ (* 2 s) 0))
                 (recurse (cdr p)) ;; Recurse the rest.
             )
       )
    ) ;; let recurse
    s
  )
)


(define (draw-decimal-tbbs sexp ox oy scale)
  (with-attrib ((:line-width 0.6) (:line-cap 'round))
      (compose
         (let* ((str (format #f "~A" (parenthesization->binexp sexp)))
                (letrsize scale)
                (font-used (font "Courier" letrsize))
                (str-glyph (simple-string->glyphpath font-used str))
                (max-pt (bounding-box:max (bounding-box str-glyph)))
                (str-width  (pt:x max-pt))
               )
                 (translate
                            (- ox str-width)
                            oy
                            (stroke str-glyph)
                 )
         ) ;; let*
      ) ;; compose
  ) ;;
)


(define (draw-dyck-path sexp ox oy scale linewidth)
  (with-attrib ((:line-width (or linewidth 0.6)) (:line-cap 'round))
    (let ((x_now (list ox)))
       (stroke
         (let recurse ((s sexp) (level 0))
            (cond ((not (null? s))
                    (compose
                      (pin-headed-line ;; Upward slope /
                          (pt (car x_now) (+ oy (* scale level)))
                          (pt (+ (car x_now) scale) (+ oy (* scale (+ 1 level))))
                      )
                      (begin (set-car! x_now (+ (car x_now) scale))
                             (recurse (car s) (+ level 1)) ;; Recurse between.
                      )
                      (pin-headed-line ;; Downward slope \
                          (pt (car x_now) (+ oy (* scale (+ 1 level))))
                          (pt (+ (car x_now) scale) (+ oy (* scale level)))
                      )
                      (begin (set-car! x_now (+ (car x_now) scale))
                             (recurse (cdr s) level) ;; Recurse the rest.
                      )
                    ) ;; compose
                  )
                  ((zero? level) ;; We want one dot . for an empty path ().
                      (pin-headed-line (pt (car x_now) oy) (pt (car x_now) oy))
                  )
                  (else the-empty-path)
            )
         )
       ) ;; stroke
    ) ;; let
  ) ;; with-attrib
)

;; Draw a non-crossing Murasaki-diagram:
(define (draw-rr-diagram sexp x y scale linewidth height-overdrive?)
 (let* ((height (or height-overdrive? (* scale (max 3 (count-pars sexp)))))
        (y (+ y height))
       )
  (with-attrib ((:line-width (or linewidth 0.6)) (:line-cap 'round))
    (stroke
      (let loop ((partitions (add-depths-to-rr-parts! (sexp->pp-qq-rr-cycles sexp))))
            (cond ((pair? partitions)
                     (let* ((part (cdar partitions))
                            (depth (caar partitions))
                            (leftmost (car part))
                            (rightmost (car (last-pair part)))
                           )
                       (compose ;; First the horizontal line: (Just a dot if leftmost = rightmost)
                            (line (pt (+ x (* scale leftmost)) (- y (* scale depth)))
                                  (pt (+ x (* scale rightmost)) (- y (* scale depth)))
                            )
                            (let inloop ((part part)) ;; then the vertical lines.
                                (cond ((null? part) the-empty-path)
                                      (else
                                          (compose
                                            (line (pt (+ x (* scale (car part))) (- y (* scale depth)))
                                                  (pt (+ x (* scale (car part))) (- y height))
                                            )
                                            (inloop (cdr part))
                                          )
                                      )
                                )
                            )
                            (loop (cdr partitions))
                       ) ;; compose
                     )
                  )
                  (else the-empty-path)
            )
      )
    )
  )
 )
)

(define (draw-polygon-triangularization sexp ox oy radius chordwidth)
    (draw-chords-with-or-without-circle (bt->pt sexp) radius ox oy #f #f chordwidth #f 0 0 0)
)


(define (draw-n-chords sexp ox oy radius chordwidth perimwidth)
    (draw-chords-with-or-without-circle (sexp->hs sexp) radius ox oy #t #t chordwidth perimwidth 1 0 0)
)

(define (draw-qq-chords sexp ox oy radius chordwidth perimwidth)
    (draw-chords-with-or-without-circle (sexp->pp-qq-rr sexp) radius ox oy #t #f chordwidth perimwidth 0 0 1)
)



(define (draw-chords-with-or-without-circle chords radius ox oy circle? curved? chordwidth perim_width r g b)
   (let ((perimwidth (or perim_width 0.3)))
    (with-attrib ((:line-width (or chordwidth 0.5)))
      (translate ox (+ oy radius) ;; According to the bottom, not the center.
             (compose
                (draw-chords chords radius curved?)
                (if circle?
                    (stroke (arc origin (+ radius perimwidth) 0 2pi)
                            (:color (rgb r g b))
                            (:line-width perimwidth)
                    )
                    the-empty-pict
                )
             )
       )
    )
   )
)


;;
;; The point 1 is a half-angle clockwise from the angle 3/2 pi
;; The point 2 is one angle and half clockwise from the angle 3/2 pi
;; The point n is a half-angle counter-clockwise from the angle 3/2 pi.
;;
;;
;;

;; angle = (3/2 pi + pi/n) - (v * (2pi/n))

(define (compute-vert-angle v n)
  (let ((a (- (+ (/ (* 3 pi) 2) (/ pi n)) ;; Subtract from the angle of n
              (* v (/ 2pi n)) ;; ... the v * angle used.
           )
       ))
     (if (< a 0) (+ a 2pi) a) ;; Ensure that it is positive angle.
  )
)


(define (get-edge-point v n radius)
  (let ((angle (compute-vert-angle v n)))
     (pt (* radius (cos angle)) (* radius (sin angle)))
  )
)

;; g1 and g2 are angles from the origo to the vertices v1 and v2
;; respectively.
;; angle = the mean of the angles g1 and g2, the angle from origo
;; to antiorigo.
;; h = height of equilateral triangle whose base is the line segment v1-v2,
;; and 2h is distance between the origo and the antiorigo

;; The angle between the first and the last vertices is straight down.

(define (draw-anti-arc v1 v2 n radius)
  (let* ((g1 (compute-vert-angle
               ((if (eq? (abs (- v1 v2)) (- n 1)) min max) v1 v2) n))
         (g2 (compute-vert-angle
               ((if (eq? (abs (- v1 v2)) (- n 1)) max min) v1 v2) n))
         (h  (* radius (cos (/ pi n))))
;;       (angle (/ (+ g1 g2) 2)) ;; Doesn't work that way...
         (angle (compute-vert-angle
                   (if (eq? (abs (- v1 v2)) (- n 1)) ;; First & the last vert?
                       (/ 1 2) ;; Then straight down.
                       (/ (+ v1 v2) 2) ;; Otherwise their average.
                   )
                   n
                )
         )
         (antiorigo (pt (* 2 h (cos angle)) (* 2 h (sin angle))))
        )
     (arc antiorigo radius (+ pi g1) (+ pi g2))
  )
)


(define (draw-chords chords radius curved?)
  (stroke
    (let ((n (find-max chords)))
      (let loop ((chords chords))
            (cond ((and (pair? chords) (pair? (car chords)))
                    (compose
                      (cond
                         ((= (caar chords) (cdar chords)) ;; Point connected only with itself!?
                            (arc (get-edge-point (caar chords) n radius) 0.5 0 2pi) ;; Then draw a smallish circle.
                         )
                         ((and curved?
                               (> n 2)
                               (memq (abs (- (caar chords) (cdar chords)))
                                     (list 1 (- n 1)) ;; Neighbours?
                               )
                          )
                            (draw-anti-arc (caar chords) (cdar chords) n radius)
                         )
                         (else ;; A straight line.
                            (line (get-edge-point (caar chords) n radius)
                                  (get-edge-point (cdar chords) n radius)
                            )
                         )
                      ) ;; cond
                      (loop (cdr chords))
                    ) ;; compose
                  )
                  (else the-empty-path)
            )
      )
    )
  )
)


;;
;; arcs: the same syntax as with chords: ((n1 . n2) (n3 . n4) ...)
;; size: distance between the beats.
;; baselinewidth: #f if no baseline is requested, otherwise its width.
;; baselinemargin: margin to the left and right of the beat 1 and max-beat



(define (draw-arcs-with-or-without-base arcs size ox oy downside? arcwidth base_linewidth baselinemargin r g b)
   (let ((baselinewidth (or base_linewidth 0.3))
         (max-beat (find-max arcs))
        )
    (with-attrib ((:line-width (or arcwidth 0.5)))
      (translate (+ ox baselinemargin) oy
             (compose
                (draw-arcs arcs max-beat size downside?)
                (if base_linewidth
                    (stroke (line (pt (- baselinemargin) 0)
                                  (pt (+ baselinemargin (* size (- max-beat 1))) 0)
                            )
                            (:color (rgb r g b))
                            (:line-width baselinewidth)
                    )
                    the-empty-pict
                )
             )
       )
    )
   )
)


(define (draw-arcs arcs max-beat size downside?)
 (stroke
   (let loop ((arcs arcs))
      (cond ((and (pair? arcs) (pair? (car arcs)))
               (compose
                      (arc (pt (* size (- (/ (+ (caar arcs) (cdar arcs)) 2) 1))
                               0
                           )
                           (* (/ size 2)
                              (- (max (caar arcs) (cdar arcs))
                                 (min (caar arcs) (cdar arcs))
                              )
                           )
                           (if downside? pi 0)
                           (if downside? 2pi pi)
                      )
                 (loop (cdr arcs))
               ) ;; compose
            )
            (else the-empty-path)
      )
   )
 )
)


;; (outeps (draw-arcs-with-or-without-base '((1 . 5) (2 . 6) (3 . 4) (4 . 8) (5 . 9) (6 . 7) (7 . 11) (8 . 12) (9 . 10)) 20 40 40 #f #f #f 0 0 0 0)  "j441.eps")
;; (outeps (draw-arcs-with-or-without-base '((1 . 5) (2 . 6) (3 . 4) (4 . 8) (5 . 9) (6 . 7) (7 . 11) (8 . 12) (9 . 10)) 20 40 40 #f #f 0.5 10 0 0 1)  "jb441.eps")

;; 45141 (= 14514): a tennis pattern with shades of 441.
;; This starts with 441 and then changes to 14514.
;; (outeps (draw-arcs-with-or-without-base '((1 . 5) (2 . 6) (3 . 4) (4 . 8) (5 . 10) (6 . 7) (7 . 11) (8 . 9) (9 . 13) (10 . 15) (11 . 12) (12 . 16) (13 . 14) (14 . 18) (15 . 20) (16 . 17) (17 . 21) (18 . 19)) 10 10 50 #f #f 1.5 5 1 0 0)  "jb14514.eps")

;; (outeps (draw-arcs-with-or-without-base '((1 . 10) (2 . 5) (3 . 4) (6 . 7) (8 . 9)) 6 0 35 #t #f 0.5 3 1 0 0)  "ob54.eps")


(define (out pict filename) (show-w/ps2-text-channel filename pict))
(define (outeps pict filename) (show-w/ps2-text-channel filename pict (:format "EPS")))


(define (read-lists-from infile)
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



;; The following functions contain some very ugly constants, until I
;; invent something better...

(define (compose-one-cycle-old sexps radius x_start x_displ y_now)
   (let loop ((sexps sexps) (x x_start))
      (cond ((not (null? sexps))
               (compose
                   (draw-n-ary-tree (car sexps)  x (car y_now) (/ radius 2) (/ radius 2) #f)
                   (draw-binary-tree (car sexps) x (- (car y_now) (* 3 radius)) (/ radius 2) (/ radius 2) #f)
                   (draw-polygon-triangularization (car sexps) x (- (car y_now) (* 6 radius)) radius #f)
                   (draw-n-chords (car sexps) x (- (car y_now) (* 9 radius)) radius #f #f)
                   (loop (cdr sexps) (+ x x_displ))
               )
            )
            (else the-empty-pict)
      )
   )
)

;; Some of these interpretations are very Ad Hoc.
;; It would be better, if we could have format (interpretation various-output-position-et-style-attributes ...)
;; instead of a simple symbol.
;; Yes, now we start building something like that.

(define (compose-all-interpretations-for-a-single sexp scale x y interpretations chordwidth perimwidth ordnow)
  (let ((somewhat-left (* 4 scale))
        (size (count-pars sexp))
       )
    (cond
      ((not (pair? interpretations)) the-empty-pict)
      (else
         (compose
            (let* ((desc (car interpretations))
                   (interp-let (if (pair? desc) (car desc) desc))
                   (centered? (and (pair? desc) (or (memq ':c desc) (memq ':C desc))))
                   (x_extra_offset (if (and (pair? desc) (pair? (cdr desc)) (number? (cadr desc))) (cadr desc) 0))
                   (transl_x
                       (+ x_extra_offset
                          (cond (centered? (* 0.5 scale size) 0) (else 0))
                       )
                   )
                   (transl_y 0)
                  )
              (translate transl_x transl_y
                (case interp-let
                   ((()) ;; Nil for padding. Draw nothing, except consume some Y-space.
                       the-empty-pict
                   )
                   ((a) ;; triangulations
                       (draw-polygon-triangularization sexp x y scale chordwidth)
                   )
                   ((d) ;; plane binary trees with 2n+1 vertices
                       (draw-binary-tree sexp x y (/ scale 2) (/ scale 2) chordwidth)
                   )
                   ((dU) ;; plane binary trees with 2n+1 vertices, uncontracted
                       (draw-uncontracted-binary-tree sexp x y (/ scale 2) (/ scale 2) chordwidth)
                   )
                   ((e) ;; plane (general) trees with n+1 vertices
                       (draw-n-ary-tree sexp x y (/ scale 2) (/ scale 2) chordwidth)
                   )
                   ((e+d) ;; plane (general) trees with n+1 vertices, shifted by 0.5 * scale * size points right. + binary trees to their left.
                     (compose
                       (translate (* 0.5 scale (count-pars sexp)) 0
                             (draw-n-ary-tree sexp x y (/ scale 2) (/ scale 2) chordwidth)
                       )
                       (draw-binary-tree sexp (- x somewhat-left) y (/ scale 2) (/ scale 2) chordwidth)
                     )
                   )
                   ((i) ;; ordinary Dyck paths
                       (draw-dyck-path sexp x y (/ scale 2) chordwidth)
                   )
                   ((n) ;; non-crossing handshakes at a circular table.
                       (draw-n-chords sexp x y scale chordwidth perimwidth)
                   )
                   ((qq) ;; non-crossing partitions.
                       (draw-qq-chords sexp x y scale chordwidth perimwidth)
                   )
                   ((rr) ;; non-crossing Murasaki-diagrams
                       (draw-rr-diagram sexp (- x (/ scale 2)) y (/ scale 2) chordwidth #f)
                   )
                   ((A071156)
                       (draw-cardinal-number (sexp->A071156 sexp) x y scale)
                   )
                   ((A071158)
                       (draw-A071158-word sexp x y scale)
                   )
                   ((L) ;; Lukasiewicz-words
                       (draw-Lukasiewicz-word sexp x y scale)
                   )
                   ((Td) ;; Totally balanced binary sequences as a decimal number, as in A014486, right-justified.
                       (draw-decimal-tbbs sexp x y scale)
                   )
                   ((Tdsmall) ;; Totally balanced binary sequences as a decimal number, as in A014486, right-justified.
                       (draw-decimal-tbbs sexp x y (- scale 2)) ;; But with a smaller font. Kludge!
                   )
                   ((O) ;; Ordinal numbers... (somewhat a kludge...)
                       (draw-ordinal-number (post-incr! ordnow) (* 4 scale) y scale)
                   )
                   ((S) ;; Size
                       (draw-cardinal-number (count-pars sexp) (* 4 scale) y scale)
                   )
                   ((S*2+1) ;; Size*2 + 1
                       (draw-cardinal-number (1+ (* 2 (count-pars sexp))) (* 4 scale) y scale)
                   )
                   ((P) ;; Parenthesizations
                       (draw-parenthesization sexp x y (/ scale 2) #f)
                   )
                   ((P+T) ;; Parenthesizations + totally balanced binary sequences.
                       (draw-parenthesization sexp x y (/ scale 2) #t)
                   )
                ) ;; case
              ) ;; translate
            ) ;; let
            (compose-all-interpretations-for-a-single sexp scale x (- y (* 3 scale)) (cdr interpretations) chordwidth perimwidth ordnow)
         ) ;; compose
      ) ;; else
    ) ;; cond
  )
)



(define (compose-one-cycle sexps radius x_start x_displ y_now interpretations ordnow)
   (let loop ((sexps sexps) (x x_start))
      (cond ((not (null? sexps))
               (compose (compose-all-interpretations-for-a-single (car sexps) radius x (car y_now) interpretations #f #f ordnow)
                        (loop (cdr sexps) (+ x x_displ))
               )
            )
            (else the-empty-pict)
      )
   )
)



(define (compose-one-partition partition radius x_start x_displ y_now y_required interpretations ordnow)
   (let loop ()
      (cond ((and (pair? partition) (pair? (cdr partition)) ;; Still something to print?
                  (> (car y_now) y_required) ;; Still fits in this page?
             )
               (compose (compose-one-cycle (cadr partition) radius x_start x_displ y_now interpretations ordnow)
                        (begin
                               (set-car! y_now (- (car y_now) y_required))
                               (delete! (cadr partition) partition)
                               (loop)
                        )
               )
            )
            (else the-empty-pict)
      )
   )
)


(define (compose-pictures-of-partition lists radius x_start x_displ y_now y_required interpretations ordnow)
  (let loop () ;; ((lists (cdr lists)))
      (cond ((and (pair? lists) (pair? (cdr lists)) ;; Still something to print?
                  (> (car y_now) y_required) ;; Still fits in this page?
             )
               (begin
;; First, insert our "anchor" to the beginning of partition (an integer, in contrast to list structures), if not already there:
                  (cond ((not (integer? (caadr lists))) (attach! (length (cadr lists)) (cadr lists))))
                  (compose (compose-one-partition (cadr lists) radius x_start x_displ y_now y_required interpretations ordnow)
                           (cond ((and (pair? (cadr lists)) (pair? (cdr (cadr lists)))) ;; Still something to print, but no space...
                                    the-empty-pict
                                 )
                                 (else ;; We exhausted this partition
                                       (delete! (cadr lists) lists)
                                       (loop)
                                 )
                           )
                  )
               )
            )
            (else the-empty-pict)
      )
  )
)



;; Use as:
;; (output-part-file-as-ps-file "a057161.sxp" "a057161.ps" 8 '(a d) #f #f)
;; (output-part-file-as-ps-file "a057501.sxp" "a057501.ps" 9 '(n i P e L d a) #f #f)
;; (output-part-file-as-ps-file "a069771.sxp" "a069771.ps" 9 '(n i P e L d) #f #f)
;; (output-part-file-as-ps-file "a079438.sxp" "a079438.ps" 10 '(i () (e :C) P () () d a n) 60 240)
;; (output-part-file-as-ps-file "a079442.sxp" "a079442.ps" 10 '(i () (e :C) P () () d a n) 60 140)
;; (output-part-file-as-ps-file "a080070.sxp" "a080070.ps" 10 '(O i (e :C) d P+T) 140 0)
;; (output-part-file-as-ps-file "a080120.sxp" "a080120.ps" 10 '(S*2+1 i (e :C) (n :C) P+T L) 140 0)
;; (output-part-file-as-ps-file "a080263.sxp" "a080263.ps" 8 '(O e (Td +16) () () dU a) 240 240)
;; (output-part-file-as-ps-file "a080973.sxp" "a080973.ps" 8 '(O e (Tdsmall +200) n) 240 240)
;; (output-part-file-as-ps-file "a086429.sxp" "a086429.ps" 9 '(n qq rr A071158 A071156 i P) #f #f)
;; (output-part-file-as-ps-file "a102241.sxp" "a102241.ps" 10 '(O i (e :C) (d -80) P+T) 140 0)


(define (output-part-file-as-ps-file infile outfile radius interpretations x_start_overwrite? x_displ_overwrite?)
   (let* ((lists (read-lists-from infile)) ;; Keep the header all the time. Because this is modified with delete!
          (y_start_below_header 730)
          (y_now (list y_start_below_header))
          (y_required (* 3 radius (+ 1 (length interpretations))))
          (ordnow (list 0))
          (outchannel (ps2-text-channel outfile))
          (format-str (string-append (caar lists) "   --   Page ~A"))
          (font-used (font "Times-Roman" 10))
          (x_start (or x_start_overwrite? (* 4 radius)))
          (x_displ (or x_displ_overwrite? (* 7 radius)))
         )
     (let loop ((pageno 1))
        (cond ((not (null? (cdr lists)))
                (format #t "Doing page ~S, ordnow=~S, (length lists)=~S, (car lists)=~S, (cadr lists)=~S~%"
                             pageno ordnow (length lists) (car lists) (cadr lists))
                (force-output (current-output-port))
                (show outchannel
                   (compose
                      (translate (* 1 radius) (+ (car y_now) 50)
                         (stroke (simple-string->glyphpath font-used (format #f format-str pageno)))
                      )
                      (compose-pictures-of-partition lists radius x_start x_displ y_now y_required interpretations ordnow)
;;                    (compose-one-instance-per-line! lists outchannel radius (* 8 radius) y_now (* 6 radius))
                   )
                )
                (set-car! y_now y_start_below_header)
                (loop (+ 1 pageno))
              )
        )
     )
     (close-channel outchannel)
   )
)


(define (compose-one-instance-per-line! sexps outchannel radius x_start y_now y_displ)
   (let loop () ;; (sexps (cdr sexps)))
      (cond ((and (pair? sexps) (pair? (cdr sexps)) ;; Still something to print?
                  (> (car y_now) (+ (* 2 radius) y_displ)) ;; Still fits in this page?
             )
               (compose
                   (draw-ordinal-number (car sexps) radius                   (- (car y_now) radius) radius)
                   (draw-dyck-path   (cadr sexps) x_start                    (car y_now) (/ radius 2) #f)
                   (draw-parenthesization
                                     (cadr sexps) x_start                    (- (car y_now) radius) (/ radius 2) #t)

                   (draw-n-chords
                                     (cadr sexps) (+ x_start (* 9.5 radius))  (car y_now) radius #f #f)

                   (draw-Lukasiewicz-word
                                     (cadr sexps) (+ x_start (* 13 radius))  (- (car y_now) radius) radius)
                   (draw-n-ary-tree  (cadr sexps) (+ x_start (* 13 radius))  (car y_now) (/ radius 2) (/ radius 2) #f)


                   (draw-qq-chords
                                     (cadr sexps) (+ x_start (* 19 radius))  (car y_now) radius #f #f)

                   (draw-cardinal-number-right
                                     (sexp->A071156 (cadr sexps))
                                     (+ x_start (* 21.5 radius))
                                     (- (car y_now) radius)
                                     radius
                   )
                   (draw-rr-diagram
                                     (cadr sexps) (+ x_start (* 22 radius))  (car y_now) (/ radius 2) #f #f)

                   (draw-A071158-word
                                     (cadr sexps) (+ x_start (* 22 radius))  (- (car y_now) radius) radius)

                   (draw-binary-tree (cadr sexps) (+ x_start (* 30 radius))  (car y_now) (/ radius 2) (/ radius 2) #f)
                   (draw-polygon-triangularization
                                     (cadr sexps) (+ x_start (* 34 radius))  (car y_now) radius #f)

                   (begin
                           (set-car! y_now (- (car y_now) y_displ))
                           (set-car! sexps (+ (car sexps) 1)) ;; Increment out ordinal number counter.
                           (delete! (cadr sexps) sexps)
                           (loop)
                   )
               )
            )
            (else the-empty-pict)
      )
   )
)


(define (decr-in-list! lp x)
  (let ((old-car (car lp)))
     (set-car! lp (- old-car x))
     old-car
  )
)

;; Use as:
;; (output-sexp-file-as-ps-file "a014486.sxp" "a014486.ps" 12 "Page ~A of http://www.research.att.com/~~njas/sequences/a014486.ps.gz" *FIRST-PAGE*)

;; or say as:
;; (output-sexp-file-as-ps-file "a014486samp.sxp" "a014486s.ps" 12 "Page ~A of http://www.iki.fi/~~kartturi/jossakin/a014486s.ps.gz" *FIRST-PAGE*)


(define *FIRST-PAGE* (list
""
" From the left to right, after the ordinal number we show:"
" - the terms of A014486 and the terms of A063171, with the corresponding Catalan mountain range"
"   (i.e. Dyck path, Stanley's interpretation (i)) on their top, and the parenthesization below."
" - noncrossing chord arrangements (red circle, Stanley's (n), noncrossing handshakes over a circular table)."
" - the plane general trees with n edges (Stanley's (e)), with the corresponding Lukasiewicz-word underneath (A079436)."
" - noncrossing partitions (blue circle, Stanley's (qq)), with the terms of A071156 underneath."
" - noncrossing Murasaki diagrams ('genji-mon', Stanley's (rr)), with the terms of A071158 underneath."
" - the plane binary trees with 2n edges (Stanley's (d)), with the corresponding zigzag tree (Stanley's (c),"
"   i.e. binary trees with n vertices) contained in their interior colored black."
" - triangulations of a polygon with n+2 edges (Stanley's (a))."
" "
" If one counts how many structures there are of each size n, one obtains Catalan numbers, A000108."
" "
" For further information, see entry A014486 in Neil J. A. Sloane's"
" On-Line Encyclopedia of Integer Sequences at http://www.research.att.com/~njas/sequences/"
" This file prepared 2003 by Antti Karttunen, http://www.iki.fi/~kartturi/"
                            )
)

(define (output-sexp-file-as-ps-file infile outfile radius pageheader on-the-first-page)
   (let* ((lists (read-lists-from infile)) ;; Keep the header all the time. Because this is modified with delete!
          (y_now (list 820))
          (outchannel (ps2-text-channel outfile))
          (font-used (font "Times-Roman" 10))
          (caption-size 10)
          (c-font-used (font "Times-Roman" caption-size))
         )
     (set-car! lists 0) ;; Now we keep our ordinal number counter here!
     (let loop ((pageno 1))
        (cond ((not (null? (cdr lists)))
                (format #t "Doing page ~S, (length lists)=~S, (car lists)=~S, (cadr lists)=~S~%"
                             pageno (length lists) (car lists) (cadr lists))
                (force-output (current-output-port))
                (show outchannel
                   (compose
                      (translate (* 1 radius) (decr-in-list! y_now (if (= 1 pageno) (* 2 radius) 70))
                         (stroke (simple-string->glyphpath font-used (format #f pageheader pageno)))
                      )
                      (cond ((= 1 pageno) ;; Only on the first page.
                               (let uloop ((rows on-the-first-page))
                                    (cond ((null? rows) (decr-in-list! y_now 32) the-empty-pict)
                                          (else
                                            (compose
                                              (translate (* 1 radius) (decr-in-list! y_now (* 1 caption-size))
                                                (stroke (simple-string->glyphpath c-font-used (car rows)))
                                              )
                                              (uloop (cdr rows))
                                            )
                                          )
                                    )
                               )
                            )
                            (else the-empty-pict)
                      )
                      (compose-one-instance-per-line! lists outchannel radius (* 8 radius) y_now (* 6 radius))
                   )
                )
                (set-car! y_now 820)
                (loop (+ 1 pageno))
              )
        )
     )
     (close-channel outchannel)
   )
)

;; (convert-sexp-file-to-multiple-ps-files "a014486_24.sxp" "kuvat/" 'n 10 0.5 0.5)
;;
;; Try also:
;; (convert-sexp-file-to-multiple-ps-or-eps-files "a014486_196.sxp" "kuvat/" 'e 10 0.4 0.4 #t)
;;

(define (output-icons-for-five-interpretations)
  (convert-sexp-file-to-multiple-ps-files "a014486_65.sxp" "kuvat/" 'n 10 0.4 0.4)
  (convert-sexp-file-to-multiple-ps-files "a014486_65.sxp" "kuvat/" 'a 10 0.5 0.5)
  (convert-sexp-file-to-multiple-ps-files "a014486_65.sxp" "kuvat/" 'd 10 0.5 0.5)
  (convert-sexp-file-to-multiple-ps-files "a014486_65.sxp" "kuvat/" 'e 10 0.4 0.4)
  (convert-sexp-file-to-multiple-ps-files "a014486_65.sxp" "kuvat/" 'i 10 0.5 0.5)
)

(define (output-icons-for-other-interpretations)
  (convert-sexp-file-to-multiple-ps-files "a014486_196.sxp" "kuvat/" 'd 10 0.5 0.5)
  (convert-sexp-file-to-multiple-ps-files "a014486_196.sxp" "kuvat/" 'qq 10 0.4 0.4)
  (convert-sexp-file-to-multiple-ps-files "a014486_196.sxp" "kuvat/" 'rr 10 0.4 0.4)
)

;;
;; Then use a C-shell-script like this:
;;
;; #!/bin/csh
;; foreach file ($*)
;; set bas=`/bin/expr $file : '\([^.]*\)'`
;; pstopnm $file -llx 1.21 -lly 1.33 -urx 1.57 -ury 1.70 -verbose -xborder 0 -yborder 0 -xs 60 -portrait -nocrop -stdout > $bas.ppm
;; ppmtogif -transparent white $bas.ppm > $bas.gif
;; /bin/rm -f $bas.ppm
;; end
;;
;; 

(define (convert-sexp-file-to-multiple-ps-files infile outfile-prefix interpretation scale chordwidth perimwidth)
   (convert-sexp-file-to-multiple-ps-or-eps-files infile outfile-prefix interpretation scale chordwidth perimwidth #f)
)


(define (convert-sexp-file-to-multiple-ps-or-eps-files infile outfile-prefix interpretation scale chordwidth perimwidth eps?)
   (let* ((lists (read-lists-from infile)) ;; The header is the first.
          (x 100)
          (y 100)
         )
     (let loop ((n 0) (lists (cdr lists)))
        (cond ((not (null? lists))
                (if eps?
                  (show-w/ps2-text-channel
                       (format #f "~A~A~S.eps" outfile-prefix interpretation n)
                       (compose-all-interpretations-for-a-single (car lists) scale x y (list interpretation) chordwidth perimwidth #f)
                       (:format "EPS")
                  )
                  (show-w/ps2-text-channel
                       (format #f "~A~A~S.ps" outfile-prefix interpretation n)
                       (compose-all-interpretations-for-a-single (car lists) scale x y (list interpretation) chordwidth perimwidth #f)
                  )
                )
                (loop (+ 1 n) (cdr lists))
              )
        )
     )
   )
)
