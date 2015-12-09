#!r6rs
(library (IntSeq memoize-definec)
  (export *MAX-CACHE-SIZE-FOR-DEFINEC* definec defineperm1)
  (import
      (rnrs base (6))
      (rnrs io simple (6))
      (IntSeq Memoize memoize-base)
      (IntSeq Utils vector-extra-utils)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  memoize-definec.ss - Macro definec for defining memoized (i.e. cached) ;;
;;    functions which take one nonnegative integer argument.               ;;
;;    May memoize any values, even other memoized functions (closures).    ;;
;;                                                                         ;;
;;    First R6RS-version created by Antti Karttunen Dec 04 2015            ;;
;;    by extracting code from the old MIT/GNU-Scheme module definech.scm   ;;
;;                                                                         ;;
;;    Copyright (C) 2002-2015 Antti Karttunen, subject to GPL v2.          ;;
;;                                                                         ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; define unary cached functions. Syntax is like
;; (define (func arg) ...) of Scheme.


;; Added this 10. July 2002 to avoid allocation catastrophes
;; caused by the careless use of cached integer functions:

(define *MAX-CACHE-SIZE-FOR-DEFINEC* 290512) ;; Was 131072 ;; XXX - How to change the value later?

(define-syntax definec
  (syntax-rules ()
   ((definec (name arg) e0 ...)
      (define name
        (implement-cached-function *MAX-CACHE-SIZE-FOR-DEFINEC* (name arg) e0 ...)
      ) ;; (define name ...)
   )
  ) ;; syntax-rules
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is for defining one-based permutations.
;; Stores automatically also the inverse permutation (in _invcache_),
;; which can be accessed with negative arguments.

;; Note that the defined function is subtly state-dependent. The defined permutation must be
;; computed before its inverse!

;; Yes, this is dirty and dangerous.
;; In general this shouldn't be used, only in emergency cases,
;; when I'm in hurry (or just lazy).

(define-syntax defineperm1
  (syntax-rules ()
   ((defineperm1 (name arg) e0 ...)
      (define name
        (letrec
           ((_cache_ (vector #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
            (_invcache_ (vector #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)) ;; For inverses.
            (name
             (lambda (arg)
               (cond
                 ((null? arg) _cache_)
                 ((eq? #t arg) _invcache_)
                 ((< arg 0) ;; (foo -n) means (foo^-1 n)
                     (cond ((>= (- arg) (vector-length _invcache_)) #t)
                           (else (vector-ref _invcache_ (- arg)))
                     )
                 )
                 ((>= arg *MAX-CACHE-SIZE-FOR-DEFINEC*)
                      e0 ...
                 )
                 (else
                   (begin
                     (if (and (>= arg (vector-length _cache_))
                              (< arg *MAX-CACHE-SIZE-FOR-DEFINEC*)
                         )
                         (set! _cache_ (grow-cache _cache_ arg *MAX-CACHE-SIZE-FOR-DEFINEC*))
                     )
                     (or (vector-ref _cache_ arg)
                         ((lambda (res)
                           (let ((invcachesize (vector-length _invcache_)))
                            (begin
                              (if (< arg *MAX-CACHE-SIZE-FOR-DEFINEC*)
                                  (vector-set! _cache_ arg res)
                              )
  ;; Handle the inverse cache. First ensure that there's enough space:
                              (cond ((and (< res *MAX-CACHE-SIZE-FOR-DEFINEC*)
                                          (or (>= res invcachesize) (>= arg invcachesize))
                                     )
                                      (begin
                                       (set! _invcache_
                                             (vector-grow
                                                 _invcache_
  ;;                                                 (min *MAX-CACHE-SIZE-FOR-DEFINEC*
                                                      (max (+ 1 res)
                                                           (+ 1 (vector-length _cache_))
                                                           (* 2 (vector-length _invcache_))
                                                      )
  ;;                                                 )
                                             )
                                       )
                                       (display
                                          (string-append "Inverse cache size: " (number->string invcachesize)
                                                         " -> " (number->string (vector-length _invcache_))
                                                         ".\n"
                                          )
                                       )
                                      )
                                    )
  ;; If this result was already stored in invcache, then check that it was not cached for different arg:
                                    ((and (< res *MAX-CACHE-SIZE-FOR-DEFINEC*)
                                          (vector-ref _invcache_ res)
                                     )
                                      =>
                                         (lambda (old_n)
                                             (if (not (= old_n arg))
                                                 (assertion-violation
                                                    'defineperm1
                                                    (string-append
                              "The defined function is not injective, so it cannot be a bijection: f("
                                                        (number->string old_n)
                                                        ")=f("
                                                        (number->string arg)
                                                        ")="
                                                        (number->string res)
                                                        ".\n"
                                                    )
                                                 )
                                             )
                                         )
                                    )
                              )
                              (if (< res *MAX-CACHE-SIZE-FOR-DEFINEC*) (vector-set! _invcache_ res arg))
                              res
                            ) ;; begin
                           )
                          ) ;; (lambda (res) ...)
                           (begin e0 ...)
                         )
                     )
                   )
                 )
               ) ; cond
             )
            )
           ) ; letrec-definitions
          name
        ) ; letrec
      ) ;; (define name ...)
   )
  ) ;; syntax-rules
)


) ;; End of module memoize-definec.ss
