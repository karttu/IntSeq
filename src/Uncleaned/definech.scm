
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;;    definech.scm (h as hygienic)                                    ;;
;;    Copyright (C) 2002-2013 Antti Karttunen, subject to GPL v2.     ;;
;;                                                                    ;;
;;    Macro definec for defining cached unary (integer) functions     ;;
;;    This one is using the hygienic macro (define-syntax), and is    ;;
;;    to be used with MIT Scheme from the release 7.7.0 onward.       ;;
;;                                                                    ;;
;;    This version coded 17. July 2002 by Antti Karttunen             ;;
;;                                                                    ;;
;;    Change July 29 2010 by A.K.:                                    ;;
;;                                                                    ;;
;;    Moved the newer version of definech (using a more generic       ;;
;;    helper macro implement-cached-function, which in turn uses      ;;
;;    macro grow-cache) as well as defineperm from intfuns1.scm       ;;
;;    to here.                                                        ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; define unary cached functions. Syntax is like
;; (define (func arg) ...) of Scheme.


;; Added this 10. July 2002 to avoid allocation catastrophes
;; caused by the careless use of cached integer functions:
(define *MAX-CACHE-SIZE-FOR-DEFINEC* 290512) ;; Was 131072


;; Note that this and other cached functions depend on MIT Scheme
;; peculiarities, like that vectors are initialized to contain #f's
;; and also that #f is actually same thing as (). To be corrected.
;; (HAVE TO BE CORRECTED IF GOING TO USE THE VERSION 9 or HIGHER!)


;; We could do much more sophisticated things with the cache.
;; E.g. after n > certain size, switch to the dictionary (hash table) based
;; caching. Or give a programmer much more control over the caching
;; parameters, or build an IDE-system where the user can monitor the caching state
;; (e.g. find cache hogs) and toggle the caching parameters on the fly,
;; and ultimately, initialize the cache (e.g. for the sequences like A000001
;; and A000668) from the central database (OEIS)
;; with variety of methods (RPC, etc.). (Might even send new computed
;; values back there (but how to check/trust them?) For the future thought!)
;;


(define-syntax grow-cache
  (syntax-rules ()
   ((grow-cache cachename arg) ;; No maxsize specified.
      (vector-grow cachename (max (1+ arg) (* 2 (vector-length cachename))))
   )
   ((grow-cache cachename arg 0) ;; Or specified as zero.
      (vector-grow cachename (max (1+ arg) (* 2 (vector-length cachename))))
   )
   ((grow-cache cachename arg maxsize)
      (vector-grow cachename (min maxsize (max (1+ arg) (* 2 (vector-length cachename)))))
   )
  )
)

(define-syntax implement-cached-function
  (syntax-rules ()
   ((implement-cached-function maxcachesize (funname argname) e0 ...)
        (letrec
           ((_cache_ (vector #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
            (funname
             (lambda (argname)
               (cond ((null? argname) _cache_) ;; For debugging.
                     ((vector? argname) argname) ;; As well as this: Caches for caches!
                     ((and (not (= 0 maxcachesize)) (>= argname maxcachesize))
                          e0 ...
                     )
                     (else
                         (if (>= argname (vector-length _cache_))
                             (set! _cache_ (grow-cache _cache_ argname maxcachesize))
                         )
                         (or (vector-ref _cache_ argname)
                             ((lambda (res)
                                (vector-set! _cache_ argname res)
                                res
                              )
                               (begin e0 ...)
                             )
                         )
                     )
               ) ; cond
             )
            )
           ) ; letrec-definitions
          funname
        ) ; letrec
   )
  )
)



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
               (cond ((null? arg) _cache_)
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
                         (if (and (>= arg (vector-length _cache_))
                                  (< arg *MAX-CACHE-SIZE-FOR-DEFINEC*)
                             )
                             (set! _cache_ (grow-cache _cache_ arg *MAX-CACHE-SIZE-FOR-DEFINEC*))
                         )
                         (or (vector-ref _cache_ arg)
                             ((lambda (res)
                               (let ((invcachesize (vector-length _invcache_)))
                                (if (< arg *MAX-CACHE-SIZE-FOR-DEFINEC*)
                                    (vector-set! _cache_ arg res)
                                )
;; Handle the inverse cache. First ensure that there's enough space:
                                (cond ((and (< res *MAX-CACHE-SIZE-FOR-DEFINEC*)
                                            (or (>= res invcachesize) (>= arg invcachesize))
                                       )
                                         (set! _invcache_
                                               (vector-grow
                                                   _invcache_
;;                                                 (min *MAX-CACHE-SIZE-FOR-DEFINEC*
                                                        (max (1+ res)
                                                             (1+ (vector-length _cache_))
                                                             (* 2 (vector-length _invcache_))
                                                        )
;;                                                 )
                                               )
                                         )
                                         (format #t "Inverse cache size: ~a -> ~a.\n"
                                                    invcachesize (vector-length _invcache_)
                                         )
                                      )
;; If this result was already stored in invcache, then check that it was not cached for different arg:
                                      ((and (< res *MAX-CACHE-SIZE-FOR-DEFINEC*)
                                            (vector-ref _invcache_ res)
                                       )
                                        =>
                                           (lambda (old_n)
                                               (if (not (= old_n arg))
                                                   (error (format #f
"The defined function is not injective, so it cannot be a bijection: f(~a)=f(~a)=~a.\n"
                                                             old_n arg res)
                                                   )
                                               )
                                           )
                                      )
                                )
                                (if (< res *MAX-CACHE-SIZE-FOR-DEFINEC*)
                                    (vector-set! _invcache_ res arg)
                                )
                                res
                               )
                              ) ;; (lambda (res) ...)
                               (begin e0 ...)
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


;; We need this file loaded when we compile other modules,
;; so it's nice to have this auxiliary function here.
;; (We can say just e.g. (compile "gatomorf") instead of
;;  all that verbose stuff:)
(define (compile filename)
  (fluid-let ((sf/default-syntax-table user-initial-environment))
     (cf filename)
  )
)

