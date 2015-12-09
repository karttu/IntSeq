#!r6rs
(library (IntSeq memoize-base)
  (export grow-cache implement-cached-function)
  (import
      (rnrs base (6))
      (IntSeq Utils vector-extra-utils)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  memoize-base.ss - Basic memoization macros for the needs               ;;
;;    of modules memoize-definec.ss and transforms-base.ss                 ;;
;;                                                                         ;;
;;    Initial R6RS-version created by Antti Karttunen Dec 04 2015          ;;
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

(define *MAX-CACHE-SIZE-FOR-DEFINEC* 290512) ;; Was 131072


;; Note that this and other cached functions depend on MIT Scheme
;; peculiarities, like that vectors are initialized to contain #f's
;; and also that #f is actually same thing as (). To be corrected.
;; (HAVE TO BE CORRECTED IF GOING TO USE THE VERSION 9 or HIGHER!) - XXX - Is this issue anymore AD 2015?


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
      (vector-grow cachename (max (+ 1 arg) (* 2 (vector-length cachename))))
   )
   ((grow-cache cachename arg 0) ;; Or specified as zero.
      (vector-grow cachename (max (+ 1 arg) (* 2 (vector-length cachename))))
   )
   ((grow-cache cachename arg maxsize)
      (vector-grow cachename (min maxsize (max (+ 1 arg) (* 2 (vector-length cachename)))))
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


) ;; End of module memoize-base.ss
