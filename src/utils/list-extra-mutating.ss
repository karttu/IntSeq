#!r6rs
(library (intseq utils list-extra-mutating)
  (export attach! reverse!)
  (import (rnrs base (6))
          (rnrs mutable-pairs (6))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;; This module Copyright (C) 2009-2015 Antti Karttunen, subject to GPL v2.  ;;
;;                                                                          ;;
;; Started writing this R6RS-module Dec 04 2015 by scavenging some useful   ;;
;; code from lstfuns1.scm                                                   ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (attach! elem lista)
  (begin
     (set-cdr! lista (cons (car lista) (cdr lista)))
     (set-car! lista elem)
     lista
  )
)

(define (reverse! lista) (reverse lista)) ;; XXX - Implement properly destructively!


) ;; End of module list-extra-mutating.ss

