
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;  http://www.iki.fi/~kartturi/matikka/Schemuli/intfuns1.scm             ;;
;;  - Often needed integer functions. Now divided crudely into two.       ;;
;;                                                                        ;;
;;  Coded by Antti Karttunen (my_firstname.my_surname@gmail.com),         ;;
;;   2002-2010                                                            ;;
;;                                                                        ;;
;;  This Scheme-code is in Public Domain and runs (at least)              ;;
;;  in MIT Scheme Release 7.6.0/7.7.?, for which one can find documents   ;;
;;  and the pre-compiled binaries (for various OS's running in            ;;
;;  Intel x86 architecture) under the URL:                                ;;
;;  http://www.swiss.ai.mit.edu/projects/scheme/                          ;;
;;                                                                        ;;
;;  Last edited  Jul 29 2010 by Antti Karttunen.                          ;;
;;                                                                        ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *MAX-CACHE-SIZE-FOR-DEFINEC* 290512) ;; Was 131072

(load "../Schemuli/lstfuns1")
(load "../Schemuli/intfun_a")
(load "../Schemuli/intfun_b")
(load "../Schemuli/intfun_c")

(load "../Schemuli/miscnum2")

(load "../Schemuli/gf2xfuns")

