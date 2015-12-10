
(define reversed_iota
  (lambda (n)
      (if (zero? n) (list)
          (cons n (reversed_iota (- n 1)))
      )
  )
)

(define iota (lambda (n) (reverse! (reversed_iota n))))

(define (iota0 upto_n)
   (let loop ((n upto_n) (result (list)))
      (cond ((zero? n) (cons 0 result))
            (else (loop (- n 1) (cons n result)))
      )
   )
)


(define (nthmemq elem lista)
   (let loop ((lista lista) (i 0))
     (cond ((null? lista) #f)
           ((eq? (car lista) elem) i)
           (else (loop (cdr lista) (1+ i)))
     )
   )
)

(define attach! ; Borrowed from Franz lisp, is like destructive cons.
  (lambda (elem lista)
     (set-cdr! lista (cons (car lista) (cdr lista)))
     (set-car! lista elem)
     lista
  )
)


(define (count-pars a)
    (cond ((not (pair? a)) 0)
          (else (+ 1 (count-pars (car a)) (count-pars (cdr a))))
    )
)


(define (nthcdr n lista)
  (if (or (zero? n) (null? lista))
      lista
      (nthcdr (- n 1) (cdr lista))
  )
)


;; For testing whether we have an identity permutation or not.
(define (first_dislocated lista)
   (let loop ((lista lista) (i 0))
     (cond ((null? lista) lista)
           ((not (eq? (car lista) i)) lista)
           (else (loop (cdr lista) (1+ i)))
     )
   )
)

(define (prsymbol x)
   (cond ((symbol? x) (write-string (string-upcase (symbol->string x))))
         (else (write x))
   )
)

(define (prlist x)
   (cond ((null? x) (newline))
         ((list? x) (prsymbol (car x))
                    (if (not (null? (cdr x))) (write-string " "))
                    (prlist (cdr x))
         )
         (else (write x))
   )
)



