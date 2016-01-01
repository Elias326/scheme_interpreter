(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (if (null? items) nil (cons (proc (car items)) (map proc (cdr items))))
  )

(define (cons-all first rests)
  (cond
    ((null? rests) nil)
    (else (cons
      (append (cons first nil) (car rests))
      (cons-all first (cdr rests))))))

(define (zip pairs)
  (list (map car pairs) (map cadr pairs)))

;; Problem 18
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN Question 18
  (define orig_len (length s))
  (define (recur s)
    (cond
    ((null? s) nil)
    (else (cons (cons (- orig_len (length s)) (cons (car s) nil)) (recur (cdr s))))))
  (recur s)
  )
  ; END Question 18

;; Problem 19
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN Question 19
  (cond
  ((null? denoms) cons nil)
  ((= total 0) cons(cons nil nil))
  ((> (car denoms) total) (list-change total (cdr denoms)))
  (else (append 
    (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
    (list-change total (cdr denoms))))
  ))
  ; END Question 19

;; Problem 20
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (analyze expr)
  (cond ((atom? expr)
         ; BEGIN Question 20
         expr
         ; END Question 20
         )
        ((quoted? expr)
         ; BEGIN Question 20
         expr         
         ; END Question 20
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20
           ;(list (car body) (cdr body) (analyze (cadr body))) 
           (cons form (cons params (map analyze body)))        
           ;(cons form (cons params (analyze (cadr body))))
           ; END Question 20
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20 
           (cons (cons 'lambda (cons (car (zip values)) (map analyze body))) (map analyze (cadr (zip values))))
           ; END Question 20
           ))
        (else
         ; BEGIN Question 20
         (cons (car expr) (map analyze (cdr expr)))
         ; END Question 20
         )))

;; Problem 21 (optional)
;; Draw the hax image using turtle graphics.
(define (hax d k)
  ; BEGIN Question 21
  'REPLACE-THIS-LINE
  )
  ; END Question 21





