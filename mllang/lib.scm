(define-syntax ++
  (syntax-rules ()
    ((_ x)  (set! x (+ x 1)))
    ((_ x y)(set! x (+ x y)))))

(define-syntax neq?
  (syntax-rules ()
    ((_ x y) (not (eqv? x y)))))

(define-syntax not-null?
  (syntax-rules ()
    ((_ x) (not (null? x)))))

(define-syntax eval-i
  (syntax-rules ()
    ((_ x) (eval x (interaction-environment)))))

(define (print . xs)
  (or (and (not-null? xs)
           (display (car xs))
           (newline)
           (apply print (cdr xs)))
      (newline)))

(define (get-token-tag token)
  (vector-ref token T-TAG))

(define (get-token-coords token)
  (vector-ref token T-COORDS))

(define (get-token-value token)
  (vector-ref token T-VALUE))

(define (get-token-pos token)
  (let ((coords (get-token-coords token)))
    (vector-ref coords C-POSITION)))

(define (set-token-tag token tag)
  (vector-set! token T-TAG tag)
  token)

(define (set-token-coords token coords)
  (vector-set! token T-COORDS coords)
  token)

(define (set-token-value token value)
  (vector-set! token T-VALUE value)
  token)

(define (string-join xs sep)
  (apply string-append
         (cons (car xs)
               (map (lambda (x) (string-append sep x)) (cdr xs)))))

(define (xs-to-string open-b xs sep)
  (string-append open-b
                 (string-join (map toString xs) sep)
                 ")"))

(define (vect-to-string vect)
  (xs-to-string "#(" (vector->list vect) " "))

(define (list-to-string xs)
  (xs-to-string "(" xs "\n"))

(define (toString x)
  (cond ((not x)   "#f")
        ((eq? x #t) "#t")
        ((symbol? x) (symbol->string x))
        ((number? x) (number->string x))
        ((list? x)   (list-to-string x))
        ((vector? x) (vect-to-string x))
        (else        x)))