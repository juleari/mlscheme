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

(define (get-true-expr)
  #(expr (#(tag-true #(0 0) "#t"))))

(define (get-token)
  (if (null? tokens)
      #(tag-end #(2 27) "eof")
      (let ((token (car tokens)))
        (set! tokens (cdr tokens))
        token)))

(define (get-rule-list ast)
  (vector-ref ast A-RULES))

(define (get-simple-rule-token rule)
  (vector-ref rule R-TOKEN))

(define (get-simple-start-pos rule)
  (let ((func-name-token (get-simple-rule-token rule)))
    (get-token-pos func-name-token)))

(define (get-expr-start-pos func-decl)
  (let* ((rule-list        (get-rule-list func-decl))
         (func-name-rule   (car rule-list)))
    (get-simple-start-pos func-name-rule)))