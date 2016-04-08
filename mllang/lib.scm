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

(define-syntax not-null?
  (syntax-rules ()
    ((_ x) (not (null? x)))))

(define (print . xs)
  (or (and (not-null? xs)
           (display (car xs))
           (newline)
           (apply print (cdr xs)))
      (newline)))

(define-syntax cdns
  (syntax-rules ()
    ((_ x xs) (append xs (list x)))))

(define (filter pred? xs)
  (if (null? xs)
      '()
      (let ((x (car xs)))
        (if (pred? x)
            (cons x (filter pred? (cdr xs)))
            (filter pred? (cdr xs))))))

(define (get-token-tag token)
  (vector-ref token T-TAG))

(define (get-token-coords token)
  (vector-ref token T-COORDS))

(define (get-token-value token)
  (vector-ref token T-VALUE))

(define (get-rule-name rule)
  (vector-ref rule R-NAME))

(define (get-rule-terms rule)
  (vector-ref rule R-TERMS))

(define (get-token-from-simple-rule rule)
  (vector-ref rule R-TERMS))

(define (get-first-term rule)
  (car (get-rule-terms rule)))

(define (get-decl-from-def func-def)
  (get-first-term func-def))

(define (get-token-name-from-decl func-decl)
  (get-token-from-simple-rule (get-first-term func-decl)))

(define (get-args-from-decl func-decl)
  (cdr (get-rule-terms func-decl)))

(define (get-simple-arg-type arg-rule)
  (let ((token (get-token-from-simple-rule arg-rule)))
    (if (eq? 'tag-sym (get-token-tag token))
        (lambda (x) #t)
        (lambda (x) (eqv? x (get-token-value first-term))))))

(define (get-simple-arg-name arg-rule)
  (get-token-value (get-token-from-simple-rule arg-rule)))

(define (get-type-of-arg arg-rule)
  (let* ((terms (get-rule-terms arg-rule))
         (first-term (car terms))
         (first-type (get-rule-name first-term)))
    (cond ((eq? first-type 'simple-argument) (get-simple-arg-type first-term)))))

(define (get-types-of-args func-args)
  (map get-type-of-arg func-args))

(define (get-name-of-arg arg-rule)
  (let* ((terms (get-rule-terms arg-rule))
         (first-term (car terms))
         (first-type (get-rule-name first-term)))
    (cond ((eq? first-type 'simple-argument) (get-simple-arg-name first-term)))))

(define (get-names-of-args func-args)
  (map get-name-of-arg func-args))

(define (get-num-of-args func-args)
  (if (null? func-args)
      (lambda (x) (zero? x))
      (let* ((last (car (reverse func-args)))
             (terms (get-rule-terms last))
             (term (car terms))
             (type (get-rule-name term))
             (num  (length func-args)))
        (if (eq? type 'continious)
            (lambda (x) (>= x (- num 1)))
            (lambda (x) (eq? x num))))))

(define (add-func-in-model model name obj)
  (cons (list name obj) model))

(define (add-type-in-model model name val)
  (let* ((pair (car model))
         (p-name (car pair))
         (p-vals (cadr pair)))
    (if (equal? p-name name)
        (cons (list name (cdns val p-vals)) (cdr model)))))

#|find-in model symbol-func-name '<model>|#
(define (find-in-model func-name model)
  (and (not-null? model)
       ;(print 'find-in-model func-name (not (not (assoc func-name (car model)))) (car model))
       (let ((car-model (car model)))
         (or (assoc func-name car-model)
             (and (find-in-params func-name car-model)
                  (print '!!!in-params!!!)
                  func-name)
             (find-in-model func-name (cdr model))))))

(define (get-args-from-type type)
  (vector-ref type TYPE-ARGS))

(define (get-args-num-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-NUM))

(define (get-args-names-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-NAMES))

(define (x-in-xs x xs)
  (and (not-null? xs)
       (or (equal? x (car xs))
           (x-in-xs x (cdr xs)))))

(define (find-in-params func-name model)
  (and (not-null? model)
       (let* ((type (car model))
              (get-args-names-from-type type))
         (x-in-xs func-name get-args-names-from-type))))

(define (make-arg-type)
  (vector (vector (lambda (x) (zero? x)) (list) (list)) (list) (list)))

(define (make-alist xs)
  (define (helper xs alist)
    (if (null? xs)
        alist
        (helper (cdr xs) (cons (list (car xs) (make-arg-type)) alist))))
  (helper xs '()))

(define (remove-first n xs)
  (if (zero? n)
      xs
      (remove-first (- n 1) (cdr xs))))