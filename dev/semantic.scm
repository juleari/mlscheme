;; examp
(define v '(#(func-def
              (#(func-decl
                 (#(func-name #(tag-sym #(1 1) "day-of-week"))
                  #(argument
                    (#(simple-argument #(tag-sym #(1 13) "day"))))
                  #(argument
                    (#(simple-argument #(tag-sym #(1 17) "month"))))
                  #(argument
                    (#(simple-argument #(tag-sym #(1 23) "year"))))))
               #(func-to #(tag-to #(1 28) "<-"))
               #(func-def
                 (#(func-decl (#(func-name #(tag-sym #(2 5) "a"))))
                  #(func-to #(tag-to #(2 7) "<-"))
                  #(expr
                    (#(tag-num #(2 11) 14)
                     #(func-decl
                       (#(func-name #(tag-sym #(2 14) "month"))))
                     #(tag-mns #(2 12) "-")
                     #(tag-num #(2 24) 12)
                     #(tag-div #(2 21) "//")))))
               #(func-def
                 (#(func-decl (#(func-name #(tag-sym #(3 5) "y"))))
                  #(func-to #(tag-to #(3 7) "<-"))
                  #(expr
                    (#(func-decl
                       (#(func-name #(tag-sym #(3 10) "year"))))
                     #(func-decl
                       (#(func-name #(tag-sym #(3 17) "a"))))
                     #(tag-mns #(3 15) "-")))))
               #(func-def
                 (#(func-decl (#(func-name #(tag-sym #(4 5) "m"))))
                  #(func-to #(tag-to #(4 7) "<-"))
                  #(expr
                    (#(func-decl
                       (#(func-name #(tag-sym #(4 10) "month"))))
                     #(func-decl
                       (#(func-name #(tag-sym #(4 19) "a"))))
                     #(tag-num #(4 23) 12)
                     #(tag-mul #(4 21) "*")
                     #(tag-pls #(4 16) "+")
                     #(tag-num #(4 29) 2)
                     #(tag-mns #(4 27) "-")))))
               #(expr
                 (#(tag-num #(6 6) 7000)
                  #(func-decl (#(func-name #(tag-sym #(6 9) "day"))))
                  #(tag-pls #(6 7) "+")
                  #(func-decl (#(func-name #(tag-sym #(6 15) "y"))))
                  #(tag-pls #(6 13) "+")
                  #(func-decl (#(func-name #(tag-sym #(6 20) "y"))))
                  #(tag-num #(6 25) 4)
                  #(tag-div #(6 22) "//")
                  #(tag-pls #(6 17) "+")
                  #(func-decl (#(func-name #(tag-sym #(6 31) "y"))))
                  #(tag-num #(6 36) 400)
                  #(tag-div #(6 33) "//")
                  #(tag-pls #(6 28) "+")
                  #(tag-num #(6 44) 31)
                  #(func-decl (#(func-name #(tag-sym #(6 47) "m"))))
                  #(tag-mul #(6 45) "*")
                  #(tag-num #(6 52) 12)
                  #(tag-div #(6 49) "//")
                  #(tag-pls #(6 41) "+")
                  #(func-decl (#(func-name #(tag-sym #(7 12) "y"))))
                  #(tag-num #(7 17) 100)
                  #(tag-div #(7 14) "//")
                  #(tag-mns #(7 9) "-")
                  #(tag-num #(7 25) 7)
                  #(tag-mod #(7 23) "%")))))))
;; end examp

;; defs
(define T-TAG 0)
(define T-COORDS 1)
(define T-VALUE 2)

(define R-NAME 0)
(define R-TERMS 1)
;; end defs

;; lib
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
    (cond ((eq? first-type 'simple-argument) (string->symbol (get-simple-arg-name first-term))))))

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

(define (get-body-list func-def model)
  (let ((b-list (cddr (get-rule-terms func-def))))
    (semantic b-list (cons '() model) '())))

(define (add-func-in-model model name obj)
  (cons (list name obj) model))

(define (add-type-in-model model name val)
  (let* ((pair (car model))
         (p-name (car pair))
         (p-vals (cadr pair)))
    (if (equal? p-name name)
        (cons (list name (cdns val p-vals)) (cdr model)))))

(define (get-from-model func-name model)
  (and (not-null? model)
       (or (assoc func-name (car model))
           (get-from-model func-name (cdr model)))))
;; end lib

(define (semantic-func-def func-def model)
  (let* ((func-decl (get-decl-from-def func-def))
         
         (func-name-token (get-token-name-from-decl func-decl))
         (func-name (string->symbol (get-token-value func-name-token)))
         (in-model (get-from-model func-name model))
         (this-model (car model))
         
         (func-args (get-args-from-decl func-decl))
         (num-of-args (get-num-of-args func-args))
         (types-of-args (get-types-of-args func-args))
         (names-of-args (get-names-of-args func-args))
         (args-vector (vector num-of-args types-of-args names-of-args))
         
         (body-list (get-body-list func-def model))
         (f-defs (car body-list))
         (f-exprs (cadr body-list))
         
         (func-val (vector args-vector f-defs f-exprs))
         (add-list (list this-model func-name func-val)))
    (cons (apply (if in-model
                     add-type-in-model
                     add-func-in-model)
                 add-list)
          (cdr model))))

(define (semantic-expr rule model)
  rule)

(define (semantic ast model exprs)
  (print "semantic" ast model)
  (if (null? ast)
      (list (reverse (car model)) (reverse exprs))
;      model
      (let* ((rule (car ast))
             (name (get-rule-name rule)))
        (cond ((eq? name 'func-def) (semantic (cdr ast) (semantic-func-def rule model) exprs))
              ((eq? name 'expr) (semantic (cdr ast) model (cons (semantic-expr rule model) exprs)))))))