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

(define TYPE-ARGS 0)
(define TYPE-ARGS-NUM 0)
(define TYPE-ARGS-CHECK 1)
(define TYPE-ARGS-NAMES 2)
(define TYPE-DEFS 1)
(define TYPE-EXPRS 2)
(define TYPE-ARGS-NAMES 2)

(define ERROR_NUM_OF_ARGS "the expected number of arguments does not match the given number")
(define ERROR_UNDEFINED_VARIABLE "cannot reference undefined identifier")
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
;; end lib

#|
<model> = ((<name> (#(<#args> '<model> '<expr>)
                    #(<#args> '<model> '<expr>))))
<#args> = #(<lambda:num-of-args> '<lambda:type-of-arg> '<name>)
|#
(define semantic
  (let ((errors '()))
    (lambda (ast)
      
      (define (add-error error-type token)
        (set! errors
              (cons (vector 'error: token '- error-type)
                    errors))
        '())
      
      (define (add-error-rule rule token)
        (set! errors
              (cons (vector 'error: (get-token-coords token) '- 'expected `,rule)
                    errors))
        '())
      
      (define (print-errors)
        (if (null? errors)
            (display "SEMANTIC OK!")
            (begin (display "SEMANTIC ERRORS:")
                   (newline)
                   (apply print (reverse errors))))
        (newline)
        (newline))
      
      (define (semantic-func-body func-def-terms names-of-args model)
        (let ((b-list (cddr func-def-terms)))
          (semantic-program b-list (cons (make-alist names-of-args) model) '())))
      
      (define (semantic-func-def func-def-terms model)
        (let* ((func-decl (car func-def-terms))
               
               (func-name-token (get-token-name-from-decl func-decl))
               (func-name (get-token-value func-name-token))
               (in-model (find-in-model func-name model))
               (this-model (car model))
               
               (func-args (get-args-from-decl func-decl))
               (num-of-args (get-num-of-args func-args))
               (types-of-args (get-types-of-args func-args))
               (names-of-args (get-names-of-args func-args))
               (args-vector (vector num-of-args types-of-args names-of-args))
               
               (body-list (semantic-func-body func-def-terms names-of-args model))
               (f-defs (car body-list))
               (f-exprs (cadr body-list))
               
               (func-val (vector args-vector f-defs f-exprs))
               (add-list (list this-model func-name func-val)))
          (cons (apply (if in-model
                           add-type-in-model
                           add-func-in-model)
                       add-list)
                (cdr model))))
      
      (define (semantic-func-call name-token args func-types)
        (let* ((name (get-token-value name-token))
               (arg-len (length args))
               (correct-types (filter (lambda (type)
                                        ((get-args-num-from-type type) arg-len))
                                      func-types)))
          (or (and (null? correct-types)
                   (add-error ERROR_NUM_OF_ARGS name-token))
              (and (zero? arg-len)
                   name)
              (cons name (map get-simple-arg-name args)))))
      
      (define (semantic-var func-decl-terms model)
        (let* ((s-rule (car func-decl-terms))
               (name-token (get-token-from-simple-rule s-rule))
               (name (get-token-value name-token))
               (args (cdr func-decl-terms))
               (in-model (find-in-model name model)))
          ;(print name in-model model)
          (or (and in-model
                   (semantic-func-call name-token args (cdr in-model)))
              (and (add-error ERROR_UNDEFINED_VARIABLE name-token)
                   func-decl-terms))))
      
      (define (semantic-expr-elem elem model)
        (let ((type (get-rule-name elem)))
          (if (eq? type 'func-decl)
              (semantic-var (get-rule-terms elem) model)
              (get-token-value elem))))
      
      (define (semantic-expr terms model)
        (map (lambda (x) (semantic-expr-elem x model)) terms))
      
      (define (semantic-program ast model exprs)
        (if (null? ast)
            (list (reverse (car model)) (reverse exprs))
            (let* ((rule (car ast))
                   (name (get-rule-name rule))
                   (terms (get-rule-terms rule)))
              (cond ((eq? name 'func-def)
                     (semantic-program (cdr ast)
                                       (semantic-func-def terms model)
                                       exprs))
                    ((eq? name 'expr)
                     (semantic-program (cdr ast)
                                       model
                                       (cons (semantic-expr terms model) exprs)))))))
      
      (let ((m (semantic-program ast '(()) '())))
        (and (print-errors) m)))))