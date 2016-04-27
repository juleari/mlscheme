;; examp
(define v '(#(func-def
              (#(func-decl
                 (#(func-name #(tag-sym #(1 1) "fub"))
                  #(argument
                    (#(array-simple
                       (#(open-braket #(tag-lbrk #(1 5) #\[))
                        #(argument (#(simple-argument #(tag-num #(1 6) 0))))
                        #(argument
                          (#(continuous
                             (#(colon #(tag-cln #(1 7) #\:))
                              #(expr
                                (#(func-decl
                                   (#(func-name #(tag-sym #(1 9) "xs"))))))))))
                        #(close-braket #(tag-rbrk #(1 11) #\]))))))))
               #(func-to #(tag-to #(1 13) "<-"))
               #(expr
                 (#(array-simple
                    (#(open-braket #(tag-lbrk #(1 16) #\[))
                     #(argument (#(simple-argument #(tag-num #(1 17) 1))))
                     #(argument
                       (#(continuous
                          (#(colon #(tag-cln #(1 18) #\:))
                           #(expr
                             (#(func-decl
                                (#(func-name #(tag-sym #(1 20) "fub"))
                                 #(argument
                                   (#(simple-argument #(tag-sym #(1 24) "xs"))))))))))))
                     #(close-braket #(tag-rbrk #(1 26) #\]))))))))
            #(func-def
              (#(func-decl
                 (#(func-name #(tag-sym #(2 1) "fub"))
                  #(argument
                    (#(array-simple
                       (#(open-braket #(tag-lbrk #(2 5) #\[))
                        #(argument (#(simple-argument #(tag-sym #(2 6) "x"))))
                        #(argument
                          (#(continuous
                             (#(colon #(tag-cln #(2 8) #\:))
                              #(expr
                                (#(func-decl
                                   (#(func-name #(tag-sym #(2 10) "xs"))))))))))
                        #(close-braket #(tag-rbrk #(2 12) #\]))))))))
               #(func-to #(tag-to #(2 14) "<-"))
               #(expr
                 (#(array-simple
                    (#(open-braket #(tag-lbrk #(2 17) #\[))
                     #(argument (#(simple-argument #(tag-sym #(2 18) "x"))))
                     #(argument
                       (#(continuous
                          (#(colon #(tag-cln #(2 20) #\:))
                           #(expr
                             (#(func-decl
                                (#(func-name #(tag-sym #(2 22) "fub"))
                                 #(argument
                                   (#(simple-argument #(tag-sym #(2 26) "xs"))))))))))))
                     #(close-braket #(tag-rbrk #(2 28) #\]))))))))
            #(func-def
              (#(func-decl
                 (#(func-name #(tag-sym #(3 1) "fub"))
                  #(argument
                    (#(array-simple
                       (#(open-braket #(tag-lbrk #(3 5) #\[))
                        #(close-braket #(tag-rbrk #(3 6) #\]))))))))
               #(func-to #(tag-to #(3 8) "<-"))
               #(expr
                 (#(array-simple
                    (#(open-braket #(tag-lbrk #(3 11) #\[))
                     #(close-braket #(tag-rbrk #(3 12) #\]))))))))))
;; end examp

;; defs
(define TAB 4)
(define TAB1 (- TAB 1))

(define % quotient)
(define // remainder)
(define ** expt)

(define ARGS-CAN-BE-FUNCS #t)
(define ARGS-CANT-BE-FUNCS #f)

(define T-TAG 0)
(define T-COORDS 1)
(define T-VALUE 2)

(define C-LINE 0)
(define C-POSITION 1)

(define A-NAME 0)
(define A-RULES 1)

(define R-NAME 0)
(define R-TOKEN 1)
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
(define-syntax eval-i
  (syntax-rules ()
    ((_ x) (eval x (interaction-environment)))))

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

(define (get-list-inner xs)
  (if (>= (length xs) 2)
      (reverse (cdr (reverse (cdr xs))))
      xs))

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
        `(lambda (x) #t)
        `(lambda (x) (eqv? x ,(get-token-value token))))))

(define (and-fold xs)
  (or (null? xs)
      (and (car xs)
           (and-fold (cdr xs)))))

;; для списков :x с continuous насоздавать (lambda (x) #t) на всю оставшуюся длину :x
(define (get-array-type arr-rule)
  (let* ((arr-terms (get-rule-terms arr-rule))
         (inner     (get-list-inner arr-terms))
         (l-inner   (length inner)))
    `(lambda (:x) (and (list? :x)
                       ,(if (null? inner)
                            `(null? :x)
                            `(and ,(if (eq? 'continuous
                                            (get-rule-name (car (get-rule-terms
                                                                 (car (reverse inner))))))
                                       `(>= (length :x) ,(- l-inner 1))
                                       `(= (length :x) ,l-inner))
                                  (and-fold (map (lambda (:lambda-i :xi)
                                                   ((eval-i :lambda-i) :xi))
                                                 ,(map (lambda (:i)
                                                         (get-type-of-arg :i))
                                                       inner)
                                                 :x))))))))

(define (get-type-of-arg arg-rule)
  (let* ((terms (get-rule-terms arg-rule))
         (first-term (car terms))
         (first-type (get-rule-name first-term)))
    (cond ((eq? first-type 'simple-argument) (get-simple-arg-type first-term))
          ((eq? first-type 'array-simple)    (get-array-type first-term))
          ((eq? first-type 'continuous)      'continuous))))

(define (get-types-of-args func-args)
  (map get-type-of-arg func-args))

(define (get-simple-arg-value arg-rule)
  (get-token-value (get-token-from-simple-rule arg-rule)))

(define (get-num-of-args func-args)
  (if (null? func-args)
      (lambda (x) (zero? x))
      (let* ((last (car (reverse func-args)))
             (terms (get-rule-terms last))
             (term (car terms))
             (type (get-rule-name term))
             (num  (length func-args)))
        (if (eq? type 'continious)
            `(lambda (x) (>= x (- ,num 1)))
            `(lambda (x) (eq? x ,num))))))

(define (add-func-in-model model name obj)
  (cdns (list name obj) model))

(define (add-type-in-model model name val)
  (let* ((pair (car model))
         (p-name (car pair))
         (p-vals (cadr pair)))
    ;(print 'add-type-in-model pair val model)
    (if (equal? p-name name)
        (cons (cdns val pair) (cdr model)))))

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
  (eval-i (vector-ref (get-args-from-type type) TYPE-ARGS-NUM)))

(define (get-args-names-from-type type)
  (eval-i (vector-ref (get-args-from-type type) TYPE-ARGS-NAMES)))

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
;; new lib
(define-syntax for
  (syntax-rules (in as)
    ((_ (item ...) in (items ...) proc ...)
     (for-each (lambda (item ...) (begin proc ...)) items ...))
    ((_ item in items proc ...)
     (for-each (lambda (item) (begin proc ...)) items))
    ((_ (items ...) as (item ...) . procs) (for (item ...) in (items ...) . procs))
    ((_ items as item . procs) (for item in items . procs))))

(define (compare-args arg1 arg2)
  (print 'compare-args arg1 arg2))

(define (find-similar-in-args args)
  (print 'find-similar-in-args args)
  (let* ((len (length args))
         (vec (list->vector args))
         (similar '())
         (ind-source 0)
         (ind-target 1)
         (get-arg (lambda (ind) (vector-ref vec ind))))
    (letrec ((helper (lambda (ind-source ind-target similar)
                       (or (and (eq? ind-source len) similar)
                           (and (>= ind-target len)
                                (helper (+ 1 ind-source) (+ 2 ind-target) similar))
                           (helper ind-source (+ 1 ind-target) (cons (compare-args (get-arg ind-target)
                                                                                   (get-arg ind-source))
                                                                     similar))))))
      (helper 0 0 '()))))
;; end lib

#|
<model> = ((<name> (#(<#args> '<model> '<expr>)
                    #(<#args> '<model> '<expr>))))
<#args> = #(<lambda:num-of-args> '<lambda:type-of-arg> '<name>)
|#
(define semantic
  (let ((errors '())
        (f-name ':))
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
      
      (define (semantic-get-new-var-name)
        (string->symbol (string-append (symbol->string f-name) "_")))
      
      (define (semantic-set-new-var-name)
        (set! f-name (semantic-get-new-var-name))
        f-name)
      
      (define (get-simple-arg-name arg-rule)
        (let ((val (get-simple-arg-value arg-rule)))
          (if (number? val)
              (semantic-set-new-var-name)
              val)))
      
      (define (get-name-of-arg arg-rule)
        (let* ((terms (get-rule-terms arg-rule))
               (first-term (car terms))
               (first-type (get-rule-name first-term)))
          (cond ((eq? first-type 'simple-argument) (get-simple-arg-name first-term))
                ((eq? first-type 'array-simple)    (semantic-set-new-var-name)))))
      
      (define (get-names-of-args func-args)
        (map get-name-of-arg func-args))
      
      (define (semantic-func-body func-def-terms names-of-args model)
        (let* ((b-list (cddr func-def-terms))
               (body (semantic-program b-list (cons (make-alist names-of-args) model) '())))
          (cons (remove-first (length names-of-args) (car body)) (cdr body))))
      
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
      ;; надо связывать индексы в списке с функциями сравнения
      ;; для тех элементов, которые являются символами нужно хранить имена... ЖИЗНЬ БОЛЬ
      (define (semantic-func-call name-token args func-types)
        ;(print 'semantic-func-call args)
        (let* ((name (get-token-value name-token))
               (arg-len (length args))
               (correct-types (filter (lambda (type)
                                        ((get-args-num-from-type type) arg-len))
                                      func-types)))
          (or (and (null? correct-types)
                   (add-error ERROR_NUM_OF_ARGS name-token))
              (and (zero? arg-len)
                   name)
              (cons name (map (lambda (arg)
                                (get-simple-arg-value (car (get-rule-terms arg))))
                              args)))))
      
      ;; проверять, что текущая функция используется
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
      
      (define (get-continuous-expr cont-rule model)
        (let* ((terms   (get-rule-terms cont-rule))
               (expr    (cadr terms))
               (e-terms (get-rule-terms expr)))
          (semantic-expr e-terms model)))
      
      ;; нужно проверять, что simple-argument в model
      (define (argument-to-expr argument model)
        (let* ((terms       (get-rule-terms argument))
               (f-term      (car terms))
               (f-term-name (get-rule-name f-term)))
          (cond ((eq? f-term-name 'simple-argument) (get-simple-arg-name f-term))
                ((eq? f-term-name 'continuous)      (get-continuous-expr f-term model)))))
      
      (define (semantic-expr-arr arr-terms model)
        (let ((inner (get-list-inner arr-terms)))
          (map (lambda (x) (argument-to-expr x model)) inner)))
      
      (define (semantic-expr-elem elem model)
        (let ((type (get-rule-name elem)))
          (cond ((eq? type 'func-decl)    (semantic-var (get-rule-terms elem) model))
                ((eq? type 'array-simple) (semantic-expr-arr (get-rule-terms elem) model))
                (else                     (get-token-value elem)))))
      
      (define (semantic-expr terms model)
        (map (lambda (x) (semantic-expr-elem x model)) terms))
      
      (define (semantic-program ast model exprs)
        ;(print 'semantic-program ast)
        (if (null? ast)
            (list (car model) (reverse exprs))
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
