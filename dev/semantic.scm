;; examp
(define v '(#(func-def
              (#(func-decl
                 (#(func-name #(tag-sym #(2 1) "count"))
                  #(argument (#(simple-argument #(tag-sym #(2 7) "x"))))
                  #(argument
                    (#(array-simple
                       (#(open-braket #(tag-lbrk #(2 9) #\[))
                        #(close-braket #(tag-rbrk #(2 10) #\]))))))))
               #(func-to #(tag-to #(2 20) "<-"))
               #(expr (#(tag-num #(2 23) 0)))))
            #(func-def
              (#(func-decl
                 (#(func-name #(tag-sym #(3 1) "count"))
                  #(argument (#(simple-argument #(tag-sym #(3 7) "x"))))
                  #(argument
                    (#(array-simple
                       (#(open-braket #(tag-lbrk #(3 9) #\[))
                        #(argument
                          (#(simple-argument #(tag-sym #(3 11) "x"))))
                        #(argument
                          (#(continuous
                             (#(colon #(tag-cln #(3 13) #\:))
                              #(continuous-list
                                #(tag-sym #(3 15) "xs"))))))
                        #(close-braket #(tag-rbrk #(3 18) #\]))))))))
               #(func-to #(tag-to #(3 20) "<-"))
               #(expr
                 (#(tag-num #(3 23) 1)
                  #(func-decl
                    (#(func-name #(tag-sym #(3 27) "count"))
                     #(argument
                       (#(simple-argument #(tag-sym #(3 33) "x"))))
                     #(argument
                       (#(simple-argument #(tag-sym #(3 35) "xs"))))))
                  #(tag-pls #(3 25) "+")))))
            #(func-def
              (#(func-decl
                 (#(func-name #(tag-sym #(4 1) "count"))
                  #(argument (#(simple-argument #(tag-sym #(4 7) "x"))))
                  #(argument
                    (#(array-simple
                       (#(open-braket #(tag-lbrk #(4 9) #\[))
                        #(argument
                          (#(simple-argument #(tag-sym #(4 11) "y"))))
                        #(argument
                          (#(continuous
                             (#(colon #(tag-cln #(4 13) #\:))
                              #(continuous-list
                                #(tag-sym #(4 15) "xs"))))))
                        #(close-braket #(tag-rbrk #(4 18) #\]))))))))
               #(func-to #(tag-to #(4 20) "<-"))
               #(expr
                 (#(func-decl
                    (#(func-name #(tag-sym #(4 23) "count"))
                     #(argument
                       (#(simple-argument #(tag-sym #(4 29) "x"))))
                     #(argument
                       (#(simple-argument #(tag-sym #(4 31) "xs"))))))))))
            #(expr
              (#(func-decl
                 (#(func-name #(tag-sym #(7 1) "count"))
                  #(argument (#(simple-argument #(tag-sym #(7 7) "'a"))))
                  #(argument
                    (#(array-simple
                       (#(open-braket #(tag-lbrk #(7 10) #\[))
                        #(argument
                          (#(simple-argument #(tag-sym #(7 11) "'a"))))
                        #(argument
                          (#(simple-argument #(tag-sym #(7 14) "'b"))))
                        #(argument
                          (#(simple-argument #(tag-sym #(7 17) "'c"))))
                        #(argument
                          (#(simple-argument #(tag-sym #(7 20) "'a"))))
                        #(close-braket #(tag-rbrk #(7 22) #\]))))))))))))
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

(define F-ARGS 0)
(define F-DEFS 1)
(define F-BODY 2)

(define TYPE-ARGS 0)
(define TYPE-ARGS-NUM 0)
(define TYPE-ARGS-CHECK 1)
(define TYPE-ARGS-NAMES 2)
(define TYPE-ARGS-SIMILAR 3)
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

(define (get-array-args-rules l-lambda inner x)
  `(and-fold (cons ,l-lambda
                   (map (lambda (:lambda-i :xi)
                          ((eval-i :lambda-i) :xi))
                        ',(map (lambda (:i)
                                 (get-type-of-arg :i))
                               inner)
                        ,x))))

;; TRY!!!
(define (get-array-type arr-rule)
  (let* ((arr-terms (get-rule-terms arr-rule))
         (inner     (get-list-inner arr-terms))
         (l-inner   (length inner)))
    `(lambda (:x) (and (list? :x)
                       ,(if (null? inner)
                            `(null? :x)
                            (if (eq? 'continuous
                                     (get-rule-name (car (get-rule-terms (car (reverse inner))))))
                                (get-array-args-rules `(>= (length :x) ,(- l-inner 1))
                                                      (remove-last inner)
                                                      `(give-first :x ,(- l-inner 1)))
                                (get-array-args-rules `(= (length :x) ,l-inner)
                                                      inner
                                                      `:x)))))))

(define (get-type-of-arg arg-rule)
  (let* ((terms (get-rule-terms arg-rule))
         (first-term (car terms))
         (first-type (get-rule-name first-term)))
    (cond ((eq? first-type 'simple-argument) (get-simple-arg-type first-term))
          ((eq? first-type 'array-simple)    (get-array-type first-term))
          ((eq? first-type 'continuous)      `(lambda :x #t)))))

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
            `(lambda (:x) (>= :x (- ,num 1)))
            `(lambda (:x) (= :x ,num))))))

(define (add-func-in-model model name obj)
  (cdns (list name obj) model))

(define (add-type-in-model model name val)
  (map (lambda (model-func)
         (let ((f-name (car model-func)))
           (if (eq? name f-name)
               (cdns val model-func)
               model-func)))
       model))

#|find-in model symbol-func-name '<model>|#
(define (find-in-model func-name model)
  (and (not-null? model)
       ;(let ((car-model (car model)))
       (let ((car-model model))
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
  (vector (vector `(lambda (:x) (zero? :x)) (list) (list)) (list) (list)))

(define (make-alist xs)
  (define (helper xs alist)
    (if (null? xs)
        alist
        (helper (cdr xs) (cons (list (car xs) (make-arg-type)) alist))))
  (helper xs '()))

(define (remove-first n xs)
  ;(print 'remove-first n xs)
  (if (zero? n)
      xs
      (remove-first (- n 1)(cdr xs))))

;; new lib
(define (remove-last xs . ns)
  (let ((rxs (reverse xs))
        (n   (if (null? ns) 1 (car ns))))
    (reverse (remove-first n rxs))))

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

(define s-name
  (let* ((:s-name ':s))
    (lambda ()
      (set! :s-name
            (string->symbol (string-append (symbol->string :s-name) "_")))
      :s-name)))

(define (get-inner-length xs)
  (if (list? xs)
      (apply + (map get-inner-length xs))
      1))

(define (multi-list->vector xs)
  (define (helper xs)
    (if (list? xs)
        (apply append (map helper xs))
        (list xs)))
  (list->vector (helper xs)))

(define (vector->multi-list v xs)
  (define (helper cur-ml xs cur-ind)
    (if (null? xs)
        cur-ml
        (let ((cur-x (car xs)))
          (if (list? cur-x)
              (helper (append cur-ml
                              (list (helper '()
                                            cur-x
                                            cur-ind)))
                      (cdr xs)
                      (+ (get-inner-length cur-x) cur-ind))
              (helper (append cur-ml (list (vector-ref v cur-ind)))
                      (cdr xs)
                      (+ 1 cur-ind))))))
  (helper '() xs 0))

(define-syntax find-similar-in-args
  (syntax-rules ()
    ((_ args) (let* ((len (get-inner-length args))
                     (vec (multi-list->vector args))
                     (similar '())
                     (ind-source 0)
                     (ind-target 1)
                     (get-arg (lambda (ind) (vector-ref vec ind)))
                     (compare-args (lambda (ind1 ind2)
                                     (if (equal? (get-arg ind1)
                                                 (get-arg ind2))
                                         (begin (vector-set! vec ind2 (s-name))
                                                (list (list ind1 ind2)))
                                         (list)))))
                (letrec ((helper (lambda (ind-source ind-target similar)
                                   (or (and (eq? ind-source len) similar)
                                       (and (>= ind-target len)
                                            (helper (+ 1 ind-source) (+ 2 ind-source) similar))
                                       (helper ind-source
                                               (+ 1 ind-target)
                                               (append (compare-args ind-source
                                                                     ind-target)
                                                       similar))))))
                  (let ((res (helper ind-source ind-target '())))
                    (set! args (vector->multi-list vec args))
                    res))))))

(define-syntax make-similar-args-checks
  (syntax-rules ()
    ((_ args) (let ((:similar-pairs (find-similar-in-args args)))
                (if (not-null? :similar-pairs)
                    `(lambda :args
                       (let ((:v-args (multi-list->vector :args)))
                         (and-fold-s ,(map (lambda (:similar-pair)
                                             `(equal? (vector-ref :v-args ,(car :similar-pair))
                                                      (vector-ref :v-args ,(cadr :similar-pair))))
                                           :similar-pairs))))
                    `(lambda :args #t))))))

(define (get-func-body f-inner)
  (vector-ref f-inner F-BODY))

(define (get-func-bodies model-func)
  (let ((f-inners (cdr model-func)))
    (map get-func-body f-inners)))

(define (make-rec-model func-name args-vector)
  (list func-name (vector args-vector (list) (list))))

#| Возвращает первые n элементов списка xs
 | @param {list} xs
 | @param {int} n
 | @returns {list}
 |#
(define (give-first xs n)
  (if (or (zero? n)
          (null? xs))
      '()
      (cons (car xs) (- n 1))))
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

      (define (get-array-name arr-rule)
        (let* ((terms (get-rule-terms arr-rule))
               (inner (get-list-inner terms)))
          (map get-name-of-arg inner)))

      ;; проверить, что там должны быть expr
      (define (get-continuous-name cont-rule)
        (let* ((terms (get-rule-terms cont-rule))
               ; terms : (list 'colon first-term)
               (first-term (cadr terms)))
          (list 'continuous (get-simple-arg-name first-term))))

      (define (get-name-of-arg arg-rule)
        (let* ((terms (get-rule-terms arg-rule))
               (first-term (car terms))
               (first-type (get-rule-name first-term)))
          ;(print 'get-name-of-arg first-term)
          (cond ((eq? first-type 'simple-argument) (get-simple-arg-name first-term))
                ((eq? first-type 'array-simple)    (get-array-name first-term))
                ((eq? first-type 'continuous)      (get-continuous-name first-term)))))

      (define (get-names-of-args func-args)
        (map get-name-of-arg func-args))

      (define (semantic-func-body func-def-terms names-of-args model)
        (let* ((b-list (cddr func-def-terms))
               (l-args (length names-of-args))
               (l-model (length model))
               (l-this (+ l-args l-model))
               (body (semantic-program b-list (append (make-alist names-of-args) model) '())))
          ;(print 'semantic-func-body (length (car body)) )
          (list (reverse (remove-last (car body) l-this)) (cdr body))))

      (define (semantic-func-def func-def-terms model)
        ;(print 'semantic-func-def)
        (let* ((func-decl (car func-def-terms))

               (func-name-token (get-token-name-from-decl func-decl))
               (func-name (get-token-value func-name-token))
               (in-model (find-in-model func-name model))

               (func-args (get-args-from-decl func-decl))
               (num-of-args (get-num-of-args func-args))
               (types-of-args (get-types-of-args func-args))
               (names-of-args (get-names-of-args func-args))
               (similar-args (make-similar-args-checks names-of-args))
               (args-vector (vector num-of-args types-of-args names-of-args similar-args))

               (this-model (cons (make-rec-model func-name args-vector) model))
               (body-list (semantic-func-body func-def-terms
                                              names-of-args
                                              this-model))
               (f-defs (car body-list))
               (f-exprs (cadr body-list))

               (func-val (vector args-vector f-defs f-exprs))
               (add-list (list model func-name func-val)))
          (apply (if in-model
                     add-type-in-model
                     add-func-in-model)
                 add-list)))

      ;; проверять, что текущая функция используется
      ;; сначала просто проходим по определениям,
      ;; когда встречаем expr сохраняем его в список выражений для текущей области видимости
      ;; после того как все определения сформировались идём по выражениям и проверяем, что всё ок
      (define (semantic-func-call name-token args func-types)
        (let* ((name (get-token-value name-token))
               (arg-len (length args))
               (correct-types (filter (lambda (type)
                                        ((eval-i (get-args-num-from-type type)) arg-len))
                                      func-types)))
          (or (and (null? correct-types)
                   (add-error ERROR_NUM_OF_ARGS name-token))
              (and (zero? arg-len)
                   name)
              (cons name (map (lambda (arg)
                                ;(print 'semantic-func-call (car (get-rule-terms arg)))
                                (get-arg-value (car (get-rule-terms arg))
                                               (list (list name-token func-types))))
                              args)))))

      ;; надо связывать индексы в списке с функциями сравнения
      ;; для тех элементов, которые являются символами нужно хранить имена... ЖИЗНЬ БОЛЬ
      (define (semantic-var func-decl-terms model)
        (let* ((s-rule (car func-decl-terms))
               (name-token (get-token-from-simple-rule s-rule))
               (name (get-token-value name-token))
               (args (cdr func-decl-terms))
               (in-model (find-in-model name model)))
          ;(print 'semantic-var name in-model model)
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
          (cond ((eq? f-term-name 'simple-argument) (get-simple-arg-value f-term))
                ((eq? f-term-name 'continuous)      (get-continuous-expr f-term model)))))

      ;; возможно, нужно переделать continuous
      (define (semantic-expr-arr arr-terms model)
        (let ((inner (get-list-inner arr-terms)))
          (if (null? inner)
              `'()
              (let* ((last (car (reverse inner)))
                     (list-elems (remove-last inner))
                     (terms       (get-rule-terms last))
                     (f-term      (car terms))
                     (f-term-name (get-rule-name f-term)))
                (if (eq? f-term-name 'continuous)
                    `(append-s ,(map (lambda (x) (argument-to-expr x model)) list-elems)
                               ,(get-continuous-expr f-term model))
                    `',(map (lambda (x) (argument-to-expr x model)) inner))))))

      (define (get-arg-value arg-rule model)
        ;(print 'get-arg-value arg-rule)
        (let ((name (get-rule-name arg-rule)))
          ;(print 'get-arg-value--name name)
          (cond ((eq? name 'simple-argument) (get-simple-arg-value arg-rule))
                ((eq? name 'array-simple)    (semantic-expr-arr (get-rule-terms arg-rule) model)))))

      (define (semantic-expr-elem elem model)
        (let ((type (get-rule-name elem)))
          ;(print 'semantic-expr-elem type model)
          (cond ((eq? type 'func-decl)    (semantic-var (get-rule-terms elem) model))
                ((eq? type 'array-simple) (semantic-expr-arr (get-rule-terms elem) model))
                (else                     (get-token-value elem)))))

      (define (semantic-expr terms model)
        ;(print 'semantic-expr terms)
        (map (lambda (x) (semantic-expr-elem x model)) terms))

      ;; need to make semantic-model-exprs
      (define (semantic-model-exprs model)
        ;(print 'semantic-model-exprs )
        ;(let* ((f-bodies (map get-func-bodies model))))
        model)

      ;; parse exprs after defs
      (define (semantic-program ast model exprs)
        ;(print 'semantic-program ast)
        (if (null? ast)
            (list model exprs)
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
                                       (append exprs (semantic-expr terms model))))))))

      (let ((m (semantic-program ast '() '())))
        (and (print-errors) m)))))
