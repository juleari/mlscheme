;; examp
(define v '(#(func-def
              (#(func-decl
                 (#(func-name #(tag-sym #(1 1) "replace"))
                  #(argument (#(simple-argument #(tag-sym #(1 9) "pred?"))))
                  #(argument (#(simple-argument #(tag-sym #(1 15) "proc"))))
                  #(argument
                    (#(array-simple
                       (#(open-braket #(tag-lbrk #(1 20) #\[))
                        #(close-braket #(tag-rbrk #(1 21) #\]))))))))
               #(func-to #(tag-to #(1 31) "<-"))
               #(expr
      (#(array-simple
         (#(open-braket #(tag-lbrk #(1 34) #\[))
          #(close-braket #(tag-rbrk #(1 35) #\]))))))))
            #(func-def
              (#(func-decl
                 (#(func-name #(tag-sym #(2 1) "replace"))
                  #(argument (#(simple-argument #(tag-sym #(2 9) "pred?"))))
                  #(argument (#(simple-argument #(tag-sym #(2 15) "proc"))))
                  #(argument
                    (#(array-simple
                       (#(open-braket #(tag-lbrk #(2 20) #\[))
                        #(argument (#(simple-argument #(tag-sym #(2 22) "x"))))
                        #(argument
                          (#(continuous
                  (#(colon #(tag-cln #(2 24) #\:))
                   #(continuous-list #(tag-sym #(2 26) "xs"))))))
                        #(close-braket #(tag-rbrk #(2 29) #\]))))))))
               #(func-to #(tag-to #(2 31) "<-"))
               #(expr
                 (#(if-expression
                    (#(if-word #(tag-kw #(3 5) "if"))
          ((#(if-cond #(tag-bor #(3 8) "|"))
            #(expr
              (#(func-decl
                 (#(func-name #(tag-sym #(3 10) "pred?"))
                  #(argument (#(simple-argument #(tag-sym #(3 16) "x"))))))))
            #(then #(tag-from #(3 18) "->"))
            #(expr
              (#(array-simple
                 (#(open-braket #(tag-lbrk #(3 21) #\[))
                  #(argument
                    (#(func-decl
                       (#(func-name #(tag-sym #(3 23) "proc"))
                        #(argument
                          (#(apply
                             (#(apply-dot #(tag-dot #(3 27) #\.))
                              #(argument
                                (#(array-simple
                                   (#(open-braket #(tag-lbrk #(3 28) #\[))
                                    #(argument
                                      (#(simple-argument #(tag-sym #(3 29) "x"))))
                                    #(close-braket #(tag-rbrk #(3 30) #\]))))))))))))))
                  #(argument
                    (#(continuous
                       (#(colon #(tag-cln #(3 32) #\:))
                        #(expr
                          (#(func-decl
                             (#(func-name #(tag-sym #(3 34) "replace"))
                              #(argument (#(simple-argument #(tag-sym #(3 42) "pred?"))))
                              #(argument (#(simple-argument #(tag-sym #(3 48) "proc"))))
                              #(argument
                                (#(simple-argument #(tag-sym #(3 53) "xs"))))))))))))
                  #(close-braket #(tag-rbrk #(3 56) #\])))))))
           (#(if-cond #(tag-bor #(4 8) "|"))
            #(expr (#(tag-true #(0 0) "#t")))
            #(then #(tag-from #(4 18) "->"))
            #(expr
              (#(array-simple
                 (#(open-braket #(tag-lbrk #(4 21) #\[))
                  #(argument (#(simple-argument #(tag-sym #(4 23) "x"))))
                  #(argument
                    (#(continuous
                       (#(colon #(tag-cln #(4 25) #\:))
                        #(expr
                          (#(func-decl
                             (#(func-name #(tag-sym #(4 27) "replace"))
                              #(argument (#(simple-argument #(tag-sym #(4 35) "pred?"))))
                              #(argument (#(simple-argument #(tag-sym #(4 41) "proc"))))
                              #(argument
                                (#(simple-argument #(tag-sym #(4 46) "xs"))))))))))))
                  #(close-braket #(tag-rbrk #(4 49) #\]))))))))))))))
            #(expr
              (#(func-decl
                 (#(func-name #(tag-sym #(6 1) "replace"))
                  #(argument (#(simple-argument #(tag-kw #(6 9) "zero?"))))
       #(argument
         (#(expr
            (#(lambda-func
               (#(lambda-func-decl
                  (#(func-name #(tag-lmbd #(7 9) "\\"))
                   #(argument (#(simple-argument #(tag-sym #(7 11) "x"))))))
                #(func-from #(tag-from #(7 13) "->"))
                #(expr
                  (#(func-decl (#(func-name #(tag-sym #(7 16) "x"))))
                   #(tag-num #(7 20) 1)
                   #(tag-pls #(7 18) "+")))))))))
       #(argument
         (#(array-simple
            (#(open-braket #(tag-lbrk #(8 9) #\[))
             #(argument (#(simple-argument #(tag-num #(8 11) 0))))
             #(argument (#(simple-argument #(tag-num #(8 13) 1))))
             #(argument (#(simple-argument #(tag-num #(8 15) 2))))
             #(argument (#(simple-argument #(tag-num #(8 17) 3))))
             #(argument (#(simple-argument #(tag-num #(8 19) 0))))
             #(close-braket #(tag-rbrk #(8 21) #\]))))))))))
            #(expr
              (#(func-decl
                 (#(func-name #(tag-sym #(10 1) "replace"))
                  #(argument (#(simple-argument #(tag-sym #(10 9) "odd?"))))
                  #(argument
                    (#(expr
                       (#(lambda-func
                          (#(lambda-func-decl
                             (#(func-name #(tag-lmbd #(11 9) "\\"))
                              #(argument (#(simple-argument #(tag-sym #(11 11) "x"))))))
                           #(func-from #(tag-from #(11 13) "->"))
                           #(expr
                             (#(func-decl (#(func-name #(tag-sym #(11 16) "x"))))
                              #(tag-num #(11 20) 2)
                              #(tag-mul #(11 18) "*")))))))))
                  #(argument
                    (#(array-simple
                       (#(open-braket #(tag-lbrk #(12 9) #\[))
                        #(argument (#(simple-argument #(tag-num #(12 11) 1))))
                        #(argument (#(simple-argument #(tag-num #(12 13) 2))))
                        #(argument (#(simple-argument #(tag-num #(12 15) 3))))
                        #(argument (#(simple-argument #(tag-num #(12 17) 4))))
                        #(argument (#(simple-argument #(tag-num #(12 19) 5))))
                        #(argument (#(simple-argument #(tag-num #(12 21) 6))))
                        #(close-braket #(tag-rbrk #(12 23) #\]))))))))))
            #(expr
              (#(func-decl
                 (#(func-name #(tag-sym #(14 1) "replace"))
                  #(argument
                    (#(expr
                       (#(lambda-func
                          (#(lambda-func-decl
                             (#(func-name #(tag-lmbd #(14 9) "\\"))
                              #(argument (#(simple-argument #(tag-sym #(14 11) "x"))))))
                           #(func-from #(tag-from #(14 13) "->"))
                           #(expr
                             (#(tag-num #(14 16) 0)
                              #(func-decl (#(func-name #(tag-sym #(14 20) "x"))))
                              #(tag-hghr #(14 18) ">")))))))))
                  #(argument (#(simple-argument #(tag-sym #(15 9) "exp"))))
                  #(argument
                    (#(array-simple
                       (#(open-braket #(tag-lbrk #(16 9) #\[))
                        #(argument (#(simple-argument #(tag-num #(16 11) 0))))
                        #(argument (#(simple-argument #(tag-num #(16 13) 1))))
                        #(argument (#(simple-argument #(tag-num #(16 15) -1))))
                        #(argument (#(simple-argument #(tag-num #(16 18) 2))))
                        #(argument (#(simple-argument #(tag-num #(16 20) -2))))
                        #(argument (#(simple-argument #(tag-num #(16 23) 3))))
                        #(argument (#(simple-argument #(tag-num #(16 25) -3))))
                        #(close-braket #(tag-rbrk #(16 27) #\]))))))))))))
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

(define (get-token-tag token)
  (vector-ref token T-TAG))

(define (get-token-coords token)
  (vector-ref token T-COORDS))

(define (get-token-value token)
  (vector-ref token T-VALUE))

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

(define (get-type-of-arg arg-rule)
  (let* ((terms (get-rule-terms arg-rule))
         (first-term (car terms))
         (first-type (get-rule-name first-term)))
    (cond ((eq? first-type 'simple-argument) (get-simple-arg-type first-term))
          ((eq? first-type 'array-simple)    (get-array-type first-term))
          ((eq? first-type 'continuous)      '()))))

(define (get-types-of-args func-args)
  (letrec ((types (map get-type-of-arg func-args))
           (helper (lambda (types)
                     (if (null? types)
                         '()
                         (if (null? (car types))
                             (helper (cdr types))
                             (cons (car types) (helper (cdr types))))))))
    (helper types)))

(define (get-name-of-arg arg-rule)
  (let* ((terms (get-rule-terms arg-rule))
         (first-term (car terms))
         (first-type (get-rule-name first-term)))
    (cond ((eq? first-type 'simple-argument)
           (get-simple-arg-name first-term)))))

(define (get-names-of-args func-args)
  (map get-name-of-arg func-args))

(define (get-num-of-args func-args)
  (if (null? func-args)
      `(lambda (x) (zero? x))
      (let* ((last (car (reverse func-args)))
             (terms (get-rule-terms last))
             (term (car terms))
             (type (get-rule-name term))
             (num  (length func-args)))  
          (if (eq? type 'continuous)
            `(lambda (:x) (>= :x ,(- num 1)))
            `(lambda (:x) (= :x ,num))))))

(define (add-func-in-model model name obj)
  (cons (list name obj) model))

(define (add-type-in-model model name val)
  (map (lambda (model-func)
         (let ((f-name (car model-func)))
           (if (equal? name f-name)
               (cdns val model-func)
               model-func)))
       model))

(define (find-in-model func-name model)
  (and (not-null? model)
       ;(let ((car-model (car model)))
       (let* ((car-model model)
              (in-model (assoc func-name car-model)))
         (or (and in-model (cons (cdr in-model)
                                 (or (find-in-model func-name (cdr model))
                                     '())))
             (and (find-in-params func-name car-model)
                  ;(print '!!!in-params!!!)
                  (list func-name))
             (find-in-model func-name (cdr model))))))


(define (get-args-from-type type)
  (vector-ref type TYPE-ARGS))

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
  (vector (vector `(lambda (:x) #t) (list) (list)) (list) (list)))

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
  
(define (get-args-check-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-CHECK))

(define (get-similar-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-SIMILAR))

(define (get-defs-from-type type)
  (vector-ref type TYPE-DEFS))

(define (get-exprs-from-type type)
  (vector-ref type TYPE-EXPRS))

(define (get-list-inner xs)
  (if (>= (length xs) 2)
      (reverse (cdr (reverse (cdr xs))))
      xs))

(define (and-fold xs)
  (or (null? xs)
      (and (car xs)
           (and-fold (cdr xs)))))

(define (or-fold xs)
  (and (not-null? xs)
       (or (car xs)
           (or-fold (cdr xs)))))

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

(define (get-simple-arg-value arg-rule)
  (get-token-value (get-token-from-simple-rule arg-rule)))

(define (get-args-num-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-NUM))

(define (get-args-names-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-NAMES))

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

(define get-new-name
  (let ((name ":g"))
    (lambda ()
      (set! name (string-append name "_"))
      (string->symbol name))))

(define (get-sym-name name)
  (if (string? name)
      (string->symbol name)
      (get-new-name)))

#| make-names-list
 | Создание списка имён.
 |  Строки преобразуются к символам.
 |  Для всех остальных типов генерируется новое имя
 | @param {list of arg-names} names список имён
 | @returns {list of names} список имён аргументов для lambda-функции
 |#
(define (make-names-list names)
  (map get-sym-name names))

#| Возвращает первые n элементов списка xs
 | @param {list} xs
 | @param {int} n
 | @returns {list}
 |#
(define (give-first xs n)
  (if (or (zero? n)
          (null? xs))
      '()
      (cons (car xs) (give-first (cdr xs) (- n 1)))))

(define (and-fold xs)
  ; (print 'and-fold xs)
  (or (null? xs)
      (and (car xs)
           (and-fold (cdr xs)))))

(define (is-cont-name? name)
  ;(print name)
  (and (list? name)
       (not-null? name)
       (eq? (car name) 'continuous)))

(define (make-lambda-var-from-list xs)
  (if (null? xs)
      xs
      (let* ((r-xs (reverse xs))
             (last (car r-xs)))
        (if (is-cont-name? last)
            (if (> (length xs) 1)
                (append (map make-lambda-var (reverse (cddr xs)))
                        `(,(make-lambda-var (cadr r-xs)) . ,(cadr last)))
                (cadr last))
            (map get-sym-name xs)))))

(define (make-lambda-var elem)
  (if (list? elem)
      (make-lambda-var-from-list elem)
      (get-sym-name elem)))

;; только один уровень вложенности
(define (make-let-var-list names cor-names)
  #| Строит список определений для аргументов-списков
   | @param {list of arg-names} n-list Список имён аргументов
   | @param {symb} a-list Текущий остаток аргументов
   | @param {alist} l-list let-list (name value) Список let-определений
   | @returns {alist} l-list
   |#
  (define (helper n-list a-list l-list)
    (if (null? n-list)
        (reverse l-list)
        (let ((cur-name (car n-list)))
          (helper (cdr n-list)
                  `(cdr ,a-list)
                  (if (is-cont-name? cur-name)
                      (cons `(,(get-sym-name (cadr cur-name)) ,a-list) l-list)
                      (cons `(,(get-sym-name cur-name) (car ,a-list)) l-list))))))
  
  (apply append (map (lambda (name cor-name)
                       (if (list? name)
                           (helper name cor-name '())
                           '()))
                     names
                     cor-names)))

;; @returns (list lambda-var-list let-var-list)
(define (make-var-lists type)
  (let* ((names (get-args-names-from-type type))
         (has-let? (not-null? (filter (lambda (x)
                                        (and (list? x)
                                             (or (null? x)
                                                 (neq? (car x) 'continuous))))
                                      names))))

    (if has-let?
        (let ((cor-names (make-lambda-var-from-list names)))
          (list cor-names (make-let-var-list names cor-names)))
        (list (make-lambda-var-from-list names)))))

(define (get-args-for-check name type)
  (let ((names (get-args-names-from-type type)))
    (if (not-null? names)
        (let ((last (car (reverse names))))
          ;(print 'get-args-for-check last)
          (if (and (list? last)
                   (not-null? last)
                   (eq? (car last) 'continuous))
              `(give-first ,name ,(- (length names) 1))
              name))
        name)))

(define (is-apply? arg-values)
  (and (eq? 1 (length arg-values))
       (let ((val (car arg-values)))
         (and (list? val)
              (not-null? val)
              (eq? ':apply (car (reverse val)))))))
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
          ;(print 'semantic-func-body body)
          (list (reverse (remove-last (car body) l-this)) (cdr body))))

      (define (semantic-func-def func-def-terms model)
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
          ;(print 'semantic-func-def add-list)
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
          (and (not-null? correct-types)
               (or  (and (zero? arg-len)
                         name)
                    (let ((arg-values (map (lambda (arg)
                                             (get-arg-value (car (get-rule-terms arg))
                                                            (list (list name-token func-types))))
                                           args)))
                      ;(print 'semantic-func-call arg-values)
                      (if (is-apply? arg-values)
                          (list ':func-call "apply" name (caar arg-values))
                          (append (list ':func-call name) arg-values)))))))

      ;; надо связывать индексы в списке с функциями сравнения
      ;; для тех элементов, которые являются символами нужно хранить имена... ЖИЗНЬ БОЛЬ
      (define (semantic-var func-decl-terms model)
        (let* ((s-rule (car func-decl-terms))
               (name-token (get-token-from-simple-rule s-rule))
               (name (get-token-value name-token))
               (args (cdr func-decl-terms))
               (in-model (find-in-model name model)))
          ;(print 'semantic-var name in-model)
          (or (and in-model
                   (or (or-fold (map (lambda (in-model) (semantic-func-call name-token args in-model))
                                     in-model))
                       (add-error ERROR_NUM_OF_ARGS name-token)))
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
          ;(print 'argument-to-expr f-term-name)
          (cond ((eq? f-term-name 'simple-argument) (get-simple-arg-value f-term))
                ((eq? f-term-name 'continuous)      (get-continuous-expr f-term model))
                ((eq? f-term-name 'func-decl)       (car (semantic-expr terms model))))))

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
                    `(:func-call append-s ,(cons ':list (map (lambda (x) (argument-to-expr x model)) list-elems))
                                 ,(cons ':list (get-continuous-expr f-term model)))
                    `,(cons ':qlist (map (lambda (x) (argument-to-expr x model)) inner)))))))

      (define (get-arg-value arg-rule model)
        (let ((name (get-rule-name arg-rule)))
          (cond ((eq? name 'simple-argument) (get-simple-arg-value arg-rule))
                ((eq? name 'array-simple)    (semantic-expr-arr (get-rule-terms arg-rule) model))
                ((eq? name 'apply)           (cons (get-arg-value (car (get-rule-terms (cadr (get-rule-terms arg-rule))))
                                                                  model)
                                                   '(:apply)))
                ((eq? name 'expr)            (semantic-expr (get-rule-terms arg-rule)
                                                            (append (make-alist
                                                                     (vector-ref (get-args-from-type
                                                                                  (car (cadar model)))
                                                                                 TYPE-ARGS-NAMES))
                                                                    model))))))
      ;; if-cond: (#if-cond
      ;;           #expr
      ;;           #then
      ;;           #expr)
      (define (semantic-if-cond if-cond model)
        ;(print 'semantic-if-cond (cdddr if-cond))
        (list (semantic-expr (get-rule-terms (cadr if-cond)) model)
              (semantic-expr (get-rule-terms (car (cdddr if-cond))) model)))

      ;; terms: (#if-word
      ;;         ((#if-cond
      ;;           #expr
      ;;           #then
      ;;           #expr)
      ;;          (...)))
      (define (semantic-if-expr terms model)
        (cons ':cond (map (lambda (x) (semantic-if-cond x model))
                         (cadr terms))))

      (define (semantic-lambda func-def-terms model)
        ;(print 'semantic-lambda func-def-terms)
        (let* ((func-decl (car func-def-terms))

               (func-args (get-args-from-decl func-decl))
               (names-of-args (get-names-of-args func-args))

               (this-model (append (make-alist names-of-args) model))
               (body-list (semantic-func-body func-def-terms
                                              names-of-args
                                              this-model))

               (f-exprs (cadr body-list)))
          (list ':lambda names-of-args f-exprs)))

      (define (semantic-expr-elem elem model)
        (let ((type (get-rule-name elem)))
          ;(print 'semantic-expr-elem type)
          (cond ((eq? type 'func-decl)    (semantic-var (get-rule-terms elem) model))
                ((eq? type 'array-simple) (semantic-expr-arr (get-rule-terms elem) model))
                ((eq? type 'if-expression)(semantic-if-expr (get-rule-terms elem) model))
                ((eq? type 'lambda-func)  (semantic-lambda (get-rule-terms elem) model))
                (else                     (get-token-value elem)))))

      (define (semantic-expr terms model)
        (map (lambda (x) (semantic-expr-elem x model)) terms))

      ;; need to make semantic-model-exprs
      (define (semantic-model-exprs model)
        ;(let* ((f-bodies (map get-func-bodies model))))
        model)

      ;; parse exprs after defs
      (define (semantic-program ast model exprs)
        (if (null? ast)
            (list model exprs)
            (let* ((rule (car ast))
                   (name (get-rule-name rule))
                   (terms (get-rule-terms rule)))
              ;(print 'semantic-program name)
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
