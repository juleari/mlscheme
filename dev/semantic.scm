;; examp
(define v '(#(func-def
              (#(func-decl
                 (#(func-name #(tag-sym #(1 1) "0?"))
                  #(argument (#(simple-argument #(tag-num #(1 4) 0))))))
               #(func-to #(tag-to #(1 6) "<-"))
               #(expr (#(tag-true #(1 9) #t)))))
            #(func-def
              (#(func-decl
                 (#(func-name #(tag-sym #(2 1) "0?"))
                  #(argument (#(simple-argument #(tag-sym #(2 4) "x"))))))
               #(func-to #(tag-to #(2 6) "<-"))
               #(expr (#(tag-fls #(2 9) #f)))))
            #(func-def
              (#(func-decl
                 (#(func-name #(tag-sym #(4 1) "my-gcd"))
                  #(argument (#(simple-argument #(tag-sym #(4 8) "a"))))
                  #(argument (#(simple-argument #(tag-sym #(4 10) "b"))))))
               #(func-to #(tag-to #(4 12) "<-"))
               #(func-def
                 (#(func-decl (#(func-name #(tag-sym #(5 5) "r"))))
                  #(func-to #(tag-to #(5 7) "<-"))
                  #(expr
                    (#(func-decl (#(func-name #(tag-sym #(5 10) "a"))))
                     #(func-decl (#(func-name #(tag-sym #(5 14) "b"))))
                     #(tag-mod #(5 12) "%")))))
               #(expr
                 (#(if-expression
                    (#(if-word #(tag-kw #(7 5) "if"))
                     ((#(if-cond #(tag-bor #(7 9) "|"))
                       #(expr
                         (#(func-decl (#(func-name #(tag-sym #(7 11) "a"))))
                          #(func-decl (#(func-name #(tag-sym #(7 15) "b"))))
                          #(tag-lwr #(7 13) "<")))
                       #(then #(tag-from #(7 17) "->"))
                       #(expr
                         (#(func-decl
                            (#(func-name #(tag-sym #(7 20) "my-gcd"))
                             #(argument
                               (#(simple-argument #(tag-sym #(7 27) "b"))))
                             #(argument
                               (#(simple-argument #(tag-sym #(7 29) "a")))))))))
                      (#(if-cond #(tag-bor #(8 9) "|"))
                       #(expr
                         (#(func-decl
                            (#(func-name #(tag-sym #(8 11) "0?"))
                             #(argument
                               (#(simple-argument #(tag-sym #(8 14) "r"))))))))
                       #(then #(tag-from #(8 17) "->"))
                       #(expr
                         (#(func-decl (#(func-name #(tag-sym #(8 20) "b")))))))
                      (#(if-cond #(tag-bor #(9 9) "|"))
                       #(expr (#(tag-true #(0 0) #t)))
                       #(then #(tag-from #(9 17) "->"))
                       #(expr
                         (#(func-decl
                            (#(func-name #(tag-sym #(9 20) "my-gcd"))
                             #(argument
                               (#(simple-argument #(tag-sym #(9 27) "b"))))
                             #(argument
                               (#(simple-argument
                                  #(tag-sym #(9 29) "r"))))))))))))))))
            #(func-def
              (#(func-decl
                 (#(func-name #(tag-sym #(11 1) "my-lcm"))
                  #(argument (#(simple-argument #(tag-sym #(11 8) "a"))))
                  #(argument (#(simple-argument #(tag-sym #(11 10) "b"))))))
               #(func-to #(tag-to #(11 12) "<-"))
               #(expr
                 (#(func-decl (#(func-name #(tag-sym #(12 10) "a"))))
                  #(func-decl (#(func-name #(tag-sym #(12 14) "b"))))
                  #(tag-mul #(12 12) "*")
                  #(func-decl
                    (#(func-name #(tag-sym #(12 18) "my-gcd"))
                     #(argument (#(simple-argument #(tag-sym #(12 25) "a"))))
                     #(argument (#(simple-argument #(tag-sym #(12 27) "b"))))))
                  #(tag-rem #(12 16) "/")
                  #(tag-kw #(12 5) "abs")))))
            #(func-def
              (#(func-decl
                 (#(func-name #(tag-sym #(14 1) "prime?"))
                  #(argument (#(simple-argument #(tag-sym #(14 8) "n"))))))
               #(func-to #(tag-to #(14 10) "<-"))
               #(func-def
                 (#(func-decl
                    (#(func-name #(tag-sym #(15 5) "fact"))
                     #(argument (#(simple-argument #(tag-num #(15 10) 0))))))
                  #(func-to #(tag-to #(15 12) "<-"))
                  #(expr (#(tag-num #(15 15) 1)))))
               #(func-def
                 (#(func-decl
                    (#(func-name #(tag-sym #(16 5) "fact"))
                     #(argument (#(simple-argument #(tag-sym #(16 10) "n"))))))
                  #(func-to #(tag-to #(16 12) "<-"))
                  #(expr
                    (#(func-decl (#(func-name #(tag-sym #(16 15) "n"))))
                     #(func-decl
                       (#(func-name #(tag-sym #(16 19) "fact"))
                        #(argument
                          (#(expr
                             (#(func-decl (#(func-name #(tag-sym #(16 25) "n"))))
                              #(tag-num #(16 29) 1)
                              #(tag-mns #(16 27) "-")))))))
                     #(tag-mul #(16 17) "*")))))
               #(memo (#(memo-word) (#(func-decl #(func-name #(16 25) "fact")))))
               #(expr
                 (#(func-decl
                    (#(func-name #(tag-sym #(18 5) "0?"))
                     #(argument
                       (#(apply
                          (#(apply-dot #(tag-dot #(18 7) #\.))
                           #(argument
                             (#(array-simple
                                (#(open-braket #(tag-lbrk #(18 8) #\[))
                                 #(argument
                                   (#(expr
                                      (#(func-decl
                                         (#(func-name #(tag-sym #(18 11) "fact"))
                                          #(argument
                                            (#(apply
                                               (#(apply-dot
                                                  #(tag-dot #(18 15) #\.))
                                                #(argument
                                                  (#(array-simple
                                                     (#(open-braket
                                                        #(tag-lbrk #(18 16) #\[))
                                                      #(argument
                                                        (#(expr
                                                           (#(simple-argument
                                                              #(tag-sym
                                                                #(18 17)
                                                                "n"))
                                                            #(simple-argument
                                                              #(tag-num
                                                                #(18 21)
                                                                1))
                                                            #(tag-mns
                                                              #(18 19)
                                                              "-")))))
                                                      #(close-braket
                                                        #(tag-rbrk
                                                          #(18 22)
                                                          #\]))))))))))))
                                       #(simple-argument #(tag-num #(18 26) 1))
                                       #(tag-pls #(18 24) "+")
                                       #(simple-argument #(tag-sym #(18 31) "n"))
                                       #(tag-mod #(18 29) "%")))))
                                 #(close-braket
                                   #(tag-rbrk #(18 33) #\]))))))))))))))))
            #(memo (#(memo-word) (#(func-decl #(func-name #(16 25) "my-gcd"))
                                  #(func-decl #(func-name #(16 25) "prime?")))))
            #(expr
              (#(func-decl
                 (#(func-name #(tag-sym #(20 1) "my-gcd"))
                  #(argument (#(simple-argument #(tag-num #(20 8) 3542))))
                  #(argument (#(simple-argument #(tag-num #(20 13) 2464))))))))
            #(expr
              (#(func-decl
                 (#(func-name #(tag-sym #(21 1) "my-lcm"))
                  #(argument (#(simple-argument #(tag-num #(21 8) 3))))
                  #(argument (#(simple-argument #(tag-num #(21 10) 4))))))))
            #(expr
              (#(func-decl
                 (#(func-name #(tag-sym #(22 1) "prime?"))
                  #(argument (#(simple-argument #(tag-num #(22 8) 11))))))))
            #(expr
              (#(func-decl
                 (#(func-name #(tag-sym #(23 1) "prime?"))
                  #(argument (#(simple-argument #(tag-num #(23 8) 12))))))))
            #(expr
              (#(func-decl
                 (#(func-name #(tag-sym #(24 1) "prime?"))
                  #(argument (#(simple-argument #(tag-num #(24 8) 3571))))))))))
#| #(scheme
   (#(scheme-word #(tag-kw #(112 1) "scheme"))
    #(scheme-expr
      #(tag-schm
        #(112 8)
        "(define (selection-sort pred? xs)\n         (define (min-xs xs x)\n           (cond ((null? xs)         x)\n                 ((pred? (car xs) x) (min-xs (cdr xs) (car xs)))\n                 (else               (min-xs (cdr xs) x))))\n  \n         (define (swap j xs)\n           (let ((xj (list-ref xs j))\n                 (vs (list->vector xs)))\n             (vector-set! vs j (car xs))\n             (vector-set! vs 0 xj)\n             (vector->list vs)))\n  \n         (define (ind x xs)\n           (- (length xs) (length (member x xs))))\n  \n         (define (helper xs)\n           (if (null? xs)\n               '()\n               (let ((x (min-xs xs (car xs))))\n                 (cons x (helper (cdr (swap (ind x xs) xs)))))))\n  \n         (helper xs))"))))
 #(scheme
   (#(scheme-word #(tag-kw #(113 1) "scheme"))
    #(scheme-expr
      #(tag-schm
        #(113 8)
        "(define (insertion-sort pred? xs)\n         (define (insert xs ys x)\n           (cond ((null? ys) (append xs (list x)))\n                 ((pred? (car ys) x) (insert (append xs (list (car ys))) (cdr ys) x))\n                 (else               (append xs (list x) ys))))\n  \n         (define (helper xs ys)\n           (if (null? ys)\n               xs\n               (helper (insert '() xs (car ys)) (cdr ys))))\n  \n         (helper '() xs))"))))
 #(export
   (#(export-word #(tag-kw #(115 1) "export"))
    (#(export-name #(tag-sym #(115 8) "selection-sort"))
     #(export-name #(tag-sym #(115 23) "insertion-sort")))))
 #(expr
   (#(func-decl
      (#(func-name #(tag-sym #(117 1) "selection-sort"))
       #(argument
         (#(expr
            (#(lambda-func
               (#(lambda-func-decl
                  (#(func-name #(tag-lmbd #(117 16) "\\"))
                   #(argument (#(simple-argument #(tag-sym #(117 18) "x"))))
                   #(argument (#(simple-argument #(tag-sym #(117 20) "y"))))))
                #(func-from #(tag-from #(117 22) "->"))
                #(expr
                  (#(func-decl (#(func-name #(tag-sym #(117 25) "x"))))
                   #(func-decl (#(func-name #(tag-sym #(117 30) "y"))))
                   #(tag-leq #(117 27) "<=")))))))))
       #(argument
         (#(array-simple
            (#(open-braket #(tag-lbrk #(118 16) #\[))
             #(argument (#(simple-argument #(tag-num #(118 17) 9))))
             #(argument (#(simple-argument #(tag-num #(118 18) 6))))
             #(argument (#(simple-argument #(tag-num #(118 20) 2))))
             #(argument (#(simple-argument #(tag-num #(118 22) 4))))
             #(argument (#(simple-argument #(tag-num #(118 24) 3))))
             #(argument (#(simple-argument #(tag-num #(118 26) 5))))
             #(argument (#(simple-argument #(tag-num #(118 28) 7))))
             #(argument (#(simple-argument #(tag-num #(118 30) 1))))
             #(argument (#(simple-argument #(tag-num #(118 32) 8))))
             #(argument (#(simple-argument #(tag-num #(118 34) 0))))
             #(close-braket #(tag-rbrk #(118 35) #\]))))))))))
 #(expr
   (#(func-decl
      (#(func-name #(tag-sym #(119 1) "insertion-sort"))
       #(argument
         (#(expr
            (#(lambda-func
               (#(lambda-func-decl
                  (#(func-name #(tag-lmbd #(119 16) "\\"))
                   #(argument (#(simple-argument #(tag-sym #(119 18) "x"))))
                   #(argument (#(simple-argument #(tag-sym #(119 20) "y"))))))
                #(func-from #(tag-from #(119 22) "->"))
                #(expr
                  (#(func-decl (#(func-name #(tag-sym #(119 25) "x"))))
                   #(func-decl (#(func-name #(tag-sym #(119 30) "y"))))
                   #(tag-leq #(119 27) "<=")))))))))
       #(argument
         (#(array-simple
            (#(open-braket #(tag-lbrk #(120 16) #\[))
             #(argument (#(simple-argument #(tag-num #(120 17) 9))))
             #(argument (#(simple-argument #(tag-num #(120 18) 6))))
             #(argument (#(simple-argument #(tag-num #(120 20) 2))))
             #(argument (#(simple-argument #(tag-num #(120 22) 4))))
             #(argument (#(simple-argument #(tag-num #(120 24) 3))))
             #(argument (#(simple-argument #(tag-num #(120 26) 5))))
             #(argument (#(simple-argument #(tag-num #(120 28) 7))))
             #(argument (#(simple-argument #(tag-num #(120 30) 1))))
             #(argument (#(simple-argument #(tag-num #(120 32) 8))))
             #(argument (#(simple-argument #(tag-num #(120 34) 0))))
             #(close-braket #(tag-rbrk #(120 35) #\]))))))))))|#
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
(define-syntax :eval-i
  (syntax-rules ()
    ((_ x) (eval x (interaction-environment)))))

(define-syntax neq?
  (syntax-rules ()
    ((_ x y) (not (eqv? x y)))))

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

(define (add-func-in-model model name memo-list obj)
  (cons (list name memo-list obj) model))

(define (add-type-in-model model name memo-list val)
  (map (lambda (model-func)
         (let ((f-name (car model-func)))
           (if (equal? name f-name)
               (append (list f-name memo-list)
                       (cddr model-func)
                       (list val))
               model-func)))
       model))

(define (set-memo-in-model model name memo-name)
  (map (lambda (model-func)
         (let ((f-name (car model-func)))
           (if (equal? name f-name)
               (append (list f-name
                             (let ((memo-names (cadr model-func)))
                               (if memo-names
                                   (if (list? memo-names)
                                       (cons memo-name memo-names)
                                       (list memo-names memo-name))
                                   memo-name)))
                       (cddr model-func))
               model-func)))
       model))

(define (find-in-model func-name model)
  (and (not-null? model)
       (let* ((first  (car model))
              (f-par (car first))
              (f-val  (cdr first)))
         (or (and (find-in-params func-name f-par)
                  (append f-val
                          (or (find-in-model func-name (cdr model))
                              '())))
             ;(and (find-in-params func-name car-model)
             ;     (cons (make-arg-type)
             ;           (or (find-in-model func-name (cdr model))
             ;               '())))
             (find-in-model func-name (cdr model))))))

(define (get-args-from-type type)
  (vector-ref type TYPE-ARGS))

(define (x-in-xs x xs)
  (and (not-null? xs)
       (or (equal? x (car xs))
           (and (list? (car xs)) (x-in-xs x (car xs)))
           (x-in-xs x (cdr xs)))))

(define (find-in-params func-name param)
  (or (and (list? param)
           (x-in-xs func-name param))
      (equal? func-name param)))

(define (make-arg-type)
  (vector (vector `(lambda (:x) #t) (list) (list) `(lambda :args #t))
          (list) (list)))

(define (make-alist xs)
  (define (helper xs alist)
    (if (null? xs)
        alist
        (helper (cdr xs) (cons (list (car xs) #f (make-arg-type)) alist))))
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

(define (:and-fold xs)
  (or (null? xs)
      (and (car xs)
           (:and-fold (cdr xs)))))

(define (:or-fold xs)
  (and (not-null? xs)
       (or (car xs)
           (:or-fold (cdr xs)))))

(define (get-array-args-rules l-lambda inner x)
  `(:and-fold (cons ,l-lambda
                   (map (lambda (:lambda-i :xi)
                          ((:eval-i :lambda-i) :xi))
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
                                     (if (and (neq? (get-arg ind1) ':continuous)
                                              (equal? (get-arg ind1)
                                                      (get-arg ind2)))
                                         (begin (vector-set! vec ind2 (s-name))
                                                (list (list ind1 ind2)))
                                         (list)))))
                ;(print 'find-similar args)
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
                         (:and-fold-s ,(map (lambda (:similar-pair)
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
  (list func-name #f (vector args-vector (list) (list))))

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
  ;(print 'is-apply arg-values)
  (and (eq? 1 (length arg-values))
       (let ((val (car arg-values)))
         (and (list? val)
              (not-null? val)
              (eq? ':apply (car (reverse val)))))))

(define (symbol-append . symbols)
  (string->symbol (apply string-append (map symbol->string symbols))))

(define (symbol-append-to-strings symbol . strings)
  (string->symbol (apply string-append (cons (symbol->string symbol) strings))))

(define (get-memo-name var name)
  (print 'get-memo-name name var)
  (symbol-append-to-strings (cadr var) "-" name "-" (car var)))

;; @returns memo-var:
;; (car memo-var) -- memo-list || #f
;; (cdr memo-var) -- new var-list
(define (get-memo-list var-list name)
  (let ((memo-names '()))
    (set! var-list (map (lambda (var)
                          (if (cadr var)
                              (let ((memo-name (get-memo-name var name)))
                                (set! memo-names (cons memo-name memo-names))
                                (append (list (car var)
                                              memo-name)
                                        (cddr var)))
                              var))
                          var-list))
    (cons (and (not-null? memo-names)
               memo-names)
          var-list)))
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
          (list ':continuous (get-simple-arg-name first-term))))

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
          (list (remove-first l-this (car body)) (cdr body))))

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
               (memo-var (get-memo-list (car body-list) func-name))
               (memo-list (car memo-var))
               (f-defs (cdr memo-var))
               (f-exprs (cadr body-list))

               (func-val (vector args-vector f-defs f-exprs))
               (add-list (list model func-name memo-list func-val)))
          ;(print 'semantic-func-def func-name f-exprs in-model)
          (apply (if in-model
                     add-type-in-model
                     add-func-in-model)
                 add-list)))

      ;; проверять, что текущая функция используется
      ;; сначала просто проходим по определениям,
      ;; когда встречаем expr сохраняем его в список выражений для текущей области видимости
      ;; после того как все определения сформировались идём по выражениям и проверяем, что всё ок
      (define (semantic-func-call name-token args func-types model)
        (let* ((name (get-token-value name-token))
               (arg-len (length args))
               (types (filter vector? func-types))
               (correct-types (filter (lambda (type)
                                        ((:eval-i (get-args-num-from-type type))
                                              arg-len))
                                      types)))
          ;(print 'semantic-func-call1 name model arg-len args 'func-types func-types 'correct-types correct-types)
          (and ;(print name 'func-types func-types (length func-types) (length correct-types))
               (not-null? (length types))
               (or  (and (zero? arg-len)
                         (or (and (> (length types)
                                     (length correct-types))
                                  (list ':func-call name))
                             name))
                    (let ((arg-values (map (lambda (arg)
                                             (get-arg-value (car (get-rule-terms arg))
                                                            model))
                                           args)))
                      ;(print 'semantic-func-call2 name arg-values (is-apply? arg-values))
                      (or (and (is-apply? arg-values)
                               (list ':func-call "apply" name (caar arg-values)))
                          (and (not-null? correct-types)
                               (append (list ':func-call name) arg-values))))))))

      ;; надо связывать индексы в списке с функциями сравнения
      ;; для тех элементов, которые являются символами нужно хранить имена... ЖИЗНЬ БОЛЬ
      (define (semantic-var func-decl-terms model)
        (let* ((s-rule (car func-decl-terms))
               (name-token (get-token-from-simple-rule s-rule))
               (name (get-token-value name-token))
               (args (cdr func-decl-terms))
               (in-model (find-in-model name model)))
          ;(print 'semantic-var name 'args args 'in-model in-model 'model model)
          (or (and in-model
                   (or (semantic-func-call name-token args in-model model)
                       (add-error ERROR_NUM_OF_ARGS name-token)))
              (and (add-error ERROR_UNDEFINED_VARIABLE name-token)
                   func-decl-terms))))

      (define (get-continuous-expr cont-rule model)
        (let* ((terms   (get-rule-terms cont-rule))
               (expr    (cadr terms))
               (e-ts    (get-rule-terms expr))
               (e-terms (if (list? e-ts)
                            e-ts
                            (list e-ts))))
          (semantic-expr e-terms model)))

      ;; нужно проверять, что simple-argument в model
      (define (argument-to-expr argument model)
        (let* ((terms       (get-rule-terms argument))
               (f-term      (car terms))
               (f-term-name (get-rule-name f-term)))
          (cond ((eq? f-term-name 'simple-argument) (get-simple-arg-value f-term))
                ((eq? f-term-name 'array-simple)    (semantic-expr-arr (get-rule-terms f-term)
                                                                       model))
                ((eq? f-term-name 'continuous)      (get-continuous-expr f-term model))
                ((eq? f-term-name 'func-decl)       (car (semantic-expr terms model)))
                ((eq? f-term-name 'expr)            (semantic-expr (get-rule-terms f-term)
                                                                   model)))))

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
                    `(:func-call :append-s ,(cons ':list (map (lambda (x) (argument-to-expr x model)) list-elems))
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
                                                                                  (car (cddar model)))
                                                                                 TYPE-ARGS-NAMES))
                                                                    model)))
                ((eq? name 'continuous)      (cons (get-arg-value (cadr (get-rule-terms arg-rule))
                                                                  model)
                                                   '(:apply)))
                ((eq? name 'func-decl)       (semantic-var (get-rule-terms arg-rule) model)))))
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
          (cond ((eq? type 'func-decl)      (semantic-var (get-rule-terms elem) model))
                ((eq? type 'array-simple)   (semantic-expr-arr (get-rule-terms elem) model))
                ((eq? type 'if-expression)  (semantic-if-expr (get-rule-terms elem) model))
                ((eq? type 'lambda-func)    (semantic-lambda (get-rule-terms elem) model))
                ((eq? type 'apply)          (get-arg-value elem model))
                ((eq? type 'simple-argument)(get-simple-arg-value elem))
                (else                       (get-token-value elem)))))

      (define (semantic-expr terms model)
        (list (map (lambda (x) (semantic-expr-elem x model)) terms)))

      ;; need to make semantic-model-exprs
      (define (semantic-model-exprs model)
        ;(let* ((f-bodies (map get-func-bodies model))))
        model)

      (define (semantic-scheme terms model)
        (list (list ':scheme (get-simple-arg-value (cadr terms)))))

      (define (semantic-export terms model)
        (append model
                (map (lambda (term)
                       (list (get-token-value (get-token-from-simple-rule term))
                             #f
                             (make-arg-type)))
                     (cadr terms))))

      (define (semantic-memo terms model)
        (map (lambda (term)
               (set! model
                     (set-memo-in-model model
                                        (get-token-value (get-token-from-simple-rule term))
                                        ':memo)))
             (cadr terms))
        model)

      ;; parse exprs after defs
      (define (semantic-program ast model exprs)
        (if (null? ast)
            (list (reverse model) exprs)
            (let* ((rule (car ast))
                   (name (get-rule-name rule))
                   (terms (get-rule-terms rule)))
              ;(print 'semantic-program name)
              (cond ((eq? name 'func-def)
                     (semantic-program (cdr ast)
                                       (semantic-func-def terms model)
                                       exprs))
                    ((eq? name 'memo)
                     (semantic-program (cdr ast)
                                       (semantic-memo terms model)
                                       exprs))
                    ((eq? name 'expr)
                     (semantic-program (cdr ast)
                                       model
                                       (append exprs (semantic-expr terms model))))
                    ((eq? name 'scheme)
                     (semantic-program (cdr ast)
                                       model
                                       (append exprs (semantic-scheme terms model))))
                    ((eq? name 'export)
                     (semantic-program (cdr ast)
                                       (semantic-export terms model)
                                       exprs))))))

      (let ((m (semantic-program ast '() '())))
        (and (print-errors) m)))))
