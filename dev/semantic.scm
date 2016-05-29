;; examp
(define v '(#(func-def
              (#(func-decl
                 (#(func-name #(tag-sym #(158 1) "newton"))
                  #(argument (#(simple-argument #(tag-sym #(158 8) "f"))))
                  #(argument (#(simple-argument #(tag-sym #(158 10) "df"))))
                  #(argument (#(simple-argument #(tag-sym #(158 13) "x"))))
                  #(argument (#(simple-argument #(tag-sym #(158 15) "e"))))))
               #(func-to #(tag-to #(158 17) "<-"))
               #(expr
                 (#(if-expression
                    (#(if-word #(tag-kw #(159 5) "if"))
                     ((#(if-cond #(tag-bor #(159 9) "|"))
                       #(expr
                         (#(func-decl
                            (#(func-name #(tag-sym #(159 13) "f"))
                             #(argument
                               (#(expr (#(simple-argument #(tag-sym #(159 15) "x"))))))))
                          #(tag-kw #(159 12) "abs")
                          #(func-decl (#(func-name #(tag-sym #(159 20) "e"))))
                          #(tag-lwr #(159 18) "<")))
                       #(then #(tag-from #(159 22) "->"))
                       #(expr (#(func-decl (#(func-name #(tag-sym #(159 25) "x")))))))
                      (#(if-cond #(tag-bor #(160 9) "|"))
                       #(expr (#(tag-true #(0 0) #t)))
                       #(then #(tag-from #(160 23) "->"))
                       #(expr
                         (#(func-decl
                            (#(func-name #(tag-sym #(160 26) "newton"))
                             #(argument (#(simple-argument #(tag-sym #(160 33) "f"))))
                             #(argument (#(simple-argument #(tag-sym #(160 35) "df"))))
                             #(argument
                               (#(expr
                                  (#(func-decl (#(func-name #(tag-sym #(160 39) "x"))))
                                   #(func-decl
                                     (#(func-name #(tag-sym #(160 43) "f"))
                                      #(argument
                                        (#(apply
                                           (#(apply-dot #(tag-dot #(160 44) #\.))
                                            #(argument
                                              (#(array-simple
                                                 (#(open-braket #(tag-lbrk #(160 45) #\[))
                                                  #(argument
                                                    (#(expr
                                                       (#(simple-argument
                                                          #(tag-sym #(160 46) "x"))))))
                                                  #(close-braket
                                                    #(tag-rbrk #(160 47) #\]))))))))))))
                                   #(func-decl
                                     (#(func-name #(tag-sym #(160 51) "df"))
                                      #(argument
                                        (#(apply
                                           (#(apply-dot #(tag-dot #(160 53) #\.))
                                            #(argument
                                              (#(array-simple
                                                 (#(open-braket #(tag-lbrk #(160 54) #\[))
                                                  #(argument
                                                    (#(expr
                                                       (#(simple-argument
                                                          #(tag-sym #(160 55) "x"))))))
                                                  #(close-braket
                                                    #(tag-rbrk #(160 56) #\]))))))))))))
                                   #(tag-rem #(160 49) "/")
                                   #(tag-mns #(160 41) "-")))))
                             #(argument
                               (#(expr
                                  (#(simple-argument
                                     #(tag-sym #(160 59) "e"))))))))))))))))))
            #(func-def
              (#(func-decl
                 (#(func-name #(tag-sym #(162 1) "golden"))
                  #(argument (#(simple-argument #(tag-sym #(162 8) "f"))))
                  #(argument (#(simple-argument #(tag-sym #(162 10) "x0"))))
                  #(argument (#(simple-argument #(tag-sym #(162 13) "x1"))))
                  #(argument (#(simple-argument #(tag-sym #(162 16) "e"))))))
               #(func-to #(tag-to #(162 18) "<-"))
               #(func-def
                 (#(func-decl (#(func-name #(tag-sym #(163 5) "fi"))))
                  #(func-to #(tag-to #(163 8) "<-"))
                  #(expr
                    (#(tag-num #(163 14) 5)
                     #(tag-kw #(163 13) "sqrt")
                     #(tag-num #(163 19) 1)
                     #(tag-pls #(163 17) "+")
                     #(tag-num #(163 24) 2)
                     #(tag-rem #(163 22) "/")))))
               #(func-def
                 (#(func-decl
                    (#(func-name #(tag-sym #(165 5) "loop"))
                     #(argument (#(simple-argument #(tag-sym #(165 10) "f"))))
                     #(argument (#(simple-argument #(tag-sym #(165 12) "x0"))))
                     #(argument (#(simple-argument #(tag-sym #(165 15) "x1"))))
                     #(argument (#(simple-argument #(tag-sym #(165 18) "e"))))))
                  #(func-to #(tag-to #(165 20) "<-"))
                  #(func-def
                    (#(func-decl (#(func-name #(tag-sym #(166 9) "a"))))
                     #(func-to #(tag-to #(166 11) "<-"))
                     #(expr
                       (#(func-decl (#(func-name #(tag-sym #(166 14) "x1"))))
                        #(func-decl (#(func-name #(tag-sym #(166 20) "x1"))))
                        #(func-decl (#(func-name #(tag-sym #(166 25) "x0"))))
                        #(tag-mns #(166 23) "-")
                        #(func-decl (#(func-name #(tag-sym #(166 31) "fi"))))
                        #(tag-rem #(166 29) "/")
                        #(tag-mns #(166 17) "-")))))
                  #(func-def
                    (#(func-decl (#(func-name #(tag-sym #(167 9) "b"))))
                     #(func-to #(tag-to #(167 11) "<-"))
                     #(expr
                       (#(func-decl (#(func-name #(tag-sym #(167 14) "x0"))))
                        #(func-decl (#(func-name #(tag-sym #(167 20) "x1"))))
                        #(func-decl (#(func-name #(tag-sym #(167 25) "x0"))))
                        #(tag-mns #(167 23) "-")
                        #(func-decl (#(func-name #(tag-sym #(167 31) "fi"))))
                        #(tag-rem #(167 29) "/")
                        #(tag-pls #(167 17) "+")))))
                  #(expr
                    (#(if-expression
                       (#(if-word #(tag-kw #(169 9) "if"))
                        ((#(if-cond #(tag-bor #(169 13) "|"))
                          #(expr
                            (#(func-decl
                               (#(func-name #(tag-sym #(169 15) "f"))
                                #(argument
                                  (#(apply
                                     (#(apply-dot #(tag-dot #(169 16) #\.))
                                      #(argument
                                        (#(array-simple
                                           (#(open-braket #(tag-lbrk #(169 17) #\[))
                                            #(argument
                                              (#(expr
                                                 (#(simple-argument
                                                    #(tag-sym #(169 18) "a"))))))
                                            #(close-braket
                                              #(tag-rbrk #(169 19) #\]))))))))))))
                             #(func-decl
                               (#(func-name #(tag-sym #(169 24) "f"))
                                #(argument
                                  (#(apply
                                     (#(apply-dot #(tag-dot #(169 25) #\.))
                                      #(argument
                                        (#(array-simple
                                           (#(open-braket #(tag-lbrk #(169 26) #\[))
                                            #(argument
                                              (#(expr
                                                 (#(simple-argument
                                                    #(tag-sym #(169 27) "b"))))))
                                            #(close-braket
                                              #(tag-rbrk #(169 28) #\]))))))))))))
                             #(tag-heq #(169 21) ">=")))
                          #(then #(tag-from #(169 30) "->"))
                          #(expr
                            (#(if-expression
                               (#(if-word #(tag-kw #(170 17) "if"))
                                ((#(if-cond #(tag-bor #(170 21) "|"))
                                  #(expr
                                    (#(func-decl (#(func-name #(tag-sym #(170 25) "x1"))))
                                     #(func-decl (#(func-name #(tag-sym #(170 30) "a"))))
                                     #(tag-mns #(170 28) "-")
                                     #(tag-kw #(170 24) "abs")
                                     #(func-decl (#(func-name #(tag-sym #(170 35) "e"))))
                                     #(tag-lwr #(170 33) "<")))
                                  #(then #(tag-from #(170 37) "->"))
                                  #(expr
                                    (#(func-decl (#(func-name #(tag-sym #(170 41) "a"))))
                                     #(func-decl (#(func-name #(tag-sym #(170 45) "x1"))))
                                     #(tag-pls #(170 43) "+")
                                     #(tag-num #(170 51) 2)
                                     #(tag-rem #(170 49) "/"))))
                                 (#(if-cond #(tag-bor #(171 21) "|"))
                                  #(expr (#(tag-true #(0 0) #t)))
                                  #(then #(tag-from #(171 40) "->"))
                                  #(expr
                                    (#(func-decl
                                       (#(func-name #(tag-sym #(171 43) "loop"))
                                        #(argument
                                          (#(simple-argument #(tag-sym #(171 48) "f"))))
                                        #(argument
                                          (#(simple-argument #(tag-sym #(171 50) "a"))))
                                        #(argument
                                          (#(simple-argument #(tag-sym #(171 52) "x1"))))
                                        #(argument
                                          (#(simple-argument
                                             #(tag-sym #(171 55) "e")))))))))))))))
                         (#(if-cond #(tag-bor #(172 13) "|"))
                          #(expr (#(tag-true #(0 0) #t)))
                          #(then #(tag-from #(172 15) "->"))
                          #(expr
                            (#(if-expression
                               (#(if-word #(tag-kw #(173 17) "if"))
                                ((#(if-cond #(tag-bor #(173 21) "|"))
                                  #(expr
                                    (#(func-decl (#(func-name #(tag-sym #(173 25) "b"))))
                                     #(func-decl (#(func-name #(tag-sym #(173 29) "x0"))))
                                     #(tag-mns #(173 27) "-")
                                     #(tag-kw #(173 24) "abs")
                                     #(func-decl (#(func-name #(tag-sym #(173 35) "e"))))
                                     #(tag-lwr #(173 33) "<")))
                                  #(then #(tag-from #(173 37) "->"))
                                  #(expr
                                    (#(func-decl (#(func-name #(tag-sym #(173 41) "x0"))))
                                     #(func-decl (#(func-name #(tag-sym #(173 46) "b"))))
                                     #(tag-pls #(173 44) "+")
                                     #(tag-num #(173 51) 2)
                                     #(tag-rem #(173 49) "/"))))
                                 (#(if-cond #(tag-bor #(174 21) "|"))
                                  #(expr (#(tag-true #(0 0) #t)))
                                  #(then #(tag-from #(174 40) "->"))
                                  #(expr
                                    (#(func-decl
                                       (#(func-name #(tag-sym #(174 43) "loop"))
                                        #(argument
                                          (#(simple-argument #(tag-sym #(174 48) "f"))))
                                        #(argument
                                          (#(simple-argument #(tag-sym #(174 50) "x0"))))
                                        #(argument
                                          (#(simple-argument #(tag-sym #(174 53) "b"))))
                                        #(argument
                                          (#(simple-argument
                                             #(tag-sym #(174 55) "e"))))))))))))))))))))))
               #(expr
                 (#(func-decl
                    (#(func-name #(tag-sym #(176 5) "loop"))
                     #(argument (#(simple-argument #(tag-sym #(176 10) "f"))))
                     #(argument (#(simple-argument #(tag-sym #(176 12) "x0"))))
                     #(argument (#(simple-argument #(tag-sym #(176 15) "x1"))))
                     #(argument (#(simple-argument #(tag-sym #(176 18) "e"))))))))))
            #(expr
              (#(func-decl
                 (#(func-name #(tag-sym #(178 7) "newton"))
                  #(argument
                    (#(expr
                       (#(lambda-func
                          (#(lambda-func-decl
                             (#(func-name #(tag-lmbd #(178 14) "\\"))
                              #(argument (#(simple-argument #(tag-sym #(178 16) "x"))))))
                           #(func-from #(tag-from #(178 18) "->"))
                           #(expr
                             (#(func-decl (#(func-name #(tag-sym #(178 21) "x"))))
                              #(tag-num #(178 26) 2)
                              #(tag-pow #(178 23) "**")))))))))
                  #(argument
                    (#(expr
                       (#(lambda-func
                          (#(lambda-func-decl
                             (#(func-name #(tag-lmbd #(179 14) "\\"))
                              #(argument (#(simple-argument #(tag-sym #(179 16) "x"))))))
                           #(func-from #(tag-from #(179 18) "->"))
                           #(expr
                             (#(tag-num #(179 21) 2)
                              #(func-decl (#(func-name #(tag-sym #(179 25) "x"))))
                              #(tag-mul #(179 23) "*")))))))))
                  #(argument (#(expr (#(simple-argument #(tag-num #(180 14) 1.0))))))
                  #(argument (#(expr (#(simple-argument #(tag-num #(181 14) 1e-08))))))))
               #(tag-kw #(178 1) "round")))
            #(expr
              (#(func-decl
                 (#(func-name #(tag-sym #(183 7) "newton"))
                  #(argument
                    (#(expr
                       (#(lambda-func
                          (#(lambda-func-decl
                             (#(func-name #(tag-lmbd #(183 14) "\\"))
                              #(argument (#(simple-argument #(tag-sym #(183 16) "x"))))))
                           #(func-from #(tag-from #(183 18) "->"))
                           #(expr
                             (#(func-decl (#(func-name #(tag-sym #(183 21) "x"))))
                              #(tag-num #(183 26) 2)
                              #(tag-pow #(183 23) "**")
                              #(tag-num #(183 30) 4)
                              #(func-decl (#(func-name #(tag-sym #(183 34) "x"))))
                              #(tag-mul #(183 32) "*")
                              #(tag-pls #(183 28) "+")
                              #(tag-num #(183 38) 4)
                              #(tag-pls #(183 36) "+")))))))))
                  #(argument
                    (#(expr
                       (#(lambda-func
                          (#(lambda-func-decl
                             (#(func-name #(tag-lmbd #(184 14) "\\"))
                              #(argument (#(simple-argument #(tag-sym #(184 16) "x"))))))
                           #(func-from #(tag-from #(184 18) "->"))
                           #(expr
                             (#(tag-num #(184 21) 2)
                              #(func-decl (#(func-name #(tag-sym #(184 25) "x"))))
                              #(tag-mul #(184 23) "*")
                              #(tag-num #(184 29) 4)
                              #(tag-pls #(184 27) "+")))))))))
                  #(argument (#(expr (#(simple-argument #(tag-num #(185 14) 5.0))))))
                  #(argument (#(expr (#(simple-argument #(tag-num #(186 14) 1e-08))))))))
               #(tag-kw #(183 1) "round")))
            #(expr
              (#(func-decl
                 (#(func-name #(tag-sym #(188 7) "golden"))
                  #(argument
                    (#(expr
                       (#(lambda-func
                          (#(lambda-func-decl
                             (#(func-name #(tag-lmbd #(188 14) "\\"))
                              #(argument (#(simple-argument #(tag-sym #(188 16) "x"))))))
                           #(func-from #(tag-from #(188 18) "->"))
                           #(expr
                             (#(func-decl (#(func-name #(tag-sym #(188 21) "x"))))
                              #(tag-num #(188 26) 2)
                              #(tag-pow #(188 23) "**")))))))))
                  #(argument (#(expr (#(simple-argument #(tag-num #(189 14) -2.0))))))
                  #(argument (#(expr (#(simple-argument #(tag-num #(190 14) 2.0))))))
                  #(argument (#(expr (#(simple-argument #(tag-num #(191 14) 1e-08))))))))
               #(tag-kw #(188 1) "round")))
            #(expr
              (#(func-decl
                 (#(func-name #(tag-sym #(193 7) "golden"))
                  #(argument
                    (#(expr
                       (#(lambda-func
                          (#(lambda-func-decl
                             (#(func-name #(tag-lmbd #(193 14) "\\"))
                              #(argument (#(simple-argument #(tag-sym #(193 16) "x"))))))
                           #(func-from #(tag-from #(193 18) "->"))
                           #(expr
                             (#(func-decl (#(func-name #(tag-sym #(193 21) "x"))))
                              #(tag-num #(193 26) 2)
                              #(tag-pow #(193 23) "**")
                              #(tag-num #(193 30) 4)
                              #(func-decl (#(func-name #(tag-sym #(193 34) "x"))))
                              #(tag-mul #(193 32) "*")
                              #(tag-pls #(193 28) "+")
                              #(tag-num #(193 38) 4)
                              #(tag-pls #(193 36) "+")))))))))
                  #(argument (#(expr (#(simple-argument #(tag-num #(194 14) -5.0))))))
                  #(argument (#(expr (#(simple-argument #(tag-num #(195 14) 5.0))))))
                  #(argument (#(expr (#(simple-argument #(tag-num #(196 14) 1e-06))))))))
               #(tag-kw #(193 1) "round")))
            #(expr (#(tag-num #(198 1) 5) #(tag-num #(198 5) 6) #(tag-pls #(198 3) "+")))
            #(expr (#(tag-num #(200 3) 8) #(tag-not #(200 1) "!")))))
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
               (f-defs (car body-list))
               (f-exprs (cadr body-list))

               (func-val (vector args-vector f-defs f-exprs))
               (add-list (list model func-name func-val)))
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
               (correct-types (filter (lambda (type)
                                        ((eval-i (get-args-num-from-type type)) arg-len))
                                      func-types)))
          ;(print 'semantic-func-call1 name arg-len func-types)
          (and (not-null? correct-types)
               (or  (and (zero? arg-len)
                         (or (and (> (length func-types) (length correct-types))
                                  (list ':func-call name))
                             name))
                    (let ((arg-values (map (lambda (arg)
                                             (get-arg-value (car (get-rule-terms arg))
                                                            model))
                                           args)))
                      ;(print 'semantic-func-call2 name arg-values)
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
          ;(print 'semantic-var name args in-model model)
          (or (and in-model
                   (or (semantic-func-call name-token args in-model model)
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
                                                                                  (cadar model))
                                                                                 TYPE-ARGS-NAMES))
                                                                    model)))
                ((eq? name 'continuous)      (cons (get-arg-value (cadr (get-rule-terms arg-rule))
                                                                  model)
                                                   '(:apply))))))
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
                    ((eq? name 'expr)
                     (semantic-program (cdr ast)
                                       model
                                       (append exprs (semantic-expr terms model))))
                    ((eq? name 'scheme)
                     (semantic-program (cdr ast)
                                       model
                                       (append exprs (semantic-scheme terms model))))))))

      (let ((m (semantic-program ast '() '())))
        (and (print-errors) m)))))
