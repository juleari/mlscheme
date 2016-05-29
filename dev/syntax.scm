(define TAB 4)
(define TAB1 (- TAB 1))

(define // quotient)
(define % remainder)
(define ** expt)

(define ARGS-CAN-BE-EXPRS #t)
(define ARGS-IN-DECL-EXPR 'args-in-expr)
(define ARGS-WITHOUT-F-CALLS 'without-func-calls)
(define ARGS-ONLY-ARRAY 'only-array)
(define ARGS-CANT-BE-EXPRS #f)
(define ARGS-CAN-BE-FUNCS #t)
(define ARGS-CANT-BE-FUNCS #f)
(define ARGS-CAN-BE-CONT #t)
(define ARGS-CANT-BE-CONT #f)

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

(define ERROR_NO_FUNC_BODY   "no expression in body")
(define ERROR_NO_EOF         "no end of file")
(define ERROR_EXPR_ARENS     "arens not closed")
(define ERR_NO_CLOSE_BRK     "no close braket")
(define ERROR_EXPR           "expr is not correct")
(define ERROR_NO_IF_CONDS    "no conditions in if")
(define ERROR_NO_IF_ACT      "no expression in if")
(define ERR_AFTER_CONTINUOUS "uncorrect after continuous")
(define ERR_NO_CONTINUOUS    "no continuous")
(define ERROR_NO_TAG_TO      "no '->' in lambda")
(define ERR_INCORRECT_APPLY  "apply has no arguments")

(define TYPE-ARGS 0)
(define TYPE-ARGS-NUM 0)
(define TYPE-ARGS-CHECK 1)
(define TYPE-ARGS-NAMES 2)
(define TYPE-DEFS 1)
(define TYPE-EXPRS 2)
(define TYPE-ARGS-NAMES 2)

(define ERROR_NUM_OF_ARGS "the expected number of arguments does not match the given number")
(define ERROR_UNDEFINED_VARIABLE "cannot reference undefined identifier")


(define tokens '(#(tag-sym #(1 1) "newton")
                 #(tag-sym #(1 8) "f")
                 #(tag-sym #(1 10) "df")
                 #(tag-sym #(1 13) "x")
                 #(tag-sym #(1 15) "e")
                 #(tag-to #(1 17) "<-")
                 #(tag-kw #(2 5) "if")
                 #(tag-bor #(2 9) "|")
                 #(tag-lprn #(2 11) #\()
                 #(tag-kw #(2 12) "abs")
                 #(tag-sym #(2 13) "f")
                 #(tag-sym #(2 15) "x")
                 #(tag-rprn #(2 16) #\))
                 #(tag-lwr #(2 18) "<")
                 #(tag-sym #(2 20) "e")
                 #(tag-from #(2 22) "->")
                 #(tag-sym #(2 25) "x")
                 #(tag-bor #(3 9) "|")
                 #(tag-from #(3 23) "->")
                 #(tag-sym #(3 26) "newton")
                 #(tag-sym #(3 33) "f")
                 #(tag-sym #(3 35) "df")
                 #(tag-lprn #(3 38) #\()
                 #(tag-sym #(3 39) "x")
                 #(tag-mns #(3 41) "-")
                 #(tag-sym #(3 43) "f")
                 #(tag-dot #(3 44) #\.)
                 #(tag-lbrk #(3 45) #\[)
                 #(tag-sym #(3 46) "x")
                 #(tag-rbrk #(3 47) #\])
                 #(tag-rem #(3 49) "/")
                 #(tag-sym #(3 51) "df")
                 #(tag-dot #(3 53) #\.)
                 #(tag-lbrk #(3 54) #\[)
                 #(tag-sym #(3 55) "x")
                 #(tag-rbrk #(3 56) #\])
                 #(tag-rprn #(3 57) #\))
                 #(tag-sym #(3 59) "e")
                 #(tag-sym #(5 1) "golden")
                 #(tag-sym #(5 8) "f")
                 #(tag-sym #(5 10) "x0")
                 #(tag-sym #(5 13) "x1")
                 #(tag-sym #(5 16) "e")
                 #(tag-to #(5 18) "<-")
                 #(tag-sym #(6 5) "fi")
                 #(tag-to #(6 8) "<-")
                 #(tag-lprn #(6 11) #\()
                 #(tag-lprn #(6 12) #\()
                 #(tag-kw #(6 13) "sqrt")
                 #(tag-num #(6 14) 5)
                 #(tag-rprn #(6 15) #\))
                 #(tag-pls #(6 17) "+")
                 #(tag-num #(6 19) 1)
                 #(tag-rprn #(6 20) #\))
                 #(tag-rem #(6 22) "/")
                 #(tag-num #(6 24) 2)
                 #(tag-sym #(8 5) "loop")
                 #(tag-sym #(8 10) "f")
                 #(tag-sym #(8 12) "x0")
                 #(tag-sym #(8 15) "x1")
                 #(tag-sym #(8 18) "e")
                 #(tag-to #(8 20) "<-")
                 #(tag-sym #(9 9) "a")
                 #(tag-to #(9 11) "<-")
                 #(tag-sym #(9 14) "x1")
                 #(tag-mns #(9 17) "-")
                 #(tag-lprn #(9 19) #\()
                 #(tag-sym #(9 20) "x1")
                 #(tag-mns #(9 23) "-")
                 #(tag-sym #(9 25) "x0")
                 #(tag-rprn #(9 27) #\))
                 #(tag-rem #(9 29) "/")
                 #(tag-sym #(9 31) "fi")
                 #(tag-sym #(10 9) "b")
                 #(tag-to #(10 11) "<-")
                 #(tag-sym #(10 14) "x0")
                 #(tag-pls #(10 17) "+")
                 #(tag-lprn #(10 19) #\()
                 #(tag-sym #(10 20) "x1")
                 #(tag-mns #(10 23) "-")
                 #(tag-sym #(10 25) "x0")
                 #(tag-rprn #(10 27) #\))
                 #(tag-rem #(10 29) "/")
                 #(tag-sym #(10 31) "fi")
                 #(tag-kw #(12 9) "if")
                 #(tag-bor #(12 13) "|")
                 #(tag-sym #(12 15) "f")
                 #(tag-dot #(12 16) #\.)
                 #(tag-lbrk #(12 17) #\[)
                 #(tag-sym #(12 18) "a")
                 #(tag-rbrk #(12 19) #\])
                 #(tag-heq #(12 21) ">=")
                 #(tag-sym #(12 24) "f")
                 #(tag-dot #(12 25) #\.)
                 #(tag-lbrk #(12 26) #\[)
                 #(tag-sym #(12 27) "b")
                 #(tag-rbrk #(12 28) #\])
                 #(tag-from #(12 30) "->")
                 #(tag-kw #(13 17) "if")
                 #(tag-bor #(13 21) "|")
                 #(tag-lprn #(13 23) #\()
                 #(tag-kw #(13 24) "abs")
                 #(tag-sym #(13 25) "x1")
                 #(tag-mns #(13 28) "-")
                 #(tag-sym #(13 30) "a")
                 #(tag-rprn #(13 31) #\))
                 #(tag-lwr #(13 33) "<")
                 #(tag-sym #(13 35) "e")
                 #(tag-from #(13 37) "->")
                 #(tag-lprn #(13 40) #\()
                 #(tag-sym #(13 41) "a")
                 #(tag-pls #(13 43) "+")
                 #(tag-sym #(13 45) "x1")
                 #(tag-rprn #(13 47) #\))
                 #(tag-rem #(13 49) "/")
                 #(tag-num #(13 51) 2)
                 #(tag-bor #(14 21) "|")
                 #(tag-from #(14 40) "->")
                 #(tag-sym #(14 43) "loop")
                 #(tag-sym #(14 48) "f")
                 #(tag-sym #(14 50) "a")
                 #(tag-sym #(14 52) "x1")
                 #(tag-sym #(14 55) "e")
                 #(tag-bor #(15 13) "|")
                 #(tag-from #(15 15) "->")
                 #(tag-kw #(16 17) "if")
                 #(tag-bor #(16 21) "|")
                 #(tag-lprn #(16 23) #\()
                 #(tag-kw #(16 24) "abs")
                 #(tag-sym #(16 25) "b")
                 #(tag-mns #(16 27) "-")
                 #(tag-sym #(16 29) "x0")
                 #(tag-rprn #(16 31) #\))
                 #(tag-lwr #(16 33) "<")
                 #(tag-sym #(16 35) "e")
                 #(tag-from #(16 37) "->")
                 #(tag-lprn #(16 40) #\()
                 #(tag-sym #(16 41) "x0")
                 #(tag-pls #(16 44) "+")
                 #(tag-sym #(16 46) "b")
                 #(tag-rprn #(16 47) #\))
                 #(tag-rem #(16 49) "/")
                 #(tag-num #(16 51) 2)
                 #(tag-bor #(17 21) "|")
                 #(tag-from #(17 40) "->")
                 #(tag-sym #(17 43) "loop")
                 #(tag-sym #(17 48) "f")
                 #(tag-sym #(17 50) "x0")
                 #(tag-sym #(17 53) "b")
                 #(tag-sym #(17 55) "e")
                 #(tag-sym #(19 5) "loop")
                 #(tag-sym #(19 10) "f")
                 #(tag-sym #(19 12) "x0")
                 #(tag-sym #(19 15) "x1")
                 #(tag-sym #(19 18) "e")
                 #(tag-kw #(21 1) "round")
                 #(tag-sym #(21 7) "newton")
                 #(tag-lmbd #(21 14) "\\")
                 #(tag-sym #(21 16) "x")
                 #(tag-from #(21 18) "->")
                 #(tag-sym #(21 21) "x")
                 #(tag-pow #(21 23) "**")
                 #(tag-num #(21 26) 2)
                 #(tag-lmbd #(22 14) "\\")
                 #(tag-sym #(22 16) "x")
                 #(tag-from #(22 18) "->")
                 #(tag-num #(22 21) 2)
                 #(tag-mul #(22 23) "*")
                 #(tag-sym #(22 25) "x")
                 #(tag-num #(23 14) 1.0)
                 #(tag-num #(24 14) 1e-08)
                 #(tag-kw #(26 1) "round")
                 #(tag-sym #(26 7) "newton")
                 #(tag-lmbd #(26 14) "\\")
                 #(tag-sym #(26 16) "x")
                 #(tag-from #(26 18) "->")
                 #(tag-sym #(26 21) "x")
                 #(tag-pow #(26 23) "**")
                 #(tag-num #(26 26) 2)
                 #(tag-pls #(26 28) "+")
                 #(tag-num #(26 30) 4)
                 #(tag-mul #(26 32) "*")
                 #(tag-sym #(26 34) "x")
                 #(tag-pls #(26 36) "+")
                 #(tag-num #(26 38) 4)
                 #(tag-lmbd #(27 14) "\\")
                 #(tag-sym #(27 16) "x")
                 #(tag-from #(27 18) "->")
                 #(tag-num #(27 21) 2)
                 #(tag-mul #(27 23) "*")
                 #(tag-sym #(27 25) "x")
                 #(tag-pls #(27 27) "+")
                 #(tag-num #(27 29) 4)
                 #(tag-num #(28 14) 5.0)
                 #(tag-num #(29 14) 1e-08)
                 #(tag-kw #(31 1) "round")
                 #(tag-sym #(31 7) "golden")
                 #(tag-lmbd #(31 14) "\\")
                 #(tag-sym #(31 16) "x")
                 #(tag-from #(31 18) "->")
                 #(tag-sym #(31 21) "x")
                 #(tag-pow #(31 23) "**")
                 #(tag-num #(31 26) 2)
                 #(tag-num #(32 14) -2.0)
                 #(tag-num #(33 14) 2.0)
                 #(tag-num #(34 14) 1e-08)
                 #(tag-kw #(36 1) "round")
                 #(tag-sym #(36 7) "newton")
                 #(tag-lmbd #(36 14) "\\")
                 #(tag-sym #(36 16) "x")
                 #(tag-from #(36 18) "->")
                 #(tag-sym #(36 21) "x")
                 #(tag-pow #(36 23) "**")
                 #(tag-num #(36 26) 2)
                 #(tag-pls #(36 28) "+")
                 #(tag-num #(36 30) 4)
                 #(tag-mul #(36 32) "*")
                 #(tag-sym #(36 34) "x")
                 #(tag-pls #(36 36) "+")
                 #(tag-num #(36 38) 4)
                 #(tag-num #(37 14) -5.0)
                 #(tag-num #(38 14) 5.0)
                 #(tag-num #(39 14) 1e-06)
                 #(tag-num #(41 1) 5)
                 #(tag-pls #(41 3) "+")
                 #(tag-num #(41 5) 6)
                 #(tag-not #(43 1) "!")
                 #(tag-num #(43 3) 8)))

(define-syntax neq?
  (syntax-rules ()
    ((_ x y) (not (eqv? x y)))))

(define-syntax not-null?
  (syntax-rules ()
    ((_ x) (not (null? x)))))

(define-syntax eval-i
  (syntax-rules ()
    ((_ x) (eval x (interaction-environment)))))

(define-syntax cdns
  (syntax-rules ()
    ((_ x xs) (append xs (list x)))))

(define (print . xs)
  (or (and (not-null? xs)
           (display (car xs))
           (newline)
           (apply print (cdr xs)))
      (newline)))

(define (get-true-expr)
  (vector 'expr (list #(tag-true #(0 0) #t))))

(define (get-token)
  (if (null? tokens)
      #(tag-end #(2 27) "eof")
      (let ((token (car tokens)))
        (set! tokens (cdr tokens))
        token)))

(define (get-token-tag token)
  (vector-ref token T-TAG))

(define (get-token-coords token)
  (vector-ref token T-COORDS))

(define (get-token-value token)
  (vector-ref token T-VALUE))

(define (get-token-pos token)
  (let ((coords (get-token-coords token)))
    (vector-ref coords C-POSITION)))

(define (get-rule-name rule)
  (vector-ref rule R-NAME))

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

(define (x-in-xs? x . xs)
  (and (not-null? xs)
       (or (equal? x (car xs))
           (apply x-in-xs? (cons x (cdr xs))))))

(define (append-to-rule-list rule xs)
  (vector (get-rule-name rule)
          (append (get-rule-list rule) xs)))

(define (make-syntax-expr terms)
  (and terms
       (vector 'expr (if (list? terms)
                    terms
                    (list terms)))))

(define syntax
  (let ((errors '())
        (ast '()))
    (lambda ()
      (define token (get-token))

      (define (add-error error-type)
        (set! errors
              (cons (vector 'error: (get-token-coords token) '- error-type)
                    errors))
        '())

      (define (add-error-rule rule)
        (set! errors
              (cons (vector 'error: 
                            (get-token-coords token)
                            '-
                            'expected
                            `,rule)
                    errors))
        '())

      (define (print-errors)
        (if (null? errors)
            (display "SYNTAX OK!")
            (begin (display "SYNTAX ERRORS:")
                   (newline)
                   (apply print (reverse errors))))
        (newline)
        (newline))

      (define (next-token)
        (set! token (get-token)))

      (define (is-type? . types)
        (apply x-in-xs? (cons (get-token-tag token) types)))

      (define (start-in? start-pos)
        (> (get-token-pos token) start-pos))

      (define (syntax-rule rule-expr . args)
        ;(print 'syntax-rule rule-expr)
        (eval-i rule-expr))

      (define (syntax-rule? rule . args)
        (or (apply rule args) '()))

      (define (syntax-rule* rule . args)
        (define (helper ast-list)
          (let ((ast-elem (apply rule args)))
            ;(print 'syntax-rule* ast-elem)
            (if ast-elem
                (helper (cons ast-elem ast-list))
                (reverse ast-list))))
        (helper '()))

      (define (syntax-rule+ rule . args)
        (define (helper ast-list)
          (let ((ast-elem (apply rule args)))
            (if ast-elem
                (helper (cons ast-elem ast-list))
                (reverse ast-list))))

        (let ((first (apply rule args)))
          (and first
               (cons first (helper '())))))

      (define (simple-rule start-pos rule-name . tags)
        (and (start-in? start-pos)
             (let ((t token))
               (and (apply is-type? tags)
                    (next-token)
                    (vector `,rule-name `,t)))))

      (define (simple-func-name name word)
        (let ((t token))
          (and (is-type? 'tag-kw)
               (equal? (get-token-value token) word)
               (next-token)
               (vector `,name `,t))))

      (define (get-first-rule start-pos first-rule-expr)
        (and (start-in? start-pos)
             (eval-i first-rule-expr)))

      (define (get-program-rule start-pos)
        `(or (,syntax-rule+ ,syntax-program ,start-pos)
             (,add-error ,ERROR_NO_FUNC_BODY)))

      (define (syntax-lambda-body start-pos)
        (syntax-partional-rule start-pos
                               `(,simple-rule ,start-pos 'func-from 'tag-from)
                               (get-program-rule start-pos)))

      (define (syntax-partional-rule start-pos
                                     first-rule-expr
                                     next-rules)
        (let ((first-rule (get-first-rule start-pos first-rule-expr)))
          (and first-rule
               (cons first-rule
                     (eval-i next-rules)))))

      (define (syntax-whole-rule rule-name
                                 start-pos
                                 first-rule-expr
                                 syntax-rule-type
                                 second-rule-expr
                                 second-rule-args
                                 error-name)
        (let ((first-rule (get-first-rule start-pos first-rule-expr)))
          ;(print 'syntax-whole-rule first-rule)
          (and first-rule
               (vector rule-name
                       (cons first-rule
                             (or (list
                                  (apply syntax-rule-type
                                         (append
                                          (list second-rule-expr
                                                (get-simple-start-pos first-rule))
                                          second-rule-args)))
                                 (and error-name (add-error error-name))))))))

      #|(define (syntax-array-simple start-pos)
        (define ast-list '())
        (if (start-in? start-pos)
            (let ((first-rule (simple-rule start-pos 'open-braket 'tag-lbrk)))
              (and first-rule
                   (set! ast-list (cons first-rule
                                        (syntax-arguments (get-simple-start-pos 
                                                           first-rule)
                                                          ARGS-CANT-BE-EXPRS
                                                          ARGS-CANT-BE-FUNCS
                                                          ARGS-CAN-BE-CONT)))
                   (or (let ((last-rule (simple-rule start-pos
                                                     'close-braket
                                                     'tag-rbrk)))
                         (and last-rule
                              (set! ast-list (append ast-list 
                                                     (list last-rule)))))
                       (add-error ERR_NO_CLOSE_BRK))
                   (list (vector 'array-simple ast-list))))
            ast-list))|#

      (define (syntax-array start-pos array-in-expr?)
        (define ast-list '())
        (and (start-in? start-pos)
             (let ((first-rule (simple-rule start-pos 'open-braket 'tag-lbrk)))
               (and first-rule
                    ;(print 'syntax-array ARGS-CAN-BE-CONT)
                    (set! ast-list (cons first-rule
                                         (syntax-arguments (get-simple-start-pos 
                                                            first-rule)
                                                           (and array-in-expr?
                                                                ARGS-WITHOUT-F-CALLS)
                                                           ARGS-CAN-BE-CONT)))
                    (or (let ((last-rule (simple-rule start-pos
                                                      'close-braket
                                                      'tag-rbrk)))
                          (and last-rule
                               (set! ast-list (append ast-list 
                                                     (list last-rule)))))
                        (add-error ERR_NO_CLOSE_BRK))
                    (list (vector 'array-simple ast-list))))))

      (define (syntax-continuous-simple start-pos)
        ;(print 'syntax-continuous-simple start-pos)
        (syntax-whole-rule 'continuous
                           start-pos
                           `(,simple-rule ,start-pos 'colon 'tag-cln)
                           syntax-rule-
                           simple-rule
                           '(continuous-list tag-sym)
                           ERR_NO_CONTINUOUS))

      (define (syntax-continuous start-pos)
        ;(print 'syntax-continuous start-pos)
        (syntax-whole-rule 'continuous
                           start-pos
                           `(,simple-rule ,start-pos 'colon 'tag-cln)
                           syntax-rule-
                           syntax-program
                           '()
                           ERR_NO_CONTINUOUS))

      (define (syntax-arg-continuous start-pos args-expr-type)
        (and (start-in? start-pos)
             (if args-expr-type
                 (let ((cont (syntax-continuous start-pos)))
                   (and cont (list cont)))
                 (let ((cont (syntax-continuous-simple start-pos)))
                   (and cont
                        (or (not (syntax-argument start-pos
                                                  args-expr-type
                                                  ARGS-CANT-BE-CONT))
                            (add-error ERR_AFTER_continuous))
                        (list cont))))))

      (define (syntax-apply start-pos)
        (syntax-whole-rule 'apply
                           start-pos
                           `(,simple-rule ,start-pos 'apply-dot 'tag-dot)
                           syntax-rule-
                           syntax-argument
                           '(ARGS-ONLY-ARRAY ARGS-CANT-BE-CONT)
                           ERR_INCORRECT_APPLY))

      (define (syntax-simple-or-apply start-pos)
        (and (start-in? start-pos)
             (let ((name (simple-rule start-pos
                                      'simple-argument
                                      'tag-sym
                                      'tag-kw
                                      'tag-str
                                      'tag-num)))
               (and name
                    (let* ((name-token (get-simple-rule-token name))
                           (:apply (syntax-apply (get-token-pos name-token))))
                      (if :apply
                          (vector 'func-decl
                                  (list (vector 'func-name
                                                name-token)
                                        (vector 'argument
                                                (list :apply))))
                          name))))))

      (define (syntax-argument start-pos
                               args-expr-type
                               args-can-be-cont?)
        (and (start-in? start-pos)
             (let* ((arg (or (and (not args-expr-type)
                                  (simple-rule start-pos
                                               'simple-argument
                                               'tag-sym
                                               'tag-kw
                                               'tag-str
                                               'tag-num
                                               'tag-fls
                                               'tag-true))
                             (syntax-array start-pos args-expr-type)
                             (and args-can-be-cont?
                                  (syntax-arg-continuous start-pos
                                                         args-expr-type))
                             (and (eq? ARGS-IN-DECL-EXPR args-expr-type)
                                  (or (syntax-apply start-pos)
                                      (let ((sa (syntax-simple-or-apply start-pos)))
                                            (and sa (make-syntax-expr sa)))))
                             (and args-expr-type
                                  (syntax-expr start-pos args-expr-type))))
             #|                ))))
             (let* ((arg (or (and (not args-can-be-funcs?)
                                  (syntax-simple-or-apply start-pos))
                             (syntax-array start-pos args-can-be-exprs?)
                             (and args-can-be-cont?
                                  (syntax-arg-continuous start-pos
                                                         args-can-be-exprs?))
                             (and args-can-be-exprs? (syntax-apply start-pos))
                             (and args-can-be-exprs?
                                  (syntax-expr start-pos
                                               (not args-can-be-cont?)))))|#
                    (larg (if (list? arg) arg (list arg))))
               (and arg (vector 'argument larg)))))

      (define (syntax-arguments start-pos
                                args-expr-type
                                args-can-be-cont?)
        (define (helper ast-list)
          (let ((arg (syntax-argument start-pos
                                      args-expr-type
                                      args-can-be-cont?)))
            (or (and arg (helper (cons arg ast-list)))
                (reverse ast-list))))
        (helper '()))

      (define (syntax-func-lambda-decl start-pos
                                       name-rule-args
                                       rule-name
                                       args-expr-type
                                       args-can-be-cont?)
        (and (start-in? start-pos)
             (let ((first-rule (apply simple-rule
                                      (cons start-pos name-rule-args))))
               (and first-rule
                    (vector rule-name
                            (cons first-rule
                                  (syntax-rule? syntax-arguments
                                                (get-simple-start-pos first-rule)
                                                args-expr-type
                                                args-can-be-cont?)))))))

      (define (syntax-func-declaration start-pos
                                       decl-in-expr?
                                       args-can-be-cont?)
        
        (syntax-func-lambda-decl start-pos
                                 '(func-name tag-sym)
                                 'func-decl
                                 (and decl-in-expr?
                                      ARGS-IN-DECL-EXPR)
                                 args-can-be-cont?))

      (define (syntax-func-body start-pos)
        (syntax-partional-rule start-pos
                               `(,simple-rule ,start-pos 'func-to 'tag-to)
                               (get-program-rule start-pos)))

      (define (syntax-if-actions start-pos)
        (syntax-partional-rule start-pos
                               `(,simple-rule ,start-pos 'then 'tag-from)
                               (get-program-rule start-pos)))

      (define (syntax-if-cond start-pos)
        (syntax-partional-rule start-pos
                               `(,simple-rule ,start-pos 'if-cond 'tag-bor)
                               `(cons (let ((expr (,syntax-rule? ,syntax-expr
                                                                 ,start-pos
                                                                 ,ARGS-CAN-BE-EXPRS)))
                                        (if (and (list? expr) (null? expr))
                                            (,get-true-expr)
                                            expr))
                                      (or (,syntax-if-actions ,start-pos)
                                          (,add-error ,ERROR_NO_IF_ACT)))))

      (define (syntax-if start-pos)
        (syntax-whole-rule 'if-expression
                           start-pos
                           `(,simple-func-name 'if-word "if")
                           syntax-rule+
                           syntax-if-cond
                           '()
                           ERROR_NO_IF_CONDS))

      (define (syntax-rule- rule . args)
        (apply rule args))

      #|(define (get-lambda-rules start-pos)
        ;(print 'get-lambda-rules start-pos)
        (cons (syntax-arguments start-pos
                                ARGS-CANT-BE-EXPRS
                                ARGS-CANT-BE-FUNCS
                                ARGS-CAN-BE-CONT)
              (or (syntax-lambda-body start-pos)
                  (add-error ERROR_NO_TAG_TO))))|#

      (define (syntax-lambda start-pos)
        (let ((func-decl (syntax-func-lambda-decl start-pos
                                                  '(func-name tag-lmbd)
                                                  'lambda-func-decl
                                                  ARGS-CANT-BE-EXPRS
                                                  ARGS-CAN-BE-CONT)))
          (and func-decl
               (let* ((new-start-pos (get-expr-start-pos func-decl))
                      (func-body (syntax-lambda-body new-start-pos)))
                 (or (and func-body
                          (vector 'lambda-func
                                  (cons func-decl func-body))))))))

      (define (syntax-expr start-pos expr-type . func-decl?)
        (if (eq? (get-token-tag token) 'tag-end)
            (and (not-null? func-decl?)
                 (make-syntax-expr (car func-decl?)))
            (make-syntax-expr (or (syntax-array start-pos ARGS-CAN-BE-FUNCS)
                                  (syntax-if start-pos)
                                  (and (neq? expr-type ARGS-ONLY-ARRAY)
                                       (or (syntax-lambda start-pos)
                                           (apply shunting-yard
                                                  (append (list start-pos
                                                                expr-type)
                                                          func-decl?))))))))

      (define (shunting-yard start-pos expr-type . out)
        ;(print 'shunting-yard token start-pos out)
        (define stack '())
        (define start-flag #t)
        (define start-pos-changed? #f)

        (define (op? t)
          (or (x-in-xs? (get-token-tag t)
                        'tag-bor 'tag-band 'tag-xor 'tag-and 'tag-or
                        'tag-neq 'tag-hghr 'tag-lwr 'tag-heq 'tag-leq
                        'tag-pls 'tag-mns  'tag-mul 'tag-div 'tag-eq
                        'tag-mod 'tag-rem  'tag-pow 'tag-not 'tag-conc)
              (is-func-call? t)))

        (define (is-type? . types)
          (apply x-in-xs? (cons (get-token-tag token) types)))

        (define (trigonometric)
          (let ((tag (get-token-tag token))
                (val (get-token-value token)))
            (and (eqv? tag 'tag-kw)
                 (x-in-xs? val "sin" "cos" "tg" "ctg"))))

        (define (is-unar-func? val)
          (or (x-in-xs? val "zero?" "odd?" "even?" "null?" "not" "abs" "!" "round" "sqrt")
              (trigonometric)))

        (define (is-binar-func? val)
          (x-in-xs? val "eq?" "eqv?" "equal?" "gcd" "lcm" "expt"))

        (define (is-nar-func? val)
          (x-in-xs? val "and" "or"))

        (define (is-func-call? t)
          (let ((tag (get-token-tag t))
                (val (get-token-value t)))
            (and (eqv? tag 'tag-kw)
                 (or (is-unar-func? val)
                     (is-binar-func? val)
                     (is-nar-func? val)))))

        (define (prior t)
          (let ((tag (get-token-tag t)))
            (cond ((x-in-xs? tag 'tag-band)                            2)
                  ((x-in-xs? tag 'tag-or)                              3)
                  ((x-in-xs? tag 'tag-xor)                             4)
                  ((x-in-xs? tag 'tag-and)                             5)
                  ((x-in-xs? tag 'tag-neq 'tag-eq)                     6)
                  ((x-in-xs? tag 'tag-lwr 'tag-leq 'tag-hghr 'tag-heq) 7)
                  ((x-in-xs? tag 'tag-pls 'tag-mns 'tag-conc)          8)
                  ((x-in-xs? tag 'tag-mul 'tag-div 'tag-mod 'tag-rem)  9)
                  ((x-in-xs? tag 'tag-pow)                            10)
                  ((x-in-xs? tag 'tag-not)                            11)
                  ((is-func-call? token)                              12)
                  (else                                                0))))

        (define (unar? t)
          (x-in-xs? t 'tag-not))

        (define (get-pos t)
          (let ((tag (get-token-tag t)))
            (cond ((eq? tag 'simple-argument) (get-simple-start-pos t))
                  ((eq? tag 'func-decl)       (get-simple-start-pos
                                               (car (get-rule-list t))))
                  (else                       (get-token-pos t)))))

        (define (try-get)
          (set! start-flag (> (get-token-pos token) start-pos))
          (and start-flag
               (let* ((tag  (get-token-tag token))
                      (val  (get-token-value token))
                      (op   (prior token))
                      (valid-op (and (> op 0)
                                     (or (not-null? stack)
                                         (not-null? out)
                                         (is-unar-func? val))))
                      (to-out? (and (x-in-xs? tag
                                              'tag-num
                                              'tag-true
                                              'tag-fls
                                              'tag-str)
                                    (or (null? out)
                                        (> (prior (car (reverse out))) 0)
                                        (not-null? stack))))
                      (proc (and (or (null? out)
                                     (> (prior (car (reverse out))) 0)
                                     (not-null? stack))
                                 (if (neq? expr-type ARGS-WITHOUT-F-CALLS)
                                     (syntax-func-declaration start-pos
                                                              ARGS-CAN-BE-EXPRS
                                                              (neq? expr-type
                                                                    ARGS-IN-DECL-EXPR))
                                     (syntax-simple-or-apply start-pos))))
                      (flag #f)
                      (end-flag #f))
                 (and (cond (valid-op             (op-to-out op)
                                                  (set! stack 
                                                        (cons token stack)))
                            (proc                 (set! flag #t)
                                                  (set! out (cons proc out)))
                            (to-out?              (set! out (cons token out)))
                            ((eqv? tag 'tag-lprn) (set! stack 
                                                        (cons token stack)))
                            ((eqv? tag 'tag-rprn) (or (and (not-null? stack)
                                                           (op-before-laren-to-out 0))
                                                      (and (set! end-flag #t)
                                                           (set! start-flag #f))))
                            (else                 (set! start-flag #f)
                                                  #f))
                      (or start-pos-changed?
                          (and (set! start-pos
                                     (or (and (not-null? out)
                                              (get-pos (car out)))
                                         (and (not-null? stack)
                                              (get-pos (car stack)))
                                         start-pos))
                               (set! start-pos-changed? #t)))
                      (cond (end-flag #f)
                            (flag     (try-get))
                            (else     (and (next-token)
                                            (neq? (get-token-tag token) 'tag-end)
                                            (try-get))))))))

        (define (op-before-laren-to-out p)
          (if (not-null? stack)
              (let ((cur (car stack)))
                (cond ((eqv? (get-token-tag cur) 'tag-lprn)
                       (set! stack (cdr stack)))
                      ((op? cur) (from-stack-to-out cur 
                                                    op-before-laren-to-out
                                                    p))
                      (else      (add-error ERROR_EXPR_ARENS))))))

        (define (from-stack-to-out cur callback p)
          (set! out (cons cur out))
          (set! stack (cdr stack))
          (callback p))

        (define (op-to-out p)
          (if (not-null? stack)
              (let ((cur (car stack)))
                (if (and (neq? (get-token-tag cur) 'tag-lprn)
                         (>= (prior cur) p))
                    (from-stack-to-out cur op-to-out p)))))

        (define (ops-to-out p)
          (if (not-null? stack)
              (let ((cur (car stack)))
                (if (op? cur)
                    (from-stack-to-out cur ops-to-out p)
                    (add-error ERROR_EXPR)))))

        (define (shunting-alg)
          (try-get)
          (and (or start-flag (not-null? out))
               (ops-to-out 0)
               (reverse out)))

        (define def #f)

        (define (shunting-alg-expr)
          (if (and (not-null? out)
                   (> (get-token-pos token) start-pos)
                   (not (> (prior token) 0)))
              (begin (set! def (car out))
                     (set! start-pos-changed? #t)
                     (set! out '())
                     (let ((res-out (shunting-alg)))
                       (if res-out
                           (begin (vector-set! def
                                               R-TERMS
                                               (cdns (vector 'argument
                                                             (list res-out))
                                                     (get-rule-list def)))
                                  def)
                           def)))
              (shunting-alg)))
        
        (if (eq? expr-type ARGS-IN-DECL-EXPR)
            (and (x-in-xs? (get-token-tag token)
                          'tag-num
                          'tag-str
                          'tag-fls
                          'tag-true
                          'tag-lprn)
                 (shunting-alg-expr))
            (shunting-alg-expr)))

      (define (syntax-scheme start-pos)
        (syntax-whole-rule 'scheme
                           start-pos
                           `(,simple-func-name 'scheme-word "scheme")
                           syntax-rule-
                           simple-rule
                           '(scheme-expr tag-schm)
                           ERROR_NO_IF_CONDS))

      (define (syntax-program start-pos)
        (let ((func-decl (syntax-func-declaration start-pos
                                                  ARGS-CANT-BE-EXPRS
                                                  ARGS-CAN-BE-CONT)))
          (or (and func-decl
                   (let* ((new-start-pos (get-expr-start-pos func-decl))
                          (func-body (syntax-func-body new-start-pos)))
                     (or (and func-body
                              (vector 'func-def
                                      (cons func-decl func-body)))
                         (let ((func-args (syntax-arguments new-start-pos
                                                            ARGS-IN-DECL-EXPR
                                                            ARGS-CAN-BE-CONT)))
                           (syntax-expr new-start-pos
                                        ARGS-CAN-BE-EXPRS
                                        (append-to-rule-list func-decl
                                                             func-args))))))
              (syntax-scheme start-pos)
              (syntax-expr start-pos ARGS-CAN-BE-EXPRS))))

      (let ((ast (syntax-rule+ syntax-program 0)))
        (and (neq? (get-token-tag token) 'tag-end)
             (add-error ERROR_NO_EOF))
        (print-errors)
        ast))))
