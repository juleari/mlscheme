(define mod quotient)
(define div remainder)
(define ** expt)

(define T-TAG 0)
(define T-COORDS 1)
(define T-VALUE 2)

(define A-NAME 0)

(define ERROR_NO_FUNC_BODY "there is no body")
(define ERROR_NO_EOF       "no end of file")
(define ERROR_EXPR_ARENS   "arens not closed")
(define ERROR_EXPR         "expr is not correct")

(define-syntax neq?
  (syntax-rules ()
    ((_ x y) (not (eqv? x y)))))

(define-syntax not-null?
  (syntax-rules ()
    ((_ x) (not (null? x)))))

(define (print . xs)
  (and (not-null? xs)
       (display (car xs))
       (newline)
       (apply print (cdr xs))))

(define tokens '(#(tag-sym #(2 1) "day-of-week")
                 #(tag-sym #(2 13) "day")
                 #(tag-sym #(2 17) "month")
                 #(tag-sym #(2 23) "year")
                 #(tag-to #(2 28) "<-")
                 #(tag-sym #(3 5) "a")
                 #(tag-to #(3 7) "<-")
                 #(tag-lprn #(3 10) #\()
                 #(tag-sym #(3 11) "14")
                 #(tag-mns #(3 14) "-")
                 #(tag-sym #(3 16) "month")
                 #(tag-rprn #(3 21) #\))
                 #(tag-kw #(3 23) "mod")
                 #(tag-num #(3 27) 12)))

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

(define syntax
  (let ((errors '())
        (token (get-token))
        (ast '()))
    (lambda ()
      
      (define (add-error error-type)
        (set! errors
              (cons (vector 'error: (get-token-coords token) '- error-type)
                    errors))
        '())
      
      (define (add-error-rule rule)
        (set! errors
              (cons (vector 'error: (get-token-coords token) '- 'expected `,rule)
                    errors))
        '())
      
      (define (print-errors)
        (if (null? errors)
            (display "SYNTAX OK!")
            (begin (display "SYNTAX ERRORS:")
                   (newline)
                   (apply print errors)))
        (newline)
        (newline))
      
      (define (next-token)
        (set! token (get-token)))
      
      (define (is-type? . types)
        (apply x-in-xs? (cons (get-token-tag token) types)))
      
      (define (x-in-xs? x . xs)
        (and (not-null? xs)
             (or (eqv? x (car xs))
                 (apply x-in-xs? (cons x (cdr xs))))))
      
      (define (syntax-rule? rule)
        (or (rule) '()))
      
      (define (syntax-rule* rule)
        (define (helper ast-list)
          (let ((ast-elem (rule)))
            (if ast-elem
                (helper (cons ast-elem ast-list))
                (reverse ast-list))))
        (helper '()))
      
      (define (syntax-rule+ rule)
        (define (helper ast-list)
          (let ((ast-elem (rule)))
            (if ast-elem
                (helper (cons ast-elem ast-list))
                (reverse ast-list))))
        
        (let ((first (rule)))
          (and first
               (cons first (helper '())))))
      
      (define (simple-rule rule-name . tags)
        (let ((t token))
          (and (apply is-type? tags)
               (next-token)
               (vector `,rule-name `,t))))
      
      (define (syntax-array-simple)
        (define ast-list '())
        
        (let ((first-rule (simple-rule 'open-braket 'tag-lbrk)))
          (and first-rule
               (set! ast-list (cons first-rule (syntax-arguments)))
               (or (let ((last-rule (simple-rule 'close-braket 'tag-rbrk)))
                     (and last-rule
                          (set! ast-list (append ast-list (list last-rule)))))
                   (add-error ERR_NO_CLOSE_BRK))
               (vector 'array-simple ast-list))))
      
      (define (syntax-continious)
        (let ((first-rule (simple-rule 'colon 'tag-cln)))
          (and first-rule
               (let ((last-rule (simple-rule 'list 'tag-sym)))
                 (vector 'continious
                         (list first-rule last-rule))))))
      
      (define (syntax-arg-continious)
        (let ((cont (syntax-continious)))
          (and cont
               (or (not (syntax-argument))
                   (add-error ERR_AFTER_CONTINIOUS))
               (set! ast-list (list cont)))))
      
      (define (syntax-argument)
        (let* ((arg (or (simple-rule 'simple-argument 'tag-num 'tag-sym)
                        (syntax-array-simple)
                        (syntax-arg-continious)))
               (larg (if (list? arg) arg (list arg))))
          (and arg (vector 'argument larg))))
      
      (define (syntax-arguments)
        (define (helper ast-list)
          (let ((arg (syntax-argument)))
            (or (and arg (helper (cons arg ast-list)))
                (reverse ast-list))))
        (helper '()))
      
      (define (syntax-func-declaration)
        (let ((first-rule (simple-rule 'func-name 'tag-sym)))
          (and first-rule
               (vector 'func-decl
                       (cons first-rule
                             (syntax-rule? syntax-arguments))))))
      
      (define (syntax-func-body)
        (let ((first-rule (simple-rule 'func-to 'tag-to)))
          (and first-rule
               (cons first-rule
                     (or (syntax-rule+ syntax-program)
                         (add-error ERROR_NO_FUNC_BODY))))))
      
      (define (syntax-expr . xs-func-decl)
        (and (neq? (get-token-tag token) 'tag-end)
             (apply shunting-yard xs-func-decl)))
      #|      (get-token)
                 #(expr))
            (and (not-null? xs-func-decl)
                 (car xs-func-decl)))|#
      
      (define (shunting-yard . out)
        (define stack '())
        
        (define (get-name ast)
          (and (vector? ast)
               (> (vector-length ast) 0)
               (vector-ref ast A-NAME)))
        
        (define (proc? cur)
          (eq? (get-name cur) 'func-decl))
        
        (define (op? t)
          (x-in-xs? (get-token-tag t)
                    'tag-bor 'tag-band 'tag-xor 'tag-and 'tag-or
                    'tag-neq 'tag-hghr 'tag-lwr 'tag-heq 'tag-leq
                    'tag-pls 'tag-mns  'tag-mul 'tag-div 'tag-eq
                    'tag-mod 'tag-rem  'tag-pow 'tag-not))
        
        (define (is-type? . types)
          (apply x-in-xs? (cons (get-token-tag token) types)))
        
        (define (x-in-xs? x . xs)
          (and (not-null? xs)
               (or (eqv? x (car xs))
                   (apply x-in-xs? (cons x (cdr xs))))))
        
        (define (trigonometric)
          (let ((tag (get-token-tag token))
                (val (get-token-value token)))
            (and (eqv? tag 'tag-kw)
                 (x-in-xs? val "sin" "cos" "tg" "ctg"))))
        
        (define (prior t)
          (let ((tag (get-token-tag t)))
            (cond ((x-in-xs? tag 'tag-bor)                             1)
                  ((x-in-xs? tag 'tag-band)                            2)
                  ((x-in-xs? tag 'tag-or)                              3)
                  ((x-in-xs? tag 'tag-xor)                             4)
                  ((x-in-xs? tag 'tag-and)                             5)
                  ((x-in-xs? tag 'tag-neq 'tag-eq)                     6)
                  ((x-in-xs? tag 'tag-lwr 'tag-leq 'tag-hghr 'tag-heq) 7)
                  ((x-in-xs? tag 'tag-pls 'tag-mns)                    8)
                  ((x-in-xs? tag 'tag-mul 'tag-div 'tag-mod 'tag-rem)  9)
                  ((x-in-xs? tag 'tag-pow)                            10)
                  ((or (x-in-xs? tag 'tag-not) (trigonometric))       11)
                  (else                                                0))))
        
        (define (try-get)
          (let ((tag  (get-token-tag token))
                (op   (prior token))
                (proc (syntax-func-declaration))
                (flag #f))
            (cond ((> op 0)             (op-to-out op)
                                        (set! stack (cons token stack)))
                  (proc                 (set! flag #t)
                                        (set! out (cons proc out)))
                  ((eqv? tag 'tag-num)  (set! out (cons token out)))
                  ((eqv? tag 'tag-lprn) (set! stack (cons token stack)))
                  ((eqv? tag 'tag-rprn) (op-before-laren-to-out 0)))
            ;(print stack out "\n")
            (if flag
                (try-get)
                (and (next-token)
                     (neq? (get-token-tag token) 'tag-end)
                     (try-get)))))
        
        (define (op-before-laren-to-out p)
          (if (not-null? stack)
              (let ((cur (car stack)))
                ;(print "op-before-laren-to-out" (get-token-tag cur) "\n")
                (cond ((eqv? (get-token-tag cur) 'tag-lprn)
                       (set! stack (cdr stack)))
                      ((op? cur) (from-stack-to-out cur op-before-laren-to-out p))
                      (else      (add-error ERROR_EXPR_ARENS))))))
        
        (define (from-stack-to-out cur callback p)
          (set! out (cons cur out))
          (set! stack (cdr stack))
          (callback p))
        
        (define (op-to-out p)
          (if (not-null? stack)
              (let ((cur (car stack)))
                (if (and (neq? (get-token-tag cur) 'tag-lprn)
                         (<= (prior cur) p))
                    (from-stack-to-out cur op-to-out p)))))
        
        (define (ops-to-out p)
          (if (not-null? stack)
              (let ((cur (car stack)))
                (if (op? cur)
                    (from-stack-to-out cur ops-to-out p)
                    (add-error ERROR_EXPR)))))
        
        (try-get)
        (ops-to-out 0)
        (reverse out))
      
      (define (syntax-program)
        (let ((func-decl (syntax-func-declaration)))
          (or (and func-decl
                   (let ((func-body (syntax-func-body)))
                     (or (and func-body
                              (vector 'func-def
                                      (cons func-decl func-body)))
                         (syntax-expr func-decl))))
              (syntax-expr))))
      
      ; (syntax-rule+ syntax-program)
      ; (syntax-func-declaration)
      (let ((ast (syntax-program)))
        (and (neq? (get-token-tag token) 'tag-end)
             (add-error ERROR_NOT_EOF))
        (print-errors)
        ast))))