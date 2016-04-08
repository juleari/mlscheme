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