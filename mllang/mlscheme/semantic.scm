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
          ;(print 'add-list func-name memo-list func-val)
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
          ;(print type elem)
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

      ;(print 'ast ast)
      (let* ((model (add-func-in-model '()
                                        "print"
                                        #f
                                        (vector (vector `(lambda (:x) #t) '() '() `(lambda :args #t)) '() '())))
             (m (semantic-program ast model '())))
        (and (print-errors) m)))))
