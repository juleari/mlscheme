(define gen-file (open-output-file "generated.scm"))

(define (prepare-gen-file)
  (let ((lib-funcs-file (open-input-file "genbase.sm")))
    (while (not (eof-object? (peek-char lib-funcs-file)))
           (display (read-char lib-funcs-file) gen-file))))

(prepare-gen-file)

(define (to-gen-file text)
  (display text gen-file)
  (newline gen-file))

(define (string-op->symbol x stack)
  (cond ((equal? x "!") (list (cons 'not stack)))
        (else           (cons (string->symbol x) stack))))

(define (calc-rpn xs)
  (define (helper stack xs)
    ;(print 'calc-rpn xs stack)
    (if (null? xs)
        (car stack)
        (let ((x (car xs))
              (s (cdr xs)))
          (cond ((number? x) (helper (cons x stack) s))
                ((is-op? x)  (helper (cons `(,(string->symbol x) ,(cadr stack) ,(car stack))
                                           (cddr stack))
                                     s))
                ((is-uop? x) (helper (cons `(,(string->symbol x) ,(car stack))
                                           (cdr stack))
                                     s))
                ((list? x)   (helper (cons (func-apply x) stack) s))
                ((string? x) (helper (string-op->symbol x stack) s))
                ((x-in-xs? x #t #f) (helper (cons x stack) s))
                (else        (helper stack s))))))
  (helper '() xs))

(define (is-variable types)
  (and (eq? (length types) 1)
       (zero? (length (get-args-names-from-type (car types))))))

(define (generate-let-var name exprs inner)
  `(letrec ((,name ,(calc-rpn (car exprs))))
     ,inner))

(define (generate-func-types types)
  `(:map-cond ,(append
                (map (lambda (type)
                       (let* ((lambda-and-let (make-var-lists type))
                              (lambda-list (car lambda-and-let))
                              (lambda-let  (cdr lambda-and-let))
                              (hash-args (get-args-for-check ':args type)))
                         ;(print 'generate-def type)
                         `((and (,(get-args-num-from-type type) (length :args))
                                (:hash ',(get-args-check-from-type type) ,hash-args)
                                (,(get-similar-from-type type) :args))
                           (apply (lambda ,lambda-list
                                    ,(if (not-null? lambda-let)
                                         `(let ,(car lambda-let)
                                            ,(generate-let (get-defs-from-type type)
                                                           type))
                                         (generate-let (get-defs-from-type type)
                                                       type)))
                                  :args))))
                     types)
                `,(list (list 'else ':ERROR_FUNC_CALL)))))

(define (generate-let-func name memo-name types inner)
  (if memo-name
      `(letrec ((,name (lambda :args
                         (let ((:m (assoc :args ,memo-name)))
                           (if :m
                               (cadr :m)
                               (let ((:res ,(generate-func-types types)))
                                 (set! ,memo-name
                                       (cons (list :args :res) ,memo-name))
                                 :res))))))
         ,inner)
      `(letrec ((,name (lambda :args ,(generate-func-types types))))
         ,inner)))

(define (generate-let defs type)
  (if (null? defs)
      (let ((exprs (get-exprs-from-type type)))
        (if (> (length exprs) 1)
            (cons 'begin
                  (map generate-expr exprs))
            (generate-expr (car exprs))))
      (let* ((def (car defs))
             (name (string->symbol (car def)))
             (memo-and-types (cdr def))
             (is-memo? (car memo-and-types))
             (types (cdr memo-and-types)))
        ;(print name types)
        (if (is-variable types)
            (generate-let-var name
                              (get-exprs-from-type (car types))
                              (generate-let (cdr defs) type))
            (generate-let-func name is-memo? types (generate-let (cdr defs) type))))))

(define (generate-expr expr)
  (calc-rpn expr))

(define (check-expr-or-func type)
  ((:eval-i (get-args-num-from-type type)) 0))

(define (generate-memo-let memo-list)
  (if (list? memo-list)
      (map (lambda (name) (list name `(list))) memo-list)
      (list (list memo-list `(list)))))

(define (generate-memo-lambda memo-name inner)
  `(lambda :args
     (let ((:m (assoc :args ,memo-name)))
       (if :m
           (cadr :m)
           (let ((:res ,inner))
             (set! ,memo-name
                   (cons (list :args :res)
                         ,memo-name))
             :res)))))

(define (generate-def def)
  (let* ((name (string->symbol (car def)))
         (memo-and-types (cdr def))
         (is-memo? (car memo-and-types))
         (types (cdr memo-and-types)))
    ;(print name types)
    (to-gen-file (if (:and-fold (map check-expr-or-func types))
                     (let* ((type (car (reverse types)))
                            (exprs (get-exprs-from-type type)))
                       (if (not-null? exprs)  
                           `(define ,name
                              ,(generate-let (get-defs-from-type type)
                                             type))
                           ""))
                     (if is-memo?
                         `(define ,name
                            (let ,(generate-memo-let is-memo?)
                              ,(if (and (list? is-memo?)
                                        (not (x-in-xs? ':memo is-memo?)))
                                   `(lambda :args ,(generate-func-types types))
                                   (generate-memo-lambda ':memo
                                                         (generate-func-types types)))))
                         `(define (,name . :args)
                            ,(generate-func-types types)))))))

(define (generate-defs defs)
  (map generate-def defs))

(define (calc-expr expr)
  (to-gen-file (generate-expr (list expr))))

(define (calc-exprs exprs)
  (map calc-expr exprs))

(define (generate model)
  (let* ((defs (car model))
         (exprs (cadr model)))
    (generate-defs defs)
    (calc-exprs exprs)
    (close-output-port gen-file)))
