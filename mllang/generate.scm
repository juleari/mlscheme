(define gen-file (open-output-file "generated.sm"))

;; в рабочей версии вместо genbase.sm должен быть задаваемый в compiler.py путь
(define (prepare-gen-file)
  (let ((lib-funcs-file (open-input-file "genbase.sm")))
    (while (not (eof-object? (peek-char lib-funcs-file)))
           (display (read-char lib-funcs-file) gen-file))))

(prepare-gen-file)

(define (to-gen-file text)
  (display text gen-file)
  (newline gen-file))

(define (calc-rpn xs)
  (define (helper stack xs)
    (if (null? xs)
        (car stack)
        (let ((x (car xs))
              (s (cdr xs)))
          (if (number? x)
              (helper (cons x stack) s)
              (if (is-op? x)
                  (helper (cons `(,(string->symbol x) ,(cadr stack) ,(car stack))
                                (cddr stack))
                          s)
                  (if (list? x)
                      (helper (cons (map toSymb x) stack) s)
                      (helper (cons (string->symbol x) stack) s)))))))
  (helper '() xs))

(define (generate-let-var name exprs inner)
  `(letrec ((,name ,exprs))
     ,inner))

(define (generate-let defs exprs)
  (if (null? defs)
      exprs
      (let* ((def (car defs))
             (name (string->symbol (car def)))
             (types (cdr def)))
        (if (is-variable types)
            (generate-let-var name
                              (get-exprs-from-type (car type))
                              (generate-let (cdr defs) exprs))
            (generate-let-func name types (generate-let (cdr defs) exprs))))))

(define (is-variable types)
  (and (eq? (length types) 1)
       (zero? (length (get-args-names-from-type (car types))))))

(define (generate-let-var name exprs inner)
  `(letrec ((,name ,(calc-rpn (car exprs))))
     ,inner))

(define (generate-let defs type)
  (if (null? defs)
      (cons 'begin
            (map generate-expr (get-exprs-from-type type)))
      (let* ((def (car defs))
             (name (string->symbol (car def)))
             (types (cdr def)))
        (if (is-variable types)
            (generate-let-var name
                              (get-exprs-from-type (car types))
                              (generate-let (cdr defs) type))
            (generate-let-func name types (generate-let (cdr defs) type))))))

(define (generate-expr expr)
  (calc-rpn expr))

(define (generate-def def)
  (let* ((name (string->symbol (car def)))
         (types (cdr def)))
    (to-gen-file `(define (,name . :args)
                  (map-cond ,(map (lambda (type)
                                    (let* ((lambda-and-let (make-var-lists type))
                                           (lambda-list (car lambda-and-let))
                                           (lambda-let  (cdr lambda-and-let)))
                                      `((and (,(get-args-num-from-type type) (length :args))
                                             (hash ',(get-args-check-from-type type) :args))
                                        (apply (lambda ,lambda-list
                                                 ,(if (not-null? lambda-let)
                                                      `(let ,(car lambda-let)
                                                         ,(generate-let (get-defs-from-type type)
                                                                        type))
                                                      (generate-let (get-defs-from-type type)
                                                                    type)))
                                               :args))))
                                  types))))))

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