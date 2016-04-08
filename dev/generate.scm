;; examp
(define v
  '((("day-of-week"
      #(#((lambda (x) (eq? x 3))
          ((lambda (x) #t)
           (lambda (x) #t)
           (lambda (x) #t))
          ("day" "month" "year"))
        (("a"
          #(#((lambda (x) (zero? x)) () ())
            ()
            ((14 "month" "-" 12 "//"))))
         ("y"
          #(#((lambda (x) (zero? x)) () ())
            ()
            (("year" "a" "-"))))
         ("m"
          #(#((lambda (x) (zero? x)) () ())
            ()
            (("month" "a" 12 "*" "+" 2 "-")))))
        ((7000
          "day"
          "+"
          "y"
          "+"
          "y"
          4
          "//"
          "+"
          "y"
          400
          "//"
          "+"
          31
          "m"
          "*"
          12
          "//"
          "+"
          "y"
          100
          "//"
          "-"
          7
          "%")))))
    ()))
;; end examp

;; defs
(define R-NAME 0)
(define R-TERMS 1)

(define T-TAG 0)
(define T-COORDS 1)
(define T-VALUE 2)

(define R-NAME 0)
(define R-TERMS 1)

(define TYPE-ARGS 0)
(define TYPE-ARGS-NUM 0)
(define TYPE-ARGS-CHECK 1)
(define TYPE-ARGS-NAMES 2)
(define TYPE-DEFS 1)
(define TYPE-EXPRS 2)
(define TYPE-ARGS-NAMES 2)
;; end defs

;; lib
(define-syntax eval-i
  (syntax-rules ()
    ((_ x) (eval x (interaction-environment)))))

(define (get-rule-name rule)
  (vector-ref rule R-NAME))

(define (get-rule-terms rule)
  (vector-ref rule R-TERMS))

(define (get-args-from-type type)
  (vector-ref type TYPE-ARGS))

(define (get-args-num-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-NUM))

(define (get-args-names-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-NAMES))
  
(define (get-args-check-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-CHECK))

(define (to-sym xs)
  (map string->symbol xs))

(define (multy-apply funcs arrs)
  (or (null? funcs)
      (cons ((car funcs) (car arrs)) (multy-apply (cdr funcs) (cdr arrs)))))
      
(define (hash f-list a-list)
  (or (null? f-list)
      (and ((car f-list) (car a-list))
           (hash (cdr f-list) (cdr a-list)))))
;; end lib

(define (calc-rpn xs)
  (define (helper stack xs)
    (if (null? xs)
        (car stack)
        (let ((x (car xs))
              (s (cdr xs)))
          (if (number? x)
              (helper (cons x stack) s)
              (helper (cons (eval `(,x ,(cadr stack) ,(car stack))
                                  (interaction-environment))
                            (cddr stack))
                      s)))))
  (helper '() xs))
  
(define (is-variable types)
  (and (eq? (length types) 1)
       (zero? (length (get-args-names-from-type (car types))))))
       
(define (generate-let-var name exprs inner)
  `(let-rec ((,name ,exprs))
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

(define (generate-def def)
  (let* ((name (string->symbol (car def)))
         (types (cdr def)))
    (write `(define (,name . :args)
              (cond ,(map (lambda (type)
                            `(and (,(get-args-num-from-type type) (length :args))
                                  (,(hash (get-args-check-from-type type) :args)))
                            ((lambda ,(to-sym (get-args-names-from-type type))
                               (generate-let (get-defs-from-type type)
                                             ,(map generate-expr (get-exprs-from-type type))))
                             :args)))
                          types)))))

(define (generate-defs defs)
  (map generate-def defs))

(define (generate model)
  (let* ((defs (car model)))
    (generate-defs defs)))
