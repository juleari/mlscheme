;; defs
(define R-NAME 0)
(define R-TERMS 1)
;; end defs

;; lib
(define (get-rule-name rule)
  (vector-ref rule R-NAME))

(define (get-rule-terms rule)
  (vector-ref rule R-TERMS))
;; end lib

(define (generate ast)
  (let* ((rule (car ast))
         (name (get-rule-name rule))
         (terms (get-rule-terms rule)))
    (cond ((eq? name 'func-def) (generate (cdr ast) (generate-func rule))))))