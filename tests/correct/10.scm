(define-syntax eval-i
  (syntax-rules ()
    ((_ x) (eval x (interaction-environment)))))

(define-syntax map-cond
  (syntax-rules ()
    ((_ ((cond-1 value-1) ...)) (cond (cond-1 value-1) ...))))

(define-syntax append-s
  (syntax-rules ()
    ((_ (x ...) (y ...)) (append (list x ...) y ...))))

(define-syntax :and-fold-s
  (syntax-rules ()
    ((_ ()) #t)
    ((_ (x)) x)
    ((_ (x y ...)) (and x (:and-fold-s (y ...))))))

(define (:and-fold xs)
  (or (null? xs)
      (and (car xs)
           (:and-fold (cdr xs)))))

(define (hash f-list a-list)
  (or (null? f-list)
      (and ((eval-i (car f-list)) (car a-list))
           (hash (cdr f-list) (cdr a-list)))))

(define (give-first xs n)
  (if (or (zero? n)
          (null? xs))
      '()
      (cons (car xs) (- n 1))))

(define (multi-list->vector xs)
  (define (helper xs)
    (if (list? xs)
        (apply append (map helper xs))
        (list xs)))
  (list->vector (helper xs)))

(define // quotient)
(define % remainder)
(define ** expt)
(define ++ append)
(define (&& x y) (and x y))

(define (selection-sort pred? xs)
         (define (min-xs xs x)
           (cond ((null? xs)         x)
                 ((pred? (car xs) x) (min-xs (cdr xs) (car xs)))
                 (else               (min-xs (cdr xs) x))))
  
         (define (swap j xs)
           (let ((xj (list-ref xs j))
                 (vs (list->vector xs)))
             (vector-set! vs j (car xs))
             (vector-set! vs 0 xj)
             (vector->list vs)))
  
         (define (ind x xs)
           (- (length xs) (length (member x xs))))
  
         (define (helper xs)
           (if (null? xs)
               '()
               (let ((x (min-xs xs (car xs))))
                 (cons x (helper (cdr (swap (ind x xs) xs)))))))
  
         (helper xs))
(define (insertion-sort pred? xs)
         (define (insert xs ys x)
           (cond ((null? ys) (append xs (list x)))
                 ((pred? (car ys) x) (insert (append xs (list (car ys))) (cdr ys) x))
                 (else               (append xs (list x) ys))))
  
         (define (helper xs ys)
           (if (null? ys)
               xs
               (helper (insert '() xs (car ys)) (cdr ys))))
  
         (helper '() xs))
(selection-sort <= '(9 6 2 4 3 5 7 1 8 0))
(insertion-sort <= '(9 6 2 4 3 5 7 1 8 0))
