(define-syntax eval-i
  (syntax-rules ()
    ((_ x) (eval x (interaction-environment)))))

(define-syntax map-cond
  (syntax-rules ()
    ((_ ((cond-1 value-1) ...)) (cond (cond-1 value-1) ...))))

(define-syntax append-s
  (syntax-rules ()
    ((_ (x ...) (y ...)) (append (list x ...) y ...))))

(define-syntax and-fold-s
  (syntax-rules ()
    ((_ ()) #t)
    ((_ (x)) x)
    ((_ (x y ...)) (and x (and-fold-s (y ...))))))

(define (and-fold xs)
  (or (null? xs)
      (and (car xs)
           (and-fold (cdr xs)))))

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
(define (count . :args) (map-cond (((and ((lambda (:x) (= :x 2)) (length :args)) (hash (quote ((lambda (x) #t) (lambda (:x) (and (list? :x) (null? :x))))) :args) ((lambda :args #t) :args)) (apply (lambda (x :g_) (let () (begin 0))) :args)) ((and ((lambda (:x) (= :x 2)) (length :args)) (hash (quote ((lambda (x) #t) (lambda (:x) (and (list? :x) (and-fold (cons (>= (length :x) 1) (map (lambda (:lambda-i :xi) ((eval-i :lambda-i) :xi)) (quote ((lambda (x) #t))) (give-first :x 1)))))))) :args) ((lambda :args (let ((:v-args (multi-list->vector :args))) (and-fold-s ((equal? (vector-ref :v-args 0) (vector-ref :v-args 1)))))) :args)) (apply (lambda (x :g__) (let ((:g___ (car :g__)) (xs (cdr :g__))) (begin (+ 1 (count x xs))))) :args)) ((and ((lambda (:x) (= :x 2)) (length :args)) (hash (quote ((lambda (x) #t) (lambda (:x) (and (list? :x) (and-fold (cons (>= (length :x) 1) (map (lambda (:lambda-i :xi) ((eval-i :lambda-i) :xi)) (quote ((lambda (x) #t))) (give-first :x 1)))))))) :args) ((lambda :args #t) :args)) (apply (lambda (x :g____) (let ((y (car :g____)) (xs (cdr :g____))) (begin (count x xs)))) :args)))))
(count 1 (quote (1 2 3 1)))
(count 0 (quote (1 2 3 4)))
(count 5 (quote (1 2 3 4 5 5 5)))
