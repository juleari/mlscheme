(define-syntax eval-i
  (syntax-rules ()
    ((_ x) (eval x (interaction-environment)))))

(define-syntax map-cond
  (syntax-rules ()
    ((_ ((cond-1 value-1) ...)) (cond (cond-1 value-1) ...))))

(define-syntax append-s
  (syntax-rules ()
    ((_ (x ...) (y ...)) (append (list x ...) y ...))))

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

(define // quotient)
(define % remainder)
(define ** expt)
(define (fub . :args) (map-cond (((and ((lambda (:x) (= :x 1)) (length :args)) (hash (quote ((lambda (:x) (and (list? :x) (null? :x))))) :args)) (apply (lambda (:g_) (let () (begin (quote ())))) :args)) ((and ((lambda (:x) (= :x 1)) (length :args)) (hash (quote ((lambda (:x) (and (list? :x) (and-fold (cons (>= (length :x) 1) (map (lambda (:lambda-i :xi) ((eval-i :lambda-i) :xi)) (quote ((lambda (x) (eqv? x 0)))) (give-first :x 1)))))))) :args)) (apply (lambda (:g__) (let ((:g___ (car :g__)) (xs (cdr :g__))) (begin (append-s (1) ((fub xs)))))) :args)) ((and ((lambda (:x) (= :x 1)) (length :args)) (hash (quote ((lambda (:x) (and (list? :x) (and-fold (cons (>= (length :x) 1) (map (lambda (:lambda-i :xi) ((eval-i :lambda-i) :xi)) (quote ((lambda (x) #t))) (give-first :x 1)))))))) :args)) (apply (lambda (:g____) (let ((x (car :g____)) (xs (cdr :g____))) (begin (append-s (x) ((fub xs)))))) :args)))))
(fub (quote (0 2 7 0 5)))
(fub (quote (0 1 0 1 0)))
