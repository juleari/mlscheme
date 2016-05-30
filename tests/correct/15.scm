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

(define (range . :args) (map-cond (((and ((lambda (:x) (= :x 3)) (length :args)) (hash (quote ((lambda (x) #t) (lambda (x) #t) (lambda (x) #t))) :args) ((lambda :args #t) :args)) (apply (lambda (a b d) (begin (map-cond (((< a b) (++ (list a) (range (+ a d) b d))) (#t (quote ())))))) :args)))))
(define (flatten . :args) (map-cond (((and ((lambda (:x) (= :x 1)) (length :args)) (hash (quote ((lambda (:x) (and (list? :x) (null? :x))))) :args) ((lambda :args #t) :args)) (apply (lambda (:g_) (let () (begin (quote ())))) :args)) ((and ((lambda (:x) (= :x 1)) (length :args)) (hash (quote ((lambda (:x) (and (list? :x) (:and-fold (cons (>= (length :x) 1) (map (lambda (:lambda-i :xi) ((eval-i :lambda-i) :xi)) (quote ((lambda (x) #t))) (give-first :x 1)))))))) :args) ((lambda :args #t) :args)) (apply (lambda (:g__) (let ((x (car :g__)) (xs (cdr :g__))) (begin (++ (flatten x) (flatten xs))))) :args)) ((and ((lambda (:x) (= :x 1)) (length :args)) (hash (quote ((lambda (x) #t))) :args) ((lambda :args #t) :args)) (apply (lambda (x) (begin (list x))) :args)))))
(define (element? . :args) (map-cond (((and ((lambda (:x) (= :x 2)) (length :args)) (hash (quote ((lambda (x) #t) (lambda (:x) (and (list? :x) (null? :x))))) :args) ((lambda :args #t) :args)) (apply (lambda (x :g___) (let () (begin #f))) :args)) ((and ((lambda (:x) (= :x 2)) (length :args)) (hash (quote ((lambda (x) #t) (lambda (:x) (and (list? :x) (:and-fold (cons (>= (length :x) 1) (map (lambda (:lambda-i :xi) ((eval-i :lambda-i) :xi)) (quote ((lambda (x) #t))) (give-first :x 1)))))))) :args) ((lambda :args (let ((:v-args (multi-list->vector :args))) (:and-fold-s ((equal? (vector-ref :v-args 0) (vector-ref :v-args 1)))))) :args)) (apply (lambda (x :g____) (let ((:g_____ (car :g____)) (xs (cdr :g____))) (begin #t))) :args)) ((and ((lambda (:x) (= :x 2)) (length :args)) (hash (quote ((lambda (x) #t) (lambda (:x) (and (list? :x) (:and-fold (cons (>= (length :x) 1) (map (lambda (:lambda-i :xi) ((eval-i :lambda-i) :xi)) (quote ((lambda (x) #t))) (give-first :x 1)))))))) :args) ((lambda :args #t) :args)) (apply (lambda (x :g______) (let ((y (car :g______)) (ys (cdr :g______))) (begin (element? x ys)))) :args)))))
(define (filter . :args) (map-cond (((and ((lambda (:x) (= :x 2)) (length :args)) (hash (quote ((lambda (x) #t) (lambda (:x) (and (list? :x) (null? :x))))) :args) ((lambda :args #t) :args)) (apply (lambda (pred? :g_______) (let () (begin (quote ())))) :args)) ((and ((lambda (:x) (= :x 2)) (length :args)) (hash (quote ((lambda (x) #t) (lambda (:x) (and (list? :x) (:and-fold (cons (>= (length :x) 1) (map (lambda (:lambda-i :xi) ((eval-i :lambda-i) :xi)) (quote ((lambda (x) #t))) (give-first :x 1)))))))) :args) ((lambda :args #t) :args)) (apply (lambda (pred? :g________) (let ((x (car :g________)) (xs (cdr :g________))) (begin (map-cond (((pred? x) (++ (list x) (filter pred? xs))) (#t (filter pred? xs))))))) :args)))))
(range 0 11 3)
(flatten (list (list 1) 2 (list 3 (list 4 5) (list 6 (list 7 8))) 9))
(element? 1 (list 3 2 1))
(element? 4 (list 3 2 1))
(filter odd? (apply range (list 0 10 1)))
(filter (lambda (x) (= (% x 3) 0)) (apply range (list 0 13 1)))
