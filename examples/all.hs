; count days of week
day-of-week day month year <-
    a <- (14 - month) // 12
    y <- year - a
    m <- month + (a * 12) - 2

    (7000 + day + y + (y // 4) + (y // 400) + (31 * m // 12) 
        - (y // 100)) % 7

day-of-week 17 5 2016
day-of-week 10 4 2016
day-of-week 29 3 2016
day-of-week 20 4 2016

0->1 [] <- []
0->1 [0 : xs] <- [1 : 0->1 xs]
0->1 [x : xs] <- [x : 0->1 xs]

0->1 [0 2 7 0 5]
0->1 [0 1 0 1 0]

; count x in xs
count x []         <- 0
count x [ x : xs ] <- 1 + count x xs
count x [ y : xs ] <- count x xs
;повторные переменные в образцах

count 1 [1 2 3 1]
count 0 [1 2 3 4]
count 5 [1 2 3 4 5 5 5]

fact 0 <- 1
fact n <- n * fact (n - 1)

fact 5
fact 10

sum <- 0
sum x : xs <- x + sum . xs

sum 1 2 3 4
sum.[5 6 7 8 9 10]

replace pred? proc []         <- []
replace pred? proc [ x : xs ] <-
    if | pred? x -> [ proc.[x] : replace pred? proc xs ]
       |         -> [ x : replace pred? proc xs ]

replace zero? 
        \ x -> x + 1
        [ 0 1 2 3 0 ]

replace odd?
        \ x -> x * 2
        [ 1 2 3 4 5 6 ]

replace \ x -> 0 > x
        exp
        [ 0 1 -1 2 -2 3 -3]

replicate x 0 <- []
replicate x n <- [ x : replicate x ( n - 1 ) ]

replicate "a" 5
replicate [ "a" "b" ] 3
replicate "a" 0

cycle xs 0 <- []
cycle xs n <- xs ++ cycle xs ( n - 1 )

cycle [ 0 1 ] 3
cycle [ 'a 'b 'c ] 5
cycle [] 0

and-fold        <- #t
and-fold x : xs <- x && and-fold : xs

and-fold #f #f #f
and-fold #f #f #t
and-fold #f #t #t
and-fold #t #t #t
and-fold

0? 0 <- #t
0? x <- #f

nil? 0 <- #t
nil? [] <- #t
nil? x <- #f

%2=0? x <- x % 2 = 0
%2!0? x <- ! %2=0? x

0? 0
0? 1

nil? 0
nil? 1
nil? []
nil? [1 2 3]

%2=0? 4
%2=0? 5

%2!0? 1
%2!0? 2

scheme (define (selection-sort pred? xs)
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

scheme (define (insertion-sort pred? xs)
         (define (insert xs ys x)
           (cond ((null? ys) (append xs (list x)))
                 ((pred? (car ys) x) (insert (append xs (list (car ys))) (cdr ys) x))
                 (else               (append xs (list x) ys))))
  
         (define (helper xs ys)
           (if (null? ys)
               xs
               (helper (insert '() xs (car ys)) (cdr ys))))
  
         (helper '() xs))

scheme (selection-sort <= '(9 6 2 4 3 5 7 1 8 0))
scheme (insertion-sort <= '(9 6 2 4 3 5 7 1 8 0))

a <- "long string"
replicate a 8

my-gcd a b <-
    r <- a % b

    if  | a < b -> my-gcd b a
        | 0? r  -> b
        |       -> my-gcd b r

my-lcm a b <-
    abs (a * b / my-gcd a b)

prime? n <-
    n! 0 <- 1
    n! n <- n * n! (n - 1)
    
    0?.[ (n!.[n - 1] + 1) % n ]

my-gcd 3542 2464
my-lcm 3 4
prime? 11
prime? 12
prime? 3571
