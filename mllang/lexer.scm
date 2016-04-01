(define kw '(scheme mod div if zero? eval))

(define (trim? s)
  (or (eqv? s #\space)
      (eqv? s #\newline)
      (eqv? s #\tab)))

(define (get-cut-tag word)
  (assq word '((#\( tag-lprn)
               (#\) tag-rprn)
               (#\[ tag-lbrk)
               (#\] tag-rbrk)
               (#\{ tag-lbrc)
               (#\} tag-rbrc)
               (#\. tag-dot)
               (#\: tag-cln))))

(define (sym-or-new-tag? word)
  (assq word '((#\\ tag-lmbd)
               (#\< tag-lwr)
               (#\% tag-mod)
               (#\^ tag-xor)
               (#\# tag-diez)
               (#\! tag-not))))

(define (sym-old-or-new-tag? word)
  (assq word '((#\- ((tag-sym  tag-sym)
                     (tag-lwr  tag-to)
                     (#f       tag-mns)))
               (#\+ ((tag-sym  tag-sym)
                     (tag-pls  tag-conc)
                     (#f       tag-pls)))
               (#\* ((tag-sym  tag-sym)
                     (tag-mul  tag-pow)
                     (#f       tag-mul)))
               (#\t ((tag-sym  tag-sym)
                     (tag-diez tag-true)
                     (#f       tag-mul)))
               (#\f ((tag-sym  tag-sym)
                     (tag-diez tag-fls)
                     (#f       tag-mul)))
               (#\/ ((tag-sym  tag-sym)
                     (tag-rem  tag-div)
                     (#f       tag-rem)))
               (#\> ((tag-sym  tag-sym)
                     (tag-mns  tag-from)
                     (#f       tag-hghr)))
               (#\| ((tag-sym  tag-sym)
                     (tag-bor  tag-or)
                     (#f       tag-bor)))
               (#&  ((tag-sym  tag-sym)
                     (tag-band tag-and)
                     (#f       tag-band)))
               (#\= ((tag-sym  tag-sym)
                     (tag-lwr  tag-leq)
                     (tag-hghr tag-heq)
                     (tag-not  tag-neq)
                     (#f       tag-eq))))))

(define tokenize
  (let ((line 1)
        (position 1)
        (iscomment #f))
    (lambda (word)
      
      (define (isnum? token)
        (let ((token-word (string->number (get-token-value token))))
          (and token-word
               (vector 'tag-num (get-token-coords token) token-word))))
      
      (define (isnumber?)
        (let ((token-word (string->number word))
              (ws         (string->list word))
              (coords     (vector line position)))
          (and token-word
               ;(x-not-in-list #\/ ws)
               (++ position (length ws))
               (list (vector 'tag-num coords token-word)))))
      
      (define (iskw?)
        (define (helper kw w)
          (and (not (null? kw))
               (or (eq? w (car kw))
                   (helper (cdr kw) w))))
        (let ((coords (vector line position)))
          (and (helper kw (string->symbol word))
               (++ position (length (string->list word)))
               (list (vector 'tag-kw coords word)))))
      
      (define (tag-sym? s token)
        (and (eqv? (get-token-tag token) 'tag-sym)
             (helper s token)))
      
      (define (new-tag? s token new-tag)
        (and (not (get-token-tag token))
             (set-token-tag token new-tag)
             (helper s token)))
      
      (define (sym-or-new-tag s token new-tag)
        (or (tag-sym? s token)
            (new-tag? s token new-tag)))
      
      (define (sym-old-or-new-tag s token old-new-tag)
        (or (tag-sym? s token)
            (let* ((old-tag      (get-token-tag token))
                   (new-tag-assq (assq old-tag old-new-tag))
                   (new-tag      (and new-tag-assq (cadr new-tag-assq))))
              (set-token-tag token new-tag)
              (helper s token))))
      
      (define (cons-tags tag tag-token token s)
        (let* ((tail (helper s token))
               (tag-tail (car tail)))
          #|(write tail)
          (newline)|#
          (set-token-tag tag-token tag)
          (if (vector? tag-tail)
              (if (get-token-tag tag-tail)
                  (cons tag-token tail)
                  (list tag-token))
              (if tag-tail
                  (cons tag-token (list tail))
                  (list tag-token)))))
      
      (define (cut-token-by-word token word)
        (define (helper ws counter)
          (let ((w (car ws))
                (s (cdr ws)))
            ;(print "a" token "b" (list->string (reverse counter)))
            (if (eqv? w word)
                (list (vector (get-token-tag token)
                              (get-token-coords token)
                              (list->string (reverse counter)))
                      (vector #f (vector line (- position 1)) word)
                      (vector #f (vector line position) (list->string s)))
                (helper s (cons w counter)))))
        (helper (string->list (get-token-value token)) '()))
      
      (define (cut-by-tag w s token cut-tag)
        (let* ((old-tag (get-token-tag token))
               (cuted-list (cut-token-by-word token w))
               (before     (car   cuted-list))
               (center     (cadr  cuted-list))
               (after      (caddr cuted-list))
               (nafter     (isnum? after))
               (consed     (if nafter
                               (list (set-token-tag center cut-tag) nafter)
                               (cons-tags cut-tag center after s))))
          (if old-tag
              (cons (or (isnum? before)
                        (set-token-tag before old-tag))
                    consed)
              consed)))
      
      (define (helper ws token)
        (if (null? ws)
            (list token)
            (let* ((w (car ws))
                   (s (cdr ws))
                   (sont  (sym-or-new-tag? w))
                   (ssont (sym-old-or-new-tag? w))
                   (ctag  (get-cut-tag w)))
              (++ position)
              (cond ((eqv? w #\newline) (and (set! iscomment #f)
                                             (set! position 1)
                                             (++ line)
                                             '()))
                    (iscomment          (and (++ position (length s))
                                             '()))
                    ((eqv? w #\tab)     (and (++ position TAB1)
                                             '()))
                    ((eqv? w #\space)   '())
                    ((eqv? w #\;)       (and (set! iscomment #t)
                                             '()))
                    (sont               (sym-or-new-tag s token (cadr sont)))
                    (ssont              (sym-old-or-new-tag s token (cadr ssont)))
                    (ctag               (cut-by-tag w s token (cadr ctag)))
                    (else               (helper s (set-token-tag token 'tag-sym)))))))
      
      (or (iskw?)
          (isnumber?)
          (helper (string->list word) (vector #f (vector line position) word))))))

(define (tokenize-file file)
  (define (add-word word words)
    (if (null? word)
        words
        (cons (list->string (reverse word)) words)))
  
  (define (read-words word words)
    (let ((ch (read-char file)))
      (or (and (eof-object? ch)
               (add-word word words))
          (and (trim? ch)
               (read-words '() (cons (string ch) (add-word word words))))
          (read-words (cons ch word) words))))
  
  (define (tokenize-words words tokens)
    (if (null? words)
        tokens
        (let ((t (tokenize (car words))))
          (tokenize-words (cdr words) (append tokens t)))))
  
  (tokenize-words (reverse (read-words '() '())) '()))