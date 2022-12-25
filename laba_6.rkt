#|
<Дробь> ::= <Число-со-знаком> "/" <Число>
<Число-со-знаком> ::= <Знак> <Число> | <Число>
<Число> ::= <Цифра> | <Число> <Цифра>
<Знак> ::= "-" | "+"
<Цифра> ::= 1|2|3|4|5|6|7|8|9|0
|#



(define (check-frac str)
  (define (numb? symb)
    (let loop ((arr (string->list "0123456789")))
      (and (not (null? arr))
           (or (eq? (car arr) symb) (loop (cdr arr))))))
  
  (define (scan-first-num arr)
    (and arr
         (not (null? arr))
         (numb? (car arr))
         (let loop ((arr (cdr arr)))
           (cond 
             ((null? arr) #f)
             ((eq? #\/ (car arr)) (cdr arr))
             ((numb? (car arr)) (loop (cdr arr)))
             (else #f)))))
  
  (define (scan-second-num arr)
    (and arr
         (not (null? arr))
         (numb? (car arr))
         (let loop ((arr (cdr arr)))
           (cond
             ((null? arr) #t)
             ((numb? (car arr)) (loop (cdr arr)))
             (else #f)))))
  
  (let ((arr (string->list str)))
    (scan-second-num
     (scan-first-num 
      (if (or (eq? (car arr) #\-) (eq? (car arr) #\+))
          (cdr arr)
          arr)))))


(define (scan-frac str)
  (and (check-frac str)
       (string->number str)))

(define (scan-many-fracs str)
  (define (space? symb)
    (or (eq? symb #\space) (eq? symb #\newline) (eq? symb #\tab)))
  
  (define (search-next-empty str ind)
    (if (= ind (string-length str))
        ind
        (if (space? (string-ref str ind))
            ind
            (search-next-empty str (+ ind 1)))))
  
  (let loop ((ind 0) (arr '()))
    (if (= (string-length str) ind)
        (reverse arr)
        (if (space? (string-ref str ind))
            (loop (+ ind 1) arr)
            (let ((drob (scan-frac (substring str ind (search-next-empty str ind)))))
              (if drob
                  (loop (search-next-empty str ind) (cons drob arr))
                  #f))))))

;; --------------------------------------------
#|
<Program>  ::= <Articles> <Body> .
<Articles> ::= <Article> <Articles> | .
<Article>  ::= define word <Body> end .
<Body>     ::= if <Body> endif <Body> | integer <Body> | word <Body> | .
|#
;; Конструктор потока
(define (make-stream items . eos)
  (if (null? eos)
      (make-stream items #f)
      (list items (car eos))))

;; Запрос текущего символа
(define (peek stream)
  (if (null? (car stream))
      (cadr stream)
      (caar stream)))

;; Продвижение вперёд
(define (next stream)
  (let ((n (peek stream)))
    (if (not (null? (car stream)))
        (set-car! stream (cdr (car stream))))
    n))

(define if-count 0)
(define in-define? #f)

(define (parse vec)
  (define stream (make-stream (vector->list vec)))
  (set! if-count 0)
  (set! in-define? #f)
  (call-with-current-continuation
   (lambda (error)
     (let ((res (parse-program stream error)))
       (if (not (equal? (peek stream) #f))
           #f
           res)))))

(define (start-define? symb)
  (equal? symb 'define))

(define (parse-program stream error)
  (list
   (parse-articles stream error)
   (parse-body stream error '())))

; <Articles> ::= <Article> <Articles> | .
(define (parse-articles stream error)
  (let ((res (parse-article stream error)))
    (if res
        (cons res (parse-articles stream error))
        '())))

(define (parse-article stream error)
  (cond
    ((start-define? (peek stream))
     (next stream)
     (set! in-define? #t)
     (cons (next stream) (list (parse-body stream error '()))))
    (else #f)))
  
(define (parse-body stream error arr)
  (cond
    ((equal? (peek stream) 'endif)
     (if (= if-count 0)
         (error #f))
     (set! if-count (- if-count 1))
     (next stream)
     (reverse arr))
    
    ((equal? (peek stream) 'end)
     (if (or (not (= if-count 0)) (equal? in-define? #f))
         (error #f))
     (set! in-define? #f)
     (next stream)
     (reverse arr))
    
    ((equal? (peek stream) #f)
     (if (or (not (= if-count 0)) (equal? in-define? #t))
         (error #f))
     (next stream)
     (reverse arr))
    
    ((equal? (peek stream) 'if) (parse-body stream error (cons (parse-if stream error '()) arr)))
    ((equal? (peek stream) 'define) (error #f))
    (else (parse-body stream error (cons (next stream) arr)))))

(define (parse-if stream error arr)
  (set! if-count (+ if-count 1))
  (cons (next stream) (list (parse-body stream error '()))))




;(parse #(1 if 2 3 if if if 1 2 endif endif endif 1 endif 2))
