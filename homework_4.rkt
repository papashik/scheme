;; -------------------------------- Задание 1 -----------------------
(define memoized-factorial
  (let ((known-fact '((1 1))))
    (lambda (n)
      (let ((res (assoc n known-fact)))
        (if res
            (cadr res)
            (let ((fact-n (* (memoized-factorial (- n 1)) n)))
              (begin (set! known-fact (cons (list n fact-n) known-fact))
                     fact-n)))))))

;; -------------------------------- Задание 2 ---------------------------------

(define-syntax lazy-cons
  (syntax-rules ()
    ((lazy-cons a b) (cons a (delay b)))))

(define (lazy-car p)
  (car p))
(define (lazy-cdr p)
  (force (cdr p)))

(define (naturals start)
  (lazy-cons start (naturals (+ start 1))))

(define (lazy-head xs k)
  (let loop ((k k) (xs xs))
    (if (= k 0)
        '()
        (cons (car xs) (loop (- k 1) (force (cdr xs)))))))

(define (lazy-ref xs k)
  (let loop ((k k) (xs xs))
    (if (= k 0)
        (car xs)
        (loop (- k 1) (force (cdr xs))))))


    
(define (lazy-factorial n)
  (define (lazy-fact n) ;; Ленивый бесконечный список
    (if (= n 1)
        (lazy-cons 1 (lazy-fact 2))
        (lazy-cons (* (car (lazy-fact (- n 1))) n) (lazy-fact (+ n 1)))))

  (car (lazy-fact n))) ;; Обращение к ленивому списку lazy-fact
;; ----------------------------------- Задание 3 -------------------------------

(define (huy n)
  (define (loop n)
    (display 5))
  (loop 3))


(define (read-words)
  (let loop ((e (read-char)) (xs '()) (cur-word '()))
    (let ((stack-empty? (null? cur-word)))
      (if (eof-object? e)
          (if stack-empty?
              (reverse xs)
              (reverse (cons (list->string (reverse cur-word)) xs)))
          (if (char-whitespace? e)
              (if stack-empty?
                  (loop (read-char) xs '())
                  (loop (read-char) (cons (list->string (reverse cur-word)) xs) '()))
              (loop (read-char) xs (cons e cur-word)))))))
        
        
; (with-input-from-file "test.txt" read-words)


;; ------------------------------- Задание 4 -----------------------------------

;; ------------------------------------------------ СОДЕРЖИМОЕ КАРКАСА --------------------------
(define ie (interaction-environment))

(define (enum xs) ;; Enumerate вида ((0 . x) (1 . y) ...)
  (let loop ((xs xs) (num 0))
    (if (null? xs)
        '()
        (cons `(,num . ,(car xs)) (loop (cdr xs) (+ num 1))))))

(define (list-set! xs num value)
  (let loop ((xs xs) (num num))
    (if (= num 0)
        (set-car! xs value)
        (loop (cdr xs) (- num 1)))))

(define-syntax define-struct ;; For example, (define-struct pos (row col)) ; Объявление типа pos
  (syntax-rules ()
    ((define-struct name (def1 ...))
     (let ((string-name (symbol->string 'name))) ;; String-name = "pos"
       (eval `(define ,(string->symbol (string-append "make-" string-name)) ;; make-pos
                (lambda (def1 ...) `(name ,def1 ...))) ie)
       (eval `(define ,(string->symbol (string-append string-name "?")) ;; pos?
                (lambda (struct-name) (eq? (car struct-name) 'name))) ie)
       (for-each ;; For-each in ((0 . row) (1 . col))
        (lambda (x)
          (let ((num (car x)) ;; num = 0 or 1
                (string-def (symbol->string (cdr x)))) ;; String-def = "row" or "col"
            (eval `(define (,(string->symbol (string-append string-name "-" string-def)) struct) ;; pos-XXX (row or col)
                     (list-ref struct ,(+ num 1))) ie)
            (eval `(define (,(string->symbol (string-append "set-" string-name "-" string-def "!")) struct new-value) ;; set-pos-XXX! (row or col)
                     (list-set! struct ,(+ num 1) new-value)) ie)))
        (enum '(def1 ...)))))))
;; ------------------------------------------------ СОДЕРЖИМОЕ КАРКАСА --------------------------

(define (test)
  (define-struct pos (row col)) ; Объявление типа pos
  (define p (make-pos 1 2))     ; Создание значения типа pos

  (display (pos? p)) (newline) 

  (display (pos-row p)) (newline)
  (display (pos-col p)) (newline)

  (set-pos-row! p 3) ; Изменение значения в поле row
  (set-pos-col! p 4) ; Изменение значения в поле col

  (display (pos-row p)) (newline)
  (display (pos-col p)) (newline))

;(test)

;; ----------------------------------- Задание 5 ----------------------------------------
