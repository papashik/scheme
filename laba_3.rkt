(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex expr)
     (begin
       (write 'expr)
       (display " => ")
       (let ((v expr))
         (write v)
         v)))))



(define (run-test expr exp-rez)
  (let ((_ (write expr))
        (rez (eval expr (interaction-environment))))
    (if (equal? rez exp-rez)
        (begin (display " => ") ;; ПРИ СДАЧЕ УБРАТЬ
               (write rez)      ;; ЭТИ ДВЕ СТРОКИ!!
               ;;(display " ok") ;; А эту разкомментить
               (display "\n")
               #t)
        (begin (display " FAIL")
               (display "\n Expected: ")
               (write exp-rez) 
               (display "\n Returned: ")
               (write rez)
               (newline)
               #f))))
;——————

(define-syntax test
  (syntax-rules ()
    ((test expr exp-rez)
     (list 'expr exp-rez))))
;——————-

(define (run-tests the-tests)
  (define (loop the-tests ans)
    (if (null? the-tests)
        ans
        (loop (cdr the-tests) 
              (and (run-test (caar the-tests) (cadar the-tests)) ans))))
  
  (loop the-tests #t))
;----------Test materials---------;
(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0)  1) ; Ошибка здесь!
    (else     1)))
(define the-tests
  (list (test (signum -2) -1)
        (test (signum  0)  0)
        (test (signum  2)  1)))

;; ------------------------------------------------------- ;; 3 задача





(define (ref data number . elem)
  (define (insert spis pos symb)
    (define (loop spis pos symb check newlist)
      (if (= check pos)
          (append newlist (list symb) spis)
          (loop (cdr spis) pos symb (+ check 1) (append newlist (list (car spis))))))
    (loop spis pos symb 0 '())
    )
  (define (length-check len data number is-elem?)
    (let* ((is-elem (if is-elem? 1 0))
           (len (+ is-elem len)))
      (and (> number -1) (< number len))))
  
  (let ((is-elem? (not (null? elem)))) ;; is-elem = 1 или 0, если есть elem или нет
    (cond
      ((string? data) ;; если строка(car elem)
       (let ((len (string-length data)))
         (and (length-check len data number is-elem?) ;; Проверку строки(вектора, списка) на длину
              (if is-elem?                            ;; вынес в отдельную функцию, т.к. не хочется 3 раза тянуть 4 строки
                  (and (char? (car elem)) ;; проверяем, что принимаем литеру
                       (list->string (insert (string->list data) number (car elem)))
                       ;;(string-append                 ;; Сращиваем списки с литерой
                       ;;(substring data 0 number)
                       ;;(string (car elem))           ;; Да, я два раза беру (car elem), но я уже запутался в (let ...)
                       ;;(substring data number len))
                       )
                  (string-ref data number)))))        ;; Либо обращаемся к нужному элементу строки 
      ((vector? data) 
       (let ((len (vector-length data)))
         (and (length-check len data number is-elem?)
              (if is-elem?
                  (list->vector (insert (vector->list data) number (car elem)))
                  (vector-ref data number)))))
      ((pair? data)   
       (let ((len (length data)))
         (and (length-check len data number is-elem?)
              (if is-elem?
                  (insert data number (car elem))
                  (list-ref data number))))))))

(define the-tests
  (list (test (ref "123" 1 #\0) "1023")
        (test (ref "123" 1 0)   #f)
        (test (ref "123" 3 #\4) "1234")
        (test (ref "123" 5 #\4) #f)
        (test (ref #(1 2 3) 1) 2)
        (test (ref '(1 2 3) 1) 2)
        (test (ref '(1 2 3) 3 #\0) '(1 2 3 #\0))
        (test (ref #(1 2 3) 1 0) #(1 0 2 3))
        (test (ref #(1 2 3) 1 #\0) #(1 #\0 2 3))))



;;(display (run-tests the-tests))



(define-syntax factorize
  (syntax-rules (expt)
    ((factorize '(- (expt x 2) (expt y 2)))
     '(* (- x y) (+ x y)))
    ((factorize '(- (expt x 3) (expt y 3)))
     '(* (- x y) (+ ( * x x) (* x y) (* y y))))
    ((factorize '(+ (expt x 3) (expt y 3)))
     '(* (+ x y) (+ ( - ( * x x) (* x y)) (* y y))))
    ))

