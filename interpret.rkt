(define (interpret program stack)
  (define (start-information)
    (display "interpret running... program: ")
    (display program)
    (display " stack: ")
    (display stack))

  (start-information)
  
  #t
  
  (define (ERROR word)
    (begin
      (display "FATAL ERROR: UNKNOWN COMMAND: ")
      (write word)))
  
  ;; ----------------------STACK COMMANDS---------------------  
  (define (displaying stack) ;; UNDEFINED ЕСЛИ УБРАТЬ СТРОКУ #T и (start-information)
    (newline) (display stack) stack)
  
  (define (plus stack)
    (cons (+ (cadr stack) (car stack)) (cddr stack)))

  (define (minus stack)
    (cons (- (cadr stack) (car stack)) (cddr stack)))

  (define (multiplication stack)
    (cons (* (cadr stack) (car stack)) (cddr stack)))

  (define (int-division stack)
    (cons (quotient (cadr stack) (car stack)) (cddr stack)))

  (define (remainder-of-division stack)
    (cons (remainder (cadr stack) (car stack)) (cddr stack)))

  (define (negative stack)
    (cons (- (car stack)) (cddr stack)))

  (define (equaling? stack)
    (cons (if (= (cadr stack) (car stack)) -1 0) (cddr stack)))

  (define (bigger? stack)
    (cons (if (> (cadr stack) (car stack)) -1 0) (cddr stack)))

  (define (smaller? stack)
    (cons (if (< (cadr stack) (car stack)) -1 0) (cddr stack)))

  (define (logical-not stack)
    (cons (if (= (car stack) 0) -1 0) (cdr stack)))

  (define (logical-and stack)
    (cons (if (or (= (car stack) 0) (= (cadr stack) 0)) 0 -1) (cddr stack)))

  (define (logical-or stack)
    (cons (if (and (= (car stack) 0) (= (cadr stack) 0)) 0 -1) (cddr stack)))

  (define (drop stack)
    (cdr stack))

  (define (swap stack)
    (cons (cadr stack) (cons (car stack) (cddr stack))))

  (define (dup stack)
    (cons (car stack) stack))

  (define (over stack)
    (cons (caddr stack) stack))

  (define (rot stack)
    (cons (caddr stack) (cons (cadr stack) (cons (car stack) (cdddr stack)))))

  (define (depth stack)
    (cons (length stack) stack))
  ;; -----------------------------------CONTROL COMMANDS---------------------
  (define (defining words-vector word-index data-stack return-stack glossary)
    (let loop ((n (+ word-index 2)))
      (if (equal? (vector-ref words-vector n) 'end)
          (list words-vector (+ n 1) data-stack return-stack (cons (cons (vector-ref words-vector (+ word-index 1)) (+ word-index 2)) glossary))
          (loop (+ n 1))))) ;; Здесь мы прикрутили к глоссарию новое определение

  (define (end words-vector word-index data-stack return-stack glossary)
    (list words-vector (car return-stack) data-stack (cdr return-stack) glossary))
  
  (define (exit words-vector word-index data-stack return-stack glossary)
    (list words-vector (car return-stack) data-stack (cdr return-stack) glossary))
  
  (define (start-of-if words-vector word-index data-stack return-stack glossary)
    (if (= (car data-stack) 0)
        (let loop ((n (+ word-index 1)))
          (if (equal? (vector-ref words-vector n) 'endif)
              (list words-vector (+ n 1) (cdr data-stack) return-stack glossary)
              (loop (+ n 1))))
        (list words-vector (+ word-index 1) (cdr data-stack) return-stack glossary)))

  (define (end-of-if stack) stack)
  
  ;; ---------------COMMANDS---------------------  
  
  (define start-glossary ;; Словарь с начальными командами '((+ . plus) (- . minus) ...)
    (list (cons 'dsp displaying)
          (cons '+ plus)
          (cons '- minus)
          (cons '* multiplication)
          (cons '/ int-division)
          (cons 'mod remainder-of-division)
          (cons 'neg negative)
          (cons '= equaling?)
          (cons '> bigger?)
          (cons '< smaller?)
          (cons 'not logical-not)
          (cons 'and logical-and)
          (cons 'or logical-or)
          (cons 'drop drop)
          (cons 'swap swap)
          (cons 'dup dup)
          (cons 'over over)
          (cons 'rot rot)
          (cons 'depth depth)
          
          (cons 'endif end-of-if) ; Не является управляющей командой! просто ключевое слово
          ;; ----------------------------
          (cons 'define defining) ;; Вынесены снизу в отдельный список
          (cons 'end end)
          (cons 'exit exit)
          (cons 'if start-of-if)
          
          ))
  
  (define control-structures (cddddr (cddddr (cddddr (cddddr (cddddr start-glossary))))))
  
  (let ((program-length (vector-length program)))
    (define (processing words-vector word-index data-stack return-stack glossary)
      (if (= word-index program-length)
          data-stack ;; Выход из processing, когда стек программы закончился
          (let* ((word (vector-ref words-vector word-index))
                 (command-pair (assoc word glossary)) ;; Тыкаемся в словарь и получаем #f или пару вида '(+ . plus)
                 (command (if command-pair (cdr command-pair) #f))) ;; Команда вида <plus> или число типа <14>
            ;(display (list command words-vector word-index data-stack return-stack glossary))
            ;(read-char)
            (if command ;; Команда лежит в словаре? (если нет - либо константа, либо ошибка)
                (if (number? command) ;; Команда - число?
                    (processing words-vector command data-stack (cons (+ word-index 1) return-stack) glossary) ;; Переход по адресу определенной в программе процедуры
                    (if (assoc word control-structures) ;; Добро пожаловать, криво сделанные управляющие команды
                        (let* ((response (command words-vector word-index data-stack return-stack glossary)) ;; Вызов команды не на стеке, а на всей информации сразу
                               (words-vector (car response)) ;; Распаковка полученного ответа от управляющей команды
                               (word-index (cadr response))
                               (data-stack (caddr response))
                               (return-stack (cadddr response))
                               (glossary (car (cddddr response))))
                          (processing words-vector word-index data-stack return-stack glossary))
                          
                        (processing words-vector (+ word-index 1) (command data-stack) return-stack glossary))) ;; Команда - стандартная, выполним ее на стеке
                ;                                                                                               ;; и перейдем на слово вперед
                (if (number? word) ;; Ладно, такого у нас в словаре нет, но может это константа?
                    (processing words-vector (+ word-index 1) (cons word data-stack) return-stack glossary) ;; Здесь команда - константа
                    (ERROR word))) ;; Здесь команда неизвестна, вызов ошибки
            

            ;(display word)
            ;; (processing words-vector (+ word-index 1) data-stack return-stack glossary) ;; Хвостовая рекурсия
            )
          ))

    

    (processing program 0 stack '() start-glossary))) ;; Вызов processing для первого слова



(define the-tests (list
                   (list 1 (delay (interpret #(7 + 2) '(1 2 3))) '(2 8 2 3))
                   (list 2 (delay (interpret #(+ + +) '(2 2 2 8))) '(14))
                   (list 3 (delay (interpret #(* + +) '(2 3 2 8))) '(16))


                   ))


(for-each (lambda (x)
            (if (equal? (force (cadr x)) (caddr x))
                (begin (display " => OK") (display (caddr x)) (newline))
                (begin (display " => ERROR ") (display (car x)) (newline)))) the-tests)
