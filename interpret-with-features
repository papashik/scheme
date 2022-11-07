(define (interpret program stack)
  (define feature-nested-if #t)
  (define feature-if-else #t)
  
  (define (start-information)
    (display "interpret running... program: ")
    (display program)
    (display " stack: ")
    (display stack))

  ;(start-information)
  
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
    (cons (- (car stack)) (cdr stack)))

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
    (cons (cadr stack) stack))

  (define (rot stack)
    (cons (caddr stack) (cons (cadr stack) (cons (car stack) (cdddr stack)))))

  (define (depth stack)
    (cons (length stack) stack))


  ;;---------------------------LOCALS------------------------
  (define program-length (vector-length program))
  
  ;;---------------------------SERVICE COMMANDS------------------------
  (define (find-first words-vector start-index word) ;; Возвращает адрес первого вхождения заданного слова(WORD) в программу(WORDS-VECTOR), начиная с start-index
    (let loop ((n start-index)
               (max-index (vector-length words-vector)))
      (and (not (= max-index n))
           (if (equal? (vector-ref words-vector n) word)
               n
               (loop (+ n 1) max-index)))))

  ;(display (find-first (vector 'end 'hello 1 2 'hello 'endif) 0 'hello))
  ;(display (find-first (vector 'end 'hello 1 2 'hello 'endif) 0 'end))


  (define (skip-if words-vector if-index data-stack return-stack glossary) ;; Если if ушел в ноль
    (let loop ((if-depth 0) ;; Глубина вложенных if 
               (n (+ if-index 1))
               (max-index (vector-length words-vector)))
      (and (not (= max-index n))
           (let* ((word (vector-ref words-vector n))
                  (word-endif? (equal? word 'endif)))
             (if (equal? word 'if)
                 (loop (+ if-depth 1) (+ n 1) max-index)
             (if (= if-depth 0)
                 (if (or word-endif? (equal? word 'else))
                     (list words-vector (+ n 1) (cdr data-stack) return-stack glossary)
                     (loop 0 (+ n 1) max-index))
                 (if word-endif?
                     (loop (- if-depth 1) (+ n 1) max-index)
                         (loop if-depth (+ n 1) max-index))))))))

  
  ;; -----------------------------------CONTROL COMMANDS---------------------
  (define (defining words-vector word-index data-stack return-stack glossary)
    (let ((first-occurence-of-end (find-first words-vector (+ word-index 2) 'end)))
      (list words-vector
            (+ first-occurence-of-end 1)
            data-stack
            return-stack
            (cons (cons (vector-ref words-vector (+ word-index 1)) (+ word-index 2)) glossary))
      )) ;; Здесь мы прикрутили к глоссарию новое определение

  (define (end words-vector word-index data-stack return-stack glossary)
    (list words-vector (car return-stack) data-stack (cdr return-stack) glossary))
  
  (define (exit words-vector word-index data-stack return-stack glossary)
    (list words-vector (car return-stack) data-stack (cdr return-stack) glossary))
  
  ;  (define (start-of-if words-vector word-index data-stack return-stack glossary) ;; Typical if
  ;    (if (= (car data-stack) 0)
  ;        (let ((first-occurence-of-endif (find-first words-vector (+ word-index 1) 'endif)))
  ;              (list words-vector
  ;                    (+ first-occurence-of-endif 1)
  ;                    (cdr data-stack)
  ;                    return-stack
  ;                    glossary))
  ;        (list words-vector (+ word-index 1) (cdr data-stack) return-stack glossary)))

  ;(define (start-of-if words-vector word-index data-stack return-stack glossary) ;; Only with else, not nested
  ;   (let ((first-occurence-of-endif (find-first words-vector (+ word-index 1) 'endif))
  ;       (first-occurence-of-else (find-first words-vector (+ word-index 1) 'else)))
  ;   (if (= (car data-stack) 0)
  ;       (if first-occurence-of-else
  ;           (list words-vector (+ first-occurence-of-else 1) (cdr data-stack) return-stack glossary)
  ;           (list words-vector (+ first-occurence-of-endif 1) (cdr data-stack) return-stack glossary))
  ;       (list words-vector (+ word-index 1) (cdr data-stack) return-stack glossary))))
  
  
  (define (start-of-if words-vector word-index data-stack return-stack glossary) ;; Переделанное под вложенные nested-if
    (if (= (car data-stack) 0)
        (skip-if words-vector word-index data-stack return-stack glossary)
        (list words-vector (+ word-index 1) (cdr data-stack) return-stack glossary)))

  (define (else-word words-vector word-index data-stack return-stack glossary)
    (let ((first-occurence-of-endif (find-first words-vector (+ word-index 1) 'endif)))
      (list words-vector (+ first-occurence-of-endif 1) data-stack return-stack glossary)))



  (define (keyword-endif stack) stack)

  
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
          
          (cons 'endif keyword-endif) ; Не является управляющей командой! просто ключевое слово
          
          ;; ----------------------------
          (cons 'define defining) ;; Вынесены снизу в отдельный список
          (cons 'end end)
          (cons 'exit exit)
          (cons 'if start-of-if)
          (cons 'else else-word)
          ))
  
  (define control-structures (list (cons 'define defining)
                                   (cons 'end end)
                                   (cons 'exit exit)
                                   (cons 'if start-of-if)
                                   (cons 'else else-word)))
  
  ;(let ((program-length (vector-length program)))
  (define (processing words-vector word-index data-stack return-stack glossary)
    (if (= word-index program-length)
        data-stack ;; Выход из processing, когда стек программы закончился
        (let* ((word (vector-ref words-vector word-index))
               (command-pair (assoc word glossary)) ;; Тыкаемся в словарь и получаем #f или пару вида '(+ . plus)
               (command (if command-pair (cdr command-pair) #f))) ;; Команда вида <plus> или число типа <14>
            
          ;(display (list command words-vector word-index data-stack return-stack)) (newline) ;; DEBUG INFORMATION

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

          )
        ))

    

  (processing program 0 stack '() start-glossary)) ;; Вызов processing для первого слова



;; ----------------------------------------- Дальше только тесты
(define the-tests (list
                   (list 1 (delay (interpret #(7 + 2) '(1 2 3))) '(2 8 2 3))
                   (list 2 (delay (interpret #(+ + +) '(2 2 2 8))) '(14))
                   (list 3 (delay (interpret #(* + +) '(2 3 2 8))) '(16))
                   (list 4 (delay (interpret #(   define =0? dup 0 = end
                                                   define <0? dup 0 < end
                                                   define signum
                                                   =0? if exit endif
                                                   <0? if drop -1 exit endif
                                                   drop
                                                   1
                                                   end
                                                   0 signum
                                                   -5 signum
                                                   10 signum       ) (quote ()))) '(1 -1 0))
                   (list 5 (delay (interpret #(   define abs
                                                   dup 0 <
                                                   if neg endif
                                                   end
                                                   9 abs
                                                   -9 abs      ) (quote ()))) '(9 9))
                   (list 6 (delay (interpret #(   define =0? dup 0 = end
                                                   define gcd
                                                   =0? if drop exit endif
                                                   swap over mod
                                                   gcd
                                                   end
                                                   90 99 gcd
                                                   234 8100 gcd    ) '())) '(18 9))
                   (list 7 (delay (interpret #(1 if 100 else 200 endif) '())) '(100))
                   (list 8 (delay (interpret #(0 if 100 else 200 endif) '())) '(200))
                   (list 9 (delay (interpret #(1 if 2 if 3 endif 4 endif 5) '())) '(5 4 3))
                   (list 10 (delay (interpret #(1 if 0 if 2 endif 3 endif 4) '())) '(4 3))
                   (list 11 (delay (interpret #(0 if 1 if 2 endif 3 endif 4) '())) '(4))
                   ))


(for-each (lambda (x)
            ;(display "-----------------------------\n")
            (if (equal? (force (cadr x)) (caddr x))
                (begin (display (car x)) (display " => OK") (display (caddr x)) (newline))
                (begin (display (car x)) (display " => ERROR") (newline)))) the-tests)
