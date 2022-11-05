;; -------------------------------------Не до конца понятое 1 задание - проблемы с void при подгрузке фреймворка ---------------
;; ----------- СОДЕРЖИМОЕ ПОДГРУЖАЕМОГО ФАЙЛА ----------
(define the-void (if #f #f))
(define (print expr)
  (if (not (equal? expr the-void))
      (begin
        (write expr)
        (newline))))
(define r #f)
(define call/cc call-with-current-continuation)

(print (call/cc
        (lambda (cont)
          (set! r cont)
          "Assertions included")))

(define-syntax assert
  (syntax-rules ()
    ((assert expr) (if expr #t (begin (display "FAILED: ") (r 'expr))))))
;; ----------- СОДЕРЖИМОЕ ПОДГРУЖАЕМОГО ФАЙЛА ----------

;; --------------------------- ПОДГРУЗКА ------------------------------
;; -------------------- К ней кстати тоже вопросы ---------------------
;(define (use-assertions)
;  (load "C:\\Users\\Zed\\Documents\\УЧЕБА\\ИНФА\\carcass_to_lab_4.scm"))


(define (1/x x)
  (assert (not (zero? x))) ; Утверждение: x ДОЛЖЕН БЫТЬ ≠ 0
  (/ 1 x))

;; ------------------------------ Data operations ---------------------
(define (save-data data path)
  (with-output-to-file path
    (lambda ()
      (display data))))

(define (load-data path)
  (read (open-input-file path)))

(define (count-strings path)
  (define (loop port n)
    (let ((char (read-char port)))
      (cond
        ((equal? char #\newline) (loop port (+ n 1)))
        ((eof-object? char) n)
        (else (loop port n)))))
  (loop (open-input-file path) 0))

;; --------------------------- Трибоначчи -------------------------

(define trib
  (let ((known-trib '((2 1) (1 0) (0 0))))
    (lambda (n)
      (let ((res (assoc n known-trib)))
        (if res
            (cadr res)
            (let ((trib-n (+ (trib (- n 1)) (trib (- n 2)) (trib (- n 3)))))
              (begin (set! known-trib (cons (list n trib-n) known-trib))
                     trib-n)))))))
                        
;(display (trib 100))

;; ------------------------------ My-If ------------------------
(define-syntax my-if
  (syntax-rules ()
    ((my-if cond? true false)
     (force (or (and cond? (delay true)) (delay false))))))


(define-syntax my-if-cc
  (syntax-rules ()
    ((my-if-cc cond true false)
     (force (call/cc 
             (lambda (return) 
               (or (and cond (return (delay true)))
                   (return (delay false)))))))))

;; ------------------------------ LETS -----------------------------
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((name1 def1)
              (name2 def2) ...)
             expr1 expr2 ...)
     ((lambda (name1 name2 ...) expr1 expr2 ...) def1 def2 ...))))

(define-syntax my-let*
  (syntax-rules ()
    ((my-let* ((name def)) expr1 ...) (my-let ((name def)) expr1 ...))
    ((my-let* ((name1 def1) . defs) expr1 ...)
     (my-let ((name1 def1)) (my-let* defs expr1 ...)))))

;(my-let ((a (+ c b)) (b 2) (c 3)) (display (* a b c)) (display (+ a b c)) (display "hello"))


;; ------------------------- 6 A --------------
(define-syntax when
  (syntax-rules ()
    ((when cond? expr1 ...) (if cond? (begin expr1 ...)))))
(define-syntax unless
  (syntax-rules ()
    ((unless cond? expr1 ...) (if (not cond?) (begin expr1 ...)))))

;; ----------------------- 6 Б ----------------
(define-syntax for
    (syntax-rules (in as)
        ((for x in xs expr1 expr2 ...) (for-each (lambda (x) (begin expr1 expr2 ...)) xs))
        ((for xs as x expr1 expr2 ...) (for-each (lambda (x) (begin expr1 expr2 ...)) xs))))


;; ------------------------ 6 В Г ----- While Repeat -----------------------   
(define-syntax while
  (syntax-rules ()
    ((while cond? expr1 expr2 ...)
     (let loop () (if cond? (begin expr1 expr2 ... (loop)))))))

(define-syntax repeat
  (syntax-rules (until)
    ((repeat (expr1 expr2 ...) until cond?)
     (let loop () (begin expr1 expr2 ... (if (not cond?) (loop)))))))
;; Как видно, repeat со скобочками особо не отличается
(define-syntax repeat
  (syntax-rules (until)
    ((repeat expr1 expr2 ... until cond?)
     (let loop () (begin expr1 expr2 ... (if (not cond?) (loop)))))))

;; ----------------------------- 6 Д ---------------------------------
;; --------------------- Этот cout очень странный - он ест ооооочень много памяти непонятно почему -----------------
;(define << '<< )
(define endl #\newline)
(define-syntax cout
  (syntax-rules()
    ((cout expr)
     (if (not (eqv? 'expr '<<))
         (display expr)))
    ((cout expr1 expr2 ...)
     (if (not (eqv? 'expr1 '<<))
         (begin (display expr1)
                (cout expr2 ...))
         (cout expr2 ...)))))

;(cout << "a = " << 1 << endl << "b = " << 2 << endl)
;(cout << 1 << 2 << endl << " hello " << 1 << 2 << endl << " hello ")
;; --------------------------------- А этот нормальный! ---------------------
(define-syntax test-cout
  (syntax-rules (<< endl)
    ((test-cout << expr) (display expr))
    ((test-cout << endl) (newline))
    ((test-cout << endl << expr1 ...) (begin (newline) (test-cout << expr1 ...)))
    ((test-cout << expr << expr1 ...) (begin (display expr) (test-cout << expr1 ...)))))

