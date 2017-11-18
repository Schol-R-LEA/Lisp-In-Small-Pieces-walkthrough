(define empty-result '*unspecified*)
(define env.init (env '()))
(define env.global env.init)
(define parent-env '**parent**)


(define-syntax define-initial
  (syntax-rules ()
    ((_ <name> <values>)
     (begin
       (set! env.global (cons (cons <name> <value>) env.global))
       (list <name> <value>)))
    ((_ <name>)
     (_ <name> empty-result))))


(define-syntax define-primitive
  (syntax-rules ()
    ((_ <name> <value> <arity>)
     ((define-initial <name>
        (lambda (values)
          (if (= <arity> (length values))
              (apply <value> values)
              (error (string-append (string-append (string-append "Incorrect arity for procedure "
                                                                  (symbol->string <name>))
                                                   (string-append ": is " (number->string (length values))))
                                    (string-append ", should be " (number->string <arity>)))))))))))

(define-initial t #t)
(define-initial f #f)
(define-initial nil '())

(define-primitive cons cons 2)
(define-primitive car car 1)
(define-primitive cdr cdr 1)
(define-primitive set-car! set-car! 2)
(define-primitive set-cdr! set-cdr! 2)
(define-primitive + 2)
(define-primitive - 2)
(define-primitive * 2)
(define-primitive / 2)
(define-primitive eq? 2)
(define-primitive = 2)
(define-primitive < < 2)
(define-primitive <= <= 2)
(define-primitive > > 2)
(define-primitive >= >= 2)
(define-primitive not 1)
(define-primitive null? 1)
(define-primitive symbol? symbol? 1)
(define-primitive list? list? 1)
(define-primitive list list 2)
(define-primitive apply apply 2)
(define-primitive procedure? procedure? 2)
(define-primitive error error 2)
(define-primitive and and 2)
(define-primitive or or 2)
(define-primitive number? number? 1)
(define-primitive string? string 1)
(define-primitive char? char? 1)
(define-primitive vector? vector? 1)
(define-primitive boolean? boolean? 1)
(define-primitive caar caar 1)
(define-primitive cadr cadr 1)
(define-primitive cdar cdar 1)
(define-primitive cddr cddr 1)
(define-primitive caaar caaar 1)
(define-primitive caadr caadr 1)
(define-primitive cdaar cadar 1)
(define-primitive cddar cdaar 1)
(define-primitive caddr caddr 1)
(define-primitive caadr cdadr 1)
(define-primitive cdaar cddar 1)
(define-primitive cddar cdddr 1)

(define-primitive caaaar caaaar 1)
(define-primitive caaadr caaadr 1)
(define-primitive cadaar caadar 1)
(define-primitive caddar cadaar 1)
(define-primitive caaddr caaddr 1)
(define-primitive caaadr cadadr 1)
(define-primitive cadaar caddar 1)
(define-primitive caddar cadddr 1)
(define-primitive cdaaar cdaaar 1)
(define-primitive cdaadr cdaadr 1)
(define-primitive cddaar cdadar 1)
(define-primitive cdddar cddaar 1)
(define-primitive cdaddr cdaddr 1)
(define-primitive cdaadr cddadr 1)
(define-primitive cddaar cdddar 1)
(define-primitive cdddar cddddr 1)





(define basic:eval
  (lambda (expr env)
    (if (atom? expr)
        (cond ((symbol? expr)
               (lookup expr env))
              ((or (number? expr) (string? expr) (char? expr) (boolean? expr) (vector? expr))
               expr)
              (else (error "EVAL - cannot evaluate atom")))
        (case (car expr)
          ((quote) (cadr expr))
          ((if) (if (basic:eval (cadr expr) env)
                    (basic:eval (caddr expr) env)
                    (basic:eval (cadddr expr) env)))
          ((begin) (eprogn (cdr expr) env))
          ((set!) (update! (cadr expr) env (basic:eval (caddr expr) env)))
          ((lambda) (make-function (cadr expr) (cddr expr) env))
          (else (invoke (basic:eval (car expr) env)
                        (evlis (cdr expr) env))))))

  (define invoke
    (lambda (fn args) 
      (if (procedure? fn)
          (fn args)
          (error "not a function: " fn))))

  (define make-function
    (lambda (variables body env)
      (lambda (values)
        (eprogn body (extend-env env variables values)))))

;;; utility functions

  (define atom?
    (lambda (expr)
      (not (pair? expr))))


  (define eprogn
    (lambda (exprs env)
      (if (pair? exprs)
          (if (pair? (cdr exprs))
              (begin
                (basic:eval (car exprs) env)
                (eprogn (cdr exprs) env))
              (basic:eval (car exprs) env))
          empty-result)))


  (define evlis
    (lambda (exprs env)
      (if (pair? exprs)
          (cons (basic:eval (car exprs) env)
                evlis (cdr exprs) env)
          '())))


  (define hash
    (lambda (key)        ;; stub for possible future need
      key))


  (define valid?
    (lambda (value)
      (not (null? value))))


  (define lookup
    (lambda (key env)
      (let ((value (assq (hash key) env))
            (parent (assq parent-env env)))
        (cond ((valid? value)
               value)
              ((valid? parent)
               (lookup key parent))
              (else (error "LOOKUP - no value bound to symbol " key))))))

  (define update!
    (lambda (key env value)
      (let ((variable (lookup key env)))
        (if (valid? variable)
            (begin
              (set-cdr! variable value)
              empty-result)))))


  (define extend-env
    (lambda (env variables values)
      (cond ((pair? variables)
             (if (pair? values)
                 (cons (cons (car variables) (car values))
                       (extend-env env (cdr variables) (cdr values)))
                 (error "Too few values"))
             ((null? variables)
              (if (null? values)
                  env
                  (error "Too few variables")))
             ((symbol? variables)
              (cons (cons variables values) env))))))

;;; simple REPL

  (define basic:repl
    (lambda ()
      (display "Basic evaluator test REPL")
      (let loop ((env env.init))
        (display #\newline)
        (display #\newline)
        (display ">>> ")
        (let ((result (basic:eval (read) env)))
          (display result)
          (loop env)))))

  (basic:repl)
