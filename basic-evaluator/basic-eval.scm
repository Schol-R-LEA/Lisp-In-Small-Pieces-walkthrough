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
          (else (invoke (basic:eval (cdr expr) env)
                        (evlis (cdr expr) env)))))))

(define invoke
  (lambda (fn args)
    (if (procedure? fn)
        (fn args)
        (error "not a function: " fn))))


(define make-function
  (lambda (variables body env)
    (eprogn body (extend-env env.init variables values))))

;;; utility functions

(define atom?
  (lambda (expr)
    (not (pair? expr))))

(define empty-result '*unspecified*)

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

(define parent-env '**parent**)

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


(define make-env (env '()))

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
    (let loop ((make-env))
      (display #\newline)
      (display #\newline)
      (display ">>> ")
      (let ((result (basic:eval (read) env)))
        (display result)
        (loop env)))))

(basic:repl)
