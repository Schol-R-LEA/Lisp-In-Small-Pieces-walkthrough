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
          (else (basic:apply (basic:eval (cdr expr) env)
                             (evlis (cdr expr) env)))))))

;;; utility functions
  
(define atom?
  (lambda (expr)
    (not (pair? expr))))

(define empty-result '*<unspecified>*)

(define eprogn
  (lambda (exprs env)
    (if (pair? exprs)
        (if (pair? (cdr exprs))
            (begin
              (basic:eval (car exprs) env)
              (eprogn (cdr exprs) env))
            (basic:eval (car exprs) env))
        empty-result)))

;;; simple REPL

(define basic:repl
  (lambda ()
    (display "Basic evaluator test REPL")
    (let loop ((lexical-env '((parent ()))))
      (display #\newline)
      (display #\newline)
      (display ">>> ")
      (let ((result (basic:eval (read) env)))
        (display result)
        (loop env)))))

(basic:repl)
