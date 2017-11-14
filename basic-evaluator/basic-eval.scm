(define basic:eval
  (lambda (expr env)
    (if (atom? expr)
        (cond ((symbol? expr)
               (lookup expr env))
              ((or (number? expr) (string? expr) (char? expr) (boolean? expr) (vector? expr))
               expr)
              (else (error "EVAL - cannot evaluate atom")))
        (case (car expr)
          (else (error "EVAL - could not evaluate list"))))))

;;; utility functions
  
(define atom?
  (lambda (expr)
    (not (pair? expr))))


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
