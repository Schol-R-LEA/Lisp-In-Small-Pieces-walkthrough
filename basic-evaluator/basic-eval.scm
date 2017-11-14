(define basic:eval
  (lambda (expr env)
    (if (atom? expr)
        (if (symbol? expr)
            (lookup expr env)
            expr))
    (case (car expr)
          (else (error "EVAL - could not evaluate list")))))

  
(define atom?
  (lambda (expr)
    (not (pair? expr))))
  
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
