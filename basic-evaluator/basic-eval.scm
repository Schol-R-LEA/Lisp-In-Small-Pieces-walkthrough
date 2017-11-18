(define void '*unspecified*)
(define env.init  '())
(define env.global env.init)
(define parent-env '**parent**)


(define-syntax define-initial
  (syntax-rules (:in)
    ((_ <name> <value> :in <env>)
     (begin
       (set! <env> (cons (cons '<name> <value>) <env>))
       '<name>))
    ((_ <name> <value>)
     (define-initial <name> <value> :in env.global))
    ((_ <name> in <env>)
     (define-initial <name> void :in <env>))
    ((_ <name>)
     (define-initial <name> void :in env.global))))


(define-syntax define-primitive
  (syntax-rules (:in)
    ((_ <name> <primitive> <arity> :in <env>)
     (define-initial <name>
       (lambda (values)
         (if (= <arity> (length values))
             (apply <primitive> values)
             (error (format #t
                            "PROCEDURE-APPLICATION: Incorrect arity for procedure ~A, expected ~A, recieved ~A"                               <name>
                            <arity>
                            (length values))))
         :in <env>)))
    ((_ <name> <primitive> <arity>)
     (define-primitive <name> <primitive> <arity> :in env.global))
    ((_ <name> <arity> :in <env>)
     (define-primitive <name> <name> <arity> :in <env>))
    ((_ <name> <arity>)
     (define-primitive <name> <name> <arity> :in env.global))))
  

(define-initial t #t)
(define-initial f #f)
(define-initial nil '())

;; list creation primitives
(define-primitive cons 2)
(define-primitive set-car! 2)
(define-primitive set-cdr! 2)
(define-primitive list 2)

;; basic math
(define-primitive + 2)
(define-primitive - 2)
(define-primitive * 2)
(define-primitive / 2)

;; basic comparisons
(define-primitive eq? 2)
(define-primitive eqv? 2)
(define-primitive equal? 2)
(define-primitive = 2)
(define-primitive < 2)
(define-primitive <= 2)
(define-primitive > 2)
(define-primitive >= 2)
(define-primitive not 1)
                                        ;(define-primitive and 2)
                                        ;(define-primitive or 2)



;; basic predicates
(define-primitive null? 1)
(define-primitive symbol? 1)
(define-primitive list? 1)
(define-primitive number? 1)
(define-primitive string? 1)
(define-primitive char? 1)
(define-primitive vector? 1)
(define-primitive boolean? 1)
(define-primitive procedure? 1)

;; list extractors
(define-primitive car 1)
(define-primitive cdr 1)
(define-primitive caar 1)
(define-primitive cadr 1)
(define-primitive cdar 1)
(define-primitive cddr 1)
(define-primitive caaar 1)
(define-primitive caadr 1)
(define-primitive cadar 1)
(define-primitive cdaar 1)
(define-primitive caddr 1)
(define-primitive cdadr 1)
(define-primitive cddar 1)
(define-primitive cdddr 1)

(define-primitive caaaar 1)
(define-primitive caaadr 1)
(define-primitive caadar 1)
(define-primitive cadaar 1)
(define-primitive caaddr 1)
(define-primitive cadadr 1)
(define-primitive caddar 1)
(define-primitive cadddr 1)
(define-primitive cdaaar 1)
(define-primitive cdaadr 1)
(define-primitive cdadar 1)
(define-primitive cddaar 1)
(define-primitive cdaddr 1)
(define-primitive cddadr 1)
(define-primitive cdddar 1)
(define-primitive cddddr 1)

;; msic. primitives
(define-primitive error 2)


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
          ((set!) (update-env! (cadr expr) env (basic:eval (caddr expr) env)))
          ((lambda) (make-function (cadr expr) (cddr expr) env))
          (else (invoke (basic:eval (car expr) env)
                        (evlis (cdr expr) env)))))))

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
              (evlis (cdr exprs) env))
        '())))


(define hash
  (lambda (key)        ;; stub for possible future need
    key))


(define valid?
  (lambda (value)
    (not (null? value))))


(define lookup-env
  (lambda (key env)
    (if (pair? env)
        (if (eq? (caar env) key)
            (cdar env)
            (lookup-env key (cadr env)))
        (error "LOOKUP - no value bound to symbol " key))))

         
(define update-env!
  (lambda (key env value)
    (let ((variable (lookup key env)))
      (if (valid? variable)
          (begin
            (set-cdr! variable value)
            empty-result))))))


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
