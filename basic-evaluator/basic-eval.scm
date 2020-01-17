(define void '*unspecified*)
(define env.init  '())
(define env.global env.init)
(define parent-env '**parent**)
(define the-false-value '**false**)
(define the-null-value '**null**)


(define-syntax define-initial
  (lambda (macro)
    (syntax-case macro (in)
      ((_ <name> <value> in <env>)
       (identifier? #'<name>)        
       #`(begin
           (set! <env> (cons (cons '<name> <value>) <env>))
           '<name>))
      ((_ <name> <value>)
       #`(define-initial <name> <value> in env.global))
      ((_ <name> in <env>)
       #`(define-initial <name> void in <env>))
      ((_ <name>)
       #`(define-initial <name> void in env.global)))))


(define-syntax define-primitive
  (lambda (macro)
    (syntax-case macro (in)
      ((_ <name> <primitive> <arity> in <env>)
       #`(define-initial <name>
           (lambda (values)
             (if (= <arity> (length values))
                 (apply <primitive> values)
                 (begin 
                   (display "PROCEDURE-APPLICATION: Incorrect arity for fn")
                   (display <name>)
                   (display ": expect ")
                   (display <arity>)
                   (display ", got ")
                   (display (length values)))))
           in <env>))
      ((_ <name> <primitive> <arity>)
       #`(define-primitive <name> <primitive> <arity> in env.global))
      ((_ <primitive-name> <arity> in <env>)
       #`(define-primitive <primitive-name> <primitive-name> <arity> in <env>))
      ((_ <primitive-name> <arity>)
       #`(define-primitive <primitive-name> <arity> in env.global)))))

(define-initial t #t)
(define-initial f the-false-value)
(define-initial nil the-null-value)

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
;; (define-primitive and 2)
;; (define-primitive or 2)

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
               (let ((value (lookup-env expr env)))
                 (if value
                     value
                     (error "No bound value for " expr))))
              ((or (number? expr) (string? expr) (char? expr) (boolean? expr) (vector? expr))
               expr)
              ((eq? expr the-null-value)
               '())
              (else (error "EVAL - cannot evaluate atom" expr)))
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
        void)))


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


(define match
  (lambda (entry key)
    (if (eq? (car entry) key)
        (cdr entry)
        #f)))

(define lookup-env
  (lambda (key env)
    (if (symbol? key)
        (if (pair? env)
            (let lookup-loop 
                ((entry (car env))
                 (entries (cdr env)))
              (cond 
               ((null? entry) #f)
               ((pair? entry)
                (let ((key-candidate (car entry))
                      (value-candidate (cdr entry)))
                  (cond 
                   ((eq? key key-candidate) value-candidate)
                   ((pair? key-candidate)
                    (let ((result-candidate
                           (lookup-loop (car key-candidate) (cdr key-candidate))))
                      (if result-candidate
                          result-candidate
                          (lookup-loop (car entries) (cdr entries)))
                      (if (pair? entries)
                          (lookup-loop (car entries) (cdr entries))
                          #f)))
                   ((pair? entries) (lookup-loop (car entries) (cdr entries)))
                   (else #f))))
               (else #f)))
            (error "LOOKUP - malformed environment" env))
        (error "LOOKUP - malformed key" key))))


(define update-env!
  (lambda (key env value)
    (let ((variable (lookup-env key env)))
      (if (valid? variable)
          (begin
            (set-cdr! variable value)
            void)))))


(define extend-env
  (lambda (env variables values)
    (cond ((pair? variables)
           (if (pair? values)
               (cons (cons (car variables) (car values))
                     (extend-env env (cdr variables) (cdr values)))
               (error "Too few values" values)))
          ((null? variables)
           (if (null? values)
               env
               (error "Too few variables" variables)))
          ((symbol? variables)
           (cons (cons variables values) env))
          (else (error "EXTEND - variable key must be a symbol" variables)))))

;;; simple REPL



(define basic:repl
  (lambda ()
    (display "Basic evaluator test REPL")
    (let loop ((env env.global))
      (display #\newline)
      (display #\newline)
      (display ">>> ")
      (let ((result (basic:eval (read) env)))
        (display result)
        (loop env)))))

(basic:repl)
