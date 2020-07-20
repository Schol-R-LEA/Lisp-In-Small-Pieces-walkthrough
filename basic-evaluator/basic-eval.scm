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
             (let* ((min-arity (abs <arity>))
                    (variadic (> 0 <arity>))
                    (comparison (if variadic <= =)))
               (if (comparison min-arity (length values))
                   (apply <primitive> values)
                   (begin 
                     (display "PROCEDURE-APPLICATION: Incorrect arity for fn ")
                     (display <name>)
                     (display ": expected ")
                     (display min-arity)
                     (if variadic 
                         (display '+'))
                     (display ", got ")
                     (display (length values))))))
           in <env>))
      ((_ <name> <primitive> <arity>)
       #`(define-primitive <name> <primitive> <arity> in env.global))
      ((_ <primitive-name> <arity> in <env>)
       #`(define-primitive <primitive-name> <primitive-name> <arity> in <env>))
      ((_ <primitive-name> <arity>)
       #`(define-primitive <primitive-name> <arity> in env.global)))))


(define-syntax-rule (define-primitives-by-arity arity primitives ...)
  (begin
    (define-primitive primitives arity)
    ...))


(define-initial t #t)
(define-initial f the-false-value)
(define-initial nil the-null-value)


(define-primitives-by-arity 0 exit)

(define-primitives-by-arity 1 
  null? procedure? pair? number? symbol? vector? char?
  positive? negative?
  integer? real? complex? rational? exact? inexact? exact-integer?
  exact->inexact inexact->exact
  real-part imag-part numerator denominator
  not negate abs
  exact-integer-sqrt
  car cdr
  caar cadr cddr cdar 
  caaar caadr caddr cadar cddar cdddr cdadr cdaar 
  caaaar caaadr caaddr caadar caddar cadddr cadadr cadaar  
  cddaar cdaaar cdaadr cdaddr cdadar cdddar cddddr cddadr 
  )

(define-primitives-by-arity 2  
  / 
  cons
  eq? eqv? equal? 
  = < <= >= >
  string=? string<? string<=? string>=? string>?
  string-ci=?  string-ci<? string-ci<=? string-ci>=? string-ci>?
  char=? char<? char<=? char>=? char>? 
  char-ci=? char-ci<? char-ci<=? char-ci>=? char-ci>? 
  )

(define-primitives-by-arity -1 + -)


(define basic:eval
  (lambda (expr env)
    (if (atom? expr)
        (cond 
         [(symbol? expr)
          (let ((value (lookup-env expr env)))
            (if value
                value
                (error "No bound value for " expr)))]
         [(or (number? expr) (string? expr) (char? expr) (boolean? expr) (vector? expr) (procedure? expr))
          expr]
         [(eq? expr the-null-value)
          '()]
         [else (error "EVAL - cannot evaluate atom" expr)])
        (case (car expr)
          [(quote) (cadr expr)]
          [(if) (if (basic:eval (cadr expr) env)
                    (basic:eval (caddr expr) env)
                    (basic:eval (cadddr expr) env))]
          [(begin) (eprogn (cdr expr) env)]
          [(set!) (update-env! (cadr expr) env (basic:eval (caddr expr) env))]
          [(lambda) (make-function (cadr expr) (cddr expr) env)]
          [else 
           (basic:apply (basic:eval (car expr) env)
                        (evlis (cdr expr) env))]))))


(define basic:apply
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
        (cond
         [(null? env)
          (error "LOOKUP - unbound variable " key)
          '()]
         [(pair? env)
          (if (eq? (caar env) key)
              (cdar env)
              (lookup-env key (cdr env)))]
         [else (error "LOOKUP - malformed environment" env)])
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
    (cond [(pair? variables)
           (if (pair? values)
               (cons (cons (car variables) (car values))
                     (extend-env env (cdr variables) (cdr values)))
               (error "Too few values" values))]
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
      (display ">>> ")
      (let ((result (basic:eval (read) env)))
        (display result)
        (loop env)))))

(basic:repl)
