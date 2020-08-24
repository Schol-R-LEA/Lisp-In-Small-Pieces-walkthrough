(define void '*unspecified*)
(define env.init  '())
(define env.global env.init)
(define parent-env '**parent**)
(define the-true-value '**true**)
(define the-false-value '**false**)
(define the-null-value '**null**)
(define trace #f)
(define restore-repl '())
(define indent-amount 1)


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
    (syntax-case macro (in is-variadic?)
      ((_ <name> <primitive> <arity> is-variadic? <variadic> in <env>)
       #`(define-initial <name>
           (lambda (values)
             (let((comparison (if <variadic> <= =)))
               (if (comparison <arity> (length values))
                   (apply <primitive> values)
                   (begin 
                     (display "PROCEDURE-APPLICATION: Incorrect arity for fn ")
                     (display <name>)
                     (display ": expected ")
                     (display <arity>)
                     (if <variadic> 
                         (display '+'))
                     (display ", got ")
                     (display (length values))))))
           in <env>))
      ((_ <name> <primitive> <arity> in <env>)
       #`(define-primitive <name> <primitive> <arity> is-variadic? #f in <env>))
      ((_ <name> <primitive> <arity> is-variadic? <variadic>)
       #`(define-primitive <name> <primitive> <arity> is-variadic? <variadic> in env.global))
      ((_ <primitive-name> <arity> is-variadic? <variadic> in <env>)
       #`(define-primitive <primitive-name> <primitive-name> <arity> is-variadic? <variadic> in <env>))
      ((_ <primitive-name> <arity> is-variadic? <variadic>)
       #`(define-primitive <primitive-name> <arity>  is-variadic? <variadic> in env.global))
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
  cddaar cdaaar cdaadr cdaddr cdadar cdddar cddddr cddadr)

(define-primitives-by-arity 2
  cons
  eq? eqv? equal? 
  = < <= >= >
  string=? string<? string<=? string>=? string>?
  string-ci=?  string-ci<? string-ci<=? string-ci>=? string-ci>?
  char=? char<? char<=? char>=? char>? 
  char-ci=? char-ci<? char-ci<=? char-ci>=? char-ci>?)


(define-primitive display 1 is-variadic? #t)
(define-primitive error 1 is-variadic? #t)
(define-primitive read 0 is-variadic? #t)

(define-primitive + 1 is-variadic? #t)
(define-primitive - 1 is-variadic? #t)

(define-primitive * 2 is-variadic? #t)
(define-primitive / 2 is-variadic? #t)
(define-primitive list 2 is-variadic? #t)


(define basic:eval
  (lambda (expr env)
    (if (atom? expr)
        (cond
         [(eq? expr #t) the-true-value]
         [(eq? expr #f) the-false-value]
         [(symbol? expr)
          (let ((value (lookup-env expr env)))
            (if value
                (begin
                  (if trace 
                      (begin
                        (indent)
                        (display expr)
                        (display " == ")
                        (display value)
                        (newline)))
                  value)
                (error "EVAL - No bound value for" expr)))]
         [(or (number? expr) (string? expr) (char? expr) (boolean? expr) (vector? expr) (procedure? expr))
          (if trace 
              (begin
                (indent)
                (display expr)
                (newline)))
          expr]
         [(eq? expr the-null-value)
          '()]
         [else (error "EVAL - cannot evaluate atom" expr)])
        (case (car expr)
          [(trace) 
           (set! trace (case (cadr expr) 
                         [(on) #t]
                         [(off) #f]
                         [else (error "EVAL - not a valid tracing option" (cadr expr))]))]
          [(quote) (cadr expr)]
          [(unquote) (basic:eval (cadr expr) env)]
          [(if) (if (basic:eval (cadr expr) env)
                    (basic:eval (caddr expr) env)
                    (basic:eval (cadddr expr) env))]
          [(or) (if (basic:eval (cadr expr) env)
                    #t
                    (if (basic:eval (caddr expr) env)
                        #t
                        #f))]
          [(and) (if (basic:eval (cadr expr) env)
                     (if (basic:eval (caddr expr) env)
                         #t
                         #f)
                     #f)]
          [(begin) (eprogn (cdr expr) env)]
          [(set!) (update-env! (cadr expr) env (basic:eval (caddr expr) env))]
          [(lambda) (make-function (cadr expr) (cddr expr) env)]
          [(define)
           (let ((signature (cadr expr))
                 (value (caddr expr)))
             (cond 
              [(null? signature)
               (error "DEFINE - no symbol to define: " expr)]
              [(atom? signature)
               (set! env.global (extend-env env.global signature (basic:eval value env)))
               signature]
              [(pair? signature)
               (set! env.global (extend-env env.global (car signature) (make-function (cdr signature) value env)))
               (car signature)]))]
          [(get-environment) env]
          [(eval) 
           (if trace
               (begin 
                 (indent)
                 (display (car expr))
                 (newline)
                 (set! indent-amount (+ indent-amount 1))))
           (let  ((result (basic:eval (car expr) (cadr expr))))
             (if trace 
                 (set! indent-amount (- indent-amount 1)))
             result)]
          [(apply) (basic:apply (car expr) (cadr expr))]
          [else 
           (if trace
               (begin 
                 (set! indent-amount (+ indent-amount 1))
                 (indent)
                 (display "(")
                 (display (car expr))
                 (display " ")
                 (display (cdr expr))
                 (display ")")               
                 (newline)
                 (set! indent-amount (- indent-amount 1))))
           (basic:apply (basic:eval (car expr) env)
                        (evlis (cdr expr) env))]))))


(define basic:apply
  (lambda (fn args)
    (if (procedure? fn)
        (fn args)
        (error "APPLY - not a function:" fn))))


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
              (if (pair? (cdr exprs)) 
                  (evlis (cdr exprs) env)
                  (cdr exprs)))
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
            (if (eq? (caar env) key)
                (cdar env)
                (lookup-env key (cdr env)))
            (error "LOOKUP-ENV - unbound" key))
        (error "LOOKUP-ENV - malformed key" key))))


(define (update-env! key env value)
  (if (pair? env)
      (if (eq? (caar env) key)
          (begin
            (set-cdr! (car env) value)
            value)
          (update-env! key (cdr env) value))
      (error "UPDATE-ENV! - Empty environment.")))


(define extend-env
  (lambda (env variables values)
    (cons (cons variables value) env)))


(define indent
  (lambda ()
    (let loop ((indent-amt indent-amount))
      (if (> indent-amt 0)
          (begin 
            (display #\tab)
            (loop (- indent-amt 1)))))))


;;; simple REPL
(define basic:repl
  (lambda ()
    (display "Basic evaluator test REPL")
    (let loop ((env env.global))      
      (newline)
      (display ">>> ")
      (let ((result (basic:eval (read) env)))
        (if (not (eqv? result void)) 
            (display result))
        (loop env.global)))))

(basic:repl)
