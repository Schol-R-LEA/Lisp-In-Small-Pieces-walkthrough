
(define prop-lists
  (lambda (select)
    (let*
        ((p-lists '((<unusable> (unusable . unusable))))      ;; initial empty list
         (get-lists (lambda () p-lists))
         (find                           ;; utility function to seek through p-list for a given symbol
          (lambda (sym lst)
            (if (not (symbol? sym))
                #f
                (let seek ((remaining lst))
                  (cond
                   ((or (null? remaining) (null? (car remaining))) #f)
                   ((eq? sym (caar remaining))
                    (cdar remaining))
                   (else (seek (cdr remaining))))))))
         (get-prop                       ;; retrieve a property from a given symbol's p-list
          (lambda (sym key)
            (let ((props (find sym p-lists)))
              (if props
                  (find key props)
                  #f))))
         (put-prop
          (lambda (sym key value)
            (let ((p-list (find sym p-lists)))
                (if p-list
                    (set-cdr! p-list (cons key value))
                    (append! p-lists (list (list sym (cons key value)))))))))
      (case select
        ((get-lists) get-lists)  
        ((find-local) find)
        ((find) (lambda (sym) (find sym (get-lists))))
        ((get-prop) get-prop)
        ((put-prop) put-prop)))))


(define find (prop-lists 'find)) 
(define find-local (prop-lists 'find-local)) 
(define get-prop (prop-lists 'get-prop))
(define put-prop (prop-lists 'put-prop))
(define get-p-lists  (prop-lists 'get-lists))

(put-prop 'a 'b 'c)
(find 'a)
(get-prop 'a 'b)
(put-prop 'a 'b 'd)
(find 'a)
(get-prop 'a 'b)
(get-p-lists)
(find-local 'a '((<unknown> (unknown . unknown)) (a (b . e))))
