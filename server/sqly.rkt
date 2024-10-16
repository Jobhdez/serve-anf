#lang racket

(provide select insert update delete)

;;;; == macros ==

(define-syntax-rule (select exprs ...)
  (compile-select  `exprs ...))

(define-syntax-rule (insert exprs ...)
  (compile-insert `exprs ...))

(define-syntax-rule (update exprs ...)
  (compile-update `exprs ...))

(define-syntax-rule (delete exprs ...)
  (compile-delete `exprs ...))

;;;; == compilers ==

;;; SELECT
(define (compile-select . expr)
  (match expr
    [(list (list fields ...) (list 'from table))
     (list
      (string-append
      "SELECT "
      (string-join (map symbol->string fields) ",")
      " FROM "
      (symbol->string table)))]
    
    [(list (list fields ...) (list 'from table) (list 'limit val))
     (flatten
      (list
      (string-append
       "SELECT "
      (join-fields fields)
      " FROM "
      (symbol->string table)
      " LIMIT $1")
      (list (field->sql-data val))))]
    
    [(list (list fields ...) (list 'from table) (list 'order-by exp order))
     (flatten
      (list
      (string-append
       "SELECT "
      (join-fields fields)
      " FROM "
      (symbol->string table)
      " ORDER BY $1 "
      (field->sql-data order))
      (list (field->sql-data exp))))]
    
    [(list (list fields ...) (list 'from table) (list 'where (list where-exprs ...)))
     (let [(joined-fields (join-fields fields))
           (whereeq (where->string where-exprs 1))]
       (flatten (list (string-append
                      "SELECT "
                      (join-fields fields)
                      " FROM "
                      (symbol->string table)
                      " WHERE "
                      (car whereeq))
                      
            (car (cdr whereeq)))))]
    
    [(list (list fields ...) (list 'from table) (list 'where (list where-exprs ...)) (list 'order-by exp order))
     (let [(where-exps (where->string where-exprs 1))]
       (flatten
        (list
         (string-append
          "SELECT "
          (join-fields fields) " FROM " (field->sql-data table) " WHERE " (first where-exps) " " " ORDER BY " (field->sql-data exp) " "  (field->sql-data order))
         (cdr where-exps))))]

    [(list (list fields ...) (list 'from table) (list 'left-join table2 'on (list (? symbol? op) (? symbol? e) (? symbol? e2))))
     (flatten (list
      (string-append
       "SELECT "
      (join-fields fields)
      " FROM "
      (field->sql-data table)
      " LEFT JOIN "
      (field->sql-data table2)
      " ON "
      "$1 "
      " "
      (field->sql-data op)
      " $2")
      (list (field->sql-data e) (field->sql-data e2))))]))

(define (where->string exprs counter)
  (match exprs
    [(list (? symbol? op) (? symbol? id) val) ;; e.g., (list '= id 5)
     (list
      (string-append
      (field->sql-data id)
      (field->sql-data op)
      "$"
      (number->string counter))
      (list (field->sql-data val)))]
    
    [(list (? and-or? oper) (list op id val) (list op2 id2 val2))
     (list
      (string-append
       (field->sql-data id)
       " "
      (field->sql-data op)
      " $"
      (number->string counter)
      (if (eq? oper 'and) " AND " " OR ")
      (field->sql-data id2)
      " "
      (field->sql-data op2)
      " $"
      (number->string (+ counter 1)))
      (list (field->sql-data val) (field->sql-data val2)))]

    [(list (? symbol? id) (list 'in fields ...))
     (list
      (string-append
      (field->sql-data id)
      " in "
      "("
      (make-params fields counter)
      ")")
      (map (lambda (f) (field->sql-data f)) fields))]))
      
     

;;; INSERT
(define (compile-insert . exp)
  (match exp
    [(list 'into table-name (list fields ...) (list 'values (list fields-vals ...)))
     (let [(joined-fields (join-fields fields))
           (joined-val-fields (join-fields fields-vals))]
       (flatten
        (list
        (string-append
        "INSERT "
        "into "
        (symbol->string table-name)
        "("
        joined-fields
        ") "
        "values "
        "("
        (make-params fields-vals 1)
        ")")
        (map (lambda (f) (field->sql-data f)) fields-vals))))]))

;;; UPDATE
(define (compile-update . exprs)
  (match exprs
    [(list (? symbol? table) (list 'set (list exps ...)))
     (match exps
       [(list (list '= id val) ...)
        (let [(vals (get-update-field-vals exps))
              (eq-exps (make-update-params exps))]
          (flatten
           (list
           (string-append
            "UPDATE "
            (field->sql-data table)
            " SET "
            (join-equal-exps eq-exps))
           vals)))])]

    [(list (? symbol? val) (list 'set (list exps ...)) (list 'where (list where-exps ...)))
     (match exps
       [(list (list '= id val2) ...)
        (let [(vals (get-update-field-vals exps))
              (eq-exps (make-update-params exps))
              (whereexp (where->string where-exps (+ (length exps) 1)))]
          (flatten
           (list
            (string-append
             "UPDATE "
            (field->sql-data val)
            " SET "
            (join-equal-exps eq-exps)
            " WHERE "
            (car whereexp))
           (flatten (cons vals (second whereexp))))))])]))
         

;;; DELETE
(define (compile-delete . exps)
  (match exps
    [(list 'from table)
     (list
      (string-append
      "DELETE "
      "FROM "
      (field->sql-data table)))]
    [(list (list 'from table) (list 'where (list where-exps ...)))
     (flatten
      (list
      (string-append
      "DELETE "
      "FROM "
      (field->sql-data table)
      " WHERE "
      (first (where->string where-exps 1)))
      (second (where->string where-exps 1))))]))

;;;; == Utils ==
(define (join-fields fields)
  (string-join (map field->sql-data fields) ","))

(define (field->sql-data field)
  (cond [(symbol? field)
         (if (string-is-number? field)
             (string->number (symbol->string field))
             (symbol->string field))]
        [(number? field)
         field]
        
        [else field]))

(define (and-or? e)
  (or (eq? e 'and)
      (eq? e 'or)))

(define (join-equal-exps exps)
  (define (exp->string exp)
    (match exp
      [(list '= id val)
       (string-append
        (field->sql-data id)
       " = "
       (field->sql-data val))]))
  (string-join (map exp->string exps) ", "))

(define (make-params fields counter)
  (string-join
   (for/list ([i fields])
     (let ([param (string-append "$" (number->string counter))])
       (set! counter (+ counter 1))
       param)) ","))


(define (make-update-params fields)
  (for/list ([i fields]
             [counter (in-naturals)])
    (define new-field (list-set i 2 (string-append "$" (number->string (+ counter 1)))))
    new-field))

(define (get-update-field-vals fields)
  (map (lambda (field) (third field)) fields))

(define (string-is-number? sym)
  (not (false? (string->number (symbol->string sym)))))
