#lang racket
(require json)
(require web-server/servlet)
(require web-server/servlet-env)
(require "../parser/pyparser.rkt"
         "../toAnf/to-anf.rkt"
         "sqly.rkt"
         "database.rkt")
(require racket/list
         racket/string
         racket/class
         racquel
         db)

(require racket/struct)

(define (exec-sql sql db)
  (apply query-exec db sql))

(define (insert-values fields vals)
  (insert into anf ,fields (values ,vals)))

(define (select-id fields id)
  (select ,fields (from anf) (where (= id ,id))))

(define (select-all fields)
  (select ,fields (from anf)))

(define (parse-json-body req)
  (bytes->jsexpr (request-post-data/raw req)))

(define (get-hash-value h v)
  (hash-ref h v))

(define (post-values req)
  (define get-property
    (curry get-hash-value (parse-json-body req)))
  (define expr (get-property 'input-exp))
  (define ast (format "~a" (parse-expression expr)))
  (define anf (format "~a" (car (syntax->anf (parse-expression expr)))))
  (define insert-stmt (flatten (insert-values '(input_exp ast anf_exp) (list expr ast anf))))
  (exec-sql insert-stmt the-db)
  (response/jsexpr
   (hasheq 'exp anf)))

(define (view-expression req pid)
  (define e (apply query-rows the-db (flatten (select-id '(*) pid))))
  (define data-list (car (map vector->list e)))
  (response/jsexpr
   (hasheq 'exp (hasheq 'id (first data-list)
                        'input-exp (second data-list)
                        'ast (third data-list)
                        'anf (fourth data-list)))))

(define (get-anf-exps req)
  (define all (select-all '(*)))
  (define data (apply query-rows the-db all))
  (define data-list (map vector->list data))
  (define data-hash (map (lambda (row) (hasheq 'id (first row)
                                               'exp (second row)
                                               'ast (third row)
                                               'anf (fourth row)))
                         data-list))
    
  (response/jsexpr
   (hasheq 'exp data-hash)))

(define-values (dispatch req)
  (dispatch-rules
   [("anfexps") #:method "post" post-values]
   [("anfexps" (integer-arg)) #:method "get" view-expression]
   [("anfexps") #:method "get" get-anf-exps]))

(serve/servlet
 (lambda (req) (dispatch req))
 #:launch-browser? #f
 #:quit? #f
 #:port 8080
 #:servlet-path "/"
 #:servlet-regexp #rx"")
