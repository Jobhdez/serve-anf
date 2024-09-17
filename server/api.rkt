#lang racket
(require json)
(require web-server/servlet)
(require web-server/servlet-env)
(require "../parser/pyparser.rkt"
         "../toANF/to-anf.rkt"
         "database.rkt")
(require racket/list
         racket/string
         racket/class
         racquel
         db)

(require racket/struct)

(define (parse-json-body req)
  (bytes->jsexpr (request-post-data/raw req)))

(define (get-hash-value h v)
  (hash-ref h v))

(define (post-values req)
  (define get-property
    (curry get-hash-value (parse-json-body req)))
  (define expr (get-property 'input-exp))
  (define ast (parse-expression expr))
  (define anf (format "~a" (car (syntax->anf ast))))
  (query-exec the-db (format "insert into a_normal_forms(input_exp, ast, anf_exp) values ('~a', '~a', '~a')" expr ast anf))
  (response/jsexpr
   (hasheq 'exp anf)))

(define (view-expression req pid)
  (define e (query-rows the-db (format "select * from a_normal_forms where id = ~a" pid)))
  (define data-list (car (map vector->list e)))
  (response/jsexpr
   (hasheq 'exp (hasheq 'id (first data-list)
                        'input-exp (second data-list)
                        'ast (third data-list)
                        'anf (fourth data-list)))))

(define (get-anf-exps req)
  (define data (query-rows the-db (format "select * from a_normal_forms")))
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
