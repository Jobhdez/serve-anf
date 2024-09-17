#lang racket/base

(require db dotenv)

(define (getenv-or-die var)
  (unless (string? var)
    (error "Variable should be a string:" var))
  (define val (getenv var))
  (unless (string? val)
    (error (format "Mandatory environment variable \"~a\" is unset." var)))
  val)

(define (load-db)
  (dotenv-load! (list ".env"))
  (define host (getenv-or-die "DB_HOST"))
  (define port (getenv-or-die "DB_PORT"))
  (define user (getenv-or-die "DB_USER"))
  (define pass (getenv-or-die "DB_PASSWORD"))
  (define database (getenv-or-die "DB_DATABASE"))
  (define port/number (string->number port
                                      10
                                      'number-or-false))
  (unless (integer? port/number)
    (error "Port cannot be understood as an integer:"
           port))
  (unless (> port/number 0)
    (error "Port should be positive:" port/number))
  (postgresql-data-source
   #:user user
   #:port port/number
   #:server host
   #:password pass
   #:database database))

(define db-source (load-db))

(define (connect!)
  (dsn-connect db-source))

(define the-db
  (virtual-connection
   (connection-pool connect!)))

(provide the-db)

;;; note for future self
;; to start the database and make a table do this ` psql -d racket -U racket`
