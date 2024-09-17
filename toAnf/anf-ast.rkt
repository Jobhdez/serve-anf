#lang racket
(require racket/struct)
(provide (all-defined-out))

(struct atomic (atom)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'atomic)
      (lambda (obj) (list (atomic-atom obj)))))])

(struct atomic-assignment (var expr)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'atomic-assignment)
      (lambda (obj) (list (atomic-assignment-var obj) (atomic-assignment-expr obj)))))])


(struct atomic-plus (e e2)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'atomic-plus)
      (lambda (obj) (list (atomic-plus-e obj) (atomic-plus-e2 obj)))))])


(struct atomic-print (e)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'atomic-print)
      (lambda (obj) (list (atomic-print-e obj)))))])

(struct atomic-minus (e e2)
  #:transparent
    #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'atomic-minus)
      (lambda (obj) (list (atomic-minus-e obj) (atomic-minus-e2 obj)))))])
(struct anf-bool (bool)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'anf-bool)
      (lambda (obj) (list (anf-bool-bool obj)))))])

(struct anf-equiv (e e2)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'anf-equiv)
      (lambda (obj) (list (anf-equiv-e obj) (anf-equiv-e2 obj)))))])

(struct anf-less (e e2)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'anf-less)
      (lambda (obj) (list (anf-less-e obj) (anf-less-e2 obj)))))])

(struct anf-greater (e e2)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'anf-greater)
      (lambda (obj) (list (anf-greater-e obj) (anf-greater-e2 obj)))))])

(struct anf-equiv-not (e e2)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'anf-equiv-not)
      (lambda (obj) (list (anf-equiv-not-e obj) (anf-equiv-not-e2 obj)))))])

(struct anf-if-exp (cnd thn els)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'anf-if-exp)
      (lambda (obj) (list (anf-if-exp-cnd obj) (anf-if-exp-thn obj) (anf-if-exp-els obj)))))])

(struct anf-while (cnd stmts)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'anf-while)
      (lambda (obj) (list (anf-while-cnd obj) (anf-while-stmts obj)))))])

(struct anf-assign (var e)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'anf-assign)
      (lambda (obj) (list (anf-assign-var obj) (anf-assign-e obj)))))])
