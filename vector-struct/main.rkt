#lang racket/base

(require racket/contract/base
         racket/contract/region
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide vecstruct)

(define-syntax (vecstruct stx)
  (syntax-parse stx
    [(_ id:id (field:id ...)
        (~optional (~and #:mutable mutable-kw)))
     ; name of the vecstruct predicate function
     (define/with-syntax predicate?
       (format-id #'id "~a?" #'id))
     ; vector constructor to use (mutable vs immutable)
     (define/with-syntax constructor (if (attribute mutable-kw)
                                         #'vector
                                         #'vector-immutable))
     ; actual size of the backing vector
     (define/with-syntax vec-size (add1 (length (syntax->list #'(field ...)))))
     ; list of getters to generate
     (define/with-syntax ((getter getter-index) ...)
       (for/list ([f (in-list (syntax->list #'(field ...)))]
                  [i (in-naturals 1)])
         (define/with-syntax getter-id (format-id f "~a-~a" #'id f))
         (define/with-syntax getter-index i)
         #'(getter-id getter-index)))
     ; list of setters to generate (if mutable)
     (define/with-syntax ((setter setter-index) ...)
       (if (attribute mutable-kw)
           (for/list ([f (in-list (syntax->list #'(field ...)))]
                      [i (in-naturals 1)])
             (define/with-syntax setter-id (format-id f "set-~a-~a!" #'id f))
             (define/with-syntax setter-index i)
             #'(setter-id setter-index))
           #'()))
     ; macro output
     #'(begin
         ; define constructor
         (define (id field ...)
           (constructor 'id field ...))
         ; define predicate
         (define (predicate? vec)
           (and (vector? vec)
                (= vec-size (vector-length vec))
                (equal? 'id (vector-ref vec 0))))
         ; define getters
         (define/contract (getter vec)
           (predicate? . -> . any)
           (vector-ref vec getter-index))
         ...
         ; define setters (if mutable)
         (define/contract (setter vec value)
           (predicate? any/c . -> . any)
           (vector-set! vec setter-index value))
         ...)]))
