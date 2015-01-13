#lang typed/racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(define-syntax (vecstruct stx)
  (syntax-parse stx #:literals (:)
    [(_ id:id ([field:id : type:expr] ...)
        (~optional (~and #:mutable mutable-kw)))
     ; submodule names
     (define/with-syntax untyped-submod-name
       (datum->syntax #'id (gensym 'vecstruct-submodule-untyped.)))
     (define/with-syntax typed-submod-name
       (datum->syntax #'id (gensym 'vecstruct-submodule-typed.)))
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
     (define/with-syntax ((getter getter-type getter-index) ...)
       (for/list ([f (in-list (syntax->list #'(field ...)))]
                  [t (in-list (syntax->list #'(type ...)))]
                  [i (in-naturals 1)])
         (define/with-syntax getter-id (format-id f "~a-~a" #'id f))
         (define/with-syntax getter-type t)
         (define/with-syntax getter-index i)
         #'(getter-id getter-type getter-index)))
     ; list of setters to generate (if mutable)
     (define/with-syntax ((setter setter-type setter-index) ...)
       (if (attribute mutable-kw)
           (for/list ([f (in-list (syntax->list #'(field ...)))]
                      [t (in-list (syntax->list #'(type ...)))]
                      [i (in-naturals 1)])
             (define/with-syntax setter-id (format-id f "set-~a-~a!" #'id f))
             (define/with-syntax setter-type t)
             (define/with-syntax setter-index i)
             #'(setter-id setter-type setter-index))
           #'()))
     ; macro output
     ;
     ; Ideally, this would work using an internal untyped submodule, then use
     ; require/typed's #:opaque to make the type entirely opaque to the typechecker
     ; and make predicate? work properly. Unfortunately, TR doesn't play nice with
     ; submodules right now, so that doesn't work.
     #'(begin
         ; define type
         (define-type id (Vector 'id type ...) #:omit-define-syntaxes)
         ; define constructor
         (define (id [field : type] ...) : id
           (constructor 'id field ...))
         ; define predicate
         (define (predicate? vec)
           (and (vector? vec)
                (= vec-size (vector-length vec))
                (equal? 'id (vector-ref vec 0))))
         ; define getters
         (define (getter [vec : id]) : getter-type
           (vector-ref vec getter-index))
         ...
         ; define setters (if mutable)
         (define (setter [vec : id] [value : setter-type]) : Void
           (vector-set! vec setter-index value))
         ...)]))
