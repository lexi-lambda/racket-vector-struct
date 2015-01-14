#lang scribble/manual

@(require (for-label vector-struct))

@title{Vector Struct: Performant Fake Structs}

@defmodule[vector-struct]

This module provides utilities for "faking" structs using vectors. In Racket, struct allocation can be
expensive, while vectors are relatively cheap. Since the complex featureset of structs is often
unnecessary, this module allows using vectors while still having the convenience of the basic struct
API to aid in clarity.

For a version of this module that is compatible with Typed Racket, see
@racketmodname[typed/vector-struct].

@; ---------------------------------------------------------------------------------------------------

@section{Examples}

Using this library is mostly equivalent to using ordinary structs. To create a structure type, use the
@racket[vecstruct] form.

@(racketblock
  (vecstruct point (x y)))

Just like ordinary structs, this will create a @racket[point] function to serve as the struct
constructor, and it will also define @racket[point-x] and @racket[point-y] accessors.

@#reader scribble/comment-reader
 (racketblock
   (define p (point 4 5))
   (point-x p) ; => 4
   (point-y p) ; => 5
 )

You can also create mutable structs using the @racket[#:mutable] keyword.

@(racketblock
  (vecstruct mpoint (x y) #:mutable))

This will generate the corresponding @racket[set-point-x!] and @racket[set-point-y!] mutators.

The @racket[vecstruct] form also creates a predicate function (e.g. @racket[point?]), which can be
used to test for values. Note that a properly-formed vector will pass this test, but
@racket[vecstruct] puts a symbol corresponding to the vector name as the first element of the vector
to help identify well-formed values.

@; ---------------------------------------------------------------------------------------------------

@section{API}

@defform[(vecstruct id (field-id ...) struct-option ...)
         #:grammar
         [(struct-option #:mutable)]]{

Creates utility functions for creating and manipulating vectors to emulate structs. Defines
@racket[id] as a constructor taking the number of parameters as there are @racket[field-id] elements.
Also defines @racket[id?] as a predicate to identify if a given object fits the proper underlying
struct format.

Subsequently generates a series of @racket[id-field-id] accessors. If @racket[#:mutable] is specified,
also generates @racket[set-id-field-id!] mutators.

If @racket[#:mutable] is specified, then the underlying datatype will be a mutable vector, otherwise
it will be an immutable vector.

}
