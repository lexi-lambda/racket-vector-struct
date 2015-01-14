#lang scribble/manual

@(require (for-label typed/vector-struct))

@title{Vector Struct: Performant Fake Structs}

@defmodule[typed/vector-struct]

This module is a typed variant of the @racketmodname[vector-struct] module. See its documentation for
examples and additional information.

@; ---------------------------------------------------------------------------------------------------

@section{API}

@defform[#:literals (:)
         (vecstruct id ([field-id : field-type] ...) struct-option ...)
         #:grammar
         [(struct-option #:mutable)]]{

Creates utility functions for creating and manipulating vectors to emulate structs. Defines
@racket[id] as a constructor taking the number of parameters as there are @racket[field-id] elements.
Each element is typed with the corresponding @racket[field-type]. Also defines @racket[id?] as a
predicate to identify if a given object fits the proper underlying struct format.

Subsequently generates a series of @racket[id-field-id] accessors. If @racket[#:mutable] is specified,
also generates @racket[set-id-field-id!] mutators.

If @racket[#:mutable] is specified, then the underlying datatype will be a mutable vector, otherwise
it will be an immutable vector.

}
