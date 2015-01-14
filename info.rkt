#lang info

(define collection 'multi)

(define pkg-desc "macros for creating fake structs backed by vectors for performance")
(define pkg-authors '(lexi.lambda))
(define version "0.1.0")

(define deps
  '("base"
    "typed-racket-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))
