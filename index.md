A Generic Programming Library for OCaml
=======================================

For OCaml v.4.04.0

Strong points
-------------

-   Separate notions of type witnesses and type representation allows to
    use many flavours of generic programming, and adapt other libraries
    (examples include Haskell Scrap your boiler plate, LIGD,
    Instant-Generics, RepLib). The type representations are called
    generic views, there is a default one, which is low level and is
    automatically derived using the ppx.

-   Automatic derivation of the boilerplate for reifying types, using
    the ppx.

-   Ad-hoc overloading through type-indexed functions. Implemented as
    extensible functions over the extensible universe of type witnesses.

-   Abstract types are respected through a user-provided embedding to
    convert back and force between the abstract implementation and a
    public representation.

-   Generic traversals are implemented on top of the library and provide
    powerful combinators to write concise definitions of recursive
    functions over complex tree types.

-   Examples include a type-safe deserialization function that respects
    type abstraction.

Resources
---------

-   [Technical Report](./generic.pdf)
-   [Library Reference](./doc/index.html)
-   [Github Repository](https://github.com/balez/generic/)

