Strong points
=============

-   Separate notions of *type witnesses* and *type representation*
    allows to use many flavours of generic programming, and adapt other
    libraries (examples include Haskell Scrap your boiler plate, LIGD,
    Instant-Generics, RepLib). The type representations are called
    generic views, there is a default one, `Generic_core_desc` which is
    low level and is automatically derived using the `reify` ppx.

-   *Automatic* derivation of the boilerplate for reifying types, using
    the `reify` ppx.

-   *Ad-hoc polymorphism* and overloading through type-indexed
    functions. Implemented as extensible functions over the extensible
    universe of type witnesses. This also makes it possible to emulate
    Haskell *type-classes*.

-   *Abstract types* are respected through a user-provided embedding to
    convert back and force between the abstract implementation and a
    public representation.

-   *Generic traversals* are implemented on top of the library and
    provide powerful combinators to write concise definitions of
    recursive functions over complex tree types.

-   Examples include a *type-safe deserialization* function that
    respects type abstraction.

Resources
=========

-   [Technical Report](./generic.pdf)
-   [Library Reference](./doc/index.html)

Compatibility
=============

Works with OCaml v.4.04.0
