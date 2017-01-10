(** Type description: a low level generic view of OCaml types.

    This module defines the datatype part of the generic view: ['a desc].
    The module {!Generic_core_desc_fun} defines the function part of the generic view:
    {!Generic_core_desc_fun.view} [: 'a ty -> 'a desc] returns the type description of a type code.

    The type description reflects the definition of a type: it gives information such as:

    - the name of the type
    - the module where it is defined
    - the number of type parameters it takes
    - its category which can be an array, a product, a record, a variant, a
      class, an extensible variant, a custom type, a class, a synonym, an abstract type.

    For each category, further information is given: for
    variant types, we may access the list describing their
    constructors, their names and the types of their
    arguments. For record types we may access a list describing their
    fields, their names and types. Et cetera.
*)

open Generic_core
open Generic_util
open Ty.T

(** Generic representation of variant constructors. *)
module Con : sig

  (** {b Constructor description.}

      [('t, 'v) desc] describes one of the constructors of variant ['v] of type ['t].

      - ['t] is a product type witnessed by the field [args].
      - The field [embed] is used to build a value of the
        variant type corresponding to the constructor.
      - The field [proj] tries to deconstruct the variant
        value using that constructor, if it succeeds, the
        arguments are returned.

      {b Caveat.}

      A constructor [C of (a * b)] is treated differently as
      constructor [C of a * b].
      We must therefore represent them differently:
      the first one with : [{ args = Product.p1 (Pair a b) ; ...}]
      and the second one with: [{ args = Product.p2 a b ; ...}]

      {b Caution!}

      [embed] must not inspect its arguments either by
      pattern matching on them or by applying a function on
      them.  [embed] should only apply the data constructor
      to its argument, for instance the [embed] function for
      the data-constructor [Some : 'a -> 'a option] is [embed
      = fun x -> Some x].

  *)
  type ('t, 'v) desc = {
    name : string; (** name of the constructor *)
    args : 't Product.t; (** arguments of the constructor *)
    embed : 't -> 'v;  (** applies the constructor to the arguments. *)
    proj : 'v -> 't option; (** tries to deconstruct that constructor *)
  }

  (** {b Constructor.}
      The type ['t] of the constructor arguments is hidden by existential quantification.
  *)
  type 'v con = Con : ('t, 'v) desc -> 'v con

  (** Synonym for convenience. *)
  type 'v t = 'v con

  val name : 'v con -> string
  val make :
    string -> 'a Product.t -> ('a -> 'b) -> ('b -> 'a option) -> 'b con
  (** Builds a [con] using the corresponding field values *)

  val c0 : string -> 'a -> 'a con
  (** Builds a constant constructor. {b Example:} [c0 "None" None] *)

  val arity : 'a con -> int
  (** The arity of a constructor is the number of its arguments.
      {b Caveat}: [C of int * bool] has arity 2 whereas
      [C of (int * bool)] has arity 1: a single argument which is a pair.
  *)

  (** {b Constructor Application.}
      A pair of a constructor and its arguments.
  *)
  type 'v conap = Conap : ('a, 'v) desc * 'a -> 'v conap

  val con : 'v conap -> 'v con
  (** @return the constructor of a constructor application O(1). *)

  val subterms_prod : 'v conap -> Product.dynprod
  (** @return the arguments of a constructor application as a dynamic product. O(1). *)

  val subterms : 'v conap -> Ty.dyn list
  (** @return the arguments of a constructor application as a list of dynamic values. O(1).  *)
end (* Con *)

(** Generic representation of variant datatypes. *)
module Variant : sig

  (**  A variant is described as a set of constructors.  Each
       constructor is described by its name, the types of its
       arguments given as a nested product, a function to embed a
       value of the nested product to the variant type, and a
       partial projection function that only succeeds when the
       value has the same constructor.
  *)


  (** {b Set of constructors}.
      ['v cons] is the set of constructors of variant ['v], it
      is an abstract type.  We may access the constructors using {!list_of_cons},
      or using {!cst} and {!ncst} which respectively return
      constant and non-constant constructors in the order that
      they are listed in the datatype definition.
      [cst_get] and [ncst_get] offer O(1) access to each constructor.
  *)
  type 'v cons

  val cons : 'v Con.t list -> 'v cons
  (** Builds a ['v cons] from a list of constructor descriptions.

      The constructors do not necessarily need to be given in
      the same order as in the datatype definition, but it is
      good practice to do so.
  *)

  val cst_len : 'v cons -> int
  (** O(1). [cst_len cs] is the number of constant constructors in [cs]. *)

  val cst_get : 'v cons -> int -> 'v Con.t
  (** O(1). [cst_get cs i] returns the [i]{^th} constant constructors
      of [cs] (in the order of the datatype definition).

      Indices start at [0] and end at [cst_len cs - 1].
      @raise Invalid_argument if [i < 0] or [i >= cst_len cs]
  *)

  val cst : 'v cons -> 'v Con.t list
  (** [cst cs] returns the list of the constant constructors in [cs]. *)

  val ncst_len : 'v cons -> int
  (** O(1). [ncst_len cs] is the number of non-constant constructors in [cs]. *)

  val ncst_get : 'v cons -> int -> 'v Con.t
  (** O(1). [ncst_get cs i] returns the [i]{^th} non-constant
      constructors of [cs] (in the order of the datatype
      definition).

      Indices start at [0] and end at [ncst_len cs - 1].
      @raise Invalid_argument if [i < 0] or [i >= ncst_len cs]
  *)

  val ncst : 'v cons -> 'v Con.t list
  (** [ncst cs] returns the list of the non-constant constructors in [cs]. *)

  val list_of_cons : 'v cons -> 'v Con.t list
  (** @return the list of constructors corresponding to
      the abstract [cons] value.  [list_of_cons (cons cs)] is a
      permutation of [cs].
  *)

  val conap : 'v cons -> 'v -> 'v Con.conap
  (** O(1). Deconstructs a value into a pair of a constructor and its arguments.
      @raise Invalid_argument if ['v cons] doesn't reflect all the possible
      constructors of variant ['v].
  *)

  (** {b Variant.}
      A variant is given by its [name] the module in which it was defined and the set [cons] of its constructors.
  *)
  type 'v variant = { name : string; module_path : string list; cons : 'v cons; }

  (** Synonym for convenience. *)
  type 'v t = 'v variant
end (* Variant *)

(** Generic representation of extensible variants. *)
module Ext : sig

  (** Extensible variants allow new constructors to be added to
      a type after it has been defined. The generic view for
      extensible variant must also be extensible so that the
      description of the new constructors may be added to the
      description of the extensible variant.

      We define an abstract type ['a extensible] to describe
      the type ['a] with the following operations:
      - adding a new constructor description
      - iterating through the list of existing constructors
      - find the constructor of a given value
      - deconstruct a value into its constructor and its arguments
  *)

  (** A constructor of an extensible type [('a0,...'an) t] is
      described by a value of type [con] which is a partial
      polymorphic function that computes the constructor
      representation for any closed type expression
      [(s0,...,sn) t] (without any type variable).
      Thus the [con] function may only be called on the witnesses
      of type [(s0,...sn) t ty].
  *)
  type con = { con : 'a. 'a ty -> 'a Con.t; }

  type cons
  type 'a extensible = {
    name : string;
    ty : 'a ty;
    cons : cons;
  }

  (** Synonym for convenience. *)
  type 'a t = 'a extensible

  val create : unit -> cons
  (** Initialises the collection of constructors for an extensible type *)

  val add_con : 'a extensible -> con -> unit
  (** [add_con ext c] adds [c] to the constructors of
      extensible type described by [ext] *)

  val fold : 'a extensible -> ('a Con.t -> 'b -> 'b) -> 'b -> 'b
  (** [fold ext] computes [List.foldr] on the constructor list. *)

  val iter : 'a extensible -> ('a Con.t -> unit) -> unit
  (** Executes an effectful function on each of the (known)
      constructors of an extensible datatype.  *)

  val con : 'a extensible -> 'a -> 'a Con.t
  (** [con ext x] @return the constructor description corresponding to the constructor of
      [x]
      @raise Invalid_argument if the type is not extensible.
      @raise Not_found if the constructor hasn't been added previously.
  *)

  val conap : 'a extensible -> 'a -> 'a Con.conap
  (** Deconstructs a value into a pair of a constructor and its arguments.
      @raise Invalid_argument if the type is not extensible.
      @raise Not_found if the constructor hasn't been added previously.
  *)

  val fix : 'a extensible -> 'a -> 'a
  (** Serialising/Deserializing an extensible value (including
      exceptions) doesn't preserve structural equality:

      {[
        umarshall (marshall x) <> x
      ]}
      In particular pattern matching the deserialized value
      doesn't work as expected. [fix] fixes that: when
      given a deserialized value, it returns a value
      structurally equal to the one that was serialized:

      {[
        fix (unmarshall (marshall x)) = x
      ]}

      In details, when an extensible value has been
      deserialized, it's memory representation will be
      different from that of the value before it was
      serialized.  This is because constructors of extensible
      variants are implemented as object blocks (same as OCaml
      objects), and they get assigned a unique identifier when
      created. [fix ext x] will replace the constructor
      of [x] with the original constructor object that is
      stored in the ['a extensible] datastructure [ext].

      @raise Invalid_argument if the type is not extensible.
      @raise Not_found if the constructor hasn't been added
      previously.
  *)
end (* Ext *)

(** Generic representation of polymorphic variants. *)
module Poly : sig

  (** Polymorphic variants have a different memory representation
      than normal variants, as such they need a distinct
      generic description.

      Each constructor of a polymorphic variant is associated to a
      hash value, thus we provide operations to compute that hash
      value and operations to compute the constructor corresponding
      to a given hash.
  *)

  (** Abstract type representing the set of constructors in a
      polymorphic variant type *)
  type 'v t

  val poly_variant : 'v Con.t list -> 'v t
  (** @return the abstract description of the polymorphic variant given the
      list of its constructors.
  *)

  val find : 'v t -> int -> 'v Con.t
  (** @return the constructor corresponding to the hash value. *)

  val hash : 'v Con.t -> int
  (** @return the hash value of a polymorphic constructor. *)

  val conap : 'v t -> 'v -> 'v Con.conap
  (** Deconstructs a value of a polymorphic variant into the pair of its constructor and its arguments.
  *)

  (** Polymorphic variants are not named types but are given by providing all the constructors,
      therefore to witness a polymorphic variant type [v], we provide a value [v Desc.Poly.t].
  *)
  type _ ty += Poly_variant : 'v t -> 'v ty
end (* Poly *)


(** {2 Records}

    A record type is described as a set of fields, each field is described by its name, type
    and a procedure to update its value if it is mutable.

    To complete the description of a record type, it is
    associated to a product type and an isomorphism to
    convert between the record and the product.
*)

(** Generic representation of a record field. *)
module Field : sig
  (** Module suitable for open. *)
  module T : sig
    (** {b Field.} The type [('t,'r) field] describes a field of type ['t] for the record ['r].
        Each field has a [name], a type witness [ty], a procedure [set] to change
        the field if it is mutable.
    *)
    type ('a, 'r) field = {
      name : string; (** name of the field *)
      ty : 'a ty; (** type of the field *)
      set : ('r -> 'a -> unit) option; (** procedure for updating the field if it is mutable *)
    }
  end
  (** {b Field.} The type [('a,'r) t] describes a field of type ['a] for the record ['r].
      Each field has a [name], a type witness [ty], a procedure [set] to change
      the field if it is mutable.
  *)
  type ('a,'r) t = ('a,'r) T.field = {
    name : string; (** name of the field *)
    ty : 'a ty; (** type of the field *)
    set : ('r -> 'a -> unit) option; (** procedure for updating the field if it is mutable *)
  }

  val is_mutable : ('t, 'r) t -> bool
  (** @return true iff the given field mutable, i.e. [is_mutable f = f.set != None] *)
end

(** Generic representation of a record's collection of fields. *)
module Fields : sig
  module T : sig
    (** {b List of fields.}
        The type [('p, 'r) fields] is the list of fields for the
        record ['r], ['p] is a product type isomorphic to the record
        type ['r], it gathers all of the field types, in the same
        order are they are provided in the list.
    *)
    type ('p, 'r) fields =
      | Nil : (unit, 'r) fields
      | Cons : ('t, 'r) Field.t * ('ts, 'r) fields -> ('t * 'ts, 'r) fields
  end

  (** {b List of fields.}
        The type [('p, 'r) t] is the list of fields for the
        record ['r], ['p] is a product type isomorphic to the record
        type ['r], it gathers all of the field types, in the same
        order are they are provided in the list.
  *)
  type ('p, 'r) t =  ('p,'r) T.fields =
    | Nil : (unit, 'r) t
    | Cons : ('t, 'r) Field.t * ('ts, 'r) t -> ('t * 'ts, 'r) t

  val product : ('p, 'r) t -> 'p Product.t
  (** @return the list of types witnessing the product type ['p].
      Everything else is discarded ([name] of field, [set] procedure).
  *)

  val types_of_mutable_fields : ('p, 'r) t -> Ty.ty' list
  (** @return the list of the types of all mutable fields. *)
end

(** Generic representation of record datatypes. *)
module Record : sig
  module T : sig
    (** {b Record.}
        A record has a [name], a list of [fields] and an isomorphism [iso]
        between the product of the fields' types and the record type.
    *)
    type ('p, 'r) record =
      { name : string
      ; module_path : string list
      ; fields : ('p, 'r) Fields.t
      ; iso : ('p, 'r) Fun.iso
      }
  end

  (** {b Record.} The type [(p,r) t] represents the record
      type [r] isomorphic to the product type [p]. *)
  type ('p,'r) t = ('p,'r) T.record =
    { name : string
    ; module_path : string list
    ; fields : ('p, 'r) Fields.t
    ; iso : ('p, 'r) Fun.iso
    }

  val product : ('p, 'r) t -> 'p Product.t
  (** @return the list of field types witnessing the product type ['p] for the record type ['r].
  *)

  val types_of_mutable_fields : ('p, 'r) t -> Ty.ty' list
  (** @return the list of the types of all mutable fields. *)

  val tuple : ('p, 'r) t -> 'r -> 'p Product.tuple
  (** @return a tuple of the fields of a record value. *)
end (* Record *)

(** Generic representation of an object's or a classe's method. *)
module Method : sig
  type ('a, 'c) desc = {
    name : string;
    send : 'c -> 'a;
    bound : int;
    ty : 'a ty;
  }
  type 'c t = Method : ('a, 'c) desc -> 'c t
end

(** Generic representation of a collection of methods. *)
module Object : sig
  type 'c t = 'c Method.t list
  type _ ty += T : 'a t -> 'a ty
end

(** Generic representation of a class. *)
module Class : sig
  type 'c t = { name : string; methods : 'c Object.t; }
end (* Class *)

(** Generic representation of array-like datatypes. *)
module Array : sig

  (** The built-in [array], [string] and [bytes] types are all
      random access memory types, sharing a common interface. The
      module type {!intf} provides an interface that can be used
      generically without knowing whether the actual type is
      [array], [string] or [bytes].
  *)

  (** Common interface for array-like types ({i eg}: string and bytes).
  *)
  module type intf =
  sig
    type t  (** [t] is the array type *)
    type elt (** [elt] is the element type *)

    val length : t -> int
    (** @return the length (number of elements) of the given array.*)

    val get : t -> int -> elt
    (** [get a n] returns the element at index [n] of array [a].
        The first element has index [0].
        The last element has index [length a - 1].

        @raise Invalid_argument
        if [n] is outside the range [0] to [(length a - 1)]. *)

    val set : t -> int -> elt -> unit
    (** [set a n x] modifies array [a] in place, replacing
        element number [n] with [x].
        You can also write [a.(n) <- x] instead of [set a n x].

        @raise Invalid_argument
        if [n] is outside the range 0 to [length a - 1].
        @raise Failure if the array is not mutable.
    *)

    val init : int -> (int -> elt) -> t
    (** [init n f] returns a fresh array of length [n],
        with element number [i] initialized to the result of [f i].
        In other terms, [init n f] tabulates the results of [f]
        applied to the integers [0] to [n-1].

        @raise Invalid_argument if [n < 0] or [n > max_length].
    *)
    val max_length : int
    (** The maximum length that can be allocated for an array of type [t], using the function [init].
        - if [t = string] or [bytes], [max_length = Sys.max_string_length];
        - if [t = float array], [max_length = Sys.max_array_length / 2];
        - if [t = elt array] and [elt] isn't [float], [max_length = Sys.max_array_length].
    *)

  end
end (* Array *)

(** Generic representation of custom datatypes. *)
module Custom : sig

  (** Custom types are opaque to OCaml, they generally come with a foreign interface in C.
      Therefore there is not much information that can be used for generic programming.
  *)

  module T : sig
    type 'a custom = {
      name : string;               (** name of the OCaml type *)
      module_path : string list;
      identifier : string; (** the identifier correspond to the homonymous field of the C-struct [custom_operations] is defined in [<caml/custom.h>] *)
    }
  end

  type 'a t = 'a T.custom
end (* Custom *)

(** {2 Type description}

    A low level generic view of ocaml types. Types fall in nine
    categories each with its specific description.
*)

(** One may open [T] to bring constructor names in scope, typically for pattern-matching. *)
module T : sig
  type 'a desc =
    | Array      : 'e ty * (module Array.intf with type t = 'a and type elt = 'e) -> 'a desc
    | Product    : 'ts Product.t * ('ts , 'p) Fun.iso -> 'p desc
    | Record     : ('p,'r) Record.t -> 'r desc
    | Variant    : 'v Variant.t -> 'v desc
    | Extensible : 'a Ext.t -> 'a desc
    | Custom     : 'a Custom.t -> 'a desc
    | Class      : 'c Class.t  -> 'c desc
    | Synonym    : 'b ty * ('a,'b) Equal.t -> 'a desc
    | Abstract   : 'a desc
    | NoDesc     : 'a desc
end

type 'a desc = 'a T.desc =
    | Array      : 'e ty * (module Array.intf with type t = 'a and type elt = 'e) -> 'a desc
    | Product    : 'ts Product.t * ('ts , 'p) Fun.iso -> 'p desc
    | Record     : ('p,'r) Record.t -> 'r desc
    | Variant    : 'v Variant.t -> 'v desc
    | Extensible : 'a Ext.t -> 'a desc
    | Custom     : 'a Custom.t -> 'a desc
    | Class      : 'c Class.t  -> 'c desc
    | Synonym    : 'b ty * ('a,'b) Equal.t -> 'a desc
    | Abstract   : 'a desc
    | NoDesc     : 'a desc

type 'a t = 'a T.desc
