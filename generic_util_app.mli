(** Type application and defunctionalisation.

    In OCaml parametric types cannot be partially applied:
    they must be saturated, all parameters must be given for
    a type expression to be valid. Therefore we cannot
    easily abstract over polymorphic type constructors and
    generalise the type of the List.map function:

    {[val map : ('a -> 'b) -> 'a list -> 'b list]}

    Since the syntax doesn't allow to replace [list] with a type variable:

    {[val invalidmap : ('a -> 'b) -> 'a 'f -> 'b 'f]}

    The purpose of this module is to be able to write such
    generalisation using defunctionalisation as described in
    the article "Lightweight Higher-Kinded Polymorphism" by
    Jeremy Yallop and Leo White.

    The type ['a 'f] is not valid, but we may write [('a,'f) app],
    where the type [app] is an extensible gadt.
    To each polymorphic type ['a p], we will associate an abstract type
    [pcode] which is interpreted by the gadt [app] such that:
    [('a,p_code) app] is isomorphic to ['a p].

    For instance, to represent the [list] type constructor,
    we define the abstract type [list_code]:

    {[type list_code]}

    and we add a [List] constructor to [app]:

    {[type (_,_) app += List : 'a list -> ('a, list_code) app]}

    The generalised [map] may then be typed:

    {[val map : ('a -> 'b) -> ('a,'f) app -> ('b,'f) app]}


    {b Uses of [app] in the generic library}.

    [app] is used to define extensible type indexed functions, see
    {!module:Generic_core_extensible}.
*)

(** {2 Type Application}
 *)

(** Module defining {!app}, suitable to be opened. *)
module T : sig
  (** Extensible GADT interpreting type application.  *)
  type (_,_) app = ..
end

(** Synonym for convenience, when namespace [Generic_util] is
    opened, and one doesn't want to open [App.T], one can
    refer to [App.T.app] as [App.t].
*)
type ('a,'b) t = ('a,'b) T.app

(** {2 Constant Functor} *)

(** The type ['t const] doesn't build useful values, we use
    it as a "code" to be interpreted by [app] so that [('a, 't
    const) app] is isomorphic to ['t].

    It would have been better to make ['t const] an abstract
    type, but it then makes typechecking troublesome for the
    constructor [Const : 't -> ('a, 't const) app]
*)
type 't const = CONST

(** [('a, 'b const) app] is isomorphic to ['t] *)
type (_, _) t += Const : 'b -> ('a, 'b const) t

(** Get the argument of the [Const] constructor *)
val get_const : ('a, 'b const) t -> 'b

(** {2 Exponential Functor}
*)

(** The type ['b exponential] doesn't build useful values,
    consider it abstract. We use it as a "code" to be
    interpreted by [app] so that [('a, 'b exponential) app] is
    isomorphic to ['a -> 'b].
*)
type 'b exponential = EXPONENTIAL

(** [('a, 'b exponential) app] is isomorphic to ['a -> 'b]. *)
type (_, _) t += Exponential : ('a -> 'b) -> ('a, 'b exponential) t

(** Get the argument of the [Exponential] constructor *)
val get_exponential : ('a, 'b exponential) t -> 'a -> 'b
