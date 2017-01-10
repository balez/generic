(** Extensible type-indexed consumers.

    Consumers are a special case of type-indexed functions which
    consume a value of the type index and produce something
    independent of that type. They have a signature of the form:
    [forall 'a . 'a ty -> 'a -> b] for some type [b] independent of ['a].

    Specialising {!Generic_core_extensible}, allows us to get
    rid of the {!Generic_util_app.t} datatype.

    See {!Generic_core_extensible} for more details.
*)

open Generic_core

(** Type-indexed consumer function *)
type 'b ty_fun = { f : 'a. 'a Ty.ty -> 'a -> 'b; }

(** A closure allows us to call a type-indexed consumer and
    to extend it to new type cases.
*)
type 'b closure = {
  f : 'a. 'a Ty.ty -> 'a -> 'b;
    (** [f]: applies the extensible function.
       @raise Generic_core_extensible.Type_pattern_match_failure when the type index doesn't
       match any of the patterns in the collection.  *)

  ext : 'a. 'a Ty.ty -> 'b ty_fun -> unit;
    (** [ext]: extends the function with a new case. We must
        provide a type pattern, (example: [List Any]). The
        [ty_fun] provided is only expected to handle types
        matching with the pattern. {b Effectful}.
        @raise Generic_core_extensible.Pattern_overwrite when called with a type pattern that
        was already registered.
    *)
}

(** [create name], creates a new closure initially empty:
    calling [f] will raise
    [Type_pattern_match_failure]. The name is used in the exception messages. {b Effectful}. *)
val create : string -> 'a closure
