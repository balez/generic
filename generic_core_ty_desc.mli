(** Generic view for the type {!Generic_core_ty.ty}.

    The extensible type {!Generic_core_ty.ty}, reflects types terms as values.
    We may write generic programs over [ty] by using the generic view computed by {!ext}:
    it is a value of type {!Generic_core_desc.Ext.t} which describes the constructors of [ty].
*)

open Generic_core
open Ty.T

(** Generic view for {!Generic_core_ty.ty}: low level description of the type [ty] itself. *)
val ext : 'a ty -> 'a ty Desc.Ext.t

(** Constructor application: the pair of a constructor and its list of arguments.
    @raise Not_found if the constructor hasn't been added previously.
*)
val conap : 'a ty -> 'a ty Desc.Con.conap

(** [con x] is the constructor of [x].
    @raise Not_found if the constructor hasn't been added previously.
*)
val con : 'a ty -> 'a ty Desc.Con.t

(** Arguments of the constructor.
    @raise Not_found if the constructor hasn't been added previously.
*)
val subterms : 'a ty -> Ty.dyn list
