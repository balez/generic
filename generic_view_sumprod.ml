(** Sum of Product view
    replicating the Haskell LIGD library

    Cheney, James, & Hinze, Ralf. (2002). A lightweight
    implementation of generics and dynamics. Pages 90--104 of
    Haskell Workshop02.

    TODO: tests
*)

open Generic_core
open Generic_util
open Sum
open Desc
open Desc.T
open Ty.T

let ( -< ) = Fun.(-<)

let desc = Desc_fun.view

type 'a sp =
  | Base  : 'a ty -> 'a sp (* base cases (char, int, float, ..) *)
  | Empty : empty sp
  | Sum   : 'a sp * 'b sp -> ('a, 'b) sum sp
  | Unit  : unit sp
  | Prod  : 'a sp * 'b sp -> ('a * 'b) sp
  | Con   : string * 'a sp -> 'a sp
  | Field : string * 'a sp -> 'a sp
  | Iso   : ('a, 'b) Fun.iso * 'a sp -> 'b sp

type 'b varsp =
  | Varsp : ('a -> 'b) * ('b -> 'a option) * 'a sp -> 'b varsp

let empty_varsp : 'a varsp
  = Varsp (empty_elim, (fun x -> None), Empty)

let iso_eq : type a b . (a, b) Equal.t -> (a, b) Fun.iso
  = function
    | Equal.Refl -> { fwd = Fun.id; bck = Fun.id }

(* forward conversion may throw Exn.Failed *)
let iso_repr r = let open Repr in
  { Fun.fwd = Option.get_some -< r.from_repr
  ; Fun.bck = r.to_repr
  }

(* backward conversion may throw Exn.Failed  *)
let iso_partial f b =
  { Fun.fwd = f
  ; Fun.bck = Option.get_some -< b
  }

let sp_of_varsp (Varsp (f, b, x))
  = Iso (iso_partial f b, x)

let rec view : type a . a ty -> a sp
  = fun t ->
    match Desc_fun.view t with
    | Product (p, iso) -> Iso (iso, prod p)
    | Record r -> Iso (r.iso, fields r.fields)
    | Variant v -> sp_of_varsp (cons (Variant.list_of_cons v.cons))
    | Extensible e -> sp_of_varsp (ext e)
    | Synonym (t', eq) -> Iso (iso_eq eq, view t')
    | Abstract ->
      (match Repr.repr t with
       | Repr.Repr r -> Iso (iso_repr r, view r.repr_ty))
    | _ -> Base t

and prod : type a . a Product.t -> a sp
  = let open Product.T in
  function
  | Nil -> Unit
  | Cons (h, t) -> Prod (view h, prod t)

and fields : type a r . (a, r) Fields.t -> a sp
  = let open Fields.T in
  function
  | Nil -> Unit
  | Cons (h, t) -> Prod (field h, fields t)

and field : type a r . (a, r) Field.t -> a sp
  = fun f ->
    Field (f.name, view f.ty)

(* The tricky part is to build the isomorphism. *)
and cons : type v . v Con.t list -> v varsp
  = fun cs -> List.fold_left var (Varsp (empty_elim, Fun.const None, Empty)) cs

and var : type v . v varsp -> v Con.t -> v varsp
  = fun (Varsp (f, b, s)) (Con.Con h) ->
        let f' = either h.embed f
        and b' x = (match h.proj x with
            | Some y -> Some (Left y)
            | None -> Option.option right (b x))
        and s' = Con (h.name, Sum (prod h.args, s))
        in Varsp (f', b', s')

and ext : type a . a Ext.t -> a varsp
  = fun e -> Ext.fold e (Fun.flip var) empty_varsp
