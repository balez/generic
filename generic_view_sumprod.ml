(** Sum of Product view
    inspired by Haskell LIGD library.

    Cheney, James, & Hinze, Ralf. (2002). A lightweight
    implementation of generics and dynamics. Pages 90--104 of
    Haskell Workshop02.

    The Haskell implementation used infinite representations
    to represent recursive datatypes.

> type 'a sp =
>   | Base  : 'a ty -> 'a sp (* base cases (char, int, float, ..) *)
>   | Empty : empty sp
>   | Sum   : 'a sp * 'b sp -> ('a, 'b) sum sp
>   | Unit  : unit sp
>   | Prod  : 'a sp * 'b sp -> ('a * 'b) sp
>   | Con   : string * 'a sp -> 'a sp
>   | Field : string * 'a sp -> 'a sp
>   | Iso   : 'a sp * ('a, 'b) Fun.iso -> 'b sp

    In OCaml, one solution is to introduce a constructor that delay computing the view:

>   | Delay : 'a ty -> 'a sp

    And the user must explicitely call the view on the
    recursive arguments to obtain the representation.

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
  | Delay   : 'a ty -> 'a sp (* to break the recursion *)
  | Con   : string * 'a sp -> 'a sp
  | Field : string * 'a sp -> 'a sp
  | Iso   : 'a sp * ('a, 'b) Fun.iso -> 'b sp

type 'b varsp =
  | Varsp : 'a sp * ('a -> 'b) * ('b -> 'a option) -> 'b varsp

let empty_varsp : 'a varsp
  = Varsp (Empty, empty_elim, (fun x -> None))

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

let sp_of_varsp (Varsp (x, f, b))
  = Iso (x, iso_partial f b)

let rec view : type a . a ty -> a sp
  = fun t ->
    match Desc_fun.view t with
    | Product (p, iso) -> Iso (prod p, iso)
    | Record r -> Iso (fields r.fields, r.iso)
    | Variant v -> sp_of_varsp (cons (Variant.con_list v.cons))
    | Extensible e -> sp_of_varsp (ext e)
    | Synonym (t', eq) -> (match eq with
          Equal.Refl -> view t')
    | Abstract ->
      (match Repr.repr t with
       | Repr.Repr r -> Iso (view r.repr_ty, iso_repr r))
    | _ -> Base t

and prod : type a . a Product.t -> a sp
  = let open Product.T in
  function
  | Nil -> Unit
  | Cons (h, t) -> Prod (Delay h, prod t)

and fields : type a r . (a, r) Fields.t -> a sp
  = let open Fields.T in
  function
  | Nil -> Unit
  | Cons (h, t) -> Prod (field h, fields t)

and field : type a r . (a, r) Field.t -> a sp
  = fun f ->
    Field (f.name, Delay f.ty)

(* The tricky part is to build the isomorphism. *)
and cons : type v . v Con.t list -> v varsp
  = fun cs -> List.fold_left var empty_varsp cs

and var : type v . v varsp -> v Con.t -> v varsp
  = fun (Varsp (s, f, b)) (Con.Con h) ->
        let s' = Con (h.name, Sum (fields h.args, s))
        and f' = either h.embed f
        and b' x = (match h.proj x with
            | Some y -> Some (Left y)
            | None -> Option.option right (b x))
        in Varsp (s', f', b')

and ext : type a . a Ext.t -> a varsp
  = fun e -> Ext.fold e (Fun.flip var) empty_varsp

let sumprod = view
