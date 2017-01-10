(** Spine view

    The spine view shows the applicative structure of a value
    as a constructor applied to its arguments.  The spine
    view is a generic view on a typed value. This is in
    contrast with most other views which show the structure
    of types. Consequently, this view can only be used to
    write generic consumer, not generators.

    The spine view underlies the Haskell SYB generic library
    (Scrap Your Boilerplate).
*)

open Generic_core
open Generic_util

open Ty.T
open Desc.T
let desc = Desc_fun.view

(**
We call left spine the path from the root of a tree to its
leftmost leaf.  The spine generic view represent the possibly
partial spine of a tree whose node are applications, and
whose root is a data constructor.

The type is a GADT. The constructor [App] uses an
existential quantification for the type of the argument.  The
representation of the argument type is useful when defining
generic computations over a spine.
*)

module T = struct
  type 'a spine =
    | Con : 'a -> 'a spine
    | App : ('a -> 'b) spine * 'a ty * 'a -> 'b spine
end

include T

type 'a t = 'a spine

(**
A spine view converts a constructor value [c(v0,...vn)]
to the form [App (....(App (c', t0, v0)..., tn, vn)]
where [tk] is the representation of [vk]'s type.
and [c' = Con (c\_desc, fun x0 ... xn -> c(x0,...,xn)].

In essence, the spine view of a value gives a data
constructor and a list of all its arguments together with
their types.

<<spine view constructors for int lists>>=
let nil = Con (nild, [])
and cons ty hd tl =
  App (App (Con (cons_desc, fun x y -> x :: y), ty, hd)
      , List ty, tl)
@
*)


(**
We derive the spine view from the generic type description.
The existential type annotation is necessary because of
matching the gadt ['a ty].

We first deal with the base cases: the built-in types whose
values cannot be subdivided. For all the other types, we
compute the spine generically, using their type description.

The case for type synonyms requires a recursive call.

For structured values, much of the work is is delegated to
[spine_of_product].

Values of type ['a tuple] are pairs of a list of
types and a list of the values of the corresponding types,
for each index in the list. Values of type ['a spine] are
lists of pairs of a type and a value of that type. Thus the conversion
[spine_of_product] performs the equivalent of [List.combine].

*)

let rec spine_of_product : type a b . (a -> b) -> a Product.tuple -> b spine
  = fun f -> function
  | Product.T.Nil , () -> Con (f ())
  | Product.T.Cons (t,ts), (x,xs) ->
     let spine_rest = spine_of_product (fun xs x -> f (x,xs)) (ts,xs) in
     App (spine_rest, t, x)
;;
let spine_of_variant (Desc.Con.Conap (desc,args)) =
  spine_of_product desc.embed (desc.args, args)   (* we should rename [desc.args] to [desc.args_ty] *)
;;

let rec spine : type a . a ty -> a -> a spine
  = fun t x -> match t with
  | Char   -> Con x
  | Int    -> Con x
  | Float  -> Con x
  | Lazy t -> invalid_arg (__MODULE__ ^ ".spine: lazy")
  | _ -> match desc t with
         | Product (p, {fwd;bck}) -> spine_of_product fwd (p, bck x)
         | Record r -> spine_of_product r.iso.fwd (Desc.Record.tuple r x)
         | Variant {name;cons} -> spine_of_variant (Desc.Variant.conap cons x)
         | Array (t, (module A)) -> Con x
         | Custom d -> Con x
         | Class c -> Con x
         | Abstract -> raise Exn.Not_yet_implemented (* TODO compute the spine using the concrete representation *)
         | Extensible e -> raise Exn.Not_yet_implemented (* TODO spine of extensible *)
         | NoDesc -> invalid_arg (__MODULE__ ^ ".spine")
    | Synonym (t', Equal.Refl) -> spine t' x
;;

let view = spine

(** Builds back a value from it's spine view. *)
let rec rebuild : type a . a spine -> a
  = function
  | Con c -> c
  | App (s,t,x) -> rebuild (s) x
