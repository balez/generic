(* This file shows how gadts can be represented by considering each possible index and
returning a different variant representation each time.
The PPX doesn't support GADTs yet, so this has to be done manually. The type should be annotated
with [@@no_desc] to prevent the PPX from generating an invalid type description.
*)

[@@@reify_all]
open Generic_core
open Generic_util

open Ty.T

type zero = Z
type 'n suc = S

(* indexed type *)

type ('a, 'n) vec =
  | Nil : ('a, zero) vec
  | Cons : 'a * ('a, 'n) vec -> ('a, 'n suc) vec
[@@no_desc]

let () =
  Generic_core.Desc_fun.ext (Vec (Generic_core.Ty.Any, Generic_core.Ty.Any))
    {
      Generic_core.Desc_fun.f = fun (type a) ->
        fun (ty : a Generic_core.Ty.ty)  ->
          (match ty with
           | Vec (a, Zero) ->
               Generic_core.Desc.Variant
                 {
                   Generic_core.Desc.Variant.name = "vec";
                   Generic_core.Desc.Variant.module_path =
                     ["Generic_test_gadt"];
                   Generic_core.Desc.Variant.cons =
                     (Generic_core.Desc.Variant.cons
                        [Generic_core.Desc.Con.make "Nil"
                           Generic_core.Product.T.Nil (fun ()  -> Nil)
                           (function | Nil  -> Some () | _ -> .)])
                 }

           | Vec (a, Suc b) ->
             Generic_core.Desc.Variant
               {
                 Generic_core.Desc.Variant.name = "vec";
                 Generic_core.Desc.Variant.module_path =
                   ["Generic_test_gadt"];
                 Generic_core.Desc.Variant.cons =
                   (Generic_core.Desc.Variant.cons
                      [Generic_core.Desc.Con.make "Cons"
                         (Generic_core.Product.T.Cons
                            (a,
                             (Generic_core.Product.T.Cons
                                ((Vec (a, b)),
                                 Generic_core.Product.T.Nil))))
                         (fun (x,(xs,()))  -> Cons (x, xs))
                         (function
                           | Cons (x, xs) -> Some (x, (xs, ()))
                           | _ -> .)])
               }
           | _ -> assert false : a Generic_core.Desc.t)
    }

(* data depending on index *)

type 'a term =
  | Const : 'a -> 'a term
  | Pair : 'a term * 'b term -> ('a * 'b) term
  | If : bool term * 'a term * 'a term -> 'a term
  | Add : int term * int term -> int term
[@@no_desc]

let () =
  Generic_core.Desc_fun.ext (Term Generic_core.Ty.Any)
    {
      Generic_core.Desc_fun.f = fun (type a) ->
        fun (ty : a Generic_core.Ty.ty)  ->
          (match ty with
           | Term (Pair (a,b) as x1) ->
               Generic_core.Desc.Variant
                 {
                   Generic_core.Desc.Variant.name = "term";
                   Generic_core.Desc.Variant.module_path =
                     ["Generic_test_gadt"];
                   Generic_core.Desc.Variant.cons =
                     (Generic_core.Desc.Variant.cons
                        [Generic_core.Desc.Con.make "Const"
                           (Generic_core.Product.T.Cons
                              (x1, Generic_core.Product.T.Nil))
                           (fun (x1,())  -> Const x1)
                           (function | Const x1 -> Some (x1, ()) | _ -> None);
                        Generic_core.Desc.Con.make "Pair"
                          (Generic_core.Product.T.Cons
                             ((Term a),
                               (Generic_core.Product.T.Cons
                                  ((Term b),
                                    Generic_core.Product.T.Nil))))
                          (fun (x1,(x2,()))  -> Pair (x1, x2))
                          (function
                           | Pair (x1,x2) -> Some (x1, (x2, ()))
                           | _ -> None);
                        Generic_core.Desc.Con.make "If"
                          (Generic_core.Product.T.Cons
                             ((Term Bool),
                               (Generic_core.Product.T.Cons
                                  ((Term x1),
                                    (Generic_core.Product.T.Cons
                                       ((Term x1),
                                         Generic_core.Product.T.Nil))))))
                          (fun (x1,(x2,(x3,())))  -> If (x1, x2, x3))
                          (function
                           | If (x1,x2,x3) -> Some (x1, (x2, (x3, ())))
                           | _ -> None);])
                 }
           | Term Int ->
               Generic_core.Desc.Variant
                 {
                   Generic_core.Desc.Variant.name = "term";
                   Generic_core.Desc.Variant.module_path =
                     ["Generic_test_gadt"];
                   Generic_core.Desc.Variant.cons =
                     (Generic_core.Desc.Variant.cons
                        [Generic_core.Desc.Con.make "Const"
                           (Generic_core.Product.T.Cons
                              (Int, Generic_core.Product.T.Nil))
                           (fun (x1,())  -> Const x1)
                           (function | Const x1 -> Some (x1, ()) | _ -> None);
                        Generic_core.Desc.Con.make "If"
                          (Generic_core.Product.T.Cons
                             ((Term Bool),
                               (Generic_core.Product.T.Cons
                                  ((Term Int),
                                    (Generic_core.Product.T.Cons
                                       ((Term Int),
                                         Generic_core.Product.T.Nil))))))
                          (fun (x1,(x2,(x3,())))  -> If (x1, x2, x3))
                          (function
                           | If (x1,x2,x3) -> Some (x1, (x2, (x3, ())))
                           | _ -> None);
                        Generic_core.Desc.Con.make "Add"
                          (Generic_core.Product.T.Cons
                             ((Term Int),
                               (Generic_core.Product.T.Cons
                                  ((Term Int), Generic_core.Product.T.Nil))))
                          (fun (x1,(x2,()))  -> Add (x1, x2))
                          (function
                           | Add (x1,x2) -> Some (x1, (x2, ()))
                           | _ -> None)])
                 }
           | Term x1 ->
               Generic_core.Desc.Variant
                 {
                   Generic_core.Desc.Variant.name = "term";
                   Generic_core.Desc.Variant.module_path =
                     ["Generic_test_gadt"];
                   Generic_core.Desc.Variant.cons =
                     (Generic_core.Desc.Variant.cons
                        [Generic_core.Desc.Con.make "Const"
                           (Generic_core.Product.T.Cons
                              (x1, Generic_core.Product.T.Nil))
                           (fun (x1,())  -> Const x1)
                           (function | Const x1 -> Some (x1, ()) | _ -> None);
                        Generic_core.Desc.Con.make "If"
                          (Generic_core.Product.T.Cons
                             ((Term Bool),
                               (Generic_core.Product.T.Cons
                                  ((Term x1),
                                    (Generic_core.Product.T.Cons
                                       ((Term x1),
                                         Generic_core.Product.T.Nil))))))
                          (fun (x1,(x2,(x3,())))  -> If (x1, x2, x3))
                          (function
                           | If (x1,x2,x3) -> Some (x1, (x2, (x3, ())))
                           | _ -> None);])
                 }
           | _ -> assert false : a Generic_core.Desc.t)
    }
