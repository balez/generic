[@@@reify_all]
open Generic
open Ty.T

[%%import Option (some_if)]

let p t x = print_endline (Gfun.show t x)

(* Naturals *)
type nat = int [@@abstract]
let () =
  Repr.ext Nat
    { f = fun (type a) (ty : a ty) -> match ty with
          | Nat -> (Repr.Repr { repr_ty = Int
                              ; to_repr = (fun x -> x)
                              ; from_repr = some_if (fun x -> x >= 0)
                              ; default = 0
                              ; update = (fun _ _ -> ())}
                    : a Repr.t)
          | _ -> assert false
    }

type my_variant =
  | A of { field : string }

let () =
  p (List Int) [3;4;5];
  p Unit ();
  p (Pair (Char, Float)) ('a', 0.5);
  p (Triple (Int32, Int64, Nativeint)) (Int32.max_int, Int64.max_int, Nativeint.max_int);
  p (Array Bool) [| false ; true |];
  p (Ty Any) Any;
  p (Ty (Pair (Int, Option Float))) (Pair (Int, Option Float));
  p (Ref String) ({contents = "hello"});
  p Bytes "test";
  p (Array Float) [| 0.0 |];
  p (List Any) [];
  p (Array Any) [||];
  p (My_variant) (A {field = "now"});
