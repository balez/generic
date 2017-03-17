(* Generic string conversion.

   We make the function extensible so that one may override the generic behaviour for specific types.
   Note: lazy values are forced.
*)

open Generic_core
open Generic_util
open Ty.T

(* Extensible function whose default case is given by the generic function. *)
let show_closure = Consumer.create "Generic_fun_show.show"
let show = show_closure.f
let show_ext = show_closure.ext

(* shows each component of the product separated by commas. *)
let rec show_product : type p . p Product.t -> p -> string
  = fun ts xs ->
  let open Product.T in
  match ts , xs with
  | Nil , () -> ""
  | Cons (t, Nil) , (x, ()) ->
    show t x
  | Cons (t, ts) , (x, xs) ->
    show t x ^ ", " ^ show_product ts xs

let show_field f x =
  f.Desc.Field.name ^ " = " ^ show f.ty x

let rec show_fields : type p v . (p, v) Desc.Fields.t -> p -> string
  = fun fs xs ->
    let open Desc.Fields.T in
    match fs , xs with
    | Nil , () -> ""
    | Cons (f, Nil), (x, ()) ->
      show_field f x
    | Cons (f, fs), (x, xs) ->
      show_field f x ^ ";" ^ show_fields fs xs

(* Constructor arguments. *)
let show_args : type p v . (p, v) Desc.Con.arguments -> p -> string
  = fun args x ->
    let open Desc.Con in
    let open Product in
    match args with
    | Product Nil -> ""
    (* depend on context *)
    (*    | Product (Cons (t, Nil)) -> " " ^ show t (fst x) *)
    | Product p -> " (" ^ show_product p x ^ ")"
    | Record r -> " {" ^ show_fields r x ^ "}"

(* Shows a constructor application. *)
let show_conap = function
  | Desc.Con.Conap (c, x) -> c.name ^ show_args c.args x


(* Show a public representation. *)
let show_repr = function
  | Repr.Repr r -> fun x -> show r.repr_ty (r.to_repr x)

let try_repr default t x =
  try show_repr (Repr.view t) x
  with Extensible.Type_pattern_match_failure s when s = Repr.repr_name
    -> default

let show_dyn = function
  | Ty.Dyn (t, x) -> show t x

(* Generic case, calls [show] recursively. *)
let show_default : type a . a ty -> a -> string
  = fun t x ->
    let open Desc.T in
    match t with
    | Any -> "<poly>"
    | Int -> string_of_int x
    | Int32 -> Int32.to_string x
    | Int64 -> Int64.to_string x
    | Nativeint -> Nativeint.to_string x
    | Float -> string_of_float x
    | Char -> "'" ^ Char.escaped x ^ "'"
    | Bytes -> "\"" ^ Bytes.to_string x ^ "\""
    | String -> "\"" ^ x ^ "\""
    | Lazy a -> show a (Lazy.force x)
    | Array a -> "[|" ^ String.concat "; " (List.map (show a) (Array.to_list x)) ^ "|]"
    | List a -> "[" ^ String.concat "; " (List.map (show a) x) ^ "]"
    | Fun (a,b) -> "<fun>"
    | Ty.Dynamic -> show_dyn x
    | t -> (match Desc_fun.view t with
            | Product (p, {fwd;bck}) ->
              "(" ^ show_product p (bck x) ^ ")"
            | Variant {cons; _} -> show_conap (Desc.Variant.conap cons x)
            | Extensible e -> show_conap (Desc.Ext.conap e x)
            | Record r -> "{" ^ show_fields r.fields (r.iso.bck x) ^ "}"
            | Custom _ -> try_repr "<custom>" t x
            | Class _ -> try_repr "<object>" t x
            | Synonym (t, eq) -> (match eq with
                | Equal.Refl -> show t x)
            | Abstract -> try_repr "<abstract>" t x
            | _ -> try_repr "<value>" t x)

let () = show_ext Any { f = show_default }
