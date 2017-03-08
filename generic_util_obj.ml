open Generic_util

module O = Obj

type tag =
  | Constructor of int
  | Lazy | Closure | Object | Infix | Forward
  | Abstract | String | Double | Double_array | Custom
  | Unaligned | Out_of_heap | Int

type obj =
  | Int of int
  | Block of tag * O.t array

let tag_view = function
  | t when t == O.lazy_tag         -> Lazy
  | t when t == O.closure_tag      -> Closure
  | t when t == O.object_tag       -> Object
  | t when t == O.infix_tag        -> Infix
  | t when t == O.forward_tag      -> Forward
  | t when t == O.abstract_tag     -> Abstract
  | t when t == O.string_tag       -> String
  | t when t == O.double_tag       -> Double
  | t when t == O.double_array_tag -> Double_array
  | t when t == O.custom_tag       -> Custom
  | t when t == O.unaligned_tag    -> Unaligned
  | t when t == O.out_of_heap_tag  -> Out_of_heap
  | t when t >= O.first_non_constant_constructor_tag
        && t <= O.last_non_constant_constructor_tag -> Constructor t
  | t when t == O.int_tag -> Int
  | t -> raise (Invalid_argument (__MODULE__ ^ ".tag_view: " ^ string_of_int t))

let fields v =
  Array.init (O.size v) (O.field v)

let view v =
  if O.is_int v then Int (O.obj v)
  else Block (tag_view (O.tag v), fields v)


(** [con_id]: return type of function [con_id]
*)
type con_id = bool * int
(** [con_id]: This function discriminates each constructor of
    a variant datatype by returning a distinct pair of
    (bool,int) for each of them.  The boolean is true iff the
    constructor is constant.  The function doesn't
    discriminate the constructors of extensible variants.
    Also, by its very nature, two constructors of different
    types might have the same con id.
*)
let con_id t =
  (* copy the memory block O.repr(t), with components set to 0 *)
  let ot = O.repr t in
  let b = O.is_int ot in
  (b, (if b then O.obj else O.tag) ot)

let non_constant_constructor_tag t =
    t >= O.first_non_constant_constructor_tag
  && t <= O.last_non_constant_constructor_tag

let is_con v =
  O.is_int v
  || non_constant_constructor_tag (O.tag v)

let is_tuple v =
  O.is_block v
  || non_constant_constructor_tag (O.tag v)

(** The size of a block or [0] for an immediate value.
*)
let gsize = function
  | v when O.is_int v -> 0
  | v -> O.size v

(** [x] and [y] must both be block of the same length,
and the binary predicate must hold for all their fields:

{v
forall i . p (x.i, y.i)
v}

*)

let fields_all2 p x y =
  is_tuple x && is_tuple y
  && O.size x == O.size y
  && Iter.for_all_in 0 (O.size x - 1)
                     (fun i -> p (O.field x i) (O.field y i))

(** Equality on objects. (Same as Pervasive.=)
   {e todo}: use memoisation to deal with cyclic datastructures
   unsafe on custom types
let rec obj_eq x y =
  con_id x = con_id y
  && (O.is_int x
      || fields_all2 obj_eq x y)
*)

(* Given a obj of type (x0,...xn)
  Computes a nested product (x0, (x1, (..., xn) ...))
 *)
let listify x =
  let rec go tail = function
    | i when i < 0 -> tail
    | i -> let c = O.new_block 0 2 in
           begin
             O.set_field c 0 (O.field x i);
             O.set_field c 1 tail;
             go c (i-1);
           end
  in if is_tuple x
  then go (O.repr ()) (O.size x - 1)
  else raise (Invalid_argument (__MODULE__ ^ ".listify: not a tuple"))

(* PARTIAL. polymorpic variants *)
let poly_hash x =
  let v = O.repr x in
  if O.is_int v then O.obj v
  else if O.size v > 0 then O.obj (O.field v 0)
  else raise (Invalid_argument (__MODULE__ ^ ".poly_hash: not a polymorphic variant"))

(* TOTAL. A constructor of an extensible variant type is
  an object with two fields: a string (the name) and an int *)
let is_ext_con x =
  O.is_block x
  && O.tag x == O.object_tag
  && O.size x == 2
  && O.tag (O.field x 0) == O.string_tag
  && O.is_int (O.field x 1)

(* PARTIAL. raise Invalid_argument
   Extract the constructor of an extensible variant,
   from a value which may be a constructor application
   ENSURES is_ext_con x *)
let ext_con x =
  let c = if O.is_block x && O.tag x == 0 && O.size x > 1
          then O.field x 0 else x
  in if is_ext_con c then c
     else raise (Invalid_argument (__MODULE__ ^ ".ext_con"))

(* PARTIAL *)
let ext_con_name x = (O.obj (O.field (ext_con x) 0) : string)

(* PARTIAL *)
let ext_con_id x = (O.obj (O.field (ext_con x) 1) : int)
let ext_con_set_id x id = O.set_field (ext_con x) 1 (O.repr id)

(* TOTAL. Returns the identifier of a custom block,
    or the empty string if the block isn't a custom one. *)
external custom_identifier : 'a -> string = "caml_custom_identifier"

(* TOTAL. duplicate blocks and returns identity on integers *)
let dup_if_block v = if O.is_int v then v else O.dup v
