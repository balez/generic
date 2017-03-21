(* TODO:

   - Optimize the hashtables by hashing the addresses of the
     blocks. This require to recomputing the tables after
     each pass of the garbage collection.

   - Make a topological sort before traversing so that the
     anti-unifier of the roots of the strongly connected
     components may be computed before traversing them.

*)
open Generic_core
open Generic_util

open App.T
open Ty.T
type ty' = Ty.ty'

(******************* IMPORTS ************************
 * Making explicit what external functions are used.
 *)

type obj = Obj.t

[%%import Obj
    ( to_obj <- repr
    ; from_obj <- obj
    ; is_int; is_block; field; set_field; size; dup; new_block
    ; tag; custom_tag; double_tag; string_tag; object_tag; double_array_tag)
    [@@type obj := t]]

[%%import Fun ((>>.); (-<))]

[%%import Exn (guard; one_of)]

[%%import Desc_fun (desc <- view)]

[%%import Obj_inspect (print_obj)]

(**************************************************)
let debug = true
exception Serialize_exception of string

let serialize_exn s = raise (Serialize_exception s)

(* [exn s f] forces the execution of [f], and re-raise all Serialize exceptions,
any other exception is caught and a new Serialize_exception is thrown with message [s]
*)
let exn s f = try Lazy.force f
  with Serialize_exception _ as e -> raise e
     | _ -> raise (Serialize_exception s)

let unboxed_exn = "Invalid unboxed type representation.";

(* direction of the conversion, TO bytes or FROM bytes *)
type direction = To | From
let direction = ref To (* global ref *)
let is_from = function
  | From -> true
  | _ -> false

(* hashtable with physical equality on untyped objects *)
module HashedObj =
  struct
    type t = Obj.t
    let equal = (==)
    let hash = Hashtbl.hash
  end

module H = Hashtbl.Make (HashedObj)

(* visited blocks global table *)
let visit_ty_table = (H.create 10 : ty' H.t)
let visit_val_table = (H.create 10 : obj H.t)
let visit_ty = H.find visit_ty_table
let visit_val v = if is_int v then v else H.find visit_val_table v
let visit_ty_set v t = H.replace visit_ty_table v (E t)
let visit_val_set = H.replace visit_val_table
let visit_val_mem = H.mem visit_val_table

(* backlinks is a hashtable indexed by the raw umarshalled
   values of abstract types. Each of them is associated to
   the list of objects and field that points to it in the
   original memory graph (obtained using from_channel) *)

type pos = obj * int
let backlinks = (H.create 10 : pos H.t)

(* path to the current value in the input graph.
the invariant is kept by calling "with_field".
one should NEVER call "field".
*)
let path = (Stack.create () : pos Stack.t)

let with_field v i f =
  match !direction with
  | To -> f (field v i)
  | From ->
     Stack.push (v,i) path;
     f (field v i);
     let _ = Stack.pop path in ()

(* Updating procedure for abstract values *)
let abstract_update = (Stack.create () : (unit -> unit) Stack.t)

let check_size s v =
  exn "Incorrect block size" (lazy (guard (is_block v && size v = s)))
let check_tag t v =
  exn "Incorrect tag" (lazy (guard (is_block v && tag v == t)))

let is_custom s v =
  tag v == custom_tag
  && Objx.custom_identifier v = s

let ty_not_comparable : type a . a ty -> bool = function
  | (Desc.Poly.Poly_variant _) -> true
  | (Desc.Object.Object _) -> true
  | _ -> false

let is_char v =
  is_int v
  && let x = from_obj v in
     try x == Char.code (Char.chr x)
     with Invalid_argument _ -> false

(* {has_repr t} tests whether the type t is associated with the representation information.
   Note in particular that any type could be in this case, whether or not ty_desc yields Abstract.
 *)
let has_repr t =
  try let _ = Repr.repr t in true
  with _ -> false

(* WARNING: unsafe coercions from obj
 *)
let obj_to_repr r x = to_obj (r.Repr.to_repr (from_obj x))
let obj_from_repr r x =
  match r.Repr.from_repr (from_obj x) with
  | Some y -> to_obj y
  | None -> raise (Serialize_exception "Invalid abstract representation.")

let obj_update r x y = r.Repr.update (from_obj x) (from_obj y)
let obj_default r = to_obj r.Repr.default

(* Set the field in each back link to the new (abstract) value *)
let restore_edge a _
  = let a' = visit_val a
    in List.iter (fun (b,i) ->
           set_field (visit_val b) i a')
                 (H.find_all backlinks a)

(* For each abstract value, go through the list of back links and update them *)
let restore_abstract_edges () =
  H.iter restore_edge backlinks;
  Stack.iter (fun f -> f ()) abstract_update

let check_if p v = (guard -< p) >>. visit_val

(* @raise Serialize_exception *)
let rec convert dir t v =
  H.clear visit_ty_table;
  H.clear visit_val_table;
  H.clear backlinks;
  Stack.clear abstract_update;
  Stack.clear path;
  direction := dir;
  let result = if debug then check t v else exn "Incompatible value." (lazy (check t v)) in
  if is_from (!direction) then restore_abstract_edges ();
  H.reset visit_ty_table; (* reclaim space *)
  H.reset visit_val_table; (* reclaim space *)
  H.reset backlinks;
  Stack.clear abstract_update;
  Stack.clear path;
  result

and check : type a . a ty -> obj -> obj
  = fun t v ->
  let t = Antiunify.resolve_synonym t in
  if is_block v && not (visit_val_mem v) then
    visit_val_set v (dup v);
  if is_int v     (* immediate values can have many types *)
     || ty_not_comparable t (* we can't check it for equality *)
  then do_check t v
  else
    begin
      (* we must add the back links NOW, before checking if
         we've already visited that value.  *)
      if is_from (!direction)
         && has_repr t
         && not (Stack.is_empty path)
      then H.add backlinks v (Stack.top path);

      try let Ty.E t' = visit_ty v in
          let u = try Antiunify.anti_unify t t'
                  with _ -> raise (Serialize_exception "Antiunification.")
          in
          one_of [ lazy begin
                     guard (Ty.eq t' u);
                     visit_val v;
                   end
                 ; lazy begin
                     guard (Ty.neq u Ty.Any); (* why ? *)
                     memo_check u v
                   end
                 ]
      with Not_found -> memo_check t v
    end

(* [memo_check t v] @require [is_bloc v] *)
and memo_check : type a . a ty -> obj -> obj
  = fun t v ->
  visit_ty_set v t;
  do_check t v

and do_check : type a . a ty -> obj -> obj
  = fun t v -> let open Desc.T in
    match t with
    | Ty.Lazy t -> check_lazy t v (* could be a lazy abstract type *)
    | _ ->
      match desc t with
      | Class _
      | Abstract ->
        (match try Repr.repr t
           with Extensible.Type_pattern_match_failure _ ->
             raise (Serialize_exception "Abstract or Class without representation.")
           with
           | Repr.Repr r ->
             match !direction with
             | To ->
               let w = new_block 0 1 in (* tag 0, size 1 *)
               if is_block v then visit_val_set v w;
               set_field w 0 (check r.repr_ty (obj_to_repr r v));
               w
             | From ->
               visit_val_set v (obj_default r);
               let f1 = field v 0 in (* no back link for this indirection *)
               let w' = check r.repr_ty f1 in
               let w = obj_from_repr r w' in
               Stack.push (fun () -> obj_update r w w') abstract_update;
               visit_val_set v w;
               w)
      | _ ->
        check_concrete t v;
        visit_val v

(* check any type except abstract types, classes and lazy types *)
and check_concrete : type a . a ty -> obj -> unit
  = let open Ty.T in function
  (*| Var n       -> raise (Serialize_exception "Inhabited polymorphic type (Var)")  I removed variables*)
  | Ty.Any         -> raise (Serialize_exception "Inhabited polymorphic type.")
  | Int         -> guard -< is_int
  | Char        -> guard -< is_char
  | Float       -> check_tag double_tag >>. check_size 1
  | Bytes       -> check_tag string_tag
  | String      -> check_tag string_tag
  | Array Float -> check_tag double_array_tag
  | Array t     -> check_array t
  | Desc.Poly.Poly_variant v -> check_poly_variant v
  | Lazy _ -> assert false (* already checked by do_check *)
  | t -> let open Desc.T in
    match desc t with
    | Product (p, iso) -> check_tag 0 >>. check_product p
    | Record r  -> check_record r
    | Variant v -> check_variant v
    | Extensible x -> check_extensible x
    | Custom d -> guard -< is_custom d.identifier
    | Synonym (t, Equal.Refl) -> assert false (* check t -- never executed since we resolved synonyms earlier *)
    | Class _
    | Abstract -> assert false (* already checked by do_check *)
    | NoDesc -> raise (Serialize_exception "Abstract or Class without representation.")
    | _ -> raise (Serialize_exception "Unsupported type description.")

(* This function should always be called to check a field.
   It takes care of the stack invariant.
 *)
and check_field : type a . a ty -> obj -> int -> unit
  = fun t v i ->
    with_field v i (fun x -> set_field (visit_val v) i (check t x))

(* ASSUMPTION: arrays have tag 0 *)
and check_array : type a . a ty -> obj -> unit =
  fun t v ->
    check_tag 0 v;
    for i = 0 to (size v - 1) do
      check_field t v i
    done

and check_product : type p . p Product.t -> obj -> unit
  = fun p v -> check_fields_from 0 p v

(* check_fields_from i p v
will check the block fields i,i+1,...(size v -1)
with the types given in the product description 'p'
 *)
and check_fields_from : type p . int -> p Product.t -> obj -> unit
  = fun a p v ->
  try List.iter2
        (fun (Ty.E t) i -> check_field t v i)
        (Product.list_of_prod p)
        (Listx.from_to a (size v - 1))
  with Invalid_argument _ ->
    raise (Serialize_exception "Block of incorrect length.")

and check_record_fields : type p r . int -> (p, r) Desc.Fields.t -> (p -> r) -> obj -> unit
  = fun i r fwd v ->
    (* Mutable fields impose a monomorphic constraint on the
       record type, thus they can't contain `Any' *)
    guard (List.for_all Patterns.no_free_var' (Desc.Fields.types_of_mutable_fields r));
    (* Recursively checking each field *)
    check_fields_from i (Desc.Fields.product r) v;
    (* Checking that the fields are in the same order as in the record_desc *)
    guard (Objx.fields_all2 (==) v (to_obj (fwd (from_obj (Objx.listify v)))))

(* "check_record": Since we cannot be sure that the record
description lists the fields in the correct order, we must
test that the isomorphism is the identity.  I wonder if we
may test this once and for all by testing the isomorphism on
a record where all fields have a distinct Obj representation.
(and assuming the isomorphism provided doesn't manipulate the
field values themselves.) *)

and check_record : type p r . (p, r) Desc.Record.t -> obj -> unit
  = fun r v ->
    let open Desc.Record in
    let open Desc.Fields in
    if r.unboxed then
      match r.fields with
      | Cons ({ty;_}, Nil) -> let _ = check ty v in ()
      | _ -> serialize_exn unboxed_exn;
    else begin
      check_tag 0 v;
      check_record_fields 0 r.fields r.iso.fwd v;
    end

and check_variant : type v . v Desc.Variant.t -> obj -> unit
  = fun d v ->
    let cs = d.cons in
    if d.unboxed then
      begin
        exn unboxed_exn (lazy (guard (Desc.Variant.cst_len cs == 0
                                      && Desc.Variant.ncst_len cs == 1)));
        match Desc.Variant.ncst_get cs 0 with
        | Desc.Con.Con {args = Product (Product.Cons (ty, Product.Nil)); _} ->
          let _ = check ty v in ()
        | Desc.Con.Con {args = Record (Desc.Fields.Cons ({ty;_}, Desc.Fields.Nil)); _} ->
          let _ = check ty v in ()
        | _ -> serialize_exn unboxed_exn;
      end
    else if is_int v
         && Fun.in_range (from_obj v) (0, Desc.Variant.cst_len cs - 1)
    then ()
    else begin
      guard (tag v < Desc.Variant.ncst_len cs);
      check_args (Desc.Variant.ncst_get cs (tag v)) v;
    end

(* Important: polymorphic variants are NOT flattened (cf manual 19.3.6)
Polymorphic variant values have different representations depending on the arity of the constructor

0: `VConstr is represented by hash_variant("VConstr").

1: `VConstr(v) is represented by a block of size 2 and tag 0, with field number
  0 containing hash_variant("VConstr") and field number 1
  containing v.

>1: `VConstr(v0,..,vn) is represented by a block of size 2 and tag 0, with field number
  0 containing hash_variant("VConstr") and field number 1
  containing the representation of the tuple (v0,..,vn) (another block of tag 0).
*)
and check_poly_variant : type v . v Desc.Poly.t -> obj -> unit
  = fun h v ->
  try let k = Objx.poly_hash v in
      let c = Desc.Poly.find h k in
      one_of [ lazy (guard (is_int v && Desc.Con.arity c == 0))
             ; lazy begin
                   guard (Desc.Con.arity c >= 2);
                   with_field v 1
                              (fun f1 ->
                                if is_int f1
                                then check_single c v
                                else
                                  one_of [ lazy (check_args c f1)
                                         ; lazy (check_single c v)
                                         ]
                              )
                 end
             ]
  with Not_found -> raise (Serialize_exception "Polymorphic variant, constructor not found.")

and check_args : type v . v Desc.Con.t -> obj -> unit
  = fun (Desc.Con.Con c) ->
    match c.args with
    | Desc.Con.Product p -> check_product p
    | Desc.Con.Record r -> check_record_fields 0 r c.embed

(* Singleton constructor *)
and check_single : type v . v Desc.Con.t -> obj -> unit
  = fun (Desc.Con.Con c) v ->
    let open Desc.Fields.T in
    let open Product.T in
    let open Desc.Con in
    match c.args with
    | Product (Cons (ty, Nil)) -> check_field ty v 1
    | Record (Cons ({ty;_}, Nil)) -> check_field ty v 1
    | _ -> raise (Serialize_exception "Polymorphic variant, invalid singleton constructor.")


and check_extensible : type t . t Desc.Ext.t -> obj -> unit
  = fun x v ->
  try let w = Desc.Ext.fix x (from_obj v) in
      visit_val_set v (to_obj w);
      let Desc.Con.Con c = Desc.Ext.con x w
      in one_of [ lazy (guard (tag v == object_tag)) (* constant constructor *)
                ; lazy (match c.args with
                      | Product p -> check_fields_from 1 p (to_obj w)
                      | Record r -> check_record_fields 1 r c.embed (to_obj w))]
  with Not_found -> raise (Serialize_exception "Extensible type, constructor not found.")

(* (comment from stdlib/lazy.ml)
 * A value of type ['a Lazy.t] can be one of three things:
 *
 * 1. A block of size 1 with tag [lazy_tag].  Its field is a
 *    closure of type [unit -> 'a] that computes the value.
 *
 * 2. A block of size 1 with tag [forward_tag].  Its field is
 *    the value of type ['a] that was computed.
 *
 * 3. Anything else except a float.  This has type ['a] and
 * is the value that was computed.
 *
 *)
and check_lazy : type t . t ty -> obj -> obj
  = fun t v ->
    match Objx.tag_view (tag v) with
    | Double (* illegal value (case 3) *)
      -> raise (Serialize_exception "Lazy type, invalid value (double).")
    | Lazy  (* case 1. We can't check a CLOSURE *)
      -> raise (Serialize_exception "Lazy type, closures are not supported.")
    | Forward -> (* case 2 *)
        guard (size v == 1);
        check_field t v 0;
        visit_val v
    | _ -> check t v (* case 3 *)

(*************************************************
 * Main functions
 *)

(* Raise: Serialize_exception (TODO: document other exceptions that may be raised) *)
let to_repr : 'a ty -> 'a -> obj
  = fun t x ->
    convert To t (to_obj x)

(* Raise: Serialize_exception (TODO: document other exceptions that may be raised) *)
let from_repr : 'a ty -> obj -> 'a
  = fun t v ->
    from_obj (convert From t v)

let to_channel : 'a ty -> out_channel -> 'a -> Marshal.extern_flags list -> unit
  = fun t oc x ->
    Marshal.to_channel oc (t, to_repr t x)

let to_string : 'a ty -> 'a -> Marshal.extern_flags list -> string
  = fun t x ->
    Marshal.to_string (t, to_repr t x)

let to_bytes : 'a ty -> 'a -> Marshal.extern_flags list -> bytes
  = fun t x ->
    Marshal.to_bytes (t, to_repr t x)

let safe_cast : 'a ty -> obj -> 'a
  = fun t v ->
    if is_block v && size v == 2 then
      let ty = Generic_fun.Deepfix.deepfix (Ty t) (from_obj (field v 0))
      and rep = field v 1
      in if ty = t then from_repr t rep
      else
        begin
          if debug then
            begin
              print_endline "*** safe_cast: the deserialized type witness doesn't match the given type witness.";
              print_endline "*** argument witness:";
              Obj_inspect.print_obj t;
              print_endline "*** deserialized witness:";
              Obj_inspect.print_obj ty;
            end;
          raise (Serialize_exception "The serialized value has a different type than was expected.")
        end
    else raise (Serialize_exception ("Incorrect serialized value, it was not serialized using module " ^ __MODULE__))

let from_channel : 'a ty -> in_channel -> 'a
  = fun t ic ->
    safe_cast t (Marshal.from_channel ic)

let from_string : 'a ty -> string -> int -> 'a
  = fun t s i ->
    safe_cast t (Marshal.from_string s i)

let from_bytes : 'a ty -> bytes -> int -> 'a
  = fun t s i ->
    safe_cast t (Marshal.from_bytes s i)
