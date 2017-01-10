(* See explanations in the book:
http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora115.html
*)

(* In order to detect cycles, we use a hashtable with
   physical equality on untyped objects *)

open Generic_util

module HashedObj =
  struct
    type t = Obj.t
    let equal = (==)
    let hash = Hashtbl.hash
  end

module H = Hashtbl.Make (HashedObj)

let visit = (H.create 10 : unit H.t)

let address v =
  Printf.sprintf "0x%x" (Obj.obj v)

let string_of_tag =
  function
  | t when t == Obj.string_tag       -> "string"
  | t when t == Obj.double_tag       -> "double"
  | t when t == Obj.double_array_tag -> "double_array"
  | t when t == Obj.closure_tag      -> "closure"
  | t when t == Obj.lazy_tag         -> "lazy"
  | t when t == Obj.object_tag       -> "object"
  | t when t == Obj.infix_tag        -> "infix"
  | t when t == Obj.forward_tag      -> "forward"
  | t when t == Obj.abstract_tag     -> "abstract"
  | t when t == Obj.custom_tag       -> "custom"
  | t                            -> string_of_int t

let rec from_to a b =
  if a > b then []
  else a :: from_to (a+1) b

let show_float_array xs =
  "[|" ^ String.concat "; "
                       (List.map (fun i -> string_of_float (xs.(i)))
                       (from_to 0 (Array.length xs - 1)))
       ^ "|]"

let newline = "\n"
let indent_step = 2
let margin n = String.make (n * indent_step) ' '
let max_margin = 8 * indent_step

let rec inspect n v =
  let prefix = if n > max_margin then ""
               else margin n
  and suffix = if H.mem visit v then
                 "block --> "^ address v ^"\n"
               else
                 begin
                   if Obj.is_block v then H.add visit v ();
                   inspect_case n v
                 end
  in prefix ^ suffix

and inspect_case n =
  function
  | v when Obj.is_int v -> "int: " ^ string_of_int (Obj.obj v) ^ newline
  | v when Obj.is_block v
           -> let s = Obj.size v in
              "block @ " ^ address v
              ^ ", size " ^ string_of_int s
              ^ ", tag " ^ string_of_tag (Obj.tag v) ^ " = " ^
                (Obj.tag v |>
                function
                | t when t == Obj.closure_tag ->
                  string_of_int (s - 1)
                  ^ " free variables\n"
                  ^ margin(n+1)
                  ^ "code pointer: "
                  ^ inspect_pointer (Obj.field v 0)
                  ^ newline
                  ^ inspect_fields v (n+1) 1 (s-1)
                | t when t == Obj.string_tag ->
                   "\""^ Obj.obj v ^"\"\n"
                | t when t == Obj.double_tag ->
                   string_of_float (Obj.obj v) ^ newline
                | t when t == Obj.double_array_tag ->
                   show_float_array (Obj.obj v) ^ newline
                | t when t < Obj.no_scan_tag && t >= 0 ->
                   "structure\n"
                   ^ inspect_fields v (n+1) 0 (s-1)
                | t when t == Obj.custom_tag ->
                   "{ identifier = " ^ Objx.custom_identifier v ^ ", ... }\n"
                | _ -> "...\n")
  | _ -> "neither a value nor a block\n"

  (* ocaml integers representation is shifted one bit to the right (divided by two),
     thus to recover a pointer'value we must multiply the integer by two. *)
and inspect_pointer =
  function
  | v when Obj.is_block v -> address v
  | _ -> "Not a pointer"
(*
and inspect_pointer = let open Obj in
  function
  | v when is_block v ->
     let open Big_int in
     let half = big_int_of_int (obj v) in
     let ptr = mult_int_big_int 2 half in
     if is_int_big_int ptr
     then Printf.sprintf "0x%x" (int_of_big_int ptr)
     else Printf.sprintf "2*0x%x" (int_of_big_int half)
  | _ -> "Not a pointer"
*)

and inspect_fields v n b e =
   String.concat ""
     (List.map (fun i -> inspect n (Obj.field v i))
     (from_to b e))

let show_obj x =
  begin
    H.clear visit;
    let str = inspect 0 (Obj.repr x) in
    H.reset visit;
    str
  end

let print_obj x = print_string (show_obj x)

let show_obj_short x =
  if Obj.is_int x then string_of_int (Obj.obj x) else address x
