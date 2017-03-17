(**  A PPX for explicitely listing the names that are imported from a module.
    usage: [[%%import M (x; y; z)]] will be translated to:

{[
    let x = M.x
    and y = M.y
    and z = M.z
]}

[[%%import M x]] is translated to [let x = M.x]

[[%%import M]] is not valid.

The identifiers may be renamed using the syntax:

[%%import M (x ; y' := y ; z)]

which is translated to:
{[
    let x  = M.x
    and y' = M.y
    and z  = M.z
]}

Invalid extensions are not interpreted and will raise an error.

[[%%import]] is like a selective "include".

TODO other syntax

[%%import M (f; g' := g; h)]

[%%import M (f; g' := g; h)
  [@@type t; u' := u; v]
  [@@module A; B' := B; C]]

[%%import M
  [@@val f; g' := g; h ]
  [@@type t; u' := u; v]
  [@@module A; B' := B; C]]

open%import M
  [@@val f; g' := g; h ]
  [@@type t; u' := u; v]
  [@@module A; B' := B; C]

{3 local imports}

TODO: At the expression level,

{[
[%import M (a; g' := g; b)] (g' a b)
]}

translates to
{[
  let a = M.a
  and g' = M.g
  and b = M.b
  in g' a b
]}
*)

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Generic_util

let import_name = "import"
let concat_map f xs = List.concat (List.map f xs)

(* [seq] takes an expression of the shape:
  (e1; e2; .. ; en) and returns the list of expressions
  [e1; e2; .. en] (and then listify each of them)
*)
let rec seq e = match e.pexp_desc with
  | Pexp_sequence (a, b) -> a :: seq b
  | _ -> [e]

let import loc module_id name new_name =
  { pvb_pat = Pat.var new_name
  ; pvb_expr = Exp.ident {txt=Ldot(module_id, name.txt); loc=name.loc}
  ; pvb_attributes = []
  ; pvb_loc = loc
  }

exception Import

let import_exp loc module_id e =
  match e.pexp_desc with
  | Pexp_ident id ->
    (match id.txt with
     | Lident name ->
       let name_loc = {txt=name; loc=id.loc}
       in import loc module_id name_loc name_loc
     | _ -> raise Import)
  (* | Pexp_apply (Pexp_ident {txt=":=";_} *)
  (*              , [ (Nolabel, Pexp_ident new_name) *)
  (*                , (Nolabel, Pexp_ident old_name)]) -> *)
  (*   import loc module_id new_name old_name *)
  | Pexp_setinstvar (new_name_loc, {pexp_desc=Pexp_ident id;_}) ->
    (match id.txt with
     | Lident name ->
       import loc module_id {txt=name; loc=id.loc} new_name_loc
     | _ -> raise Import)
  | _ -> raise Import

let import_list loc mod_id is =
  { pstr_desc = Pstr_value (Nonrecursive, List.map (import_exp loc mod_id) is)
  ; pstr_loc = loc}

let extension loc = function
  | Pexp_construct (module_name, Some e)
    -> import_list loc module_name.txt (seq e)
  | _ -> raise Import


let ext_structure default s = match s.pstr_desc with
  | Pstr_eval (e, _) ->
    (try extension s.pstr_loc e.pexp_desc
     with Import -> default)
  | _ -> default

let structure_item m si =
  match si.pstr_desc with
  | Pstr_extension ((name, PStr str),_)
    when name.txt = import_name
    -> List.map (ext_structure (m.structure_item m si)) str
  | _ -> [m.structure_item m si]

let top =
  { default_mapper with
    structure =
      (fun m ss ->
         List.concat (List.map (structure_item m) ss))
  }

let () = register "import" (fun argv -> top)
