(**  A PPX for explicitely listing the names that are imported from a module.
    usage: [[%%import Module (x, y, z)]] will be translated to:

{[
    let x = Module.x
    and y = Module.y
    and z = Module.z
]}

[[%%import Module x]] is translated to [let x = Module x]

[[%%import Module]] is not valid.

The identifiers may be renamed using the syntax:

[%%import Module (x, y' <- y, z)]

TODO which is equivalent to

[%%import Module (x, (y' <- y), z)]

Invalid extensions are not interpreted and will raise an error.

"import" is like a selective "include".
*)

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Generic_util

let import_name = "import"

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
    -> (match e.pexp_desc with
        | Pexp_tuple is
          -> import_list loc module_name.txt is
        | Pexp_ident _ -> import_list loc module_name.txt [e]
        | _ -> raise Import)
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
