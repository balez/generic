(** reify generates type witness and type descriptions for
    the types definitions annotated with [@@reify].  The top
    level annotation [@@@reify_all] will process every type
    definition in the current file, unless they are annotated
    with [@@dont_reify].

    A [Generic_core.Ty.t] constructor (type witness) is added
    directly after each annotated type definition. The name
    of the constructor is the same as the type name but with
    the first letter in uppercase.

    All the type witnesses that are needed must be explicitly
    imported by the user in the current namespace. In
    particular, the witnesses for the builtin types should be
    imported from [Generic_core.Ty.T].

    Be careful with name shadowing!

    TODO: better error locations.
*)


(* implementation notes

   when using the ast_mapper approach, we often needs to pass
   down contextual information down the tree. We can do that
   either by using an implicit state, with mutable
   variables. or by using the open recursive style of the
   mapper. The later approach doesn't have side effects.  We
   define clojures to share the contextual information
   between the records fields. (See [main] for instance)
*)

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Generic_util

exception Not_implemented

let (-<) = Fun.(-<)

let local_invalid_arg msg = invalid_arg (__MODULE__ ^ "." ^ msg)
let reify_attrname = "reify"
let reify_all_attrname = "reify_all"
let abstract_attrname = "abstract"
let dont_reify_attrname = "dont_reify"
let generic_core_ty = Ldot (Lident "Generic_core", "Ty")
let ty_lid = Ldot (generic_core_ty, "ty")

(* predicates on attributes *)
let filter_attr attr_name =
  List.filter (fun (l,p) -> l.txt = attr_name)

let has_attr attr_name attrs =
  filter_attr attr_name attrs != []

let tydecl_has_attr attrname tydecl =
  has_attr attrname tydecl.ptype_attributes

let filter_reify = filter_attr reify_attrname
let has_reify = has_attr reify_attrname
let tydecl_has_reify = tydecl_has_attr reify_attrname
let has_dont_reify = has_attr dont_reify_attrname
let tydecl_dont_reify = tydecl_has_attr dont_reify_attrname
let tydecl_has_abstract = tydecl_has_attr abstract_attrname

(* Obtain the name of the witness given the name of the type.
   (not a longident)
*)
let witness_name s = String.capitalize_ascii s

let witness_lid = function
  | Ldot (Lident "Lazy", "t") -> Ldot (generic_core_ty, "Lazy")
  | Lident t -> Lident (witness_name t)
  | Ldot (mod_lid, t) -> Ldot (mod_lid, witness_name t)
  | _ -> local_invalid_arg ("witness_lid: expected type identifier")

let loc ?(loc = !default_loc) txt = {txt; loc}
let lid s = loc (Lident s)
let to_lid {txt; loc} = {txt=Lident txt ;loc}

let exp_str s = Exp.constant (Pconst_string (s, None))
let exp_str_append x y = [%expr [%e exp_str x] ^ [%e exp_str y]]

let rec exp_str_list = function
  | [] -> Exp.construct (lid "[]") None
  | x :: y -> Exp.construct (lid "::") (Some [%expr ([%e exp_str x], [%e exp_str_list y])])

(** builds an expression denoting a list of expression *)
let exp_list =
  Listx.foldr
    (fun e es -> [%expr [%e e] :: [%e es]])
    [%expr []]

let var name n = name ^ string_of_int n

(** builds a list of variable names of size [n] *)
let rec vars name n =
  List.map (var name) (Listx.from_to 1 n)

(** builds a list of type variables of size [n] *)
let ty_vars n =
  List.map Typ.var (vars "a" n)

let var_name = "x"

let pat_var n =
  Pat.var (loc (var var_name n))

(** List of pattern variables of size [n] *)
let pat_vars n =
  List.map pat_var (Listx.from_to 1 n)

let exp_var n =
  Exp.ident (lid (var var_name n))

let exp_vars n =
  List.map exp_var (Listx.from_to 1 n)

(** {[ prod [t1;t2..;tn] = Cons (t1 , Cons (t2, ... Cons (tn, Nil) ...)) ]} *)
let prod =
  Listx.foldr
    (fun head tail -> [%expr Generic_core.Product.T.Cons
        ([%e head] , [%e tail])])
    [%expr Generic_core.Product.T.Nil]

(** [Ty x1; ... Ty xn] *)
let ty_args n =
  List.map (fun x -> [%expr Generic_core.Ty.Ty [%e x]]) (exp_vars n)

(** Cons (Ty x1 , Cons (Ty x2, ... Cons (Ty xn, Nil) ...)) *)
let ty_prod n =
  prod (ty_args n)

(** (x1, (x2, ..., (xn, ())...)) *)
let exp_nested_tuple =
  Listx.foldr
    (fun head tail -> [%expr ([%e head], [%e tail])])
    [%expr ()]

(** (x1, (x2, ..., (xn, ())...)) *)
let pat_nested_tuple =
  Listx.foldr
    (fun head tail -> [%pat? ([%p head], [%p tail])])
    [%pat? ()]

(** {f1, f2, ... fn} *)
let exp_record_pun fields =
  Exp.record (List.map (fun loc -> (loc, Exp.ident loc))
                fields) None

(** {f1, f2, ... fn} *)
let pat_record_pun fields =
  Pat.record (List.map (fun loc -> (to_lid loc, Pat.var loc))
                fields) Closed


(** General constructor where the arguments are given as a list.
 *)
let cons constructor tuple name =
  let consN args = constructor (loc name) args
  in
  let cons0 = consN None
  and cons1 param = consN (Some param)
  and cons2 params = consN (Some (tuple params))
  in function
    | [] -> cons0
    | [p] -> cons1 p
    | ps -> cons2 ps

let exp_cons n = cons Exp.construct Exp.tuple (Lident n)
let pat_cons n = cons Pat.construct Pat.tuple (Lident n)

let exp_cons' = cons Exp.construct Exp.tuple
let pat_cons' = cons Pat.construct Pat.tuple

(** Builds the [core_type] corresponding to [sigma ty] given
the core type representing [sigma] *)
let ty sigma = (* Typ.constr (loc ty) [param] *)
  [%type: [%t sigma] Generic_core.Ty.ty]

let unit_pattern = (* Pat.construct (loc (Lident "()")) None *)
  [%pat? ()]

(** let () = expr *)
let statement expr =   (* Str.value Nonrecursive [Vb.mk unit_pattern expr]*)
  [%stri let () = [%e expr]]

let anys n = Listx.replicate n [%expr Generic_core.Ty.Any]

(** Builds the [Ty] constructor (eg: [Int]) from the
    [core_type] representation of the type (eg: [int]) The first
    argument is the list of type parameters in the context. *)
let rec witness vars ct = match ct.ptyp_desc with
  | Ptyp_constr (lid, args)
    -> exp_cons'
         (witness_lid lid.txt)
         (List.map (witness vars) args)
  | Ptyp_var x
    -> (try exp_var (1 + Listx.index x vars)
       with Not_found -> [%expr Generic_core.Ty.Any])

  | Ptyp_tuple [a;b]
    -> [%expr Generic_core.Ty.Pair
        ([%e witness vars a], [%e witness vars b]) ]
  | Ptyp_tuple [a;b;c]
    -> [%expr Generic_core.Ty.Triple
        ([%e witness vars a], [%e witness vars b], [%e witness vars c])]
  | Ptyp_tuple [a;b;c;d]
    -> [%expr Generic_core.Ty.Quadruple
        ([%e witness vars a], [%e witness vars b], [%e witness vars c], [%e witness vars d])]
  | Ptyp_tuple [a;b;c;d;e]
    -> [%expr Generic_core.Ty.Quintuple
        ([%e witness vars a], [%e witness vars b], [%e witness vars c], [%e witness vars d], [%e witness vars e])]
  | Ptyp_tuple [a;b;c;d;e;f]
    -> [%expr Generic_core.Ty.Sextuple
        ([%e witness vars a], [%e witness vars b], [%e witness vars c], [%e witness vars d], [%e witness vars e]
        , [%e witness vars f])]
  | Ptyp_tuple [a;b;c;d;e;f;g]
    -> [%expr Generic_core.Ty.Septuple
        ([%e witness vars a], [%e witness vars b], [%e witness vars c], [%e witness vars d], [%e witness vars e]
        , [%e witness vars f], [%e witness vars g])]
  | Ptyp_tuple [a;b;c;d;e;f;g;h]
    -> [%expr Generic_core.Ty.Octuple
        ([%e witness vars a], [%e witness vars b], [%e witness vars c], [%e witness vars d], [%e witness vars e]
        , [%e witness vars f], [%e witness vars g], [%e witness vars h])]
  | Ptyp_tuple [a;b;c;d;e;f;g;h;i]
    -> [%expr Generic_core.Ty.Nonuple
        ([%e witness vars a], [%e witness vars b], [%e witness vars c], [%e witness vars d], [%e witness vars e]
        , [%e witness vars f], [%e witness vars g], [%e witness vars h], [%e witness vars i])]
  | Ptyp_tuple [a;b;c;d;e;f;g;h;i;j]
    -> [%expr Generic_core.Ty.Decuple
        ([%e witness vars a], [%e witness vars b], [%e witness vars c], [%e witness vars d], [%e witness vars e]
        , [%e witness vars f], [%e witness vars g], [%e witness vars h], [%e witness vars i], [%e witness vars j])]

  | _ -> [%expr Generic_core.Ty.Any]

(** Builds a constructor description [Desc.Con] from :

    [single]: true if the constructor is the only one for this variant.
    [args]: list of the type parameters of arguments
    [constr]: name of the constructor
*)
let make_con single args constr =
  let num_args = List.length args in
  let pat_args = pat_vars num_args in
  let exp_args = exp_vars num_args in
  let pat_embed = pat_nested_tuple pat_args in
  let exp_embed = exp_cons constr exp_args in
  let pat_proj = pat_cons constr pat_args in
  let exp_proj = exp_nested_tuple exp_args in
  [%expr Generic_core.Desc.Con.make
      [%e exp_str constr]
      [%e prod args]
      (fun [%p pat_embed] -> [%e exp_embed])
      [%e if single
        then [%expr (fun [%p pat_proj] -> Some [%e exp_proj])]
        else [%expr
          (function
            | [%p pat_proj] -> Some [%e exp_proj]
            | _ -> None)]]
  ]

(** [single]: true if the constructor is the only one for this variant.
    [vars] the type parameters of the variant.
    [c] the constructor declaration.
*)
let variant_constructor single vars c =
  let args = List.map (witness vars) (
    match c.pcd_args with
    | Pcstr_tuple cts -> cts
    | Pcstr_record lds -> List.map (fun ld -> ld.pld_type) lds)
  and constr = c.pcd_name.txt in
  make_con single args constr

(* The argument must be a type variable. *)
let var_name ct = match ct.ptyp_desc with
  | Ptyp_var name -> name
  | Ptyp_any -> "_"
  | _ -> local_invalid_arg "var_name: expected type variable."

(** Returns the list of type variables names from the
    parameter list of a type declaration *)
let params t =
  List.map (fun (t,_) -> var_name t) t.ptype_params

(** Assumes [t.ptype_kind = Ptype_variant cds] *)
let desc_variant module_path t cds =
  let single = List.length cds = 1 in
  [%expr Generic_core.Desc.Variant
      { Generic_core.Desc.Variant.name = [%e exp_str t.ptype_name.txt]
      ; Generic_core.Desc.Variant.module_path = [%e exp_str_list module_path]
      ; Generic_core.Desc.Variant.cons =
          Generic_core.Desc.Variant.cons
            [%e exp_list (List.map (variant_constructor single (params t)) cds)]
      }]

let exp_field_mutable ld =
  let field = to_lid ld.pld_name in
  match ld.pld_mutable with
  | Immutable -> [%expr None]
  | Mutable ->
    [%expr Some (fun r x -> [%e
                  Exp.setfield [%expr r] field [%expr x]])]

let exp_field vars ld =
  [%expr { Generic_core.Desc.Field.name = [%e exp_str ld.pld_name.txt]
         ; Generic_core.Desc.Field.ty = [%e witness vars ld.pld_type]
         ; Generic_core.Desc.Field.set = [%e exp_field_mutable ld]
         }]

let exp_fields vars =
  Listx.foldr (fun ld ts ->
      [%expr Generic_core.Desc.Fields.Cons ([%e exp_field vars ld], [%e ts])])
    [%expr Generic_core.Desc.Fields.Nil]

let exp_iso vars lds =
  let fields = List.map (fun ld -> ld.pld_name) lds in
  let lids = List.map to_lid fields in
  let idents = List.map Exp.ident lids in
  let vars = List.map Pat.var fields in
  [%expr { Generic_util_fun.fwd = (fun [%p pat_nested_tuple vars] -> [%e exp_record_pun lids])
         ; Generic_util_fun.bck = (fun [%p pat_record_pun fields] -> [%e exp_nested_tuple idents])
         }
  ]

let desc_record module_path t lds =
  [%expr Generic_core.Desc.Record
      { Generic_core.Desc.Record.name = [%e exp_str t.ptype_name.txt]
      ; Generic_core.Desc.Record.module_path = [%e exp_str_list module_path]
      ; Generic_core.Desc.Record.fields = [%e exp_fields (params t) lds]
      ; Generic_core.Desc.Record.iso = [%e exp_iso (params t) lds]
      }
  ]

let no_desc = [%expr Generic_core.Desc.NoDesc]
let desc_open t = no_desc

let desc_synonym module_path t t' =
  let wt' = witness (params t) t' in
  [%expr Generic_core.Desc.Synonym ([%e wt'], Generic_core.Equal.Refl)]

(** Extends [Desc_fun.view] for type [t] *)
let desc_ext module_path t =
  if tydecl_has_abstract t then
    [%expr Generic_core.Desc.Abstract]
  else
  match t.ptype_manifest with
  | Some t' -> desc_synonym module_path t t'
  | None ->
    match t.ptype_kind with
    | Ptype_variant cds -> desc_variant module_path t cds
    | Ptype_record lds ->  desc_record module_path t lds
    | Ptype_abstract -> [%expr Generic_core.Desc.Abstract]
    | _ -> [%expr Generic_core.Desc.NoDesc]

(* [str_witness] Build the structure item
   {[
       _ ty += X : x ty
   ]}
*)

let str_witness t =
  let ty_name = t.ptype_name.txt in
  let lid_name = {txt = Lident ty_name; loc = t.ptype_name.loc} in
  let constr = witness_name ty_name in
  let num_params = List.length t.ptype_params in
  let vars = ty_vars num_params in
   (* extend Generic_core.Ty.ty *)
    Str.type_extension
      (Te.mk ~params:[(Typ.any (), Invariant)] (loc ty_lid)
         [Te.constructor (loc constr)
            (Pext_decl    (* Pext_decl of constructor_arguments * core_type option *)
               ( Pcstr_tuple (List.map ty vars)
               , Some (ty (Typ.constr lid_name vars)))
            )
         ])

(* [sig_witness] Build the signature item
   {[
       _ ty += X : x ty
   ]}
*)

let sig_witness t =
  let ty_name = t.ptype_name.txt in
  let lid_name = {txt = Lident ty_name; loc = t.ptype_name.loc} in
  let constr = witness_name ty_name in
  let num_params = List.length t.ptype_params in
  let vars = ty_vars num_params in
   (* extend Generic_core.Ty.ty *)
    Sig.type_extension
      (Te.mk ~params:[(Typ.any (), Invariant)] (loc ty_lid)
         [Te.constructor (loc constr)
            (Pext_decl    (* Pext_decl of constructor_arguments * core_type option *)
               ( Pcstr_tuple (List.map ty vars)
               , Some (ty (Typ.constr lid_name vars)))
            )
         ])

let make_ext_con exp_ty pat_ty constr args =
  [%stri let () = Generic_core.Desc_fun.ext_add_con
               [%e exp_ty]
    { Generic_core.Desc.Ext.con =
        fun (type a)
            (ty : a Generic_core.Ty.ty) -> (match ty with
            | [%p pat_ty]
              -> [%e make_con false args constr]
            | _ -> assert false : a Generic_core.Desc.Con.t)}
    ]

(* only use this function if the constructor is not a synonym *)
let ext_constructor ty params c =
  let num_params = List.length params in
  let pat_params = pat_vars num_params
  and exp_params = anys num_params
  in let exp_t = exp_cons' ty exp_params
  and pat_t = pat_cons' ty pat_params
  and constr = c.pext_name.txt
  and args = match c.pext_kind with
    | Pext_decl (args, _) ->
      List.map (witness params) (
          match args with
          | Pcstr_tuple cts -> cts
          | Pcstr_record lds -> List.map (fun ld -> ld.pld_type) lds)
    | _ -> assert false
  in
  make_ext_con exp_t pat_t constr args

(* Effectful statements, to update the type description:
   Desc.view, and Ty_desc.ext
*)
let new_desc module_path t =
  let ty_name = t.ptype_name.txt in
  let constr = witness_name ty_name in
  let num_params = List.length t.ptype_params in
  let exp_conpat = exp_cons constr (anys num_params) in
  let pat_constr = pat_cons constr (pat_vars num_params) in
  let ty_desc_ext = (* extend Generic_core.Ty_desc.ext *)
    make_ext_con
      [%expr Generic_core.Ty.Ty [%e exp_conpat]]
      [%pat? Generic_core.Ty.Ty [%p pat_constr]]
      constr
      (ty_args num_params)

  and desc_fun_ext = (* extend Generic_core.Desc_fun.view *)
    [%stri
      let () =
        Generic_core.Desc_fun.ext [%e exp_conpat]
          { Generic_core.Desc_fun.f =
              fun (type a) (ty : a Generic_core.Ty.ty) ->
                (match ty with
                 | [%p pat_constr] -> [%e desc_ext module_path t]
                 | _ -> assert false : a Generic_core.Desc.t)
          }
    ]
  and ext_reg = (* register an extensible type *)
    [%stri
      let () =
        Generic_core.Desc_fun.ext_register
          [%e exp_conpat]
          [%e exp_str ty_name]
    ]
  in
  let open_type =
    t.ptype_manifest = None
    && t.ptype_kind == Ptype_open
  in
  [ty_desc_ext;
   if open_type && not (tydecl_has_abstract t)
   then ext_reg else desc_fun_ext]

(** Structure and signature items have commonalities that we
    capture with the `item` type. This allows us to share the
    code of `new_item' for structures and signatures. *)

type item =
  | Type of rec_flag * type_declaration list
  | Attr of attribute
  | Exn of extension_constructor (* reify only in structures *)
  | TypExt of type_extension (* reify only in structures *)
  | Other

(** checks if there is a global [@@@reify_all] attribute, and
    The attribute must appear in the file before the first
    type declaration.
*)

let check_reify_all proj ss =
  let reify_all item = match proj item with
    | Attr (attr, _)
      when attr.txt = reify_all_attrname
      -> Some attr
    | _ -> None
  and is_tydecl item = match proj item with
    | Type (_,_) -> true
    | _ -> false
  in
  let is_reify_all_attr x = reify_all x <> None in
  let rest = Listx.drop_while (fun x -> not (is_reify_all_attr x
                                            || is_tydecl x)) ss
  in
  if rest = [] then false
  else let first = List.hd rest in
    if is_tydecl first then
      match Listx.find_some reify_all rest with
      | Some {txt; loc} ->
        let msg = "[@@@" ^ reify_all_attrname ^ "] must be used before any type definition"
        in raise (Location.Error (Location.error ~loc msg))
      | None -> false
    else let attrs = Listx.filter_some reify_all rest in
      if List.length attrs > 1 then
        match List.nth attrs 1 with
        | {txt=_;loc} ->
          let msg = "[@@@" ^ reify_all_attrname ^ "] must be used only once per file."
          in raise (Location.Error (Location.error ~loc msg))
      else true

(** Remove all occurences of the attribute "reify" *)
let rm_reify_mapper =
  { default_mapper with
    attributes = fun sub attributes ->
      let remove_reify =
        List.filter (fun ({txt;loc},p) -> txt <> reify_attrname)
      in default_mapper.attributes sub (remove_reify attributes)
  }

(** Check that "reify" is used properly and remove the attribute. *)
let rm_reify_tydecl sub tydecl =
  match filter_reify tydecl.ptype_attributes  with
  | [] -> default_mapper.type_declaration sub tydecl
  | [(_,PStr [])] -> default_mapper.type_declaration rm_reify_mapper tydecl
  | ({txt=_;loc},_) :: _ ->
    let msg = "[@@"^ reify_attrname ^"] doesn't take any payload and should be used only once.per type declaration" in
    raise (Location.Error (Location.error ~loc msg))

(** Computes the new items if the structure item is a group
    of type declarations. *)
let new_items new_repr ext_exn typ_ext reify_all inside_module module_name = function
  | Type (_, tydecls) ->
    let types =
      if reify_all
      then List.filter (not -< tydecl_dont_reify) tydecls
      else List.filter tydecl_has_reify tydecls
    in
    new_repr types

  (* checking that reify_all is not used in a submodule *)
  | Attr ({txt;loc}, _)
    when txt = reify_all_attrname
      && inside_module
    -> let msg = "[@@@" ^ reify_all_attrname
                 ^ "] must not be used inside submodules (" ^ module_name ^ ")." (* "or in the payload of other attributes." *)
    in raise (Location.Error (Location.error ~loc msg))

  | Exn e ->
    if reify_all
    || has_reify e.pext_attributes
    then ext_exn e else []

  | TypExt e ->
    let attrs = e.ptyext_attributes
    in if reify_all && not (has_dont_reify attrs)
       || has_reify attrs
       then typ_ext e else []
  | _ -> []

let str_proj = function
  | Pstr_type (r, td) -> Type (r, td)
  | Pstr_attribute (a, p) -> Attr (a, p)
  | Pstr_exception e -> Exn e
  | Pstr_typext e -> TypExt e
  | _ -> Other
let sig_proj = function
  | Psig_type (r, td) -> Type (r, td)
  | Psig_attribute (a, p) -> Attr (a, p)
  | _ -> Other

(** main is the mapper that is used on the whole file after
    the initial parameters have been computed.

    - [reify_all] states whether all types should be reified or
      only those that have an attribute.
    - [module_lid] is the [longident] to the current module.
*)

let rec main super reify_all module_lid =
  (** [inside_module] is true inside a submodule, false in
      the top level module. *)
  let inside_module = match module_lid with
    | Lident _ -> false
    | _ -> true
  in
  let module_path = Longident.flatten module_lid in
  let module_name = String.concat "." module_path in
  let str_desc types =
    (* first we extend Generic_core.Ty.t in a mutual block *)
    List.map str_witness types
    @ (* then we extend ty_desc and desc_fun.view *)
    List.concat (List.map (new_desc module_path) types)
  and sig_desc types =
    List.map sig_witness types
  and str_exn e =
    match e.pext_kind with
    | Pext_decl (args, _) ->
      let args = List.map (witness []) (
          match args with
          | Pcstr_tuple cts -> cts
          | Pcstr_record lds -> List.map (fun ld -> ld.pld_type) lds)
      and constr = e.pext_name.txt
      in
      [make_ext_con [%expr Exn] [%pat? Exn] constr args]
    | _ -> [] (* TODO DEAL WITH THIS CASE  *)
  and sig_exn _ = []
  and str_typ_ext e =
    let params = List.map (fun (ct,_) -> var_name ct) e.ptyext_params
    and ty = witness_lid e.ptyext_path.txt in
    List.map (ext_constructor ty params) e.ptyext_constructors
  and sig_typ_ext e = []
  in
  let new_str_items =
    new_items str_desc str_exn str_typ_ext reify_all
      inside_module module_name -< str_proj in
  let new_sig_items =
    new_items sig_desc sig_exn sig_typ_ext reify_all
      inside_module module_name -< sig_proj in

  let rec self =
  { super with
  (** Insert new items for each type that needs to be reified.
   *)
    structure = (fun sub -> function
        | [] -> []
        | s :: ss ->
          let s' = sub.structure_item sub s in
          let ss' = sub.structure sub ss in
          s' :: new_str_items s.pstr_desc @ ss'
      )
  (** keep track of the current module *)
  ; module_binding = (fun sub x ->
      match x.pmb_expr.pmod_desc with
      | Pmod_structure s ->
          let m = main super reify_all
              (Ldot (module_lid, x.pmb_name.txt))
          in super.module_binding m x
      | _ -> super.module_binding self x
    )
  ; signature = (fun sub -> function
      | [] -> []
      | s :: ss ->
        let s' = sub.signature_item sub s in
        let ss' = sub.signature sub ss in
        s' :: new_sig_items s.psig_desc @ ss'
    )
  }
  in self


(* module.ml -> Module
*)
let module_of_file s =
  String.capitalize_ascii (List.hd (String.split_on_char '.' s))

(* This mapper is only applied to the top level sructure or
   signature, it computes the parameters needed by the main
   mapper.
*)
let top =
  let top_module () =
    Lident (module_of_file !Location.input_name)
  and str_reify =
    check_reify_all str_proj -< List.map (fun s -> s.pstr_desc)
  and sig_reify =
    check_reify_all sig_proj -< List.map (fun s -> s.psig_desc)
  in
  { default_mapper with
    structure = (fun sub ss ->
        let m = main default_mapper (str_reify ss) (top_module ())
        in m.structure m ss)
  ; signature = (fun sub ss ->
        let m = main default_mapper (sig_reify ss) (top_module ())
        in m.signature m ss)
  }

let () = register "reify" (fun argv -> top)
