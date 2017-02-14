open Generic_core
open Generic_util

open App.T
open Product.Build
open Desc.Fields.Build

type 'a ty = 'a Ty.t

let local_invalid_arg s = invalid_arg (__MODULE__ ^ "." ^ s)

let cons = Desc.Variant.cons
let c0 = Desc.Con.c0
let cn = Desc.Con.make

(***************************************************)
let cons_option t = cons
  [ c0 "None" None
  ; cn "Some" (f1 t)
           (fun (x,()) -> Some x)
           (function Some x -> Some (x,()) | _ -> None)
  ]
let cons_list t = cons
  [ c0 "[]" []
  ; cn "::" (f2 t (Ty.List t))
           (fun (x,(xs,())) -> x :: xs)
           (function x :: xs -> Some (x,(xs,())) | _ -> None)
  ]
(* Using the (unsafe) magic trick
let cons_list t = consM (fun {m} ->
  [ cM0 "[]" []
  ; cM2 "::" (m :: m) (t, List t)
  ])
 *)
let cons_bool = cons
  [ c0 "false" false
  ; c0 "true" true
  ]

(***************************************************)
type tag = Tag
type (_,_) app += App : 'a Desc.t -> ('a, tag) app

let unapp = function
  | App x -> x
  | _ -> local_invalid_arg "unapp"

type desc_fun =
  { f : 'a . 'a ty -> 'a Desc.t }

let desc_closure = Extensible.create "Generic_core_desc_fun.view" (* private *)

let view t = unapp (desc_closure.f t)

let ext t f = desc_closure.ext t { f = fun t -> App (f.f t) }

(**************************************************
 * Arrays *)

let array_desc : type a . a ty -> a array Desc.t
  = fun t ->
    let max_array_length = if Ty.eq t Ty.Float
      then Sys.max_array_length / 2
      else Sys.max_array_length
    in Desc.Array (t, (module struct
        type t = a array
        type elt = a
        let get = Array.get
        let set = Array.set
        let init = Array.init
        let length = Array.length
        let max_length = max_array_length
      end))
let string_desc =
  Desc.Array (Ty.Char, (module struct
      type t = string
      type elt = char
      let get = String.get
      let set x = local_invalid_arg "String.set on a string"
      let init = String.init
      let length = String.length
      let max_length = Sys.max_string_length
    end))

let bytes_desc =
  Desc.Array (Ty.Char, (module struct
      type t = bytes
      type elt = char
      let get    = Bytes.get
      let set    = Bytes.set
      let init   = Bytes.init
      let length = Bytes.length
      let max_length = Sys.max_string_length
    end))

(**************************************************)
let () =
  begin
    ext Ty.Any { f = fun _ -> NoDesc }; (* in particular, [Fun(a,b)] *)

    ext Ty.pair
      { f = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Pair (a,b) -> Product (p2 a b
                                   , { fwd = (fun (x,(y,())) -> (x,y))
                                     ; bck = (fun (x,y) -> (x,(y,())))})
           | _ -> assert false : a Desc.t)
      };
    ext (Ty.Triple (Ty.Any, Ty.Any, Ty.Any))
      { f = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Triple (a,b,c) -> Product (p3 a b c
                                   , { fwd = (fun (x,(y,(z,()))) -> (x,y,z))
                                     ; bck = (fun (x,y,z) -> (x,(y,(z,()))))})
           | _ -> assert false : a Desc.t)
      };
    ext (Ty.Quadruple (Ty.Any, Ty.Any, Ty.Any, Ty.Any))
      { f = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Quadruple (t1,t2,t3,t4) -> Product (p4 t1 t2 t3 t4
                                   , { fwd = (fun (x1,(x2,(x3,(x4,())))) -> (x1,x2,x3,x4))
                                     ; bck = (fun (x1,x2,x3,x4) -> (x1,(x2,(x3,(x4,())))))})
           | _ -> assert false : a Desc.t)
      };
    ext (Ty.Quintuple (Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any))
      { f = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Quintuple (t1,t2,t3,t4,t5) -> Product (p5 t1 t2 t3 t4 t5
                                   , { fwd = (fun (x1,(x2,(x3,(x4,(x5,()))))) -> (x1,x2,x3,x4,x5))
                                     ; bck = (fun (x1,x2,x3,x4,x5) -> (x1,(x2,(x3,(x4,(x5,()))))))})
           | _ -> assert false : a Desc.t)
      };
    ext (Ty.Sextuple (Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any))
      { f = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Sextuple (t1,t2,t3,t4,t5,t6) -> Product (p6 t1 t2 t3 t4 t5 t6
                                   , { fwd = (fun (x1,(x2,(x3,(x4,(x5,(x6,())))))) -> (x1,x2,x3,x4,x5,x6))
                                     ; bck = (fun (x1,x2,x3,x4,x5,x6) -> (x1,(x2,(x3,(x4,(x5,(x6,())))))))})
           | _ -> assert false : a Desc.t)
      };
    ext (Ty.Septuple (Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any))
      { f = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Septuple (t1,t2,t3,t4,t5,t6,t7) -> Product (p7 t1 t2 t3 t4 t5 t6 t7
                                   , { fwd = (fun (x1,(x2,(x3,(x4,(x5,(x6,(x7,()))))))) -> (x1,x2,x3,x4,x5,x6,x7))
                                     ; bck = (fun (x1,x2,x3,x4,x5,x6,x7) -> (x1,(x2,(x3,(x4,(x5,(x6,(x7,()))))))))})
           | _ -> assert false : a Desc.t)
      };
    ext (Ty.Octuple (Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any))
      { f = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Octuple (t1,t2,t3,t4,t5,t6,t7,t8) -> Product (p8 t1 t2 t3 t4 t5 t6 t7 t8
                                   , { fwd = (fun (x1,(x2,(x3,(x4,(x5,(x6,(x7,(x8,())))))))) -> (x1,x2,x3,x4,x5,x6,x7,x8))
                                     ; bck = (fun (x1,x2,x3,x4,x5,x6,x7,x8) -> (x1,(x2,(x3,(x4,(x5,(x6,(x7,(x8,())))))))))})
           | _ -> assert false : a Desc.t)
      };
    ext (Ty.Nonuple (Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any))
      { f = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Nonuple (t1,t2,t3,t4,t5,t6,t7,t8,t9) -> Product (p9 t1 t2 t3 t4 t5 t6 t7 t8 t9
                                   , { fwd = (fun (x1,(x2,(x3,(x4,(x5,(x6,(x7,(x8,(x9,()))))))))) -> (x1,x2,x3,x4,x5,x6,x7,x8,x9))
                                     ; bck = (fun (x1,x2,x3,x4,x5,x6,x7,x8,x9) -> (x1,(x2,(x3,(x4,(x5,(x6,(x7,(x8,(x9,()))))))))))})
           | _ -> assert false : a Desc.t)
      };
    ext (Ty.Decuple (Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any, Ty.Any))
      { f = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Decuple (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) -> Product (p10 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10
                                   , { fwd = (fun (x1,(x2,(x3,(x4,(x5,(x6,(x7,(x8,(x9,(x10,())))))))))) -> (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
                                     ; bck = (fun (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> (x1,(x2,(x3,(x4,(x5,(x6,(x7,(x8,(x9,(x10,())))))))))))})
           | _ -> assert false : a Desc.t)
      };
    ext Ty.String
      { f = fun (type a) (ty : a ty) -> match ty with
           | Ty.String -> (string_desc : a Desc.t)
           | _ -> assert false
      };
    ext Ty.Bytes
      { f = fun (type a) (ty : a ty) -> match ty with
           | Ty.Bytes -> (bytes_desc : a Desc.t)
           | _ -> assert false
      };
    ext Ty.array
      { f = fun (type a) (ty : a ty) -> match ty with
           | Ty.Array t -> (array_desc t : a Desc.t)
           | _ -> assert false
      };
    ext Ty.Bool
      { f = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Bool -> Variant {name = "bool"; module_path = ["Core"]; cons = cons_bool}
           | _ -> assert false : a Desc.t)
      };

    ext Ty.option
      { f = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Option a -> Variant {name = "option"; module_path = ["Core"]; cons = cons_option a}
           | _ -> assert false : a Desc.t)
      };

    ext Ty.list
      { f = fun (type a) (ty : a ty) -> (match ty with
           | Ty.List a -> Variant {name = "list"; module_path = ["Core"]; cons = cons_list a}
           | _ -> assert false : a Desc.t)
      };

    ext Ty.Int32
      { f = fun (type a) (ty : a ty) -> match ty with
           | Ty.Int32 -> Desc.Custom { name = "int32"; module_path = ["Core"]
                             ; identifier = Objx.custom_identifier (Int32.of_int 0)
                             }
           | _ -> assert false
      };
    ext Ty.Int64
      { f = fun (type a) (ty : a ty) -> match ty with
           | Ty.Int64 -> Desc.Custom { name = "int64"; module_path = ["Core"]
                             ; identifier = Objx.custom_identifier (Int64.of_int 0)
                             }
           | _ -> assert false
      };
    ext Ty.Nativeint
      { f = fun (type a) (ty : a ty) -> match ty with
           | Ty.Nativeint -> Desc.Custom { name = "nativeint"; module_path = ["Core"]
                                 ; identifier = Objx.custom_identifier (Nativeint.of_int 0)
                                 }
           | _ -> assert false
      };

    ext (Ty.Ref Ty.Any)
      { f = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ref t ->
             Record { name = "ref"
                    ; module_path = ["Core"]
                    ; fields = Cons ( { name = "contents"
                                      ; ty = t
                                      (*; bound = 0 *)
                                      ; set = Some (fun r x -> r := x)
                                      }
                                    , Nil
                                    )
                    ; iso = { fwd = (fun (x,()) -> ref x)
                            ; bck = (fun r -> (!r, ()))
                            }
                    }
           | _ -> assert false : a Desc.t)
      };
  end

(*************************************************
 * Extensible types are declared with this function
 *)

let ext_register typat name =
  let cons = Desc.Ext.create() in (* IMPORTANT: the table is created outside the function *)
    ext (Ty.conpat typat)
    { f = fun (type a) (ty : a ty)
          -> (Extensible {name;ty; cons}
              : a Desc.t)
    }

let extensible ty =
  match view ty with
  | Desc.Extensible x -> x
  | _ -> local_invalid_arg "extensible"

(* ext_add_con t {con = f} --> f must match all (x : 'a ty) such that
     Generic_core_patterns.leq x t
 *)
let ext_add_con ty =
  Desc.Ext.add_con (extensible ty)
let ext_iter ty =
  Desc.Ext.iter (extensible ty)
let ext_fold ty =
  Desc.Ext.fold (extensible ty)
let ext_conap ty =
  Desc.Ext.conap (extensible ty)

let () =
  begin
    ext (Ty.Ty Ty.Any)
    { f = fun (type a) (ty : a ty) -> (match ty with
          | Ty.Ty t -> Extensible (Ty_desc.ext t)
          | _ -> assert false : a Desc.t) };

    ext_add_con (Ty.Ty Ty.Any)
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty Ty.Any -> c0 "Any" Ty.Any
           | _ -> assert false : a Desc.Con.t) };

    ext_add_con (Ty.Ty Ty.Int32)
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty Ty.Int32 -> c0 "Int32" Ty.Int32
           | _ -> assert false : a Desc.Con.t) };

    ext_add_con (Ty.Ty Ty.Int64)
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty Ty.Int64 -> c0 "Int64" Ty.Int64
           | _ -> assert false : a Desc.Con.t)
      };
    ext_add_con (Ty.Ty Ty.Nativeint)
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty Ty.Nativeint -> c0 "Nativeint" Ty.Nativeint
           | _ -> assert false : a Desc.Con.t) };

    ext_add_con (Ty.Ty Ty.Exn)
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty Ty.Exn -> c0 "Exn" Ty.Exn
           | _ -> assert false : a Desc.Con.t) };
    ext_add_con (Ty.Ty Ty.Bool)
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty Ty.Bool -> c0 "Bool" Ty.Bool
           | _ -> assert false : a Desc.Con.t) };
    ext_add_con (Ty.Ty Ty.Int)
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty Ty.Int -> c0 "Int" Ty.Int
           | _ -> assert false : a Desc.Con.t) };
    ext_add_con (Ty.Ty Ty.Float)
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty Ty.Float -> c0 "Float" Ty.Float
           | _ -> assert false : a Desc.Con.t) };

    ext_add_con (Ty.Ty Ty.Char)
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty Ty.Char -> c0 "Char" Ty.Char
           | _ -> assert false : a Desc.Con.t) };

    ext_add_con (Ty.Ty Ty.Bytes)
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty Ty.Bytes -> c0 "Bytes" Ty.Bytes
           | _ -> assert false : a Desc.Con.t) };

    ext_add_con (Ty.Ty Ty.String)
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty Ty.String -> c0 "String" Ty.String
           | _ -> assert false : a Desc.Con.t) };

    ext_add_con (Ty.Ty Ty.option)
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty (Ty.Option x)
             -> cn "Option" (f1 (Ty.Ty x))
                  (fun (t,()) -> Ty.Option t)
                  (function | Ty.Option t -> Some (t,()) | _ -> None)
           | _ -> assert false : a Desc.Con.t)};

    ext_add_con (Ty.Ty Ty.list)
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty (Ty.List x)
             -> cn "List" (f1 (Ty.Ty x))
                  (fun (t,()) -> Ty.List t)
                  (function | Ty.List t -> Some (t,()) | _ -> None)
           | _ -> assert false : a Desc.Con.t)};

    ext_add_con (Ty.Ty (Ty.Array Ty.Any))
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty (Ty.Array x)
             -> cn "Array" (f1 (Ty.Ty x))
                  (fun (t,()) -> Ty.Array t)
                  (function | Ty.Array t -> Some (t,()) | _ -> None)
           | _ -> assert false : a Desc.Con.t)};

    ext_add_con (Ty.Ty (Ty.Ref Ty.Any))
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty (Ty.Ref x)
             -> cn "Ref" (f1 (Ty.Ty x))
                  (fun (t,()) -> Ty.Ref t)
                  (function | Ty.Ref t -> Some (t,()) | _ -> None)
           | _ -> assert false : a Desc.Con.t)};

    ext_add_con (Ty.Ty (Ty.Lazy Ty.Any))
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty (Ty.Lazy x)
             -> cn "Ty.Lazy" (f1 (Ty.Ty x))
                  (fun (t,()) -> Ty.Lazy t)
                  (function | Ty.Lazy t -> Some (t,()) | _ -> None)
           | _ -> assert false : a Desc.Con.t)};

    ext_add_con (Ty.Ty (Ty.Ty Ty.Any))
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty (Ty.Ty x)
             -> cn "Ty" (f1 (Ty.Ty x))
                  (fun (t,()) -> Ty.Ty t)
                  (function | Ty.Ty t -> Some (t,()) | _ -> None)
           | _ -> assert false : a Desc.Con.t)};

    ext_add_con (Ty.Ty Ty.pair)
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Ty (Ty.Pair (x,y))
             -> cn "Pair" (f2 (Ty.Ty x) (Ty.Ty y))
                  (fun (x,(y,())) -> Ty.Pair(x,y))
                  (function | Ty.Pair (x,y) -> Some (x,(y,())) | _ -> None)
           | _ -> assert false : a Desc.Con.t)};
  end


let exn_add_con (c : exn Desc.Con.t) =
    ext_add_con Ty.Exn
      {con = fun (type a) (ty : a ty) -> (match ty with
           | Ty.Exn -> c
           | _ -> assert false : a Desc.Con.t) }

let () =
  begin
    ext_register Ty.Exn "exn";
    exn_add_con (c0 "Not_found" Not_found);
    exn_add_con (cn "Invalid_argument" (f1 Ty.String)
                        (fun (x,()) -> Invalid_argument x)
                        (function Invalid_argument x -> Some (x,()) | _ -> None))
  end
