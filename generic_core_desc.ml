open Generic_core
open Generic_util
open Ty.T

module Con = struct
  let local_invalid_arg str = invalid_arg (__MODULE__ ^ str)

  (* ** Caveat

    A constructor C of (a * b) is treated differently as
    constructor C of a * b.

    We must therefore represent them differently:
    the first one will have : { args = p1 (Pair a b) ; ...}
    while the second one will have: { args = p2 a b ; ...}
   *)

  type ('a, 'v) desc =
    { name  : string
    ; args  : 'a Product.t (* we should rename desc.args to desc.args_ty *)
    ; embed : 'a -> 'v (* this function should NEVER inspect 'a or the library would cause a segfault. *)
    ; proj  : 'v -> 'a option
    }

  type 'v con =
    | Con : ('a, 'v) desc -> 'v con

  type 'v t = 'v con

  let name (Con c) = c.name

  let make name args embed proj = Con {name; args; embed; proj}
  let c0 n x = make n Product.p0 (fun () -> x)
                      (fun y -> if x == y then Some () else None)

  (* PRIVATE! and DANGEROUS
  val dummy : 'v con -> 'v
  We feed a product of null values to the embed function of a constructor.
  The value may cause SEGFAULT if inspected.
   *)

  let dummy (Con c) =
    let n = Product.length c.args in
    let dummy_args = Obj.magic (Listx.replicate n 0)
    in Obj.repr (c.embed dummy_args)

  (* DANGEROUS: nothing enforces the product to correspond to the fields. *)
  (* let cn n a e = make n a e *)
  (*                       (fun y -> *)
  (*                         if Objx.con_id y = Objx.con_id (Con.dummy a e) *)
  (*                         then Some (Obj.obj (Objx.listify (Obj.repr y))) *)
  (*                         else None) *)

  (* PRIVATE
    [set_fields v i (ts,xs)]
    sets the fields of [v] from field index [i] onwards with the values [xs].
    No check is performed on the size of [v], nor the fact that it should be a block.
   *)
  let rec set_fields : type a . Obj.t -> int -> a Product.t * a -> unit =
    fun v i -> let open Product.T in function
            | Nil, () -> ()
            | Cons (t,ts), (x, xs) ->
               begin
                 Obj.set_field v i (Obj.repr x);
                 set_fields v (i+1) (ts, xs)
               end

  (* PRIVATE embed_variant : int -> 'a product -> 'a -> Obj.t
     creates a constructor value using the tag and fields *)
  let embed_variant tg ts xs =
    let open Obj in
    let n = Product.length ts in
    if n == 0 then Obj.repr tg
    else let v = new_block tg n in
         begin set_fields v 0 (ts,xs); v end

  (* The arity of a constructor *)
  let arity (Con d) = Product.length (d.args)

  (* PRIVATE
   "ctag" computes the tag of the values corresponding to a
   constructor description. It gives the "embed" function
   dummy arguments, which are all null values. This
   is in general an invalid ocaml value, but the only thing
   we do with it is access its tag and size. So it is SAFE (as long as
   the embed function doesn't inspect the product!)
   *)
  let ctag c =
    let v = dummy c
    in if Objx.is_con v && Objx.gsize v == arity c
       then snd (Objx.con_id v)
       else local_invalid_arg ("cons: inconsistent description of constructor ("^name c^")")

  (* constructor application *)
  type 'v conap =
    | Conap : ('a, 'v) desc * 'a -> 'v conap

  (* PRIVATE. PARTIAL. raise Invalid_argument *)
  let partial_conap (Con c) x = match c.proj x with
    | Some y -> Conap (c,y)
    | None -> local_invalid_arg "partial_conap: incorrect constructor"

  let con (Conap (c,x)) = Con c
  let subterms_prod (Conap (c,x)) = Product.Dynprod (c.args, x)
  let subterms x = Product.list_of_dynprod (subterms_prod x)
end (* Con *)

module Variant = struct
  let local_invalid_arg str = invalid_arg (__MODULE__ ^ str)

  (* * Variants

     A variant is described as a set of constructors.  each
     constructor is described by its name, the types of its
     arguments given as a nested product, a function to embed a
     value of the nested product to the variant type, and a
     partial projection function that only succeeds when the
     value has the same constructor.
   *)

  (* "cons": ABSTRACT type, we store constant constructors and
     non-constant constructors separately, and ordered by tag *)
  type 'v cons = 'v Con.t array * 'v Con.t array (* constant * non-constant *)

  (* public functions on "cons" *)
  let cst_len (c0,_)  = Array.length c0
  let cst_get (c0,_)  = Array.get c0
  let cst (c0,_)      = Array.to_list c0
  let ncst_len (_,cn) = Array.length cn
  let ncst_get (_,cn) = Array.get cn
  let ncst (_,cn)     = Array.to_list cn
  let list_of_cons cs = cst cs @ ncst cs

  (* PRIVATE

     An empty constructor is needed for completing the constructor
     list of gadts.  Ocaml type inference would make it
     monomorphic if we define it as a constant.  But defining it
     as a function allows the type checker to infer a polymorphic
     type.
  *)
  let empty_con () = Con.Con
                       { name = "empty_con"
                       ; args = Product.p1 Sum.Empty
                       ; embed = (fun _ -> assert false) (* this is the empty function *)
                       ; proj = (fun _ -> None)
                       }

  (* PRIVATE
   Completes a list of constructors so that all tags are
   covered.  This is for gadts: we assume the constructors
   that weren't given correspond to empty cases.
   we ASSUME the input list is sorted by tags.
   *)
  let rec complete c = function
    | [] -> []
    | [(_,x)] -> [x]
    | (n,x) :: ((m,_) :: _ as l) ->
       x :: Listx.replicate (m - n - 1) c
       @ complete c l


  (* PRIVATE
   "cons_array" takes a list of constructor descriptions,
   sort them by tags, completes it with empty constructors for
   the missing tags, and make an array out of it, indexed by tags.
   *)
  let cons_array cs =
    let tagged = List.map (fun c -> (Con.ctag c, c)) cs in
    let sorted = List.sort (Fun.opon compare fst) tagged in
    let c = empty_con () in
    let completed = List.tl (complete c ((-1, c) :: sorted)) in
    Array.of_list completed

  (* "cons" construct a proper value of type "cons", ensuring
   the invariant holds: we check that each constant constructor
   is represented with an immediate value,
   that each non-constant constructor is represented with a block.
   That all the tags are used in increasing order.
   *)
  let cons cs =
    let (c0,cn) = List.partition (fun c -> Con.arity c == 0) cs
    in (cons_array c0, cons_array cn)

  type 'v variant =
    { name : string
    ; module_path : string list
    ; cons : 'v cons
    }

  type 'v t = 'v variant

  (* Find the constructor of a value, O(1) *)
  let conap : 'v cons -> 'v -> 'v Con.conap
    = fun (c0,cn) x ->
    let (cst,k) = Objx.con_id x in
    let c = try (if cst then c0 else cn).(k)
            with _ -> local_invalid_arg "conap: no matching constructor"
    in Con.partial_conap c x
end (* Variant *)

module Ext = struct
  module H = Hashtbl

  let local_invalid_arg str = invalid_arg (__MODULE__ ^ str)

  (* * Variants extensibles

     Chaque constructor est traduit par un objet dont le
     premier champ est le nom et le second est un identifiant
     entier.  L'identifiant d'objet n'est pas préservé par la
     sérialisation: un nouvel identifiant est créé lors de la
     désérialisation. Il faut donc restaurer l'identifiant originel.

     L'application d'un constructeur est traduite par un bloc de tag 0
     dont le premier champ est l'objet correspondant au constructeur
     et le reste des champ correspond aux arguments.

     Pour spécifier un type extensible, nous utilisons un type abstrait
     "extensible", sur lequel nous définissons plusieurs fonctions permettant de:
      - ajouter un nouveau constructeur,
      - parcourir la liste de tous les constructeurs,
      - obtenir le constructeur correspondant à une valeur
   *)

  (*  A polymorphic function that computes the constructor
    representation for any type.

    In practice, that function will only be called on the
    type given by the field [ty] of the record
    "extensible". This allows parametric extensible types to
    be dealt with.
   *)
  type con = { con : 'a . 'a ty -> 'a Con.t}

  (* ABSTRACT [cons] is indexed by the unique name of
     extensible constructors and maps to the constructor
     object and a polymorphic function that computes the
     constructor description when given the type of the
     extensible variant.
   *)
  type cons = (string, (Obj.t * con)) H.t
  type 'a extensible = { name : string
                       ; ty : 'a ty
                       ; cons : cons}
  type 'a t = 'a extensible
  let create () = H.create 10


  (* STATEFUL
     @raise Invalid_argument if the value is not of an extensible type *)
  let add_con ext c =
    let k = Objx.ext_con (Con.dummy (c.con ext.ty)) in
    let n = Objx.ext_con_name k in
    H.add ext.cons n (k, c)

  (* STATEFUL
   * Careful: c.con ext.ty may throw an exception, but we catch it.
   *)
  let fold ext cons nil =
    H.fold (fun _ (k,c) a -> Option.unopt_try a (Fun.flip cons a) (lazy (c.con ext.ty)))
           ext.cons nil

  (* STATEFUL *)
  let iter ext f = fold ext (fun c _ -> f c) ()

  (* PRIVATE. STATEFUL. PARTIAL.
  @raise Invalid_argument if the value is not of an extensible type.
  @raise Not_found if the constructor hasn't been added previously or
         if the type doesn't correspond.
   *)
  let find : 'a extensible -> 'a -> Obj.t * 'a Con.t
    = fun ext v ->
    try let (k,{con}) = H.find ext.cons (Objx.ext_con_name (Obj.repr v))
        in (k, con ext.ty)
    with Match_failure _ -> raise Not_found

  (* Replace an extensible constructor with the one stored in
   the hashtable this is necessary for pattern matching to
   work on unmarshalled data. *)
  let fix ext v =
    let rv = Obj.repr v in
    let k = fst (find ext v) in
    if Objx.is_ext_con rv then Obj.obj k (* [v] is a constant constructor. *)
    else if Objx.is_ext_con (Obj.field rv 0) then
      let rw = Obj.dup rv in
      Obj.set_field rw 0 k;
      Obj.obj rw
    else local_invalid_arg "fix"

  let con ext v = snd (find ext v)

  (* STATEFUL. PARTIAL. raise Not_found, Invalid_argument, Type_pattern_overwrite
     Find the constructor of a value, O(1),
     @raise Invalid_argument if the type is not extensible
     @raise Not_found if the constructor hasn't been added previously.
   *)
  let conap ext x =
    Con.partial_conap (con ext x) x
end (* Ext *)

module Poly = struct
  let local_invalid_arg str = invalid_arg (__MODULE__ ^ str)

  (* * Polymorphic Variants

     We use again [Con.desc] and [Con.t] but a poly_variant
     uses a different abstraction from [cons], due to the
     difference in constructor representation in memory.
   *)

  (* "t": ABSTRACT type, each constructor is stored
   at the key obtained with hash_variant (cf manual 19.3.6)
   There are two choices for the implementation: either using
   Btype.hash_variant on the "name" field of the "con" value
   (requiring compiler-libs) or like with regular variants,
   generate a value and inspect its tag.
   *)
  type 'v t = (int, 'v Con.t) Hashtbl.t

  (* We could even check that the name is correct with:
  {[
     let hash c =
       let h = Objx.poly_hash (Con.dummy c) in
       if h == Btype.hash_variant (Con.name name) then h
       else raise (Invalid_argument ("hash: the constructor name is incorrect (" ^ Con.name c ^")"))
  ]}
  *)

  let hash c = Objx.poly_hash (Con.dummy c)

  let find h k = Hashtbl.find h k

  let poly_variant cs =
    let h = Hashtbl.create (List.length cs) in
    let insert_con c =
      Hashtbl.add h (hash c) c
    in begin List.iter insert_con cs; h end


  (* PRIVATE
     [embed_poly_variant : int -> 'a product -> 'a -> Obj.t]
     creates a constructor value using the hash and fields
   *)
  let embed_poly_variant (type a) h (ts : a Product.t) (xs : a) =
    let open Product.T in
    let n = Product.length ts in
    if n == 0 then Obj.repr h
    else let v = Obj.new_block 0 2 in
         begin
           Obj.set_field v 0 (Obj.repr h);
           begin match ts , xs with
           | Cons (_, Nil), (x,()) -> Obj.set_field v 1 (Obj.repr x)
           | _ -> let w = Obj.new_block 0 n in
                  Con.set_fields w 0 (ts,xs);
           end;
           v;
         end

  let conap h x =
    let c = try Hashtbl.find h (Objx.poly_hash x)
            with Not_found -> local_invalid_arg "conap: no matching constructor"
    in Con.partial_conap c x

  type _ ty += Poly_variant : 'v t -> 'v ty
end (* Poly *)

(** Generic representation of record fields. *)
module Field = struct
  module T = struct
    type ('a, 'r) field = {
      name : string;
      ty : 'a ty;
      (*  bound : int;*)
      set : ('r -> 'a -> unit) option;
    }
  end

  type ('a,'r) t = ('a,'r) T.field = {
    name : string; (** name of the field *)
    ty : 'a ty; (** type of the field *)
    set : ('r -> 'a -> unit) option; (** procedure for updating the field if it is mutable *)
  }

  let is_mutable fd = fd.set != None
end

module Fields = struct
  module T = struct
    type ('p, 'r) fields =
      | Nil : (unit, 'r) fields
      | Cons : ('t, 'r) Field.t * ('ts, 'r) fields -> ('t * 'ts, 'r) fields
  end

  type ('p, 'r) t =  ('p,'r) T.fields =
    | Nil : (unit, 'r) t
    | Cons : ('t, 'r) Field.t * ('ts, 'r) t -> ('t * 'ts, 'r) t

  let rec product : type p . (p,'r) t -> p Product.t
    = function
      | T.Nil -> Nil
      | T.Cons (f,fs) -> Cons (f.ty, product fs)

  let rec types_of_mutable_fields : type p . (p, 'r) t -> Ty.ty' list
    = function
      | T.Nil -> []
      | T.Cons (f, fs) ->
        if Field.is_mutable f then
          E f.ty :: types_of_mutable_fields fs
        else types_of_mutable_fields fs
end

module Record = struct
  let local_invalid_arg str = invalid_arg (__MODULE__ ^ str)
  module T = struct
    type ('p,'r) record =
      { name : string
      ; module_path : string list
      ; fields : ('p, 'r) Fields.t
      ; iso : ('p, 'r) Fun.iso
      }
  end
  type ('p,'r) t = ('p,'r) T.record =
    { name : string
    ; module_path : string list
    ; fields : ('p, 'r) Fields.t
    ; iso : ('p, 'r) Fun.iso
    }
  let product r = Fields.product r.fields
  let types_of_mutable_fields r = Fields.types_of_mutable_fields r.fields
  let tuple r x = (product r, r.iso.bck x)
end (* Record *)

module Method = struct
  (* indices of bound variables are 0..bound - 1 *)
  type ('a, 'c) desc = {
    name : string;
    send : 'c -> 'a;
    bound : int; (* number of bound variables (universally quantified) *)
    ty : 'a ty;
  }
  type 'c t = Method : ('a, 'c) desc -> 'c t
end

module Object = struct
  type 'c t = 'c Method.t list
  type _ ty += T : 'a t -> 'a ty
end

module Class = struct
  type 'c t = { name : string; methods : 'c Object.t; }
end

module Array = struct
  (* Minimal interface for mutable arrays *)
  module type intf = sig
    type t
    type elt
    val length : t -> int
    val get : t -> int -> elt
    val set : t -> int -> elt -> unit
    val init : int -> (int -> elt) -> t
    val max_length : int
  end
 end (* Array *)
module Custom = struct
  module T = struct
    type 'a custom =
      { name : string
      ; module_path : string list
      ; identifier : string
      }
  end
  type 'a t = 'a T.custom
end (* Custom *)

module T = struct
  type 'a desc =
    | Array      : 'e ty * (module Array.intf with type t = 'a and type elt = 'e) -> 'a desc
    | Product    : 'ts Product.t * ('ts , 'p) Fun.iso -> 'p desc
    | Record     : ('p,'r) Record.t -> 'r desc
    | Variant    : 'v Variant.t -> 'v desc
    | Extensible : 'a Ext.t -> 'a desc
    | Custom     : 'a Custom.t -> 'a desc
    | Class      : 'c Class.t  -> 'c desc
    | Synonym    : 'b ty * ('a,'b) Equal.t -> 'a desc
    | Abstract   : 'a desc
    | NoDesc     : 'a desc
end (* T *)

include T
type 'a t = 'a T.desc
