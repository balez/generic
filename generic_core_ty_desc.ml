(* Representation de chaque constructor du type extensible
  Ty, La représentation est nécessairement extensible
  également, on l'implemente avec une fonction extensible.

NOTE: Desc_fun depends on Extensible which depends on Ty_desc.
This is why we had to provide those functions in a separate module.
*)

open Generic_core

let cons = Desc.Ext.create()
let ext t = {Desc.Ext.name = "Ty"; ty = Ty.Ty t; cons}

(* @raise Type_pattern_overwrite
   @raise Not_found *)
let conap t = Desc.Ext.conap (ext t) t

(* @raise Not_found
   @raise Invalid_argument *)
let con t = Desc.Con.con (conap t)

let subterms t = Desc.Con.subterms (conap t)
