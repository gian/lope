signature TYPES =
sig
	type ty

	val insert_binding : ty -> ty -> unit

	val is_subtype : ty -> ty -> bool
	val is_equal : ty -> ty -> bool
	val union : ty -> ty -> ty list
	val intersection : ty -> ty -> ty list

	val ty_var_replace : Bigraph.lope_control Bigraph.bigraph -> Bigraph.lope_control Bigraph.bigraph
	val infer : Bigraph.lope_control Bigraph.bigraph -> ty 
end

structure Types : TYPES =
struct
	structure B = Bigraph

	type ty = B.ty

	fun insert_binding (lhs:ty) (rhs:ty) = ()

	fun subst_type (B.TyCon (x, B.TyName "site")) (B.TyCon (y, B.TyName "site")) = true
	  | subst_type (B.TyName x) (B.TyName y) = x = y
	  | subst_type _ _ = false

(*	fun is_subtype (BgType (t1, [])) (BgType (t2, ch2)) = 
		if not (subst_type t1 t2) then false
		  else true
      | is_subtype (BgType (t1, h::t)) (b2 as (BgType (t2, ch2))) =
	    (List.exists (is_subtype h) ch2) andalso 
			(is_subtype (BgType (t1, t)) b2) *)

	fun is_subtype k l = false
	
	fun is_equal _ _ = false

	fun union _ _ = []

	fun intersection _ _ = []

	fun ty_var_replace k = k

	fun infer _ = B.TyUnknown 
end
