signature TYPES =
sig
	type ty

	datatype bg_type = BgType of ty * bg_type list
	                 | BgEmpty

	val to_string : bg_type -> string

	val insert_binding : ty -> ty -> unit

	val is_subtype : bg_type -> bg_type -> bool
	val is_equal : bg_type -> bg_type -> bool
	val union : bg_type -> bg_type -> bg_type list
	val intersection : bg_type -> bg_type -> bg_type list

	val infer : Bigraph.lope_control Bigraph.bigraph -> bg_type
end

structure Types : TYPES =
struct
	structure B = Bigraph

	type ty = B.ty

	datatype bg_type = BgType of ty * bg_type list
	                 | BgEmpty

	fun to_string BgEmpty = ""
	  | to_string (BgType (t, [])) = B.ty_name t
	  | to_string (BgType (t, l)) = B.ty_name t ^ "{" ^ (String.concatWith "," (map to_string l)) ^ "}"

	fun insert_binding (lhs:ty) (rhs:ty) = ()

	fun subst_type (B.TyCon (x, B.TyName "site")) (B.TyCon (y, B.TyName "site")) = true
	  | subst_type (B.TyName x) (B.TyName y) = x = y
	  | subst_type _ _ = false

	fun is_subtype (BgType (t1, [])) (BgType (t2, ch2)) = 
		if not (subst_type t1 t2) then false
		  else true
      | is_subtype (BgType (t1, h::t)) (b2 as (BgType (t2, ch2))) =
	    (List.exists (is_subtype h) ch2) andalso 
			(is_subtype (BgType (t1, t)) b2)
	
	fun is_equal _ _ = false

	fun union _ _ = []

	fun intersection _ _ = []

	fun infer _ = BgEmpty
end
