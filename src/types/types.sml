signature TYPES =
sig
	type ty

	val insert_binding : ty -> ty -> unit

	val is_subtype : ty -> ty -> bool
	val is_equal : ty -> ty -> bool
	val union : ty -> ty -> ty list
	val intersection : ty -> ty -> ty list

	val reset_ty_var : unit -> unit

	val ty_var_replace : Bigraph.lope_control Bigraph.bigraph -> Bigraph.lope_control Bigraph.bigraph
	val constrain : Bigraph.lope_control Bigraph.bigraph -> Bigraph.ty
	val get_constraints : unit -> (Bigraph.ty * Bigraph.ty list) list
	val infer : Bigraph.lope_control Bigraph.bigraph -> ty 
end

structure Types : TYPES =
struct
	structure B = Bigraph

	type ty = B.ty

	val tyvar = ref 0

	fun insert_binding (lhs:ty) (rhs:ty) = ()

	fun subst_type (B.TyCon (x, B.TyName "site")) (B.TyCon (y, B.TyName "site")) = true
	  | subst_type (B.TyName x) (B.TyName y) = x = y
	  | subst_type _ _ = false

	fun is_subtype (B.TyComp (t1, [])) (B.TyComp (t2, ch2)) = 
		if not (subst_type t1 t2) then false
		  else true
      | is_subtype (B.TyComp (t1, h::t)) (b2 as (B.TyComp (t2, ch2))) =
	    (List.exists (is_subtype h) ch2) andalso 
			(is_subtype (B.TyComp (t1, t)) b2) 
	  | is_subtype (t1 as (B.TyName _)) (t2 as (B.TyName _)) = subst_type t1 t2
	  | is_subtype (t1 as (B.TyName _)) (B.TyComp (t2, _)) = subst_type t1 t2
	  | is_subtype (B.TyArrow (t1, t2)) (B.TyArrow (t3, t4)) = is_subtype t1 t3 andalso is_subtype t2 t4
	  | is_subtype j k = false

	fun is_equal _ _ = false

	fun union _ _ = []

	fun intersection _ _ = []

	fun fresh_ty_var () = let val k = !tyvar in (tyvar := !tyvar + 1; B.TyVar k) end 
	fun reset_ty_var () = tyvar := 0

	fun ty_replace B.TyUnknown = fresh_ty_var ()
	  | ty_replace (B.TyArrow(a,b)) = B.TyArrow (ty_replace a, ty_replace b)
	  | ty_replace (B.TyCon(a,b)) = B.TyCon (ty_replace a, ty_replace b)
	  | ty_replace (B.TyTuple k) = B.TyTuple (map ty_replace k)
	  | ty_replace (B.TyComp (t,l)) = B.TyComp (ty_replace t, map ty_replace l)
	  | ty_replace b = b 

	fun ty_var_replace_cntrl (B.NodeControl (l,t)) = B.NodeControl (l, ty_replace t)
	  | ty_var_replace_cntrl (B.AnonControl t) = B.AnonControl (ty_replace t)
	  | ty_var_replace_cntrl (B.ParamNodeControl (l,t,lst)) = B.ParamNodeControl (l, ty_replace t, map (fn (l,t') => (l, ty_replace t')) lst)
	  | ty_var_replace_cntrl (B.SiteControl (l,t)) = B.SiteControl (l, ty_replace t)
	  | ty_var_replace_cntrl (B.WildControl t) = B.WildControl (ty_replace t)
	  | ty_var_replace_cntrl (B.DataControl d) = B.DataControl (case d of B.VarData (l,t) => B.VarData (l,ty_replace t) | p => p)

	val constr : (B.ty * B.ty list) list ref = ref []

	fun add_constraint t1 t2 = constr := !constr @ [(t1,t2)]

	fun get_constraints () = (List.app (fn (x,l) => Debug.debug 2 (" * " ^ B.ty_name x ^ " = " ^ (String.concatWith "," (map B.ty_name l)))) (!constr);
								!constr)

	fun constrain b = 
		(case B.ty b of B.TyVar x => (add_constraint (B.TyVar x) (map constrain (B.children b)); B.TyVar x)
		              | B.TyName x => (add_constraint (B.TyName x) (map constrain (B.children b)); B.TyName x)
					  | B.TyComp (t,ch) => (add_constraint (B.TyComp(t,ch)) (map constrain (B.children b)); t)
					  | B.TyCon (t,t') => (add_constraint (B.TyCon(t,t')) (map constrain (B.children b)); B.TyCon(t,t'))
					  | B.TyUnit => B.TyUnit
					  | B.TyArrow (t1,t2) =>
					  	let
					  		val redex = hd (B.children b)
							val reactum = hd (tl (B.children b))
					    	val redexty = constrain redex
							val reactumty = constrain reactum
							val _ = add_constraint (B.TyArrow (t1,t2)) ([B.TyArrow (redexty,reactumty)])
					  	in
					  		B.TyArrow(redexty,reactumty)
					  	end
					  | B.TyUnknown => raise Fail "Constraint failure - ??? appears where only type variables should be present.")

	fun infer b = B.TyUnknown

	fun ty_var_replace b = (B.traverse 
						(fn (p as ref (z as {control, children, parent, links, symtab})) => 
							(p := {control = ty_var_replace_cntrl control,
								   children=children,
								   parent=parent,
								   links=links,
								   symtab=symtab})) b; b)
 
	fun ty_replace b t1 t2 = (B.traverse 
						(fn (p as ref (z as {control, children, parent, links, symtab})) => 
							(p := {control = ty_var_replace_cntrl control,
								   children=children,
								   parent=parent,
								   links=links,
								   symtab=symtab})) b; b)
end
