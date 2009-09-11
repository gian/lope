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
	val ty_substitute : Bigraph.lope_control Bigraph.bigraph -> Bigraph.ty -> Bigraph.ty list -> Bigraph.lope_control Bigraph.bigraph
	val simplify_constraints : Bigraph.lope_control Bigraph.bigraph -> (Bigraph.ty * Bigraph.ty list) list -> (Bigraph.ty * Bigraph.ty list) list 
	val constrain : Bigraph.lope_control Bigraph.bigraph -> Bigraph.ty
	val get_constraints : unit -> (Bigraph.ty * Bigraph.ty list) list
	val reset_constraints : unit -> unit
	val infer : Bigraph.lope_control Bigraph.bigraph -> ty 
	val overlaps : ty -> ty -> bool
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
	  | ty_replace (B.TyAComp l) = B.TyAComp (map ty_replace l)
	  | ty_replace b = b 

	fun substinty h n B.TyUnknown = fresh_ty_var ()
	  | substinty h n (B.TyArrow(a,b)) = B.TyArrow (substinty h n a, substinty h n b)
	  | substinty h n (B.TyCon(a,b)) = B.TyCon (substinty h n a, substinty h n b)
	  | substinty h n (B.TyTuple k) = B.TyTuple (map (substinty h n) k)
	  | substinty (B.TyComp(a,_)) n (B.TyComp(b,x)) = if a = b then n else B.TyComp(b,x)
	  | substinty h n (B.TyComp (t,l)) = B.TyComp (substinty h n t, map (substinty h n) l)
	  | substinty h n (B.TyAComp l) = B.TyAComp (map (substinty h n) l)
	  | substinty (B.TyVar k1) n (B.TyVar k2) = if k1 = k2 then n else (B.TyVar k2)
	  | substinty (B.TyName k1) n (B.TyName k2) = if k1 = k2 then n else (B.TyName k2)
	  | substinty h n b = b 

	fun map_cntrl tr (B.NodeControl (l,t)) = B.NodeControl (l, tr t)
	  | map_cntrl tr (B.AnonControl t) = B.AnonControl (tr t)
	  | map_cntrl tr (B.ParamNodeControl (l,t,lst)) = B.ParamNodeControl (l, tr t, map (fn (l,t') => (l, tr t')) lst)
	  | map_cntrl tr (B.SiteControl (l,t)) = B.SiteControl (l, tr t)
	  | map_cntrl tr (B.WildControl t) = B.WildControl (tr t)
	  | map_cntrl tr (B.DataControl d) = B.DataControl (case d of B.VarData (l,t) => B.VarData (l,tr t) | p => p)

	val constr : (B.ty * B.ty list) list ref = ref []

	fun add_constraint t1 t2 = constr := !constr @ [(t1,t2)]

	fun get_constraints () = (List.app (fn (x,l) => Debug.debug 2 (" * " ^ B.ty_name x ^ " = " ^ (String.concatWith "," (map B.ty_name l)))) (!constr);
								!constr)

	fun reset_constraints () = constr := []

	val uniq = ref 10000

	fun uniq_ty b = B.TyUniq (B.name_opt b, (uniq := !uniq + 1; !uniq))

	fun opt b [] = [uniq_ty b]
	  | opt b k = k

	fun constrain b = 
		(case B.ty b of B.TyVar x => (add_constraint (B.TyVar x) (opt b (map constrain (B.children b))); B.TyVar x)
		              | B.TyName x => (add_constraint (B.TyName x) (opt b (map constrain (B.children b))); B.TyName x)
					  | B.TyComp (t,ch) => 
					  	let
							val k = if (B.children b) = [] then uniq_ty b else B.TyComp(t,map constrain (B.children b))
						in
							(add_constraint (B.TyComp(t,ch)) [k]; k)
						end
					  | B.TyAComp l => (add_constraint (B.TyAComp l) (map constrain (B.children b)); B.TyAComp l)
					  | B.TyCon (t,t') => (add_constraint (B.TyCon(t,t')) (opt b (map constrain (B.children b))); B.TyCon(t,t'))
					  | B.TyUnit => B.TyUnit
					  | B.TyArrow (t1,t2) =>
					  	let
					  		val redex = hd (B.children b)
							val reactum = hd (tl (B.children b))
					    	val redexty = constrain redex
							val reactumty = constrain reactum
							val _ = add_constraint t1 [redexty]
							val _ = add_constraint t2 [reactumty]
					  	in
					  		B.TyArrow(redexty,reactumty)
					  	end
					  | B.TyUniq s => B.TyUniq s
					  | B.TyUnknown => raise Fail "Constraint failure - ??? appears where only type variables should be present.")

	fun infer b = B.TyUnknown

	fun ty_var_replace b = (B.traverse 
						(fn (p as ref (z as {control, children, parent, links, symtab})) => 
							(p := {control = map_cntrl ty_replace control,
								   children=children,
								   parent=parent,
								   links=links,
								   symtab=symtab})) b; b)

	fun wrap r [] = B.TyName "empty"
	  | wrap r l = (case r of B.TyName x => B.TyComp (r, l)
	                        | B.TyCon _ => B.TyComp(r,l)
							| _ => B.TyAComp l)


	fun ty_substitute b t1 t2 =
						(B.traverse 
						(fn (p as ref (z as {control, children, parent, links, symtab})) => 
							(p := {control = map_cntrl (fn x => substinty t1 (wrap (B.ty (B.Bigraph p)) t2) x) control,
								   children=children,
								   parent=parent,
								   links=links,
								   symtab=symtab})) b; b)

	fun simplify_constraints b [] = []
	  | simplify_constraints b ((t1,t2)::t) = (t1,t2) :: (ty_substitute b t1 t2; simplify_constraints b (map (fn (x,y) => (x, (map (fn z => substinty t1 (wrap t1 t2) z) y))) t))

	fun overlaps (B.TyAComp []) (B.TyAComp []) = false
	  | overlaps (B.TyAComp x) (B.TyAComp y) = not (List.all (fn t => List.all (fn k => not (overlaps t k)) y) x)
	  | overlaps (B.TyComp (w,x)) (B.TyComp (y,z)) = overlaps w y andalso (List.all (fn t => List.all (fn k => not (overlaps t k)) z) x)
	  | overlaps (B.TyComp (w,x)) (B.TyAComp z) = (List.all (fn t => List.all (fn k => not (overlaps t k)) z) x)
	  | overlaps (B.TyAComp x) (B.TyComp (y,z)) = (List.all (fn t => List.all (fn k => not (overlaps t k)) z) x)
	  | overlaps (B.TyName x) (B.TyName y) = x = y
	  | overlaps (B.TyCon (w,x)) (B.TyCon (y,z)) = overlaps w y andalso overlaps x z
	  | overlaps (B.TyArrow (w,x)) (B.TyArrow (y,z)) = overlaps w y andalso overlaps x z
	  | overlaps (B.TyUnit) (B.TyUnit) = true
	  | overlaps x (B.TyComp (t, _)) = overlaps x t
	  | overlaps (B.TyUniq (NONE,n)) (B.TyUniq (NONE,m)) = n = m
	  | overlaps (B.TyUniq (SOME x,_)) (B.TyUniq (SOME y,_)) = x = y
	  | overlaps x y = (print ("Unhandled Case: " ^ B.ty_name x ^ " and " ^ B.ty_name y ^ "\n"); false)
	  
end
