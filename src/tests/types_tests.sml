structure TypesTests =
struct
	open Test
	open Types
	structure B = Bigraph


	val tests = [
	fn () => (* ty_name *)
	let
		val tn = B.ty_name
	in
	    assert ("ty_name1", tn (B.TyName "A") = "A") ; 
	    assert ("ty_name2", tn (B.TyCon (B.TyName "A", B.TyName "B")) = "A B") ; 
	    assert ("ty_name3", tn (B.TyTuple [B.TyName "A", B.TyName "B"]) = "A * B") ; 
	    assert ("ty_name4", tn (B.TyPoly "a") = "'a") ; 
	    assert ("ty_name5", tn (B.TyVar 1) = "?X1") ; 
	    assert ("ty_name6", tn (B.TyUnknown) = "???")  
	end,
	fn () => (* to_string *)
	let
		val t =	B.TyComp (B.TyName "A", [B.TyName "B", B.TyName "C"])
	in
	    assert ("to_string1", B.ty_name t = "A{B,C}") 
	end,
	fn () => (* is_subtype *)
	let
		val t =	B.TyComp (B.TyName "A", [B.TyName "B"])	
		val t' = B.TyComp (B.TyName "A", [B.TyComp (B.TyName "B", [B.TyName "C"])])
		val t'' = B.TyComp (B.TyName "A", [B.TyComp (B.TyName "B", [B.TyName "C"]), B.TyName "D"])
	in
	    assert ("is_subtype1", is_subtype t t') ; 
	    assert ("is_subtype2", is_subtype t t'') ; 
	    assert ("is_subtype3", is_subtype t' t'') ; 
	    assert ("is_subtype4", not (is_subtype t'' t)) ; 
	    assert ("is_subtype5", not (is_subtype t'' t'))  
	end,
    
	fn () => (* ty_var_replace *)
	let
		val a = B.new (B.AnonControl (B.TyName "A"))
		val b = B.new (B.AnonControl (B.TyUnknown))
		val c = B.new (B.AnonControl (B.TyName "C"))
		val _ = B.add_child a b
		val _ = B.add_child b c
		val _ = reset_ty_var ()
		val t = ty_var_replace a
		val _ = Debug.debug 2 ("ty_var_replace: " ^ B.name t)
	in
	    assert ("ty_var_replace1", B.name t = ": A") ;
	    assert ("ty_var_replace2", B.name (hd (B.children t)) = ": ?X0") 
	end,

	fn () => (* constrain_anon *)
	let
			val _ = reset_ty_var ()
			val p = ty_var_replace (Parse.parse_string 
			("A { B { }\nreaction R { redex { B {} } reactum { C {} D {} } } }\n" ^ 
			"E { F { }\nreaction R' { redex { G {} } reactum { H {} I {} } } }\n" ))

			val _ = reset_constraints ()
			val p' = constrain p
			val _ = Debug.debug 2 ("Constraint: " ^ B.ty_name p')
			val _ = Debug.debug 2 (B.to_string p)
			val k = get_constraints ()
			val (k',_) = hd k
	in
		assert("constrain_anon1", k' = B.TyVar 2)
	end,
	
	fn () => (* constrain_typed *)
	let
			val _ = reset_ty_var ()
			val p = ty_var_replace (Parse.parse_string 
			("A : T1 { B { }\nreaction R { redex { C : T2 {} } reactum { {} E {} } } }\n" ^ 
			"F { G { }\nreaction R' { redex { H {} } reactum { I {} {} } } }\n" ))

			val _ = reset_constraints()
			val p' = constrain p
			val _ = Debug.debug 2 ("Constraint: " ^ B.ty_name p')
			val _ = Debug.debug 2 (B.to_string p)
			val c = get_constraints ()

			val k = simplify_constraints p c
			val _ = Debug.debug 2 (B.to_string p)
	in
		assert("constrain_typed1", List.exists (fn (B.TyName "T1", [B.TyVar 1, 
					B.TyArrow _]) => true | _ => false) c)
	end
	]
	
	fun run_all_tests () = (print "[TypesTests]\n"; app run_test tests) handle b => (raise b)
end
