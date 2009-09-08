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
		val t = ty_var_replace a
	in
	    assert ("ty_var_replace", B.name t = ": A{?X1{C}}")
	end
	]
	
	fun run_all_tests () = (print "[TypesTests]\n"; app run_test tests) handle b => raise TestFailed "types" 
end
