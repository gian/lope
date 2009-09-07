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
		val t = BgType(B.TyName "a", [BgType (B.TyName "b",[]), 
										BgType (B.TyName "c", [])])
	in
	    assert ("to_string1", to_string t = "a{b,c}") 
	end,
	fn () => (* is_subtype *)
	let
		val t = BgType(B.TyName "a",[])
		val t' =  BgType(B.TyName "a", [BgType (B.TyName "b",[])])
		val t'' = BgType(B.TyName "a", [BgType (B.TyName "b",[]), 
										BgType (B.TyName "c", [])])

	in
	    assert ("is_subtype1", is_subtype t t') ; 
	    assert ("is_subtype2", is_subtype t t'') ; 
	    assert ("is_subtype3", is_subtype t' t'') ; 
	    assert ("is_subtype4", not (is_subtype t'' t)) ; 
	    assert ("is_subtype5", not (is_subtype t'' t'))  
	end
	]
	
	fun run_all_tests () = (print "[TypesTests]\n"; app run_test tests) handle b => raise TestFailed "types" 
end
