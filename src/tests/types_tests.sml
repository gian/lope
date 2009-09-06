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
	end
	]
	
	fun run_all_tests () = (print "[TypesTests]\n"; app run_test tests) handle b => raise TestFailed "types" 
end
