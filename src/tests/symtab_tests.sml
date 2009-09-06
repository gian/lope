structure SymtabTests =
struct
	open Test
	open Symtab
	structure B = Bigraph


	val tests = [
	fn () => (* insert *)
	let
		val s : B.lope_control B.bigraph symtab = new (B.empty)
		val b = B.new (B.AnonControl (B.TyName "foo"))
		val _ = insert s "test" b
		val b' = get s "test"
	in
	    assert ("insert1", b = b') 
	end,
	fn () => (* get_name *)
	let
		val w = B.new (B.AnonControl (B.TyName "W"))
		val a = B.new (B.NodeControl ("A",B.TyUnknown))
		val x = B.new (B.AnonControl (B.TyName  "X"))
		val y = B.new (B.AnonControl (B.TyName "Y"))
		val z = B.new (B.AnonControl (B.TyName "Z"))
		val _ = B.add_child w x
		val _ = B.add_child w a
		val _ = B.add_child x y
		val _ = B.add_child y z
		val a' = B.get_name z "A"
		
	in
	    assert ("get_name1", a = a') 
	end
	]
	
	fun run_all_tests () = (print "[SymtabTests]\n"; app run_test tests) handle (B.BigraphNameException b) => (Debug.debug 1 (b ^ "\n"); raise TestFailed "symtab") 
end
