structure CCodeGenTests =
struct
	open Test
	open CCodeGen
	structure  T = Types
	structure B = Bigraph
	structure R = Reaction

	val tests = [
	fn () => (* valid_code *)
	let
			val _ = T.reset_ty_var ()
			val p = T.ty_var_replace (Parse.parse "tests/underground.lope")

			val _ = Debug.debug 2 ("Underground: " ^ B.to_string p)
			val _ = T.reset_constraints()
			val p' = T.constrain p
			val _ = Debug.debug 2 ("Constraint: " ^ B.ty_name p')
			val _ = Debug.debug 2 (B.to_string p)
			val c = T.get_constraints ()

			val k = T.simplify_constraints p c
			val _ = Debug.debug 2 (B.to_string p)
			val r = R.reactions p
			val _ = Debug.debug 2 ("Reaction set size: " ^ Int.toString (length r))
			val sr = R.interference r
			val _ = Debug.debug 2 ("interference set:\n" ^ (String.concatWith "\n" (map (fn (x,y) => B.name x ^ ", " ^ B.name y) sr)))
			val ss = R.slice r sr
			val _ = Debug.debug 2 ("slice set:\n" ^ (String.concatWith "\nSlice:\n" (map (fn y => (String.concatWith "\n" (map (fn x => B.name x) y))) ss)))


		val r = (CCodeGen.generate p ss <> "") handle _ => false
	in
		assert ("valid_code", r)
	end
	]

	fun run_all_tests () = (print "[CCodeGenTests]\n"; app run_test tests) 
		handle (B.BigraphStructureException e) => Debug.debug 1 ("BigraphStructureException: " ^ e ^ "\n")
		     | (B.BigraphLinkException e) => Debug.debug 1 ("BigraphLinkException: " ^ e ^ "\n")
		     | (B.BigraphNameException e) => Debug.debug 1 ("BigraphNameException: " ^ e ^ "\n")
end
