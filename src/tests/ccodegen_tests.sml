structure CCodeGenTests =
struct
	open Test
	open CCodeGen
	structure B = Bigraph

	val tests = [
	fn () => (* valid_code *)
	let
		val r = (CCodeGen.generate B.empty [] <> "") handle _ => false
	in
		assert ("valid_code", r)
	end
	]

	fun run_all_tests () = (print "[CCodeGenTests]\n"; app run_test tests) 
		handle (B.BigraphStructureException e) => Debug.debug 1 ("BigraphStructureException: " ^ e ^ "\n")
		     | (B.BigraphLinkException e) => Debug.debug 1 ("BigraphLinkException: " ^ e ^ "\n")
		     | (B.BigraphNameException e) => Debug.debug 1 ("BigraphNameException: " ^ e ^ "\n")
end
