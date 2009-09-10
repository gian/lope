structure ReactionTests =
struct
	open Test
	open Reaction 
	structure B = Bigraph
	structure T = Types

	val tests = [
	fn () => (* reaction_collect *)
	let
		val _ = T.reset_ty_var ()
		val p = T.ty_var_replace (Parse.parse_string 
			("A { B { }\nreaction R { redex { B {} } reactum { B {} C {} } } }\n" ^ 
			"D { E { }\nreaction R' { redex { F {} } reactum { G {} H {} } } }\n" ))
		val _ = Debug.debug 2 ("p: " ^ B.to_string p)
		val k = reactions p
		val _ = Debug.debug 2 ("reactions:\n" ^ (String.concatWith "\n" (map B.to_string k)))
	in
	    assert ("reaction_collect1", length k = 2) ; 
	    assert ("reaction_collect2", B.name_opt (hd k) = SOME "R") ; 
	    assert ("reaction_collect3", B.name_opt (hd (tl k)) = SOME "R'") 
	end,

	fn () => (* reaction_sort *)
	let
			val _ = T.reset_ty_var ()
			val p = T.ty_var_replace (Parse.parse_string 
			("A : T1 { B { }\nreaction R { redex { C : T2 {} } reactum { {} E {} } } }\n" ^ 
			"F { G { }\nreaction R' { redex { H {} } reactum { I {} {} } } }\n" ))

			val _ = T.reset_constraints()
			val p' = T.constrain p
			val _ = Debug.debug 2 ("Constraint: " ^ B.ty_name p')
			val _ = Debug.debug 2 (B.to_string p)
			val c = T.get_constraints ()

			val k = T.simplify_constraints p c
			val _ = Debug.debug 2 (B.to_string p)
			val r = reactions p
			val _ = Debug.debug 2 ("reactions:\n" ^ (String.concatWith "\n" (map B.to_string r)))
	in
		assert("reaction_sort1", false)
	end
	]
	
	fun run_all_tests () = (print "[ReactionTests]\n"; app run_test tests) handle b => (raise b)
end
