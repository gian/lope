structure ReactionTests =
struct
	open Test
	open Reaction 
	structure B = Bigraph
	structure T = Types

	val tests = [
	fn () => (* reactions *)
	let
		val p = T.ty_var_replace (Parse.parse_string 
			("A { B { }\nreaction R { redex { B {} } reactum { B {} C {} } } }\n" ^ 
			"D { E { }\nreaction R' { redex { F {} } reactum { G {} H {} } } }\n" ))
		val _ = Debug.debug 2 ("p: " ^ B.to_string p)
		val k = reactions p
		val _ = Debug.debug 2 ("reactions:\n" ^ (String.concatWith "\n" (map B.to_string k)))
	in
	    assert ("reactions1", false) 
	end
	]
	
	fun run_all_tests () = (print "[ReactionTests]\n"; app run_test tests) handle b => (raise b)
end
