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

	fn () => (* reaction_nointerf *)
	let
			val _ = T.reset_ty_var ()
			val p = T.ty_var_replace (Parse.parse_string 
			("A { reaction R { redex { B {} } reactum { C {} } } }\n" ^ 
			 "D { reaction R' { redex { E {} } reactum { F {} } } }\n" ^ 
			 "G { reaction R'' { redex { H {} } reactum { I {} } } }\n" ))

			val _ = T.reset_constraints()
			val p' = T.constrain p
			val _ = Debug.debug 2 ("Constraint: " ^ B.ty_name p')
			val _ = Debug.debug 2 (B.to_string p)
			val c = T.get_constraints ()

			val k = T.simplify_constraints p c
			val _ = Debug.debug 2 (B.to_string p)
			val r = reactions p
			val _ = Debug.debug 2 ("Reaction set size: " ^ Int.toString (length r))
			val sr = interference r
			val _ = Debug.debug 2 ("interference set:\n" ^ (String.concatWith "\n" (map (fn (x,y) => B.name x ^ ", " ^ B.name y) sr)))
	in
		assert("reaction_nointerf1", length sr = 0) 
	end,

	fn () => (* reaction_interf *)
	let
			val _ = T.reset_ty_var ()
			val p = T.ty_var_replace (Parse.parse_string 
			("A { reaction R { redex { B : A {} } reactum { C {} } } }\n" ^ 
			 "D { reaction R' { redex { C {} } reactum { F {} } } }\n" ^ 
			 "G { reaction R'' { redex { H {} } reactum { I : A {} } } }\n" ))

			val _ = T.reset_constraints()
			val p' = T.constrain p
			val _ = Debug.debug 2 ("Constraint: " ^ B.ty_name p')
			val _ = Debug.debug 2 (B.to_string p)
			val c = T.get_constraints ()

			val k = T.simplify_constraints p c
			val _ = Debug.debug 2 (B.to_string p)
			val r = reactions p
			val _ = Debug.debug 2 ("Reaction set size: " ^ Int.toString (length r))
			val sr = interference r
			val _ = Debug.debug 2 ("interference set:\n" ^ (String.concatWith "\n" (map (fn (x,y) => B.name x ^ ", " ^ B.name y) sr)))
	in
		assert("reaction_interf1", length sr = 3) ;
		assert("reaction_interf2", not (List.exists (fn (x,y) => x = y) sr)) 
	end,

	fn () => (* reaction_slice1 *)
	let
			val _ = T.reset_ty_var ()
			val p = T.ty_var_replace (Parse.parse_string 
			("A { reaction R { redex { B : A {} } reactum { C {} } } }\n" ^ 
			 "D { reaction R' { redex { C {} } reactum { F {} } } }\n" ^ 
			 "G { reaction R'' { redex { H {} } reactum { I {} } } }\n" ))

			val _ = T.reset_constraints()
			val p' = T.constrain p
			val _ = Debug.debug 2 ("Constraint: " ^ B.ty_name p')
			val _ = Debug.debug 2 (B.to_string p)
			val c = T.get_constraints ()

			val k = T.simplify_constraints p c
			val _ = Debug.debug 2 (B.to_string p)
			val r = reactions p
			val _ = Debug.debug 2 ("Reaction set size: " ^ Int.toString (length r))
			val sr = interference r
			val _ = Debug.debug 2 ("interference set:\n" ^ (String.concatWith "\n" (map (fn (x,y) => B.name x ^ ", " ^ B.name y) sr)))
			val ss = slice r sr
			val _ = Debug.debug 2 ("slice set:\n" ^ (String.concatWith "\nSlice:\n" (map (fn y => (String.concatWith "\n" (map (fn x => B.name x) y))) ss)))
	in
		assert("reaction_slice1", length ss = 2)
	end,
	
	fn () => (* reaction_slice2 *)
	let
			val _ = T.reset_ty_var ()
			val p = T.ty_var_replace (Parse.parse_string 
			("A { reaction R { redex { B  {} } reactum { C {} } } }\n" ^ 
			 "D { reaction R' { redex { D {} } reactum { E {} } } }\n" ^ 
			 "G { reaction R'' { redex { F {} } reactum { I {} } } }\n" ))

			val _ = T.reset_constraints()
			val p' = T.constrain p
			val _ = Debug.debug 2 ("Constraint: " ^ B.ty_name p')
			val _ = Debug.debug 2 (B.to_string p)
			val c = T.get_constraints ()

			val k = T.simplify_constraints p c
			val _ = Debug.debug 2 (B.to_string p)
			val r = reactions p
			val _ = Debug.debug 2 ("Reaction set size: " ^ Int.toString (length r))
			val sr = interference r
			val _ = Debug.debug 2 ("interference set:\n" ^ (String.concatWith "\n" (map (fn (x,y) => B.name x ^ ", " ^ B.name y) sr)))
			val ss = slice r sr
			val _ = Debug.debug 2 ("slice set:\n" ^ (String.concatWith "\nSlice:\n" (map (fn y => (String.concatWith "\n" (map (fn x => B.name x) y))) ss)))
	in
		assert("reaction_slice2", length ss = 3)
	end
	]
	
	fun run_all_tests () = (print "[ReactionTests]\n"; app run_test tests) handle b => (raise b)
end
