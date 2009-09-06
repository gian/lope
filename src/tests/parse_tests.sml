structure ParseTests =
struct
	open Test
	open Parse
	structure B = Bigraph


	val tests = [
	fn () => (* empty_anon *)
	let
		val p = Parse.parse_string "{}\n"
		val p' = hd (B.children p)
	in
	    assert ("world1", B.name p = "World : world") ;
		assert ("empty_anon1(" ^ B.name p' ^ ")",B.name p' = ": ") ;
		assert ("empty_anon2",B.children p' = [])
	end,
	
	fn () => (* empty_named_ut *)
	let
		val p = hd (B.children (Parse.parse_string "A {}\n"))
	in
		assert ("empty_named_ut1", B.name p = "A : ") ;
		assert ("empty_named_ut2", B.children p = [])
	end,

	fn () => (* empty_named_t *)
	let
		val p = hd (B.children (Parse.parse_string "A : t {}\n"))
	in
		assert ("empty_named_t1", B.name p = "A : t") ;
		assert ("empty_named_t2", B.children p = [])
	end,

    fn () => (* nested_named_t *)
	let
		val p = hd (B.children (Parse.parse_string "A : t1 { B : t2 {}\nC : t3 {} }"))
		val _ = print (B.to_string p)
	in
		assert ("nested_named_t1", B.name p = "A : t1") ;
		assert ("nested_named_t2", length (B.children p) = 2) ;
		assert ("nested_named_t3", B.name (hd (B.children p)) = "B : t2") ;
		assert ("nested_named_t4", B.name (hd (tl (B.children p))) = "C : t3")
	end,

    fn () => (* param_empty *)
	let
		val p = hd (B.children (Parse.parse_string "A : t (x : t1, y) {}\n"))
		val _ = print (B.to_string p)
	in
		assert ("param_empty1", B.name p = "A : t (...)") ;
		assert ("param_empty2", length (B.params p) = 2) ;
		assert ("param_empty3", (fn (x,_) => x) (hd (B.params p)) = "x") ;
		assert ("param_empty4", (fn (_,x) => x) (hd (B.params p)) = "t1") ;
		assert ("param_empty5", (fn (x,_) => x) (hd (tl (B.params p))) = "y") ;
		assert ("param_empty6", (fn (_,x) => x) (hd (tl (B.params p))) = "") 
	end,

    fn () => (* param_nested *)
	let
		val p = hd (B.children (Parse.parse_string "A : t (x : t1, y : t2) { B (z : t3) {} }\n"))
		val p' = hd (B.children p)
		val _ = print (B.to_string p)
	in
		assert ("param_nested1", B.name p' = "B :  (...)") ;
		assert ("param_nested2", length (B.params p') = 1) ;
		assert ("param_nested3", (fn (x,_) => x) (hd (B.params p')) = "z") ;
		assert ("param_nested4", (fn (_,x) => x) (hd (B.params p')) = "t3") 
	end,

    fn () => (* reaction *)
	let
		val p = hd (B.children (Parse.parse_string 
			"A { B { }\nreaction R { redex { B {} } reactum { B {} C {} } } }\n"))
		val r = hd (tl (B.children p))
		val _ = print (B.to_string p)
	in
		assert ("reaction1", B.name r = "R : reaction") ;
		assert ("reaction2", length (B.children r) = 2) ;
		assert ("reaction3(" ^ B.name (hd (B.children r)) ^ ")", B.name (hd (B.children r)) = ": redex") ;
		assert ("reaction4", B.name (hd (tl (B.children r))) = ": reactum") ;
		assert ("reaction5", length (B.children (hd (B.children r))) = 1) ;
		assert ("reaction6", length (B.children (hd (tl (B.children r)))) = 2) 
	end,

    fn () => (* site *)
	let
		val p = hd (B.children (Parse.parse_string "A : t1 { $x : t2 }\n"))
		val r = hd (B.children p)
		val _ = print (B.to_string p)
	in
		assert ("site1", B.name r = "$x : t2 site") 
	end,

	fn () => (* link_tl *)
	let
		val a = hd (B.children (Parse.parse_string 
			"A : t1 { }\nB : t2 { }\nlink A <-> B \n"))
		val _ = print (B.to_string a)
	in
		assert ("link_tl1", B.name a = "A : t1") ;
		assert ("link_tl2", length (B.link_targets a) = 1) ;
		assert ("link_tl3", B.name (hd (B.link_targets a)) = "B : t2")
	end
	]

	fun run_all_tests () = (print "[ParseTests]\n"; app run_test tests) 
		handle (B.BigraphStructureException e) => print ("BigraphStructureException: " ^ e ^ "\n")
		     | (B.BigraphLinkException e) => print ("BigraphLinkException: " ^ e ^ "\n")
		     | (B.BigraphNameException e) => print ("BigraphNameException: " ^ e ^ "\n")
end
