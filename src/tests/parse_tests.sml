structure ParseTests =
struct
	open Test
	open Parse
	structure B = Bigraph


	val tests = [
	fn () => (* empty_anon *)
	let
		val p = Parse.parse_string "{}"
	in
		assert ("empty_anon1",B.name p = " : ") ;
		assert ("empty_anon2",B.children p = [])
	end,
	
	fn () => (* empty_named_ut *)
	let
		val p = Parse.parse_string "A {}"
	in
		assert ("empty_named_ut1", B.name p = "A : ") ;
		assert ("empty_named_ut2", B.children p = [])
	end,

	fn () => (* empty_named_t *)
	let
		val p = Parse.parse_string "A : t {}"
	in
		assert ("empty_named_t1", B.name p = "A : t") ;
		assert ("empty_named_t2", B.children p = [])
	end,

    fn () => (* nested_named_t *)
	let
		val p = Parse.parse_string "A : t1 { B : t2 {} ; C : t3 {} }"
	in
		assert ("nested_named_t1", B.name p = "A : t") ;
		assert ("nested_named_t2", length (B.children p) = 2) ;
		assert ("nested_named_t3", B.name (hd (B.children p)) = "B : t2") ;
		assert ("nested_named_t4", B.name (hd (B.children p)) = "C : t3")
	end
	]

	fun run_all_tests () = (print "[ParseTests]\n"; app run_test tests)
end
