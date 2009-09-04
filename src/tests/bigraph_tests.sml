structure BigraphTests =
struct
	open Test
	open Bigraph


	val tests = [
	fn () => (* add_child *)
	let
		val a = new (AnonControl "foo")
		val b = new (AnonControl "bar")
		val _ = add_child a b
		val b' = hd (children a)
	in
		assert ("add_child1",a = parent b') ;
		assert ("add_child2", name b' = ": bar")
	end,

	fn () => (* siblings *)
	let
		val a = new (AnonControl "foo")
		val b = new (AnonControl "child1")
		val c = new (AnonControl "child2")
		val _ = add_child a b
		val _ = add_child a c
		val b' = hd (children a)
		val s = siblings b'
	in
		 assert ("siblings1", length s = 1) ;
		 assert ("siblings2", name (hd s) =
		    (if name b' = ": child1" then ": child2" else ": child1"))
	end,

	fn () => (* add_link *)
	let
		val a = new (AnonControl "foo")
		val b = new (AnonControl "bar")
		val _ = add_link a "l" "int" b
		val l1 = link_targets a
		val l2 = link_targets b
		val _ = assert("add_link1", length l1 = 1)
		val _ = assert("add_link2", length l2 = 1)
		val l1' = hd l1
		val l2' = hd l2
	in
		assert ("add_link3", name l1' = name b) ;
		assert ("add_link4", name l2' = name a)
	end,

	fn () => (* delete_child *)
	let
		val a = new (AnonControl "foo")
		val b = new (AnonControl "child1")
		val c = new (AnonControl "child2")
		val d = new (AnonControl "child3")
		val _ = add_child a b
		val _ = add_child a c
		val _ = add_child a d
		val _ = delete_child a c
		val r = children a
	in
		 assert ("delete_child1", length r = 2) ;
		 assert ("delete_child2", name (hd r) = ": child3") ;
		 assert ("delete_child3", name (hd (tl r)) = ": child1") ; 
		 assert ("delete_child4", parent c = Empty) 
	end,

	fn () => (* delete_link *)
	let
		val a = new (AnonControl "foo")
		val b = new (AnonControl "bar")
		val c = new (AnonControl "baz")
		val _ = add_link a "l" "int" b
		val _ = add_link a "m" "int" c
		val l1 = link_targets a
		val _ = assert("delete_link1", length l1 = 2)
		val _ = delete_link a b
		val l2 = link_targets a
		val _ = assert("delete_link2", length l2 = 1)
		val l2' = hd l2
	in
		assert ("delete_link3", name l2' = name c) ;
		assert ("delete_link4", length (link_targets b) = 0) 
	end
	]

	fun run_all_tests () = (print "[BigraphTests]\n"; app run_test tests)
end
