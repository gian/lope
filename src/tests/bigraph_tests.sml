structure BigraphTests =
struct
	open Test
	open Bigraph

	fun cname k =
		(fn (AnonControl t) => t) (control k)

	val tests = [
	fn () =>
	let
		val a = new (AnonControl "foo")
		val b = new (AnonControl "bar")
		val _ = add_child a b
		val b' = hd (children a)
	in
		assert ("add_child1",a = parent b') ;
		assert ("add_child2", cname b' = "bar")
	end,

	fn () =>
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
		 assert ("siblings2", cname (hd s) =
		    (if cname b' = "child1" then "child2" else "child1"))
	end,

	fn () =>
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
		assert ("add_link3", cname l1' = cname b) ;
		assert ("add_link4", cname l2' = cname a)
	end
	]

	fun run_all_tests () = app run_test tests 
end
