structure BigraphTests =
struct
	open Test
	open Bigraph

	fun cname k =
		(fn (AnonControl t) => t) (control k)

	fun test_add_child () =
	let
		val a = new (AnonControl "foo")
		val b = new (AnonControl "bar")
		val _ = add_child a b
		val b' = hd (children a)
	in
		assert ("add_child",a = parent b') ;
		assert ("add_child", cname b' = "bar")
	end

	fun test_siblings () =
	let
		val a = new (AnonControl "foo")
		val b = new (AnonControl "child1")
		val c = new (AnonControl "child2")
		val _ = add_child a b
		val _ = add_child a c
		val b' = hd (children a)
		val s = siblings b'
	in
		 assert ("siblings", length s = 1) ;
		 assert ("siblings", cname (hd s) =
		    (if cname b' = "child1" then "child2" else "child1"))
	end

	fun run_all_tests () =
	    (
		run_test test_add_child ;
		run_test test_siblings
	    )
end
