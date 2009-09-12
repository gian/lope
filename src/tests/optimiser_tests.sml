structure OptimiserTests =
struct
	open Test
	open Optimiser
	structure B = Bigraph

	val tests = [
	fn () => (* reduction *)
	let
		fun bigraph_size x = 0
		val b = B.empty
		val s = []
		val (b',s') = optimise (b,s)
		val ssize = foldl (op +) 0 (map bigraph_size s)
		val ssize' = foldl (op +) 0 (map bigraph_size s')
	in
		assert ("reduction1", bigraph_size b' <= bigraph_size b) ;
		assert ("reduction2", ssize' <= ssize) ;
		assert ("reduction3", false)
	end
	]

	fun run_all_tests () = (print "[OptimiserTests]\n"; app run_test tests) 
		handle (B.BigraphStructureException e) => Debug.debug 1 ("BigraphStructureException: " ^ e ^ "\n")
		     | (B.BigraphLinkException e) => Debug.debug 1 ("BigraphLinkException: " ^ e ^ "\n")
		     | (B.BigraphNameException e) => Debug.debug 1 ("BigraphNameException: " ^ e ^ "\n")
end
