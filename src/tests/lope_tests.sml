structure LopeTests =
struct
	fun run_all_tests () = let
		val _ =
		(BigraphTests.run_all_tests ();
		 SymtabTests.run_all_tests ();
		 ParseTests.run_all_tests ();
		 TypesTests.run_all_tests ();
		 ReactionTests.run_all_tests ();
		 OptimiserTests.run_all_tests ();
		 CCodeGenTests.run_all_tests ())
		val _ = print ("\nTotal: " ^ Int.toString (!Test.totalCount) ^ "\nFailures: " ^ Int.toString (!Test.failedCount) ^ "\n")
		in
			if (!Test.failedCount) > 0 then raise (Fail (Int.toString (!Test.failedCount) ^ " tests failed")) else ()
		end
end
