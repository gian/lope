structure LopeTests =
struct
	fun run_all_tests () =
		(BigraphTests.run_all_tests ();
		 ParseTests.run_all_tests ())
end
