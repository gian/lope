structure Test =
struct
	exception TestFailed of string

	fun assert (str, true) = ()
	  | assert (str, false) = raise (TestFailed str)

	fun assertFalse (s, v) = assert (s, not v)

	fun run_test f = f () handle TestFailed m =>
		(print ("Test Failed: " ^ m ^ "\n"))
end
