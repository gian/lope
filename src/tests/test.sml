structure Test =
struct
	exception TestFailed of string

	fun pad s n =
		if size s >= n then s else pad (" " ^ s) n

	fun assert (str, true) = print ("[TEST] " ^ pad str 17 ^ ":\tpassed\n")
	  | assert (str, false) = raise (TestFailed str)

	fun assertFalse (s, v) = assert (s, not v)

	fun run_test f = f () handle TestFailed m =>
		print ("[TEST] " ^ pad m 17 ^ ":\tFAILED\n")
end
