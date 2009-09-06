structure Debug =
struct
	val verbosity = ref 0

	fun set_verbosity n = (verbosity := n)

	fun debug n msg = if (!verbosity) >= n then (print ("[DEBUG] " ^ msg ^ "\n")) else ()
end
