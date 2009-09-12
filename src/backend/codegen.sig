signature CODEGEN =
sig
	type bigraph

	val generate : bigraph -> bigraph list list -> string	
end
