signature OPTIMISER =
sig
	type bigraph

	val optimise : bigraph * bigraph list list -> bigraph * bigraph list list
	
end

structure Optimiser : OPTIMISER =
struct
	type bigraph = Bigraph.lope_control Bigraph.bigraph

	fun optimise (world,sliceset) = (world,sliceset)
end
