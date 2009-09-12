structure CCodeGen : CODEGEN =
struct
	structure B = Bigraph

	type bigraph = Bigraph.lope_control Bigraph.bigraph

	fun filter_reactions (l, b) = 
		(case B.ty b of B.TyArrow (_,_) => l 
		              | k => b :: l)

	fun collect_names b = B.fold filter_reactions [] b  

	fun generate world sliceset = 
		let
			val _ = print ("Generate:\n" ^ (String.concatWith "\n" (map B.name (collect_names world))))
		in
			""
		end
end
