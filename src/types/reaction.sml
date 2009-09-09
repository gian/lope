signature REACTION =
sig
	val reactions : Bigraph.lope_control Bigraph.bigraph -> Bigraph.lope_control Bigraph.bigraph list
end

structure Reaction : REACTION =
struct
	structure B = Bigraph

	fun filter_reaction (l, B.Empty) = l
	  | filter_reaction (l, b as (B.Bigraph _)) =
	  	(case B.control b of B.NodeControl (n,B.TyArrow (_,_)) => l @ [b]
		                   | B.AnonControl (B.TyArrow (_,_)) => l @ [b]
						   | B.ParamNodeControl (_,B.TyArrow (_,_),_) => l @ [b]
						   | _ => l)

	fun reactions b = B.fold filter_reaction [] b  
end
