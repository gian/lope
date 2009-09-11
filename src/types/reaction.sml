signature REACTION =
sig
	val reactions : Bigraph.lope_control Bigraph.bigraph -> Bigraph.lope_control Bigraph.bigraph list

	val interference : Bigraph.lope_control Bigraph.bigraph list -> (Bigraph.lope_control Bigraph.bigraph * Bigraph.lope_control Bigraph.bigraph) list
	val slice : (Bigraph.lope_control Bigraph.bigraph * Bigraph.lope_control Bigraph.bigraph) list -> Bigraph.lope_control Bigraph.bigraph list list
end

structure Reaction : REACTION =
struct
	structure B = Bigraph
	structure T = Types

	fun filter_reaction (l, B.Empty) = l
	  | filter_reaction (l, b as (B.Bigraph _)) =
	  	(case B.control b of B.NodeControl (n,B.TyArrow (_,_)) => l @ [b]
		                   | B.AnonControl (B.TyArrow (_,_)) => l @ [b]
						   | B.ParamNodeControl (_,B.TyArrow (_,_),_) => l @ [b]
						   | _ => l)

	fun reactions b = B.fold filter_reaction [] b  

	fun interference [] = []
	  | interference r = 
	  	let
			(* This is NOT commutative!
			 * interferes b1 b2 implies reactum of b1 overlaps with redex of b2.
			 * NO check is made against the overlap of the reactum of b2 with the
			 * redex of b1
			 *)
			fun interferes (b1,b2) =
				let
					val B.TyArrow (b1redexty,b1reactumty) = B.ty b1
					val B.TyArrow (b2redexty,b2reactumty') = B.ty b2
				in
					T.overlaps b1reactumty b2redexty 
				end

			local  
				fun crossProduct_aux f (h::t) l result = crossProduct_aux f t l ((List.mapPartial (f h) l) @ result)  
				  | crossProduct_aux f [] l result = result  
			in  
				fun crossProduct f (a, b) = crossProduct_aux f (List.rev a) b []  
			end  
		in
			List.filter interferes (crossProduct (fn x => fn y => if x = y then NONE else SOME (x,y)) (r,r))
		end
		
	(*TODO: Start with any reaction and build the set of the things that interfere with it,
	   recursing up until you reach a cycle or a root (something affected by nothing) *)
	fun slice s = 
		let
			fun has_edge j k = List.exists (fn (j',k') => j = j' andalso k = k')
			fun preset j = List.filter (fn (k,l) => k = j)
			fun postset j = List.filter (fn (k,l) => l = j)
		
		in
			[]
		end

		
end
