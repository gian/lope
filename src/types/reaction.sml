signature REACTION =
sig
	val reactions : Bigraph.lope_control Bigraph.bigraph -> Bigraph.lope_control Bigraph.bigraph list

	val interference : Bigraph.lope_control Bigraph.bigraph list -> (Bigraph.lope_control Bigraph.bigraph * Bigraph.lope_control Bigraph.bigraph) list
	val slice :  Bigraph.lope_control Bigraph.bigraph list -> (Bigraph.lope_control Bigraph.bigraph * Bigraph.lope_control Bigraph.bigraph) list -> Bigraph.lope_control Bigraph.bigraph list list
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
	fun slice r s = 
		let
			fun has_edge j k = List.exists (fn (j',k') => j = j' andalso k = k')
			fun preset j l = List.map (fn (x,y) => y) (List.filter (fn (k,_) => k = j) l)
			fun postset j = List.filter (fn (_,l) => l = j)

			fun member(x,ys) = List.exists (fn y => x = y) ys
			fun insert(x,ys) = if member(x,ys) then ys else x::ys;
			fun union(xs, ys) = List.foldl insert ys xs; 
			fun inter(xs,ys) = List.filter (fn x => member(x,ys)) xs
		
			fun mkuniq [] = []
         	  | mkuniq (h::t) = h :: (List.filter (fn x => x <> h) (mkuniq t))


			fun trace [] c = []
			  | trace m c = 
			  	let 
					fun rmopts a b = List.filter (fn (_,x) => not (List.exists (fn y => x = y) a)) b
					val p = (foldl (fn (l,n) => (preset l c) @ n) [] m)

				in
					mkuniq (p @ trace p (rmopts p c))
				end

			local
		  		fun merge'(t1, t2) = if inter(t1,t2) <> [] then mkuniq (t1 @ t2) else t2
				fun merge_o t = mkuniq (List.foldl (fn (x,y) => map (fn z => merge' (x,z)) y) t t)
				fun merge_c l1 l2 = if l1 = l2 then l1 else merge_c l2 (merge_o l2)
			in
				fun merge t = merge_c [] t
			end

			val m = merge (map (fn (n,m) => (trace [n] s)) s)

			val r' = map (fn a => [a]) (List.filter (fn x => List.all (fn (y,z) => x = y orelse x = z) s) r)
		in
			m @ r' 
		end
		
end
