structure ParseTree =
struct
	structure B = Bigraph
	structure T = Types

	datatype parse_tree = Node of B.lope_control B.bigraph * parse_tree list
	                    | Link of B.label * link_val * link_val
						| TypeDef of B.ty * B.ty
						| ValDef of B.label * B.ty * parse_tree
						| Instantiate of B.label * parse_tree list
	and link_val = LinkMask of link_val * link_mask
	             | LinkAnon of parse_tree
				 | LinkName of B.label
				 | LinkType of B.ty
	and link_mask = MaskInt of int
	              | MaskName of B.label
				  | MaskType of B.ty
				  | MaskList of link_mask list
				  | MaskNull

	fun to_bigraph par (Node (B.Empty, [])) = B.empty
	  | to_bigraph par (Node (B.Empty, l)) =
	  	let
			val _ = Debug.debug 3 "Creating World\n"
			val world = B.new (B.NodeControl ("World", B.TyUnknown))
			val _ = (List.app (fn x => (B.add_child world (to_bigraph world x))) l)
		in
			world
		end
	  | to_bigraph par (Node (b, l)) = 
	  	let
			val _ = Debug.debug 3 ("Creating node: " ^ B.name b ^ "\n")
		in
			(List.app (fn x => (B.add_child b (to_bigraph b x))) l; b)
		end
	  | to_bigraph par (TypeDef (t1, t2)) = (T.insert_binding t1 t2; B.empty)
      | to_bigraph par (ValDef (n,t,ex)) = 
	    let
			val nb = B.new (B.NodeControl (n,t))
			val _ = B.insert_name par n nb 
			val _ = B.add_child par nb
			val _ = B.add_child nb (to_bigraph nb ex)
		in
			B.empty
		end
      | to_bigraph par (Instantiate (n, params)) = B.empty
	  | to_bigraph par (Link (l, b1, b2)) = 
	  	let
			(* The links are divided into two parts like so:
			 * link bigraph_expr [ <mask_expr> ] <-> bigraph_expr [ <mask_expr> ]
			 *
			 * E.g.: link A <-> B connects all open links of both A and B
			 * link A<0> <-> B<1> connects the link at index 0 of A to B 1
			 * link _ : T<foo> <-> MyT<foo> connects the link named 'foo' of any object of type T to MyT<foo>
			 * link A<:int,k> <-> B<:int,j> connect all links of type int of A to the int typed links of B, and k and j.
			 *)

			fun makeLinkFace (MaskInt i) = B.NumberedFace i
			  | makeLinkFace (MaskName n) = B.NamedFace n
			  | makeLinkFace (MaskType t) = B.TypedFace t
			  | makeLinkFace (MaskNull) = B.NullFace

			fun bigraphSet (LinkMask (b,m)) = [((fn ((nf,msk)::_) => nf) (bigraphSet b), makeLinkFace m)]
			  | bigraphSet (LinkAnon b) = [(to_bigraph par b, B.NullFace)]
			  | bigraphSet (LinkName n) = [(B.get_name par n, B.NullFace)]
			  | bigraphSet (LinkType t) = [] (* TODO *) 

			val (b1',m1) = hd (bigraphSet b1)
			val (b2',m2) = hd (bigraphSet b2)
			val _ = B.add_link b1' m1 b2' m2
		in
			B.empty
	end

end
