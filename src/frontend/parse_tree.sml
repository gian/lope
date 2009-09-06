structure ParseTree =
struct
	structure B = Bigraph

	datatype parse_tree = Node of B.lope_control B.bigraph * parse_tree list
	                    | Link of B.label * link_val * link_val
	and link_val = LinkMask of link_val * link_mask list
	             | LinkAnon of parse_tree
				 | LinkName of B.label
				 | LinkType of B.ty
	and link_mask = MaskInt of int
	              | MaskName of B.label
				  | MaskType of B.ty

	fun to_bigraph par (Node (B.Empty, [])) = B.empty
	  | to_bigraph par (Node (B.Empty, l)) =
	  	let
			val _ = print "Creating World\n"
			val world = B.new (B.NodeControl ("World", "world"))
			val _ = (List.app (fn x => (B.add_child world (to_bigraph world x))) l)
		in
			world
		end
	  | to_bigraph par (Node (b, l)) = 
	  	let
			val _ = print ("Creating node: " ^ B.name b ^ "\n")
		in
			(List.app (fn x => (B.add_child b (to_bigraph b x))) l; b)
		end
	  | to_bigraph par (Link (l, LinkName b1, LinkName b2)) = 
	  	let
			val _ = print ("Looking in context " ^ (B.name par) ^ " for " ^ b1 ^ " and " ^ b2 ^ "\n")
			val b1' = B.get_name par b1
			val b2' = B.get_name par b2
			val _ = B.add_link b1' l "" b2'
		in
			B.empty
		end
	  | to_bigraph par (Link (l, b1, b2)) = (print ("Link " ^ l ^ "\n"); B.empty)

end
