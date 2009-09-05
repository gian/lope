structure ParseTree =
struct
	structure B = Bigraph

	datatype parse_tree = Node of B.lope_control B.bigraph * parse_tree list
	                    | Link of B.label * link_val * link_val
	and link_val = AllInterface of parse_tree

	fun to_bigraph (Node (B.Empty, [])) = B.empty
	  | to_bigraph (Node (B.Empty, l)) =
	  	let
			val world = B.new (B.NodeControl ("World", "world"))
			val _ = List.app (B.add_child world) (List.map to_bigraph l)
		in
			world
		end
	  | to_bigraph (Node (b, l)) = (List.app (B.add_child b) (List.map to_bigraph l); b)

end
