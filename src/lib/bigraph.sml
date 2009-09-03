(*****************************************************************************
 * Lope Programming Language
 * Copyright (C) 2009 Gian Perrone
 *****************************************************************************)
signature BIGRAPH =
sig
	type label
	type ty

  	datatype 'a bigraph = Bigraph of 'a node ref
	and 'a node = Node of {control : 'a, children : 'a bigraph list, parent : 'a bigraph, links : 'a  * 'a bigraph list}
                    | Site of 'a
                    | Empty

	datatype lope_control = NodeControl of label * ty
                              | LinkControl of label * ty
                              | AnonControl of ty
                              | ParamNodeControl of label * ty * ((label * ty) list)
	
	val empty : lope_control bigraph

	val siblings : lope_control bigraph -> lope_control bigraph list
	val parent : lope_control bigraph -> lope_control bigraph

end

structure Bigraph : BIGRAPH =
struct
	type label = string
	type ty = string


	datatype 'a bigraph = Bigraph of 'a node ref
	and 'a node = Node of {control : 'a, children : 'a bigraph list, parent : 'a bigraph, links : 'a  * 'a bigraph list}
                    | Site of 'a
                    | Empty


	datatype lope_control = NodeControl of label * ty
                              | LinkControl of label * ty
                              | AnonControl of ty
                              | ParamNodeControl of label * ty * ((label * ty) list)

	val empty = Bigraph (ref (Empty : lope_control node))

	fun siblings (Bigraph (ref (Node k))) = #children k
          | siblings (Bigraph _) = []

	fun parent (Bigraph (ref (Node k))) = #parent k
          | parent (Bigraph _) = empty 
end

