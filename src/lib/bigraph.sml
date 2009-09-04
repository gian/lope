(*****************************************************************************
 * Lope Programming Language
 * Copyright (C) 2009 Gian Perrone
 *****************************************************************************)
signature BIGRAPH =
sig
	type label
	type ty

	exception BigraphStructureException of string

  	datatype 'a bigraph = Bigraph of 'a node ref
                            | Empty
	and 'a node = Node of {control : 'a, children : 'a bigraph list, parent : 'a bigraph, links : ('a  * 'a bigraph) list}
                    | Site of 'a

	datatype lope_control = NodeControl of label * ty
                              | LinkControl of label * ty
                              | AnonControl of ty
                              | ParamNodeControl of label * ty * ((label * ty) list)
	
	val empty : 'a bigraph

	val parent : lope_control bigraph -> lope_control bigraph
	val control : lope_control bigraph -> lope_control
	val children : lope_control bigraph -> lope_control bigraph list
	val siblings : lope_control bigraph -> lope_control bigraph list

	val new : 'a -> 'a bigraph
	val add_child : lope_control bigraph -> lope_control bigraph -> unit
	
	val unbox : 'a bigraph -> 'a node
end

structure Bigraph : BIGRAPH =
struct
	type label = string
	type ty = string

	exception BigraphStructureException of string

	datatype 'a bigraph = Bigraph of 'a node ref
                            | Empty
	and 'a node = Node of {control : 'a, children : 'a bigraph list, parent : 'a bigraph, links : ('a  * 'a bigraph) list}
                    | Site of 'a


	datatype lope_control = NodeControl of label * ty
                              | LinkControl of label * ty
                              | AnonControl of ty
                              | ParamNodeControl of label * ty * ((label * ty) list)

	val empty = Empty


	fun parent (Bigraph (ref (Node k))) = #parent k
          | parent (Bigraph _) = empty 

	fun control (Bigraph (ref (Node k))) = #control k
          | control (Bigraph _) = raise (BigraphStructureException "Accessing control of uncontrolled node")

	fun children (Bigraph (ref (Node k))) = #children k
          | children _ = []

	fun siblings (b as (Bigraph (ref (Node k)))) = List.filter (fn x => x <> b) (children (parent b))
          | siblings (Bigraph _) = []

	fun new (control : 'a) = Bigraph (ref (Node {control = control, parent = empty, children = [], links = [] : ('a * 'a bigraph) list}))

	fun add_child (parent as (Bigraph (p as ref (Node k)))) (child as (Bigraph (c as ref (Node l)))) =
		(p := Node {control = #control k, parent = #parent k, children = child :: #children k, links = #links k} ;
	         c := Node {control = #control l, parent = parent, children = #children l, links = #links k})
	  | add_child _ _ = raise BigraphStructureException ("Attempting to add a child to a non-node bigraph")

	fun unbox (Bigraph (ref n)) = n
          | unbox _ = raise BigraphStructureException ("Attempting to unbox an empty bigraph")
end


