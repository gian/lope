(*****************************************************************************
 * Lope Programming Language
 * Copyright (C) 2009 Gian Perrone
 *****************************************************************************)
signature BIGRAPH =
sig
	type label
	type ty


	exception BigraphStructureException of string

	type 'a node
  	
	datatype 'a bigraph = Bigraph of 'a node ref
                            | Empty

	datatype lope_control = NodeControl of label * ty
                              | AnonControl of ty
                              | ParamNodeControl of label * ty * ((label * ty) list)
	
	val empty : 'a bigraph

	val unbox : 'a bigraph -> 'a node

	val name : lope_control bigraph -> string

	(* Accessors *)
	val parent : lope_control bigraph -> lope_control bigraph
	val control : lope_control bigraph -> lope_control
	val children : lope_control bigraph -> lope_control bigraph list
	val siblings : lope_control bigraph -> lope_control bigraph list
	val links : lope_control bigraph -> (label * ty * lope_control bigraph) list

	val new : 'a -> 'a bigraph
	val add_child : lope_control bigraph -> lope_control bigraph -> unit
	val delete_child : lope_control bigraph -> lope_control bigraph -> unit

	(* Links *)
	val add_link : 'a bigraph -> label -> ty -> 'a bigraph -> unit 
	val link_targets : 'a bigraph -> 'a bigraph list
	val delete_link : 'a bigraph -> 'a bigraph -> unit

end

structure Bigraph : BIGRAPH =
struct
	type label = string
	type ty = string

	exception BigraphStructureException of string
	exception BigraphLinkException of string

	datatype 'a bigraph = Bigraph of 'a node ref
                            | Empty
	withtype 'a node = {control : 'a, children : 'a bigraph list, parent : 'a bigraph, links : (label * ty  * 'a bigraph) list} 


	datatype lope_control = NodeControl of label * ty
                              | AnonControl of ty
                              | ParamNodeControl of label * ty * ((label * ty) list)

	val empty = Empty

	fun unbox (Bigraph (ref b)) = b
          | unbox _ = raise (BigraphStructureException "Attempted to unbox empty bigraph")



	fun parent b = #parent (unbox b)

	fun control b = #control (unbox b)

	fun children b = #children (unbox b)

	fun siblings b = List.filter (fn x => unbox x <> unbox b) (children (parent b))

	fun links b = #links (unbox b)

	fun name k = (fn (AnonControl t) => ": " ^ t
                       | (NodeControl (l,t)) => l ^ " : " ^ t
                       | (ParamNodeControl (l,t,p)) => l ^ " : " ^ t ^ "<...>") 
				(control k)

	fun new (control : 'a) = Bigraph (ref {control = control, parent = empty, children = [], links = [] : (label * ty * 'a bigraph) list})

	fun add_child (parent as (Bigraph (p as ref k))) (child as (Bigraph (c as ref l))) =
		(p := {control = #control k, parent = #parent k, children = child :: #children k, links = #links k} ;
	         c := {control = #control l, parent = parent, children = #children l, links = #links k})
	  | add_child _ _ = raise BigraphStructureException ("Attempting to add a child to a non-node bigraph")

	fun delete_child (parent as (Bigraph (p as ref k))) (child as (Bigraph (c as ref l))) =
		(p := {control = #control k, parent = #parent k, children = List.filter (fn (Bigraph x) => x <> c) (#children k), links = #links k} ;
	         c := {control = #control l, parent = Empty, children = #children l, links = #links k})
	  | delete_child _ _ = raise BigraphStructureException ("Attempted to delete child from invalid bigraph")

	fun add_link (s1 as (Bigraph (b1 as ref k1))) l ty (s2 as (Bigraph (b2 as ref k2))) = 
		(b1 := {control = #control k1,
			       parent = #parent k1,
			       children = #children k1,
			       links = (l,ty,s2) :: #links k1} ;
		 b2 := {control = #control k2,
			       parent = #parent k2,
			       children = #children k2,
			       links = (l,ty,s1) :: #links k2})
	  | add_link _ l ty _ = raise BigraphLinkException ("Attempted to add a link " ^ l ^ " : " ^ ty ^ " to invalid bigraphs")

	fun delete_link (s1 as (Bigraph (b1 as ref k1))) (s2 as (Bigraph (b2 as ref k2))) =
		(b1 := {control = #control k1,
			       parent = #parent k1,
			       children = #children k1,
			       links = List.filter (fn (_,_,Bigraph l) => l <> b2) (#links k1)} ;
		 b2 := {control = #control k2,
			       parent = #parent k2,
			       children = #children k2,
			       links = List.filter (fn (_,_,Bigraph l) => l <> b1) (#links k2)})
	  | delete_link _ _ = raise BigraphLinkException ("Attempted to delete link from invalid bigraph")

	fun link_targets b = map (fn (l,t,k) => k) (links b)
end


