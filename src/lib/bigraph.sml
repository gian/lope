(*****************************************************************************
 * Lope Programming Language
 * Copyright (C) 2009 Gian Perrone
 *****************************************************************************)
signature BIGRAPH =
sig
	type label
	datatype ty = TyTuple of ty list
	            | TyCon of ty * ty
				| TyName of string
				| TyPoly of string
				| TyUnknown
				| TyVar of int
				| TyComp of ty * ty list

	exception BigraphStructureException of string
	exception BigraphLinkException of string
	exception BigraphNameException of string

	type 'a node

	datatype link_face = NamedFace of label
	                   | TypedFace of ty
					   | NumberedFace of int
					   | NullFace

	datatype 'a bigraph = Bigraph of 'a node ref
                            | Empty

	datatype lope_control = NodeControl of label * ty
                          | AnonControl of ty
                          | ParamNodeControl of label * ty * ((label * ty) list)
						  | SiteControl of label * ty
						  | WildControl of ty
						  | DataControl of lope_data
		 and lope_data	  = IntData of int
		                  | StringData of string
						  | VarData of label * ty
						  | UnitData
	
	val empty : 'a bigraph

	val unbox : lope_control bigraph -> lope_control node

	val ty_name : ty -> string
	val name : lope_control bigraph -> string

	(* Accessors *)
	val parent : lope_control bigraph -> lope_control bigraph
	val control : lope_control bigraph -> lope_control
	val children : lope_control bigraph -> lope_control bigraph list
	val siblings : lope_control bigraph -> lope_control bigraph list
	val links : lope_control bigraph -> (link_face * lope_control bigraph * link_face) list
	val params : lope_control bigraph -> (label * ty) list
	val symtab : lope_control bigraph -> lope_control bigraph Symtab.symtab

	val new : lope_control -> lope_control bigraph
	val add_child : lope_control bigraph -> lope_control bigraph -> unit
	val delete_child : lope_control bigraph -> lope_control bigraph -> unit

	(* Links *)
	val add_link : 'a bigraph -> link_face -> 'a bigraph -> link_face -> unit 
	val link_targets : 'a bigraph -> 'a bigraph list
	val delete_link : 'a bigraph -> link_face -> 'a bigraph -> link_face -> unit

	(* Names *)
	val get_name : lope_control bigraph -> label -> lope_control bigraph
	val insert_name : lope_control bigraph -> label -> lope_control bigraph -> unit

	(* Pretty printer *)
	val to_string : lope_control bigraph -> string

end

structure Bigraph : BIGRAPH =
struct
	type label = string
	datatype ty = TyTuple of ty list
	            | TyCon of ty * ty
				| TyName of string
				| TyPoly of string
				| TyUnknown
				| TyVar of int
				| TyComp of ty * ty list

	exception BigraphStructureException of string
	exception BigraphLinkException of string
	exception BigraphNameException of string

	datatype link_face = NamedFace of label
	                   | TypedFace of ty
					   | NumberedFace of int
					   | NullFace
	
	datatype 'a bigraph = Bigraph of 'a node ref
                            | Empty
	withtype 'a node = {control : 'a, 
					 	children : 'a bigraph list, 
						parent : 'a bigraph, 
						links : (link_face  * 'a bigraph * link_face) list, 
						symtab : 'a bigraph Symtab.symtab} 


	datatype lope_control = NodeControl of label * ty
                          | AnonControl of ty
                          | ParamNodeControl of label * ty * ((label * ty) list)
						  | SiteControl of label * ty
						  | WildControl of ty
						  | DataControl of lope_data
		 and lope_data	  = IntData of int
		                  | StringData of string
						  | VarData of label * ty
						  | UnitData
	val empty = Empty

	fun unbox (Bigraph (ref b)) = b
      | unbox _ = raise (BigraphStructureException "Attempted to unbox empty bigraph")

	fun parent b = #parent (unbox b)

	fun control b = #control (unbox b)

	fun children b = #children (unbox b)

	fun siblings (Bigraph b) = List.filter (fn (Bigraph x) => x <> b) (children (parent (Bigraph b)))

	fun links b = #links (unbox b)

	fun symtab b = #symtab (unbox b)

	fun ty_name' (TyName n) = n
	  | ty_name' (TyTuple l) = String.concatWith " * " (map ty_name l)
	  | ty_name' (TyPoly n) = "'" ^ n
	  | ty_name' (TyCon (n1, n2)) = ty_name n1 ^ " " ^ ty_name n2
	  | ty_name' (TyUnknown) = "???"
	  | ty_name' (TyVar i) = "?X" ^ Int.toString i
	  | ty_name' (TyComp (t, [])) = ty_name' t
	  | ty_name' (TyComp (t, l)) = ty_name' t ^ "{" ^ (String.concatWith "," (map ty_name' l)) ^ "}"
	and ty_name x = (Debug.debug 2 "ty_name\n"; ty_name' x)

	fun data_ty_name (IntData i) = Int.toString i ^ " : int"
	  | data_ty_name (StringData d) = "\"" ^ d ^ "\" : string"
	  | data_ty_name (VarData (n,t)) = n ^ " : " ^ ty_name t
	  | data_ty_name (UnitData) = "() : unit"

	fun name k = (fn (AnonControl t) => ": " ^ ty_name t
                       | (NodeControl (l,t)) => l ^ " : " ^ ty_name t
                       | (ParamNodeControl (l,t,p)) => l ^ " : " ^ ty_name t ^ " (...)"
					   | (SiteControl (l,t)) => l ^ " : " ^ ty_name t ^ " site" 
					   | (WildControl t) => "_ : " ^ ty_name t
					   | (DataControl k) => data_ty_name k)
				(control k)

	local
		fun indent 0 s = s
      	  | indent n s = "  " ^ indent (n-1) s

		fun to_string_h n Empty = "(empty)"
		  | to_string_h n b = 
		  let
		  	val mindent = indent n
		  in
		  	"\n- " ^ mindent (name b) ^ " {" ^ mindent (String.concatWith "\n" (map (to_string_h (n+1)) (children b))) ^ "\n" ^ mindent "  }\n"
		  end
	in
		val to_string = to_string_h 0
	end

	fun params k = (fn (ParamNodeControl (l,t,p)) => p
	                 | _ => []) (control k)

	fun new (control : lope_control) = Bigraph (ref {control = control, 
													 parent = empty, 
													 children = [], 
													 links = [] : (link_face * lope_control bigraph * link_face) list, 
													 symtab = Symtab.new Empty})

	fun get_name Empty n = raise BigraphNameException ("Name '" ^ n ^ "' not found")
	  | get_name b n = (Symtab.get (symtab b) n handle NotFound => get_name (parent b) n)

	fun insert_name Empty n v = raise BigraphNameException ("Cannot insert name '" ^ n ^ "' into empty bigraph!") 
	  | insert_name b n v = (Debug.debug 2 ("INSERT: " ^ n ^ " into " ^ (name b) ^ "\n"); Symtab.insert (symtab b) n v)

	fun add_child (parent as (Bigraph (p as ref k))) (child as (Bigraph (c as ref l))) =
		let
			val _ = Debug.debug 2 ("add_child: " ^ name parent ^ " parent of " ^ name child ^ "\n")
			val _ = p := {control = #control k, 
				parent = #parent k, 
				children = #children k @ [child],
				links = #links k, 
				symtab = #symtab k}
			val _ = Symtab.set_parent (#symtab k) parent
			val _ = (case (#control l) of (NodeControl (n,_)) => insert_name parent n child 
			 					 | (ParamNodeControl (n,_,_)) => insert_name parent n child
			 					 | (SiteControl (n,_)) => insert_name parent n child
								 | _ => ())
			 val _ = c := {control = #control l,
			 	parent = parent, 
				children = #children l, 
				links = #links k, 
				symtab = #symtab k}
		in
			()
		end
	  | add_child p Empty = ()
	  | add_child p q = raise BigraphStructureException ("Attempting to add a child to a non-node bigraph: " ^ to_string p ^ "\n\n" ^ to_string q)

	fun delete_child (parent as (Bigraph (p as ref k))) (child as (Bigraph (c as ref l))) =
		(p := {control = #control k, 
				parent = #parent k, 
				children = List.filter (fn (Bigraph x) => x <> c) (#children k), 
				links = #links k, 
				symtab = #symtab k} ;
			 Symtab.set_parent (#symtab k) Empty;
	         c := {control = #control l, 
			    parent = Empty, 
				children = #children l, 
				links = #links k, 
				symtab = #symtab k})
	  | delete_child _ _ = raise BigraphStructureException ("Attempted to delete child from invalid bigraph")

	fun add_link (s1 as (Bigraph (b1 as ref k1))) face1 (s2 as (Bigraph (b2 as ref k2))) face2 = 
		(b1 := {control = #control k1,
			       parent = #parent k1,
			       children = #children k1,
			       links = (face1,s2,face2) :: #links k1,
				   symtab = #symtab k1} ;
		 b2 := {control = #control k2,
			       parent = #parent k2,
			       children = #children k2,
			       links = (face2,s1,face1) :: #links k2,
				   symtab = #symtab k2})
	  | add_link _ _ _ _ = raise BigraphLinkException ("Attempted to add a link to invalid bigraphs")

	fun delete_link (s1 as (Bigraph (b1 as ref k1))) face1 (s2 as (Bigraph (b2 as ref k2))) face2 =
		(b1 := {control = #control k1,
			       parent = #parent k1,
			       children = #children k1,
			       links = List.filter (fn (_,Bigraph l,_) => l <> b2) (#links k1),
				   symtab = #symtab k1} ;
		 b2 := {control = #control k2,
			       parent = #parent k2,
			       children = #children k2,
			       links = List.filter (fn (_,Bigraph l,_) => l <> b1) (#links k2),
				   symtab = #symtab k2})
	  | delete_link _ _ _ _ = raise BigraphLinkException ("Attempted to delete link from invalid bigraph")

	fun link_targets b = map (fn (_,k,_) => k) (links b)


end


