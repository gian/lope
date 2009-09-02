(*****************************************************************************
 * Lope Programming Language
 * Copyright (C) 2009 Gian Perrone
 *****************************************************************************)
signature BIGRAPH =
sig
  type label
  type ty

  datatype 'a bigraph = Bigraph of 'a node ref
       and 'a node = Node of {control : 'a, children : 'a bigraph list, links : 'a  * 'a bigraph list}
                   | Site of 'a

  datatype lope_control = NodeControl of label * ty
                        | LinkControl of label * ty
                        | AnonControl of ty
                        | ParamNodeControl of label * ty * ((label * ty) list)
end

structure Bigraph : BIGRAPH =
struct
	type label = string
	type ty = string
end

