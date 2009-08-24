(*****************************************************************************
 * Lope Programming Language
 * Copyright (C) 2009 Gian Perrone
 *****************************************************************************)
signature BIGRAPH =
sig
  datatype 'a node = Node of 'a * 'a node list
                   | Site of 'a

  datatype 'a link = Link of 'a node ref * 'a node ref

  type 'a bigraph = 'a node list * 'a link list

  datatype node_op = Add
                   | Mult
	 	   | Div
		   | Sub
 
  datatype node_data = DataNode of data_node_type
                     | OpNode of node_data node * node_op * node_data node
		     | NameNode of string * node_data node
   
end



