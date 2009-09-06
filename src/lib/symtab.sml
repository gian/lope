signature SYMTAB =
sig
	type 'a symtab

	exception NotFound
	exception EmptyTable of string

	val new : 'a -> 'a symtab (* Must pass 'self' to new *)
	val insert : 'a symtab -> string -> 'a -> unit
	val get : 'a symtab -> string -> 'a
	val set_parent : 'a symtab -> 'a -> unit

	val empty : 'a symtab
end

structure Symtab : SYMTAB =
struct
	datatype 'a symtab = SymbolTable of (string,'a) HashTable.hash_table
	                   | Empty

	exception NotFound
	exception EmptyTable of string
	
	fun new (k:'a) = 
		let
			val ht : (string,'a) HashTable.hash_table = HashTable.mkTable (HashString.hashString, op=) (3, NotFound)
			val _ = HashTable.insert ht ("_self_", k)
		in
			SymbolTable ht
		end

	fun insert (SymbolTable s) n v = HashTable.insert s (n, v)

	fun get (SymbolTable s) k = HashTable.lookup s k
	  | get Empty k = raise EmptyTable ("Attempted to lookup '" ^ k ^ "' in an empty symbol table")

	fun set_parent (SymbolTable s) v = HashTable.insert s ("_parent_", v)
	  | set_parent Empty _ = ()

	val empty = Empty
end


