signature PARSE =
sig
	val parse : string -> lope_control bigraph
	val parse_string : string -> lope_control bigraph
end

structure Parse : PARSE =
struct 
  structure LmlLrVals = LmlLrValsFun(structure Token = LrParser.Token)
  structure Lex = LmlLexFun(structure Tokens = LmlLrVals.Tokens)
  structure LmlP = Join(structure ParserData = LmlLrVals.ParserData
			structure Lex=Lex
			structure LrParser = LrParser)

  fun parseerror(s,l,p1,p2) = print ("Parse Error at line " ^ Int.toString l ^ ":" ^ (Int.toString p1) ^ "-" ^ (Int.toString p2) ^ ": " ^ s ^ "\n") 

  fun parse_string inp = let
	  val file = TextIO.openString inp
	  fun get _ = TextIO.input file
	  val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
	  val (bg,_) = LmlP.parse(30,lexer,parseerror,())
       in 
	  bg
      end  

  fun parse filename = let
	  val file = TextIO.openIn filename
	  fun get _ = TextIO.input file
	  val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
	  val (bg,_) = LmlP.parse(30,lexer,parseerror,())
       in TextIO.closeIn file;
	    bg
      end  
end


