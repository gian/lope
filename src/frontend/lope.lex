type lineno = int
type pos = int
type svalue = Tokens.svalue
type ('a,'b,'c) token = ('a,'b,'c) Tokens.token
type lexresult = (lineno,svalue,pos) token

fun eof() = Tokens.EOF(0,0,0)

val tliteral = ref ""
val tlstart = ref 0

val lineno = ref 1

%%
%header (functor LopeLexFun(structure Tokens : Lope_TOKENS));

%s LOPE LINECOMMENT COMMENT STRINGLIT;

digits=[0-9]+;
real=([0-9]+"."[0-9]*)|([0-9]*"."[0-9]+);
ident=['a-zA-Z_][a-zA-Z0-9_']*;
stringlit="\""([^\"]*)"\"";
ws=[\ \t];
eol=[\n\r];
%%
<INITIAL>{ws}*		=> (YYBEGIN LOPE; continue());

<LOPE>"(*"		=> (YYBEGIN COMMENT; continue());

<COMMENT>"*)"		=> (YYBEGIN LOPE; continue());
<COMMENT>{eol}		=> (lineno := !lineno + 1; continue());
<COMMENT>.		=> (continue());

<LOPE>{eol}		=> (lineno := !lineno + 1; continue());
<LOPE>{ws}*		=> (continue());
<LOPE>.			=> (Tokens.ERROR(!lineno,yypos,yypos+size yytext));

