type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

fun eof() = Tokens.EOF(0,0)

val tliteral = ref ""
val tlstart = ref 0

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos 

fun err(p1,p2) = ErrorMsg.error p1

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

<LOPE>"{"		=> (Tokens.LBR(yypos,yypos+1));
<LOPE>"}"		=> (Tokens.RBR(yypos,yypos+1));
<LOPE>{ident}	=> (Tokens.IDENT(yytext,yypos,yypos+size yytext));
<LOPE>"(*"		=> (YYBEGIN COMMENT; continue());

<COMMENT>"*)"		=> (YYBEGIN LOPE; continue());
<COMMENT>{eol}		=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT>.		=> (continue());

<LOPE>{eol}		=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<LOPE>{ws}*		=> (continue());
<LOPE>.			=> (Tokens.ERROR(yypos,yypos+size yytext));

