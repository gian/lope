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
ident=['a-zA-Z_][a-zA-Z0-9_'\.]*;
siteident=\$['a-zA-Z_][a-zA-Z0-9_'\.]*;
stringlit="\""([^\"]*)"\"";
ws=[\ \t];
eol=[\n\r];
%%
<INITIAL>{ws}*		=> (YYBEGIN LOPE; continue());

<LOPE>"{"		=> (Tokens.LBR(yypos,yypos+1));
<LOPE>"}"		=> (Tokens.RBR(yypos,yypos+1));
<LOPE>"("		=> (Tokens.LPAR(yypos,yypos+1));
<LOPE>")"		=> (Tokens.RPAR(yypos,yypos+1));
<LOPE>":"		=> (Tokens.COLON(yypos,yypos+1));
<LOPE>";"		=> (Tokens.SEMI(yypos,yypos+1));
<LOPE>","		=> (Tokens.COMMA(yypos,yypos+1));
<LOPE>"reaction" => (Tokens.REACTION(yypos, yypos+8));
<LOPE>"redex" => (Tokens.REDEX(yypos, yypos+5));
<LOPE>"reactum" => (Tokens.REACTUM(yypos, yypos+7));
<LOPE>"link" => (Tokens.LINK(yypos, yypos+4));
<LOPE>"<->" => (Tokens.ILINK(yypos, yypos+3));
<LOPE>"<" => (Tokens.LT(yypos, yypos+1));
<LOPE>">" => (Tokens.GT(yypos, yypos+1));
<LOPE>{ident}	=> (Tokens.IDENT(yytext,yypos,yypos+size yytext));
<LOPE>{siteident} => (Tokens.SITEIDENT(yytext,yypos,yypos+size yytext));
<LOPE>"(*"		=> (YYBEGIN COMMENT; continue());

<COMMENT>"*)"		=> (YYBEGIN LOPE; continue());
<COMMENT>{eol}		=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT>.		=> (continue());

<LOPE>{eol}		=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<LOPE>{ws}*		=> (continue());
<LOPE>.			=> (Tokens.ERROR(yypos,yypos+size yytext));

