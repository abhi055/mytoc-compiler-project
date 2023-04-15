%{
#include <cstdio>
#include <string>
#include "node.hpp"
#include "parser.hpp"

%}

%option noyywrap
%option yylineno

%%

[ \t\n]                         ;
"//".*$                         ;
"extern"                        {return TEXTERN;}
"return"                        {return TRETURN;}
"int"                           {return TINT_T;}
"if"                            {return TIF;}

	/* ABHI*****/
	/*--add return type for void & double --*/

"void"							{return VOID;}
"double"						{return DOUBLE;}
	/**************************************************************/


"else"                          {return TELSE;}
[a-zA-Z_][a-zA-Z0-9_]*          {return TIDENTIFIER;}
[0-9]+                          {yylval.value=std::stoi(yytext);return TINTEGER;}

	/*--Vatsal--*/
	/*-- added token for Number with fractional part --*/
[0-9]+\.[0-9]*			{yylval.value=std::stod(yytext);return TDOUBLE;}
	/*-- Digit Character class followed with '+' ensures there exist at least 1 digit. Punctuation dot. 
	     Digit character class followed with '*' indicate that after fractional part there can be No digit or any number of digit.
	  -- In yacc, the token (double type number here) read by yyin stream contained in yytext and 
	     associated actual value being store in yylval using stod (string to double) conversion function.
	  -- returned defined value "TDOUBLE" indicate obtained value is of double type.
	  -- ****************************************************************************************************************************/


"="                             {return TASSIGN;}
"=="                            {yylval.value=EQ;return TCOP1;}
"!="                            {yylval.value=NE;return TCOP1;}
"<"                             {yylval.value=LT;return TCOP1;}
"<="                            {yylval.value=LE;return TCOP1;}
">"                             {yylval.value=GT;return TCOP1;}
">="                            {yylval.value=GE;return TCOP1;}

"("                             {return TLPAREN;}
")"                             {return TRPAREN;}
"{"                             {return TLBRACE;}
"}"                             {return TRBRACE;}

","                             {return TCOMMA;}
";"                             {return TSEMICOL;}

"+"                             {yylval.value=PLUS; return TBOP2;}
"-"                             {yylval.value=MINUS;return TBOP2;}
"*"                             {yylval.value=MUL;  return TBOP3;}
"/"                             {yylval.value=DIV;  return TBOP3;}

.                               {fprintf(stderr,"Unknown token '%s' in line %d!\n",yytext,yylineno); yyterminate();}

%%
