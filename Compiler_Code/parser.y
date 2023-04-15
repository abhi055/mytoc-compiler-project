%{
    #include "node.hpp"
    #include <cstdlib>
    #include "tokens.hpp"

    NBlock programBlock; /* the top level root node of our final AST */

    void yyerror(const char *s) { fprintf(stderr,"Error in line %d: %s\n", yylineno,s);exit(1); }
    
%}

/* Represents the many different ways we can access our data */
%union {
    PNODE(Node) node;
    PNODE(NBlock) block;
    PNODE(NExpression) expr;
    PNODE(NStatement) stmt;
    PNODE(NInteger) nint;
    PNODE(NIdentifier) id;
    PNODE(NVariableDeclaration) var_decl;
    PNODE(NVariableList) varlist;
    PNODE(NExpressionList) exprlist;
    PNODE(NStatementList) stmtlist;
    int value;
    /* --Vatsal-- */
    /* double type variable added */
    double value2;
    /* Description: double type variable declared inside YYSTYPE union. To return value of type double via yylval this type needs to declare in this union. */  
}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */

/* -- ABHI -- */
/* token for void type and double type added with TINT_T */

%token TINT_T VOID DOUBLE

/* Description: All new tokens need to declare here in order to identify and accept each specific token by yacc command. */


%token TIDENTIFIER
%token<value> TINTEGER
    /* --Vatsal-- */
    /* token declare which is of double type */
%token <value2> TDOUBLE
    /* Description: New tokens need to declare with it's type(double here) so it can be identify and accept by yacc command. 
				Did  in similar fashion to already pre-declared integer type */
%token TASSIGN
// TxOPy: < x=C: Comparison, x=B: Binary > operator; y=precedence
%token<value> TCOP1
%token TLPAREN TRPAREN TLBRACE TRBRACE
%token TSEMICOL TCOMMA
%token<value> TBOP2 TBOP3
%token<value> TUOP4
%token TRETURN TEXTERN
%token TIF TELSE
%token TNONE

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */
 
    /*-- Vatsal--*/ 
    /*-- "type" added as a non-terminal token which need to pass in node.*/

%type<id> ident type
%type<expr> numeric expr
%type<varlist> func_decl_args
%type<exprlist> call_args
%type<block> program block
%type<stmtlist> stmts
%type<stmt> stmt block_stmt simple_stmt func_decl extern_decl
%type<var_decl> var_decl_with_init var_decl
%type<stmt> if_stmt
%type<expr> comparison var_init


/* Operator precedence for mathematical operators */
%left TCOP1 // EQ NE LT LE GT GE
%left TBOP2 // PLUS MINUS
%left TBOP3 // MUL DIV
%left TUOP4 // UNARY OPERATOR OF HIGHEST PRECEDENCE

%start program

%%

	/*-- Did together, Abhi and Vatsal --*/
	/*-- Changes in Grammar, (Common Description for changes in rules)
	  -- New object (node in tree) created using new defined variadic node (i.e. NEW_PNODE - defined in node.hpp)
	  -- For every new node "type" also passed as argument in NEW_PNODE. (For each applicable rule)	*/

program : stmts { programBlock.statements = *$1; (*$1).clear(); delete $1; }
        ;

stmts : stmt { $$ = new NStatementList();
               $$->push_back($1);
             }
      | stmts stmt { $$=$1;$$->push_back($2); }
      ;

stmt : block_stmt | simple_stmt TSEMICOL
     ;

block_stmt: func_decl | if_stmt
          ;

simple_stmt: var_decl_with_init { $$ = $1; }
         | extern_decl
         | expr { $$ = new NExpressionStatement($1); }
         | TRETURN expr { $$ = NEW_PNODE(NReturnStatement,$2); }
         | TRETURN { $$ = NEW_PNODE(NReturnStatement); }
         ;

// Note: We disallow empty statement blocks here -- actually
// only, because it makes LLVM code generation simpler!
block : TLBRACE stmts TRBRACE
         { $$ = NEW_PNODE(NBlock); $$->statements = *$2; delete $2; }
      ;
      
type: TINT_T { $$ = NEW_PNODE(NIdentifier, "int");} 
      | VOID { $$ = NEW_PNODE(NIdentifier, "void");}
      | DOUBLE { $$ = NEW_PNODE(NIdentifier, "double");}
      ; // INTEGERS and VOID and DOUBLE ALLOWED!

var_init: TASSIGN expr { $$ = $2; } | { $$ = nullptr; } ;
             
var_decl : type ident { $$ = NEW_PNODE(NVariableDeclaration,$2); };

var_decl_with_init: var_decl var_init
                   { $$ = $1; (*$1).assignmentExpr = $2; }
                  ;

extern_decl : TEXTERN type ident TLPAREN func_decl_args TRPAREN
              { $$ = NEW_PNODE(NExternDeclaration,$2,$3,*$5); delete $5;}
            ;

func_decl : type ident TLPAREN func_decl_args TRPAREN block 
            { $$ = NEW_PNODE(NFunctionDeclaration,
               $1,$2,*$4,$6);
                delete $4;
            }
          ;

func_decl_args:  /*blank*/  { $$ = NEW_PNODE(NVariableList); }
          | var_decl { $$ = NEW_PNODE(NVariableList);
                   $$->push_back($1); }
          | func_decl_args TCOMMA var_decl
            { $$=$1;$$->push_back($3);}
          ;

if_stmt: TIF TLPAREN expr TRPAREN block TELSE block
          { $$ = NEW_PNODE(NIfStatement,
            $3,$5,$7);
           }
        | TIF TLPAREN expr TRPAREN block
          { $$ = NEW_PNODE(NIfStatement,$3,$5); }
        ;
        
expr : ident TLPAREN call_args TRPAREN
      { $$ = NEW_PNODE(NFunctionCall,$1,*$3); delete $3;}
     | ident { $$ = $1; /* needed, because expr is type expr, ident is type id */ }
     | numeric
     | expr TBOP3 expr
      { $$ = NEW_PNODE(NBinaryOperator,$1, $2,$3); }
     | TBOP2 expr %prec TUOP4
     { // this might be unary plus or minus (+42 or -42)
       if(PLUS == $1){
            $$ = $2;
      	} else { 
       	    // OK, this is not in general optimal, but to always use 3 adresses,
            // -x can be expressed as 0-x
        	$$ = new NBinaryOperator(
                    NEW_PNODE(NInteger,0),
                    MINUS,
                    $2);
        }
      }
      | expr TBOP2 expr
      { $$ = NEW_PNODE(NBinaryOperator,$1, $2,$3); }
     | ident TASSIGN expr
      { $$ = NEW_PNODE(NAssignment,$1,$3);}
     | comparison { $$ = $1; }
     | TLPAREN expr TRPAREN { $$ = $2; }
     ;

ident : TIDENTIFIER { $$ = NEW_PNODE(NIdentifier,yytext); }
      ;

	// numeric non-terminal extended to support double type numeric value as terminal
numeric : TINTEGER { $$ = NEW_PNODE(NInteger, $1); }
          | TDOUBLE  {$$ = NEW_PNODE(NDouble, $1); }
        ;

call_args : /*blank*/  { $$ = NEW_PNODE(NExpressionList); }
          | expr
           { $$ = NEW_PNODE(NExpressionList);
             $$->push_back($1); }
          | call_args TCOMMA expr
           { $$=$1;
             $$->push_back($3);
           }
          ;

comparison : expr TCOP1 expr
             { $$ = NEW_PNODE(NComparisonOperator,$1,$2,$3); }
           ;

%%
