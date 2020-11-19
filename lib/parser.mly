%{
	open Ast
%}
%token EOF
%token <int> INT
%token <bool> BOOL
%token <char> ID
%token COMMA LPAREN RPAREN
%token ASSIGN
%token EQ GT LT
%token MINUS PLUS MULT DIV NEG
%token IF
%token LET IN
%token ZERO

%start <Ast.t> program

%%

program	:
  | exp EOF {$1}

exp:
  | IF pred=exp e1=exp e2=exp {If (pred,e1,e2)}
  | LPAREN e=exp RPAREN {e}
  | NEG LPAREN e=exp RPAREN {Neg e}
  | MINUS LPAREN e1=exp COMMA e2=exp RPAREN {BinOp ((<->), e1, e2)}
  | PLUS LPAREN e1=exp COMMA e2=exp RPAREN {BinOp ((<+>), e1, e2)}
  | MULT LPAREN e1=exp COMMA e2=exp RPAREN {BinOp ((<*>), e1, e2)}
  | DIV LPAREN e1=exp COMMA e2=exp RPAREN {BinOp ((</>), e1, e2)}
  | EQ LPAREN e1=exp COMMA e2=exp RPAREN {BinOp ((<=>), e1, e2)}
  | GT LPAREN e1=exp COMMA e2=exp RPAREN {BinOp ((<>>), e1, e2)}
  | LT LPAREN e1=exp COMMA e2=exp RPAREN {BinOp ((<<>), e1, e2)}
  | LET c=ID ASSIGN e1=exp IN e2=exp {Let (c,e1,e2)}
  | ZERO LPAREN e=exp RPAREN {Zero e}
  | c = ID {Var c}
  | b = BOOL {Val (Bool b)}
  | i = INT {Val (Int i)}
