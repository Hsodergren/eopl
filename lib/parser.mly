%{
	open Ast
%}
%token EOF
%token <int> INT
%token <bool> BOOL
%token <char> ID
%token NIL CONS
%token PROC
%token COMMA LPAREN RPAREN
%token ASSIGN
%token EQ GT LT
%token MINUS PLUS MULT DIV NEG
%token CAR CDR NULL
%token IF
%token LIST
%token LET IN LETSTAR
%token ZERO
%token UNPACK

%start <Ast.t> program

%%

program	:
  | exp EOF {$1}

exp:
  | IF pred=exp e1=exp e2=exp {If (pred,e1,e2)}
  | LPAREN e1=exp e2=exp RPAREN {Apply (e1,e2)}
  | LPAREN e=exp RPAREN {e}
  | NEG LPAREN e=exp RPAREN {Neg e}
  | MINUS LPAREN e1=exp COMMA e2=exp RPAREN {BinOp ((<->), e1, e2)}
  | PLUS LPAREN e1=exp COMMA e2=exp RPAREN {BinOp ((<+>), e1, e2)}
  | MULT LPAREN e1=exp COMMA e2=exp RPAREN {BinOp ((<*>), e1, e2)}
  | DIV LPAREN e1=exp COMMA e2=exp RPAREN {BinOp ((</>), e1, e2)}
  | EQ LPAREN e1=exp COMMA e2=exp RPAREN {BinOp ((<=>), e1, e2)}
  | GT LPAREN e1=exp COMMA e2=exp RPAREN {BinOp ((<>>), e1, e2)}
  | LT LPAREN e1=exp COMMA e2=exp RPAREN {BinOp ((<<>), e1, e2)}
  | CAR LPAREN e=exp RPAREN {Car e}
  | CDR LPAREN e=exp RPAREN {Cdr e}
  | NULL LPAREN e=exp RPAREN {Null e}
  | PROC LPAREN c=ID RPAREN body=exp {Procedure (c,body)}
  | LIST LPAREN es=explist RPAREN {Val (es)}
  | LET c=ID ASSIGN e1=exp IN e2=exp {Let (c,e1,e2)}
  | LETSTAR es=assignlist IN body=exp {LetStar (List.rev es,body)}
  | ZERO LPAREN e=exp RPAREN {Zero e}
  | UNPACK cs=idlist ASSIGN e=exp IN body=exp {Unpack (cs,e,body)}
  | CONS LPAREN e1=exp COMMA e2=exp RPAREN {Val (Cons(e1,e2))}
  | NIL {Val (Nil)}
  | c = ID {Var c}
  | b = BOOL {Val (Bool b)}
  | i = INT {Val (Int i)}

idlist:
  | {[]}
  | c=ID cs=idlist {c::cs}

assignlist:
  | {[]}
  | c=ID ASSIGN e=exp l=assignlist {(c,e)::l}

explist:
  | {Nil}
  | e=exp {Cons (e, Val (Nil))}
  | e=exp COMMA es=explist {Cons (e, Val (es))}
