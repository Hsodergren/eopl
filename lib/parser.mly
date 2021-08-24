%{
    open Ast
%}
%token EOF
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token NIL CONS
%token PROC
%token COMMA LPAREN RPAREN LBRACK RBRACK
%token ASSIGN
%token EQ GT LT
%token MINUS PLUS MULT DIV NEG
%token CAR CDR NULL
%token IF THEN ELSE
%token LIST
%token LET LETSTAR LETREC IN
%token ZERO
%token UNPACK

%start <Ast.t> program

%%

program:
  | exp EOF {$1}

exp:
  | IF pred=exp THEN e1=exp ELSE e2=exp {If (pred,e1,e2)}
  | LPAREN e1=exp es=explist RPAREN
    {
      List.fold_left (fun acc v -> Apply(acc, v)) e1 es
    }
  | PROC LPAREN cs=idlist_comma RPAREN body=exp
    {
      List.fold_right (fun v acc -> Procedure (v, acc)) cs body
    }
  | LBRACK e=exp RBRACK {e}
  | NEG LPAREN e=exp RPAREN {Neg e}
  | MINUS LPAREN e1=exp COMMA e2=exp RPAREN {BinOp (Sub, e1, e2)}
  | PLUS LPAREN e1=exp COMMA e2=exp RPAREN {BinOp (Add, e1, e2)}
  | MULT LPAREN e1=exp COMMA e2=exp RPAREN {BinOp (Mul, e1, e2)}
  | DIV LPAREN e1=exp COMMA e2=exp RPAREN {BinOp (Div, e1, e2)}
  | EQ LPAREN e1=exp COMMA e2=exp RPAREN {BinOp (EQ, e1, e2)}
  | GT LPAREN e1=exp COMMA e2=exp RPAREN {BinOp (GT, e1, e2)}
  | LT LPAREN e1=exp COMMA e2=exp RPAREN {BinOp (LT, e1, e2)}
  | CAR LPAREN e=exp RPAREN {Car e}
  | CDR LPAREN e=exp RPAREN {Cdr e}
  | NULL LPAREN e=exp RPAREN {Null e}
  | LIST LPAREN es=explist_comma RPAREN {es}
  | LET c=ID ASSIGN e1=exp IN e2=exp {Let (c,e1,e2)}
  | LETSTAR es=assignlist IN body=exp {LetStar (List.rev es,body)}
  | LETREC recs=letrecs IN body=exp
    {
      LetRec (recs, body)
    }
  | ZERO LPAREN e=exp RPAREN {Zero e}
  | UNPACK cs=idlist ASSIGN e=exp IN body=exp {Unpack (cs,e,body)}
  | CONS LPAREN e1=exp COMMA e2=exp RPAREN {ConsT(e1,e2)}
  | NIL {Val (Nil)}
  | c = ID {Var c}
  | b = BOOL {Val (Bool b)}
  | MINUS i = INT {Val (Int (-i))}
  | i = INT {Val (Int i)}

letrecs:
  | {[]}
  | name=ID LPAREN cs=idlist_comma RPAREN ASSIGN let_body=exp tl=letrecs
    {
      let let_body = List.fold_right (fun v acc -> Procedure(v, acc)) cs let_body in
      (name,let_body)::tl
    }
idlist:
  | {[]}
  | c=ID cs=idlist {c::cs}

idlist_comma:
  | c=ID {[c]}
  | c=ID COMMA cs=idlist_comma {c::cs}

assignlist:
  | {[]}
  | c=ID ASSIGN e=exp l=assignlist {(c,e)::l}

explist:
  | e=exp {[e]}
  | e=exp es=explist {e::es}

explist_comma:
  | {Val Nil}
  | e=exp {ConsT (e, Val (Nil))}
  | e=exp COMMA es=explist_comma {ConsT (e, es)}
