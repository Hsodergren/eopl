{
open Parser
}

let whitespace = " " | "\t" | "\n"
let number = ['0'-'9']
let lowercase = ['a'-'z']
let alphanumeric = ['a'-'z' '0'-'9']

rule tokenize = parse
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | "minus" {NEG}
  | "let" {LET}
  | "let*" {LETSTAR}
  | "in" {IN}
  | "(" {LPAREN}
  | "[" {LBRACK}
  | ")" {RPAREN}
  | "]" {RBRACK}
  | "," { COMMA }
  | "=" {ASSIGN}
  | "true" {BOOL true}
  | "false" {BOOL false}
  | "zero?" {ZERO}
  | "equal?" {EQ}
  | "nil" {NIL}
  | "cons" {CONS}
  | "car" {CAR}
  | "cdr" {CDR}
  | "null?" {NULL}
  | "list" {LIST}
  | "proc" {PROC}
  | "unpack" {UNPACK}
  | "greater?" {GT}
  | "less?" {LT}
  | "-" {MINUS}
  | "+" {PLUS}
  | "*" {MULT}
  | "/" {DIV}
  | number+ as i { INT (int_of_string i) }
  | alphanumeric+ as c { ID c }
  | whitespace {tokenize lexbuf}
  | eof {EOF}
