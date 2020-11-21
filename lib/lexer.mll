{
open Parser
}

let whitespace = " " | "\t" | "\n"
let number = ['0'-'9']
let lowercase = ['a'-'z']

rule tokenize = parse
  | "if" {IF}
  | "minus" {NEG}
  | "let" {LET}
  | "let*" {LETSTAR}
  | "in" {IN}
  | "(" {LPAREN}
  | ")" {RPAREN}
  | number+ as i { INT (int_of_string i) }
  | lowercase as c { ID (c) }
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
  | whitespace {tokenize lexbuf}
  | eof {EOF}
