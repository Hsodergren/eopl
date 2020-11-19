{
open Parser
}

let whitespace = " " | "\t"
let number = ['0'-'9']
let alphanumeric = ['a'-'z' 'A'-'Z']

rule tokenize = parse
  | "if" {IF}
  | "minus" {NEG}
  | "let" {LET}
  | "in" {IN}
  | "(" {LPAREN}
  | ")" {RPAREN}
  | number+ as i { INT (int_of_string i) }
  | alphanumeric as c { ID (c) }
  | "," { COMMA }
  | "=" {ASSIGN}
  | "true" {BOOL true}
  | "false" {BOOL false}
  | "zero?" {ZERO}
  | "equal?" {EQ}
  | "greater?" {GT}
  | "less?" {LT}
  | "-" {MINUS}
  | "+" {PLUS}
  | "*" {MULT}
  | "/" {DIV}
  | whitespace {tokenize lexbuf}
  | eof {EOF}
