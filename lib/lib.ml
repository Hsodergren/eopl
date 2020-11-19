include Ast

let of_string str =
  let lexbuf = Lexing.from_string str in
  Parser.program Lexer.tokenize lexbuf
