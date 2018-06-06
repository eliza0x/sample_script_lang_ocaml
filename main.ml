open Core
open Lexer
open Parser

let () =
    let tokens = Lexer.tokenize "( 1 + 2 ) + 3"
    in let expr = Option.value (Parser.parse tokens) ~default:(Num 0)
    in print_string (Parser.show_expr expr ^ "\n")
