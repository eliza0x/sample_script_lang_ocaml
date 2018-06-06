open Core
open Lexer
open Parser

let rec eval expr = 
    match expr with
    | Parser.Add (t1, t2) -> eval t1 + eval t2
    | Parser.Num n     -> n

let () =
    let tokens = Lexer.tokenize "( 1 + 2 ) + 3"
    in let expr = Option.value (Parser.parse tokens) ~default:(Num 0)
    in print_string (string_of_int (eval expr) ^ "\n")
