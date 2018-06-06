open Core

type token =
    | LParen           (* (      *)
    | RParen           (* )      *)
    | Op     of string (* a + b  *)
    | Num    of int    (* number *)
[@@deriving show]

let tokenize source =
    let rec tokenize_iter pos = 
        if phys_equal pos (String.length source) 
            then []
            else match source.[pos] with
            | ' ' -> tokenize_iter (pos + 1)
            | '(' -> LParen                   :: tokenize_iter (pos + 1)
            | ')' -> RParen                   :: tokenize_iter (pos + 1)
            | '+' -> Op "+"                   :: tokenize_iter (pos + 1)
            | n   -> Num (int_of_char n - 48) :: tokenize_iter (pos + 1)
    in tokenize_iter 0

