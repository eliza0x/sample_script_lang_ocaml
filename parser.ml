open Core
open Lexer

type expr =
    | Num of int         (* number *)
    | Add of expr * expr (* a + b *)
[@@deriving show]

let parse tokens =
    let bind f m = Option.value_map m ~default:None ~f:f

    in let rec eq_token t' ts = 
        match (List.hd ts, t') with
        | (Some (LParen), LParen) -> Some (List.tl_exn ts)
        | (Some (RParen), RParen) -> Some (List.tl_exn ts)
        | (Some (Op op),  Op op') -> Some (List.tl_exn ts)
        | (Some (Num n),  Num n') -> Some (List.tl_exn ts)
        | _                -> None

    and expr_parser ts = 
        let    result  = add_parser ts
        in let result' = term_parser ts
        in Option.first_some result result'

    and add_parser ts = 
        let    result   = term_parser ts
        in let result'  = bind (fun (ts', _) -> eq_token (Lexer.Op "+") ts') result
        in let result'' = bind (fun ts' -> expr_parser ts') result'
        in match (result, result', result'') with
        | (Some (_, t), Some _, Some(ts', e)) -> Some (ts', Add (t, e))
        | _                                    -> None

    and term_parser ts = 
        let    result  = paren_parser ts
        in let result' = form_parser ts
        in Option.first_some result result'

    and paren_parser ts = 
        let    result   = eq_token Lexer.LParen ts
        in let result'  = bind expr_parser result
        in let result'' = bind (fun (ts', _) -> eq_token Lexer.RParen ts') result'
        in match (result', result'') with
        | (Some (_, e), Some ts' ) -> Some (ts', e)
        | _                        -> None

    and form_parser ts = 
        match List.hd ts with
        | Some(Lexer.Num n) -> Some (List.tl_exn ts, Num n)
        | _                 -> None

    in bind (fun (ts, result) -> if phys_equal 0 (List.length ts) then Some(result) else None) 
            (expr_parser tokens)

