open Angstrom
open Compute
open Printf

let is_int = function '0'..'9' -> true | _ -> false

let is_course_code = function '0'..'9' | ':' -> true | _ -> false

let spaces = function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false

let ws = skip_while spaces

let between a b p = a *> p <* b

let pstring s = between ws ws (string s)

let sep_by_strict s p = both p (many1 (s *> p)) >>| fun (h,t) -> h::t

let parens p = between (pstring "(") (pstring ")") p

let p_coursecode =
    let ccode = 
        sep_by_strict (string ":") (take_while is_int)
        >>| String.concat ":"
        >>| fun s -> CourseCode(s, Empty) in
    choice [ccode ; parens ccode]

let rec print_coursereq = function
    | CourseCode (s, _) -> Printf.sprintf "%s" s
    | Or (l) -> String.concat " or " (List.map print_coursereq l) |> sprintf "(%s)"
    | And (l) -> String.concat " and " (List.map print_coursereq l) |> sprintf "(%s)"
    | Empty -> "None"

let p_requirements =
    fix (fun p_requirements ->
            let p_or = sep_by_strict (pstring "or") (parens p_requirements <|> p_coursecode) >>| fun (l) -> Or(l) in
            let p_and = sep_by_strict (pstring "and") (parens p_requirements <|> p_coursecode) >>| fun (l) -> And(l) in
            let preqs = choice [p_and ; p_or ; p_coursecode] in
            preqs <|> parens preqs)

let parse_requirements s =
    match parse_string ~consume:Prefix p_requirements (String.lowercase_ascii s) with
    | Result.Ok (res) -> Some res
    | Result.Error (_) -> None
