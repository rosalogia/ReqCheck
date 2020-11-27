open Lwt
open Cohttp_lwt_unix

(* List.combine isn't working and I don't know why *)
let rec zip a b =
    match (a, b) with
    | ([], _)               -> []
    | (_, [])               -> []
    | ((x::xs), (y::ys))    -> (x, y) :: (zip xs ys)

let get s =
    Client.get (Uri.of_string s) >>= fun (_, body) ->
        body
        |> Cohttp_lwt.Body.to_string

let getJson s =
    try Some (Yojson.Safe.from_file s)
    with Yojson.Json_error (_) -> None

let course_info s = 
    let open Yojson.Safe.Util in
    let json = getJson s in
    match json with
    | Some course_info ->
        let course_list =
            [course_info]
            |> flatten in
        let course_names =
            course_list
            |> filter_member "course_title"
            |> filter_string in
        let course_prereqs =
            course_list
            |> filter_member "course_requirements"
            |> filter_string
            |> List.map (Str.global_replace (Str.regexp "</?em>") "") in
        Some (zip course_names course_prereqs)
    | None -> None
