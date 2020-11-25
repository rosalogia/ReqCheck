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
    get s
    |> Lwt_main.run
    |> Yojson.Safe.from_string

let reqUrl = "https://sis.rutgers.edu/oldsoc/courses.json?subject=198&semester=12020&campus=NB&level=UG"

let course_info = 
    let open Yojson.Safe.Util in
    let course_list =
        [getJson reqUrl]
        |> flatten in
    let course_names =
        course_list
        |> filter_member "title"
        |> filter_string in
    let course_prereqs =
        course_list
        |> filter_member "preReqNotes"
        |> filter_string
        |> List.map (Str.global_replace (Str.regexp "</?em>") "") in
    zip course_names course_prereqs
