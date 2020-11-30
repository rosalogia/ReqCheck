(* List.combine isn't working and I don't know why *)
let rec zip a b =
    match (a, b) with
    | ([], _)               -> []
    | (_, [])               -> []
    | ((x::xs), (y::ys))    -> (x, y) :: (zip xs ys)

let course_data_json = Yojson.Safe.from_file "./files/rutgers.json"

let course_info = 
    let open Yojson.Safe.Util in
    let course_list =
        [course_data_json]
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
    zip course_names course_prereqs
