open ReqCheck
open Opium

let course_data =
    let choose_valid_course (name, reqs) =
        match Parser.parse_requirements reqs with
        | Some requirements -> Some (Compute.CourseCode (name, requirements))
        | None -> None in
    Data.course_info
    |> List.filter_map choose_valid_course

let code_from_course = function
    | Compute.CourseCode(s, _) ->
            s
            |> Str.split (Str.regexp " - ")
            |> List.hd
    | _ -> "No code for this course"

let json_list_of (strings: string list) =
    strings
    |> List.map (fun s -> `String (s))
    |> (fun l -> `List (l))

let access_control_allow_origin = Response.add_header ("Access-Control-Allow-Origin", "*")
    

let bad_request =
    Response.of_json ~status:(Status.of_code 400) (`Assoc [("error", `String ("Bad Request"))])
    |> access_control_allow_origin

let json_ok v =
    Response.of_json ~status:(Status.of_code 200) v
    |> access_control_allow_origin

let all_handler _ =
    Data.course_data_json
    |> json_ok
    |> Lwt.return

let allowed_handler request =
    match Request.query "courses" request with
    | Some (course_list) ->
            let courses_taken = String.split_on_char ',' course_list in
            course_data
            |> List.filter (Compute.allowed courses_taken)
            |> List.filter (fun course ->
                    let course_code = code_from_course course in
                    List.mem course_code courses_taken
                    |> not)
            |> List.map (Parser.print_coursereq)
            |> json_list_of
            |> json_ok
            |> Lwt.return
    | None -> Lwt.return bad_request

let () =
    (* Log configuration *)
    Logs.set_reporter (Logs_fmt.reporter ());
    Logs.set_level (Some Logs.Debug);
    (* Running the app itself *)
    App.empty
    |> App.get "/all" all_handler
    |> App.get "/allowed" allowed_handler
    |> App.run_command
