open ReqCheck
open Opium
open Printf

let course_data fpath =
    let choose_valid_course (name, reqs) =
        match Parser.parse_requirements reqs with
        | Some requirements -> Some (Compute.CourseCode (name, requirements))
        | None -> None in
    match Data.course_info fpath with
    | Some course_info -> Some (course_info |> List.filter_map choose_valid_course)
    | None -> None

let course_data_handler request =
    match Request.query "school" request with
    | Some (school_name) ->
            (let course_json_path =
                school_name
                |> String.lowercase_ascii
                |> sprintf "./files/%s.json" in
            match Request.query "courses" request with
            | Some (course_list) ->
                    (let courses_taken = String.split_on_char ',' course_list in
                    match course_data course_json_path with
                    | Some (data) ->
                            data
                            |> List.filter (Compute.allowed courses_taken)
                            |> List.map (Parser.print_coursereq)
                            |> List.map (fun s -> `String (s))
                            |> fun l -> Lwt.return (Response.of_json ~status:(Status.of_code 200) (`List (l)))
                    | None -> Lwt.return (Response.of_json ~status:(Status.of_code 200) (Yojson.Safe.from_file course_json_path)))
            | None -> Lwt.return (Response.of_json ~status:(Status.of_code 200) (Yojson.Safe.from_file course_json_path)))
    | None -> Lwt.return (Response.of_plain_text ~status:(Status.of_code 400) "400 Bad Request")

let upload_handler request =
  let open Lwt.Syntax in
  let files = Hashtbl.create ~random:true 5 in
  let fpath = ref "" in
  let callback ~name:_ ~filename string =
    let filename = Filename.basename filename |> sprintf "./files/%s" in
    let write file =
      string |> String.length |> Lwt_unix.write_string file string 0 |> Lwt.map ignore
    in
    match Hashtbl.find_opt files filename with
    | Some file -> write file
    | None ->
      let* file =
        Lwt_unix.openfile filename Unix.[ O_CREAT; O_TRUNC; O_WRONLY; O_NONBLOCK ] 0o600
      in
      Hashtbl.add files filename file;
      fpath := filename;
      write file
  in
  let* _ = Request.to_multipart_form_data_exn ~callback request in
  let close _ file prev =
    let* () = prev in
    Lwt_unix.close file
  in
  let* () = Hashtbl.fold close files Lwt.return_unit in
  match Data.getJson !fpath with
  | Some _ -> Lwt.return @@ Response.of_plain_text ~status:(Status.of_code 200) "200 Success"
  | None -> Sys.remove !fpath; Lwt.return @@ Response.of_plain_text ~status:(Status.of_code 400) "400 Bad Request"

(* let deprecated_main = *)
(*     print_endline *)
(*         {|Thank you for using ReqCheck. This program takes as input the courses *)
(* you've taken so far and outputs which courses you're eligible to take next *)
(* academic term. Please enter the course codes of every course you've taken *)
(* (or those which you feel are relevant) in one line, separated by semi-colons. *)

(* For example, here is a valid input: *)
(* 01:198:111 ; 01:198:112 *)
(* |}; *)

(*     (1* print_endline "Here is a catalog of available courses in general"; *1) *)

(*     (1* course_data *1) *)
(*     (1* |> List.map (fun cr -> *1) *)
(*     (1*         match cr with *1) *)
(*     (1*         | Compute.CourseCode (s, r) -> sprintf "Course: %s; Requirements: %s" s (Parser.print_coursereq r) *1) *)
(*     (1*         | _ -> "None") *1) *)
(*     (1* |> List.iter print_endline; *1) *)


(*     let student = *)
(*         read_line () *)
(*         |> Str.split (Str.regexp " ; ") in *)
        
(*     print_endline "The list of courses you'll be able to take next semester is as follows:\n"; *)

(*     match course_data "courses.json" with *)
(*     | Some (data) -> *)
(*             data *)
(*             |> List.filter (Compute.allowed student) *)
(*             |> List.map Parser.print_coursereq *)
(*             |> List.iter print_endline *)
(*     | None -> print_endline "Error: JSON invalid" *)

let () =
    App.empty
    |> App.post "/upload" upload_handler
    |> App.get "/allowed" course_data_handler
    |> App.run_command
