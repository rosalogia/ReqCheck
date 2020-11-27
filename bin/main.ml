open ReqCheck

let course_data =
    let choose_valid_course (name, reqs) =
        match Parser.parse_requirements reqs with
        | Some requirements -> Some (Compute.CourseCode (name, requirements))
        | None -> None in
    Data.course_info
    |> List.filter_map choose_valid_course

let () =
    print_endline
        {|Thank you for using ReqCheck. This program takes as input the courses
you've taken so far and outputs which courses you're eligible to take next
academic term. Please enter the course codes of every course you've taken
(or those which you feel are relevant) in one line, separated by semi-colons.

For example, here is a valid input:
01:198:111 ; 01:198:112
|};

    (* print_endline "Here is a catalog of available courses in general"; *)

    (* course_data *)
    (* |> List.map (fun cr -> *)
    (*         match cr with *)
    (*         | Compute.CourseCode (s, r) -> sprintf "Course: %s; Requirements: %s" s (Parser.print_coursereq r) *)
    (*         | _ -> "None") *)
    (* |> List.iter print_endline; *)

    print_endline "The list of courses you'll be able to take next semester is as follows:\n";

    let student =
        read_line ()
        |> Str.split (Str.regexp " ; ") in
        
    course_data
    |> List.filter (Compute.allowed student)
    |> List.map Parser.print_coursereq
    |> List.iter print_endline
