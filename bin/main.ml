open ReqCheck
open Printf

let () =
    Data.course_info
    |> List.iter (fun (name, prereqs) ->
            printf "Course Name: %s ; Course Prereqs: %s\n" name prereqs)
