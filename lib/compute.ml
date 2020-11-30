type course_requirement =
    | CourseCode of string * course_requirement
    | Or of course_requirement list
    | And of course_requirement list
    | Empty

let rec taken student course =
    match course with
    | CourseCode (s, _)  -> List.mem s student
    | Or reqs               -> List.exists (taken student) reqs
    | And reqs              -> List.for_all (taken student) reqs
    | Empty                 -> true

let allowed student course =
    match course with
    | CourseCode (_, reqs) -> taken student reqs
    | _ -> false
