type course_requirement =
    | CourseCode of string * course_requirement
    | Or of course_requirement list
    | And of course_requirement list
    | Empty

(* Just a list of courses the student has taken *)
type student_t = string list

(* Sample data, uncomment if you want to use them to test the functions below

let my_courses = [
    "314";
    "213"
]

let class_requirements =
    Or [
        And [Or [CourseCode ("314", Empty); CourseCode ("336", Empty)]; CourseCode ("213", Empty)];
        And [Or [CourseCode ("352", Empty); CourseCode ("416", Empty)]; CourseCode ("213", Empty)]
    ]

let some_course = CourseCode ("1000", class_requirements) *)

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
