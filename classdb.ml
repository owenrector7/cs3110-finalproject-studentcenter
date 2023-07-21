exception ClassNotFound
exception ClassFull
exception ClassClosed
exception InvalidEntry
exception InvalidSpec

open Yojson.Basic.Util

type json_file = Yojson.Basic.t

type time = {
  hour : int;
  minute : int;
}

type department_identifier = {
  dep : string;
  dep_num : int;
}

type day =
  | Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun

type student = {
  stud_name : string;
  net_id : string; (*Should the list of student classes be stored here?*)
}

type c = {
  class_name : string;
  class_code : int;
  section_num : string;
  location : string;
  instructor : string;
  credits : int;
  class_time : time * time;
  dep_id : department_identifier;
  days : day list;
  students_enrolled : student list;
}

type t = {
  classes : c list;
  size : int;
}

(* -------------------------------------------------------------------------- *)

let time_of_json j =
  {
    hour = j |> member "hour" |> to_int;
    minute = j |> member "minute" |> to_int;
  }

let department_of_json j =
  {
    dep = j |> member "department" |> to_string;
    dep_num = j |> member "number" |> to_int;
  }

exception DayMatch

let day_of_string = function
  | "Monday" -> Mon
  | "Tuesday" -> Tue
  | "Wednesday" -> Wed
  | "Thursday" -> Thu
  | "Friday" -> Fri
  | "Saturday" -> Sat
  | "Sunday" -> Sun
  | _ -> raise DayMatch

let student_of_json j =
  {
    stud_name = j |> member "name" |> to_string;
    net_id = j |> member "netid" |> to_string;
  }

let id_to_code t s =
  let idmap =
    List.map
      (fun c ->
        (c.dep_id.dep ^ " " ^ string_of_int c.dep_id.dep_num, c.class_code))
      t.classes
  in
  match List.assoc_opt s idmap with
  | Some a -> a
  | None ->
      print_endline s;
      raise ClassNotFound

exception ToTuple

let to_tuple lst =
  match lst with
  | [ h1; h2 ] -> (h1, h2)
  | _ -> raise ToTuple

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let instructor_string_of_json_list (j_list : json_file list) =
  try
    if List.length j_list > 0 then
      let j = List.hd j_list in
      j |> member "lastName" |> to_string
    else ""
  with Failure s -> failwith s

exception TimeNotFound

let military_of_standard_start (std : string) =
  if String.length std <> 7 then { hour = 9; minute = 5 }
  else
    let hr = String.sub std 0 2 in
    let min = String.sub std 3 2 in
    let postfix = String.sub std 5 2 in
    if postfix = "AM" then
      if int_of_string hr = 12 then { hour = 0; minute = int_of_string min }
      else { hour = int_of_string hr; minute = int_of_string min }
    else if postfix = "PM" then
      if int_of_string hr = 12 then
        { hour = int_of_string hr; minute = int_of_string min }
      else { hour = int_of_string hr + 12; minute = int_of_string min }
    else { hour = 9; minute = 5 }

let military_of_standard_end (std : string) =
  if String.length std <> 7 then { hour = 9; minute = 55 }
  else
    let hr = String.sub std 0 2 in
    let min = String.sub std 3 2 in
    let postfix = String.sub std 5 2 in
    if postfix = "AM" then
      if int_of_string hr = 12 then { hour = 0; minute = int_of_string min }
      else { hour = int_of_string hr; minute = int_of_string min }
    else if postfix = "PM" then
      if int_of_string hr = 12 then
        { hour = int_of_string hr; minute = int_of_string min }
      else { hour = int_of_string hr + 12; minute = int_of_string min }
    else { hour = 9; minute = 5 }

exception DayNotFound

let days_of_pattern (p : string) : day list =
  let day_of_char c =
    match c with
    | 'M' -> Mon
    | 'T' -> Tue
    | 'W' -> Wed
    | 'R' -> Thu
    | 'F' -> Fri
    | _ -> raise DayNotFound
  in

  String.fold_left (fun acc c -> acc @ [ day_of_char c ]) [] p

let loc_of_facility_descr f =
  match to_string_option f with
  | Some s -> s
  | None -> "To Be Assigned"

let class_of_api_json (j : json_file) : c =
  let section =
    j |> member "enrollGroups" |> to_list |> List.hd |> member "classSections"
    |> to_list |> List.hd
  in

  {
    class_name = j |> member "titleShort" |> to_string;
    class_code = section |> member "classNbr" |> to_int;
    (* TODO : remove section num *)
    section_num = "remove this";
    location =
      section |> member "meetings" |> to_list |> List.hd
      |> member "facilityDescr" |> loc_of_facility_descr;
    instructor =
      section |> member "meetings" |> to_list |> List.hd |> member "instructors"
      |> to_list |> instructor_string_of_json_list;
    credits =
      j |> member "enrollGroups" |> to_list |> List.hd |> member "unitsMinimum"
      |> to_int;
    class_time =
      ( section |> member "meetings" |> to_list |> List.hd |> member "timeStart"
        |> to_string |> military_of_standard_start,
        section |> member "meetings" |> to_list |> List.hd |> member "timeEnd"
        |> to_string |> military_of_standard_end );
    dep_id =
      {
        dep = j |> member "subject" |> to_string;
        dep_num = j |> member "catalogNbr" |> to_string |> int_of_string;
      };
    days =
      section |> member "meetings" |> to_list |> List.hd |> member "pattern"
      |> to_string |> days_of_pattern;
    students_enrolled = [] (* TODO: REMOVE THIS*);
  }

let class_of_api_test j =
  let section =
    j |> member "enrollGroups" |> to_list |> List.hd |> member "classSections"
    |> to_list |> List.hd
  in
  section |> member "facilityDescr" |> to_string

(* let from_json_api j = try let _ = j |> member "data" |> member "classes" |>
   to_list |> List.map class_of_api_test in

   { classes = []; size = 0 } with | DayNotFound -> failwith "Parsing error:\n
   \"day not found\"" | TimeNotFound -> failwith "Parsing error: \"time not\n
   found\"" | Type_error (s, _) -> failwith ("Parsing error: " ^ s) | ToTuple ->
   failwith "Parsing error: \"start end\"" | DayMatch -> failwith "Parsing\n
   error: \"days\"" | _ -> failwith "horrendous failure" *)

let from_json_api (j : json_file) : t =
  try
    let class_list =
      j |> member "data" |> member "classes" |> to_list
      |> List.map class_of_api_json
    in
    { classes = class_list; size = List.length class_list }
  with
  | DayNotFound -> failwith "Parsing error: \"day not found\""
  | TimeNotFound -> failwith "Parsing error: \"time not found\""
  | Type_error (s, _) -> failwith ("Parsing\n   error: " ^ s)
  | ToTuple -> failwith "Parsing error: \"start end\""
  | DayMatch -> failwith "Parsing error: \"days\""
  | _ ->
      print_endline "horrendous\n\n   failure";
      failwith "bruh"

let class_of_json (j : json_file) : c =
  {
    class_name = j |> member "name" |> to_string;
    class_code = j |> member "class code" |> to_int;
    section_num = j |> member "number" |> to_string;
    location = j |> member "location" |> to_string;
    instructor = j |> member "instructor" |> to_string;
    credits = j |> member "credits" |> to_int;
    class_time =
      j |> member "start end" |> to_list |> List.map time_of_json |> to_tuple;
    days =
      j |> member "days" |> to_list |> List.map to_string
      |> List.map day_of_string;
    dep_id = j |> member "department identifier" |> department_of_json;
    students_enrolled =
      j |> member "students enrolled" |> to_list |> List.map student_of_json;
  }

let from_json (j : json_file) =
  try
    let class_list =
      j |> member "classes" |> to_list |> List.map class_of_json
    in
    { classes = class_list; size = List.length class_list }
  with
  | Type_error (s, _) -> failwith ("Parsing error: " ^ s)
  | ToTuple -> failwith "Parsing error: \"start end\""
  | DayMatch -> failwith "Parsing error: \"days\""

let to_json (file : string) (classes : t) = failwith "not implemented"

let rec search_credits credits clst =
  List.filter (fun x -> x.credits = credits) clst

(** [name_list clst] is the list of pairs of class names and class code in the
    order of classes in [clst].*)
let rec name_list = function
  | [] -> []
  | h :: t -> (h.class_name, h.class_code) :: name_list t

(** [mem2 query_list name_list] is boolean value of whether any of the elements
    in the query_list is a member of name_list.*)
let rec mem2 query_list name_list =
  match query_list with
  | [] -> false
  | h :: t -> List.mem h name_list || mem2 t name_list

let rec search_name word_lst = function
  | [] -> []
  | h :: t ->
      let h_splitted = String.split_on_char ' ' h.class_name in
      let not_a_set =
        if mem2 word_lst h_splitted then h :: search_name word_lst t
        else search_name word_lst t
      in
      List.sort_uniq compare not_a_set

let rec search_code code clst = List.filter (fun x -> x.class_code = code) clst

let rec search_dept_id prefix number clst =
  List.filter (fun x -> x.dep_id.dep = prefix && x.dep_id.dep_num = number) clst

let rec search_instructor instructor clst =
  List.filter (fun x -> x.instructor = instructor) clst

let view_all_class t = t.classes
let get_class_code cls = cls.class_code

let add_student (s : student) (cls : c) =
  raise (Failure "To be implemented : add_student")

let remove_student (s : student) (cls : c) =
  raise (Failure "To be implemented : remove_student")

let exists (cls : int) (db : t) : bool =
  let class_lst = db.classes in
  let rec exists_aux = function
    | [] -> false
    | h :: t -> if h.class_code = cls then true else exists_aux t
  in
  exists_aux class_lst

let clst_to_string (clst : c list) =
  let rec aux clst acc =
    match clst with
    | [] -> acc
    | h :: t -> aux t (acc ^ "    " ^ h.class_name ^ "\n")
  in
  aux clst ""

let check_class_exists (cls_code : int) (classdb : t) =
  if not (List.exists (fun x -> x.class_code = cls_code) classdb.classes) then
    raise ClassNotFound
  else ()

type pp = {
  name : string;
  code : int;
  times : time * time;
  dep_id : string * int;
  professor : string;
  credits : string;
}

let classes_of_codes (codes : int list) (clst : c list) : c list =
  List.filter (fun (cl : c) -> List.mem cl.class_code codes) clst

let pp_of_c cl =
  {
    name = cl.class_name;
    code = cl.class_code;
    times = cl.class_time;
    dep_id = (cl.dep_id.dep, cl.dep_id.dep_num);
    professor = cl.instructor;
    credits = string_of_int cl.credits;
  }

let map_pp = List.map pp_of_c

let clst_to_pplst (codes : int list) (orig_clst : c list) : pp list =
  let clst = classes_of_codes codes orig_clst in
  map_pp clst

let new_class_list classlst cls =
  let rec aux cls classlst acc =
    match classlst with
    | [] -> raise ClassNotFound
    | h :: t ->
        if h.class_code = cls.class_code then acc @ (cls :: t)
        else aux cls t (h :: acc)
  in
  aux cls classlst []

let add_student_to_class classdb cls_code stud =
  let target_class =
    List.find (fun x -> x.class_code = cls_code) classdb.classes
  in
  let new_enrolledlist = stud :: target_class.students_enrolled in
  let new_class = { target_class with students_enrolled = new_enrolledlist } in
  { classdb with classes = new_class_list classdb.classes new_class }

let remove_student_from_class classdb cls_code stud =
  let target_class =
    List.find (fun x -> x.class_code = cls_code) classdb.classes
  in
  let new_enrolledlist =
    List.filter
      (fun x -> x.net_id <> stud.net_id)
      target_class.students_enrolled
  in
  let new_class = { target_class with students_enrolled = new_enrolledlist } in
  { classdb with classes = new_class_list classdb.classes new_class }

let create_student name net_id = { stud_name = name; net_id }

exception TimeConflict of c list

type time_simple = {
  s : float;
  e : float;
}

let time_add f c : float =
  float_of_int (f c.class_time).hour
  +. (float_of_int (f c.class_time).minute /. 60.)

let tuples_of_time c = { s = time_add fst c; e = time_add snd c }

(*(𝑠𝑡𝑎𝑟𝑡1≤𝑒𝑛𝑑1<𝑠𝑡𝑎𝑟𝑡2≤𝑒𝑛𝑑2)∨(𝑠𝑡𝑎𝑟𝑡2≤𝑒𝑛𝑑2<𝑠𝑡𝑎𝑟𝑡1≤𝑒𝑛𝑑1)*)
let conflict_check cadd ccheck =
  let t1, t2 = (tuples_of_time cadd, tuples_of_time ccheck) in
  if t2.s > t1.e || t1.s > t2.e then ()
  else raise (TimeConflict (cadd :: [ ccheck ]))

let time_conflict code_lst classdb c_code =
  let clst = classes_of_codes code_lst classdb.classes in
  List.iter
    (conflict_check (List.hd (classes_of_codes [ c_code ] classdb.classes)))
    clst

let time_test s1 s2 = fst s2 > snd s1 || fst s1 > snd s2
