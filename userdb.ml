open Yojson.Basic.Util
open Classdb

exception UserNotFound
exception AlreadyEnrolled
exception NotEnrolled

type json_file = Yojson.Basic.t

type u = {
  user_name : string;
  netid : string;
  password : string;
  is_admin : bool;
  schedule : int list;
}

type users = {
  user_lst : u list;
  size : int;
}

(* this is what was wrong the whole time I want to cry *)
(* let schedule_of_json j = j |> member "schedule" |> to_int *)

let user_of_json (j : json_file) : u =
  {
    user_name = j |> member "name" |> to_string;
    netid = j |> member "netid" |> to_string;
    password = j |> member "password" |> to_string;
    is_admin = j |> member "is admin" |> to_bool;
    schedule = j |> member "schedule" |> to_list |> List.map to_int;
  }

let from_json (j : json_file) : users =
  let user_list = j |> member "users" |> to_list |> List.map user_of_json in
  { user_lst = user_list; size = List.length user_list }

let rec single_user_schedule schedule =
  match schedule with
  | [] -> []
  | h :: t -> `Int h :: single_user_schedule t

let single_user_json user =
  `Assoc
    [
      ("name", `String user.user_name);
      ("netid", `String user.netid);
      ("password", `String user.password);
      ("is admin", `Bool user.is_admin);
      ("schedule", `List (single_user_schedule user.schedule));
    ]

let rec all_user_json user_lst =
  match user_lst with
  | [] -> []
  | user :: t -> single_user_json user :: all_user_json t

let to_json (users : users) : unit =
  `Assoc [ ("users", `List (users.user_lst |> all_user_json)) ]
  |> Yojson.Basic.to_file "data/users.json"

let check_user_exists (usr : u) (userdb : users) =
  if not (List.exists (fun x -> x.netid = usr.netid) userdb.user_lst) then
    raise UserNotFound
  else ()

(** [new_user_list usr userdb] is the user list of [userdb] with [usr]'s new
    schedule. *)
let new_user_list usr userdb =
  let rec aux usr usrlst acc =
    match usrlst with
    | [] -> raise UserNotFound
    | h :: t ->
        if h.netid = usr.netid then acc @ (usr :: t) else aux usr t (h :: acc)
  in
  aux usr userdb.user_lst []

let add_class (usr : u) (cls_code : int) (userdb : users) (classdb : t) :
    users * t =
  check_user_exists usr userdb;
  check_class_exists cls_code classdb;
  let new_schedule =
    if List.mem cls_code usr.schedule then raise AlreadyEnrolled
    else time_conflict usr.schedule classdb cls_code;
    cls_code :: usr.schedule
  in
  let new_user = { usr with schedule = new_schedule } in
  let new_userdb = { userdb with user_lst = new_user_list new_user userdb } in
  let new_classdb =
    add_student_to_class classdb cls_code
      (create_student usr.user_name usr.netid)
  in
  (new_userdb, new_classdb)

(* find the usr in usrdb, use get_user *)
(* check that the class exists. If not, raise ClassNotFound*)
(* check that the class is open. If not, raise ClassClosed *)
(* change the users class list in userdb *)
(* change the students enrolled in classdb *)

let drop_class (usr : u) (cls_code : int) (userdb : users) (classdb : t) :
    users * t =
  check_user_exists usr userdb;
  check_class_exists cls_code classdb;
  let new_schedule = List.filter (fun x -> x <> cls_code) usr.schedule in
  if new_schedule = usr.schedule then raise NotEnrolled
  else
    let new_user = { usr with schedule = new_schedule } in
    let new_userdb = { userdb with user_lst = new_user_list new_user userdb } in
    let new_classdb =
      remove_student_from_class classdb cls_code
        (create_student usr.user_name usr.netid)
    in
    (new_userdb, new_classdb)
(* if exists cls_list cls_code then List.filter (fun x -> x <> cls_code)
   usr.schedule else raise ClassNotFound *)

let view_schedule (usr : u) (cls_list : t) : pp list =
  clst_to_pplst usr.schedule cls_list.classes

let view_schedule_list (usr : u) (cls_list : t) = usr.schedule

let get_user (net_id : string) (hashed_pass : string) (userdb : users) : u =
  let rec helper = function
    | [] -> raise UserNotFound
    | h :: t ->
        if h.netid <> net_id then helper t
        else if h.password = hashed_pass then h
        else raise UserNotFound
  in
  helper userdb.user_lst

(** factor this out later because test/main.ml already has this function*)
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

let user_schedule_to_string userdb user =
  let target_user = List.find (fun x -> x.netid = user.netid) userdb.user_lst in
  pp_list string_of_int target_user.schedule

let get_new_user old_user userdb =
  List.find (fun x -> x.netid = old_user.netid) userdb.user_lst
