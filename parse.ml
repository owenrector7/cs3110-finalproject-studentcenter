open Classdb
open Userdb

type u_inp =
  | DepartmentId' of string
  | Code' of int

type int_or_string =
  | Int of int
  | String of string

type u =
  | Add of int_or_string
  | Drop of int_or_string

type e =
  | Credits of int
  | Name of string list
  | Code of int
  | DepartmentId of string * int
  | Instructor of string

type inp =
  | ClassEntry of e list
  | UserEntry of u

exception Empty
exception Malformed
exception NoArgs

let clean_bool bad_bool =
  match bad_bool with
  | Some b -> b
  | None -> raise Malformed

let clean_int bad_int =
  match bad_int with
  | Some i -> i
  | None -> raise Malformed

let string_of_list lst =
  let s =
    List.fold_left (fun acc s -> acc ^ " " ^ String.uppercase_ascii s) "" lst
  in
  String.sub s 1 (String.length s - 1)

let parse_user_search : string list -> u = function
  | [ "add" ] | [ "drop" ] -> raise NoArgs
  | "add" :: t -> begin
      try Add (Int (t |> string_of_list |> int_of_string))
      with _ -> Add (String (t |> string_of_list))
    end
  | "drop" :: t -> begin
      try Drop (Int (t |> string_of_list |> int_of_string))
      with _ -> Drop (String (t |> string_of_list))
    end
  | _ -> raise Malformed

let parse_class_search = function
  | [] -> raise NoArgs
  | [ "credits" ]
  | [ "name" ]
  | [ "code" ]
  | [ "dept-id" ]
  | [ "department-id" ]
  | [ "instructor" ] -> raise Malformed
  | "credits" :: t ->
      if List.length t > 1 then raise Malformed
      else [ Credits (t |> List.hd |> int_of_string_opt |> clean_int) ]
  | "name" :: t -> [ Name t ]
  | "code" :: t ->
      if List.length t > 1 then raise Malformed
      else [ Code (t |> List.hd |> int_of_string_opt |> clean_int) ]
  | "department-id" :: t | "dept-id" :: t -> begin
      match t with
      | [ s; n ] -> [ DepartmentId (s, n |> int_of_string_opt |> clean_int) ]
      | _ -> raise Malformed
    end
  | "instructor" :: t ->
      if List.length t > 1 then raise Malformed
      else [ Instructor (t |> List.hd) ]
  | _ -> raise Malformed

let parse str =
  str (* |> String.lowercase_ascii *) |> String.split_on_char ' '
  |> List.filter (fun x -> x <> "")
  |> function
  | [] -> raise Empty
  | h :: t -> begin
      match h with
      | "search" -> ClassEntry (parse_class_search t)
      | _ -> UserEntry (parse_user_search (h :: t))
    end

let rec search (classes : c list) (entries : e list) : c list =
  match entries with
  | [] -> classes
  | h :: t -> begin
      match h with
      | Credits n -> search (search_credits n classes) t
      | Name s -> search (search_name s classes) t
      | Code n -> search (search_code n classes) t
      | DepartmentId (dept, num) -> search (search_dept_id dept num classes) t
      | Instructor n -> search (search_instructor n classes) t
    end

let rec handle_user_entry (command : u) (user : Userdb.u) (users : Userdb.users)
    (cls_list : t) : users * t =
  match command with
  | Add (Int code) -> add_class user code users cls_list
  | Add (String d_id) ->
      add_class user (id_to_code cls_list d_id) users cls_list
  | Drop (Int code) -> drop_class user code users cls_list
  | Drop (String did) ->
      drop_class user (id_to_code cls_list did) users cls_list
