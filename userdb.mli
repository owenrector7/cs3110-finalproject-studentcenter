open Yojson.Basic.Util
open Classdb
(* open Userdb *)

exception UserNotFound
exception AlreadyEnrolled
exception NotEnrolled

type u
(** This type represents a user, containing their name, netid, password, 
    is_admin status, and courses enrolled *)
type users


val from_json : Yojson.Basic.t -> users
(** TODO: add_class, drop_class, and view_schedule all need to return a value of
   type users, so we can change the state *)

val to_json : users -> unit 
(** [to_json users] writes the current state of the users to the file "data/users.json" *)



val add_class : u -> int -> users -> t -> users * t
(** [add_class stud cls_code userdb classdb] is the tuple of two elements: 1. user database [userdb] with a given class [cls_code] in the class database [classdb] added to the specified student [stud]'s schedule. 2. the class database [classdb] with [stud] added to the student enrolled list of the class represented by [cls_code].  Raise [ClassFull] if class [c] is full. Raise [ClassClosed] if class [c] is closed. Raise [TimeConflict] if class time overlaps with a class already in the student [stud] class list. Requires: none *)

val drop_class : u -> int -> users -> t -> users * t
(** [drop_class stud cls_code usrdb classdb] is the tuple of two elements: 1. user database [userdb] with a given class [cls_code] in the class database [classdb] removed from the specified student [stud]'s schedule. 2. The class database [classdb] with the student [stud] removed from the student enrolled list of the class represented by [cls_code]. Requires: none *)

val view_schedule : u -> t -> pp list
(** [view_schedule stud] returns a list of all the classes [c] associated with a
    given student [stud]. A student attempting to view their own schedule will
    not need to enter a student [stud] argument, as it will be associated with
    their account. An administrator attempting to view the schedule of a student
    must enter a valid student [stud] as an argument. Requires: none. *)

    val view_schedule_list : u -> t -> int list
(** [view_schedule_list stud] returns a list of ints of the codes of the classes [c] associated with a
    given student [stud]. A student attempting to view their own schedule will
    not need to enter a student [stud] argument, as it will be associated with
    their account. An administrator attempting to view the schedule of a student
    must enter a valid student [stud] as an argument. Requires: none. *)

val get_user : string -> string -> users -> u
(** [get_user net_id pass userdb] is the user corresponding to [net_id] if and only if [net_id] is in [userdb], and [pass] matches its corresponding password.Otherwise, it raises a UserNotFound*)

val user_schedule_to_string : users -> u -> string
(** Returns the schedule of the user in user list*)

val get_new_user : u -> users -> u
(** [get_new_user old_user userdb] is the new user whose netid matches that of [old_user] in [userdb]. This function is necessary because OCaml is functional language. The some of the new user's fields are different from the old user. *)

