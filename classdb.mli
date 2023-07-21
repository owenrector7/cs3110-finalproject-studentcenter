exception ClassNotFound
exception ClassFull
exception ClassClosed
exception InvalidEntry
exception InvalidSpec

(** We'll have to agree upon what valid a student is as we implement
    [view_schedule]. We will also have to consider if we can assume that a type
    c will be valid when inputted as an argument into any functions that take it
    as an argument. Otherwise, will need a case to raise [ClassNotFound] in each
    function that requires a class as an input.*)
type time = {
  hour : int;
  minute : int;
}
(** The abstract type of values representing a time. *)

type student
(** The abstract type of values representing a student. *)

type c
(* * The abstract type of values representing a class. *)

type t = {
  classes : c list;
  size : int;
}

type pp = {
  name : string;
  code : int;
  times : time * time;
  dep_id : string * int;
  professor : string;
  credits : string;
}
(** The abstract type of values representing a class database. *)

val from_json_api : Yojson.Basic.t -> t

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the class database that [j] represents. Requires: [j] is a
    valid JSON class database representation. *)

val to_json : string -> t -> Yojson.Basic.t
(** [to_json file classes] writes [classes] to [file] in json format conforming
    to data/class-schema.json *)

(** Ben: Should we add make these search functions return set-like functions?
    Currently, the results are not set-like, but I feel like it will be easier
    to reason about these search functions if we make the results set-like.*)

val id_to_code : t -> string -> int
(** [id_to_code] returns the class code associated with the given department
    identifier string. Raises [ClassNotFound] if the id is not in the database*)

val search_credits : int -> c list -> c list
(** [search_credits credits clst] returns classes in [clst] with credits
    [credits].*)

val search_name : string list -> c list -> c list
(** [search_credits word_lst clst] returns classes in [clst] whose name contains
    any of the words in [word_lst]. The result is set-like. ex.An example
    [word_lst] is
    ["Objected"; "Oriented"; "Programming"; "and"; "Data"; "Structures"] *)

val search_code : int -> c list -> c list
(** [search_code code clst] returns classes in [clst] with class code [code]. An
    example [code] is [12312].*)

val search_dept_id : string -> int -> c list -> c list
(** [search_dept_id prefix number clst] returns classes in [clst] with class
    prefix [prefix] and number [number]. An example [prefix] is ["CS"] and an
    example [number] is [2110]. *)

val search_instructor : string -> c list -> c list
(** [search_instructor instructor clst] returns classes in [clst] with
    instructor [instructor]. [instructor] should be the last name. An example
    [instructor] is ["Clarkson"]. *)

val view_all_class : t -> c list
(** [view_all_class class_db] prints in the terminal the names of all the
    classes in the class database [class_db]. This class database contains all
    the classes. Returns: None. Requires: None.*)

val get_class_code : c -> int
(** [get_class_code cls] is the class code of the class [cls]. *)

val add_student_to_class : t -> int -> student -> t
(** This function returns the classdb with the given student added to the class. *)

val remove_student_from_class : t -> int -> student -> t
(** This function returns the classdb with the given student removed from the
    class. *)

val add_student : student -> c -> unit
(** [add_student stud c] adds a given student [stud] to the class roster of a
    class [c]. This will modify the students' [stud] database. This can only be
    done by an administrator, and they will have no restrictions based on a
    class being either full or closed. Raise [TimeConflict] if adding class [c]
    to a students' [stud] schedule results in a time conflict with another class
    in their schedule. Requires: none *)

val remove_student : student -> c -> unit
(** [remove_student stud c] removes a given student [stud] from the class roster
    of a class [c]. This will modify the students' [stud] database. This can
    only be done by an administrator. Requires: none *)

val clst_to_string : c list -> string
(** [clst_to_string clst] is the string representation of [clst], which will be
    provided to the user for searches*)

val check_class_exists : int -> t -> unit
(** This function checks whether class of classcode exists in the classdb.*)

val create_student : string -> string -> student
(** Creates a student of given name and netid *)

val clst_to_pplst : int list -> c list -> pp list
(** [clst_to_pplst code_list class_lst] takes a list of codes and the entire
    database of classes and returns the list of classes that relate to the given
    codes in a format that only contains information relevant to printing *)

val pp_of_c : c -> pp
(** [pp_of_c c] is the pp containing information from c *)

val map_pp : c list -> pp list
(** [map_pp class_lst] is a list of records that contains only the information
    relevant from each class in [class_lst] for printing a students sechdule to
    the terminal *)

exception TimeConflict of c list
(** [TimeConflict c1 c2] denotes a time conflict between [c1], the added class,
    and the students current schedule, specifically class [c2]. *)

val time_conflict : int list -> t -> int -> unit
(** [time_coflict c1 c2 ] is whether or not the intervals of times that classes
    [c1] [c2] meet intersect/overlap *)
