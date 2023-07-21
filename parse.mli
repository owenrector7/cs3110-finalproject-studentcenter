open Classdb

open Userdb
(** Query example: search open search class 3110 search credits 4 search people *)

type e

type u_inp =
  | DepartmentId' of string
  | Code' of int

type int_or_string =
  | Int of int
  | String of string

type u =
  | Add of int_or_string
  | Drop of int_or_string

type inp =
  | ClassEntry of e list
  | UserEntry of u

exception Empty
exception Malformed
exception NoArgs

val parse : string -> inp
(** [parse str] parses the command given by [str] and delegates the necessary
    function. Example commands are ["search credits 3 instructor Clarkson"],
    ["schedule"], ["quit"], or ["add 12534"]. Currently, parse only works for
    one command (i.e ["search credits 3"]) *)

val search : c list -> e list -> c list
(** [search entries classes] returns the classes that correspond to the search
    criteria in entries. entries is an e list, where each element is a specific
    filter criteria of type e *)

val handle_user_entry : u -> Userdb.u -> Userdb.users -> t -> users * t
