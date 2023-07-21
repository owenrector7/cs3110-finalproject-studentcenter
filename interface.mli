val help_student : unit -> unit
(** [help_student] will print out in terminal a list of all the commands that a
    student can do.*)

val help_admin : unit -> unit
(** [help_admin] will print out in terminal a list of all the commands that an
    administrator can do.*)

val logout : unit -> unit
(** [logout] will send the terminal back to the "existing user? y/n" command,
    with a quit message attached.*)

val quit : unit -> unit
(** [quit] will quit the program.*)