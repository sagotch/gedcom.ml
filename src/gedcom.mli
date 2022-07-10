(** {1 GEDCOM 5.5.1 handling in OCaml.} *)

type gedcom_line

(** You will need to use [-rectypes] compiler flag with this type. *)
type gedcom_node

(** {2 Parsing function.} *)

(** [next report input]
    Get the next line.
*)
val next : (string -> unit) -> (unit -> string * int * int) -> gedcom_line option

val next0
  : (string -> unit)
  -> (unit -> string * int * int)
  -> gedcom_line list
  -> gedcom_node option * gedcom_line option

(** [fold0 report input acc fn]
    Fold over nodes of level 0.
*)
val fold0 : (string -> unit) -> (unit -> string * int * int) -> 'a -> ('a -> gedcom_node -> 'a) -> 'a

(** {2 [gedcom_line] getters.} *)

(** Get the level of a [gedcom_line] *)
val lvl : gedcom_line -> int

(** Get the xref of a [gedcom_line].
    Raise [Not_found] if no such information is present. *)
val xref : gedcom_line -> string

(** Get the tag of a [gedcom_line]. *)
val tag : gedcom_line -> string

(** Get the value of a [gedcom_line].
    Raise [Not_found] if no such information is present. *)
val value : gedcom_line -> string

(** {2 [gedcom_node] getters.} *)

(** Get node value, i.e. the current [gedcom_line]. *)
val node : gedcom_node -> gedcom_line

(** Get node children. *)
val children : gedcom_node -> gedcom_node list

(** {2 Helpers (internal functions that may be useful for users).} *)

(** Parse a GEDCOM line. *)
val parse_line : (string -> unit) -> string -> int -> int -> gedcom_line option

type gedcom_calendar = JULIAN | GREGORIAN | FRENCH | HEBREW | UNKNOWN | ROMAN

(* (day, month, year, calendar, alt year) 0 is use for day and/or month when unspecified *)
type gedcom_dmy = int * int * int * gedcom_calendar * int option

type gedcom_date =
  | Date_SURE of gedcom_dmy
  | Date_ABT of gedcom_dmy
  | Date_CAL of gedcom_dmy
  | Date_EST of gedcom_dmy
  | Date_INT of gedcom_dmy * string
  | Date_TEXT of string
  | Range_BEF of gedcom_dmy
  | Range_AFT of gedcom_dmy
  | Range_BET_AND of gedcom_dmy * gedcom_dmy
  | Period_FROM of gedcom_dmy
  | Period_TO of gedcom_dmy
  | Period_FROM_TO of gedcom_dmy * gedcom_dmy

(** Parse a GEDCOM date. *)
val parse_date : string -> int -> int -> gedcom_date

(** Parse a GEDCOM pointer (e.g. @I1@). *)
val parse_pointer : string -> int -> int -> string option

(** Turn a list of [gedcom_line] into a tree using tags level. *)
val tree : int -> gedcom_line list -> gedcom_node list
