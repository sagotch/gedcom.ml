(** OCaml parser for GEDCOM 5.5.1 files. *)

(** OCaml 4.00.1 compatibility. *)
let ( |> ) x f = f x

let may_apply (f : 'a -> 'b) (x : 'a option) : 'b option =
  match x with None -> None | Some x -> Some (f x)

type gedcom_line = int * string option * string * string option

let lvl (lvl, _, _, _) = lvl

let xref (_, xref, _, _) =
  match xref with Some xref -> xref | _ -> raise Not_found

let tag (_, _, tag, _) = tag

let value  (_ ,_ ,_ , value) =
  match value with Some value -> value | _ -> raise Not_found

type gedcom_node = gedcom_line * gedcom_node list

let node (node, _) = node

let children (_, children) = children

(** Alias for matched_group function.  *)
let nth n str = Str.matched_group n str

(** Returns Some matched_group or None. *)
let nth_opt n str = try Some (nth n str) with Not_found -> None

let parse_line line =

  (* Remove leading and trailing whitespaces, CR or LF. *)
  let line = String.trim line in

  let grp s      = "\\(" ^ s ^ "\\)" in
  (* Sub-regexp for GECOM line components. *)
  let level      = "[0-9]+" in
  let delim      = " *" in
  let xref_id    = "@[^@]+@" in
  let tag        = "[a-zA-Z0-9]+" in
  let line_value = ".*" in

  (* This regexp represent a valid GEDCOM line. *)
  let (reg : Str.regexp) =
    "^"
    ^ grp level
    ^ delim
    ^ grp xref_id ^ "?"
    ^ delim
    ^ grp tag
    ^ delim
    ^ grp line_value ^ "?"
    ^ "$"
    |> Str.regexp in

  if (Str.string_match reg line 0)
  then (int_of_string (nth 1 line),
	may_apply (fun x -> String.sub x 1 (String.length x - 2))
		  (nth_opt 2 line),
	nth 3 line,
	nth_opt 4 line)
  else raise Not_found

let mk_tree l =

  (** Add element [el] of relative level [d] to list [li]. *)
  let rec add_to_tree
	    (d : int)
	    (li : gedcom_node list)
	    (el : gedcom_line)
	  : gedcom_node list =
    let li = List.rev li in
    if lvl el = d then List.rev ((el, []) :: li)
    else let item = match List.hd li with
	   | (el', []) -> assert (lvl el = d + 1) ; (el', [(el, [])])
           | (el', child)  -> (el', add_to_tree (d + 1) child el)
	 in List.rev (item :: (List.tl li))

  in List.fold_left (add_to_tree 0) [] l

let parse_lines chan =
  let rec parse acc =
    try parse (try parse_line (input_line chan) :: acc
	       with Not_found -> acc)
    with End_of_file -> List.rev acc
  in parse []

let concat list =
  List.fold_left
    (fun (acc : gedcom_line list) (x : gedcom_line) ->
     match tag x with
     | "CONC"
     | "CONT" ->
	let (l, i, t, v) = List.hd acc in
	(l, i, t, Some ( ( match v with Some x -> x | _ -> ""    )
			 ^ ( if tag x = "CONT" then "\n" else "" )
			 ^ ( try value x with Not_found -> ""    ) ) )
	:: List.tl acc
     | _ -> x :: acc)
    []
    list
  |> List.rev

module GedcomNAME = struct

    type gedcom_name = (string option * string option * string option)

    let parse_name name
	: (string option * string option * string option)  =
      let reg = Str.regexp "^\\([^/]+\\)?/?\\([^/]+\\)?/?\\(.+\\)?$" in
      if Str.string_match reg name 0
      then (nth_opt 1 name |> may_apply String.trim,
	    nth_opt 2 name |> may_apply String.trim,
	    nth_opt 3 name |> may_apply String.trim)
      else raise Not_found

    let firstname = function
      | (Some x, _, _) -> x
      | _ -> raise Not_found

    let lastname = function
      | (_, Some x, _) ->  x
      | _ -> raise Not_found

    let title = function
      | (_, _, Some x) -> x
      | _ -> raise Not_found

  end

module GedcomPrint = struct

    let may_print f =
      function Some x -> print_char ' ' ; f x | _ -> ()

    let print_gedcom_line (lvl, id, tag, value) =
      print_int lvl ;
      may_print (fun x -> print_char '@' ;
			  print_string x ;
			  print_char '@' )
		id ;
      print_char ' ' ;
      print_string tag ;
      may_print print_string value

    let rec print_gedcom_node n =
      print_gedcom_line (node n) ;
      print_newline () ;
      List.iter print_gedcom_node (children n)

end
