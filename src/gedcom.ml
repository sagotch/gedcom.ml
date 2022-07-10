(** OCaml parser for GEDCOM 5.5.1 files. *)

let may_apply (f : 'a -> 'b) (x : 'a option) : 'b option =
  match x with None -> None | Some x -> Some (f x)

type gedcom_line = int * string * string * string

type gedcom_node = gedcom_line * gedcom_node list

let lvl (lvl, _, _, _) = lvl

let xref (_, xref, _, _) = xref

let tag (_, _, tag, _) = tag

let value  (_ ,_ ,_ , value) = value

let mk_line lvl xref tag value = (lvl, xref, tag, value)

let node (node, _) = node

let children (_, children) = children

module Parser = struct

  open Re

  let alpha = alt [ rg 'a' 'z' ; rg 'A' 'Z' ; char '\x5F' ]
  let digit = rg '0' '9'
  let alphanum = alt [ alpha ; digit ]
  let otherchar = alt [ rg '!' '"'
                      ; rg '$' '/'
                      ; rg ':' '?'
                      ; rg '[' '^'
                      ; char '`'
                      ; rg '{' '~'
                      ; rg '\x80' '\xFE' ]
  let any_char = alt [ alpha
                     ; digit
                     ; otherchar
                     ; char '#'
                     ; char ' '
                     ; seq [ char '@'
                           ; char '@' ] ]
  let delim = char ' '
  let escape_text = rep1 any_char
  let non_at = alt [ alpha ; digit ; otherchar ; char ' ' ; char '#' ]
  let pointer = seq [ char '@' ; alphanum ; rep non_at ; char '@' ]
  let gedcom_line = seq [ group (rep1 digit)
                        ; delim
                        ; opt (seq [ group pointer ; delim ])
                        ; group (rep1 alphanum)
                        ; opt (seq [ delim ; group (rep1 any) ]) ]
  let space = alt [ char ' ' ; char '\t' ]

  let trimmed_line =
    seq [ rep space ; gedcom_line ; rep space ; opt (seq [ opt (char '\013') ; char '\010' ]) ]
    |> compile

  let trimmed_xref =
    seq [ rep space ; group pointer ; rep space ]
    |> compile

  let date_calendar_escape = alt [ str "@#DHEBREW@" ; str "HEBREW"
                                 ; str "@#DROMAN@" ; str "ROMAN"
                                 ; str "@#DFRENCH R@" ; str "FRENCH R"
                                 ; str "@#FRENCH@" ; str "FRENCH"
                                 ; str "@#DFRENCH_R@" ; str "FRENCH_R"
                                 ; str "@#DGREGORIAN@" ; str "GREGORIAN"
                                 ; str "@#DJULIAN@" ; str "JULIAN"
                                 ; str "@#DUNKNOWN@"
                                 ]

  let era = alt [ str "A.D."
                ; str "AD"
                ; str "B.C."
                ; str "BC"
                ; str "BCE"
                ; str "CE"
                ]

  let month =
    alt [ str "JAN"
        ; str "FEB"
        ; str "MAR"
        ; str "APR"
        ; str "MAY"
        ; str "JUN"
        ; str "JUL"
        ; str "AUG"
        ; str "SEP"
        ; str "OCT"
        ; str "NOV"
        ; str "DEC"
        (* French *)
        ; str "VEND"
        ; str "BRUM"
        ; str "FRIM"
        ; str "NIVO"
        ; str "PLUV"
        ; str "VENT"
        ; str "GERM"
        ; str "FLOR"
        ; str "PRAI"
        ; str "MESS"
        ; str "THER"
        ; str "FRUC"
        ; str "COMP"
        (* Hebrew *)
        ; str "TSH"
        ; str "CSH"
        ; str "KSL"
        ; str "TVT"
        ; str "SHV"
        ; str "ADR"
        ; str "ADS"
        ; str "NSN"
        ; str "IYR"
        ; str "SVN"
        ; str "TMZ"
        ; str "AAV"
        ; str "ELL"
        ]

  let roman_int = alt [ char 'I'
                      ; char 'V'
                      ; char 'X'
                      ; char 'L'
                      ; char 'C'
                      ; char 'D'
                      ; char 'M'
                      ]

  (* Groups:
     - 1: calendar
     - 2: day
     - 3: month
     - 4: year
     - 5: /NN (* TODO: handle this *)
     - 6: era
  *)
  let date_r =
    let sep = alt [ seq [ rep space ; char '/' ; rep space ] ; rep1 space ] in
    seq [ opt (seq [ group date_calendar_escape ; sep ])
        ; opt (seq [ opt (seq [ group (seq [ digit ; opt digit ]) ; sep ]) ; group (alt [ seq [ digit ; opt digit ] ; month ]) ; sep ])
        ; group (alt [ seq [ opt (char '-') ; rep1 digit ]
                     ; seq [ opt (seq [ alt [ str "AN" ; str "YEAR" ] ; rep1 space ]) ; rep1 roman_int ]
                     ])
        ; opt (seq [ char '/' ; group (seq [ digit ; opt digit ]) ])
        ; opt (group (seq [ rep space ; era ]) )
        ]

  (* Groups:
     - 1: FROM (FROM ... [TO ...])
     - 2-7: date (FROM ... [TO ...])
     - 8: TO (FROM ... TO ...)
     - 9-14: date (FROM ... TO ...)
     - 15: TO (TO ...)
     - 16-21: date (TO ...)
  *)
  let date_period =
    seq [ alt [ bol ; bos ]
        ; rep space
        ; alt [ seq [ group (str "FROM")
                    ; rep space
                    ; date_r
                    ; opt (seq [ rep space
                               ; group (str "TO")
                               ; rep space
                               ; date_r ]) ]
              ; seq [ group (str "TO")
                    ; rep space
                    ; date_r ] ]
        ; rep space
        ; alt [ eol ; eos ]
        ]
    |> no_case
    |> compile

  (* Groups:
     - 1: BEF
     - 2-7: date
     - 8: AFT
     - 9-14: date
     - 15: BET
     - 16-21: date
     - 22: AND
     - 23-28: date
  *)
  let date_range =
    seq [ alt [ bol ; bos ]
        ; rep space
        ; alt [ seq [ group (str "BEF")
                    ; rep space
                    ; date_r ]
              ; seq [ group (str "AFT")
                    ; rep space
                    ; date_r ]
              ; seq [ group (str "BET")
                    ; rep space
                    ; date_r
                    ; rep space
                    ; group (str "AND")
                    ; rep space
                    ; date_r ] ]
        ; rep space
        ; alt [ eol ; eos ]
        ]
    |> no_case
    |> compile

  (* Groups:
     - 1: [ABT|CAL|EST]
     - 2-7: date
  *)
  let date_approximated =
    seq [ alt [ bol ; bos ]
        ; rep space
        ; group (opt (alt [ str "ABT" ; str "CAL" ; str "EST" ]))
        ; rep space
        ; date_r
        ; rep space
        ; alt [ eol ; eos ]
        ]
    |> no_case
    |> compile

    (* Groups:
     - 1-6: date
     - 7: text
  *)
  let date_interpreted =
    seq [ alt [ bol ; bos ]
        ; rep space
        ; str "INT"
        ; rep space
        ; date_r
        ; rep space
        ; opt (seq [ char '('
                   ; group (rep any)
                   ; char ')'
                   ])
        ; alt [ eol ; eos ]
        ]
    |> no_case
    |> compile

end

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

let parse_date s pos len =
  let decode_roman_int s =
    let i =
      let rec loop i =
        if i < 0 then 0
        else match String.unsafe_get s i with
          | 'I' | 'V' | 'X' | 'L' | 'C' | 'D' | 'M' -> loop (i - 1)
          | _ -> i + 1
      in loop (String.length s - 1)
    in
    let s = if i = 0 || s = "" then s else String.sub s i (String.length s - i) in
    let decode_digit one five ten r =
      let rec loop cnt i =
        if i >= String.length s then 10 * r + cnt, i
        else if s.[i] = one then loop (cnt + 1) (i + 1)
        else if s.[i] = five then
          if cnt = 0 then loop 5 (i + 1) else 10 * r + 5 - cnt, i + 1
        else if s.[i] = ten then 10 * r + 10 - cnt, i + 1
        else 10 * r + cnt, i
      in
      loop 0
    in
    let (r, i) = decode_digit 'M' 'M' 'M' 0 0 in
    let (r, i) = decode_digit 'C' 'D' 'M' r i in
    let (r, i) = decode_digit 'X' 'L' 'C' r i in
    let (r, i) = decode_digit 'I' 'V' 'X' r i in
    if i = String.length s then r else raise Not_found
  in
  let month s =
    match String.uppercase_ascii s with
    | "" -> 0, None
    (* Gregorian *)
    | "JAN" -> 1, Some GREGORIAN
    | "FEB" -> 2, Some GREGORIAN
    | "MAR" -> 3, Some GREGORIAN
    | "APR" -> 4, Some GREGORIAN
    | "MAY" -> 5, Some GREGORIAN
    | "JUN" -> 6, Some GREGORIAN
    | "JUL" -> 7, Some GREGORIAN
    | "AUG" -> 8, Some GREGORIAN
    | "SEP" -> 9, Some GREGORIAN
    | "OCT" -> 10, Some GREGORIAN
    | "NOV" -> 11, Some GREGORIAN
    | "DEC" -> 12, Some GREGORIAN
    (* French *)
    | "VEND" -> 1, Some FRENCH
    | "BRUM" -> 2, Some FRENCH
    | "FRIM" -> 3, Some FRENCH
    | "NIVO" -> 4, Some FRENCH
    | "PLUV" -> 5, Some FRENCH
    | "VENT" -> 6, Some FRENCH
    | "GERM" -> 7, Some FRENCH
    | "FLOR" -> 8, Some FRENCH
    | "PRAI" -> 9, Some FRENCH
    | "MESS" -> 10, Some FRENCH
    | "THER" -> 11, Some FRENCH
    | "FRUC" -> 12, Some FRENCH
    | "COMP" -> 13, Some FRENCH
    (* Hebrew *)
    | "TSH" -> 1, Some HEBREW
    | "CSH" -> 2, Some HEBREW
    | "KSL" -> 3, Some HEBREW
    | "TVT" -> 4, Some HEBREW
    | "SHV" -> 5, Some HEBREW
    | "ADR" -> 6, Some HEBREW
    | "ADS" -> 7, Some HEBREW
    | "NSN" -> 8, Some HEBREW
    | "IYR" -> 9, Some HEBREW
    | "SVN" -> 10, Some HEBREW
    | "TMZ" -> 11, Some HEBREW
    | "AAV" -> 12, Some HEBREW
    | "ELL" -> 13, Some HEBREW
    | _ -> raise Not_found
  in
  let module G = Re.Group in
  let int g n = match G.get g n with exception Not_found -> 0 | "" -> 0 | i -> int_of_string i in
  let date g start =
    let d = int g (start + 1) in
    let m, calendar = let i = start + 2 in try int g i, None with _ -> month (G.get g i) in
    let calendar =
      let def c = match calendar with None -> c | Some c -> c in
      match String.uppercase_ascii (G.get g start) with
      | exception Not_found ->
        def GREGORIAN
      | "" ->
        def GREGORIAN
      | "@#DGREGORIAN@" | "GREGORIAN"
        -> GREGORIAN
      | "@#DHEBREW@" | "HEBREW"
        -> HEBREW
      | "@#DROMAN@"| "ROMAN"
        -> ROMAN
      | "@#DFRENCH R@" | "FRENCH R"
      | "@#FRENCH@" | "FRENCH"
      | "@#DFRENCH_R@" | "FRENCH_R"
        -> FRENCH
      | "@#DJULIAN@" | "JULIAN"
        -> JULIAN
      | _ ->
        def UNKNOWN
    in
    let y = try int g (start + 3) with _ -> decode_roman_int (G.get g @@ start + 3) in
    let y' = match G.get g (start + 4) with
      | exception Not_found -> None | "" -> None
      | "00" | "0" -> Some (y - y mod 10 + 10)
      | i ->
        let i = int_of_string i in
        if i < 10 then Some (y - y mod 10 + i)
        else Some (y - y mod 100 + i)
    in
    let y =
        match String.uppercase_ascii (G.get g @@ start + 5) with
        | exception Not_found -> y
        | "B.C." | "BC" | "BCE" when y > 0 -> - y
        | _ -> y
    in
    (d, m, y, calendar, y')
  in
  match Re.exec_opt Parser.date_approximated s with
  | Some g ->
    begin match String.uppercase_ascii (G.get g 1) with
      | "" -> Date_SURE (date g 2)
      | "ABT" -> Date_ABT (date g 2)
      | "CAL" -> Date_CAL (date g 2)
      | "EST" -> Date_EST (date g 2)
      | _ -> assert false
    end
  | None ->
    match Re.exec_opt Parser.date_period s with
    | Some g ->
      if G.test g 1
      then
        if G.test g 8 then Period_FROM_TO (date g 2, date g 9)
        else Period_FROM (date g 2)
      else Period_TO (date g 16)
    | None ->
      match Re.exec_opt Parser.date_range s with
      | Some g ->
        if G.test g 1 then Range_BEF (date g 2)
        else if G.test g 8 then Range_AFT (date g 9)
        else Range_BET_AND (date g 16, date g 23)
      | None ->
        match Re.exec_opt Parser.date_interpreted s ~pos ~len with
        | Some g ->
          Date_INT (date g 1, try Re.Group.get g 7 with Not_found -> "")
        | None ->
          Date_TEXT s

let parse_pointer s pos len =
  match Re.exec_opt Parser.trimmed_xref s ~pos ~len with
  | Some g -> Some (Re.Group.get g 1)
  | None -> None

let parse_line report s pos len =
  try
    let g = Re.exec Parser.trimmed_line s ~pos ~len in
    Some (Re.Group.( (get g 1 |> int_of_string)
                   , (try get g 2 with Not_found -> "")
                   , (get g 3)
                   , (try get g 4 with Not_found -> "")
                   )
         )
  with Not_found ->
    let s = String.sub s pos len in
    if String.trim s <> ""
    then report ("unable to parse: \"" ^ String.escaped s ^ "\"") ;
    None

let tree start = function
  | [] -> []
  | hd :: tl ->
    (** Add element [el] of relative level [d] to list [li]. *)
    let rec add_to_tree (d : int) (li : gedcom_node list) (el : gedcom_line) =
      let li = List.rev li in
      if lvl el = d then List.rev ((el, []) :: li)
      else
        let item = match List.hd li with
	  | (el', []) ->
            assert (lvl el = d + 1) ;
            (el', [(el, [])])
          | (el', child)  ->
            (el', add_to_tree (d + 1) child el)
        in List.rev (item :: (List.tl li))
    in List.fold_left (add_to_tree start) [ (hd, []) ] tl

let next report input =
  let rec next () =
    match input () with
    | exception End_of_file -> None
    | line, pos, len ->
      match parse_line report line pos len with
      | None -> next ()
      | line -> line
  in next ()

let next0 report input acc =
  let tree acc = match tree 0 (List.rev acc) with [x] -> Some x | _ -> assert false in
  let rec read acc =
    match next report input with
    | None -> tree acc, None
    | Some line as r ->
      if lvl line = 0 && acc <> []
      then tree acc, r
      else read (line :: acc)
  in read acc

let fold0 report input acc fn =
  let acc0 = ref None in
  let rec fold0 acc =
    let acc0' = match !acc0 with Some x -> [x] | None -> [] in
    let () = acc0 := None in
    match next0 report input acc0' with
    | None, None -> assert (!acc0 = None) ; acc
    | None, Some _ -> assert false
    | Some n, None -> fn acc n
    | Some n, Some nn -> acc0 := Some nn ; fold0 (fn acc n)
  in fold0 acc

let concat = function
  | [] -> []
  | hd :: tl ->
    List.fold_left begin fun (acc : gedcom_line list) (x : gedcom_line) ->
      match tag x with
      | "CONC"
      | "CONT" ->
        begin match acc with
          | [] -> assert false
          | hd :: tl ->
            let l = lvl hd in
            let i = xref hd in
            let t = tag hd in
            let v =
              value hd
              ^ (if tag x = "CONT" then "\n" else "")
              ^ (try value x with Not_found -> "")
            in
            mk_line l i t v :: acc
        end
      | _ -> x :: acc
    end [ hd ] tl
    |> List.rev
