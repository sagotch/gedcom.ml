(** OCaml parser for GEDCOM 5.5.1 files. *)

let may_apply (f : 'a -> 'b) (x : 'a option) : 'b option =
  match x with None -> None | Some x -> Some (f x)

type gedcom_line = int * string * string * string [@@deriving show]

type gedcom_node = gedcom_line * gedcom_node list [@@deriving show]

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
    seq [ alt [ bol ; bos ] ; rep space ; gedcom_line ; rep space ; alt [ eos ; eol ] ]
    |> compile

  let trimmed_xref =
    seq [ bol ; rep space ; group pointer ; rep space ; eol ]
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

type gedcom_calendar = JULIAN | GREGORIAN | FRENCH | HEBREW | UNKNOWN | ROMAN [@@deriving show]

(* (day, month, year, calendar, alt year) 0 is use for day and/or month when unspecified *)
type gedcom_dmy = int * int * int * gedcom_calendar * int option [@@deriving show]

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
[@@deriving show]

let parse_date s =
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
        match Re.exec_opt Parser.date_interpreted s with
        | Some g ->
          Date_INT (date g 1, try Re.Group.get g 7 with Not_found -> "")
        | None ->
          Date_TEXT s

let%test_module _ = (module struct

  let ( <?> ) input expected =
    if parse_date input = expected then true
    else (print_endline (input ^ " -> " ^ show_gedcom_date (parse_date input)) ; false)

  (* https://www.gedcomassessment.com/en/area-date-valid.htm *)
  let%test _ = "1801" <?> Date_SURE (0, 0, 1801, GREGORIAN, None)
  let%test _ = "FEB 1802" <?> Date_SURE (0, 2, 1802, GREGORIAN, None)
  let%test _ = "3 MAR 1803" <?> Date_SURE (3, 3, 1803, GREGORIAN, None)
  let%test _ = "BEF 1811" <?> Range_BEF (0, 0, 1811, GREGORIAN, None)
  let%test _ = "BEF FEB 1812" <?> Range_BEF (0, 2, 1812, GREGORIAN, None)
  let%test _ = "BEF 3 MAR 1813" <?> Range_BEF (3, 3, 1813, GREGORIAN, None)
  let%test _ = "AFT 1821" <?> Range_AFT (0, 0, 1821, GREGORIAN, None)
  let%test _ = "AFT FEB 1822" <?> Range_AFT (0, 2, 1822, GREGORIAN, None)
  let%test _ = "AFT 3 MAR 1833" <?> Range_AFT (3, 3, 1833, GREGORIAN, None)
  let%test _ = "BET 1831 AND 1841" <?> Range_BET_AND ((0, 0, 1831, GREGORIAN, None), (0, 0, 1841, GREGORIAN, None))
  let%test _ = "BET FEB 1832 AND FEB 1842" <?> Range_BET_AND ((0, 2, 1832, GREGORIAN, None), (0, 2, 1842, GREGORIAN, None))
  let%test _ = "BET 3 MAR 1833 AND 3 MAR 1843" <?> Range_BET_AND ((3, 3, 1833, GREGORIAN, None), (3, 3, 1843, GREGORIAN, None))
  let%test _ = "FROM 1851" <?> Period_FROM (0, 0, 1851, GREGORIAN, None)
  let%test _ = "FROM FEB 1852" <?> Period_FROM (0, 2, 1852, GREGORIAN, None)
  let%test _ = "FROM 3 MAR 1853" <?> Period_FROM (3, 3, 1853, GREGORIAN, None)
  let%test _ = "FROM 1851 TO 1861" <?> Period_FROM_TO ((0, 0, 1851, GREGORIAN, None), (0, 0, 1861, GREGORIAN, None))
  let%test _ = "FROM FEB 1852 TO FEB 1862" <?> Period_FROM_TO ((0, 2, 1852, GREGORIAN, None), (0, 2, 1862, GREGORIAN, None))
  let%test _ = "FROM 3 MAR 1853 TO 3 MAR 1863" <?> Period_FROM_TO ((3, 3, 1853, GREGORIAN, None), (3, 3, 1863, GREGORIAN, None))
  let%test _ = "TO 1871" <?> Period_TO (0, 0, 1871, GREGORIAN, None)
  let%test _ = "TO FEB 1872" <?> Period_TO (0, 2, 1872, GREGORIAN, None)
  let%test _ = "TO 3 MAR 1873" <?> Period_TO (3, 3, 1873, GREGORIAN, None)
  let%test _ = "ABT 1881" <?> Date_ABT (0, 0, 1881, GREGORIAN, None)
  let%test _ = "ABT FEB 1882" <?> Date_ABT (0, 2, 1882, GREGORIAN, None)
  let%test _ = "ABT 3 MAR 1883" <?> Date_ABT (3, 3, 1883, GREGORIAN, None)
  let%test _ = "CAL 1891" <?> Date_CAL (0, 0, 1891, GREGORIAN, None)
  let%test _ = "CAL FEB 1892" <?> Date_CAL (0, 2, 1892, GREGORIAN, None)
  let%test _ = "CAL 3 MAR 1893" <?> Date_CAL (3, 3, 1893, GREGORIAN, None)
  let%test _ = "EST 1901" <?> Date_EST (0, 0, 1901, GREGORIAN, None)
  let%test _ = "EST FEB 1902" <?> Date_EST (0, 2, 1902, GREGORIAN, None)
  let%test _ = "EST 3 MAR 1903" <?> Date_EST (3, 3, 1903, GREGORIAN, None)
  let%test _ = "INT 1912 (Phrase)" <?> Date_INT ((0, 0, 1912, GREGORIAN, None), "Phrase")
  let%test _ = "INT FEB 1913 (Phrase)" <?> Date_INT ((0, 2, 1913, GREGORIAN, None), "Phrase")
  let%test _ = "INT 3 MAR 1914 (Phrase)" <?> Date_INT ((3, 3, 1914, GREGORIAN, None), "Phrase")
  let%test _ = "(Phrase)" <?> Date_TEXT "(Phrase)"

  (* https://www.gedcomassessment.com/en/area-date-questionable.htm *)
  let%test _ = "Feb 1801" <?> Date_SURE (0, 2, 1801, GREGORIAN, None)

  (* https://www.gedcomassessment.com/en/area-date-dual-year.htm *)
  let%test _ = "11 FEB 1721/22" <?> Date_SURE (11, 2, 1721, GREGORIAN, Some 1722)
  let%test _ = "11 FEB 1721/2" <?> Date_SURE (11, 2, 1721, GREGORIAN, Some 1722)
  let%test _ = "17 FEB 1719/20" <?> Date_SURE (17, 2, 1719, GREGORIAN, Some 1720)
  let%test _ = "14 FEB 1699/00" <?> Date_SURE (14, 2, 1699, GREGORIAN, Some 1700)

  (* https://www.gedcomassessment.com/en/area-date-invalid.htm *)
  let%test _ = "INT 1911" <?> Date_INT ((0, 0, 1911, GREGORIAN, None), "")
  let%test _ = "Invalid Phrase" <?> Date_TEXT "Invalid Phrase"
  let%test _ = "MAR" <?> Date_TEXT "MAR"
  let%test _ = "8 MAR"  <?> Date_TEXT "8 MAR"

  let%test _ = "AN XI" <?> Date_SURE (0, 0, 11, GREGORIAN, None) (* should it be DFRENCH? *)
  let%test _ = "10 FRIM XIV" <?> Date_SURE (10, 3, 14, FRENCH, None)
  let%test _ = "10 NIVO XIV" <?> Date_SURE (10, 4, 14, FRENCH, None)
  let%test _ = "BET 16 FLOR YEAR LXXIX AND 3 PRAI YEAR LXXIX" <?> Range_BET_AND ((16, 8, 79, FRENCH, None), (3, 9, 79, FRENCH, None))

end)

let parse_line report s =
  try
    let g = Re.exec Parser.trimmed_line s in
    Some (Re.Group.( (get g 1 |> int_of_string)
                   , (try get g 2 with Not_found -> "")
                   , (get g 3)
                   , (try get g 4 with Not_found -> "")
                   )
         )
  with Not_found ->
    if String.trim s <> ""
    then report ("unable to parse: \"" ^ String.escaped s ^ "\"") ;
    None

let tree = function
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
    in List.fold_left (add_to_tree 0) [ (hd, []) ] tl

let next report input =
  let rec next () =
    match input () with
    | exception End_of_file -> None
    | line ->
      match parse_line report line with
      | Some _ as line -> line
      | None -> next ()
  in next ()

let next0 report input acc =
  let tree acc = match tree (List.rev acc) with [x] -> Some x | _ -> assert false in
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

let%test_module _ = (module struct
  let lines =
    [ "0 HEAD"
    ; "1 SOUR PAF 2.2"
    ; "1 DEST PAF"
    ; "1 DATE 20 NOV 1992"
    ; "1 FILE ROYALS.GED"
    ; "1 CHAR ANSEL"
    ; "0 @S1@ SUBM"
    ; "1 NAME Denis R. Reid"
    ; "1 ADDR 149 Kimrose Lane"
    ; "2 CONT Broadview Heights, Ohio 44147-1258"
    ; "2 CONT Internet Email address:  ah189@cleveland.freenet.edu"
    ; "1 PHON (216) 237-5364"
    ; "1 COMM >> In a message to Cliff Manis (cmanis@csoftec.csf.com)"
    ; "2 CONT >> Denis Reid wrote the following:"
    ; "2 CONT >> Date: Fri, 25 Dec 92 14:12:32 -0500"
    ; "2 CONT >> From: ah189@cleveland.Freenet.Edu (Denis Reid)"
    ; "2 CONT >> Subject: THE ROYALS"
    ; "2 CONT >> First of all,  MERRY CHRISTMAS!"
    ; "2 CONT >>"
    ; "2 CONT >> You may make this Royal GEDCOM available available to whomever."
    ; "2 CONT >> As you know this is a work in process and have received suggestions,"
    ; "2 CONT >> corrections and additions from all over the planet..."
    ; "2 CONT >> some even who claim to be descended from Charlemange, himself!"
    ; "2 CONT >>"
    ; "2 CONT >> The weakest part of the Royals is in the French and Spanish lines."
    ; "2 CONT >> I found that many of the French Kings had multiple mistresses whose"
    ; "2 CONT >> descendants claimed noble titles, and the Throne itself in some"
    ; "2 CONT >> cases.  I have had the hardest time finding good published sources"
    ; "2 CONT >> for French and Spanish Royalty."
    ; "2 CONT >>"
    ; "2 CONT >> If you do post it to a BBS or send it around, I would appreciate"
    ; "2 CONT >> it if you'd append a message to the effect that I would welcome"
    ; "2 CONT >> comments and suggestions and possible sources to improve"
    ; "2 CONT >> the database."
    ; "2 CONT >>"
    ; "2 CONT >> Since the Royals had so many names and many titles it was difficult"
    ; "2 CONT >> to \"fill in the blanks\" with their name.  In the previous version,"
    ; "2 CONT >> I included all their titles, names, monikers in the notes."
    ; "2 CONT >>"
    ; "2 CONT >> Thanks for your interest.   Denis Reid"
    ; "0 @I1@ INDI"
    ; "1 NAME Victoria  /Hanover/"
    ; "1 TITL Queen of England"
    ; "1 SEX F"
    ; "1 BIRT"
    ; "2 DATE 24 MAY 1819"
    ; "2 PLAC Kensington,Palace,London,England"
    ; "1 DEAT"
    ; "2 DATE 22 JAN 1901"
    ; "2 PLAC Osborne House,Isle of Wight,England"
    ; "1 BURI"
    ; "2 PLAC Royal Mausoleum,Frogmore,Berkshire,England"
    ; "1 REFN 1"
    ; "1 FAMS @F1@"
    ; "1 FAMC @F42@"
    ]

  let aux a b c d = (a, b, c, d)

  let e1 =
    ( aux 0 "" "HEAD" ""
    , [ aux 1 "" "SOUR" "PAF 2.2", []
      ; aux 1 "" "DEST" "PAF", []
      ; aux 1 "" "DATE" "20 NOV 1992", []
      ; aux 1 "" "FILE" "ROYALS.GED", []
      ; aux 1 "" "CHAR" "ANSEL", []
      ] )

  let e2 =
    ( aux 0 "@S1@" "SUBM" ""
    , [ aux 1 "" "NAME" "Denis R. Reid", []
      ; aux 1 "" "ADDR" "149 Kimrose Lane",
        [ aux 2 "" "CONT" "Broadview Heights, Ohio 44147-1258", []
        ; aux 2 "" "CONT" "Internet Email address:  ah189@cleveland.freenet.edu", []
        ]
      ; aux 1 "" "PHON" "(216) 237-5364", []
      ; aux 1 "" "COMM" ">> In a message to Cliff Manis (cmanis@csoftec.csf.com)",
        [ aux 2 "" "CONT" ">> Denis Reid wrote the following:", []
        ; aux 2 "" "CONT" ">> Date: Fri, 25 Dec 92 14:12:32 -0500", []
        ; aux 2 "" "CONT" ">> From: ah189@cleveland.Freenet.Edu (Denis Reid)", []
        ; aux 2 "" "CONT" ">> Subject: THE ROYALS", []
        ; aux 2 "" "CONT" ">> First of all,  MERRY CHRISTMAS!", []
        ; aux 2 "" "CONT" ">>", []
        ; aux 2 "" "CONT" ">> You may make this Royal GEDCOM available available to whomever.", []
        ; aux 2 "" "CONT" ">> As you know this is a work in process and have received suggestions,", []
        ; aux 2 "" "CONT" ">> corrections and additions from all over the planet...", []
        ; aux 2 "" "CONT" ">> some even who claim to be descended from Charlemange, himself!", []
        ; aux 2 "" "CONT" ">>", []
        ; aux 2 "" "CONT" ">> The weakest part of the Royals is in the French and Spanish lines.", []
        ; aux 2 "" "CONT" ">> I found that many of the French Kings had multiple mistresses whose", []
        ; aux 2 "" "CONT" ">> descendants claimed noble titles, and the Throne itself in some", []
        ; aux 2 "" "CONT" ">> cases.  I have had the hardest time finding good published sources", []
        ; aux 2 "" "CONT" ">> for French and Spanish Royalty.", []
        ; aux 2 "" "CONT" ">>", []
        ; aux 2 "" "CONT" ">> If you do post it to a BBS or send it around, I would appreciate", []
        ; aux 2 "" "CONT" ">> it if you'd append a message to the effect that I would welcome", []
        ; aux 2 "" "CONT" ">> comments and suggestions and possible sources to improve", []
        ; aux 2 "" "CONT" ">> the database.", []
        ; aux 2 "" "CONT" ">>", []
        ; aux 2 "" "CONT" ">> Since the Royals had so many names and many titles it was difficult", []
        ; aux 2 "" "CONT" ">> to \"fill in the blanks\" with their name.  In the previous version,", []
        ; aux 2 "" "CONT" ">> I included all their titles, names, monikers in the notes.", []
        ; aux 2 "" "CONT" ">>", []
        ; aux 2 "" "CONT" ">> Thanks for your interest.   Denis Reid", []
        ]
      ] )

  let e3 =
    ( aux 0 "@I1@" "INDI" ""
    , [ aux 1 "" "NAME" "Victoria  /Hanover/", []
      ; aux 1 "" "TITL" "Queen of England", []
      ; aux 1 "" "SEX" "F", []
      ; aux 1 "" "BIRT" "",
        [ aux 2 "" "DATE" "24 MAY 1819", []
        ; aux 2 "" "PLAC" "Kensington,Palace,London,England", [] ]
      ; aux 1 "" "DEAT" "",
        [ aux 2 "" "DATE" "22 JAN 1901", []
        ; aux 2 "" "PLAC" "Osborne House,Isle of Wight,England", [] ]
      ; aux 1 "" "BURI" "",
        [ aux 2 "" "PLAC" "Royal Mausoleum,Frogmore,Berkshire,England", [] ]
      ; aux 1 "" "REFN" "1", []
      ; aux 1 "" "FAMS" "@F1@", []
      ; aux 1 "" "FAMC" "@F42@", []
      ] )

  let input lines =
    let acc = ref lines in
    fun () -> match !acc with hd :: tl -> acc := tl ; hd | [] -> raise End_of_file

  let list = function Some x -> [x] | None -> []

  let input = input lines

  let n1, k1 = next0 prerr_endline input []
  let%test _ = n1 = Some e1
  let%test _ = k1 = Some (aux 0 "@S1@" "SUBM" "")

  let n2, k2 = next0 prerr_endline input (list k1)
  let%test _ = n2 = Some e2
  let%test _ = k2 = Some (aux 0 "@I1@" "INDI" "")

  let n3, k3 = next0 prerr_endline input (list k2)
  let%test _ = n3 = Some e3
  let%test _ = k3 = None

end)
