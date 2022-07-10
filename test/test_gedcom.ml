(* FIXME: get rid of theses Obj.magic *)

type gedcom_line = int * string * string * string [@@deriving show]

type gedcom_node = gedcom_line * gedcom_node list [@@deriving show]

type gedcom_calendar = JULIAN | GREGORIAN | FRENCH | HEBREW | UNKNOWN | ROMAN [@@deriving show]

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

open Gedcom

let%test_module _ = (module struct

  let ( <?> ) input expected =
    let r = parse_date input 0 (String.length input) in
    if r = expected then true
    else (print_endline (input ^ " -> " ^ show_gedcom_date (Obj.magic r)) ; false)

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
    fun () -> match !acc with hd :: tl -> acc := tl ; hd, 0, String.length hd | [] -> raise End_of_file

  let list = function Some x -> [x] | None -> []

  let input = input lines

  let n1, k1 = next0 prerr_endline input []
  let%test _ = n1 = Some (Obj.magic e1)
  let%test _ = k1 = Some (Obj.magic @@ aux 0 "@S1@" "SUBM" "")

  let n2, k2 = next0 prerr_endline input (list k1)
  let%test _ = n2 = Some (Obj.magic e2)
  let%test _ = k2 = Some (Obj.magic @@ aux 0 "@I1@" "INDI" "")

  let n3, k3 = next0 prerr_endline input (list k2)
  let%test _ = n3 = Some (Obj.magic e3)
  let%test _ = k3 = None

end)
