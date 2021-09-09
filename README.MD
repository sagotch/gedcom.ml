# gedcom.ml

GEDCOM 5.5.1 handling in OCaml. See documentation for details
about what it can do for you.

## Installation

Using OPAM: `opam install gedcom`.

## Usage example

```ocaml
open Gedcom

let _ =
  parse_lines stdin
  |> concat
  |> mk_tree
  |> List.iter (fun n -> GedcomPrint.print_node n ;
     	                 print_newline ())
```

## License

Released under [MIT License](http://opensource.org/licenses/MIT).
