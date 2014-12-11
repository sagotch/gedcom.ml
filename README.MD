# gedcom.ml

![travis-ci](https://travis-ci.org/sagotch/gedcom.ml.svg?branch=master)

GEDCOM 5.5.1 handling in OCaml. See documentation for details
about what it can do for you.

## Documentation

`make doc` or browse
[online documentation](http://sagotch.github.io/gedcom.ml/).

## Installation

You need `ocamlbuild` and `ocamlfind` in order to build
the library and to install it.

Using OPAM: `opam install gedcom`.

From sources: `make -C src build install`.

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
