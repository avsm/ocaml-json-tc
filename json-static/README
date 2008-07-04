                          json-static

Introduction
============

json-static is a syntax extension of OCaml that can make the use of
JSON data easier. From a special type declaration, the camlp4 preprocessor
generates the code that converts between a JSON "abstract syntax tree"
and specialized OCaml data structures such as objects, polymorphic variants,
lists, arrays, tuples, etc.

It will at the same time check that the structure of the JSON document
is correct and produce OCaml data which is statically typed.
For example, the following declaration defines the type of a point object:

  type json point = < x: float; y: float >

This automatically makes two functions available, with the following signature:

  val json_of_point : point -> Json_type.t
  val point_of_json : Json_type.t -> point

Json_type.t is the type of parsed JSON data, which is provided by 
the json-wheel library.

Function json_of_point would convert an OCaml object of type point into
a JSON object. point_of_json works the other way around, and fails 
by raising the Json_type.Json_error exception 
if the input JSON data doesn't have the right format.


Installation
============

Installation:
  make
  make install

Uninstallation:

  make uninstall


Usage
=====

Basically, you must preprocess your OCaml file(s) with 
camlp4o pa_json_static.cmo. Once installed using the standard
procedure (ocamlfind), you can compile a file using these commands:

# compile
ocamlfind ocamlopt -c yourfile.ml -syntax camlp4o -package json-static

# link
ocamlfind ocamlopt -o yourprog yourfile.cmx -linkpkg -package json-wheel

Build tools like OCamlMakefile take care of this nicely.


Syntax
======

You must write a special type declaration that describes the expected
format of the JSON data. There is a predefined mapping from OCaml types
to JSON:

OCaml type              JSON type         Properties of JSON data
----------              ---------         -----------------------

string                  String
float                   Number            not an int
int                     Number            an int
number*                 Number            a float or an int
bool                    Boolean

list                    Array             homogenous
array                   Array             homogenous
tuple                   Array             fixed length

(string * 'a) assoc**   Object            an object read as an associative list
(string, 'a) Hashtbl.t  Object
object or record        Object            additional methods/fields are ignored

option                  any               null means None

polymorphic variants    String or Array   a String for constructors without
                                          an argument, 
                                          or an Array of length 2
                                          where the first element is a
                                          String that represents the 
                                          constructor
                                          and the second element is the
                                          argument.

classic variants        String or Array   a String for contructors without 
                                          an argument,
                                          or an Array where the first element
                                          is the String that represents the
                                          constructor and the rest are the
                                          arguments. Unlike polymorphic
                                          variants, there may be several 
                                          arguments (just don't use parentheses
                                          around them in the type definition).

X.t***                  defined by X.of_json and X.to_json


---
*: the number type is an alias for float, but accepts JSON ints and converts
   them to OCaml floats.
**: the assoc type is an alias for list, but converts from a JSON object.
***: X can be any simple module name, but module fields t, of_json and to_json
     are mandatory.

A type definition is done like regular type definitions, but the keyword
"json" is placed just after "type", as in:

  type json t = int * float
       ^^^^

The type cannot be polymorphic, i.e. it doesn't support type parameters.
A small set of basic types are supported (see table above). Other type
names can be used only if they are part of the same definition.
This works:

  type json a = b
  and b = int list

But the following doesn't work:

  type json b = int list

  type json a = b  (* b is unknown to the preprocessor *)


In addition to the basic syntax for type declarations, a few extensions
have been added:

1) Object labels or variant names can be followed by an arbitrary string.
   When it is the case, this defines the string to be found in the JSON
   data.

2) Object methods can be preceded by a questionmark. If it is the case,
   JSON objects that do not have this field could still be read. 
   Their value is set to null instead of being undefined and causing an error.

3) A default value can be specified a after a method definition.
   The syntax is a "=" followed by an expression. The expression should
   be a constant. The default expression is used whenever 
   the JSON null value is encountered, even if the type converter
   knows how to deal with null.

4) Records and variant types must be named, as always in OCaml. Reusing
   an existing definition is possible by using the "predefined" keyword
   as in 
     type point = predefined { x : float; y : float }
   or
     type t = predefined A | B of bool * t

   Predefined read-only records or variant types (private)
   cannot be used as of version 0.9.3 of this program.


A common use of optional methods without a default argument is when omitting
an object field is allowed and considered equivalent to a null value.

Optional methods of a type that doesn't accept the null value don't make
much sense but are not rejected. The following is most likely an error:

  type json point = < ?x : int >

(if field "x" is not found in the JSON object, then the error would be
that null is not a valid int, instead of saying that field "x" is missing)

It should be either

  type json point = < ?x : int option >

or

  type json point = < ?x : int = 0 >



Example 1
=========

The following definition is correct:

type json point = < x: number; y: number >
and coords = point array

It can load successfully the following JSON data:

[ { "x": 1, "y": 0.5 },
  { "x": 0, "y": 0.3333333 } ]


Full example:

(* File example1.ml *)

type json point = < x: number; y: number >
and coords = point array

let json_string = "
  [ { \"x\": 1, \"y\": 0.5 },
    { \"x\": 0, \"y\": 0.3333333 } ]
"

let json_tree = Json_io.json_of_string json_string
let my_coords = coords_of_json json_tree
let _ = Array.iter (fun p -> Printf.printf "(%g, %g)\n" p#x p#y) my_coords

(* EOF *)

Save the example as "example1.ml", compile it and run it:

$ ocamlfind ocamlopt -o example1 -linkpkg -package json-static -syntax camlp4o example1.ml
$ ./example1
(1, 0.5)
(0, 0.333333)




Example 2
=========

This example shows you the representation that we chose for sum types in JSON:

(* File example2.ml *)

type json colors = [ `Black 
                   | `White 
                   | `Rgb of (float * float * float)
                   | `Any "*" ] list

let my_colors = [ `Black; `White; `Any;
                  `Rgb (1., 0., 0.);
                  `Rgb (0., 1., 0.);
                  `Rgb (0., 0., 1.) ]

let _ = print_endline (Json_io.string_of_json (json_of_colors my_colors))

(* EOF *)


$ ocamlfind ocamlopt -o example2 -linkpkg -package json-static -syntax camlp4o example2.ml
$ ./example2
[
  "Black",
  "White",
  "*",
  [ "Rgb",
    [ 1.0,
      0.0,
      0.0 ]
  ],
  [ "Rgb",
    [ 0.0,
      1.0,
      0.0 ]
  ],
  [ "Rgb",
    [ 0.0,
      0.0,
      1.0 ]
  ]
]

Note how we specified that `Any translates into "*" rather than "Any".
The same technique is available to rename object methods, and it is crucial
when some existing JSON format uses method names that are not valid
OCaml identifiers.
