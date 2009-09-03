(*
Initialization commands to type in the ocaml toplevel:

#use "topfind";;
#camlp4o;;
#require "json-wheel";;
#load "pa_json_static.cmo";;

*)


(**************** Miscellaneous tests (should compile fine) ************)

type coord = { x : int; 
	       y : int;
	       z : int }

type json variant = 
    A 
  | B of coord 
  | C of float * variant 
  | D of (bool * int option)

and coord = predefined { 
  x : int;
  y : int;
  z : int
}


type json a = b list
and b = int

type json c = (string * d * d) list
and d = [ `A ]
