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

(******************* Main example with runtime test **********************)
(*
module Custom =
struct
  type t = int
  let of_json = Json_type.Browse.int
  let to_json = Json_type.Build.int
  let typedef = Run_json_static.Int
end


type json t = < x: int list list;
                y: z;
		assoc: (string * int) assoc;
		?opt1: string option;
		?opt2: string = "abc" >
and z = [ `A 
	| `B "b!" of bool
	| `C of (Json_type.json_type * (string, number) Hashtbl.t)
	| `Custom of (Custom.t * int array * z * z) ]


(* Another type that accepts exactly the same data *)
type json t2 = { x: int list list;
                 y: z2;
		 assoc: (string * int) assoc;
		 ?opt1: string option;
		 ?opt2: string = "abc" }
and z2 = 
    A 
  | B "b!" of bool
  | C of (Json_type.json_type * (string, number) Hashtbl.t)
  | Custom of (Custom.t * int array * z2 * z2)


let sample = "
{ \"x\" : [ [1], [2, 3] ],
  \"y\" : [ \"Custom\", 
              [ 123, 
                [ 4, 5, 6 ], 
		[ \"b!\", true ], 
                [ \"C\", [ null, { \"a\" : 100,
                                   \"b\" : 3.14 } ] ] ] ],
  \"assoc\": { \"x1\": 1, \"x2\": 2, \"x3\": 3 }
}
"

let json = Json_io.json_of_string sample
let obj = t_of_json json
let json' = json_of_t obj
let sample' = Json_io.string_of_json json'

let json2 = Json_io.json_of_string sample
let obj2 = t2_of_json json
let json2' = json_of_t2 obj2
let sample2' = Json_io.string_of_json json2'



let _ =
  print_endline sample';
  assert (sample' = sample2');
  assert (json' = json2');

  (match obj#y with
       `Custom (_, _, _, `C (_, tbl)) -> 
	 (try assert (Hashtbl.find tbl "a" = 100.)
	  with Not_found -> assert false);
     | _ -> assert false);
  print_endline "Passed!"
*)
