(* test file for the type-conv version, compile with "make jtc-check_tc" *)

type x = [ `One | `Two ]
and y = Three of int | Four of (char * int32 * int64)
and t = { 
  foo: string;
  bar: int;
  wha: bool;
  bla: string * int * unit;
  pol: x;
  vat: y; 
  h: (string, int64) Hashtbl.t;
  a: (string * string) list
} with json

type o = <
  foo: string;
  bar: int
> and
odd = {
  fn: int -> int;
  fn2: unit
} with json(skip:odd)

let _ = 
  let h = Hashtbl.create 1 in 
  Hashtbl.add h "a" 1L;
  Hashtbl.add h "b" 2L;
  let a = [ "one", "ONE"; "two","TWO"; "three","THREE" ] in
  let t = { foo="foo"; bar=10; wha=true; bla=("foo1",5,()); pol=`Two; vat=(Four ('x',500l,6000L)); h=h; a=a } in
  let json_string = Json_io.string_of_json (json_of_t t) in
  let _ = t_of_json (Json_io.json_of_string json_string) in
  prerr_endline json_string;
  let o = object method foo="foo" method bar=1 end in
  let json_string = Json_io.string_of_json (json_of_o o) in
  let json_o = o_of_json (Json_io.json_of_string json_string) in
  assert (json_o#foo = o#foo);
  assert (json_o#bar = o#bar);
  prerr_endline json_string
