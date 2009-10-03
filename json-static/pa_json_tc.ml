(*
  Conversion between OCaml types and JSON types as provided by the json-wheel
  library. 
  
  Author: Martin Jambon <martin_jambon@emailuser.net>

Copyright (c) 2007 Burnham Institute for Medical Research
Copyright (c) 2008 Martin Jambon
Copyright (c) 2009 Anil Madhavapeddy <anil@recoil.org>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(* This version was tested successfully with camlp4 3.10.0+beta.

   The upgrade from 3.09 to 3.10+beta was performed with the help 
   of Nicolas Pouillard.

   Command that compiles this program:

     ocamlc -c -pp camlp4orf -I +camlp4 \
        pa_json_static_3100beta.ml

   Before 3.10, it used to be: 
     ocamlc -c -pp 'camlp4o q_MLast.cmo pa_extend.cmo' -I +camlp4 \
        pa_json_static.ml


   Command that works for using this syntax extension when it is present
   in the current directory (not installed, no ocamlfind). It preprocesses
   a file that uses the json-static syntax and pretty-prints it to
   standard OCaml syntax:

     camlp4o -parser ./pa_json_static_3100beta.cmo -printer o example.ml

   Before 3.10, it used to be:
     camlp4o ./pa_json_static.cmo pr_o.cmo example.ml


   It passes the "make test" stage of the json-static package!
*)

open Camlp4.PreCast
open Printf

let check_unique f l =
  let tbl = Hashtbl.create 50 in
  List.iter
    (fun x -> 
       let (_loc, id) = f x in
       if Hashtbl.mem tbl id then
	 Loc.raise _loc
	   (Failure "this tag or label is not unique")
       else Hashtbl.add tbl id ())
    l

let unopt default = function
    None -> default
  | Some x -> x

let rec optmap f = function
    [] -> []
  | hd :: tl ->
      match f hd with
	  None -> optmap f tl
	| Some x -> x :: optmap f tl
    

type field = { field_caml_name : string;
	       field_json_name : string;
	       field_type : t;
	       field_caml_loc : Loc.t;
	       field_json_loc : Loc.t;
	       optional : bool;
	       default : Ast.expr option;
	       is_mutable : bool }

and constructor = { cons_caml_name : string;
		    cons_json_name : string;
		    cons_args : t list;
		    cons_caml_loc : Loc.t;
		    cons_json_loc : Loc.t }

and type_expr =
    List of t
  | Array of t
  | Option of t
  | Object of field list
  | Record of field list
  | Hashtbl of t
  | Assoc of t
  | Tuple of t list
  | Variant of constructor list
  | Poly of constructor list
  | Name of string
  | String
  | Bool
  | Int
  | Int32
  | Int64
  | Char
  | Unit
  | Float
  | Number
  | Raw
  | Custom of string

and t = Loc.t * type_expr

let numbered_list l =
  Array.to_list
    (Array.mapi 
       (fun i x -> (x, "x" ^ string_of_int i))
       (Array.of_list l))

let eta_expand = function
    <:expr< fun [ $_$ ] >> as f -> f
  | e -> let _loc = Ast.loc_of_expr e in <:expr< fun x -> $e$ x >>

let make_ofjson _loc l =
  let browse _loc f = <:expr< Json_type.Browse.$lid:f$ >> in

  let rec convert (_loc, def) =
    match def with
	List x -> <:expr< $browse _loc "list"$ $convert x$ >>
      | Array x -> 
	  <:expr< fun x -> 
	    Array.of_list (($browse _loc "list"$ $convert x$) x) >>
      | Option x -> 
	  <:expr< $browse _loc "optional"$ $convert x$ >>
      | Object l -> convert_object _loc l
      | Record r -> convert_record _loc r
      | Hashtbl x -> 
	  <:expr< 
	     fun x -> 
	       let l = $browse _loc "objekt"$ x in
	       let tbl = Hashtbl.create (List.length l) in
               do { List.iter (fun (s, x) -> 
				 Hashtbl.add tbl s ($convert x$ x)) l;
		    tbl } >>
      | Assoc x -> 
	  <:expr< fun x ->
	            List.map (fun (key, data) -> (key, $convert x$ data))
	              ($browse _loc "objekt"$ x) >>
      | Tuple l ->
	  let nl = numbered_list l in
	  let pl = 
	    List.fold_right 
	      (fun ((_loc, _), name) tl -> <:patt< [ $lid:name$ :: $tl$ ] >>) 
	      nl <:patt< [] >> in
	  let el = 
	    List.fold_right (fun ((_loc, _) as x, name) acc ->
			<:expr< $convert x$ $lid:name$, $acc$ >>)
	      nl <:expr<>> in
	  <:expr< fun [ Json_type.Array $pl$ -> ( $tup:el$ )
		      | Json_type.Array _ as x ->
			  __json_static_error x
			    "wrong number of elements in JSON array"
		      | x ->
			  __json_static_error x
			    "not a JSON array" ] >>
      | Poly l ->
	  convert_variants (fun _loc name -> <:expr< ` $name$ >>) _loc l
      | Variant l ->
	  convert_variants (fun _loc name -> <:expr< $uid:name$ >>) _loc l
      | Name x -> <:expr< $lid: x ^ "_of_json"$ >>
      | String -> browse _loc "string"
      | Bool -> browse _loc "bool"
      | Int -> browse _loc "int"
      | Float -> browse _loc "float"
      | Number -> browse _loc "number"
      | Raw -> <:expr< fun x -> x >>
      | Custom modul -> <:expr< $uid:modul$ . of_json >>
      | Unit -> <:expr< let browse_unit _ = () in browse_unit >>
      | Char -> <:expr< 
         let browse_char x = match $browse _loc "string"$ x with [
           "" -> raise (Json_type.Json_error "empty character")
         | x  -> x.[0] ] 
         in browse_char >>
      | Int32 -> <:expr<
         let browse_int32 x = Int32.of_float ($browse _loc "number"$ x) in
         browse_int32 >>
      | Int64 -> <:expr<
         let browse_int64 x = Int64.of_float ($browse _loc "number"$ x) in
         browse_int64 >>

   and convert_object _loc l =
     let pel = convert_object_field_list _loc l in
     let methods = 
       List.fold_right
	 (fun x acc ->
	    let name = x.field_caml_name in
            <:class_str_item< method $name$ = $lid:name$ ; $acc$ >>)
	 l <:class_str_item<>> in
     eval_with_tbl _loc <:expr< let $list:pel$ in object $methods$ end >>

  and convert_record _loc r =
     let pel = convert_record_field_list _loc r in
     eval_with_tbl _loc <:expr< { $list:pel$ } >>

  and convert_field_list _loc l =
     List.map 
       (fun { field_caml_name = name;
	      field_json_name = json_name;
	      field_type = x;
	      optional = optional;
	      default = default } ->
	  let e1 = 
	    let f = if optional then "fieldx" else "field" in
	    <:expr< Json_type.Browse.$lid:f$ tbl $str:json_name$ >> in
	  let e2 =
	    match default with
		Some e -> 
		  (<:expr< 
		   match $e1$ with 
		       [ Json_type.Null -> $e$
		       | x -> $convert x$ x ] >>)
	      | None -> <:expr< $convert x$ $e1$ >> in

	  (name, e2))
       l

  and convert_record_field_list _loc l = 
    List.map (fun (name, e) -> <:rec_binding< $lid:name$ = $e$ >>)
      (convert_field_list _loc l)

  and convert_object_field_list _loc l =
    List.map (fun (name, e) -> <:binding< $lid:name$ = $e$ >>)
      (convert_field_list _loc l)

  and convert_variants make_cons _loc l =
    let l0, l1 =
      List.partition (fun x -> x.cons_args = []) l in
    let pwel0 =
      List.fold_right
	(fun { cons_caml_name = name;
	       cons_json_name = json_name } acc ->
	   <:match_case< $str:json_name$ -> $make_cons _loc name$ | $acc$ >>)
	l0 <:match_case<>> in
    let pwel1 =
      List.fold_right
	(fun { cons_caml_name = name;
	       cons_json_name = json_name;
	       cons_args = args } acc ->
	   let argnames = numbered_list args in
	   let list_patt =
	     List.fold_right 
	       (fun (_, s) l -> 
		  <:patt< [ $lid:s$ :: $l$ ] >>)
	       argnames <:patt< [] >> in
	   let e =
	     List.fold_left
	       (fun cons (arg, s) -> 
		  <:expr< $cons$ ($convert arg$ $lid:s$) >>)
	     (make_cons _loc name) argnames in
	   <:match_case< ($str:json_name$, $list_patt$) -> $e$ | $acc$ >>)
	l1 <:match_case<>> in
    let default_case =
      <:match_case< _ -> __json_static_error x
                           "invalid variant name or \
                            wrong number of arguments" >>
    in
    
    (<:expr< 
     fun
	 [ Json_type.String s as x -> 
	     match s with [ $pwel0$ | $default_case$ ]
	       | Json_type.Array 
		   [ Json_type.String s :: ([ _ :: _ ] as args) ] as x -> 
		   match (s, args) with [ $pwel1$ | $default_case$ ]
	       | x -> __json_static_error x
		   "not able to read this as \
                    a variant" ]
     >>)


  and eval_with_tbl _loc e =
    (<:expr< 
     fun x ->
       let tbl = 
	 Json_type.Browse.make_table (Json_type.Browse.objekt x) in
       $e$ >>)
  in

  let error =
    <:str_item< 
    value __json_static_error obj msg =
      let m = 400 in
      let s = Json_io.string_of_json obj in
      let obj_string =
	if String.length s > m then String.sub s 0 (m - 4) ^ " ..."
	else s in
      Json_type.json_error (msg ^ ":\n" ^ obj_string) >> in

  let defs = 
    List.fold_right
      (fun ((_loc, name), x) acc -> 
	 (*if x.is_private then acc
	 else*)
	   let fname = name ^ "_of_json" in
           <:binding< ( $lid:fname$ : Json_type.t -> $lid:name$ ) = 
                      $eta_expand (convert x)$ and $acc$ >>)
      l <:binding<>>
  in
    <:str_item< $error$; value rec $defs$ >>

let make_tojson _loc l =
  let build _loc s = <:expr< Json_type.Build. $lid:s$ >> in

  let rec convert (_loc, def) =
    match def with
	List x -> <:expr< Json_type.Build.list $convert x$ >>
      | Array x -> 
	  <:expr< fun x -> 
                    Json_type.Build.list $convert x$ (Array.to_list x) >>
      | Option x -> <:expr< Json_type.Build.optional $convert x$ >>
      | Object l ->
	  convert_field_list (fun name -> <:expr< x#$lid:name$ >>) 
	    _loc l
      | Record r -> 
	  convert_field_list (fun name -> <:expr< x.$lid:name$ >>)
	    _loc r
      | Hashtbl x ->
	  <:expr< fun tbl -> 
	    Json_type.Object 
	      (Hashtbl.fold (fun key data tl -> 
			       [ (key, $convert x$ data) :: tl ])
		 tbl []) >>
      | Assoc x ->
	  <:expr< 
	    fun x ->
	      Json_type.Object
	        ((List.map (fun (key, data) -> (key, $convert x$ data))) x) >>
      | Tuple l ->
	  let nl = numbered_list l in
	  let pl = List.fold_right 
                    (fun (_, name) acc -> <:patt< $lid:name$, $acc$ >>)
                    nl <:patt<>> in
	  let a = List.fold_right 
		    (fun (x, name) tl -> 
		       <:expr< [ $convert x$ $lid:name$ :: $tl$ ] >>)
		    nl <:expr< [] >> in
	  <:expr< fun [ ( $tup:pl$ ) -> Json_type.Array $a$ ] >>
      | Poly l -> 
	  let match_cases =
	    List.map
	      (fun { cons_caml_name = name;
		     cons_json_name = json_name;
		     cons_args = args } ->
		 match args with
		     [] -> 
		       <:match_case< 
		          `$name$ -> Json_type.String $str:json_name$ >>
		   | [x] ->
		       <:match_case< 
		          `$name$ arg ->
		              Json_type.Array 
		                [ Json_type.String $str:json_name$;
			          $convert x$ arg ] >>
		   | _ -> assert false)
	      l in
	  <:expr< fun [ $list:match_cases$ ] >>
      | Variant v -> 
	  let match_cases =
	    List.map
	      (fun { cons_caml_name = name;
		     cons_json_name = json_name;
		     cons_args = args } ->
		 match args with
		     [] -> 
		       <:match_case< 
		          $uid:name$ -> Json_type.String $str:json_name$ >>
		   | l ->
		       let args = numbered_list l in
		       let p =
			 List.fold_left
			   (fun cons (_, s) -> <:patt< $cons$ $lid:s$ >>)
			   <:patt< $uid:name$ >> args in
		       let e =
			 List.fold_right
			   (fun (x, s) l -> 
			      <:expr< [ $convert x$ $lid:s$ :: $l$ ] >>)
			   args <:expr< [] >> in
		       <:match_case< $p$ ->
			Json_type.Array 
			  [ Json_type.String $str:json_name$ :: $e$ ] >>)
	      v in
	  <:expr< fun [ $list:match_cases$ ] >>
      | Name x -> <:expr< $lid: "json_of_" ^ x$ >>
      | String -> build _loc "string"
      | Bool -> build _loc "bool"
      | Int -> build _loc "int"
      | Int32 -> <:expr< let build_int32 x = Json_type.Build.float (Int32.to_float x) in build_int32 >>
      | Int64 -> <:expr< let build_int64 x = Json_type.Build.float (Int64.to_float x) in build_int64 >>
      | Unit -> <:expr< let build_unit () = Json_type.Build.int 0 in build_unit >>
      | Char -> <:expr< let build_char x = Json_type.Build.string (String.make 1 x) in build_char >>
      | Float -> build _loc "float"
      | Number -> build _loc "float"
      | Raw -> <:expr< fun x -> x >>
      | Custom modul -> <:expr< $uid:modul$ . to_json >>

  and convert_field_list access _loc l =
    let pairs = 
      List.fold_right
	(fun { field_caml_name = name;
	       field_json_name = json_name;
	       field_type = x } tl ->
	   <:expr< [ ( $str:json_name$, $convert x$ $access name$ )
		     :: $tl$ ] >>)
	l <:expr< [] >> in
    <:expr< fun x -> Json_type.Object $pairs$ >>
  in

  let defs = 
    List.fold_right
      (fun ((_loc, name), x) acc -> 
	 let fname = "json_of_" ^ name in
	 <:binding< ( $lid:fname$ : $lid:name$ -> Json_type.t ) =
	            $eta_expand (convert x)$ and $acc$ >>)
      l <:binding<>> in
  <:str_item< value rec $defs$ >>


let expand_typedefs _loc l =
  check_unique (fun (name, _) -> name) l;
  let ofjson = make_ofjson _loc l in
  let tojson = make_tojson _loc l in
  <:str_item< $ofjson$; $tojson$ >>

let type_fail ctyp msg =
  Loc.raise (Ast.loc_of_ctyp ctyp) (Failure msg)

let rec process_tds tds =
  let rec fn ty =
    match ty with
    |Ast.TyAnd (_loc, tyl, tyr) ->
       fn tyl @ (fn tyr)
    |Ast.TyDcl (_loc, id, _, ty, []) ->
       [ (_loc, id ) , (_loc, process_td _loc ty) ]
    | other -> type_fail other "process_tds: unexpected AST"
   in fn tds

and process_fields _loc cs =
  let rec fn = function
    | <:ctyp< $t1$; $t2$ >> -> fn t1 @ (fn t2)
    | <:ctyp< $lid:id$ : mutable $t$ >> -> fnt ~mut:true ~id ~t
    | <:ctyp< $lid:id$ : $t$ >> ->  fnt ~mut:false ~id ~t
    | other -> type_fail other "process_fields: unexpected AST"
  and fnt ~mut ~id ~t =
    [ { field_caml_name = id; field_json_name = id;
        field_type = (_loc, process_td _loc t);
        field_caml_loc = _loc; field_json_loc = _loc;
        optional = false; default=None; is_mutable = mut } ]
  in fn cs

and process_constructor _loc rf =
  List.map (function
    | <:ctyp< `$uid:id$ of $t$ >> 
    | <:ctyp< $uid:id$ of $t$ >> ->
       let cons_args = List.map (fun x -> _loc, process_td _loc x) (Ast.list_of_ctyp t []) in
       { cons_caml_name=id; cons_json_name=id; cons_caml_loc=_loc;
         cons_json_loc=_loc; cons_args=cons_args }
    | <:ctyp< `$uid:id$ >> 
    | <:ctyp< $uid:id$ >> ->
       { cons_caml_name=id; cons_json_name=id; cons_caml_loc=_loc;
         cons_json_loc=_loc; cons_args=[] }
    | other -> type_fail other "process_constructor: unexpected AST"
  ) (Ast.list_of_ctyp rf [])
 
and process_td _loc = function
 | <:ctyp< string >> -> String
 | <:ctyp< int >> -> Int
 | <:ctyp< float >> -> Float
 | <:ctyp< bool >> -> Bool
 | <:ctyp< int32 >> -> Int32
 | <:ctyp< int64 >> -> Int64
 | <:ctyp< unit >> -> Unit
 | <:ctyp< char >> -> Char

 | <:ctyp< option $t$ >> -> Option (_loc, process_td _loc t)
 | <:ctyp< list $t$ >> -> List (_loc, process_td _loc t)
 | <:ctyp< array $t$ >> -> Array (_loc, process_td _loc t)

 | <:ctyp< < $cs$ > >> -> Object (process_fields _loc cs)
 | <:ctyp< { $cs$ } >> -> Record (process_fields _loc cs)

 | <:ctyp< [< $rf$ ] >> 
 | <:ctyp< [> $rf$ ] >>
 | <:ctyp< [= $rf$ ] >> -> Poly (process_constructor _loc rf)
 | <:ctyp< [ $rf$ ] >> -> Variant (process_constructor _loc rf)
 | <:ctyp< ( $tup:tp$ ) >> -> 
   let tps = List.map 
     (fun t -> _loc, process_td _loc t) 
       (Ast.list_of_ctyp tp []) in
   Tuple tps

 | <:ctyp< $uid:id$.t >> -> Custom id (* XXX broken, how to check for TyApp? *)
 | <:ctyp< $lid:id$ >> -> Name id
 | other -> type_fail other "unknown_type"

open Pa_type_conv
let _ =
  add_generator "json"
   (fun tds -> <:str_item< $expand_typedefs Loc.ghost (process_tds tds)$ >>)
