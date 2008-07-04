(*
  Runtime library for json-static.
  
  Author: Martin Jambon <martin_jambon@emailuser.net>

Copyright (c) 2007 Wink Technologies Inc.
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

module Dynlist =
struct
  type t = unit -> Json_type.t list
  let to_json x = Json_type.Array (x ())
  let of_json x = 
    let l = Json_type.Browse.array x in
    (fun () -> l)
end

type json field = { field_caml_name : string;
		    field_json_name : string;
		    field_type : t;
		    optional : bool;
		    json_default : Json_type.t option;
		    json_subset : json_subset option;
		    is_mutable : bool }

and json_subset =
    [ `List of Json_type.t list
    | `Dynlist of Dynlist.t
    | `Abstract_set ]

and constructor = { cons_caml_name : string;
		    cons_json_name : string;
		    cons_args : t list }

and t =
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
  | Float
  | Number
  | Raw


and typedef = { typename : string;
		def : t;
		is_predefined : bool;
		is_private : bool }

