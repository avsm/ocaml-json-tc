type field = {
  field_caml_name : string;
  field_json_name : string;
  field_type : t;
  optional : bool;
  json_default : Json_type.t option;
  json_subset : json_subset option;
  is_mutable : bool;
}
and json_subset =
    [ `Abstract_set 
    | `Dynlist of unit -> Json_type.t list 
    | `List of Json_type.t list ]
and constructor = {
  cons_caml_name : string;
  cons_json_name : string;
  cons_args : t list;
}
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
and typedef = {
  typename : string;
  def : t;
  is_predefined : bool;
  is_private : bool;
}
val __json_static_error : Json_type.t -> string -> 'a
val field_of_json : Json_type.t -> field
val json_subset_of_json : Json_type.t -> json_subset
val constructor_of_json : Json_type.t -> constructor
val t_of_json : Json_type.t -> t
val typedef_of_json : Json_type.t -> typedef
val json_of_field : field -> Json_type.t
val json_of_json_subset : json_subset -> Json_type.t
val json_of_constructor : constructor -> Json_type.t
val json_of_t : t -> Json_type.t
val json_of_typedef : typedef -> Json_type.t
