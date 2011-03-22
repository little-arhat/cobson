open CalendarLib

(* Thanks to: kiyoto (caml-mongo) *)


(* TODO: csting shouldn't contain null byte -- implement it *)
(* TODO: objectid must be 12 bytes long -- implement it *)
(* TODO: make functor to use custom types for list at least *)


type bson_element =
  | Double of float
  | String of bson_string
  | Document of bson_document
  | Array of bson_array
  | BinaryData of bson_binary (* change it *)
  | Objectid of bson_objectid
  | Datetime of Calendar.t
  | Null
  | Boolean of bool
  | Regex of (bson_cstring * bson_cstring)
  | JSCode of bson_string
  | Symbol of bson_string
  | JSCodeWithScope of (bson_string * bson_document)
  | Int32 of int32
  | Timestamp of int64
  | Int64 of int64
  | Minkey
  | Maxkey
and bson_cstring = UTF8.t
and bson_string = UTF8.t
and bson_objectid = UTF8.t
and bson_binary =
  | Generic of bson_string
  | Function of bson_string
  | GenericOld of bson_string
  | UUID of bson_string
  | MD5 of bson_string
  | UserDefined of bson_string
and bson_document = (bson_cstring * bson_value) list
and bson_array = bson_value list (* array instead of list? *)

let par
