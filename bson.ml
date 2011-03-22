open CalendarLib

(* Thanks to: kiyoto (caml-mongo) *)


(* TODO: csting shouldn't contain null byte -- implement it *)
(* TODO: objectid must be 12 bytes long -- implement it *)
(* TODO: make functor to use custom types for list at least *)


type  bson_element =
  | BSON_Double of float
  | BSON_String of bson_string
  | BSON_Document of bson_document
  | BSON_Array of bson_array
  | BSON_BinaryData of bson_binary (* change it *)
  | BSON_Objectid of bson_objectid
  | BSON_Datetime of Calendar.t
  | BSON_Null
  | BSON_Boolean of bool
  | BSON_Regex of (bson_cstring * bson_cstring)
  | BSON_JSCode of bson_string
  | BSON_Symbol of bson_string
  | BSON_JSCodeWithScope of (bson_string * bson_document)
  | BSON_Int32 of int32
  | BSON_Timestamp of int64
  | BSON_Int64 of int64
  | BSON_Minkey
  | BSON_Maxkey
and bson_cstring = UTF8.t
and bson_string = UTF8.t
and bson_objectid = UTF8.t
and bson_binary =
  | BSON_Generic of bson_string
  | BSON_Function of bson_string
  | BSON_GenericOld of bson_string
  | BSON_UUID of bson_string
  | BSON_MD5 of bson_string
  | BSON_UserDefined of bson_string
and bson_document = (bson_cstring * bson_value) list
and bson_array = bson_value list (* array instead of list? *)



