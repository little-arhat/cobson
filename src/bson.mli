open CalendarLib

exception MalformedBSON of string

type element =
  | Double of float
  | String of string
  | Document of document
  | Array of array
  | BinaryData of binary (* change it *)
  | ObjectId of objectid
  | Datetime of Calendar.t
  | Null
  | Boolean of bool
  | Regex of (cstring * cstring)
  | JSCode of string
  | Symbol of string
  | JSCodeWithScope of (string * document)
  | Int32 of int32
  | Timestamp of int64
  | Int64 of int64
  | Minkey
  | Maxkey
and cstring = UTF8.t
and string = UTF8.t
and objectid = UTF8.t
and binary =
  | Generic of string
  | Function of string
  | GenericOld of string
  | UUID of string
  | MD5 of string
  | UserDefined of string
and document = (cstring * element) list
and array = element list (* array instead of list? *)

val decode_stream : char Stream.t -> document

val decode_file : string -> document

val decode_string : string -> document
