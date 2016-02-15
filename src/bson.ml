open CalendarLib

open Util
open Binary

module ES = ExStream
module S = Stream

(* TODO: make functor to use custom types for list at least *)
(* TODO: use Res monad (manatki are cool!)  *)
(* TODO: check ranges for timestamps *)

exception MalformedBSON of string
let malformed s = raise (MalformedBSON s)

type cstring = string
type objectid = string

exception IncorrectValue of string
let incorrect s = raise (IncorrectValue s)

let from_objectid x = x
let to_objectid x =
  if String.length x = 12 then x
  else incorrect x

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
and binary =
  | Generic of string
  | Function of string
  | GenericOld of string
  | UUID of string
  | MD5 of string
  | UserDefined of string
and document = (cstring * element) list
and array = element list (* array instead of list? *)


let decode_stream bytes =
  let rec parse_document = parser
    | [< len = parse_int32; st; >] ->
      parse_list [] (ES.take_int32 len st)
    | [< >] -> malformed "parse_document"
  and parse_list acc = parser
    | [< ''\x00' >] -> List.rev acc
    | [< 'code; key = parse_cstring; el = parse_element code; st >] ->
      parse_list ((key, el)::acc) st
    | [< >] -> malformed "parse_list : doesn't contain null byte"
  and parse_element c st = match c with
    | '\x01' -> Double (parse_double st)
    | '\x02' -> String (parse_string st)
    | '\x03' -> Document (parse_document st)
    | '\x04' -> Array (List.map snd @@ parse_document st)
    | '\x05' -> BinaryData (parse_binary st)
    | '\x07' -> ObjectId (ES.take_string 12 st)
    | '\x08' -> Boolean (parse_boolean @@ S.next st)
    | '\x09' -> Datetime (Calendar.from_unixfloat @@ parse_double st)
    | '\x0A' -> Null
    | '\x0B' -> let first = parse_cstring st in
                let sec = parse_cstring st in
                Regex (first, sec)
    | '\x0D' -> JSCode (parse_string st)
    | '\x0E' -> Symbol (parse_string st)
    | '\x0F' -> JSCodeWithScope (
      (s_comb (flip ES.take_int32) parse_int32 st) |> parse_jscode)
    | '\x10' -> Int32 (parse_int32 st)
    | '\x11' -> Timestamp (parse_int64 st)
    | '\x12' -> Int64 (parse_int64 st)
    | '\xFF' -> Minkey
    | '\x7F' -> Maxkey
    | c -> malformed @@
      Printf.sprintf "parse_element: invalid type: %s" (Char.escaped c)
  and parse_cstring = ES.take_while (fun c -> c <> '\x00') >> ES.to_string
  and parse_double = ES.take_string 8 >> unpack_float
  and parse_int32 = ES.take_string 4 >> unpack_int32
  and parse_int64 = ES.take_string 8 >> unpack_int64
  and parse_boolean = function
    | '\x00' -> false
    | '\x01' -> true
    |  _ -> malformed "parse_boolean"
  and parse_string = parser
    | [< len = parse_int32; rest >] ->
      let len' = Int32.sub len 1l in
      let int_len = Int32.to_int len' in
      let s = ES.take_int32 len' rest |> ES.to_string_opt ~len:int_len
      in S.junk rest ; s (* junk trailing null *)
    | [< >] -> malformed "parse_string"
  and parse_binary = parser
    | [< len = parse_int32; 'c; st >] -> ES.take_string_int32 len st |>
                                         parse_subtype c
    | [< >] -> malformed "parse_binary"
  and parse_subtype c st = match c with
    | '\x00' -> Generic st
    | '\x01' -> Function st
    | '\x02' -> GenericOld st
    | '\x03' -> UUID st
    | '\x05' -> MD5 st
    | '\x80' -> UserDefined st
    | _ -> malformed "invalid binary subtype!"
  and parse_jscode = parser
    | [< st = parse_string; doc = parse_document >] -> (st, doc)
    | [< >] -> malformed "parse_jscode"
  in
  let res =
    try parse_document bytes
    with S.Failure -> malformed "malformed bson data"
  in
  match S.peek bytes with
    | None -> res
    | Some c ->
      let () = print_int (Char.code c); print_newline() in
      malformed "data after trailing null byte!"

let decode_string = S.of_string >> decode_stream

let decode = decode_string

let decode_file = flip with_file_in @@ S.of_channel >> decode_stream

let encode_to_buffer document =
  let buf = Buffer.create 16 in
  let addc = Buffer.add_char buf in
  let adds = Buffer.add_string buf in
  let curpos () = Buffer.length buf in
  let dummy = "\000\000\000\000" in
  let patch_length pos =
    let len = Int32.of_int @@ (Buffer.length buf) - pos in
    buffer_change_substring buf pos @@ pack_int32 len
  in
  let rec encode_document doc pos = match doc with
    | (key, element)::tail ->
      encode_element element (fun chr -> addc chr; encode_cstring key);
      encode_document tail pos
    | _ -> addc '\x00'; patch_length pos
  and encode_element el addp = match el with
    | Double d -> addp '\x01'; adds @@ pack_float d
    | String s -> addp '\x02'; encode_string s
    | Document d -> let () = addp '\x03' in
                    let pos = curpos () in
                    adds dummy; encode_document d pos
    | Array l -> let len = List.length l in
                 let d = List.combine (List.map string_of_int @@ range len) l in
                 let () = addp '\x04' in
                 let pos = curpos () in
                 adds dummy; encode_document d pos
    | BinaryData bd -> addp '\x05'; encode_binary bd
    | ObjectId s -> addp '\x07'; adds s
    | Boolean b -> addp '\x08'; addc (if b then '\x01' else '\x00')
    | Datetime dt -> addp '\x09'; adds @@ pack_float @@ Calendar.to_unixfloat dt
    | Null -> addp '\x0A'
    | Regex (first, sec) -> addp '\x0B'; encode_cstring first; encode_cstring sec
    | JSCode s -> addp '\x0D'; encode_string s
    | Symbol s -> addp '\x0E'; encode_string s
    | JSCodeWithScope (s, d) -> let () = addp '\x0f' in
                                let pos_js = curpos () in
                                let () = adds dummy; encode_string s in
                                let pos_d = curpos () in
                                encode_document d pos_d; patch_length pos_js
    | Int32 i -> addp '\x10'; adds @@ pack_int32 i
    | Timestamp l -> addp '\x11'; adds @@ pack_int64 l
    | Int64 l -> addp '\x12'; adds @@ pack_int64 l
    | Minkey -> addp '\xFF'
    | Maxkey -> addp '\x7F'
  and encode_string s =
    (* length with trailing null byte *)
    let len = Int32.add 1l @@ str_length_int32 s in
    adds @@ pack_int32 len; encode_cstring s
  and encode_cstring s = adds s; addc '\x00'
  and encode_binary bd =
    (* think, that i should patch length here too *)
    let (c, st) = encode_subtype bd in
    let len = str_length_int32 st in
    adds @@ pack_int32 len; addc c; adds st
  and encode_subtype bd = match bd with
    | Generic st -> ('\x00', st)
    | Function st -> ('\x01', st)
    | GenericOld st -> ('\x02', st)
    | UUID st -> ('\x03', st)
    | MD5 st -> ('\x05', st)
    | UserDefined st -> ('\x80', st)
  in
  let () = adds dummy; encode_document document 0 in
  buf

let encode_to_string = encode_to_buffer >> Buffer.contents

let encode = encode_to_string
