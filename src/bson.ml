open CalendarLib

open Util
open Binary

module ES = ExStream
module S = Stream

(* TODO: make functor to use custom types for list at least *)
(* TODO: use Res monad (manatki are cool!)  *)

exception MalformedBSON of string
let malformed s = raise (MalformedBSON s)

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
    | '\x04' -> Array (List.map snd <| parse_document st)
    | '\x05' -> BinaryData (parse_binary st)
    | '\x07' -> ObjectId (ES.take_string 12 st)
    | '\x08' -> Boolean (parse_boolean <| S.next st)
    | '\x09' -> Datetime (Calendar.from_unixfloat <| parse_double st)
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
    | _ -> malformed "parse_element: invalid type"
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
      let s = ES.take_int32 len' rest |> ES.to_string ~len:int_len
      in S.junk rest ; s (* junk trailing null *)
    | [< >] -> malformed "parse_string"
  and parse_subtype c st = match c with
    | '\x00' -> Generic st
    | '\x01' -> Function st
    | '\x02' -> GenericOld st
    | '\x03' -> UUID st
    | '\x05' -> MD5 st
    | '\x80' -> UserDefined st
    | _ -> malformed "invalid binary subtype!"
  and parse_binary = parser
    | [< len = parse_int32; 'c; st >] -> ES.take_string_int32 len st |>
                                         parse_subtype c
    | [< >] -> malformed "parse_binary"
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
    | Some _ -> malformed "data after trailing null byte!"

let decode_string = S.of_string >> decode_stream

let decode_file = flip with_file_in <| S.of_channel >> decode_stream

let encode_to_stream document =
  let rec encode_document doc = match doc with
    | (key, element)::tail ->
      S.lapp (fun _ -> encode_cstring key)
        (S.lapp (fun _ -> encode_element element) <|
           S.slazy (fun _ -> encode_document tail))
    | _ -> [< '"\x00" >]
  and encode_list l = S.sempty
  and encode_element el = match el with
    | Double d -> [< '"\x01"; '(pack_float d) >]
    | String s -> [< '"\x02"; encode_string s >]
    | Document d -> let len = list_length_int32 d in
                    [< '"\x03"; '(pack_int32 len); encode_document d >]
    | Array l -> [< '"\x04"; encode_list l >]
    | BinaryData bd -> [< '"\x05"; encode_binary bd >]
    | ObjectId s -> [< '"\x07"; 's >]
    | Boolean b -> [< '"\x08"; '(if b then "\x01" else "\x00") >]
    | Datetime dt -> [< '"\x09"; '(Calendar.to_unixfloat dt |> pack_float) >]
    | Null -> [< '"\x0A" >]
    | Regex (first, sec) -> [< '"\x0B"; encode_cstring first;
                               encode_cstring sec >]
    | JSCode s -> [< '"\x0D"; encode_string s >]
    | Symbol s -> [< '"\x0E"; encode_string s >]
    | JSCodeWithScope (s, d) ->
      let len = Int32.add (str_length_int32 s) (list_length_int32 d) |>
                Int32.add 10l (* 2*4 - int32 fields + 2*1 -- trailing nulls *)
      in [< '"\x0f"; '(pack_int32 len); encode_string s; encode_document d >]
    | Int32 i -> [< '"\x10"; '(pack_int32 i) >]
    | Timestamp l -> [< '"\x11"; '(pack_int64 l) >]
    | Int64 l -> [< '"\x12"; '(pack_int64 l) >]
    | Minkey -> [< '"\xFF" >]
    | Maxkey -> [< '"\x7F" >]
  and encode_binary bd = [< >]
  and encode_string s = [< '(str_length_int32 s |> pack_int32);
                           (encode_cstring s) >]
  and encode_cstring s = [< '"\x00" >] (* FIX IMPLEMENT ENCODE*)
  in
  let len = list_length_int32 document in
  Stream.icons (pack_int32 len) <| encode_document document
