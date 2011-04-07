
open QuickCheck
open QuickCheck_gen

open Bson

let arbitrary_element = oneof [ret_gen Minkey; ret_gen Maxkey]

let arbitrary_cstring = arbitrary_string

let arbitrary_item = arbitrary_pair arbitrary_cstring arbitrary_element

let arbitrary_document = arbitrary_list arbitrary_item

let show_element = function
  | Minkey -> "Minkey"
  | Maxkey -> "Maxkey"
  | _ -> ""

let show_cstring = show_string

let show_item = show_pair show_cstring show_element

let show_document = show_list show_item

let testable_doc_to_bool =
  testable_fun arbitrary_document show_document testable_bool

let cl = quickCheck testable_doc_to_bool

let prop_parseunparse doc =
  try
    decode (encode doc) = doc
  with MalformedBSON s ->
    let () = print_endline s in
    false

let main () =
  cl prop_parseunparse

let () = main ()
