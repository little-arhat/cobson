
open QuickCheck
open QuickCheck_gen


module Arbitrary_objectid = struct
  type t = Bson.objectid
  let arbitrary =
    such_that (fun s -> String.length s = 12)  Arbitrary_string.arbitrary
end

module Testable_objectid_to_bool =
  Testable_fun
    (Arbitrary_objectid)
    (PShow_string)
    (Testable_bool)

module CO = Check(Testable_objectid_to_bool)

let prop_length oid = String.length oid = 12


let main () =
  CO.quickCheck prop_length

let () = main ()
