
let test_data = "@\x02\x00\x00\x05tock\x00\t\x00\x00\x00\x00tick tock\x04piggie\x00;\x01\x00\x00\x050\x00\x04\x00\x00\x00\x00Tick\x051\x00\x05\x00\x00\x00\x00tock,\x052\x00\x04\x00\x00\x00\x00tick\x053\x00\x05\x00\x00\x00\x00tock,\x054\x00\x04\x00\x00\x00\x00tick\x055\x00\x04\x00\x00\x00\x00tock\x056\x00\x05\x00\x00\x00\x00Three\x057\x00\x07\x00\x00\x00\x00piggies\x058\x00\x08\x00\x00\x00\x00standing\x059\x00\x02\x00\x00\x00\x00in\x0510\x00\x03\x00\x00\x00\x00the\x0511\x00\x04\x00\x00\x00\x00dock\x0512\x00\x05\x00\x00\x00\x00Their\x0513\x00\x07\x00\x00\x00\x00pockets\x0514\x00\x03\x00\x00\x00\x00all\x0515\x00\x05\x00\x00\x00\x00empty\x0516\x00\x03\x00\x00\x00\x00and\x0517\x00\x04\x00\x00\x00\x00bare\x0518\x00\x05\x00\x00\x00\x00Their\x0519\x00\x08\x00\x00\x00\x00futures,\x0520\x00\x05\x00\x00\x00\x00their\x0521\x00\x06\x00\x00\x00\x00homes,\x0522\x00\x07\x00\x00\x00\x00nowhere\x00\x10auf wiedersehn\x00\x17\x00\x00\x00\x01do vdjenja\x00\x00\x00\x00\x00\x00\x00E@\x12rrivederci\x00]\x00\x00\x00\x00\x00\x00\x00\x02tick\x00\x9e\x00\x00\x00This is a story about people who like to waste time... waste my time, waste your time; but time is against them. Time is everyone's master, including them...\x00\x00"

let parsed_data =  [
  ("tock", Bson.BinaryData (Bson.Generic "tick tock"));
  ("piggie",
   Bson.Array
     [Bson.BinaryData (Bson.Generic "Tick");
      Bson.BinaryData (Bson.Generic "tock,");
      Bson.BinaryData (Bson.Generic "tick");
      Bson.BinaryData (Bson.Generic "tock,");
      Bson.BinaryData (Bson.Generic "tick");
      Bson.BinaryData (Bson.Generic "tock");
      Bson.BinaryData (Bson.Generic "Three");
      Bson.BinaryData (Bson.Generic "piggies");
      Bson.BinaryData (Bson.Generic "standing");
      Bson.BinaryData (Bson.Generic "in");
      Bson.BinaryData (Bson.Generic "the");
      Bson.BinaryData (Bson.Generic "dock");
      Bson.BinaryData (Bson.Generic "Their");
      Bson.BinaryData (Bson.Generic "pockets");
      Bson.BinaryData (Bson.Generic "all");
      Bson.BinaryData (Bson.Generic "empty");
      Bson.BinaryData (Bson.Generic "and");
      Bson.BinaryData (Bson.Generic "bare");
      Bson.BinaryData (Bson.Generic "Their");
      Bson.BinaryData (Bson.Generic "futures,");
      Bson.BinaryData (Bson.Generic "their");
      Bson.BinaryData (Bson.Generic "homes,");
      Bson.BinaryData (Bson.Generic "nowhere")]);
  ("auf wiedersehn", Bson.Int32 23l); ("do vdjenja", Bson.Double 42.);
  ("rrivederci", Bson.Int64 93L);
  ("tick",
   Bson.String
     "This is a story about people who like to waste time... waste my time, waste your time; but time is against them. Time is everyone's master, including them...")]

let test_eq () =
  let pr = Bson.decode_string test_data in
  if (pr = parsed_data)
  then ()
  else raise (Failure "parsed data isn't equal to sample")

let main () =
  let () = test_eq () in
  print_endline "All tests succeeded."

let () = main ()
