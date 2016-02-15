(* kiyoto (caml-mongo) *)

(* packing and unpacking binary
 * Heavily reference Jane Street Core, but packing is done in place,
 * and per BSON's spec, little endian is used. *)

let pack_int32 n =
  let buf = Bytes.create 4 in
    Bytes.set buf 3 @@
      Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 24));
    Bytes.set buf 2 @@
      Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 16));
    Bytes.set buf 1 @@
      Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 8));
    Bytes.set buf 0 @@ Char.unsafe_chr (0xFF land Int32.to_int n);
    buf

let unpack_int32 buf =
  let pos = 0 in
  Int32.logor
    (Int32.shift_left (Int32.of_int (Char.code buf.[pos + 3])) 24)
    (Int32.of_int
      ((Char.code buf.[pos + 2] lsl 16)
        lor (Char.code buf.[pos + 1] lsl 8)
        lor (Char.code buf.[pos])))

let pack_int64 v =
  let buf = Bytes.create 8 in
    let top3 = Int64.to_int (Int64.shift_right v 40) in
    let mid3 = Int64.to_int (Int64.shift_right v 16) in
    let bot2 = Int64.to_int v in
    Bytes.set buf 7 @@ Char.unsafe_chr (0xFF land (top3 lsr 16));
    Bytes.set buf 6 @@ Char.unsafe_chr (0xFF land (top3 lsr 8));
    Bytes.set buf 5 @@ Char.unsafe_chr (0xFF land top3);
    Bytes.set buf 4 @@ Char.unsafe_chr (0xFF land (mid3 lsr 16));
    Bytes.set buf 3 @@ Char.unsafe_chr (0xFF land (mid3 lsr 8));
    Bytes.set buf 2 @@ Char.unsafe_chr (0xFF land mid3);
    Bytes.set buf 1 @@ Char.unsafe_chr (0xFF land (bot2 lsr 8));
    Bytes.set buf 0 @@ Char.unsafe_chr (0xFF land bot2);
    buf

let unpack_int64 buf =
  let pos = 0 in
  Int64.logor
    (Int64.logor
      (Int64.shift_left
        (Int64.of_int (Char.code buf.[pos + 7] lsl 16
                        lor Char.code buf.[pos + 6] lsl 8
                        lor Char.code buf.[pos + 5]))
        40)
      (Int64.shift_left
        (Int64.of_int (Char.code buf.[pos + 4] lsl 16
                        lor Char.code buf.[pos + 3] lsl 8
                        lor Char.code buf.[pos + 2]))
        16))
    (Int64.of_int (Char.code buf.[pos + 1] lsl 8
                    lor Char.code buf.[pos]))

let pack_float f = pack_int64 (Int64.bits_of_float f)

let unpack_float buf = Int64.float_of_bits (unpack_int64 buf)
