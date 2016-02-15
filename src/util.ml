

(* composition, don't forget about -no-quot *)
let ( >> ) f g x = g (f x)
let ( >>>) f g x y = g (f x y)

let flip f x y = f y x
let double x = (x, x)

(* combinators *)
let k_comb x _y = x
let s_comb x y z = x z (y z);;


module ExStream = struct
  open Stream

  let rec take n s =
    if n > 0
    then icons (next s) @@ slazy (fun _ -> take (n-1) s)
    else sempty

  let rec take_int32 n s =
    if n > 0l
    then icons (next s) @@ slazy (fun _ -> take_int32 (Int32.sub n 1l) s)
    else sempty

  (* from BatStream *)
  let rec take_while f s =
    slazy (fun _ -> match peek s with
      | Some h -> junk s;
        if f h
        then icons h (slazy (fun _ -> take_while f s))
        else sempty
      | None -> sempty)

  let to_list s =
    let buf = ref [] in
    (iter (fun x -> buf := x :: !buf) s; List.rev !buf)

  let to_string_opt ?(len=16) s =
    let buf = Buffer.create len in
    iter (fun ch -> Buffer.add_char buf ch) s; Buffer.contents buf
  let to_string s = to_string_opt ~len:16 s

  let to_string_fun fn s =
    let buf = Buffer.create 16 in
    iter (fun it -> Buffer.add_string buf @@ fn it ) s; Buffer.contents buf

  let take_string = take >>> to_string
  let take_string_int32 = take_int32 >>> to_string

end


let list_length_int32 l = List.length l |> Int32.of_int

let list_unfold cons pred start =
  let rec loop step acc =
      if pred step then acc
      else let (value, step') = cons step in
           loop step' (value::acc)
  in loop start []

let range ?(start=0) stop = list_unfold (pred >> double) ((==) start) stop

let str_length_int32 s = Int32.of_int @@ String.length s

(* resource management operations *)
let try_finally action finally =
  try
    let result = action () in
      finally ();
      result;
  with x -> finally (); raise x

let with_resource resource action post =
  try_finally (fun () -> action resource) (fun () -> post resource)

let with_file_in filename action =
  with_resource (open_in filename) action close_in
let with_file_out filename action =
  with_resource (open_out filename) action close_out

(* Ocaml batteries *)
type buffer =
    {mutable buffer : string;
     mutable position : int;
     mutable length : int;
     initial_buffer : string
    }

external buffer_of_t : Buffer.t -> buffer = "%identity"
external t_of_buffer : buffer -> Buffer.t = "%identity";;

let buffer_change_substring buf pos str =
  let b = buffer_of_t buf in
  String.blit str 0 b.buffer pos (String.length str)
