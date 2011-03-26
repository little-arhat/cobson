
(* pipelining *)
let ( |> ) x f = f x
let ( <| ) f x = f x
let ( & ) f x = f x

(* composition, don't forget about -no-quot *)
let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)
let ( >>>) f g x y = g (f x y)
let ( % ) f g = fun x -> f (g x)
let ( %% ) f g = fun x y -> f (g x y)
let ( %%% ) f g = fun x y z -> f (g x y z)


(* `right currying` *)
let flip f x y = f y x

module Stream = struct
  include Stream

  let rec take n s =
    if n > 0
    then icons (next s) & slazy (fun _ -> take (n-1) s)
    else sempty

  let rec take_int32 n s =
    if n > 0l
    then icons (next s) & slazy (fun _ -> take_int32 (Int32.sub n 1l) s)
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

  let to_string s =
    let sl = to_list s in
    let len = List.length sl in
    let st = String.create len
    in (List.iter (let i = ref 0 in fun x -> (st.[!i] <- x; incr i)) sl; st)

  let take_string = take >>> to_string
  let take_string_int32 = take_int32 >>> to_string

end

module List = struct
  include List
  let length_int32 l = length l |> Int32.of_int
end

module String = struct
  include String
  let length_int32 l = length l |> Int32.of_int
end

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
