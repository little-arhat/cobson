
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

let rec stake n s =
  if n > 0
  then Stream.icons (Stream.next s) & Stream.slazy (fun _ -> take (n-1) s)
  else Stream.sempty

(* from BatStream *)
let rec stake_while f s =
  Stream.slazy
    (fun _ ->
      match Stream.peek s with
        | Some h ->
          Stream.junk s;
          if f h
          then
             Stream.icons h (Stream.slazy (fun _ -> stake_while f s))
          else Stream.sempty
        | None -> Stream.sempty)

let sto_list s =
  let buf = ref [] in
  (Stream.iter (fun x -> buf := x :: !buf) s; List.rev !buf)

let sto_string s =
  let sl = sto_list s in
  let len = List.length sl in
  let st = String.create len
  in (List.iter (let i = ref 0 in fun x -> (st.[!i] <- x; incr i)) sl; st)

let stake_string = stake >>> sto_string
