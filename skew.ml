let update_skew_count c = function
    | 'C' -> (c - 1)
    | 'G' -> (c + 1)
    | 'A'
    | 'T' -> c
    | x   -> raise (Invalid_argument (Printf.sprintf "skew: %c" x))

let skew str =
  let n = String.length str in
  let c = ref 0 in
  Array.init (n + 1) (function 0 -> 0 | i ->
    let nc = update_skew_count !c str.[i-1] in
    c := nc;
    nc)

let skew2 str =
  let n = String.length str in
  let a = Array.make (n + 1) 0 in
  for i = 1 to n do
    a.(i) <- update_skew_count a.(i-1) str.[i-1];
  done;
  a

(* What if we explicitly calculate the offset index. *)
let skew2b str =
  let n = String.length str in
  let a = Array.make (n + 1) 0 in
  for i = 1 to n do
    let j = i - 1 in
    a.(i) <- update_skew_count a.(j) str.[j];
  done;
  a

let skew2a str =
  let n = String.length str in
  let a = Array.make (n + 1) 0 in
  let rec loop i =
    if i > n then
      a
    else begin
      a.(i) <- update_skew_count a.(i-1) str.[i-1];
      loop (i + 1)
    end
  in
  loop 1

let skew3 str =
  let n = String.length str in
  let a = Array.make 1 0 in
  let c = ref 0 in
  let b = Array.init n (fun i ->
    let nc = update_skew_count !c str.[i] in
    c := nc;
    nc) in
  Array.append a b

(* Current (2015-01-22) native test rankings:
   skew2 ~ skew2b >   hard to distinguish
   skew2a >   for loops faster than manual loops
   skew >     Array make + loop faster than direct Array init
   skew3      Array append is slow
   *)
open Core.Std

let time f =
  let n = Time.now () in
  let r = f () in
  let s = Time.diff (Time.now ()) n in
  Printf.printf "%f\n" (Core.Span.to_sec s);
  r

let () =
  let lst = In_channel.read_lines "TestFile.txt" in
  let sal = String.concat lst in
  Printf.printf "skew ----\n";
  let _ = time (fun () -> skew sal) |> ignore in
  let _ = time (fun () -> skew sal) |> ignore in
  let _ = time (fun () -> skew sal) |> ignore in
  let _ = time (fun () -> skew sal) |> ignore in
  Printf.printf "skew2 ----\n";
  let _ = time (fun () -> skew2 sal) |> ignore in
  let _ = time (fun () -> skew2 sal) |> ignore in
  let _ = time (fun () -> skew2 sal) |> ignore in
  let _ = time (fun () -> skew2 sal) |> ignore in
  Printf.printf "skew2b ----\n";
  let _ = time (fun () -> skew2b sal) |> ignore in
  let _ = time (fun () -> skew2b sal) |> ignore in
  let _ = time (fun () -> skew2b sal) |> ignore in
  let _ = time (fun () -> skew2b sal) |> ignore in
 Printf.printf "skew2a ----\n";
  let _ = time (fun () -> skew2a sal) |> ignore in
  let _ = time (fun () -> skew2a sal) |> ignore in
  let _ = time (fun () -> skew2a sal) |> ignore in
  let _ = time (fun () -> skew2a sal) |> ignore in
  Printf.printf "skew3 ----\n";
  let _ = time (fun () -> skew3 sal) |> ignore in
  let _ = time (fun () -> skew3 sal) |> ignore in
  let _ = time (fun () -> skew3 sal) |> ignore in
  let _ = time (fun () -> skew3 sal) |> ignore in
  ()

(* Open questions:
  Why do the timings go up and down? Memory alignment/GC?
*)
