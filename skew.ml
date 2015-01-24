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

(* What if we perform one addition instead of two subtractions? *)
let skew2c str =
  let n = String.length str in
  let a = Array.make (n + 1) 0 in
  for i = 0 to (n - 1) do
    a.(i+1) <- update_skew_count a.(i) str.[i];
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
  let d = Core.Span.to_sec s in
  (d, r)

let prof n f =
  let r = ref 0.0 in
  let nf = Float.of_int n in
  for i = 1 to n do
    let (t, _) = time f in
    r := !r +. (t /. nf);
  done;
  !r


let () =
  let lst = In_channel.read_lines "TestFile.txt" in
  let sal = String.concat lst in
  let n   = 50 in
  Printf.printf "skew:    %f \n" (prof n (fun () -> skew sal));
  Printf.printf "skew2:   %f \n" (prof n (fun () -> skew2 sal));
  Printf.printf "skew2c:  %f \n" (prof n (fun () -> skew2c sal));
  Printf.printf "skew2b:  %f \n" (prof n (fun () -> skew2b sal));
  Printf.printf "skew2a:  %f \n" (prof n (fun () -> skew2a sal));
  Printf.printf "skew3:   %f \n" (prof n (fun () -> skew3 sal))

(* Open questions:
  Why do the timings go up and down? Memory alignment/GC?
  If I don't aggregate them in a profile
*)
