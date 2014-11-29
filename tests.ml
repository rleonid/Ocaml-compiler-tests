
let ml n = 
  let rec ml_acc acc i = 
    if i < 0 
    then acc 
    else ml_acc (i::acc) (i - 1)
  in ml_acc [] n ;;

let rec non_taily_ml n =
  if n = 0
  then []
  else n::(non_taily_ml (n - 1))

let rec find_break n =
  let _ = non_taily_ml n in
  Printf.printf "passsed %d\n" n;
  find_break (n * 10)

(* let () = find_break 1000;; *)
let break_size = 1_000_000 ;;

let is_prefix t p =
  let t_a = Array.of_list t in    (* Avoid List.nth *)
  let p_a = Array.of_list p in
  let n = List.length p in
  let rec loop i =
    (* Is this tail rec? ... Yes! *)
    i = n || (t_a.(i) = p_a.(i)) && loop (i + 1)       
  in
  loop 0

let ()   =
  let long = ml break_size in
  let pre  = List.rev (List.tl (List.rev long)) in
  let res  = is_prefix long pre in
  Printf.printf "%B all done\n" res;;
