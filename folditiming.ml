
let time f =
  let n = Sys.time () in
  let _ = f () in
  Sys.time () -. n

let profile f n = 
  let r = ref 0.0 in
  for i = 0 to n - 1 do
    r := !r +. time f;
  done;
  !r /. (float_of_int n)

let million = 1000000
let lst = 
  let rec loop i acc = if i = 0 then 0 :: acc else loop (i - 1) (i :: acc) in
  loop million []

let foldi lst ~f ~init = 
  snd (ListLabels.fold_left ~init:(0,init) ~f:(fun (i,a) x -> (i+1,f i a x)) lst)

let () = 
  let pprof f = Printf.printf "%f\n" (profile f 100) in
  pprof (fun () -> foldi lst ~f:(fun i a x -> i + a + x) ~init:0); 
  pprof (fun () -> snd (ListLabels.fold_left ~init:(0,0) ~f:(fun (i,a) x -> (i + 1, i + a + x)) lst));

(*

let as_array arr ~f ~init =
  let r = ref init in
  for i = 0 to Array.length arr - 1 do
    r := f i !r arr.(i)
  done;
  !r

  let arr = Array.of_list lst in
  pprof (fun () -> as_array arr ~f:(fun i a x -> i + a + x) ~init:0)


  let opp3 a b c = a + b * c in
  pprof (fun () -> foldi lst ~f:(fun i a x -> opp3 (i + 1) (a + 1) (x + 1)) ~init:0); 
  pprof (fun () -> snd (ListLabels.fold_left ~init:(0,0) ~f:(fun (i,a) x -> (i + 1, opp3 (i + 1) (a + 1) (x + 1))) lst));
  *)
