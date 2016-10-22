open Merkle
open Benchmark

let setup file =
  if is_verifier then setup_verifier file else 
  if is_prover then setup_prover file else
  if is_ideal then ignore() else
  failwith Sys.executable_name;;

(* Red Black tree *)
let min_k = 4;;
let max_k = 21;;

let rec two_to = function 0 -> 1 | n -> 2 * two_to (n - 1);;
let range = let rec _range acc lo hi = 
    if lo < hi then _range (hi-1 :: acc) lo (hi-1) else acc
in _range []

let rand_odd () = Random.int(50000000) * 2;;
let rand_even () = Random.int (50000000) * 2 + 1;;

let prepare_tree k =
  let tree = ref Redblack.empty in
  flush_cache();
  Random.init (0xBEEF + k);
  for i = 0 to two_to k do
    let a = rand_odd() in
    tree := Redblack.insert a (string_of_int a) !tree;
    insist();
  done;
  !tree;;


let read_tree_prover k : Redblack.tree =
  Marshal.from_channel (open_in_bin (Printf.sprintf "data/bst_ann_%03d.dat" k));;

let read_tree_verifier k : Redblack.tree = 
  Marshal.from_channel (open_in_bin (Printf.sprintf "data/bst_shal_%03d.dat" k));;


let write_tree_prover k =
  setup_prover "/dev/null";
  Printf.printf "Building tree 2^%d... " k; flush_cache();
  let tree = prepare_tree k in
  Printf.printf "OK\n";
  let file = open_out_bin (Printf.sprintf "data/bst_ann_%03d.dat" k) in
  Marshal.to_channel file (tree) [];
  Printf.printf "OK\n"; flush_cache();
  close_out file;;

let write_tree_verifier k =
  let t = (match (read_tree_prover k) with
     | MerkleSusp(_,d,_) -> Shallow d
     | Merkle(d,_)      -> Shallow d) in
  let file = open_out_bin (Printf.sprintf "data/bst_shal_%03d.dat" k) in
  Marshal.to_channel file t [];
  close_out file;;


let bench_ins iter k =
  let tree = if is_prover then read_tree_prover k
  else read_tree_verifier k
  in
  Gc.compact();
  let res = throughput1 2
      ~repeat:1
      ~fdigits:5
      ~name:(Printf.sprintf "(%s) insert (x%d) rand into 2^%d" Merkle.mode_name iter k)
      (fun () ->
        flush_cache();
        Random.init (0x7070 + k);
        setup (Printf.sprintf "data/proof_rbp_ins_%03d.dat" k);
        for i = 1 to iter do 
          let a = rand_even() in
          Redblack.insert a (string_of_int a) tree;
          insist();
        done;
        flush_cache()
        )
      ()
  in
  Printf.printf "Allocated bytes: %d\n" (Gc.stat()).live_words;
  flush_cache();
  tabulate res;
  ;;

let bench_look iter k =
  let tree = if is_prover then read_tree_prover k
  else read_tree_verifier k
  in
  Gc.compact();
  let res = throughput1 2
      ~repeat:1
      ~fdigits:5
      ~name:(Printf.sprintf "(%s) lookup (x%d) rand into 2^%d" Merkle.mode_name iter k)
      (fun () ->
        flush_cache();
        Random.init (0x7070 + k);
        setup (Printf.sprintf "data/proof_rbp_look_%03d.dat" k);
        for i = 1 to iter do 
          let a = rand_even() in
          Redblack.lookup a tree;
          insist();
        done;
        flush_cache();
        )
      ()
  in
  Printf.printf "Allocated bytes: %d\n" (Gc.stat()).live_words;
  flush_cache();
  tabulate res;
  ;;

let prepare_all() =
  if is_prover then
    for k = min_k to max_k do write_tree_prover k done
  else if is_verifier then
    for k = min_k to max_k do write_tree_verifier k done
  else ignore();;

prepare_all()

let () = for i = min_k to max_k do bench_ins 100000 i done;; 
let () = for i = min_k to max_k do bench_look 100000 i done;;
