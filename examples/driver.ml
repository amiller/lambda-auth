open Merkle
open Benchmark

(* Binary Tree *)
let t = ref Bintree.Tip;;

let setup file =
  if is_verifier then setup_verifier file else 
  if is_prover then setup_prover file else
  if is_ideal then ignore() else
  failwith Sys.executable_name;;

setup "proof_bintree.dat";
for i = 0 to 10 do
  t := Bintree.ins i !t
done;

(*
for i = -1 to 11 do
  if Bintree.member i !t then
    Printf.printf "member %d: true\n" i
  else
    Printf.printf "member %d: false\n" i
done;
*)

insist();
flush_cache();


(* Skiplist *)
setup "proof_skiplist.dat";
Random.init(0xbadbeef);;

let t = ref Skiplist.empty;;

for i = 0 to 10 do
  t := Skiplist.insert !t i
done;

(*
for i = -1 to 11 do
  if Skiplist.member !t i then
    Printf.printf "member %d: true\n" i
  else
    Printf.printf "member %d: false\n" i
done;
*)

insist();
flush_cache();;


(* Red Black tree *)
setup "proof_redblack.dat";;

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
  Random.init (0xBEEF + k);
  for i = 0 to two_to k do
    let a = rand_odd() in
    tree := Redblack.insert a (string_of_int a) !tree;
  done;
  !tree;;

let write_tree_prover k =
  setup_prover "/dev/null";
  Printf.printf "Building tree 2^%d... " k; flush_cache();
  let tree = prepare_tree k in
  Printf.printf "OK\n";
  let file = open_out_bin (Printf.sprintf "data/bst_ann_%03d.dat" k) in
  Marshal.to_channel file (tree) [];
  Printf.printf "OK\n"; flush_cache();;

let read_tree_prover k : Redblack.tree =
  Marshal.from_channel (open_in_bin (Printf.sprintf "data/bst_ann_%03d.dat" k));;

let read_tree_verifier k : Redblack.tree = 
  Marshal.from_channel (open_in_bin (Printf.sprintf "data/bst_shal_%03d.dat" k));;

let write_tree_verifier k =
  assert false (* must comment out for Ideal.. Hack!! *)
  (*
  let t = shallow_func (read_tree_prover k) in
  let file = open_out_bin (Printf.sprintf "data/bst_shal_%03d.dat" k) in
  Marshal.to_channel file t [];
  close_out file *)


let write_tree_ideal k =
  setup_prover "/dev/null";
  Printf.printf "Building tree 2^%d... " k; flush_cache();
  let tree = prepare_tree k in
  Printf.printf "OK\n";
  let file = open_out_bin (Printf.sprintf "data/bst_%03d.dat" k) in
  Marshal.to_channel file tree [];
  Printf.printf "OK\n"; flush_cache();;

let read_tree_ideal k : Redblack.tree =
  Marshal.from_channel (open_in_bin (Printf.sprintf "data/bst_%03d.dat" k));;


let t = ref Redblack.empty;;

for i = 0 to 10 do
  t := Redblack.insert i (string_of_int i) !t
done;
flush_cache();;

(*
for i = -1 to 11 do
  match Redblack.lookup i !t with
  | None -> Printf.eprintf "member %d: false\n" i
  | Some v -> Printf.eprintf "member %d (%s): true\n" i v
done;
*)
for i = 0 to 10 do
  t := Redblack.delete i !t
done;
flush_cache();;


for i = -1 to 11 do
  match Redblack.lookup i !t with
  | None -> Printf.eprintf "member %d: false\n" i
  | Some v -> Printf.eprintf "member %d (%s): true\n" i v
done;


insist();
flush_cache();;

let bench_ins iter k =
  let tree = if is_prover then read_tree_prover k
  else if is_ideal then read_tree_ideal k 
  else read_tree_verifier k
  in
  Gc.compact();
  let res = throughput1 2
      ~repeat:5
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
  else if is_ideal then read_tree_ideal k
  else read_tree_verifier k
  in
  Gc.compact();
  let res = throughput1 2
      ~repeat:5
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
  else if is_ideal then 
    for k = min_k to max_k do write_tree_ideal k done
  else if is_verifier then
    for k = min_k to max_k do write_tree_verifier k done
  else ignore();;


(* Merkle tree test *)

let random_leaves k = 
  Random.init (0x1234 + k);
  let random_string size = 
    let s = String.create size in
    for i = 0 to size-1 do s.[i] <- BatRandom.char() done;
    s in
  let leaves = ref [] in
  for i = 1 to two_to k do 
    leaves := random_string Mtree.size_leaf :: !leaves 
  done; 
  !leaves

let write_leaves k =
  let file = open_out_bin (Printf.sprintf "data/leaves_%03d.dat" k) in
  let leaves = random_leaves k in
  List.iter (output_string file) leaves;
  close_out file
    

let write_mtree k =
  setup_prover "/dev/null";
  Printf.printf "Building mtree 2^%d... " k; flush_cache();
  let leaves = random_leaves k in
  let file = open_out_bin (Printf.sprintf "data/mtree_%03d.dat" k) in
  let tree = Mtree.from_list leaves in
  Marshal.to_channel file tree [];
  Printf.printf "OK\n"; flush_cache();
  let file = open_out_bin (Printf.sprintf "data/mtree1_%03d.dat" k) in
  let tree = Mtree.from_list1 leaves in
  Marshal.to_channel file tree [];
  Printf.printf "OK\n"; flush_cache();;

let read_mtree_prover k : Mtree.tree =
  Marshal.from_channel (open_in_bin (Printf.sprintf "data/mtree_%03d.dat" k));;

let read_mtree1_prover k : Mtree.tree1 =
  Marshal.from_channel (open_in_bin (Printf.sprintf "data/mtree1_%03d.dat" k));;

let bench_mtree iter k =
  let n = two_to k in
  let tree = if is_prover then read_mtree_prover k
  else assert false (* shallow_func (read_mtree_prover k) *) (* This must be commented out for Ideal. Hack! *)
  in
  Gc.compact();
  let res = throughput1 2
      ~repeat:5
      ~fdigits:5
      ~name:(Printf.sprintf "(%s) validate (x%d) rand into 2^%d" Merkle.mode_name iter k)
      (fun () ->
        flush_cache();
        Random.init (0x7070 + k);
        setup (Printf.sprintf "data/proof_mtree_look_%03d.dat" k);
        for i = 1 to iter do 
          let a = Mtree.from_int n (Random.int n) in
          Mtree.fetch a tree;
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


let bench_mtree1 iter k =
  let n = two_to k in
  let tree = read_mtree1_prover k in
  let root : string = fst tree in
  Gc.compact();
  let res = throughput1 2
      ~repeat:5
      ~fdigits:5
      ~name:(Printf.sprintf "(%s) validate (x%d) rand into 2^%d" Merkle.mode_name iter k)
      (fun () ->
        let filename = Printf.sprintf "data/proof_mtree1_look_%03d.dat" k in
        let outfile = open_out_bin (if is_prover then filename else "/dev/null") in
        let infile = open_in_bin (if is_verifier then filename else "/dev/null") in
        Random.init (0x7071 + k);
        for i = 1 to iter do
          let idx = Random.int n in
          let a = Mtree.from_int n idx in
          (if is_prover then
            let leaf, path = Mtree.make_path a (snd tree) in
            Mtree.write_path outfile k leaf path
          else begin
            let leaf, path = Mtree.read_path infile k in
            Mtree.validate a leaf root path end);
          insist();
        done;
        flush_cache();
        close_in infile;
        close_out outfile;
        )
      ()
  in
  Printf.printf "Allocated bytes: %d\n" (Gc.stat()).live_words;
  flush_cache();
  tabulate res;
  ;;


(* prepare_all() *)

 let () = for i = min_k to max_k do bench_ins 100000 i done;; 
(* let () = for i = min_k to max_k do bench_look 100000 i done;; *)
(*  bench_ins 1 4;; *)
(* bench_ins 10000 (int_of_string (Sys.argv.(1)));; *)

(* Merkle test *)
(* let () = for i = 4 to 18 do write_mtree i done;; *)
(* for i = 4 to 18 do bench_mtree 100000 i done;; *)
(*  for i = 4 to 18 do bench_mtree1 100000 i done;; *)
(* for i = 4 to 18 do write_leaves i done;; *)
