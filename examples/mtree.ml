open Merkle

let size_leaf = 1024 (* one kilobyte strings *)

type bit = Lo | Hi
type _tree = Tip of string | Bin of _tree authtype * _tree authtype
type tree = _tree authtype

type stack = E | SL of stack authtype * tree | SR of stack authtype * tree

external merkle : 'a -> 'a = "MERKLE"

let split l =
  let rec split_aux l left right = 
    match l,left,right with
    | ([] | [_]),_,_ -> (List.rev left),right
    | (_::_::t),_,h::right_t -> split_aux t (h::left) right_t
    | _ -> assert false
  in
  split_aux l [] l

let rec from_list x =
  match x with
  | [] -> failwith "empty"
  | [a] -> auth (Tip a)
  | x -> let left, right = split x in 
    auth (Bin(from_list left, from_list right))

let rec fetch idx t =
  match idx, unauth t with
  | [], Tip a -> a
  | Lo::idx, Bin(l,_) -> fetch idx l
  | Hi::idx, Bin(_,r) -> fetch idx r
  | _ -> failwith "bad index"

let rec update idx t newval : tree =
  match idx, unauth t with
  | [], Tip _ -> auth(Tip newval)
  | Lo::idx, Bin(l,r) -> auth(Bin(update idx l newval, r))
  | Hi::idx, Bin(l,r) -> auth(Bin(l, update idx r newval))

let update_stk idx t newval : tree = 
  let rec _build idx t s : (tree * stack) =
    match idx, unauth t with
    | [], Tip _ -> auth(Tip newval), s
    | Lo::idx, Bin(l,r) -> _build idx l (SL(auth s, r))
    | Hi::idx, Bin(l,r) -> _build idx r (SR(auth s, l)) in
  let rec _apply (child, s) =
    match s with 
    | E -> child
    | SL(s, r) -> _apply (auth(Bin(child,r)), unauth s)
    | SR(s, l) -> _apply (auth(Bin(l,child)), unauth s) in
  _apply(_build idx t E)
  

(*
let update_cps idx t newval : tree =
  let rec _update (f : (tree -> 'a) authtype) idx t newval : 'a =
    match idx, unauth t with
    | [], Tip _ -> (unauth f) (auth(Tip newval))
    | Lo::idx, Bin(l,r) -> _update (auth(fun t -> auth(Bin(t,r)))) idx l newval
    | Hi::idx, Bin(l,r) -> _update (auth(fun t -> auth(Bin(l,t)))) idx r newval
  in _update (auth(fun t -> t)) idx t newval
*)

let rec from_int n x = 
  (* Bit string representing a number, from LSB to MSB *)
  let of_bit = function | 0 -> Lo | 1 -> Hi | _ -> assert false in
  match n with
  | 1 -> []
  | _ -> of_bit (x mod 2) :: from_int (n/2) (x/2)

let bstr = function | Lo -> "0" | Hi -> "1"

(* Hand rolled merkle path validation *)
type _tree1 = Tip1 of string | Bin1 of (string * _tree1) * (string * _tree1)
type _tree2 = Tip2 of string | Bin2 of string * string
type tree1 = string * _tree1

let hash1 = function
  | Tip1 s -> Merkle.sha1hash s
  | Bin1((l,_),(r,_)) -> Merkle.sha1hash (l ^ r)

let rec from_list1 x =
  match x with
  | [] -> failwith "empty"
  | [a] -> hash1 (Tip1 a), Tip1 a
  | x -> let left, right = split x in
    let t = Bin1(from_list1 left, from_list1 right) in
    hash1 t, t

let make_path bits t = 
  let rec _make_path bits t = match bits,t with
  | [], Tip1 a -> a, []
  | Lo::bits, Bin1((d,l),(s,_)) ->
      let leaf, rest = _make_path bits l in leaf, s :: rest
  | Hi::bits, Bin1((s,_),(d,r)) -> 
      let leaf, rest = _make_path bits r in leaf, s :: rest
  in let leaf, path = _make_path (List.rev bits) t in leaf, List.rev path

let write_path file k leaf path = 
  assert (String.length leaf = size_leaf);
  output_string file leaf;
  List.iter (fun sibling -> 
    assert (String.length sibling = 20);
    output_string file sibling) path

let read_path file k =
  let leaf = String.create size_leaf in
  really_input file leaf 0 size_leaf;
  let path = ref [] in
  for i = 1 to k do
    let sibling = String.create 20 in
    really_input file sibling 0 20;
    path := sibling :: !path
  done;
  leaf, List.rev !path

let validate a leaf root path =
  let rec _validate a child path = 
    match path,a with
  | [],[] -> child
  | sibling::path, Lo::a -> 
      _validate a (Merkle.sha1hash (child ^ sibling)) path;
  | sibling::path, Hi::a -> 
      _validate a (Merkle.sha1hash (sibling ^ child)) path
  | _ -> assert false
  in assert (_validate a (Merkle.sha1hash leaf) path = root)
