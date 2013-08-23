open Merkle

(* Data structure model of bitcoin block validation
   Parameters:
     N - the number of blocks (i.e., transactions)
     M - a bound for the number of coins in the coinset at any time
 *)

type coin = int

type transaction =
    coin list (* coins to remove *)
  * coin list (* coins to insert *)

module IntSet = Set.Make (struct type t = int let compare = compare end)
type coinset = IntSet.t
type _blockchain = Genesis | Block of _blockchain authtype * transaction authtype
type blockchain = _blockchain authtype

type stack = E | S of (transaction authtype * stack) authtype

type coinset_auth = Redblack.tree (* a set of coins *)

external merkle : 'a -> 'a = "MERKLE"

(* Apply a transaction to a coinset *)
let apply tx coins =
  let after_remove = List.fold_right (IntSet.remove) (fst tx) coins in
  let after_insert = List.fold_right (IntSet.add) (snd tx) after_remove in
  after_insert

(* 1. Scan from the head of the chain to the base, then validate forwards *)
(* Verifier: O(N + M) storage, O(N log M) runtime
   Prover: O(N + M) storage, O(N log M) runtime
   VO size: O(N)
 *)
let rec validate (newblk : blockchain) : coinset =
  match unauth newblk with 
  | Genesis -> IntSet.empty
  | Block(prevblk, tx) ->
      let prevcoins = validate prevblk in
      apply (unauth tx) prevcoins

(* 2. Tail recursive version with less storage, using authenticated stack *)
(* Verifier: O(M) storage, O(N log M) runtime
   Prover: O(N + M) storage, O(N log M) runtime
   VO size: O(N)
 *)
let validate_tr newblk =
  (* Start by following the headers down to the fork point *)
  let rec _build s blk =
    match unauth newblk with 
    | Genesis -> s
    | Block(prevblk, tx) -> _build (S(auth(tx,s))) prevblk 
  in
  let stk = _build E newblk in
  
  (* Then validate from the earliest fork point back up to the recent head *)
  let rec _validate s coins =
    match s with
    | E -> coins
    | S txs -> let tx, s = unauth txs in
      _validate s (apply (unauth tx) coins)
  in 
  _validate stk IntSet.empty


(* 3. Optimized version with authenticated set *)
(* Verifier: O(log M) storage, O(N log M) runtime
   Prover: O(N + M) storage, O(N log M) runtime
   VO size: O(N log M)
 *)
let add i t = Redblack.insert i "" t
let apply_auth tx coins : coinset_auth =
  (* Remove elements as necessary *)
  let after_remove = List.fold_right (Redblack.delete) (fst tx) coins in
  (* Insert elements as necessary *)
  let after_insert = List.fold_right (add) (snd tx) after_remove in
  after_insert

let rec validate_auth newblk = 
  (* Start by following the headers down to the fork point *)
  let rec _build s blk =
    match unauth newblk with
    | Genesis -> s
    | Block(prevblk, tx) -> _build (S(auth(tx,s))) prevblk 
  in
  let stk = _build E newblk in
  
  (* Then validate from the earliest fork point back up to the recent head *)
  let rec _validate s coins =
    match s with
    | E -> coins
    | S txs -> let tx, s = unauth txs in
      _validate s (apply_auth (unauth tx) coins)
  in 
  _validate stk Redblack.empty

