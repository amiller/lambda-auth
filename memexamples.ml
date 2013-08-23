open Merkle

type _list = Nil | Cons of int * _list authtype
type list = _list authtype

(* Prover: O(N) memory
   Verifier: tail recursive, uses O(1) memory *)
let sum0 l = 
  let rec _sum l acc = match unauth l with
  | Cons(x, xs) -> _sum xs (x + acc)
  | Nil -> acc
  in _sum 0
  
(* Prover: O(N) memory
   Verifier: not tail recursive, uses O(N) stack *)
let rec sum1 l = match unauth l with
| Cons(x, xs) -> x + sum1 xs
| Nil -> 0

(* Prover: O(N) memory
   Verifier: tail recursive, but uses O(N) heap *)
type stack = E | S of int * stack
let sum2 l = 
  let rec _build l s = match l with
  | Cons(x, xs) -> _build xs S(x, s)
  | Nil -> s in
  let rec _sum l acc = match l with
  | S(x, xs) -> x + _sum xs (x + acc)
  | NilX -> 0
  in _sum (_build l E)
