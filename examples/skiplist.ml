(* Randomized skip list, keys only (no values), insert but no delete *)
open Merkle

type a = int
type a_ord = Sentinel | Key of a
type a_skiplist = 
    Nil
  | Node of ((*down*)a_skiplist * a_ord * (*right*)a_skiplist) authtype

external merkle : 'a -> 'a = "MERKLE"

let empty = Node(auth(Nil, Sentinel, Nil))

let rec member t x =
  match t with
      Nil -> false
    | Node a -> let (down,y,right) = unauth a in
        if Key x = y then true else
        if Key x < y then false else
        if member right x then true else
        member down x

let flip() = Random.bool()

let insert t x =
  let rec ins t : a_skiplist * a_skiplist option =
    match t with
      Nil -> failwith "ins in empty tree"
    | Node a -> let (down, y, right) = unauth a in
        (* invariant: x >= y, because of Sentinel *)
        let godown() = match down with
        | Nil -> let leaf = Node (auth(Nil, Key x, right)) in 
          Node (auth(down, y, leaf)), Some leaf
        | Node _ -> match ins down with
          | down1, None -> Node (auth(down1, y, right)), None
          | down1, Some leaf -> if flip()
          then Node (auth(down1, y, right)), None
          else let leaf1 = Node (auth(leaf, Key x, right)) in
          Node (auth(down1, y, leaf1)), Some leaf
        in
        match right with
        | Nil -> godown()
        | Node a -> let (_,z,_) = unauth a in
            if Key x = z then failwith "duplicate insert"
            else if Key x < z then godown()
            else let right1, fin = ins right in
            Node (auth(down, y, right1)), fin
  in
  let rec grow t leaf = if flip() then t
  else let leaf1 = Node (auth(leaf, Key x, Nil)) in 
  grow (Node(auth(t, Sentinel, leaf1))) leaf1
  in
  match ins t with
  | t1, None -> t1
  | t1, Some leaf -> grow t1 leaf
