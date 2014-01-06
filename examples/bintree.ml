open Merkle (* authtype, unauth, auth *)

type tree = Tip | Bin of (tree * int * tree) authtype

(* unauth_xxx, shallow_xxx gets inserted here, after
   user type definitions *)
external merkle : 'a -> 'a = "MERKLE"

let rec member x = function
  | Tip -> false
  | Bin a -> let (l,y,r) = (fun x -> unauth x) a in
    if x = y then true else if x < y
    then member x l
    else member x r

let rec ins x = function
  | Tip -> Bin (auth (Tip, x, Tip))
  | Bin a -> let (l,y,r) = unauth a in
    if x = y then Bin a else if x < y
    then Bin (auth (ins x l, y, r))
    else Bin (auth (l, y, ins x r))
