(* search tree *)

type distance = float

type separator =
    Tip
  | Bin of separator * int * distance list * separator

type sep_tree = 
    STip
  | SBin of sep_tree * separator * sep_tree

let rec member key tree = match tree with
| Tip -> false
| Bin (l, k, _, r) -> 
    if (key = k) then true
    else if (key < k) then member key l
    else member key r

let rec lookup key tree = match tree with
| Bin (l, k, a, r) -> 
    if (key = k) then a
    else if (key < k) then lookup key l
    else lookup key r

(* separator tree *)
(* a separator of k nodes (for a graph of n nodes) is represented as a distance
   map where
   - each (of n) keys is a node-id (int)
   - each value is a list of k distances, each distance corresponds to the
       shortest path between the key node and the corresponding node 
       in the separator *)

(* A node w is contained in the separator tree if the tree is non-null and the 
   distance map contains w *)
let rec smember w tree = match tree with
| STip -> false
| SBin (_, sep , _) -> member w sep

(* The shortest distance between u and v passing through the separator *)
let shortest_across u v sep = 
  let dists = List.map2 (fun x y -> x+.y) (lookup u sep) (lookup v sep) in
  List.fold_right min dists infinity

(* The distance of the shortest path between u and v, computed as the minimum 
   of the shortest distance *crossing* the separator or the shortest distance 
   strictly on one side of the separator, if applicable (i.e., if u and v lie 
   on the same side of the partition) *)
let rec shortest u v tree = match tree with
| SBin (l, sep, r) ->
    let withinL = if smember u l && smember v l then shortest u v l else infinity in
    let withinR = if smember u r && smember v r then shortest u v r else infinity in
    let across = shortest_across u v sep in
    min across (min withinL withinR)
