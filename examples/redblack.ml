open Merkle

type k = int
type v = string
type color = R | B
type voption = Non | Som of v
type _tree = Tip | Bin of color * _tree authtype * (k * voption) * _tree authtype
type tree = _tree authtype

external merkle : 'a -> 'a = "MERKLE"

let empty = auth Tip

let lookup x = 
  let rec look t = match unauth t with
    | Tip -> None
    | Bin (_, _, (y, Som v), _) -> if y = x then Some v else None
    | Bin (_, l, (y, Non  ), r) -> if x <= y then look l else look r
  in look

let blacken = function 
  | Bin(_, l, x, r) -> Bin(B, l, x, r) 
  | Tip -> Tip

let redden = function
  | Bin(_, l, x, r) -> Bin(R, l, x, r)
  | Tip -> assert false

let balanceL (t : tree) : tree = match unauth t with
| Bin (B, l, a, r) -> begin match unauth l with
  | Bin (R, l1, a1, r1) -> begin match unauth l1 with
    | Bin (R, l2, a2, r2) -> auth(Bin(R,auth(Bin(B,l2,a2,r2)),a1,auth(Bin(B,r1,a,r))))
    | _ -> begin match unauth r1 with
      | Bin (R, l2, a2, r2) -> auth(Bin(R,auth(Bin(B,l1,a1,l2)),a2,auth(Bin(B,r2,a,r))))
      | _ -> t
    end
  end
  | _ -> t
end
| _ -> t

let balanceR (t : tree) : tree = match unauth t with
| Bin (B, l, a, r) -> begin match unauth r with
  | Bin (R, l1, a1, r1) -> begin match unauth l1 with
    | Bin (R, l2, a2, r2) -> auth(Bin(R,auth(Bin(B,l,a,l2)),a2,auth(Bin(B,r2,a1,r1))))
    | _ -> begin match unauth r1 with
      | Bin (R, l2, a2, r2) -> auth(Bin(R,auth(Bin(B,l,a,l1)),a1,auth(Bin(B,l2,a2,r2))))
      | _ -> t
    end
  end
  | _ -> t
end
| _ -> t

let insert x v t = 
  let leaf = auth(Bin(B,auth Tip,(x,Som v),auth Tip)) in
  let rec ins t = match unauth t with
  | Tip -> leaf
  | Bin(c,l,(y,Som _),r) ->
    if x = y then t else
    (* if x = y then failwith "duplicate insert" else *)
    if x < y then auth(Bin(R,leaf,(x,Non),t)) else
    if x > y then auth(Bin(R,t,(y,Non),leaf)) else assert false
  | Bin(c,l,(y,Non as yv),r) ->
    if x = y then t else
    if x < y then balanceL(auth(Bin(c,ins l,yv,r))) else
    if x > y then balanceR(auth(Bin(c,l,yv,ins r))) else assert false
  in auth(blacken(unauth(ins t)))

let unbalancedL (t : _tree) : tree * bool = match t with
| Bin(c, l, a, r) -> begin match unauth l with
  | Bin(B, l1, a1, r1) ->
      balanceL (auth(Bin(B,auth(Bin(R,l1,a1,r1)),a,r))), c==B
  | Bin(R, l1, a1, r1) ->
      auth(Bin(B,l1,a1,balanceL(auth(Bin(B,auth(redden(unauth r1)),a,r))))), false
  | _ -> assert false
end
| _ -> assert false

let unbalancedR (t : _tree) : tree * bool = match t with
| Bin(c, l, a, r) -> begin match unauth r with
  | Bin(B, l1, a1, r1) -> 
      balanceR (auth(Bin(B,l,a,auth(Bin(R,l1,a1,r1))))), c==B
  | Bin(R, l1, a1, r1) -> 
      auth(Bin(B,balanceR(auth(Bin(B,l,a,auth(redden(unauth l1))))),a1,r1)), false
  | _ -> assert false
end
| _ -> assert false

let delete x t =
  let rec del t = match unauth t with
  | Tip -> failwith "delete called on empty tree"
  | Bin(_,_,(y,Som _),_) -> 
      if x = y then (auth Tip, true), None
      else failwith "delete: element found"
  | Bin(c,l,(y,Non),r) ->
      if x <= y then
        let (l,d),m = del l in
        if unauth l = Tip then (r, c=B), None
        else 
          let k = if x = y then match m with Some m -> m,Non
          | _ -> assert false else y,Non in
          let t = Bin(c,l,k,r) in
          if d then unbalancedR t, None
          else (auth t, false), None
      else
        let (r,d),m = del r in
        if unauth r = Tip then (l, c=B), Some y
        else
          let k = (y,Non) in
          let t = Bin(c,l,k,r) in
          if d then unbalancedL t, m
          else (auth t, false), m
  in let (t,_),_ = del t in auth(blacken (unauth t))
