module auth type 'a authtype
external unauth: 'a authtype -> 'a = "UNAUTH"
external auth: 'a -> 'a authtype = "AUTH"

type tree = Tip | Bin of (tree * int * tree) authtype
let rec member t x : bool = match t with
  Tip -> false
| Bin a -> let (l,y,r) = unauth a in
  if x = y then true else if x < y 
  then member l x
  else member r x
