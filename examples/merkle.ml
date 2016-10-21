type suspension = Tag of < > * int | Hash of string
type 'a authtype = 
  | Merkle of string * 'a
  | Shallow of string 
  | Ideal of 'a 

  (* Additional cases for suspension-buffer *)
  | Suspended
  | Suspension of suspension ref
  | MerkleSusp of bool ref * string * 'a


let from_channel_with_string chan =
  let s = String.create Marshal.header_size in
  really_input chan s 0 Marshal.header_size;
  let d = Marshal.data_size s 0 in
  let str = String.create (Marshal.header_size + d) in
  String.blit s 0 str 0 Marshal.header_size;
  really_input chan str Marshal.header_size d;
  Marshal.from_string str 0, str

let no_share = [Marshal.No_sharing]

let unauth : 'a authtype -> 'a = function x -> assert false
let auth : 'a -> 'a authtype = function x -> assert false


let equal : ('a authtype -> 'a authtype -> bool) = 
  fun x y ->
  match x,y with
  | Merkle(x,_),Merkle(y,_)
  | Shallow x, Shallow y -> x = y
  | Ideal x, Ideal y -> compare x y = 0 (* inefficient unless physically equal *)
  | _ -> failwith "equality doesn't work so well for other types"

(*let forget : 'a authtype -> 'a authtype = function 
  | Merkle(_,x) as t -> 
      x := None;
      t
  | t -> t*)

type any_authmap = { authmap : 'a . 'a authtype -> 'a authtype }

let map_0 __func x = __func.authmap x

let shallow_func = function
  | Merkle(d,_) -> Shallow d
  | _ -> failwith "shallow_func called on not merkle"

let unmerkle_func = function
  | Merkle(_,a) -> a
  | _ -> failwith "unmerkle_func called on not merkle"

let prf_output = ref (open_out_bin "/dev/null")
let prf_input = ref (open_in_bin "/dev/null")

let setup_prover path = close_out !prf_output; prf_output := open_out_bin path
let setup_verifier path = close_in !prf_input; prf_input := open_in_bin path

(* let to_hex = Cryptokit.transform_string (Cryptokit.Hexa.encode()) *)
(* let from_hex = Cryptokit.transform_string (Cryptokit.Hexa.decode()) *)

(* let sha1hash s = Cryptokit.hash_string (Cryptokit.Hash.sha1()) s  *)
let sha1hash s = Sha1.to_bin (Sha1.string s) 

let hash x = sha1hash (Marshal.to_string x no_share)

let discard _ x = x

let rec remove_all tbl key = if Hashtbl.mem tbl key
then (Hashtbl.remove tbl key; remove_all tbl key) 
else ignore()


(*************************
  Ordinary prover/verifier
 *************************)


(* prover version of auth and unauth *)
let _unauth_prover map x =
  let y = unmerkle_func x in
(*  Printf.eprintf "unauth prover %s\n" (to_hex (hash (map shallow_func y))); *)
  Marshal.to_channel !prf_output (map {authmap=shallow_func} y) no_share;
  y

let _auth_prover map x = Merkle(hash (map {authmap=shallow_func} x), x)

(* verifier version of auth and unauth *)
let _unauth_verifier _ : 'a authtype -> 'a = function | Shallow d ->
  let x,str = from_channel_with_string !prf_input in
  if d = sha1hash str then x
  else failwith "Invalid data in proof stream"
| _ -> failwith "_unauth_verifier called without (Shallow d)"

let _auth_verifier _ : 'a -> 'a authtype = function x -> Shallow (hash x)




(*************************
  Reuse buffer optimization
 *************************)

module IMap = Map.Make(struct type t = int let compare : int -> int -> int = compare end)

let max_vrf_buf = 1000
let counter = ref 0
let lru_buf = ref IMap.empty
let hash_buf = Hashtbl.create max_vrf_buf

let insert_and_resize d str =
  (* Insert this in the table *)
  lru_buf := IMap.add !counter d !lru_buf;
  Hashtbl.add hash_buf d (!counter,str);
   
  (* Resize the table if necessary *)
  if Hashtbl.length hash_buf > max_vrf_buf then begin
    let cnt,d = IMap.min_binding !lru_buf in
    lru_buf := IMap.remove cnt !lru_buf;
    Hashtbl.remove hash_buf d
  end
      
let _unauth_prover_buf (map: any_authmap -> 'a -> 'a) (x:'a authtype) : 'a =
  let y,d = match x with Merkle(d,y) -> y,d | _ -> failwith "unauth called wrong" in
  let shal_y = map {authmap=shallow_func} y in
  (if Hashtbl.mem hash_buf d then begin 
    (* This is already in the buf, but we'll freshen it *)
    let idx,_ = Hashtbl.find hash_buf d in
    Hashtbl.remove hash_buf d;
    lru_buf := IMap.remove idx !lru_buf
  end else begin
    (* Write it out! *)
    Marshal.to_channel !prf_output shal_y no_share
  end);

  (* Insert into the table *)
  insert_and_resize d (Marshal.to_string shal_y no_share);
  counter := !counter + 1;
  y

let _auth_prover_buf (map: any_authmap->'a->'a) (x:'a) : 'a authtype =
  let y = map {authmap=shallow_func} x in
  let d = hash y in
  insert_and_resize d (Marshal.to_string y no_share);
  counter := !counter + 1;
  Merkle (hash y, x)

let _unauth_verifier_buf (map: any_authmap->'a->'a) : 'a authtype -> 'a = function 
  | Shallow d ->
      let x:'a =
        if Hashtbl.mem hash_buf d then begin
          (* Cache hit *)
          let idx,str = Hashtbl.find hash_buf d in
          Hashtbl.remove hash_buf d;
          lru_buf := IMap.remove idx !lru_buf;
          insert_and_resize d str;
          Marshal.from_string str 0
        end else
          let x = Marshal.from_channel !prf_input in
(*  Printf.eprintf "unauth verifier %s %s\n" (to_hex d) (to_hex (hash x)); *)
          if d = hash x then begin
            insert_and_resize d (Marshal.to_string x no_share);
            x
          end else failwith "Invalid data in proof stream"
      in
      counter := !counter + 1;
      x
  | _ -> failwith "unauth_verifier_buf"
  
let _auth_verifier_buf (_: (any_authmap)->'a->'a) (x:'a) : 'a authtype =
  let d = hash x in
  let str = Marshal.to_string x no_share in
  insert_and_resize d str;
  counter := !counter + 1;
  Shallow (hash x)



(*************************
  Suspended-disbelief buffer optimization
 *************************)

let susp_buf = ref []

let _insist_prover () = 
  List.iter (fun f -> f()) !susp_buf;
  susp_buf := []

(* Suspended-disbelief buffer version of Prover *)
let _unauth_prover_susp map w =
  let x = match w with
  | MerkleSusp(r,_,a) -> 
      (* We've now visited this node. Set the register! *)
      r := true;
      a
  | Merkle(_,a) -> a
  | _ -> failwith "unauth_prover mismatch" in

  (* Replace the next layer with fresh references, set to false *)
  let y = map {authmap=(function
    | Merkle(d,a) -> MerkleSusp({contents=false},d,a)
    | _ -> failwith "broken!")} x in

  (* Enqueue the thunk for writing this node *)
  let finish () =
    let z = map {authmap=(function
      | MerkleSusp({contents=false},d,_) -> Shallow d
      | MerkleSusp({contents=true},_,_) -> Suspended
      | _ -> failwith "finish_merksusp called wrong")} y in
    Marshal.to_channel !prf_output z no_share
  in
  susp_buf := !susp_buf @ [finish];

  (* Finally return the node *)
  y

let _auth_prover_susp map (x : 'a) : 'a authtype = 
  let y = map {authmap=(function
    | MerkleSusp(_,d,a) -> Merkle(d,a)
    | _ -> failwith "merkle authsusp called wrong")} x in
  MerkleSusp({contents=true},hash (map {authmap=shallow_func} y), y)


(* Suspended disbelief Verifier *)

(* Traverse a data stucture (up to the next layer of authtype) and
   replace each Suspended (no data) with a Suspension (mutable ref
   to a digest). Also returns an array containing the Suspension refs
   so they can each be updated in constant time. *)
let build_susp tag map x =
  let refs = ref [] in
  let count = ref 0 in
  let y = map {authmap=(function
    | Suspended -> 
      let s = ref (Tag (tag, !count)) in
      refs := s :: !refs;
      count := 1 + !count;
      Suspension s
    | Shallow d -> Shallow d
    | _ -> failwith "Unexpected in build_susp")} x in
  y, Array.of_list (List.rev !refs)

let susp_table = Hashtbl.create 20

let _insist_verifier () = assert (Hashtbl.length susp_table = 0)

let propagate_beliefs d tag i =
  let finish,ctr,arr = Hashtbl.find susp_table tag in
  arr.(i) := Hash d;
  if ctr = 1 then begin
    (* With this final hash, we can finish the parent *)
    Hashtbl.remove susp_table tag;
    finish()
  end else if ctr > 1 then begin
    (* Progress, but still more to do... *)
    Hashtbl.replace susp_table tag (finish,ctr - 1,arr);
  end else failwith "ctr should not go negative"

let finish_func : 'a authtype -> 'a authtype = function
  | Shallow d
  | Suspension {contents=Hash d} -> Shallow d
  | _ -> failwith "finish_func called on something else"

let _unauth_verifier_susp map (a : 'a authtype) : 'a =
  let x = Marshal.from_channel !prf_input in
  let tag = object end in
  let y, refs = build_susp tag map x in
  let finish () =
    let z = map {authmap=finish_func} y in
    match a with
    | Shallow d
    | Suspension {contents=Hash d} ->
        (* already valid *)
        if d = hash z then ignore()
        else failwith "Invalid data in proof stream"
    | Suspension {contents=Tag(tag,i)} ->
        (* propagate evidence *)
        propagate_beliefs (hash z) tag i;
        (* proceed with suspended disbelief *)
        x
    | _ -> failwith "_unauth_verifier finish called with other than"
  in
  if Array.length refs = 0 then begin
    finish()
  end else begin
    Hashtbl.add susp_table tag (finish,Array.length refs,refs)
  end;
  y

let _auth_verifier_susp map  (x : 'a) : 'a authtype = 
  (* Can't build an authtype with something that's been propagated *)
  Shallow (hash (map {authmap=finish_func} x))



let _unauth_verifier_susp map (a : 'a authtype) : 'a =
  let x = Marshal.from_channel !prf_input in
  let tag = object end in
  let y, refs = build_susp tag map x in
  let finish () =
    let z = map {authmap=finish_func} y in
    match a with
    | Shallow d
    | Suspension {contents=Hash d} ->
        (* already valid *)
        if d = hash z then ignore()
        else failwith "Invalid data in proof stream"
    | Suspension {contents=Tag(tag,i)} ->
        (* propagate evidence *)
        propagate_beliefs (hash z) tag i;
        (* proceed with suspended disbelief *)
        x
    | _ -> failwith "_unauth_verifier finish called with other than"
  in
  if Array.length refs = 0 then begin
    finish()
  end else begin
    Hashtbl.add susp_table tag (finish,Array.length refs,refs)
  end;
  y


let _auth_ideal _ a = Ideal a
let _unauth_ideal _ = function | Ideal a -> a 
| _ -> failwith "_unauth_ideal called without (Ideal a)"

let endswith s suf = 
  let lsuf = String.length suf in 
  let l    = String.length s   in
  (suf = String.sub s (l - lsuf) lsuf);;
let is_verifier = endswith (Filename.basename(Sys.executable_name)) "_vrf";;
let is_prover = endswith (Filename.basename(Sys.executable_name)) "_prv";;
let is_ideal = endswith (Filename.basename(Sys.executable_name)) "_idl";;

let mode_name = 
  if is_prover then "prover"
  else if is_verifier then "verifier"
  else "ideal"


let insist x = 
  if is_verifier then _insist_verifier() else
  if is_prover then _insist_prover() else
  ignore();
  x

let flush_cache () =
  counter := 0;
  Hashtbl.clear hash_buf;
  lru_buf := IMap.empty;
  flush_all()
