open Types
open Typedtree
open Parsetree
open Ident
open Asttypes
open Location


let print_ident ppf id = Format.fprintf ppf "%s/%d" id.Ident.name id.Ident.stamp

let rec print_path ppf = function
  | Path.Pident id -> print_ident ppf id
  | Path.Pdot (p, name, n) -> Format.fprintf ppf "%a.%s__%d" print_path p name n
  | Path.Papply (p1, p2) -> Format.fprintf ppf "%a(%a)" print_path p1 print_path p2

let get_name = function
  | Path.Pident id -> Ident.name id
  | Path.Pdot (_, name, _) -> name
  | Path.Papply _ -> assert false

let test env ty vdesc =
  let snapshot = Btype.snapshot () in
  let ity = Ctype.instance env vdesc.val_type in
  let res = try  Ctype.unify env ty ity; true with _ -> false in
  Btype.backtrack snapshot;
  res

let resolve_overloading exp lidloc path = 
  let env = exp.exp_env in

  let name = get_name path in

  let rec find_candidates (path : Path.t) mty =
    (* Format.eprintf "Find_candidates %a@." print_path path; *)

    let sg = match Mtype.scrape env mty with
      | Mty_signature sg -> sg
      | _ -> assert false
    in
    List.fold_right (fun sitem st -> match sitem with
    | Sig_value (id, _vdesc) when Ident.name id = name -> 
        let lident = Longident.Ldot (Untypeast.lident_of_path path, Ident.name id) in
        let path, vdesc = Env.lookup_value lident env  in
        if test env exp.exp_type vdesc then (path, vdesc) :: st else st
    | Sig_module (id, _mty, _) -> 
        let lident = Longident.Ldot (Untypeast.lident_of_path path, Ident.name id) in
        let path, mty = Env.lookup_module lident env  in
        find_candidates path mty @ st
    | _ -> st) sg []
  in
  
  let lid_opt = match path with
    | Path.Pident _ -> None
    | Path.Pdot (p, _, _) -> Some (Untypeast.lident_of_path p)
    | Path.Papply _ -> assert false
  in

  match 
    Env.fold_modules (fun _name path mty st -> 
      find_candidates path mty @ st) lid_opt env []
  with
  | [] -> failwith "overload resolution failed: no match" 
  | [path, vdesc] -> 
      Format.eprintf "RESOLVED: %a@." print_path path;
      let ity = Ctype.instance env vdesc.val_type in
      Ctype.unify env exp.exp_type ity; (* should succeed *)
      { exp with 
        exp_desc = Texp_ident (path, {lidloc with Asttypes.txt = Untypeast.lident_of_path path}, vdesc);
        exp_type = exp.exp_type }
  | _ -> failwith "overload resolution failed: too ambiguous" 

let loc_table = Hashtbl.create 100
let loc_table_auth = Hashtbl.create 100
let hasauth_table = Hashtbl.create 100
let map_table = Hashtbl.create 100

let map_ident id = Longident.Lident("map_" ^ string_of_int id)
let var i = "__x" ^ string_of_int i

let rec apply_map env var typ =
  if has_authtype env typ then match Hashtbl.find hasauth_table typ.id with 
    None -> assert(false) 
  | Some typid ->
      {pexp_loc=Location.none;
       pexp_desc=Pexp_apply(
       {pexp_loc=Location.none;
        pexp_desc=Pexp_ident(Location.mknoloc(map_ident typid))},
       ["", {pexp_loc=Location.none;
             pexp_desc=Pexp_ident(Location.mknoloc(Longident.Lident "__func"))};
        "", {pexp_loc=Location.none;
             pexp_desc=Pexp_ident(var)}])}
  else {pexp_loc=Location.none; 
        pexp_desc=Pexp_ident(var)}

and has_authtype env typ =
  let make_func exp = {pexp_loc=Location.none;
                       pexp_desc=Pexp_function(
                       "", None, [{ppat_loc=Location.none;
                                   ppat_desc=Ppat_var(Location.mknoloc("__func"))},
                                  exp])} in
  if Hashtbl.mem hasauth_table typ.id then Hashtbl.find hasauth_table typ.id <> None
  else (match typ.desc with
  | Ttuple(exprs) ->
      let res = List.fold_right (||) (List.map (has_authtype env) exprs) false
      in
      if not res then Hashtbl.add hasauth_table typ.id None else
      Hashtbl.add hasauth_table typ.id (Some typ.id);
      Hashtbl.add map_table typ.id 
        (make_func
           {pexp_loc=Location.none;
            pexp_desc=Pexp_function
              ("", None,
               [{ppat_desc=Ppat_tuple(List.mapi (fun i _ ->
                 {ppat_desc=Ppat_var(Location.mknoloc(var i));
                  ppat_loc=Location.none}) exprs);
                 ppat_loc=Location.none},
                {pexp_desc=Pexp_tuple(List.mapi (fun i expr ->
                  apply_map env (Location.mknoloc(Longident.Lident(var i))) expr) exprs);
                 pexp_loc=Location.none}])});
      res
  | Tconstr(path,args,abbrv) ->
      let res,id = (match Env.find_type path env with
      | {type_kind=Type_variant(({Ident.name="Merkle"},_,_)::_)} ->
          true, 0
      | {type_kind=Type_variant(cstrs)} ->
          let res = List.fold_right (||)
            (List.map (fun (id,exprs,expropt) ->
              List.fold_right (||) (List.map (has_authtype env) exprs) false)
               cstrs) false in
          if res then
            Hashtbl.add map_table typ.id
              (make_func 
                 {pexp_loc=Location.none;
                  pexp_desc=Pexp_function
                    ("",None,List.map (fun (id,exprs, expropt)
                      -> {ppat_loc=Location.none;
                          ppat_desc=Ppat_construct
                            (Location.mknoloc(Longident.Lident(id.name)), (match exprs with
                            | [] -> None
                            | [expr] -> Some ({ppat_loc=Location.none;
                                               ppat_desc=Ppat_var(Location.mknoloc "x")})
                            | xs -> Some ({ppat_loc=Location.none;
                                           ppat_desc=Ppat_tuple
                                             (List.mapi (fun i x -> 
                                               {ppat_loc=Location.none;
                                                ppat_desc=Ppat_var(Location.mknoloc(var i))}) xs)})
                                                                          ), false)},
                        {pexp_loc=Location.none;
                         pexp_desc=Pexp_construct
                           (Location.mknoloc(Longident.Lident(id.name)), (match exprs with
                           | [] -> None
                           | [expr] -> Some (apply_map env (Location.mknoloc(Longident.Lident"x")) expr)
                           | xs -> Some ({pexp_loc=Location.none;
                                        pexp_desc=Pexp_tuple
                                            (List.mapi (fun i x ->
                                              apply_map env (Location.mknoloc(Longident.Lident(var i))) x) exprs)})
                                                                         ),false)}) 
                       cstrs)});
          res, typ.id
      | {type_kind=Type_abstract}
      | {type_kind=Type_record(_)} -> false, typ.id)
      in
      Hashtbl.add hasauth_table typ.id (if not res then None else Some id);
      res
  | Tvariant(row_desc) ->
      Printf.eprintf "variant type:\n";
      Printtyp.raw_type_expr Format.err_formatter typ;
      assert false
  | Tnil ->
      Printf.eprintf "nil type:\n";
      Printtyp.raw_type_expr Format.err_formatter typ;
      assert false
  | Tvar(v) ->
      Printf.eprintf "var type:\n";
      (match v with 
        None -> Printf.eprintf "None\n" 
      | Some vv -> Printf.eprintf "%s\n" vv);
      Printtyp.raw_type_expr Format.err_formatter typ;
      assert false
  | Tlink(texp)
  | Tsubst(texp) -> 
      let res = has_authtype env texp in
      Hashtbl.add hasauth_table typ.id (Hashtbl.find hasauth_table texp.id);
      res
  | _ -> 
      Printf.eprintf "concrete type problem:\n";
      Printtyp.raw_type_expr Format.err_formatter typ;
      Printf.eprintf "ok\n";
      failwith "unsupported")


class map = object (self)
  inherit Ttmap.map as super

  method! expression = function
    | ({ exp_desc= Texp_apply({exp_desc=Texp_ident(_,{txt=Longident.Lident "unauth"},_)} as f,
                            [_,Some x,_]); 
         exp_type=a} as e) ->
           Printf.eprintf "unauth\n";
           Printtyp.type_expr Format.err_formatter a;
           Hashtbl.add loc_table f.exp_loc a;
           has_authtype e.exp_env a;
           ignore(super#expression x);
           self, e
    | ({ exp_desc= Texp_apply({exp_desc=Texp_ident(_,{txt=Longident.Lident "auth"},_)} as f, 
                              [_,Some({exp_type=a} as x),_]);
         exp_type=_} as e) ->
           Printf.eprintf "auth\n";
           Printtyp.type_expr Format.err_formatter a;
           Hashtbl.add loc_table_auth f.exp_loc a;
           has_authtype e.exp_env a;
           ignore(super#expression x);
           self, e
    | e -> super#expression e
end

let umap =
  let optim = try
    if Sys.getenv "STRATEGY" = "susp" then "_susp"
    else if Sys.getenv "STRATEGY" = "buf" then "_buf"
    else ""
  with Not_found -> "" in
  object(self)
    inherit Ast_mapper.mapper as super

    method! typ = function
      | {ptyp_desc=Ptyp_constr({txt=Longident.Lident "authtype"},[a])} as e ->
          if Sys.getenv "MODE" = "ideal" then super#typ a
          else super#typ e
      | e -> super#typ e

    method! expr = function
      | {pexp_loc=loc} when Hashtbl.mem loc_table loc ->
          if Sys.getenv "MODE" = "ideal" then
            {pexp_loc=Location.none;
             pexp_desc=Pexp_function("",None,[
                                     {ppat_loc=Location.none;
                                      ppat_desc=Ppat_var(Location.mknoloc "x")},
                                     {pexp_loc=Location.none;
                                      pexp_desc=Pexp_ident(Location.mknoloc(Longident.Lident"x"))}])
           }
          else
            {pexp_desc=Pexp_ident (Location.mknoloc (Longident.Lident("unauth_" ^ string_of_int (Hashtbl.find loc_table loc).id)));
             pexp_loc=Location.none}
      | {pexp_loc=loc} when Hashtbl.mem loc_table_auth loc ->
          if Sys.getenv "MODE" = "ideal" then
            {pexp_loc=Location.none;
             pexp_desc=Pexp_function("",None,[
                                     {ppat_loc=Location.none;
                                      ppat_desc=Ppat_var(Location.mknoloc "x")},
                                     {pexp_loc=Location.none;
                                      pexp_desc=Pexp_ident(Location.mknoloc(Longident.Lident"x"))}])
           }
          else
            {pexp_desc=Pexp_ident (Location.mknoloc (Longident.Lident("auth_" ^ string_of_int (Hashtbl.find loc_table_auth loc).id)));
             pexp_loc=Location.none}
      | e -> super#expr e

    method! structure_item = function
      | {pstr_desc=Pstr_primitive(_,{pval_prim=["MERKLE"]})} -> 
          if Sys.getenv "MODE" = "ideal" then [] else
          [{pstr_loc=Location.none;
            pstr_desc=Pstr_value(Recursive, 
                                 (Hashtbl.fold (fun id mapf ps -> 
                                   ({ppat_desc=Ppat_var
                                       (Location.mknoloc("map_" ^ string_of_int id));
                                     ppat_loc=Location.none},
                                    mapf) :: ps) map_table []))}] @
          (Hashtbl.fold (fun _ t ps ->
            {pstr_loc=Location.none;
             pstr_desc=Pstr_value
               (Nonrecursive,
                [{ppat_desc=Ppat_var(Location.mknoloc("unauth_" ^ string_of_int t.id));
                  ppat_loc=Location.none},
                 {pexp_loc=Location.none;
                  pexp_desc=Pexp_apply
                    ({pexp_loc=Location.none;
                      pexp_desc=Pexp_ident(Location.mknoloc(Longident.Lident (if Sys.getenv "MODE" = "prover" then "_unauth_prover" ^ optim else if Sys.getenv "MODE" = "verifier" then "_unauth_verifier" ^ optim else "_unauth_ideal")))},
                     ["",{pexp_loc=Location.none;
                          pexp_desc=Pexp_ident(Location.mknoloc(Longident.Lident (match Hashtbl.find hasauth_table t.id with None -> "discard" | Some id -> "map_" ^ string_of_int id)))}])}])} :: ps) loc_table []) @
          (Hashtbl.fold (fun _ t ps ->
            {pstr_loc=Location.none;
             pstr_desc=Pstr_value
               (Nonrecursive,
                [{ppat_desc=Ppat_var(Location.mknoloc("auth_" ^ string_of_int t.id));
                  ppat_loc=Location.none},
                 {pexp_loc=Location.none;
                  pexp_desc=Pexp_apply
                    ({pexp_loc=Location.none;
                      pexp_desc=Pexp_ident(Location.mknoloc(Longident.Lident 
                                                              (if Sys.getenv "MODE" = "prover" then "_auth_prover" ^ optim else if Sys.getenv "MODE" = "verifier" then "_auth_verifier" ^ optim else "_auth_ideal")))},
                     ["",{pexp_loc=Location.none;
                          pexp_desc=Pexp_ident(Location.mknoloc(Longident.Lident (match Hashtbl.find hasauth_table t.id with None -> "discard" | Some id -> "map_" ^ string_of_int id)))}])}])} :: ps) loc_table_auth [])
      | str -> super#structure_item str
end

let structure str = 
  let o = new map in
  let _, str =  o#structure str in
  str

let splice_in str =
  let str = umap#structure str in
  let str = str @
    [] in
  Printf.eprintf("done splicing\n");
  str
