open Cil
module C = Cil

(*type step = 
  | Start of int
  | Edge of int * C.stmt * int
  | Exit of int*)

let debug = false


let pathID = ref 0
let nextPathID () : int = pathID := !pathID + 1; !pathID

type spath = C.stmt list

type path =
 | ROPath of int * C.fundec * string * spath
 | WRPath of int * C.fundec * string * spath * C.exp * C.stmt * spath 
                             (* prefix   cond     atm wr  suffix *)

let length p =  match p with
  | ROPath(_,fd,fn,rostmts) -> List.length rostmts

let wr_name p = match p with 
  | WRPath(id,_,fn,pre,e,wr,_) -> Printf.sprintf "wr%d:%s:ARW[%s][%s]" id fn (Utils.pp_exp e) (Utils.pp_stmt_nl wr)

let rd_name p = match p with 
  | ROPath(id,_,fn,pth) ->
    let last_s = List.hd (List.tl (List.rev pth)) in
    Printf.sprintf "rd%d:%s:[last stmt: %s]" id fn (Utils.pp_stmt_nl last_s)

let get_funname p = match p with 
  | ROPath(_,_,fn,_)
  | WRPath(_,_,fn,_,_,_,_) -> fn

let get_id p = match p with 
  | ROPath(id,_,_,_)
  | WRPath(id,_,_,_,_,_,_) -> id

let pp_path p : string =
  match p with 
  | ROPath(id,fd,fn,sl) ->
      ("%%% Path rd"^(string_of_int id)^" from "^fn^":\n"^(String.concat "\n" (List.map Utils.pp_stmt sl)))
  | WRPath(id,fd,fn,pre,b,a,sfx) ->
      ("%%% Path wr"^(string_of_int id)^"  from "^fn^":\nPrefix:\n"^(String.concat "\n" (List.map Utils.pp_stmt pre))
      ^"\nCond: "^(Utils.pp_exp b)
      ^"\nWrite: "^(Utils.pp_stmt a))
      ^"Suffix:\n"^(String.concat "\n" (List.map Utils.pp_stmt sfx))
  | _ -> "TODO"   

let pp_paths (pl:path list) (sep:string) : string =
   String.concat sep (List.map pp_path pl)

(* **************************************************************** *)
(* DFS Search *)
(* **************************************************************** *)
type dfs_state = 
 | ROdfs | WRcond of C.exp * C.instr list | WRsucc of spath * C.exp * C.stmt * C.stmt * spath | WRfail of C.exp * C.stmt
let pp_dfs d = match d with 
  | ROdfs -> "ROdfs" | WRcond(e,il) -> "WRcond("^(Utils.pp_exp e)^","^(Utils.pp_instrl il)^")"
  | WRsucc(pre,e,wr_il,rest_il,_) -> "WRsucc("^(Utils.pp_exp e)^",<wr_instrs>,<rest_instrs>)" 
  | WRfail(e,ro_ils) -> "WRfail("^(Utils.pp_exp e)^",<ro_instrs>)"

let dfs_trans d s (root_to_here:spath) = 
  if debug then Printf.printf "\ndfs_trans from %s via stmt:\n" (pp_dfs d);
  if debug then (C.dumpStmt C.defaultCilPrinter stdout 1 s);
  match (d,s.skind) with
  | (ROdfs,Return(v,w)) -> ROdfs
  | (WRsucc(pref,e,wrs,sfx,rest),Return(v,w)) -> WRsucc(pref,e,wrs,sfx,rest)
  | (WRfail(e,ils),Return(v,w)) -> d
  | (WRfail(e,ils),Break(_)) -> d
  | (WRfail(e,ils),Instr([i])) -> if (Utils.is_set_instr i "_endARWfail_") then d else failwith "dfs strange2"
  | (WRcond(_),Return(v,w)) -> failwith "weird return"
  | (ROdfs,If(BinOp(BOr,Lval(Var v,_),cond,_),_,_,_)) ->
    if String.compare v.vname "_beginARW_" = 0 then WRcond(cond,[])
    else ROdfs
  | (ROdfs,If(_,_,_,_)) -> ROdfs
  | (WRcond(e,wr_ils),Instr(il)) ->
     let rec _help isPrefix il acc_wr_stmts = match il with 
       | i :: r ->
           if (Utils.is_set_instr i "_endARWsucc_") then
              WRsucc(root_to_here,e,C.mkStmt (Instr(acc_wr_stmts@[i])),C.mkStmt (Instr(r)),[])
           else if (Utils.is_set_instr i "_endARWfail_") then
              WRfail(e,C.mkStmt (Instr(acc_wr_stmts@il)))
           else 
              _help isPrefix r (acc_wr_stmts@[i])
       | [] ->
          if debug then print_endline "  reached end of instr list. insters were:";
          if debug then List.iter (fun i -> Printf.printf "  instr: %s\n" (Utils.pp_instr i)) il;
          (*failwith "dfs_trans: il after WRCond didn't have an endARW. maybe it's forthcoming?"*)
          WRcond(e,wr_ils)
     in
     _help true (wr_ils@il) []
  | (WRcond(e,ils),If(b,e1,e2,_)) -> WRcond(e,ils@[Utils.mkAssumeInstr e])
  | (ROdfs,Instr(il)) -> ROdfs
  | (ROdfs,Switch(_,_,_,_)) -> ROdfs
  | (ROdfs,Break(_)) -> ROdfs
  | _ -> failwith ("dfs_trans unknown how to transition from: " ^(pp_dfs d)^" Stmt was: "^(Utils.pp_stmt s))

let strip_beginARW e = match e with
  | BinOp(BOr,Lval(Var v,_),cond,_) ->
    if String.equal v.vname "_beginARW_" then cond else e
  | _ -> e

let mkAssumes s = 
   let fdec = emptyFunction "assume" in
   fdec.svar.vtype <- TFun(voidType,Some [],true, []);
   match s.skind with 
   | If (exp,tblk,eblk,loc) -> 
      let exp' = strip_beginARW exp in
      (
        mkStmtOneInstr (Call(None, (Lval(Var(fdec.svar),NoOffset)), [exp'], loc)),
        mkStmtOneInstr (Call(None, (Lval(Var(fdec.svar),NoOffset)), [UnOp(LNot,exp',typeOf(exp'))], loc))
      )
  | _ -> failwith "mkAssumes: not an If"

let rec dfs (d:dfs_state) (s:C.stmt) (root_to_here:spath) accRO accWR fname fd : (path list * path list) =
    if debug then print_endline "dfs"; flush stdout;
    (* compute next dfs state based on s *)
    let d' = dfs_trans d s root_to_here in 
    
    (* now decide how to proceed into successors *)
    match s.succs with
    | [] -> begin if debug then print_endline "\n------ end of path"; (match d' with
            | ROdfs -> (ROPath(nextPathID(),fd,fname,root_to_here) :: accRO,accWR)
            | WRsucc(pref,e,wr_ils,rest_ils,sfx) -> 
                (accRO,WRPath(nextPathID(),fd,fname,pref,e,wr_ils,rest_ils::sfx)::accWR)
            | WRfail(e,ils) -> (ROPath(nextPathID(),fd,fname,root_to_here)::accRO,accWR)
            | _ -> failwith "dfs reach end of a non-RO path") end
    | [s'] -> 
        dfs d' s' (root_to_here @ [s]) accRO accWR fname fd
    | [s';s''] ->
        let (asmT,asmF) = mkAssumes s in
        let (ros1,wrs2) = dfs d' s' (root_to_here @ [asmT]) [] [] fname fd in
        if debug then Printf.printf "finished %d, %d\n" (List.length ros1) (List.length wrs2);
        if debug then print_endline ("   now to other branch point, backtracked to: "^(pp_dfs d')^". will now assume: "^(Utils.pp_stmt asmF));
        dfs d' s'' (root_to_here @ [asmF]) (ros1@accRO) (wrs2@accWR) fname fd
    | [s1;s2;s3;s4] ->
        let (ros1,wrs1) = dfs d' s1 root_to_here [] [] fname fd in
        let (ros2,wrs2) = dfs d' s2 root_to_here [] [] fname fd in 
        let (ros3,wrs3) = dfs d' s3 root_to_here [] [] fname fd in 
        dfs d' s4 root_to_here (ros1@ros2@ros3@accRO) (wrs1@wrs2@wrs3@accWR) fname fd

(*

      (match s.skind with 
      | Return (_,_) ->
         let fullPath = root_to_here @ [(s'.sid,s')] in
         (match d' with 
         | ROdfs -> ([ROPath(fname,fullPath)],[])
         | WRdone(b,a) -> ([],[WRPath(fname,b,a,fullPath)])
         | _ -> failwith "dfs got to return in a weird dfs state")
      | _ -> 
          dfs d' s' (root_to_here @ [(s'.sid,s')]) accRO accWR fname)
*)
let findAll (fds:C.fundec list) : (path list * path list) =
   let rec _findAll accRO accWR fds =
     match fds with 
     | fd :: rest ->
        Printf.printf "  - searching %s ..." fd.svar.vname;
        C.dumpGlobal C.defaultCilPrinter stdout (GFun(fd,Utils.dumLoc)); 
        let (ro_paths,wr_paths) = dfs ROdfs (List.hd fd.sbody.bstmts) [] [] [] fd.svar.vname fd in
        Printf.printf " %d read paths. %d write paths.\n" (List.length ro_paths) (List.length wr_paths);
        _findAll (ro_paths@accRO) (wr_paths@accWR) rest
      | [] -> print_endline "    search done"; (accRO,accWR)
  in
  _findAll [] [] fds
