open List
open Cil
module C = Cil

let string_of_doc d : string = 
  Pretty.sprint 80 d

let string_of_lval lv : string = 
   let d = C.d_lval () lv in
   string_of_doc d

let pp_exp e : string = 
   let d = C.d_exp () e in
   string_of_doc d

let pp_stmt s : string =
   (* C.printCilAsIs := true; *)
   let d = C.d_stmt () s in
   string_of_doc d

let pp_stmt_nl s : string = 
   let s' = pp_stmt s in
   Str.global_replace (Str.regexp "_endARWsucc_ = 1;") "" (
      Str.global_replace (Str.regexp "\n") "" s')

let pp_instr (i:C.instr) : string = 
   pp_stmt (C.mkStmt (Instr([i])))

let pp_instrl (il:C.instr list) : string = 
   pp_stmt (C.mkStmt (Instr(il)))

class sidVisitor (sid: int) = object (self)
  inherit nopCilVisitor
  val found = ref false
  method vstmt (s: stmt) : stmt visitAction = 
    if s.sid = sid then begin
       found := true;
       ignore (C.dumpStmt C.defaultCilPrinter stdout 1 s) end;
    DoChildren
  method found() : bool = !found
end
let print_sid sid (fd:C.fundec) : unit =
   print_endline ("[[sid="^(string_of_int sid)^" statement is:");
   let v = (new sidVisitor(sid)) in
   visitCilFunction (v :> nopCilVisitor) fd;
   if v#found() = false then print_endline "NOT FOUND!";
   print_endline "\n]]";
   ()

let is_set_instr i var_name : bool =
  match i with
  | Set(lv,_,_) -> 
     (String.equal (string_of_lval lv) var_name)
  | _ -> false

(* ****************************************************************** *)
(* creating CIL code *)

let dumLoc = {line=1;file="foo";byte=1}

let mkAssumeInstr (e:C.exp) : C.instr = 
  let fdec = emptyFunction "assume" in
  fdec.svar.vtype <- TFun(voidType, Some [], true, []);
  (Call(None, (Lval(Var(fdec.svar),NoOffset)), [e], dumLoc))

let mkAssume (e:C.exp) : C.stmt = mkStmtOneInstr (mkAssumeInstr e)

let edebug = false
let assertf: varinfo option ref = ref None
let addProto = ref false
let makeAssertF () : varinfo = 
    match !assertf with 
      Some v -> v
    | None -> begin 
        let v = makeGlobalVar "assert" 
                     (TFun(voidType, Some [("cond", intType, [])],
                             true, [])) in
        assertf := Some v;
        addProto := true;
        v
    end

let mkAssert e loc : C.instr = 
   let a:varinfo = makeAssertF () in
   Call(None, Lval(var a), [e], loc)

let nondetf: varinfo option ref = ref None
let mkNondet_vi () : C.varinfo = 
   match !nondetf with 
   | Some(v) -> v
   | None ->
      let vi : varinfo = makeGlobalVar "__VERIFIER_nondet_int" (TFun(C.intType, None,true, [])) in
      nondetf := Some(vi);
      vi


let neg e : C.exp = UnOp(LNot,e,C.intType)
let mkLand b1 b2 : C.exp = BinOp(LAnd,b1,b2,TInt(IBool, []))
let mkBO op varname e ty : C.exp =
  BinOp(op,Lval(C.var (C.makeVarinfo true varname C.intType)),e,ty)

let call_nd_var vname : C.stmt = 
   let ctr_lval = C.var (C.makeVarinfo true vname C.intType) in
   let nd_vi = mkNondet_vi() in
   mkStmtOneInstr (Call(Some(ctr_lval),Lval(Var nd_vi, NoOffset),[],dumLoc))

let call_nd_PtrField vname fieldName fieldTy : C.stmt = 
   let nd_vi = mkNondet_vi() in
   let ptrName = C.makeVarinfo true vname C.voidPtrType in
   let offs = Field({fcomp=(C.mkCompInfo true "node_t" (fun t->[]) []);fname=fieldName;
                     ftype=fieldTy;fbitfield=None;fattr=[];floc=dumLoc},NoOffset) in
   mkStmtOneInstr (Call(Some((Var ptrName, offs)),Lval(Var nd_vi, NoOffset),[],dumLoc))
   
let notev: varinfo option ref = ref None
let mkNote str : C.stmt = 
   let vi = (match !notev with 
      | Some(v) -> v
      | None -> 
          let t = makeGlobalVar "_NOTE_" (TPtr(TVoid([]),[])) in 
          notev := Some(t); t) in 
   mkStmtOneInstr (Set((Var(vi),NoOffset),Const(CStr(str)),dumLoc))

let split_stmts (ll':C.stmt list) n = 
   let rec _help ll i accFront =
      if i = n+1 then failwith "list_split: invalid position"
      else if i = n then 
         (accFront,ll)
      else 
         match ll with
         | [] -> if i < n then failwith "list_split: not enough elements" else (accFront,ll)
         | h :: r ->
            _help r (i+1) (accFront@[h])
   in
   _help ll' 0 []

let all_combinations opts : C.exp list = 
   let rec _help choices_so_far opts_left = 
      match opts_left with 
      | [(a,b)] ->
            ([List.fold_left (fun e ac -> BinOp(LAnd,e,ac,C.intType)) C.one (a::choices_so_far);
              List.fold_left (fun e ac -> BinOp(LAnd,e,ac,C.intType)) C.one (b::choices_so_far)])
      | (a,b) :: rest -> 
         (_help (a::choices_so_far) rest)
         @
         (_help (b::choices_so_far) rest)
      | _ ->failwith "all_combinatoins"
   in
   _help [] opts
            
(* apply f to each element of the list, with a second argument of every other *)
let iter_q_qs (f: 'a -> 'a list -> unit) (qs:'a list) : unit =
   if List.length qs = 0 then failwith "iter_q_qs";
   let rec _help pre cur sfx = 
    f cur (pre @ sfx);
    match sfx with 
    | [] -> ()
    | s :: rest -> _help (pre @ [cur]) s rest 
   in
   _help [] (List.hd qs) (List.tl qs)

let rec find_fd gls funname : C.fundec =
  match gls with 
  | [] -> failwith "find_fd: not found"
  | GFun(fd,_) :: rest when (String.equal fd.svar.vname funname) -> fd
  | _ :: rest -> find_fd rest funname

let grepAfter line filename : string = 
  let result = ref "" in
  let beg = ref false in
  let chan = open_in filename in
  try
    while true; do
      let s = input_line chan in 
      (* print_endline ("read:["^s^"]"); *)
      let s' = (Str.global_replace (Str.regexp "&") "\\&" s) in
      if !beg then
        result := (!result) ^ s' ^"\n"
      else if String.equal s line then begin print_endline "found";
        result := (!result) ^ s' ^"\n"; beg := true
      end else 
        ()
    done; !result
  with End_of_file ->
    close_in chan; !result

let texesc s = 
 Str.global_replace (Str.regexp "(unsigned long )") "" (
 Str.global_replace (Str.regexp "_") "\\_" (
   Str.global_replace (Str.regexp "%") "\\%" (
      Str.global_replace (Str.regexp "&") "\\&" s
  )))

let dotesc s = 
 Str.global_replace (Str.regexp "(unsigned long )") "" (
      Str.global_replace (Str.regexp "&&") "\\l&&" s
  )