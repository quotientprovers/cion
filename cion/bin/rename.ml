open Cil
open Cfg

module C = Cil
module E = Errormsg
module H = Hashtbl
module IH = Inthash
class renameVisitor (prefix: string) = object (self)
  inherit nopCilVisitor
        (* Keep here a maping from locals to their copies *)
  val map : (string, varinfo) H.t = H.create 113 
      (* Keep here a maping from statements to their copies *)
  val stmtmap : (int, stmt) H.t = H.create 113
  val sid = ref 0 (* Will have to assign ids to statements *)
      (* Keep here a list of statements to be patched *)
  val patches : stmt list ref = ref []

  val argid = ref 0

        (* We must create a new varinfo for each declaration. Memoize to 
       * maintain sharing *)
  method vvdec (v: varinfo) = 
    (* Some varinfo have empty names. Give them some name *)
    if v.vname = "" then begin
      v.vname <- "arg" ^ string_of_int !argid; incr argid
    end;
    try
      ChangeTo (H.find map v.vname)
    with Not_found -> begin
      let v' = {v with vid = newVID (); vname = prefix ^ v.vname } in
      H.add map v.vname v';
      ChangeDoChildrenPost (v', fun x -> x)
    end

      (* We must replace references to local variables *)
  method vvrbl (v: varinfo) = 
    if v.vglob then SkipChildren else 
    try
      ChangeTo (H.find map v.vname)
    with Not_found -> 
      E.s (bug "Cannot find the new copy of local variable %s" v.vname)

        (* Replace statements. *)
  method vstmt (s: stmt) : stmt visitAction = 
    s.sid <- !sid; incr sid;
    let s' = {s with sid = s.sid} in
    H.add stmtmap s.sid s'; (* Remember where we copied this *)
    (* if we have a Goto or a Switch remember them to fixup at end *)
    (match s'.skind with
      (Goto _ | Switch _) -> patches := s' :: !patches
    | _ -> ());
    (* Do the children *)
    ChangeDoChildrenPost (s', fun x -> x)
end

let rename locals stmts prefix = 
  let (v:renameVisitor) = (new renameVisitor(prefix)) in
  let tmp_fd = {
        svar=C.makeVarinfo true "tmpEJK" (TFun(C.voidType,None,false,[]));
        sformals=[];
        slocals=locals;
        smaxid=100000;
        sbody={ battrs=[]; bstmts=stmts };
        smaxstmtid=None;
        sallstmts=[]
  } in 
  let fd' = visitCilFunction (v :>  nopCilVisitor) tmp_fd in
  (fd'.slocals,fd'.sbody.bstmts)