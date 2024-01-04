
open Path

let debug = false

let readEnabled q ropath allglobals inits = 
  let note : string = Printf.sprintf "checking if read(%s) is enabled from q:(%s) ... "
     (Path.rd_name ropath) (Aut.pp_q q) in
  print_endline note;
  let asmq = Utils.mkAssume q in
  match ropath with 
  | ROPath(id,fd,fname,pth) ->  
      let r = (Solver.feasible note fd.slocals (inits @ [asmq] @ pth) fd allglobals) in
      print_endline (if r then "enabled" else "NOT enabled");
      r

let rec splitOnReadEnabled q ropaths accEn accDis allglobals inits = 
  match ropaths with 
  | [] -> (accEn,accDis)
  | rpath :: rest -> 
      if readEnabled q rpath allglobals inits then 
         let feasFN = Solver.last_fn() in 
         let funName = Path.get_funname rpath in 
         splitOnReadEnabled q rest ((rpath,funName,feasFN)::accEn) accDis allglobals inits
      else
         splitOnReadEnabled q rest accEn (rpath::accDis) allglobals inits

let writeEnabled q wrpath allglobals inits = 
  let note : string = Printf.sprintf "checking if write(%s) is enabled from q:(%s) ... " (Path.wr_name wrpath) (Aut.pp_q q) in
  print_endline note;
  let asmq = Utils.mkAssume q in
  match wrpath with 
  | WRPath(id,fd,fname,pref,e,wr,sfx) ->  
      let asme = Utils.mkAssume e in
      let r = (Solver.feasible note fd.slocals (inits @ [asmq] @ pref @ [asme]) fd allglobals) in
      print_endline (if r then "enabled" else "NOT enabled");
      r

let writePostFeasible q wrpath q' allglobals inits = 
  let note = Printf.sprintf "checking if q:(%s)->q'(%s) is feasible post from this write\n" (Aut.pp_q q) (Aut.pp_q q') in
  let asmq  = Utils.mkAssume q in
  let asmq' = Utils.mkAssume q' in
  match wrpath with 
  | WRPath(id,fd,fname,pref,e,wr,sfx) ->  
      let asme = Utils.mkAssume e in
      Solver.feasible note fd.slocals (inits @ [asmq] @ pref @ [asme] @ [wr] @ sfx @ [asmq']) fd allglobals

(* check if this interleaving is feasible *)
let feasibleInterleaving wrpath (ropath:path) q i allglobals inits : bool = 
  match ropath with
  | ROPath(id,fd,fn,rostmts) -> begin 
    let (locals_renamed,rostmts_renamed) = Rename.rename fd.slocals rostmts "reader_" in
    if debug then List.iter (fun s -> print_endline ("renamed:"^(Utils.pp_stmt s))) rostmts_renamed;
    let (roA,roB) = Utils.split_stmts rostmts_renamed i in 
    let note = Printf.sprintf "checking if interleaving is feasible from q:(%s) at position %d\n" (Aut.pp_q q) i in
    let asmq = Utils.mkAssume q in
    (* let noteinj = Utils.mkNote "--- Writer injected here:" in *)
    match wrpath with 
    | WRPath(id,wfd,fname,pref,e,wr,sfx) ->  
        let asme = Utils.mkAssume e in
        Solver.feasible note (wfd.slocals@locals_renamed) (inits @ [asmq] @ roA (*@ [noteinj]*) @ pref @ [asme;wr] @ sfx @ roB ) fd allglobals;
  end

(* return an interleaving position that makes the ropath feasible *)
let rec findFeasInterleaving wrpath (ropath:path) q i allglobals inits : Layer.itl_rd option =
  match ropath with
  | ROPath(id,fd,fn,rostmts) ->
   if i = 0 then begin
      Printf.printf "  - did not find any feasible interleavings for reader(%s)\n" (Path.rd_name ropath);
      None 
   end else if feasibleInterleaving wrpath ropath q i allglobals inits then begin
      if debug then Printf.printf "  -> found feasible interleaving at position %d!\n" i;
      Some (Layer.InterleavedReader(ropath,i,Solver.last_fn()))
   end else
      findFeasInterleaving wrpath ropath q (i-1) allglobals inits


let algorithm ropaths wrpaths qs allglobals inits resultFN dotFN disablePostCheck shortOutput stats_in : Statistics.t_out =
  let oc = open_out resultFN in 
  let ocDOT = open_out dotFN in 
  Aut.dot_begin ocDOT;
  print_endline " + begin Algorithm";
  let createROLayer q  = 
    let (enAb,disAb) = splitOnReadEnabled q ropaths [] [] allglobals inits in
    let rolay = Layer.maybeCreateRO enAb in
    Aut.recordROTrans oc ocDOT q rolay shortOutput;
    disAb
  in
  (* if the write is feasible, collect all readers for whom there is *)
  (* an interleaving that makes the reader's post-condition non-0 *)
  let tryCreateTrans q wrpath q' disabledReaders =
      Printf.printf "Trying layer: q(%s)--%s-->q(%s): " (Aut.pp_q q) (Path.wr_name wrpath) (Aut.pp_q q');
      let postFeas = if disablePostCheck then true else (
         writePostFeasible q wrpath q' allglobals inits
      ) in
      if postFeas then begin
         (* *)
         print_endline " writePost is feasible.";
         let rec _collectInterleavedReaders ropaths =
            match ropaths with 
            | [] -> []
            | rpath :: rest -> begin
               Printf.printf "    searching for an interleaving for reader (%s)...\n" (Path.rd_name rpath); flush stdout;
               match findFeasInterleaving wrpath rpath q ((Path.length rpath)-1) allglobals inits with
               | None -> _collectInterleavedReaders rest
               | Some Layer.InterleavedReader(rpath,i,fn) -> 
                  Printf.printf " * reader(%s), interleaved at %d, see %s\n" (Path.rd_name rpath) i fn; 
                  Layer.InterleavedReader(rpath,i,fn) :: (_collectInterleavedReaders rest)
               end
         in
         let readers = _collectInterleavedReaders disabledReaders in
         let lay = Layer.maybeCreate wrpath readers in
         Aut.recordTrans oc ocDOT q lay q' shortOutput
      end else 
         print_endline " writePost is NOT feasible."
  in
  let selfloop wrpath q disabledReaders = 
     tryCreateTrans q wrpath q disabledReaders
  in
  let rec nonselfloop wrpath q qq disabledReaders =
      match qq with 
      | [] -> ()
      | q' :: r -> tryCreateTrans q wrpath q' disabledReaders; nonselfloop wrpath q r disabledReaders
  in
  let ith = ref (List.length qs) in
  Utils.iter_q_qs (fun q qq -> 
     (* Collect the read-only paths enabled from q (and return those disabled) *)
     let disabledReaders = createROLayer q in
     (* Now create layers that each have one write path *)
     (List.iter (fun wrpath -> 
      if writeEnabled q wrpath allglobals inits then begin
        selfloop wrpath q disabledReaders;
        nonselfloop wrpath q qq disabledReaders;
      end;
      ) wrpaths)
  ) qs;
  Layer.all_to_dot ocDOT;
  Aut.dot_end ocDOT;
  close_out ocDOT;
  close_out oc;
  Statistics.mkSolution stats_in (Aut.getTransCount()) (Layer.count()) (Solver.get_ctr())