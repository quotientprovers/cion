open Cil

module C = Cil

let ctr = ref 0
let debug = false 

type verifier_result = 
 | STrue | SFalse | SUnknown

let string_of_vr v = match v with 
  | STrue -> "STrue" | SFalse -> "SFalse" | SUnknown -> "SUnknown"

let vr_of_string s = match s with 
  | "STrue" -> STrue  | "SFalse" -> SFalse | "SUnknown" -> SUnknown

(* **************************************************************** *)
(* Cache: md5(out/feas_*.c) => STrue|SFalse|SUnknown *)
(* **************************************************************** *)
let digs = Hashtbl.create 8000
let digs_opened = ref false

let load_digs () = 
  let chan = open_in "cache" in
  digs_opened := true;
  try
    while true; do
      let s = input_line chan in 
      let cols = Str.split (Str.regexp "|") s in 
      if List.length cols <> 2 then failwith "load_digs error" else
      let vr = vr_of_string (List.hd (List.tl cols)) in
      Hashtbl.replace digs (List.hd cols) vr;
      (*digs := (List.hd cols, vr) :: !digs; *)
      (*Printf.printf "loadde dig: %s -> %s\n" (List.hd cols) (List.hd (List.tl cols));*)
      done;
  with End_of_file ->
    close_in chan

let save_dig fn vr = 
   Hashtbl.replace digs (Digest.to_hex (Digest.file fn)) vr

let dig_persist () = 
  if !digs_opened then begin
    Sys.remove "cache";
    let oc = open_out "cache" in
    let tmp = Hashtbl.fold (fun k v ac ->  ac ^ k ^ "|" ^ (string_of_vr v) ^ "\n") digs "" in
    output_string oc tmp;
    close_out oc
  end

let get_cached_result fn = 
  let hex = Digest.to_hex (Digest.file fn) in 
  try 
     let c = Hashtbl.find digs hex in
     if debug then Printf.printf "found cached result for %s: %s\n" fn (string_of_vr c);
     Some(c)
  with Not_found -> None

(* **************************************************************** *)
(* duplicate the globals from fd *)
(* **************************************************************** *)
let collect_globals (gls: C.global list) = 
  List.filter (fun g -> 
  match g with
  | GCompTag _ | GType _ | GCompTagDecl _ | GEnumTag _ | GEnumTagDecl _ | GVarDecl _ | GVar _ -> true
  | GFun _ | GAsm _ | GPragma _ -> false
  | _ -> false
  ) gls

let header = String.concat "\n" [
  "extern void __VERIFIER_assume(int  ) ;";
  "extern void __VERIFIER_error() ;";
  "extern int __VERIFIER_nondet_int() ;";
  "#define assert(b) if (!(b)) __VERIFIER_error();";
  "#define assume(b) __VERIFIER_assume(b);";
  "#define LAND2(tt1,tt2) (tt1 && tt2)";
  "#define LAND3(tt1,tt2,tt3) (tt1 && tt2 && tt3)";
  "#define LAND4(tt1,tt2,tt3,tt4) (tt1 && tt2 && tt3 && tt4)";
  "#define LAND5(tt1,tt2,tt3,tt4,tt5) (tt1 && tt2 && tt3 && tt4 && tt5)";
  "#define LAND6(tt1,tt2,tt3,tt4,tt5,tt6) (tt1 && tt2 && tt3 && tt4 && tt5 && tt6)";
  "#define LAND7(tt1,tt2,tt3,tt4,tt5,tt6,tt7) (tt1 && tt2 && tt3 && tt4 && tt5 && tt6 && tt7)";
  ]

let mygrep fn = 
  let chan = open_in fn in
  let res = ref None in
  try
    while true; do
      let nstr = input_line chan in 
      (*print_endline ("grepping ultimate log: "^nstr);*)
      (match nstr with 
      | "RESULT: Ultimate proved your program to be correct!" -> res := Some(STrue); raise End_of_file
      | "RESULT: Ultimate proved your program to be incorrect!" -> res := Some(SFalse); raise End_of_file
      | "RESULT: Ultimate could not prove your program: Toolchain returned no result." -> begin
          print_endline ("Ultimate output: "^nstr);
          print_endline ("check the log at: ~/ultimate/Ultimate.log");
          res := None; raise End_of_file end
      | _ -> ());
    done; !res
  with End_of_file ->
    close_in chan;
    !res


let run cmd fn = 
  load_digs();
  match get_cached_result fn with 
  | Some(vr) -> Some(vr)
  | None -> begin 
    let cwd = Sys.getcwd () in
    (try Sys.chdir(Sys.getenv "ULTIMATE_HOME") with Not_found -> failwith "could not find ultimate. Set ULTIMATE_HOME");
    ignore (Sys.command cmd);
    let r = mygrep "Ultimate.log" in
  (*  | "TRUE\n" ->  STrue
    | "FALSE\n" -> Sys.chdir cwd; SFalse
    | "UNKNOWN\n" -> Sys.chdir cwd; SUnknown
    | t -> failwith ("solver.run result weird:"^t)*)
    Sys.chdir cwd; 
    let d = Digest.file fn in 
    (match r with | Some(rr) -> print_endline ("digest: "^(Digest.to_hex d)^"|"^(string_of_vr rr)); | _ -> ());
    r
  end

let get_ctr() = !ctr
let last_fn () = 
  ("out/feas"^(string_of_int !ctr)^".c")
(* **************************************************************** *)
(* check feasibility *)
(* **************************************************************** *)
let feasible note locals stmts fd allglobals : bool = 
  ctr := !ctr + 1;
  let fn = "/tmp/feas"^(string_of_int !ctr)^".c" in
  if debug then Printf.printf "\nSolver: feasibility check: %s\n" fn;
  let assert_false = (mkStmtOneInstr (Utils.mkAssert (Cil.zero) Utils.dumLoc)) in
  let funname = "main" in
  let blk = {
        svar=C.makeVarinfo true funname (TFun(C.voidType,None,false,[]));
        sformals=[];
        slocals=locals;
        smaxid=100000;
        sbody={ battrs=[]; bstmts=stmts@[assert_false] };
        smaxstmtid=None;
        sallstmts=[]
  } in
  let gls = collect_globals allglobals in
  let oc = Pervasives.open_out fn in
  Pervasives.output_string oc header;
  C.lineDirectiveStyle := None; (*Some (LineCommentSparse);*)
  C.dumpGlobal C.defaultCilPrinter oc (GText("// " ^ note));
  List.iter (fun g -> C.dumpGlobal C.defaultCilPrinter oc g) gls;
  C.dumpGlobal C.defaultCilPrinter oc (GText("// " ^ note));
  C.dumpGlobal C.defaultCilPrinter oc (GFun(blk,Utils.dumLoc)); 
  close_out oc;
  (* create the reachability property file in /tmp *)
  let ocr = Pervasives.open_out "/tmp/reach.prp" in
  Pervasives.output_string ocr "CHECK( init(main()), LTL(G ! call(__VERIFIER_error())) )";
  close_out ocr;
  if debug then print_endline ("generated feasbility task: "^fn);
  let cmd = ("./Ultimate.py --spec /tmp/reach.prp --architecture 64bit --file " ^ fn) in
  match run cmd fn with 
  | Some(STrue) -> save_dig fn STrue; false
  | Some(SFalse) -> save_dig fn SFalse; true
  | Some(SUnknown) -> failwith "feasbility was unknown"
  | None -> failwith "feasbility didn't run"
