module C = Cil
open Cil 
let debug = false 

(* ******************************************************************* *)
(* Every benchmark is configured (see below) as one of these "bench" types *)
(* ******************************************************************* *)

type bench = { 
  name: string;
  filename: string; 
  init_stmts:C.stmt list;
  make_state_exprs: (C.global list -> C.exp list);
  expected_layers: int;
  expected_transitions: int}


(* ******************************************************************* *)
(* Helper functions - Loading automaton states from source code *)
(* ******************************************************************* *)

let get_state_exprs allglobals : C.exp list =
  List.iter (fun g -> () (* TODO *) ) allglobals;
  [C.one]

let states_of_aps (aps:C.exp list) : C.exp list = 
  let rec _help (aps:C.exp list) : C.exp list =
     match aps with 
     | [ap] -> [ap;(Utils.neg ap)]
     | ap :: rest -> 
        let combos = _help rest in 
        ((List.map (fun c -> Utils.mkLand ap c) combos)
        @
        (List.map (fun c -> Utils.mkLand (Utils.neg ap) c) combos))
  in
  _help aps

let load_from_fun allglobals : C.exp list =
  try 
     let fd = Utils.find_fd allglobals "__states" in 
     let aps = List.flatten (List.map (fun s -> 
           print_endline (Utils.pp_stmt s);
           match s.skind with 
           | Instr(il) ->
                 List.map (fun i -> match i with 
                    | Set(lval,e,_) ->
                       if debug then Printf.printf "  found state %s\n" (Utils.pp_exp e);
                       e
                    (* __state = e;*)
                 ) il
           | _ -> []
     ) fd.sbody.bstmts) in
     states_of_aps aps
  with Not_found -> failwith "load_from_fun"

let load_states_from_choice allglobals : C.exp list = 
  let fd = Utils.find_fd allglobals "__choice" in 
  Utils.all_combinations (
     List.flatten (List.map (fun s -> 
           match s.skind with 
           | Instr(il) ->
                 List.map (fun i -> match i with 
                    | Call(_,fn,[a1;a2],_) -> 
                       Printf.printf "  found CHOICE (%s or %s)\n" (Utils.pp_exp a1) (Utils.pp_exp a2); 
                             (* (String.concat " /\\ " (List.map (fun a -> Utils.pp_exp a) [a1;a2])); *)
                       (a1,a2)
                    (* __state = e;*)
                 ) il
           | Return(_,_) -> []
           | _ -> failwith ("__choice not a Call instr:"^(Utils.pp_stmt s))
     ) fd.sbody.bstmts)
  )


let load_from_fullstates allglobals : C.exp list =
  try 
     let fd = Utils.find_fd allglobals "__fullstates" in 
     List.flatten (List.map (fun s -> 
           print_endline (Utils.pp_stmt s);
           match s.skind with 
           | Instr(il) ->
                 List.map (fun i -> match i with 
                    | Call(_,fn,args,_) ->
                       if debug then Printf.printf "  found state %s\n" 
                             (String.concat " /\\ " (List.map (fun a -> Utils.pp_exp a) args));
                       List.fold_left (fun e acc -> 
                          BinOp(LAnd,e,acc, C.intType)
                       ) C.one args
                    | _ -> failwith ("dunno i:"^(Utils.pp_instr i))
                    (* __state = e;*)
                 ) il
           | _ -> []
     ) fd.sbody.bstmts)
  with Not_found -> failwith "load_from_fullstates2"

(* ******************************************************************* *)
(* Configuration of Benchmarks *)
(* ******************************************************************* *)

let get_bench s =
  match s with 
  (**** Configuration for: Counter ****)
  | "counter" -> {
      name="counter";
      filename="../bench/counter.c";
      init_stmts=[Utils.call_nd_var "counter"];
      make_state_exprs=(fun allglobals -> [
          BinOp(Eq,Lval(C.var (C.makeVarinfo true "counter" C.intType)),C.zero,C.intType);
          BinOp(Gt,Lval(C.var (C.makeVarinfo true "counter" C.intType)),C.zero,C.intType);
      ]);
      expected_layers=5;
      expected_transitions=6;
  }
  (**** Configuration for: Even/Odd ****)
  | "evenodd" -> {
      name="evenodd";
      filename="../bench/evenodd.c";
      init_stmts=[Utils.call_nd_var "counter"];
      make_state_exprs=(fun allglobals -> [
        BinOp(Eq,
        BinOp(Mod,Lval(C.var (C.makeVarinfo true "counter" C.intType)),C.integer 2, C.intType),
        C.zero,
        C.intType
     );
     BinOp(Eq,
        BinOp(Mod,Lval(C.var (C.makeVarinfo true "counter" C.intType)),C.integer 2, C.intType),
        C.one,
        C.intType
     );
    ]);
      expected_layers=3;
      expected_transitions=6;
  }
  (**** Configuration for: Descriptor ****)
  | "descriptor" -> {
    name="descriptor";
    filename="../bench/descriptor.c";
    init_stmts=[Utils.call_nd_var "lock";Utils.call_nd_var "descriptor"];
    make_state_exprs=(fun allglobals -> 
      let locked = Utils.mkBO Eq "lock" C.zero C.intType in
      let descNull = Utils.mkBO Eq "descriptor" C.zero C.voidPtrType in 
    [
       Utils.mkLand locked descNull;
       Utils.mkLand locked (Utils.neg descNull);
       Utils.mkLand (Utils.neg locked) descNull;
       Utils.mkLand (Utils.neg locked) (Utils.neg descNull);
    ]);
    expected_layers=5;
    expected_transitions=6;
  }
  (**** Configuration for: Treiber's Stack ****)
  | "treiber" -> {
    name="treiber";
    filename="../bench/treiber.c";
    init_stmts=[Utils.call_nd_var "top"];
    make_state_exprs=(fun allglobals -> load_from_fun allglobals);
      (* let states = load_from_fun allglobals in
      List.iter (fun s -> Printf.printf "Loaded state: %s\n" (Utils.pp_exp s)) states;
      states *)
    expected_layers=4;
    expected_transitions=6;
  }
  (**** Configuration for: Michael/Scott Queue ****)
  | "msq" -> {
      name="msq";
      filename="../bench/msq.c";
      init_stmts= [Utils.call_nd_var "Q.head";Utils.call_nd_var "Q.tail";
                   Utils.call_nd_var "Q.head->next";Utils.call_nd_var "Q.tail->next"];
      make_state_exprs=(fun allglobals -> load_from_fun allglobals);
      expected_layers=7;
      expected_transitions=17;
    }  
  (**** Configuration for: List Set ****)
  | "listset" -> {
      name="listset";
      filename="../bench/listset.c";
      init_stmts=(List.map Utils.call_nd_var [
        "reader_x";"x";"reader_y";"y";"reader_k";"k";
        "reader_x->del";"x->del";
        "reader_x->key";"x->key";
        "reader_y->key";"y->key";
        "reader_x->next";"x->next";
      ]);
      make_state_exprs=(fun allglobals -> load_from_fullstates allglobals);
      expected_layers=8;
      expected_transitions=77;
    }  
  | _ -> failwith ("get_bench did not find benchmark called "^s)
