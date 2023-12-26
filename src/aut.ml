open Cil 
open Layer

let debug = false 

module C = Cil

type q_aut = C.exp

type trans =
  | Trans of int * q_aut * layer * q_aut
  | ROTrans of int * q_aut * layer


(* ******************************************************************* *)
(* printers *)
(* ******************************************************************* *)

let pp_q q =
   Utils.pp_exp q

let q_ht = Hashtbl.create 8
let qID = ref 0
let id_of_q q : string = 
  try 
     (string_of_int (Hashtbl.find q_ht q))
   with Not_found ->
      qID := !qID + 1;
      Hashtbl.replace q_ht q !qID;
      string_of_int !qID

let pp_trans t = match t with 
  | Trans(id,q,l,q') -> "TRANS#"^(string_of_int id)^": " ^ (pp_q q) ^ "--" ^ (Layer.pp_short l) ^ "-->" ^ (pp_q q')
  | ROTrans(id,q,l)  -> "TRANS#"^(string_of_int id)^": " ^ (pp_q q) ^ "--" ^ (Layer.pp_short l) ^ "-->(selfloop)"

(* TODO: factor q/q' out of Layer *)
let pp_trans_tex tr sO : string = 
   match tr with 
   | Trans(id,q,Layer(i,wrp,rdrs),q') -> 
      String.concat "\n" ([
      "\\noindent\\begin{tabular}{|p{5.0in}|}";
      "\\hline\n{\\bf Transition "^(string_of_int i)^" with layer:}\\\\\n\\hline";
      "\\begin{minipage}{4.8in}";
      "\\[\\begin{array}{l}";
      "   \\mathfrak{q}(\\textrm{\\lstinline|"^(Utils.texesc (pp_q q))^"|})\\\\";
      "   \\xrightarrow{\\lambda}\\mathfrak{q}'(\\texttt{\\lstinline|"^(Utils.texesc (pp_q q'))^"|})\\\\";
      "\\end{array}\\]";
      "where the $\lambda$'s non-local $k_w$ has ARW: \\lstinline|"^(Utils.texesc (Path.wr_name wrp))^"|\\\\ ";
      "and $supp(\lambda)$ is defined below";
      "\\end{minipage}\\\\";
      "\\hline";
      "\\end{tabular}\n\n";
      "Support($\\lambda$) $k_{l1} \\cdot k_w \\cdot k_{l2}$ involve the following "^(string_of_int (List.length rdrs))^" readers:"]
      @ (if (List.length rdrs) == 0 then [] else
          (["\\begin{itemize}"] @ (List.map (fun x -> pp_itl_tex x sO) rdrs) @ ["\\end{itemize}"])))
   | ROTrans(id,q,ROLayer(i,rdrs)) -> 
      String.concat "\n" ([
      "\\noindent\\begin{tabular}{|p{5.0in}|}";
      "\\hline\n{\\bf Transition "^(string_of_int i)^" with local (read only) layer}\\\\\n\\hline";
      "\\begin{minipage}{4.8in}";
      "\\[\\begin{array}{l}";
      "   \\mathfrak{q}(\\texttt{\\lstinline|"^(Utils.texesc (pp_q q))^"|})\\\\";
      "   \\xrightarrow{\\textrm{local } \\lambda} \\mathfrak{q}(\\texttt{\\lstinline|"^(Utils.texesc (pp_q q))^"|})\\\\";
      "\\end{array}\\]";
      "where local paths are defined below.";
      "\\end{minipage}\\\\";
      "\\hline";
      "\\end{tabular}\n\n";
      "Support($\\lambda$) involves "^(string_of_int (List.length rdrs))^" readers:";
      ] @ (if (List.length rdrs) == 0 then [] else
          (["\\begin{itemize}"] @ (List.map (fun x -> pp_reader_tex x sO) rdrs) @ ["\\end{itemize}"])))


(* ******************************************************************* *)
(* Loading automaton states from source code *)
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
                              (String.concat " /\\ " (List.map (fun a -> Utils.pp_exp a) [a1;a2]));
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
(* Configuration. TODO: Move this to a config file or cmdline arg      *)
(* ******************************************************************* *)

let get_state_exprs_tmp allglobals fn : C.exp list =
   match fn with 
   | "bench/evenodd.c" -> [
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
   ]
   | "bench/counter.c" -> [
        BinOp(Eq,Lval(C.var (C.makeVarinfo true "counter" C.intType)),C.zero,C.intType);
        BinOp(Gt,Lval(C.var (C.makeVarinfo true "counter" C.intType)),C.zero,C.intType);
   ]
   | "bench/descriptor.c" -> 
     let locked = Utils.mkBO Eq "lock" C.zero C.intType in
     let descNull = Utils.mkBO Eq "descriptor" C.zero C.voidPtrType in 
   [
      Utils.mkLand locked descNull;
      Utils.mkLand locked (Utils.neg descNull);
      Utils.mkLand (Utils.neg locked) descNull;
      Utils.mkLand (Utils.neg locked) (Utils.neg descNull);
   ]
   | "bench/listset-original.c" 
   | "bench/listset3.c"
   | "bench/listset.c" -> 
        let states = load_from_fullstates allglobals in
        List.iter (fun s -> Printf.printf "Loaded state from __fullstates: %s\n" (Utils.pp_exp s)) states;
        states
   | "bench/msq.c" 
   | "bench/treiber.c" ->
        let states = load_from_fun allglobals in
        List.iter (fun s -> Printf.printf "Loaded state: %s\n" (Utils.pp_exp s)) states;
        states
   | "bench/listset-quantified.c" ->
        (* let states = load_from_fullstates allglobals in *)
        let states = load_states_from_choice allglobals in 
        List.iter (fun s -> Printf.printf "Loaded state from CHOICE: %s\n" (Utils.pp_exp s)) states;
        states
   | "bench/testset-counter.c" -> 
      let locked = Utils.mkBO Eq "lock" C.zero C.intType in
      [
         BinOp(Eq,Lval(C.var (C.makeVarinfo true "counter" C.intType)),C.zero,C.intType);
         BinOp(Gt,Lval(C.var (C.makeVarinfo true "counter" C.intType)),C.zero,C.intType);
         locked;
         (Utils.neg locked);
      ]
   | _ -> failwith "get_state_exprs_tmp not defined"

let get_init_stmts fn : C.stmt list =  
   match fn with 
   | "bench/evenodd.c" -> [Utils.call_nd_var "counter"]
   | "bench/counter.c" -> [Utils.call_nd_var "counter"]
   | "bench/descriptor.c" -> [Utils.call_nd_var "lock";Utils.call_nd_var "descriptor"]
   | "bench/msq.c" -> [Utils.call_nd_var "Q.head";Utils.call_nd_var "Q.tail";
                        Utils.call_nd_var "Q.head->next";Utils.call_nd_var "Q.tail->next"]
   | "bench/treiber.c" -> [Utils.call_nd_var "top"]
   | "bench/listset-original.c"
   | "bench/listset3.c"   
   | "bench/listset.c"
   | "bench/listset-quantified.c" -> 
        (List.map Utils.call_nd_var [
             "reader_x";"x";"reader_y";"y";"reader_k";"k";
             "reader_x->del";"x->del";
             "reader_x->key";"x->key";
             "reader_y->key";"y->key";
             "reader_x->next";"x->next";
           ])
   | "bench/testset-counter.c" -> [Utils.call_nd_var "counter"; Utils.call_nd_var "lock"]
   | _ -> failwith "get_init_stmts not defined"
  

(* ******************************************************************* *)
(* making and saving transitions  *)
(* ******************************************************************* *)

let transCount = ref 0
let nextTrans () : int = transCount := !transCount + 1; !transCount
let getTransCount () = !transCount

let labeled_arc q l q' isRO : string =
  let edgeid = String.concat "i" ["id";(id_of_q q);(Layer.id_of_l l);(id_of_q q')] in
  let color = (if isRO then "" else "color=lightblue,") in 
  let ro = (if isRO then "RO " else "") in 
  Printf.sprintf " q%s -> %s [dir=none];\n %s -> q%s\n %s [%sstyle=filled,label=\"%sLayer %s\"];\n"
     (id_of_q q) edgeid edgeid (id_of_q q') edgeid color ro (Layer.id_of_l l)

let recordTrans oc ocDOT q l q' shortOutput : unit = 
  let tr = Trans(nextTrans(),q,l,q') in
  let arc = labeled_arc q l q' false in
  output_string ocDOT arc;
  output_string oc ((pp_trans_tex tr shortOutput)^"\n\n")

let recordROTrans oc ocDOT q rolay shortOutput : unit = 
  let tr = ROTrans(nextTrans(),q,rolay) in
(*  Printf.fprintf ocDOT " q%s -> q%s  [style=bold,decorate=true,label=<\n<B>ROLayer %s</B>\n>];\n"
     (id_of_q q) (id_of_q q) (Layer.id_of_l rolay);*)
  let arc = labeled_arc q rolay q true in 
  output_string ocDOT arc;
  output_string oc ((pp_trans_tex tr shortOutput)^"\n\n")

let dot_begin ocDOT : unit = 
  output_string ocDOT "digraph {\n"

let dot_q q = 
   let s = Utils.pp_exp q in
   Utils.dotesc s

let dot_end ocDOT : unit = 
  Hashtbl.iter (fun q id -> 
    let s = Printf.sprintf "  q%s [shape=box,style=rounded,label=\"%s\"];\n" (id_of_q q) (dot_q q) in
    output_string ocDOT s
  ) q_ht;
  output_string ocDOT "}\n"