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