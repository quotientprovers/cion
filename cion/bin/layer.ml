
module C = Cil


let layID = ref 0
let nextLayID () : int = layID := !layID + 1; !layID
let count () = !layID

type itl_rd = 
  | InterleavedReader of Path.path * int * string (* reader, index, feasibility query fn *)

type layer = 
  | Layer of int * Path.path * itl_rd list
  | ROLayer of int * (Path.path * string * string) list
  | EmptyLayer

let id_of_l l = match l with 
  | Layer(id,_,_)
  | ROLayer(id,_) -> (string_of_int id) 
  | _ -> failwith "id_of_l empty"

(* 
let layer_sig wrPath s_i_s_list  = match l with 
  | *)
let sort_itls itls = List.sort (fun i1 i2 -> 
    match (i1,i2) with 
    | (InterleavedReader(Path.ROPath(id1,_,_,_),_,_),InterleavedReader(Path.ROPath(id2,_,_,_),_,_)) -> id2 - id1
  ) itls

let layer_sig wrPath itls = 
    let s = "wr" ^ (string_of_int (Path.get_id wrPath)) ^ "-"
      ^ (String.concat "-" (List.map 
           (fun (InterleavedReader(Path.ROPath(pid,_,_,_),idx,_)) ->
               "rd" ^ (string_of_int pid) ^ ":" ^ (string_of_int idx)
      ) (sort_itls itls))) in
    print_endline ("layer_sig: "^s);
    s

let l_ht = Hashtbl.create 50

let maybeCreate wrpath itl_readers = 
   let s = layer_sig wrpath itl_readers in
   try 
       Hashtbl.find l_ht s
    with Not_found -> 
        let l = Layer(nextLayID(),wrpath,itl_readers) in
        Hashtbl.add l_ht s l;
        l

let rol_ht = Hashtbl.create 50
let rolayer_sig rdrs : string = 
   String.concat "-" 
      (List.map (fun rp -> (string_of_int (Path.get_id rp)))
                (List.sort (fun rp1 rp2 -> Path.get_id rp2 - Path.get_id rp1) rdrs))

let maybeCreateRO (enabReaders : (Path.path * string * string) list) = 
   (*if List.length enabReaders = 0 then EmptyLayer else*)
   let rdrs = List.map (fun (rpath,_,_) -> rpath) enabReaders in
   let s = rolayer_sig rdrs in
    print_endline ("rolayer_sig: "^s);
   try 
       Hashtbl.find rol_ht s
    with Not_found -> 
        let l = ROLayer(nextLayID(),enabReaders) in
        Hashtbl.add rol_ht s l;
        l

let pp_short l = "TODO:pp_short"

let pp_itl_tex itl sO : string = 
   match itl with 
   | InterleavedReader(rdpath,pos,fn) -> 
       let fname = Path.rd_name rdpath in 
       "\\item Reader \\texttt{"^(Utils.texesc fname)^"} interleaved at position "^(string_of_int pos)^". "
       ^ (if sO then "(Feasibility query: "^fn^")\n" else ("Feasibility query ("^fn^") as follows:\n\\begin{verbatim}"^(Utils.grepAfter "void main() " fn)^"\\end{verbatim}"))

let pp_itl_dot itl sO : string = 
   match itl with 
   | InterleavedReader(rdpath,pos,fn) -> 
       let fname = Path.rd_name rdpath in 
       (Utils.texesc fname)^" itl at pos "^(string_of_int pos)^". "
       ^ (if sO then "("^fn^")\n" else ("Feasibility query ("^fn^") as follows:\n\\begin{verbatim}"^(Utils.grepAfter "void main() " fn)^"\\end{verbatim}"))

let pp_reader_tex (rpath,fname,fn) sO : string = 
  "\\item Reader \\texttt{"^(Utils.texesc fname)^"} \n"
  ^ (if sO then "(Feasibility query: "^fn^")\n" else ("Feasibility query ("^fn^") as follows:\n\\begin{verbatim}"^(Utils.grepAfter "void main() " fn)^"\\end{verbatim}"))

let pp_reader_dot (rpath,fname,fn) sO : string = 
  (Utils.texesc fname)
  ^ (if sO then "("^fn^")\n" else ("Feasibility query ("^fn^") as follows:\n\\begin{verbatim}"^(Utils.grepAfter "void main() " fn)^"\\end{verbatim}"))

let lay_to_dot l : string =
  match l with 
  | Layer(lid,wrp,itls) -> 
    Printf.sprintf "  l%d [shape=rectangle,color=lightblue,style=filled,label=\"Layer %d\\l%s\\n%s\"];\n" 
        lid lid (Path.wr_name wrp)
        (String.concat "" (List.map (fun i -> pp_itl_dot i true) itls))
  | ROLayer(lid,readers) -> 
    Printf.sprintf "  l%d [shape=rectangle,label=\"ROLayer %d\\l%s\"];\n"
      lid lid
      (String.concat "\\n" (List.map (fun r -> pp_reader_dot r true) readers))

let all_lids () = 
  (Hashtbl.fold (fun k l acc -> ("l"^(id_of_l l)) :: acc) l_ht [])
  @
  (Hashtbl.fold (fun k l acc -> ("l"^(id_of_l l)) :: acc) rol_ht [])

let all_to_dot oc = 
   output_string oc "subgraph layers { \n";
   output_string oc " edge [style=invis]\n";
   Hashtbl.iter (fun k l -> output_string oc (lay_to_dot l)) l_ht;
   Hashtbl.iter (fun k l -> output_string oc (lay_to_dot l)) rol_ht;
   output_string oc (String.concat " -> " (all_lids())); 
   output_string oc ";\n label=\"Layer Definitions\";\n";
   output_string oc " color=blue;\n";
   output_string oc " rankdir=TB;\n";
   output_string oc " ranksep=0.05;\n";
   output_string oc "}\n";
