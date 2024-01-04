open Path

module C = Cil

let debug = false 

type t_in = {
    filename:string; 
    roPaths: path list;
    wrPaths: path list;
    qs: C.exp list
    }

type t_out = {
    problem:t_in;
    transitions:int;
    layers:int;
    queries:int;
}

let mkProblem fn rpaths wpaths _qs =
  { filename=fn; roPaths=rpaths; wrPaths=wpaths; qs=_qs }

let mkSolution stats_in numTrans numLayers qs =
  { problem=stats_in; transitions=numTrans; layers=numLayers; queries = qs }

let check stats_out expected_transitions expected_layers =
   let _assert trans layers = 
      if debug Printf.printf "does %d=%d?\n" stats_out.transitions trans;
      assert (stats_out.transitions == trans);
      if debug Printf.printf "does %d=%d?\n" stats_out.layers layers;
      assert (stats_out.layers == layers)
  in 
  Printf.printf "Statistics.check %s. trans=%d, layers=%d\n" stats_out.problem.filename stats_out.transitions stats_out.layers;
  _assert expected_transitions expected_layers

let pp stats_out tm = 
  print_endline "                (filename)          states RdPaths WrPaths #Trans #layers Time Queries";
  Printf.sprintf "RESULT: \\texttt{%-18s} & %4d & %4d & %4d & %4d & %4d & %5.1f & %4d\\\\\n"
     stats_out.problem.filename
     (List.length stats_out.problem.qs)
     (List.length stats_out.problem.roPaths) 
     (List.length stats_out.problem.wrPaths) 
     stats_out.transitions
     stats_out.layers
     tm 
     stats_out.queries
