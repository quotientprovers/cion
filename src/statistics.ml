open Path

module C = Cil

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

let check stats_out =
   let _assert trans layers = 
      assert (stats_out.transitions == trans);
      assert (stats_out.layers == layers)
  in 
  Printf.printf "Statistics.check %s. trans=%d, layers=%d\n" stats_out.problem.filename stats_out.transitions stats_out.layers;
   match stats_out.problem.filename with 
   | "bench/evenodd.c"    -> _assert  6 3
   | "bench/counter.c"    -> _assert  6 5
   | "bench/descriptor.c" -> _assert  6 5
   | "bench/treiber.c"    -> _assert  6 4
   | "bench/msq.c"        -> _assert 17 7
   | "bench/listset.c"    -> _assert 77 8
   | _ -> failwith ("Stats: unfamiliar with "^stats_out.problem.filename)

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
