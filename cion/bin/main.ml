open Cfg

module F = Frontc
module C = Cil

let rec allFds gls = 
    match gls with 
    | [] -> []
    | C.GFun(fd,loc) :: rest ->
        if String.equal fd.svar.vname "__states" then allFds rest
        else if String.equal fd.svar.vname "__fullstates" then allFds rest
        else if String.equal fd.svar.vname "__choice" then allFds rest
        else (fd :: (allFds rest))
    | _ :: rest -> allFds rest

let argExists str : bool = 
  let rec _help i =
    if i >= Array.length Sys.argv then false 
    else if String.equal Sys.argv.(i) str then true
    else _help (i+1)
  in _help 0

let _ =
  if Array.length Sys.argv <= 1 then 
    failwith ("usage: "^Sys.argv.(0)^" <BenchmarkName> [--disablePostCheck] [--shortOutput]")
  else
  let bm = Benchmarks.get_bench Sys.argv.(1) in
  let filename = bm.filename in
  let c = F.parse filename () in
  computeFileCFG c;
  let fds = allFds c.globals in
  print_endline "+ computing paths:";
  let (roPaths,wrPaths) = Path.findAll fds in
  C.lineDirectiveStyle := None; (*Some (LineCommentSparse);*)
  print_endline "\n+ found all paths. here they are:";
  print_endline (Path.pp_paths roPaths "\n---\n");
  print_endline (Path.pp_paths wrPaths "\n---\n");
  let qs = bm.make_state_exprs c.globals in
  let inits : C.stmt list = bm.init_stmts in
  let t0 = Unix.gettimeofday () in (*Sys.time() in*)
  let disablePostCheck = argExists "--disablePostCheck" in
  let shortOutput = argExists "--shortOutput" in

  (* Run the algorithm, wrapping it with statistics tracking *)
  let stats_in = Statistics.mkProblem filename roPaths wrPaths qs in
  let stats_out = Alg.algorithm roPaths wrPaths qs c.globals inits (filename^".tex") (filename^".dot") disablePostCheck shortOutput stats_in in
  print_endline (Statistics.pp stats_out (Unix.gettimeofday () -. t0));
  Solver.dig_persist ();
  Statistics.check stats_out bm.expected_transitions bm.expected_layers;
  print_endline ("+ complete. output: "^filename^".tex");

