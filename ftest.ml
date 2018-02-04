open Graph
open Fordfulk

let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and _source = Sys.argv.(2)
  and _sink = Sys.argv.(3)
  and outfile = Sys.argv.(4) in

  let graph = Gfile.from_file infile in

  (* Rewrite the graph that has been read. *)
  let g = map graph (fun x -> x) (fun x -> int_of_string x) in
  let () = ford g _source _sink in
  let g3 = map g (fun x -> x) (fun x -> string_of_int x) in
  let () = Gfile.export outfile g3 in ()
