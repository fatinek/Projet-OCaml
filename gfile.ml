open Graph

type path = string

(* Format of files: lines of the form 
 *
 *  v label id         (vertex with the given (string) label, identifier is id.)
 *  e label id1 id2    (edge with the given (string) label. Goes from vertex id1 to vertex id2.)
 *
 *)

let write_file path graph =

  let outfile = open_out path in
  let out fmt = Printf.fprintf outfile fmt in

  out "=== Graph file ===\n\n" ;

  (* Vertices *)
  v_iter graph (fun vi -> out "v \"%s\" %s\n" vi.label vi.id) ;
  out "\n" ;

  (* Edges *)
  v_iter graph (fun vi -> List.iter (fun (label, id2) -> out "e \"%s\" %s %s\n" label vi.id id2) vi.outedges) ;
  
  out "\n=== End of graph ===\n" ;
  
  close_out outfile ;
  ()

(* Reads a line with a vertex. *)
let read_vertex graph line =
  try Scanf.sscanf line "v \"%s@\" %s" (fun label id -> add_vertex graph label id)
  with e -> Printf.printf "Cannot read vertex in line - %s:\n%s\n" (Printexc.to_string e) line

(* Reads a line with an edge. *)
let read_edge graph line =
  try Scanf.sscanf line "e \"%s@\" %s %s" (fun label id1 id2 -> add_edge graph id1 id2 label)
  with e -> Printf.printf "Cannot read edge in line - %s:\n%s\n" (Printexc.to_string e) line

let from_file path =

  let infile = open_in path in

  (* Create new graph *)
  let graph = new_graph () in

  (* Populate it *)
  let rec loop () =
    try
      let line = input_line infile in
      let () =
        if line = "" then ()
        else match line.[0] with
          | 'v' -> read_vertex graph line
          | 'e' -> read_edge graph line
          | _ -> ()
      in                 
      loop ()        
    with End_of_file -> ()
  in

  loop () ;
  
  close_in infile ;
  graph

let export path graph =
	(* Ouverture du fichier*)
	let outfile = open_out path in
	(*Ecriture*)
	let out fmt = Printf.fprintf outfile fmt in
	(*Syntaxe de graphviz*)
	out "digraph finite_state_machine {\n
    rankdir=LR;\n
    size=\"8,5\"\n
    node [shape = circle];\n" ;
	(*Ecriture des arrêtes*)
	v_iter graph (fun noeud_source -> List.iter (fun (label, noeud_dest) -> out "%s -> %s [ label = \"%s\" ];\n" noeud_source.id noeud_dest label) noeud_source.outedges);
out "}";
	close_out outfile ;
	()
			 





