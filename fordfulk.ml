open Graph

(* Initalise le flow pour le graphe utilisé.
	 Flow étant une Hashtable avec comme clé (noeudSource, noeudDestination) qui donnera (flowActuel,capaciteMaximum) *)
let initialize_flow graph =
	let g1 = Hashtbl.create 60 in
		v_iter graph (fun contenu -> (List.iter (fun (capacite,dest) -> (Hashtbl.add g1 (contenu.id,dest) (0,capacite))) contenu.outedges));
		g1

(* Affichage de notre HashMap *)
let printfHashtbl table =
	Hashtbl.iter (fun (idOrigine,idDestination) (flow,capacity) -> (Printf.printf "IdOrigine %s vers IdDestination %s avec le rapport flow/capacity %d/%d \n" idOrigine idDestination flow capacity)) table;
	Printf.printf "----------------------------\n";;

(*Affichage de la liste ayant un tuple à 3 arguments (NoeudSource,noeudDestination,Capacite) *)
let printfList liste =
	List.iter (fun (source,dest,capacite) -> (Printf.printf "Source %s, Destination %s, Capacite %d \n" source dest capacite)) liste;
	Printf.printf "----------------------------\n";;

(* Retrouve le minimum de la liste*)
let rec getMin liste =
	match liste with
		| [] -> 10000
		| (_,_,c)::rest -> min c (getMin rest)

(* Cherche la liste des arrêtes empruntés pour une source et une destination donnée avec une capacité supérieure à 0
	Renvois une liste avec des éléments sous la forme (noeudSourceArrete,noeudDestinationArrete,capacitéArrete)*)
		let rec find_augmenting_path graph source sink =
			match find_edge graph source sink with
				| Some 0 -> []
				| Some capacite -> [(source,sink,capacite)]
				| _ -> let rec f pote =
					match pote with
						| [] ->[]
						| (0,dest)::r -> f r
						| (cap, intermediaire)::rest ->
							match find_augmenting_path graph intermediaire sink with
							| [] -> f rest
							| x ->(source,intermediaire,cap)::x
					in
					f (find_vertex graph source).outedges


(* Augmente le flow dans la hashtable (aka la liste des flow) en ayant la capacité utilisée
	(min) et la liste des arrêtes utilisées (en commentaire : Pour les arrêtes retours)*)
let rec retireFlow fgraph min liste =
	match liste with
		| [] -> fgraph
		| (src,dest,capacite)::rest -> let (flot,cap) = Hashtbl.find fgraph (src,dest) in
			Hashtbl.replace fgraph (src,dest) (flot+min,cap);retireFlow fgraph min rest(*;
			match Hashtbl.find fgraph (dest,src) with
			| exception Not_found -> Hashtbl.add fgraph (dest,src) (min,cap); retireFlow fgraph min rest;
			| _ -> retireFlow fgraph min rest
			| (flo,cap) -> Hashtbl.replace fgraph (dest,src) (flo+min,cap); retireFlow fgraph min rest*)

	(* Modifie le graphe grâce à la hashtable des arrêtes empruntés et met à jour leur nouvelles capacité *)
	let modifierGraphe graph htable =
			v_iter graph (fun noeud_source ->
					List.iter (fun (edge,id) ->
							add_edge graph noeud_source.id id (
									match Hashtbl.find htable (noeud_source.id,id) with
										|(flow,cap)->cap-flow);(*match Hashtbl.find htable (id,noeud_source.id) with
									 			| exception Not_found -> Printf.printf ""
												| (flow,cap)-> add_edge graph id noeud_source.id flow*)
										)
										noeud_source.outedges)

(* Algorithme de Ford-Fulkerson, en commentaire quelques débugs pour la présentation *)
		let ford graph source sink =
			let flotinitial = initialize_flow graph in
			let path = ref (find_augmenting_path graph source sink) in
			let htable = ref (Hashtbl.create 60) in
			let total = ref (getMin !path) in
			while (!path != [] ) do
				printfList !path;
				htable := retireFlow flotinitial (getMin !path) !path;
				let () = modifierGraphe graph !htable in
				let bol = (!path != []) in
				Printf.printf "Total %d %B %d \n " !total bol (getMin !path);
				path := find_augmenting_path graph source sink;
				total := !total + (getMin !path);
			done
