open Graph
val initialize_flow : ('a, 'b) graph -> (id * id, int * 'b) Hashtbl.t
val printfHashtbl : (string*string, int * int) Hashtbl.t -> unit

val find_augmenting_path : ('a, int) graph -> id -> id -> (id * id * int) list
val printfList : (string * string * int) list -> unit

(*val retireFlow :
('a * 'a, int * 'b) Hashtbl.t ->
           int -> ('a * 'a * 'c) list -> ('a * 'a, int * 'b) Hashtbl.t*)
           

  val modifierGraphe :
  ('a, int) graph -> (id * id, int * int) Hashtbl.t -> unit

  val ford : ('a, int) graph -> id -> id -> unit
