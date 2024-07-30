(*Tree Data Structure *)

(*Leaf represents the current game st *)
type 'a t =
  | Leaf
  | Node of ('a * 'a t list)

(**Represents a leaf node*)
let empty = Leaf

let insert x = function
  | Leaf -> Node (x, [])
  | Node (root, children) -> Node (root, Node (x, []) :: children)

let rec mem x = function
  | Leaf -> false
  | Node (root, children) ->
      if root = x then true else List.exists (mem x) children
(* let rec dfs f = function | Leaf -> () | Node (root, children) -> f root;
   List.iter (dfs f) children *)
