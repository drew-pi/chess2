(*Tree Data Structure (not binary, each state has several transitions)*)

(*Leaf represents the current game st *)
type 'a t =
  | Leaf
  | Node of ('a * 'a t list)

val empty : 'a t
(**Represents a leaf node*)

val insert : 'a -> 'a t -> 'a t
(**[insert x t] is t with a new node containing [x] added to its children *)

val mem : 'a -> 'a t -> bool
(**[mem x t] is true iff [x] is within [t]. *)
