open Unsigned.UInt64

val rook_magics : t array
(** Magic numbers associated with the rook coordinates - allows for perfect hashing and lookup *)

val rook_mask : t array
(** The complete rook mask (where the maximum number of bits is taken) for each coordinate position *)

val rooks_relevent_possible_moves : int array
(** Length 64 array that shows how many possible moves/blockers a rook may have on each square*)

val create_rook_mask : int * int -> t
(** [create_rook_mask (rank, file)] creates the rook blocker mask for a rook on a given [(rank,file)]  *)

val generate_rook_attack : int -> t -> t
(** [generate_rook_attack coord blocker_mask] generates a rook attack given a blocker setup *)

