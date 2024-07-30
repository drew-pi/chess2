open Unsigned.UInt64

(** Magic numbers associated with the bishops coordinates - allows for perfect hashing and lookup *)
val bishop_magics : t array

(** The complete bishop mask (where the maximum number of bits is taken) for each coordinate position *)
val bishop_mask : t array 

(** Length 64 array that shows how many possible moves/blockers a bishop may have on each square*)
val bishops_relevent_possible_moves : int array

(** [create_bishop_mask (r, f)] creates the bishop blocker mask for a bishop on a given [(rank,file)] *)
val create_bishop_mask : int * int -> t

(** [generate_bishop_attack coord blocker_mask] generates a bishop attack given a blocker setup *)
val generate_bishop_attack : int -> t -> t