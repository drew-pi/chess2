open Unsigned.UInt64
open Macros

val access_attack : t -> t -> int -> 'a array -> 'a
(** [access_attack magic blocker index_bits attack_table] accesses an index of [attack_table] to return the attack that corresponds to the [blocker] layout. Utilizes perfect hashing function with [magic] number  *)

val set_attack : t -> t -> int -> t -> t array -> bool
(** [set_attack magic blocker index_bits attack attack_table] sets an index of [attack_table] with an [attack] that corresponds to the [blocker] layout. Utilizes perfect hashing function with [magic] number  *)

val set_blocker_occupancy : int -> t -> t
(** [set_blockers index blocker_mask] creates a possible arrangement of blockers that fits the [blocker_mask]. The index is just the arrangement number and must be between [0 <= index < (2 ^ popcount blocker_mask)] 
    - at index 0, it is no blockers on the board, this should theoretically hash to 0 *)

val initialize_slide_attacks : unit -> t array array * t array array
(** Creates the lookup/hash tables for rook and bishop attacks in the form [(bishop_attack_table, rook_attack_table)]*)

val generate_magic_number : int -> pieces -> t
(** [generate_magic_number coord piece] generates a magic for the [piece] at [coord: 0 <= x <= 63] *)