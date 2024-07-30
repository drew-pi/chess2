open Unsigned.UInt64

(** [create_pawn_attacks coord side] simulates the [side] pawn attack *)
val create_pawn_attacks : int -> int -> t

val init_pawn_attacks : t array array -> t array array
(** [init_pawn_attacks attack_table] generates all of the pawn attacks(for black and white) and populates the [attack_table]*)