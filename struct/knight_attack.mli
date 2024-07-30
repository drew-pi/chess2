open Unsigned.UInt64


val create_knight_attacks : int -> t
(** [create_knight_attacks coord] generates a king attack for the given coordinate *)

val init_knight_attacks : t array -> t array
(** [init_knight_attacks attack_table]  initializes all knight attacks into the given [attack_table] *)