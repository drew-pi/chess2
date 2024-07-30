open Unsigned.UInt64


val create_king_attacks : int -> t
(** [create_king_attacks coord] generates a king attack for every coordinate *)

val init_king_attacks : t array -> t array
(** [init_king_attacks attack_table] initializes all king attacks into the given [attack_table] *)
