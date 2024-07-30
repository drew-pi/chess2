open Unsigned.UInt64
open Macros
let not_a_file = of_string "18374403900871474942"
let not_h_file = of_string "9187201950435737471"


(** [create_king_attacks coord side] simulates the [side] king attack *)
let create_king_attacks coord = 
  let bitboard = set_bit zero coord in

  let attack1 = if not (equal (logand (shift_right bitboard 7) not_a_file) zero) then shift_right bitboard 7 else zero in

  let attack2 = if not (equal (logand (shift_left bitboard 1) not_a_file) zero) then shift_left bitboard 1 else zero in

  let attack3 = if not (equal (logand (shift_left bitboard 9) not_a_file) zero) then shift_left bitboard 9 else zero in

  (* let attack4 = if not (equal (logand (shift_left bitboard 8) zero) zero) then shift_left bitboard 8 else zero in *)
  let attack4 = shift_left bitboard 8 in

  let attack5 = if not (equal (logand (shift_left bitboard 7) not_h_file) zero) then  shift_left bitboard 7 else zero in

  let attack6 = if not (equal (logand (shift_right bitboard 1) not_h_file) zero) then shift_right bitboard 1 else zero in

  let attack7 = if not (equal (logand (shift_right bitboard 9) not_h_file) zero) then shift_right bitboard 9 else zero in

  (* let attack8 = if not (equal (logand (shift_right bitboard 8) zero) zero) then shift_right bitboard 8 else zero in *)
  let attack8 = shift_right bitboard 8 in

  (* Combine the two attacks into one attack mask*)
  logor (logor (logor (logor (logor (logor (logor attack1 attack2) attack3) attack4) attack5) attack6) attack7) attack8


let init_king_attacks attack_table = 
   let rec init n = 
    if n < 0 then attack_table else
    let () = attack_table.(n) <- create_king_attacks n in
    init (n-1)
  in
  init 63




