open Unsigned.UInt64
open Macros
let not_a_file = of_string "18374403900871474942"
let not_h_file = of_string "9187201950435737471"
let not_gh_file = of_string "4557430888798830399" 
let not_ab_file = of_string "18229723555195321596"



(*
  Knight move for position at: D4
  - each number represents the attack# in the function below

  8     0  0  0  0  0  0  0  0  
 	7     0  0  0  0  0  0  0  0  
 	6     0  0  7  0  8  0  0  0  
 	5     0  6  0  0  0  1  0  0  
 	4     0  0  0  0  0  0  0  0  
 	3     0  5  0  0  0  2  0  0  
 	2     0  0  4  0  3  0  0  0  
 	1     0  0  0  0  0  0  0  0  
 
	      a  b  c  d  e  f  g  h

 	Int representation (base 10):  5666883501293568 
*)


(** Generate knight attacks *)
let create_knight_attacks coord =
  let bitboard = set_bit zero coord in

  let attack1 = if not (equal (logand (shift_right bitboard 6) not_ab_file) zero) then shift_right bitboard 6 else zero in

  let attack2 = if not (equal (logand (shift_left bitboard 10) not_ab_file) zero) then shift_left bitboard 10 else zero in

  let attack3 = if not (equal (logand (shift_left bitboard 17) not_a_file) zero) then shift_left bitboard 17 else zero in

  let attack4 = if not (equal (logand (shift_left bitboard 15) not_h_file) zero) then shift_left bitboard 15 else zero in

  let attack5 = if not (equal (logand (shift_left bitboard 6) not_gh_file) zero) then  shift_left bitboard 6 else zero in

  let attack6 = if not (equal (logand (shift_right bitboard 10) not_gh_file) zero) then shift_right bitboard 10 else zero in

  let attack7 = if not (equal (logand (shift_right bitboard 17) not_h_file) zero) then shift_right bitboard 17 else zero in

  let attack8 = if not (equal (logand (shift_right bitboard 15) not_a_file) zero) then shift_right bitboard 15 else zero in

  logor (logor (logor (logor (logor (logor (logor attack1 attack2) attack3) attack4) attack5) attack6) attack7) attack8


let init_knight_attacks attack_table = 
  let rec init n = 
    if n < 0 then attack_table else
    let () = attack_table.(n) <- create_knight_attacks n in
    init (n-1)
  in
  init 63



