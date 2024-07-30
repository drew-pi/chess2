open Unsigned.UInt64
open Macros

let not_a_file = of_string "18374403900871474942"
let not_h_file = of_string "9187201950435737471"


let create_pawn_attacks coord side = 
  let bitboard = set_bit zero coord in

  (* white pawns*)
  let attack = 
    if side = 0 then

    let right_attack =
    (* make sure there is no off board clipping on the a file *)
    if not (equal (logand (shift_right bitboard 7) not_a_file) zero) then 
      shift_right bitboard 7
    else 
      zero
    in 

    let left_attack = 
    (* make sure there is no off board clipping on the h file *)
    if not (equal (logand (shift_right bitboard 9) not_h_file) zero) then
      shift_right bitboard 9
    else 
      zero
    in
    (* Combine the two attacks into one attack mask *)
    logor left_attack right_attack
      
  (* black pawns*)
  else 

    let left_attack =
    (* make sure there is no off board clipping on the a file *)
    if not (equal (logand (shift_left bitboard 7) not_h_file) zero) then 
      shift_left bitboard 7
    else 
      zero
    in 
    let right_attack = 
    (* make sure there is no off board clipping on the h file *)
    if not (equal (logand (shift_left bitboard 9) not_a_file) zero) then
      shift_left bitboard 9
    else 
      zero
    in
    
    (* Combine the two attacks into one attack mask*)
    logor left_attack right_attack
  in attack


let init_pawn_attacks attack_table = 
   let rec init n = 
    if n < 0 then attack_table else
    let () = attack_table.(0).(n) <- create_pawn_attacks n 0 in
    let () = attack_table.(1).(n) <- create_pawn_attacks n 1 in
    init (n-1)
  in
  init 63






