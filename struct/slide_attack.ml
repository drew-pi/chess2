open Unsigned.UInt64
open Macros

open Bishop_attack
open Rook_attack





(* let indexes = 4096
let bishop_attacks = Array.make indexes zero
let rook_attacks = Array.make indexes zero *)




let hash_function magic blockers index_bits = to_int (shift_right (mul blockers magic) (64 - index_bits))
(** Perfect hash function for bishop and rook attack. [has_function magic blockers index_bits] converts the number to a [2 ^ index_bits] length array index  *)
 
let access_attack magic blockers index_bits attack_table =
  let hash = hash_function magic blockers index_bits in
  attack_table.(hash)

let set_attack magic blocker index_bits attack attack_table = 
  let hash = hash_function magic blocker index_bits in
  (* let () = print_endline (string_of_int hash) in *)
  if equal attack_table.(hash) zero then let () = attack_table.(hash) <- attack in true else false




let set_blocker_occupancy index mask = 
  let bitcount = popcount mask in
  if index >= pow 2 bitcount then failwith "Index too high" else
  (* let () = print_int bitcount in *)
  let blockers = zero in

  (* Loop over all of the bits in the attack map *)
  let rec fill_blocker_map blockers attack_map count = 
    if count > bitcount then blockers else
      let coord = bitscan attack_map in
      let attack_map = pop_bit attack_map coord in

      (* This is the most important part 
        - This part makes sure that each indexes corresponds to a unique blocker setup 
        - If the index is a number greater than the number of possible blocker positions for that given piece, it will return bits in random spots
      *)
      if equal (logand (of_int index) (shift_left one count)) zero then
        fill_blocker_map blockers attack_map (count + 1)
      else 
      let blockers = logor blockers (shift_left one coord) in
      fill_blocker_map blockers attack_map (count + 1)
(* This line protects the program from putting bits in random spots on the board, restricts it to only those that are legal attacks *)
in logand (fill_blocker_map blockers mask 0) mask
(* in fill_blocker_map blockers attacks 0 *)


(* [fill_blocker_array max_index block_mask] creates a uint64 array of blocker positions that is len [max_index], doing logor with all the elements of the resulting array is just [block_mask] *)
(* let fill_blocker_array max_index block_mask =
  let rec looper attack count attack_mask_array =
    if count > (max_index - 1) then attack_mask_array else
      let () = attack_mask_array.(count) <- set_blocker_occupancy count attack in looper attack (count+1) attack_mask_array 
    in
  looper block_mask 0 (Array.make max_index zero) *)



(* Only used for generating the 64 length arrays to precalculate number of possible blocker positions *)
(* let rec iterf rank file f = 
  if file >= 8 then () else let _ = Printf.printf "%d; " (popcount (f (rank, file))) in iterf rank (file + 1) f

let rec iterr rank f = 
  if rank >= 8 then () else let () = iterf rank 0 f in let _ = Printf.printf " \n" in iterr (rank + 1) f

(** Generate possible blocker masks array for relevent squares for rooks and bishops in string form *)
let generate_relevent_possibiliites () = 
  let () = iterr 0 create_bishop_mask in
  let () = print_newline () in
  let () = iterr 0 create_rook_mask in
  () *)



let test_magic magic rel_bits blockers = 
  (* determines how many possible variations of blockers exists for this blocker formation (i.e piece and coordinate)*)
  let max_occupancy = pow 2 rel_bits in

  (* make sure that this is correct number of posibilities*)
  if rel_bits <> (popcount blockers) then failwith "uneven number of bits" else

  (* create teh attack table for this blocker setup and fill it with zeros*)
  let attack_hash_table = Array.make max_occupancy zero in

  let rec block_mask_looper count = 
    if count > (max_occupancy-1) then true else 

      (* Have to create a new blocker occupancy with each progressive index *)
      let blocker_occupancy = set_blocker_occupancy count blockers in

      (* Check if this magic number can be used for perfect hashing *)
      if (set_attack magic blocker_occupancy rel_bits one attack_hash_table) 
        then 
          block_mask_looper (count+1) 
        else 
          false

    in 
  block_mask_looper 0


let rec magic_repeater count rel_bits blocker_mask=  
  let max = 100_000_000 in
  if count > max then let () = Printf.printf "Something went wrong could not generate magic number" in zero 
  else
    let magic_candidate = logand (logand (random_uint64 ()) (random_uint64 ()))  (random_uint64 ()) in
    if (test_magic magic_candidate rel_bits blocker_mask) then 
      let () = flush stdout in
      magic_candidate else
      
      (* let () = print_endline (string_of_int count) in
      let () = print_endline (string_of_int rel_bits) in *)
    magic_repeater (count+1) rel_bits blocker_mask




let generate_magic_number coord piece = 

  let (r, f) = from_coord_rf coord in

  (* The blocker mask array has all possible moves *)
  let (blocker_mask, rel_bits) = 
    match piece with 
    | Bishop -> create_bishop_mask (r, f), bishops_relevent_possible_moves.(coord) 
    | Rook -> create_rook_mask (r, f), rooks_relevent_possible_moves.(coord)
    | _ -> failwith "Expected Bishop or Rook"
  in 
  magic_repeater 0 rel_bits blocker_mask


(** Generate all magic numbers for both rooks and bishops and print to terminal for hardcoding*)
(* let generate_magics () = 
  
  let magic_looper piece =
    (* if coord > 63 then () 
      else
      let () = Printf.printf "  of_string (\"%s\");\n" (to_string (generate_magic_number coord piece)) in 
      magic_looper (coord + 1) piece *)
    
    for i = 0 to 63 do 
      Printf.printf "  of_string (\"%s\");\n" (to_string (generate_magic_number i piece));
    done

  in 
  let () = Printf.printf "let rook_magics = [|\n" in
  let () = magic_looper Rook in
  let () = Printf.printf "|]\n\n" in


  let () = Printf.printf "let bishop_magics =[|\n" in
  let () = magic_looper Bishop in
  let () = Printf.printf "|]\n\n" in
  () *)



  
let generate_lookup_for_coord magic rel_bits blockers move_generator coord attack_table = 
  let max_occupancy = pow 2 rel_bits in

  if rel_bits <> (popcount blockers) then failwith "uneven number of bits" else
  let attack_hash_table = Array.make max_occupancy zero in

  let rec setter count = 
    if count > (max_occupancy-1) then () else 
      (* Have to create a new blocker occupancy with the coord*)
      let blocker_occupancy = set_blocker_occupancy count blockers in
      let attack = move_generator coord blocker_occupancy in

      (* let () = Printf.printf "This is what was at the index before it is taken by the attack ( %s )\n\n" (to_string (access_attack magic blocker rel_bits attack_table.(coord))) in *)
      if (set_attack magic blocker_occupancy rel_bits attack attack_hash_table) 
        then 
          setter (count+1) 
        else 
          let () = Printf.printf "This ( %s ) magic number does not work somehow for the blocker ( %s ) and the attack ( %s ). The previous value that was already place there was ( %s ), for the coord (%d) and for the index ( %d ) for the hash ( %d ) + ( %d ) (magic blocker rel_bits: hash_function (of_string \"%s\") (of_string \"%s\") %d )" (to_string magic) (to_string blocker_occupancy) (to_string attack) (to_string (access_attack magic blocker_occupancy rel_bits attack_hash_table)) coord count (hash_function magic blocker_occupancy rel_bits) (to_int (shift_right (mul magic blocker_occupancy) (64 - rel_bits))) (to_string magic) (to_string blocker_occupancy) (rel_bits) in
          failwith "This did not work"
    in 
  let () = setter 0 in
  Array.set attack_table coord attack_hash_table



    
let initialize_slide_attacks () = 
  let bishop_attacks = Array.make 64 (Array.make 1 zero) in
  let rook_attacks = Array.make 64 (Array.make 1 zero) in

  let () = 
  for i = 0 to 63 do
    (* Generate bishop table *)
    (* let (r, f) = from_coord_rf i in *)
    let blocker_mask = create_bishop_mask (from_coord_rf i) in
    let rel_bits = bishops_relevent_possible_moves.(i) in
    let magic = bishop_magics.(i) in 
    
    generate_lookup_for_coord magic rel_bits blocker_mask generate_bishop_attack i bishop_attacks
	done

in

  let () = 
  for i = 0 to 63 do
    (* Generate bishop table *)
    let blocker_mask = create_rook_mask (from_coord_rf i) in
    let rel_bits = rooks_relevent_possible_moves.(i) in
    let magic = rook_magics.(i) in 
    
    generate_lookup_for_coord magic rel_bits blocker_mask generate_rook_attack i rook_attacks
	done
  
in

  (bishop_attacks, rook_attacks)












