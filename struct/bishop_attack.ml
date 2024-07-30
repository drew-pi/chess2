open Unsigned.UInt64
open Macros


(*
   
Hardcoded magic numbers

*)
let bishop_magics =[|
  of_string ("4656757276387000962");
  of_string ("1157427322601734272");
  of_string ("1130315205576704");
  of_string ("2319639683283353604");
  of_string ("299342049312832");
  of_string ("2306125721409421312");
  of_string ("2306969476325249025");
  of_string ("360572228381180944");
  of_string ("35201620149252");
  of_string ("4965124934144");
  of_string ("288274940736602120");
  of_string ("153197171309553932");
  of_string ("1193463007657590784");
  of_string ("5764612127373918400");
  of_string ("578836814379639808");
  of_string ("576462429623881728");
  of_string ("18577486038253824");
  of_string ("297239800279469185");
  of_string ("154249558550642704");
  of_string ("38280601668698112");
  of_string ("577023719454277696");
  of_string ("4629773096391278856");
  of_string ("306759397659904096");
  of_string ("4614681096725086720");
  of_string ("6861520566878720");
  of_string ("2742701518997700736");
  of_string ("36068380008516096");
  of_string ("18018798741291104");
  of_string ("810793073838661632");
  of_string ("1157425381804888084");
  of_string ("1148989794717697");
  of_string ("72269250380431624");
  of_string ("2311490375812187136");
  of_string ("2252933822420480");
  of_string ("1153308979393988612");
  of_string ("144689135297757313");
  of_string ("4612258331409646082");
  of_string ("342555433203990784");
  of_string ("2639391973311644160");
  of_string ("4666856703067701504");
  of_string ("1162777536502047136");
  of_string ("2313728715232684032");
  of_string ("1583847045402624");
  of_string ("6346148708685711360");
  of_string ("2260604632957952");
  of_string ("4541051893194800");
  of_string ("5766860715499586560");
  of_string ("290275645850368");
  of_string ("325407104247005248");
  of_string ("4611721761289961504");
  of_string ("4785358207254544");
  of_string ("574014351745032");
  of_string ("4716973770539012");
  of_string ("9359085992476680");
  of_string ("2311626458696777744");
  of_string ("2315490158629126432");
  of_string ("581000640253149312");
  of_string ("37422071091200");
  of_string ("151451131032634400");
  of_string ("288234774286434816");
  of_string ("8813575012864");
  of_string ("1154610644445036672");
  of_string ("1873603148460327169");
  of_string ("2603083887586377792");
|]



let bishop_mask = [|
  (of_string "18049651735527936"); 
  (of_string "70506452091904"); 
  (of_string "275415828992"); 
  (of_string "1075975168"); 
  (of_string "38021120"); 
  (of_string "8657588224"); 
  (of_string "2216338399232"); 
  (of_string "567382630219776"); 
  (of_string "9024825867763712"); 
  (of_string "18049651735527424"); 
  (of_string "70506452221952"); 
  (of_string "275449643008"); 
  (of_string "9733406720"); 
  (of_string "2216342585344"); 
  (of_string "567382630203392"); 
  (of_string "1134765260406784"); 
  (of_string "4512412933816832"); 
  (of_string "9024825867633664"); 
  (of_string "18049651768822272"); 
  (of_string "70515108615168"); 
  (of_string "2491752130560"); 
  (of_string "567383701868544"); 
  (of_string "1134765256220672"); 
  (of_string "2269530512441344"); 
  (of_string "2256206450263040"); 
  (of_string "4512412900526080"); 
  (of_string "9024834391117824"); 
  (of_string "18051867805491712"); 
  (of_string "637888545440768"); 
  (of_string "1135039602493440"); 
  (of_string "2269529440784384"); 
  (of_string "4539058881568768"); 
  (of_string "1128098963916800"); 
  (of_string "2256197927833600"); 
  (of_string "4514594912477184"); 
  (of_string "9592139778506752"); 
  (of_string "19184279556981248"); 
  (of_string "2339762086609920"); 
  (of_string "4538784537380864"); 
  (of_string "9077569074761728"); 
  (of_string "562958610993152"); 
  (of_string "1125917221986304"); 
  (of_string "2814792987328512"); 
  (of_string "5629586008178688"); 
  (of_string "11259172008099840"); 
  (of_string "22518341868716544"); 
  (of_string "9007336962655232"); 
  (of_string "18014673925310464"); 
  (of_string "2216338399232"); 
  (of_string "4432676798464"); 
  (of_string "11064376819712"); 
  (of_string "22137335185408"); 
  (of_string "44272556441600"); 
  (of_string "87995357200384"); 
  (of_string "35253226045952"); 
  (of_string "70506452091904"); 
  (of_string "567382630219776"); 
  (of_string "1134765260406784"); 
  (of_string "2832480465846272"); 
  (of_string "5667157807464448"); 
  (of_string "11333774449049600"); 
  (of_string "22526811443298304"); 
  (of_string "9024825867763712"); 
  (of_string "18049651735527936"); 
|]



let bishops_relevent_possible_moves = [|
  6; 5; 5; 5; 5; 5; 5; 6;  
  5; 5; 5; 5; 5; 5; 5; 5;  
  5; 5; 7; 7; 7; 7; 5; 5;  
  5; 5; 7; 9; 9; 7; 5; 5;  
  5; 5; 7; 9; 9; 7; 5; 5;  
  5; 5; 7; 7; 7; 7; 5; 5;  
  5; 5; 5; 5; 5; 5; 5; 5;  
  6; 5; 5; 5; 5; 5; 5; 6;  
|]



(*
Left Up - from E3

  8     0  0  0  0  0  0  0  0  
  7     0  0  0  0  0  0  0  0  
  6     0  1  0  0  0  0  0  0  
  5     0  0  1  0  0  0  0  0  
  4     0  0  0  1  0  0  0  0  
  3     0  0  0  0  0  0  0  0  
  2     0  0  0  0  0  0  0  0  
  1     0  0  0  0  0  0  0  0  

        a  b  c  d  e  f  g  h
*)

(**Left Up [bitboard rank file] where rank and file refer to the position of the bishop*)
let rec iterate_file_lu bitboard (r, f) = if r <= 1 || f <= 1 then bitboard else let move = set_bit zero (get_coord (r-1) (f-1)) in iterate_file_lu (logor bitboard move) ((r-1), (f-1))

(*
Left Down - from E5

	8     0  0  0  0  0  0  0  0  
 	7     0  0  0  0  0  0  0  0  
 	6     0  0  0  0  0  0  0  0  
 	5     0  0  0  0  0  0  0  0  
 	4     0  0  0  1  0  0  0  0  
 	3     0  0  1  0  0  0  0  0  
 	2     0  1  0  0  0  0  0  0  
 	1     0  0  0  0  0  0  0  0  
 
	      a  b  c  d  e  f  g  h
*)

(** Left Down *)
let rec iterate_file_ld bitboard (r, f) = if r > 5 || f <= 1 then bitboard else let move = set_bit zero (get_coord (r+1) (f-1)) in iterate_file_ld (logor bitboard move) ((r+1), (f-1))

(*
Right Up - from D4

  8     0  0  0  0  0  0  0  0  
  7     0  0  0  0  0  0  1  0  
  6     0  0  0  0  0  1  0  0  
  5     0  0  0  0  1  0  0  0  
  4     0  0  0  0  0  0  0  0  
  3     0  0  0  0  0  0  0  0  
  2     0  0  0  0  0  0  0  0  
  1     0  0  0  0  0  0  0  0  

        a  b  c  d  e  f  g  h
*)

(** Right Up*)
let rec iterate_file_ru bitboard (r, f) = if r <= 1 || f > 5 then bitboard else let move = set_bit zero (get_coord (r-1) (f+1)) in iterate_file_ru (logor bitboard move) ((r-1), (f+1))

(*
Right Down - from D4

	8     0  0  0  0  0  0  0  0  
 	7     0  0  0  0  0  0  0  0  
 	6     0  0  0  0  0  0  0  0  
 	5     0  0  0  0  0  0  0  0  
 	4     0  0  0  0  1  0  0  0  
 	3     0  0  0  0  0  1  0  0  
 	2     0  0  0  0  0  0  1  0  
 	1     0  0  0  0  0  0  0  0  
 
	      a  b  c  d  e  f  g  h
*)

(** Right Down *)
let rec iterate_file_rd bitboard (r, f) = if r > 5 || f > 5 then bitboard else let move = set_bit zero (get_coord (r+1) (f+1)) in iterate_file_rd (logor bitboard move) ((r+1), (f+1))

let create_bishop_mask (r, f) = 
  let lu = iterate_file_lu zero (r,f) in
  let ld = iterate_file_ld zero (r,f) in
  let ru = iterate_file_ru zero (r,f) in
  let rd = iterate_file_rd zero (r,f) in
  logor lu (logor ld (logor ru rd))


(* let init_bishop_mask () = 
  let rec init n = 
    if n > 63 then () else
    let () = Printf.printf "  (of_string \"%s\"); \n" (to_string (create_bishop_mask (from_coord_rf n))) in
    (* let () = rook_attacks.(n) <- create_rook_mask (n / 8) (n mod 8) in *)
    init (n+1)
  in
  let () = Printf.printf "let bishop_mask = [|\n" in
  let () = init 0 in
  let () = Printf.printf "|]\n\n" in 
  () *)
 
 
(* let print_bishop_mask bishop_attacks = 
  for i = 0 to 63 do
    let _ = Printf.printf " %d" i in 
    print_bitboard bishop_attacks.(i)
  done *)






(*
Left Up - from E3

  8     0  0  0  0  0  0  0  0  
  7     0  0  0  0  0  0  0  0  
  6     0  1  0  0  0  0  0  0  
  5     0  0  1  0  0  0  0  0  
  4     0  0  0  1  0  0  0  0  
  3     0  0  0  0  0  0  0  0  
  2     0  0  0  0  0  0  0  0  
  1     0  0  0  0  0  0  0  0  

        a  b  c  d  e  f  g  h
*)

(**Left Up [bitboard rank file] where rank and file refer to the position of the bishop*)
let rec gen_attack_file_lu bitboard (r, f) blocker = if r <= 0 || f <= 0 then bitboard else 
  let move = set_bit zero (get_coord (r-1) (f-1)) in 
  if (equal (logand move blocker) zero) then
		(* continue generating attack as did not hit a blocker *)
		gen_attack_file_lu (logor bitboard move) ((r-1), (f-1)) blocker
	 (* Add this move as an attack and return*)
	else logor bitboard move

  

(*
Left Down - from E5

	8     0  0  0  0  0  0  0  0  
 	7     0  0  0  0  0  0  0  0  
 	6     0  0  0  0  0  0  0  0  
 	5     0  0  0  0  0  0  0  0  
 	4     0  0  0  1  0  0  0  0  
 	3     0  0  1  0  0  0  0  0  
 	2     0  1  0  0  0  0  0  0  
 	1     0  0  0  0  0  0  0  0  
 
	      a  b  c  d  e  f  g  h
*)

(** Left Down *)
let rec gen_attack_file_ld bitboard (r, f) blocker = if r > 6 || f <= 0 then bitboard else 
  let move = set_bit zero (get_coord (r+1) (f-1)) in 
  if (equal (logand move blocker) zero) then
		(* continue generating attack as did not hit a blocker *)
		gen_attack_file_ld (logor bitboard move) ((r+1), (f-1)) blocker
	 (* Add this move as an attack and return*)
	else logor bitboard move
  

(*
Right Up - from D4

  8     0  0  0  0  0  0  0  0  
  7     0  0  0  0  0  0  1  0  
  6     0  0  0  0  0  1  0  0  
  5     0  0  0  0  1  0  0  0  
  4     0  0  0  0  0  0  0  0  
  3     0  0  0  0  0  0  0  0  
  2     0  0  0  0  0  0  0  0  
  1     0  0  0  0  0  0  0  0  

        a  b  c  d  e  f  g  h
*)

(** Right Up*)
let rec gen_attack_file_ru bitboard (r, f) blocker = if r <= 0 || f > 6 then bitboard else 
  let move = set_bit zero (get_coord (r-1) (f+1)) in 
  if (equal (logand move blocker) zero) then
		(* continue generating attack as did not hit a blocker *)
		gen_attack_file_ru (logor bitboard move) ((r-1), (f+1)) blocker
	 (* Add this move as an attack and return*)
	else logor bitboard move
  

(*
Right Down - from D4

	8     0  0  0  0  0  0  0  0  
 	7     0  0  0  0  0  0  0  0  
 	6     0  0  0  0  0  0  0  0  
 	5     0  0  0  0  0  0  0  0  
 	4     0  0  0  0  1  0  0  0  
 	3     0  0  0  0  0  1  0  0  
 	2     0  0  0  0  0  0  1  0  
 	1     0  0  0  0  0  0  0  0  
 
	      a  b  c  d  e  f  g  h
*)

(** Right Down *)
let rec gen_attack_file_rd bitboard (r, f) blocker = if r > 6 || f > 6 then bitboard else 
  let move = set_bit zero (get_coord (r+1) (f+1)) in 
  if (equal (logand move blocker) zero) then
		(* continue generating attack as did not hit a blocker *)
    gen_attack_file_rd (logor bitboard move) ((r+1), (f+1)) blocker
	 (* Add this move as an attack and return*)
	else logor bitboard move

let generate_bishop_attack coord blocker_mask = 
  let (rank,file) = from_coord_rf coord in
  let lu = gen_attack_file_lu zero (rank,file) blocker_mask in
  let ld = gen_attack_file_ld zero (rank,file) blocker_mask in
  let ru = gen_attack_file_ru zero (rank,file) blocker_mask in
  let rd = gen_attack_file_rd zero (rank,file) blocker_mask in
  logor lu (logor ld (logor ru rd))


(* let print_bishop_attacks () = 
  for i = 0 to 63 do
    let _ = Printf.printf " %d" i in 
    print_bitboard (generate_bishop_attack i zero)
  done *)