open Printf

type card = int * int
type hand = card * card list

type step  =
  | Play of int * card
  | Discard of int * card
  | ColorClue of int
  | RankClue of int 

type record = hand * hand list * step list

let null_card = -1,-1

(* ******* Printing a record ****)
let print_card card = 
  if card = null_card then ""
  else let col, rank = card in
    let scol = match col with
      | 0 -> "R"
      | 1 -> "Y"
      | 2 -> "V"
      | 3 -> "B"
      | 4 -> "W"
      | 5 -> "M"
      | a -> string_of_int a in
    scol ^ string_of_int (rank+1)

let rec print_list sep pa = function
  | [] -> ""
  | [a] -> (pa a)
  | a::la -> (pa a) ^ sep ^ (print_list sep pa la)
  
let rec print_hands ind (hands : hand list) = match hands with
  | [] -> ""
  | (ch,ct)::ht -> (
    (sprintf "Player %d : " ind) ^
    (print_list ", " print_card (ch::ct)) ^ "\n" ^
    print_hands (ind + 1) ht )

let print_step step =
  let s, i, c = match step with
    | Play (i, c) -> "Play", i, c
    | Discard (i, c) -> "Discard", i, c
    | ColorClue i -> "ColorClue", i, null_card
    | RankClue i -> "RankClue", i, null_card
  in  s ^ " " ^ string_of_int i ^ " " ^ print_card c

let print (hh, ht, sl) =
  print_hands 1 (hh::ht) ^ print_list "\n" print_step sl
