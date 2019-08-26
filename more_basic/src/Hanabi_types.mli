type card = int * int (* color, rank *)
type hand = card * card list

type step  =
  | Play of int * card (* result would be error or okay, but that can be computed later *)
  | Discard of int * card(* card is the card received by the player, game ends when there are no more cards to deal*)
  | ColorClue of int (* TODO only works for 2 players *)
  | RankClue of int 

type record = hand * hand list * step list

val null_card : card

val print : record -> string

val print_step : step -> string
