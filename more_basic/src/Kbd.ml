open Printf

module HT = Hanabi_types

(*type action =
  | Play of int
  | Discard of int
  | RankClue of int (* TODO only valid for 2 players *)
  | ColorClue of int 
*)

type propform =
  | Atom of string
  | And of propform * propform
  | Or of propform * propform
  (* other stuff *)
(*
type ssfive =
  | Know of propform
  | Not of ssfive

type kbp =
  | Empty
  | Action of action
  | Seq of kbp * kbp
  | Ifcond of ssfive * kbp * kbp
*)
(* could rename to primitive : ie basic stuff about the world *)
(* they don't need to be independent, they aren't. *)
type tatom =
  | Cardcolor of int * int * int (* player offset color *)
  | Cardrank of int * int * int
  | Firework of int * int (* color rank *)
  | Cluetoken of int
  | Errortoken of int
  (* just got indication : player offset color/rank *)
  (* this card should be played : player offset *)

  (* could also assign three different possible value to each atom : true, false, unknown which would be base state *)
type atom =
  | Fact of tatom (* represent if we know ATOM is true, false if we just don't know *)
  | NotFact of tatom (* rpz if we know ATOM is false *)

type 'a elist = 'a * 'a list

let ehd (e, l) = e

(* [1,2,3] -> [2,3,1] *)
let rollover_e = function
  | [] -> []
  | h::t -> t @ [h]

(* 1 [2,3] -> 2 [3, 1] *)
let rollover e = function
  | [] -> e, []
  | h::t -> h, t @ [e]


let emap f e l =
  (f e, List.map f l)

module BeliefBase =
    Set.Make(struct type t = atom let compare = compare end)

module BB = BeliefBase
  (*beliefbase.find atom*)

(* add a clue token for every card inside the other's hand *)
(* for debugging purposes *)
let addo m ct = match ct with
| [(c, ct)] -> (
  let rec aux m i = function
    | [] -> m
    | h::t -> aux (BeliefBase.add (Fact (Cluetoken i)) true m) (i + 1) t in
  aux m 0 (c::ct))
| _-> m


(* Beliefbase things *)
let bb_mem m k = BeliefBase.mem k m
 
let bb_add m k = BeliefBase.add k m

(* add list of atoms *)
let bb_addl = List.fold_left bb_add

(*let rec bb_addl m = function
  | [] -> m
  | (k, v)::l -> bb_addl (bb_add m k v) l*)

(* retrieve key,values from list of keys *)
(*TODO tail recurs *) (* use BINDINGS *)
let bb_findl m relevants = List.filter (bb_mem m) relevants

let cons_cardcol a b c = Cardcolor (a,b,c)
let cons_cardrank a b c = Cardrank (a,b,c)
let cons_Fac a = Fact a
let cons_NFac a = NotFact a

(* return all atom(keys) related to a specific offset *)
let get_card_atoms_aux ply off =
  if off < 0 then []
  else
    let nb_col = 4 in  (* TODO this could be handled better? *)
    let nb_rank = 4 in
    let rec aux n consN constr acc i =
      if i >= n then acc
      else aux n consN constr (consN (constr ply off i) :: acc) (i+1) in
    let colfacts = aux nb_col cons_Fac cons_cardcol [] 0 in
    let rnkfacts = aux nb_rank cons_Fac cons_cardrank colfacts 0 in
    let colnfact = aux nb_col cons_NFac cons_cardcol rnkfacts 0 in
    aux nb_rank cons_NFac cons_cardrank colnfact 0

let get_card_atoms =
  let hash = Hashtbl.create () in
  fun ply off ->
    if Hashtbl.mem (ply, off) hash then Hashtbl.find (ply, off) hash
    else (let result = get_card_atom_aux ply off in
          Hashtbl.add (ply, off) result hash;
          result)


(*TODO tail rec *)
(* shift all key given to the i+1 offset, while keeping same truth value *)
let shift_one =
  let shift_ato x = match x with
    | Cardcolor(ply, off, col) -> Cardcolor(ply, (off+1), col)
    | Cardrank (ply, off, rank) -> Cardrank(ply, (off+1), rank)
    | _ -> x in
  function
    | Fact x -> Fact (shift_ato x)
    | NotFact x -> NotFact (shift_ato x)

let shift = List.map shift_one  

let bb_removes m l = List.fold_left BB.remove m l
(* move card from ofa to ofb, leaving null card in ofa *)
let move_card m ply ofa =
  let atoms_a = get_card_atoms ply ofa in
  let atoms_b = get_card_atoms ply (ofa+1) in
  let relevants = List.filter (bb_mem m) atoms_a in
  let m = bb_removes (bb_removes m atoms_a) atoms_b in
  let shifted = shift relevants in
  bb_addl m shifted

(* remove the card and shift all other to fill the gap, leaving nullcard in offset 0 *)
let remove_card m ply off =
  let rec aux mm i =
    if i < 0 then mm else aux (move_card mm ply (i-1)) (i-1) in
  aux m off


let stuff_aux cons ply off posi_k max_k =
  let rec aux accu = function
    | k when k < 0 -> accu
    | k when k = posi_k -> aux (Fact (cons ply off k) :: accu) (k-1)
    | k -> aux (NotFact (cons ply off k) :: accu) (k-1) in
  aux [] max_k

(* is red so : not blue not green *)
let stuff_col = stuff_aux cons_cardcol (* could precompute with a hashtbl *)
(* same for ranks *)
let stuff_rank = stuff_aux cons_cardrank

(*
(* is red so : not blue not green *)
let set_negativ_stuff_col m ply off posi_k max_k =
  let rec aux am = function
    | k when k < 0 -> am
    | k when k = posi_k -> aux am (k-1)
    | k -> let nam = bb_add am (NotFact (Cardcolor (ply, off, k))) true in
           aux nam (k-1)
  in aux m max_k
(* same for ranks *)
let set_negativ_stuff_rank m ply off posi_k max_k =
  let rec aux am = function
    | k when k < 0 -> am
    | k when k = posi_k -> aux am (k-1)
    | k -> let nam = bb_add am (NotFact (Cardrank (ply, off, k))) true in
           aux nam (k-1)
  in aux m max_k*)

(* set all truth value as we directly observe a new card *)
(* this replace all values *)
let place_card m ply off col rank =
  let nb_col, nb_rank = 4, 4 in  (* TODO this could be handled better? *)
  let m = bb_removes m (get_card_atoms ply off) in
  let atoms_col = stuff_col m ply off col nb_col
  and atoms_rnk = stuff_rnk m ply off col nb_col in
  bb_addl atoms_rnk (bb_addl atoms_col m)


let prepare_hands hands =
  let treat_hand ply hand = PList.mapi (fun off col_rank -> (col_rank, off, ply)) 0 hand in
  let all_triples = PList.mapi treat_hand 1 hands in
  PList.flatten all_triples

let add_all_hand_to_ob m hand_list =
  let record_card nm ((col, rnk), off, ply) = place_card nm ply off col rnk in
  List.fold_left record_card m (prepare_hands hands)

  (* return col,rank of card at offset of player *)
let get_card m ply off =
  let all_colors = (*TODO*)
  let relevants = get_card_atoms ply off in
  let kv = bb_findl m relevants in
  let rec aux acol arank = function
    | [] -> (acol, arank)
    | Fact (Cardcolor (ply, off, i)) :: t -> aux i arank t
    | Fact (Cardrank (ply, off, i)) :: t -> aux acol i t
    | x :: t -> aux acol arank t
  in aux (-1) (-1) kv

let get_card m ply off =
  let relevants = get_card_atoms ply off in
  let kv = bb_findl m relevants in
  let rec aux acol arank = function
    | [] -> (acol, arank)
    | Fact (Cardcolor (ply, off, i)) :: t -> aux i arank t
    | Fact (Cardrank (ply, off, i)) :: t -> aux acol i t
    | x :: t -> aux acol arank t
  in aux (-1) (-1) kv

let add_initial_fireworks m =
  let nb_col = 4 in (* actually 5 colors.. hmm TODO *)
  let nb_rank = 4 in
  let rec aux mm = function
    | i, _ when i < 0 -> mm
    | i, j when j < 0 -> aux mm ((i-1),nb_rank)
    | i, j -> let mmm = bb_add mm (NotFact(Firework(i, j))) true in aux mmm (i, (j-1))
  in aux m (nb_col, nb_rank)

let rec add_initial_errors_tokens m n = match n with
  | j when j < 1 -> m
  | j -> let mm = bb_add m (NotFact(Errortoken((j-1)))) true in
        add_initial_errors_tokens mm (n-1)

let rec add_multiple_clue_tokens m n = match n with
  | j when j < 1 -> m
  | j -> let mm = bb_add m (Fact(Cluetoken((j-1)))) true in
        add_multiple_clue_tokens mm (n-1)

(*let add_initial_errors_tokens m n = ignore n; m*)
(*let add_initial_fireworks m = m*)
let add_perfect_hands m ht = add_all_hand_to_ob m ht


(* first argument would be our hand, which we'll ignore *)
  (* the rest is the other hands, we need to make the belief of one player *)
let belief_base_initial (_, ht) =
  let m = BeliefBase.empty in
  (* TODO add that we know we have 8 clue tokens
   * add we know we do not have any of the 3 error tokens
   * and we know we don't have any firework
   * and know we know perfectly other's card (we know it's Red and not any other color)
   * by default we don't know anything by default on our cards *)
 let m = add_multiple_clue_tokens m 8 in
 let m = add_initial_errors_tokens m 3 in  
 let m = add_initial_fireworks m in
 let m = add_perfect_hands m ht in
 m

let playable bb (col, rank) =
  if rank = 0 then if not (bb_get bb (Fact(Firework(col, rank)))) then true else false
  else
  if (bb_get bb (Fact(Firework(col-1, rank-1)))) then
                                if not (bb_get bb (Fact(Firework(col, rank)))) then 
                                  true
                                else
                                  false
                              else
                                false


let get_error bb =
  let maxerror = 2 in
  let rec aux aerr = function
    | i when i > maxerror -> aerr
    | i when (bb_get bb (Fact(Errortoken(i)))) -> aux i (i+1)
    | i -> aux aerr (i+1)
  in aux (-1) 0

let get_clue bb =
  let maxclue = 7 in
  let rec aux aerr = function
    | i when i > maxclue -> aerr
    | i when (bb_get bb (Fact(Cluetoken(i)))) -> aux i (i+1)
    | i -> aux aerr (i+1)
  in aux (-1) 0
let place_fire bb pcard =
  let col,rank = pcard in
  let bb = bb_add bb (Fact(Firework(col, rank))) true in
  bb_add bb (NotFact(Firework(col, rank))) false

let increase_error bb = 
  let n = get_error bb in
  let bb = bb_add bb (Fact(Errortoken(n+1))) true in
  bb_add bb (NotFact(Errortoken(n+1))) false

let increase_clue bb = 
  let n = get_clue bb in
  let bb = bb_add bb (Fact(Cluetoken(n+1))) true in
  bb_add bb (NotFact(Cluetoken(n+1))) false

let decrease_clue bb = 
  let n = get_clue bb in
  let bb = bb_add bb (Fact(Cluetoken(n))) false in
  bb_add bb (NotFact(Cluetoken(n))) true 

(* placed a card *)
let play_guy off pcard bb =
  let col, rank = pcard in
  let bb = if rank = 4 then increase_clue bb else bb in
  let bb = place_fire bb pcard in
  remove_card bb 0 off

  (* reverse the belief, the last player to play see the player that just played as the next player
   * so ply 1 for him. *)
  (* for others, place card in firework, remove the card, place the new card *)
let play_oth off pcard bbt =
  let rec aux i acc l = match l with
  | [] -> acc
  | bb::bbt -> 
    let bb = place_fire bb pcard in
    let col,rank = pcard in
    let bb = if rank = 4 then increase_clue bb else bb in
    let bb = remove_card bb i off in
    let col,rank = pcard in
    let bb = place_card bb i off col rank
    in aux (i+1) (bb::acc) bbt
  in aux 1 [] (List.rev bbt)

let bplay_guy off pcard bb =
  let bb = increase_error bb in
  remove_card bb 0 off

  (* reverse the belief, the last player to play see the player that just played as the next player
   * so ply 1 for him. *)
  (* this is in case the card was wrongly payed
   * so create error token *)
let bplay_oth off pcard bbt =
  let rec aux i acc l = match l with
  | [] -> acc
  | bb::bbt -> 
    let bb = increase_error bb in
    let bb = remove_card bb i off in
    let col,rank = pcard in
    let bb = place_card bb i off col rank
    in aux (i+1) (bb::acc) bbt
  in aux 1 [] (List.rev bbt)


let play_card off pcard bb bbt =
  let bb = play_guy off pcard bb in
  let bbt = play_oth off pcard bbt in
  (bb, bbt)

let bplay_card off pcard bb bbt = 
  let bb = bplay_guy off pcard bb in
  let bbt = bplay_oth off pcard bbt in
  (bb, bbt)

let rec get_last = function
  | [] -> assert false
  | [h] -> h
  | h::t -> get_last t

(*  bb is the player who made the step *)
let update_belief (bb,bbt) (step:HT.step) = match step with
  | Hanabi_types.Play(off, ncard) -> (let pcard = get_card (get_last bbt) 1 off in
                        if (playable bb pcard) then
                          play_card off pcard bb bbt
                        else
                          bplay_card off pcard bb bbt )
  | Hanabi_types.Discard(off, ncard) -> (bb, bbt) (* TODO *)
  | Hanabi_types.ColorClue(off) -> (bb, bbt)
  | Hanabi_types.RankClue(off) -> (bb, bbt)


(* gets list e2 e3 e1 (cos n= 2, i = 0 we return 2 elements only
 * do (e2 e3 e1) :: (e3 e1 e2) *)
(* this got weird because of non empty list *)
let rec all_pov n e l acc i = match i with
  | i when i >= n -> acc
  | i -> (let re, rl = rollover e l in
  all_pov n re rl ((e, l)::acc) (i+1)
  )

(* anything *)
let make_initial_belief (h:Hanabi_types.hand) (ht:Hanabi_types.hand list) =
  let n = List.length ht in
  let rh, rht = rollover h ht in 
  (* we have ply1_hand,ply2_hand,ply3_hand
   * povs_hands return the three cyclic permutation
   * because each player only see the other's hand initially *)
  let povs_hands = all_pov n rh rht [] 0 in  
  emap belief_base_initial (h,ht) povs_hands (* apply that function to everypair *)
  


  (*  the step is made by the first player in beliefbase *)

let rec make_belief_action_pairs belief_nel = function
  | [] -> []
  | step::sl -> ( let (bb, bbl) = update_belief belief_nel step in
      (ehd belief_nel, step) :: make_belief_action_pairs (rollover bb bbl) sl
  )

let print_atomic = function
  | Cardcolor(p, off, col) -> sprintf "card %d off %d col %d" p off col
  | Cardrank(p, off, rank)-> sprintf "card %d off %d rank %d" p off rank
  | Firework(col, rank) -> sprintf "firework %d %d" col rank
  | Cluetoken(n) -> sprintf "Clue_%d" n
  | Errortoken(n) -> sprintf "Error_%d" n

let print_atom key = function
  | false -> ""
  | true -> match key with
    | Fact(a) -> "Know_"^(print_atomic a)^",\n"
    | NotFact(a) -> "Know_Not_true_"^(print_atomic a)^",\n"

let print_belief ubb =
  let kvs = BeliefBase.bindings ubb in
  let rec aux acc = function
    | [] -> acc
    | (k,v)::kvt -> aux ((print_atom k v)^acc) kvt
  in aux "" kvs 


(*sprintf "%d" (BeliefBase.cardinal ubb)*)
  (*ignore ib;ignore sl;[(BeliefBase.empty, Play 1)]*)
let print_pair (ubb, step) =  
  sprintf "(%s, %s)" (print_belief ubb) (Hanabi_types.print_step step)

let print_such_pairs any = let s = sprintf "There are %d pairs\n" (List.length any) in
  let l = List.map print_pair any in
  s ^ String.concat "\n\n" l 


(* outputs all (user's BB, action he took) from one record *)
let analyse ((h, ht, sl): Hanabi_types.record) : string = 
  let belief_nel = make_initial_belief h ht in (* return belief*list of belief - one for each player *)
  let bap = make_belief_action_pairs belief_nel sl
  in print_such_pairs bap
