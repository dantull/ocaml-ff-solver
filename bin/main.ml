type suit =
  | Thorn
  | Goblet
  | Sword
  | Coin;;

type rank =
  | A
  | N of int
  | J
  | Q
  | K;;

type tarot = T of int

type card =
  | Suited of rank * suit
  | Tarot of tarot;;

type game = {
  stacks : card list list;
  stash : card option;
  foundations : card list list;
}

let to_string card =
  match card with
  | Suited (r, s) ->
      let rank_str =
        match r with
        | A -> "A"
        | J -> "J"
        | Q -> "Q"
        | K -> "K"
        | N n -> string_of_int n
      in
      let suit_str =
        match s with
        | Thorn -> "t"
        | Goblet -> "g"
        | Sword -> "s"
        | Coin -> "c"
      in
      rank_str ^ suit_str
  | Tarot (T n) -> "t" ^ (string_of_int n);;

let rec range i j acc =
  if i = j then acc
  else if i < j then range (i + 1) j (i :: acc)
  else range j i acc;;

let all_suits = [Thorn; Goblet; Sword; Coin];;

let make_deck _ =
  let ns = List.map (fun n -> N(n)) (range 2 11 []) in
  let ranks = J :: Q :: K :: ns in
  let suited_cards s = List.map (fun r -> Suited(r, s)) ranks in
  let tarots = List.map (fun n -> Tarot(T(n))) (range 0 22 []) in
  List.concat (tarots::(List.map suited_cards all_suits));;

let make_game _ =
  {
    stacks = List.map (fun _ -> []) (range 0 11 []);
    stash = None;
    foundations = ([Tarot(T(-1))] :: [Tarot(T(22))] :: List.map (fun s -> [Suited(A, s)]) all_suits)
  };;

let as_card_list stash =
  match stash with
    | None -> []
    | Some card -> [card]
  
let show_game game_state =
  List.flatten [[(as_card_list game_state.stash)]; game_state.stacks; game_state.foundations];;

let g = make_game () in
let cards = make_deck () in
let more_cards = show_game g in
let all_cards = List.append [cards] more_cards in
  print_endline(String.concat "\n" (List.map to_string (List.flatten all_cards)));;
