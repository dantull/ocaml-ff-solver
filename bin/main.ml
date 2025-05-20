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

  let shuffle_cards cards =
    let arr = Array.of_list cards in
    let len = Array.length arr in
    for i = len - 1 downto 1 do
      let j = Random.int (i + 1) in
      let tmp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- tmp
    done;
    arr
  ;;

  let partition n arr =
    if n <= 0 then invalid_arg "partition: n must be positive"
    else
      let len = Array.length arr in
      let rec aux i acc =
        if i >= len then List.rev acc
        else
          let last = min (i + n) len in
          let group = Array.sub arr i (last - i) |> Array.to_list in
          aux last (group :: acc)
      in
      aux 0 []
  ;;

let make_game cards =
  let shuffled = shuffle_cards cards in
  {
    stacks = [] :: partition 7 shuffled;
    stash = None;
    foundations = ([Tarot(T(-1))] :: [Tarot(T(22))] :: List.map (fun s -> [Suited(A, s)]) all_suits)
  };;

let stash_str stash =
  match stash with
    | None -> ""
    | Some card -> to_string card

let stack_str cards =
  String.concat " " (List.map to_string cards)

let show_game gs =
  let stash_msg = String.concat "" ["stash: "; (stash_str gs.stash)] in
  let f_lines = List.map stack_str gs.foundations in
  let s_lines = List.map stack_str gs.stacks in
  stash_msg :: List.flatten [f_lines; s_lines];;

let g = make_game (make_deck ()) in
let game_lines = show_game g in
  print_endline(String.concat "\n" game_lines);;
