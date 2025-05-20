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

let is_adjacent r1 r2 =
  match (r1, r2) with
  | (A, N 2) | (N 2, A) -> true
  | (N n1, N n2) when abs (n1 - n2) = 1 -> true
  | (N 10, J) | (J, N 10) -> true
  | (J, Q) | (Q, J) -> true
  | (Q, K) | (K, Q) -> true
  | _ -> false

let can_play_on c1 c2 =
  match (c1, c2) with
  | Suited (r1, s1), Suited (r2, s2) when s1 = s2 && is_adjacent r1 r2 -> true
  | _ -> false

let top_pairs game_state =
  let tops = List.filter_map (fun l -> List.nth_opt l 0) game_state.stacks in
  let rec pairs acc = function
    | [] | [_] -> List.rev acc
    | x :: xs -> pairs (List.rev_append (List.map (fun y -> (x, y)) xs) acc) xs
  in
  pairs [] tops

let playable_top_pairs game_state =
  List.filter (fun (c1, c2) -> can_play_on c1 c2) (top_pairs game_state)

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

let rec insert_at n elt lst acc =
  if n = 0 then
    List.rev acc @ [elt] @ lst
  else
    match lst with
    | [] -> List.rev acc @ [elt]
    | fst :: rest -> insert_at (n - 1) elt rest (fst :: acc)

let make_game cards =
  let shuffled = shuffle_cards cards in
  {
    stacks = insert_at 5 [] (partition 7 shuffled) [];
    stash = None;
    foundations = ([Tarot(T(-1))] :: [Tarot(T(22))] :: List.map (fun s -> [Suited(A, s)]) all_suits)
  };;

let stash_str stash =
  match stash with
    | None -> ""
    | Some card -> to_string card

let stack_str cards =
  String.concat " " (List.map to_string (List.rev cards))

let show_game gs =
  let stash_msg = String.concat "" ["stash: "; (stash_str gs.stash)] in
  let f_lines = List.map stack_str gs.foundations in
  let s_lines = List.map stack_str gs.stacks in
  stash_msg :: List.flatten [f_lines; s_lines];;

  let playable_pairs_to_strings game_state =
    playable_top_pairs game_state
    |> List.map (fun (c1, c2) -> to_string c1 ^ " -> " ^ to_string c2)
  ;;

let g = make_game (make_deck ()) in
let game_lines = show_game g in
let pair_strings = playable_pairs_to_strings g in
  print_endline(String.concat "\n" (game_lines @ pair_strings));;
