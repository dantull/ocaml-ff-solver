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

let make_deck _ =
  let ns = List.map (fun n -> N(n)) (range 2 11 []) in
  let ranks = J :: Q :: K :: ns in
  let suited_cards s = List.map (fun r -> Suited(r, s)) ranks in
  let tarots = List.map (fun n -> Tarot(T(n))) (range 0 22 []) in
  List.concat (tarots::(List.map suited_cards [Thorn; Goblet; Sword; Coin]));;

let cards = make_deck () in
  print_endline(String.concat "\n" (List.map to_string cards));;
