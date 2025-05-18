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

let cards = [Suited (A, Thorn); Suited (J, Goblet); Suited (Q, Sword); Suited(K, Coin); Suited(N(2), Thorn); Tarot(T(1))] in
  print_endline(String.concat "\n" (List.map to_string cards));;
