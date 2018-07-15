
let rec has a b = 
	match b with
	| [] -> false
	| b1::b2 -> (b1 = a) || (has a b2)   

let rec remove_dups a = 
	match a with
	| [] -> [] 
	| [a1] -> [a1]
	| a1::a2 -> if has a1 a2 then remove_dups a2 else a1::(remove_dups a2)
	
let rec subset a b =
	match a with 
	| [] -> true
	| [a1] -> has a1 b
	| a1::a2 -> has a1 b && subset a2 b

let equal_sets a b =
	subset a b && subset b a

let rec set_union a b =
	match a with 
	| [] -> remove_dups b
	| [a1] -> remove_dups (a1::b)
	| a1::a2 -> remove_dups (a1::(set_union a2 b))

let rec set_intersection a b = 
	match a with
	| [] -> []
	| [a1] -> if has a1 b then a else []
	| a1::a2 -> if has a1 b then remove_dups (a1::(set_intersection a2 b)) else remove_dups (set_intersection a2 b)

let rec set_diff a b = 
	match a with
	| [] -> []
	| [a1] -> if has a1 b then [] else a
	| a1::a2 -> if has a1 b then set_diff a2 b else a1::(set_diff a2 b)

let rec computed_fixed_point eq f x = 
	if eq (f x) x then x else computed_fixed_point eq f (f x)

let rec is_periodic_point eq f p x1 x0 = 
	match p with
	| 0 -> eq x1 x0
	| a -> is_periodic_point eq f (p - 1) (f x1) x0

let rec computed_periodic_point eq f p x = 
	if is_periodic_point eq f p x x then x else computed_periodic_point eq f p (f x)

let rec while_away s p x = 
	if p x then x::(while_away s p (s x)) else []

let rec rle_decode lp = 
	match lp with
	| [] -> []	
	| (0, b)::c -> rle_decode c
	| (a, b)::c -> b::(rle_decode ((a - 1, b)::c))


type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let is_valid_symbol s vs = 
	match s with
	| T _ -> true
	| N a -> has a vs

let rec all_symbols_valid ss vs = 
	match ss with
	| [] -> true
	| [a] -> is_valid_symbol a vs
	| a::b -> is_valid_symbol a vs && all_symbols_valid b vs

(* let rec find_rule_by_symbol s l = 
	match l with
	| [] -> []
	| [a] -> if (fst a) == s then [a] else []
	| a::b -> if (fst a) == s then a::(find_rule_by_symbol s b) else find_rule_by_symbol s 
*)

let rec update_valid_symbols (rs, vs) =
	match rs with
	| [] -> vs
	| [a] -> if all_symbols_valid (snd a) vs then (fst a)::vs else vs
	| a::b -> if all_symbols_valid (snd a) vs then (fst a)::(update_valid_symbols (b, vs)) else update_valid_symbols (b, vs)

let make_pair_for_output (rs, vs) = 
	rs, update_valid_symbols (rs, vs)

let compare_update_output (rs1, vs1) (rs2, vs2) = 
	equal_sets vs1 vs2

let get_valid_symbols rs = 
	snd (computed_fixed_point (compare_update_output) (make_pair_for_output) (rs, []))

let rec get_valid_rules rs vs = 
	match rs with
	| [] -> []
	| [a, b] -> if all_symbols_valid b vs then rs else []
	| a::b -> if all_symbols_valid (snd a) vs then a::(get_valid_rules b vs) else get_valid_rules b vs

let filter_blind_alleys g = 
	 (fst g), (get_valid_rules (snd g) (get_valid_symbols (snd g)))
