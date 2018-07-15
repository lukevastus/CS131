
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec find_rule_by_symbol s l = 
	match l with
	| [] -> []
	| a::b -> if (fst a) == s then (snd a)::(find_rule_by_symbol s b) else find_rule_by_symbol s b

let convert_grammar gram1 = 
	(fst gram1, fun x -> find_rule_by_symbol x (snd gram1))

(*
let rec match_symbol_to_T nonterminal production symbol d = 
	let rec match_symbol_from_rules rules production symbol d = 
		match rules with
		| [] -> []
		| [r1] -> 
			(match r1 with
			| [] -> []
			| [T s1] -> if s1 == symbol then d@[T s1] else []
			| [N s1] -> match_symbol_to_T s1 production symbol d@[N s1]
			| s1::s2 -> []
			)
		| r1::r2 -> if (match_symbol_from_rules [r1] production symbol d) != [] then (match_symbol_from_rules [r1] production symbol d) else (match_symbol_from_rules r2 production symbol d)
	in
	match_symbol_from_rules (production nonterminal) production symbol d


let rec match_fragment_to_Ts nonterminal production frag d = 
			


let rec actual_matcher nonterminal production accept frag d =  
	match frag with
	| [] -> None
	| a::b -> 
*)

let rec append_to_list l a = 
	match l with
	| [] -> [a]
	| b::c -> b::(append_to_list c a)

let rec match_frag_to_a_rule rule production accept d frag = 
	match rule with
	| [] -> accept d frag
	| (T s1)::s2 -> 
		(match frag with
		| [] -> None
		| a::b -> if s1 = a then (match_frag_to_a_rule s2 production accept d b) else None
		) 
	| (N s1)::s2 ->
		(match frag with
		| [] -> None
		| a::b -> match_frag_from_rule_list (production s1) s1 production (match_frag_to_a_rule s2 production accept) d frag
		)

and match_frag_from_rule_list rules nonterminal production accept d frag =
	match rules with
	| [] -> None
	| r1::r2 -> 		
		(match (match_frag_to_a_rule r1 production accept (append_to_list d (nonterminal, r1)) frag) with
		| None -> match_frag_from_rule_list r2 nonterminal production accept d frag
		| Some(deri, suffix) -> Some(deri, suffix)
		)
	   
let rec actual_matcher symbol production accept d frag = 
	match_frag_from_rule_list (production symbol) symbol production accept d frag	

let parse_prefix gram =
	fun accept frag -> actual_matcher (fst gram) (snd gram) accept [] frag 


