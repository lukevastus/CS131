
let accept_all derivation string = Some (derivation, string)
let accept_a_sup derivation = function
	| "Sup"::a -> Some(derivation, "Sup"::a)
	| _ -> None

type my_nonterminals =
    | A | B | C

let my_grammar =
	(A, 
	function
    	|A -> [[N B; T "Sup"];[T "KK"; N A]]
    	|B -> [[T "Wow"];[N C]]
    	|C -> [[T "3"]]
	)

let test_1 = ((parse_prefix my_grammar accept_all ["KK";"3";"Sup"]) = 
	Some([(A, [T "KK"; N A]); (A, [N B; T "Sup"]); (B, [N C]); (C, [T "3"])], []))

let test_2 = ((parse_prefix my_grammar accept_a_sup ["KK";"KK";"Wow";"Sup";"Sup"]) = 
	Some([(A, [T "KK"; N A]); (A, [T "KK"; N A]); (A, [N B; T "Sup"]);(B, [T "Wow"])],
    ["Sup"]))
