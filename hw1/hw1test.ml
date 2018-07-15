
let my_subset_test0 = subset [] [1]
let my_subset_test1 = subset [1] [1;1]
let my_subset_test2 = subset [1;2] [1;3;1;2]
let my_subset_test3 = not (subset [1;1;2] [3;1])

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [1;2] [2;1;1;1;2]
let my_equal_sets_test2 = not (equal_sets [1;2] [2;3;1])

let my_set_union_test0 = equal_sets [1;2] (set_union [] [1;2])
let my_set_union_test1 = equal_sets [1;2;3] (set_union [2] [2;3;1])

let my_set_intersection_test0 = equal_sets [] (set_intersection [1] [2;3])
let my_set_intersection_test1 = equal_sets [1;2] (set_intersection [2;1] [1;2;3])

let my_set_diff_test0 = equal_sets (set_diff [] [1;2;3]) []
let my_set_diff_test1 = equal_sets [1] (set_diff [1;2;3] [2;3;3;3;2])
let my_set_diff_test2 = equal_sets [1;2] (set_diff [2;2;2;1;1;1] [3])

let my_computed_fixed_point_test0 = 
	computed_fixed_point (=) (fun x -> x / 3) 10 = 0

let my_computed_periodic_point_test0 = 
	computed_periodic_point (=) (fun x -> -1 * x) 2 1 = 1

let my_while_away_test0 = 
	equal_sets [2;4;8] (while_away (fun x -> x * 2) ((>) 9) 2)

let mt_rle_decode_test0 = 
	equal_sets ["ha";"ha";"hee";"hee";"hee"] (rle_decode [2,"ha";3,"hee"])

type my_nonterminals = 
	| A | B | C 

let my_grammar = 
	A, 
	[A, [N B; T "Sup"];
	A, [T "KK"; N A];
	B, [T "Wow"];
	B, [N C];
	C, [N C]]

let my_filter_blind_alleys_test0 = 
	filter_blind_alleys my_grammar = 
	(A,
	[A, [N B; T "Sup"];
    A, [T "KK"; N A]; 
    B, [T "Wow"]]) 
