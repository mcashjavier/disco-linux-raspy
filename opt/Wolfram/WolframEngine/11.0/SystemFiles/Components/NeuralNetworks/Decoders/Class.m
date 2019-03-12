Input: VectorT[$Dimensions]

Parameters:
	$Labels: ListT[$Dimensions, ExpressionT]
	$Dimensions: SizeT

ArrayDepth: 1

ToDecoderFunction: Function[
	toDecisionFunction[#2, #Labels]
]

ToPropertyDecoderFunction: Function @ With[
	{labels = #Labels, dims = #Dimensions},
	Replace[#3, {
		"Decision" :> toDecisionFunction[#2, labels],
		"Probabilities" :> MapB[#2, threadProbabilities[labels, dims]],
		{"Probabilities"|"Probability", class_ /; MemberQ[labels, class]} :> With[
			{index = IndexOf[labels, class]},
			If[#2, Function[input, Part[input, All, index]], Extract @ index]
		],
		"TopProbabilities" :> MapB[#2, topProbs[labels]],
		{"TopProbabilities", n_Integer /; 1 <= n <= dims} :> MapB[#2,
			Function[input, TakeLargestBy[Thread[labels -> input], Last, n]]
		],
		{"TopDecisions", n_Integer /; 1 <= n <= dims} :> MapB[#2,
			Function[input, Part[labels, Reverse @ Ordering[input, -n]]]
		],
		"Entropy" :> DEntropy,
		_ :> $Failed
	}]
]

AvailableProperties: {"Decision", "TopProbabilities", {"TopDecisions", _}, {"TopProbabilities", _Integer}, "Probabilities", {"Probabilities", _}, "Entropy"}

toDecisionFunction[batchq_, labels_] :=
	If[batchq, 
		Function[input, Part[labels, listMaxIndex[input]]],
		Function[input, Part[labels, First @ Ordering[input, -1]]]
	];

DEntropy = Compile[{{probs, _Real, 1}}, Module[{e = 0.},
	Do[
		If[2*$MachineEpsilon <= p <= 1., e -= p * Log[p]],
		{p, probs}
	];
	e
], RuntimeAttributes -> {Listable}];

topProbs[labels_][input_] := Scope[
	rules = Reverse @ SortBy[Thread[labels -> input], Last];
	Select[rules, Last /* GreaterThan[0.1 * rules[[1, 2]]]]
];

listMaxIndex = 
	Compile[{{values, _Real, 2}},
	Map[First @ Ordering[#, -1]&, values]
];

threadProbabilities[labels_, len_] :=
	Function[input, If[Length[input] =!= len, Panic[], AssociationThread[labels, input]]];