Input: ListT[$$InputCount, TensorT[$$Rank]]

Output: ComputedType[
	TensorT[],
	dims = Map[GetTensorDimensions, $Input];
	If[AllSameBy[dims, Rest], 
		total = Total @ All1 @ dims;
		TensorT[$$Rank, Prepend[total] @ Rest @ First @ dims],
		FailValidation["all inputs must have the same rank and trailing dimensions."]
	]
]

Parameters:
	$$InputCount: SizeT
	$$Rank: NaturalT

MXNet:
	Name: "Concat"
	Parameters:
		$$InputCount: "num_args"

WLEquivalent: Catenate