Input: ListT[$$InputCount, TensorT[]]

Output: ComputedType[
	TensorT[],
	dims = Map[GetTensorDimensions, $Input];
	If[SameQ @@ dims,
		First[$Input, FailValidation["at least one input must be provided."]],
		FailValidation["all inputs must have same dimensions."]
	]
]

Parameters:
	$$InputCount: SizeT

MXNet:
	Name: "ElementWiseSum"
	Parameters:
		$$InputCount: "num_args"

WLEquivalent: Total