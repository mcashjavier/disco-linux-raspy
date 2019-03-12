Input: TensorT[NaturalT, $$IDimensions]

Output: TensorT[$$Rank, $Dimensions]

Parameters:
	$Dimensions: SizeListT[$$Rank]
	$$Rank: NaturalT
	$$IDimensions: SizeListT[]

PostInferenceFunction: Function[
	If[VectorQ[$Dimensions, IntegerQ] && VectorQ[$$IDimensions, IntegerQ] && Apply[Times, $Dimensions] =!= Apply[Times, $$IDimensions], 
		FailValidation["number of elements in output tensor must equal number of elements in input tensor."]
	]
]

MXNet:
	Name: "Reshape"
	Writer: Function[
		"shape" -> NeuralNetworks`MXNet`PackagePrivate`writeIntList[Prepend[#Dimensions, -1]]
	]