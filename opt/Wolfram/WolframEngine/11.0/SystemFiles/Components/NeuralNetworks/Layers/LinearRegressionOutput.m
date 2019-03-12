Inputs: 
	$Input: TensorT[$Rank, $InputSize]
	$Target: TensorT[$Rank, $InputSize]

Outputs: 
	$Output: TensorT[$Rank, $InputSize]

Parameters:
	$InputSize: SizeListT[$Rank]
	$Rank: NaturalT
	$Weight: Defaulting[ScalarT, 1]

MXNet:
	Name: "LinearRegressionOutput"
	Parameters: 
		$Weight: "grad_scale"