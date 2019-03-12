Input: $$Shape
Output: $$Shape

Parameters:
	$DropoutProbability: Defaulting[ScalarT, 0.5] (* TODO: make this ProbabilityT *)
	$$Shape: TensorT[]

PostInferenceFunction: Function[
	If[$DropoutProbability < 0 || $DropoutProbability > 1, 
		FailValidation["probability must be between 0 and 1."]
	]
]

MXNet:
	Name: "Dropout"
	Parameters:
		$DropoutProbability: "p"
