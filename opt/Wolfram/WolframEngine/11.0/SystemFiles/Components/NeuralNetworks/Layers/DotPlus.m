Input: VectorT[$$InputSize]

Output: VectorT[$Size]

Arrays:
	$Weights: MatrixT[$Size, $$InputSize]
	$Biases: Nullable @ VectorT[$Size]

Parameters:
	$Size: SizeT
	$$InputSize: SizeT

MXNet:
	Name: "FullyConnected"
	Parameters: 
		$Size: "num_hidden"
	Arrays:
		$Weights: "weight"
		$Biases: "bias"
	Writer: Function["no_bias" -> If[#2["Biases"] === None, "True", "False"]] 