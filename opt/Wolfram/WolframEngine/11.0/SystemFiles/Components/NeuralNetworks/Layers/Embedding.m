Input: IndexIntegerT[$ClassCount]

Output: VectorT[$OutputDimension]

Arrays:
	$Weights: MatrixT[$ClassCount, $OutputDimension]

Parameters:
	$ClassCount: SizeT
	$OutputDimension: SizeT

MXNet:
	Name: "Embedding"
	Parameters:
		$OutputDimension: "output_dim"
		$ClassCount: "input_dim"
	Arrays:
		$Weights: "weight"