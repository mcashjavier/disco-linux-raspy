Output: ScalarT

Parameters:
	$Mean: Defaulting[ScalarT, 0.]
	$StandardDeviation: Defaulting[ScalarT, 1.]

ToEncoderFunction: Function @ With[
	{mean = #Mean, stddev = #StandardDeviation},
	Function[input, (input - mean) / stddev]
]

MLType: Function[$Failed]
