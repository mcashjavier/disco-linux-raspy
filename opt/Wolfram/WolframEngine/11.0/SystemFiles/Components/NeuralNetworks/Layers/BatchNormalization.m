Input: $$Shape

Output: $$Shape

Parameters:
	$Momentum: Defaulting[ScalarT, 0.9]
	$Epsilon: Defaulting[ScalarT, 0.001]
	$Channels: SizeT
	$$Shape: EitherT[{VectorT[$Channels], TensorT[{$Channels, SizeT, SizeT}]}]

Arrays:
	$Gamma: VectorT[$Channels]
	$Beta: VectorT[$Channels]
	$MovingVariance: VectorT[$Channels]
	$MovingMean: VectorT[$Channels]

AuxArrays: {"MovingMean", "MovingVariance"}

MXNet:
	Name: "BatchNorm"
	Parameters: 
		$Epsilon: "eps"
		$Momentum: "momentum"
	Arrays:
		$Gamma: "gamma"
		$Beta: "beta"
		$MovingVariance: "moving_var"
		$MovingMean: "moving_mean"

ShortName: "BatchNorm"
