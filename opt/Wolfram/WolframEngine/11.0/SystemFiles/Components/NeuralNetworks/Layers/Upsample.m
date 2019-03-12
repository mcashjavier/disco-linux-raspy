Input: ChannelT[$Channels, TensorT[2, $InputDimensions]]

Output: ChannelT[$Channels, TensorT[2, $OutputDimensions]]
	
Parameters:
	$Scale: PosIntegerT
	$Channels: SizeT
	$InputDimensions: SizeListT[2]
	$OutputDimensions: ComputedType[SizeListT[2], $InputDimensions * $Scale]

MXNet:
	Name: "UpSampling"
	Parameters:
		$Scale: "scale"
	Writer: Function[{
		"num_args" -> "1",
		"sample_type" -> "nearest"
	}]
