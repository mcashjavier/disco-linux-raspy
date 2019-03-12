Input: ChannelT[$Channels, TensorT[2, $$InputSize]]

Output: ChannelT[$Channels, TensorT[2, $$OutputSize]]

Parameters:
	$KernelSize: SizeListT[2]
	$Stride: Defaulting[ListT[2, PosIntegerT], {1,1}]
	$PaddingSize: Defaulting[ListT[2, NaturalT], {0,0}]
	$Function: Defaulting[PoolingFunctionT, Max]
	$Channels: SizeT
	$$InputSize: ListT[2, SizeT]
	$$OutputSize: ComputedType[SizeListT[2], poolingOutputSize[$$InputSize, $PaddingSize, $KernelSize, $Stride]]
	$$InputSize: SizeListT[2]

MXNet:
	Name: "Pooling"
	Parameters:
		$KernelSize: "kernel"
		$PaddingSize: "pad"
		$Stride: "stride"
		$Function: "pool_type"

ShortName: "Pool"

PostInferenceFunction: Function[
	If[Or @@ Thread[$$InputSize < $KernelSize], 
		FailValidation["kernel size must be smaller than input size."]
	];
	If[MatchQ[$Stride, {i_Integer, j_Integer} /; i =!= j],
		FailValidation["strides in each dimension must be the same (this restriction will be lifted in future)."]
	];
]
