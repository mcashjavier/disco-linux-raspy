Input: ChannelT[$InputChannels, TensorT[2, $$InputSize]]

Output: ChannelT[$OutputChannels, TensorT[2, $$OutputSize]]

Arrays:
	$Weights: ChannelT[$OutputChannels, ChannelT[$InputChannels, TensorT[2, $KernelSize]]]
	$Biases: Nullable[VectorT[$OutputChannels]]

Parameters:
	$OutputChannels: SizeT
	$KernelSize: SizeListT[2]
	$Stride: Defaulting[ListT[2, PosIntegerT], {1,1}]
	$PaddingSize: Defaulting[ListT[2, NaturalT], {0,0}]
	$Dilation: Defaulting[ListT[2, PosIntegerT], {1,1}]
	$InputChannels: SizeT
	$$GroupNumber: Defaulting[SizeT, 1]
	$$InputSize: SizeListT[2]
	$$OutputSize: ComputedType[SizeListT[2], Block[{outsize},
		outsize = convolutionOutputSize[$$InputSize, $PaddingSize, $KernelSize, $Stride, $Dilation];
		If[Min[outsize] < 1, FailValidation["output dimensions cannot be smaller than 1."]];
		outsize
	]]

PostInferenceFunction: Function[
	If[Apply[Or, Thread[$$InputSize < $KernelSize]], 
		FailValidation["kernel size must be smaller than input size."]
	]
]

MXNet:
	Name: "Convolution"
	Parameters: 
		$OutputChannels: "num_filter"
		$KernelSize: "kernel"
		$Dilation: "dilate"
		$PaddingSize: "pad"
		$Stride: "stride"
		$$GroupNumber: "num_group"
	Arrays:
		$Weights: "weight"
		$Biases: "bias"
	Writer: Function[{
		"no_bias" -> If[#2["Biases"] === None, "True", "False"],
		"workspace" -> IntegerString[convolutionRequiredWorkspace[
			#InputChannels, #OutputChannels, #$OutputSize, #KernelSize, #$GroupNumber
		]]
	}] 

ShortName: "Conv"