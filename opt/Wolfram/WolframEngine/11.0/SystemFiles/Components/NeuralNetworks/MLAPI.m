Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]


PackageExport["MLAPIEvaluate"]

MLAPIEvaluate[net_, input_] :=
	Block[{$DisableOutputDecoders = True}, 
		Log[NetApply[net, input] + $MinMachineNumber]
	];


PackageExport["MLAPIOutputClasses"]

MLAPIOutputClasses[net_] := 
	Match[
		OutputTypes[net],
			{DecodedType[HoldPattern @ NetDecoder["Class", assoc_], _]} :>
				assoc["Parameters", "Labels"],
		$Failed
	];


PackageExport["MLAPIInputTypes"]

MLAPIInputTypes[net_] := Catch[
	KeyValueMap[
		<|"Name" -> #1, "Type" -> mltype @ #2|>&,
		Inputs[net]
	]
];

mltype[EncodedType[HoldPattern @ NetEncoder[type_, assoc_], _]] := 
	$EncoderData[type, "MLType"][assoc["Parameters"]];

mltype[ScalarT] := "Numerical";

mltype[TensorT[1, {n_Integer}]] := {"NumericalVector", n};

mltype[(c_TensorT | c_ChannelT)] := {"NumericalTensor", GetTensorDimensions[c]};

mltype[PosIntegerT] := "PositiveInteger";

mltype[IntegerT] := "Integer";

mltype[_] := Throw[$Failed];

