Inputs: 
	$Input: VectorT[$$Dimensions]
	$Target: ComputedType[
		EitherT[{VectorT[$$Dimensions], IndexIntegerT[$$Dimensions]}],
		If[$TargetForm == "Index", 
			IndexIntegerT[$$Dimensions], 
			VectorT[$$Dimensions]
		],
		{$TargetForm, $$Dimensions},
		StringQ[$TargetForm]
	]

Outputs: 
	$Loss: ScalarT 

Parameters:
	$TargetForm: ComputedType[
		EnumT[{"Index", "Probabilities"}], 
		If[Head[$Target] === IndexIntegerT, "Index", "Probabilities"], 
		{$Target}, 
		!MatchQ[$Target, _EitherT]
	]
	$$Dimensions: SizeT

MXNet:
	Name: "NLLLoss"
	FusionRules: {"SoftmaxActivation" -> "CrossEntropyLoss"}
	Writer: Function["sparse" -> If[#TargetForm === "Index", "true", "false"]]