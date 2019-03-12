Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]
PackageImport["Developer`"]


PackageExport["$ForceTrainingMode"]

$ForceTrainingMode = False;


PackageExport["NetApply"]

General::inpmiss = "Required input slot `` was not provided.";
General::notinit = "Net cannot be evaluated unless on an input unless all parameters have been initialized or trained.";

NetApply[net_, data_, prop_:None] := Scope[

	inputs = Inputs[net]; 
	numInputs = Length[inputs];

	If[AssociationQ[data],
		(* check that the right number of slots were provided *)
		If[numInputs =!= Length[data], ThrowFailure["invargc", Length[data], numInputs]];
		(* get the data in canonical order *)
		data = ALookup[data, Keys[inputs], ThrowFailure["inpmiss", #]&]
		,
		(* non-association inputs correspond exactly to nets with only 1 input *)
		If[numInputs =!= 1, ThrowFailure["invargc", 1, numInputs]];
		data = {data};
	];

	$batchsize = Automatic;

	Which[
		FullySpecifiedNetQ[net], 
			(* it's ready to go *)
			Null, 
		InitializedNetQ[net], 
			(* arrays present, but we need to infer shape based on the input *)
			net = InferViaInputs[net, data],
		True, 
			(* net is not ready *)
			ThrowFailure["notinit"]
	];

	(* ToEvaluatorFunctions returns {singleFunc, batchFunc} *)
	$functions = Cached[ToEvaluationFunctions, net, {"CPU", 0}];

	(* force $batchsize to be resolved if it wasn't resolved by InferViaInputs *)
	If[$batchsize === Automatic, 
		CheckTensorDims[ 
			First @ Normal @ inputs,
			First[data]
		];
	];

	$whichf = If[$batchsize === None, 1, 2];

	outputs = Outputs[net];
	resfunc = If[prop === None,
		If[Length[outputs] === 1, First, Identity],
		$ports := OutputNames[net];
		procPropSpec[outputs, prop]
	];

	Catch[
		(* apply either the batch decoder or single decoder to the data *)
		result = $functions[[$whichf]] @ data;
		(* construct the result, depending on whether multi-output or not *)
		resfunc @ result,
		(* if any encoders failed, catch the failure here *)
		EncodeFail
	]
];

NetApply[net_, len___] := ThrowFailure["invargc", Length[{len}], 1];

General::invargc = "`` inputs provided, `` were expected."
checkInputCount[netn_, datan_] := If[netn =!= datan, 
	ThrowFailure["invargc", datan, netn]
];

$PropPattern = _String | {_String, _} | None;

General::nodec = "Properties can only be queried on networks that have an attached NetDecoder."
procPropSpec[<|name_ -> type_|>, prop:$PropPattern] /; name =!= prop := (
	replaceDecoder[1, type, prop];
	First
);

General::invoutport = "`` is not the name of an output port for the net."
procPropSpec[assoc_, port_String] := 
	Extract @ IndexOf[Keys[assoc], port, ThrowFailure["invoutport", port]];

procPropSpec[assoc_, port_String -> prop:$PropPattern] := Scope[
	index = IndexOf[Keys[assoc], port, ThrowFailure["invoutport", port]];
	replaceDecoder[index, assoc[[index]], prop];
	Extract[index]
];

procPropSpec[assoc_, All -> prop:$PropPattern] := (
	Do[
		replaceDecoder[index, assoc[[index]], prop],
		{index, Length[assoc]}
	];
	Identity
)

General::invppspec = "`` is not a valid property or port specification."
procPropSpec[_, spec_] := ThrowFailure["invppspec", spec];

replaceDecoder[index_, DecodedType[decoder_, _], None] := (
	$functions[[$whichf, 3, index]] = Identity;
)

replaceDecoder[index_, DecodedType[decoder_, _], prop_] := (
	$functions[[$whichf, 3, index]] = Cached[GetDecoderFunction, decoder, prop, $batchsize =!= None];
)

General::nodeconport = "Port `` does not have a NetDecoder associated with it."
replaceDecoder[index_, _, _] :=
	ThrowFailure["nodeconport", $ports[[index]]];


PackageExport["NetDerivative"]

Options[NetDerivative] = {
	"GradientScale" -> 1.0,
	"BatchSize" -> Automatic
};

NetDerivative[net_ ? NetLayerQ, OptionsPattern[]][data_] := CatchFailure[
	NetDerivativeApply[net, data, OptionValue[{"GradientScale", "BatchSize"}]]
];

(* this is almost verbatim the same as NetApply, but we can't factor without 
introducing hacky dynamic scoping etc. *)
NetDerivativeApply[net_, data_, {scale_, batchsize_}] := Scope[
	inputs = Inputs[net]; numInputs = Length[inputs];

	If[AssociationQ[data],
		(* check that the right number of slots were provided *)
		If[numInputs =!= Length[data], ThrowFailure["invargc", Length[data], numInputs]];
		(* get the data in canonical order *)
		data = ALookup[data, Keys[inputs], ThrowFailure["inpmiss", #]&]
		,
		(* non-association inputs correspond exactly to nets with only 1 input *)
		If[numInputs =!= 1, ThrowFailure["invargc", 1, numInputs]];
		data = {data};
	];

	$batchsize = batchsize;

	Which[
		FullySpecifiedNetQ[net], 
			(* it's ready to go *)
			Null, 
		InitializedNetQ[net], 
			(* arrays present, but we need to infer shape based on the input *)
			net = InferViaInputs[net, data],
		True, 
			(* net is not ready *)
			ThrowFailure["notinit"]
	];

	If[$batchsize === Automatic, 
		CheckTensorDims[ 
			First @ Normal @ inputs,
			First[data]
		];
	];

	{executor, auxMapping, inputs, outputs, encoders, decoders} = ToMXNetExecutor[net, All, $batchsize, "Write", {"CPU", 0}];
	
	If[FailureQ @ CheckedNDArraySet[inputs, ComposeThread[encoders, data]], ReturnFailed[]];
	
	If[scale =!= None, 
		outputGrads = Map[
			MXNetLink`NDArrayCreateEmpty[MXNetLink`NDArrayDimensions[#]]&, 
			Values[outputs]
		];
		Scan[MXNetLink`NDArraySet[#, scale]&, outputGrads];
	,
		outputGrads = None;
	];

	MXNetLink`MXExecutorForward[executor, True];
	MXNetLink`MXExecutorBackward[executor, outputGrads];

	KeyMap[MXUnmanglePort, Map[Normal, executor["GradientArrays"]]]
];

(* TODO: combine this with VerboseNDArraySet *)
CheckedNDArraySet[ndarrays_, inputs_] := Catch[
	ScanAssocList[
		Function[
			checkInputDims[#1, 
				Drop[MXNetLink`NDArrayDimensions[#2], Boole[$batchsize === None]], 
				MachineArrayDimensions[#3]
			];
			MXNetLink`NDArraySet[#2, #3]
		],
		ndarrays, inputs
	],
	EncodeFail
];


PackageScope["InferViaInputs"]

$batchsize = Automatic;

General::nfspec = "Net not sufficiently specified to allow evaluation."
InferViaInputs[net:head_[assoc_Association], data_] := Scope[
	inputs = Inputs[assoc];
	(* if all the net's existing input types were already fully specified, adding the
	dimensions of the actual input data will add nothing, so bail now. *)
	If[AllTrue[inputs, FullySpecifiedTypeQ], ThrowFailure["nfspec"]];
	(* TODO: this prevents you from doing a property on a partially specified net *)
	checkInputCount[Length[inputs], Length[data]];
	inputs = MapThread[InferInputType, {data, Normal[inputs]}];
	assoc["Inputs"] = Association @ inputs;
	net2 = ConstructWithInference[head, assoc];
	If[FailureQ[net2], ThrowRawFailure[net2]];
	If[!FullySpecifiedNetQ[net2], ThrowFailure["unspecnetp"]];
	net2
]; 

General::unspecnetp = "Net contains one or more parameters that are not fully specified.";

PackageScope["InferInputType"]

(* TODO: deal gracefully with {} *)

(* TODO: be more discerning, e.g. the encoder accepts lists *)
InferInputType[data_List, name -> dt:EncodedType[_, _]] :=
	(setBatchSize[Length[data]]; name -> dt);

InferInputType[data_, name_ -> dt:EncodedType[_, _]] :=
	(setBatchSize[None]; name -> dt);

InferInputType[img_Image, name_ -> (_ChannelT | _TensorT)] := Scope[
	enc = NetEncoder["Image"];
	setBatchSize[None]; 
	name -> EncodedType[enc, EncoderOutputType[enc]]
];

InferInputType[data_, name_ -> type_] := Scope[
	$name = name; $type = type;
	name -> infer[type, data]
];

(* gah this is just for the ListT case below *)
InferInputType[data_, name_ -> type_] := Scope[
	$name = name; $type = type;
	name -> infer[type, data]
];

Clear[infer];

(* TODO: infer batchness *)
infer[ListT[n_, t_], data_] := Scope[
	If[!ListQ[data], ipanic];
	len = Length[data];
	If[IntegerQ[n] && len =!= n, ipanic];
	Do[
		t = infer[t, d];
		If[FailureQ[t], ipanic], 
		{d, data}
	];
	Table[t, len]
];

infer[t_TensorT | t_ChannelT, data_List] := Scope[
	If[!MachineArrayQ[data], ipanic];
	checkDims[t, data]
];

infer[ScalarT, data_List] := Scope[
	If[!MachineArrayQ[data], ipanic];
	BatchedTensorT[{Length[data], 1}]
];

infer[EitherT[alts_], data_] := Scope[
	Switch[$batchsize,
		None,
			(* data should match one of the alts *)
			inferEither[data, alts],
		Automatic,
			(* first try match data... *)
			res = inferEither[data, alts];
			If[FailureQ[res],
				(* if that fails and we have a list, try match first elem of data *)
				If[ListQ[data], res = inferEither[First[data], alts]]
			];
			res,
		_Integer,
			(* first elem of data should match one of the alts *)
			inferEither[First[data], alts]
	] // OnFail[ipanic]
];


inferEither[data_, alts_] := Block[
	{ipanic := Throw[$Failed], res, last = $batchsize},
	Do[
		res = Catch[infer[t, data]];
		If[!FailureQ[res], Return[res, Block]];
		$batchsize = last
	,
		{t, alts}
	];
	$Failed
];

(*
infer[type_, data_List] := Scope[
	(* for EitherT in CrossEntropyLossLayer *)
	If[IntegerQ[$batchsize] || FailureQ[t2 = UnifyTypes[t, TensorT[Rest[dims]]]],
		If[FailureQ[t2 = UnifyTypes[t, TensorT[dims]]],
			ipanic;
			setBatchSize[First[dims]];
		],
		setBatchSize[None];
	];
	t2
];*)

infer[type_, data_ ? MachineQ] := Scope[
	t2 = UnifyTypes[type, If[IntegerQ[data], IntegerT, ScalarT]];
	If[FailureQ[t2] || !testType[data, t2], ipanic];
	setBatchSize[None];
	t2
];

infer[type_, data_] := Scope[
	If[TestType[data, type], 
		setBatchSize[None],
		If[ListQ[data] && TestType[First[data], type],
			setBatchSize[Length[data]],
			ipanic
		]
	];
	type
];

General::invinpnm = "Data provided to port `` was not ``."
ipanic := ThrowFailure["invinpnm", $name, TypeForm @ $type];

(*
The strategy here is to deal with various levels of partially specified tensor.
if the dims are not known, we might still know the rank, so check that.
if the dims are known, they have to agree perfectly.
this doesn't unfortunately take care of partially known dims, e.g. ChannelT[5, TensorT[]].
for that we'll have to pick up an error from inference itself. *)

PackageScope["CheckTensorDims"]

Clear[CheckTensorDims];

CheckTensorDims[name_ -> t_, data_] := Scope[
	$name = name; $type = t;
	checkDims[t, data]
];

CheckTensorDims[name_ -> EncodedType[enc_, t_], data_] := (
	If[ListQ[data],
		If[AcceptsListsQ[enc],
			If[FailureQ[Quiet[enc @ First @ data]],
				setBatchSize[None],
				setBatchSize[Length[data]]
			],
			setBatchSize[Length[data]]
		],
		setBatchSize[None];
	];
	t
);


(* convenience: for singleton vector, we allow pure scalar (unbatched) *)
checkDims[TensorT[1, {1}], data_] := 
	Match[Dimensions[data],
		{}|{1} :> UnbatchedTensorT[{1}],
		{n_} | {n_, 1} :> BatchedTensorT[{n, 1}],
		ThrowFailure["invscalarnm", $name]
	];

checkDims[ScalarT|PosIntegerT|_IndexIntegerT, data_] := 
	Match[Dimensions[data],
		{} :> UnbatchedTensorT[{}],
		{n_} :> BatchedTensorT[{n}],
		ThrowFailure["invscalarnm", $name]
	];

General::invscalarnm = "Data provided to port `` should be a scalar or list of scalars."

(* TODO: Infer batchness *)
checkDims[l:ListT[n_, t_], data_] := (
	If[!ListQ[data] || Length[data] =!= n, ipanic];
	Scan[checkDims[t, #]&, data];
	l
);

checkDims[t_, data_] := Scope[
	dims = Dimensions[data];
	rank = Length[dims];
	If[FailureQ[tdims = GetTensorDimensions[t]],
		If[FailureQ[trank = GetTensorRank[t]],
			EitherTensorT[dims] 
			(* TODO: should this be here, e.g. if PosIntegerT wasn't caught above,
			this will produce a batched tensor *)
			,
			Which[
				rank == trank + 1,
					BatchedTensorT[dims],
				rank == trank,
					UnbatchedTensorT[dims],
				True,
					ThrowFailure["invranknm", $name, rank, trank]
			]
		],
		tdims = tdims /. SizeT -> _;
		Which[
			MatchQ[dims, tdims],
				UnbatchedTensorT[dims],
			MatchQ[dims, Prepend[tdims, _]],
				BatchedTensorT[dims],
			True,
				ThrowFailure["invdimsnm", $name, dims, tdims]
		]
	]
];

General::invranknm = "Data provided to port `` has the wrong rank (`` instead of ``)."


BatchedTensorT[dims_] := (
	setBatchSize[First @ dims];
	TensorT[Rest @ dims]
);

UnbatchedTensorT[dims_] := (
	setBatchSize[None];
	TensorT[dims]
);

EitherTensorT[dims_] := 
	If[IntegerQ[$batchsize], 
		BatchedTensorT[dims], 
		UnbatchedTensorT[dims]
	];

General::incbatch = "Inconsistent batch sizes across ports."
setBatchSize[size_] := 
	Which[
		$batchsize === Automatic,
			$batchsize = size,
		$batchsize =!= size,
			ThrowFailure["incbatch"]
	];
