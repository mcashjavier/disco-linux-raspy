Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]
PackageImport["Developer`"]
PackageImport["MXNetLink`"]

PackageExport["ImportMXNetModel"]

ImportMXNetModel[jsonPath_String, paramPath_String] := CatchFailure @ Scope[
	$Arrays = NDArrayImport[paramPath];
	json = Developer`ReadRawJSONFile[jsonPath];
	If[FailureQ[json], ReturnFailed[]];
	{keys, vals} = KeysValues[json];
	If[keys =!= {"nodes", "arg_nodes", "heads"}, ReturnFailed[]];
	{nodes, argNodes, heads} = vals;
	argNodes += 1;
	allNames = nodes[[All, "name"]];
	namedTensors = Part[nodes, argNodes, "name"];
	CollectTo[{$edges}, layers = Map[readNode, nodes]];
	NetGraph[Association[layers], $edges]
];

readNode[layer_] := Scope[
	{type, name, inputs} = Lookup[layer, {"op", "name", "inputs"}];
	layer = readLayer[layer];
	If[layer === None, Return[Nothing]];
	inputs = inputs[[All, 1]] + 1;
	inputs = allNames[[Select[inputs, !MemberQ[argNodes, #]&]]];
	ninputs = Length[inputs];
	Which[
		type === "Concat", 
			BagInsert[$edges, inputs -> name],
		ninputs > 1,
			Panic["MultiInputs"],
		ninputs == 1,
			BagInsert[$edges, First[inputs] -> name]
	];
	name -> layer
];


PackageScope["readLayer"]
PackageScope["toLayerReader"]
PackageScope["toValueReader"]

readLayer[assoc_Association] := Scope[
	op = assoc["op"];
	If[op === "null", Return[None]];
	name = Lookup[$FromMXNetName, op, Panic["UnsupportedLayer", "Layer `` is not supported.", op]];
	layerData = $LayerData[name];
	symbol = layerData["Symbol"];
	reader = toLayerReader[name];
	rules = Flatten @ reader @ assoc["param"];
	arrays = layerData["Arrays"];
	If[arrays =!= <||>, 
		$arrayTypes = layerData["Arrays"];
		rules = Join[rules, readArrays[assoc["name"], layerData["MXNet", "Arrays"]]];
	];
	result = symbol @@ rules;
	If[FailureQ[result],
		Panic["InvalidLayerCreated", 
			"Layer named `` couldn't be created, error was ``.\nOriginal inputs were: ``.", 
			assoc["name"], ToString @ result, rules /. _RawArray -> RawArray
		]
	];
	result
];

readArrays[name_, assoc_] :=
	KeyValueMap[
		readSingleArray[name, #1, #2]&,
		assoc
	];

readSingleArray[name_, wl_, mx_] := Block[{},
	wl -> Lookup[$Arrays, "arg:" <> name <> "_" <> mx, 
		Lookup[$Arrays, "aux:" <> name <> "_" <> mx, 
			(* TODO: minor, but better to make this a Reader function keyed off no_bias *)
			Replace[$arrayTypes[wl], n_Nullable :> None]
		]
	]
];

toLayerReader[name_] := Memoized @ makeLayerReaderWriter[name, "Reader"];

toFieldReader[param_, FixedValue[v_]] := param -> v;

toFieldReader[param_, key_] := 
	toFieldReader[param, key, toValueReader @ Lookup[$paramTypes, param]];

toFieldReader[param_, key_, reader_] := 
	param -> Quoted[reader[ALookup[#, key]]];

toFieldReader[param_, key_, {reader_, default_}] := 
	param -> Quoted[Replace[Lookup[#, key], {_Missing :> default, v_ :> reader[v]}]];

toValueReader[t_] := Match[t,
	ListT[SizeT, SizeT | NaturalT] :> readIntList,
	ListT[SizeT, PosIntegerT] :> readIntList /* checkPos,
	ListT[2, SizeT | NaturalT] :> readIntPair,
	ListT[2, PosIntegerT] :> readIntPair /* checkPos,
	EnumT[list_List] :> readEnum[list],
	Defaulting[z_, v_] :> {%[z], v},
	NaturalT :> readInt /* checkNat,
	PosIntegerT :> readInt /* checkPos,
	SizeT :> readInt /* checkPos,
	ScalarT :> readReal,
	BooleanT :> $FromBoolean,
	PoolingFunctionT :> $FromPooling,
	ActivationFunctionT :> $FromActivation,
	Panic["UnreadableType", "No reader for ``.", t]
];

toMissingValueReader[Defaulting[_, v_]] := Function[v];
toMissingValueReader[_] := Panic["MissingRequiredMXNetParameter", "Param `` is missing.", #]&;

readEnum[list_List][str_String] := If[MemberQ[list, str], str, Panic[]]; 
readEnum[_][_] := Panic[];
readReal[real_String] := Internal`StringToDouble[real];
readInt[int_Integer] := int;
readInt[str_String] := FromDigits[str];
readIntPair[str_String] := StringMatch[str, "(" ~~ a:Number ~~ (", " | ",") ~~ b:Number ~~ ")" :> Map[FromDigits, {a,b}]];
readIntList[str_String] := Map[FromDigits, StringSplit[str, "("|", "|")"]];
checkPos[p_] := If[Min[p] < 1, Panic[], p];
checkNat[p_] := If[Min[p] < 0, Panic[], p];

Scan[
	Function[sym,
		sym[val_] := Panic["MalformedMXNetParameter", "Value `` could not be read by ``.", val, sym]
	], 
	{readReal, readInt, readIntPair, readIntList}
];

PackageExport["ToMXNetJSON"]

PackageScope["$MXNetJSONCompact"]

$MXNetJSONCompact = 2;

ToMXNetJSON[net_] := ToMXNetJSON[net, All];

ToMXNetJSON[net_NetP, outputs_] := Scope[
	$Arrays = Association[];
	$MXNodeIDs = Association[];
	$AuxArrays = Association[];
	$AuxArrayMapping = Association[];
	$MXNextID = 0;
	$PortFilter = True&;
	If[outputs === All, 
		oports = OutputPorts[net],
		If[!StringVectorQ[outputs], Panic["BadOutputSpec", "`` is not a valid output spec.", outputs]];
		oports = NetPort["Outputs", #]& /@ outputs;
		If[Length[oports] < Length[Outputs[net]],
			$PortFilter = ContributingNodeFilter[net, oports];
		];
	];
	CollectTo[{$MXNodes, $MXArgNodes}, 
		ScanFields["Inputs", SowMXBatchArgNode, net];
		numInputs = $MXNextID;
		MXScan[net]
	];	
	outputIDs = LookupMXID[oports];
	result = Association[
		"nodes" -> $MXNodes,
		"arg_nodes" -> FromPackedArray[$MXArgNodes], (* work around 314706 *)
		"heads" -> outputIDs
	];
	result = Developer`WriteRawJSONString[result, "Compact" -> $MXNetJSONCompact];
	(* inputNames are the mangled inputs *)
	inputNames = InputNames[net];
	inputMXNames = Keys @ Take[$Arrays, numInputs];
	outputNames = oports[[All, 2]];
	(* remove any nullable arrays *)
	$Arrays = DeleteCases[None] @ $Arrays;
	$AuxArrays = DeleteCases[None] @ $AuxArrays;
	{result, $Arrays, $AuxArrays, $AuxArrayMapping, inputNames, inputMXNames, outputNames}
];


PackageScope["ContributingNodeFilter"]

ContributingNodeFilter[net_, ports_] := Scope[
	graph = PortConnectivityGraph[net];
	contributing = VertexInComponent[graph, ports];
	all = VertexList[graph];
	Data`UnorderedAssociation[
		Thread[all -> False],
		Thread[contributing -> True]
	]
];

PackageExport["ToMXNetSymbol"]

ToMXNetSymbol[net_] := ToMXNetSymbol[net, All];

ToMXNetSymbol[net_NetP, outputs_] := Scope[
	jsondata = ToMXNetJSON[net, outputs];
	mxsymbol = MXSymbolFromJSON[First[jsondata]];
	If[FailureQ[mxsymbol], ReturnFailed[]];
	jsondata[[1]] = mxsymbol;
	Append[jsondata, MXSymbolOutputs[mxsymbol]]
];


$Arrays = Association[]; (* this maps tensor names to their tensor values *)
$MXNodeIDs = Association[]; (* this maps ports to IDs *)
$MXNextID = 0; (* next ID to use *)

$MXNodes = Internal`Bag[]; (* this is a bag of associations *)
$MXArgNodes = Internal`Bag[]; (* this is a bag of argnode ids *)

PackageScope["BatchT"]

SowMXBatchArgNode[EncodedType[_, t_]] := SowMXBatchArgNode[t];
SowMXBatchArgNode[arr_] := SowMXArgNode[BatchT[arr]];

SowMXArgNode[None] := Nothing;

SowMXArgNode[arr_] := Scope[
	If[MemberQ[$aux, Last[$path]] || !$PortFilter[$path], Return[Nothing]];
	id = $MXNextID++;
	name = MXManglePort[$path];
	BagInsert[$MXNodes, Association[
		"op" -> "null", "name" -> name, 
		"param" -> <||>, "inputs" -> {}, 
		"backward_source_id" -> -1
	]];
	$Arrays[name] = arr;
	$MXNodeIDs[$path] = {id, 0};
	BagInsert[$MXArgNodes, id];
	$path
];


PackageScope["SowMXNode"]

(* Usage: SowMXNode[
	"Pooling", <|...|>, 
	{NetPort["Inputs", "Input"], NetPort["Arrays", "foo"]}, 
	{NetPort["Output", "Output"]}
] *)

SowMXNode[op_, hyperParams_, inputs_, outputs_] := Scope[
	If[!$PortFilter[$path], Return[Nothing]];
	$id = $MXNextID++;
	inputIDs = flatIns /@ LookupMXID[inputs];
	BagInsert[$MXNodes, Association[
		"op" -> op, "name" -> MXManglePort[$path], 
		"param" -> hyperParams, "inputs" -> inputIDs, 
		"backward_source_id" -> -1
	]];
	i = 0;
	Block[{$path = Append[$path, "Outputs"]},
		KeyValueScan[sowOutputPort, outputs];
	];
];

sowOutputPort[port_, type_] := $MXNodeIDs[Append[$path, port]] = {$id, i++};

sowOutputPort[port_, l_List] := Scope[
	port = Append[$path, port];
	Do[
		$MXNodeIDs[Append[port, i++]] = {$id, i},
		Length[l]
	];
];


flatIns[pair:{_Integer, _Integer}] := pair;
flatIns[list_List] := Sequence @@ list;

(* TODO: investigate why MXSymbolBind fails if we use / as the separator. *)

PackageScope["MXManglePort"]

MXManglePort[NetPort[p___, i_Integer]] :=
	MXManglePort[NetPort[p]] <> "_output" <> IntegerString[i];

MXManglePort[NetPort[p___]] := StringJoin[".", Riffle[{p}, "."]];


PackageScope["MXUnmanglePort"]

MXUnmanglePort[s_String] := NetPort @@ StringSplit[s, "."];


PackageScope["SowMXConnections"]

SowMXConnections[rules_] := 
	AssociateTo[$MXNodeIDs, 
		PrefixPorts[rules] /. (list_List -> p_NetPort) :> expandInputList[list, p]
	];

expandInputList[dests_, src_] := Scope[
	i = 0;
	Sequence @@ Table[
		dest -> Append[src, i++],
		{dest, dests}
	]
];

PackageScope["LookupMXID"]

(* this can recurse, as it makes doing connections easy. *)
LookupMXID[port_NetPort] := LookupMXID @ Lookup[$MXNodeIDs, port, Panic["NetPortNotFound", "NetPort `` not found, `` available.", port, $MXNodeIDs]];
LookupMXID[list_List] := Map[LookupMXID, list];
LookupMXID[pair:{_Integer, _Integer}] := pair;


PackageScope["MXScan"]
PackageScope["MXScanner"]

MXScan[net_NetP] := Call[net, MXScanner];

MXScanner[_] := MXScanLayer;

MXScanLayer[assoc_] := Scope[
	If[!$PortFilter[$path], Return[]];
	type = assoc["Type"];
	inputs = InputPorts[assoc];
	outputs = Outputs[assoc];
	$aux = $LayerData[type, "AuxArrays"]; (* <- will be used to skip aux arrays by SowMXArgNode *)
	params = MapFields["Arrays", SowMXArgNode, assoc];
	mxinfo = $LayerData[type, "MXNet"];
	mxtype = Lookup[$ToMXNetName, type, Panic["UnsupportedLayer", "The layer type `` is unsupported.", type]];
	dumpAuxArrays[$aux, assoc];
	If[ListQ[fusionRules = mxinfo["FusionRules"]],
		inputIDs = LookupMXID[inputs];
		inputNode = Internal`BagPart[$MXNodes, inputIDs[[1, 1]]+1];
		newname = Replace[inputNode["op"], Append[fusionRules, _ -> $Failed]];
		If[StringQ[newname],
			mxtype = newname;
			inputs = Join[inputNode["inputs"], Rest @ inputIDs];
		];
	];
	hyperWriter = toLayerWriter[type];
	hypers = Association @ hyperWriter[assoc["Parameters"], assoc["Arrays"]];
	SowMXNode[mxtype, hypers, Join[inputs, params], outputs];
];

dumpAuxArrays[{}, _] := Null;
dumpAuxArrays[aux_List, assoc_] := Scope[
	prefix = MXManglePort[$path];
	KeyValueMap[
		Function[
			mxnetName = prefix <> "_" <> #2;
			Set[$AuxArrayMapping[mxnetName], prefix <> ".Arrays." <> #1];
			Set[$AuxArrays[mxnetName], assoc["Arrays", #1]]
		],
		mxinfo[["Arrays", aux]]
	]
];

PackageScope["toLayerWriter"]
PackageScope["toValueWriter"]

Clear[toLayerWriter];

toLayerWriter[name_] := Memoized @ makeLayerReaderWriter[name, "Writer"];

toFieldWriter[param_, key_] := With[
	{writer = toValueWriter @ ALookup[$paramTypes, param]}, 
	key -> Quoted[writer[Slot[param]]]
];

toFieldWriter[param_, FixedValue[v_]] := Nothing;

IntP = SizeT | PosIntegerT | IntegerT | NaturalT;
toValueWriter[t_] := Match[t,
	ListT[_, IntP] :> writeIntList,
	SizeT|PosIntegerT :> IntegerString,
	IntegerT|NaturalT :> TextString,
	ScalarT :> ToString,
	ActivationFunctionT :> $ToActivation,
	BooleanT :> $ToBoolean,
	PoolingFunctionT :> $ToPooling,
	Defaulting[type_, _] :> %[type],
	Identity
];

intString[i_Integer] := If[Negative[i], "-" <> IntegerString[i], IntegerString[i]];
intString[_] := Panic[];
writeIntList[{a_, b_}] := StringJoin["(", intString[a], ", ", intString[b], ")"];
writeIntList[{a_, b_, c_}] := StringJoin["(", intString[a], ", ", intString[b], ", ",  intString[c], ")"];
writeIntList[list_] := StringRiffle[list, {"(", ", ", ")"}];

makeLayerReaderWriter[name_, type_] := Scope[
	mxinfo = $LayerData[name, "MXNet"];
	$paramTypes = $LayerData[name, "Parameters"];
	If[mxinfo === None, Return[{}&]];
	readerWriter = KeyValueMap[
		If[type === "Writer", toFieldWriter, toFieldReader],
		Lookup[mxinfo, "Parameters", <||>]
	];
	If[mxinfo[type] =!= None,
		AppendTo[readerWriter, Quoted @@ mxinfo[type]]
	];	
	ReplaceAll[Compose[Function, readerWriter], Quoted[h_] :> h]
];

ToFrom[rules___] := {Association[rules], Association[Reverse[{rules}, 2]]};

{$ToBoolean, $FromBoolean} = ToFrom[True -> "true", False -> "false"];
{$ToPooling, $FromPooling} = ToFrom[Max -> "max", Mean -> "avg", Total -> "sum"];
{$ToActivation, $FromActivation} = ToFrom[Ramp -> "relu", Tanh -> "tanh", LogisticSigmoid -> "sigmoid"];

PackageScope["FixedValue"]

PackageExport["ToMXNetExecutor"]

General::mxneterr = "An error occured while compiling the net. Please contact technical support."

NNSetUsage @ "
ToMXNetExecutor[net$, outports$, batchsize$, gradmethod$, context$] returns the tuple {executor$, auxmapping$, inputs$, outputs$, encoders$, decoders$}, where the arguments are:
	net$ - the net to make an executor for
	outports$ - a list of output ports to compute, or All
	batchsize$ - either None, or an integer
	gradmethod$ - either None or 'Write'
	context$ - a tuple like {'CPU', 0}
and the output tuple consists of:
	executor$ - an MXExecutorData[$$] object
	auxmapping$ - an association mapping positions of aux arrays to their canonical MXNet names
	inputs$ - an association mapping input ports to NDArray[$$] objects
	outputs$ - an association mapping output ports to NDArray[$$] objects
	encoders$ - a list of encoder functions corresponding to the inputs
	decoders$ - a list of decoder functions corresponding to the outputs
"

ToMXNetExecutor[net_NetP, outputs_, batchSize_, grad_, context_] := Scope[
	$DefaultContext = context;
	res = ToMXNetSymbol[net, outputs];
	If[FailureQ[res], ThrowFailure["mxneterr"]];
	{symbol, arrays, auxArrays, auxArrayMapping, inputNames, inputMXNames, outputNames, outputMXNames} = res;
	$batchSize = Replace[batchSize, None -> 1];
	executor = MXSymbolBind[
		symbol, 
		Map[procArgArray, arrays], 
		"AuxilliaryArrays" -> Map[procArgArray, auxArrays],
		"Context" -> $DefaultContext,
		"GradientUpdateMethod" -> grad
	];
	If[FailureQ[executor], ThrowFailure["badnnexec", executor]];
	inputs = AssociationThread[inputNames, ALookup[executor["ArgumentArrays"], inputMXNames]];
	(* TODO: How do we know the outputs come back in the same order as the JSON head_nodes? *)
	outputs = AssociationThread[outputNames, Values @ executor["OutputArrays"]];
	encoders = Map[ToEncoderFunction[#, batchSize =!= None]&, InputTypes[net]];
	decoders = Map[ToDecoderFunction[#, batchSize =!= None]&, OutputTypes[net]];
	{executor, auxArrayMapping, inputs, outputs, encoders, decoders}
];

(*
hmmm

PackageExport["ToTrainingFunction"]

PackageScope["$TrainingBatchSize"]
$TrainingBatchSize = 64;

ToTrainingFunction[net_NetP, context_:{"CPU", 0}] := Scope[
	$DefaultContext = context;
	{symbol, arrays, auxArrays, inputNames, inputMXNames, outputNames, outputMXNames} = 
		Cached[ToMXNetSymbol, net, All] ! ThrowFailure["badlayer"];
	$batchSize = $TrainingBatchSize;
	executor = MXSymbolBind[
		symbol, 
		Map[procArgArray, arrays], 
		"AuxilliaryArrays" -> Map[procArgArray, auxArrays],
		"Context" -> $DefaultContext,
		"GradientUpdateMethod" -> grad
	];
	If[FailureQ[executor], ThrowFailure["badnnexec", executor]];
	inputs = AssociationThread[inputNames, inputMXNames];
	outputs = AssociationThread[outputNames, Keys @ executor["OutputArrays"]];
	encoders = Map[ToEncoderFunction[#, batchSize =!= None]&, InputTypes[net]];
	TrainingFunction[executor, inputs, outputs, encoders]
];

TrainingFunction[executor_, inputs_, outputs_, encoders_][inputData_] := (
	setter[inputArrays, List /@ ComposeThread[encoders, inputData]];
	MXExecutorForward[executor, $ForceTrainingMode];
	If[$DisableOutputDecoders, Identity, ComposeThread[decoders]] @ 
		Map[Normal /* First, outputArrays]
);
*)

PackageExport["ToEvaluationFunctions"]

PackageScope["$EvaluationBatchSize"]
$EvaluationBatchSize = 128;

ToEvaluationFunctions[net_, context_:{"CPU", 0}] := Scope[
	If[!MissingQ[LayerProp[net, "WLEquivalent"]], Return @ ToEquivalentEvaluationFunctions[net]];
	res = ToMXNetSymbol[net, All];
	If[FailureQ[res], ThrowFailure["mxneterr"]];
	{symbol, arrays, auxArrays, auxArrayMapping, inputNames, inputMXNames, outputNames, outputMXNames} = res;
	(* setup the full batch iterator. ideally this size would be chosen automatically
	based on the target device and the input size etc etc *)
	$batchSize = $EvaluationBatchSize;
	batchExecutor = MXSymbolBind[
		symbol, 
		Map[procArgArray, arrays], 
		"AuxilliaryArrays" -> Map[procArgArray, auxArrays],
		"Context" -> context,
		"GradientUpdateMethod" -> None
	];
	(* make another version of the executor for size-1 inputs *)
	newInputSizes = Map[NDArrayDimensions, batchExecutor["ArgumentArrays"][[inputMXNames]]];
	newInputSizes[[All, 1]] = 1;
	(* disabled for now because of bug *)
	singleExecutor = MXExecutorReshape[batchExecutor, newInputSizes];
	(* make batch and non-batch versions of the encoders/decoders*)
	inputTypes = InputTypes[net];
	outputTypes = OutputTypes[net];
	singleEncoders = Map[ToEncoderFunction[#, False]&, inputTypes];
	singleDecoders = Map[ToDecoderFunction[#, False]&, outputTypes];
	batchEncoders =  Map[ToEncoderFunction[#, True]&, inputTypes];
	batchDecoders =  Map[ToDecoderFunction[#, True]&, outputTypes];
	(* fast path for singleton executor *)
	inputNDArrays = Lookup[singleExecutor["ArgumentArrays"], inputMXNames];
	outputNDArrays = AssociationThread[outputNames, Values @ singleExecutor["OutputArrays"]];
	(* setter function does last-minute checking that the NDArrays match the decoded inputs, etc *)
	setter = VerboseNDArraySet[inputNames];
	{
		EvaluationFunction[singleExecutor, singleEncoders, singleDecoders, inputNDArrays, outputNDArrays, setter],
		BatchEvaluationFunction[batchExecutor, batchEncoders, batchDecoders, inputMXNames, outputMXNames, outputNames, setter]
	}
];

PackageExport["$ForceTrainingMode"]
$ForceTrainingMode = False;

PackageScope["$DisableOutputDecoders"]
$DisableOutputDecoders = False;

PackageExport["EvaluationFunction"]

EvaluationFunction[executor_, encoders_, decoders_, inputArrays_, outputArrays_, setter_][inputData_] := (
	setter[inputArrays, List /@ ComposeThread[encoders, inputData]];
	MXExecutorForward[executor, $ForceTrainingMode];
	If[$DisableOutputDecoders, Identity, ComposeThread[decoders]] @ 
		Map[Normal /* First, outputArrays]
);

PackageExport["BatchEvaluationFunction"]

BatchEvaluationFunction[executor_, encoders_, decoders_, inputMXNames_, outputMXNames_, outputNames_, setter_][inputData_] := 
	AssociationThread[
		outputNames, 
		If[$DisableOutputDecoders, Identity, ComposeThread[decoders]] @ 
		MXEvaluate[
			executor,
			AssociationThread[inputMXNames, inputData],
			encoders,
			outputMXNames,
			setter
		]
	];

ToEquivalentEvaluationFunctions[net_] := Scope[
	inputTypes = InputTypes[net];
	outputTypes = OutputTypes[net];
	singleEncoders = Map[ToEncoderFunction[#, False]&, inputTypes];
	singleDecoders = Map[ToDecoderFunction[#, False]&, outputTypes];
	batchEncoders =  Map[ToEncoderFunction[#, True]&, inputTypes];
	batchDecoders =  Map[ToDecoderFunction[#, True]&, outputTypes];
	With[{equiv = LayerProp[net, "WLEquivalent"]},
	{
		EquivalentEvaluationFunction[Apply[equiv], singleEncoders, singleDecoders],
		EquivalentEvaluationFunction[Function[inputs, MapThread[equiv, inputs]], batchEncoders, batchDecoders]
	}
	]
];


PackageScope["EquivalentEvaluationFunction"]

EquivalentEvaluationFunction[equiv_, encoders_, {decoder_}][inputData_] := 
	List @ decoder @ equiv @ ComposeThread[encoders] @ inputData;


PackageScope["VerboseNDArraySet"]

VerboseNDArraySet[names_][ndarrays_, inputs_] := 
	MapThread[
		$i = 1;
		Function[
			checkInputDims[names[[$i++]], 
				NDArrayDimensions[#1],
				MachineArrayDimensions[#2]
			];
			NDArraySet[#1, #2]
		],
		{ndarrays, inputs}
	];

Clear[checkInputDims];

General::notarray = "Input to network is not a numeric array."
checkInputDims[_, _, $Failed] := ThrowFailure["notarray"];

checkInputDims[name_, {n_, 1}, {n_}] := Null;

checkInputDims[name_, netdims_, datadims_] := 
	If[netdims =!= datadims, dimfail[name, Rest @ netdims, Rest @ datadims]];

General::invscavec = "Data provided to port `` was a ``, expected a scalar."
dimfail[name_, {}, data_] := ThrowFailure["invscavec", name, Switch[Length[data], 1, "vector", 2, "matrix", _, "tensor"]];

General::invdimsnm = "Data provided to port `` has the wrong dimensions (`` instead of ``).";
dimfail[name_, net_, data_] := If[
	Length[net] =!= Length[data],
	ThrowFailure["invranknm", name, Length[data], Length[net]],
	ThrowFailure["invdimsnm", name, data, net]
];


General::badnnexec = "Could not set up an execution context for given network. The error was: ``. Please contact Wolfram Research.";

procArgArray[Nullable[t_]] := procArgArray[t];
procArgArray[ra_RawArray] := ra;
procArgArray[BatchT[t_]] := procArgArray[ChannelT[$batchSize, t]];
procArgArray[BatchT[ScalarT|PosIntegerT|IntegerT|_IndexIntegerT]] := {$batchSize};
procArgArray[t_ChannelT | t_TensorT] := Scope[
	dims = GetTensorDimensions @ t;
	If[!VectorQ[dims, IntegerQ], ThrowFailure["mxneterr"]];
	dims
];

procArgArray[t_] := ThrowFailure["mxneterr"];