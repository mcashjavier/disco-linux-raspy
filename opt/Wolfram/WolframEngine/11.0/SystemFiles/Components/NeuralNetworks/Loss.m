Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]


PackageExport["GetOutputLayers"]

General::nooutput = "Network does not have an \"Output\" port."

GetOutputLayers[layer_Association] := 
	AssociationMap[layer&, OutputNames[layer]];

GetOutputLayers[head_Symbol[assoc_]] := GetOutputLayers[assoc];

GetOutputLayers[NetChain[assoc_]] := GetOutputLayers @ assoc[["Layers", -1]];

GetOutputLayers[NetGraph[assoc_]] := Scope[
	outputs = OutputNames[assoc];
	outputPorts = Thread[NetPort["Outputs", outputs]];
	outputSources = Lookup[assoc["Edges"], outputPorts];
	outputLayers = Lookup[assoc["Vertices"], outputSources[[All, 2]]];
	AssociationThread[outputs, outputLayers]
];


PackageScope["HasLossPortQ"]

HasLossPortQ[net_] := MemberQ[OutputNames[net], "Loss"];


PackageScope["LossLayerQ"]

LossLayerQ[_Symbol[assoc_Association] ? System`Private`NoEntryQ] := 
	LossLayerQ[assoc];

LossLayerQ[assoc_Association] :=
	MatchQ[Outputs[assoc], <|_ -> ScalarT|>];

LossLayerQ[_] := False;


PackageScope["LossTypeQ"]

LossTypeQ[ScalarT] := True;
LossTypeQ[_] := False;


PackageExport["LinearRegressionOutputLayer"]

PackageExport["AttachLoss"]

Clear[AttachLoss];

NetTrain::invlossport = "Specified loss port `` is not a scalar value, the sum of its components will be minimized instead."
General::misslossport = "The net has no output called ``."

AttachLoss[net_, All] := AttachLoss[net, OutputNames[net]];

AttachLoss[net_, spec_String] := AttachLoss[net, {spec}];

AttachLoss[net_, ports_List ? Developer`StringVectorQ] := Scope[
	outputs = Outputs[net];
	Do[
		If[!MatchQ[
			Lookup[outputs, name, ThrowFailure["misslossport", name]],
			ScalarT | TensorT[1, {1}]],
				Message[NetTrain::invlossport, name]
		],
		{name, ports}
	];
	{net, NetPort[], ports}
];

AttachLoss[net_, Automatic] := Scope[
	If[HasLossPortQ[net], Return[{net, NetPort[], {"Loss"}}]];
	iAttachLoss[net, GetOutputLayers[net]]
];

General::duploss = "Network already has a \"Loss\" port."
AttachLoss[net_, lossLayer_ ? NetLayerQ] := Scope[
	If[HasLossPortQ[net], ThrowFailure["duploss"]]; 
	iAttachLoss[net, <|getSingleOutput[net] -> Normal[lossLayer]|>]
];

General::inapploss = "Cannot connect loss layer to output of network."

General::nouniqueout = "Net should have a single output port when specifying a single loss layer.";
getSingleOutput[net_] := 
	Match[OutputNames[net], 
		{name_} :> name,
		ThrowFailure["nouniqueout"];
	];

iAttachLoss[net_, bindings_] := Scope[
	otypes = Outputs[net];
	{layers, edges} = Flatten /@ Transpose @ KeyValueMap[
		MakeLossData[#1, #2, Lookup[otypes, #1]]&,
		bindings
	];
	lossGraph = NetGraph[<|"Net" -> net, layers|>, edges];
	If[FailureQ[lossGraph], ThrowFailure["inapploss"]];
	{lossGraph, NetPort["Vertices", "Net"], OutputNames[lossGraph]}
];

General::nelossport = "Can't attach loss to non-existent output port ``.";

(*
AttachLoss[net_, lossSpec:{Rule[_String, _?LossLayerQ]..}] := Scope[
	unknown = Complement[Keys[lossSpec], OutputNames[net]];
	If[unknown =!= {}, ThrowFailure["nelossport", First @ unknown]];
	graph = NetGraph[<|"Model" -> net, lossSpec|>, Map[NetPort["Model", #] -> #&, Keys[lossSpec]]];
	{graph, NetPort["Model"], {"Loss"}}
];
*)

General::invlossspec = "Loss specification should be a loss layer, Automatic, or an output port or list of ports.";
AttachLoss[net_, _] := ThrowFailure["invlossspec"];


PackageScope["MakeLossData"]

General::badlosslayer = "Could not deduce a loss to use for port `` based on the final layer (``)."

MakeLossData[name_, assoc_, type_] := Scope[
	layer = Switch[assoc["Type"],
		"MeanAbsoluteLoss" | "MeanSquaredLoss" | "CrossEntropyLoss",
			$Raw[assoc],
		"Softmax",
			CrossEntropyLossLayer["Target" -> toTargetType[type]],
		_,
			MeanSquaredLossLayer["Target" -> toTargetType[type]]
	];
	If[FailureQ[layer], ThrowFailure["badlosslayer", name, assoc["Type"] <> "Layer"]];
	layerName = name <> "Loss";
	{layerName -> layer, {
		NetPort["Net", name] -> NetPort[layerName, "Input"],
		NetPort[name] -> NetPort[layerName, "Target"],
		NetPort[layerName, "Loss"] -> NetPort[layerName]
	}}
];

Clear[toTargetType];

toTargetType[DecodedType[HoldPattern @ NetDecoder["Class", assoc_], _]] := NetEncoder[{"Class", assoc["Parameters", "Labels"]}];
toTargetType[DecodedType[HoldPattern @ NetDecoder["Image", assoc_], t_]] := NetEncoder[{"Image", Rest @ GetTensorDimensions[t], assoc["Parameters", "ColorSpace"]}];
toTargetType[DecodedType[HoldPattern @ NetDecoder["Scalar", _], t_]] := NetEncoder["Scalar"];
toTargetType[DecodedType[_, t_]] := toTargetType[t];
toTargetType[t_TensorT | t_ChannelT] := GetTensorDimensions[t];
toTargetType[t_] := Panic["BadAutoLossTargetType", "Don't know how to deal with ``.", t];

PackageScope["ToLossFunction"]

General::lfnotrealout = "Output of custom loss function should be a single real number."
General::lfnotsymbolic = "Custom loss function must support symbolic differentiation."

(* this is an experimental WIP, not used anywhere *)
ToLossFunction[f_Function, sz_] := Scope[
    vars = Array[Subscript[x, #]&, sz];
    testres = Internal`UnsafeQuietCheck[f[RandomReal[1, sz]]];
    If[!Developer`MachineRealQ[testres], ThrowFailure["lfnotrealout"]];
    res = Internal`UnsafeQuietCheck[f[vars]];
    If[FailureQ[res], ThrowFailure["lfnotsymbolic"]];
    derivative = D[res, {vars}];
    derivative = Compose[Hold, derivative] /. Subscript[x, i_] :> Part[cvar, i];
    cdfunc = Compile @@ Prepend[derivative, {{cvar, _Real, 1}}];

    {Compile @@ cdargs, Function @@ Prepend[derivative, cvar]}
];


PackageExport["DeleteTrainingLayers"]

(* DeleteTrainingLayers will be completely deleted at some point.
NetTake is strictly more powerful *)

DeleteTrainingLayers::arg1 = "First argument should be a NetChain or NetGraph."

DeleteTrainingLayers[_] := ToFailure[DeleteTrainingLayers::arg1];
