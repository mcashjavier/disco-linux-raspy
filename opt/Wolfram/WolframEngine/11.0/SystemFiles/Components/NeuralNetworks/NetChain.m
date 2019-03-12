Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]
PackageImport["Developer`"]


PackageExport["NetChain"]

SetupGenericDispatch[NetChain];

(ng_NetChain ? System`Private`HoldEntryQ) := UseMacros @ RuleCondition @ CatchFailure[NetChain, make[ng]];

SetHoldAll[make];

make[nc:NetChain[<|"Type" -> "Chain", ___|>]] := 
	System`Private`HoldSetNoEntry[nc];

make[NetChain[layers_Association | layers_List, rules___Rule]] := 
	toNetChain[toLayersAssoc[layers], {rules}];

make[ng_NetChain] /; Internal`PatternFreeQ[Unevaluated[ng]] := 
	(Developer`CheckArgumentCount[ng, 1, 1]; $Failed);

make[_] := Fail;

NetChain::empty = "NetChain objects must contain at least one vertex."

toNetChain[layers_, rules_] := Scope[
	If[Length[layers] === 0, ThrowFailure["empty"]];
	ins = Normal @ layers[[All, "Inputs"]];
	outs = Normal @ layers[[All, "Outputs"]];
	conns = Flatten @ MapThread[glueLayers, {Prepend[outs, None], Append[ins, None]}];
	(* handle custom type specifications *)
	inputs = ins[[1,2]];
	outputs = outs[[-1,2]];
	Which[
		rules === {}, 
			Null,
		ListQ[rules],
			$parseinputhead = NetChain;
			kin = Keys[inputs];
			kout = Keys[outputs];
			extra = Complement[Keys[rules], kin, kout];
			If[extra =!= {} && FreeQ[rules, $Raw], ThrowFailure["netinvcport", First[extra]]];
			Set[inputs[#1],  ParseInputSpec[#1,  inputs[#1],  #2]]& @@@ FilterRules[rules, kin];
			Set[outputs[#1], ParseOutputSpec[#1, outputs[#1], #2]]& @@@ FilterRules[rules, kout],
		AssociationQ[rules], (* only used internally by NetChainTakeDrop *)
			inputs = Lookup[rules, "Inputs", inputs];
			outputs = Lookup[rules, "Outputs", outputs];
	];
	checkMultiIn[inputs, NetChain];
	assoc = <|
		"Type" -> "Chain",
		"Layers" -> layers,
		"Connections" -> conns,
		"Inputs" -> inputs,
		"Outputs" -> outputs
	|>;
	ConstructWithInference[NetChain, assoc]
];

NetChain::netinvcport = "`` is neither a valid input or output port for the given NetChain.";

glueLayers[None, bname_ -> b_] :=
	NetPort["Layers", bname, "Inputs", #] -> NetPort["Inputs", #]& /@ Keys[b]; 

glueLayers[aname_ -> a_, None] :=
	NetPort["Outputs", #] -> NetPort["Layers", aname, "Outputs", #]& /@ Keys[a];

General::incompports = "Output of layer `` cannot be connected to input of layer ``.";
glueLayers[aname_ -> a_, bname_ -> b_] := Scope[
	{aport, atype} = get1port[aname, "Outputs", a];
	{bport, btype} = get1port[bname, "Inputs", b];
	res = UnifyTypes[atype, btype];
	If[FailureQ[res], ThrowFailure["incompports", aname, bname]];
	bport -> aport
];

General::notuport = "Layer `` should have exactly one `` port.";
get1port[name_, ptype_, <|portname_ -> porttype_|>] := {NetPort["Layers", name, ptype, portname], porttype};
get1port[name_, ptype_, _] := ThrowFailure["notuport", name, StringDrop[ptype, -1]];


PackageExport["$NetChainInteractivity"]

$NetChainInteractivity := !$CloudEvaluation;

PackageScope["netChainGrid"]

netChainGrid[assoc_] := Scope[
	UnpackAssociation[assoc, layers, inputs, outputs];
	rows = ToList[
		KeyValueMap[toInputRow, inputs],
		KeyValueMap[toLayerRow, layers],
		KeyValueMap[toOutputRow, outputs]
	];
	grid = Grid[rows, Alignment -> Left, Spacings -> 1.1];
	If[$NetChainInteractivity,
		DynamicModule[{grid = grid, assoc = RemoveRawArrays[assoc], opart, part, selected = Null},
			Dynamic[
				Column[{
					EventHandler[grid, "MouseClicked" :> If[
						ListQ[part = MouseAnnotation[]],
						If[opart === part, 
							selected = Null; opart = Null,
							selected = Part[assoc, Sequence @@ part]; opart = part;
						];
					]], 
					fmtSelected[selected, part]
				}, Spacings -> 1],
				TrackedSymbols :> {selected}
			]
		],
		grid
	]
];


Clear[fmtSelected];

fmtSelected[Null, _] := Nothing;
fmtSelected[type_, {"Inputs"|"Outputs", name_}] := Item[typeInfo[name -> type], Alignment -> {Center, Center}];
fmtSelected[layer_, {"Layers", name_}] := Item[itemInfo[name -> layer], Alignment -> {Center, Center}];

(* TODO: Handle last layer properly *)
toLayerRow[name_, assoc_] := 
	selector["Layers", name] /@ {Style[name, Gray], $TypeToSymbol[assoc["Type"]], fmtItem @ First[assoc["Outputs"]]};

toInputRow[name_, type_] := 
	selector["Inputs", name] /@ {"", name, fmtItem @ type};

toOutputRow[name_, type_] := 
	selector["Outputs", name] /@ {"", name, fmtItem @ type};

selector[part___] := If[$NetChainInteractivity, MouseAppearance[Annotation[#, {part}, "Mouse"], "LinkHand"]&, Identity];

DefineCustomBoxes[NetChain, 
	NetChain[assoc_Association] :> formatNetChain[assoc]
];

formatNetChain[assoc_Association] := 
	MakeCustomHeadBox["NetChain", netChainGrid[assoc], Automatic];


PackageScope["GetLayers"]

GetLayers[HoldPattern @ NetChain[assoc_Association]] := 
	Map[ConstructLayer, assoc["Layers"]];


NetChain /: Take[nc_NetChain, spec_] := CatchFailure[NetChain, NetChainTakeDrop[Take, nc, spec]];
NetChain /: Drop[nc_NetChain, spec_] := CatchFailure[NetChain, NetChainTakeDrop[Drop, nc, spec]];


NetChain::invtakespec = "Invalid `` specification."

NetChain::invlayername = "`` is not the name of layer."

$LayerNameP = _String | _Integer;

NetChainTakeDrop[f_, HoldPattern @ NetChain[assoc_Association], spec:$LayerNameP | {$LayerNameP, $LayerNameP}] := Scope[
	layers = assoc["Layers"];
	first = First[layers];
	last = Last[layers];
	len = Length[layers];
	keys = Keys[layers];
	spec = spec /. str_String :> IndexOf[keys, str, ThrowFailure["invlayername", str]];
	newLayers = Internal`UnsafeQuietCheck[f[layers, spec], $Failed];
	If[FailureQ[newLayers] || Length[newLayers] == 0, ThrowFailure["invtakespec", f]];
	newSpecs = KeyTake[assoc, {"Inputs", "Outputs"}];
	If[First[newLayers] =!= First[layers], KeyDropFrom[newSpecs, "Inputs"]];
	If[Last[newLayers] =!= Last[layers], KeyDropFrom[newSpecs, "Outputs"]];
	toNetChain[newLayers, newSpecs]
];

NetChainTakeDrop[f_, _, _] := ThrowFailure["invtakespec", f];

(* Dispatch *)

$TypeToSymbol["Chain"] = NetChain;

InferenceRulesScanner["Chain"] = Function[
	SowInferenceRules[#Connections];
	ScanFields["Layers", ScanInferenceRules, #];
]

FullySpecifiedTest["Chain"] = Function[
	And[
		AllTrue[#Inputs, FullySpecifiedTypeQ],
		AllTrue[#Outputs, FullySpecifiedTypeQ],
		AllTrue[#Layers, FullySpecifiedNetQ]
	]
];

ConcreteTest["Chain"] = Function[
	AllTrue[#Layers, ConcreteNetQ]
];

InitializedTest["Chain"] = Function[
	AllTrue[#Layers, InitializedNetQ]
];

PortGraphBuilder["Chain"] = Function[
	MapAtFields["Layers", BuildPortGraph, #];
	Scan[SowPortGraphEdge, #Connections];
];

MXScanner["Chain"] := Function[
	SowMXConnections[#Connections];
	ScanFields["Layers", MXScan, #];
];

PrepareForExport["Chain"] = 
	MapAtFields["Layers", Call[PrepareForExport]]


PackageExport["DeleteTrainingLayers"]

DeleteTrainingLayers[nc_NetChain] := Scope[
	lossLayers = Keys @ Select[Normal[nc]["Layers"], #Type === "Dropout"&];
	If[lossLayers === {}, Return[nc]];
	DeleteLayers[nc, lossLayers]
];


PackageExport["DeleteLayers"]

DeleteLayers[nc_NetChain, deleteList_List] := Scope[
	data = Normal[nc];
	layers = data["Layers"];
	If[DigitStringKeysQ[layers], noKeys = True];
	layerNames = AssociationThread[#, #]& @ Keys[layers];
	deleteList = PartElse[layerNames, #, badLayer[#]]& /@ deleteList;
	KeyDropFrom[layers, deleteList];
	If[noKeys, layers = First @ RemapKeys[layers]];
	portHints = Join[
		If[MemberQ[deleteList, First @ layerNames], {}, Normal @ Inputs[nc]], 
		If[MemberQ[deleteList, Last @ layerNames], {}, Normal @ Outputs[nc]]
	];
	CatchFailure[NetChain, toNetChain[layers, (#1 -> $Raw[#2]& @@@ portHints)]]
];

NetChain::invlayerspec = "The layer `` does not exist.";
badLayer[spec_] := ThrowFailure["invlayerspec", spec];
