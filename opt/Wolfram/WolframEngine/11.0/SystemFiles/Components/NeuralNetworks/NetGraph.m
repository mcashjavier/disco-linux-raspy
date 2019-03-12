Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]
PackageImport["Developer`"]

PackageScope["$Raw"]

PackageExport["NetGraph"]

SetupGenericDispatch[NetGraph];

(ng_NetGraph ? System`Private`HoldEntryQ) := 
	UseMacros @ RuleCondition @ CatchFailure[NetGraph, make[ng]];

SetHoldAll[make];

make[NetGraph[chain_NetChain]] := Scope[
	layers = GetLayers[chain];
	NetGraph[layers, Table[i -> (i+1), {i, Length[layers]-1}]]
];

make[NetGraph[layers_Association | layers_List, conns_List, rules___Rule]] := 
	toNetGraph[toLayersAssoc[layers], Flatten @ conns, {rules}];

make[ng:NetGraph[<|"Type" -> "Graph", ___|>]] :=
	System`Private`HoldSetNoEntry[ng];

make[ng_NetGraph] /; Internal`PatternFreeQ[Unevaluated[ng]] :=
	(Developer`CheckArgumentCount[ng, 2, 2]; $Failed);

make[_] := Fail;


PackageScope["toLayersAssoc"]

toLayersAssoc[list_List] := Scope[
	layers = Map[toNetLayer, list];
	AssociationThread[IntegerString[Range[Length[layers]]], layers]
];

NetGraph::notstrkey = "All layer keys must be strings."

toLayersAssoc[assoc_Association] := (
	If[!StringVectorQ[Keys[assoc]], 
		If[VectorQ[Keys[assoc], IntegerQ],
			Return @ toLayersAssoc[KeyMap[IntegerString, assoc]]
		];
		ThrowFailure["notstrkey"]
	];
	DeleteCases[Nothing] @ Map[toNetLayer, assoc]
);

toNetLayer[Identity] := Nothing;
toNetLayer[i_Integer] := Normal @ DotPlusLayer[i];
toNetLayer[f:Alternatives[LogisticSigmoid, Ramp, Tanh]] := Normal @ ElementwiseLayer[f];
toNetLayer[$Raw[e_]] := e;

General::netinvnodes = "`` is not a net function."
toNetLayer[e_] := If[NetLayerQ[e], Normal @ e, ThrowFailure["netinvnodes", e]];


PackageScope["toNetGraph"]

NetGraph::acyclic = "The connectivity of layers within a NetGraph must be acyclic."
NetGraph::empty = "NetGraph objects must contain at least one vertex."

toNetGraph[vertices_Association, edges_List, rules_List] := Scope[
	$vertices = vertices;

	If[Length[vertices] === 0, ThrowFailure["empty"]];

	(* global ports that are mentioned explicitly *)
	$iexplicit = $oexplicit = {};

	(* mapping from layer input and output ports to their types *)
	$itypes = $otypes = Association[];
	KeyValueScan[collectPortTypes, $vertices];

	(* canonicalize edges *)
	edges = cedge /@ edges;

	(* find in and out ports that aren't bound *)
	ofree = Complement[Keys[$otypes], Flatten @ Values @ edges];
	ifree = Complement[Keys[$itypes], Flatten @ Keys @ edges];

	(* if these types clash that'll get picked up in the inference step *)
	inputs = KeyMap[Last] @ KeyTake[$itypes, ifree];
	outputs = KeyMap[Last] @ KeyTake[$otypes, ofree];

	(* add on connections to virtual ports for the unbound inputs and outputs *)
	edges = ToList[
		edges, 
		virtIn /@ ifree, 
		Flatten @ KeyValueMap[virtOut, GroupBy[ofree, Last]]
	];

	(* explicit ports need to be created, if necessary, from scratch *)
	addExplicitPorts[inputs, $iexplicit];
	addExplicitPorts[outputs, $oexplicit];

	If[!DuplicateFreeQ[edges[[All, 1]]],
		edges = Flatten @ Map[handleDuplicateEdge, GatherBy[edges, First]];
	];

	(* sort the edges into topological order *)
	graph = NetPortGraph[edges];
	If[!AcyclicGraphQ[graph] || !LoopFreeGraphQ[graph], ThrowFailure["acyclic"]];

	(* ensure the vertices are in topological order, so that when we scan
	them to feed to MXNet a node can assume its inputs have already been scanned. *)
	vertexOrder = TopologicalSort @ graph;
	$vertices = KeySortBy[$vertices, -First[FirstPosition[vertexOrder, #]]&];

	(* handle custom type specifications *)
	scanTypeHints[rules, inputs, outputs];

	(* make sure that inputs that are used first come earlier. should not matter but
	a temporary workaround for a problem tracking the input vs label in NetTrain. *)
	inputs = KeySortBy[inputs, FirstPosition[edges, #]&];
	
	(* check that none of the inputs are ListTs, because those are pseudo objects that
	are an accounting trick rather than something that MXNet knows about *)
	checkMultiIn[inputs, NetGraph];

	(* build the association*)
	assoc = Association[
		"Type" -> "Graph",
		"Inputs" -> inputs, 
		"Outputs" -> outputs,
		"Vertices" -> $vertices,
		"Edges" -> edges
	];
	ConstructWithInference[NetGraph, assoc]
];

SetHoldRest[scanTypeHints];

scanTypeHints[{}, _, _] := Null;

scanTypeHints[rules_, inputs_, outputs_] := Scope[
	$parseinputhead = NetGraph;
	Scan[
		Match[#,
			(name_String -> spec_) :> Which[
				KeyExistsQ[inputs, name], 
					setType[inputs[name], ParseInputSpec, spec],
				KeyExistsQ[outputs, name],
					setType[outputs[name], ParseOutputSpec, spec],
				True,
					ThrowFailure["netinvgport", name]
			],
			(NetPort["Inputs", name_] -> spec_) :> 
				setType[inputs[name], ParseInputSpec, spec],
			(NetPort["Outputs", name_] -> spec_) :> 
				setType[outputs[name], ParseOutputSpec, spec],
			ThrowFailure["invtypehint", #]
		]&,
		rules
	];
];

NetGraph::invtypehint = "`` should be a rule of the form \"port\" -> type."

SetHoldFirst[setType];

setType[slot:(_[name_]), func_, spec_] := If[
	MissingQ[slot], ThrowFailure["netinvgport", name], 
	Set[slot, func[name, slot, spec]]
];

NetGraph::netinvgport = "`` is neither a valid input or output port for the given NetGraph.";


PackageScope["checkMultiIn"]

General::novarin = "Port `` expects multiple connections; such ports are not permitted as inputs to a ``."
checkMultiIn[assoc_, head_] := 
	KeyValueMap[
		If[MatchQ[#2, _List | _ListT],ThrowFailure["novarin", #1, head]]&,
		assoc
	];

SetHoldFirst[addExplicitPorts];

addExplicitPorts[var_, {}] := Null;

addExplicitPorts[var_, list_] := AssociateTo[var, Thread[list -> TypeT]];

Clear[handleDuplicateEdge];

handleDuplicateEdge[{rule_}] := rule;

handleDuplicateEdge[list_List] /; MatchQ[$itypes[list[[1,1]]], _ListT | _List] :=
	list[[1,1]] -> Flatten[list[[All, 2]]];

NetGraph::dupedges =  "Multiple vertices have been connected to the ``.";

handleDuplicateEdge[list_List] := If[
	True,
	ThrowFailure["dupedges", NetPortForm @ list[[1,1]]]
];

handleDuplicateEdge[other_] := Panic[];

collectPortTypes[i_, assoc_] := (
	AssociateTo[$itypes, KeyValueMap[NetPort["Vertices", i, "Inputs", #1] -> #2&, assoc["Inputs"]]];
	AssociateTo[$otypes, KeyValueMap[NetPort["Vertices", i, "Outputs", #1] -> #2&, assoc["Outputs"]]];
);

NetGraph /: Part[NetGraph[assoc_Association], part___] := Part[assoc, part];


gfail[msg_, args___] := ThrowTaggedFailure["InvalidGraph", Evaluate["net" <> msg], args];

(* this takes care of remapping pure int keys (i.e. keys from a NetGraph
that used a list spec), as well as preserving port hints (i.e. Encoded and DecodedTypes *)
makeSubNetGraph[oldnet_, vertices_, edges_] := Scope[
	{ledges, redges} = KeysValues[edges];
	{iports, oports} = PortTypes[oldnet];
	hints = Join[
		Cases[iports, Rule[port_, type_] /; !FreeQ[redges, port] :> 
			Rule[port, $Raw[type]]],
		Cases[oports, Rule[port_, type_] /; !FreeQ[ledges, port] :> 
			Rule[port, $Raw[type]]]
	];
	If[DigitStringKeysQ[oldnet["Vertices"]],
		{vertices, mapping} = RemapKeys[vertices];
		rule = NetPort["Vertices", vert_, rest__] :> NetPort["Vertices", mapping @ vert, rest];
		edges = edges /. rule;
		hints = hints /. rule;
	];
	toNetGraph[vertices, $Raw /@ edges, hints]
];


PackageScope["NetPortGraph"]

NetPortGraph[list_] := Graph[
	Flatten[Thread /@ list][[All, All, 1;;2]], 
	VertexLabels -> "Name"
];


PackageScope["FullNetPortGraph"]

FullNetPortGraph[edges_, vertices_] := Scope[
	rules = Flatten[{
		Thread /@ edges,
		KeyValueMap[Function[
			vert = NetPort["Vertices", #1];
			{Thread[vert -> Thread[NetPort["Vertices", #1, "Inputs", InputNames[#2]]]], 
			 Thread[Thread[NetPort["Vertices", #1, "Outputs", OutputNames[#2]]] -> vert]}], 
			vertices
		]
	}];
	Graph[rules, VertexLabels -> Placed["Name",Tooltip]]
];


typeMatchQ[s:Except[_List|_ListT], ListT[_, t_]] := typeMatchQ[s, t];
typeMatchQ[s_, t_] := (s === t) || !FailureQ[UnifyTypes[s, t]];

typeMatchTupleQ[a_, ListT[_, b_]] := typeMatchQ[a, b];
typeMatchTupleQ[_, _] := False;


(* when someone specifies 1 -> 2, we must lookup the first input of 1 and 
	the first output of 2 *)
cportAuto[vertex_, type_] :=
	cport[vertex, type, First @ Keys @ PartElse[$vertices, toVertex[vertex], type, badVertex[vertex]]];

(* check the port exists and canonicalize it *)
General::invport = "The `` does not exist.";
cport[vertex_, parts___] := Scope[
	vertex = toVertex[vertex];
	port = NetPort["Vertices", vertex, parts];
	(* check the provided port actually exists *)
	PartElse[
		$vertices, vertex, parts,
		ThrowFailure["invport", NetPortForm @ port]
	];
	port
];

toVertex[n_ ? Internal`PositiveMachineIntegerQ] := toVertex[IntegerString[n]];
toVertex[name_String /; KeyExistsQ[$vertices, name]] := name;
toVertex[spec_] := badVertex[spec];

NetGraph::invvertex = "The vertex `` does not exist.";
badVertex[spec_] := ThrowFailure["invvertex", spec];

ciport[NetPort[p:NetPortElemP, name_String]] := cport[p, "Outputs", name];
ciport[p:NetPortElemP] := cportAuto[p, "Outputs"];
ciport[NetPort[name_String]] := explicitPort[$otypes, $iexplicit, NetPort["Inputs", name]];

NetGraph::invedgesrc = "`` is not a valid source for an edge."
ciport[e_] := ThrowFailure["invedgesrc", e];

coport[NetPort[p:NetPortElemP, name_String]] := cport[p, "Inputs", name];
coport[p:NetPortElemP] := cportAuto[p, "Inputs"];
coport[NetPort[name_String]] := explicitPort[$itypes, $oexplicit, NetPort["Outputs", name]];

NetGraph::invedgedst = "`` is not a valid destination for an edge."
coport[e_] := ThrowFailure["invedgedst", e];

SetHoldAll[explicitPort];
explicitPort[typesym_, listsym_, p_] := (
	AppendTo[listsym, Last[p]];
	If[!KeyExistsQ[typesym, p], typesym[p] = TypeT]; 
	p
);

Clear[cedge];

cedge[$Raw[edge_]] := (
	Cases[edge, Alternatives[
		NetPort["Outputs", name_] /; AppendTo[$oexplicit, name],
		NetPort["Inputs", name_] /; AppendTo[$iexplicit, name]],
		Infinity];
	edge
);

cedge[ins_List -> out_] := Scope[
	oport = coport[out]; otype = $itypes[oport];
	iports = Table[
		iport = ciport[in]; itype = $otypes[iport];
		If[!typeMatchTupleQ[itype, otype],
			edgePanic[iport, oport, itype, otype]
		]; 
		iport
	,
		{in, ins}
	];
	oport -> iports
];

cedge[in_ -> outs_List] := Scope[
	iport = ciport[in]; itype = $otypes[iport];
	oports = Table[
		oport = coport[out]; otype = $itypes[oport];
		If[!typeMatchTupleQ[otype, itype],
			edgePanic[iport, oport, itype, otype]
		]; 
		oport
	,
		{out, outs}
	];
	oports -> iport
];

cedge[in_ -> (mid_ -> out_)] := Sequence[
	cedge[in -> mid],
	cedge[mid -> out]
];
	
cedge[in_ -> out_] := Scope[
	iport = ciport[in];  itype = $otypes[iport]; 
	oport = coport[out]; otype = $itypes[oport];
	If[!typeMatchQ[itype, otype],
		edgePanic[iport, oport, itype, otype]
	];
	oport -> iport
];

General::invedge = "`` is not a valid edge specification.";
cedge[e_] := ThrowFailure["invedge", e];

General::inctyp = "Incompatible types for `` (``) and `` (``).";

edgePanic[iport_, oport_, itype_, otype_] := 
	ThrowFailure["inctyp", NetPortForm[iport], TypeForm[itype], NetPortForm[oport], TypeForm[otype]];

Clear[virtOut];

virtOut[name_, {p_NetPort}] := NetPort["Outputs", name] -> p;

(* make a unique output port for each clashing dangling output *)
virtOut[name_, list_] := (
	(* this removes the original clashing port from the outputs var in toNetGraph *)
	KeyDropFrom[outputs, name]; 
	Table[
		explicitPort[$itypes, $oexplicit, NetPort["Outputs", name <> IntegerString[i]]] -> list[[i]], 
		{i, Length[list]}
	]
);

virtIn[ns:NetPort[__, name_]] := ns -> NetPort["Inputs", name];

uniqueLabelling[list_] :=
	Values @ Sort @ Flatten @ KeyValueMap[labelGroup, PositionIndex[list]];

labelGroup[name_, {ind_}] := ind -> name;
labelGroup[name_, inds_List] := 
	Table[inds[[i]] -> name <> IntegerString[i], {i, Length[inds]}];

Language`SetMutationHandler[NetGraph, netGraphHandler];

SetHoldAllComplete[netGraphHandler];

netGraphHandler[Set[_Symbol, _]] := Language`MutationFallthrough;
netGraphHandler[Set[sym_Symbol[[pos_]], newlayer_]] := Scope[
	res = ReplaceLayer[sym, pos -> newlayer];
	If[FailureQ[res], res, sym ^= res]
];


PackageExport["ReplaceLayer"]

ReplaceLayer[HoldPattern[NetGraph[assoc_Association]], pos_ -> newlayer_] := CatchFailure @ Scope[
	If[!NetLayerQ[newlayer], ReturnFailed[]];
	newlayer = Normal[newlayer];
	vertices = assoc["Vertices"];
	Which[
		StringQ[pos],		
			If[KeyExistsQ[vertices, pos],
				vertices[[pos]] = newlayer,
				AppendTo[vertices, pos -> newlayer];
			],
		IntegerQ[pos],
			If[1 <= Abs[pos] <= Length[vertices],
				vertices[[pos]] = newlayer,
				ThrowFailure[NetGraph::oorpart, pos];
			],
		True,
			ThrowFailure[NetGraph::badpart, pos];
	];
	assoc["Vertices"] = vertices;
	ConstructWithInference[NetGraph, assoc]
];


DefineCustomBoxes[NetGraph, 
	NetGraph[assoc_Association] :> formatNetGraph[assoc]
];

formatNetGraph[assoc_Association] :=
	MakeCustomHeadBox["NetGraph", netGraphPlot[assoc], Top -> Scaled[3]];


vportType[NetPort["Vertices", id_]] := $TypeToSymbol[vertices[id, "Type"]];
vportType[NetPort[_, name_]] := Null;

NetTypeColor[name_] := Lookup[$VertexColorScheme, name, Gray];

$Red = Hue[0, 0.7, 0.9];
$Green = Hue[0.22, 1, 0.6];
$Yellow = Hue[0.13, 0.8, 0.85];

$VertexColorScheme = <|
	NetGraph -> 					Black,
	NetChain -> 					Black,
	DotPlusLayer -> 				GrayLevel[0.35],
	CrossEntropyLossLayer -> 		RGBColor[0.8, 0.2, 0.8],
	MeanAbsoluteLossLayer -> 		Lighter[Magenta,.2],
	MeanSquaredLossLayer ->			Lighter[Magenta,.4],
	ElementwiseLayer -> 			RGBColor[0.29, 0.29, 0.83],
	FlattenLayer -> 				RGBColor[1., 0.4, 0.],
	SummationLayer -> 				RGBColor[1., 0.81, 0.],
	ConvolutionLayer ->				Hue[0.61, 0.75, 1],
	BatchNormalizationLayer -> 		GrayLevel[0.8],
	DropoutLayer -> 				GrayLevel[0.7],
	CatenateLayer -> 				Darker[Red, .1],
	SoftmaxLayer -> 				RGBColor[0.78, 0.64, 0.98],
	PoolingLayer -> 				Darker[$Green, 0.05],
	DeconvolutionLayer -> 			Darker[$Green, 0.25],
	TotalLayer -> 					Brown,
	EmbeddingLayer -> 				White
|>;

PackageExport["$NetGraphInteractivity"]

$NetGraphInteractivity := !$CloudEvaluation;

PackageScope["netGraphPlot"]

netGraphPlot[assoc_Association] := Scope[
	UnpackAssociation[assoc, vertices:"Vertices", edges:"Edges", inputs:"Inputs", outputs:"Outputs"];
	edges = List @@@ Flatten[Thread /@ Reverse[edges, 2]];
	edges = DeleteDuplicatesBy[edges, ReplaceAll[p_NetPort :> Take[p, 2]]]; 
	(* ^ temp workaround for 316828 *)
	edgeTypes = Extract[assoc, List @@@ edges[[All, 1]]];
	edges = edges /. p_NetPort :> Take[p, 2];
	{edges, vports} = Labelling[edges, 2];
	labels = vports[[All, 2]];
	bigmode = Length[vertices] > 16;
	{opTypes, opNames} = Labelling @ Map[vportType, vports];
	opStyles = Map[NetTypeColor, opNames];
	Deploy @ If[TrueQ[$NetGraphInteractivity],
		DynamicModule[
			{selection = None, outIndex = Length[inputs] + Length[vertices], inputs = inputs, outputs = outputs, labels = labels, vertices = RemoveRawArrays[vertices], vports = vports},
			Grid[List /@ {
				netLayerPlot[edges, Dynamic[selection], bigmode, labels, opStyles, opTypes, opNames, edgeTypes],
				Dynamic[
					If[IntegerQ[selection],
						Item[
							vportInfo[vports[[selection]], inputs, outputs, vertices], 
							Alignment -> Center
						],
						Item["", ItemSize -> {0,0}]
					],
					TrackedSymbols :> {selection}
				]
			}, Alignment -> Left
			], Initialization :> {NetGraph}
		],
		netLayerPlot[edges, None, bigmode, labels, opStyles, opTypes, opNames, edgeTypes]
	]
];

vportInfo[NetPort["Inputs", port_], inputs_, outputs_, vertices_] := 
	typeInfo[port -> inputs[[port]]];

vportInfo[NetPort["Outputs", port_], inputs_, outputs_, vertices_] := 
	typeInfo[port -> outputs[[port]]];

vportInfo[NetPort["Vertices", name_], inputs_, outputs_, vertices_] := 
	itemInfo[name -> Lookup[vertices, name]];

netLayerPlot[edges_, selectionVar_, bigmode_, labels_, opStyles_, opTypes_, opNames_, edgeTypes_] :=
	LayerPlot[
		edges, 
		"BaseEdgeStyle" -> GrayLevel[0.7],
		"VertexSelectionVariable" -> selectionVar, 
		"VertexLabels" -> If[bigmode, None, Placed[labels, Below]],
		"VertexTooltips" -> If[bigmode, labels, None],
		"VertexTypeData" -> <|"VertexStyles" -> opStyles|>,
		"VertexTypes" -> opTypes,
		"VertexTypeLabels" -> Replace[opNames, {Null -> Null, e_ :> Style[e, GrayLevel[0.35]]}, {1}],
		"EdgeLabels" -> If[bigmode, None, Placed[Map[fmtDims, edgeTypes], Above]],
		"BaseEdgeLabelStyle" -> {FontColor -> Gray, FontSize -> 8, FontWeight -> "Thin"},
		"RotateEdgeLabels" -> True,
		"Rotated" -> True,
		"ArrowShape" -> "NarrowKite",
		"ImageMargins" -> {{0, 0}, {15, 15}}
	];


PackageExport["ExtendNetGraph"]

ExtendNetGraph[NetGraph[oldAssoc_], newVertices_Association, newEdges_List] := CatchFailure @ Scope[
	assoc = oldAssoc;
	newNames = Keys[newVertices];
	oldNames = Keys @ assoc["Vertices"];

	(* we don't allow overwriting of old layers... (shoudl we?) *)
	If[IntersectingQ[oldNames, newNames], Panic[]];

	(* ensure new layers are actually layers *)
	newVertices = toNetLayer /@ newVertices;
	
	(* add new vertices *)
	$vertices = assoc["Vertices"];
	AssociateTo[$vertices, newVertices];

	(* gather up types in preparation for checking etc *)
	$itypes = $otypes = Association[];
	KeyValueScan[collectPortTypes, $vertices];

	(* canonicaliez new edges *)
	$iexplicit = $oexplicit = {};
	newEdges = cedge /@ newEdges;

	(* explicit ports need to be created, if necessary, from scratch *)
	addExplicitPorts[assoc["Inputs"], $iexplicit];
	addExplicitPorts[assoc["Outputs"], $oexplicit];

	(* update the assoc *)
	assoc["Vertices"] = $vertices;
	assoc["Edges"] = Join[assoc["Edges"], newEdges];

	(* construct a new graph *)
	ConstructWithInference[NetGraph, assoc]
];


PackageScope["GetLayers"]

GetLayers[HoldPattern @ NetGraph[assoc_Association]] := 
	Map[ConstructLayer, assoc["Vertices"]];

NetGraph /: Take[ng_NetGraph, spec_] := CatchFailure[NetGraph, NetGraphTake[ng, spec]];

NetGraph::seqs = "Sequence specification {start, end} expected."

NetGraphTake[net_NetGraphP, {a_, b_}]:= Scope[
	$vertices = net["Vertices"];
	edges = net["Edges"];
	$inputs = InputNames[net];
	$outputs = OutputNames[net];
	
	$starting = procTakeStartElem[a];
	$ending = procTakeEndElem[b];

	graph = FullNetPortGraph[edges, $vertices];

	(* get component *)
	in = VertexOutComponent[graph, $ending];
	out = VertexInComponent[graph, $starting];

	newGraph = Intersection[in, out];
	$newVertices = Cases[newGraph, NetPort["Vertices", v_] :> v];
	
	(* subset vertices *)
	$vertices = KeySelect[$vertices, MemberQ[$newVertices, #]&];
	edges = Select[edges, keepEdgeQ];
	
	makeSubNetGraph[net, $vertices, edges]
]

NetGraphTake[_, _] := ThrowFailure["seqs"];

procTakeStartElem[elem_] := 
	Block[{$io = $inputs, $type = "Inputs"}, Flatten @ List @ procTakeElem[elem]];

procTakeEndElem[elem_] := 
	Block[{$io = $outputs, $type = "Outputs"}, Flatten @ List @ procTakeElem[elem]];

procTakeElem[e_List] := procTakeElem /@ e;
procTakeElem[All] := NetPort[$type, #]& /@ $io;

NetGraph::invportspec = "The port specification `` is invalid."
procTakeElem[p:NetPort[name_String]] :=
	If[MemberQ[$io, name], 
		NetPort[$type, name],
		ThrowFailure["invportspec", p]
	];

procTakeElem[p:NetPort[part_Integer | part_String, name_String]] := Scope[
	part = toVertex[part];
	inputs = $vertices[[part, "Inputs"]];
	outputs = $vertices[[part, "Outputs"]];
	NetPort["Vertices", part, Which[
		KeyExistsQ[inputs, name], "Inputs",
		KeyExistsQ[outputs, name], "Outputs",
		True, ThrowFailure["invportspec", p]
	], name]
];

procTakeElem[part_] := NetPort["Vertices", toVertex[part]];

Clear[keepEdgeQ];

keepEdgeQ[NetPort["Vertices", name1_, __] -> NetPort["Vertices", name2_, __]] := 
	MemberQ[$newVertices, name1] && MemberQ[$newVertices, name2]

keepEdgeQ[end:NetPort["Outputs", _] -> NetPort["Vertices", name_, __]] :=
	MemberQ[$newVertices, name] && MemberQ[$ending, end | NetPort["Vertices", name]];

keepEdgeQ[NetPort["Vertices", name_, __] -> start:NetPort["Inputs", _]] :=
	MemberQ[$newVertices, name] && MemberQ[$starting, start | NetPort["Vertices", name]];

NetGraph::invsplit = "Vertex `` has multiple inputs that are not all in the graph."

keepEdgeQ[NetPort["Vertices", name1_, __] -> verts_List] := 
	MemberQ[$newVertices, name1] && Or[
		AllTrue[verts[[All, 2]], MemberQ[$newVertices, #]&],
		ThrowFailure["invsplit", name1]
	];
	
keepEdgeQ[pat_] := False

(* Dispatch *)

$TypeToSymbol["Graph"] = NetGraph;

InferenceRulesScanner["Graph"] = Function[
	SowInferenceRules[#Edges];
	ScanFields["Vertices", ScanInferenceRules, #];
];

FullySpecifiedTest["Graph"] = Function[
	And[
		AllTrue[#Inputs, FullySpecifiedTypeQ],
		AllTrue[#Outputs, FullySpecifiedTypeQ],
		AllTrue[#Vertices, FullySpecifiedNetQ]
	]
];

ConcreteTest["Graph"] = Function[
	AllTrue[#Vertices, ConcreteNetQ]
];

InitializedTest["Graph"] = Function[
	AllTrue[#Vertices, InitializedNetQ]
];

PortGraphBuilder["Graph"] = Function[
	MapAtFields["Vertices", BuildPortGraph, #];
	Scan[SowPortGraphEdge, #Edges];
];

MXScanner["Graph"] = Function[
	SowMXConnections[#Edges]; 
	ScanFields["Vertices", MXScan, #];
];

PrepareForExport["Graph"] = 
	MapAtFields["Vertices", Call[PrepareForExport]]


PackageExport["DeleteTrainingLayers"]

DeleteTrainingLayers[ng_NetGraph] := Scope[
	lossVerts = NetGraphPortContributors[ng, "Loss"];
	vertices = Normal[ng]["Vertices"];
	lossVerts = Union[lossVerts, Keys @ Select[vertices, #Type === "Dropout"&]];
	DeleteLayers[ng, lossVerts]
];


PackageExport["NetGraphPortContributors"]

NetGraphPortContributors[ng_NetGraph, port_] := Scope[
	outNames = OutputNames[ng];
	If[!MemberQ[outNames, port], Return[{}]];
	UnpackAssociation[Normal[ng], vertices, edges];
	graph = NetPortGraph[edges];
	delPorts = Cases[outNames, port];
	keepPorts = DeleteCases[outNames, port];
	contribEdges = Complement[
		VertexOutComponent[graph, NetPort["Outputs", #]& /@ delPorts],
		VertexOutComponent[graph, NetPort["Outputs", #]& /@ keepPorts]
	];
	Cases[contribEdges, NetPort["Vertices", name_] :> name]
];

PackageExport["DeleteLayers"]

NetGraph /: VertexDelete[ng_NetGraph, e_] := 
	CatchFailure[NetGraph, DeleteLayers[ng, ToList[e]]];

DeleteLayers[net_NetGraphP, deleteList_List] := Scope[
	UnpackAssociation[net, $vertices:"Vertices", edges];
	deleteList = toVertex /@ deleteList;
	elisions = KeyValueMap[toElisionRules, $vertices[[deleteList]]];
	KeyDropFrom[$vertices, deleteList];
	edges = Select[edges /. elisions, FreeQ[Alternatives @@ deleteList]];
	makeSubNetGraph[net, $vertices, edges]
];

toElisionRules[name_, vertex_] :=
	Match[
		{vertex["Inputs"], vertex["Outputs"]}, 
		{<|inname_ -> type1_|>, <|outname_ -> type2_|>} /; EquivalentTypeQ[type1, type2] :> Rule[
			NetPort["Vertices", name, "Outputs", outname],
			Replace[
				NetPort["Vertices", name, "Inputs", inname], 
				Append[edges, _ :> Return[Nothing]]
			]
		],
		Nothing
	];


(*
PackageExport["NetFlatten"]

NetGraph /: Flatten[ng_NetGraph] := NetFlatten[ng];
NetChain /: Flatten[nc_NetChain] := NetFlatten[nc];

NetFlatten[net_NetP] := CatchFailure @ Scope[
	{nodes, edges} = LayerConnections[net];
	keys = IntegerString /@ Range[Length[nodes]];
	$nodeIDs = AssociationThread[nodes, keys];
	edges = ReplaceAll[edges, 
		p:NetPort[__, io:"Inputs"|"Outputs", name_] :> 
			NetPort["Vertices", $nodeIDs @ Drop[p, -2], io, name]
	];
	layers = AssociationThread[keys, net @@@ nodes];
	toNetGraph[layers, Map[$Raw, Reverse[edges, 2]], {}]
];

*)