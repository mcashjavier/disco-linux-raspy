Package["MXNetLink`"]

PackageImport["GeneralUtilities`"]

(****** Symbol Exports ******)

PackageExport["MXSymbolPlot"]

Options[MXSymbolPlot] = {
	"ShowTensors" -> True,
	"Rotated" -> False
};

MXSymbolPlot[sym_MXSymbol, OptionsPattern[]] := CatchFailure @ Scope[
	UnpackOptions[showTensors, rotated];
	expr = MXSymbolToJSON@sym;
	{nodes, argnodes, heads} = Lookup[expr, {"nodes", "arg_nodes", "heads"}, Panic[]];
	nodes = MapAt[#[[All, 1]]+1&, nodes, {All, "inputs"}];
	{ids, names} = Transpose @ MapIndexed[
		Append[#2, #name]&,
		nodes
	];
	argnodes += 1;
	heads += 1;
	edges = Join @@ MapIndexed[
		If[#op === "null", Nothing,
			If[showTensors, 
				Thread[Prepend[#2, #inputs]],
				Thread[Prepend[#2, Complement[#inputs, argnodes]]]
		]]&
	,
		nodes
	];
	{opTypes, opNames} = Labelling @ nodes[[All, "op"]];
	(* Add head tensors *)
	nullType = IndexOf[opNames, "null"];
	If[showTensors,
		outputTensors = MXSymbolOutputs@sym;
		opTypes = Join[opTypes, ConstantArray[nullType, Length@outputTensors]];
		argnodes = Join[argnodes, Range@Length@outputTensors + Max@edges];
		names = Join[names, outputTensors];
		maxIndex = Max@edges;
		MapIndexed[AppendTo[edges, {First@#1, (First[#2] + maxIndex)}]&, heads]
	];
	
	(* Do plot *)
	nops = Length[opNames];
	opStyles = ColorData[90] /@ Range[nops];
	opSizes = ReplacePart[ConstantArray[8, nops], nullType -> 5];
	opStyles = ReplacePart[opStyles, nullType -> Gray];
	opNames = opNames /. "null" -> "Tensor";

	vertexTypeData = <|"VertexStyles" -> opStyles|>;
	If[showTensors, vertexTypeData = Join[vertexTypeData, <|"VertexSizes" -> opSizes|>]];
	labels = Map[toLabel, names];
	LayerPlot[edges, 
		"VertexLabels" -> Placed[labels, Below], 
		"BaseVertexLabelStyle" -> FontSize -> 8,
		"VertexTooltips" -> names, 
		"HiddenVertices" -> If[showTensors, None, argnodes],
		"VertexTypeData" -> vertexTypeData,
		"VertexTypeLabels" -> opNames,
		"MaximumImageSize" -> {400, 300},
		"VertexSizes" -> 5,
		If[showTensors, "VertexTypes" -> opTypes, "VertexStyles" -> opTypes],
		"Rotated" -> True,
		"ArrowShape" -> "Chevron"
	]
]

toLabel[s_String] := If[StringContainsQ[s, "_"], Last[StringSplit[s, "_"]], Null];

(******************************************************************************)
PackageExport["MXSymbolGraph"]

MXSymbolGraph[sym_MXSymbol, OptionsPattern[]] := CatchFailure @ Scope[
	expr = MXSymbolToJSON@sym;
	{nodes, heads} = Lookup[expr, {"nodes", "heads"}, Panic[]];

	(* Add output nodes *)
	outputs = MXSymbolOutputs@sym;
	outputNodes = Map[
		<|"op" -> "null","param" -> <||>,
		"name"-> #, "inputs" -> {},"backward_source_id"->-1
		|>&, outputs
	];
	nodes = Join[nodes, outputNodes];

	(* convert to 1 indexing *)
	heads += 1;
	nodes = MapAt[#[[All, 1]] + 1&, nodes, {All, "inputs"}];
	{ids, names} = Transpose @ MapIndexed[
		Append[#2, #name]&,
		nodes
	];
	(* Get node association *)
	nodesAssociation = Association[#name -> #& /@ nodes];
	(* Get edges *)
	edges = Thread[names[[#inputs]] -> #name]& /@ nodes;
	edges = getPortInfo[#, nodesAssociation]& /@ edges;
	edges = Flatten@edges;
	
	(* Add output edges *)
	outputOperators = names[[heads[[;;, 1]]]];
	outputEdges = Thread[outputOperators -> outputs];
	edges = Join[edges, outputEdges];

	(* Add vertex colours *)
	opTypes = Union[#op& /@ nodes];
	opNums = Range@Length@opTypes/ Length@opTypes;
	opStyles = ColorData["Rainbow"]/@ opNums;
	opStyles = AssociationThread[opTypes -> opStyles];
	opStyles["null"] = Black;
	opShape = If[# === "null", "Square", "Circle"]&;
	
	(* add properties *)
	properties = Property[#name, {
		"op"-> #op, 
		"param" -> #param, 
		VertexStyle -> opStyles@#op,
		VertexShapeFunction -> opShape@#op
	}]& /@ nodes;
	graph = Graph[
		properties, 
		edges, 
		EdgeLabels -> Placed["Name", Tooltip], 
		VertexLabels -> Placed["Name", Tooltip] 
	]
]

getPortInfo[edges_, nodesAssoc_] := Scope[
	If[edges === {}, Return@edges];
	op = nodesAssoc[Last@First@edges, "op"];
	portNames = Lookup[$SymbolArgumentOrdering[], op];
	If[Length@portNames < Length@edges, 
		portNames = ConstantArray[Missing[], Length@edges]
	];
	portNames = Thread["PortName" -> portNames];
	portNumber = Thread["Port" -> Range@Length@edges];
	MapThread[Property[#1, #2]&, {edges, Transpose[{portNumber, portNames}]}]
]
