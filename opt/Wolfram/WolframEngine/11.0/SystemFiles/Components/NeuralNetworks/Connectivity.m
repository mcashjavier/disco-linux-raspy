Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]


PackageExport["LayerDepedencyGraph"]

LayerDepedencyGraph[net_NetP] := Scope[
	$path = NetPort[];
	CollectTo[{$GraphNodes, $GraphEdges}, BuildPortGraph[net]];
	$predFunc = Merge[$GraphEdges, Identity];
	virtNodes = Complement[DeepCases[$GraphEdges, _NetPort], $GraphNodes];
	$virtQ = ConstantAssociation[virtNodes, True];
	edges = Flatten @ Map[layerPredecessorEdges, $GraphNodes];
	Graph[
		$GraphNodes, Reverse[edges, 2],
		VertexLabels -> Placed["Name", Tooltip],
		GraphLayout -> {"LayeredDigraphEmbedding", "Orientation" -> Left},
		ImageSize -> {800}
	]
];

layerPredecessorEdges[layer_] := 
	Thread @ Rule[layer, 
		Flatten @ ReplaceRepeated[
			$predFunc[layer],
			p_NetPort ? $virtQ :> Lookup[$predFunc, p, {}]
		]
	];


PackageExport["PortConnectivityGraph"]

PortConnectivityGraph[net_NetP] := Scope[
	$path = NetPort[];
	CollectTo[{$GraphNodes, $GraphEdges}, BuildPortGraph[net]];
	Graph[$GraphNodes, Reverse[Flatten @ $GraphEdges, 2], 
		VertexLabels -> Placed["Name", Tooltip],
		GraphLayout -> {"LayeredDigraphEmbedding", "Orientation" -> Left},
		ImageSize -> {800}
	]
];


PackageScope["BuildPortGraph"]

BuildPortGraph[assoc_Association] := Call[assoc, PortGraphBuilder];


PackageScope["PortGraphBuilder"]

(* Todo: use a visitor type API, because NetChain and NetGraph are pretty much identical. *)

PortGraphBuilder[node_] := Function[
	SowGraphNode[#];
	BagInsert[$GraphEdges, List[
		Thread[OutputPorts[#] -> $path],
		Thread[$path -> InputPorts[#]]
	]];
];

PackageScope["SowGraphNode"]

SowGraphNode[node_] := BagInsert[$GraphNodes, $path];


PackageScope["SowPortGraphEdge"]

SowPortGraphEdge[rule:Rule[_, _List]] := Scan[SowPortGraphEdge, Thread[rule]];
SowPortGraphEdge[edge_] := BagInsert[$GraphEdges, PrefixPorts[edge]];
