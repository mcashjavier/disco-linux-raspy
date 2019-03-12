Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]
PackageImport["Developer`"]


(******************************************************************************)
(* NetInitializer: a graph based parameter initializer *)

PackageExport["NetInitialize"]

Options[NetInitialize] = {
	Method -> Automatic
};

$NetInitializeMethods := $NetInitializeMethods = <|
	"Xavier" -> {Values /* Apply[XavierInitializer], <|
		"FactorType" -> Defaulting[EnumT[{"In", "Out", "Mean"}], "Mean"],
		"Distribution" -> Defaulting[EnumT[{"Uniform", "Normal"}]]
	|>},
	"Orthogonal" -> {OrthogonalInitializer&, <||>},
	"Identity" -> {IdentityInitializer[#Distribution]&, <|"Distribution" -> Defaulting[DistributionT, NormalDistribution[0,0.01]]|>},
	"Random" -> {DistributionInitializer[#Weights, #Biases]&, <|"Weights" -> Defaulting[DistributionT, NormalDistribution[]], "Biases" -> Defaulting[DistributionT, None]|>}
|>;

NetInitialize::notfspec = "All parameters must be fully specified before a network can be initialized."
NetInitialize::notudist = "`` should be a valid univariate distribution."

NetInitialize[list_List, opts:OptionsPattern[]] :=
	Map[NetInitialize[#, opts]&, list];

NetInitialize[expr:(head_Symbol[assoc_Association]), replaceExisting:Except[_Rule]:Automatic, OptionsPattern[]] := CatchFailure @ Scope[
	$replace = (replaceExisting === All);
	If[FullySpecifiedNetQ[expr] && !$replace, Return[expr]];
	$makeArray = makeArray;
	Match[OptionValue[Method],
		{"Custom", method_} :> (
			$makeArray = makeCustomArray;
			$customFunction = method;
		),
		method_ :> (
			$initializer = ParseMethod[method, $NetInitializeMethods];
		)
	];	
	$assoc = assoc;
	$initialLayers = None;
	$graph = LayerDepedencyGraph[assoc];
	Scan[updateVertex, TopologicalSort[$graph]];
	System`Private`ConstructNoEntry[head, $assoc]
]

NetInitialize::arg1 = "First argument to NetInitialize should be a net or list of nets."

NetInitialize[__] := RuleCondition[Message[NetInitialize::arg1]; Fail];

getVertexDepth[port_] := (
	If[$initialLayers === None, 
		$initialLayers = Complement[VertexList[$graph], EdgeList[$graph][[All, 1]]];
	];
	Min[GraphDistance[$graph, port, #]& /@ $initialLayers]
);

updateVertex[port:NetPort[pos___]] := Scope[
	subnet = Part[$assoc, pos];
	arrays = DeleteCases[None] @ subnet["Arrays"];
	type = subnet["Type"];
	If[!$replace, arrays = DeleteCases[_RawArray] @ arrays];
	$port = port;
	$depth := $depth = getVertexDepth[port];
	KeyValueMap[
		Set[
			$assoc[[pos, "Arrays", #1]], 
			$makeArray[type, #1, getDims[#2]]
		]&,
		arrays
	];
]

getDims[Nullable[t_]] := getDims[t];
getDims[arr_RawArray] := Dimensions[arr];
getDims[t_] := Replace[GetTensorDimensions[t], SizeT | $Failed :> ThrowFailure["notfspec"], {0, Infinity}];

(* this is used as needed by makeArray. *)
LookupNextLayer[port_] := Scope[
	out = VertexOutComponent[$graph, port, 1];
	If[Length[out] === 1, None, Part[$assoc, Sequence @@ Last[out]]]
];

(******************************************************************************)
(* This contains the special Port/Layer level init logic. If adding a new layer
	with special init needs, add it here.
*)

NetInitialize::badcustominit = "Custom initializer did not return a number, tensor, or distribution."

makeCustomArray[type_, name_, dim_List] := UseMacros @ 
	RawArray["Real32", Match[
		$customFunction[<|
			"Type" -> $TypeToSymbol[type], "Name" -> name, "Dimensions" -> dim, 
			"Outputs" -> First[dim], "Depth" -> $depth, 
			"NextLayer" -> LookupNextLayer[$port]|>],
		0.|0 :> ConstantArray[0., dim],
		r_ ? NumberQ :> RandomVariate[NormalDistribution[0, N[r]], dim],
		e_List ? MachineArrayQ :> If[Dimensions[e] =!= dim, Panic[], e],
		e_ ? UnivariateDistributionQ :> RandomVariate[e, dim],
		ThrowFailure["badcustominit"]
	]];

(* Weights: Weight ports have special methods that can depend on activation functions *)
makeArray[type_, "Weights", dim_List] := Scope[
	result = $initializer[type, dim, LookupNextLayer[$port]];
	If[!RawArrayQ[result], Panic["NotRawArray"]];
	result
];

makeArray[_, "Biases", dim_List] /; setsBiasesQ[$initializer] := Scope[
	result = $initializer[type -> "Biases", dim, LookupNextLayer[$port]];
	If[!RawArrayQ[result], Panic["NotRawArray"]];
	result
];

(* Biases: All ports named Bias are zero initialized *)
makeArray[_, "Biases", dim_List] :=
	RawArray["Real32", ConstantArray[0., dim]]

setsBiasesQ[_] := False;

(* BatchNorm Arrays: special init *)
makeArray["BatchNormalization", "Beta" | "MovingMean", dim_List] := 
	RawArray["Real32", ConstantArray[0., dim]]

makeArray["BatchNormalization", "Gamma" | "MovingVariance", dim_List] := 
	RawArray["Real32", ConstantArray[1., dim]]

(******************************************************************************)
(* Weight Initialization Methods: Xavier, Orthogonal, Distribution, and Identity. *)

setsBiasesQ[DistributionInitializer[_, bdist_]] := bdist =!= None;

DistributionInitializer[wdist_, bdist_][_, dim_, nextLayer_:None] :=
	RawArray["Real32", RandomVariate[wdist, dim]];

DistributionInitializer[wdist_, bdist_][_ -> "Biases", dim_, nextLayer_:None] :=
	RawArray["Real32", RandomVariate[bdist, dim]];

IdentityInitializer[dist_][type_, dim_List, nextLayer_:None] :=
	RawArray["Real32", AddNoise[IdentityTensor @@ dim, dist]];

IdentityInitializer[dist_]["Convolution"|"Deconvolution", dims:{nout_, nin_, w_, h_}, nextLayer_:None] := 
	RawArray["Real32", AddNoise[IdentityConvolutionKernel @@ dims, dist]];

AddNoise[arr_, dist_] := arr + RandomVariate[dist, Dimensions[arr]];

IdentityConvolutionKernel[nout_, nin_, w_, h_] := Scope[
	unitKernel = ToPackedArray @ System`CenterArray[1, {w, h}];
	zeroKernel = ConstantArray[0, {w, h}];
	Table[
		If[out == in, unitKernel, zeroKernel],
		{out, nout},
		{in, nin}
	]
];

IdentityTensor[a_, b_] := Take[IdentityMatrix @ Max[a, b], a, b];
IdentityTensor[m_, rest__] := Table[IdentityTensor[rest], m];


(***********
	xavierInitializer: based on the following papers
	1. Understanding the difficulty of training deep feedforward neural networks,
		X. Glorot and Y. Bengio
	2. Delving Deep into Rectifiers: Surpassing Human-Level Performance on ImageNet Classification,
		K. He et al
***********)

XavierInitializer[factorType_, distribution_][type_, dim_List, nextLayer_:None] := Scope[
	
	(* 1. Get number of input + output neurons *)
	fanin = Times @@ Rest[dim];
	fanout = First[dim];
	
	(* 2. The scale depends on the activation function. See He et al *)
	variance = Switch[factorType, 
		"In", 		2 / fanin,
		"Out", 		2 / fanout,
		"Mean", 	2 / (fanin + fanout)
	];
	scaleFactor = activationScaleFactor[nextLayer];
	variance *= scaleFactor;
	(* 3. Sample from distribution of given variance *)
	stddev = Sqrt[variance];
	values = Switch[distribution,
		"Normal",
			RawArray["Real32", RandomVariate[NormalDistribution[0, stddev], dim]],
		"Uniform",
			(* using StandardDeviation@UniformDistribution[{-n, n}] = n/Sqrt[3] *)
			RawArray["Real32", RandomVariate[UniformDistribution[{-1,1} * -stddev * Sqrt[3.]], dim]]
	]
]

(***********
 orthogonalInitializer: based on:
	Exact solutions to the nonlinear dynamics of learning in deep linear neural networks
		http://arxiv.org/pdf/1312.6120v3.pdf
		A.M. Saxe et al 2014
	NOTE: we follow Lasagne implementation (https://github.com/Lasagne/Lasagne/blob/master/lasagne/init.py),
		and 
***********)

OrthogonalInitializer[type_, dim_List, nextLayer_:None] := Scope[
	(* 1. Get number of input + output neurons *)
	fanin = Times @@ Rest[dim];
	fanout = First[dim];
	flatShape = {fanout, fanin};
	
	scaleFactor = activationScaleFactor[nextLayer];
	a = RandomVariate[NormalDistribution[0, 1], flatShape];
	{u, w, v} = SingularValueDecomposition[a, Min@Dimensions@a];

	(* Choose one with correct shape *)
	q = If[Dimensions[u] === flatShape, u, v];
	q = ArrayReshape[q, dim] * Sqrt[scaleFactor];

	RawArray["Real32", q]
]

(***********
	 Scale factors:
		Decide on the scale depending on type of rectifier. 
		Used by orthogonalInitializer + xavierInitializer
		see http://arxiv.org/pdf/1505.00853.pdf for overview of rectifiers
***********)

(* high level scale *)
activationScaleFactor[None] := 1
activationScaleFactor[assoc_Association] := 
	iActivationScaleFactor[assoc["Type"], assoc["Parameters"]];

(* Case 1: Relu/Ramp *)
iActivationScaleFactor["Elementwise", param_] := If[param["Function"] === Ramp, 2, 1];

(* Case Default: Relu/Ramp *)
iActivationScaleFactor[other_, param_] := 1;


