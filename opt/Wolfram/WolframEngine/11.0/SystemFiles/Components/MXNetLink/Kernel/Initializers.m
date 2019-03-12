Package["MXNetLink`"]
PackageImport["GeneralUtilities`"]

(******************************************************************************)

(* MXInitializer: 
	all params are zero initialized, except for two cases: weights + fix_gamma params. 
*)
PackageExport["MXInitializer"]
MXSetUsage[MXInitializer, 
"MXInitializer[arrays$, MXSymbol[$$]] Initialize an association of NDArrays \
given the associated MXSymbol[$$] object. The initialization used for weight arrays \
can be controlled via the option \"WeightInitializer\", which can be one of \
 {\"Xavier\", \"Orthogonal\"}. 
"]

Options[MXInitializer] = {
	"WeightInitializer" -> "Xavier" (* or Orthogonal *)
};

MXInitializer[arrays_Association, symbol_MXSymbol, OptionsPattern[]] := Scope[
	UnpackOptions[weightInitializer];
	graph = MXSymbolGraph@symbol;
	(* Get all edges. If a param has two edges, use the first *)
	ops = VertexOutComponent[graph, {#}, 1][[2]]& /@ Keys@arrays;
	edges = Thread[DirectedEdge[Keys@arrays, ops]];
	opPortTypes = PropertyValue[{graph, #}, "PortName"]& /@ edges;
	(* Get activations: if multiple outs, take first *)
	activations = VertexOutComponent[graph, {#}, 1]& /@ ops;
	(* Account for case where there is no next symbol *)
	activations = If[Length@# === 1, 
		None, 
		<|
			"op" -> PropertyValue[{graph, #[[2]]},"op"], 
			"param" -> PropertyValue[{graph, #[[2]]}, "param"]
		|>
	]& /@ activations;
	(* Initialize *)
	MapThread[
		MXIntializerNDArray[#2, #1, weightInitializer, #2]&, 
		{Values@arrays, opPortTypes, activations}
	];
]

(* 1. Weights *)
MXIntializerNDArray["weight", nd_NDArray, "Xavier", activation_] := MXInitializerXavier[nd, activation];
MXIntializerNDArray["weight", nd_NDArray, "Orthogonal", activation_] := MXInitializerOrthogonal[nd, activation];
(* 2. gamma for batchnorm layer *)
MXIntializerNDArray["fix_gamma", nd_NDArray, _, _] := NDArraySet[nd, 1]
(* 3. all other layers *)
MXIntializerNDArray[_, nd_NDArray, _, _] := NDArraySet[nd, 0]

(******************************************************************************)
(* MXInitializerXavier: based on the following papers
	1. Understanding the difficulty of training deep feedforward neural networks,
		X. Glorot and Y. Bengio
	2. Delving Deep into Rectifiers: Surpassing Human-Level Performance on ImageNet Classification,
		K. He et al
*)

PackageExport["MXInitializerXavier"]

Options[MXInitializerXavier] = {
	"FactorType" -> "In", (* or "Out" or "Average" *)
	"SampleDistribution" -> "UniformDistribution" (* Can also be NormalDistribution *)
}

MXInitializerXavier[array_NDArray, activationFunction_, OptionsPattern[]] := Scope[
	UnpackOptions[factorType, sampleDistribution];
	(* 1. Get number of input + output neurons *)
	dim = Dimensions@array;
	fanin = Times@@Rest@dim;
	fanout = First@dim;
	
	(* 2. The scale depends on the activation function. See He et al *)
	variance  = Switch[factorType, 
		"In",
			2 / fanin,
		"Out",
			2/fanout,
		"Average",
			2 / (fanin + fanout)
	];
	scaleFactor = activationScaleFactor@activationFunction;
	variance *= scaleFactor;
	(* 3. Sample from distribution of given variance *)
	stddev = Sqrt@variance;
	values = Switch[sampleDistribution,
		"NormalDistribution",
			NDArrayRandomGaussian[array, 0, stddev],
		"UniformDistribution",
			(* using StandardDeviation@UniformDistribution[{-n, n}] = n/Sqrt[3] *)
			NDArrayRandomUniform[array, {-stddev * Sqrt[3], stddev * Sqrt[3]}],
		_,
			Panic["Invalid distribution!"]
	];
]

(******************************************************************************)

(* MXInitializerOrthogonal: based on:
	Exact solutions to the nonlinear dynamics of learning in deep linear neural networks
		http://arxiv.org/pdf/1312.6120v3.pdf
		A.M. Saxe et al 2014
	NOTE: we follow Lasagne implementation (https://github.com/Lasagne/Lasagne/blob/master/lasagne/init.py),
		and 
*)

PackageExport["MXInitializerOrthogonal"]

Options[MXInitializerOrthogonal] = {
	"ActivationFunction" -> None
}

MXInitializerOrthogonal[array_NDArray, activationFunction_, OptionsPattern[]] := Scope[
	
	(* 1. Get number of input + output neurons *)
	dim = Dimensions@array;
	fanin = Times@@Rest@dim;
	fanout = First@dim;
	flatShape = {fanout, fanin};
	
	scaleFactor = activationScaleFactor@activationFunction;		
	a = RandomVariate[NormalDistribution[0, 1], flatShape];
	{u, w, v} = SingularValueDecomposition[a, Min@Dimensions@a];

	(* Choose one with correct shape *)
	q = If[Dimensions@u === flatShape, u, v];
	q = ArrayReshape[q, dim] * Sqrt@scaleFactor;
	NDArraySet[array, q]
]

(******************************************************************************)
(* Scale factors:
		Decide on the scale depending on type of rectifier:
		see http://arxiv.org/pdf/1505.00853.pdf for overview of rectifiers
*)

(* high level scale *)
activationScaleFactor[None] := 1
activationScaleFactor[actFunc_] := iActivationScaleFactor[actFunc["op"], actFunc["param"]]

iActivationScaleFactor[other_, param_] := 1

iActivationScaleFactor["Activation", param_] := If[param["act_type"] == "relu", 2, 1];

iActivationScaleFactor["LeakyReLU", param_] := Scope[
	(* For non-RRELU: take slope *)
	If[param["act_type"] =!= "rrelu",
		Return@slopeToScaleFactor@param["slope"]
	];
	(* for RRELU: take mean slope *)
	lower = param["lower_bound"];
	upper = param["upper_bound"];
	slopeToScaleFactor@Mean[{lower, upper}]
]

(* For anything else, use 1 *)
iActivationScaleFactor[_, param_] := 1

slopeToScaleFactor[slope_] := 2/(1 + slope^2)

