Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]
PackageImport["Developer`"]


(* Type aliases provide a mechanism to declare new types that are really just
names for other types, e.g. ColorSpaceT = EnumT[{"RGB", "HSV", ...}]. It's up
to APIs like Coerce and so on to hook into this mechanism.
*)

$TypeAliasPatterns = Alternatives[];
$InvalidTypeAlias = _ :> Panic["InvalidTypeAliasUsage"];
$TypeAliasRules = {$InvalidTypeAlias};

PackageScope["DeclareTypeAlias"]

DeclareTypeAlias[rule:RuleDelayed[new_, old_]] := (
	$TypeAliasRules[[-1]] = rule;
	AppendTo[$TypeAliasRules, $InvalidTypeAlias];
	AppendTo[$TypeAliasPatterns, new];
);
DeclareTypeAlias[_] := Panic["InvalidAlias"];

PackageScope["TypeAliasP"]
PackageScope["ResolveAlias"]

TypeAliasP := _ ? (MatchQ[$TypeAliasPatterns]);
ResolveAlias[p_] := Replace[p, $TypeAliasRules];

PackageExport["TypeT"]
PackageExport["TypeExpressionT"]

NNSetUsage @ "TypeT represents another type, like IntegerT or TensorT[$$]."
NNSetUsage @ "TypeExpressionT represents a type expression, which may involve types that reference ports."

PackageExport["PosIntegerT"]
PackageExport["IntegerT"]
PackageExport["NaturalT"]
PackageExport["ScalarT"]
PackageExport["IndexIntegerT"]
PackageExport["IntervalScalarT"]

NNSetUsage @ "PosIntegerT represents a positive integer."
NNSetUsage @ "IntegerT represents an integer."
NNSetUsage @ "NaturalT represents a non-negative integer."
NNSetUsage @ "ScalarT represents a real number, and is equivalent to TensorT[{}]."
NNSetUsage @ "IndexIntegerT[max$] represents an integer in the range [1,max$]."
NNSetUsage @ "IntervalScalarT[min$, max$] represents a real number in the range [min$, max$]."

ScalarT = TensorT[0, {}];

PackageExport["RuleT"]
PackageExport["FunctionT"]
PackageExport["MatchT"]
PackageExport["StructT"]
PackageExport["AssocT"]
PackageExport["EnumT"]
PackageExport["StringT"]
PackageExport["BooleanT"]
PackageExport["SizeT"]

NNSetUsage @ "RuleT[lhs$, rhs$] represents a rule whose LHS is of type lhs$ and whose RHS is of type $rhs."
NNSetUsage @ "FunctionT represents an expression with head Function."
NNSetUsage @ "MatchT[patt$] represents an expression which matches patt$."
NNSetUsage @ "StructT[{key$1->type$1,$$}] represents an association with keys key$i whose values match type$i."
NNSetUsage @ "AssocT[ktype$,vtype$] represents an association whose keys have type ktype$ and values have type vtype$."
NNSetUsage @ "EnumT[{val$1,val$2,$$}] represents an expression whose values are one of the literal expressions val$i."
NNSetUsage @ "StringT represents a string."
NNSetUsage @ "BooleanT represents the symbols True and False."
NNSetUsage @ "SizeT represents a positive integer used as a 'size' of some sort."

PackageExport["ComputedType"]

NNSetUsage @ "
ComputedType[type$,expr$,{dep$1,$$}] represents a value of form type$ that is computed by expr$, and depends on ports dep$i.
ComputedType[type$,expr$,{dep$1,$$},trigger$] only attempts to compute expr$ when trigger$ is satisfied.
"


PackageExport["EitherT"]

NNSetUsage @ "EitherT[{type$1,type$2,$$}] represents a value of one of the type$i."


PackageExport["ListT"]

NNSetUsage @ "
ListT[type$] represents a list of any length, whose values are of type type$.
ListT[n$, type$] represents a list of length n$.
"

DeclareSymbolicHead[ListT];
ListT[type_] := ListT[NaturalT, type];


PackageExport["SizeListT"]

NNSetUsage @ "
SizeListT[] represents a list of positive integers.
SizeListT[n$] represents a list of n$ positive integers.
SizeListT[$$] evaluates to a ListT.
"

SizeListT[n_] := ListT[n, SizeT];
SizeListT[] := ListT[NaturalT, SizeT];


PackageExport["TensorT"]

NNSetUsage @ "
TensorT[] represents a tensor of any rank.
TensorT[n$] represents a tensor of rank $n.
TensorT[n$,dims$] represents a tensor of rank n$ and specific dimensions dims$.
TensorT[{dim$1,$$,dim$n}] evaluates to TensorT[n$,dims$].
"

DeclareSymbolicHead[TensorT];
TensorT[] := TensorT[NaturalT];
TensorT[n_] := TensorT[n, SizeListT[n]];
TensorT[e_List] := TensorT[Length[e], e];


PackageExport["ChannelT"]

NNSetUsage @ "
ChannelT[SizeT,tensor$] represents a tensor with rank one higher than tensor$.
ChannelT[n$,tensor$] represent tensor$ with an additional initial dimension n$.
"

PackageExport["MatrixT"]

NNSetUsage @ "
MatrixT[rows$,cols$] represents a rank-2 tensor.
Matrix[$$] evaluates to a TensorT[$$].
"

MatrixT[rows_, cols_] := TensorT[2, {rows, cols}];


PackageExport["VectorT"]

NNSetUsage @ "
VectorT[] represents a rank-1 tensor (a vector).
Vector[n$] represents a vector of length n$.
VectorT[$$] evaluates to a TensorT[$$].
"

VectorT[size_] := TensorT[1, {size}];
VectorT[] := TensorT[1, {SizeT}];


PackageExport["ImageT"]

NNSetUsage @ "
ImageT[] represents an image.
ImageT[dims$] represents an image of dimensions dims$.
ImageT[dims$,cspace$] represents an image with color space cspace$.
"

DeclareSymbolicHead[ImageT];
ImageT[] := ImageT[SizeListT[2], ColorSpaceT]; 


PackageExport["ColorSpaceT"]

NNSetUsage @ "ColorSpaceT represents a color space string ('RGB', 'Grayscale', etc.)."

PackageScope["$ColorSpaces"]

$ColorSpaces = {"Grayscale", "RGB", "CMYK", "HSB", "XYZ", "LAB", "LCH", "LUV", Automatic};
DeclareTypeAlias[ColorSpaceT :> EnumT[$ColorSpaces]];


PackageExport["ActivationFunctionT"]

NNSetUsage @ "ActivationFunctionT represents an activation function for ElementwiseLayer (one of LogisticSigmoid, Ramp, Tanh)."

PackageScope["$ActivationFunctions"]

$ActivationFunctions = {LogisticSigmoid, Ramp, Tanh};
DeclareTypeAlias[ActivationFunctionT :> EnumT[$ActivationFunctions]];


PackageScope["PoolingFunctionT"]

NNSetUsage @ "PoolingFunctionT represents a pooling function for PoolingLayer (one of Mean, Max, Total)."

$PoolingFunctions = {Mean, Max, Total}
DeclareTypeAlias[PoolingFunctionT :> EnumT[$PoolingFunctions]];


PackageExport["ExpressionT"]

NNSetUsage @ "ExpressionT represents any expression."


PackageExport["DistributionT"]

NNSetUsage @ "DistributionT represents a univariate distribution."


PackageExport["Defaulting"]

NNSetUsage @ "Defaulting[type$,value$] represents an expression of type type$ that defaults to value$."

DeclareSymbolicHead[Defaulting];
Defaulting[t_ListT] := Defaulting[t, {}];
Defaulting[t_AssocT] := Defaulting[t, <||>];
Defaulting[t_EnumT] := Defaulting[t, t[[1,1]]];
Defaulting[t_Nullable] := Defaulting[t, None];
Defaulting[t_] := Defaulting[t, None];


PackageExport["Nullable"]

NNSetUsage @ "Nullable[type$] represents a type type$ that can also be None."


PackageExport["EncodedType"]
PackageExport["DecodedType"]

NNSetUsage @ "EncodedType[encoder$,type$] represents a type of an input to a net that is encoded by encoder$ and has type type$."
NNSetUsage @ "DecodedType[decoder$,type$] represents a type of an output to a net that is decoded by decoder$ and has type type$."


PackageScope["ExpandChannels"]	

NNSetUsage @ "
ExpandChannels[type$] takes a tensor type that might involve ChannelT and rewrites it to be a simple\
TensorT instead.
"

ExpandChannels[e_] := Match[e, 
	ChannelT[c_, c2_ChannelT] :> %[ChannelT[c, %[c2]]],
	ChannelT[n_, t_TensorT] :> prependDim[t, n],
	_TensorT :> e,
	$Failed
];

prependDim[TensorT[n_Integer, ListT[n_Integer, SizeT]], p_] := 
	TensorT[n + 1, Prepend[Table[SizeT, n], p]];

prependDim[TensorT[n_Integer, e_List], p_] :=
	TensorT[n + 1, Prepend[e, p]];

prependDim[TensorT[RankTP, _], p_] :=
	TensorT[SizeT];


PackageScope["GetTensorDimensions"]

NNSetUsage @ "
GetTensorDimensions[type$] returns the list of dimensions of a numeric tensor type (TensorT), \
or $Failed if the rank is not fixed or the type is not a tensor type. Note: the list return \
could contain SizeT.
"

GetTensorDimensions[e_TensorT | e_ChannelT] := 
	Replace[ExpandChannels[e], {TensorT[_, dims_List] :> dims, _ :> $Failed}];

GetTensorDimensions[_] := $Failed;


PackageScope["GetTensorRank"]

NNSetUsage @ "
GetTensorRank[type$] returns the rank of a numeric tensor type (TensorT), or $Failed if \
this isn't known or the type isn't a tensor type.
"

GetTensorRank[type_] := Match[type,
	Nullable[t_] :> %[t],
	TensorT[r_Integer, _] :> r,
	ChannelT[_, t_] :> Replace[%[t], i_Integer :> i + 1],
	TypeAliasP :> %[ResolveAlias[type]],
	$Failed
];


PackageScope["$AtomicTypes"]
$AtomicTypes = {
	PosIntegerT, IntegerT, NaturalT,
	SizeT,
	StringT,
	BooleanT,
	TypeT, ExpressionT, FunctionT, TypeExpressionT
};

PackageExport["TypeForm"]

NNSetUsage @ "
TypeForm[type$] gives an expression that formats as a human-readable string decsribing a type.
"

TypeForm[type_] := fromStringForm @ ReplaceRepeated[
	form[type], {
	plural[e_] :> ReplaceAll[e, {
		blockplural -> Identity,
		pform -> pluralform,
		psform -> psingleform,
		ppform -> pluralform,
		s_single :> s
	}],
	single[e_] :> ReplaceAll[e, {
		pform -> singleform,
		psform -> singleform,
		ppform -> psingleform,
		p_plural :> p
	}],
	blockplural -> Identity,
	pform -> singleform,
	psform -> singleform,
	ppform -> psingleform
}];

pluralform[p_String] := p <> "s";
pluralform[a_, b_] := b;

psingleform[a_, b_] := a;
psingleform[a_] := a;

singleform[a_, b_] := singleform[a];
singleform[s_String] := 
	If[StringStartsQ[s, "a"|"e"|"i"|"o"|"u"], 
		StringForm["an ``", s],
		StringForm["a ``", s]
	];

singleform[s_] := StringForm["a ``", s];

form[type_] := Match[type,
	ListT[1, t_] :> StringForm["`` containing ``", pform["list"], % @ t],
	ListT[n_, t_] :> of["list", num[n, % @ t]],
	AssocT[k_, v_] :> from["association", % @ k, % @ v],
	RuleT[k_, v_] :> from["rule", % @ k, % @ v],
	StructT[rules_] :> structForm[rules],
	EitherT[list_] :> eitherForm[list],
	EnumT[list_] :> % @ EitherT[list],
	s_String :> "\"" <> s <> "\"",
	ScalarT :> pform["number"],
	IntervalScalarT[min_, max_] :> StringForm["`` between `` and ``", pform["number"], min, max],
	TensorT[RankTP, _] :> pform["tensor"],
	TensorT[2, {m_Integer, n_Integer}] :> adj[size[{m, n}], pform["matrix", "matrices"]],
	TensorT[2, _] :> pform["matrix", "matrices"],
	TensorT[1, {n_Integer}] :> dashadj["length", n, "vector"],
	TensorT[1, _] :> pform["vector"],
	TensorT[_Integer, dims:{__Integer}] :> adj[size[dims], "tensor"],
	ChannelT[n_Integer, TensorT[SizeT, _]] :> StringForm["`` with outer dimension ``", pform["tensor"], n],
	TensorT[n_Integer, _] :> dashadj["rank", n, "tensor"],
	t_ChannelT :> %[ExpandChannels[t]],
	StringT :> pform["string"],
	BooleanT :> pform["boolean"],
	IntegerT :> pform["integer"],
	IndexIntegerT[max_Integer] :> StringForm["`` between 1 and ``", pform["integer"], max],
	IndexIntegerT[_] :> adj["bounded", "integer"],
	SizeT|PosIntegerT :> adj["positive", "integer"],
	NaturalT :> adj["non-negative", "integer"],
	ImageT :> pform["image"],
	FunctionT :> pform["function"],
	MatchT[sym_Symbol] :> sym,
	MatchT[patt_] :> StringForm["`` matching ``", pform["expression"], patt],
	TypeT :> pform["type"],
	TypeExpressionT :> pform["type expression"],
	ExpressionT :> pform["expression"],
	Defaulting[t_, _] :> %[t],
	Nullable[t_] :> %[t],
	_ImageT :> pform["image"], (* TODO: special cases for known size, colspace *)
	sym_Symbol /; Context[sym] === "System`" :> sym,
	EncodedType[_, t_] :> %[t],
	DecodedType[_, t_] :> %[t],
	TypeAliasP :> %[ResolveAlias @ type],
	l_List :> Map[%, l],
	DistributionT :> adj["univariate", "distribution"],
	type
];

eitherForm[{a_, b_}] := 
	StringForm["either `` or ``", form[a], form[b]];
eitherForm[list_] := 
	StringForm["either ``, or ``", Row[form /@ Most[list], ","], form @ Last[list]];

tupleForm[list_] := Panic["NotImplemented"];

structForm[rules_] := Scope[
	StringForm[
		"`` containing the keys `` and ``", 
		pform["association"],
		Row[structEntry @@@ Most[rules], ","], structEntry @@ Last[rules]
	]
];

structEntry[key_, t_] := 
	StringForm["`` (``)", "\"" <> key <> "\"", single[form @ t]]; 

size[n_List] := Row[n, "\[Times]"];

DeclareSymbolicHead[ppform];

ppform[p_pform] := ppform @@ p;

adj[a_, b_] := StringForm["`` ``", psform[a], ppform[b]];
dashadj[a_, b_, c_] := StringForm["``-`` ``", psform[a], b, ppform[c]];

num[SizeT|NaturalT, b_] := plural[b];
num[1, b_] := b;
num[a_, b_] := StringForm["`` ``", a, plural[b]];

of[a_, b_] := StringForm["`` of ``", pform[a], b];
from[a_, k_, v_] := StringForm["`` from `` to ``", pform[a], plural[k], plural[v]];


PackageScope["Coerce"]

NNSetUsage @ "
Coerce[src$, target$] coerces src$ to match target$, or panics if that is impossible.
"

Coerce[in_, type_] := ($type = None; coerce[in, type]);

coerce[m_Missing, Defaulting[t_, d_]] := d;

General::netreqparam = "Required parameter `1` was not specified.";
coerce[m_Missing, _] := panic["netreqparam", key];

coerce[in_, t:IntervalScalarT[min_, max_]] := Scope[
	n = N[in];
	If[!MachineRealQ[n] || n < min || n > max, panictype[t]];
	n
];

coerce[in_, ScalarT] := 
	Replace[N[in], {t_ ? MachineRealQ :> t, _ :> panictype[ScalarT]}];

$SimplePredicates = Association[
	BooleanT -> BooleanQ,
	StringT  -> StringQ,
	IntegerT -> MachineIntegerQ,
	NaturalT -> Internal`NonNegativeMachineIntegerQ,
	PosIntegerT -> Internal`PositiveMachineIntegerQ,
	SizeT -> Internal`PositiveMachineIntegerQ,
	TypeT -> ValidTypeQ,
	TypeExpressionT -> ValidTypeExpressionQ,
	FunctionT -> MatchQ[_Function]
];

coerce[in_, type:(Alternatives @@ Keys[$SimplePredicates])] :=
	If[TrueQ[$SimplePredicates[type] @ in],
		in, panictype[type]
	];

(* to make InheritsFrom work, bit of a hack/special case *)
coerce[x_, Defaulting[_, x_]] := x;

(* currently coercing type, used for 'panic' *)
$type = None; 

(* TODO: these are mainly for convolution, revisit later when we support arbitrary rank *)
coerce[n_Integer ? Positive, ListT[2, PosIntegerT|SizeT]] := {n, n};
coerce[n_Integer ? NonNegative, ListT[2, NaturalT]] := {n, n};

coerce[in_, type_] := Match[
	If[$type === None, $type = type, type]
	,
	RuleT[kt_, vt_] :>
		Match[in, 
			Rule[k_, v_] :> Rule[%[k, kt], %[v, vt]],
			panic[]
		],

	TensorT[_, dims_] :> 
		CoerceArray[in, Replace[dims, SizeListT[n_] :> n]],

	c_ChannelT :> 
		coerce[in, ExpandChannels[c]],

	IndexIntegerT[max_Integer] :> 
		If[Developer`PositiveMachineIntegerQ[in] && in <= max,
			in, panic[]
		],

	ImageT[size_, color_] :> Scope[
		If[!Image`ValidImageQ[in], panic[]];
		img = in;
		If[MatchQ[size, {_Integer,_Integer}] && ImageDimensions[in] =!= size, 
			img = ImageResize[img, size]];
		If[StringQ[color] && ImageColorSpace[in] =!= color,
			img = ColorConvert[img, color]];
		img
	],

(*  this shouldn't come up, and wouldn't TypeForm correctly on panic anyway.
	t_List :> (
		If[!ListQ[in] || Length[in] =!= Length[t], panic[]];
		MapThread[coerce, {in, t}]
	),
*)
	AssocT[k_, v_] :> 
		If[!AssociationQ[in], panic[],
			Association @ KeyValueMap[
				%[#1, k] -> %[#2, v]&, 
				in
			]
		],

	StructT[rules_] :> Scope @ 
		If[!AssociationQ[in], panic[],
		If[!SubsetQ[Keys[rules], Keys[in]], 
			keypanic[First @ Complement[Keys[in], Keys[rules]]],
			Association[
				#1 -> CoerceParam[#1, Lookup[in, #1], #2]& @@@
				rules
			]
		]],

	EnumT[alts_] :> 
		If[!MatchQ[in, Alternatives @@ alts], panic[], in],

	EitherT[ts_] :> Scope[
		Do[
			If[!FailureQ[res = CoerceSoft[in, t]], Return[res, Block]],
			{t, ts}
		];
		panic[]
	],

	ListT[n_, t_] :> (
		If[!ListQ[in], panic[]];
		If[IntegerQ[n] && n =!= Length[in], panic[]];
		Catch @ Block[{panic := Throw[$Failed]},
			Map[coerce[#, t]&, in]
		] // OnFail[panic[]]
	),

	Defaulting[t_, _] :> %[in, t],

	Nullable[t_] :> If[in === None, None, %[in, t]],

	MatchT[t_] :> If[!MatchQ[in, t], panic[], in],

	TypeAliasP :> %[in, ResolveAlias @ type],

	EncodedType[_, t_] :> %[t],
	DecodedType[_, t_] :> %[t],

	ExpressionT :> in,

	DistributionT :> Which[
		UnivariateDistributionQ[in], in, 
		NumericQ[in], If[in < $MachineEpsilon, 
			UniformDistribution[{0,0}], 
			NormalDistribution[0, N[in]]
		],
		True, panic[]
	],

	panic[]
];

General::netinvkey = "`2` is not a valid parameter to `1`.";
keypanic[key_] := panic["netinvkey", key];


PackageScope["CoerceArray"]

CoerceArray[arr_List, dims_] /; ArrayQ[arr, _, NumberQ] && dimsMatchQ[arr, dims] := 
	RawArray["Real32", arr];

CoerceArray[arr_RawArray ? RawArrayQ, dims_] /; dimsMatchQ[arr, dims] := 
	If[RawArrayType[arr] === "Real32", arr,
		RawArray["Real32", Normal[arr]]
	];

dimsMatchQ[arr_, type_] := Match[type, 
	RankTP :> True,
	n_Integer :> ArrayDepth[arr] === n,
	list_List :> And[
		ArrayDepth[arr] === Length[list],
		And @@ MapThread[dimMatchQ, {Dimensions[arr], list}]
	]
];

dimMatchQ[n_, m_Integer] := n == m;
dimMatchQ[n_, SizeT] := True;

General::netinvrank = "The value of `` should be a numeric tensor of rank ``.";
CoerceArray[_, n_Integer] := panic["netinvrank", n];

General::netinvdims = "The value of `` should be a numeric tensor of dimensions ``.";
CoerceArray[_, dims_List] := 
	If[MatchQ[dims, {SizeT..}],
		panic["netinvrank", Length[dims]],
		panic["netinvdims", dims]
	];

General::netinvtensor = "The value of `` should be a numeric tensor.";
CoerceArray[_, _] := panic["netinvtensor"];


PackageScope["CoerceSoft"]

CoerceSoft[data_, type_] := Block[{panic := Throw[$Failed], $type}, Catch @ coerce[data, type]];

General::netinvparam = "The value of the parameter `` should be ``.";
panic[] := panic["netinvparam", TypeForm[$type]];

(* for one or two custom places that don't use $type *)
panictype[type_] := panic["netinvparam", TypeForm[type]];

panic[msg_, args___] := 
	ThrowTaggedFailure["InvalidParameter", msg, $currParam, args];


PackageScope["CoerceParam"]

CoerceParam[_, Automatic, type_] := type;

CoerceParam[name_, data_, type_] := Scope[
	$currParam = name;
	Coerce[data, type]
];


PackageScope["TensorTypeQ"]

NNSetUsage @ "
TensorTypeQ[type$] gives True if type$ represents a numeric tensor.
"

TensorTypeQ[_ChannelT | _TensorT] := True;
TensorTypeQ[_] := False;


PackageScope["RandomType"]

NNSetUsage @ "
RandomType[] evaluates to a pseudorandom type.
"

DefineMacro[Zipf,
Zipf[exprs__] := 
	ToQuoted[RandomChoice,
		1./Range[Length[Unevaluated[{exprs}]]] :> 
		{exprs}
	]
];

RandomType[] := randType;

randType := Zipf[
	randAtom,
	ListT[randSize, randAtom],
	EnumT[Zipf[{"A","B"},{"A","B","C"}, ListT[randSize, StringT]]],
	Zipf[
		Nullable[randType],
		With[t = randType, Defaulting[t, RandomInstance[t]]]
	],
	Zipf[
		StructT[{"A" -> randAtom, "B" -> randAtom}],
		AssocT[randType, randAtom],
		TypeT, ExpressionT
	]
];

randTensor := Zipf[
	TensorT[Table[randSize, randLen]],
	TensorT[randSize],
	ChannelT[randSize, randTensor]
];

randAtom := Zipf[
	Zipf[ScalarT, IntegerT],
	Zipf[SizeT, NaturalT, PosIntegerT, IndexIntegerT[RandomInteger[10]]],
	Zipf[StringT, BooleanT],
	randTensor
];

randSize := RandomChoice[{SizeT, randLen}];
randLen := RandomInteger[{1,3}];


PackageScope["EnumerateSimpleTypes"]

$simpleLeaves = {
	PosIntegerT, IntegerT, ScalarT, NaturalT,
	SizeT, StringT, BooleanT, TypeT
};

$simpleTensors = Flatten @ {
	Table[TensorT[i], {i, 1, 3}],
	Table[TensorT[Range[i]], {i, 1, 3}],
	TensorT[]
};

EnumerateSimpleTypes[] := Scope[
	leaves = Flatten @ {
		$simpleLeaves,
		$simpleTensors
	};
	channeledTensors1 = channeled @ $simpleTensors;
	channeledTensors2 = channeled @ channeledTensors1;
	complexLeaves = Flatten @ {
		channeledTensors1,
		channeledTensors2
	};
	Flatten[{
		leaves, 
		decorate /@ leaves,
		complexLeaves,
		decorate /@ complexLeaves,
		combine @@@ Tuples[leaves, 2]
	}]
];

channeled[t_] := Flatten @ {
	ChannelT[SizeT, #]& /@ t,
	ChannelT[2, #]& /@ t
};

decorate[t_] := {
	Nullable[t], Defaulting[t], 
	ListT[NaturalT, t], ListT[2, t]
};

combine[a_, b_] := {
	RuleT[a, b],
	AssocT[a, b],
	StructT[{"A" -> a, "B" -> b}],
	EitherT[{a, b}]
};

PackageScope["RandomInstance"]

NNSetUsage @ "
RandomInstance[type$] generates a random instance of type$.
"

RandomInstance[type_] := CatchFailure @ Match[type,

	ScalarT :> RandomReal[],

	TensorT[_, dims_List] :> RandomReal[1, dims /. SizeT -> RandomInteger[{1,10}]],
	
	TensorT[rank_Integer, _] :> RandomReal[1, RandomInteger[{1,5}, rank]],
	
	TensorT[_, _] :> %[TensorT[RandomInteger[{1,3}], Null]],
	
	c_ChannelT :> %[ExpandChannels[c]],

	BooleanT :> RandomChoice[{True,False}],

	StringT :> RandomChoice[{"A","B","C"}],

	IntegerT :> RandomInteger[{-10,10}],

	NaturalT :> RandomInteger[10],

	PosIntegerT | SizeT :> RandomInteger[{1,10}],

	IndexIntegerT[max_Integer] :> RandomInteger[{1, max}],

	IndexIntegerT[_] :> RandomInteger[{1,10}],

	EnumT[e_List] :> RandomChoice[e],

	EnumT[_] :> RandomChoice[{"A","B","C"}],

	EitherT[e_List] :> %[RandomChoice[e]],

	ListT[NaturalT, t_] :> %[ListT[RandomInteger[5], t]],

	ListT[n_Integer, t_] :> Table[%[t], n],

	StructT[rules_List] :> Map[%, Association[rules]],

	RuleT[k_, v_] :> Rule[%[k], %[v]],

	AssocT[k_, v_] :> Association @ Table[
		Rule[%[k], %[v]], RandomInteger[6]
	],

	ImageT[size_List, color_String] :> RandomImage[1, size /. SizeT :> RandomInteger[{1,32}], ColorSpace -> color],
	ImageT[SizeListT[2], space_] :> %[ImageT[{SizeT, SizeT}, space]],
	ImageT[size_, ColorSpaceT] :> %[ImageT[size, RandomChoice[{"RGB", "Grayscale"}]]],

	Defaulting[t_, _] :> %[t],

	Nullable[t_] :> If[RandomReal[] < 0.5, %[t], None],

	TypeT :> RandomType[],

	EncodedType[_, t_] :> %[t],
	DecodedType[_, t_] :> %[t],

	TypeAliasP :> %[ResolveAlias[type]]
];


PackageScope["TestType"]

TestType[data_, type_] :=
	ToLiteralTypeTest[type] @ data;


PackageScope["ToLiteralTypeTest"]

ToLiteralTypeTest[type_] := 
	Memoized[
		makeTest[type] /. (* bugfix for VectorQ[{}, MachineIntegerQ] -> False *)
			HoldPattern[VectorQ[q_, MachineIntegerQ]] :> 
				MachineIntegerVectorQ[q]
		, 
		Method -> "Inline"
	];

MachineIntegerVectorQ[{}] := True;
MachineIntegerVectorQ[e_] := VectorQ[e, MachineIntegerQ];

makeTest[type_] := Match[type,

	ScalarT :> RealQ,
	
	TensorT[_, dims_List] :> Composition[
		MatchQ[dims /. SizeT -> _],
		MachineArrayDimensions
	],
	
	TensorT[rank_Integer, _] :> Composition[
		EqualTo[rank],
		MachineArrayRank
	],

	TensorT[_, _] :> MachineArrayQ,

	BooleanT :> BooleanQ,

	c_ChannelT :> %[ExpandChannels[c]],

	StringT :> StringQ,

	IntegerT :> MachineIntegerQ,

	NaturalT :> Internal`NonNegativeMachineIntegerQ,

	PosIntegerT | SizeT :> Internal`PositiveMachineIntegerQ,

	IndexIntegerT[max_] :> Function[in, Internal`PositiveMachineIntegerQ[in] && in <= max],

	TypeT :> ValidTypeQ,

	ExpressionT :> (True&),

	FunctionT :> MatchQ[_Function],

	EitherT[{a_, b_}] :> With[{ap = %[a], bp = %[b]}, ap[#] || bp[#]&],

	EitherT[ts_List] :> OrOperator[% /@ ts],

	ListT[NaturalT, t_] :> With[t2 = %[t], VectorQ[#, t2]&],
	
	ListT[n_Integer, t_] :> With[t2 = %[t], Length[#] === n && VectorQ[#, t2]&],

	StructT[rules_List] :> StructPredicate[MapAt[%, rules, {All, 2}]],
	
	RuleT[k_, v_] :> With[{kt = %[k], vt = %[v]}, MatchQ[Rule[_ ? kt, _ ? vt]]],	

	AssocT[k_, v_] :> With[{kt = %[k], vt = %[v]}, 
		AssociationQ[#] && VectorQ[Keys[#], kt] && VectorQ[Values[#], vt]&
	],

	MatchT[patt_] :> MatchQ[patt],

	ImageT[size_, color_] :> ReplaceAll[
		Function @ And[
			Image`ValidImageQ[#],
			%[size] @ ImageDimensions[#],
			%[color] @ ImageColorSpace[#]
		],
		m_makeTest :> RuleCondition[m]
	],

	Defaulting[t_, _] :> With[p = %[t],
		MissingQ[#] || p[#]&
	],

	Nullable[t_] :> With[p = %[t],
		If[# === None, True, p[#]]&
	],

	EncodedType[_, t_] :> %[t],
	DecodedType[_, t_] :> %[t],

	TypeAliasP :> %[ResolveAlias[type]]
];

PackageScope["StructPredicate"]

StructPredicate[rules_][assoc_] := 
	AssociationQ[assoc] && SubsetQ[Keys[rules], Keys[assoc]] && Catch[
		MapThread[
			If[!#1[#2], Throw[False]]&,
			{Values[rules], Lookup[assoc, Keys[rules]]}
		];
		True
	];


PackageScope["MachineArrayDimensions"]

MachineArrayDimensions[e_] := 
	If[RawArrayQ[e] || PackedArrayQ[e] || ArrayQ[e, _, MachineQ], 
		Dimensions[e],
		If[MachineQ[e], {}, $Failed]
	];

PackageScope["MachineArrayRank"]

MachineArrayRank[e_] := 
	If[RawArrayQ[e] || PackedArrayQ[e] || ArrayQ[e, _, MachineQ],
		ArrayDepth[e],
		$Failed
	];


PackageExport["MachineArrayQ"]

MachineArrayQ[e_] := RawArrayQ[e] || PackedArrayQ[e] || ArrayQ[e, _, MachineQ];

NNSetUsage @ "
MachineArrayQ[expr$] gives True if expr$ is a RawArray, a packed array, or an array of machine numbers.
"


PackageScope["ValidTypeQ"]

NNSetUsage @ "
ValidTypeQ[type$] gives True if type$ is a well-formed type.
"

(* TODO: prevent things like ChannelT[5, StringT] *)

ValidTypeQ[e_] := Match[e, 
	MacroEvaluate[Alternatives @@ $AtomicTypes] :> True,
	IndexIntegerT[SizeP] :> True,
	ListT[NatP, t_] :> %[t],
	EitherT[ts_] :> VectorQ[ts, %],
	Defaulting[t_, _] :> %[t],
	Nullable[t_] :> %[t],
	ChannelT[SizeP, t_] :> %[t],
	TensorT[NatP, SizeListP] :> True,
	EnumT[ListT[NatP, StringT]] :> True,
	EnumT[{StringP..}] :> True,
	ImageT[SizePairP, ColorSpaceT | MacroEvaluate[Alternatives @@ $ColorSpaces]] :> True,
	MacroEvaluate[$TypeAliasPatterns] :> True,
	MatchT[_] :> True,
	RuleT[k_, v_] :> %[k] && %[v],
	StructT[{Repeated[_String -> (_?%)]}] :> True,
	AssocT[k_, v_] :> %[k] && %[v],
	EncodedType[_, t_] :> %[t],
	DecodedType[_, t_] :> %[t],
	False
];

PackageScope["ValidTypeExpressionQ"]

NNSetUsage @ "
ValidTypeExpressionQ[type$] gives True if type$ is a well-formed type that can potentially include NetPort expressions.
"

NatPortP = NatP | NetPortP;
SizePortP = SizeP | NetPortP;
SizePortListP = ListT[NatPortP, SizeT] | {SizePortP...} | NetPortP;

ValidTypeExpressionQ[e_] := Match[e,
	NetPortP :> True,
	IndexIntegerT[SizePortP] :> True,
	ListT[NatPortP, t_] :> %[t],
	EitherT[ts_] :> VectorQ[ts, %],
	Defaulting[t_, _] :> %[t],
	Nullable[t_] :> %[t],
	ChannelT[SizePortP, t_] :> %[t],
	TensorT[NatPortP, SizePortListP] :> True,
	ImageT[SizePortListP, _String | ColorSpaceT | NetPortP] :> True,
	_ComputedType :> True,
	ValidTypeQ[e]
];


PackageScope["UnifyTypes"]

NNSetUsage @ "
UnifyTypes[type$1, type$2] unifies two types into the narrowest type that satisfies both.
"

UnifyTypes[t1_, t2_] := Catch[unify[t1, t2], unify];


PackageScope["unify"]

Clear[unify];

SetAttributes[unify, Orderless];
unify[e_, e_] := e;

unify[e_, ExpressionT] := e;

unify[m_Missing, e_] := Throw[$Failed, unify];
unify[$Failed, e_] := Throw[$Failed, unify];

unify[TypeT, e_] := e;

unify[value_String | value_Symbol, EnumT[alts_List]] := If[MemberQ[alts, value], value, Throw[$Failed, unify]];
unify[StringT, s_String] := s;

unify[BooleanT, v:True|False] := v;

unify[IntegerT, n_Integer] := n;
unify[PosIntegerT, n:PosIntP] := n;
unify[NaturalT, n:NatP] := n;
unify[IntegerT, PosIntegerT] := PosIntegerT;
unify[IntegerT, NaturalT] := NaturalT;
unify[NaturalT, PosIntegerT] := PosIntegerT;

unify[i_IndexIntegerT, PosIntegerT|NaturalT|IntegerT] := i;
unify[IndexIntegerT[n_Integer], x_Integer] /; 1 <= x <= n := x;
unify[IndexIntegerT[n1_], IndexIntegerT[n2_]] := 
	IndexIntegerT[unify[n1, n2]];

unify[ScalarT, r_ ? NumberQ] := N[r];

unify[ImageT[sz1_, c1_], ImageT[sz2_, c2_]] := ImageT[unify[sz1,sz2], unify[c1,c2]];
unify[img_Image, t_ImageT] := Coerce[img, t];
unify[t:SizeP, IntegerT|PosIntegerT] := t;
unify[SizeT, n:PosIntP] := n;

unify[EncodedType[d_, t1_], EncodedType[_, t2_]] := EncodedType[d, unify[t1, t2]];
unify[DecodedType[d_, t1_], DecodedType[_, t2_]] := DecodedType[d, unify[t1, t2]];
unify[t_, EncodedType[d_, dt_]] := EncodedType[d, unify[t, dt]];
unify[t_, DecodedType[d_, dt_]] := DecodedType[d, unify[t, dt]];

unify[EitherT[a_List], EitherT[b_List]] /; Length[a] === Length[b] := 
	EitherT[MapThread[unify, {a, b}]];

unify[d_, EitherT[tlist_List]] := 
	Replace[
		DeleteCases[$Failed] @ Map[UnifyTypes[d, #]&, tlist], {
		{} :> Throw[$Failed, unify],
		{e_} :> e,
		e_List :> EitherT[e]
	}];

unify[c_ChannelT, other_] := chunify[c, other];

Clear[chunify];
SetAttributes[chunify, Orderless];

chunify[ChannelT[n1_, t1_], ChannelT[n2_, t2_]] :=
	ChannelT[unify[n1, n2], unify[t1, t2]];

chunify[ChannelT[n1_, TensorT[rank_, dims_List]], t2_TensorT] :=
	unify[TensorT[rank+1, Prepend[n1] @ dims], t2];

chunify[ChannelT[SizeT, TensorT[n_Integer]], t_TensorT] := 
	unify[TensorT[n+1], t];

chunify[ChannelT[m_Integer, TensorT[n1_Integer]], TensorT[n2_Integer]] /; n2 == n1 + 1 := 
	ChannelT[m, TensorT[n1]];

chunify[ChannelT[m_Integer, TensorT[SizeT, _ListT]], TensorT[rank_Integer, {m_, rest___}]] := 
	ChannelT[m, TensorT[rank-1, {rest}]];

chunify[ChannelT[cn_, c_], t:TensorT[RankTP, _ListT]] := 
	ChannelT[cn, unify[t, c]];

chunify[ChannelT[cn_, c_], t:TensorT[_, {n_, rest___}]] := 
	ChannelT[unify[cn, n], unify[c, TensorT[{rest}]]];

chunify[chan_ChannelT, TensorT[rank_Integer, dims_List]] := Module[
	{tdims = GetTensorDimensions[chan]},
	TensorT[Length[tdims], unify[tdims, dims]] /; tdims =!= $Failed
];

chunify[_, _] := Throw[$Failed, unify];

unify[TensorT[n1_, t1_], TensorT[n2_, t2_]] :=
	TensorT[unify[n1, n2], unify[t1, t2]];

unify[ListT[n1_, t1_], ListT[n2_, t2_]] := ListT[unify[n1, n2], unify[t1, t2]];
unify[ListT[n_, t_], e_List] := (unify[n, Length[e]]; unify[t, #]& /@ e);
unify[t_List, e_List] /; Length[t] == Length[e] := MapThread[unify, {t, e}];

unify[EnumT[t1_], EnumT[t2_]] := EnumT[unify[t1, t2]];

unify[Defaulting[t1_, d_], Defaulting[t2_, d_]] := Defaulting[unify[t1, t2], d];

unify[d_, Defaulting[t_, _]] := unify[d, t];

unify[Nullable[t1_], Nullable[t2_]] := Nullable @ unify[t1, t2];

unify[d_, Nullable[t_]] := If[d === None, None, unify[d, t]];

unify[t_, p:TypeAliasP] := unify[t, ResolveAlias[p]];

unify[a_, b_] := Throw[$Failed, unify]


PackageScope["ConcreteParameterQ"]

NNSetUsage @ "
ConcreteParameterQ[param$] gives True if param$ is a value (e.g. a RawArray) or a fully-specialized type (e.g. a Tensor with known rank and dimensions).
"

(* TODO: rename this to SizedParameterQ *)

ConcreteTensorP = TensorT[_Integer, {___Integer}];

ConcreteParameterQ[param_] := Match[param,
	_String :> True,
	True|False|None :> True,
	t_List :> VectorQ[t, %],
	t_RawArray :> RawArrayQ[t],
	ConcreteTensorP :> True,
	ChannelT[_Integer, ConcreteTensorP] :> True,
	ChannelT[_Integer, c_ChannelT] :> %[c], 
	ImageT[{_Integer, _Integer}, _String] :> True,
	_Integer | _Real :> True,
	EncodedType[_, t_] :> %[t],
	DecodedType[_, t_] :> %[t],
	sym_Symbol /; Context[sym] === "System`" :> True, (* ActivationFunctionTon types, etc *)
	Nullable[t_] :> %[t],
	None :> True,
	False
];


(* TODO: Explain why there is a difference betwen these two. Maybe there isn't.
It's awkward. Probably reason to go with a separate "Types" key to help separate
runtime for spec-time parametricity. Then
"Type" -> FullySpecifiedTypeQ.
"Parameter" -> FullySpecifiedParmeterQ (no higher-level types like ListT etc)
"Array" -> FullySpecifiedArrayQ (TensorT, ChannelT, or actual RawArrays etc)
 *)

PackageExport["FullySpecifiedTypeQ"]

FullySpecifiedTypeQ[t_ ? System`Private`ValidQ] := True;
FullySpecifiedTypeQ[t_] := If[fullq[t], System`Private`SetValid[t]; True, False];

fullq[type_] := Match[type,
	ChannelT[_Integer, t_] :> %[t],
	ConcreteTensorP :> True,
	ScalarT|IntegerT|PosIntegerT :> True,
	IndexIntegerT[_Integer] :> True,
	EncodedType[_, t_] :> %[t],
	DecodedType[_, t_] :> %[t],
	t_List :> VectorQ[t, %],
	False
];

PackageScope["ParseMethod"]

NNSetUsage @ "
ParseMethod[spec, <|'name$1' -> {f, <|'opt$1' -> type$1, $$|>}, $$|>]'
"

ParseMethod[Automatic, methods_] := 
	defaultMethod @ methods[[1]];

defaultMethod[{f_, args_}] := 
	Compose[f, args /. Defaulting[_, v_] :> v];

General::invmethname = "Value of option Method -> `` is not Automatic, ``.";
getMethod[methods_, spec_] := 
	Lookup[methods, spec, ThrowFailure["invmethname", QuotedString[spec], QuotedStringList @ Keys[methods]]];

ParseMethod[spec_String, methods_] := 
	defaultMethod @ getMethod[methods, spec];

ParseMethod[{spec_String, rules___Rule}, methods_] := Scope[
	{f, args} = getMethod[methods, spec];
	f @ CoerceParam[spec, Association[rules], StructT @ Normal @ args]
];

General::invmethodspec = "Value of Method option should be a string or a string with options."
ParseMethod[_, _] :=
	ThrowFailure["invmethodspec"];

