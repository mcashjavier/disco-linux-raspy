Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]

PackageScope["ScanInferenceRules"]
PackageScope["SowInferenceRules"]
PackageScope["SowPostInferenceFunction"]
PackageScope["ReapInferenceRules"]
PackageScope["InferenceRulesScanner"]

ScanInferenceRules[assoc_Association] := Call[assoc, InferenceRulesScanner];

InferenceRulesScanner[type_] := Function[
	SowInferenceRules[$LayerData[type, "InferenceRules"]];
	Replace[
		$LayerData[type, "PostInferenceFunction"], 
		f:Except[None] :> SowPostInferenceFunction[f]
	];
];

SowInferenceRules[e_List] := 
	Internal`StuffBag[$irules, PrefixPorts[e]];

SowPostInferenceFunction[f_] := 
	Internal`StuffBag[$pfunctions, PrefixPorts[f]];

ReapInferenceRules[e_] := Scope[
	CollectTo[{$irules, $pfunctions}, ScanInferenceRules[e]];
	{$irules, $pfunctions}
];


PackageScope["ConstructWithInference"]

ConstructWithInference[head_Symbol, assoc_Association] := Scope[
	{irules, pfuncs} = ReapInferenceRules[assoc];
	assoc = DoInference[assoc, irules, pfuncs];
	If[!FreeQ[assoc, EncodedType | DecodedType],
		(* strip the encoders/decoders from everything except the periphery of the network *)
		assoc = MapIndexed[
			If[MatchQ[#2, {Key["Inputs"|"Outputs"]}], #1,
				ReplaceAll[#1, (EncodedType|DecodedType)[_, t_] :> t]]&,
			assoc
		]
	]; 
	System`Private`ConstructNoEntry[head, assoc]
];


PackageScope["DoInference"]

Clear[DoInference];

DoInference[expr_, rules_, pfuncs_, resolveDefaults_:True] := Scope[
	ports = Dedup @ Cases[rules, _NetPort, Infinity];
	portVals = Internal`UnsafeQuietCheck[
		Extract[expr, List @@@ ports],
		findInvalidPorts[expr, ports]
	];
	vals = Data`UnorderedAssociation @ AssociationThread[ports, portVals];
	newvals = iDoInference[vals, Flatten @ rules, ports];
	$expr = expr; 
	Scan[executePostFunc, pfuncs];
	KeyValueMap[setPart, newvals]; 
	If[resolveDefaults && !FreeQ[$expr, Defaulting], 
		$expr = DoInference[$expr /. Defaulting[_, val_] :> val, rules, pfuncs];
	];
	$expr
];

executePostFunc[f_] := 
	Apply[f /. p_NetPort :> RuleCondition[Lookup[newvals, p, $expr @@ p]], {}];

findInvalidPorts[expr_, ports_] :=
	Do[
		If[MissingQ[slice[expr]], 
			Panic["InvalidPort", "`` is not present in expression.", NetPort @@ slice]],
		{slice, Slice @@@ ports}
	];

setPart[NetPort[p___], val_] := If[!ConcreteParameterQ[$expr[p]], $expr[p] = val];

makeIndex[list_, keys_] := Scope[
	rules = Dispatch[MapIndexed[# -> (Place @@ #2)&, keys]];
	invert = MapIndexed[Cases[#, Place[n_] :> Rule[n, First[#2]], {0, Infinity}]&, list /. rules];
	KeyMap[
		Part[keys, #]&, 
		Data`UnorderedAssociation @ Merge[Flatten @ invert, DeleteDuplicates]
	]
];

PackageExport["$MaxInferenceSteps"]

$MaxInferenceSteps = 32768;

iDoInference[types_, rules_, keys_] := Scope[
	
	$types = types /. r_RawArray :> RuleCondition @ TensorT[ArrayDepth[r], Dimensions[r]];
	$rules = List @@@ rules;

	$ruleindex = makeIndex[$rules, keys];
	n = Length[$rules];
	$dirty = ConstantArray[1, n];
	range = Range[n];

	maxCount = Min[32 * Length[keys], $MaxInferenceSteps];

	count = 0;
	While[
		Total[$dirty] > 0,
		Do[
			pair = {a, b} = Part[$rules, i];
			{ea, eb} = eval @ pair;
			u = Catch[unify[ea, eb], unify, catchUnifyFailure];
			If[ea =!= u, set[a, u]];
			If[eb =!= u && !FreeQ[b, NetPort], set[b, u]];
			Part[$dirty, i] = 0;
			If[count++ == maxCount, ThrowFailure["netinfexc"]];
			,
			{i, Pick[range, $dirty, 1]}
		];
	];

	(* condense expanded-out types that don't actually contain any info *)
	$types /. TensorT[rank_, dims:{SizeT..}] :> RuleCondition @ TensorT[rank, SizeListT[Length[dims]]]
];

General::netinfexc = "The given net appears to be too large to correctly infer all types. Consider increasing NeuralNetworks`$MaxInferenceSteps from its default value."

catchUnifyFailure[_, _] := reportFailure[a, b, ea, eb];

General::tyinc = "Value for `` (``) is inconsistent with value for `` (``)."
reportFailure[p1_NetPort, p2_NetPort, t1_, t2_] :=
	ThrowFailure["tyinc", NetPortForm[p1], TypeForm[t1], NetPortForm[p2], TypeForm[t2]];

General::tyfail = "Inferred inconsistent types for `` (`` versus ``).";
reportFailure[p_NetPort, p2_, t1_, t2_] := 
	ThrowFailure["tyfail", NetPortForm[p], TypeForm[t1], TypeForm[t2]];

fmtSlot[name_, s_, pos_, col_] := 
	Interpretation[
		Tooltip[
			Style[StringRiffle[pos, ":"], Bold, Darker[col, .2], ShowStringCharacters -> False],
			RawBoxes @ Cell[BoxData @ RowBox[{"NetPort", "[", 
				Sequence @@ Riffle[
					ToBoxes[Style[#, ShowStringCharacters -> True]]& /@ pos, 
					","], 
				"]"
			}], "Input"]
		],
		s
	];


Clear[set];

set[p_NetPort, value_] :=
	If[!EquivalentTypeQ[$types[p], value],
		Part[$dirty, $ruleindex[p]] = 1;
		$types[p] = value;
	];

PackageScope["EquivalentTypeQ"]

SetAttributes[EquivalentTypeQ, Orderless];
EquivalentTypeQ[a_, a_] := True;

EquivalentTypeQ[a_ChannelT, b_TensorT] := Block[
	{adim = GetTensorDimensions[a]}, 
	adim === GetTensorDimensions[b] && adim =!= $Failed
];

EquivalentTypeQ[_, _] := False;

set[TensorT[r1_, d1_], TensorT[r2_, d2_]] :=
	(set[r1, r2]; set[d1, d2];)

set[ListT[n1_, t1_], ListT[n2_, t2_]] := (set[n1, n2]; set[t1, t2];)
set[ListT[n_, t_], e_List] := (set[n, Length[e]]; Scan[set[t, #]&, e];)
set[e_List, ListT[n_, t_]] := (set[Length[e], n]; Scan[set[#, t]&, e];)
set[a_List, b_List] /; Length[a] == Length[b] := MapThread[set, {a, b}];

set[EnumT[t1_], EnumT[t2_]] := set[t1, t2];

set[ImageT[sz_, c_], img_Image] := 
	(set[sz, ImageDimensions[img]]; set[c, ImageColorSpace[img]];);

set[ImageT[sz1_, c1_], ImageT[sz2_, c2_]] :=
	(set[sz1, sz2]; set[c1,c2];);

set[ChannelT[c1_, e1_], ChannelT[c2_, e2_]] := 
	(set[c1, c2]; set[e1, e2];);

set[ChannelT[c1_, e1_], TensorT[rank_, {d1_, rest___}]] := 
	(set[c1, d1]; set[e1, TensorT[rank-1, {rest}]];);

set[TensorT[rank_, dims_], chan_ChannelT] := Module[
	{cdims = GetTensorDimensions[chan]},
	If[ListQ[cdims],
		set[dims, cdims]; 
		set[rank, Length[cdims]];
	]
];

set[TensorT[rank_, {d1_, rest___}], ChannelT[c1_, e1_]] := 
	(set[d1, c1]; set[TensorT[rank, {rest}], e1];);

set[ChannelT[chans_, TensorT[rank1_, dims1_]], TensorT[rank2_Integer, dims2_]] := (
	set[rank1, rank2 - 1];
);

set[t_, EncodedType[_, t2_]] := set[t, t2];
set[t_, DecodedType[_, t2_]] := set[t, t2];

set[PosIntegerT, PosIntP] := Null;
set[NaturalT, NatP] := Null;
set[SizeT, PosIntP] := Null;
set[SizeT, SizeT] := Null;

set[a_Integer, b_Integer] := If[a =!= b, spanic[]];

set[_ComputedType, _] := Null;

set[ExpressionT, _] := Null;

set[Nullable[_], None] := Null;

set[Nullable[t_], d_] := set[t, d];

set[EitherT[list_List], d_] := Block[
	{spanic := Throw[$Failed]},
	Do[ (* gah, this technically has to be transactional if types are ambigious. f that. *)
		If[!FailureQ[Catch @ set[t, d]], Break[];],
		{t, list}
	]
];

set[Defaulting[t_, _], d_] := set[t, d];

set[IndexIntegerT[n_Integer], x_Integer] := If[x < 1 || x > n, spanic[]];

set[IndexIntegerT[n1_], IndexIntegerT[n2_]] := set[n1, n2];

set[a_, b_] := spanic[];

spanic[] := ipanic[];

eval[e_] := e /. {
	c_ComputedType :> evalCT[strongeval @ c],
	p_NetPort :> Lookup[$types, p, Panic["Could not find port `` in types ``.", p, $types]]
};

strongeval[e_] := e //. {
	EncodedType[_, t_] :> t,
	DecodedType[_, t_] :> t,
	p_NetPort :> RuleCondition[$types[p]]
};

evalCT[ComputedType[type_, expr_, deps_, trigger_:False]] := Scope[
	If[!VectorQ[deps, ConcreteParameterQ] && !TrueQ[trigger],
		Return[type]];
	Check[
		res = expr;
		If[!FailureQ[res] && !FailureQ[UnifyTypes[res, type]], res, type]
	, 
		type
	]
]


PackageScope["TypeDependenceGraph"]

(* TODO: make this easier to use directly, e.g. takes a net directly *)

TypeDependenceGraph[rules_, index_] := Scope[
	edges = Flatten @ KeyValueMap[toRule[#1, rules[[#2]]]&, index];
	edges = DeleteDuplicatesBy[edges, Sort[First[#]]&];
	Graph[edges, VertexLabels -> Placed["Name", Tooltip]]
];

SetAttributes[toRule, Listable];
toRule[port_, rule_Rule] := Tooltip[port <-> #, rule]& /@ DeleteCases[Dedup @ DeepCases[rule, _NetPort], port];



PackageScope["FailValidation"]

General::valfail = "Validation failed for ``: ``";

FailValidation[layer_, reason_] := ThrowFailure["valfail", layer, reason];