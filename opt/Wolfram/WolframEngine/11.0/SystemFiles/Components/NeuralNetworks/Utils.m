Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]
PackageImport["Developer`"]


PackageScope["NNSetUsage"]

SetHoldFirst[NNSetUsage];

NNSetUsage[str_] := Scope[
	str = StringTrim[str];
	sym = First @ StringCases[str, WordCharacter..];
	ToExpression[sym, InputForm, Function[s, MessageName[s, "usage"] = str, {HoldAll}]]
];

NNSetUsage[sym_, str_] :=
	MessageName[sym, "usage"] = str;

(* make sure this definition gets applied before downvalues are created *)
ScalarT = TensorT[0, {}];


PackageScope["Self"]


PackageScope["NetP"]
PackageScope["NetGraphP"]

(* This makes it easier to declare patterns that work on either heads or pure assocs *)
Quiet[
NetP /: Verbatim[Pattern][sym_Symbol, NetP] := sym_Association | _Symbol[sym_Association];
NetP /: Verbatim[Blank][NetP] := NetP;
NetGraphP /: Verbatim[Pattern][sym_Symbol, NetGraphP] := sym_Association | HoldPattern[NetGraph[sym_Association]];
NetGraphP /: Verbatim[Blank][NetGraphP] := NetGraphP;
,
RuleDelayed::rhs
];


PackageScope["MachineQ"]

 MachineQ[x_] := MachineRealQ[x] || MachineIntegerQ[x];


PackageScope["EmbedVariables"]

SetAttributes[EmbedVariables, HoldRest];
EmbedVariables[expr_, avoid_] := 
	With[{vars = Complement[findSetVariables[expr], 
		Map[HSym, Unevaluated[avoid]]]},
		expr /. HoldPattern[Block][body_ ? checkMult] :> Block[vars, body] /. 
			{HSym[z_] :> z, Inherited[z_] :> z} /. 
			HoldPattern[Block[{}, body_]] :> body
	];

findSetVariables[hc_] := Complement[
	findDeep[hc, (Set|SetDelayed)[lhs_, _] :> scanSymbols[lhs]],
	findDeep[hc, Inherited[sym_Symbol] :> HSym[sym]]
];

SetAttributes[checkMult, HoldAllComplete];

checkMult[CompoundExpression[___, _Times, ___]] :=
	(Message[Scope::mult]; True);

checkMult[___] := True;

Scope::mult = "Multiplication detected in Scoped declaration.";


PackageScope["ScopePureFunction"]

ScopePureFunction[HoldPattern[Function[body_]]] := 
	Function @@ EmbedVariables[HoldComplete[Block[body]], {}];

ScopePureFunction[HoldPattern[Function[vars_, body_]]] := 
	Function @@ EmbedVariables[HoldComplete[vars, Block[body]], {}];

ScopePureFunction[HoldPattern[Function[vars_, body_, attrs_]]] := 
	Function @@ EmbedVariables[HoldComplete[vars, Block[body], attrs], {}];


PackageScope["findDeep"]

findDeep[hc_, rule_] := Dedup @ Flatten @ Cases[hc, rule, Infinity, Heads -> True];

SetAttributes[scanSymbols, HoldAllComplete];
 scanSymbols[sym_Symbol] := HSym[sym];
 scanSymbols[syms:{__Symbol}] := Thread @ HSym[syms];
 scanSymbols[(DownValues|UpValues|SubValues|FormatValues)[___]] := {};
 scanSymbols[lhs_Symbol[___]] /; Context[lhs] =!= "System`" := HSym[lhs];
 scanSymbols[___] := {};
 HSym[Inherited] := {};


PackageScope["AMap"]

AMap[f_, list_] := Association[# -> f[#]& /@ list];
AMap[f_][list_] := AMap[f, list];


PackageScope["IMap"]

IMap[f_, assoc_Association] := MapIndexed[f[#2[[1,1]], #1]&, assoc];
IMap[f_, list_] := MapIndexed[f[#2[[1]], #1]&, list];
IMap[f_][list_] := IMap[f, list];


PackageScope["ScanAssocList"]

ScanAssocList[f_, assoc_, list_] := Scope[
	i = 1; KeyValueMap[f[#1, #2, list[[i++]]];&, assoc];
];


PackageScope["MapFirst"]
PackageScope["MapLast"]

MapFirst[f_, e_] := MapAt[f, e, 1];
MapFirst[f_][e_] := MapFirst[f, e];
MapLast[f_, e_] := MapAt[f, e, -1];
MapLast[f_][e_] := MapLast[f, e];


PackageScope["All1"]
 All1[e_] := Part[e, All, 1];


PackageScope["Dedup"]
 Dedup[e_] := DeleteDuplicates[e];


PackageScope["SplitSelect"]

 SplitSelect[e_List, f_] := Scope[
	pred = Map[f /* TrueQ, e];
	{Pick[e, pred, True], Pick[e, pred, False]}];

 SplitSelect[e_Association, f_] := 
 	Association /@ SplitSelect[Normal[e], Last /* f];

 SplitSelect[f_][e_] := SplitSelect[e, f];


PackageScope["OnFail"]

SetAttributes[OnFail, HoldFirst];

OnFail[expr_][result_] := OnFail[expr, result];
OnFail[expr_, result_] := If[FailureQ[result], expr, result];


PackageScope["SizeP"]
PackageScope["RankTP"]
PackageScope["PosIntP"]
PackageScope["NatP"]
PackageScope["StringP"]
PackageScope["SizePairP"]
PackageScope["NetPortP"]
PackageScope["NetPortElemP"]
PackageScope["SizeListP"]

RankTP = SizeT | NaturalT;
PosIntP = _Integer ? Positive;
NatP = _Integer ? NonNegative | SizeT | NaturalT;
SizeP = PosIntP | SizeT;
StringP = _String | StringT;
SizePairP = ListT[2, SizeT] | {SizeP, SizeP};
NetPortElemP = _Integer | _String;
NetPortP = NetPort[NetPortElemP..];
SizeListP = ListT[NatP, SizeT] | {NatP...};

 SetAttributes[ComputedType, HoldRest];

(* canonicalization code: this has to live here to ensure these short forms 
evaluate properly in LHS code *)


PackageScope["convolutionOutputSize"]
convolutionOutputSize[insize_, pad_, kern_, stride_, dilation_] := 
	Floor[(insize + 2*pad - (dilation * (kern - 1) + 1)) / stride + 1];

PackageScope["deConvolutionOutputSize"]
deConvolutionOutputSize[insize_, pad_, kern_, stride_] := stride * (insize - 1) + kern - 2 * pad

PackageScope["poolingOutputSize"]
poolingOutputSize[insize_List, pad_, kern_, stride_] := Scope[
	dim1 = insize + 2*pad - kern + stride - 1;
	dim2 = insize + 2*pad - 1;
	dim = MapThread[Min, {dim1, dim2}];
	Floor[dim/stride] + 1
]

(* Compute the required workspace (in mb) for conv and deconv *)
PackageScope["convolutionRequiredWorkspace"]
PackageScope["deconvolutionRequiredWorkspace"]

PackageScope["$MinimumConvolutionWorkspaceSize"]

$MinimumConvolutionWorkspaceSize = 512;

Clear[convolutionRequiredWorkspace, deconvolutionRequiredWorkspace];

convolutionRequiredWorkspace[ichannels_, ochannels_, osize_, kernSize_, numGroup_, dtype_:"Real32"] := Scope[
	ospace = Times @@ osize;
	shapeColunit = ichannels * kernSize[[1]] * kernSize[[2]] * ospace;
	shapeDstunit = numGroup * Floor[ochannels / numGroup] * ospace;
	requiredSize = shapeColunit + shapeDstunit;
	(* num bytes per type *)
	sizeOf = Switch[dtype, "Real16", 2, "Real32", 4, "Real64", 8];
	requiredSize = Ceiling[sizeOf * requiredSize / 10^6];
	(* the minimum is not actually allocated *)
	Max[requiredSize, $MinimumConvolutionWorkspaceSize]
]

deconvolutionRequiredWorkspace[ichannels_, ochannels_, isize_, kernSize_, numGroup_, dtype_:"Real32"] := 
	convolutionRequiredWorkspace[ochannels, ichannels, isize, kernSize, numGroup, dtype]

PackageScope["indexOf"]

SetAttributes[indexOf, HoldRest];
indexOf[list_List, thing_, else_] := Block[{t = thing},
	Do[If[list[[i]] === t, Return[i, Block]], {i, Length[list]}];
	else
];


PackageScope["niceGrid"]

niceGrid[e_] := Block[{$inner = False}, igrid[e]];

igrid[e:{__Rule}] := assocGrid[Association[e], $inner];
igrid[<||>] := <||>;
igrid[{}] := {};
igrid[a_Association] := assocGrid[a, $inner];
igrid[e_] := e;

$hicolor := GrayLevel[0.96];
assocGrid[assoc_, inner_:False] := Scope[
	$inner = True;
	rowLabelGrid[List /@ Map[igrid] @ Values[assoc], Keys[assoc], inner]
];


PackageScope["rowLabelGrid"]

rowLabelGrid[rows_, labels_, inner_:False] := Scope[
	sz = If[Developer`StringVectorQ[rows], 1+0.5 * Max[StringLength /@ rows, 8], Automatic];
	grid = Grid[
		MapThread[Prepend, {rows, fmtLabels @ labels}],
		Background -> {{$hicolor}, {}}, FrameStyle -> LightGray,
		Alignment -> {Left, Baseline}, Dividers -> All, 
		If[inner, 
			{Spacings -> {1.1, 0.5}, ItemSize -> {{sz, Automatic}, 1.4}},
			{Spacings -> {1.5, 0.5}, ItemSize -> {{sz, Automatic}, 1.5}, BaseStyle -> {ShowStringCharacters -> True}}
		]
	];
	If[!inner, grid, Framed[grid, FrameStyle -> None, FrameMargins -> {{3,3},{5,5}}]]
];

fmtLabels[e_] := Map[fmtLabel1, e];

fmtLabel1[e_] := Pane[fmtLabel2[e], {Automatic, 18}, Alignment -> Bottom, BaselinePosition -> Baseline];
fmtLabel2[e_String] := Style[e, FontFamily -> "Verdana", FontSize -> 12, ShowStringCharacters -> False];
fmtLabel2[e_] := e;


PackageScope["ALookup"]

ALookup[assoc_Association, keys_] := 
	ALookup[assoc, keys, Panic["KeyMissing", "Key `` missing.", #]&];

ALookup[assoc_Association, keys_, func_] :=
	Lookup[assoc, keys, Return[ALookup2[assoc, keys, func], Lookup]];

ALookup2[assoc_, key_, func_] := func[key];

ALookup2[assoc_, keys_List, func_] := 
	Table[
		Lookup[assoc, key, func[key]],
		{key, keys}
	];


PackageScope["PrefixPorts"]
PackageScope["$path"]

PrefixPorts[e_] := If[$path === NetPort[], e, e /. p_NetPort :> RuleCondition[Join[$path, p]]];

$path = NetPort[];


PackageScope["MapAtFields"]

MapAtFields[field_, f_, net_NetP] := Block[
	{$path = Join[$path, NetPort[field, Null]]},
	MapAt[
		MapIndexed[($path[[-1]] = #2[[1,1]]; f[#1])&],
		net,
		field
	]
];

MapAtFields[field_, f_][net_] := MapAtFields[field, f, net];


PackageScope["ScanFields"]

ScanFields[field_, f_, net_NetP] := Block[
	{$path = Join[$path, NetPort[field, Null]]},
	KeyValueScan[($path[[-1]] = #1; f[#2])&, net[field]];
];
ScanFields[field_, f_][net_] := ScanFields[field, f, net];


PackageScope["MapFields"]

MapFields[field_, f_, net_NetP] := Block[
	{$path = Join[$path, NetPort[field, Null]]},
	KeyValueMap[($path[[-1]] = #1; f[#2])&, net[field]]
];
MapFields[field_, f_][net_] := MapFields[field, f, net];



PackageScope["Call"]

Call[net_, method_] := method[net["Type"]] @ net;

Call[method_][net_] := Call[net, method];


PackageScope["OrOperator"]

OrOperator[list_][e_] := AnyTrue[list, #[e]&];


PackageScope["NetPortForm"]

NetPortForm[p_NetPort] := fromStringForm[pform @@ p];

pform["Vertices", vert_, rest___] := StringForm["`` of vertex ``", pform[rest], vert];
pform[___, "Arrays", param_] := StringForm["array ``", param];
pform[___, "Parameters", param_] := StringForm["parameter ``", param];
pform[___, "Inputs", "Input"] := "input";
pform[___, "Outputs", "Output"] := "output";
pform[___, "Inputs", param_] := StringForm["input ``", param];
pform[___, "Outputs", param_] := StringForm["output ``", param];


PackageScope["Replace1"]

Replace1[rules_] := With[{u = Unique["input"]}, Function[u, Replace[u, rules, {1}]]];


PackageScope["ComposeThread"]

ComposeThread[funcs_][args_] := ComposeThread[funcs, args];

ComposeThread[funcs_, args_] :=
	MapThread[Compose, {funcs, args}];

ComposeThread[funcs_, assoc_Association] :=
	Block[{i = 0}, Map[funcs[[++i]][#]&, assoc]];


PackageScope["MapAssocAssoc"]

NNSetUsage @ "
MapAssocAssoc[f, assoc$1, assoc$2, missf$, extraf$] calls f[key$, v$1, v$2] for corresponding\
values in assoc$1, assoc$2, in the order of the keys in assoc$1, but if a key in assoc$2
is missing, missf$[key$] is called, or if an extra key is present, extraf$[key$] is called.
"

MapAssocAssoc[f_, a_, b_, missf_, extraf_] :=
	If[Length[a] =!= Length[b],
		findBadKeys[a, b, missf, extraf],
		KeyValueMap[
			f[#1, #2, Lookup[b, #1, Return[missf[#1]]]]&,
			a
		]
	];

findBadKeys[a_, b_, missf_, extraf_] := Scope[
	ka = Keys[a]; kb = Keys[b];
	missf @ First[
		Complement[ka, kb],
		Return @ extraf @ First @ Complement[kb, ka]
	]
];


PackageScope["FindDuplicates"]

FindDuplicates[elems_] :=
	First @ SelectFirst[Tally[elems], #[[2]] > 1&, Return[$Failed]];


PackageScope["PartElse"]

SetHoldRest[PartElse];
PartElse[expr_, p___, else_] := 
	Replace[
		Internal`UnsafeQuietCheck[expr[[p]], Missing[]],
		_Missing :> else
	];


PackageScope["PartExistsQ"]

PartExistsQ[expr_, part___] := 
	Internal`UnsafeQuietCheck[expr[[part]]; True, False];


PackageScope["MapB"]

MapB[False, t_] := t;
MapB[True, t_] := Map[t];


PackageScope["QuotedStringList"]

QuotedStringList[{a_}] := QuotedString[q];
QuotedStringList[{a_, b_}] := Row[{a, b}, "or", BaseStyle -> {ShowStringCharacters -> True}];
QuotedStringList[{a__,b_}] := 
	Row[{Row[{a}, ",", BaseStyle -> {ShowStringCharacters -> True}], QuotedString[b]}, ", or"];

PackageScope["QuotedString"]

QuotedString[q_] := Style[q, ShowStringCharacters -> True];



PackageScope["TestContext"]

General::invtd = "TargetDevice -> `` could not be used, please ensure that you have a compatible graphics card and have installed CUDA drivers."
TestContext[{"CPU", _}] = Null;
TestContext[gpu:{"GPU", _}] := If[
	Quiet @ FailureQ @ CatchFailure[General, 
		MXNetLink`NDArrayCreate[{1,2,3}, "Context" -> gpu]
	],
	ThrowFailure["invtd", gpu],
	TextContext[gpu] = Null;
];


PackageExport["ValidNetQ"]

ValidNetQ[_] := False;


PackageScope["TestPositiveInteger"]
PackageScope["TestPositiveIntegerVector"]

General::notposint = "Expected a positive integer."
General::notposintvec = "Expected a vector of positive integers."

TestPositiveInteger[in_] := If[!Internal`PositiveMachineIntegerQ[in], ThrowFailure["notposint"], in];
TestPositiveIntegerVector[in_] := If[!VectorQ[in, Internal`PositiveMachineIntegerQ], ThrowFailure["notposintvec"], in];


PackageScope["TestIndexInteger"]
PackageScope["TestIndexIntegerVector"]

General::notiint = "Expected an index between 1 and ``."
General::notiintvec = "Expected a vector of indices between 1 and ``."

TestIndexInteger[max_][in_] := 
	If[Internal`PositiveMachineIntegerQ[in] && in <= max, in,
		ThrowFailure["notiint", max]];

TestIndexIntegerVector[max_][in_] := 
	If[VectorQ[in, Internal`PositiveMachineIntegerQ] && Max[in] <= max, in,
		ThrowFailure["notiintvec", max]];


PackageScope["DigitStringKeysQ"]

DigitStringKeysQ[assoc_] := VectorQ[Keys[assoc], StringMatchQ[DigitCharacter..]];


PackageScope["RemapKeys"]

RemapKeys[assoc_] := Scope[
	newkeys = IntegerString @ Range[Length[assoc]];
	remapping = AssociationThread[Keys[assoc], newkeys];
	{KeyMap[remapping, assoc], remapping}
];


PackageScope["MakeCustomHeadBox"]

MakeCustomHeadBox[head_, contents_, baseline_] := Scope[
	boxes = ToBoxes @ Panel[contents, BaselinePosition -> If[$CloudEvaluation, Automatic, baseline]];
	StyleBox[TagBox[TagBox[
		RowBox[{head, RowBox[{"[", boxes, "]"}]}],
		False
	], Deploy], LineBreakWithin -> False]
];


PackageScope["RemoveRawArrays"]

RemoveRawArrays[expr_] := 
	expr /. ra_RawArray :> RuleCondition @ TensorT @ Dimensions[ra];


PackageScope["fromStringForm"]

fromStringForm[e_] := If[$CloudEvaluation, ReplaceAll[e, s_StringForm :> ToString[s]], e];

