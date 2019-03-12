Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]
PackageImport["Developer`"]


PackageExport["$EncoderData"]

$EncoderData = Data`UnorderedAssociation[];


PackageExport["NetEncoder"]


PackageScope["EncoderName"]

EncoderName[HoldPattern @ NetEncoder[type_, _]] := type;


PackageScope["AcceptsListsQ"]

AcceptsListsQ[enc_NetEncoder] := System`Private`ValidQ[enc];


PackageScope["EncodeFail"]

General::invencin = "Invalid input, ``.";

$noisy = True;
EncodeFail[msg_] := (
	If[$noisy && !inSingularityCodeQ[], Message[NetEncoder::invencin, fromStringForm[msg]]]; 
	Throw[$Failed, EncodeFail]
);

(* see 317744 *)
inSingularityCodeQ[] := Stack[_Simplify`FunctionSingularities] =!= {};


PackageExport["EncoderOutputType"]

EncoderOutputType[HoldPattern @ NetEncoder[type_, assoc_Association]] := 
	assoc["Output"];


PackageScope["CreateEncoder"]

SetHoldAllComplete[CreateEncoder];

NetEncoder::badtype = "`` is not a valid NetEncoder type."
NetEncoder::decnfs = "Not enough information was provided to fully specify the output of the NetEncoder.";

CreateEncoder[enc:NetEncoder[_String, Association["Parameters" -> _, "Output" -> _]]] :=
	System`Private`HoldSetNoEntry[enc];

CreateEncoder[NetEncoder[{type_String, args___}]] := 
	CreateEncoder[NetEncoder[type, args]];

CreateEncoder[NetEncoder[type_String, args___]] := Scope[
	
	data = $EncoderData[type];
	If[MissingQ[data], ThrowFailure[NetEncoder::badtype, type]];
	
	params = data["Parameters"];

	$i = 0; $nargs = Length[params];
	Scan[scanArg, {args}];

	assoc = <|"Parameters" -> params, "Output" -> data["Output"]|>;

	assoc = DoInference[assoc, data["InferenceRules"], List @ data["PostInferenceFunction"]];

	If[!FullySpecifiedTypeQ[assoc["Output"]],
		ThrowFailure[NetEncoder::decnfs]
	];

	res = System`Private`ConstructNoEntry[
		NetEncoder, type, assoc
	];

	If[TrueQ @ data["AcceptsLists"] @ assoc, 
		System`Private`SetValid[res]
	];

	res
];

scanArg[key_Symbol -> val_] := scanArg[SymbolName[key] -> val];

scanArg[key_String -> val_] :=
	params[key] = CoerceParam[
		key, val,
		Lookup[params, key, ThrowFailure[NetEncoder::netinvkey, NetEncoder, key]]
	];

scanArg[arg_] := If[
	++$i > $nargs,
	ThrowFailure[NetEncoder::netinvarg, $head, $nargs],
	key = Keys[params][[$i]];
	params[key] = CoerceParam[Keys[params][[$i]], arg, params[[$i]]];
];

CreateEncoder[_] := Fail;


PackageExport["EncoderApply"]

EncoderApply[encoder_, input_] :=
	Catch[Cached[GetEncoderFunction, encoder, False] @ input, EncodeFail];

(* for encoders that accept lists, we must first try the non-batched version,
then if that fails try the batched version (achieved via fallthrough *)
EncoderApply[encoder_ ? System`Private`ValidQ, input_List] := Block[
	{$noisy, res},
	res = Catch[Cached[GetEncoderFunction, encoder, False] @ input, EncodeFail]; 
	res /; res =!= $Failed
];

EncoderApply[encoder_, input_List] := Catch[
	Cached[GetEncoderFunction, encoder, True] @ input,
	EncodeFail
];


PackageScope["ToEncoderFunction"]

ToEncoderFunction[EncodedType[encoder_, _], batchq_] := 
	GetEncoderFunction[encoder, batchq];

ToEncoderFunction[PosIntegerT, batchq_] :=
	If[batchq, TestPositiveIntegerVector, TestPositiveInteger];

ToEncoderFunction[IndexIntegerT[max_Integer], batchq_] :=
	If[batchq, TestIndexIntegerVector[max], TestIndexInteger[max]];

ToEncoderFunction[_, _] := Identity;

GetEncoderFunction[HoldPattern @ NetEncoder[type_, assoc_Association], batchq_] :=
	$EncoderData[type, "ToEncoderFunction"][assoc["Parameters"], batchq];


PackageExport["DefineEncoder"]

DefineEncoder[name_, assoc_] := Scope[

	assoc = CoerceParam[DefineEncoder, assoc, EncoderDefinitionT];

	DesugarTypeDefinitions[assoc, {"Parameters", "Output"}];

	$EncoderData[name] = assoc;
];

EncoderDefinitionT = StructT[{
	"Output" -> TypeExpressionT,
	"Parameters" -> Defaulting @ AssocT[StringT, TypeExpressionT],
	"InferenceRules" -> Defaulting @ ListT @ RuleT[MatchT[_NetPort], TypeExpressionT],
	"ToEncoderFunction" -> FunctionT,
	"AcceptsLists" -> Defaulting[FunctionT, False&],
	"MLType" -> FunctionT
}];

(enc_NetEncoder ? System`Private`HoldEntryQ) := UseMacros @ RuleCondition @ CatchFailure[NetEncoder, CreateEncoder[enc]];
(enc_NetEncoder ? System`Private`NoEntryQ)[input_] := CatchFailure @ EncoderApply[enc, input];
DefineCustomBoxes[NetEncoder, enc_NetEncoder ? System`Private`HoldNoEntryQ :> MakeEncoderBoxes[enc]];

MakeEncoderBoxes[HoldPattern @ NetEncoder[type_, assoc_Association]] :=
	BoxForm`ArrangeSummaryBox[
		NetEncoder, None, None,
		fmtEntries[Prepend["Type" -> type] @ assoc["Parameters"]],
		{},
		StandardForm,
		"Interpretable" -> False
	]

MakeEncoderBoxes[_] := $Failed;