Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]
PackageImport["Developer`"]


PackageExport["$DecoderData"]

$DecoderData = Data`UnorderedAssociation[];


PackageExport["NetDecoder"]


PackageScope["DecoderName"]

DecoderName[HoldPattern @ NetDecoder[type_, _]] := type;


PackageExport["DecoderInputType"]

DecoderInputType[HoldPattern @ NetDecoder[type_, assoc_Association]] := 
	assoc["Input"];


PackageScope["CreateDecoder"]

SetHoldAllComplete[CreateDecoder];

CreateDecoder[enc:NetDecoder[_String, Association["Parameters" -> _, "Input" -> _]]] :=
	System`Private`HoldSetNoEntry[enc];

NetDecoder::badtype = "`` is not a valid NetDecoder type."

CreateDecoder[NetDecoder[{type_String, args___}]] := 
	CreateDecoder[NetDecoder[type, args]];

CreateDecoder[NetDecoder[type_String, args___]] := Scope[
	
	data = $DecoderData[type];

	If[MissingQ[data], ThrowFailure[NetDecoder::badtype, type]];
	params = data["Parameters"];

	$i = 0; $nargs = Length[params];
	Scan[scanArg, {args}];

	assoc = <|"Parameters" -> params, "Input" -> data["Input"]|>;

	assoc = DoInference[assoc, data["InferenceRules"], List @ data["PostInferenceFunction"]];

	res = System`Private`ConstructNoEntry[
		NetDecoder, type, assoc
	];

	If[assoc, 
		System`Private`SetValid[res]
	];

	res
];

scanArg[key_Symbol -> val_] := scanArg[SymbolName[key] -> val];

scanArg[key_String -> val_] :=
	params[key] = CoerceParam[
		key, val,
		Lookup[params, key, ThrowFailure[NetDecoder::netinvkey, NetDecoder, key]]
	];

scanArg[arg_] := If[
	++$i > $nargs,
	ThrowFailure[NetDecoder::netinvarg, $head, $nargs],
	key = Keys[params][[$i]];
	params[key] = CoerceParam[Keys[params][[$i]], arg, params[[$i]]];
];

CreateDecoder[_] := Fail;


PackageExport["DecoderDepth"]

DecoderDepth[HoldPattern @ NetDecoder[type_, _]] := $DecoderData[type, "ArrayDepth"];


PackageExport["DecoderDimensions"]

DecoderDimensions[HoldPattern @ NetDecoder[_, assoc_]] := GetTensorDimensions @ assoc["Input"] /. SizeT -> _;


PackageExport["DecoderApply"]

(* for decoders that accept lists, we must first try the non-batched version,
then if that fails try the batched version (achieved via fallthrough *)

NetDecoder::notarray = "Input to NetDecoder should be a numeric tensor."
NetDecoder::invarray = "Input to NetDecoder is not a tensor of the correct dimensions."
NetDecoder::invrank = "Input to NetDecoder is not a tensor of the correct rank."

DecoderApply[decoder_, input_, prop_] := Scope[
	If[!MachineArrayQ[input] && !MachineQ[input], ThrowFailure["notarray"]];
	ddepth = DecoderDepth[decoder];
	ddims = DecoderDimensions[decoder];
	idims = Dimensions[input];
	idepth = Length[idims];
	If[prop =!= None, checkProp[decoder, prop]];
	Which[
		ddepth === 0, 
			Which[
				idepth == 0,
					checkDims[idims, ddims];
					Cached[GetDecoderFunction, decoder, prop, False] @ input,
				idepth == 1, 
					checkDims[Rest[idims], ddims];
					Cached[GetDecoderFunction, decoder, prop, True] @ input,
				True,
					ThrowFailure["invarray"]
			],
		idepth === ddepth,
			checkDims[idims, ddims];
			Cached[GetDecoderFunction, decoder, prop, False] @ input,
		idepth === ddepth + 1,
			checkDims[Rest[idims], ddims];
			Cached[GetDecoderFunction, decoder, prop, True] @ input,
		ddepth === None,
			Cached[GetDecoderFunction, decoder, prop, False] @ input,
		True,
			ThrowFailure["invrank"]
	]
];

checkDims[_, $Failed] := Null;
checkDims[idims_, ddims_] := If[!MatchQ[idims, ddims], ThrowFailure["invarray"]];

DecoderApply[___] := $Failed;


PackageScope["ToDecoderFunction"]

ToDecoderFunction[DecodedType[decoder_, _], batchq_] := 
	GetDecoderFunction[decoder, None, batchq];

ToDecoderFunction[_, _] := Identity;


PackageScope["GetDecoderFunction"]

GetDecoderFunction[HoldPattern @ NetDecoder[type_, assoc_Association], None, batchq_] :=
	$DecoderData[type, "ToDecoderFunction"][assoc["Parameters"], batchq];

GetDecoderFunction[HoldPattern @ NetDecoder[type_, assoc_Association], prop_, batchq_] := 
	OnFail[
		invProp[type, assoc],
		$DecoderData[type, "ToPropertyDecoderFunction"][assoc["Parameters"], batchq, prop]
	];

NetDecoder::invprop = "NetDecoder of type `` only supports the following properties: ``."
NetDecoder::noprops = "NetDecoder of type `` does not support properties."

invProp[type_, assoc_] := Match[
	$DecoderData[type, "AvailableProperties"],
	{} :> ThrowFailure[NetDecoder::noprops, type],
	list_List :> ThrowFailure[NetDecoder::invprop, type, list]
];


PackageExport["DefineDecoder"]

DefineDecoder[name_, assoc_] := Scope[

	assoc = CoerceParam[DefineDecoder, assoc, DecoderDefinitionT];

	DesugarTypeDefinitions[assoc, {"Parameters", "Input"}];

	$DecoderData[name] = assoc;
];

DecoderDefinitionT = StructT[{
	"Input" -> TypeExpressionT,
	"Parameters" -> Defaulting @ AssocT[StringT, TypeExpressionT],
	"InferenceRules" -> Defaulting @ ListT @ RuleT[MatchT[_NetPort], TypeExpressionT],
	"ToDecoderFunction" -> FunctionT,
	"ToPropertyDecoderFunction" -> Defaulting[FunctionT, $Failed&],
	"AvailableProperties" -> Defaulting[ListT[ExpressionT], {}],
	"ArrayDepth" -> Nullable[IntegerT]
}];


(dec_NetDecoder ? System`Private`HoldEntryQ) := UseMacros @ RuleCondition @ CatchFailure[NetDecoder, CreateDecoder[dec]];
(dec_NetDecoder ? System`Private`NoEntryQ)[input_] := CatchFailure @ DecoderApply[dec, input, None];
(dec_NetDecoder ? System`Private`NoEntryQ)[input_, prop_] := CatchFailure @ DecoderApply[dec, input, prop];
DefineCustomBoxes[NetDecoder, dec_NetDecoder ? System`Private`HoldNoEntryQ :> MakeDecoderBoxes[dec]];

MakeDecoderBoxes[HoldPattern @ NetDecoder[type_, assoc_Association]] :=
	BoxForm`ArrangeSummaryBox[
		NetDecoder, None, None,
		fmtEntries[Prepend["Type" -> type] @ assoc["Parameters"]],
		{},
		StandardForm,
		"Interpretable" -> False
	]

MakeDecoderBoxes[_] := $Failed;