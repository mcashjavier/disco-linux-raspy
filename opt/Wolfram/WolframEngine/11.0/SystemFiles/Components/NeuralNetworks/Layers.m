Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]
PackageImport["Developer`"]


PackageExport["$FromMXNetName"]
PackageExport["$ToMXNetName"]
PackageExport["$ShortNames"]

$FromMXNetName = Data`UnorderedAssociation[];
$ToMXNetName = Data`UnorderedAssociation[];
$ShortNames = Data`UnorderedAssociation[];


PackageExport["$LayerData"]

$LayerData = Data`UnorderedAssociation[];


PackageExport["DefineLayer"]

DefineLayer::noparent = "There is no layer called `` to inherit from (yet)."

DefineLayer[name_, assoc_Association] := CatchFailure @ Scope[

	symbolName = name <> "Layer";
	If[NameQ["System`" <> symbolName],
		symbolName = "System`" <> symbolName,
		symbolName = "NeuralNetworks`" <> symbolName
	];
	symbol = Symbol[symbolName];

	If[StringQ[parent = assoc["InheritsFrom"]],
		If[!AssociationQ[parentData = $LayerData[parent]],
			ThrowFailure["noparent", parent];
		];
		assoc = Join[KeyDrop[parentData, {"Type", "Symbol"}], assoc];
	];

	assoc = CoerceParam[DefineLayer, assoc, LayerDefinitionT] /. 
		With[{symbol = symbol}, FailValidation[reason_] :> FailValidation[symbol, reason]];

	(* turn "Input" -> foo into "Inputs" -> <|"Input" -> foo|> *)
	Do[
		If[assoc[key] =!= None,
			assoc[key <> "s"] = <|key -> assoc[key]|>;
		];
		KeyDropFrom[assoc, key];
		,
		{key, {"Input", "Output"}}
	];

	DesugarTypeDefinitions[assoc, {"Inputs", "Outputs", "Arrays", "Parameters"}];

	If[assoc["MXNet"] =!= None,
		mxname = assoc["MXNet", "Name"];
		$FromMXNetName[mxname] = name;
		$ToMXNetName[name] = mxname;
		aliases = assoc["MXNet", "Aliases"];
		If[ListQ[aliases],
			AssociateTo[$FromMXNetName, Thread[aliases -> name]]
		];
	];

	AssociateTo[assoc, {
		"Type" -> name,
		"Symbol" -> symbol
	}];

	options = Map[
		Match[Defaulting[_, val_] :> val, Automatic],
		assoc["Parameters"]
	];

	$LayerData[name] ^= assoc;
	$ShortNames[name] = Lookup[assoc, "ShortName", name];
	$TypeToSymbol[name] = symbol;

	SetupLayerDispatch[
		symbol, name, Keys @ assoc["Arrays"], 
		Keys @ assoc["Inputs"], Keys @ assoc["Outputs"], 
		options
	];

	assoc
];

LayerDefinitionT = StructT[{
	"Input" -> Defaulting @ TypeExpressionT,
	"Output" -> Defaulting @ TypeExpressionT,
	"Inputs" -> Defaulting @ AssocT[StringT, TypeExpressionT],
	"Outputs" -> Defaulting @ AssocT[StringT, TypeExpressionT],
	"Arrays" -> Defaulting @ AssocT[StringT, TypeExpressionT],
	"Parameters" -> Defaulting @ AssocT[StringT, TypeExpressionT],
	"InferenceRules" -> Defaulting @ ListT @ RuleT[MatchT[_NetPort], TypeExpressionT],
	"PostInferenceFunction" -> Defaulting @ ExpressionT,
	"AuxArrays" -> Defaulting @ ListT[StringT],
	"MXNet" -> Defaulting @ StructT[{
		"Name" -> StringT, 
		"FusionRules" -> Defaulting[ListT[RuleT[ExpressionT, StringT]], None],
		"Parameters" -> Defaulting @ AssocT[StringT, EitherT[{StringT, MatchT[_FixedValue]}]],
		"Arrays" -> Defaulting @ AssocT[StringT, StringT],
		"Reader" -> Defaulting @ FunctionT,
		"Writer" -> Defaulting @ FunctionT,
		"Skip" -> Defaulting[BooleanT, False],
		"Aliases" -> Defaulting @ ListT[StringT]
	}],
	"ToInputPreprocessor" -> Defaulting @ FunctionT,
	"ShortName" -> Defaulting @ StringT,
	"WLEquivalent" -> Defaulting[ExpressionT, Missing[]],
	"InheritsFrom" -> Defaulting[StringT, None]
}];

SetupLayerDispatch[symbol_, name_, pkeys_, ins_, outs_, options_] := (
	(s_symbol ? System`Private`HoldEntryQ) := CatchFailure @ CanonicalizeLayer[name, s];
	Options[symbol] = Join[
		Thread[pkeys -> None], 
		Normal @ KeySelect[options, !StringStartsQ[#, "$"]&],
		Thread[ins -> Automatic],
		Thread[outs -> Automatic]
	];
	SetupGenericDispatch[symbol];
	DefineCustomBoxes[symbol, s_symbol ? System`Private`HoldNoEntryQ :> MakeLayerBoxes[s]];
);


PackageExport["LayerData"]

LayerData[name_String, parts___] := Slice[parts] @ $LayerData[name];
LayerData[nf_Association, parts___] := Slice[parts] @ nf;
LayerData[parts___] := Slice[parts] @ $LayerData;



PackageExport["LayerInfo"]

LayerInfo[parts___] := niceGrid[LayerData[parts]]


PackageScope["ConstructLayer"]

ConstructLayer[assoc_Association] :=
	ConstructLayer[$TypeToSymbol[assoc["Type"]], assoc];

ConstructLayer[head_Symbol, assoc_Association] := 
	System`Private`ConstructNoEntry[head, assoc];


PackageScope["CanonicalizeLayer"]

SetHoldRest[CanonicalizeLayer];

CanonicalizeLayer[name_String, head_Symbol[data_Association ? AssociationQ]] := Scope[
	net = System`Private`ConstructNoEntry[head, data];
	FullySpecifiedNetQ[net]; (* trigger setting of valid flag *)
	net
]

CanonicalizeLayer[name_String, head_Symbol[args___]] := iCanonicalizeLayer[name, head, args];

iCanonicalizeLayer[name_String, head_Symbol, Verbatim[Blank][___] | Verbatim[Pattern][_, _]] := 
	Fail;

General::netnargs = "`` arguments were provided, expected ``."

iCanonicalizeLayer[name_, head_, args___] := Scope[
	$definition = $LayerData[name];
	$parseinputhead = head;
	UnpackAssociation[$definition,  
		$params:"Arrays", 
		$hypers:"Parameters", 
		$inputs:"Inputs", 
		$outputs:"Outputs"
	];
	$userhypers = Association[];
	$nargs = Length[$hypers]; 
	$args = Keys[$hypers]; 
	$i = 0; 
	Scan[scanArg, {args}];
	(* TODO: clean this up a bit *)

	finalHypers = Association @ KeyValueMap[
		#1 -> resolveDefault[#2, $userhypers[#1]]&,
		$hypers
	];
	assoc = Association[
		"Type" -> name,
		"Arrays" -> $params,
		"Parameters" -> finalHypers,
		"Inputs" -> $inputs,
		"Outputs" -> $outputs
	];
	ConstructWithInference[head, assoc]
];

(* this all could be simpler if we tried HyperParameters and Parameters simply as 
StructTs *)

resolveDefault[Defaulting[t_, val_], uval_] := If[MissingQ[uval], val, uval];

resolveDefault[t_, uval_] := If[MissingQ[uval], t, uval];

Clear[procArg];

General::netinvarg = "`` takes a maximum of `` arguments.";

scanArg[value_] := If[++$i > $nargs, 
	ThrowFailure["netinvarg", $parseinputhead, $nargs],
	key = $args[[$i]]; 
	$userhypers[key] = CoerceParam[key, value, $hypers[key]];
];

General::netinvrule = "`` is not a valid option for ``."
scanArg[r_Rule] := ThrowFailure["netinvrule", r, $parseinputhead];

(* TODO: Lookup either a Parameter OR a HyperParameter *)
scanArg[Rule[param_String, value_]] := 
	Which[
		KeyExistsQ[$params, param],
			$params[param] = CoerceParam[param, value, $params[param]];,
		KeyExistsQ[$hypers, param],
			$userhypers[param] = CoerceParam[param, value, $hypers[param]];,
		KeyExistsQ[$inputs, param],
			$inputs[param] = ParseInputSpec[param, $inputs[param], value];,
		KeyExistsQ[$outputs, param],
			$outputs[param] = ParseOutputSpec[param, $outputs[param], value];,
		True,
			ThrowFailure["netinvkey", $parseinputhead, param]
	];



PackageScope["ParseInputSpec"]

General::invenc = "Encoder of type ``, which produces ``, is not appropriate for port ``, which expects ``."
ParseInputSpec[param_, type_, $Raw[t_]] := t;

ParseInputSpec[param_, type_, encoderSpec:(_String | {_String, ___})] := Scope[
	enc = NetEncoder[encoderSpec];
	If[FailureQ[enc], ThrowRawFailure[enc]];
	ParseInputSpec[param, type, enc]
];

ParseInputSpec[param_, type_, encoder_NetEncoder] := UseMacros @ 
	EncodedType[encoder, UnifyTypes[type, EncoderOutputType[encoder]] ! 
		ThrowFailure["invenc", EncoderName @ encoder, TypeForm @ EncoderOutputType @ encoder, 
			param, TypeForm @ type]];

General::invlistinlen = "Specification for input port `` should be a list of tensor shapes of length ``."
ParseInputSpec[param_, type:ListT[n_, t_], sizes_List] := Scope[
	If[IntegerQ[n] && Length[sizes] =!= n, ThrowFailure["invlistinlen", n]];
	result = CatchFailure[General, parseSpec[t, #]& /@ sizes];
	If[FailureQ[result], ThrowFailure["netinvportshape", param, TypeForm @ type]];
	result
];

ParseInputSpec[param_, type_, value_] := 
	Block[{$name = param}, 
		parseSpec[type, value]
	];

PackageScope["ParseOutputSpec"]

General::invdec = "Decoder of type ``, which expects ``, is not appropriate for port ``, which produces ``."
ParseOutputSpec[param_, type_, $Raw[t_]] := t;

ParseOutputSpec[param_, type_, decoder_NetDecoder] := UseMacros @ 
	DecodedType[decoder, UnifyTypes[type, DecoderInputType[decoder]] ! 
		ThrowFailure["invdec", DecoderName @ decoder, TypeForm @ DecoderInputType @ decoder, 
			param, TypeForm @ type]];

ParseOutputSpec[param_, type_, decoderSpec:(_String | {_String, ___})] := Scope[
	dec = NetDecoder[decoderSpec];
	If[FailureQ[dec], ThrowRawFailure[dec]];
	ParseOutputSpec[param, type, dec]
];

ParseOutputSpec[param_, type_, value_] := 
	Block[{$name = param},
		parseSpec[type, value]
	]

(* Also used by NetGraph *)
PackageScope["$parseinputhead"]

parseSpec[type_, t_ ? TensorTypeQ] := Scope[
	res = UnifyTypes[type, t];
	res /; !FailureQ[res]
];

parseSpec[_Missing, _] :=
	ThrowFailure["netinvkey", $parseinputhead, $name];

parseSpec[type_, Integer] := Scope[
	res = UnifyTypes[type, IntegerT];
	res /; !FailureQ[res]
];
	
parseSpec[type_, size_Integer] := 
	parseSpec[type, {size}];

parseSpec[type_, sizes:{___Integer} /; VectorQ[sizes, Internal`PositiveMachineIntegerQ]] := Scope[
	res = UnifyTypes[type, TensorT[sizes]];
	res /; !FailureQ[res]
];

General::netinvportshape = "Specification `` should describe ``."
parseSpec[type_, spec_] :=
	ThrowFailure["netinvportshape", $name -> spec, TypeForm[type]];


PackageExport["NetLayerQ"]

NetLayerQ[head_Symbol[arg_Association] ? System`Private`NoEntryQ] := True;
NetLayerQ[_] := False;
