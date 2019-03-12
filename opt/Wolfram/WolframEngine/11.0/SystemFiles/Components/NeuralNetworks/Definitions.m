Package["NeuralNetworks`"]

PackageImport["GeneralUtilities`"]
PackageImport["Developer`"]


PackageScope["ReadDefinitionFile"]

$ConfigContextPath = {
	"NeuralNetworks`", 
	"NeuralNetworks`PackageScope`", 
	"GeneralUtilities`", 
	"Developer`", 
	"DummyContext`",
	"System`"
};

General::defsyntax = "Syntax error in definition file ``.";
General::defnexists = "Definition file `` does not exist.";
General::defevalmsg = "Messages occured while loading definition file ``.";

ReadDefinitionFile[file_String, baseContext_String] := CatchFailure @ Scope[
	If[!FileExistsQ[file], ThrowFailure["defnexists", file]];
	name = FileBaseName[file];
	string = FileString[file];
	keystack = {};
	string = StringReplace[string, {
		StartOfLine ~~ tabs:"\t"... ~~ Repeated["$", {0,1}] ~~ word:(Repeated["$",{0,1}] ~~ LetterCharacter..) ~~ ":" :> (
			keystack = Append[Take[keystack, StringLength[tabs]], word];
			ToString[K @@ keystack, InputForm] <> ";"
		)
	}];
	$data = <||>;
	Block[{
		$Context = baseContext <> name <> "`",
		$ContextPath = $ConfigContextPath},
		DummyContext`$Input; DummyContext`$Output;
		Check[
			statements = ToExpression[string, InputForm, Hold],
			ThrowFailure["defsyntax", file]
		];
		Scan[proc, statements];
		$data = $data /. {
			f_Function :> RuleCondition @ ScopePureFunction[f],
			DummyContext`$Input -> NetPort["Inputs", "Input"],
			DummyContext`$Output -> NetPort["Outputs", "Output"]
		};
		rewrites = FlatMap[
			Function[field, Map[toPortRule[field, #]&, Keys @ Lookup[$data, field, {}]]],
			{"Inputs", "Outputs", "Arrays", "Parameters"}
		];
	];
	$data = $data /. rewrites;
	Clear @@ All1[rewrites];
	$data
];

toPortRule[field_, symname_] := ToExpression["$" <> symname] -> NetPort[field, symname];


SetHoldAll[proc];

proc[ce_CompoundExpression] := ce;
proc[K[args___]; Null] := $data[args] = <||>;
proc[K[args___]; value_] := $data[args] = value;
proc[Null] := Null;
proc[expr_] := Panic["UnexpectedExpression", "The expression `` was not expected.", HoldForm[expr]];


PackageScope["DesugarTypeDefinitions"]

SetAttributes[DesugarTypeDefinitions, HoldFirst];
DesugarTypeDefinitions[assoc_, fields_] := Block[{irules},
	$irules = assoc["InferenceRules"];
	assoc = assoc /. c:ComputedType[_, _] :> Append[c, findDeep[c, _NetPort]];
	Do[
		assoc[field] = desugarDefs[NetPort[field], assoc[field]],
		{field, fields}
	];
	assoc = DoInference[assoc, $irules, {}, False (* preserve Defaulting *)];
	assoc["InferenceRules"] = $irules;
];

desugarDefs[path_, assoc_Association] := 
	IMap[
		desugarDefs[Append[path, #1], #2]&,
		assoc
	]

desugarDefs[path_, type_ /; FreeQ[type, NetPort]] := type;

desugarDefs[path_, type_] := (
	AppendTo[$irules, path -> type]; 
	TypeT
);


PackageExport["LoadLayerDefinitions"]

$LayerDefinitionsPath = FileNameJoin[{DirectoryName[$InputFileName], "Layers"}];

LoadLayerDefinitions[] := CatchFailure @ Scan[
	loadDefinition[#, "Layers`", DefineLayer]&, 
	FileNames["*.m", $LayerDefinitionsPath]
];

PackageExport["LoadEncoderDefinitions"]

$EncoderDefinitionsPath = FileNameJoin[{DirectoryName[$InputFileName], "Encoders"}];

LoadEncoderDefinitions[] := CatchFailure @ 
	Scan[
		loadDefinition[#, "Encoders`", DefineEncoder]&, 
		FileNames["*.m", $EncoderDefinitionsPath]
	];


PackageExport["LoadDecoderDefinitions"]

$DecoderDefinitionsPath = FileNameJoin[{DirectoryName[$InputFileName], "Decoders"}];

LoadDecoderDefinitions[] := CatchFailure @ 
	Scan[
		loadDefinition[#, "Decoders`", DefineDecoder]&, 
		FileNames["*.m", $DecoderDefinitionsPath]
	];

General::invnetdeffile = "Definition in file `` is invalid.\nThe failed definition is available as $FailedDefinition. The error was:\n``"

PackageExport["$FailedDefinition"]

$FailedDefinition = None;

loadDefinition[file_, context_, func_] := Scope[
	name = FileBaseName[file];
	Debug[Print[file]];
	res = ReadDefinitionFile[file, "NeuralNetworks`" <> context];
	result = func[name, res];
	If[FailureQ[result], 
		$FailedDefinition ^= res;
		ThrowFailure["invnetdeffile", file, TextString @ result]];
	result
];
