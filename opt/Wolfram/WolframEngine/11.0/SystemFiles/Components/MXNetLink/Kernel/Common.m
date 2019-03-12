Package["MXNetLink`"]

PackageImport["GeneralUtilities`"]
PackageImport["Developer`"]

(****** Symbol Exports ******)

PackageExport["$MXNetLinkLib"]
PackageExport["LoadLibraries"]

PackageExport["MXFailureObject"]
PackageExport["MXGetLastError"]
PackageExport["LibraryFunctionFailureQ"]
PackageExport["MXFailureDescription"]
PackageExport["MXErrorCheck"]
PackageExport["MXOneHotEncoder"]

(******************************************************************************)
(****** Enums ******)

PackageScope["MXSetUsage"]

SetHoldFirst[MXSetUsage];

MXSetUsage[str_] := Scope[
	str = StringTrim[str];
	sym = First @ StringCases[str, WordCharacter..];
	ToExpression[sym, InputForm, Function[s, MessageName[s, "usage"] = str, {HoldAll}]]
];

MXSetUsage[sym_, str_] :=
	MessageName[sym, "usage"] = str;

PackageExport["$DeviceCode"]
PackageExport["$GradientUpdateCode"]
PackageExport["$DatatypeCode"]
PackageExport["$NoGradientCode"]

$DeviceCode = <|"CPU" -> 1, "GPU" -> 2|>;
$GradientUpdateCode = <|None -> 0, "Write" -> 1, "Add" -> 3|>;
$NoGradientCode = 0;

$DatatypeCode = <|
	"Real32" -> 0, 
	"Real64" -> 1, 
	"Real16" -> 2, 
	"UnsignedInteger8" -> 3,
	"Integer32" -> 4
|>;

PackageExport["$DeviceCodeReverse"]
PackageExport["$GradientUpdateCodeReverse"]
PackageExport["$DatatypeCodeReverse"]

$DeviceCodeReverse = AssociationMap[Reverse, $DeviceCode]
$GradientUpdateCodeReverse = AssociationMap[Reverse, $GradientUpdateCode]
$DatatypeCodeReverse = AssociationMap[Reverse, $DatatypeCode]

(****** Enums Callers ******)
PackageScope["toDeviceCode"]

General::invrtt = "Invalid RuntimeTarget ``, should be one of ``."

toDeviceCode[name_] :=
	Lookup[
		$DeviceCode, name, 
		ThrowFailure["invrtt", name, Keys @ $DeviceCode]
	];

General::aotype := "Type `` is not one of the supported array types ``";

PackageScope["toDatatypeCode"]

toDatatypeCode[name_] := 
	Lookup[
		$DatatypeCode, name, 
		ThrowFailure["aotype", name, Keys @ $DatatypeCode]
];


(******************************************************************************)
(****** Library Loader ******)

PackageExport["$MXNetLinkLib"]

$MXNetLinkLib::loadfail = "Failed to load MXNetLink library at ``.";

LibraryLoadChecked[$Failed] = $Failed;
LibraryLoadChecked[e_] := Quiet[Check[LibraryLoad[e], $Failed]];

Windows10Q[] := Windows10Q[] = Module[{res,version},
	If[$OperatingSystem =!= "Windows",
		Return[False]
	];
	res = RunProcess[{"cmd","/c","ver"}];
	If[res["ExitCode"] =!= 0,
		Return[$Failed]
	];
	If[!StringMatchQ[res["StandardOutput"], ___ ~~ "Version" ~~ ___],
		Return[$Failed]
	];
	version = First[StringCases[res["StandardOutput"], DigitCharacter..]];
	ToExpression[version] >= 10
];

$LibraryResources = FileNameJoin[{ParentDirectory[DirectoryName @ $InputFileName], "LibraryResources", $SystemID}];

If[$OperatingSystem === "Windows" && !MemberQ[$LibraryResources, Environment["PATH"]],
	SetEnvironment["PATH" -> $LibraryResources <> ";" <> Environment["PATH"]];
	If[Windows10Q[],
		SetEnvironment["PATH" -> FileNameJoin[{$LibraryResources, "win10"}] <> ";" <> Environment["PATH"]];
	]
]; 

PackageExport["$LibrariesLoaded"]

$LibrariesLoaded = False;

LoadLibraries[] := (
	If[!MemberQ[$LibraryPath, $LibraryResources], AppendTo[$LibraryPath, $LibraryResources]];
	If[!FileExistsQ[$LibraryResources], Message[$MXNetLinkLib::locate, $LibraryResources]; Return[$Failed]];
	If[!StringQ[$MXNetLinkLib], $MXNetLinkLib = FindLibrary["MXNetLink"]];
	If[FailureQ @ LibraryLoadChecked[$MXNetLinkLib], Message[$MXNetLinkLib::loadfail, $MXNetLinkLib]; Return[$Failed]];
	$LibrariesLoaded = True;
);

CheckedLibraryFunctionLoad[name_, args___] :=
	Replace[
		LibraryFunctionLoad[$MXNetLinkLib, name, args],
		Except[_LibraryFunction] :> Panic["MXNetLibraryFunctionLoadError", "Couldn't load `` from MXNetLink.", name]
	];


PackageScope["DeclareLibraryFunction"]

SetAttributes[DeclareLibraryFunction, HoldFirst];

DeclareLibraryFunction[symbol_Symbol, name_String, args___] := 
	SetDelayed[symbol, Set[symbol, CheckedLibraryFunctionLoad[name, args]]];

DeclareLibraryFunction[___] := Panic["MalformedDeclareLibraryFunction"];


(****** Initializer ******)

PackageScope["mxInit"]

DeclareLibraryFunction[mxInit, "WL_MXInit", 
	{}
	, 
	"Void"				
];

(******************************************************************************)
(****** Atomic symbol info generator ******)
PackageScope["deviceInfoToString"]

Clear[deviceInfoToString];
deviceInfoToString[json_] := deviceInfoToString[json] = Block[
	{name, dev, devid},
	{dev, devid} = Values @ Developer`ReadRawJSONString[json];
	name = FirstPosition[$DeviceCode, dev, {Key["Unknown"]}][[1,1]];
	If[devid > 0, name = name <> " " <> IntegerString[devid+1]];
	name
];

(******************************************************************************)
(****** Error Handling functionality ******)

DeclareLibraryFunction[MXGetLastError, "WL_MXGetLastError", 
	{
	}, 
	"UTF8String"						
];	

(******************************************************************************)

LibraryFunctionFailureQ[call_] :=
	If[Head@call === LibraryFunctionError, True, False];

(******************************************************************************)

MXFailureObject[parentFunc_] := Module[
	{lastError}
	,
	(* Get the last MXNEt error *)
	lastError = MXGetLastError[];
	If[LibraryFunctionFailureQ@lastError, lastError = "Unknown Error"];
	
	(* Return Failure Object *)
	Failure[parentFunc, 
		<|"MessageTemplate" -> lastError, "MessageParameters"-> <||>|>
	]
];

(******************************************************************************)

MXFailureDescription[] := Scope[
	lastError = MXGetLastError[];
	If[LibraryFunctionFailureQ@lastError, 
		Return@"Unknown Error",
		Return@lastErrors
	]
];
	
(******************************************************************************)

MXErrorCheck[parentSymbol_, result_] := If[
	LibraryFunctionFailureQ@result, 
		parentSymbol::mxnetError = MXFailureDescription[];
		Panic[parentSymbol::mxnetError]
];

PackageScope["MXNetInvoke"]

General::mxneterr = "MXNet encountered an error: ``";
General::gpumem = "GPU memory exhausted."

MXNetInvoke[func_, args___] :=
    Replace[
        func[args], 
        {
            _LibraryFunctionError :> (
                lastError = MXGetLastError[];
                If[StringContainsQ[lastError, "CUDA" ~~ ___ ~~ "out of memory"],
                	ThrowFailure["gpumem"],
                	Panic["MXNetError", lastError]
                ];
            ),
            _LibraryFunction[___] :> (
                Panic["LibraryFunctionUnevaluated", "Library function `` with args `` did not evaluate.", func[[2]], {args}]
            )
        }
    ];


(******************************************************************************)
(* NOTE: this is way too slow. Bypassing this for now *)
PackageScope["NumericArrayQ"]
PackageScope["MNumberQ"]

MNumberQ[e_] := Developer`MachineIntegerQ[e] || Developer`MachineRealQ[e];

NumericArrayQ[arr_List] := Developer`PackedArrayQ[arr] || ArrayQ[arr, _, MNumberQ];
NumericArrayQ[_] := False;

(******************************************************************************)

MXSetUsage[MXOneHotEncoder,
"MXOneHotEncoder[labelMap$][labels$] returns an array of dimensions {Length@labels$, label dim} given \
an association labelMap$ of the form <|\"label1\" -> int1, $$|> and a list of labels$.
MXOneHotEncoder[dim$][labels$] returns an array of dimensions {Length@labels$, dim$} given \
the dimension dim$ of the embedding and a list of integers labels$."
]

MXOneHotEncoder[labelMap_Association][labels_List] := Normal@SparseArray[labelMap@# -> 1, Length@labelMap]& /@ labels
MXOneHotEncoder[dim_Integer][labels_List] := Normal@SparseArray[# -> 1, dim]& /@ labels

(******************************************************************************)
(* Association to MXNet string key-value parameter format converter *)

PackageScope["mxStringParameterFormat"]


mxStringParameterFormat[params_List] := Scope[
	values =  (#/. x_List :> listToTupleString@x)& /@ params;
	values = Developer`WriteRawJSONString[ToString /@ values, "Compact"-> True];	
	values
];


mxStringParameterFormat[params_Association] := Scope[
	vals = mxStringParameterFormat[Values@params];
	keys = Developer`WriteRawJSONString[Keys@params, "Compact"-> True];	
	<|"Keys" -> keys, "Values" -> vals|>
];

listToTupleString[list_List] := Module[
	{string}
	,
	string = Developer`WriteRawJSONString[list,"Compact"-> True];
	string = StringTake[string, {2, -2}];
	"(" <> string <> ")"
]

(******************************************************************************)
(* A checker whether something is a valid NDArray *)
PackageExport["NDArrayQ"]
NDArrayQ[NDArray[_Integer]?System`Private`NoEntryQ] := True
NDArrayQ[_] = False

PackageExport["NDArrayOrRawArrayQ"]
NDArrayOrRawArrayQ[e_] := NDArrayQ[e] || RawArrayQ[e];

(******************************************************************************)
(* A checker whether something is a valid NDArray *)
PackageExport["MXSymbolQ"]
MXSymbolQ[MXSymbol[_Integer]?System`Private`NoEntryQ] := True
MXSymbolQ[_] = False

(******************************************************************************)
PackageScope["MLEID"]
MLEID := ManagedLibraryExpressionID

(******************************************************************************)

PackageExport["MXSeedRandom"]
DeclareLibraryFunction[MXSeedRandom, "WL_MXRandomSeed", 
	{
		Integer
	}, 
	"Void"						
];	

(******************************************************************************)

PackageExport["MXNotifyShutdown"]
DeclareLibraryFunction[MXNotifyShutdown, "WL_MXNotifyShutdown", 
	{
	}, 
	"Void"						
];	

(******************************************************************************)

PackageExport["GetManagedLibraryKeys"]

DeclareLibraryFunction[getManagedLibraryKeys, "WL_GetManagedLibraryKeys", 
	{
		Integer
	}, 
	{Integer, 1}						
];	

GetManagedLibraryKeys[name_String] := CatchFailure@Scope[
	nameInt = Switch[name,
		"MXExecutor", 0,
		"NDArray", 1,
		"MXSymbol", 2,
		"MXOptimizer", 3,
		_, ThrowFailure["InvalidName"]
	];
	MXNetInvoke[getManagedLibraryKeys, nameInt]
]


PackageScope["FastStringTemplate"]

FastStringTemplate[str_String] := Scope[
	template = StringReplace[str, "`" ~~ w:WordCharacter.. ~~ "`" :> slot[w]];
	StringJoin @@@ ReplaceAll[Compose[Function, template], slot[w_] :> TextString[Slot[w]]]
];


