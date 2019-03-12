Package["MXNetLink`"]

PackageImport["GeneralUtilities`"]


(****** Symbol Exports ******)

PackageExport["MXSymbol"]

PackageExport["$MXAtomicSymbolInformation"]

PackageExport["MXSymbolFromJSON"]
PackageExport["MXSymbolToJSON"]
PackageExport["MXSymbolEnumerate"]
PackageExport["MXSymbolGradient"]
PackageExport["MXSymbolArguments"]
PackageExport["MXSymbolOutputs"]
PackageExport["MXSymbolAuxilliaryStates"]
PackageExport["MXSymbolInferShape"]
PackageExport["MXSymbolCopy"]
PackageExport["MXSymbolCompose"]
PackageExport["MXSymbolVariable"]
PackageExport["MXSymbolGroup"]


(******************************************************************************)
(* Global Variables *)


(******************************************************************************)
(****** Load Library Functions ******)

DeclareLibraryFunction[mxSymbolCreateFromJSON, "WL_MXSymbolCreateFromJSON", 
	{
		Integer, 		(* symbol handle key *)				
		"UTF8String" 	(* json symbol representation *)
	}, 
	"Void"						
	]	

DeclareLibraryFunction[mxSymbolSaveToJSON, "WL_MXSymbolSaveToJSON", 
	{
		Integer 		(* symbol handle key *)				
	}, 
	"UTF8String"		(* OUtput JSON *)				
	]	

PackageScope["mxSymbolCreateAtomicSymbol"]

DeclareLibraryFunction[mxSymbolGetAtomicSymbolInfo, "WL_MXSymbolGetAtomicSymbolInfo", 
	{			
	}, 
	"UTF8String"		(* OUtput JSON *)				
	]		

DeclareLibraryFunction[mxSymbolStructureInfo, "WL_MXSymbolStructureInfo", 
	{		
		Integer, 		(* symbol handle key *)	
		Integer         (* 0: args, 1: outputs, 2: aux states *)
	}, 
	"UTF8String"		(* OUtput JSON *)				
	]	

DeclareLibraryFunction[mxSymbolGrad, "WL_MXSymbolGrad", 
	{		
		Integer, 		(* symbol handle key *)	
		Integer, 		(* gradient symbol key *)
		"UTF8String"    (* parameters to differentiate wrt *)
	}, 
	"Void"			
	]		

DeclareLibraryFunction[mxSymbolInferShape, "WL_MXSymbolInferShape", 
	{		
		Integer, 					(* symbol handle key *)	
		Integer, 					(* number args *)
		{Integer, 1},   (* arg_shape_data *)
		{Integer, 1}    (* arg_ind_ptr *)
	}, 
	"UTF8String"			
	]	

DeclareLibraryFunction[mxSymbolCreateVariable, "WL_MXSymbolCreateVariable", 
	{		
		Integer, 				(* output symbol handle key *)	
		"UTF8String"   			(* variable name *)
	}, 
	"Void"			
	]

DeclareLibraryFunction[mxSymbolCreateAtomicSymbol, "WL_MXSymbolCreateAtomicSymbol", 
	{		
		Integer, 					(* output symbol handle key *)	
		Integer, 					(* symbol creator handle key *)
		"UTF8String",   			(* arg keys json *)
		"UTF8String"    			(* arg param values json *)
	}, 
	"Void"			
	]

DeclareLibraryFunction[mxSymbolCompose, "WL_MXSymbolCompose", 
	{		
		Integer, 					(* symbol handle key *)	
		"UTF8String",   			(* name *)
		{Integer, 1},				(* input symbol handle keys *)
		"UTF8String"    			(* json of input symbol keys *)
	}, 
	"Void"			
	]

DeclareLibraryFunction[mxSymbolCopy, "WL_MXSymbolCopy", 
	{		
		Integer, 					(* input symbol handle key *)	
		Integer						(* output symbol handle key *)
	}, 
	"Void"			
	]

DeclareLibraryFunction[mxSymbolCreateGroup, "WL_MXSymbolCreateGroup", 
	{		
		{Integer, 1}, 					(* input symbol handle keys *)	
		Integer						(* output symbol handle key *)
	}, 
	"Void"			
	]	

DeclareLibraryFunction[mxSymbolGetInternals, "WL_MXSymbolGetInternals", 
	{		
		Integer, 					(* input symbol handle keys *)	
		Integer						(* output symbol handle key *)
	}, 
	"Void"			
	]	

DeclareLibraryFunction[mxSymbolSetAttr, "WL_MXSymbolSetAttr", 
	{		
		Integer, 					(* symbol handle key *)	
		"UTF8String",   			(* key *)
		"UTF8String"				(* value *)
	}, 
	"Void"			
	]
	
DeclareLibraryFunction[mxSymbolGetAttr, "WL_MXSymbolGetAttr", 
	{		
		Integer, 					(* symbol handle key *)	
		"UTF8String"	   			(* key *)
	}, 
	"UTF8String"			
	]

(******************************************************************************)

MXSetUsage @ "
MXSymbolGroup[{MXSymbol[$$], $$}] creates a new symbol by grouping together the input symbols.
Used to create multi-output networks."

MXSymbolGroup[symbols_:{__}] := CatchFailure @ Scope[
	outputSymbol = CreateManagedLibraryExpression["MXSymbol", MXSymbol];
	System`Private`SetNoEntry@outputSymbol;
	result = MXNetInvoke[mxSymbolCreateGroup, MLEID /@ symbols, MLEID@outputSymbol];
	outputSymbol
] 

(******************************************************************************)

MXSetUsage @ 
"MXSymbolVariable[name$] createss a variable symbol with the given name."

MXSymbolVariable[name_String] := CatchFailure @ iMXSymbolVariable[name]

iMXSymbolVariable[name_String] := Scope[
	mxInit[];
	symbolHandle = CreateManagedLibraryExpression["MXSymbol", MXSymbol];
	System`Private`SetNoEntry@symbolHandle;
	result = MXNetInvoke[mxSymbolCreateVariable, MLEID@symbolHandle, name];
	symbolHandle
] 

(******************************************************************************)

MXSetUsage @ "
MXSymbolFromJSON[File['path$']] creates a symbol from a JSON file containing a definition.
MXSymbolFromJSON[assoc$] loads a file from an association containing a definition.
MXSymbolFromJSON['str$'] loads a file from a string containing a JSON-encoded definition.
"

MXSymbolFromJSON[File[path_]] := Scope[
	If[!FileQ[path], ReturnFailed[]];
	MXSymbolFromJSON @ FileString[path]
];

MXSymbolFromJSON[def_Assocation] := Scope[
	Lookup[def, {"nodes", "arg_nodes", "heads"}, ReturnFailed[]];
	json = Developer`WriteRawJSONString[def];
	MXSymbolFromJSON[json]
];	

MXSymbolFromJSON[def_String] := Scope[
	mxInit[];
	handle = CreateManagedLibraryExpression["MXSymbol", MXSymbol];
	System`Private`SetNoEntry@handle;
	MXNetInvoke[mxSymbolCreateFromJSON, MLEID@handle, def];
	handle
]

MXSymbolFromJSON[json_Association] := 
	CatchFailure @ MXSymbolFromJSON[Developer`WriteRawJSONString@json]

(******************************************************************************)

MXSetUsage @ "
MXSymbolToJSON[MXSymbol[$$]] returns a JSON-like expression that describes the given MXSymbol.
"

MXSymbolToJSON[symbol_MXSymbol] := 
	CatchFailure @ Developer`ReadRawJSONString@MXNetInvoke[mxSymbolSaveToJSON, MLEID@symbol]

(******************************************************************************)

MXSetUsage @ "
MXSymbolCopy[MXSymbol[$$]] returns a copy of the MXSymbol.
MXSymbolCopy[inSymbol$, outSymbol$] copies the MXSymbol inSymbol$ to outSymbol$.
"

MXSymbolCopy[symbol_MXSymbol, outSymbol_MXSymbol] := CatchFailure @ Scope[
	result = MXNetInvoke[mxSymbolCopy, MLEID@symbol, MLEID@outSymbol];	
	outputSymbol
]

MXSymbolCopy[symbol_MXSymbol] := CatchFailure @ Scope[
	outputSymbol = CreateManagedLibraryExpression["MXSymbol", MXSymbol];
	System`Private`SetNoEntry@outputSymbol;
	MXSymbolCopy[symbol, outputSymbol]
]

(******************************************************************************)

MXSetUsage @ "
MXSymbolEnumerate[] returns an association mapping MXSymbols to information about that symbol.
"

MXSymbolEnumerate[] := CatchFailure @ Scope[
	result = MXNetInvoke[mxSymbolGetAtomicSymbolInfo];
	Developer`ReadRawJSONString@result
]	

(******************************************************************************)
	
MXSetUsage @ "
MXSymbolOutputs[MXSymbol[$$]] returns a list of output symbol names of an MXSymbol.
"

MXSymbolOutputs[symbol_/; ManagedLibraryExpressionQ@symbol] := CatchFailure @ Scope[
	json = MXNetInvoke[mxSymbolStructureInfo, MLEID@symbol, 1];
	Developer`ReadRawJSONString@json
]

(******************************************************************************)

MXSetUsage @ "
MXSymbolAuxilliaryStates[MXSymbol[$$]] returns the names of the auxilliary state symbols of an MXSymbol.
"

MXSymbolAuxilliaryStates[symbol_/; ManagedLibraryExpressionQ@symbol] := CatchFailure @ Scope[
	json = MXNetInvoke[mxSymbolStructureInfo, MLEID@symbol, 2];
	Developer`ReadRawJSONString@json
]

(******************************************************************************)

MXSetUsage @ "
MXSymbolArguments[MXSymbol[$$]] returns the argument names of an MXSymbol.
"

MXSymbolArguments[symbol_MXSymbol] := CatchFailure @ Scope[
	json = MXNetInvoke[mxSymbolStructureInfo, MLEID@symbol, 0];	
	Developer`ReadRawJSONString@json
]

(******************************************************************************)
PackageExport["MXSymbolAttributes"]
MXSetUsage @ "
MXSymbolAttributes[MXSymbol[$$]] returns the attributes of a MXSymbol.
"

MXSymbolAttributes[symbol_MXSymbol] := CatchFailure @ Scope[
	json = MXNetInvoke[mxSymbolStructureInfo, MLEID@symbol, 3];	
	Developer`ReadRawJSONString@json
]

(******************************************************************************)
PackageExport["MXSymbolSetAttribute"]
MXSetUsage @ "
MXSymbolSetAttribute[MXSymbol[$$], att$, val$] sets the attribute att$ of symbol MXSymbol[$$].
"

MXSymbolSetAttribute[symbol_MXSymbol, attribute_String, value_] := 
	CatchFailure @ MXNetInvoke[mxSymbolSetAttr, MLEID@symbol, attribute, ToString@value];	

(******************************************************************************)
PackageExport["MXSymbolGetAttribute"]
MXSetUsage @ "
MXSymbolGetAttribute[MXSymbol[$$], att$] gets the attribute att$ of symbol MXSymbol[$$].
"

MXSymbolGetAttribute[symbol_MXSymbol, attribute_String] := 
	CatchFailure @ MXNetInvoke[mxSymbolGetAttr, MLEID@symbol, attribute];	

(******************************************************************************)

PackageExport["MXSymbolOperators"]

MXSetUsage @ "
MXSymbolOperators[MXSymbol[$$]] gives the operator names of an MXSymbol.
"

MXSymbolOperators[symbol_MXSymbol] := CatchFailure @ Scope[
	json = MXSymbolToJSON@symbol;
	If[FailureQ@json, Return@json];
	{nodes, argnodes, heads} = Lookup[json, {"nodes", "arg_nodes", "heads"}, Panic[]];
	argnodes += 1; (* change from zero indexed *)
	len = Length@nodes;
	opsIndices = Complement[Range@Length@nodes, argnodes];
	#name& /@ nodes[[opsIndices]]
]

(******************************************************************************)

MXSetUsage @ "
MXSymbolGradient[MXSymbol[$$], params$] returns the autodiff gradient symbol with respect to params$, a list of parameter names. 
NOTE: This function can only be used if symbol is a loss function.
"

MXSymbolGradient[symbol_MXSymbol, params_List] := CatchFailure @ Scope[
	gradHandle = CreateManagedLibraryExpression["MXSymbol", MXSymbol];
	System`Private`SetNoEntry@gradHandle;
	json = Developer`WriteRawJSONString@params;
	MXNetInvoke[mxSymbolGrad, MLEID@symbol, MLEID@gradHandle, json];		
	gradHandle
]

(******************************************************************************)
PackageExport["MXSymbolGetInternals"]

MXSetUsage @ "
MXSymbolGetInternals[MXSymbol[$$]] returns a new symbol whose outputs are the internal states of the MXSymbol.
"

MXSymbolGetInternals[symbol_MXSymbol] := CatchFailure @ Scope[
	internalSymbol = CreateManagedLibraryExpression["MXSymbol", MXSymbol];
	System`Private`SetNoEntry@internalSymbol;
	MXNetInvoke[mxSymbolGetInternals, MLEID@symbol, MLEID@internalSymbol];
	internalSymbol
]

(******************************************************************************)

MXSetUsage @ "
MXSymbolInferShape[MXSymbol[$$], <|var1$ -> dim1$, $$|>] infers dimensions of Variable symbols given known dimensions. 
"

MXSymbolInferShape[symbol_MXSymbol, argShapes_Association] := Scope[
	
	(* Check that num args is correct *)
	If[Length@argShapes === 0,
		ReturnFailed["InsufficientShapes"]
	];
	
	(* Get all relevant symbol details *)
	args = MXSymbolArguments@symbol;
	outputs = MXSymbolOutputs@symbol;
	auxStates = MXSymbolAuxilliaryStates@symbol;

	(* Put into CSR format *)
	argShapeData = {};
	argIndPtr = {0};
	Do[
		arg = args[[i]];
		value = Lookup[argShapes, arg];
		If[Not@MissingQ@value, argShapeData = Join[argShapeData, value]];
		argIndPtr = Join[argIndPtr, {Length@argShapeData}]
		,
		{i, Length@args}
	];
	(* Get JSON *)
	result = MXNetInvoke[
		mxSymbolInferShape, 
		MLEID@symbol, 
		Length@args, 
		Developer`ToPackedArray@argShapeData, 
		Developer`ToPackedArray@argIndPtr
	];
	out = Developer`ReadRawJSONString@result;
	
	(* Deal with Complete *)
	If[out["Complete"] =!= 1, ReturnFailed[]];

	(* create associations *)
	out["ArgumentArrays"] = AssociationThread[args, out["Arguments"]];
	out["AuxilliaryArrays"] = AssociationThread[auxStates, out["AuxilliaryStates"]];
	out["OutputArrays"] = AssociationThread[outputs, out["Outputs"]];
	(* return *)
	out 
]

MXSymbolInferShape::InsufficientShapes = "Cannot infer shape with no dimension information."

(******************************************************************************)

MXSetUsage @ "MXSymbolCompose[MXSymbol[$$], name$, args$] returns a new symbol with name name$ that \
represents the composition of the given symbol with args$, which should be an association mapping \
argument names to existing MXSymbols.
"

MXSymbolCompose[inputSym_MXSymbol, name_String, arguments___] := CatchFailure @ Scope[
	
	args = Select[arguments, Not@RuleQ@#&];
	kwargs = Association@Select[arguments, RuleQ];
	
	(* error check *)
	If[(Length@args > 0) && (Length@kwargs > 0), 
		ThrowFailure["Can only accept input Symbols either as positional or keyword arguments, not both"]
	];
	
	numArgs = Length@args + Length@kwargs;
	
	If[Length@kwargs > 0, 
		symbols = MLEID /@ Values@kwargs;
		names = Developer`WriteRawJSONString@Keys@kwargs
		,
		symbols = MLEID /@ args;
		names = Developer`WriteRawJSONString@{};
	];

	outSymbol = MXSymbolCopy[inputSym];
	If[FailureQ@outSymbol, Return@outSymbol];
	MXNetInvoke[mxSymbolCompose, MLEID@outSymbol, name, symbols, names];
	outSymbol
]

(******************************************************************************)
(*** $MXAtomicSymbolInformation creator 
		- Parse MXSymbolEnumerate[] into WL readable form 
		-Notes:
			- we need to remove underscores from arg names (like _MinusScalar) as
				can't create symbol from that
***)

$MXAtomicSymbolInformation := $MXAtomicSymbolInformation = Module[
	{func, output}
	,
	func = <|
		"Description" -> #["Description"], 
		"VariableArgumentKey" -> #["VariableArgumentKey"],
		"Arguments" -> parseSymbolArgInfo@#,
		"AtomicSymbolCreatorsHandle" -> #["AtomicSymbolCreatorsHandle"]
	|>&;
	output = Map[func, MXSymbolEnumerate[]];
	(* Replace all symbols starting with _ with $ *)
	output = KeyMap[StringReplace[#, "_" .. -> "$"]&, output];	
	output
]

(*** Extracts info from both "ArgumentNames" "ArgumentTypeInfo" keys.
	Create argument info association of form:
	<|"argname" -> <|"Description" -> ... |>|> ***)

parseSymbolArgInfo[symbolInfo_Association] := Scope[
	output = Association@MapIndexed[
		#1 -> <|
			"Description" -> symbolInfo["ArgumentDescriptions"][[First@#2]],
			argTypeInfoParse@symbolInfo["ArgumentTypeInfo"][[First@#2]]
			|>&
		, 
		symbolInfo["ArgumentNames"]
	];
	(* NOTE: this is to deal with MXNet bug https://github.com/dmlc/mxnet/issues/1526 *)
	KeyMap[ReplaceAll[#, {"array" -> "lhs", "src" -> "data"}]&, output]
]

(*** Parse arg type + optional + defaults from ArgTypeString ***)
argTypeInfoParse[typeString_String] := Module[
	{return}
	,
	return = <||>;
	(* Check whether arg is optional *)
	return["Optional"] = StringContainsQ[typeString, "optional"];
	return["Type"] = StringSplit[typeString, {", required", ", optional", ",optional", ",required"}];
	(* Deal with certain ops (like _Native) that have no type *)
	return["Type"] = If[return["Type"] == {}, "Missing[]", First@return["Type"]];
	(* determine default value *)
	If[return["Optional"], return["Default"] = parseMXDefaults@typeString];
	(* If type is Symbol, always optional *)
	If[return["Type"] === "Symbol", 
		return["Optional"] = True;
		return["Default"] = Automatic
	];
	(* return *)
	return
]

(*** Extract default argument ***)
parseMXDefaults::unableToParse = "Cannot parse MXNet defaults. Fix the parser!"

parseMXDefaults[defaultString_String] := Module[
	{s, exp}
	,
	(* Get only default part *)
	s = First@StringCases[defaultString, ___~~"default="~~x___ -> x];
	s = StringDelete[s, "'"];
	s = StringReplace[s, {"("-> "{", ")"-> "}"}];
	exp = s;
	(* Incase MXNet changes format, immediately fail *)
	If[FailureQ@exp, Panic[parseMXDefaults::unableToParse]];
	exp
]

(******************************************************************************)
(* Get canonical ordering of symbol args *)
PackageExport["$SymbolArgumentOrdering"]

$SymbolArgumentOrdering := $SymbolArgumentOrdering = Scope[
	argTypes = Thread[#ArgumentNames -> #ArgumentTypeInfo] & /@ MXSymbolEnumerate[];
	canonicalSymbolNames = Select[#, Function[x, StringContainsQ[Last@x, "Symbol"]]] & /@ argTypes;
	canonicalSymbolNames = #[[;; , 1]] & /@ canonicalSymbolNames;
	canonicalSymbolNames = Select[canonicalSymbolNames, (Length@# =!= 0) &]
]

(******************************************************************************)

MXSymbolUsageGenerate[symbolName_String] := Module[
	{
		description, argInfo, optArgs, requiredArgs,
		reqArgDesc, optArgDesc
	},
	argInfo = $MXAtomicSymbolInformation[symbolName, "Arguments"];
	description = $MXAtomicSymbolInformation[symbolName, "Description"];
	optArgs = KeySelect[argInfo, argInfo[#, "Optional"]&];
	requiredArgs = KeyDrop[argInfo, Keys@optArgs];

	(* Argument Templates *)
	reqArgDesc = KeyValueMap[$argSymbolTemplate[<|"Name" -> #1, #2|>]&, requiredArgs];
	optArgDesc = KeyValueMap[$argSymbolTemplate[<|"Name" -> #1, #2|>]&, optArgs];
	AppendTo[optArgDesc, "\t Name (Automatic): name of output symbol"];

	(* Main Template *)
	$mainSymbolTemplate[<|
		"functionName" -> symbolName,
		"Description" -> description, 
		"args" -> StringJoin@reqArgDesc,
		"optArgs" -> StringJoin@optArgDesc,
		"reqArgs" -> StringTake[ToString@Keys@requiredArgs, {2, -2}] (* remove {} *)
	|>]

]

$mainSymbolTemplate = FastStringTemplate[
"`functionName`[`reqArgs`, opts_]:
  Description: `Description`
  Arguments:
`args`
  Optional Arguments:
`optArgs`
"];

$argSymbolTemplate = FastStringTemplate["\t `Name` (`Type`): `Description` \n"];

(* Generate canonical ordering for named symbolic inputs *)
PackageExport["$MXSymbolicInputCanonicalOrdering"]
$MXSymbolicInputCanonicalOrdering := $MXSymbolicInputCanonicalOrdering = Scope[
	func = Function[assoc, Keys@Select[assoc["Arguments"], 
     	StringMatchQ[#Type, "Symbol" ~~ ___] &]
     ];
	func[#] & /@ $MXAtomicSymbolInformation[]	
]

(******************************************************************************)
(* Name Manager *)


symbolNameCounterManager[] := If[AssociationQ@$SymbolNameCounter,
	$SymbolNameCounter,
	$SymbolNameCounter = <||>;
	$SymbolNameCounter
];

nameManager[name_, hint_] := Scope[
	(* If name supplied, return name. MXNet python doesn't append this name to $SymbolNameStore *)
	If[name =!= Automatic, Return@name];
	If[Not@KeyExistsQ[symbolNameCounterManager[], hint],
		$SymbolNameCounter[hint] = 0
	];
	outputName = hint <>  ToString@$SymbolNameCounter[hint];
	$SymbolNameCounter[hint] += 1;
	outputName
]

(******************************************************************************)
(* Generate all symbol functions *)

makeSymbol[symbolType_String] := Block[
	{argInfo, handleKey}
	,
    argInfo = $MXAtomicSymbolInformation[symbolType, "Arguments"];
	handleKey = argInfo["AtomicSymbolCreatorsHandle"];
	
    (* Define symbol options *)
    mxsymbol = Symbol["MX`" <> symbolType];
	With[{sym = mxsymbol},
		(* Create usage *)
		sym::usage = MXSymbolUsageGenerate[symbolType];
		(* Define symbol creator *)
		sym[x___] := CatchFailure @ Block[
			{},
			mxSymbolCreator[symbolType, List@x]
		]];
];

mxSymbolCreator[symbolType_String, arguments_] := Scope[
	(* Symbol Info Key *)
	symbolInfo = $MXAtomicSymbolInformation[symbolType];
	atomicHandleKey = symbolInfo@"AtomicSymbolCreatorsHandle";
	varArgKey = symbolInfo@"VariableArgumentKey";
	
	(* variables *)
	paramKeys = {};
	paramVals = {};
	symbolKwargs = <||>;
	args = Select[arguments, Not@RuleQ@#&];
	kwargs = Association@Select[arguments, RuleQ];
	
	(* pop off name + attr kwargs *)
	name = Lookup[kwargs, "name"];
	If[MissingQ@name, name = Automatic, DropFrom[kwargs, "name"]];
	attr = Lookup[kwargs, "attr"];
	If[MissingQ@attr, attr = None, DropFrom[kwargs, "attr"]];
	(* deal with varargs *)
	If[(StringLength@varArgKey > 0) && MissingQ@Lookup[kwargs, varArgKey],
		AppendTo[paramKeys, varArgKey];
		AppendTo[paramVals, ToString@Length@args]
	];
	
	(* deal with kwargs *)
	KeyValueMap[
		If[MXSymbolQ@#2, 
			symbolKwargs[#1] = #2, 
			AppendTo[paramKeys, #1];
			AppendTo[paramVals, #2]
		]&,
		kwargs
	];

	(* Stringify params *)
	keysString = mxStringParameterFormat@paramKeys;
	valsString = mxStringParameterFormat@paramVals;

	(* Create output symbol *)
	outputSymbol = CreateManagedLibraryExpression["MXSymbol", MXSymbol];
	System`Private`SetNoEntry@outputSymbol;
	MXNetInvoke[mxSymbolCreateAtomicSymbol, MLEID@outputSymbol, atomicHandleKey, keysString, valsString];
	
	(* error check *)
	If[(Length@args > 0) && (Length@symbolKwargs > 0), 
		ThrowFailure["Can only accept input Symbols either as positional or keyword arguments, not both"]
	];
	
	(* Get layer name *)
	symbolNameHint = ToLowerCase@symbolType;
	name = nameManager[name, symbolNameHint];
	
	(* Compose *)
	MXSymbolCompose[outputSymbol, name, Join[args, Normal@symbolKwargs]]
]


(* Initialize the symbols *)

PackageExport["InitialiazeSymbols"]

InitialiazeSymbols::notloaded = "Cannot initialize symbols if libraries aren't loaded."

InitialiazeSymbols[] := If[
	TrueQ[$LibrariesLoaded],
	Scan[makeSymbol, Keys@$MXAtomicSymbolInformation],
	Message[InitialiazeSymbols::notloaded]; $Failed
];

(******************************************************************************)
(***** Special Symbols: Note, these are also handled separately in Python interface ******)

(******************************************)
(* binary op *)

MXSymbol /: Plus[x_MXSymbol, y_MXSymbol] := MX`$Plus[x, y]
MXSymbol /: Plus[x_MXSymbol, y_] := MX`$PlusScalar[x, "scalar" -> y]
MXSymbol /: Plus[x_, y_MXSymbol] := Plus[x, y]

MXSymbol /: Subtract[x_MXSymbol, y_MXSymbol] := MX`$Minus[x, y]
MXSymbol /: Subtract[x_MXSymbol, y_] := MX`$MinusScalar[x, "scalar" -> y]
MXSymbol /: Subtract[x_, y_MXSymbol] := MX`$RMinusScalar[y, "scalar" -> x]

MXSymbol /: Divide[x_MXSymbol, y_MXSymbol] := MX`$Div[x, y]
MXSymbol /: Divide[x_MXSymbol, y_] := MX`$DivScalar[x, "scalar" -> y]
MXSymbol /: Divide[x_, y_MXSymbol] := MX`$RDivScalar[y, "scalar" -> x]

MXSymbol /: Times[x_MXSymbol, y_MXSymbol] := MX`$Mul[x, y]
MXSymbol /: Times[x_MXSymbol, y_] := MX`$MulScalar[x, "scalar" -> x]
MXSymbol /: Times[x_, y_MXSymbol] := Times[y, x]

MXSymbol /: Power[x_MXSymbol, y_MXSymbol] := MX`$Power[x, y]
MXSymbol /: Power[x_MXSymbol, y_] := MX`$PowerScalar[x, "scalar" -> y]
MXSymbol /: Power[x_, y_MXSymbol] := MX`$RPowerScalar[y, "scalar" -> x]
	
MXSymbol /: Max[x_MXSymbol, y_MXSymbol] := MX`$Maximum[x, y]
MXSymbol /: Max[x_MXSymbol, y_] := MX`$MaximumScalar[x, "scalar" -> y]
MXSymbol /: Max[x_, y_MXSymbol] := Max[y, x]

MXSymbol /: Min[x_MXSymbol, y_MXSymbol] := MX`$Minimum[x, y]
MXSymbol /: Min[x_MXSymbol, y_] := MX`$MinimumScalar[x, "scalar" -> y]
MXSymbol /: Min[x_, y_MXSymbol] := Min[y, x]

(******************************************************************************)

DefineCustomBoxes[MXSymbol,
	sym_MXSymbol ? System`Private`NoEntryQ :> MXSymbolBoxes[sym]
];

MXSymbolBoxes[sym:MXSymbol[id_]] := Scope[
	plot = Dynamic[MXSymbolPlot[sym], TrackedSymbols :> {}];
	outs = MXSymbolOutputs[sym];
	args = MXSymbolArguments[sym];
	BoxForm`ArrangeSummaryBox[
		MXSymbol,
		sym,
		None,
		{makeItem["ID", id], 
		 makeItem["Outputs", Column @ outs]},
		{makeItem["Arguments", If[Length[args] < 16, Column[args], Skeleton[Length[args]]]],
		 makeItem["Plot", plot]},
		StandardForm
	]
];

makeItem[name_, value_] := BoxForm`MakeSummaryItem[{Pane[name <> ": ", {60, Automatic}], value}, StandardForm];


(* This is the subvalue that allows one to quickly and dirtily evaluate
an mxsymbol on a simple numeric input. There needs to be a numeric check here,
as well as an association input form for when there are multiple inputs, along
with a vararg form as well *)

bindWithInputs[sym_, inputs_List] := Scope[
	inames = Take[MXSymbolArguments[sym], Length[inputs]];
	iarrays = AssociationThread[inames, NDArrayCreate[{#}]& /@ inputs];
	MXSymbolBind[sym, iarrays]
];

toMArrays[outputs_Association] := Map[Normal /* First, outputs];

sym_MXSymbol[inputs__] := Scope[
	exec = bindWithInputs[sym, {inputs}];
	MXExecutorForward[exec];
	toMArrays[exec["OutputArrays"]]
];

PackageExport["MXGradSymbol"]

MXSymbol /: Derivative[1][sym_MXSymbol] := MXGradSymbol[sym];

MXGradSymbol[sym_MXSymbol][inputs__] := Scope[
	{exec, arrays} = bindWithInputs[sym, {inputs}];
	MXExecutorForward[exec, True];
	outGrad = NDArrayCopy /@ Values @ arrays["Outputs"];
	Scan[NDArraySet[#, 1.]&, outGrad];
	MXExecutorBackward[exec, outGrad];
	toMArrays[arrays["Gradients"]]
];