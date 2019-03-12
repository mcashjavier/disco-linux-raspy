Package["TextSearch`"]

PackageImport["PacletManager`"]

PackageScope["LoadLibraryLinkAPI"]

$typeSugarRules = {String -> "UTF8String", Boole -> (True|False), None -> "Void"};

SetAttributes[LoadLibraryLinkAPI, HoldRest];
LoadLibraryLinkAPI[{path_String, prefix_String}, rule___Rule] := 
	Block[{$path = path, $prefix = prefix},
		Apply[List] @ Map[declareLibFunc] @ ReplaceAll[Hold[rule], $typeSugarRules];
	];

SetAttributes[declareLibFunc, HoldFirst];
declareLibFunc[sym_Symbol[argTypes___] -> retType_] := (
	Clear[sym];
	Set[sym, checkLibError @ LibraryFunctionLoad[
		$path, 
		$prefix <> StringTrim[SymbolName[sym], "$lib"],
		Replace[{argTypes}, {LinkObject} -> LinkObject], retType
	]];
);

SearchIndexObject::liberr = "Failed to load the internal search library. Please contact technical support.";
SetAttributes[checkLibError, HoldFirst];
checkLibError[expr_] := Block[
	{res = Check[expr, $Failed]},
	If[FailureQ[res], 
		Message[SearchIndexObject::liberr];
		Throw[$Failed];
	,
		res
	]
];


PackageScope["CheckedLibraryLoad"]

CheckedLibraryLoad[path_] := checkLibError[LibraryLoad[path]];

