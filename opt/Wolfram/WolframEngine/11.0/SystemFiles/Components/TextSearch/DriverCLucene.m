Package["TextSearch`"]

PackageImport["PacletManager`"]

PackageExport["CLuceneHandle"]

DCreateHandle[path_String, "CLucene", version_ /; version > 0.1] := Catch[
	LoadCLuceneLink[];
	CLuceneHandle[$libCreateHandle[AbsoluteFileName[path]]]
];

DDeleteHandle[CLuceneHandle[id_Integer]] :=
	$libDeleteHandle[id]; 

PackageExport["SetCLuceneLogging"]

SetCLuceneLogging[str_String] := $libEnableFileLogging[ExpandFileName @ str];
SetCLuceneLogging[True] := $libEnablePrintLogging[];
SetCLuceneLogging[False] := $libDisableLogging[];

PackageExport["LoadCLuceneLink"]

LoadCLuceneLibs[] := Block[{libDir},
	libDir = PacletResource["TextSearch", "libraryDir"];
	$CLuceneLibraryPath = FileNameJoin[{libDir, "libTextSearch"}];
	If[!$windowsQ,
		(* hernanm built windows as a single library, but other platforms
			as multiple libraries, for some reason *)
		Scan[
			CheckedLibraryLoad[FileNameJoin[{libDir, #}]]&,
			{"libclucene-shared", "libclucene-core"}
		];
	];
	CheckedLibraryLoad[$CLuceneLibraryPath];
];

LoadCLuceneLink[] := (
	LoadCLuceneLibs[];
	LoadLibraryLinkAPI[{$CLuceneLibraryPath, "CLuceneLink_"},
		$libCreateHandle[String] -> Integer,
		$libDeleteHandle[Integer] -> None,
		$libCreateEmptyIndex[Integer] -> None,
		$libAddStringField[Integer, String, String] -> None,
		$libAddTextField[Integer, String, String] -> None,
		$libAddTextFileField[Integer, String, String] -> None,
		$libAddUnixTimeField[Integer, String, Integer] -> None,
		$libAddIntegerField[Integer, String, Integer] -> None,
		$libAddBooleanField[Integer, String, Boole] -> None,
		$libAddPathField[Integer, String, String] -> None,
		$libAddDocument[Integer] -> None,
		$libClearDocument[Integer] -> None,
		$libGetSchema[Integer] -> String,
		$libRemoveStaleFiles[Integer] -> String,
		$libRemoveSelectedDocuments[Integer] -> None,
		$libFinalizeWrites[Integer] -> None,
		$libSelect[Integer, String] -> Integer,
		$libSelectByColumn[Integer, String, String] -> Integer,
		$libSelectSymbolic[LinkObject] -> LinkObject,
		$libGetDocumentCount[Integer] -> Integer,
		$libGetColumn[Integer, String, Boole] -> String,
		$libGetAllColumns[Integer, Boole] -> String,
		$libDumpAllColumns[Integer, String, Boole] -> String,
		$libEnableFileLogging[String] -> None,
		$libEnablePrintLogging[] -> None,
		$libDisableLogging[] -> None
	];
	Clear[LoadCLuceneLink];
);

DCreateEmptyIndex[CLuceneHandle[id_]] := 
	$libCreateEmptyIndex[id];

$writingIndexText = "writing to index";

DAddToIndex[CLuceneHandle[id_], files_, OptionsPattern[]] := Block[
	{$done = False, $AccumulatedData = 0, $id = id, error}, 
	Internal`WithLocalSettings[
		displayProgressBar[files];
	,
		Scan[addDocument, files];
		error = $libFinalizeWrites[id]; (*Note: this returns a boolean, use to spot problems*)
		done = True;  
	,
		If[!done, 
			libClearAll[]; (* definition missing; should this flush everything?*)
		,
			setProgressBarText[$writingIndexText];
		];
		deleteProgressBar[];
	];
];

addDocument[$ExplicitFile[path_]] := 
	Block[{$FileSizeLimitsEnabled = False}, addDocument[path]];

addDocument[path_String] := Catch @ Block[
	{fields = ReadFileFields[path], $added, $doc},
	$doc = fields;
	incrementProgressBar[path];
	If[FailureQ[fields], Return[]];
	addFields[fields];
	$added = !$libAddDocument[$id];
	(*Note: this returns lastError != NULL, so False is good
	Do something with the error?
	*)
	$IndexedFileCount++;    
];

addDocument[fields_Association] := Catch @ Block[{$doc = fields},
	(* We should increment the progess bar with something
	path might be not present in the association*)
	addFields[fields];
	$libAddDocument[$id]; 
	(* note: this produces a boolean;
	should be used to detect errors*)
	$IndexedFileCount++;
]

CreateSearchIndex::incont = "Document `` has a type for the key `` that is inconsistent with other documents. This document will be skipped.";

addFields[fields_] := KeyValueMap[addField, fields];

CreateSearchIndex::badfield = "Field `` with value `` is not supported.";

addField[n_, val_] := Replace[addField2[n, val], _LibraryFunctionError :> reportInconsistentField[n]];
addField2[name_, Text[File[path_String]]] := 	$libAddTextFileField[$id, name, path];
addField2[name_, File[path_]] := 				$libAddPathField[$id, name, path];
addField2[name_, Text[str_String]]:= 			$libAddTextField[$id, name, str];
addField2[name_, str_String] := 				$libAddStringField[$id, name, str];
addField2[name_, int_Integer] :=  				$libAddIntegerField[$id, name, int];
addField2[name_, d_DateObject] := 				$libAddUnixTimeField[$id, name, UnixTime @ d];
addField2[name_, bool_?BooleanQ] :=  			$libAddBooleanField[$id, name, bool];
addField2[name_, value_] := (Message[CreateSearchIndex::badfield, name, value]; $Failed);

reportInconsistentField[field_] := ( 
	Message[CreateSearchIndex::incont, ContentObject[$doc], field];
	$libClearDocument[$id];
	Throw[$Failed];
);

DGetAllDocuments[CLuceneHandle[id_]] :=
	parse @ $libGetAllColumns[id, False];

DGetColumn[CLuceneHandle[id_], column_] :=
	parse @ $libGetColumn[id, column, False];

DRemoveStaleDocumentsFromIndex[CLuceneHandle[id_]] := 
	parse @ $libRemoveStaleFiles[id];

DGetDocumentCount[CLuceneHandle[id_]] := 
	$libGetDocumentCount[id];

DQueryCount[CLuceneHandle[id_], ColumnQuery[queryColumn_, query_]] := (
	$libSelectByColumn[id, queryColumn, query]
)

SetAttributes[intThen, HoldAllComplete];
intThen[a_, b_] := If[IntegerQ[a], b, $Failed];

DQuery[CLuceneHandle[id_], ColumnQuery[queryColumn_, query_], returnColumn_] := intThen[
	$libSelectByColumn[id, queryColumn, query],
	parse @ $libGetColumn[id, returnColumn, True]
];

DQueryBulkReturn[CLuceneHandle[id_], ColumnQuery[queryColumn_, query_]] := intThen[
	$libSelectByColumn[id, queryColumn, query],
	parse @ $libGetAllColumns[id, True]
];

DSupportsQ[_CLuceneHandle, SymbolicQuery|LuceneQuery] := True

DQuery[CLuceneHandle[id_], SymbolicQuery[query_], returnColumn_] := intThen[
	$libSelectSymbolic[id, query],
	parse @ $libGetColumn[id, returnColumn, True]
];

DQueryBulkReturn[CLuceneHandle[id_], SymbolicQuery[query_]] := intThen[
	$libSelectSymbolic[id, query],
	parse @ $libGetAllColumns[id, True]
];

DQueryCount[CLuceneHandle[id_], SymbolicQuery[query_]] :=
	Replace[$libSelectSymbolic[id, query], Except[_Integer] -> $Failed];

DQuery[CLuceneHandle[id_], LuceneQuery[query_], returnColumn_] := intThen[
	$libSelect[id, query],
	parse @ $libGetColumn[id, returnColumn, True]
]

DQueryBulkReturn[CLuceneHandle[id_], LuceneQuery[query_]] := intThen[
	$libSelect[id, query],
	parse @ $libGetAllColumns[id, True]
]

DQueryCount[CLuceneHandle[id_], LuceneQuery[query_]] :=
	Replace[$libSelect[id, query], Except[_Integer] -> $Failed]

parse[str_String] := 
	Block[{$Context = "TextSearch`SandboxContext`"},
		Quiet @ Check[ToExpression[str], $Failed]
	];

parse[e_] := Throw[$Failed];



PackageScope["CreateManagedLibraryExpressionWithFunction"]

managedExpressionData = <||>;
scheduledToBeReleased = {};

(* JLink does not provide automatic memory management for loaded objects. Managed library
 * expressions provide that automatic memory management, but it's designed to be used
 * with "library link" libraries. The followgin functions provide that functionality
 * to any Mathematica code, so it can be used with JLink code.
 *
 * CreateManagedLibraryExpressionWithFunction[f, 99] will return an expression. At some
 * point after that expression is no longer referenced f[99] will be called. So the idea
 * is to put this expression inside the object you want to have its memory automatically managed.
 *)
CreateManagedLibraryExpressionWithFunction[fun_Function, args___] := Module[{e},
	LoadCLuceneLink[];

	(* This "mlewf" string (stands for Managed Library Expression With Function) is used
	 * by the managed library expression feature to call the right "manager
	 * function" on the C++ side. So the same string must be used in the C++
	 * code to register that function.
	 *)
	e = CreateManagedLibraryExpression["mlewf", MLEWF];

	managedExpressionData[First@e] = {fun, List@args};
	e
]

PackageScope["ManagedExpressionReleased"]

(* I schedule the execution of removal functions because CreateManagedLibraryExpression is buggy and
 * notifies that an expression is unused slightly before it is OK to release it. So far as we know,
 * this isn't a "false positive" problem -- notifications are simply a bit too learly. To work
 * around this, we are not freeing the expression immediately, but are rather adding the expressions
 * to a queue which will be released the next time the user does a TextSearch. (which calls
 * ReleasePendingManagedExpressions)
 *
 * UPDATE: Nicolas later installed Mathematica V11 and tried to reproduce this, but couldn't.
 *         So this might be fixed. A 'todo' would perhaps be to test this a bit more and then
 *         remove the workaround, but with V11 frozen, etc, we won't do that right now.
 *
 * TODO: Try to get this functionality into Mathematica itself. ex. Show Todd Gayley, but
 *       probably others as well.
 *
 * If this is fixed then the removal code whould be called immediately without any scheduling.
 *)

ManagedExpressionReleased[id_] := AppendTo[scheduledToBeReleased, id]

PackageScope["ReleasePendingManagedExpressions"]

ReleasePendingManagedExpressions[] := (Function[id, With[{x=managedExpressionData[id]},
		(First@x)@@@(Rest@x);
		KeyDropFrom[managedExpressionData, id];
	]] /@ scheduledToBeReleased;
	scheduledToBeReleased = {};
)
