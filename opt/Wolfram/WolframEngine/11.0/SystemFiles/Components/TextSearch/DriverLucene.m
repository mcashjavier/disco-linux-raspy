Package["TextSearch`"]

PackageScope["DetermineFieldType"]

PackageScope["ValidateOptions"]

PackageScope["ValidateContentFieldOptions"]

PackageScope["FieldFile"]

PackageScope["ReadFieldFile"]

PackageScope["GetRankingModel"]

PackageImport["PacletManager`"]
PackageImport["JLink`"]

PackageExport["LuceneHandle"]

(* I use this to disable Print statements. *)
Attributes[xPrint] = {HoldAllComplete};

(* With $ShowJavaExceptions...
 *     False: Hide all Java exceptions.
 *     True:  show all Java exceptions, but hide the ones that are really messages to be shown.
 *     All:   Show even the exceptions generate user messages.
 *)
PackageScope["$ShowJavaExceptions"]
If [!ValueQ[$ShowJavaExceptions],
	$ShowJavaExceptions = False;
];

$lastJavaStackTrace = Null;

summarizeCamelCase[str_] := StringJoin @ StringCases[str, RegularExpression["[[:upper:]]"]]

removeJavaPackage[cl_] := StringReplace[cl, RegularExpression["^(?:[^.]+\\.)*([^.]+)$"] -> "$1"]

summarizeJavaClassName[cl_] := summarizeCamelCase @ removeJavaPackage @ cl

(* Creates a minimal summary of a stack trace that can be included in a user message as
 * an obscure code (as described in GetJavaExceptionObscureSummary).
 *)
summarizeJavaStackTrace[st_] := ToLowerCase@StringJoin@Riffle[Flatten @ {
		summarizeJavaClassName @ StringReplace[st, RegularExpression["(?s)^([^:]*):.*$"] -> "$1"],
		Function[fr,
			Block[{fname, num}, Quiet@Check[
				{fname, num} = First@StringReplace[fr, RegularExpression@"^.*\\((.+):([0-9]+)\\).*$" -> {"$1", "$2"}];
				{summarizeCamelCase@fname, num},
				"*"
			]]
		] /@ Take[StringCases[st, RegularExpression["at .*"]], UpTo[4]]
	}, "."]

PackageScope["ClearJavaException"]
ClearJavaException[] := $lastJavaStackTrace = Null

(* Returns a minimal summary of the last thrown exception stack trace that can be included in a user message as an obscure code.
 *
 * If the exception is:
 *
 * Java::excptn: A Java exception occurred: java.lang.IllegalArgumentException: something
 *     at com.wolfram.textsearch.TextSearchIndex.search(TextSearchIndex.java:731)
 *     at com.wolfram.textsearch.TextSearchIndex.search(TextSearchIndex.java:722).
 *
 * ...this function returns:
 *
 *     IAE:TSI:731:TSI:722
 *)
PackageScope["GetJavaExceptionObscureSummary"]
GetJavaExceptionObscureSummary[] := $lastJavaStackTrace /. {st_String :> summarizeJavaStackTrace[st]}

exceptionHandler[s_, t_, trace_] := Module[{e},
	e = GetJavaException[];
	If[InstanceOf[e, "com.wolfram.textsearch.MessageException"],
			With[{ms = Symbol[e@symbol], mt = e@tag, margs = e@args},
				Message[MessageName[ms, mt], Sequence@@(margs)]
			];
			$FailureExplained = True;
			If[$ShowJavaExceptions === All,
				Message[MessageName[s, t], trace]]
		,
			$lastJavaStackTrace = trace;
			If[$ShowJavaExceptions =!= False,
				Message[MessageName[s, t], trace]]
	];
]

callJava[code_] := JavaBlock[Block[{$JavaExceptionHandler = exceptionHandler}, code]]
SetAttributes[callJava, HoldFirst]

DSupportsQ[_LuceneHandle, SymbolicQuery|LuceneQuery|SearchResultObject] := True

DCreateHandle[path_String, "Lucene", version_ /; version > 0.1] := Catch[
	SetupJavaLucene[];
	callJava[With[{idx = TextSearchIndex`of[AbsoluteFileName @ path]},
		KeepJavaObject@idx;
		LuceneHandle[idx]
	]]
];

DDeleteHandle[LuceneHandle[idx_]] := callJava[idx@close[]; ReleaseJavaObject@idx;]

SetupJavaLucene[] := (
	InstallJava[];

	With[{base=DirectoryName[FindFile["TextSearch`"]]},
		AddToClassPath[FileNameJoin[{base, "javaBuild"}], FileNameJoin[{base, "Java"}]];
	];

	LoadJavaClass["com.wolfram.textsearch.TextSearchIndex"];

	SetupJavaLucene[] = Null;
);

DCreateEmptyIndex[LuceneHandle[idx_]] := callJava[idx@createEmpty[]]

DAddToIndex[LuceneHandle[idx_], objectsIn_, OptionsPattern[]] :=
	callJava@Block[{assocs, fieldOptions, fieldNames, firstChunkFlag, files, objects = objectsIn,
		   partitionSize = 100, numFilesToRead, objectsToExamineToDetermineFields, fields, theseAssocs,
		   dir, fieldsForSetFields, fieldNameList, fieldNameListFromOptions, unknownFields, readFiles, 
		   thisChunksFields, seenFieldNames, neverBeforeSeenFields, subsetOfAssocs, theseFields},
		
		(* Used for whitebox testing. Namely, we can set
		   $partitionSizeOverride = 1 to test the situation where
		   non-first chunks contain fields not yet before seen. *)
		If [IntegerQ[$partitionSizeOverride],
			partitionSize = $partitionSizeOverride;
		];
		
		xPrint[objectsIn];
		
		idx@setRankingModel[OptionValue[Method]];
		
		If [OptionValue["SearchSynonyms"] =!= None,
			idx@parseSynonyms[OptionValue["SearchSynonyms"]];
		];
		
		dir = idx@getDirectory[];
		
		fieldOptions = OptionValue[ContentFieldOptions] /. None :> <||>;
		If [MatchQ[fieldOptions, List[___Rule]],
			(* If the user specified the field options as a list instead
			   of an association, silently fix it. *)
			fieldOptions = Association @@ fieldOptions;
		];
		
		If [FailureQ[ValidateContentFieldOptions[fieldOptions]], Return[$Failed, Module]];
		
		displayProgressBar[objects];
		
		(* Earlier process should ensure that we don't have
		   things in the list that are neither associations nor
		   strings. *)
		assocs = Cases[objects, _Association];
		subsetOfAssocs = Take[assocs, Min[partitionSize, Length[assocs]]];
		files = Cases[objects, _String | _$ExplicitFile];
		objects = Join[assocs, files];
		
		(* Read some of the files so that we can see the fields
		   and their contents, which we then use to determine
		   the list of field names as well as to do some analysis
		   on the field contents to determine their type, etc. *)
		If [files =!= {},
			
			numFilesToRead = Min[partitionSize, Length[files]];
			
			readFiles =
				Replace[
					Take[files, numFilesToRead],
					{
					path_String :> ReadFileFields[path],
					$ExplicitFile[path_String] :> ReadFileFields[path]
					},
					{1}
				];
				
			
			(* TODO: Why do files like SELLIDAE (Britannica) fail to read? And
			   should we really silently skip them? *)
			readFiles = DeleteCases[readFiles, $Failed];
			
			files =
				Join[
					readFiles,
					files[[ numFilesToRead + 1 ;; -1 ]]
				];
			
			xPrint[files];
			
			objectsToExamineToDetermineFields =
				Join[
					subsetOfAssocs,
					readFiles
				];
			,
			objectsToExamineToDetermineFields = subsetOfAssocs;
		];
		
		If [Length[objectsToExamineToDetermineFields] === 0,
			(* Don't issue a message here, because CreateSearchIndex
			   will take care of that, and if UpdateSearchIndex is
			   called, we don't want to complain about there being
			   no new files. *)
			Return[$Failed, Block];
		];
		
		If [OptionValue[Language] === Automatic,
			With[{defaultLanguage = ClassifyLanguageOfContentObjects[objectsToExamineToDetermineFields]},
				If [defaultLanguage =!= Automatic,
					$DefaultLanguage = defaultLanguage;
				];
			];
			,
			$DefaultLanguage = OptionValue[Language];
		];
		
		xPrint[$DefaultLanguage];
		
		fields = determineFields[objectsToExamineToDetermineFields, fieldOptions];
		fields = DeleteCases[fields, KeyValuePattern[{"Type" -> $Failed}]];
		
		xPrint[fields[[All, {"Name", "Type", "JavaType"}]] // Indent2];
		
		xPrint[fields // Indent2];
		
		(* Ignore fields that only have Null values. *)
		fields = Select[fields, #["Type"] =!= Null &];
		
		seenFieldNames = Lookup[fields, "Name"]
			(* If all field values seen so far are null, then
			   in theory we might not have seen any fields
			   with non-null values. *)
			/. _Missing :> {};
		
		(* Ensure that all named fields in the ContentFieldOptions actually exist. *)
		fieldNameList = fields[[All, "Name"]];
		fieldNameListFromOptions = Keys[fieldOptions];
		unknownFields = Complement[fieldNameListFromOptions, Append[fieldNameList, All]];
		If [Length[unknownFields] > 0,
			Message[CreateSearchIndex::uf, unknownFields[[1]]];
			Return[$Failed, Module];
		];
		
		(* Tell the index about the fields *)
		fieldsForSetFields = convertFieldForSetFields /@ fields;
		idx@setFields[fieldsForSetFields];
		
		(* Write the field metadata to disk so that
		   future uses of the index can load it. *)
		WriteFileIndented[fieldsForSetFields, FieldFile[dir]];
		
		fieldNames = Lookup[fields, "Name"];
		
		firstChunkFlag = True;
		
		Function[{subsetOfObjects},
			
			(* Read files. *)
			theseAssocs =
				Replace[
					Replace[
						subsetOfObjects,
						{
						path_String :> ReadFileFields[path],
						$ExplicitFile[path_String] :> ReadFileFields[path]
						},
						{1}
					],
					_Missing :> Null,
					{2, 3}
				];
			
			(* Sometimes when we try to read a file, it fails.
			   For example, we might not have permission to read the file. (?) *)
			theseAssocs = DeleteCases[theseAssocs, $Failed];
			
			thisChunksFields = DeleteDuplicates[Flatten[Keys[theseAssocs]]];
			neverBeforeSeenFields = Complement[thisChunksFields, seenFieldNames];
			
			(* Fields in this chunk not seen before? *)
			If [neverBeforeSeenFields =!= {},
				xPrint["Never-before-seen fields: ", neverBeforeSeenFields];
				theseFields = determineFields[theseAssocs, fieldOptions];
				theseFields = DeleteCases[theseFields, KeyValuePattern[{"Type" -> $Failed}]];

				(* Ignore fields that only have Null values. *)
				theseFields = Select[theseFields, #["Type"] =!= Null &];
				
				theseFields = Select[theseFields, MemberQ[neverBeforeSeenFields, #["Name"]] &];
				
				If [theseFields =!= {},
					seenFieldNames = Join[seenFieldNames, Lookup[theseFields, "Name"]];
					fields = Join[fields, theseFields];
					idx@setFields[convertFieldForSetFields /@ theseFields];
				];
			];
			
			Function[{field},
				With[{fieldName = field["Name"], javaType = field["JavaType"]},
					With[{values =
							Replace[
								processFieldValues[field["Type"], javaType, Lookup[theseAssocs, fieldName, Null]],
								(* We allow lists of values, but if a list is empty,
								   we treat that as a null/unset field value. *)
								{} :> Null,
								1
							]},
						Switch[javaType,
							"String",
							idx@sendStringValuesToJava[fieldName, values];
							,
							"Long",
							idx@sendLongValuesToJava[fieldName, values];
							,
							"Double",
							idx@sendDoubleValuesToJava[fieldName, values];
							,
							"Boolean",
							idx@sendStringValuesToJava[
									fieldName,
									Replace[
										Replace[values, {True -> "T", False -> "F"}, {1, 2}],
										(* Null values inside of values that are lists of booleans. *)
										Null -> "N",
										{2}
									]
								];
							,
							"TextOrTextFilesViaExpr",
							idx@sendExprValuesToJava[fieldName, values];
							,
							_,
							Message[CreateSearchIndex::cdft, fieldName];
							Return[$Failed, Module];
						];
					]
				]
			] /@ fields;
			
			idx@addDocuments[];
			
			$IndexedFileCount += Length@subsetOfObjects;
			
			firstChunkFlag = False;
			
			(* Update the progress bar. *)
			incrementProgressBar["", Length[subsetOfObjects]];
	
		] /@ Partition[objects, partitionSize, partitionSize, {1, 1}, {}];
	
		idx@finalizeWrites[];
	
		deleteProgressBar[];
	]

DGetAllDocuments[LuceneHandle[idx_]] := callJava[ parse[(idx @ search[All]) @ serialize[0, 0, False, Null]] ]

DGetColumn[LuceneHandle[idx_], returnColumn_] := callJava[ parse[(idx @ search[All]) @ serialize[0, 0, False, returnColumn]][returnColumn] ]

DRemoveStaleDocumentsFromIndex[LuceneHandle[idx_]] := callJava[parse[idx@removeStaleFiles[]]];

DGetDocumentCount[LuceneHandle[idx_]] := callJava[idx@getDocumentCount[]];

DQuery[LuceneHandle[idx_], (LuceneQuery|SymbolicQuery)[query_], returnColumn_] := callJava[ parse[(idx @ search[query]) @ serialize[0, 0, True, returnColumn]][returnColumn] ]

DQueryBulkReturn[LuceneHandle[idx_], (LuceneQuery|SymbolicQuery)[query_]] := callJava[ parse[(idx @ search[query]) @ serialize[0, 0, True, Null]] ]

DQueryCount[LuceneHandle[idx_], (LuceneQuery|SymbolicQuery)[query_]] := callJava[idx@searchCount[query]]

DQueryReturnObject[LuceneHandle[idx_], (LuceneQuery|SymbolicQuery)[query_]] := callJava[With[{res = idx @ search[query]},
	(* "Managed expressions" are not being automatically released due to a bug.
	 * So, somethat arbitratily, we have them processed here.
	 *)
	ReleasePendingManagedExpressions[];
	If[!JavaObjectQ[res],
		Return[res]];
	KeepJavaObject[res];
	SearchResultObject[LuceneHandle[idx], res, CreateManagedLibraryExpressionWithFunction[ReleaseJavaObject@# &, res]]
]]

DQueryReturnObject[LuceneHandle[idx_], All] := DQueryReturnObject[LuceneHandle[idx], SymbolicQuery[All]]

SearchResultObject[LuceneHandle[idx_], res_, ___]["Count"] := callJava[res @ getCount[]]

DGetSearchResultObjectInternalProperty[SearchResultObject[LuceneHandle[idx_], res_, ___], All] := callJava[parse @ res @ serialize[0, 0, True, Null]]
DGetSearchResultObjectInternalProperty[SearchResultObject[LuceneHandle[idx_], res_, ___], Span[i_, j_]] := callJava[parse @ res @ serialize[i - 1, j /. {All -> 0, x_Integer :> (x-i) + 1}, True, Null]]

DCreateSearchResultSnippet[SearchResultObject[LuceneHandle[idx_], res_, ___], field_String, File[fileName_String]] :=
	callJava[res @ createSnippetFromFile[field, fileName]]

parse[str_String] :=
	Block[{$Context = "TextSearch`SandboxContext`"},
		With[{rawRes = Quiet @ Check[ToExpression[str], $Failed]},
			
			xPrint[str];
			xPrint[rawRes];
			
			If [ListQ[rawRes],
				(* Calls such as parse just return a list. *)
				rawRes
				,
				(* TextSearch returns a list of ContentObjects, which doesn't
				   leave room for metadata that pertains to the entire query.
				   But debugging tools can benefit from such metadata, such
				   as the string form of the Lucene query string. To allow
				   for this kind of thing, the $AdditionalQueryResultMetadata
				   variable will be used. If we want to use this more
				   extensively, then we may wish to change all of the
				   functions that call into 'parse' (and their callers)
				   to expect a data structure that isn't simply a list of
				   search results. *)
				TextSearch`Private`$AdditionalQueryResultMetadata = rawRes;
				TextSearch`Private`$AdditionalQueryResultMetadata = Delete[TextSearch`Private`$AdditionalQueryResultMetadata, "SearchResults"];
				
				rawRes["SearchResults"]
			]
		]
	];

DQuery[h_LuceneHandle, ColumnQuery[queryColumn_, query_], returnColumn_] := DQuery[h, SymbolicQuery[QString[queryColumn, query]], returnColumn]

DQueryBulkReturn[h_LuceneHandle, ColumnQuery[queryColumn_, query_]] := DQueryBulkReturn[h, SymbolicQuery[QString[queryColumn, query]]]

DQueryCount[h_LuceneHandle, ColumnQuery[queryColumn_, query_]] := DQueryCount[h, SymbolicQuery[QString[queryColumn, query]]]

DQueryReturnObject[h_LuceneHandle, ColumnQuery[queryColumn_, query_]] := DQueryReturnObject[h, SymbolicQuery[QString[queryColumn, query]]]

parse[e_] := Throw[$Failed];

(*!
	\function createFieldDefintion
	
	\calltable
		createFieldDefintion[name_String, type_String, options_Association] '' Wrapper around the Java method of the same name for testing calls to it.

	Examples:
	
	TextSearch`DriverLucene`PackagePrivate`createFieldDefintion[
	<|
		"Name" -> "Title",
		"Type" -> "Title",
		"Tokenized" -> True,
		"Stored" -> False,
		"Weight" -> 10,
		"Language" -> "English",
		"IgnoreCase" -> True,
		"Stemmed" -> True,
		"CamelCaseMatching" -> True
	|>
	]
	
	\related '
	
	\maintainer danielb
*)
createFieldDefintion[options_Association] :=
	Block[{},
		LoadJavaClass["com.wolfram.textsearch.FieldDefinition"];
		JavaNew[
			"com.wolfram.textsearch.FieldDefinition",
			options
		]
	]

(*!
	\function DetermineFieldType
	
	\calltable
		DetermineFieldType[fieldName, values] '' given a field's name and its values, return its type, as well as its Java type.

	Examples:
	
	DetermineFieldType["Path", {}] === {"FilePath", "String"}

	Unit tests: DetermineFieldType.mt

	\maintainer danielb
*)

Clear[DetermineFieldType];

DetermineFieldType[_, _] := $Failed

(* If something's values are all null, then we won't commit
   to any guess about what it's type might be. *)
DetermineFieldType[_, {Repeated[Null]}] := {Null, Null}

(* MIXTURE_OF_TEXT_AND_TEXT_FILES (Don't delete the text to the left, it's referenced elsewhere)

   The function ReadFileFields treats .txt files and other files (ex. nb) differently.
   For text files, it doesn't actually read the file, it returns a designator
   such as Text[File["C:\\Temp\\Test.txt"]]. For .nb files, etc, it does read the
   file, and returns the content as Text[fileContents_String]. We use the following
   pattern to try and detect the use of ReadFileFields, and we assign such fields
   the Java type Expr[], since we might end up having to send Java not just strings, but
   possibly a mixture of file names and file contents, which we need to distinguish
   between within Java. To do that, we will leave the file names wrapped in
   File[...], thus requiring the Expr[] data type when passing to Java. Note that
   detecting the need for Expr using this pattern is a bit weak. For example, if the
   ReadFileFields function was changed so that it no longer wrapped file contents
   in Text[...], this pattern wouldn't match, and the transfer to Java would then fail
   if any Text[File[path_String]] entries were included. On the other hand, if
   for some reason a customer (or we internally) were to call CreateSearchIndex
   with values wrapped in Text, it would see that and use Expr as the transfer
   mechanism. That's probably OK, though, no harm done there I suppose. *)
DetermineFieldType[_, list:{Repeated[Text[File[_]] | Text[_String] | Null]}] := {"Text", "TextOrTextFilesViaExpr"}

DetermineFieldType["File", {RepeatedNull[_String | {RepeatedNull[_String | Null]} | Null]}] := {"File", "String"}
DetermineFieldType["Path" | "FilePath" | "Location", {RepeatedNull[_String | File[_String] | {RepeatedNull[_String | File[_String] | Null]} | Null]}] := {"FilePath", "String"}
DetermineFieldType["FileExtension", {RepeatedNull[_String | File[_String] | {RepeatedNull[_String | File[_String] | Null]} | Null]}] := {"FileExtension", "String"}
DetermineFieldType["FileName", {RepeatedNull[_String | {RepeatedNull[_String | Null]} | Null]}] := {"FileName", "String"}
DetermineFieldType["Title" | "Subject", {RepeatedNull[_String | {RepeatedNull[_String | Null]} | Null]}] := {"Title", "String"}
DetermineFieldType["Body" | "Content", {RepeatedNull[_String | {RepeatedNull[_String | Null]} | Null]}] := {"Body", "String"}
DetermineFieldType["Author", {RepeatedNull[_String | {RepeatedNull[_String | Null]} | Null]}] := {"Author", "String"}

(* By default, we treat string values as text which should
   be tokenized. Note that this down value must come below ones
   such as "Title", otherwise it will trigger when we don't want
   to. *)
DetermineFieldType[_, {Repeated[_String | {RepeatedNull[_String | Null]} | {} | Null]}] := {"Text", "String"}

DetermineFieldType[_, vals:{Repeated[_Integer | {Repeated[_Integer | _Real | Null]} | _Real | {} | Null]}] :=
	If [FreeQ[vals, _Real, 2],
		 {"Integer", "Long"}
		 ,
		 {"Real", "Double"}
	]

DetermineFieldType["Weight", vals:{Repeated[_Integer | {Repeated[_Integer | _Real]} | _Real | {} | Null]}] := {"Real", "Double"}
	
DetermineFieldType[_, {Repeated[DateObject[_, _TimeObject, ___] | {RepeatedNull[DateObject[_, _TimeObject, ___] | Null]} | {} | Null]}] := {"DateTime", "Long"}
DetermineFieldType[_, {Repeated[DateObject[{_, _, _, __}] | {RepeatedNull[DateObject[{_, _, _, __}] | Null]} | {} | Null]}] := {"DateTime", "Long"}
DetermineFieldType[_, {Repeated[DateObject[{_, _, _}] | {RepeatedNull[DateObject[{_, _, _}] | Null]} | {} | Null]}] := {"Date", "Long"}
DetermineFieldType[_, {Repeated[DateObject[{_, _}] | {RepeatedNull[DateObject[{_, _}] | Null]} | {} | Null]}] := {"Month", "Long"}
DetermineFieldType[_, {Repeated[DateObject[{_}] | {RepeatedNull[DateObject[{_}] | Null]} | {} | Null]}] := {"Year", "Long"}
DetermineFieldType[_, {Repeated[True | False | {RepeatedNull[True | False | Null]} | {} | Null]}] := {"Boolean", "Boolean"}

(*!
	\function determineFields
	
	\calltable
		determineFields[assocs, fieldOptions] '' given a list of associations, returns back the list of fields (keys) present, along with information about those fields, such as their type, etc.

	If a field's type couldn't be determined, the type will be specified as $Failed, and
	a message will be emitted.

	Examples:
	
    determineFields[
        {
            Association["Field1" -> "a", "Field2" -> 1],
            Association["Field1" -> "b", "Field2" -> 2],
            Association["Field1" -> "c", "Field2" -> 3]
        },
        Association[]
    ]

    ===

    {
        Association[
            "Name" -> "Field1",
            "Type" -> "Text",
            "JavaType" -> "String",
            "ContentFieldOptions" -> <||>,
            "Values" -> {"a", "b", "c"}
        ],
        Association[
            "Name" -> "Field2",
            "Type" -> "Integer",
            "JavaType" -> "Long",
            "ContentFieldOptions" -> <||>,
            "Values" -> {1, 2, 3}
        ]
    }

	Unit tests: determineFields.mt

	\maintainer danielb
*)
determineFields[assocs:{___Association}, fieldOptionsIn_Association] :=
	Block[{fieldNames, fieldOptions = fieldOptionsIn, fieldType, javaFieldType, userGivenDefaults,
		   theseFieldOptions},
		
		fieldNames = DeleteDuplicates[Flatten[Keys /@ assocs]];
		
		userGivenDefaults = fieldOptions[All] /. _Missing :> <||>;
		
		Function[{fieldName},
			(* If a given field isn't specified for a particular document, we
			   use Null. Also: If any values were specified as _Missing, we turn those
			   into Null. *)
			With[{values = Replace[Lookup[assocs, fieldName, Null], _Missing :> Null, {1, 2}]},
				theseFieldOptions = Lookup[fieldOptions, fieldName, <||>];
				fieldType = Lookup[fieldOptions, "Type", Automatic];
				If [fieldType === Automatic,
					fieldType = DetermineFieldType[fieldName, values];
					,
					(* We'll still make use of DetermineFieldType to determine
					   the Java type of the values. *)
					With[{tmp = DetermineFieldType[fieldName, values]},
						fieldType = {fieldType, tmp[[2]]}
					]
				];
				If [fieldType === $Failed,
					Message[CreateSearchIndex::cdft, fieldName];
				];
				
				javaFieldType = fieldType /. Except[$Failed] :> fieldType[[2]];
				fieldType = fieldType /. Except[$Failed] :> fieldType[[1]];
				<|
					"Name" -> fieldName,
					"Type" -> fieldType,
					"JavaType" -> javaFieldType,
					"ContentFieldOptions" -> Merge[{theseFieldOptions, userGivenDefaults, defaultContentFieldOptions[fieldType]}, First],
					"Values" -> values
				|>
			]
		] /@ fieldNames
	];

(* The ContentFieldOptions option to CreateSearch index supports a variety
   of field options. This scheme captures those options and their
   expected types so that we can validate what the user provides. *)
$fieldOptionsSchema =
<|
	"Type" -> _String,
	"Tokenized" -> True | False,
	"Stored" -> True | False,
	"Weight" -> _Real | _Integer,
	"Language" ->
		Function[{lang},
			StringQ[lang] &&
			MemberQ[
				$SupportedLanguages,
				lang
			]
		],
	"IgnoreCase" -> True | False,
	"Stemmed" -> True | False,
	"CamelCaseMatching" -> True | False
|>;

(*!
	\function ValidateOptions
	
	\calltable
		ValidateOptions[options, schema, unknownOptionErrorFunc, invalidOptionValueErrorFunc] '' validates the given options using the given schema. If a bad option is found, the given error function is called.

	Examples:

    ValidateOptions[
        Association["BadOption" -> "English"],
        $fieldOptionsSchema,
        Function[{optionName}, "ERROR1"],
        Function[{optionName, optionValue}, "ERROR2"]
    ]

    ===

    "ERROR1"

    Unit tests: ValidateOptions.mt

    \maintainer danielb
*)
ValidateOptions[options_, schema_, unknownOptionErrorFunc_, invalidOptionValueErrorFunc_] :=
	Block[{optionSchema},
		Function[{option},
			With[{optionName = option[[1]], optionValue = option[[2]]},
				optionSchema = schema[optionName];
				If [MissingQ[optionSchema],
					Return[unknownOptionErrorFunc[optionName], Block];
				];
				Switch[Head[optionSchema],
					Function,
					(* We've been given a function to call to validate the option value. *)
					If [!TrueQ[optionSchema[optionValue]],
						Return[invalidOptionValueErrorFunc[optionName, optionValue], Block];
					];
					,
					_,
					(* We've been given a pattern to validate the option value. *)
					If [!MatchQ[optionValue, optionSchema],
						Return[invalidOptionValueErrorFunc[optionName, optionValue], Block];
					];
				];
			]
		] /@ Normal[options];
	];

(*!
	\function ValidateContentFieldOptions
	
	\calltable
		ValidateContentFieldOptions[fieldOptions] '' validates the field options passed to CreateSearchIndex.
		ValidateContentFieldOptions[fieldName, options] '' validates the field options passed to CreateSearchIndex.

	Examples:
    
    ValidateContentFieldOptions["MyField", Association["Langage" -> "English", Null]] === $Failed

    Unit tests: ValidateContentFieldOptions.mt

    \maintainer danielb
*)
ValidateContentFieldOptions[fieldName_, options_] :=
	Block[{},
		ValidateOptions[
			options,
			$fieldOptionsSchema,
			Function[{optionName},
				Message[CreateSearchIndex::ufo, optionName, fieldName];
				$Failed
			],
			Function[{optionName, optionValue},
				Message[CreateSearchIndex::ifov, optionValue, optionName, fieldName];
				$Failed
			]
		]
	];
	
ValidateContentFieldOptions[fieldOptions_Association] :=
	Block[{},
		Function[{field},
			If [FailureQ[ValidateContentFieldOptions[field[[1]], field[[2]]]],
				Return[$Failed, Block];
			]
		] /@ Normal[fieldOptions];
	]

(*!
	\function convertFieldForSetFields
	
	\calltable
		convertFieldForSetFields[field] '' converts a field to the format expected by the setFields Java method.

	Examples:
    
    convertFieldForSetFields[
        Association[
            "Name" -> "MyField",
            "Type" -> "Text",
            "JavaType" -> "String",
            "ContentFieldOptions" -> Association["Language" -> "French"],
            "Values" -> {"a", "b", "c"}
        ]
    ]

    ===

    <|"Name" -> "MyField", "Type" -> "Text", "Language" -> "French"|>

    Unit tests: convertFieldForSetFields.mt

    \maintainer danielb
*)
convertFieldForSetFields[field_] :=
	Block[{options},
		options = field["ContentFieldOptions"];
		Join[
			Delete[field, {{"JavaType"}, {"ContentFieldOptions"}, {"Values"}}],
			options
		]
	];

(*!
	\function FieldFile
	
	\calltable
		FieldFile[dir] '' returns the path to the .wl file that contains field information.
	
	\maintainer danielb
*)
FieldFile[dir_] := FileNameJoin[{dir, "fields.wl"}];

(*!
	\function ReadFieldFile
	
	\calltable
		ReadFieldFile[file] '' reads the fields.wl file and updates the Java object.

	Examples:
    
    ReadFieldFile[dir] === TODO
	
	\related '
	
	\maintainer danielb
*)
ReadFieldFile[file_, schemaObj_] :=
	Block[{fields},
		fields = Get[file];
		If [!FailureQ[fields],
			callJava[schemaObj@setFields[fields]]];
		fields
	];

(*!
	\function processFieldValues
	
	\calltable
		processFieldValues[fieldType, javaType, values] '' do any processing of values prior to sending them to Java. For example, we convert DateObjects to integers.

	Examples:
    
    processFieldValues["Date", {DateObject[{2016, 5, 2}], DateObject[{2016, 5, 3}]}]

    ===

    {1462161600, 1462248000}

    Unit tests: processFieldValues.mt

    \maintainer danielb
*)

Clear[processFieldValues];

processFieldValues[fieldType_, javaType_, values_] := values

processFieldValues["Date" | "Month" | "Year", javaType_, values_] :=
	(* We should specify the time zone as 0, otherwise it is
	   assumed to be the local time zone which may not be
	   consistant with the time zone of the computer that
	   ends up using this index. *)
	Block[{$TimeZone = 0},
		processFieldValues["DateTime", javaType, values]
	]

processFieldValues["DateTime", javaType_, values_] :=
	Replace[
		(* Use Replace to target DateObjects so that we don't call UnixTime on Null if present *)
		Replace[
			values,
			{
				d_DateObject :> UnixTime[d]
			},
			{1, 2}
		],
		(* Representation of null when inside of a list. *)
		Null :> 0,
		{2}
	]

processFieldValues["FilePath", "String", values_] :=
	processFieldValues[
		"Text",
		"String",
		Replace[values, e_File :> e[[1]], {1, 2}]
	]

(* For more information, search in this file for "MIXTURE_OF_TEXT_AND_TEXT_FILES" *)
processFieldValues["Text", "TextOrTextFilesViaExpr", values_] :=
	Replace[
		Replace[
			values,
			{
				(* Remove the Text wrapper *)
				Text[str_String] :> str,
				(* Remove the text wrapper but keep the File wrapper
				   so that Java will be able to differentiate between
				   text content VS files that it still needs to read
				   from disk. *)
				Text[file:File[_]] :> file
			},
			{1}
		],
		(* Representation of null when inside of a list. *)
		Null :> Text["<<NULL>>"],
		{2}
	]
	
processFieldValues[_, "String", values_] :=
	Replace[
		values,
		(* Representation of null when inside of a list. *)
		Null :> "<<NULL>>",
		{2}
	]
	
processFieldValues[_, "Long", values_] :=
	Replace[
		values,
		(* Representation of null when inside of a list.
		   Maximum value of a 32-bit value. (Long.MAX_VALUE) *)
		Null :> 9223372036854775807,
		{2}
	]
	
processFieldValues[_, "Double", values_] :=
	Replace[
		values,
		(* Representation of null when inside of a list.
		   Maximum value of a 64-bit double in Java. (Double.MAX_VALUE) *)
		Null :> 1.7976931348623157 * 10^308,
		{2}
	]

(*!
	\function defaultContentFieldOptions
	
	\calltable
		defaultContentFieldOptions[fieldType] '' given a field type, what are the default field options?

	Examples:
    
    defaultContentFieldOptions["Text"]

    ===

    Association[
        "Tokenized" -> True,
        "Stored" -> False,
        "Weight" -> 1,
        "Language" -> Null,
        "IgnoreCase" -> True,
        "Stemmed" -> True,
        "CamelCaseMatching" -> True
    ]

    Unit tests: defaultContentFieldOptions.mt

    \maintainer danielb
*)

Clear[defaultContentFieldOptions];

defaultContentFieldOptions[_] :=
<|
	"Tokenized" -> True,
	"Stored" -> False,
	"Weight" -> 1,
	"Language" -> $DefaultLanguage,
	"IgnoreCase" -> True,
	"Stemmed" -> True,
	"CamelCaseMatching" -> True
|>

defaultContentFieldOptions["Title"] :=
<|
	"Tokenized" -> True,
	(* Let's store the Title field by default. This will allow us
	   to check for exact title matches (etc) wrt secondary ranking.
	   And it seems reasonable to want to return back the title of
	   a document in the search results. *)
	"Stored" -> True,
	"Weight" -> 5,
	"Language" -> $DefaultLanguage,
	"IgnoreCase" -> True,
	"Stemmed" -> True,
	"CamelCaseMatching" -> True
|>;

defaultContentFieldOptions["FileName"] :=
<|
	"Tokenized" -> True,
	"Stored" -> True,
	(* ex. In britannica, the file "APPLE" should get boosted since
	       it is acting as the title. *)
	"Weight" -> 3,
	"Language" -> $DefaultLanguage,
	"IgnoreCase" -> True,
	"Stemmed" -> True,
	"CamelCaseMatching" -> True
|>;

defaultContentFieldOptions["FileExtension"] :=
<|
	"Tokenized" -> True,
	"Stored" -> True,
	"Weight" -> 2,
	"Language" -> $DefaultLanguage,
	"IgnoreCase" -> True,
	"Stemmed" -> False,
	"CamelCaseMatching" -> True
|>;

defaultContentFieldOptions["FilePath"] :=
<|
	(* We probably don't want to tokenize file paths by default.
	   For example, if the path is "C:\\Users\\Daniel\\...", and
	   we've built an index of Wikipedia, then searching for "Daniel"
	   shouldn't match every single document on account of them
	   all being inside of my user directory. *)
	"Tokenized" -> False,
	"Stored" -> True,
	"Weight" -> 1,
	"Language" -> $DefaultLanguage,
	"IgnoreCase" -> True,
	"Stemmed" -> True,
	"CamelCaseMatching" -> True
|>;

defaultContentFieldOptions["Text"] :=
<|
	"Tokenized" -> True,
	"Stored" -> False,
	"Weight" -> 1,
	"Language" -> $DefaultLanguage,
	"IgnoreCase" -> True,
	"Stemmed" -> True,
	"CamelCaseMatching" -> True
|>;

defaultContentFieldOptions["String" | "Integer" | "Long" | "Real" | "Boolean" | "DateTime" | "Date" | "Month"] :=
<|
	"Tokenized" -> False,
	"Stored" -> True,
	"Weight" -> 1
|>;

(*!
	\function GetRankingModel
	
	\calltable
		GetRankingModel[dir] '' gets the ranking model from the metadata file.
		
	Called from TextSearchIndex.java upon loading an index to determine
	the ranking model.
	
	\maintainer danielb
*)
GetRankingModel[dir_] :=
	Block[{},
		GetIndexMetadata[SearchIndexObject[File[dir]], "Method"]
	];
