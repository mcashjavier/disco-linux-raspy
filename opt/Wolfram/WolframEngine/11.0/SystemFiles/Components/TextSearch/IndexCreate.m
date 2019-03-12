Package["TextSearch`"]

PackageImport["Macros`"]

PackageExport["CreateSearchIndex"]
PackageExport["$DefaultDriver"]

$DefaultDriver = "CLucene";

SetArgumentCount[CreateSearchIndex, {1, 2}]

Options[CreateSearchIndex] = {
	OverwriteTarget -> False,					(*< Delete the existing index of the given name if it already exists. *) 
	ContentFieldOptions -> <||>,				(*< Options such as "IgnoreCase" can be specified on a per-field basis. *)
	Method -> "BM25",							(*< The ranking method to use. ex. "BM25", "TFIDF" *)
	Language -> Automatic,						(*< The language of the content being indexed. When Automatic, we attempt to automatically detect the language. *)
	SearchSynonyms -> None,						(*< Synonyms to be used when searching. *)
	ContentLocationFunction -> Automatic		(*< A function can be specified so that once an index has been deployed to the cloud, the path to a file can be mapped to a URI for use with the hyperlink. ContentLocationFunction is passed a string and should return a string. *)
};

(* Internal options that shouldn't show up in Options[CreateSearchIndex]. *)
$optionsHiddenFromUser =
{
	"Driver" -> "Lucene"						(*< "CLucene" or "Lucene", where "Lucene" means Java Lucene. *)
};

$allOptions =
Join[
	Options[CreateSearchIndex],
	$optionsHiddenFromUser
];

Clear[CreateSearchIndex];
CreateSearchIndex[args___] :=
	With[{res = Catch[iCreateSearchIndex[args], iCreateSearchIndex]},
		res /;
			HoldComplete[res] =!= HoldComplete[$BadArgs]
	]

Clear[iCreateSearchIndex];
iCreateSearchIndex[argsIn___] :=
	Block[{args, standardArgList, optionsList},
		
		(* Validates that there are between 1 and 2 standard arguments, and
		   that the options provided are supported in $allOptions.
		   This includes both user-facing options and hidden options. *)
		args = System`Private`Arguments[CreateSearchIndex[argsIn], {1, 2}, List, $allOptions];
		
		If [!MatchQ[args, {_List, _List}], Throw[$BadArgs, iCreateSearchIndex]];
		
		{standardArgList, optionsList} = args;
		
		If [!MatchQ[
				standardArgList,
				(* The standard argument patterns accepted *)
				{_String | _List | _Association} |
				{_String | _List | _Association, _String}
			],
			Throw[$BadArgs, iCreateSearchIndex];
		];
		
		iCreateSearchIndex2[Sequence @@ standardArgList, Sequence @@ optionsList]
	]

Clear[iCreateSearchIndex2];
Options[iCreateSearchIndex2] = $allOptions;
iCreateSearchIndex2[sources_, opts:OptionsPattern[]] :=
	iCreateSearchIndex2[sources, Automatic, opts];

iCreateSearchIndex2[sources_, name:Except[_Rule|_RuleDelayed], opts:OptionsPattern[]] := 
	If[!(name === Automatic || ValidSearchIndexObjectNameQ[name]), 
		Message[CreateSearchIndex::invindnm, name]; $Failed
		,
		iCreateSearchIndex3[
			sources, 
			Replace[name, Automatic :> CreateUUID[]], 
			opts
		]
	];

CreateSearchIndex::crfail = "Failed to create search index with name `` in directory ``.";
CreateSearchIndex::eind = "A SearchIndexObject with name `` already exists. Use DeleteSearchIndex to remove it.";
CreateSearchIndex::invdriver = "`` is not one of ``.";
CreateSearchIndex::indabort = "Abort occured during indexing. The partial index at `` will be deleted.";
CreateSearchIndex::invco = "Cannot index invalid ContentObject.";
CreateSearchIndex::cdft = "Couldn't determine field type for field `1`. Ensure field values use supported types.";
CreateSearchIndex::ufo = "Unsupported field option `1` for field `2`";
CreateSearchIndex::ifov = "Invalid value `1` for field option `2` for field `3`";
CreateSearchIndex::uf = "Unknown field `1` in the ContentFieldOptions";
CreateSearchIndex::invsyn = "Invalid synonym list.";
CreateSearchIndex::invrm = "Invalid ranking model `1` is not one of `2`";
CreateSearchIndex::invl = "Unsupported language `1` is not one of `2`";
CreateSearchIndex::invclf = "Invalid ContentLocationFunction.";
General::indempty = "No files were indexed. Files may have been ignored, or the provided path(s) may be empty.";

$validRankingModels = {"BM25", "TFIDF"};

$validContentLocationFunctionNames = {"ReferenceLocation", "SnippetSource"};
$validContentLocationFunction =
Alternatives[
	Automatic,
	_Function,
	Association[
		a_String -> _Function,
		b_String -> _Function
	] /; Complement[{a, b}, $validContentLocationFunctionNames] === {},
	Association[
		a_String -> _Function
	] /; Complement[{a}, $validContentLocationFunctionNames] === {}
];

Clear[iCreateSearchIndex3];
Options[iCreateSearchIndex3] = $allOptions;
iCreateSearchIndex3[sources_, name_, opts:OptionsPattern[]] :=
Internal`InheritedBlock[{$DefaultLanguage},
Module[
	{content, locations, obj, handle, finished, meta,
	 replace = OptionValue[OverwriteTarget],
	 driver = OptionValue["Driver"]},
	
	If[OptionValue[Language] =!= Automatic && !MemberQ[$SupportedLanguages, OptionValue[Language]],
		Message[CreateSearchIndex::invl, OptionValue[Language], $SupportedLanguages];
		Return[$Failed];
	];
	If[!MemberQ[$ValidDrivers, driver],
		Message[CreateSearchIndex::invdriver, driver, $ValidDrivers];
		Return[$Failed];
	];
	If[!MatchQ[OptionValue[ContentLocationFunction], $validContentLocationFunction],
		Message[CreateSearchIndex::invclf];
		Return[$Failed];
	];
	If[!MemberQ[Alternatives @@ $validRankingModels, OptionValue[Method]],
		Message[CreateSearchIndex::invrm, OptionValue[Method], $validRankingModels];
		Return[$Failed];
	];
	If[!MatchQ[OptionValue[SearchSynonyms], None | {RepeatedNull[{__String}]}],
		Message[CreateSearchIndex::invsyn];
		Return[$Failed];
	];
	path = ToObjectPath[name];
	If[FailureQ[path], Return[$Failed]];
	obj = SearchIndexObject[File[path]];
	If[DirectoryQ[path], 
		If[replace, 
			DeleteSearchIndex[obj];
		,
			Message[CreateSearchIndex::eind, name];
			Return[$Failed]
		];
	];
	If[FailureQ @ CreateDirectory[path], 
		Message[CreateSearchIndex::crfail, name, path];
		Return[$Failed];
	];
	{content, locations} = ParseInputSources[sources];
	If[content === {}, 
		Message[CreateSearchIndex::nosrcs];
		DeleteSearchIndex[obj];
		Return[$Failed];
	];
	meta = <|
		"IndexedPaths" -> locations, 
		"Driver" -> driver,
		"CreationDate" -> Now,
		"Version" -> $CurrentVersion,
		"Synonyms" -> OptionValue[SearchSynonyms],
		"Method" -> OptionValue[Method],
		"Language" -> OptionValue[Language],
		"ContentLocationFunction" -> OptionValue[ContentLocationFunction]
	|>;
	PutIndexMetadata[obj, meta];
	handle = CreateHandle[obj];
	If[FailureQ[handle],
		Return[$Failed];
	];
	DCreateEmptyIndex[handle];
	Internal`WithLocalSettings[
		$IndexedFileCount = 0;
		finished = False;
	,
		finished = DAddToIndex[handle, content, Sequence @@ FilterRules[{opts}, Options[DAddToIndex]]] =!= $Failed;
	,
		If[!finished, 
			If[$IndexedFileCount > 0,
				Message[CreateSearchIndex::indabort, First[obj]];
			];
			DeleteSearchIndex[obj];
			Return[$Failed, Module]
			,
			If[$IndexedFileCount === 0,
				Message[CreateSearchIndex::indempty];
				DeleteSearchIndex[obj];
				Return[$Failed, Module];
			]
		];
	];
	obj
]
]

ParseInputSources[sources_] := Block[{content, locations},
	locations = None;
	Which[
		VectorQ[sources, Head[#] === ContentObject&],
			If[!VectorQ[sources, ValidContentObjectQ], 
				Message[CreateSearchIndex::invco];
				Return[$Failed]
			];
			content = First /@ sources;
		,
		Developer`AssociationVectorQ[sources],
			content = sources;
		,
		AssociationQ[sources] && AllTrue[sources, ListQ],
			content = GeneralUtilities`AssociationTranspose[sources];
			If[content === $Failed, Return[$Failed]];
		,
		True,
			locations = NormalizeSources[sources, CreateSearchIndex];
			content = EnumerateFilesToIndex[locations];
	];
	{content, locations}
];


PackageScope["$DefaultLanguage"]
$DefaultLanguage = "English";

PackageScope["$SupportedLanguages"]
$SupportedLanguages =
Sort @
{
	"Arabic",
	"Bulgarian",
	(*"Brazilian Portuguese",*)
	"Catalan",
	"Chinese",
	"Japanese",
	"Korean",
	"KurdishCentral", (*"Sorani Kurdish",*)
	"Czech",
	"Danish",
	"German",
	"Greek",
	"English",
	"Spanish",
	"Basque",
	"Persian",
	"Finnish",
	"French",
	"GaelicIrish",
	"Galician",
	"Hindi",
	"Hungarian",
	"Armenian",
	"Indonesian",
	(*"Indian languages",*)
	"Italian",
	"Lithuanian",
	"Latvian",
	"Dutch",
	"Norwegian",
	"Portuguese",
	"Romanian",
	"Russian",
	"Serbian",
	"Swedish",
	"Thai",
	"Turkish"
};
