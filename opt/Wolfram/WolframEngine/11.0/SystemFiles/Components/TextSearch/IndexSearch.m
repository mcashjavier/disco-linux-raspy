Package["TextSearch`"]

PackageImport["Macros`"]
PackageImport["GeneralUtilities`"]

PackageScope["QUnion"]
PackageScope["QComplement"]
PackageScope["QIntersection"]
PackageScope["QRange"]
PackageScope["QString"]
PackageScope["QBoolean"]
PackageScope["QMust"]
PackageScope["QWildcard"]
PackageScope["QFuzzy"]
PackageScope["QNot"]
PackageScope["QBooleanQuery"]
PackageScope["QSearchBoost"]
PackageScope["ApplyContentLocationFunction"]

(* $FailureExplained should be used by top-level functions and set to False
   initially. If a lower level function displays an explanatory message
   to explain failure, it should set $FailureExplained to True to let the
   top-level function that it need not issue a generic "An internal error
   occurred while performing the search. Please contact Technical Support."
   message. *)
PackageScope["$FailureExplained"]

TextSearch::invq = "Invalid query ``.";
TextSearch::invqt = "Invalid query ``. Try: field -> `1`.";
TextSearch::invqe = "Invalid query element ``.";
TextSearch::invneg = "Negation in query `` is invalid.";
TextSearch::invrang = "Range bound \"``\" cannot have spaces.";
TextSearch::invsao = "Invalid SearchAdjustment option: `1`.";
TextSearch::invsa = "Invalid SearchAdjustment: `1`.";
TextSearch::invsav = "Invalid SearchAdjustment option value `1` for option `2`.";
TextSearch::invfa = "Invalid FixedOrder argument: `1`.";
TextSearch::unkfld = "Unknown field \"`1`\".";
TextSearch::nqu = "After stopword removal, the query had no content.";
TextSearch::pucsi = "Use of this feature requires an index to be created. Please use CreateSearchIndex."
TextSearch::pucsic = "Use of this feature in the cloud requires an index to be created. Please use CreateSearchIndex."

compileQuery[query_] := Catch @ Block[{$fq = query, res}, 
	
	res = cQuery[query];
	
	xPrint[res // Indent2];
	
	(* coallesce ranges *)
	
	If[!FreeQ[res, _QRange], 
		res = res //. Alternatives[
			QIntersection[a___, QRange[f_, lower_, Null, includeLower_, _], b___, QRange[f_, Null, upper_, _, includeUpper_], c___],
			QIntersection[a___, QRange[f_, Null, upper_, _, includeUpper_], b___, QRange[f_, lower_, Null, includeLower_, _], c___]
		] :> QIntersection[a, b, QRange[f, lower, upper, includeLower, includeUpper], c];
	];
	(* deal with QNots, by rewriting them as QComplements *)
	If[!FreeQ[res, _QNot],
		res = res //. q_QIntersection /; MemberQ[q, _QNot] :>
			If[Count[q, Except[_QNot]] === 0,
				Message[TextSearch::invneg, $fq]; Throw[$Failed]
				,
				QComplement[
					DeleteCases[q, _QNot],
					QIntersection @@ Cases[q, QNot[s_] :> s]
				]
			];
		(* if we have any QNots left over, we must give up: we cannot execute QNot directly 
		(because it would be potentially VERY expensive), we only execute QComplement.
		*)
		If[!FreeQ[res /. QBooleanQuery[pre___, _QNot, post___] :> QBoolean[pre, post], _QNot], 
			Message[TextSearch::invneg, $fq]; Throw[$Failed]
		];
	];
	res
];

(* QUnion[a..., Qunion[b...], c...] <=> QUnion[a, b, c] *)
SetAttributes[{QIntersection, QUnion}, Flat];
q_QIntersection /; Length[Unevaluated[q]] == 1 := First[Unevaluated[q]];
q_QUnion[_] /; Length[Unevaluated[q]] == 1 := First[Unevaluated[q]];


(* The grammar here is: cQuery -> cField -> cTerm, or more precisely:

cQuery:	one or more "'field' -> cField" specs, 
combined via And, Or, or List. If the 'field' is missing, it is
assumed to be "Plaintext".

cField: one or more cFields (via And, Or, Not, or List), or 
one or more cTerms, via ContainsXXX, or just a single cTerm.

cTerm: a string, integer, or symbol (e.g. True or False) 
*)


PackageExport["SearchBoost"]
PackageExport["SearchQueryString"]

Clear[cQuery, cField, cFixedOrderArg];

cQuery[Verbatim[Except][not_, term_]] := QComplement[cQuery[term], cQuery[not]];
cQuery[(Except|Not)[not_]] := QNot @ cQuery[not];
cQuery[e_List | e_And] := QIntersection @@ Map[cQuery, e];
cQuery[e_Or | e_Alternatives] := QUnion @@ Map[cQuery, e];
cQuery[field:(_String | {__String} | All) -> spec_] := Block[{$field = field}, cField[spec]];
cQuery[field:(_String | {__String} | All) -> spec_FixedOrder] := Block[{$field = field}, cQuery[spec]];
cQuery[q_String | q_ContainsAll | q_ContainsAny] := Block[{$field = If [StringQ[$field], $field, All]}, cField[q]];
cQuery[All] := All;
cQuery[SearchBoost[e_, boost_]] := QSearchBoost[cQuery[e], boost]
cQuery[SearchQueryString[q_String]] := ParseQueryString[q]
cQuery[e_FixedOrder] := FixedOrder @@ Map[cFixedOrderArg, e];
cQuery[t_] := (Message[TextSearch::invq, t]; Throw[$Failed];);
cQuery[SearchAdjustment[q_, weight:(_Integer | _Real), opts___Rule]] := (validateSearchAdjustmentOptions[{opts}]; QSearchBoost[SearchAdjustment[cQuery[q], opts], weight])
cQuery[SearchAdjustment[q_, opts___Rule]] := (validateSearchAdjustmentOptions[{opts}]; SearchAdjustment[cQuery[q], opts])
cQuery[sa:SearchAdjustment[q_, ___]] := (Message[TextSearch::invsa, sa]; Throw[$Failed])

cQuery[q:(_LessThan | _GreaterEqualThan | _LessEqualThan | _Between)] :=
	(Message[TextSearch::invqt, q]; Throw[$Failed])

cFixedOrderArg[e_Or | e_Alternatives] := QUnion @@ Map[cQuery, e];
cFixedOrderArg[q_String | q_ContainsAny] := Block[{$field = If [StringQ[$field], $field, All]}, cField[q]];
cFixedOrderArg[t_] := (Message[TextSearch::invfa, t]; Throw[$Failed];);

$valsPatt = {((_String|_Integer|SearchBoost[_String|_Integer, _])..)};
cField[e_And] := cField /@ QIntersection @@ e;
cField[e_Or] :=  cField /@ QUnion @@ e;
cField[ContainsAll[e:$valsPatt]] := cTerm /@ QIntersection @@ e;
cField[ContainsAny[e:$valsPatt]] := cTerm /@ QUnion @@ e;
cField[ContainsNone[e:$valsPatt]] := QIntersection @@ Map[QNot @* cTerm, e];
cField[Not[e_]] := QNot @ cField[e];
cField[SearchBoost[e_, boost_]] := QSearchBoost[cField[e], boost]
cField[e_] := cTerm[e];

cField[GreaterThan[value_]] := 			cRange[value, Null, False, False];
cField[LessThan[value_]] := 			cRange[Null, value, False, False];
cField[GreaterEqualThan[value_]] := 	cRange[value, Null, True, False];
cField[LessEqualThan[value_]] := 		cRange[Null, value, False, True];
cField[Between[{lower_, upper_}]] := 	cRange[lower, upper, True, True];

cRange[from_, to_, includeFrom_, includeTo_] := 
	QRange[$field, cRangeVal @ from, cRangeVal @ to, includeFrom, includeTo];

cRangeVal[Null] := Null;
cRangeVal[i_Integer] := IntegerString[i];
cRangeVal[s_String] := Block[{s2 = StringTrim @ ToLowerCase @ s}, 
	If[StringContainsQ[s2, Whitespace], 
		Message[TextSearch::invrang, s]; Throw[$Failed]];
	s2
];

cTerm[b:True|False] := QBoolean[$field, b];
cTerm[s_Symbol] := QString[$field, SymbolName[s]];
cTerm[i_Integer] := QString[$field, IntegerString[i]];
cTerm[s_String] := QString[$field, StringTrim @ s]
cTerm[SearchBoost[e_, boost_]] := QSearchBoost[cTerm[e], boost]
cTerm[t_] := (Message[TextSearch::invqe, t]; Throw[$Failed];);


ExecuteQuery[srcs_List, q_, "Count"] := Total @ execList[srcs, q, "Count"];

(* FIXME: this is only right for lists of docs, not e.g. assoc of lists *)
(* FIXME: this will discard the original result ordering *)
ExecuteQuery[srcs_List, q_, r:Except["SearchResultObject"]] := Union @@ execList[srcs, q, r]

ExecuteQuery[srcs_List, q_, r:"SearchResultObject"] := SearchResultObject@execList[srcs, q, r]
ExecuteQuery[{src_}, q_, r:"SearchResultObject"] := exec[src, q, r]

ExecuteQuery[{src_}, q_, r_] := exec[src, q, r];

execList[srcs_, q_, r_] := Map[exec[#, q, r]&, srcs];


exec[s_ ? (DSupportsQ[#,SymbolicQuery]&), q:Except[All|_LuceneQuery], type_] :=
	execQuery[s, SymbolicQuery[q], type];

exec[s_, q_QIntersection | q_QComplement | q_QUnion, "Count"] :=
	Length @ exec[s, q, "Location"]; 
	(* ^^^ this is bankrupt on non-file based entries *)

exec[s_, q_LuceneQuery, type_] := execQuery[s, q, type]

(* smart[Union] etc is here to deal with column-oriented results. It
looks whether the args are row or column oriented and does the 
transposed 'lift' of the op if necessary. *)

smart[f_][{e_}] := e;
smart[f_][{e__List}] := f[e];
smart[f_][args:{__Association}] := Block[{res},
	res = Apply[f] @ Map[AssociationTranspose] @ args;
	If[res =!= {}, 
		AssociationTranspose @ res,
		AssociationMap[{}&, Keys @ First @ args]
	]
];
smart[f_][l:{__SearchResultObject}] := SearchResultObject[Null, Null, smart[f][assocsToColumns[#[All][[All, 1]]]& /@ l]]

(* TODO: support "FileSize" -> GreaterThan[blah] but for files on disk,
not just indexed files. This is essentially the unix 'find' utility. *)

exec[s_, q_QIntersection | q_QComplement | q_QUnion, "Columns"] :=
	$Failed;

exec[s_, q_QIntersection, r_] := smart[Intersection] @ exec[s, List @@ q, r];
exec[s_, QComplement[q1_, q2_], r_] := smart[Complement] @ {exec[s, q1, r], exec[s, q2, r]};
exec[s_, q_QUnion, r_] := smart[Union] @ exec[s, List @@ q, r];

(* TODO: ^^ I'm ignoring that it is bankrupt to Union, Intersection, or 
Complement based on any expression that doesn't uniquely identify each 
document (e.g. Location, or the whole ContentObject). 

The solution is to have a nominal "ID" column that a backend can choose to support,
that would behave like any other column when getting. Then we'd need a single
SetSelected API call that you can pass in an MTensor of those integer IDs, which
would let us do the unions etc in Mathematica-land on the IDs, and then go back
later for the values.
Still, this doesn't apply with the new symbolic queries at all, so it is a bit
academic.. just the legacy backends would be affected, and they're probably
bitrotted anyway.
*)

exec[s_, q_List, r_] := Map[exec[s, #, r]&, q];

exec[s_, All, r_] := execQuery[s, All, r];
exec[s_, QString[col_, q_], r_] := execQuery[s, ColumnQuery[col, q], r];

(* Catch more advanced queries that SilverSearcher doesn't support. *)
exec[path_String, ___] := (Message[TextSearch::pucsi, field]; Throw[$Failed])

exec[___] := $Failed;

execQuery[path_String, All, res_] :=
	filesToResult[
		EnumerateFilesToIndex[path],
		res
	];

execQuery[IndexHandle[h_], All, res_] :=
	NIFallback[
		Switch[res,
			"ContentObject",
				DGetAllDocuments[h] // columnsToContentObjects,
			"Count",
				DGetDocumentCount[h],
			"Location",
				DGetIndexedFiles[h],
			"Columns",
				DGetAllDocuments[h],
			"Association",
				DGetAllDocuments[h] // columnsToAssocs,
			"SearchResultObject",
				DQueryReturnObject[h, All],
			_String,
				DGetColumn[h, res],
			_, 
				Missing["UnknownProperty", res]
		],
		filesToResult[
			DGetIndexedFiles[h],
			res
		]
	];


(* search on disk*)
execQuery[path_String, ColumnQuery["Plaintext"|All, str_], res_] :=
	filesToResult[
		IFileSearch[path, str, {True, True, Automatic, None}, "Files"],
		res
	];

(* Catch more advanced queries that SilverSearcher doesn't support. *)
execQuery[path_String, _, _] := (Message[TextSearch::pucsi, field]; Throw[$Failed])

execQuery[IndexHandle[h_], query_, res_] :=
	NIFallback[
		Switch[res,
			"ContentObject",
				ApplyContentLocationFunction[
					DQueryBulkReturn[h, query] // columnsToContentObjects,
					h,
					res
				],
			"Count",
				DQueryCount[h, query],
			"Location",
				DQuery[h, query, "Location"],
			"Columns",
				DQueryBulkReturn[h, query],
			"Association",
				ApplyContentLocationFunction[
					DQueryBulkReturn[h, query] // columnsToAssocs,
					h,
					res
				],
			"SearchResultObject",
				DQueryReturnObject[h, query],
			_String,
				DQuery[h, query, res], (* TODO: check the field worked! *)
			_, 
				Missing["UnknownProperty", res]
		],
		filesToResult[
			DQuery[h, query, "Location"],
			res
		]
	];

o_SearchResultObject[n_Integer, "Snippet"] := Catch[Snippet[o[n], o]]

o_SearchResultObject[span_Span, "Snippet"] := Catch[Snippet[o[#], o] & /@ Range[1, o["Count"]][[span]]]

o_SearchResultObject[All, "Snippet"] := Catch[Snippet[#, o] & /@ o[All]]

o_SearchResultObject[n_Integer, property_String] := o[n][property]

o_SearchResultObject[n_Integer, propertyList_List] := o[n][propertyList]

o_SearchResultObject[span_Span, property_String] := o[#][property] & /@ Range[1, o["Count"]][[span]]

o_SearchResultObject[span_Span, propertyList_List] := o[#][propertyList] & /@ Range[1, o["Count"]][[span]]

o_SearchResultObject[All, property_String] := o[All][[All, 1, property]]

o_SearchResultObject[All, propertyList_List] := #[propertyList] & /@ o[All]

SearchResultObject[args__][p:(All|_Span)] :=
	ApplyContentLocationFunction[
		DGetSearchResultObjectInternalProperty[SearchResultObject[args], p] // columnsToContentObjects,
		First[{args}],
		"ContentObject"
	]

SearchResultObject[args__][i_Integer] :=  SearchResultObject[args][i;;i] /. {f_} :> f

filesToResult[files_, res_] := 
	Switch[res,
		"ContentObject",
			files // filesToContentObjects,
		"Count",
			files // Length,
		"Location",
			files,
		"Association",
			files // filesToAssocs,
		"Columns",
			files // filesToAssocs // assocsToColumns,
		"SearchResultObject",
			SearchResultObject[files, Null, If[files === {}, {}, files // filesToAssocs // Block[{$FileFields = DeleteCases[$FileFields, "Plaintext"]}, assocsToColumns[#]] &]],
		_,
			Missing["UnknownProperty", res]
	];

filesToAssocs[files_] :=
	If[!Developer`StringVectorQ[files], $Failed,
		DeleteCases[$Failed] @ Map[ReadFileFields[#, False]&, files]];

filesToContentObjects[files_] :=
	If[!Developer`StringVectorQ[files], $Failed,
		ContentObject[File[#]]& /@ files];

assocsToColumns[assocs_] := Block[{keys, vals},
	If[!Developer`AssociationVectorQ[assocs], Return[$Failed]];
	Association[# -> Lookup[assocs, #]& /@ $FileFields]
];

columnsToAssocs[assoc_] := columnsTo[assoc, Identity];

columnsToContentObjects[assoc_] := columnsTo[assoc, ContentObject];

columnsTo[$NotImplemented, _] := $NotImplemented;
columnsTo[{}, _] := {}
columnsTo[Association[], f_] := {}
columnsTo[assoc_, f_] := Block[{keys, vals},
	If[!AssociationQ[assoc], Return[$Failed]];
	keys = Keys[assoc];
	vals = Values[assoc];
	Quiet @ Check[
		f[AssociationThread[keys, #]]& /@ Transpose[vals],
		$Failed
	]
];


PackageExport["TextSearch"]

SetArgumentCount[TextSearch, {2, 2}];

PackageScope[$DefaultOutput]

$DefaultOutput = "SearchResultObject"

TextSearch[HoldPattern[source_CloudObject], query_, type_:$DefaultOutput] := URLExecute[
	source,
	{"q" -> ToString[query, InputForm], "t" -> type}
]

TextSearch[sources_, query_] := 
	iTextSearch[sources, query, $DefaultOutput]

TextSearch[sources_, query_, type_String] := 
	iTextSearch[sources, query, type]
	
TextSearch::interr = "An internal error occurred while performing the search. Please contact Technical Support.";
TextSearch::interrcode = "An internal error occurred while performing the search. Please contact Technical Support. Error code: ``.";
TextSearch::cld = "Operation not yet supported in the Cloud";

(* fast path *)
iTextSearch[path_String ? FileExistsQ, query_String, result_] := Block[
	{abs = AbsoluteFileName @ path, res, $FailureExplained = False},
	ClearJavaException[];
	Catch[
		res = execQuery[
			promoteSearchIndexDirectories[abs],
			ColumnQuery["Plaintext", query], result
		];
		handleFailure[res];
		res
	]
];

iTextSearch[sources_, query_, result_] :=
	Block[{cquery, paths, res, $FailureExplained = False},
	ClearJavaException[];
	cquery = compileQuery[query];
	If[FailureQ[cquery], Return[$Failed]];
	oldIndex =
		Check[
			paths = NormalizeSources[sources, TextSearch],
			$oldIndexFlag,
			SearchIndexObject::versionexp
		];
	If [oldIndex === $oldIndexFlag,
		(* We already issued a message, so just return $Failed rather
		   than also issuing a second message. *)
		Return[$Failed];
	];
	If[Developer`EmptyQ[paths], 
		Message[TextSearch::nosrcs];
		Return[$Failed]];
	Catch[
		res = ExecuteQuery[paths, cquery, result];
		handleFailure[res];
		res
	]
]

(*!
	\function handleFailure
	
	\calltable
		handleFailure[res] '' if 'res' is FailureQ, then we will appropriately display a failure message unless that has already been done. If the failure was Java-related, we'll display an error code that is a compressed stack trace. Note that the caller must wrap its implementation in Block so that the Return returns from the CALLER'S Block.
	
	\maintainer danielb
*)
handleFailure[res_] :=
	If[FailureQ[res],
		If [!TrueQ[$FailureExplained],
			With[{errcode = GetJavaExceptionObscureSummary[]},
				If[StringQ[errcode],
					Message[TextSearch::interrcode, errcode]
					,
					Message[TextSearch::interr]
				]
			];
		];
		Return[$Failed, Block]
	];

toContentObject[str_String] := ContentObject[File[str]];
toContentObject[_] := $Failed;

PackageExport["$LastError"]


PackageExport["TextSearchReport"]

SetArgumentCount[TextSearchReport, {2, 2}]

TextSearchReport[sources_, query_] := ConditionalRHS[True, {}, iTextSearchReport[sources, query]];

iTextSearchReport[sources_, query_] := Module[{res},
	res = iTextSearch[sources, query, "Association"];
	If[!Developer`AssociationVectorQ[res], Return[$Failed]];
	If[res === {}, Return[Dataset[{}]]];
    Dataset @ Map[
		KeySortBy[#, Position[keyOrder, #]&]&,
    	res
	]
];

PackageScope["keyOrder"]

keyOrder = {"FileName", "ModificationDate", "CreationDate", "Snippet", "FileByteCount", "FileExtension", "Location", "Score"};

(*!
	\function validateSearchAdjustmentOptions
	
	\calltable
		validateSearchAdjustmentOptions[options] '' validates the options of SearchAdjustment.

	Examples:
    
    validateSearchAdjustmentOptions[{MaxWordGap -> 1}] === Null

    Unit tests: validateSearchAdjustmentOptions.mt

    \maintainer danielb
*)
$searchAdjustmentOptionSchema =
<|
	MaxWordGap -> _Integer | None
|>;
Options[SearchAdjustment] = {
	MaxWordGap -> 0							(*< Number of words that can occur between terms of a phrase. *)
};
validateSearchAdjustmentOptions[options_] :=
	ValidateOptions[
		options,
		$searchAdjustmentOptionSchema,
		Function[{optionName},
			Message[TextSearch::invsao, optionName];
			Throw[$Failed]
		],
		Function[{optionName, optionValue},
			Message[TextSearch::invsav, optionValue, optionName];
			Throw[$Failed]
		]
	]

(*!
	\function ApplyContentLocationFunction
	
	\calltable
		ApplyContentLocationFunction[objects, handle, type] '' applies the ContentLocationFunction to file locations.
	
	\related 'applyContentLocationFunctionHelper
	
	\maintainer danielb
*)
ApplyContentLocationFunction[objects_, handle_, type_] :=
	Block[{func, res},
		func = IndexMetadata[handle]["ContentLocationFunction"];
		Switch[
			func,
			_Function,
			applyContentLocationFunctionHelper[objects, func, "ReferenceLocation", type]
			,
			_Association,
			res = applyContentLocationFunctionHelper[objects, func["ReferenceLocation"], "ReferenceLocation", type];
			res = applyContentLocationFunctionHelper[res, func["SnippetSource"], "SnippetSource", type]
			,
			_,
			objects
		]
	];

(*!
	\function applyContentLocationFunctionHelper
	
	\calltable
		applyContentLocationFunctionHelper[objects, func, key, type] '' applies the ContentLocationFunction to file locations.

	Examples:
    
    applyContentLocationFunctionHelper[
        {Association["Location" -> "/tmp/apple"], Association["Location" -> "/tmp/orange"]},
        "Wrapper"[#1] & ,
        "Association"
    ]

    ===

    {<|"Location" -> "Wrapper"["/tmp/apple"]|>, <|"Location" -> "Wrapper"["/tmp/orange"]|>}

    Unit tests: applyContentLocationFunctionHelper.mt

    \maintainer danielb
*)
Clear[applyContentLocationFunctionHelper];
applyContentLocationFunctionHelper[objects_, func_Function, key_, "ContentObject"] :=
	Block[{},
		ContentObject /@ applyContentLocationFunctionHelper[objects[[All, 1]], func, key, "Association"]
	]
	
applyContentLocationFunctionHelper[objects_, func_Function, key_, "Association"] :=
	Block[{},
		Replace[
			objects,
			a:KeyValuePattern[{"Location" -> val_}] :>
				With[{tmp =
					Module[{tmp2 = a},
						tmp2[key] = func[a];
						tmp2
					]},
					tmp /; True
				],
			{0, Infinity}
		]
	]

applyContentLocationFunctionHelper[objects_, _Missing, key_, _] := objects

(* Normal[SearchResultObject[...]] converts it to a list of ContentObjects *)
Unprotect[Normal];
Normal[s_SearchResultObject] := s[All]
Protect[Normal];

(* Dataset[SearchResultObject[...]] creates a Dataset from the ContentObject associations *)
SearchResultObject /: Dataset[s_SearchResultObject] := With[{assocs = s[All][[All, 1]]}, Dataset[assocs]]

s_SearchResultObject["Dataset"] := With[{assocs = s[All][[All, 1]]}, Dataset[assocs]]
