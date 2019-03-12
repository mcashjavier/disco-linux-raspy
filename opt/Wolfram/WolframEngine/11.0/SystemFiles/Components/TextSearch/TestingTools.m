Package["TextSearch`"]

PackageImport["GeneralUtilities`"]

PackageScope["RunTrainingSet"]

PackageScope["RunQuerySet"]

PackageScope["DiffQuerySetRuns"]

PackageScope["SummarizeQuerySetRunDiff"]

PackageScope["GetQuerySet"]

PackageScope["SaveQuerySet"]

PackageScope["SetQueryRelevance"]

PackageScope["GetQueryRelevance"]

PackageScope["QuerySets"]

PackageScope["QuerySet"]

PackageScope["SaveQuerySetRun"]

PackageScope["GetPreviousQuerySetRun"]

PackageScope["QuerySetSavedResults"]

PackageScope["WithTemporaryFiles"]

PackageScope["WithTemporaryDirectories"]

PackageScope["CreateSearchIndexDeleteIfExists"]

PackageScope["CreateTestSearchIndex"]

PackageScope["RunTestFile"]

PackageScope["RunTestFiles"]

PackageScope["PrintTestResult"]

PackageScope["TestResultToJUnitXml"]

PackageScope["TestResultsToJUnitXml"]

PackageScope["TestResultToString"]

PackageScope["RunTests"]

(*!
	\function trainingSetPath
	
	\calltable
		trainingSetPath[name] '' given a training name, returns its path.

	Examples:
    
    trainingSetPath["Britannica"] === "E:\\Users\\Daniel\\git\\TextSearch\\Tests\\QuerySets\\Britannica\\TrainingSet.m"
	
	\maintainer danielb
*)
$tsDir := $tsDir = FileNameDrop[FindFile["TextSearch`"], -2];
$testDir := $testDir = FileNameJoin[{$tsDir, "Tests"}];
$querySetDir := $querySetDir = FileNameJoin[{$testDir, "QuerySets"}];
trainingSetPath[name_] := FileNameJoin[{$querySetDir, name, "TrainingSet", "TrainingSet.m"}]

(*!
	\function getTrainingSet
	
	\calltable
		getTrainingSet[name] '' given a training set's name, reads it from disk.

	Examples:
    
    getTrainingSet["Britannica"]
    
    ===
    
	Association[
	    "Name" -> "Britannica",
	    "Queries" ->
	   	{
	        Association[
	            "Query" -> "fruit" | "temperate climates",
	            "SearchResults" -> {<|"FileName" -> "APPLE", "Relevance" -> 1|>}
	        ]
	    }
	]
	
	\maintainer danielb
*)
getTrainingSet[name_] := Get[trainingSetPath[name]]

(*!
	\function RunTrainingSet
	
	\calltable
		RunTrainingSet[name] '' runs the training set of the given name, and returns the results of the run. Note that this looks for an index with the same name.

	See 'RunQuerySet for further details.

	Examples:
    
    RunTrainingSet["Britannica"]
    
    \related 'RunQuerySet
	
	\maintainer danielb
*)
RunTrainingSet::noi = "Couldn't find index with name `1`";
RunTrainingSet[name_] :=
	Scope[
		
		trainingSet = Get[trainingSetPath[name]] !;
		
	    RunQuerySet[
	    	trainingSet
	    ]
	];

(*!
	\function rankDilution
	
	\calltable
		rankDilution[rank] '' given a document's rank in search result, what proportion of its value remains VS if it were in first spot?

	- A rank dilution of 1 means there hasn't been any dilution.
	  ie. That an item is contributing as much value to the search
	  results as it possibly can.
	- As a search item's rank drops to #2 spot, it becomes slightly
	  less likely that a person will see it and consider it, and
	  so its dilution value becomes lower. (0.7)
	- Likewise, as a search item's rank drops lower and lower,
	  the likelihood that it will be seen and consider drops off.
	
	Todo: We might want to introduce a discontinuity after 10,
	or whatever size one page of search results is.

	Examples:
    
    rankDilution[1] === 1.
    
    rankDilution[2] === 0.7.
    
    rankDilution[3] === 0.49.
    
    ...
    
    rankDilution[10] === 0.04

    Unit tests: rankDilution.mt

    \maintainer danielb
*)
rankDilution[rank_] := 0.7^(rank - 1)
rankDilution[None] = 0.

(*!
	\function RunQuerySet
	
	\calltable
		RunQuerySet[querySet] '' given a set of queries with relevance judgements, run the query set and return the results.
		RunQuerySet[querySetName, querySetType] '' ...

	- For each search result item in the training set:
	  - Determinese its current search rank
	  - Computes its RankDilution (see function rankDilution for more information)
	  - Computes its SearchResultValue (RankDilution * Relevance)
	- Computes a SearchResultValue for each query
	  (the sum of the SearchResultValue for each of its search results)
	- Computes SearchResultValue for the entire test set.
	  This can be used at a glance to see whether a change to the
	  query system has improved the performance of the training
	  set or not.

	Examples:
    
    RunQuerySet[
    	Get[trainingSetPath["Britannica"]]
    ]
    
    ===
    
	Association[
	    "SearchResultValueTotal" -> 0.16806999999999994,
	    "Queries" ->
	    {
	        Association[
	            "Query" -> "fruit" | "temperate climates",
	            "SearchResults" ->
	            {
	                Association[
	                    "FileName" -> "APPLE",
	                    "Relevance" -> 1,
	                    "ResultRank" -> 6,
	                    "SearchResultValue" -> 0.16806999999999994,
	                    "RankDilution" -> 0.16806999999999994
	                ],
	                ...
	            },
	            "SearchResultValueTotal" -> 0.16806999999999994
	        ],
	        ...
	    }
	]
	
	\related 'RunTrainingSet
	
	\maintainer danielb
*)
Clear[RunQuerySet];
Options[RunQuerySet] =
{
    "IndexOverride" -> None			(*< For example, I just created a "BritannicaBM25" index using BM25Similarity, and I'd like to run it against the Britannica query set. *)
};
RunQuerySet[querySet_, OptionsPattern[]] :=
	Scope[
		
		indexName = querySet["Name"];
		Which [
			OptionValue["IndexOverride"] =!= None,
			indexName = OptionValue["IndexOverride"];
			,
			StringQ[TextSearch`Private`$IndexNameOverride],
			indexName = TextSearch`Private`$IndexNameOverride;
		];
		
		index = SearchIndexObject[indexName];
		If [index === $Failed, Return[$Failed]];
		
		searchResultValueTotal = 0;
		
		queries =
		Function[{item},
			
			itemCopy = item;
			
			resItems = TextSearch[index, item["Query"]];
			
			targetResults = item["SearchResults"];
			
			searchResultValueTotalForQuery = 0;
			
			itemCopy["SearchResults"] =
			Function[{targetResult},
				targetResultIndex = Missing[];
				i = 0;
				Function[{resItem},
					++i;
					If [resItem[[1, "FileName"]] === targetResult["FileName"],
						targetResultIndex = i;
						Return[];
					]
				] /@ resItems;
				targetResultCopy = targetResult;
				targetResultCopy["ResultRank"] = targetResultIndex;
				targetResultCopy["SearchResultValue"] = targetResult["Relevance"] * rankDilution[targetResultIndex];
				searchResultValueTotal += targetResultCopy["SearchResultValue"];
				searchResultValueTotalForQuery += targetResultCopy["SearchResultValue"];
				targetResultCopy["RankDilution"] = rankDilution[targetResultIndex];
				targetResultCopy
			] /@ targetResults;
			
			itemCopy["SearchResultValueTotal"] = searchResultValueTotalForQuery;
			
			itemCopy
			
		] /@ querySet["Queries"];
		
		<|
			"QuerySetName" -> querySet["Name"],
			"QuerySetType" -> querySet["Type"],
			"Timestamp" -> Date[],
			"SearchResultValueTotal" -> searchResultValueTotal,
			"Queries" -> queries
		|>
	];

(*!
	\function DiffQuerySetRuns
	
	\calltable
		DiffQuerySetRuns[run1, run2] '' given two outputs from RunQuerySet, produce a diff.

	Examples:

    DiffQuerySetRuns[
        Association[
            "SearchResultValueTotal" -> 0.16806999999999994,
            "Queries" ->
            {
                Association[
                    "Query" -> "fruit" | "temperate climates",
                    "SearchResults" ->
                    {
                        Association[
                            "FileName" -> "APPLE",
                            "Relevance" -> 1,
                            "ResultRank" -> 6,
                            "SearchResultValue" -> 0.16806999999999994,
                            "RankDilution" -> 0.16806999999999994
                        ]
                    },
                    "SearchResultValueTotal" -> 0.16806999999999994
                ],
                Association[
                    "Query" -> "dogs",
                    "SearchResults" -> {},
                    "SearchResultValueTotal" -> 0.16806999999999994
                ]
            }
        ],
        Association[
            "SearchResultValueTotal" -> 1,
            "Queries" ->
            {
                Association[
                    "Query" -> "fruit" | "temperate climates",
                    "SearchResults" ->
                    {
                        Association[
                            "FileName" -> "APPLE",
                            "Relevance" -> 1,
                            "ResultRank" -> 1,
                            "SearchResultValue" -> 1,
                            "RankDilution" -> 1
                        ],
                        Association[
                            "FileName" -> "PEAR",
                            "Relevance" -> 1,
                            "ResultRank" -> 1,
                            "SearchResultValue" -> 1,
                            "RankDilution" -> 1
                        ]
                    },
                    "SearchResultValueTotal" -> 1
                ]
            }
        ]
    ]

    ===

    QuerySetRunDiff[
        Association[
            "SearchResultValueTotal" -> Diff[0.16806999999999994, 1],
            "Queries" ->
            {
                Association[
                    "Query" -> "fruit" | "temperate climates",
                    "SearchResults" ->
                    {
                        Association[
                            "FileName" -> "APPLE",
                            "Relevance" -> 1,
                            "ResultRank" -> Diff[6, 1],
                            "SearchResultValue" -> Diff[0.16806999999999994, 1],
                            "RankDilution" -> Diff[0.16806999999999994, 1]
                        ]
                    },
                    "SearchResultValueTotal" -> Diff[0.16806999999999994, 1]
                ]
            }
        ]
    ]

    Unit tests: DiffQuerySetRuns.mt

    \maintainer danielb
*)
Options[DiffQuerySetRuns] =
{
    "RemoveMissingItems" -> True				(*< By default, we remove any queries or search results in one set but not the other, since that would create an unhelpful comparison if we want to see whether a change has helped or hindered. *)
};
DiffQuerySetRuns[run1_, run2_, OptionsPattern[]] :=
	Scope[
		(* Union but with order preserved. *)
		keyUnion = DeleteDuplicates[Join[Keys[run1], Keys[run2]]];
		
		res =
		QuerySetRunDiff[
			Association @@
			Function[{key},
				If [key === "Queries",
					"Queries" ->
						DiffListOfAssociations[run1[key], run2[key], "Query"]
					,
					key ->
						DiffIfNecessary[run1[key], run2[key]]
				]
			] /@ keyUnion
		];
		
		If [TrueQ[OptionValue["RemoveMissingItems"]],
			res = ReplaceValuesOfKey[res, "Queries", DeleteCases[#, _Diff] &];
			res = ReplaceValuesOfKey[res, "SearchResults", DeleteCases[#, _Diff] &];
			res = updateSearchResultValueTotals[res];
		];
		
		res
	];

(*!
	\function updateSearchResultValueTotals
	
	\calltable
		updateSearchResultValueTotals[diff] '' given a QuerySetRunDiff that has had some internal items removed, re-compute the SearchResultValueTotal value.

	Examples:
    
    updateSearchResultValueTotals[
        QuerySetRunDiff[
            Association[
                "SearchResultValueTotal" -> Diff[7, 9],
                "Queries" ->
                {
                    Association[
                        "Query" -> "fruit" | "temperate climates",
                        "SearchResults" ->
                        {
                            Association[
                                "FileName" -> "APPLE",
                                "Relevance" -> 1,
                                "ResultRank" -> Diff[6, 1],
                                "SearchResultValue" -> Diff[0.16806999999999994, 1],
                                "RankDilution" -> Diff[0.16806999999999994, 1]
                            ],
                            Association[
                                "FileName" -> "PEAR",
                                "Relevance" -> 1,
                                "ResultRank" -> 1,
                                "SearchResultValue" -> 1,
                                "RankDilution" -> 1
                            ]
                        },
                        "SearchResultValueTotal" -> Diff[7, 9]
                    ]
                }
            ]
        ]
    ]

    ===

    QuerySetRunDiff[
        Association[
            "SearchResultValueTotal" -> Diff[1.16807, 2],
            "Queries" ->
            {
                Association[
                    "Query" -> "fruit" | "temperate climates",
                    "SearchResults" ->
                    {
                        Association[
                            "FileName" -> "APPLE",
                            "Relevance" -> 1,
                            "ResultRank" -> Diff[6, 1],
                            "SearchResultValue" -> Diff[0.16806999999999994, 1],
                            "RankDilution" -> Diff[0.16806999999999994, 1]
                        ],
                        Association[
                            "FileName" -> "PEAR",
                            "Relevance" -> 1,
                            "ResultRank" -> 1,
                            "SearchResultValue" -> 1,
                            "RankDilution" -> 1
                        ]
                    },
                    "SearchResultValueTotal" -> Diff[1.16807, 2]
                ]
            }
        ]
    ]

    Unit tests: updateSearchResultValueTotals.mt

    \maintainer danielb
*)
updateSearchResultValueTotals[diff_] :=
	Scope[
		diffCopy = diff;
		diffCopy[[1, "Queries"]] =
			Function[{query},
				values =
					Function[{searchResult},
						searchResult["SearchResultValue"] /. Diff[a_, b_] :> {a, b}
					] /@ query["SearchResults"];
				queryCopy = query;
				diffValues = Cases[values, _List];
				sameValues = Cases[values, _Real | _Integer];
				With[{sameValuesSum = Plus@@sameValues},
					queryCopy["SearchResultValueTotal"] = DiffIfNecessary[sameValuesSum + Plus @@ diffValues[[All, 1]], sameValuesSum + Plus @@ diffValues[[All, 2]]];
				];
				queryCopy
			] /@ diff[[1, "Queries"]];
		
		values = diffCopy[[1, "Queries", All, "SearchResultValueTotal"]];
		diffValues = Cases[values, _Diff];
		sameValues = Cases[values, _Real | _Integer];
		With[{sameValuesSum = Plus@@sameValues},
			diffCopy[[1, "SearchResultValueTotal"]] = DiffIfNecessary[sameValuesSum + Plus @@ diffValues[[All, 1]], sameValuesSum + Plus @@ diffValues[[All, 2]]];
		];
			
		diffCopy
	];

(*!
	\function SummarizeQuerySetRunDiff
	
	\calltable
		SummarizeQuerySetRunDiff[diff] '' given the output from DiffQuerySetRuns, summarize the differences.

	Examples:
    
    SummarizeQuerySetRunDiff[
        (
            ReloadTextSearch[];
            DiffQuerySetRuns[
                Association[
                    "SearchResultValueTotal" -> 0.16806999999999994,
                    "Queries" ->
                    {
                        Association[
                            "Query" -> "fruit" | "temperate climates",
                            "SearchResults" ->
                            {
                                Association[
                                    "FileName" -> "APPLE",
                                    "Relevance" -> 1,
                                    "ResultRank" -> 6,
                                    "SearchResultValue" -> 0.16806999999999994,
                                    "RankDilution" -> 0.16806999999999994
                                ]
                            },
                            "SearchResultValueTotal" -> 0.16806999999999994
                        ]
                    }
                ],
                Association[
                    "SearchResultValueTotal" -> 1,
                    "Queries" ->
                    {
                        Association[
                            "Query" -> "fruit" | "temperate climates",
                            "SearchResults" ->
                            {
                                Association[
                                    "FileName" -> "APPLE",
                                    "Relevance" -> 1,
                                    "ResultRank" -> 1,
                                    "SearchResultValue" -> 1,
                                    "RankDilution" -> 1
                                ],
                                Association[
                                    "FileName" -> "PEAR",
                                    "Relevance" -> 1,
                                    "ResultRank" -> 1,
                                    "SearchResultValue" -> 1,
                                    "RankDilution" -> 1
                                ]
                            },
                            "SearchResultValueTotal" -> 1
                        ]
                    }
                ]
            ]
        )
    ]

    ===

    Association[
        "SearchResultValueTotal" -> Diff[0.16806999999999994, 1],
        "Improvements" ->
        {
            Association[
                "Query" -> "fruit" | "temperate climates",
                "SearchResultValueTotal" -> Diff[0.16806999999999994, 1],
                "Improvements" ->
                {
                    Association[
                        "FileName" -> "APPLE",
                        "Relevance" -> 1,
                        "ResultRank" -> Diff[6, 1],
                        "SearchResultValue" -> Diff[0.16806999999999994, 1],
                        "RankDilution" -> Diff[0.16806999999999994, 1]
                    ]
                },
                "Regressions" -> {}
            ]
        },
        "Regressions" -> {}
    ]

    Unit tests: SummarizeQuerySetRunDiff.mt

    \maintainer danielb
*)
SummarizeQuerySetRunDiff[diff_] :=
	Scope[
		
		assoc = diff[[1]];
		
		queries = assoc[["Queries"]];
		
		improvedQueries = Select[queries, With[{value = #["SearchResultValueTotal"]}, MatchQ[value, _Diff] && value[[2]] > value[[1]]] &];
		improvedQueries = summarizeSearchResultsDiff /@ improvedQueries;
		
		regressedQueries = Select[queries, With[{value = #["SearchResultValueTotal"]}, MatchQ[value, _Diff] && value[[2]] < value[[1]]] &];
		regressedQueries = summarizeSearchResultsDiff /@ regressedQueries;
		
		assoc = Delete[assoc, "Queries"];
		
		assoc["Improvements"] = improvedQueries;
		assoc["Regressions"] = regressedQueries;
		
		QuerySetRunDiffSummary[
			assoc
		]
	];

(*!
	\function summarizeSearchResultsDiff
	
	\calltable
		summarizeSearchResultsDiff[query] '' given a query, summarize the improvements and regressions to individual search results.

	Examples:
    
    summarizeSearchResultsDiff[
        Association[
            "Query" -> "fruit" | "temperate climates",
            "SearchResults" ->
            {
                Association[
                    "FileName" -> "APPLE",
                    "Relevance" -> 1,
                    "ResultRank" -> Diff[6, 1],
                    "SearchResultValue" -> Diff[0.16806999999999994, 1],
                    "RankDilution" -> Diff[0.16806999999999994, 1]
                ]
            },
            "SearchResultValueTotal" -> Diff[0.16806999999999994, 1]
        ]
    ]

    ===

    Association[
        "Query" -> "fruit" | "temperate climates",
        "SearchResultValueTotal" -> Diff[0.16806999999999994, 1],
        "Improvements" ->
        {
            Association[
                "FileName" -> "APPLE",
                "Relevance" -> 1,
                "ResultRank" -> Diff[6, 1],
                "SearchResultValue" -> Diff[0.16806999999999994, 1],
                "RankDilution" -> Diff[0.16806999999999994, 1]
            ]
        },
        "Regressions" -> {}
    ]

    Unit tests: summarizeSearchResultsDiff.mt

    \maintainer danielb
*)
summarizeSearchResultsDiff[searchResultDiffs_] :=
	Scope[
		
		assoc = searchResultDiffs;
		
		searchResults = assoc["SearchResults"];
		
		improvements = Select[searchResults, With[{value = #["SearchResultValue"]}, MatchQ[value, _Diff] && value[[2]] > value[[1]]] &];
		regressions = Select[searchResults, With[{value = #["SearchResultValue"]}, MatchQ[value, _Diff] && value[[2]] < value[[1]]] &];
		
		(* Sort the improvements and regressions so that the largest changes come first. *)
		improvements = SortBy[improvements, -(#[["SearchResultValue", 2]] - #[["SearchResultValue", 1]]) &];
		regressions = SortBy[regressions, #[["SearchResultValue", 2]] - #[["SearchResultValue", 1]] &];
		
		assoc = Delete[assoc, "SearchResults"];
		
		assoc["Improvements"] = improvements;
		assoc["Regressions"] = regressions;
		
		assoc
	];

(* Formatting *)
Format[diff_QuerySetRunDiffSummary] :=
	Scope[
		
		searchResultValueTotal = diff[[1, "SearchResultValueTotal"]];
		
		improvements = diff[[1, "Improvements"]];
		regressions = diff[[1, "Regressions"]];	
		
		Style[
			Column[
				{
					Column[
						{
							Style["Summary", FontSize -> 14, FontWeight -> Bold],
							Framed[#, FrameStyle -> None, FrameMargins -> {{20, 0}, {0, 10}}] & @
							If [!MatchQ[searchResultValueTotal, _Diff],
								"No overall change to search score."
								,
								FormatNumericalDiff[searchResultValueTotal]
							]
						}
					],
					formatImprovedOrRegressedQueries[improvements, "Improvements"],
					formatImprovedOrRegressedQueries[regressions, "Regressions"]
				}
			],
			FontFamily -> "Arial"
		]
	]

(*!
	\function formatImprovedOrRegressedQueries
	
	\calltable
		formatImprovedOrRegressedQueries[queries, type] '' given either the 'Improvements' or 'Regressions' list (queries) from a QuerySetRunDiffSummary, formats the output.

	type: Either "Improvements" or "Regressions"

	Examples:
    
    formatImprovedOrRegressedQueries[
        {
            Association[
                "Query" -> "fruit" | "temperate climates",
                "SearchResultValueTotal" -> Diff[0.8680699999999999, 1.49],
                "Improvements" ->
                {
                    Association[
                        "FileName" -> "APPLE",
                        "Relevance" -> 1,
                        "ResultRank" -> Diff[6, 1],
                        "SearchResultValue" -> Diff[0.16806999999999994, 1],
                        "RankDilution" -> Diff[0.16806999999999994, 1]
                    ]
                },
                "Regressions" ->
                {
                    Association[
                        "FileName" -> "PEAR",
                        "Relevance" -> 1,
                        "ResultRank" -> Diff[2, 3],
                        "SearchResultValue" -> Diff[0.7, 0.49],
                        "RankDilution" -> Diff[0.7, 0.49]
                    ]
                }
            ]
        },
        "Improvements"
    ]

    Unit tests: formatImprovedOrRegressedQueries.mt

    \maintainer danielb
*)
formatImprovedOrRegressedQueries[queries_, type_] :=
	Scope[
		
		useOpenerView = True;
		
		maxQueriesToShow = 5;
		
		If [ListQ[queries] && Length[queries] > 0,
			
			Column[{
				Framed[#, FrameStyle -> None, FrameMargins -> {{0, 0}, {0, 10}}] & @
				Style[If [type === "Improvements", "Improved", "Regressed"] <> " Queries", FontSize -> 14, FontWeight -> Bold],
				Framed[#, FrameStyle -> None, FrameMargins -> {{10, 0}, {0, 10}}] & @
				Column[
					Function[{query},
						If [useOpenerView, OpenerView, Column][{
							Framed[#, FrameStyle -> None, FrameMargins -> {{10, 0}, {0, 0}}] & @
							Column[{
								FormatInfoBoxRow["Query", InputForm[query["Query"]]],
								FormatInfoBoxRow[
									"Overall",
									FormatNumericalDiff[query["SearchResultValueTotal"]]
								]
							}],
							Column[{
								formatImprovedOrRegressedResults[query["Improvements"]],
								formatImprovedOrRegressedResults[query["Regressions"]]
							}]
							}]
						
					] /@ Take[queries, Min[Length[queries], maxQueriesToShow]]
				],
				If [Length[queries] > maxQueriesToShow,
					(* TODO: Have a way to show more? *)
					Framed[#, FrameStyle -> None, FrameMargins -> {{20, 0}, {0, 10}}] & @
						"Showing " <> ToString[maxQueriesToShow] <> " of " <> Length[queries] <> "queries."
					,
					Sequence @@ {}
				]
			}]
			,
			Sequence @@ {}
		]
	];
	
formatImprovedOrRegressedQueries[_String] := Sequence @@ {}

(*!
	\function formatImprovedOrRegressedResults
	
	\calltable
		formatImprovedOrRegressedResults[searchResultDiff] '' given the individual search results that either improved or regressed, format them.

	Examples:
    
    formatImprovedOrRegressedResults[
        {
            Association[
                "FileName" -> "APPLE",
                "Relevance" -> 1,
                "ResultRank" -> Diff[6, 1],
                "SearchResultValue" -> Diff[0.16806999999999994, 1],
                "RankDilution" -> Diff[0.16806999999999994, 1]
            ]
        }
    ]

    Unit tests: formatImprovedOrRegressedResults.mt

    \maintainer danielb
*)
formatImprovedOrRegressedResults[searchResultDiff_] :=
	Block[{},
		If [searchResultDiff === {},
			Sequence @@ {}
			,
			Column[
				Function[{searchResult},
	
					Framed[#, FrameStyle -> None, FrameMargins -> {{20, 0}, {0, 10}}] & @
					Column[{
						TitleOrFileName[searchResult],
						FormatInfoBoxRow["Rank", With[{rank = searchResult["ResultRank"]}, ToString[rank[[1]]] <> " -> " <> ToString[rank[[2]]]]],
						FormatInfoBoxRow["Relevance", searchResult["Relevance"]],
						FormatInfoBoxRow["Value", FormatNumericalDiff[searchResult["SearchResultValue"]]]
					}]
					
				] /@ searchResultDiff
			]
		]
	];
	
formatImprovedOrRegressedResults[] := Sequence @@ {}

(*!
	\function GetQuerySet
	
	\calltable
		GetQuerySet[querySetName, type] '' given a query set's name, returns the query set. Uses caching to avoid repeated disk access.

	Examples:
    
    GetQuerySet["Britannica", "TrainingSet"]

    ===

    Association[
        "Name" -> "Britannica",
        "Queries" ->
        {
            Association[
                "Query" -> "fruit" | "temperate climates",
                "SearchResults" -> {<|"FileName" -> "APPLE", "Relevance" -> 1|>}
            ],
            ...
        }
    ]

    Unit tests: GetQuerySet.mt

    \maintainer danielb
*)
querySetCache[querySetName_, type_] := Null;
GetQuerySet[querySetName_, type_] :=
	Block[{path, res},
		path = querySetPath[querySetName, "TrainingSet"];
		If [!FileExistsQ[path], Message[GetQuerySet::noopen, path]; Return[$Failed]];
		
		res = querySetCache[querySetName, type];
		If [res === Null,
			querySetCache[querySetName, type] = res = Get[path];
		];
		
		res
	];

(*!
	\function querySetPath
	
	\calltable
		querySetPath[querySetName, type] '' given the name of a query set, and its type (ex. TrainingSet, etc.) returns its path.

	Examples:
    
    querySetPath["Britannica", "TrainingSet"] === "E:\\Users\\Daniel\\git\\TextSearch\\Tests\\QuerySets\\Britannica\\TrainingSet.m"
	
	\related 'GetQuerySet 'SaveQuerySet
	
	\maintainer danielb
*)
querySetPath[querySetName_, type_] := FileNameJoin[{$querySetDir, querySetName, type, type <> ".m"}]

(*!
	\function SaveQuerySet
	
	\calltable
		SaveQuerySet[querySetName, type] '' given a query set name and type, saves the current in-memory copy to disk. If the query set has yet to be loaded, nothing is done.

	Examples:
    
    SaveQuerySet["Britannica", "TrainingSet"]
	
	\related 'GetQuerySet
	
	\maintainer danielb
*)
SaveQuerySet[querySetName_, type_] :=
	Block[{path, data},
		path = querySetPath[querySetName, "TrainingSet"];
		If [!FileExistsQ[path], Message[GetQuerySet::noopen, path]; Return[$Failed]];
		
		data = querySetCache[querySetName, type];
		
		If [data =!= Null,
			Export[
				path,
				tabsToSpace @
				Indent2[
					data,
					"AlwaysIndentToLevel" -> 3,
					"RuleLeftHandSidesNotToIndent" -> {"Query", "Name"}
				],
				"Text"
			];
		];
	];
	
tabsToSpace[str_String] := StringReplace[str, StartOfLine ~~ w:(WhitespaceCharacter..) :> If [Mod[StringLength[w], 4] === 0, StringJoin[Table["\t", {StringLength[w] / 4}]], w]]

(*!
	\function SetQueryRelevance
	
	\calltable
		SetQueryRelevance[querySetName, querySetType, query, docFileName, relevance] '' sets the relevance for a particular document as it pertains to a particular query in a query set.

	Examples:
    
    SetQueryRelevance[querySet, querySetType, query, docFileName, relevance] === TODO
	
	\related 'GetQuerySet 'SaveQuerySet
	
	\maintainer danielb
*)
SetQueryRelevance::clqs = "Couldn't load query set `1`.";
SetQueryRelevance[querySetName_, querySetType_, query_, docFileName_, relevance_] :=
	Scope[
		querySet = GetQuerySet[querySetName, querySetType] ! ReturnFailed["clqs", querySetName];
		
		querySet = setQueryRelevanceHelper[querySet, query, docFileName, relevance];
		
		querySetCache[querySetName, querySetType] = querySet;
		
		SaveQuerySet[querySetName, querySetType];
	];

(*!
	\function setQueryRelevanceHelper
	
	\calltable
		setQueryRelevanceHelper[querySet, query, docFileName, relevance] '' Helper function for SetQueryRelevance. Update 'querySet' with the given relevance for a particular query/file.

	Examples:
    
    setQueryRelevanceHelper[
        Association[
            "Name" -> "Britannica",
            "Queries" ->
            {
                Association[
                    "Query" -> "fruit" | "temperate climates",
                    "SearchResults" -> {Association["FileName" -> "APPLE", "Relevance" -> 1]}
                ]
            }
        ],
        "fruit" | "temperate climates",
        "APPLE",
        0.5
    ]

    ===

    Association[
        "Name" -> "Britannica",
        "Queries" ->
        {
            Association[
                "Query" -> "fruit" | "temperate climates",
                "SearchResults" -> {<|"FileName" -> "APPLE", "Relevance" -> 0.5|>}
            ]
        }
    ]

    Unit tests: setQueryRelevanceHelper.mt

    \maintainer danielb
*)
setQueryRelevanceHelper[querySet_, query_, docFileName_, relevance_] :=
	Scope[
		queries = Lookup[querySet, "Queries", {}];
		matchingQueries = Select[queries, #["Query"] === query &];
		If [Length[matchingQueries] > 0,
			(* Found the query. *)
			queryCopy = matchingQueries[[1]];
			searchResults = queryCopy["SearchResults"];
			matchingFiles = Select[searchResults, #["FileName"] === docFileName &];
			If [Length[matchingFiles] > 0,
				(* Found the document file record. *)
				docFile = matchingFiles[[1]];
				docFile["Relevance"] = relevance;
				queryCopy["SearchResults"] =
                    Replace[
                        queryCopy["SearchResults"],
                        Verbatim[matchingFiles[[1]]] :>
                        	If [relevance =!= Null,
                            	docFile
                            	,
                            	Sequence @@ {}
                        	],
                        1
                    ];
				,
				If [relevance =!= Null,
					(* The document file record doesn't exist yet. Create it. *)
					docFile = <|"FileName" -> docFileName, "Relevance" -> relevance|>;
					queryCopy["SearchResults"] = Append[queryCopy["SearchResults"], docFile];
				];
			];
			(* Need to use Verbatim because inside of the LHS the "Query" -> ...
			   can contain pattern elements. *)
			queries = Replace[queries, Verbatim[matchingQueries[[1]]] :> queryCopy, 1];
			,
			(* The query doesn't exist in the query set yet. Add it. *)
			If [relevance =!= Null,
				queries =
				Append[
					queries,
					<|
						"Query" -> query,
						"SearchResults" ->
						{
							<|"FileName" -> docFileName, "Relevance" -> relevance|>
						}
					|>
				];
			];
		];
		
		querySetCopy = querySet;
		querySetCopy["Queries"] = queries;
		
		querySetCopy
	];

(*!
	\function GetQueryRelevance
	
	\calltable
		GetQueryRelevance[querySetName, querySetType, query, docFileName] '' gets the relevance for a particular document as it pertains to a particular query in a query set.

	Examples:
    
    GetQueryRelevance["Britannica", "TrainingSet", "fruit" | "temperate climates", "APPLE"]

    ===

    1

    Unit tests: GetQueryRelevance.mt

    \maintainer danielb
*)
GetQueryRelevance[querySetName_, querySetType_, query_, docFileName_] :=
	Scope[
		path = querySetPath[querySetName, "TrainingSet"];
		If [FileExistsQ[path],
			querySet = GetQuerySet[querySetName, querySetType];
			
			queries = Lookup[querySet, "Queries", {}];
			matchingQueries = Select[queries, #["Query"] === query &];
			If [Length[matchingQueries] > 0,
				(* Found the query. *)
				queryCopy = matchingQueries[[1]];
				searchResults = queryCopy["SearchResults"];
				matchingFiles = Select[searchResults, #["FileName"] === docFileName &];
				If [Length[matchingFiles] > 0,
					(* Found the document file record. *)
					docFile = matchingFiles[[1]];
					docFile["Relevance"]
					,
					Null
				]
				,
				Null
			]
		]
	];

(*!
	\function querySetList
	
	\calltable
		querySetList[] '' returns the list of query sets.
	
	\related 'QuerySets
	
	\maintainer danielb
*)
querySetList[] :=
	Block[{},
		<|
			"Name" -> FileNameTake[#, {-3}],
			"Type" -> StringReplace[FileNameTake[#, {-2}], ".m" ~~ EndOfString :> ""]
		|> & /@
			Select[
				FileNames["*.m", $querySetDir, Infinity],
				FileNameTake[#, {-4}] === "QuerySets" && FileNameTake[#, {-1}] =!= "SavedResults.m" &
			]
	];

(*!
	\function QuerySets
	
	\calltable
		QuerySets[] '' displays a user interface displaying available query sets.
	
	\related 'QuerySet
	
	\maintainer danielb
*)
QuerySets[] :=
	DynamicModule[{uiContent},
		
		querySets = querySetList[];
		
		uiContent =
		(
			Style[#, FontFamily -> "Arial", FontSize -> 12] & @
			Column[
				Function[{item},
					Framed[	
						Row[{
							LightButton["Select", uiContent = QuerySet[item["Name"], item["Type"]], "Padding" -> 10],
							Framed[#, FrameStyle -> None, FrameMargins -> {{20, 0}, {0, 0}}] & @
								Style[item["Name"] <> " " <> item["Type"], "Subtitle"]
						}],
						FrameStyle -> LightGray,
						FrameMargins -> 20,
						ImageSize -> {600, Automatic}
					]
				] /@
					querySets
			]
		);
		
		Dynamic[
			uiContent
		]
	];

(*!
	\function QuerySet
	
	\calltable
		QuerySet[querySetName, querySetType] '' displays a UI for performing actions on a query set.

	Examples:
    
    QuerySet["Britannica", "TrainingSet"]
    
    \related 'QuerySets
	
	\maintainer danielb
*)
QuerySet[querySetName_, querySetType_ : "TrainingSet"] :=
	DynamicModule[{uiContent},
	Module[{querySetRunRes},
		
		(* Previously I had the Dynamic surrounding the entire UI
		   so that I could set uiContent to Sequence @@ {} here,
		   but that causes problems. Notably, when the cell gets
		   deleted, LightButton seems to evaluate its action
		   as if it were clicked. (crazy!) Using the following
		   approach instead creates an unsightly white gap
		   at the bottom of the UI. *)
		uiContent = Framed["", ImageSize -> {0, 0}, FrameStyle -> None];
		
		Style[#, FontFamily -> "Arial", FontSize -> 12] & @
		Framed[	
			Column[{
				Style[querySetName <> " " <> querySetType, "Subtitle"],
                LightButton[
                    "Run Query Set",
                    querySetRunRes = RunQuerySet[GetQuerySet[querySetName, querySetType]];
                	uiContent =
                	Column[
                		{
                		With[{prevResult = GetPreviousQuerySetRun[querySetName, querySetType]},
                			If [MatchQ[prevResult, $Failed | None],
                				Sequence @@
                				{
                					FormatInfoBoxRow["Overall", querySetRunRes["SearchResultValueTotal"]],
                					"No previous results to compare to."
                				}
                				,
                				SummarizeQuerySetRunDiff[DiffQuerySetRuns[prevResult, querySetRunRes]]
                			]
                		],
                		
                		LightButton[
                			"Save Results",
                			If [SaveQuerySetRun[querySetRunRes] =!= $Failed,
                				MessageDialog["Saved query run result."]
                			],
                			"Padding" -> 10
                		]
                		},
						Automatic,
						1.3
                    ] 
                    ,
                    "Padding" -> 10
                ],
                Dynamic[uiContent]
				},
				Automatic,
				1.3
			],
			FrameStyle -> LightGray,
			FrameMargins -> 20,
			ImageSize -> {600, Automatic}
		]
	]
	];

(*!
	\function SaveQuerySetRun
	
	\calltable
		SaveQuerySetRun[querySetRun] '' given a query set run result, save it to disk so that we can compare future results to it.

	Examples:
    
    SaveQuerySetRun[RunQuerySet[GetQuerySet["Britannica", "TrainingSet"]]]
	
	\related 'RunQuerySet 'DiffQuerySetRuns
	
	\maintainer danielb
*)
querySetSavedResultsFile[querySetName_, querySetType_] := FileNameJoin[{$querySetDir, querySetName, querySetType, "SavedResults.m"}]
SaveQuerySetRun[querySetRun_] :=
	Scope[
		file = querySetSavedResultsFile[querySetRun["QuerySetName"], querySetRun["QuerySetType"]];
		
		If [FileExistsQ[file],
			data = Get[file];
			If [!ListQ[data], data = {}];
			,
			data = {};
		];
		
		PrependTo[data, querySetRun];
		
		(* For example, if the person saved the same result twice in a row. *)
		data = DeleteDuplicates[data];
		
		Export[file, tabsToSpace @ Indent2[data], "Text"]
	];

(*!
	\function GetPreviousQuerySetRun
	
	\calltable
		GetPreviousQuerySetRun[querySetName, querySetType, offset] '' gets the previously saved query set run result.

	offset: -1 implies the most recent item, -2 the next-most recent, etc. 

	Examples:
    
    GetPreviousQuerySetRun[querySetName, querySetType, offset] === TODO
	
	\related '
	
	\maintainer danielb
*)
GetPreviousQuerySetRun::bado = "Bad offset for previous query set run: `1`";
GetPreviousQuerySetRun[querySetName_, querySetType_, offset_:-1] :=
	Scope[
		list = QuerySetSavedResults[querySetName, querySetType];
		
		If [list === {} && offset == -1,
			Return[None];
			,
			If [offset < -1 && Abs[offset] <= Length[list],
				Message[GetPreviousQuerySetRun::bado, offset];
				Return[$Failed];
			];
		];
		
		list[[Abs[offset]]] 
	];

(*!
	\function QuerySetSavedResults
	
	\calltable
		QuerySetSavedResults[querySetName, querySetType] '' reads from disk the previously saved query run results.

	Examples:
    
    QuerySetSavedResults[querySetName, querySetType] === TODO
	
	\related 'SaveQuerySetRun
	
	\maintainer danielb
*)
QuerySetSavedResults[querySetName_, querySetType_] :=
	Scope[
		file = querySetSavedResultsFile[querySetName, querySetType];
		
		If [FileExistsQ[file],
			Get[file]
			,
			{}
		]
	]

(*!
	\function WithTemporaryDirectories
	
	\calltable
		WithTemporaryDirectories[{x = x0, y = y0, ...}, expr] '' for each symbol assignment, creates a temporary directory whos name is the assignment value, and assigns the temporary directory path to the symbol, then acting like 'With', replaces any uses of the given symbol in the inner expression with the directory path. Once the inner expression has finished evaluating, the temporary directories are cleaned up. Useful when unit testing functions that act on files/directories.

	Example:
	
	WithTemporaryDirectories[
		{a = "MyDir"},
		Wrapper[a]
	]
	
	===
	
	Wrapper["E:\\Users\\Daniel\\AppData\\Local\\Temp\\m-d5860c9c-4c0d-43f9-9091-d667add45346\\MyDir"]

	\maintainer danielb
*)
Attributes[WithTemporaryDirectories] = {HoldAllComplete};
WithTemporaryDirectories[assignmentsIn_List, expr_] :=
	Module[{assignments, heldExpr = HoldComplete[expr], temporaryHoldComplete,
			temporaryDirectories = {}, symbolToTemporaryDirMapping,
			tempPath},
		
		Attributes[temporaryHoldComplete] = {HoldAllComplete};
		
		If [!MatchQ[HoldComplete[assignmentsIn], HoldComplete[{Repeated[Set[_Symbol, _]]}]],
			Print["WithTemporaryDirectories: Invalid assignments: ", Indent2[HoldComplete[assignmentsIn], "RemoveHold" -> True]];
			$Failed
			,
			assignments = HeldListToListOfHeld[HoldComplete[assignmentsIn]];
			
			(* Create a mapping from With symbols to temporary directories, and creates
			   those directories. *)
			symbolToTemporaryDirMapping =
				Function[{assignment},
					
					tempPath = TemporaryFile[];
					DeleteFile[tempPath];
									   
					assignment /. HoldComplete[Set[symbol_, value_]] :>
						(
						tempPath = FileNameJoin[{tempPath, value}];
						CreateDirectory[tempPath];
						
						(* Create the list this way since Append/AppendTo are slow.
						   Not that it matters much for small lists like this. *)
						temporaryDirectories = {temporaryDirectories, tempPath};
						
						HoldComplete[symbol] -> tempPath
						)
					
			   ] /@ assignments;
			   
		   (* Replace instances of the 'With' symbols with their corresponding
			  temporary file. *)
		   heldExpr = ReplaceHeldExpressions[heldExpr, symbolToTemporaryDirMapping, _Symbol];
		   
		   (* Evaluate the expression, capture the result, cleanup the temporary
			  files, and then returned the evaluated expression. *)
		   With[{res = ReleaseHold[heldExpr]},
			   DeleteDirectory[#, DeleteContents -> True] & /@ Flatten[temporaryDirectories];
			   res
		   ]
		]
	]

(*!
	\function WithTemporaryFiles
	
	\calltable
		WithTemporaryFiles[{x = x0, y = y0, ...}, expr] '' for each symbol assignment, writes the assignment value to a new temporary file and assigns the temporary file name to the given symbol, then acting like 'With', replaces any uses of the given symbol in the inner expression with the file path. Once the inner expression has finished evaluating, the temporary files are cleaned up. Useful when unit testing functions that act on files.

	Example:
	
	WithTemporaryFiles[
		{a = "1"},
		Wrapper[a]
	]
	
	===
	
	Wrapper["E:\\Users\\Daniel\\AppData\\Local\\Temp\\m-bcec1816-bd2b-4cbb-9dc6-99c1583b4587"]
	
	Example:
	
	WithTemporaryFiles[
		{a = "1"},
		Get[a]
	]
	
	===
	
	"1"

	Unit tests:

	RunUnitTests[CalculateParse`GeneralLibrary`WithTemporaryFiles]

	\maintainer danielb
*)
Clear[WithTemporaryFiles];
Attributes[WithTemporaryFiles] = {HoldAllComplete};
Options[WithTemporaryFiles] =
{
	"Directory" -> Automatic,				(*< The directory in which to create the temporary files. If Automatic, then $TemporaryDirectory is used. *)
	"Extension" -> None						(*< The extension to use for the files. *)
};
WithTemporaryFiles[assignmentsIn_List, expr_, OptionsPattern[]] :=
	Module[{assignments, heldExpr = HoldComplete[expr], temporaryHoldComplete,
			temporaryFiles = {}, symbolToTemporaryFileMapping, dir},
			
		dir = OptionValue["Directory"];
		If [dir === Automatic,
			dir = $TemporaryDirectory;
		];
		
		Attributes[temporaryHoldComplete] = {HoldAllComplete};
		
		If [!MatchQ[HoldComplete[assignmentsIn], HoldComplete[{Repeated[Set[_Symbol, _]]}]],
			Print["WithTemporaryFiles: Invalid assignments: ", Indent2[HoldComplete[assignmentsIn], "RemoveHold" -> True]];
			$Failed
			,
			assignments = HeldListToListOfHeld[HoldComplete[assignmentsIn]];

			(* Create a mapping from With symbols to temporary files, and write
			   the desired expression into the temporary files. *)
			symbolToTemporaryFileMapping =
				Function[{assignment},
					With[{newTemporaryFile = TemporaryFile["Directory" -> dir, "Extension" -> OptionValue["Extension"]]},

						(* Create the list this way since Append/AppendTo are slow.
						   Not that it matters much for small lists like this. *)
						temporaryFiles = {temporaryFiles, newTemporaryFile};
					   
						assignment /. HoldComplete[Set[symbol_, value_]] :>
							(
							Export[newTemporaryFile, value, "Text"];
							HoldComplete[symbol] -> newTemporaryFile
							)
					]
			   ] /@ assignments;
			   
		   (* Replace instances of the 'With' symbols with their corresponding
			  temporary file. *)
		   heldExpr = ReplaceHeldExpressions[heldExpr, symbolToTemporaryFileMapping, _Symbol];
		   
		   (* Evaluate the expression, capture the result, cleanup the temporary
			  files, and then returned the evaluated expression. *)
		   With[{res = ReleaseHold[heldExpr]},
			   If [FileExistsQ[#], DeleteFile[#]] & /@ Flatten[temporaryFiles];
			   res
		   ]
		]
	]

(*!
	\function CreateSearchIndexDeleteIfExists
	
	\calltable
		CreateSearchIndexDeleteIfExists[objs, name] '' create an index but automatically delete it if it already exists. Useful for testing.

	Examples:
    
    MatchQ[
        index =
            CreateSearchIndexDeleteIfExists[
                {
                    Association["Field1" -> "Just testing"]
                },
                "MyIndex2",
                "Driver" -> "Lucene"
            ],
        _SearchIndexObject
    ]

    ===

    True

    Unit tests: CreateSearchIndexDeleteIfExists.mt

    \maintainer danielb
*)
Options[CreateSearchIndexDeleteIfExists] = Options[CreateSearchIndex];
CreateSearchIndexDeleteIfExists[objs_, name_, opts:OptionsPattern[]] :=
	Block[{index},
		index = Quiet[SearchIndexObject[name], SearchIndexObject::neind];
		If [index =!= $Failed,
			DeleteSearchIndex[name];
		];
		CreateSearchIndex[
			objs,
			name,
			opts
		]
	];

(*!
	\function CreateTestSearchIndex
	
	\calltable
		CreateTestSearchIndex[objs] '' creates a search index with name TestSearchIndex and deletes it first if it already exists.

	Examples:
    
    MatchQ[
        index =
            CreateTestSearchIndex[
                {
                    Association["Field1" -> "Just testing"]
                },
                "Driver" -> "Lucene"
            ],
        _SearchIndexObject
    ]

    ===

    True

    Unit tests: CreateTestSearchIndex.mt

    \maintainer danielb
*)
Options[CreateTestSearchIndex] = Options[CreateSearchIndex];
CreateTestSearchIndex[objs_, opts:OptionsPattern[]] := CreateSearchIndexDeleteIfExists[objs, "TestSearchIndex", opts]

(*!
	\function RunTestFile
	
	\calltable
		RunTestFile[mtFile] '' runs a .mt file and returns the list of TestResult objects.
	
	\maintainer danielb
*)
RunTestFile[mtFile_] :=
	Block[{tests, expressions, res},
		If [!FileExistsQ[mtFile], Print["RunTestFile: File not found: ", mtFile]; Return[$Failed]];
		expressions = Quiet@ReadList[mtFile, Hold[Expression]];	
		tests = Cases[expressions, Hold[_MUnit`Test|_MUnit`TestMatch|_VerificationTest], Infinity];
		res = ReleaseHold /@ tests
	];

(*!
	\function RunTestFiles
	
	\calltable
		RunTestFiles[files] '' run a list of .mt files, printing results using AntLog. Returns the total number of failures as well as the JUnit XML results.
	
	\maintainer danielb
*)
Clear[RunTestFiles];
Options[RunTestFiles] =
{
	"PrintFiles" -> True,			(*< Print to indicate when running a file. *)
	"PrintSuccess" -> True			(*< Print a line to indicate when a test was successful? *)
};
RunTestFiles[files_, OptionsPattern[]] :=
	(* Prevent Global`AntLog from evaluating when the TextSearch paclet loads, since we
	   don't want it defined on end-user machines. *)
	Block[{AntLog = With[{sym = ToExpression["Global`AntLog"]}, If [DownValues[sym] =!= {}, sym, Print]],
		   testRes = {}, testFileRes, numFailures, numTests},
		
		numFailures = 0;
		numTests = 0;
		
		Function[{testFile},
			If [TrueQ[OptionValue["PrintFiles"]],
				AntLog["Running: " <> ToString[FileNameTake[testFile, -1]]];
			];
			testFileRes = RunTestFile[testFile];
			numFailures += Length[Select[testFileRes, (MUnit`FailureMode[#] =!= "Success") &]];
			numTests += Length[testFileRes];
			Function[{testResult},
				If [TrueQ[OptionValue["PrintSuccess"]] || MUnit`FailureMode[testResult] =!= "Success",
					PrintTestResult[testResult];
				]
			] /@ testFileRes;
			AppendTo[testRes, testFile -> testFileRes];
		] /@ files;
		
		<|
			"NumFailures" -> numFailures,
			"NumTests" -> numTests,
			"JUnitXml" -> TestResultsToJUnitXml[testRes]
		|>
	];

(*!
	\function PrintTestResult
	
	\calltable
		PrintTestResult[testResult] '' prints an MUnit TestResult and uses AntLog if it is defined.
	
	\maintainer danielb
*)
PrintTestResult[testResult_] :=
	(* Prevent Global`AntLog from evaluating when the TextSearch paclet loads, since we
	   don't want it defined on end-user machines. *)
	Block[{AntLog = With[{sym = ToExpression["Global`AntLog"]}, If [DownValues[sym] =!= {}, sym, Print]]},
		AntLog[TestResultToString[testResult]]
	];

(*!
	\function TestResultToJUnitXml
	
	\calltable
		TestResultToJUnitXml[testFileName, testResult] '' converts an MUnit TestResult to JUnit XML.
		TestResultToJUnitXml[testFileName, testResults] '' converts a list of MUnit TestResults (from the same test file) to JUnit XML.
	
	\maintainer danielb
*)
Clear[TestResultToJUnitXml];
TestResultToJUnitXml[testFileName_String, testResult_List] :=
	Block[{},
		StringJoin[
			Riffle[
				Function[{thisTestResult},
					TestResultToJUnitXml[testFileName, thisTestResult]
				] /@ testResult,
				"\n"
			]
		]
	];
	
TestResultToJUnitXml[testFileName_String, testResult_] :=
	Block[{},
		"      <testcase classname=" <> ToString[testFileName, InputForm] <> " name=" <> ToString[TestID[testResult], InputForm] <> " time=\"0\">\n" <>
		"         <system-out>" <> escapeXmlContent[TestResultToString[testResult]] <> "</system-out>\n" <>
		"      </testcase>"
	];

(*!
	\function TestResultsToJUnitXml
	
	\calltable
		TestResultsToJUnitXml[testResults] '' converts a MUnit TestResults to JUnit XML.
		
	testResults: List of: filePath_String -> {TestResult...}
	
	\maintainer danielb
*)
TestResultsToJUnitXml[testResults_List] :=
	Block[{numErrors, numFailures, numTests, timestamp},
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" <>
		"<testsuites>\n" <>
		StringJoin[
			Riffle[
				Function[{testFileResult},
					With[{fileName = FileNameTake[testFileResult[[1]], -1], testResultsForThisFile = testFileResult[[2]]},
						numErrors = 0;
						numFailures = Length[Select[testResultsForThisFile, (MUnit`FailureMode[#] =!= "Success") &]];
						numTests = Length[testResultsForThisFile];
						timestamp = "TODO";
						"   <testsuite name=" <> ToString[FileNameTake[fileName, -1], InputForm] <> " errors=\"" <> ToString[numErrors] <> "\" skipped=\"0\" tests=\"" <> ToString[numTests] <> "\" failures=\"" <> ToString[numFailures] <> "\" time=\"0\" timestamp=\"" <> timestamp <> "\">\n" <>
						TestResultToJUnitXml[fileName, testResultsForThisFile] <> "\n" <>
						"    </testsuite>"
					]
				] /@ testResults,
				"\n"
			]
		] <> "\n" <>
		"</testsuites>\n"
	];

(*!
	\function TestResultToString
	
	\calltable
		TestResultToString[testResult] '' converts a MUnit TestResult to a string.
	
	\maintainer danielb
*)
TestResultToString[testResult_] :=
	Block[{str = "", out},
		
		out[line_] := (If [str =!= "", str = str <> "\n"]; str = str <> line); 
		
		If [MUnit`FailureMode[testResult] =!= "Success",
			out[TestID[testResult] <> ": FAILED"];
			out[Indent2[MUnit`TestInput[testResult], 1, 4]];
			out["    EXPECTED:"];
			out[Indent2[MUnit`ExpectedOutput[testResult], 2, 4]];
			out["    ACTUAL:"];
			out[Indent2[MUnit`ActualOutput[testResult], 2, 4]];
			If [MUnit`ActualMessages[testResult] =!= MUnit`ExpectedMessages[testResult],
				If [MUnit`ExpectedMessages[testResult] =!= {},
					out["    EXPECTED MESSAGES:"];
					out[Indent2[MUnit`ExpectedMessages[testResult], 2, 4]];
					out["    ACTUAL MESSAGES:"];
					out[Indent2[MUnit`ActualMessages[testResult], 2, 4]];
					,
					out["    MESSAGES:"];
					out[Indent2[MUnit`ActualMessages[testResult], 2, 4]];
				]
			]
			,
			out[TestID[testResult] <> ": SUCCESS"];
		];
		
		str
	];

(*!
	\function escapeXmlContent
	
	\calltable
		escapeXmlContent[content] '' escapes things like < and >.

	Examples:
    
    escapeXmlContent["This is a <test>."] === "This is a &lt;test&gt;."

    Unit tests: escapeXmlContent.mt

    \maintainer danielb
*)
escapeXmlContent[content_String] :=
	Block[{},
		StringReplace[
			content,
			{
				"\"" :> "&quot;",
				"'" :> "&apos;",
				"<" :> "&lt;",
				">" :> "&gt;",
				"&" :> "&amp;"
			}
		]
	];

(*!
	\function RunTests
	
	\calltable
		RunTests[] '' runs the TextSearch tests.
	
	\related '
	
	\maintainer danielb
*)
Clear[RunTests];
Options[RunTests] =
{
	"RunMUnitTests" -> True,			(*< run .mt files? *)
	"RunPacletToolsTests" -> True		(*< run the PacletTools tests? *)
};
RunTests[OptionsPattern[]] :=
	Block[{testFiles, res},
		If [TrueQ[OptionValue["RunMUnitTests"]],
			Print[Style["MUnit Tests", "Section"]];
			testFiles = FileNames["*.mt", $testDir, Infinity];
			res = RunTestFiles[testFiles, "PrintSuccess" -> False, "PrintFiles" -> False];
			Print[ToString[res["NumFailures"]] <> " failures (of " <> ToString[res["NumTests"]] <> " tests)"];
		];
		
		If [TrueQ[OptionValue["RunPacletToolsTests"]],
			Print[Style["PacletTools Tests", "Section"]];
			Get["PacletTools`"];
			PacletTools`RunPacletTests["TextSearch`"];
			Print["Done"];
		];
	];

