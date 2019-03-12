Package["TextSearch`"]

PackageImport["GeneralUtilities`"]

PackageImport["JLink`"]

PackageScope["DevFormatContentObject"]

PackageScope["DevTextSearch"]

PackageScope["ScoreExplanationSummary"]

PackageScope["ScoreExplanationToSummary"]

PackageScope["DiffScoreSummaries"]

PackageScope["ScoreSummaryDiff"]

PackageScope["FormatInfoBoxRow"]

PackageScope["TitleOrFileName"]

PackageScope["RelevanceControl"]

PackageScope["LightButton"]

PackageScope["FormatTokenization"]

PackageScope["LuceneTokenize"]

PackageScope["LuceneTokenization"]

(* Useful for disabling Print statements. *)
Attributes[XPrint] = {HoldAllComplete};

(*!
	\function DevFormatContentObject
	
	\calltable
		DevFormatContentObject[obj, i] '' formats a ContentObject for development/debugging. Makes available additional features.
	   
	obj:	The ContentObject 
	i:		The search result number

	Examples:
	
	DevFormatContentObject[
		First[TextSearch[britannicaIndex, "crab apple"]]
	]
	
	\related 'DevTextSearch
	
	\maintainer danielb
*)
Options[DevFormatContentObject] =
{
	"Width" -> Automatic,					(*< The width, in pixels, of the box. *)
	"Position" -> None,						(*< The search rank. ex. #1 *)
	"Highlight" -> False,					(*< Highlight this result? *)
	"TopResultScoreExplanation" -> Null,	(*< Can be specified to enable the 'Compare to Top Result' option. *)
	"Index" -> Null,						(*< The index that was searched. When specified, we enable the relevance control. *)
	"Query" -> Null
};
DevFormatContentObject[objIn_ContentObject, OptionsPattern[]] :=
	DynamicModule[{scoreExplanation = "", scoreExplanationMode = None, obj = objIn[[1]],
				   frameMargins = 20, relevanceControlContent = "",
				   fileName, index = OptionValue["Index"],
				   query = OptionValue["Query"],
				   topResultScoreExplanation = OptionValue[TopResultScoreExplanation],
				   currentRelevance = "", title},
		
		title = TitleOrFileName[obj];
		fileName = obj["FileName"];

		If [StringQ[index] && ValueQ[query] && query =!= Null,
			currentRelevance =
				GetQueryRelevance[
					index,
					"TrainingSet",
					query,
					fileName
				];
			If [currentRelevance =!= Null,
				relevanceControlContent =
					RelevanceControl[
						Function[{value},
							SetQueryRelevance[
								index,
								"TrainingSet",
								query,
								fileName,
								value
							];
						],
						"CurrentValue" -> currentRelevance
					]
			];
		];
		
		Framed[
			Column[
				{
				Grid[
					{{
					Row[
						{
						Hyperlink[
							Style[
								If [!IntegerQ[OptionValue["Position"]], "", ToString[OptionValue["Position"]] <> ": "] <> title,
								"Subtitle"
							],
							obj["Location"]
						],
						"  ",
						If [StringQ[topResultScoreExplanation] ||
							index =!= Null,
							
							Framed[#, FrameStyle -> None, FrameMargins -> {{0, 0}, {7, 0}}] & @
							ActionMenu[
								" \:25BE ",
								{
									If [StringQ[topResultScoreExplanation],
										Sequence @@
										{
										"Compare to Top Result" :>
											(
											scoreExplanationMode = "Comparison";
											scoreExplanation =
											DiffScoreSummaries[
												ScoreExplanationToSummary[topResultScoreExplanation],
												ScoreExplanationToSummary[obj["ScoreExplanation"]]
											]
											),
										"Select for Comparison" :>
											(
											TextSearch`Tools`Private`$SelectedScoreExplanation = obj["ScoreExplanation"];
											),
										"Compare with Selected" :>
											(
											If [!ValueQ[TextSearch`Tools`Private`$SelectedScoreExplanation],
												MessageDialog["Before you can 'Compare with Selected' you must choose 'Select for Comparison' for another search result."];
												,
												scoreExplanationMode = "Comparison";
												scoreExplanation =
												DiffScoreSummaries[
													ScoreExplanationToSummary[TextSearch`Tools`Private`$SelectedScoreExplanation],
													ScoreExplanationToSummary[obj["ScoreExplanation"]]
												]
											]
											)
										}
										,
										Sequence @@ {}
									],
									If [OptionValue["Index"] =!= Null,
										"Set Relevance" :>
											(
											Which [
												!StringQ[index],
												MessageDialog["To enable relevance features, the first argument to DevTextSearch must be a String, the name of the index, rather than a SearchIndexObject."];
												,
												query === Null,
												MessageDialog["The 'Query' option must be specified when calling DevFormatContentObject for relevance setting to work."];
												,
												True,
												relevanceControlContent =
													RelevanceControl[
														Function[{value},
															SetQueryRelevance[
																index,
																"TrainingSet",
																query,
																fileName,
																value
															];
														]
													]
											];
											)
										,
										Sequence @@ {}
									]
								},
								Appearance -> "Button"
							]
							,
							Sequence @@ {}
						]
						},
						Alignment -> {Left, Center}
					]
					,
					Framed[
						Dynamic[relevanceControlContent],
						ImageSize -> Full,
						FrameStyle -> None,
						Alignment -> {Right}
					]
					}},
					ImageSize -> {Full, Full}
				],
				FormatInfoBoxRow[
					"Score",
					If [StringQ[obj["ScoreExplanation"]],
						Row[
							{
								Round[obj["Score"], 0.001],
								"  ",
								LightButton[
									" ... ",
									Switch[
										scoreExplanationMode,
										None | "Comparison",
											scoreExplanation = ScoreExplanationToSummary[obj["ScoreExplanation"]];
											scoreExplanationMode = "Summary";
										"Summary",
											(* Second click: Display the raw textual score explanation. *)
											scoreExplanation = Style[obj["ScoreExplanation"], FontFamily -> "Courier", FontSize -> 12];
											scoreExplanationMode = "Raw";
										"Raw",
											scoreExplanation = "";
											scoreExplanationMode = None;
									]
								]
							}
						]
						,
						obj["Score"]
					]
				],
				(* Ideally I'd like to use the commented out code below, but that only
				   works if the entire column is surrounded by Dynamic. But if you do that,
				   then any change to the UI causes any nested uses of DynamicModule to
				   be re-generated, which resets any of their dynamic state. Ugh.
				   Lou seems to suggest there isn't a way to do this, so I'm stuck
				   using the following approach which leaves an unsightly large
				   bottom margin at the bottom of the UI. *)
				Dynamic[scoreExplanation]
				(*
				If [scoreExplanation === Null,
					Sequence @@ {}
					,
					scoreExplanation
				]*)
				}
			],
			FrameStyle -> LightGray,
			FrameMargins -> frameMargins,
			ImageSize -> {OptionValue["Width"], Automatic},
			Background -> If [TrueQ[OptionValue["Highlight"]], RGBColor[244/255, 250/255, 1], None]
		]
	];

$helpIcon := $helpIcon =
Uncompress[
"1:eJzNVE9vEkEUJ/5JjPHgQT+A38Kr9dD0CvoBtrolTRA2C9XtbQ8cuOyJQDLRdaMRTbaJ4SD9Q6wHMUCbGDXEEAk1RNqKDanFskjZna3vLRtYtqCejC+ZybyZ95v33u/NvCuzIe/cGY/HEz4L08wdxs/OnUL1HExe5t41nmcWfRdBuRkMz/uD7O2ZYIT1s/zV2dOweQnGZRi4Pv7PxNC7n9+/UR8rybglyQeZ9c39dtf8E9A0aS2fBhTHcREhKhFZlokUFSIcCnmSPegak7H6xjOCdqKcqzY0pzSqOVmEk3gytdPWx6K31hQwEEgBkY13qiT2nUZE6WlhGyza9bcKIXGSaRvUBdYPKni7qGoUpKuiIiSy5e1qgVjryj463d1Mg6LmGy547SW6XiodwrrX+hQDJZbtJ1B7jvhMqQlH1DhcUSCA1Q4dBmCaRjEFdJE93c7LpNQ0kelmWYWrCFn6qtmklVeAn6iTAUp/IGXR5SM6kpRWz4uITX381hlsNksZsN2o9xzwziPBDYeQXklIx9b3EapPwk8G36/jcgxcr2mjIVnBx3a0kTud1DlusBkYxqm3XhA3dcdW4RR4bFbh7J2jvQQmTopffg7MJhVuEICQyOmWR0o1q/rD3Fu1ojLO9SDZD2ml/2h3W0h1/8HikdGrvE5DJJMfrZ3sb7/Mers3xq9LXB+W3H/4lx/234sPW+bUYsRulqh5FwJs+DwsrocCId7HMbdYH7ZF7/SUy+gCdlroonyAZe7OB/3WyQ1+gf0Flh9xEw=="
];

(*!
	\function DevTextSearch
	
	\calltable
		DevTextSearch[index, form] '' Performs a TextSearch and formats content objects with information and UI tools helpful to developers (uses DevFormatContentObject)

	Examples:
	
	DevTextSearch[britannicaIndex, "crab apple"]
	
	\related 'DevFormatContentObject
	
	\maintainer danielb
*)
Options[DevTextSearch] =
{
	"ResultCount" -> 10,			(*< Maximum number of search results. *)
	"HighlightFunction" -> None		(*< Highlight results for which passing the content object to that function returns True. Example: "HighlightFunction" -> Function[{obj}, obj["FileName"] === "APPLE"] *)
};
DevTextSearch[indexIn_, form_, OptionsPattern[]] :=
	Block[{textSearchRes, numResultsToShow = OptionValue["ResultCount"], resToShow,
		   topResultScoreExplanation, prevDebugFlag, highlight,
		   highlightQ = OptionValue["HighlightFunction"] =!= None,
		   foundTargetItem = False, numResultsToConsider, itemsNotYetConsidered, res,
		   targetItemPosition, index = indexIn, indexObject = indexIn,
		   showJavaExceptions = True},

		(* The index that was passed in might be a SearchIndexObject,
		   but it might also be the name of the index. *)
		If [StringQ[index],
			indexObject = SearchIndexObject[index];
			If [!MatchQ[indexObject, _SearchIndexObject],
				Return[$Failed];
			];
		];
		
		LoadJavaClass["com.wolfram.textsearch.LuceneDebug"];
		prevDebugFlag = LuceneDebug`debuggingIsEnabled[];
		LuceneDebug`enableDebugging[];
		
		res = textSearchRes = TextSearch[indexObject, form];
		
		If [ListQ[textSearchRes],
			
			numResultsToConsider = Min[numResultsToShow, Length[textSearchRes]];
			resToShow = Take[textSearchRes, numResultsToConsider];
			If [resToShow === {},
				Return["No matches."];
			];
			
			topResultScoreExplanation = resToShow[[1]]["ScoreExplanation"];
			
			res =
			Column[
				Join[
					MapIndexed[
						With[{contentObject = #1, position = #2[[1]]},
							
							If [highlightQ,
								highlight = TrueQ[OptionValue["HighlightFunction"][contentObject]];
								If [highlight && !TrueQ[foundTargetItem],
									foundTargetItem = True;
									targetItemPosition = position;
								];
							];
							
							DevFormatContentObject[
								contentObject,
								(* Should be wide enough to display score explanations *)
								"Width" -> 800,
								"Position" -> position,
								"Highlight" -> highlight,
								"TopResultScoreExplanation" -> topResultScoreExplanation,
								"Index" -> index,
								"Query" -> form
							]
						] &,
						resToShow
					],
					{
						If [Length[res] > numResultsToShow,
							Framed[#, FrameStyle -> None, FrameMargins -> {{0, 0}, {0, 7}}] & @
								("Showing first " <> ToString[numResultsToShow] <> " of " <> ToString[Length[res]] <> " results")
							,
							Sequence @@ {}
						]
					}
				]
			];
			
			(* If we didn't find the target/highlighted item in the first N search
			   results (the number of serach results to display), see if we can
			   find it further down. *)
			If [highlightQ && !TrueQ[foundTargetItem],
				itemsNotYetConsidered = Take[textSearchRes, {numResultsToConsider, Length[textSearchRes]}];
				Function[{position},
					With[{contentObject = itemsNotYetConsidered[[position]]},
						If [TrueQ[OptionValue["HighlightFunction"][contentObject]],
							targetItemPosition = numResultsToConsider + position - 1;
							Return[];
						]
					]
				] /@ Range[1, Length[itemsNotYetConsidered]];
			];
			
			If [highlightQ,
				res =
					Column[{
						Framed[#, FrameStyle -> None, FrameMargins -> {{0, 0}, {7, 0}}] & @
							If [IntegerQ[targetItemPosition],
								"Result of interest is at position " <> ToString[targetItemPosition] <> "."
								,
								"Result of interest wasn't found."
							],
						res
					}]; 
			];
		];
		
		With[{mainRes = res},
			res =
			DynamicModule[{optionalUi = {}},
				Dynamic[
					Column[
						{
							Row[{
								TextSearch`Private`$AdditionalQueryResultMetadata["LuceneQueryString"],
								"   ",
								LightButton[
									" ... ",
									If [optionalUi === {},
										optionalUi =
										{
											FormatInfoBoxRow["Profiling", (ToString[#] <> " ms") & /@ TextSearch`Private`$AdditionalQueryResultMetadata["Profiling"]]
										}
										,
										optionalUi = {}
									];
								],
								"   ",
								EventHandler[
									$helpIcon,
									{
										"MouseClicked" :>
											NotebookOpen[FileNameJoin[{FileNameDrop[FindFile["TextSearch`"], -1], "Documentation", "DevTextSearch.nb"}]]
									}
								]
							}]
						,
						Sequence @@ optionalUi
						,
						mainRes
						},
						Automatic,
						1.3
					]
				]
			]
		];
		
		res =
			Style[
				res,
				FontFamily -> "Arial",
				FontSize -> 12
			];
		
		If [!TrueQ[prevDebugFlag],
			LuceneDebug`disableDebugging[];
		];
		
		res
			
	];

(*!
	\function formatInfoBoxRow
	
	\calltable
		FormatInfoBoxRow[key, value] '' creates a formatted row for inclusion in an info box.

	Examples:
	
	FormatInfoBoxRow["Score", 0.35]
	
	\maintainer danielb
*)
FormatInfoBoxRow[key_, value_] :=
	Block[{},
		Style[
			Row[
			{
				Style[
					key <> ": ",
					GrayLevel[0.4]
				],
				value
			}
			],
			FontFamily -> "Arial",
			FontSize -> 12
		]
	];

(*!
	\function LightButton
	
	\calltable
		LightButton[caption, action] '' a button that has a lighter background color.

	Examples:
	
	LightButton["...", Print["Hello, world!"]]
	
	\maintainer danielb
*)
Attributes[LightButton] = {HoldAllComplete};
Options[LightButton] =
{
	"Background" -> None,			(*< The background color of the button. *)
	"TextColor" -> Black,			(*< The color of the button's caption text. *)
	"FrameColor" -> GrayLevel[0.7],	(*< The color of the button's border. *)
	"Padding" -> Automatic			(*< Padding around button caption. *)
};
LightButton[caption_, action_, OptionsPattern[]] :=
	Block[{},
		MouseAppearance[
			EventHandler[
				Framed[
					Style[caption, OptionValue["TextColor"]],
					FrameStyle -> OptionValue["FrameColor"],
					Background -> OptionValue["Background"],
					FrameMargins -> OptionValue["Padding"]
				],
				{
					"MouseClicked" :>
						action
				}
			],
			"Arrow"
		]
	];

(*!
	\function scoreExplanationToList
	
	\calltable
		scoreExplanationToList[scoreExplanation] '' parses the score explanation into a list of the form {indentLevel, score, description}

	Examples:
	
	scoreExplanationToList[
		"0.6419789 = product of:\n  1.2839578 = sum of:\n	..."
	]

	===

	{
		{0, "0.6419789", "product of:"},
		{1, "1.2839578", "sum of:"},
		...
	}

	Unit tests: scoreExplanationToList.mt

	\maintainer danielb
*)
scoreExplanationToList::cpl = "Couldn't parse Lucene score explanation line: `1`";
scoreExplanationToList[scoreExplanationIn_] :=
	Block[{lines, scoreExplanation},
		
		(* BM25Similarity seems to result in one of the lines wrapping, which messes things up. Try and undo that wrapping. *)
		scoreExplanation = StringReplace[scoreExplanationIn, "\n)" :> ")"];
		
		lines = StringSplit[scoreExplanation, "\n"];
		Function[{line},
			StringCases[
				line,
				StartOfLine ~~
					whitespace:(WhitespaceCharacter...) ~~
					score:(DigitCharacter.. ~~ "." ~~ DigitCharacter..) ~~
					" = " ~~
					rest:__
				:> {StringLength[whitespace] / 2, ToExpression[score], rest}
			] /.
				{
					{list_List} :> list,
					_ :> (Message[scoreExplanationToList::cpl, line]; $Failed)
				}
		] /@ lines
	];

(*!
	\function scoreExplanationToNestedExpression
	
	\calltable
		scoreExplanationToNestedExpression[scoreExplanation] '' given a Lucene score explanation, parse it into a nested expression.

	Examples:
	
	scoreExplanationToNestedExpression[
		"0.1 = desc\n  0.2 = desc\n  0.3 = desc"
	]
	
	===
	
	Association[
		"Score" -> 0.1,
		"Description" -> "desc",
		"Children" ->
		{
			<|"Score" -> 0.2, "Description" -> "desc", "Children" -> {}|>,
			<|"Score" -> 0.3, "Description" -> "desc", "Children" -> {}|>
		}
	]

	Unit tests: scoreExplanationToNestedExpression.mt

	\maintainer danielb
*)
scoreExplanationToNestedExpression[scoreExplanation_] :=
	Block[{list, indentLevel, score, description, prevIndentLevel,
		  itemAtLevel, newItem, itemAtPrevLevel, parsedDescription, onLevelDecreased},
		
		(* Parse the raw string into a list of semi-parsed lines. *)
		list = scoreExplanationToList[scoreExplanation];
		If [!FreeQ[list, $Failed], Return[$Failed]];
		
		If [Length[list] === 0,
			Return[None];
		];
		
		(* Create a dummy level -1 to act as the parent of items at indentation level 0. *)
		itemAtLevel = <|-1 -> <|"Children" -> {}|>|>;
		
		(* Update the last items since we may have added new children to them. *)
		onLevelDecreased =
			Function[
				Function[{level},
					itemAtPrevLevel = itemAtLevel[level - 1];
					itemAtPrevLevel["Children"] = Append[Most[itemAtPrevLevel["Children"]], itemAtLevel[level]];
					itemAtLevel[level - 1] = itemAtPrevLevel;
					itemAtLevel[level] = Missing[];
				] /@ Range[prevIndentLevel - 1, indentLevel, -1];
			];
		
		(* Process each line, building up a nested Association structure.
		   We use the indentation level of the lines to indicate what level
		   of the nested heirarchy they are. *)
		prevIndentLevel = -1;
		Function[{item},
			{indentLevel, score, description} = item;
			
			If [indentLevel < prevIndentLevel,
				onLevelDecreased[];
			];
			
			newItem = <|"Score" -> score, "Description" -> description, "Type" -> guessLineType[description], "Children" -> {}|>;
			
			parsedDescription = parseScoreSummaryLine[newItem["Type"], description];
			If [MatchQ[parsedDescription, _Association],
				newItem = Insert[newItem, Normal[parsedDescription], 4];
			];
			
			itemAtLevel[indentLevel] = newItem;
			
			itemAtPrevLevel = itemAtLevel[indentLevel - 1];
			itemAtPrevLevel["Children"] = Append[itemAtPrevLevel["Children"], newItem];
			
			itemAtLevel[indentLevel - 1] = itemAtPrevLevel;
			
			prevIndentLevel = indentLevel;
			
		] /@ list;
		
		If [indentLevel > 0,
			indentLevel = 1;
			onLevelDecreased[];
		];
		
		itemAtLevel[0]
	];

(*!
	\function guessLineType
	
	\calltable
		guessLineType[description] '' given a description from a Lucene scoring explanation, return its type. These are types we've made up to classify the different types of Lucene scoring lines.

	Examples:
	
	guessLineType["idf(docFreq=549, maxDocs=27069)"] === "IDF"
	
	\maintainer danielb
*)
guessLineType[description_] :=
	Block[{},
		Which[
			!StringFreeQ[description, "weight("],
			"SubQuery"
			,
			!StringFreeQ[description, "score("],
			"Score"
			,
			!StringFreeQ[description, "fieldWeight in" | "tfNorm"],
			"Field"
			,
			!StringFreeQ[description, "tf(" | "termFreq="],
			"TermFrequency"
			,
			!StringFreeQ[description, "idf()"],
			"IDFSum"
			,
			!StringFreeQ[description, "idf(docFreq="],
			"IDF"
			,
			!StringFreeQ[description, "queryNorm"],
			"QueryNorm"
			,
			!StringFreeQ[description, "fieldNorm"],
			"FieldNorm"
			,
			!StringFreeQ[description, "fieldLength"],
			"FieldLength"
			,
			!StringFreeQ[description, "avgFieldLength"],
			"AverageFieldLength"
			,
			!StringFreeQ[description, "queryWeight"],
			"QueryWeight"
			,
			!StringFreeQ[description, "sum of"],
			"Sum"
			,
			!StringFreeQ[description, "product of"],
			"Product"
			,
			!StringFreeQ[description, "coord("],
			"NumTermsMatchedFactor"
			,
			True,
			None
		]
	];

(*!
	\function parseScoreSummaryLine
	
	\calltable
		parseScoreSummaryLine[type, line] '' given a line of a score summary, and the 'type' we assigned it, return an association of keys/values that represent any additional information we'd like to extract from it.

	Examples:
	
	parseScoreSummaryLine[
		"IDF",
		"idf(docFreq=549, maxDocs=27069)"
	]
	
	===
	
	<|"DocumentFrequency" -> 549, "DocumentCount" -> 27069|>
	
	\related 'scoreExplanationToNestedExpression
	
	\maintainer danielb
*)
parseScoreSummaryLine[type_, line_] := None

parseScoreSummaryLine["SubQuery", line_] :=
	Replace[
		StringCases[
			line,
			"weight(" ~~ subQueryDesc:Shortest[__] ~~ " in" :> subQueryDesc
		],
		{
			{inner_String} :>
				With[{term = StringReplace[inner, StartOfString ~~ "Plaintext:" ~~ rest__ :> rest]},
					<|"Term" -> term|>
				],
			_ :> None
		}
	]

parseScoreSummaryLine["IDF", line_] :=
	Replace[
		StringCases[
			line,
			"idf(docFreq=" ~~ a:(DigitCharacter..) ~~ ", maxDocs=" ~~ b:(DigitCharacter..) :> <|"DocumentFrequency" -> FromDigits[a], "DocumentCount" -> FromDigits[b]|>
		],
		{
			{inner_Association} :> inner,
			_ :> None
		}
	]
	
parseScoreSummaryLine["TermFrequency", line_] :=
	Replace[
		StringCases[
			line,
			"tf(freq=" ~~ a:((DigitCharacter|".")..) :> <|"TermFrequency" -> ToExpression[a]|>
		],
		{
			{inner_Association} :> inner,
			_ :> None
		}
	]

parseScoreSummaryLine["NumTermsMatchedFactor", line_] :=
	Replace[
		StringCases[
			line,
			"coord(" ~~ a:((DigitCharacter|"/")..) :> <|"NumTermsMatchedFactor" -> ToExpression[a]|>
		],
		{
			{inner_Association} :> inner,
			_ :> None
		}
	]

(*!
	\function ScoreExplanationToSummary
	
	\calltable
		ScoreExplanationToSummary[scoreExplanation] '' given a lucene score explanation, convert it to a summarized form. (Association)

	Example:

	ScoreExplanationToSummary[
		"0.6419789 = product of:\n  1.2839578 = sum of:\n	1.2839578 = weight(Plaintext:\"temperate climates\" in 1055) [DefaultSimilarity], result of:\n	  1.2839578 = score(doc=1055,freq=1.0), product of:\n		0.9424959 = queryWeight, product of:\n		  10.898363 = idf(), sum of:\n			4.896226 = idf(docFreq=549, maxDocs=27069)\n			6.0021377 = idf(docFreq=181, maxDocs=27069)\n		  0.0864805 = queryNorm\n		1.3622954 = fieldWeight in 1055, product of:\n		  1.0 = tf(freq=1.0), with freq of:\n			1.0 = phraseFreq=1.0\n		  10.898363 = idf(), sum of:\n			4.896226 = idf(docFreq=549, maxDocs=27069)\n			6.0021377 = idf(docFreq=181, maxDocs=27069)\n		  0.125 = fieldNorm(doc=1055)\n  0.5 = coord(1/2)"
	]

	===

	ScoreExplanationSummary[
		Association[
			"Score" -> 0.6419789,
			"SubQueries" ->
			{
				Association[
					"Term" -> "\"temperate climates\"",
					"Score" -> 1.2839578,
					"Fields" -> {<|"TermFrequency" -> 1., "FieldLengthNorm" -> 0.125, "IDF" -> 10.898363|>},
					"QueryWeight" -> 0.9424959
				]
			},
			"NumTermsMatchedFactor" -> 1/2
		]
	]

	Unit tests: ScoreExplanationToSummary.mt

	\related 'scoreExplanationToNestedExpression
	
	\maintainer danielb
*)
ScoreExplanationToSummary[scoreExplanation_] :=
	Block[{nestedExpression, subQueries, summary, numTermsMatchedFactor, similarityType},
		
		nestedExpression = scoreExplanationToNestedExpression[scoreExplanation];
		If [nestedExpression === $Failed, Return[$Failed]];
		
		subQueries = Cases[{nestedExpression}, KeyValuePattern[{"Type" -> "SubQuery"}], Infinity];
		
		summary =
		<|
			"Score" -> nestedExpression["Score"],
			"SubQueries" ->
				Function[{subQuery},
					XPrint[subQuery // Indent2];
					
					(* ex. "TFIDFSimilarity", "BM25Similarity" *)
					similarityType = StringCases[subQuery["Description"], "[" ~~ name:(LetterCharacter | DigitCharacter).. ~~ "]" :> name][[1]];
					
					<|
						"Term" -> subQuery["Term"],
						"Score" -> subQuery["Score"],
						"Fields" ->
							scoreSummaryFields[similarityType, subQuery],
						With[{queryWeight = getFirstValueOfKey[subQuery, "QueryWeight", "Score"]},
							If [MissingQ[queryWeight],
								Sequence @@ {}
								,
								"QueryWeight" -> queryWeight
							]
						] 
					|>
				] /@ subQueries
		|>;
		
		numTermsMatchedFactor = getFirstValueOfKey[nestedExpression, "NumTermsMatchedFactor"];
		If [!MissingQ[numTermsMatchedFactor],
			AssociateTo[summary, "NumTermsMatchedFactor" -> numTermsMatchedFactor];
		];
		
		ScoreExplanationSummary[
			summary
		]
	];

(* Given some nested associations, search for the given key,
   and return the value associated with the first match. *)
getFirstValueOfKey[nestedAssociations_, key_] :=
	With[{cases = Cases[nestedAssociations, KeyValuePattern[{key -> _}], Infinity]},
		If [Length[cases] > 0,
			cases[[1]][key]
			,
			Missing[]
		]
	]

(* Given some nested associations, search for an association of
   the given type, and return the value for the given key. *)
getFirstValueOfKey[nestedAssociations_, type_, key_] :=
	With[{cases = Cases[nestedAssociations, KeyValuePattern[{"Type" -> type}], Infinity]},
		If [Length[cases] > 0,
			cases[[1]][key]
			,
			Missing[]
		]
	]

(* Formatting for ScoreExplanationSummary. Displays a summary of
   the Lucene scoring explanation is a formatted way for easier reading. *)
(*Format[summary_ScoreExplanationSummary] = .;*)
Format[summary_ScoreExplanationSummary] :=
	Block[{assoc = summary[[1]], queryWeightIndent = 20},
		Replace[
			assoc,
			KeyValuePattern[
				{
				"Score" -> score_,
				"SubQueries" -> subQueries_
				}
			] :>
			Style[
				Column[
					{
						(*Style[Round[score, 0.001], FontSize -> 20],*)
						Framed[#, FrameStyle -> None, FrameMargins -> {{10, 0}, {0, 0}}] & @
						Column[
							{
								If [!MissingQ[assoc["NumTermsMatchedFactor"]],
									FormatInfoBoxRow["Missing terms factor", assoc["NumTermsMatchedFactor"]]
									,
									Sequence @@ {}
								],
								Sequence @@
								Function[{subQuery},
									Framed[#, FrameStyle -> None, FrameMargins -> {{0, 0}, {0, 8}}] & @
									Column[{
										Style[
											Row[{Style[subQuery["Term"], FontWeight -> "Bold"], " (", Round[subQuery["Score"], 0.001], ")"}],
											FontSize -> 15
										],
										If [!MissingQ[subQuery["QueryWeight"]],
											Framed[#, FrameStyle -> None, FrameMargins -> {{20, 0}, {0, 3}}] & @
											FormatInfoBoxRow["Query Weight", Round[subQuery["QueryWeight"], 0.01]]
											,
											queryWeightIndent = 0;
											Sequence @@ {}
										],
										Framed[#, FrameStyle -> None, FrameMargins -> {{20 + queryWeightIndent, 0}, {0, 3}}] & @
										Column[
											Function[{field},
												Column[{
													FormatInfoBoxRow["Matches", field["TermFrequency"]],
													If [!MissingQ[field["FieldLengthNorm"]],
														FormatInfoBoxRow["Field Length Norm", Round[field["FieldLengthNorm"], 0.001]]
														,
														Sequence @@ {}
													],
													Sequence @@
													If [!MissingQ[field["AverageFieldLength"]],
														{
															FormatInfoBoxRow["Field Length", Round[field["FieldLength"], 0.001]],
															FormatInfoBoxRow["Average Field Length", Round[field["AverageFieldLength"], 0.001]]
														}
														,
														{}
													],
													FormatInfoBoxRow["IDF", Round[field["IDF"], 0.1]]
												}]
											] /@ subQuery["Fields"]
										]
									}]
								] /@ subQueries
							}
						]
					}
				],
				FontFamily -> "Arial"
			]
		]
	]

(*!
	\function DiffScoreSummaries
	
	\calltable
		DiffScoreSummaries[summary1, summary2] '' creates a diff for two score summaries to help explain why one document ended up with a higher score than another. The returned expression is formatted by default for easy readability.

	Examples:

    DiffScoreSummaries[
        ScoreExplanationToSummary[
            "0.6419789 = product of:\n  1.2839578 = sum of:\n    1.2839578 = weight(Plaintext:\"temperate climates\" in 1055) [DefaultSimilarity], result of:\n      1.2839578 = score(doc=1055,freq=1.0), product of:\n        0.9424959 = queryWeight, product of:\n          10.898363 = idf(), sum of:\n            4.896226 = idf(docFreq=549, maxDocs=27069)\n            6.0021377 = idf(docFreq=181, maxDocs=27069)\n          0.0864805 = queryNorm\n        1.3622954 = fieldWeight in 1055, product of:\n          1.0 = tf(freq=1.0), with freq of:\n            1.0 = phraseFreq=1.0\n          10.898363 = idf(), sum of:\n            4.896226 = idf(docFreq=549, maxDocs=27069)\n            6.0021377 = idf(docFreq=181, maxDocs=27069)\n          0.125 = fieldNorm(doc=1055)\n  0.5 = coord(1/2)"
        ],
        ScoreExplanationToSummary[
            "0.39255184 = sum of:\n  0.11168605 = weight(Plaintext:fruit in 1540) [DefaultSimilarity], result of:\n    0.11168605 = score(doc=1540,freq=10.0), product of:\n      0.33421776 = queryWeight, product of:\n        3.8646605 = idf(docFreq=1542, maxDocs=27069)\n        0.0864805 = queryNorm\n      0.3341715 = fieldWeight in 1540, product of:\n        3.1622777 = tf(freq=10.0), with freq of:\n          10.0 = termFreq=10.0\n        3.8646605 = idf(docFreq=1542, maxDocs=27069)\n        0.02734375 = fieldNorm(doc=1540)\n  0.2808658 = weight(Plaintext:\"temperate climates\" in 1540) [DefaultSimilarity], result of:\n    0.2808658 = score(doc=1540,freq=1.0), product of:\n      0.9424959 = queryWeight, product of:\n        10.898363 = idf(), sum of:\n          4.896226 = idf(docFreq=549, maxDocs=27069)\n          6.0021377 = idf(docFreq=181, maxDocs=27069)\n        0.0864805 = queryNorm\n      0.29800212 = fieldWeight in 1540, product of:\n        1.0 = tf(freq=1.0), with freq of:\n          1.0 = phraseFreq=1.0\n        10.898363 = idf(), sum of:\n          4.896226 = idf(docFreq=549, maxDocs=27069)\n          6.0021377 = idf(docFreq=181, maxDocs=27069)\n        0.02734375 = fieldNorm(doc=1540)"
        ]
    ]

    ===

    ScoreSummaryDiff[
        Association[
            "Score" -> Diff[0.6419789, 0.39255184],
            "SubQueries" ->
            {
                Diff[
                    Missing[],
                    Association[
                        "Term" -> "fruit",
                        "Score" -> 0.11168605,
                        "Fields" ->
                        {<|"TermFrequency" -> 10., "FieldLengthNorm" -> 0.02734375, "IDF" -> 3.8646605|>},
                        "QueryWeight" -> 0.33421776
                    ]
                ],
                Association[
                    "Term" -> "\"temperate climates\"",
                    "Score" -> Diff[1.2839578, 0.2808658],
                    "Fields" ->
                    {
                        Association[
                            "TermFrequency" -> 1.,
                            "FieldLengthNorm" -> Diff[0.125, 0.02734375],
                            "IDF" -> 10.898363
                        ]
                    },
                    "QueryWeight" -> 0.9424959
                ]
            },
            "NumTermsMatchedFactor" -> Diff[1/2, Missing["KeyAbsent", "NumTermsMatchedFactor"]]
        ]
    ]

	Unit tests: DiffScoreSummaries.mt

	\maintainer danielb
*)
DiffScoreSummaries[summary1_, summary2_] :=
	Block[{assoc1 = summary1[[1]], assoc2 = summary2[[1]], keyUnion},
		
		(* Union but with order preserved. *)
		keyUnion = DeleteDuplicates[Join[Keys[assoc1], Keys[assoc2]]];
		
		ScoreSummaryDiff[
			Association @@
			Function[{key},
				If [key === "SubQueries",
					"SubQueries" ->
						subQueryExplanationDiff[assoc1[key], assoc2[key]]
					,
					key ->
						DiffIfNecessary[assoc1[key], assoc2[key]]
				]
			] /@ keyUnion
		]
	];

(*!
	\function subQueryExplanationDiff
	
	\calltable
		subQueryExplanationDiff[subqueries1, subqueries2] '' given the subqueries from two different score explanations, attempt to create a diff.

	Examples:
	
	subQueryExplanationDiff[
		{
			Association[
				"Term" -> "fruit",
				"Score" -> 0.11168605,
				"Fields" ->
				{Association["TermFrequency" -> 10., "FieldLengthNorm" -> 0.02734375, "IDF" -> 3.8646605]},
				"QueryWeight" -> 0.33421776
			],
			Association[
				"Term" -> "\"temperate climates\"",
				"Score" -> 0.2808658,
				"Fields" ->
				{Association["TermFrequency" -> 1., "FieldLengthNorm" -> 0.02734375, "IDF" -> 10.898363]},
				"QueryWeight" -> 0.9424959
			]
		},
		{
			Association[
				"Term" -> "\"temperate climates\"",
				"Score" -> 1.2839578,
				"Fields" ->
				{Association["TermFrequency" -> 1., "FieldLengthNorm" -> 0.125, "IDF" -> 10.898363]},
				"QueryWeight" -> 0.9424959
			]
		}
	]

	===

	{
		Diff[
			Association[
				"Term" -> "fruit",
				"Score" -> 0.11168605,
				"Fields" ->
				{<|"TermFrequency" -> 10., "FieldLengthNorm" -> 0.02734375, "IDF" -> 3.8646605|>},
				"QueryWeight" -> 0.33421776
			],
			Missing[]
		],
		Association[
			"Term" -> "\"temperate climates\"",
			"Score" -> Diff[0.2808658, 1.2839578],
			"Fields" ->
				Diff[
					{<|"TermFrequency" -> 1., "FieldLengthNorm" -> 0.02734375, "IDF" -> 10.898363|>},
					{<|"TermFrequency" -> 1., "FieldLengthNorm" -> 0.125, "IDF" -> 10.898363|>}
				],
			"QueryWeight" -> 0.9424959
		]
	}

	Unit tests: subQueryExplanationDiff.mt

	\maintainer danielb
*)
subQueryExplanationDiff::sqd = "Couldn't align sub-queries.";
subQueryExplanationDiff[subqueries1_, subqueries2_] :=
	Block[{i, j, sequenceAlignment},
		
		(* Indexes into 'subqueries1' and 'subqueries2'. *)
		i = 1;
		j = 1;
		
		(* Try to solve the fuzzy problem of which subqueries
		   in the one score explanation correspond with
		   the subqueries in the other score explanation.
		   We could in theory usually get away with a 1-to-1
		   match, except that it's possible to have multiple
		   instances of a subquery. *)
		sequenceAlignment =
			SequenceAlignment[
				subqueries1[[All, "Term"]],
				subqueries2[[All, "Term"]]
			];
			
		XPrint[sequenceAlignment];
		
		Flatten[
			Function[{alignmentItem},
				Switch[alignmentItem,
					(* A cluster of sub-queries that appear in both score
					   explanations. *)
					{__String},
					Function[{k},
						subQueryExplanationDiff2[
							subqueries1[[i++]],
							subqueries2[[j++]]
						]
					] /@ Range[1, Length[alignmentItem]]
					,
					(* A cluster of sub-queries that appear in one
					   score explanation but not the other. *)
					{_List, {}},
					Function[{k},
						Diff[
							subqueries1[[i++]],
							Missing[]
						]
					] /@ Range[1, Length[First[alignmentItem]]]
					,
					(* A cluster of sub-queries that appear in one
					   score explanation but not the other. *)
					{{}, _List},
					Function[{k},
						Diff[
							Missing[],
							subqueries2[[j++]]
						]
					] /@ Range[1, Length[alignmentItem[[2]]]]
					,
					_,
					(* Can this occur? *)
					Message[subQueryExplanationDiff::sqd];
					Return[$Failed, Block];
				]
			] /@ sequenceAlignment,
			1
		]
	];

(*!
	\function subQueryExplanationDiff2
	
	\calltable
		subQueryExplanationDiff2[subquery1, subquery2] '' produce a diff of two subqueries.

	Examples:
	
	subQueryExplanationDiff2[
		Association[
			"Term" -> "\"temperate climates\"",
			"Score" -> 0.2808658,
			"Fields" ->
			{Association["TermFrequency" -> 1., "FieldLengthNorm" -> 0.02734375, "IDF" -> 10.898363]},
			"QueryWeight" -> 0.9424959
		],
		Association[
			"Term" -> "\"temperate climates\"",
			"Score" -> 1.2839578,
			"Fields" ->
			{Association["TermFrequency" -> 1., "FieldLengthNorm" -> 0.125, "IDF" -> 10.898363]},
			"QueryWeight" -> 0.9424959
		]
	]

	===

	Association[
		"Term" -> "\"temperate climates\"",
		"Score" -> Diff[0.2808658, 1.2839578],
		"Fields" ->
			Diff[
				{<|"TermFrequency" -> 1., "FieldLengthNorm" -> 0.02734375, "IDF" -> 10.898363|>},
				{<|"TermFrequency" -> 1., "FieldLengthNorm" -> 0.125, "IDF" -> 10.898363|>}
			],
		"QueryWeight" -> 0.9424959
	]

	Unit tests: subQueryExplanationDiff2.mt

	\maintainer danielb
*)
subQueryExplanationDiff2[subquery1_, subquery2_] :=
	Block[{keyUnion},
		(* Union but with order preserved. *)
		keyUnion = DeleteDuplicates[Join[Keys[subquery1], Keys[subquery2]]];
		
		Association @@
		Function[{key},
			If [key === "Fields",
				fieldsDiff[subquery1[key], subquery2[key]]
				,
				key ->
					DiffIfNecessary[subquery1[key], subquery2[key]]
			]
		] /@ keyUnion
	];

(*!
	\function fieldsDiff
	
	\calltable
		fieldsDiff[fields1, fields2] '' given field match information from two different score explanations, produce a diff.

	Examples:
	
	fieldsDiff[
		{Association["TermFrequency" -> 1., "FieldLengthNorm" -> 0.02734375, "IDF" -> 10.898363]},
		{Association["TermFrequency" -> 1., "FieldLengthNorm" -> 0.125, "IDF" -> 10.898363]}
	]

	===

	"Fields" ->
		Diff[
			{<|"TermFrequency" -> 1., "FieldLengthNorm" -> 0.02734375, "IDF" -> 10.898363|>},
			{<|"TermFrequency" -> 1., "FieldLengthNorm" -> 0.125, "IDF" -> 10.898363|>}
		]

	Unit tests: fieldsDiff.mt

	\maintainer danielb
*)
fieldsDiff[fields1_, fields2_] :=
	Block[{},
		"Fields" ->
			If [Length[fields1] === 1 && Length[fields2] === 1,
				(* Actually, if I'm thinking straight, terms are specific to a single
				   field, so there should typically only be a single field here,
				   and they should be the same field. *)
				{
					DiffIfNecessary[fields1[[1]], fields2[[1]]]
				},
				(* We can't use "UseIdiomThatFirstKeyIsAssumedToBeUniqueId" because
				   the Field associations don't have a reliable key. *)
				DiffIfNecessary[fields1, fields2, "UseIdiomThatFirstKeyIsAssumedToBeUniqueId" -> False]
			]
	];

Format[summary_ScoreSummaryDiff] :=
	Block[{assoc = summary[[1]]},
		Replace[
			assoc,
			KeyValuePattern[
				{
				"Score" -> score_,
				"SubQueries" -> subQueries_
				}
			] :>
			Style[
				Column[
					{
						(*Style[Round[score, 0.001], FontSize -> 20],*)
						Framed[#, FrameStyle -> None, FrameMargins -> {{10, 0}, {0, 0}}] & @
						Column[
							{
								If [!MissingQ[assoc["NumTermsMatchedFactor"]],
									FormatInfoBoxRow["Missing terms factor", format[assoc["NumTermsMatchedFactor"]]]
									,
									Sequence @@ {}
								],
								Sequence @@
								Function[{subQuery},
									Framed[#, FrameStyle -> None, FrameMargins -> {{0, 0}, {0, 8}}] & @
									format[
										Replace[
											subQuery,
											{
												a_Association :> Term[a],
												d_Diff :> Term /@ d 
											}
										]
									]
								] /@ subQueries
							}
						]
					}
				],
				FontFamily -> "Arial"
			]
		]
	]
	
Term[_Missing] := Missing[]

(*!
	\function format
	
	\calltable
		format[expr] '' given an expression, apply formatting to it if applicable.

	Examples:
	
	format[Diff[1, 2]] === Row[{2, " vs. ", 1}]

	Unit tests: format.mt

	\maintainer danielb
*)
format[expr_] := expr

format[a_Missing] := "N/A"

(* Previously defined but now missing. Pink background. *)
format[Diff[a_, b_Missing]] :=
	Framed[
		format[a],
		Background -> LightPink,
		FrameStyle -> LightGray,
		FrameMargins -> If [MatchQ[a, _Term], 20, 5]
	]

(* Previously missing but now defined. Green background. *)
format[Diff[a_Missing, b_]] :=
	Framed[
		format[b],
		Background -> LightGreen,
		FrameStyle -> LightGray,
		FrameMargins -> If [MatchQ[b, _Term], 20, 5]
	]
	
format[Diff[a_, b_]] :=
	Row[
		{
			Framed[
				format[b],
				Background -> LightOrange,
				FrameStyle -> LightGray,
				FrameMargins -> If [MatchQ[a, _Fields], 20, 5]
			]
			,
			If [MatchQ[a, _Fields],
				"	 vs.	 "
				,
				" vs. "
			]
			,
			format[a]
		}
	]
	
format[Term[assoc_]] :=
	Block[{queryWeightIndent = 20},
		Column[{
			Style[
				Row[{Style[assoc["Term"], FontWeight -> "Bold"], " (", format[assoc["Score"] /. r_Real :> Round[r, 0.001]], ")"}],
				FontSize -> 15
			],
			
			If [!MissingQ[assoc["QueryWeight"]],
				Framed[#, FrameStyle -> None, FrameMargins -> {{20, 0}, {0, 3}}] & @
				FormatInfoBoxRow["Query Weight", Round[assoc["QueryWeight"], 0.01]]
				,
				queryWeightIndent = 0;
				Sequence @@ {}
			],
			
			Framed[#, FrameStyle -> None, FrameMargins -> {{20 + queryWeightIndent, 0}, {0, 3}}] & @
			format[
				Replace[
					assoc["Fields"],
					{
						a_List :> Fields[a],
						d_Diff :> Fields /@ d 
					}
				]
			]
		}]
	]

format[Fields[fields_List]] :=
	Column[
		Function[{field},
			Column[{
				FormatInfoBoxRow["Matches", format[field["TermFrequency"]]],
				If [!MissingQ[field["FieldLengthNorm"]],
					FormatInfoBoxRow["Field Length Norm", format[smartRound[field["FieldLengthNorm"], 0.001]]]
					,
					Sequence @@ {}
				],
				Sequence @@
				If [!MissingQ[field["AverageFieldLength"]],
					{
						FormatInfoBoxRow["Field Length", format[smartRound[field["FieldLength"], 0.001]]],
						FormatInfoBoxRow["Average Field Length", format[smartRound[field["AverageFieldLength"], 0.001]]]
					}
					,
					{}
				],
				FormatInfoBoxRow["IDF", format[smartRound[field["IDF"], 0.1]]]
			}]
		] /@ fields
	]
	
smartRound[Diff[a_, b_], val_] := Diff[Round[a, val], Round[b, val]]
smartRound[a_, val_] := Round[a, val]

(*!
	\function TitleOrFileName
	
	\calltable
		TitleOrFileName[assoc] '' returns the Title if defined, otherwise the FileName.

	Examples:
	
	TitleOrFileName[Association["Title" -> "My Title", "FileName" -> "MyFileName.txt"]]

	===

	"My Title"

	Unit tests: TitleOrFileName.mt

	\maintainer danielb
*)
TitleOrFileName[assoc_] :=
	Block[{fieldsToCheck = {"Title", "Subject", "FileName"}},
		Function[{field},
			If [!MissingQ[assoc[field]],
				Return[assoc[field], Block]
			]
		] /@ fieldsToCheck;
		
		(* If all else fails, just label it "ContentObject" *)
		"ContentObject"
	]
	


(*!
	\function RelevanceControl
	
	\calltable
		RelevanceControl[func] '' a UI control for specifying relevance. When a relevance is selected, the given function is called with the choice.

	Examples:
	
	RelevanceControl[Function[Print["Clicked: ", #]]]
	
	\maintainer danielb
*)
Clear[RelevanceControl];
Options[RelevanceControl] =
{
	"CurrentValue" -> Null			(*< The currently selected value. *)
};
RelevanceControl[func_, OptionsPattern[]] :=
	DynamicModule[{choice = OptionValue["CurrentValue"]},
		Dynamic[
			Row[
				Table[
					With[{i2 = i /. 0. :> Null},
						LightButton[
							ToString[i /. 0. :> "X"],
							(
							choice = i2;
							func[choice]
							),
							"Background" -> If [choice === i, RGBColor[220/255, 230/255, 1], None],
							"TextColor" -> If [choice === i || choice === Null, Black, LightGray],
							"FrameColor" -> If [choice === i || choice === Null, GrayLevel[0.7], LightGray]
						]
					],
					{i, 0, 1, 0.1}
				]
			]
		]
	];

(*!
	\function scoreSummaryFields
	
	\calltable
		scoreSummaryFields[similarityType, subQuery] '' given a subquery of a lucene score explanation, return the field summary. The 'similarityType' argument allows for different implementations depending on the similarity type, such as 'TFIDFSimilarity', 'BM25Similarity', etc.

	Exmaple output:

    {<|"TermFrequency" -> 10., "FieldLengthNorm" -> 0.02734375, "IDF" -> 3.8646605|>}

    Unit tests: scoreSummaryFields.mt

    \maintainer danielb
*)
scoreSummaryFields["TFIDFSimilarity" | "DefaultSimilarity", subQuery_] :=
	Block[{},
		Function[{field},
			<|
				"TermFrequency" -> getFirstValueOfKey[field, "TermFrequency"],
				"FieldLengthNorm" -> getFirstValueOfKey[field, "FieldNorm", "Score"],
				"IDF" ->
				With[{a = getFirstValueOfKey[field, "IDFSum", "Score"], b = getFirstValueOfKey[field, "IDF", "Score"]},
					If [!MissingQ[a],
						a
						,
						b
					]
				]
			|>
		] /@ Cases[subQuery, KeyValuePattern[{"Type" -> "Field"}], Infinity]
	];

scoreSummaryFields["BM25Similarity", subQuery_] :=
	Block[{},
		Function[{field},
			<|
				"TermFrequency" -> getFirstValueOfKey[field, "TermFrequency", "Score"],
				"FieldLength" -> getFirstValueOfKey[field, "FieldLength", "Score"],
				"AverageFieldLength" -> getFirstValueOfKey[field, "AverageFieldLength", "Score"],
				"IDF" -> getFirstValueOfKey[field, "IDF", "Score"]
			|>
		] /@ Cases[subQuery, KeyValuePattern[{"Type" -> "Score"}], Infinity]
	];

(*!
	\function FormatTokenization
	
	\calltable
		FormatTokenization[str_, tokens] '' given a list of tokens, display them visually.
	
	\maintainer danielb
*)
$rowHeight = 0.15;
FormatTokenization[str_, tokens_] :=
	Block[{charRow, charWidth, height},
		
		charRow[_] := 0;
		
		charWidth = 1 / StringLength[str];
		
		Grid[
			{{
			Graphics[
				{
				FaceForm[RGBColor[0.95, 0.95, 0.95]],
				EdgeForm[{RGBColor[0.7, 0.7, 0.7]}],
				(* The token rectangles. *)
				Sequence @@ Flatten[(drawToken[#, charWidth] & /@ tokens), 1],
				height = Max[charRow /@ Range[1, StringLength[str]]] * $rowHeight;
				Sequence[Flatten[drawTitle[str], 1]]
				},
				ImageSize -> Max[StringLength[str] / 3 * 100, 500]
			]
			}},
			Spacings -> {4, 4},
			Frame -> True,
			FrameStyle -> {Thickness[4.0], GrayLevel[0.95]}
		]
	]

(*!
	\function drawToken
	
	\calltable
		drawToken[token] '' draws a token.
	
	\maintainer danielb
*)
drawToken[token_, charWidth_] :=
	Module[{rowStart, xPos1, xPos2, yPos1, yPos2},
		With[{label = token["Term"]},
			  
			rowStart = Max[charRow /@ Range[token["Start"], token["End"]]];
			xPos1 = (token["Start"] - 1) * charWidth;
			xPos2 = (token["End"]) * charWidth;
			yPos1 = $rowHeight * rowStart;
			yPos2 = $rowHeight * (rowStart + 1);
			
			(charRow[#] = (rowStart + 1)) & /@ Range[token["Start"], token["End"]];
			
			{
				RGBColor[0.95, 0.95, 0.95]
				,
				Rectangle[{xPos1, yPos1}, {xPos2, yPos2}],
				Sequence @@
					{
					{Text[Style[label, FontFamily -> "Arial", 12, Black], {(xPos1 + xPos2) / 2, (yPos1 + yPos2) / 2}, {0, 0}]}
					}
			}
		]
	]

(*!
	\function drawTitle
	
	\calltable
		drawTitle[str_String] '' draw the title of the tokenization.
	
	\maintainer danielb
*)
drawTitle[str_String] :=
	MapIndexed[Text[Style[#1, Bold, FontFamily -> "Arial", 14, Black], {(#2[[1]] - 1 + 0.5) * charWidth, height + $rowHeight / 4}, {0, -1}] &, Characters[str]]

(*!
	\function LuceneTokenize
	
	\calltable
		LuceneTokenize[str] '' tokenizes the given string using Lucene.

	Examples:
	
	LuceneTokenize["pb+j images"]
	
	===
	
	LuceneTokenization[
		Association[
			"String" -> "pb+j images",
			"Tokens" ->
			{
				<|"Term" -> "pb", "Start" -> 1, "End" -> 2|>,
				<|"Term" -> "j", "Start" -> 4, "End" -> 4|>,
				<|"Term" -> "images", "Start" -> 6, "End" -> 11|>
			}
		]
	]
	
	\related '
	
	\maintainer danielb
*)
LuceneTokenize[str_] :=
	Block[{},
		LoadJavaClass["com.wolfram.textsearch.LuceneDebug"];
		LuceneTokenization[
			<|
			"String" -> str,
			"Tokens" -> ToExpression[LuceneDebug`tokenize[str]]
			|>
		]
	];
	
LuceneTokenize[str_, index_, field_String] :=
	Block[{indexPath, index2 = index},
		
		If [MatchQ[index2, _String],
			index2 = SearchIndexObject[index];
		];
		
		If [MatchQ[index2, _SearchIndexObject],
			indexPath = index[[1, 1]];
			,
			Return[$Failed];
		];
		
		LoadJavaClass["com.wolfram.textsearch.LuceneDebug"];
		LuceneTokenization[
			<|
			"String" -> str,
			"Tokens" -> ToExpression[LuceneDebug`tokenize[str, indexPath, field]]
			|>
		]
	]

Format[tokenization_LuceneTokenization] := FormatTokenization[tokenization[[1, "String"]], tokenization[[1, "Tokens"]]]
