Package["TextSearch`"]

SearchIndexObject /: HoldPattern[CloudObject`CloudDeployActiveQ][sio_SearchIndexObject] := True


(* The companion code to this is in the IndexSearch.m where
   TextSearch[_CloudObject, ...] is defined.

   When someone calls TextSearch[_CloudObject, ...], we make a
   call to "run" that CloudObject on the cloud server, passing it
   the query, etc. (that CloudObject is a deployed SearchIndexObject)
   
   The below GenerateHTTPResponse down value acts as the handler, in the cloud,
   for cloud deployed SearchIndexObjects. It simply recurses to
   GenerateHTTPResponse[APIFunction[...]] so that the handler behaves
   as if it were a handler for a cloud deployed APIFunction.
   
   Namely, it reads the "q" and "t" parameters sent from the client,
   validates them, and then performs a TextSearch in the cloud.
*)
copyContent[f_File] /; TrueQ[$CloudEvaluation] := f
copyContent[f_File] /; !TrueQ[$CloudEvaluation] :=
	With[{
		filename = FileNameTake[First[f]]},
		With[{position =
			CloudEvaluate[
				TextSearch;
				With[
					{dir = CloudObject[
						StringJoin[
							FileNameDrop[
								ToExpression["TextSearch`$SearchIndicesDirectory"], 
								FileNameDepth[ToExpression["$HomeDirectory"]]
							],
							"/",
							filename
						]
					]},
					Quiet[DeleteDirectory[dir, DeleteContents -> True], DeleteDirectory::nodir];
					{dir, FileNameJoin[{ToExpression["TextSearch`$SearchIndicesDirectory"], FileNameTake[First[f]]}]}
				]
			]},
			CopyDirectory[
				First[f],
				First[position]
			];
			File[Last[position]]
		]
	]

SearchIndexObject /: HoldPattern[CloudDeploy][SearchIndexObject[f_File], HoldPattern[co_CloudObject], opts:OptionsPattern[]] := CloudDeploy[
		ExportForm[
			SearchIndexObject[copyContent[f]],
			{"WL", "application/vnd.wolfram.expression.searchindexobject"}
		],
		co,
		opts
	]
	
TextSearchPage /: HoldPattern[CloudDeploy][TextSearchPage[SearchIndexObject[f_File]], HoldPattern[co_CloudObject], opts:OptionsPattern[]] := CloudDeploy[
		ExportForm[
			TextSearchPage[SearchIndexObject[copyContent[f]]],
			{"WL", "application/vnd.wolfram.expression.textsearchpage"}
		],
		co,
		opts
	]

SearchIndexObject /: HoldPattern[GenerateHTTPResponse][sio_SearchIndexObject] := GenerateHTTPResponse[
	APIFunction[
		{
			"q" -> <|
				"Interpreter" -> Restricted[
					"Expression",
					{
						And, List, Except, Alternatives, Rule, 
						Entity, FixedOrder, ContainsAll, ContainsExactly, ContainsNone, 
						ContainsAny, ContainsOnly, GreaterThan,  GreaterEqualThan, 
						LessThan, LessEqualThan, TakeLargest, TakeSmallest, Between, EqualTo
					},
					{},
					None
				],
				"CodeLanguage" -> "WL"
			|>,
			"t" -> "String" -> $DefaultOutput
		},
		TextSearch[sio, #q, #t]&
	]
]

PackageExport[TextSearchPage]

TextSearchPage /: HoldPattern[CloudObject`CloudDeployActiveQ][_TextSearchPage] := True

convertSnippet[{}] := EmbeddedHTML[""]
convertSnippet[l_List] := EmbeddedHTML["&#133; " <> StringJoin[Riffle[convertSnippet /@ l, " &#133; "]] <> " &#133;"]
convertSnippet[Row[l_List]] := StringJoin[Replace[l, Highlighted[s_] :> "<mark>" <> s <> "</mark>", {1}]]

TextSearchPage /: HoldPattern[GenerateHTTPResponse][TextSearchPage[HoldPattern[co_CloudObject]]] := GenerateHTTPResponse @ Replace[
	CloudGet[co],
	{
		sio_SearchIndexObject :> TextSearchPage[sio],
		_ :> HTTPResponse["The supplied cloud object does not contain a search index.", <|"StatusCode" -> 500, "ContentType" -> "text/plain"|>]
	}
]

TextSearchPage /: HoldPattern[GenerateHTTPResponse][TextSearchPage[HoldPattern[sio:(_SearchIndexObject|_CloudObject)]]] := GenerateHTTPResponse[
	With[{form = FormObject["query" -> <|
						"Interpreter" -> "String",
						"Label" -> None,
						"Hint" -> "Search",
						"Control" -> Function[EmbeddedHTML[
							"<div class=\"input-group\">
								<span class=\"input-group-btn\">
									<button type=\"submit\" class=\"btn btn-primary form-submit\">
										<i class=\"glyphicon glyphicon-search\">
									</button>
								</i></span>
								<input type=\"text\"" <>
								"value=\"" <> # <>
								"\" placeholder=\"Search\" class=\"form-control smart-input\" name=\"query\" id=\"id_query\" data-field-name=\"String\" data-field-type=\"Structured\" data-field-verbose=\"string\" tabindex=\"1\">
							</div>"
						]],
						"Default" -> ""
					|>][HTTPRequestData["Query"]]},
		Replace[
			Setting[form],
			{
				KeyValuePattern["query" -> q:Except[""]] :> With[{res = TextSearch[sio, SearchQueryString[q], "SearchResultObject"]},
					GalleryView[
						If[
							res["Count"] === 0,
							{Grid[{{"Nothing found."}}, Alignment -> Left]},
							Sequence @@ {
								Function[
									index,
									Grid[
										{
											{Replace[
												res[index]["Location"],
												{
													url_URL :> 
														Hyperlink[Style[res[index]["FileName"], Bold], First[url]],
													_ :> Style[res[index]["FileName"], Bold]
												}
											]},
											{convertSnippet[res[index, "Snippet"]]}
										},
										Alignment -> Left
									]
								],
								res["Count"]
							}
						],
						"Pagination" -> {1, 10},
						AppearanceRules -> {"Description" -> form}
					]
				],
				_ :> GalleryView[{""}, AppearanceRules -> {"Description" -> form}]
			}
		]
	]
]
