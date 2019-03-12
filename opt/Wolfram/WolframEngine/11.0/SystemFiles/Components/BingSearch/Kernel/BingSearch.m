Get["BingSearchFunctions.m"]

Begin["BingSearch`"] (* Begin Private Context *)

Begin["`Private`"](* Begin Private Context *)

(******************************* BingSearch *************************************)
(* Authentication information *)
bingsearchdata[] = {
        "ServiceName" 		-> "BingSearch",
        "URLFetchFun"		:> (With[{params=Lookup[{##2},"Parameters",{}]},
        		(
        			URLFetch[#1,{"StatusCode","ContentData"},Sequence@@FilterRules[{##2},Except["Parameters"|"Headers"]],
        					"Parameters"->Cases[params, Except[Rule["customerID", _]|Rule["primaryAccountKey", _]]],
        					"Headers" -> {},
        					"Username" -> ("customerID"/.params),
             				"Password" -> ("primaryAccountKey"/.params)]
             	)]&)
        	,
        "ClientInfo"		:> OAuthDialogDump`Private`MultipleKeyDialog["BingSearch",{"Customer ID"->"customerID","Primary Account Key"->"primaryAccountKey"},
        								"https://datamarket.azure.com/dataset/bing/search","https://datamarket.azure.com/dataset/bing/search#terms"],

	 	"Gets"				-> {"Search"},
	 	"Posts"				-> {},
	 	"RawGets"			-> {"RawSearch"},
	 	"RawPosts"			-> {},
 		"Information"		-> "Wolfram Language connection to BingSearch API"
}

formatresults[rawdata_] := Module[{status},
	(
		status = rawdata[[1]];
		If[status!=200, rawdata[[2]], formatresults0[rawdata[[2]]]]
	)]

formatresults0[raw_String]:="results"/.("d" /. (ImportString[raw,"RawJSON"]))
formatresults0[raw:{_Integer..}]:=formatresults0[FromCharacterCode[raw]]
formatresults0[_]:=$Failed
(****** Raw Properties ******)

bingsearchdata["RawSearch"] := {
        "URL"				-> (ToString@StringForm["https://api.datamarket.azure.com/Bing/Search/`1`", #]&),
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"Query","Market","Adult","WebFiletype","ImageFilters","$top","$skip","$format"},
        "PathParameters"	-> {"SearchType","$format"},
        "RequiredParameters"-> {"SearchType","$format"},
        "ResultsFunction"	-> formatresults
    }

bingsearchdata[___]:=$Failed

(****** Cooked Properties ******)
bingsearchcookeddata[prop_,id_, rest__] := bingsearchcookeddata[prop,id,{rest}]

bingsearchcookeddata["Search",id_,args_] :=
	Module[{args2=If[MatchQ[args,_Rule],List@args,args],rawdata0,nresultsperpage,rawdata,calls,residual,progress,items = {},totalresults, params = {"$format" -> "json"},invalidParameters},
        invalidParameters = Select[Keys@args2,!MemberQ[{"Query","FileType","Site","Language","Country","ContentFiltering","SearchType","MaxItems",MaxItems,"StartIndex","Elements","ImageFilters"},#]&];

        If[Length[invalidParameters]>0,
    		(
    			Message[ServiceObject::noget,#,"BingSearch"]&/@invalidParameters;
    			Throw[$Failed]
    		)];
		If[KeyExistsQ[args2,"SearchType"],
			If[!MemberQ[{"News","Web","Image","Video","RelatedSearch","SpellingSuggestions","Images","Videos","Pictures"},"SearchType"/.args2],
				Message[ServiceExecute::nval,"SearchType","BingSearch"];
				Throw[$Failed]
			]
		];
        params = If[!KeyExistsQ[args2,"SearchType"],
			Append[params,"SearchType"->"Web"], (* default value *)
            Append[params,"SearchType"->BSGetType["SearchType" /. args2]]
        ];

        params=If[ Xnor[!KeyExistsQ[args2,"MaxItems"],!KeyExistsQ[args2,MaxItems]],
            Append[params,"$top"->10],
            (If[Or[And[IntegerQ[MaxItems/.args2],Positive[MaxItems/.args2]],And[IntegerQ["MaxItems"/.args2],Positive["MaxItems"/.args2]]],
              (args2=args2/."MaxItems"->MaxItems;
              Append[params,"$top"->MaxItems/.args2]),
              (Message[ServiceExecute::nval,MaxItems,"BingSearch"];
              Throw[$Failed])
              ]
            )
        ];


        params = If[!KeyExistsQ[args2,"StartIndex"],
        	Append[params,"$skip"->0],
            Append[params,"$skip"->("StartIndex"/.args2)]
        ];

        If[KeyExistsQ[args2,"Query"],
			params = Append[params,"Query"->ToString["Query"/.args2]],
			(
				Message[ServiceExecute::nparam,"Query"];
				Throw[$Failed]
			)
		];

		If[KeyExistsQ[args2,"Elements"],
			If[!MemberQ[{"Data","Images","Thumbnails","ImageLinks","ImageThumbnailsLinks"},"Elements"/.args2],
				Message[ServiceExecute::nval,"Elements","BingSearch"];
				Throw[$Failed]
			]
		];

        If[KeyExistsQ[args2,"Site"],
            params = ReplaceAll[params,Rule["Query",q_] :> Rule["Query",(q <> " " <> BSSiteParse["Site" /. args2])]];
        ];

        If[KeyExistsQ[args2,"FileType"],
            If[!MatchQ[BSSearchParameter["FileType"/.args2,BSfiletypeMap],_Missing],
                params = ReplaceAll[params, Rule["Query",q_] :> Rule["Query",q <> " " <> BSFileTypeParse["FileType"/.args2]]]
            ]
        ];

        params = ReplaceAll[params, Rule["Query",q_] :> Rule["Query","'" <>BSBooleanParse@q<> "'"]];

        If[KeyExistsQ[args2,"Country"] && KeyExistsQ[args2,"Language"],
        	params = Append[params, "Market"->BSSearchParameter[{"Language"/.args2,"Country"/.args2},BSmarketMap]]
        ];

        If[KeyExistsQ[args2,"ContentFiltering"],
        	params = Append[params, "Adult"->BSSearchParameter["ContentFiltering"/.args2,BSadultMap]]
        ];

        If[KeyExistsQ[args2,"ImageFilters"],
        	params = Append[params, "ImageFilters"-> ("'"<>StringJoin[Riffle[StringJoin[Riffle[#, ":"]] & /@ (("ImageFilters"/.args2) /. Rule -> List), "+"]]<>"'")]
        ];

        nresultsperpage = If[MatchQ["SearchType"/.params,"News"],15,50];

        calls = Quotient["$top"/.params, nresultsperpage];
        residual = ("$top"/.params) - (calls*nresultsperpage);

        PrintTemporary[ProgressIndicator[Dynamic[progress], {0, calls}]];
        params = ReplaceAll[params, Rule["$top",m_] :> Rule["$top",ToString[m]]];
        If[calls > 0,
            (
              (
  				params=ReplaceAll[params,Rule["$skip",i_] :> Rule["$skip",ToString[i]]];
                rawdata0 = KeyClient`rawkeydata[id,"RawSearch",params];
                rawdata0 = {rawdata0[[1]],FromCharacterCode[rawdata0[[2]], "UTF8"]};(*This is to correctly encode characters*)
                If[rawdata0[[1]]!=200,
				(
					If[rawdata0[[1]]=403,
						Message[ServiceExecute::serrormsg,BSParseHTMLErrors[rawdata0[[2]]]];
						Throw[$Failed]
					];
					Message[ServiceExecute::serrormsg,rawdata0[[2]]];
					Throw[$Failed]

				)];
                rawdata = formatresults[rawdata0];
                totalresults = Length@rawdata;
                items = Join[items, rawdata];
                progress = progress +1;
                params = ReplaceAll[params,Rule["$skip",i_] :> Rule["$skip",ToString[FromDigits@ToString@i+#*nresultsperpage+1]]]
              )& /@ Range[0,calls-1]
            )
        ];
        If[residual > 0,
            (
              params = ReplaceAll[params,Rule["$skip",i_]:>Rule["$skip",ToString[FromDigits@ToString@i+calls*nresultsperpage+1]]];
              params = ReplaceAll[params,Rule["$top",_] :> Rule["$top",ToString[residual]]];
              rawdata0 = KeyClient`rawkeydata[id,"RawSearch",params];
              rawdata0 = {rawdata0[[1]],FromCharacterCode[rawdata0[[2]], "UTF8"]};(*This is to correctly encode characters*)
              If[rawdata0[[1]]!=200,
				(
					If[rawdata0[[1]]==403,
						Message[ServiceExecute::serrormsg,BSParseHTMLErrors[rawdata0[[2]]]];
						Throw[$Failed]
					];
					Message[ServiceExecute::serrormsg,rawdata0[[2]]];
					Throw[$Failed]

				)];
              rawdata = formatresults[rawdata0];
              totalresults = Length@rawdata;
              items = Join[items, rawdata]
            )
        ];
        If[!MatchQ[totalresults,0],
	        Switch["SearchType"/.params,
	                "News",
	                    Dataset[
	                        Association[
	                            {"Title" -> #[[1]],
	                             "Source"->#[[2]],
	                             "Link" -> #[[3]],
	                              "Summary" -> #[[4]]}] & /@
	                              ({"Title", "Source", "Url", "Description"}/.items)
	                            ],
	                "Web",
	                    Dataset[
	                        Association[
	                            {"Title" -> #[[1]],
	                             "Snippet" -> #[[3]],
	                             "Link"->#[[2]]
	                             (*"Link" -> Hyperlink[#[[3]], #[[2]]]*)
	                              }] & /@
	                              ({"Title", "Url", (*"DisplayUrl",*)"Description"}/.items)
	                            ],
	                "Image"|"Images"|"Pictures",
	                    (
	                        Switch["Elements"/.args2,
	                            "Images",
	                            Import/@(("MediaUrl"/.items)),
	                            "Thumbnails",
	                            Import/@("MediaUrl"/.("Thumbnail"/.items)),
	                            "ImageThumbnailsLinks",
	                            "MediaUrl"/.("Thumbnail"/.items),
	                            "ImageLinks",
	                            "MediaUrl"/.items,
	                            "Elements",
	                            Dataset[Association[
	                            	{"Thumbnail" -> Import["MediaUrl" /. #[[4]]],
	                                 "Title" -> #[[2]],
	                                 "ContextLink" -> #[[1]],
	                                 "ImageLink" -> #[[3]],
	                                 "Width"->FromDigits@#[[5]],
	                                 "Height"->FromDigits@#[[6]],
	                                 "ByteSize"->FromDigits@#[[7]],
	                                 "FileExtension"->#[[8]]
	                                 }] & /@ ({"SourceUrl","Title","MediaUrl", "Thumbnail","Width","Height","FileSize","ContentType"}/.items)
	                            ]
	                       ]
	                    )
	                    ,
	                "Videos"|"Video",
	                    Dataset[
	                        Association[
	                            {"Thumbnail" -> EventHandler[Import["MediaUrl" /. #[[4]]], {"MouseClicked" :> SystemOpen[#[[1]]]}],
	                              "Title" -> #[[1]],
	                              "Link" -> #[[2]],
	                              "RunTime"->Quantity[FromDigits@#[[3]],"MilliSeconds"]
	                              }] & /@
	                              ({"Title", "MediaUrl", "RunTime","Thumbnail"}/.items)
	                            ],
	                "RelatedSearch",
	                    Dataset[
	                        Association[
	                            {"Tittle" -> #[[1]],
	                              "Link"-> #[[2]]
	                              }] & /@
	                              ({"Title", "BingUrl"}/.items)
	                            ],
	                "SpellingSuggestions",
	                    Dataset[
	                        Association[
	                            {"Value" -> #[[1]]
	                              }] & /@
	                              ({"Value"}/.items)
	                            ]
	            ],
	            Dataset[Association[items]]
	      ]
    ]



bingsearchcookeddata[req_, id_] := bingsearchcookeddata[req, id,{}]

bingsearchcookeddata[___] := $Failed

bingsearchsendmessage[___] := $Failed

End[]

End[]

(*SetAttributes[{},{ReadProtected, Protected}];*)

(* Return two functions to define oauthservicedata, oauthcookeddata  *)

{BingSearch`Private`bingsearchdata,
 BingSearch`Private`bingsearchcookeddata,
 BingSearch`Private`bingsearchsendmessage}
