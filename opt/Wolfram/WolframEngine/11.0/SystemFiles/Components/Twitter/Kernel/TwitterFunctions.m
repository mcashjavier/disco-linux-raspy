(*** Service specific utilites ****)

BeginPackage["TwitterFunctions`"];

TwitterUploadImages::usage = "";
TwitterPaginationCalls::usage = "";
TwitterBuildnetwork::usage = "";
TwitterGetscreennames::usage = "";
TwitterGetuserids::usage = "";
TwitterGetallparameters::usage = "";
TwitterFilterParameters::usage = "";
TwitterCamelCase::usage = "";
TwitterFormatvalue::usage = "";
TwitterReadDate::usage = "";
TwitterUserdataparse::usage = "";
TwitterFormatgeoposition::usage = "";
TwitterEventTimeline::usage = "";
TwitterPrettygrid::usage = "";
TwitterFormatValueDate::usage = "";
TwitterToString::usage = "";
TwitterStyleLimit::usage = "";
TwitterFormatByElementType::usage = "";

Begin["`Private`"];

TwitterUploadImages[id_,images_]:=Module[
	{rawdata, media, mediaBytes, params, otherparams, response},

	response = (
		media = #;
		mediaBytes=Which[
			MatchQ[media,{_?IntegerQ...}],media,
			TrueQ[Quiet[FileExistsQ[media],FileExistsQ::fstr]],Import[media, "Byte"],
			ImageQ[media],ImportString[ExportString[media, "JPG"], "Byte"],
			True,
				Check[ImportString[ExportString[media, "JPG"], "Byte"],Throw[$Failed]]
		];

		(*other params like "additional_owners"*)
		(*otherparams=FilterRules[args,Except["Image"]];

		otherparams = Normal@Map[TwitterToString,Association[otherparams]];
		otherparams=TwitterFilterParameters[otherparams,TwitterGetallparameters["RawUpload"]];
		If[otherparams=!={},
			otherparams[[All,2]]=ImportString[#, "Byte"]&/@otherparams[[All,2]];
		];
		*)
		params=Flatten[{"media"->mediaBytes,{} (*otherparams*)}];
		rawdata=OAuthClient`rawoauthdata[id,"RawUpload",params];
		TwitterOAuth`Private`twitterimport[rawdata]
	) & /@ images;

	response
]

(***************** Network *********************)
TwitterBuildnetwork[prop_,id_,par_]:=Module[
	{params=par,names, rootid, rootname, v, len, res1, res2, a, edges, res, vertices,vertprop,edgeprop,OAuthClient`$CacheResults=True,twitterid,screenname},
	(* Get vertices *)
	vertprop=Switch[prop,
		"FollowerMentionNetwork"|"FollowerNetwork"|"FollowerReplyToNetwork","FollowerIDs",
		"FriendMentionNetwork"|"FriendNetwork"|"FriendReplyToNetwork","FriendIDs",
		"SearchNetwork" | "SearchReplyToNetwork" | "SearchMentionNetwork","UserIDSearch"
	];
	edgeprop=Switch[prop,
		"FollowerMentionNetwork"|"FriendMentionNetwork"|"SearchMentionNetwork","UserMentions",
		"FollowerReplyToNetwork"|"FriendReplyToNetwork"|"SearchReplyToNetwork", "UserReplies",
		"FriendNetwork","FriendIDs",
		"FollowerNetwork"|"SearchNetwork","FollowerIDs"
	];
	params = Association@params;
	params["Elements"] = "RawData";
	params = Normal@params;

    v = TwitterOAuth`Private`twittercookeddata[vertprop,id,params];

    (* get user names and prepend the root user to the list *)
    If[v === $Canceled, Return[$Canceled]];
    (
	    names = TwitterGetscreennames[id,v];
	    If[!ListQ[names], Return[$Failed]];
	    (
	    	If[!StringMatchQ[prop,"Search*"],
	    		twitterid=Lookup[params, "user_id", Null];
				screenname = Lookup[params, "screen_name", Null];

				If[twitterid =!= Null,
					res1 = TwitterOAuth`Private`twittercookeddata["UserData",id,"user_id"->twitterid];
					If[!ListQ[res1], Return[$Failed]];
					{rootid, rootname} = {"ID", "Name"} /. First[res1];
					v     = Join[{ToString[rootid]}, v];
					names = Join[{rootname}, names]
				,
					If[screenname =!= Null,
						res1 = TwitterOAuth`Private`twittercookeddata["UserData",id,"screen_name"->screenname];
						If[!ListQ[res1], Return[$Failed]];
						{rootid, rootname} = {"ID", "Name"} /. First[res1];
						v     = Join[{ToString[rootid]}, v];
						names = Join[{rootname}, names]
					];
				];
		   	];
	    	If[Length[v]===1,
	    		vertices = MapThread[Property[#1, "Name" -> #2] &, {v, names}];
	    		res = Graph[vertices, {}];
	    		Return[res];
	    	];
	    	len   = Min[TwitterStyleLimit[], Length[v]];
	    	v     = Take[v,UpTo[len]];
	    	names = Take[names,UpTo[len]];
	    	(* Get the outgoing edges from each vertex *)
	    	res2=(Catch[TwitterOAuth`Private`twittercookeddata[edgeprop,id,{"user_id"->#,"Elements"->"RawData"}]]&/@v)/.($Failed|Missing|Missing[__])->{};
			(*res2 = Normal@res2;*)
			If[edgeprop==="UserMentions",
		       	(* Bug 255746 work around
		        res2=TwitterGetuserids[id,#]&/@res2;
				*)
				res2=StringReplace[StringJoin[Riffle[#,","]],"@"->""]&/@res2;
				res2=With[{
					rawdata=OAuthClient`rawoauthdata[id,"RawUsers","screen_name"->#]
					},
		       	StringCases[rawdata, RegularExpression["(?ms)\"id\":\\s*(\\d+)"] -> "$1"]
					]&/@res2;

	       	];
			(* End work around *)
			edges = Flatten[Table[a = Complement[Intersection[v, res2[[i]]], {v[[i]]}];
				If[a != {}, DirectedEdge[v[[i]], #] & /@ a, {}], {i, len}]];
			vertices = MapThread[Property[#1, "Name" -> #2] &, {v, names}];
			res = Graph[vertices, edges];
			res=SetProperty[res, VertexLabels -> MapThread[#1 -> Placed[#2, Tooltip] &, {v, names}]];
			res /; GraphQ[res]
		) /; (names =!= $Failed)
	) /; (v =!= $Failed)
]
TwitterBuildnetwork[___]:=$Failed

TwitterGetscreennames[_,{}]:={}
TwitterGetscreennames[id_,twitterids_]:=TwitterGetscreennames[_,twitterids]=Flatten[Lookup[#,"Name",{}]&/@TwitterOAuth`Private`twittercookeddata["UserData",id,"user_id"->twitterids]]

TwitterGetuserids[_,{}]:={}
TwitterGetuserids[id_,screennames_List]:= TwitterGetuserids[_,screennames]=Flatten[Lookup[#,"ID",{}]&/@TwitterOAuth`Private`twittercookeddata["UserData",id,"user_id"->twitterids]]

TwitterGetallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.TwitterOAuth`Private`twitterdata[str]], ("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

TwitterPaginationCalls[id_,prop_,np_,pagesize_] := Module[
    {calls,residual,progress=0,rawdata,totalresults,maxid,items = {},params = p,newParams = np,count=FromDigits[np["count"]]}, calls = Quotient[count, pagesize+1];
    residual = count - (calls*pagesize);

    PrintTemporary[ProgressIndicator[Dynamic[progress], {0, calls}]];
    If[ calls > 0,
        (
          (
            rawdata = TwitterOAuth`Private`twitterimport[OAuthClient`rawoauthdata[id,prop,Normal[newParams]]];
            If[ListQ[rawdata],
                rawdata = <|"statuses"->rawdata,"search_medatada"->{}|>;
            ];
			tweets = rawdata["statuses"];
			metadata = rawdata["search_metadata"];
            totalresults = Length@tweets;
            items = Join[items, tweets];
            progress = progress +1;
            If[prop==="RawTweetSearch",
                maxid = TGetMaxID[metadata]
            ,
                maxid = ToString[Last[items]["id"] - 1];
            ];

			If[!MatchQ[maxid,_Missing],
				newParams = Join[newParams,<|"max_id"->maxid|>];
				newParams["count"] = ToString[count-pagesize];
				count = count-pagesize;
			]
          )& /@ Range[0,calls-1]
        )
    ];
    If[ residual > 0,
        (

            newParams["count"] = ToString[residual];
    		rawdata = TwitterOAuth`Private`twitterimport[OAuthClient`rawoauthdata[id,prop,Normal[newParams]]];
            If[ListQ[rawdata],
                rawdata = <|"statuses"->rawdata,"search_medatada"->{}|>;
            ];
    		tweets = rawdata["statuses"];
    		metadata = rawdata["search_metadata"];
            totalresults = Length@tweets;
            items = Join[items, tweets]
        )
    ];
    items
]

TGetMaxID[metadata_]:=Quiet[First[StringCases[metadata["next_results"],Shortest[___~~"max_id="~~x__~~"&"]:>x]]]

TwitterFilterParameters[given:{(_Rule|_RuleDelayed)...},accepted_,separators_:{"_"}]:=Module[{camel=TwitterCamelCase[accepted,separators]},
	Cases[given,HoldPattern[Rule|RuleDelayed][Alternatives@@Join[accepted, camel],_],Infinity]/.Thread[camel->accepted]
]
TwitterFilterParameters[___]:=Throw[$Failed]

TwitterCamelCase[l_List, rest___]:=TwitterCamelCase[#,rest]&/@l
TwitterCamelCase[str_String, separators_:{"_"}]:=StringReplace[
 StringReplace[
  StringReplace[str,
   Thread[separators -> " "]], {WordBoundary ~~ word_ :>
    ToUpperCase[word]}], {"Id"~~WordBoundary->"ID",WhitespaceCharacter -> "","Url"~~WordBoundary->"URL","Urls"~~WordBoundary->"URLs"}]

TwitterFormatvalue[_[label_,fun_]]:=(Rule[label,value_]:>Rule[label,fun[value]])

TwitterReadDate[date_, form_: DateObject] := Block[
  {},
  	If[form === DateObject,
   		form[
    StringReplace[
     date, (RegularExpression["[+-]"] ~~ DigitCharacter .. ~~ " ") ->
      ""], TimeZone -> 0] (* Twitter works with UTC *)
   	,
   		form[
    StringReplace[
     date, (RegularExpression["[+-]"] ~~ DigitCharacter .. ~~ " ") ->
      ""]]
   	]
  ]

TwitterEventTimeline[data_, times_] := DateListPlot[MapThread[Tooltip[{#, 1}, #2] &, {times, data}], Filling -> Axis, FrameTicks -> {None, {Automatic, Automatic}}, Joined -> False]

$loadentitydata=False;
TwitterUserdataparse[x_,"location"]:=If[$loadentitydata,If[x===Null || x==="", Missing["NotAvailable"], WolframAlpha[x, "MathematicaResult"]/.Missing[___]:>(x)],x]
TwitterUserdataparse[x_,_]:=x

TwitterFormatgeoposition[pos_]:={"lat"->ToString[QuantityMagnitude@Latitude[pos]],"long"->ToString[QuantityMagnitude@Longitude[pos]]}

TwitterPrettygrid[args___]:=Grid[args, Background -> {None, {Lighter[Yellow, .9], {White,
    Lighter[Blend[{Blue, Green}], .8]}}}, Dividers -> {{Darker[
    Gray, .6], {Lighter[Gray, .5]},
   Darker[Gray, .6]}, {Darker[Gray, .6], Darker[Gray, .6], {False},
   Darker[Gray, .6]}}, Alignment -> {{Left, Right, {Left}}}]

TwitterFormatValueDate["created_at"->b_]:="created_at"->TwitterReadDate[b]
TwitterFormatValueDate[a_Association]:=Association[TwitterFormatValueDate /@ (Normal@a)]
TwitterFormatValueDate[x_]:=x

TwitterToString[l_List]:=TwitterToString /@ l
TwitterToString[a_Association]:=TwitterToString /@ l
TwitterToString[n_?NumberQ]:=ToString[n]
TwitterToString[etc_]:=etc

TwitterStyleLimit[]:=50;

TwitterFormatByElementType[res_, opt_]:=Block[
	{data,dataset,count=Length[res],imgSize,options=opt},

	If[!KeyExistsQ[options, "Elements"] || FreeQ[{"Text", "Images", "Data", "Default", Default, "FullData"}, options["Elements"]],
		options["Elements"] = Default;
	];

	Switch[options["Elements"],
		"Text",
			If[!KeyExistsQ[options, "ShowIDs"] || FreeQ[{True, False}, options["ShowIDs"]],
				options["ShowIDs"] = False;
			];
		,

		"Images",
			If[!KeyExistsQ[options, "MediaResolution"] || FreeQ[{"Thumbnail", "Low", "Standard", "Large"}, options["MediaResolution"]],
				options["MediaResolution"] = "Standard";
			];

			imgSize = options["MediaResolution"] /. "Thumbnail" -> "thumb" /. "Low" -> "small" /. "High" -> "large" /. ("Standard" | options["MediaResolution"]) -> "medium";
		,

		"FullData"|"Default"|Default|"Data"|_,
			If[!KeyExistsQ[options, "ShowThumbnails"] || (options["ShowThumbnails"] =!= True && options["ShowThumbnails"] =!= False),
				options["ShowThumbnails"] = False;
			];
	];

	data = Block[
		{var = #, tmp},

		tmp = var;
		Switch[options["Elements"],
			"Images"|"Default"|Default|"Data"|"FullData",
				If[!KeyExistsQ[var["entities"], "media"],
					AssociateTo[var["entities"], "media" -> Null];
					tmp = var;
				];

				If[options["Elements"] === "Images",
					If[tmp["entities"]["media"] === Null,
						tmp["images"] = {};
					,
						tmp["images"] = Import[(#["media_url_https"] <> ":" <> imgSize)] & /@ tmp["extended_entities"]["media"];
					];
				,

					tmp["username"] = tmp["user"]["screen_name"];
					tmp["created_at"] = TwitterReadDate[tmp["created_at"]];
					tmp["url"] = "https://twitter.com/" <> tmp["user"]["screen_name"] <> "/status/" <> tmp["id_str"];

					If[tmp["entities"]["media"] === Null,
						If[options["ShowThumbnails"],
							tmp["thumbnails"] = Missing["NotAvailable"];
						,
							tmp["thumbnailsURL"] = {};
						];
					,
						If[options["ShowThumbnails"],
							tmp["thumbnails"] = Import[(#["media_url_https"] <> ":thumb")] & /@ tmp["extended_entities"]["media"];
						,
							tmp["thumbnailsURL"] = (#["media_url_https"] <> ":thumb") & /@ tmp["extended_entities"]["media"];
						];
					];

					If[options["Elements"] === "FullData",
						tmp["hashtags"] = #["text"]& /@ tmp["entities"]["hashtags"];
						tmp["name"] = tmp["user"]["name"];
						tmp["profileImageThumbnail"] = If[options["ShowThumbnails"], Import[tmp["user"]["profile_image_url_https"]], tmp["user"]["profile_image_url_https"]];
						tmp["location"] = If[tmp["geo"] === Null, Missing["NotAvailable"], GeoPosition[{tmp["geo"]["coordinates"][[1]],tmp["geo"]["coordinates"][[2]]}]];
					];
				];
		];

		tmp
	] & /@ Flatten[{res}];

	Switch[options["Elements"],
		"Text",
			If[options["ShowIDs"],
				dataset=Dataset[data];
				dataset[1 ;; count, <|"Text" -> "text", "ID"->"id"|>]
			,
				#["text"] & /@data
			]
		,

		"FullData",
			dataset=Dataset[data];
			dataset[1 ;; count, <|"Text" -> "text", "ID"->"id", (If[options["ShowThumbnails"], "Thumbnails" -> "thumbnails", "ThumbnailsURL" -> "thumbnailsURL"]), "Date" -> "created_at",
			"Hashtags" -> "hashtags", "Location" -> "location", "Username" -> "username", "Name" -> "name", "ProfileImageThumbnail" -> "profileImageThumbnail",
			"RetweetCount" -> "retweet_count", "FavoriteCount" -> "favorite_count", "URL" -> "url"|>]
		,

		"Images",
			dataset=Dataset[data];
			Normal[dataset[1 ;; count, "images"]]
		,

		"Default"|Default|"Data"|_,
			dataset=Dataset[data];
			dataset[1 ;; count, <|"Text" -> "text", "ID"->"id", (If[options["ShowThumbnails"], "Thumbnails" -> "thumbnails", "ThumbnailsURL" -> "thumbnailsURL"]), "Username"->"username", "Date" -> "created_at", "URL" -> "url"|>]
	]
]

End[];

EndPackage[];
