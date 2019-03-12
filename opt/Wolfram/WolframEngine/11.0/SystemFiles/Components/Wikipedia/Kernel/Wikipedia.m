
Begin["Wikipedia`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* Wikipedia *************************************)

(* Authentication information *)

wikipediadata[] = {
		"ServiceName" 		-> "Wikipedia", 
	 	"RawGets"			-> 
	 		{
	 			"RawArticleContributors",
	 			"RawArticleOpenSearch",
	 			"RawArticlePlaintext",
	 			"RawArticleWikicode",
	 			"RawBacklinksRules",
	 			"RawCategoryArticles",
	 			"RawCategoryMembers",
	 			"RawCategorySearch",
	 			"RawContentSearch",
	 			"RawContributorArticles",
	 			"RawExternalLinks",
	 			"RawGeoNearbyArticles",
	 			"RawGeoPosition",
	 			"RawLanguagesURLRules",
	 			"RawLinksRules",
	 			"RawParentCategories",
	 			"RawRevisions",
	 			"RawSummaryWikicode",
	 			"RawTitleSearch",
	 			"RawWikipediaRecentChanges"
	 		},
	 	"Gets"				-> 
	 		{
	 			"ArticleContributors",
	 			"ArticleOpenSearch",
	 			"ArticlePlaintext",
	 			"ArticleWikicode",
				"BacklinksList",
				"BacklinksRules",
	 			"CategoryArticles",
				"CategoryArticleIDs",
	 			"CategoryLinks",
	 			"CategoryMembers",
	 			"CategoryMemberIDs",
	 			"CategorySearch",
	 			"ContentSearch",
	 			"ContributorArticles",
	 			"ExternalLinks",
	 			"GeoNearbyArticles",
	 			"GeoNearbyDataset",
	 			"GeoPosition",
	 			"ImageDataset",
	 			"ImageList",
	 			"ImageURLs",
	 			"LanguagesList",
	 			"LanguagesURLRules",
	 			"LanguagesURLs",
	 			"LinksRules",
	 			"LinksList",
	 			"PageID",
	 			"ParentCategories",
	 			"Revisions",
	 			"SeeAlsoList",
	 			"SeeAlsoRules",
				"SummaryPlaintext",
	 			"SummaryWikicode",
	 			"Tables",
	 			"Title",
	 			"TitleSearch",
				"TitleTranslationRules",
	 			"TitleTranslations",
	 			"WikipediaRecentChanges"
	 		},
	 	"Posts"				-> {},
	 	"RawPosts"			-> {},
 		"Information"		-> "Import Wikipedia API data to the Wolfram Language"
	}

formats = {
		"dbg",
		"dbgfm",
		"dump",
		"dumpfm",
		"json",
		"jsonfm",
		"php",
		"phpfm",
		"txt",
		"txtfm",
		"wddx",
		"wddxfm",
		"xml",
		"xmlfm",
		"yaml",
		"yamlfm"
	};

namespaceTranslation = {
		0->"(Main/Article)",
		1->"Talk",
		2->"User",
		3->"User talk",
		4->"Wikipedia",
		5->"Wikipedia talk",
		6->"File",
		7->"File talk",
		8->"MediaWiki",
		9->"MediaWiki talk",
		10->"Template",
		11->"Template talk",
		12->"Help",
		13->"Help talk",
		14->"Category",
		15->"Category talk",
		100->"Portal",
		101->"Portal talk",
		108->"Book",
		109->"Book talk",
		118->"Draft",
		119->"Draft talk",
		446->"Education Program",
		447->"Education Program talk",
		710->"TimedText",
		771->"TimedText talk",
		828->"Module",
		829->"Module talk",
		2600->"Topic"
	};
   
(********************************************************************************************************************************************)

wikiIDtoTitle2[ids_List] := Module[{result, buffer, remainingTitles},
	result={};
	If[MatchQ[ids,{__String}],
		remainingTitles=ids;
		buffer=StringJoin@Riffle[remainingTitles,"|"];
		buffer=ImportString[URLFetch["http://en.wikipedia.org/w/api.php",
			"Parameters"->{"action"->"query","prop"->"info","format"->"json","pageids"->buffer}],"json"];
		buffer=Cases[buffer,Rule["query",{Rule["pages",a_],___}]->a,10];
		buffer=Cases[buffer,{___,Rule["pageid",_],___},10];
		buffer=If[MatchQ[#[[2]],"missing"],ToString[#[[1]]]->#[[3]],ToString[#[[1]]]->Missing["TitleNotAvailable"]]&/@({"pageid","missing","title"}/.buffer);
		remainingTitles=Complement[remainingTitles,buffer[[All,1]]];
		AppendTo[result, buffer];
		While[remainingTitles=!={},
			buffer=StringJoin@Riffle[remainingTitles,"|"];
			buffer=ImportString[URLFetch["http://en.wikipedia.org/w/api.php",
				"Parameters"->{"action"->"query","prop"->"info","format"->"json","pageids"->buffer}],"json"];
			buffer=Cases[buffer,Rule["query",{Rule["pages",a_],___}]->a,10];
			buffer=Cases[buffer,{___,Rule["pageid",_],___},10];
			buffer=If[MatchQ[#[[2]],"missing"],ToString[#[[1]]]->#[[3]],ToString[#[[1]]]->Missing["TitleNotAvailable"]]&/@({"pageid","missing","title"}/.buffer);
			remainingTitles=Complement[remainingTitles,buffer[[All,1]]];
			AppendTo[result, buffer];
		];
		result=Flatten[result];
		result=ids/.result,
		result=Missing["InvalidInput"]
	];
	result
	];

titleMapping2[arg_] := Block[{title,pageid,result,replacementRules},
	title=Lookup[arg,"Title",Missing["Title"]];
	pageid=Lookup[arg,"PageID",Missing["PageID"]];
	result={};
	Which[
		!MatchQ[title,_Missing],
		
		If[!MatchQ[title,_List],title={title}];
		(*solving Entity wrappers*)
		title=Which[MatchQ[#,_String],#,MatchQ[#,_Entity],#,True,Missing["InvalidTitleFormat"]]&/@title;
		title=title/.x_Entity :> Quiet[Check[x["WikipediaEnID"], $Failed]];
		title=Which[MatchQ[#,_String],#,MatchQ[#,{_String}],If[StringMatchQ[#[[1]],RegularExpression["[0-9]+"]],#,Missing["InvalidPageID"]],MatchQ[#,_Missing],#,True,Missing["EntityNotLinkedToWikipedia"]]&/@title;
		replacementRules=Select[title,MatchQ[#,{_String}]&];
		If[replacementRules=!={},
			replacementRules=Thread[Rule[replacementRules,wikiIDtoTitle2[Flatten[replacementRules]]]];
			title=title/.replacementRules;
		];
		(*validating title shape*)
		title=If[MatchQ[#,_String],If[StringMatchQ[#,RegularExpression["[^|]+"]],#,Missing["SeparatorInsideTitle"]],#]&/@title;
		result=title,
		
		!MatchQ[pageid,_Missing],
		
		If[!MatchQ[pageid,_List],pageid={pageid}];
		pageid=Which[MatchQ[#,_String],If[StringMatchQ[#,RegularExpression["[0-9]+"]],#,Missing["InvalidPageID"]],MatchQ[#,_Integer],ToString[#],True,Missing["InvalidPageID"]]&/@pageid;
		(*mapping pageids to titles*)
		replacementRules=Select[pageid,MatchQ[#,_String]&];
		If[replacementRules=!={},
			replacementRules=Thread[Rule[replacementRules,wikiIDtoTitle2[Flatten[replacementRules]]]];
			pageid=pageid/.replacementRules;
		];
		result=pageid,
		
		True,
		
		result=Missing["ArticleParameterNotAvailable"]
	];
	result
];
	
(****************************************************************************************************)

wikipediarawdata["RawArticleContributors",arg_] := Block[{result,title,limit,format,continue,parameters},
	(*Get the list of logged-in contributors and the count of anonymous contributors to a page*)
	title=Lookup[arg,"Title",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems","max"];
	format=Lookup[arg,"Format","json"];
	continue=Lookup[arg,"Continue",Missing["NotAvailable"]];
	
	If[MatchQ[title,_String]||MatchQ[title,{__String}],title=StringReplace[title," "->"_"],Throw[$Failed]];
	If[MatchQ[title,{__String}],title=StringJoin[Riffle[title,"|"]]];
	If[MatchQ[limit,_Integer],limit=ToString[limit],If[limit=!="max",Throw[$Failed]]];
	
	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];

	parameters={
		"action"->"query",
		"prop"->"contributors",
		"continue"->"",
		"format"->format,
		"pclimit"->limit,
		"titles"->title,
		"redirects"->""
		};

	If[MatchQ[continue,_String],AppendTo[parameters,Rule["pccontinue",continue]]];

	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result
	]

wikipediacookeddata["ArticleContributors",arg_] := Block[{result,title,limit,buffer,paging},
	(*Get the list of logged-in contributors and the count of anonymous contributors to a page*)
	title=titleMapping2[arg];

	If[MatchQ[title,_Missing],result=title];
	
	If[MatchQ[title,{__}],
		result={};

		limit=Lookup[arg,"MaxItems",100];
		If[!MatchQ[limit,_Integer]&&!MatchQ[limit,All],Throw[$Failed]];
		
		buffer=If[MatchQ[#,_String],wikipediarawdata["RawArticleContributors",{"Title"->#}],#]&/@title;
		buffer=If[MatchQ[#,_String],ImportString[#,"json"],#]&/@buffer;
		
		paging=Cases[#,Rule["pccontinue",a_String]->a,10]&/@buffer;
		paging=paging/.{a_String}->a;
		
		buffer=If[MatchQ[#,_Missing],#,Cases[#,Rule["query",{___,Rule["pages",a__],___}]->a,10]]&/@buffer;
		buffer=If[MatchQ[#,_Missing],#,"contributors"/.Flatten[Cases[#,{___,Rule["title",_],___},10]]]&/@buffer;
		buffer=If[MatchQ[#,_Missing],#,If[#=!="contributors","name"/.#,Missing["NotAvailable"]]]&/@buffer;

		AppendTo[result,#]&/@buffer;
	 	
		If[MatchQ[limit,_Integer],
			While[Or@@((#=!={})&/@paging)&&And@@((#<=limit)&/@(Length/@result)),
				buffer=If[MatchQ[#[[1]],_Missing],#,If[MatchQ[#[[2]],_String],wikipediarawdata["RawArticleContributors",{"Title"->#[[1]],"Continue"->#[[2]]}],#[[2]]]]&/@Transpose[{title,paging}];
				buffer=If[MatchQ[#,_String],ImportString[#,"json"],#]&/@buffer;
				
				paging=Cases[#,Rule["pccontinue",a_String]->a,10]&/@buffer;
				paging=paging/.{a_String}->a;
				
				buffer=If[MatchQ[#,_Missing],#,Cases[#,Rule["query",{___,Rule["pages",a__],___}]->a,10]]&/@buffer;
				buffer=If[MatchQ[#,_Missing],#,"contributors"/.Flatten[Cases[#,{___,Rule["title",_],___},10]]]&/@buffer;
				buffer=If[MatchQ[#,_Missing],#,If[#=!="contributors","name"/.#,Missing["NotAvailable"]]]&/@buffer;
				
				result=Flatten[#]&/@Transpose[{result,buffer}];
		 	];
			result=If[Length[#]>limit,#[[1;;limit]],#]&/@result
		];

		If[MatchQ[limit,All],
			While[Or@@((#=!={})&/@paging),
				buffer=If[MatchQ[#[[1]],_Missing],#,If[MatchQ[#[[2]],_String],wikipediarawdata["RawArticleContributors",{"Title"->#[[1]],"Continue"->#[[2]]}],#[[2]]]]&/@Transpose[{title,paging}];
				buffer=If[MatchQ[#,_String],ImportString[#,"json"],#]&/@buffer;
				
				paging=Cases[#,Rule["pccontinue",a_String]->a,10]&/@buffer;
				paging=paging/.{a_String}->a;
				
				buffer=If[MatchQ[#,_Missing],#,Cases[#,Rule["query",{___,Rule["pages",a__],___}]->a,10]]&/@buffer;
				buffer=If[MatchQ[#,_Missing],#,"contributors"/.Flatten[Cases[#,{___,Rule["title",_],___},10]]]&/@buffer;
				buffer=If[MatchQ[#,_Missing],#,If[#=!="contributors","name"/.#,Missing["NotAvailable"]]]&/@buffer;
				
				result=Flatten[#]&/@Transpose[{result,buffer}];
		 	];
		];

	 	result=If[MatchQ[#,{__Missing}],First@#,#]&/@result;
		result=If[MatchQ[#,_List],Select[#,!MatchQ[#,_Missing]&],#]&/@result;
		If[Length[result]==1,result=First@result];
	];
	
	result
	]

(****************************************************************************************************)

wikipediarawdata["RawArticleOpenSearch",arg_] := Block[{result,search,format,parameters},
	(*Search the wiki using the OpenSearch protocol*)
	search=Lookup[arg,"Search",Throw[$Failed]];
	format=Lookup[arg,"Format","json"];
	
	If[MatchQ[search,_String],search=StringReplace[search," "->"_"],Throw[$Failed]];

	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];

	parameters={
		"limit"->"max",
		"action"->"opensearch",
		"format"->format,
		"search"->search
		};

	result=FromCharacterCode[URLFetch["http://en.wikipedia.org/w/api.php","ContentData","Parameters"->parameters],"UTF8"];
	result
	]

wikipediacookeddata["ArticleOpenSearch",arg_] := Block[{result,search,limit},
	(*Search the wiki using the OpenSearch protocol*)
	search=Lookup[arg,"Search",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems",Missing["NotAvailable"]];
	
	If[!MatchQ[limit,_Integer]&&!MatchQ[limit,_Missing],Throw[$Failed]];

	result=wikipediarawdata["RawArticleOpenSearch",{"Search"->search,"Format"->"xml"}];
	result=Cases[ImportString[result,"xml"],XMLElement["Item",___],10];
	result={Cases[#,XMLElement["Text",_,{a_}]->a,10],Cases[#,XMLElement["Description",_,{a_}]->a,10]}&/@result;
	result={"Title"->#[[1,1]],"Snippet"->(#[[2]]/.{{t_String}->t,{}->Missing["NotAvailable"]})}&/@result;
	If[MatchQ[limit,_Integer],
		If[limit<Length[result],result=result[[1;;limit]]]
	];
	result=Association@@@result;
	result
	]

(****************************************************************************************************)

wikipediarawdata["RawArticlePlaintext",arg_] := Block[{result,title,format,continue,parameters},
	(*Extract and cleans wikipedia articles*)
	title=Lookup[arg,"Title",Throw[$Failed]];
	format=Lookup[arg,"Format","json"];
	continue=Lookup[arg,"Continue",Missing["NotAvailable"]];
	
	If[MatchQ[title,_String]||MatchQ[title,{__String}],title=StringReplace[title," "->"_"],Throw[$Failed]];
	If[MatchQ[title,{__String}],title=StringJoin[Riffle[title,"|"]]];

	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];

	parameters={
		"action"->"query",
		"prop"->"extracts",
		"continue"->"",
		"format"->format,
		"redirects"->"true",
		"titles"->title,
		"explaintext"->"",
		"redirects"->""
		};

	If[MatchQ[continue,_String],AppendTo[parameters,Rule["excontinue",continue]]];

	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result
	];

wikipediacookeddata["ArticlePlaintext",arg_] := Block[{result,title,buffer},
	(*Extract and cleans wikipedia articles*)
	title=titleMapping2[arg];

	If[MatchQ[title,_Missing],result=title];
	
	If[MatchQ[title,{__}],
		result={};
		
		buffer=If[MatchQ[#,_String],wikipediarawdata["RawArticlePlaintext",{"Title"->#}],#]&/@title;
		buffer=If[MatchQ[#,_String],ImportString[#,"json"],#]&/@buffer;

		buffer=If[MatchQ[#,_Missing],#,Cases[#,Rule["query",{___,Rule["pages",a__],___}]->a,10]]&/@buffer;
		buffer=If[MatchQ[#,_Missing],#,"extract"/.Flatten[Cases[#,{___,Rule["title",_],___},10]]]&/@buffer;
		buffer=If[MatchQ[#,_Missing],#,If[#=!="extract",#,Missing["NotAvailable"]]]&/@buffer;
		result=buffer;
		If[Length[result]==1,result=First@result]
	];

	result
	];

(****************************************************************************************************)

wikipediarawdata["RawArticleWikicode",arg_] := Block[{result,title,format,parameters},
	(*Get revision information - wikicode in this case*)
	title=Lookup[arg,"Title",Throw[$Failed]];
	format=Lookup[arg,"Format","json"];
	
	If[MatchQ[title,_String]||MatchQ[title,{__String}],title=StringReplace[title," "->"_"],Throw[$Failed]];
	If[MatchQ[title,{__String}],title=StringJoin[Riffle[title, "|"]]];

	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];

	parameters={
		"continue"->"",
		"action"->"query",
		"format"->format,
		"prop"->"revisions",
		"rvprop"->"content",
		"titles"->title,
		"redirects"->""
		};

	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result
	]

wikipediacookeddata["ArticleWikicode",arg_] := Block[{result,title,titleOrder},
	(*Get revision information - wikicode in this case*)
	title=titleMapping2[arg];

	If[MatchQ[title,_Missing],result=title];

	If[MatchQ[title,{__}],
		result={};
		
		result=If[MatchQ[#,_String],wikipediarawdata["RawArticleWikicode",{"Title"->#}],#]&/@title;
		result=If[MatchQ[#,_String],ImportString[#,"json"],#]&/@result;
		result=If[MatchQ[#,_Missing],#,Flatten@Cases[#,{___,Rule["title",_],___},10]]&/@result;

		result=If[MatchQ[#,_Missing],#,Flatten@({"revisions"}/.#)]&/@result;
		result=If[MatchQ[#,_Missing],#,If[MatchQ[#,{__Rule}],"*"/.#,Missing["NotAvailable"]]]&/@result;
		
		If[Length[result]==1,result=First@result]
	];

	result
	]

(****************************************************************************************************)

wikipediarawdata["RawBacklinksRules",arg_] := Block[{result,title,limit,format,continue,parameters},
	(*Find all pages that link to the given page*)
	title=Lookup[arg,"Title",Throw[$Failed]];
	limit=Lookup[arg,"MaxLevelItems","max"];
	format=Lookup[arg,"Format","json"];
	continue=Lookup[arg,"Continue",Missing["NotAvailable"]];
	
	If[MatchQ[title,_String],title=StringReplace[title," "->"_"],Throw[$Failed]];
	If[MatchQ[limit,_Integer],limit=ToString[limit],If[limit=!="max",Throw[$Failed]]];
	
	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];

	parameters={
		"action"->"query",
		"list"->"backlinks",
		"continue"->"",
		"format"->format,
		"bllimit"->limit,
		"blnamespace"->"0",
		"bltitle"->title,
		"redirects"->""
		};

	If[MatchQ[continue,_String],AppendTo[parameters,Rule["blcontinue",continue]]];

	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result
	]

Backlinks[title_,limit_] := Module[{result,buffer,paging},
	result={};
	
	buffer=wikipediarawdata["RawBacklinksRules",{"Title"->title}];
	buffer=ImportString[buffer,"json"];

	paging=Cases[buffer,Rule["blcontinue",a_String]->a,10];
	If[MatchQ[paging,{_String}],paging=paging[[1]]];

 	buffer=Cases[buffer,Rule["backlinks",{a__List}]->a,10];
 	buffer="title"/.Select[buffer,MatchQ[#,{_,_,_}]&];
 	
 	AppendTo[result,#]&/@buffer;
 	
 	If[MatchQ[limit,_Missing],
 		While[paging=!={},
			buffer=wikipediarawdata["RawBacklinksRules",{"Title"->title,"Continue"->paging}];
			buffer=ImportString[buffer,"json"];
		
			paging=Cases[buffer,Rule["blcontinue",a_String]->a,10];
			If[MatchQ[paging,{_String}],paging=paging[[1]]];
		
		 	buffer=Cases[buffer,Rule["backlinks",{a__List}]->a,10];
		 	buffer="title"/.Select[buffer,MatchQ[#,{_,_,_}]&];
		 	
		 	AppendTo[result,#]&/@buffer;
 		],
 		If[limit<=Length[result],
 			result=result[[1;;limit]],
 			While[paging=!={}&&limit>Length[result],
				buffer=wikipediarawdata["RawBacklinksRules",{"Title"->title,"Continue"->paging}];
				buffer=ImportString[buffer,"json"];
				
				paging=Cases[buffer,Rule["blcontinue",a_String]->a,10];
				If[MatchQ[paging,{_String}],paging=paging[[1]]];
			
			 	buffer=Cases[buffer,Rule["backlinks",{a__List}]->a,10];
			 	buffer="title"/.Select[buffer,MatchQ[#,{_,_,_}]&];
			 	
			 	AppendTo[result,#]&/@buffer;
	 		];
	 		If[limit<=Length[result],result=result[[1;;limit]]]
 		]
 	];

	result=Union[result];
	result
	]

BackLinksTree[title_,limit_,level_] := Module[{result,nodes,newNodes,buffer,i},
	nodes={title};
	newNodes={};
	result={};
	Do[
		newNodes=Backlinks[#,limit]&/@nodes;
		buffer=Thread[List[nodes,newNodes]];
		newNodes=Union[Flatten[newNodes]];
		buffer=Thread[Rule[#[[2]],#[[1]]]]&/@buffer;
		AppendTo[result,buffer];
		nodes=newNodes;
		,{i,1,level}];
	result=Flatten[result];
	result=Select[result,!MatchQ[#,Rule[a_,a_]]&];
	result
	]

wikipediacookeddata["BacklinksRules",arg_] := Block[{result,title,limit,level},
	(*Find all pages that link to the given page*)
	title=titleMapping2[arg];

	If[MatchQ[title,_Missing],result=title];

	If[MatchQ[title,{__}],
		limit=Lookup[arg,"MaxLevelItems",Missing["NotAvailable"]];
		level=Lookup[arg,"MaxLevel",1];

		If[!MatchQ[limit,_Integer]&&!MatchQ[limit,_Missing],Throw[$Failed]];
		If[!MatchQ[level,_Integer],Throw[$Failed]];

		result=If[MatchQ[#,_String],BackLinksTree[#,limit,level],#]&/@title;
		If[Length[result]==1,result=First@result]
	];
	result/.{s_String:>StringReplace[s,"_"->" "]}
	]

wikipediacookeddata["BacklinksList",arg_] := Block[{result},
	(*Find all pages that link to the given page*)
	result=wikipediacookeddata["BacklinksRules",arg];
	Which[
		MatchQ[result,{__Rule}],
		result=result[[All,1]],
		MatchQ[result,{{__Rule}..}],
		result=#[[All,1]]&/@result
	];
	result/.{s_String:>StringReplace[s,"_"->" "]}
	]

(****************************************************************************************************)

wikipediarawdata["RawCategoryArticles",arg_] := Block[{result,category,limit,format,continue,parameters},
	(*List all articles in a given category*)
	category=Lookup[arg,"Category",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems","max"];
	format=Lookup[arg,"Format","json"];
	continue=Lookup[arg,"Continue",Missing["NotAvailable"]];
	
	If[MatchQ[category,_String],category=StringReplace[category," "->"_"],Throw[$Failed]];
	If[MatchQ[category,_String],category=StringReplace[category,RegularExpression["(?i)^Category:(.+)"]->"$1"],Throw[$Failed]];
	
	If[MatchQ[limit,_Integer],limit=ToString[limit],If[limit=!="max",Throw[$Failed]]];
	
	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];
	
	parameters={
		"action"->"query",
		"list"->"categorymembers",
		"continue"->"",
		"format"->format,
		"cmlimit"->limit,
		"cmnamespace"->"0",
		"cmtitle"->"Category:"<>category
		};

	If[MatchQ[continue,_String],AppendTo[parameters,Rule["cmcontinue",continue]]];

	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result
	]

wikipediacookeddata["CategoryArticles",arg_] := Block[{result,category,limit,paging,buffer,showID},
	(*List all articles in a given category*)
	category=Lookup[arg,"Category",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems",All];
	showID=Lookup[arg,"ShowID",False];

	If[!MatchQ[category,_String]&&!MatchQ[category,{__String}],Throw[$Failed]];
	If[MatchQ[category,_String],category={category}];
	If[!MatchQ[limit,_Integer]&&!MatchQ[limit,All],Throw[$Failed]];

	result={};

	buffer=wikipediarawdata["RawCategoryArticles",{"Category"->#}]&/@category;
	buffer=ImportString[#,"json"]&/@buffer;

	paging=Cases[#,Rule["cmcontinue",a_String]->a,10]&/@buffer;
	paging=paging/.{a_String}->a;

 	buffer=Cases[#,Rule["categorymembers",{a__List}]->a,10]&/@buffer;
	If[showID,
		buffer="pageid"/.buffer,
		buffer="title"/.buffer;
	];

 	AppendTo[result,#]&/@buffer;
 	
	If[MatchQ[limit,All],
		While[Or@@((#=!={})&/@paging),
			buffer=If[#[[2]]=!={},wikipediarawdata["RawCategoryArticles",{"Category"->#[[1]],"Continue"->#[[2]]}],"{}"]&/@Transpose[{category,paging}];
			buffer=ImportString[#,"json"]&/@buffer;
		
			paging=Cases[#,Rule["cmcontinue",a_String]->a,10]&/@buffer;
			paging=paging/.{a_String}->a;
				
		 	buffer=Cases[#,Rule["categorymembers",{a__List}]->a,10]&/@buffer;
			If[showID,
				buffer="pageid"/.buffer,
				buffer="title"/.buffer;
			];
			
			result=Flatten[#]&/@Transpose[{result,buffer}];
		];
		result=If[MatchQ[#,{__}],Select[#,!MatchQ[#,"title"]&&!MatchQ[#,"pageid"]&],#]&/@result;
 		result=result/.{"title"->Missing["NotAvailable"],"pageid"->Missing["NotAvailable"],{}->Missing["NotAvailable"]};
 		If[MatchQ[result,{{__String}}]||MatchQ[result,{{__Integer}}],result=result[[1]]]
	];
	
	If[MatchQ[limit,_Integer],
 		If[And@@((#>=limit)&/@(Length/@result)),
 			result=#[[1;;limit]]&/@result,
 			While[Or@@((#=!={})&/@paging)&&And@@((#<limit)&/@(Length/@result)),
				buffer=If[#[[2]]=!={},wikipediarawdata["RawCategoryArticles",{"Category"->#[[1]],"Continue"->#[[2]]}],"{}"]&/@Transpose[{category,paging}];
				buffer=ImportString[#,"json"]&/@buffer;
			
				paging=Cases[#,Rule["cmcontinue",a_String]->a,10]&/@buffer;
				paging=paging/.{a_String}->a;
					
			 	buffer=Cases[#,Rule["categorymembers",{a__List}]->a,10]&/@buffer;
				If[showID,
					buffer="pageid"/.buffer,
					buffer="title"/.buffer;
				];
				
				result=Flatten[#]&/@Transpose[{result,buffer}];
				result=Select[#,!MatchQ[#,"title"]&&!MatchQ[#,"pageid"]&]&/@result
	 		];
	 		result=If[Length[#]>limit,#[[1;;limit]],#]&/@result;
	 		result=result/.{"title"->Missing["NotAvailable"],"pageid"->Missing["NotAvailable"],{}->Missing["NotAvailable"]};
	 	];
 		If[MatchQ[result,{{__String}}]||MatchQ[result,{{__Integer}}],result=result[[1]]]
 	];

	result
	]

wikipediacookeddata["CategoryArticleIDs",arg_] := Block[{result,category,limit},
	category=Lookup[arg,"Category",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems",All];

	If[!MatchQ[category,_String]&&!MatchQ[category,{__String}],Throw[$Failed]];
	If[!MatchQ[limit,_Integer]&&!MatchQ[limit,All],Throw[$Failed]];

	result={};
	If[MatchQ[limit,_Integer],
		result=wikipediacookeddata["CategoryArticles",{"Category"->category,"ShowID"->True,"MaxItems"->limit}],
		result=wikipediacookeddata["CategoryArticles",{"Category"->category,"ShowID"->True}]
 	];
	result
	]

(****************************************************************************************************)

RawCategoryExtraction[category_String,continue_] := Module[{query,result,parameters},
	result={};
	parameters={
		"continue"->"",
		"format"->"json",
		"action"->"query",
		"list"->"categorymembers",
		"cmtitle"->"Category:"<>StringReplace[StringReplace[StringTrim[category],RegularExpression["(?i)^category:(.+)"]->"$1"]," "->"_"],
		"cmsort"->"timestamp",
		"cmdir"->"desc",
		"cmlimit"->"max"
		};

	If[MatchQ[continue,_String],AppendTo[parameters,Rule["cmcontinue",continue]]];

	query=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
    result=ImportString[query,"json"];
    result
    ]

CategoryExtraction[category_String] := Module[{result,paging,buffer},
	result={};
	
	buffer=RawCategoryExtraction[category,{}];
	
	paging=Cases[buffer,Rule["cmcontinue",a_String]->a,10];
	paging=paging/.{a_String}->a;
	buffer=Cases[buffer,Rule["query",{Rule["categorymembers",{categories__}]}]->categories,10];

    If[Length[buffer]>0,
    	buffer=Select[buffer,("ns"/.#)==14&];
    	buffer=Select[buffer,MatchQ[#,{___,Rule["ns",14],___}]&];
    	If[buffer=!={},buffer="title"/.buffer];
   	];
   	AppendTo[result,buffer];
   	
   	While[paging=!={},
		buffer=RawCategoryExtraction[category,paging];
		
		paging=Cases[buffer,Rule["cmcontinue",a_String]->a,10];
		paging=paging/.{a_String}->a;
		buffer=Cases[buffer,Rule["query",{Rule["categorymembers",{categories__}]}]->categories,10];
	
	    If[Length[buffer]>0,
	    	buffer=Select[buffer,("ns"/.#)==14&];
	    	buffer=Select[buffer,MatchQ[#,{___,Rule["ns",14],___}]&];
	    	If[buffer=!={},buffer="title"/.buffer];
	   	];
	   	AppendTo[result,buffer];
   	];
   	
   	result=Union@Flatten[result];
   	result=Thread[Rule[category,result]];
    result
	]

CategoryExtraction[category_List] := Module[{result},
	result={};
	If[MatchQ[category,{__String}],
		result=CategoryExtraction[#]&/@category;
	];
	result
	]

wikipediacookeddata["CategoryLinks",arg_] := Module[{result,category,treeLevel,root,leaves},
	(*extracts the category tree up to certain level*)
	category=Lookup[arg,"Category",Throw[$Failed]];
	treeLevel=Lookup[arg,"MaxLevel",2];

	result={};
	
	If[MatchQ[category,_String]||MatchQ[category,{__String}],
		If[MatchQ[category,_String],category={category}],
		Throw[$Failed]
	];
	If[!MatchQ[treeLevel,_Integer],Throw[$Failed]];
	
	root=category;
	result={}&/@category;
	
	Do[
		leaves=CategoryExtraction[#]&/@root;
		result=Flatten[#]&/@Transpose[{result,leaves}];
		root=Flatten[#/.Rule[a_,b_]->b]&/@leaves;
	,{i,1,treeLevel}];
	
	result=Union[#]&/@result;
	If[Length[result]==1,result=First@result];

	result
	]

(****************************************************************************************************)

wikipediarawdata["RawCategoryMembers",arg_] := Block[{result,category,limit,format,continue,parameters},
	(*List all pages in a given category*)
	category=Lookup[arg,"Category",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems","max"];
	format=Lookup[arg,"Format","json"];
	continue=Lookup[arg,"Continue",Missing["NotAvailable"]];
	
	If[MatchQ[category,_String],category=StringReplace[category," "->"_"],Throw[$Failed]];
	If[MatchQ[limit,_Integer],limit=ToString[limit],If[limit=!="max",Throw[$Failed]]];

	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];

	parameters={
		"action"->"query",
		"list"->"categorymembers",
		"continue"->"",
		"format"->format,
		"cmlimit"->limit,
		"cmtitle"->("Category:"<>category),
		"cmprop"->"ids|title|sortkey|sortkeyprefix|type|timestamp",
		"cmsort"->"timestamp",
		"cmdir"->"desc"
		};

	If[MatchQ[continue,_String],AppendTo[parameters,Rule["cmcontinue",continue]]];

	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result
	]

wikipediacookeddata["CategoryMembers",arg_] := Block[{result,category,limit,buffer,paging,showID},
	(*List all pages in a given category*)
	category=Lookup[arg,"Category",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems",100];
	showID=Lookup[arg,"ShowID",False];

	If[!MatchQ[category,_String]&&!MatchQ[category,{__String}],Throw[$Failed]];
	If[MatchQ[category,_String],category={category}];
	If[MatchQ[category,{__String}],category=StringReplace[category,RegularExpression["(?i)^Category:(.+)"]->"$1"]];
	If[!MatchQ[limit,_Integer]&&!MatchQ[limit,All],Throw[$Failed]];
	
	result={};

	buffer=wikipediarawdata["RawCategoryMembers",{"Category"->#}]&/@category;
	buffer=ImportString[#,"json"]&/@buffer;

	paging=Cases[#,Rule["cmcontinue",a_String]->a,10]&/@buffer;
	paging=paging/.{a_String}->a;


 	buffer=Cases[#,Rule["categorymembers",{a__List}]->a,10]&/@buffer;
	If[showID,
		buffer="pageid"/.buffer,
		buffer="title"/.buffer;
	];
(* 	
 	buffer={"Date"->("timestamp"/.#),"Title"->("title"/.#),"Namespace"->("ns"/.#),"PageID"->("pageid"/.#)}&/@buffer;
 	buffer={#[[1,1]]->DateObject[#[[1,2]],TimeZone->0],#[[2]],#[[3,1]]->(#[[3,2]]/.namespaceTranslation),#[[4]]}&/@buffer;
*)

 	AppendTo[result,#]&/@buffer;

 	If[MatchQ[limit,All],
 		While[Or@@((#=!={})&/@paging),
			buffer=If[#[[2]]=!={},wikipediarawdata["RawCategoryMembers",{"Category"->#[[1]],"Continue"->#[[2]]}],"{}"]&/@Transpose[{category,paging}];
			buffer=ImportString[#,"json"]&/@buffer;
 			
			paging=Cases[#,Rule["cmcontinue",a_String]->a,10]&/@buffer;
			paging=paging/.{a_String}->a;

		 	buffer=Cases[#,Rule["categorymembers",{a__List}]->a,10]&/@buffer;
			If[showID,
				buffer="pageid"/.buffer,
				buffer="title"/.buffer;
			];
		(* 	
		 	buffer={"Date"->("timestamp"/.#),"Title"->("title"/.#),"Namespace"->("ns"/.#),"PageID"->("pageid"/.#)}&/@buffer;
		 	buffer={#[[1,1]]->DateObject[#[[1,2]],TimeZone->0],#[[2]],#[[3,1]]->(#[[3,2]]/.namespaceTranslation),#[[4]]}&/@buffer;
		*)
			
			result=Flatten[#]&/@Transpose[{result,buffer}];
 		];
		result=If[MatchQ[#,{__}],Select[#,!MatchQ[#,"title"]&&!MatchQ[#,"pageid"]&],#]&/@result;
 		result=result/.{"title"->Missing["NotAvailable"],"pageid"->Missing["NotAvailable"],{}->Missing["NotAvailable"]};
 		If[MatchQ[result,{{__String}}]||MatchQ[result,{{__Integer}}],result=result[[1]]]
 	];
 	
 	If[MatchQ[limit,_Integer],
 		If[Or@@((#>limit)&/@(Length/@result)),
			result=If[Length[#]>limit,#[[1;;limit]],#]&/@result;
	 		result=result/.{"title"->Missing["NotAvailable"],"pageid"->Missing["NotAvailable"],{}->Missing["NotAvailable"]},
 			While[Or@@((#=!={})&/@paging)&&And@@((#<limit)&/@(Length/@result)),
				buffer=If[#[[2]]=!={},wikipediarawdata["RawCategoryMembers",{"Category"->#[[1]],"Continue"->#[[2]]}],"{}"]&/@Transpose[{category,paging}];
				buffer=ImportString[#,"json"]&/@buffer;
			
				paging=Cases[#,Rule["cmcontinue",a_String]->a,10]&/@buffer;
				paging=paging/.{a_String}->a;
					
			 	buffer=Cases[#,Rule["categorymembers",{a__List}]->a,10]&/@buffer;
				If[showID,
					buffer="pageid"/.buffer,
					buffer="title"/.buffer;
				];
		(*
		 	buffer={"Date"->("timestamp"/.#),"Title"->("title"/.#),"Namespace"->("ns"/.#),"PageID"->("pageid"/.#)}&/@buffer;
		 	buffer={#[[1,1]]->DateObject[#[[1,2]],TimeZone->0],#[[2]],#[[3,1]]->(#[[3,2]]/.namespaceTranslation),#[[4]]}&/@buffer;
		*)
				
				result=Flatten[#]&/@Transpose[{result,buffer}];
				result=Select[#,!MatchQ[#,"title"]&&!MatchQ[#,"pageid"]&]&/@result
	 		];
	 		result=If[Length[#]>limit,#[[1;;limit]],#]&/@result;
	 		result=result/.{"title"->Missing["NotAvailable"],"pageid"->Missing["NotAvailable"],{}->Missing["NotAvailable"]};
 		];
 		If[MatchQ[result,{{__String}}]||MatchQ[result,{{__Integer}}],result=result[[1]]]
 	];
 	result
	]

wikipediacookeddata["CategoryMemberIDs",arg_] := Block[{result,category,limit,buffer,paging},
	(*List all pages in a given category*)

	category=Lookup[arg,"Category",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems",100];

	If[!MatchQ[category,_String]&&!MatchQ[category,{__String}],Throw[$Failed]];
	If[!MatchQ[limit,_Integer]&&!MatchQ[limit,All],Throw[$Failed]];

	result=wikipediacookeddata["CategoryMembers",{"Category"->category,"ShowID"->True,"MaxItems"->limit}];
	result
	]

(****************************************************************************************************)

wikipediarawdata["RawCategorySearch",arg_] := Block[{result,search,limit,format,continue,parameters},
	(*Enumerate all categories*)
	search=Lookup[arg,"Search",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems","max"];
	format=Lookup[arg,"Format","json"];
	continue=Lookup[arg,"Continue",Missing["NotAvailable"]];
	
	If[MatchQ[search,_String],search=StringReplace[search," "->"_"],Throw[$Failed]];
	If[MatchQ[limit,_Integer],limit=ToString[limit],If[limit=!="max",Throw[$Failed]]];

	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];

	parameters={
		"action"->"query",
		"list"->"allcategories",
		"continue"->"",
		"format"->format,
		"acprefix"->search,
		"acprop"->"size",
		"aclimit"->limit
		};
	
	If[MatchQ[continue,_String],AppendTo[parameters,Rule["accontinue",continue]]];
		
	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result	
	]

wikipediacookeddata["CategorySearch",arg_] := Block[{result,search,limit,buffer,paging},
	(*Enumerate all categories*)
	search=Lookup[arg,"Search",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems",100];
	If[MatchQ[search,_String],search=StringReplace[search," "->"_"],Throw[$Failed]];
	If[!MatchQ[limit,_Integer]&&!MatchQ[limit,All],Throw[$Failed]];

	result={};

	buffer=wikipediarawdata["RawCategorySearch",{"Search"->search}];
	buffer=ImportString[buffer,"json"];

	paging=Cases[buffer,Rule["accontinue",a_String]->a,10];
	paging=paging/.{a_String}->a;

 	buffer=Cases[buffer,Rule["allcategories",{a__List}]->a,10];
	buffer=("*"/.#)&/@buffer;
(*
	buffer={"SubCategories"->("subcats"/.#),"Files"->("files"/.#),"Category"->("*"/.#),"PageCount"->("size"/.#),"ArticleCount"->("pages"/.#)}&/@buffer;
*)
 	AppendTo[result,#]&/@buffer;

 	If[MatchQ[limit,All],
 		While[paging=!={},
			buffer=wikipediarawdata["RawCategorySearch",{"Search"->search,"Continue"->paging}];
			buffer=ImportString[buffer,"json"];
		
			paging=Cases[buffer,Rule["accontinue",a_String]->a,10];
			paging=paging/.{a_String}->a;
			
		 	buffer=Cases[buffer,Rule["allcategories",{a__List}]->a,10];
			buffer=("*"/.#)&/@buffer;
		(*
			buffer={"SubCategories"->("subcats"/.#),"Files"->("files"/.#),"Category"->("*"/.#),"PageCount"->("size"/.#),"ArticleCount"->("pages"/.#)}&/@buffer;
		*)
		 	
		 	AppendTo[result,#]&/@buffer;
 		]
 	];
 	
 	If[MatchQ[limit,_Integer],
 		If[limit<Length[result],
 			result=result[[1;;limit]],
 			While[paging=!={}&&limit>Length[result],
				buffer=wikipediarawdata["RawCategorySearch",{"Search"->search,"Continue"->paging}];
				buffer=ImportString[buffer,"json"];
			
				paging=Cases[buffer,Rule["accontinue",a_String]->a,10];
				paging=paging/.{a_String}->a;
					
			 	buffer=Cases[buffer,Rule["allcategories",{a__List}]->a,10];
				buffer=("*"/.#)&/@buffer;
			(*
				buffer={"SubCategories"->("subcats"/.#),"Files"->("files"/.#),"Category"->("*"/.#),"PageCount"->("size"/.#),"ArticleCount"->("pages"/.#)}&/@buffer;
			*)
			 	
			 	AppendTo[result,#]&/@buffer;
	 		];
	 		If[limit<Length[result],result=result[[1;;limit]]]
 		]
 	];
 	result
	]

(****************************************************************************************************)

wikipediarawdata["RawContentSearch",arg_] := Block[{result,search,limit,format,continue,parameters,exactSearch},
	(*Perform a full content text search*)
	search=Lookup[arg,"Content",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems","max"];
	format=Lookup[arg,"Format","json"];
	continue=Lookup[arg,"Continue",Missing["NotAvailable"]];
	exactSearch=Lookup[arg,"ExactSearch",True];
	
	If[!MatchQ[search,_String],Throw[$Failed]];
	search=URLEncode@search;
	If[!MemberQ[{True,False},exactSearch],Throw[$Failed]];
	If[exactSearch,search="\""<>search<>"\""];
	If[MatchQ[limit,_Integer],limit=ToString[limit],If[limit=!="max",Throw[$Failed]]];
	
	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];

	parameters={
		"action"->"query",
		"list"->"search",
		"continue"->"",
		"format"->format,
		"srlimit"->limit,
		"srwhat"->"text",
		"srsearch"->search
		};

	If[MatchQ[continue,_String],AppendTo[parameters,Rule["sroffset",continue]]];

	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result
	]

wikipediacookeddata["ContentSearch",arg_] := Block[{result,search,limit,paging,buffer,exact},
	(*Perform a full content text search*)
	search=Lookup[arg,"Content",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems",100];
	exact=False;

	If[!MatchQ[search,_String]&&!MatchQ[search,{__String}],Throw[$Failed]];
	If[MatchQ[search,{__String}],
		search=StringJoin@Riffle[search," "];
		exact=True
	];
	
	If[!MatchQ[limit,_Integer]&&!MatchQ[limit,All],Throw[$Failed]];

	result={};

	buffer=wikipediarawdata["RawContentSearch",{"Content"->search,"ExactSearch"->exact}];
	buffer=ImportString[buffer,"json"];

	paging=Cases[buffer,Rule["sroffset",a_]->a,10];
	paging=paging/.{a_}:>ToString[a];

 	buffer=Cases[buffer,Rule["search",{a__List}]->a,10];
	buffer={"Title"->("title"/.#),"Snippet"->("snippet"/.#)}&/@buffer;
	buffer={#[[1]],#[[2,1]]->StringReplace[#[[2,2]],{"<span class=\"searchmatch\">"->"","</span>"->""}]}&/@buffer;
(*
	buffer={"WordCount"->("wordcount"/.#),"Size"->("size"/.#),"Title"->("title"/.#),"Snippet"->("snippet"/.#),"Date"->("timestamp"/.#)}&/@buffer;
	buffer={#[[1]],#[[2]],#[[3]],#[[4,1]]->StringReplace[#[[4,2]],{"<span class=\"searchmatch\">"->"","</span>"->""}],#[[5,1]]->DateObject[#[[5,2]],TimeZone->0]}&/@buffer;
*)
 	AppendTo[result,#]&/@buffer;
 	
 	If[MatchQ[limit,All],
 		While[paging=!={},
			buffer=wikipediarawdata["RawContentSearch",{"Content"->search,"ExactSearch"->exact,"Continue"->paging}];
			buffer=ImportString[buffer,"json"];
		
			paging=Cases[buffer,Rule["sroffset",a_]->a,10];
			paging=paging/.{a_}:>ToString[a];

		 	buffer=Cases[buffer,Rule["search",{a__List}]->a,10];
			buffer={"Title"->("title"/.#),"Snippet"->("snippet"/.#)}&/@buffer;
			buffer={#[[1]],#[[2,1]]->StringReplace[#[[2,2]],{"<span class=\"searchmatch\">"->"","</span>"->""}]}&/@buffer;
		(*
			buffer={"WordCount"->("wordcount"/.#),"Size"->("size"/.#),"Title"->("title"/.#),"Snippet"->("snippet"/.#),"Date"->("timestamp"/.#)}&/@buffer;
			buffer={#[[1]],#[[2]],#[[3]],#[[4,1]]->StringReplace[#[[4,2]],{"<span class=\"searchmatch\">"->"","</span>"->""}],#[[5,1]]->DateObject[#[[5,2]],TimeZone->0]}&/@buffer;
		*)
		
		 	AppendTo[result,#]&/@buffer;
 		]
 	];
 	
 	If[MatchQ[limit,_Integer],
 		If[limit<Length[result],
 			result=result[[1;;limit]],
 			While[paging=!={}&&limit>Length[result],
				buffer=wikipediarawdata["RawContentSearch",{"Content"->search,"ExactSearch"->exact,"Continue"->paging}];
				buffer=ImportString[buffer,"json"];
			
				paging=Cases[buffer,Rule["sroffset",a_]->a,10];
				paging=paging/.{a_}:>ToString[a];

			 	buffer=Cases[buffer,Rule["search",{a__List}]->a,10];
				buffer={"Title"->("title"/.#),"Snippet"->("snippet"/.#)}&/@buffer;
				buffer={#[[1]],#[[2,1]]->StringReplace[#[[2,2]],{"<span class=\"searchmatch\">"->"","</span>"->""}]}&/@buffer;
			(*
				buffer={"WordCount"->("wordcount"/.#),"Size"->("size"/.#),"Title"->("title"/.#),"Snippet"->("snippet"/.#),"Date"->("timestamp"/.#)}&/@buffer;
				buffer={#[[1]],#[[2]],#[[3]],#[[4,1]]->StringReplace[#[[4,2]],{"<span class=\"searchmatch\">"->"","</span>"->""}],#[[5,1]]->DateObject[#[[5,2]],TimeZone->0]}&/@buffer;
			*)
			
			 	AppendTo[result,#]&/@buffer;
	 		];
	 		If[limit<Length[result],result=result[[1;;limit]]]
 		]
 	];
 	
 	result=Association@@@result;
	result
	]

(****************************************************************************************************)

wikipediarawdata["RawContributorArticles",arg_] := Block[{result,contributor,limit,format,continue,parameters},
	(*Lists articles made by a contributor*)
	contributor=Lookup[arg,"Contributor",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems","max"];
	format=Lookup[arg,"Format","json"];
	continue=Lookup[arg,"Continue",Missing["NotAvailable"]];

	If[!MatchQ[contributor,_String],Throw[$Failed]];
	If[MatchQ[limit,_Integer],limit=ToString[limit],If[limit=!="max",Throw[$Failed]]];

	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];

	parameters={
		"action"->"query",
		"list"->"usercontribs",
		"ucprop"->"ids|title|timestamp|comment|parsedcomment|size|sizediff|flags|tags",
		"continue"->"",
		"format"->format,
		"ucuser"->contributor,
		"uclimit"->limit
		};

	If[MatchQ[continue,_String],AppendTo[parameters,Rule["uccontinue",continue]]];

	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result
	]

wikipediacookeddata["ContributorArticles",arg_] := Block[{result,contributor,limit,buffer,paging},
	(*Lists articles made by a contributor*)
	contributor=Lookup[arg,"Contributor",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems",100];

	If[!MatchQ[contributor,_String],Throw[$Failed]];
	If[!MatchQ[limit,_Integer]&&!MatchQ[limit,All],Throw[$Failed]];

	result={};

	buffer=wikipediarawdata["RawContributorArticles",{"Contributor"->contributor}];
	buffer=ImportString[buffer,"json"];

	paging=Cases[buffer,Rule["uccontinue",a_String]->a,10];
	paging=paging/.{a_String}->a;

 	buffer=Cases[buffer,Rule["usercontribs",{a__List}]->a,10];
	buffer=("title"/.#)&/@buffer;
(*
	buffer={"UserID"->("userid"/.#),"Date"->DateObject[("timestamp"/.#),TimeZone->0],"Title"->("title"/.#), 
		"Namespace"->(("ns"/.#)/.namespaceTranslation),"PageID"->("pageid"/.#),
		"RevisionID"->("revid"/.#),"Size"->("size"/.#),"SizeDifference"->("sizediff"/.#)}&/@buffer;
*)
 	AppendTo[result,#]&/@buffer;
 	
 	If[MatchQ[limit,All],
 		While[paging=!={},
			buffer=wikipediarawdata["RawContributorArticles",{"Contributor"->contributor,"Continue"->paging}];
			buffer=ImportString[buffer,"json"];
		
			paging=Cases[buffer,Rule["uccontinue",a_String]->a,10];
			paging=paging/.{a_String}->a;
		
		 	buffer=Cases[buffer,Rule["usercontribs",{a__List}]->a,10];
			buffer=("title"/.#)&/@buffer;
		(*
			buffer={"UserID"->("userid"/.#),"Date"->DateObject[("timestamp"/.#),TimeZone->0],"Title"->("title"/.#), 
				"Namespace"->(("ns"/.#)/.namespaceTranslation),"PageID"->("pageid"/.#),
				"RevisionID"->("revid"/.#),"Size"->("size"/.#),"SizeDifference"->("sizediff"/.#)}&/@buffer;
		*)
		 	AppendTo[result,#]&/@buffer;
 		]
	];
	If[MatchQ[limit,_Integer],
 		If[limit<Length[result],
 			result=result[[1;;limit]],
 			While[paging=!={}&&limit>Length[result],
				buffer=wikipediarawdata["RawContributorArticles",{"Contributor"->contributor,"Continue"->paging}];
				buffer=ImportString[buffer,"json"];
			
				paging=Cases[buffer,Rule["uccontinue",a_String]->a,10];
				paging=paging/.{a_String}->a;
			
			 	buffer=Cases[buffer,Rule["usercontribs",{a__List}]->a,10];
				buffer=("title"/.#)&/@buffer;
			(*
				buffer={"UserID"->("userid"/.#),"Date"->DateObject[("timestamp"/.#),TimeZone->0],"Title"->("title"/.#), 
					"Namespace"->(("ns"/.#)/.namespaceTranslation),"PageID"->("pageid"/.#),
					"RevisionID"->("revid"/.#),"Size"->("size"/.#),"SizeDifference"->("sizediff"/.#)}&/@buffer;
			*)
				AppendTo[result,#]&/@buffer;
			];
			If[limit<Length[result],result=result[[1;;limit]]]
		]
	];
	result
	]

(****************************************************************************************************)

wikipediarawdata["RawExternalLinks",arg_] := Block[{result,title,format,continue,parameters},
	(*Get the list of logged-in contributors and the count of anonymous contributors to a page*)
	title=Lookup[arg,"Title",Throw[$Failed]];
	format=Lookup[arg,"Format","json"];
	continue=Lookup[arg,"Continue",Missing["NotAvailable"]];
	
	If[MatchQ[title,_String]||MatchQ[title,{__String}],title=StringReplace[title," "->"_"],Throw[$Failed]];
	If[MatchQ[title,{__String}],title=StringJoin[Riffle[title, "|"]]];
	
	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];

	parameters={
		"action"->"query",
		"prop"->"extlinks",
		"continue"->"",
		"format"->format,
		"titles"->title,
		"redirects"->""
		};

	If[MatchQ[continue,_Integer],AppendTo[parameters,Rule["eloffset",ToString[continue]]]];

	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result
	]

wikipediacookeddata["ExternalLinks",arg_] := Block[{result,title,limit,continue,buffer,paging},
	(*Returns external links of the given page(s)*)
	title=titleMapping2[arg];
	
	If[MatchQ[title,_Missing],result=title];
	
	If[MatchQ[title,{__}],
		result={};
		
		buffer=If[MatchQ[#,_String],wikipediarawdata["RawExternalLinks",{"Title"->#}],#]&/@title;
		buffer=If[MatchQ[#,_String],ImportString[#,"json"],#]&/@buffer;

		paging=Cases[#,Rule["eloffset",a_Integer]->a,10]&/@buffer;
		paging=paging/.{a_Integer}->a;

		buffer=If[MatchQ[#,_Missing],#,Cases[#,Rule["query",{___,Rule["pages",a__],___}]->a,10]]&/@buffer;
		buffer=If[MatchQ[#,_Missing],#,Cases[#,{___,Rule["title",__],___},10]]&/@buffer;
		buffer=If[MatchQ[#,_Missing],#,Flatten@({"extlinks"}/.#)]&/@buffer;
		buffer=If[MatchQ[#,_Missing],#,If[MatchQ[#,{__Rule}],#/.Rule[a_,b_]->b,#]]&/@buffer;
		buffer=buffer/.{"extlinks"}->Missing["NotAvailable"];

		result=buffer;
		
		While[Or@@((#=!={})&/@paging),
			buffer=If[MatchQ[#[[1]],_Missing],#,If[MatchQ[#[[2]],_Integer],wikipediarawdata["RawExternalLinks",{"Title"->#[[1]],"Continue"->#[[2]]}],#[[2]]]]&/@Transpose[{title,paging}];
			buffer=If[MatchQ[#,_String],ImportString[#,"json"],#]&/@buffer;

			paging=Cases[#,Rule["eloffset",a_Integer]->a,10]&/@buffer;
			paging=paging/.{a_Integer}->a;
	
			buffer=If[MatchQ[#,_Missing],#,Cases[#,Rule["query",{___,Rule["pages",a__],___}]->a,10]]&/@buffer;
			buffer=If[MatchQ[#,_Missing],#,Cases[#,{___,Rule["title",__],___},10]]&/@buffer;
			buffer=If[MatchQ[#,_Missing],#,Flatten@({"extlinks"}/.#)]&/@buffer;
			buffer=If[MatchQ[#,_Missing],#,If[MatchQ[#,{__Rule}],#/.Rule[a_,b_]->b,#]]&/@buffer;
			buffer=buffer/.{"extlinks"}->Missing["NotAvailable"];

			result=Flatten[#]&/@Transpose[{result,buffer}];
		];
		
	 	result=If[MatchQ[#,{__Missing}],First@#,#]&/@result;
		result=If[MatchQ[#,_List],Select[#,!MatchQ[#,_Missing]&],#]&/@result;
		If[Length[result]==1,result=First@result];
	];

	result
	]

(****************************************************************************************************)

wikipediarawdata["RawGeoNearbyArticles",arg_] := Block[{result,title,limit,gsradius,coord,format,geodisk,parameters},
	(*Returns pages around the given point*)
	geodisk="";
	geodisk=Lookup[arg,"Geodisk"];
	If[MatchQ[geodisk,GeoDisk[{_,_},Quantity[_?NumberQ,_String]]],
		If[MemberQ[arg,Rule["Title",_]],Throw[$Failed]];
		If[MemberQ[arg,Rule["GeoDistance",_]],Throw[$Failed]];
		coord=Replace[geodisk,GeoDisk[{a_,b_},Quantity[_,_]]:>ToString[a]<>"|"<>ToString[b]];
		gsradius=Replace[geodisk,GeoDisk[{_,_},Quantity[a_,b_]]:>UnitConvert[Quantity[a,b],"Meters"]];
		gsradius=Replace[gsradius,Quantity[c_,_]:>ToString[c]];
		
		limit=Lookup[arg,"MaxItems","max"];
		format=Lookup[arg,"Format","json"];
	
		If[MatchQ[limit,_Integer],limit=ToString[limit],If[limit=!="max",Throw[$Failed]]];
		If[!MatchQ[format,_String],Throw[$Failed]];
		format=ToLowerCase[format];
		If[!MemberQ[formats,format],Throw[$Failed]];
		
		parameters={
			"format"->format,
			"action"->"query",
			"list"->"geosearch",
			"gslimit"->limit,
			"gsradius"->gsradius,
			"gscoord"->coord,
			"continue"->""
			};
		
		result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters],
	
		title=Lookup[arg,"Title",Throw[$Failed]];
		gsradius=Lookup[arg,"GeoDistance",10000];
		limit=Lookup[arg,"MaxItems","max"];
		format=Lookup[arg,"Format","json"];
	
		If[MatchQ[title,_String],title=StringReplace[title," "->"_"],Throw[$Failed]];
		
		Which[NumberQ[gsradius],gsradius=ToString[gsradius],
			MatchQ[gsradius,Quantity[_?NumberQ,_String]],gsradius=Replace[UnitConvert[gsradius, "Meters"],Quantity[c_, _] :> ToString[c]],
			True,Throw[$Failed]
			];
			
		If[MatchQ[limit,_Integer],limit=ToString[limit],If[limit=!="max",Throw[$Failed]]];
	
		If[!MatchQ[format,_String],Throw[$Failed]];
		format=ToLowerCase[format];
		If[!MemberQ[formats,format],Throw[$Failed]];
		
		result=URLFetch["http://en.wikipedia.org/w/api.php?continue=&format=json&action=query&prop=coordinates&titles="<>StringReplace[title," "->"_"]];
		result=ImportString[result,"json"];
		coord=Cases[result,Rule["coordinates",{{___,Rule["globe",globe_],___,Rule["lat",lat_],___,Rule["lon", long_],___}}]->{"planet"->globe,"latitude"->lat,"longitude"->long},10];
		If[coord=!={},
			coord=Flatten[coord];

			parameters={
				"format"->format,
				"action"->"query",
				"list"->"geosearch",
				"gslimit"->limit,
				"gsradius"->gsradius,
				"gscoord"->ToString["latitude"/.coord]<>"|"<>ToString["longitude"/.coord],
				"continue"->"",
				"redirects"->""
				};
			
			result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters],
			result={}
			];
		];
	result
	]

wikipediacookeddata["GeoNearbyArticles",arg_] := Block[{result},
	(*Returns pages around the given point*)
	result=Append[arg,"Format"->"json"];
	result=wikipediarawdata["RawGeoNearbyArticles",result];
	If[MatchQ[result,_String],
		result=ImportString[result,"json"];
		result=Cases[result,Rule["geosearch",{c__}]->c,10];
		result=("title"/.#)&/@result;
	];
	result
	]

wikipediacookeddata["GeoNearbyDataset",arg_] := Block[{result},
	(*Returns pages around the given point*)
	result=Append[arg,"Format"->"json"];
	result=wikipediarawdata["RawGeoNearbyArticles",result];
	If[MatchQ[result,_String],
		result=ImportString[result,"json"];
		result=Cases[result,Rule["geosearch",{c__}]->c,10];
		result={"Title"->("title"/.#),"PageID"->("pageid"/.#),"Position"->GeoPosition[{"lat","lon"}/.#],"Distance"->Quantity[("dist"/.#),"Meters"]}&/@result;
		result=Association@@@result;
		result=Dataset[result];
	];
	result
	]

wikipediarawdata["RawGeoPosition",arg_] := Block[{result,title,format,parameters},
	(*Returns coordinates of the given page(s)*)
	title=Lookup[arg,"Title",Throw[$Failed]];
	format=Lookup[arg,"Format","json"];
	
	If[MatchQ[title,_String],title=StringReplace[title," "->"_"],Throw[$Failed]];
	
	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];

	parameters={
		"action"->"query",
		"continue"->"",
		"format"->format,
		"prop"->"coordinates",
		"coprimary"->"primary",
		"coprop"->"type|name|dim|country|region|globe",
		"colimit"->"max",
		"titles"->title
		};
	
	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result
	]

wikipediacookeddata["GeoPosition",arg_] := Block[{result,title,order},
	(*Returns coordinates of the given page(s)*)
	title=titleMapping2[arg];

	If[MatchQ[title,_Missing],result=title];

	result={};
	If[MatchQ[title,{__}],
		result=If[MatchQ[#,_String],wikipediarawdata["RawGeoPosition",{"Title"->#}],#]&/@title;
		result=If[MatchQ[#,_String],ImportString[#,"json"],#]&/@result;
		result=If[MatchQ[#,_Missing],#,Cases[#,Rule["query",{___,Rule["pages",a__],___}]->a,10]]&/@result;
		result=If[MatchQ[#,_Missing],#,Cases[#,{___,Rule["title",_],___},10]]&/@result;
		result=If[MatchQ[#,_Missing],#,Flatten[#]]&/@result;

		result={If[MatchQ[#,{___,Rule["title",_],___}],("title"/.#),Missing["ArticleNotFound"]],
			If[MatchQ[#,{___,Rule["missing",_],___}]||!MatchQ[#,{___,Rule["pageid",_],___}],Missing["ArticleNotFound"],("pageid"/.#)],
			If[MatchQ[#,{___,Rule["coordinates",_],___}],Flatten[("coordinates"/.#)],Missing["CoordinatesNotFound"]]}&/@result;
		result={#[[1]],#[[2]],If[!MatchQ[#[[3]],_Missing],{"globe","country","lat","lon"}/.#[[3]],#[[3]]]}&/@result;
		result={#[[1]],#[[2]],If[!MatchQ[#[[3]],_Missing],#[[3,1]],#[[3]]],If[!MatchQ[#[[3]],_Missing],{#[[3,3]],#[[3,4]]},#[[3]]]}&/@result;
		result={"Title"->#[[1]],"PageID"->#[[2]],
			"GeoPosition"->If[!MatchQ[#[[4]],_Missing],
				If[!MatchQ[#[[3]],_Missing],
					If[ToLowerCase[#[[3]]]=="earth",GeoPosition[#[[4]]],GeoPosition[#[[4]],Interpreter["AstronomicalObject"][#[[3]]]]],
					#[[3]]
				],
				#[[4]]
			]}&/@result;
		(*using just the geoposition wrapper*)
		result="GeoPosition"/.result;
		If[Length[result]==1,result=First@result]
	];

	result
	]

(****************************************************************************************************)

ExtractImageFilename[title_] := Module[{result,parameters},
	result={};
	If[StringQ[title],
		parameters={
			"format"->"json",
			"action"->"query",
			"prop"->"images",
			"imlimit"->"max",
			"continue"->"",
			"titles"->StringReplace[title," "->"_"],
			"redirects"->""
			};
		
		result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
		result=ImportString[result,"json"];
		
		result=#[[2]]&/@Flatten[Cases[result,{___,Rule["pages",a_],___}->a,10],1];
		result={"title"/.#,"missing"/.#,("images"/.#)}&/@result;
		result=Rule[#[[1]],If[#[[2]]=="missing","title"/.#[[3]],Missing["NotAvailable"]]]&/@result;
	];
	result
];

wikipediacookeddata["ImageDataset",arg_] := Block[{result,title,limit,size,elements,filter,parameters,i},
	title=titleMapping2[arg];

	result={};
	If[MatchQ[title,_Missing],result=title];
	If[MatchQ[title,{_Missing}],result=First@title];
	
	If[MatchQ[title,{__}],
		result={};
		
		limit=Lookup[arg,"MaxItems","max"];
		size=Lookup[arg,"Size",1000];
		elements=ToLowerCase/@Lookup[arg,"Elements",{"timestamp","user","userid","comment","canonicaltitle"}];
	
		title=If[MatchQ[#,_String],StringReplace[#," "->"_"],#]&/@title;
		If[MatchQ[limit,_Integer],limit=ToString[limit],If[limit=!="max",Throw[$Failed]]];
		If[MatchQ[size,_Integer],size=ToString[size],Throw[$Failed]];
		If[!MatchQ[elements,{__String}],Throw[$Failed]];
		If[MatchQ[elements,{"data"}],elements={"timestamp","user","userid","comment","canonicaltitle"}];
		If[MatchQ[elements,{"fulldata"}],elements={"timestamp","user","userid","comment","parsedcomment","canonicaltitle","size","dimensions","sha1","mime","thumbmime","mediatype","metadata","commonmetadata","extmetadata","archivename","bitdepth","uploadwarning"}];
	
		filter=elements;
		elements=StringJoin@Riffle[elements,"|"];
		
		result=ExtractImageFilename[#]&/@title;
		result=result/.{{Rule[a_,b_]}:>b,{}->Missing["NotAvailable"]};

		If[StringMatchQ[limit,RegularExpression["[0-9]+"]],
			limit=FromDigits[limit];
			result=If[MatchQ[#,_Missing],#,If[Length[#]>limit,#[[1;;limit]],#]]&/@result;
			result=If[MatchQ[#,_Missing],#,StringJoin[Riffle[#,"|"]]&/@Partition[#,50,50,1,{}]]&/@result
			];
		
		result=If[MatchQ[#, _Missing],#,{"format"->"json","action"->"query","prop"->"imageinfo","iiprop"->"url|"<>elements,
			"iiurlwidth"->size,"titles"->#,"redirects"->""}&/@#]&/@result;
		result=If[MatchQ[#,_Missing],#,ImportString[URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->#],"json"]&/@#]&/@result;
		result=If[MatchQ[#,_Missing],#,Cases[#,Rule["query",{Rule["pages",a_]}]->a,10]&/@#]&/@result;
		result=If[MatchQ[#,_Missing],#,Flatten[("imageinfo"/.#)]&/@Cases[#,{___,Rule["ns",_],___},10]&/@#]&/@result;
		result=If[MatchQ[#,_Missing],#,Flatten[#,1]]&/@result;
		result=If[MatchQ[#,_Missing],#,{"thumbnail"->Import["thumburl"/.#],Sequence@@#}&/@#]&/@result;
		result=If[MatchQ[#,_Missing],#,Select[#,MemberQ[Prepend[filter,"thumbnail"],#[[1]]]&]&/@#]&/@result;
		result=result/.{"thumbnail"->"Thumbnail","timestamp"->"Date","user"->"User","userid"->"UserID","comment"->"Comment","parsedcomment"->"ParsedComment",
			"canonicaltitle"->"Title","size"->"Size","dimensions"->"Dimensions","sha1"->"Sha1","mime"->"Mime","thumbmime"->"Thumbmime",
			"mediatype"->"MediaType","metadata"->"Metadata","commonmetadata"->"CommonMetadata","extmetadata"->"ExternalMetadata",
			"archivename"->"ArchiveName","bitdepth"->"BitDepth","uploadwarning"->"UploadWarning"};
		result=If[MatchQ[#,_Missing],#,Dataset[Association@@@Sort[#]]]&/@result;
		If[Length[result]==1,result=First@result]

	];
	
	result
	]

wikipediacookeddata["ImageList",arg_] := Block[{result,title},
	(*Gets image thumbnails from a given page*)
	title=titleMapping2[arg];
	
	If[MatchQ[title,_Missing],result=title];
	
	If[MatchQ[title,{__}],
		result={};

		result=If[MatchQ[#,_String],ExtractImageFilename[#],#]&/@title;
		result=result/.{a_->b_}->b;
		
		result=If[MatchQ[#,{__String}],ImportString[URLFetch["http://en.wikipedia.org/w/api.php",
			"Parameters"->{"format"->"json","action"->"query","prop"->"imageinfo","iiprop"->"url","iiurlwidth"->"200","titles"->StringJoin@Riffle[#,"|"]}],"json"],#]&/@result;
		result=If[MatchQ[#,_Missing],#,Cases[#,Rule["query",{Rule["pages",a_]}]->a,10]]&/@result;
		result=If[MatchQ[#,_Missing],#,Cases[#,{___,Rule["ns",_],___},10]]&/@result;
		result=If[MatchQ[#,_Missing],#,Flatten[("thumburl"/.("imageinfo"/.#))]]&/@result;
		result=If[MatchQ[#,_Missing],#,(Import@#&/@#)]&/@result;

		If[Length[result]==1,result=First@result]
	];

	result
	]

wikipediacookeddata["ImageURLs",arg_] := Block[{result,title},
	(*Gets image URLs from a given page*)
	title=titleMapping2[arg];
	
	If[MatchQ[title,_Missing],result=title];
	
	If[MatchQ[title,{__}],
		result={};
		
		result=If[MatchQ[#,_String],ExtractImageFilename[StringReplace[#," "->"_"]],#]&/@title;
		result=result/.{a_->b_}->b;

		result=If[MatchQ[#,{__String}],ImportString[URLFetch["http://en.wikipedia.org/w/api.php",
			"Parameters"->{"format"->"json","action"->"query","prop"->"imageinfo","iiprop"->"url","titles"->StringJoin@Riffle[#,"|"]}],"json"],#]&/@result;

		result=If[MatchQ[#,_Missing],#,Cases[#,Rule["query",{Rule["pages",a_]}]->a,10]]&/@result;
		result=If[MatchQ[#,_Missing],#,Cases[#,{___,Rule["ns",_],___},10]]&/@result;
		result=If[MatchQ[#,_Missing],#,Flatten[("descriptionurl"/.("imageinfo"/.#))]]&/@result;

		If[Length[result]==1,result=First@result];
	];

	result
	]

(****************************************************************************************************)

wikipediarawdata["RawLanguagesURLRules",arg_] := Block[{result,title,limit,format,continue,parameters},
	(*Returns all interlanguage links from the given page(s)*)
	title=Lookup[arg,"Title",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems","max"];
	format=Lookup[arg,"Format","json"];
	continue=Lookup[arg,"Continue",Missing["NotAvailable"]];

	If[MatchQ[title,_String]||MatchQ[title,{__String}],title=StringReplace[title," "->"_"],Throw[$Failed]];
	If[MatchQ[title,{__String}],title=StringJoin[Riffle[title, "|"]]];
	If[MatchQ[limit,_Integer],limit=ToString[limit],If[limit=!="max",Throw[$Failed]]];
	
	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];

	parameters={
		"action"->"query",
		"prop"->"langlinks",
		"continue"->"",
		"format"->format,
		"lllimit"->limit,
		"llprop"->"url|langname|autonym",
		"titles"->title,
		"redirects"->""
		};

	If[MatchQ[continue,_String],AppendTo[parameters,Rule["llcontinue",continue]]];

	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result
	]

wikipediacookeddata["LanguagesURLRules",arg_] := Block[{result,title,buffer},
	(*Returns all interlanguage links from the given page(s)*)
	title=titleMapping2[arg];

	If[MatchQ[title,_Missing],result=title];
	
	If[MatchQ[title,{__}],
		result={};
		
		buffer=If[MatchQ[#,_String],wikipediarawdata["RawLanguagesURLRules",{"Title"->#}],#]&/@title;
		buffer=If[MatchQ[#,_String],ImportString[#,"json"],#]&/@buffer;
	 	buffer=If[MatchQ[#,_Missing],#,Cases[#,Rule["query",{___,Rule["pages",a__],___}]->a,10]]&/@buffer;
	 	buffer=If[MatchQ[#,_Missing],#,Cases[#,{___,Rule["langlinks",a__],___}->a,10]]&/@buffer;
	 	buffer=If[MatchQ[#,_Missing],#,Flatten[#,1]]&/@buffer;
	 	buffer=If[MatchQ[#,_Missing],#,(Rule["langname","url"]/.#)]&/@buffer;
	 	buffer=If[MatchQ[#,_Missing],#,(If[#=!=Rule["langname","url"],#,Missing["NotAvailable"]])]&/@buffer;
		result=buffer;

		If[Length[result]==1,result=First@result];
	];

	result
	]

wikipediacookeddata["LanguagesList",arg_] := Module[{result,title,pageid},
	title=Lookup[arg,"Title",Missing["NotAvailable"]];
	pageid=Lookup[arg,"PageID",Missing["NotAvailable"]];
	result={};
	
	Which[
		!MatchQ[title,_Missing],
		result=wikipediacookeddata["LanguagesURLRules",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipediacookeddata["LanguagesURLRules",{"PageID"->pageid}],
		True,
		Throw[$Failed]
	];
	
	result=Replace[result,Rule[a_,b_]->a,10];
	result
	];

wikipediacookeddata["LanguagesURLs",arg_] := Module[{title,result,pageid},
	title=Lookup[arg,"Title",Missing["NotAvailable"]];
	pageid=Lookup[arg,"PageID",Missing["NotAvailable"]];
	result={};
	
	Which[
		!MatchQ[title,_Missing],
		result=wikipediacookeddata["LanguagesURLRules",{"Title"->title}],
		!MatchQ[pageid,_Missing],
		result=wikipediacookeddata["LanguagesURLRules",{"PageID"->pageid}],
		True,
		Throw[$Failed]
	];
	
	result=Replace[result,Rule[a_,b_]->b,10];
	result
	]

(****************************************************************************************************)

wikipediarawdata["RawLinksRules",arg_] := Block[{result,format,title,limit,continue,parameters},
	(*Find all pages that link to the given page*)
	title=Lookup[arg,"Title",Throw[$Failed]];
	limit=Lookup[arg,"MaxLevelItems","max"];
	format=Lookup[arg,"Format","json"];
	continue=Lookup[arg,"Continue",Missing["NotAvailable"]];

	If[MatchQ[title,_String]||MatchQ[title,{__String}],title=StringReplace[title," "->"_"],Throw[$Failed]];
	If[MatchQ[title,{__String}],title=StringJoin[Riffle[title,"|"]]];
	If[MatchQ[limit,_Integer],limit=ToString[limit],If[limit=!="max",Throw[$Failed]]];

	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];

	parameters={
		"action"->"query",
		"prop"->"links",
		"plnamespace"->"0",
		"continue"->"",
		"format"->format,
		"titles"->title,
		"pllimit"->limit,
		"redirects"->""
		};

	If[MatchQ[continue,_String],AppendTo[parameters,Rule["plcontinue",continue]]];

	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result
	]

LinkedArticles[title_,limit_] := Module[{result,buffer,paging},
	result={};

	buffer=wikipediarawdata["RawLinksRules",{"Title"->title}];
	buffer=ImportString[buffer,"json"];

	paging=Cases[buffer,Rule["plcontinue",a_String]->a,10];
	If[MatchQ[paging,{_String}],paging=paging[[1]]];

 	buffer=Cases[buffer,Rule["links",{a__List}]->a,10];
 	buffer="title"/.Select[buffer,MatchQ[#,{_,_}]&];
 	
 	AppendTo[result,#]&/@buffer;
 	
 	If[MatchQ[limit,All],
 		While[paging=!={},
			buffer=wikipediarawdata["RawLinksRules",{"Title"->title,"Continue"->paging}];
			buffer=ImportString[buffer,"json"];
		
			paging=Cases[buffer,Rule["plcontinue",a_String]->a,10];
			If[MatchQ[paging,{_String}],paging=paging[[1]]];
		
		 	buffer=Cases[buffer,Rule["links",{a__List}]->a,10];
		 	buffer="title"/.Select[buffer,MatchQ[#,{_,_}]&];
		 	
		 	AppendTo[result,#]&/@buffer;
 		]
 	];
 	
 	If[MatchQ[limit,_Integer],
 		If[limit<=Length[result],
 			result=result[[1;;limit]],
 			While[paging=!={}&&limit>Length[result],
				buffer=wikipediarawdata["RawLinksRules",{"Title"->title,"Continue"->paging}];
				buffer=ImportString[buffer,"json"];
			
				paging=Cases[buffer,Rule["plcontinue",a_String]->a,10];
				If[MatchQ[paging,{_String}],paging=paging[[1]]];
			
			 	buffer=Cases[buffer,Rule["links",{a__List}]->a,10];
			 	buffer="title"/.Select[buffer,MatchQ[#,{_,_}]&];
			 	
			 	AppendTo[result,#]&/@buffer;
	 		];
	 		If[limit<=Length[result],result=result[[1;;limit]]]
 		]
 	];
 	
	result=Union[result];
	result
	]

LinksTree[title_,limit_,level_]:=Module[{result,nodes,newNodes,buffer,i},
	nodes={title};
	newNodes={};
	result={};
	Do[
		newNodes=LinkedArticles[#,limit]&/@nodes;
		buffer=Thread[List[nodes,newNodes]];
		newNodes=Union[Flatten[newNodes]];
		buffer=Thread[Rule[#[[1]],#[[2]]]]&/@buffer;
		AppendTo[result,buffer];
		nodes=newNodes;
		,{i,1,level}];
	result=Flatten[result];
	result=Select[result,!MatchQ[#,Rule[a_,a_]]&];
	result
]

wikipediacookeddata["LinksRules",arg_] := Block[{result,title,limit,level,nodes,buffer,newNodes},
	(*Find all pages that link to the given page*)
	title=titleMapping2[arg];

	If[MatchQ[title,_Missing],result=title];
	
	If[MatchQ[title,{__}],
		title=If[MatchQ[#,_String],StringReplace[#," "->"_"],#]&/@title;
		limit=Lookup[arg,"MaxLevelItems",All];
		level=Lookup[arg,"MaxLevel",1];

		If[!MatchQ[limit,_Integer]&&!MatchQ[limit,All],Throw[$Failed]];
		If[!MatchQ[level,_Integer],Throw[$Failed]];
	
		result=If[MatchQ[#,_String],LinksTree[#,limit,level],#]&/@title;
		If[Length[result]==1,result=First@result],
		result=Missing["InvalidInput"]
	];

	result/.{s_String:>StringReplace[s,"_"->" "]}
	];

wikipediacookeddata["LinksList",arg_] := Module[{result},
	result=wikipediacookeddata["LinksRules",arg];
	Which[
		MatchQ[result,{__Rule}],
		result=result[[All,2]],
		MatchQ[result,{{__Rule}..}],
		result=#[[All,2]]&/@result
	];
	result/.{s_String:>StringReplace[s,"_"->" "]}
	];

(****************************************************************************************************)

wikipediacookeddata["PageID",arg_] := Block[{result,translate},
	(*Finds the wikipedia id of an Entity or PageID, also validates the existence of a title*)
	result=wikipediacookeddata["Title",arg];
	
	If[!MatchQ[result,_List],result={result}];
	translate=Select[result,MatchQ[#,_String]&];
	translate={#,ImportString[URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->{"action"->"query","titles"->#,"format"->"json"}],"json"]}&/@translate;
	translate={#[[1]],Cases[#[[2]],{___,Rule["title",_String],___},10]}&/@translate;
	translate=Rule[#[[1]],If[MatchQ[#[[2]],{{___,Rule["missing",___],___}}],Missing["TitleNotAvailable"],"pageid"/.#[[2]]]]&/@translate;
	result=result/.translate;
	result=result/.{a_Integer}->a;
	result=result/.a_Integer:>ToString[a];
	
	If[Length[result]==1,result=First@result];

	result
	];

(****************************************************************************************************)

wikipediarawdata["RawParentCategories",arg_] := Block[{result,title,limit,format,continue,parameters},
	(*List all categories the page(s) belong to*)
	title=Lookup[arg,"Title",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems","max"];
	format=Lookup[arg,"Format","json"];
	continue=Lookup[arg,"Continue",Missing["NotAvailable"]];
	
	If[MatchQ[title,_String]||MatchQ[title,{__String}],title=StringReplace[title," "->"_"],Throw[$Failed]];
	If[MatchQ[title,{__String}],title=StringJoin[Riffle[title, "|"]]];
	If[MatchQ[limit,_Integer],limit=ToString[limit],If[limit=!="max",Throw[$Failed]]];
	
	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];

	parameters={
		"action"->"query",
		"prop"->"categories",
		"continue"->"",
		"format"->format,
		"cllimit"->limit,
		"titles"->title,
		"redirects"->""
		};

	If[MatchQ[continue,_String],AppendTo[parameters,Rule["clcontinue",continue]]];

	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result
	]

wikipediacookeddata["ParentCategories",arg_] := Block[{result,title,continue,buffer},
	(*List all categories the page(s) belong to*)
	title=titleMapping2[arg];
	
	If[MatchQ[title,_Missing],result=title];
	
	If[MatchQ[title,{__}],
		result={};

		buffer=If[MatchQ[#,_String],wikipediarawdata["RawParentCategories",{"Title"->#}],#]&/@title;
		buffer=If[MatchQ[#,_String],ImportString[#,"json"],#]&/@buffer;
		buffer=If[MatchQ[#,_Missing],#,Cases[#,Rule["query",{___,Rule["pages",a__],___}]->a,10]]&/@buffer;
		buffer=If[MatchQ[#,_Missing],#,Cases[#,Rule["categories",{a__List}]->a,10]]&/@buffer;
		buffer=If[MatchQ[#,_Missing],#,Cases[#,{___,Rule["title",a__]}->a,10]]&/@buffer;
		
		result=buffer/.{}->Missing["NotAvailable"];
		
		If[Length[result]==1,result=First@result];
	];

	result
	]

(****************************************************************************************************)

wikipediarawdata["RawRevisions",arg_] := Block[{result,title,limit,format,continue,start,end,parameters},
	(*Returns past revisions of the given page(s)*)
	title=Lookup[arg,"Title",Throw[$Failed]];
	start=Lookup[arg,"StartDate",Now];
	end=Lookup[arg,"EndDate",DateObject[{2000, 1, 1}]];
	limit=Lookup[arg,"MaxItems",10];
	format=Lookup[arg,"Format","json"];
	continue=Lookup[arg,"Continue",continue=Missing["NotAvailable"]];

	If[MatchQ[title,_String],title=StringReplace[title," "->"_"],Throw[$Failed]];
	
	If[MatchQ[start,_DateObject],
		start=StringJoin[StringReplace[ToString/@start[[1]],RegularExpression["^(.)$"]->"0$1"]]<>"000000",
		Throw[$Failed]
	];
	If[MatchQ[end,_DateObject],
		end=StringJoin[StringReplace[ToString/@end[[1]],RegularExpression["^(.)$"]->"0$1"]]<>"000000",
		Throw[$Failed]
	];

	If[MatchQ[limit,_Integer],limit=ToString[limit],Throw[$Failed]];
	
	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];
	
	parameters={
		"format"->format,
		"action"->"query",
		"prop"->"revisions",
		"rvprop"->"timestamp|content",
		"rvlimit"->limit,
		"titles"->title,
		"rvstart"->start,
		"rvend"->end,
		"continue"->""
		};
	
	If[MatchQ[continue,_String],AppendTo[parameters,Rule["rvcontinue",continue]]];

	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result
	]

wikipediacookeddata["Revisions",arg_] := Block[{result,title,limit,continue,buffer,paging,startDate,endDate,maxItems},
	(*Returns past revisions of the given page(s)*)
	startDate=Lookup[arg,"StartDate",Now];
	If[!MatchQ[startDate,_DateObject],Throw[$Failed]];

	endDate=Lookup[arg,"EndDate",DateObject[{2000, 1, 1}]];
	If[!MatchQ[endDate,_DateObject],Throw[$Failed]];

	(*setting the limit to 10 results, if we want all the revisions this value must be set to All*)
	limit=Lookup[arg,"MaxItems",10];
	If[!MatchQ[limit,_Integer]&&!MatchQ[limit,All],Throw[$Failed]];

	title=titleMapping2[arg];
	Which[MatchQ[title,_Missing],result=title,!MatchQ[title,{_String}],result=Missing["InvalidInput"]];
	
	If[MatchQ[title,{_String}],
		title=First@title;

		result={};
	
		buffer=wikipediarawdata["RawRevisions",{"Title"->title,"StartDate"->startDate,"EndDate"->endDate}];
		buffer=ImportString[buffer,"json"];
	
		paging=Cases[buffer,Rule["rvcontinue",a_]->a,10];
		paging=paging/.{a_}:>ToString[a];
	
		buffer=Cases[buffer,{___,Rule["timestamp",t_],___,Rule["*",a_],___}->{"Date"->DateObject[t,TimeZone->0],"Text"->a},10];
	
	 	AppendTo[result,#]&/@buffer;
	 	
	 	If[MatchQ[limit,All],
	 		While[paging=!={},
				buffer=wikipediarawdata["RawRevisions",{"Title"->title,"StartDate"->startDate,"EndDate"->endDate,"Continue"->paging}];
				buffer=ImportString[buffer,"json"];
			
				paging=Cases[buffer,Rule["rvcontinue",a_]->a,10];
				paging=paging/.{a_}:>ToString[a];
			
				buffer=Cases[buffer,{___,Rule["timestamp",t_],___,Rule["*",a_],___}->{"Date"->DateObject[t,TimeZone->0],"Text"->a},10];
			
			 	AppendTo[result,#]&/@buffer;
	 		]
	 	];
	 	
	 	If[MatchQ[limit,_Integer],
	 		If[limit<Length[result],
	 			result=result[[1;;limit]],
	 			While[paging=!={}&&limit>Length[result],
					buffer=wikipediarawdata["RawRevisions",{"Title"->title,"StartDate"->startDate,"EndDate"->endDate,"Continue"->paging}];
					buffer=ImportString[buffer,"json"];
				
					paging=Cases[buffer,Rule["rvcontinue",a_]->a,10];
					paging=paging/.{a_}:>ToString[a];
				
					buffer=Cases[buffer,{___,Rule["timestamp",t_],___,Rule["*",a_],___}->{"Date"->DateObject[t,TimeZone->0],"Text"->a},10];
				
				 	AppendTo[result,#]&/@buffer;
		 		];
		 		If[limit<Length[result],result=result[[1;;limit]]]
	 		]
	 	];
	];

	result
	]

(****************************************************************************************************)

wikipediacookeddata["SeeAlsoList",arg_] := Module[{result},
	result=wikipediacookeddata["SeeAlsoRules",arg];
	result=result/.(a_->b_)->b;
	If[MatchQ[result,{{__String}}],result=First@result];
	result
	]

wikipediacookeddata["SeeAlsoRules",arg_] := Module[{result,title,translate},
	title=titleMapping2[arg];

	If[MatchQ[title,_Missing],result=title];
	
	If[MatchQ[title,{__}],
		result={};

		translate=ImportString[URLFetch["http://en.wikipedia.org/w/api.php",
			"Parameters"->{"format"->"json","action"->"query","prop"->"info","redirects"->"","titles"->StringReplace[#," "->"_"],"continue"->""}],"json"]&/@Select[title,MatchQ[#,_String]&];
		translate=Cases[translate,{Rule["to",a_],Rule["from",b_]}->Rule[b,a],10];
		translate=Rule[StringReplace[#[[1]],"_"->" "],StringReplace[#[[2]],"_"->" "]]&/@translate;
		title=FixedPoint[#/.translate&,title];

		result={#,If[MatchQ[#,_String],wikipediacookeddata["ArticleWikicode",{"Title"->#}],#]}&/@title;

		result={#[[1]],If[MatchQ[#[[2]],_String],StringReplace[#[[2]],"\n"->""],#[[2]]]}&/@result;
		result={#[[1]],If[MatchQ[#[[2]],_String],StringCases[#[[2]],RegularExpression["(?i)==\\s*see also\\s*==([^=]+)(==|$)"]->"$1"],#[[2]]]}&/@result;
		result=Rule[#[[1]],If[MatchQ[#[[2]],{_String}],Flatten@StringCases[#[[2]],RegularExpression["\\[\\[([^\\[\\]]+)\\]\\]"]->"$1"],#[[2]]]]&/@result;
		result=Rule[#[[1]],If[MatchQ[#[[2]],{__String}],StringReplace[#[[2]],RegularExpression["([^|]+)\\|([^|]+)"]->"$1"],#[[2]]]]&/@result;
		result=result/.{}->Missing["NotAvailable"]
	];

	result
	]

(****************************************************************************************************)

wikipediarawdata["RawSummaryWikicode",arg_] := Block[{result,title,section,format,parameters},
	(*Get revision information - wikicode sections in this case*)
	title=Lookup[arg,"Title",Throw[$Failed]];
	section=Lookup[arg,"Section",0];
	format=Lookup[arg,"Format","json"];
	
	If[MatchQ[title,_String]||MatchQ[title,{__String}],title=StringReplace[title," "->"_"],Throw[$Failed]];
	If[MatchQ[title,{__String}],title=StringJoin[Riffle[title, "|"]]];
	If[MatchQ[section,_Integer],section=ToString[section],Throw[$Failed]];

	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];

	parameters={
		"continue"->"",
		"action"->"query",
		"format"->format,
		"prop"->"revisions",
		"rvprop"->"content",
		"rvsection"->section,
		"titles"->title,
		"redirects"->""
		};

	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result
	]

wikipediacookeddata["SummaryPlaintext",arg_] := Block[{result,title},
	title=titleMapping2[arg];

	If[MatchQ[title,_Missing],result=title];
	
	If[MatchQ[title,{__}],
		result={};
		result=If[MatchQ[#,_String],wikipediacookeddata["ArticlePlaintext",{"Title"->#}],#]&/@title;
		result=If[MatchQ[#,_String],StringTrim@First[Flatten[StringSplit[#,RegularExpression["==[^=]+=="]]]],#]&/@result;
		If[Length[result]==1,result=result[[1]]];
	];

	result
	]

wikipediacookeddata["SummaryWikicode",arg_] := Block[{result,title,section},
	(*Get revision information - wikicode sections in this case*)
	title=titleMapping2[arg];

	If[MatchQ[title,_Missing],result=title];
	
	If[MatchQ[title,{__}],
		result={};
		
		section=Lookup[arg,"Section",0];
		result=If[MatchQ[#,_String],wikipediarawdata["RawSummaryWikicode",{"Title"->#,"Section"->section}],#]&/@title;
		result=If[MatchQ[#,_String],ImportString[#,"json"],#]&/@result;
		result=If[MatchQ[#,_Missing],#,Cases[#,Rule["query",{___,Rule["pages",a__],___}]->a,10]]&/@result;
		result=If[MatchQ[#,_Missing],#,Flatten[#]]&/@result;
		result=If[MatchQ[#,_Missing],#,Cases[#,{___,Rule["title",_],___},10]]&/@result;
		result=If[MatchQ[#,_Missing],#,("revisions"/.#)]&/@result;
		result=If[MatchQ[#,_Missing],#,Flatten[#]]&/@result;
		result=If[MatchQ[#,_Missing],#,If[#=!={"revisions"},"*"/.#,Missing["NotAvailable"]]/.{s_String}->s]&/@result;

		If[Length[result]==1,result=result[[1]]];
	];

	result
	]

(****************************************************************************************************)

solveTags[rawTable_List]:=Module[{result},
	result = (StringReplace[#, {RegularExpression["<img (.*)src=\"([^\"]*)\"(.*)/>"] -> "$2"}] &) /@ rawTable;
	result = (StringReplace[#, {RegularExpression["<sup( [^>]+)*>([^<^>]*)</sup>"] -> "$2"}] &) /@ result;
	result = (StringReplace[#, {RegularExpression["<span( [^>]+)*>([^<^>]*)</span>"] -> "$2"}] &) /@ result;
	result = (StringReplace[#, {RegularExpression["<div( [^>]+)*>([^<^>]*)</div>"] -> "$2"}] &) /@ result;
	result = (StringReplace[#, {RegularExpression["<code( [^>]+)*>([^<^>]*)</code>"] -> "$2"}] &) /@ result;
	result = (StringReplace[#, {RegularExpression["<caption( [^>]+)*>([^<^>]*)</caption>"] -> "$2"}] &) /@ result;
	result = (StringReplace[#, {RegularExpression["<small( [^>]*)*>([^<^>]*)</small>"] -> "$2"}] &) /@ result;
	result = (StringReplace[#, {RegularExpression["<strong( [^>]*)*>([^<^>]*)</strong>"] -> "$2"}] &) /@ result;
	result = (StringReplace[#, {RegularExpression["<a( [^>]*)*>([^<^>]*)</a>"] -> "$2"}] &) /@ result;
	result = (StringReplace[#, {RegularExpression["<abbr( [^>]*)*>([^<^>]*)</abbr>"] -> "$2"}] &) /@ result;
	
	result = (StringReplace[#, {RegularExpression["<li( [^>]+)*>([^<^>]*)</li>"] -> "|$2|"}] &) /@ result;
	result = (StringReplace[#, {RegularExpression["<ul( [^>]+)*>([^<^>]*)</ul>"] -> "{|$2|}"}] &) /@ result;
	result = (StringReplace[#, {RegularExpression["<pre>([^<^>]*)</pre>"] -> "$1"}] &) /@ result;
	result = (StringReplace[#, {RegularExpression["<p>([^<^>]*)</p>"] -> "$1"}] &) /@ result;
	result = (StringReplace[#, {RegularExpression["<i>([^<^>]*)</i>"] -> "$1"}] &) /@ result;
	result = (StringReplace[#, {RegularExpression["<b>([^<^>]*)</b>"] -> "$1"}] &) /@ result;
	result = (StringReplace[#, {RegularExpression["<h4>([^<^>]*)</h4>"] -> "$1"}] &) /@ result;
	result = (StringReplace[#, {"<br />" -> "\n"}] &) /@ result
]

generateRawTables[html_String]:=Module[{result},
	result = StringReplace[html, {
		RegularExpression["<table( [^>]+)*>"] -> "<#table>",
		"</table>" -> "@-table-@<#table>",
		RegularExpression["<tr( [^>]+)*>"] -> "", "</tr>" -> "<#row>",
		RegularExpression["<td( [^>]+)*>"] -> "", "</td>" -> "<#div>",
		RegularExpression["<th( [^>]+)*>"] -> "", "</th>" -> "<#div>"
		}];
	result = StringSplit[result, "<#table>"];
	result = StringReplace[Select[result, ! StringFreeQ[#, "@-table-@"] &], "@-table-@" -> ""];
	result = StringSplit[#, "<#row>"] & /@ result;
	result = StringSplit[#, "<#div>"] & /@ result;
	result
]

wikipediacookeddata["Tables",arg_] := Block[{result,title},
	(*Extracts tables from html code*)
	title=titleMapping2[arg];

	If[MatchQ[title,_Missing],result=title];
	
	If[MatchQ[title,{__}],
		result=If[MatchQ[#,_String],StringReplace[URLFetch["https://en.wikipedia.org/wiki/"<>StringReplace[#," "->"_"]],{"\n"->""}],#]&/@title;
		result=If[MatchQ[#,_String],generateRawTables[#],#]&/@result;
		result=If[MatchQ[#,_List],FixedPoint[solveTags[#]&,#],#]&/@#&/@result;
	 	If[Length[result]==1,result=First@result]
	];

	result
]

(****************************************************************************************************)

wikipediacookeddata["Title",arg_] := Block[{result,title,pageid,filter,buffer,toValidate},
	(*Finds the wikipedia title of an Entity or PageID, also validates the existence of a title*)
	title=Lookup[arg,"Title",Missing["Title"]];
	pageid=Lookup[arg,"PageID",Missing["PageID"]];
	
	Which[
		!MatchQ[title,_Missing],
		
		If[!MatchQ[title,_List],title={title}];
		buffer=titleMapping2[arg];
		filter=If[MatchQ[#[[1]],_String]&&MatchQ[#[[2]],_String],True,False]&/@Transpose[{title,buffer}];
		toValidate=Pick[title,filter];
		If[toValidate=={},
			result=buffer,
			toValidate={#,ImportString[URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->{"action"->"query","titles"->#,"format"->"json"}],"json"]}&/@toValidate;
			toValidate={#[[1]],Cases[#[[2]],{___,Rule["title",_String],___},10]}&/@toValidate;
			toValidate=Rule[#[[1]],If[MatchQ[#[[2]],{{___,Rule["missing",___],___}}],Missing["TitleNotAvailable"],"title"/.#[[2]]]]&/@toValidate;
			toValidate=toValidate/.{a_String}->a;
			result=buffer/.toValidate,
			result=buffer
		];
		If[Length[result]==1,result=First@result],
		
		!MatchQ[pageid,_Missing],
		
		result=titleMapping2[arg];
		If[Length[result]==1,result=First@result],
		
		True,
		
		result=Missing["InvalidInput"]
	];
	
	result
	]

(****************************************************************************************************)

wikipediarawdata["RawTitleSearch",arg_] := Block[{result,search,limit,format,continue,parameters},
	(*Perform a title search*)
	search=Lookup[arg,"Search",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems","max"];
	format=Lookup[arg,"Format","json"];
	continue=Lookup[arg,"Continue",Missing["NotAvailable"]];

	If[!MatchQ[search,_String],Throw[$Failed]];
	If[MatchQ[limit,_Integer],limit=ToString[limit],If[limit=!="max",Throw[$Failed]]];

	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];

	search=StringJoin[Riffle[Select[StringSplit[search," "],#=!=""&],"|"]];

	parameters={
		"action"->"query",
		"list"->"search",
		"srwhat"->"text",
		"continue"->"",
		"format"->format,
		"srsearch"->"intitle:"<>search,
		"srlimit"->limit
		};

	If[MatchQ[continue,_String],AppendTo[parameters,Rule["sroffset",continue]]];

	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result
	];
	
wikipediacookeddata["TitleSearch",arg_] := Block[{result,search,limit,continue,buffer,paging},
	(*Perform a title search*)
	search=Lookup[arg,"Search",Throw[$Failed]];
	limit=Lookup[arg,"MaxItems",100];

	If[!MatchQ[search,_String],Throw[$Failed]];
	If[!MatchQ[limit,_Integer]&&!MatchQ[limit,All],Throw[$Failed]];

	result={};

	buffer=wikipediarawdata["RawTitleSearch",{"Search"->search}];
	buffer=ImportString[buffer,"json"];

	paging=Cases[buffer,Rule["sroffset",a_]->a,10];
	paging=paging/.{a_}:>ToString[a];

	buffer=Flatten[Cases[buffer,Rule["search",{a__List}]->{a},10],1];
	buffer={"Title"->("title"/.#),"Snippet"->StringReplace[("snippet"/.#),{"<span class=\"searchmatch\">"->"","</span>"->""}]}&/@buffer;
(*
	buffer={"Title"->("title"/.#),"WordCount"->("wordcount"/.#),"Size"->("size"/.#),
		"Snippet"->StringReplace[("snippet"/.#),{"<span class=\"searchmatch\">"->"","</span>"->""}],
		"Date"->DateObject[("timestamp"/.#),TimeZone->0]}&/@buffer;
*)
 	AppendTo[result,#]&/@buffer;
 	
 	If[MatchQ[limit,All],
 		While[paging=!={},
			buffer=wikipediarawdata["RawTitleSearch",{"Search"->search,"Continue"->paging}];
			buffer=ImportString[buffer,"json"];
		
			paging=Cases[buffer,Rule["sroffset",a_]->a,10];
			paging=paging/.{a_}:>ToString[a];
		
			buffer=Flatten[Cases[buffer,Rule["search",{a__List}]->{a},10],1];
			buffer={"Title"->("title"/.#),"Snippet"->StringReplace[("snippet"/.#),{"<span class=\"searchmatch\">"->"","</span>"->""}]}&/@buffer;
		(*
			buffer={"Title"->("title"/.#),"WordCount"->("wordcount"/.#),"Size"->("size"/.#),
				"Snippet"->StringReplace[("snippet"/.#),{"<span class=\"searchmatch\">"->"","</span>"->""}],
				"Date"->DateObject[("timestamp"/.#),TimeZone->0]}&/@buffer;
		*)
		 	AppendTo[result,#]&/@buffer;
 		]
 	];
 	
 	If[MatchQ[limit,_Integer],
		If[limit<Length[result],
 			result=result[[1;;limit]],
 			While[paging=!={}&&limit>Length[result],
				buffer=wikipediarawdata["RawTitleSearch",{"Search"->search,"Continue"->paging}];
				buffer=ImportString[buffer,"json"];
			
				paging=Cases[buffer,Rule["sroffset",a_]->a,10];
				paging=paging/.{a_}:>ToString[a];
			
				buffer=Flatten[Cases[buffer,Rule["search",{a__List}]->{a},10],1];
				buffer={"Title"->("title"/.#),"Snippet"->StringReplace[("snippet"/.#),{"<span class=\"searchmatch\">"->"","</span>"->""}]}&/@buffer;
			(*
				buffer={"Title"->("title"/.#),"WordCount"->("wordcount"/.#),"Size"->("size"/.#),
					"Snippet"->StringReplace[("snippet"/.#),{"<span class=\"searchmatch\">"->"","</span>"->""}],
					"Date"->DateObject[("timestamp"/.#),TimeZone->0]}&/@buffer;
			*)
			 	AppendTo[result,#]&/@buffer;
	 		];
	 		If[limit<Length[result],result=result[[1;;limit]]]
 		]
 	];

	result=Association@@@result;
	result
	]

(****************************************************************************************************)

wikipediacookeddata["TitleTranslationRules",arg_] := Module[{result,title,limit},
	title=titleMapping2[arg];
	
	If[MatchQ[title,_Missing],result=title];
	
	If[MatchQ[title,{__}],
		limit=Lookup[arg,"MaxItems",All];
		If[!MatchQ[limit,_Integer]&&!MatchQ[limit,All],Throw[$Failed]];
		
		result=If[MatchQ[#,_String],wikipediacookeddata["TitleTranslations",{"Title"->#,"MaxItems"->limit}],#]&/@title;
		
		result=result/.{{"Language"->a_,"Translation"->b_}->Rule[a, b]};
	 	If[Length[result]==1,result=First@result]
	];

	result
	]

wikipediacookeddata["TitleTranslations",arg_] := Block[{result,title,limit,buffer},
	(*Returns title translations for a given page(s)*)
	title=titleMapping2[arg];
	
	If[MatchQ[title,_Missing],result=title];
	
	If[MatchQ[title,{__}],
		limit=Lookup[arg,"MaxItems",100];
		If[!MatchQ[limit,_Integer]&&!MatchQ[limit,All],Throw[$Failed]];
		
		result={};
		buffer=If[MatchQ[#,_String],wikipediarawdata["RawLanguagesURLRules",{"Title"->#}],#]&/@title;
		buffer=If[MatchQ[#,_String],ImportString[#,"json"],#]&/@buffer;
		
	 	buffer=If[MatchQ[#,_Missing],#,Cases[#,Rule["langlinks",a_List]->a,10]]&/@buffer;
	 	buffer=If[MatchQ[#,_Missing],#,If[#=={},Missing["NotAvailable"],#]]&/@buffer;
		buffer=If[MatchQ[#,_Missing],#,Sequence@@(Rule["langname","*"]/.#)]&/@buffer;
		buffer=If[MatchQ[#,_Missing],#,{"Language"->#[[1]],"Translation"->#[[2]]}&/@#]&/@buffer;
	 	result=buffer;

	 	If[!MatchQ[limit,All],
	 		result=If[MatchQ[#,_Missing],#,If[Length[#]>=limit,#[[1;;limit]],#]]&/@result
	 	];
	 	If[Length[result]==1,result=First@result]
	];

	result
	]

(****************************************************************************************************)

wikipediarawdata["RawWikipediaRecentChanges",arg_] := Block[{result,limit,format,continue,parameters},
	(*Enumerate recent changes*)
	limit=Lookup[arg,"MaxItems","max"];
	format=Lookup[arg,"Format","json"];
	continue=Lookup[arg,"Continue",Missing["NotAvailable"]];
	
	If[MatchQ[limit,_Integer],limit=ToString[limit],If[limit=!="max",Throw[$Failed]]];
	
	If[!MatchQ[format,_String],Throw[$Failed]];
	format=ToLowerCase[format];
	If[!MemberQ[formats,format],Throw[$Failed]];

	parameters={
		"action"->"query",
		"list"->"recentchanges",
		"continue"->"",
		"format"->format,
		"rclimit"->limit,
		"rcprop"->"user|userid|timestamp|title|ids|redirect"
		};

	If[MatchQ[continue,_String],AppendTo[parameters,Rule["rccontinue",continue]]];

	result=URLFetch["http://en.wikipedia.org/w/api.php","Parameters"->parameters];
	result
	]

wikipediarawdata["RawWikipediaRecentChanges"] := wikipediarawdata["RawWikipediaRecentChanges",{}]

wikipediacookeddata["WikipediaRecentChanges",arg_] := Block[{result,limit,continue,buffer,paging},
	(*Enumerate recent changes*)
	limit=Lookup[arg,"MaxItems",100];
	If[!MatchQ[limit,All]&&!MatchQ[limit,_Integer],Throw[$Failed]];

	result={};

	buffer=wikipediarawdata["RawWikipediaRecentChanges"];
	buffer=ImportString[buffer,"json"];

	paging=Cases[buffer,Rule["rccontinue",a_String]->a,10];
	paging=paging/.{a_String}->a;

	buffer=Cases[buffer,Rule["recentchanges",{a__List}]->a,10];
	buffer={"old_revid"/.#,"ns"/.#,"type"/.#,"title"/.#,"rcid"/.#,"pageid"/.#,"user"/.#,"revid"/.#,"anon"/.#,"userid"/.#,"timestamp"/.#,"redirect"/.#}&/@buffer;
	buffer={"OldRevisionID"->#[[1]],"Namespace"->(#[[2]]/.namespaceTranslation),"Type"->#[[3]],"Title"->#[[4]],"RecentChangeID"->#[[5]],
		"PageID"->#[[6]],"User"->#[[7]],"RevisionID"->#[[8]],"Anonymous"->If[#[[9]]=="anon","no","yes"],"UserID"->#[[10]],"Date"->DateObject[#[[11]], TimeZone -> 0]}&/@buffer;
	
 	AppendTo[result,#]&/@buffer;
 	
 	If[MatchQ[limit,All],
 		While[paging=!={},
			buffer=wikipediarawdata["RawWikipediaRecentChanges",{"Continue"->paging}];
			buffer=ImportString[buffer,"json"];
		
			paging=Cases[buffer,Rule["rccontinue",a_String]->a,10];
			paging=paging/.{a_String}->a;
		
			buffer=Cases[buffer,Rule["recentchanges",{a__List}]->a,10];
			buffer={"old_revid"/.#,"ns"/.#,"type"/.#,"title"/.#,"rcid"/.#,"pageid"/.#,"user"/.#,"revid"/.#,"anon"/.#,"userid"/.#,"timestamp"/.#,"redirect"/.#}&/@buffer;
			buffer={"OldRevisionID"->#[[1]],"Namespace"->(#[[2]]/.namespaceTranslation),"Type"->#[[3]],"Title"->#[[4]],"RecentChangeID"->#[[5]],
				"PageID"->#[[6]],"User"->#[[7]],"RevisionID"->#[[8]],"Anonymous"->If[#[[9]]=="anon","no","yes"],"UserID"->#[[10]],"Date"->DateObject[#[[11]], TimeZone -> 0]}&/@buffer;
			
		 	AppendTo[result,#]&/@buffer;
 		]
 	];
 	
	If[MatchQ[limit,_Integer],
		If[limit<Length[result],
 			result=result[[1;;limit]],
 			While[paging=!={}&&limit>Length[result],
				buffer=wikipediarawdata["RawWikipediaRecentChanges",{"Continue"->paging}];
				buffer=ImportString[buffer,"json"];
			
				paging=Cases[buffer,Rule["rccontinue",a_String]->a,10];
				paging=paging/.{a_String}->a;
				
				buffer=Cases[buffer,Rule["recentchanges",{a__List}]->a,10];
				buffer={"old_revid"/.#,"ns"/.#,"type"/.#,"title"/.#,"rcid"/.#,"pageid"/.#,"user"/.#,"revid"/.#,"anon"/.#,"userid"/.#,"timestamp"/.#,"redirect"/.#}&/@buffer;
				buffer={"OldRevisionID"->#[[1]],"Namespace"->(#[[2]]/.namespaceTranslation),"Type"->#[[3]],"Title"->#[[4]],"RecentChangeID"->#[[5]],
					"PageID"->#[[6]],"User"->#[[7]],"RevisionID"->#[[8]],"Anonymous"->If[#[[9]]=="anon","no","yes"],"UserID"->#[[10]],"Date"->DateObject[#[[11]], TimeZone -> 0]}&/@buffer;
				
			 	AppendTo[result,#]&/@buffer;
	 		];
	 		If[limit<Length[result],result=result[[1;;limit]]]
 		]
 	];
	result
	]

wikipediacookeddata["WikipediaRecentChanges"] := wikipediacookeddata["WikipediaRecentChanges",{}]

wikipediacookeddata[___] := $Failed

wikipediasendmessage[___]:= $Failed

End[]

End[]

SetAttributes[{},{ReadProtected, Protected}];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{Wikipedia`Private`wikipediadata,Wikipedia`Private`wikipediacookeddata,Wikipedia`Private`wikipediasendmessage,Wikipedia`Private`wikipediarawdata}
