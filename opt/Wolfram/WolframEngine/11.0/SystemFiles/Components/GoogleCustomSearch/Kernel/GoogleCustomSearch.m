Begin["GoogleCustomSearch`"] (* Begin Private Context *)

Begin["`Private`"](* Begin Private Context *)

(******************************* GoogleCustomSearch *************************************)

(* Authentication information *)

googlecustomsearchdata[]={
		"ServiceName" 		-> "GoogleCustomSearch",
        "URLFetchFun"		:> (With[{params=Lookup[{##2},"Parameters",{}]},
        		URLFetch[#1,{"StatusCode","ContentData"},
        		Sequence@@FilterRules[{##2},Except["Parameters"|"Headers"]],
        		"Parameters"->params,
        		"Headers" -> {}]]&)
        	,
        "ClientInfo"		:> OAuthDialogDump`Private`MultipleKeyDialog["GoogleCustomSearch",{"API key"->"key","Custom search engine ID"->"cx"},
        								"https://cse.google.com/cse/manage/all","https://developers.google.com/custom-search/terms"],

	 	"Gets"				-> {"Search"},
	 	"Posts"				-> {},
	 	"RawGets"			-> {"RawGoogleSearch"},
	 	"RawPosts"			-> {},
 		"Information"		-> "Import Google Custom Search API data to the Wolfram Language"
}

(**** Raw Requests ****)

googlecustomsearchdata["RawGoogleSearch"] := {
        "URL"				-> "https://www.googleapis.com/customsearch/v1",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"cref","q","alt","callback","fields","prettyPrint","quotaUser","userIp","num","start","searchType","cr","lr","safe","dateRestrict"},
        "RequiredParameters"-> {"q"},
        "ResultsFunction"	-> formatresults
    }

googlecustomsearchdata[___]:=$Failed

(**** Cooked Requests ****)

googlecustomsearchcookeddata[prop_,id_, rest__]:=googlecustomsearchcookeddata[prop,id,{rest}]

googlecustomsearchcookeddata["Search", id_,args_List] := Block[{args2=If[MatchQ[args,_Rule],List@args,args],query,limit=10,offset=1,calls,residual,params={},rawdata,data,items={},
												fileType,country,language,safe,site,outdata,orderlist={3,2,4,1,5},imgorderlist={3,2,4,1,5},
												type="web",totalResults,invalidParameters,errorMsg,startIndex=0,progress=0,langEntity,countryEntity,
												argsCopy,dr,prefix,mag,unit},

	invalidParameters = Select[Keys[args2],!MemberQ[{"Query","FileType","Site","Language","Country","ContentFiltering","SearchType","MaxItems",MaxItems,"StartIndex","DateRestrict","Elements"},#]&];

	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"GoogleCustomSearch"]&/@invalidParameters;
			Throw[$Failed]
		)];

	argsCopy = ReplaceAll[args,Rule["MaxItems",m_]:>Rule[MaxItems,m]];
	If[KeyExistsQ[args,"Query"],
		(
			query = "Query"/.args;
			query = booleanParser[query];
		),
		(
			Message[ServiceExecute::nparam,"Query"];
			Throw[$Failed]
		)
		];

	If[KeyExistsQ[args,"FileType"],
			fileType = "FileType"/.args;
			(* file type validation *)
			query = query <> " " <> filetypeParser[fileType];
		];

	If[KeyExistsQ[args,"Site"],
			site = "Site"/.args;
			(* url format validation *)
			query = query <> " " <> siteParser[site];

		];

	If[KeyExistsQ[args,"Language"],
			language = "Language"/.args;
			If[!MatchQ[Head[language],String],
				If[MatchQ[language,Entity["Language",_]],
					If[KeyExistsQ[langMap,language],
						language=language/.langMap,
						(	(* Entity not aligned *)
							Message[ServiceExecute::nval,"Language","GoogleCustomSearch"];
							Throw[$Failed]
						)],
					(	(* Invalid type: should be String or Language Entity *)
						Message[ServiceExecute::nval,"Language","GoogleCustomSearch"];
						Throw[$Failed]
					)
				],
				(
					langEntity = Interpreter["Language"][language];
					If[MatchQ[langEntity,Entity["Language",_]] && KeyExistsQ[langMap,langEntity],
						language = langEntity /. langMap
					]
				)
			];
			params = Append[params,"lr"->language];
		];

	If[KeyExistsQ[args,"Country"],
			country = "Country"/.args;
			If[!MatchQ[Head[country],String],
				If[MatchQ[country,Entity["Country",_]],
					If[KeyExistsQ[countryMap,country],
						country=country/.countryMap,
						(
							(* Entity not aligned *)
							Message[ServiceExecute::nval,"Country","GoogleCustomSearch"];
							Throw[$Failed]
						)],
					(	(* Invalid type: should be String or Country Entity *)
						Message[ServiceExecute::nval,"Country","GoogleCustomSearch"];
						Throw[$Failed]
					)
				],
				(
					countryEntity = Interpreter["Country"][country];
					If[MatchQ[countryEntity,Entity["Country",_]] && KeyExistsQ[countryMap,countryEntity],
						country = countryEntity /. countryMap
					]
				)
			];
			params = Append[params,"cr"->country];
		];

	If[KeyExistsQ[Association[args],"ContentFiltering"],
			safe = "ContentFiltering"/.args;
			If[!MemberQ[{"Off","Medium","High","off","medium","high"},safe],
			(
				Message[ServiceExecute::nval,"ContentFiltering","GoogleCustomSearch"];
				Throw[$Failed]
			)];
			params = Append[params,"safe"-> ToLowerCase[safe]];
		];

	If[KeyExistsQ[args,"SearchType"],
			type = ToLowerCase["SearchType"/.args];
			If[!MemberQ[{"Web","Image","Images","web","image","images"},type],
			(
				Message[ServiceExecute::nval,"SearchType","GoogleCustomSearch"];
				Throw[$Failed]
			)];
		];

	If[KeyExistsQ[args,"DateRestrict"],
		(
			dr = "DateRestrict"/.args;
			prefix = "";
			mag = 0;
			If[MatchQ[dr,_Integer],
				(
					prefix = "d";
					mag = dr;
				),
				If[MatchQ[dr,_Quantity],
				(
					mag = QuantityMagnitude[dr];
					unit = QuantityUnit[dr];
					Switch[unit,
						"Days",
						prefix = "d",
						"Weeks",
						prefix = "w",
						"Months",
						prefix = "m",
						"Years",
						prefix = "y",
						_,
						(
							Message[ServiceExecute::nval,"DateRestrict","GoogleCustomSearch"];
							Throw[$Failed]
						)
					]
				),
				(
					Message[ServiceExecute::nval,"DateRestrict","GoogleCustomSearch"];
					Throw[$Failed]
				)]
			];
			params = Append[params,"dateRestrict"-> prefix <> ToString[mag]]
		)];

	If[KeyExistsQ[argsCopy,MaxItems],
			limit = MaxItems/.argsCopy;
			If[!IntegerQ[limit],
			(
				Message[ServiceExecute::nval,"MaxItems","GoogleCustomSearch"];
				Throw[$Failed]
			)];
		];

	If[KeyExistsQ[args,"Elements"],
			If[!MemberQ[{"Data","Images","Thumbnails","ImageLinks","ImageThumbnailsLinks"},"Elements"/.args2],
				Message[ServiceExecute::nval,"Elements","GoogleCustomSearch"];
				Throw[$Failed]
			]
	];

	If[KeyExistsQ[args,"StartIndex"],
			startIndex = "StartIndex"/.args;
			If[!IntegerQ[startIndex],
			(
				Message[ServiceExecute::nval,"StartIndex","GoogleCustomSearch"];
				Throw[$Failed]
			)];
		];

	calls = Quotient[limit, 10];
	residual = limit - (calls*10);

	params = Join[params,{"q"->query, "num"->"10", "start"->ToString[startIndex]}];
	If[type === "image" || type === "images", params = Append[params, Rule["searchType","image"]]];

	(* this prints the progress indicator bar *)
	PrintTemporary[ProgressIndicator[Dynamic[progress], {0, calls}]];

	If[calls > 0,
	(
		(
			params = ReplaceAll[params,Rule["start",_] -> Rule["start",ToString[startIndex+#*10+1]]];
			rawdata = KeyClient`rawkeydata[id,"RawGoogleSearch",params];
			rawdata = {rawdata[[1]],FromCharacterCode[rawdata[[2]], "UTF8"]};(*This is to correctly encode characters*)
			data = formatresults[rawdata];

			If[rawdata[[1]]!=200,
			(
				If[KeyExistsQ[data,"error"],
					(
						errorMsg = "error" /. data;
						If[KeyExistsQ[errorMsg,"message"],
						(
							Message[ServiceExecute::serrormsg,"message"/.errorMsg];
							Throw[$Failed]
						)]
					)];
				Message[ServiceExecute::serror];
				Throw[$Failed]
			)];

			totalResults = FromDigits["totalResults"/.("searchInformation"/.data)];
			items = Join[items, If[totalResults>0,("items"/.data),{}]];
			progress = progress + 1;
		)& /@ Range[0,calls-1];

	)];

	If[residual > 0,
	(
		params = ReplaceAll[params,Rule["start",_] -> Rule["start",ToString[startIndex+calls*10+1]]];
		params = ReplaceAll[params,Rule["num",_] -> Rule["num",ToString[residual]]];
		rawdata = KeyClient`rawkeydata[id,"RawGoogleSearch",params];
		rawdata = {rawdata[[1]],FromCharacterCode[rawdata[[2]], "UTF8"]};(*This is to correctly encode characters*)
		If[rawdata[[1]]!=200,
			(
				rawdata = ImportString[rawdata[[2]],"JSON"];
				If[KeyExistsQ[rawdata,"error"],
					(
						errorMsg = Cases["error" /. rawdata,Rule["message",m_]:>m,Infinity];
						Message[ServiceExecute::serrormsg,errorMsg];
						Throw[$Failed]
					)];
				Message[ServiceExecute::serror];
				Throw[$Failed]
			)];

		data = formatresults[rawdata];
		totalResults = FromDigits["totalResults"/.("searchInformation"/.data)];
		items = Join[items, If[totalResults>0,("items"/.data),{}]];
	)];

	Switch[type,
		"web",
		(
			outdata = FilterRules[#,{"link","snippet","title","pagemap"}] &/@ items;
			(*outdata = outdata[[All,orderlist]];*)
			(*outdata = ReplaceAll[outdata,Rule["link",x_]->Rule["link",Hyperlink[x]]];*)
			outdata = ReplaceAll[outdata, Rule[x_,y_] :> Rule[camelCase[x],y]];
			outdata = {If[KeyExistsQ[#,"Title"],"Title"->("Title"/.#),"Title"->Missing["NotAvailable"]],
						If[KeyExistsQ[#,"Snippet"],"Snippet"->("Snippet"/.#),"Snippet"->Missing["NotAvailable"]],
						If[KeyExistsQ[#,"Link"],"Link"->("Link"/.#),"Link"->Missing["NotAvailable"]],
						If[KeyExistsQ[#,"Pagemap"],"PageMap"->("Pagemap"/.#),"PageMap"->Missing["NotAvailable"]]}&/@outdata;
			Dataset[Association /@ outdata]
		),
		"image"|"images",
		(
			outdata = FilterRules[Join[#,"image"/.#],{"link","snippet","title","mime","width","height","contextLink","byteSize","thumbnailLink"}] &/@ items;
			outdata = Prepend[#,Rule["Thumbnail",Import["thumbnailLink"/.#]]]&/@outdata;
			(*outdata = ReplaceAll[outdata,Rule["contextLink",x_]->Rule["contextLink",Hyperlink[x]]];*)

			outdata = ReplaceAll[outdata, Rule[x_,y_] :> Rule[camelCase[x],y]];
			outdata = {If[KeyExistsQ[#,"ThumbnailLink"],"ThumbnailLink"->("ThumbnailLink"/.#),"ThumbnailLink"->Missing["NotAvailable"]],
						If[KeyExistsQ[#,"Thumbnail"],"Thumbnail"->("Thumbnail"/.#),"Thumbnail"->Missing["NotAvailable"]],
						If[KeyExistsQ[#,"Title"],"Title"->("Title"/.#),"Title"->Missing["NotAvailable"]],
						If[KeyExistsQ[#,"Snippet"],"Snippet"->("Snippet"/.#),"Snippet"->Missing["NotAvailable"]],
						If[KeyExistsQ[#,"ContextLink"],"ContextLink"->("ContextLink"/.#),"ContextLink"->Missing["NotAvailable"]],
						If[KeyExistsQ[#,"Link"],"ImageLink"->("Link"/.#),"ImageLink"->Missing["NotAvailable"]],
						If[KeyExistsQ[#,"Width"],"Width"->("Width"/.#),"Width"->Missing["NotAvailable"]],
						If[KeyExistsQ[#,"Height"],"Height"->("Height"/.#),"Height"->Missing["NotAvailable"]],
						If[KeyExistsQ[#,"ByteSize"],"ByteSize"->("ByteSize"/.#),"ByteSize"->Missing["NotAvailable"]],
						If[KeyExistsQ[#,"Mime"],"FileExtension"->("Mime"/.#),"FileExtension"->Missing["NotAvailable"]]}&/@outdata;
			Switch["Elements"/.args,
						"Images",Import/@("ImageLink"/.outdata),
						"ImageLinks","ImageLink"/.outdata,
						"Thumbnails","Thumbnail"/.outdata,
						"ImageThumbnailsLinks","ThumbnailLink"/.outdata,
						"Elements",Dataset[KeyDrop[Association /@ outdata, "ThumbnailLink"]],
						__,Throw[$Failed]
			]
			(*Dataset[Association /@ outdata]*)
		)]

]

googlecustomsearchcookeddata[req_, id_] := googlecustomsearchcookeddata[req, id,{}]

googlecustomsearchcookeddata[___]:=$Failed

googlecustomsearchsendmessage[___]:=$Failed

formatresults[rawdata_] := ImportString[rawdata[[2]],"RawJSON"]

camelCase[text_] := Module[{split, partial}, (
	(*text = ToLowerCase[text];*)
    split = StringSplit[text, {" ","_","-"}];
    partial = Prepend[Rest[Characters[#]], ToUpperCase[Characters[#][[1]]]] & /@ split;
    partial = StringJoin[partial];
    partial = StringReplace[partial,RegularExpression["[Uu][Rr][Ll]"]->"URL"];
    partial
    )]

booleanParser[e_] :=
 e //. {Verbatim[Alternatives][x_] :> x,
   Verbatim[Alternatives][x_, y__] :>
    "(" ~~ x ~~ " OR " ~~ Alternatives[y] ~~ ")",
   Verbatim[Except][x_] :> "-" ~~ x, List[x_] :> x,
   List[x_, y__] :> "(" ~~ x ~~ " AND " ~~ List[y] ~~ ")"}

siteParser[e_] := Module[{tmp}, (
   tmp = e /. {Verbatim[Except][Verbatim[Alternatives][x___]] :>
       List[Sequence @@ Except /@ List[x]],
      Verbatim[Except][List[x___]] :>
       Alternatives[Sequence @@ Except /@ List[x]]};
   tmp = booleanParser[tmp];
   tmp = StringJoin[
     Riffle[(If[! MemberQ[{"AND", "OR", "("}, #],
          If[StringMatchQ[#, "-" ~~ ___],
           "-site:" ~~ StringDrop[#, 1], "site:" ~~ #], #]) & /@
       Flatten[(If[
            StringMatchQ[#, "(" ~~ ___], {"(",
             StringDrop[#, 1]}, #] & /@ StringSplit[tmp])], " "]];
   StringReplace[tmp, {"( " -> "", ")" -> ""}]
   )]

filetypeParser[e_] := Module[{tmp}, (
   tmp = e /. {Verbatim[Except][Verbatim[Alternatives][x___]] :>
       List[Sequence @@ Except /@ List[x]],
      Verbatim[Except][List[x___]] :>
       Alternatives[Sequence @@ Except /@ List[x]]};
   tmp = tmp /. Entity["FileFormat", x_] :> StringReplace[EntityValue[Entity["FileFormat", x], "Extension"], "." -> ""];

   tmp = booleanParser[tmp];
   tmp = StringReplace[tmp, "." -> ""];

   tmp = StringJoin[
     Riffle[(If[! MemberQ[{"AND", "OR", "("}, #],
          If[StringMatchQ[#, "-" ~~ ___],
           "-filetype:" ~~ StringDrop[#, 1],
           "filetype:" ~~ #], #]) & /@
       Flatten[(If[
            StringMatchQ[#, "(" ~~ ___], {"(",
             StringDrop[#, 1]}, #] & /@ StringSplit[tmp])], " "]];
   StringReplace[tmp, {"( " -> "", ")" -> ""}]
   )]

allowedLanguageValues = {"lang_ar", "lang_bg", "lang_ca",
  "lang_zh-CN", "lang_zh-TW", "lang_hr", "lang_cs", "lang_da",
  "lang_nl", "lang_en", "lang_et", "lang_fi", "lang_fr", "lang_de",
  "lang_el", "lang_iw", "lang_hu", "lang_is", "lang_id", "lang_it",
  "lang_ja", "lang_ko", "lang_lv", "lang_lt", "lang_no", "lang_pl",
  "lang_pt", "lang_ro", "lang_ru", "lang_sr", "lang_sk", "lang_sl",
  "lang_es", "lang_sv", "lang_tr"};

allowedCountryValues = {"countryAF", "countryAL", "countryDZ", "countryAS", "countryAD", \
"countryAO", "countryAI", "countryAQ", "countryAG", "countryAR", \
"countryAM", "countryAW", "countryAU", "countryAT", "countryAZ", \
"countryBS", "countryBH", "countryBD", "countryBB", "countryBY", \
"countryBE", "countryBZ", "countryBJ", "countryBM", "countryBT", \
"countryBO", "countryBA", "countryBW", "countryBV", "countryBR", \
"countryIO", "countryBN", "countryBG", "countryBF", "countryBI", \
"countryKH", "countryCM", "countryCA", "countryCV", "countryKY", \
"countryCF", "countryTD", "countryCL", "countryCN", "countryCX", \
"countryCC", "countryCO", "countryKM", "countryCG", "countryCD", \
"countryCK", "countryCR", "countryCI", "countryHR", "countryCU", \
"countryCY", "countryCZ", "countryDK", "countryDJ", "countryDM", \
"countryDO", "countryTP", "countryEC", "countryEG", "countrySV", \
"countryGQ", "countryER", "countryEE", "countryET", "countryEU", \
"countryFK", "countryFO", "countryFJ", "countryFI", "countryFR", \
"countryFX", "countryGF", "countryPF", "countryTF", "countryGA", \
"countryGM", "countryGE", "countryDE", "countryGH", "countryGI", \
"countryGR", "countryGL", "countryGD", "countryGP", "countryGU", \
"countryGT", "countryGN", "countryGW", "countryGY", "countryHT", \
"countryHM", "countryVA", "countryHN", "countryHK", "countryHU", \
"countryIS", "countryIN", "countryID", "countryIR", "countryIQ", \
"countryIE", "countryIL", "countryIT", "countryJM", "countryJP", \
"countryJO", "countryKZ", "countryKE", "countryKI", "countryKP", \
"countryKR", "countryKW", "countryKG", "countryLA", "countryLV", \
"countryLB", "countryLS", "countryLR", "countryLY", "countryLI", \
"countryLT", "countryLU", "countryMO", "countryMK", "countryMG", \
"countryMW", "countryMY", "countryMV", "countryML", "countryMT", \
"countryMH", "countryMQ", "countryMR", "countryMU", "countryYT", \
"countryMX", "countryFM", "countryMD", "countryMC", "countryMN", \
"countryMS", "countryMA", "countryMZ", "countryMM", "countryNA", \
"countryNR", "countryNP", "countryNL", "countryAN", "countryNC", \
"countryNZ", "countryNI", "countryNE", "countryNG", "countryNU", \
"countryNF", "countryMP", "countryNO", "countryOM", "countryPK", \
"countryPW", "countryPS", "countryPA", "countryPG", "countryPY", \
"countryPE", "countryPH", "countryPN", "countryPL", "countryPT", \
"countryPR", "countryQA", "countryRE", "countryRO", "countryRU", \
"countryRW", "countrySH", "countryKN", "countryLC", "countryPM", \
"countryVC", "countryWS", "countrySM", "countryST", "countrySA", \
"countrySN", "countryCS", "countrySC", "countrySL", "countrySG", \
"countrySK", "countrySI", "countrySB", "countrySO", "countryZA", \
"countryGS", "countryES", "countryLK", "countrySD", "countrySR", \
"countrySJ", "countrySZ", "countrySE", "countryCH", "countrySY", \
"countryTW", "countryTJ", "countryTZ", "countryTH", "countryTG", \
"countryTK", "countryTO", "countryTT", "countryTN", "countryTR", \
"countryTM", "countryTC", "countryTV", "countryUG", "countryUA", \
"countryAE", "countryUK", "countryUS", "countryUM", "countryUY", \
"countryUZ", "countryVU", "countryVE", "countryVN", "countryVG", \
"countryVI", "countryWF", "countryEH", "countryYE", "countryYU", \
"countryZM", "countryZW"}

langMap = {Entity["Language", "Arabic"] -> "lang_ar",
   Entity["Language", "Bulgarian"] -> "lang_bg",
   Entity["Language", "CatalanValencianBalear"] -> "lang_ca",
   Entity["Language", "Croatian"] -> "lang_hr",
   Entity["Language", "Czech"] -> "lang_cs",
   Entity["Language", "Danish"] -> "lang_da",
   Entity["Language", "Dutch"] -> "lang_nl",
   Entity["Language", "English"] -> "lang_en",
   Entity["Language", "Estonian"] -> "lang_et",
   Entity["Language", "Finnish"] -> "lang_fi",
   Entity["Language", "French"] -> "lang_fr",
   Entity["Language", "German"] -> "lang_de",
   Entity["Language", "Greek"] -> "lang_el",
   Entity["Language", "Hebrew"] -> "lang_iw",
   Entity["Language", "Hungarian"] -> "lang_hu",
   Entity["Language", "Icelandic"] -> "lang_is",
   Entity["Language", "Indonesian"] -> "lang_id",
   Entity["Language", "Italian"] -> "lang_it",
   Entity["Language", "Japanese"] -> "lang_ja",
   Entity["Language", "Korean"] -> "lang_ko",
   Entity["Language", "Latvian"] -> "lang_lv",
   Entity["Language", "Lithuanian"] -> "lang_lt",
   Entity["Language", "Norwegian"] -> "lang_no",
   Entity["Language", "Polish"] -> "lang_pl",
   Entity["Language", "Portuguese"] -> "lang_pt",
   Entity["Language", "Romanian"] -> "lang_ro",
   Entity["Language", "Russian"] -> "lang_ru",
   Entity["Language", "Serbian"] -> "lang_sr",
   Entity["Language", "Slovak"] -> "lang_sk",
   Entity["Language", "Slovenian"] -> "lang_sl",
   Entity["Language", "Spanish"] -> "lang_es",
   Entity["Language", "Swedish"] -> "lang_sv",
   Entity["Language", "Turkish"] -> "lang_tr"};

countryMap = {Entity["Country", "Afghanistan"] -> "countryAF",
 Entity["Country", "Albania"] -> "countryAL",
 Entity["Country", "Algeria"] -> "countryDZ",
 Entity["Country", "AmericanSamoa"] -> "countryAS",
 Entity["Country", "Andorra"] -> "countryAD",
 Entity["Country", "Angola"] -> "countryAO",
 Entity["Country", "Anguilla"] -> "countryAI",
 Entity["Country", "AntiguaBarbuda"] -> "countryAG",
 Entity["Country", "Argentina"] -> "countryAR",
 Entity["Country", "Armenia"] -> "countryAM",
 Entity["Country", "Aruba"] -> "countryAW",
 Entity["Country", "Australia"] -> "countryAU",
 Entity["Country", "Austria"] -> "countryAT",
 Entity["Country", "Azerbaijan"] -> "countryAZ",
 Entity["Country", "Bahamas"] -> "countryBS",
 Entity["Country", "Bahrain"] -> "countryBH",
 Entity["Country", "Bangladesh"] -> "countryBD",
 Entity["Country", "Barbados"] -> "countryBB",
 Entity["Country", "Belarus"] -> "countryBY",
 Entity["Country", "Belgium"] -> "countryBE",
 Entity["Country", "Belize"] -> "countryBZ",
 Entity["Country", "Benin"] -> "countryBJ",
 Entity["Country", "Bermuda"] -> "countryBM",
 Entity["Country", "Bhutan"] -> "countryBT",
 Entity["Country", "Bolivia"] -> "countryBO",
 Entity["Country", "BosniaHerzegovina"] -> "countryBA",
 Entity["Country", "Botswana"] -> "countryBW",
 Entity["Country", "Brazil"] -> "countryBR",
 Entity["Country", "Brunei"] -> "countryBN",
 Entity["Country", "Bulgaria"] -> "countryBG",
 Entity["Country", "BurkinaFaso"] -> "countryBF",
 Entity["Country", "Burundi"] -> "countryBI",
 Entity["Country", "Cambodia"] -> "countryKH",
 Entity["Country", "Cameroon"] -> "countryCM",
 Entity["Country", "Canada"] -> "countryCA",
 Entity["Country", "CapeVerde"] -> "countryCV",
 Entity["Country", "CaymanIslands"] -> "countryKY",
 Entity["Country", "CentralAfricanRepublic"] -> "countryCF",
 Entity["Country", "Chad"] -> "countryTD",
 Entity["Country", "Chile"] -> "countryCL",
 Entity["Country", "China"] -> "countryCN",
 Entity["Country", "ChristmasIsland"] -> "countryCX",
 Entity["Country", "CocosKeelingIslands"] -> "countryCC",
 Entity["Country", "Colombia"] -> "countryCO",
 Entity["Country", "Comoros"] -> "countryKM",
 Entity["Country", "DemocraticRepublicCongo"] -> "countryCG",
 Entity["Country", "DemocraticRepublicCongo"] -> "countryCD",
 Entity["Country", "CookIslands"] -> "countryCK",
 Entity["Country", "CostaRica"] -> "countryCR",
 Entity["Country", "IvoryCoast"] -> "countryCI",
 Entity["Country", "Cuba"] -> "countryCU",
 Entity["Country", "Cyprus"] -> "countryCY",
 Entity["Country", "CzechRepublic"] -> "countryCZ",
 Entity["Country", "Denmark"] -> "countryDK",
 Entity["Country", "Djibouti"] -> "countryDJ",
 Entity["Country", "Dominica"] -> "countryDM",
 Entity["Country", "DominicanRepublic"] -> "countryDO",
 Entity["Country", "EastTimor"] -> "countryTP",
 Entity["Country", "Ecuador"] -> "countryEC",
 Entity["Country", "Egypt"] -> "countryEG",
 Entity["Country", "ElSalvador"] -> "countrySV",
 Entity["Country", "EquatorialGuinea"] -> "countryGQ",
 Entity["Country", "Eritrea"] -> "countryER",
 Entity["Country", "Estonia"] -> "countryEE",
 Entity["Country", "Ethiopia"] -> "countryET",
 Entity["Country", "FalklandIslands"] -> "countryFK",
 Entity["Country", "FaroeIslands"] -> "countryFO",
 Entity["Country", "Fiji"] -> "countryFJ",
 Entity["Country", "Finland"] -> "countryFI",
 Entity["Country", "France"] -> "countryFR",
 Entity["Country", "FrenchGuiana"] -> "countryGF",
 Entity["Country", "FrenchPolynesia"] -> "countryPF",
 Entity["Country", "Gabon"] -> "countryGA",
 Entity["Country", "Gambia"] -> "countryGM",
 Entity["Country", "Georgia"] -> "countryGE",
 Entity["Country", "Germany"] -> "countryDE",
 Entity["Country", "Ghana"] -> "countryGH",
 Entity["Country", "Gibraltar"] -> "countryGI",
 Entity["Country", "Greece"] -> "countryGR",
 Entity["Country", "Greenland"] -> "countryGL",
 Entity["Country", "Grenada"] -> "countryGD",
 Entity["Country", "Guadeloupe"] -> "countryGP",
 Entity["Country", "Guam"] -> "countryGU",
 Entity["Country", "Guatemala"] -> "countryGT",
 Entity["Country", "Guinea"] -> "countryGN",
 Entity["Country", "GuineaBissau"] -> "countryGW",
 Entity["Country", "Guyana"] -> "countryGY",
 Entity["Country", "Haiti"] -> "countryHT",
 Entity["Country", "VaticanCity"] -> "countryVA",
 Entity["Country", "Honduras"] -> "countryHN",
 Entity["Country", "HongKong"] -> "countryHK",
 Entity["Country", "Hungary"] -> "countryHU",
 Entity["Country", "Iceland"] -> "countryIS",
 Entity["Country", "India"] -> "countryIN",
 Entity["Country", "Indonesia"] -> "countryID",
 Entity["Country", "Iran"] -> "countryIR",
 Entity["Country", "Iraq"] -> "countryIQ",
 Entity["Country", "Ireland"] -> "countryIE",
 Entity["Country", "Israel"] -> "countryIL",
 Entity["Country", "Italy"] -> "countryIT",
 Entity["Country", "Jamaica"] -> "countryJM",
 Entity["Country", "Japan"] -> "countryJP",
 Entity["Country", "Jordan"] -> "countryJO",
 Entity["Country", "Kazakhstan"] -> "countryKZ",
 Entity["Country", "Kenya"] -> "countryKE",
 Entity["Country", "Kiribati"] -> "countryKI",
 Entity["Country", "SouthKorea"] -> "countryKR",
 Entity["Country", "Kuwait"] -> "countryKW",
 Entity["Country", "Kyrgyzstan"] -> "countryKG",
 Entity["Country", "Laos"] -> "countryLA",
 Entity["Country", "Latvia"] -> "countryLV",
 Entity["Country", "Lebanon"] -> "countryLB",
 Entity["Country", "Lesotho"] -> "countryLS",
 Entity["Country", "Liberia"] -> "countryLR",
 Entity["Country", "Libya"] -> "countryLY",
 Entity["Country", "Liechtenstein"] -> "countryLI",
 Entity["Country", "Lithuania"] -> "countryLT",
 Entity["Country", "Luxembourg"] -> "countryLU",
 Entity["Country", "Macau"] -> "countryMO",
 Entity["Country", "Macedonia"] -> "countryMK",
 Entity["Country", "Madagascar"] -> "countryMG",
 Entity["Country", "Malawi"] -> "countryMW",
 Entity["Country", "Malaysia"] -> "countryMY",
 Entity["Country", "Maldives"] -> "countryMV",
 Entity["Country", "Mali"] -> "countryML",
 Entity["Country", "Malta"] -> "countryMT",
 Entity["Country", "MarshallIslands"] -> "countryMH",
 Entity["Country", "Martinique"] -> "countryMQ",
 Entity["Country", "Mauritania"] -> "countryMR",
 Entity["Country", "Mauritius"] -> "countryMU",
 Entity["Country", "Mayotte"] -> "countryYT",
 Entity["Country", "Mexico"] -> "countryMX",
 Entity["Country", "Micronesia"] -> "countryFM",
 Entity["Country", "Moldova"] -> "countryMD",
 Entity["Country", "Monaco"] -> "countryMC",
 Entity["Country", "Mongolia"] -> "countryMN",
 Entity["Country", "Montserrat"] -> "countryMS",
 Entity["Country", "Morocco"] -> "countryMA",
 Entity["Country", "Mozambique"] -> "countryMZ",
 Entity["Country", "Myanmar"] -> "countryMM",
 Entity["Country", "Namibia"] -> "countryNA",
 Entity["Country", "Nauru"] -> "countryNR",
 Entity["Country", "Nepal"] -> "countryNP",
 Entity["Country", "Netherlands"] -> "countryNL",
 Entity["Country", "NetherlandsAntilles"] -> "countryAN",
 Entity["Country", "NewCaledonia"] -> "countryNC",
 Entity["Country", "NewZealand"] -> "countryNZ",
 Entity["Country", "Nicaragua"] -> "countryNI",
 Entity["Country", "Niger"] -> "countryNE",
 Entity["Country", "Nigeria"] -> "countryNG",
 Entity["Country", "Niue"] -> "countryNU",
 Entity["Country", "NorfolkIsland"] -> "countryNF",
 Entity["Country", "NorthernMarianaIslands"] -> "countryMP",
 Entity["Country", "Norway"] -> "countryNO",
 Entity["Country", "Oman"] -> "countryOM",
 Entity["Country", "Pakistan"] -> "countryPK",
 Entity["Country", "Palau"] -> "countryPW",
 Entity["Country", "WestBank"] -> "countryPS",
 Entity["Country", "Panama"] -> "countryPA",
 Entity["Country", "PapuaNewGuinea"] -> "countryPG",
 Entity["Country", "Paraguay"] -> "countryPY",
 Entity["Country", "Peru"] -> "countryPE",
 Entity["Country", "Philippines"] -> "countryPH",
 Entity["Country", "PitcairnIslands"] -> "countryPN",
 Entity["Country", "Poland"] -> "countryPL",
 Entity["Country", "Portugal"] -> "countryPT",
 Entity["Country", "PuertoRico"] -> "countryPR",
 Entity["Country", "Qatar"] -> "countryQA",
 Entity["Country", "Reunion"] -> "countryRE",
 Entity["Country", "Romania"] -> "countryRO",
 Entity["Country", "Russia"] -> "countryRU",
 Entity["Country", "Rwanda"] -> "countryRW",
 Entity["Country", "SaintHelena"] -> "countrySH",
 Entity["Country", "SaintKittsNevis"] -> "countryKN",
 Entity["Country", "SaintLucia"] -> "countryLC",
 Entity["Country", "SaintPierreMiquelon"] -> "countryPM",
 Entity["Country", "SaintVincentGrenadines"] -> "countryVC",
 Entity["Country", "Samoa"] -> "countryWS",
 Entity["Country", "SanMarino"] -> "countrySM",
 Entity["Country", "SaoTomePrincipe"] -> "countryST",
 Entity["Country", "SaudiArabia"] -> "countrySA",
 Entity["Country", "Senegal"] -> "countrySN",
 Entity["Country", "Seychelles"] -> "countrySC",
 Entity["Country", "SierraLeone"] -> "countrySL",
 Entity["Country", "Singapore"] -> "countrySG",
 Entity["Country", "Slovakia"] -> "countrySK",
 Entity["Country", "Slovenia"] -> "countrySI",
 Entity["Country", "SolomonIslands"] -> "countrySB",
 Entity["Country", "Somalia"] -> "countrySO",
 Entity["Country", "SouthAfrica"] -> "countryZA",
 Entity["Country", "Spain"] -> "countryES",
 Entity["Country", "SriLanka"] -> "countryLK",
 Entity["Country", "Sudan"] -> "countrySD",
 Entity["Country", "Suriname"] -> "countrySR",
 Entity["Country", "Svalbard"] -> "countrySJ",
 Entity["Country", "Swaziland"] -> "countrySZ",
 Entity["Country", "Sweden"] -> "countrySE",
 Entity["Country", "Switzerland"] -> "countryCH",
 Entity["Country", "Syria"] -> "countrySY",
 Entity["Country", "Taiwan"] -> "countryTW",
 Entity["Country", "Tajikistan"] -> "countryTJ",
 Entity["Country", "Tanzania"] -> "countryTZ",
 Entity["Country", "Thailand"] -> "countryTH",
 Entity["Country", "Togo"] -> "countryTG",
 Entity["Country", "Tokelau"] -> "countryTK",
 Entity["Country", "Tonga"] -> "countryTO",
 Entity["Country", "TrinidadTobago"] -> "countryTT",
 Entity["Country", "Tunisia"] -> "countryTN",
 Entity["Country", "Turkey"] -> "countryTR",
 Entity["Country", "Turkmenistan"] -> "countryTM",
 Entity["Country", "TurksCaicosIslands"] -> "countryTC",
 Entity["Country", "Tuvalu"] -> "countryTV",
 Entity["Country", "Uganda"] -> "countryUG",
 Entity["Country", "Ukraine"] -> "countryUA",
 Entity["Country", "UnitedArabEmirates"] -> "countryAE",
 Entity["Country", "UnitedKingdom"] -> "countryUK",
 Entity["Country", "UnitedStates"] -> "countryUS",
 Entity["Country", "Uruguay"] -> "countryUY",
 Entity["Country", "Uzbekistan"] -> "countryUZ",
 Entity["Country", "Vanuatu"] -> "countryVU",
 Entity["Country", "Venezuela"] -> "countryVE",
 Entity["Country", "Vietnam"] -> "countryVN",
 Entity["Country", "BritishVirginIslands"] -> "countryVG",
 Entity["Country", "UnitedStatesVirginIslands"] -> "countryVI",
 Entity["Country", "WallisFutuna"] -> "countryWF",
 Entity["Country", "WesternSahara"] -> "countryEH",
 Entity["Country", "Yemen"] -> "countryYE",
 Entity["Country", "Zambia"] -> "countryZM",
 Entity["Country", "Zimbabwe"] -> "countryZW",
 Entity["Country", "NorthKorea"] -> "countryKP",
 Entity["Country", "Serbia"] -> "countryCS",
 Entity["Country", "Montenegro"] -> "countryCS",
 Entity["Country", "Croatia"] -> "countryHR"};

End[]

End[]

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{GoogleCustomSearch`Private`googlecustomsearchdata,GoogleCustomSearch`Private`googlecustomsearchcookeddata,GoogleCustomSearch`Private`googlecustomsearchsendmessage}
