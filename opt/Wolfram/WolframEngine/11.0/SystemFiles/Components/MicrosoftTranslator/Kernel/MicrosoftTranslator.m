
Begin["MicrosoftTranslatorAPI`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* AT&T Speech API*************************************)

(* Authentication information *)

microsofttranslatordata[]={
		"ServiceName" 		-> "MicrosoftTranslator", 
        "URLFetchFun"		:> (With[{params=Lookup[{##2},"Parameters",{}],headers=Lookup[{##2},"Headers",{}]},
        	(URLFetch[#1 <> "?" <> StringJoin[Riffle[(#[[1]] <> "=" <> #[[2]] & /@ FilterRules[params,Except["id"|"secret"]]),"&"]],{"StatusCode","ContentData"},Sequence@@FilterRules[{##2},Except["Parameters"|"Headers"]],        	        		
        		"Headers"->Join[{"Authorization" -> ("Bearer " <> getToken["id"/.params,"secret"/.params])},headers]])]&)
        	
        	,
        "ClientInfo"		:> OAuthDialogDump`Private`MultipleKeyDialog["MicrosoftTranslator",{"Client ID"->"id","Client secret"->{"secret",FieldMasked->True}},
        				"https://datamarket.azure.com/dataset/bing/microsofttranslator","https://datamarket.azure.com/dataset/bing/microsofttranslator#terms"],
	 	"Gets"				-> {"LanguageList","LanguageEntityList"},
	 	"Posts"				-> {"GetTranslations","Translate","RoundTripTranslate"},
	 	"RawGets"			-> {"RawGetLanguagesForTranslate","RawLanguageCodeList","RawTranslate"},
	 	"RawPosts"			-> {"RawGetTranslations","RawTranslateArray"},
 		"Information"		-> "Use Microsoft Translator API with Wolfram Language"
}

(*microsofttranslatorimport[rawdata_]:=ImportString[StringReplace[StringReplace[ToString[rawdata], "[" ~~ data___ ~~ "]" :> data], ___ ~~ "," ~~ xml___ ~~ "}" :> xml], "XML"]*)
microsofttranslatorimport[rawdata_]:= Module[{data},
(
	(*data = StringReplace[ToString[rawdata], "[" ~~ d___ ~~ "]" :> d]; This was before using ContentData*)
	data = StringReplace[ToString[FromCharacterCode[rawdata[[2]], "UTF8"]], "[" ~~ d___ ~~ "]" :> d];
	StringReplace[data, Shortest[___] ~~ "," ~~ xml___ ~~ "}" :> xml]
)]

importLanguageCodes[rawdata_]:= Flatten[ImportString[microsofttranslatorimport[rawdata], "XML"][[2, 3]][[All, 3]]];
(*importLanguageCodes[rawdata_]:= Flatten[ImportString[StringReplace[ToString@rawdata,"[" ~~ xml___ ~~ "]" :> xml], "XML"][[2, 3]][[All, 3]]];*)

(* Raw *)
microsofttranslatordata["RawGetLanguagesForTranslate"] := {
        "URL"				-> "http://api.microsofttranslator.com/V2/Http.svc/GetLanguagesForTranslate",
		"HTTPSMethod"		-> "GET",
        "Parameters"		-> {},
        "RequiredParameters"-> {},        
        "ResultsFunction"	-> microsofttranslatorimport
    }
    
microsofttranslatordata["RawLanguageCodeList"] := {
        "URL"				-> "http://api.microsofttranslator.com/V2/Http.svc/GetLanguagesForTranslate",
		"HTTPSMethod"		-> "GET",
        "Parameters"		-> {},
        "RequiredParameters"-> {},        
        "ResultsFunction"	-> importLanguageCodes
    } 

microsofttranslatordata["RawGetTranslations"] := {
        "URL"				-> "http://api.microsofttranslator.com/V2/Http.svc/GetTranslations",
		"HTTPSMethod"		-> "POST",
		"Headers"			-> {"Content-Type" -> "text/xml", "Content-Length" -> "0"},
        "Parameters"		-> {"text","from","to","maxTranslations"},
        "RequiredParameters"-> {"text","from","to","maxTranslations"},        
        "ResultsFunction"	-> microsofttranslatorimport
    }
    
microsofttranslatordata["RawTranslate"] := {
        "URL"				-> "http://api.microsofttranslator.com/V2/Http.svc/Translate",
		"HTTPSMethod"		-> "GET",
		"Headers"			-> {"Content-Type" -> "text/plain"},
		"Parameters"		-> {"text","from","to"},
        "RequiredParameters"-> {"text","to"},        
        "ResultsFunction"	-> microsofttranslatorimport
    }
    
microsofttranslatordata["RawTranslateArray"] := {
        "URL"				-> "http://api.microsofttranslator.com/V2/Http.svc/TranslateArray",
		"HTTPSMethod"		-> "POST",
		"BodyData"			-> {"ParameterlessBodyData"->"Data"},
		"Headers"			-> {"Content-Type" -> "text/xml"},
		"RequiredParameters"-> {"Data"},        
        "ResultsFunction"	-> microsofttranslatorimport
    }
    

   
(* Cooked *)

microsofttranslatorcookeddata["LanguageList", id_, args_] := Block[{rawdata,dataxml,list,result,status,msg},
	If[Length[args]>0,
		(
			Message[ServiceObject::noget,#[[1]],"MicrosoftTranslator"]&/@args;
			Throw[$Failed]
		)];		
	
	
	rawdata = KeyClient`rawkeydata[id,"RawGetLanguagesForTranslate",{}];
	(*rawdata = ServiceExecute["MicrosoftTranslator", "RawGetLanguagesForTranslate", {}];*)	
	(*rawdata = StringReplace[ToString[rawdata], "[" ~~ data___ ~~ "]" :> data]; This was before using ContentData*)
	rawdata = ToString[{rawdata[[1]], StringReplace[ToString[FromCharacterCode[rawdata[[2]], "UTF8"]], "[" ~~ data___ ~~ "]" :> data]}];
	dataxml = ImportString[StringReplace[rawdata, Shortest[___] ~~ "," ~~ xml___ ~~ "}" :> xml], "XML"];
	
	status = StringReplace[rawdata, "{" ~~ Shortest[s___] ~~ "," ~~ ___ :> s];
	If[status != "200",
	(
		msg = Cases[dataxml, XMLElement["p", _, content_List] :> content, Infinity];
		Message[ServiceExecute::serrormsg,msg];
		Throw[$Failed]
	)];
		
	
	list = Flatten[dataxml[[2, 3]][[All, 3]]];
	list
	
]

microsofttranslatorcookeddata["LanguageEntityList", id_, args_] := Block[{rawdata,dataxml,list,result,status,newlist},
	If[Length[args]>0,
		(
			Message[ServiceObject::noget,#[[1]],"MicrosoftTranslator"]&/@args;
			Throw[$Failed]
		)];		
	entityToLanguageCodeAlignment[[All,1]]
]

microsofttranslatorcookeddata["GetTranslations", id_, args_] := Block[{params,textP,toP,fromP,maxP,rawdata,dataxml,translations,status,invalidParameters,msg},
	invalidParameters = Select[Keys[args],!MemberQ[{"Text","From","To","MaxTranslations"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"GetTranslations"]&/@invalidParameters;
			Throw[$Failed]
		)];	
	
	If[KeyExistsQ[args,"Text"],
		textP = URLEncode["Text" /. args],
		(
			Message[ServiceExecute::nparam,"Text"];			
			Throw[$Failed]
		)
	];
	
	If[KeyExistsQ[args,"From"],
		(
			fromP = "From" /. args;
			fromP = mapParameterToLanguageCode[fromP];
			If[fromP == "Failed",
			(	
				Message[ServiceExecute::nval,"From","MicrosoftTranslator"];
				Throw[$Failed]
			)]
		),
		(
			Message[ServiceExecute::nparam,"From"];			
			Throw[$Failed]
		)
	];
	
	If[KeyExistsQ[args,"To"],
		(
			toP = "To" /. args;
			toP = mapParameterToLanguageCode[toP];
			If[toP == "Failed",
			(	
				Message[ServiceExecute::nval,"To","MicrosoftTranslator"];
				Throw[$Failed]
			)]
		),
		(
			Message[ServiceExecute::nparam,"To"];			
			Throw[$Failed]
		)
	];
	
	If[KeyExistsQ[args,"MaxTranslations"],
		maxP = ToString["MaxTranslations" /. args],
		(
			Message[ServiceExecute::nparam,"MaxTranslations"];			
			Throw[$Failed]
		)
	];
	
	params = {"text"->textP, "from"->fromP, "to"->toP, "maxTranslations"->maxP};
	
	rawdata = KeyClient`rawkeydata[id,"RawGetTranslations",params];
		
	(*rawdata = StringReplace[ToString[rawdata], "[" ~~ data___ ~~ "]" :> data]; This was before using ContentData*)
	rawdata = ToString[{rawdata[[1]], StringReplace[ToString[FromCharacterCode[rawdata[[2]], "UTF8"]], "[" ~~ data___ ~~ "]" :> data]}];
	dataxml = ImportString[StringReplace[rawdata, Shortest[___] ~~ "," ~~ xml___ ~~ "}" :> xml], "XML"];
	status = StringReplace[rawdata, "{" ~~ Shortest[s___] ~~ "," ~~ ___ :> s];
	If[status != "200",
	(
		msg = Cases[dataxml, XMLElement["p", _, content_List] :> content, Infinity];
		Message[ServiceExecute::serrormsg,msg];
		Throw[$Failed]
	)];
		
	translations = Cases[dataxml, XMLElement["TranslationMatch", _, _], Infinity];
	
	Dataset[Association /@ (formatTranslations /@ translations)]
]

microsofttranslatorcookeddata["Translate", id_, args_] := Block[{params,textP,toP,fromP,rawdata,dataxml,translations,results,languageRules=False,groups,tuples,f,current,status,invalidParameters,msg},
	invalidParameters = Select[Keys[args],!MemberQ[{"LanguageRules","Text","From","To"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"GetTranslations"]&/@invalidParameters;
			Throw[$Failed]
		)];
	
	If[KeyExistsQ[args,"LanguageRules"],languageRules = "LanguageRules" /. args];
	
	If[KeyExistsQ[args,"Text"],
		textP = "Text" /. args,
		(
			Message[ServiceExecute::nparam,"Text"];			
			Throw[$Failed]
		)
	];
	
	If[KeyExistsQ[args,"From"],
		(
			fromP = "From" /. args;
			fromP = mapParameterToLanguageCode[fromP];
			If[fromP == "Failed",
			(	
				Message[ServiceExecute::nval,"From","MicrosoftTranslator"];
				Throw[$Failed]
			)];
			If[MatchQ[Head[textP], List],tuples = {fromP,textP[[#]],#} &/@ Range[Length[textP]]]			
		),
			If[MatchQ[Head[textP], String],(*Autodetects the language using WL*)
				fromP = mapParameterToLanguageCode[Classify["Language",textP]],
				If[MatchQ[Head[textP], List],tuples = ({mapParameterToLanguageCode[Classify["Language",textP[[#]]]],textP[[#]],#})&/@Range[Length[textP]],
				(	
					Message[ServiceExecute::nval,"From","MicrosoftTranslator"];
					Throw[$Failed]
				)]
			]; 
	];
	
	If[KeyExistsQ[args,"To"],
		(
			toP = "To" /. args;
			toP = mapParameterToLanguageCode[toP];
			If[toP == "Failed",
			(	
				Message[ServiceExecute::nval,"To","MicrosoftTranslator"];
				Throw[$Failed]
			)]
		),
		(
			Message[ServiceExecute::nparam,"To"];			
			Throw[$Failed]
		)
	];
	
	If[MatchQ[textP, _String],
		(
			rawdata = KeyClient`rawkeydata[id,"RawTranslate",{"text"->URLEncode[textP], "from"->fromP, "to"->toP}];
			(*rawdata = ServiceExecute["MicrosoftTranslator", "RawTranslate", {"text"->URLEncode[textP], "from"->fromP, "to"->toP}];*)
			(*rawdata = StringReplace[ToString[rawdata], "[" ~~ data___ ~~ "]" :> data]; This was before using ContentData*)
			rawdata = ToString[{rawdata[[1]], StringReplace[ToString[FromCharacterCode[rawdata[[2]], "UTF8"]], "[" ~~ data___ ~~ "]" :> data]}];
			dataxml = ImportString[StringReplace[rawdata, Shortest[___] ~~ "," ~~ xml___ ~~ "}" :> xml], "XML"];
			
			status = StringReplace[rawdata, "{" ~~ Shortest[s___] ~~ "," ~~ ___ :> s];
			If[status != "200",
			(
				msg = Cases[dataxml, XMLElement["p", _, content_List] :> content, Infinity];
				Message[ServiceExecute::serrormsg,msg];
				Throw[$Failed]
			)];
			
			results=entityFromLanguageCode[fromP]->parseTranslateOutput[dataxml];
		),
		(
			groups = GroupBy[tuples, First] // Normal;
			(
				results = (
					f = #[[1]];
					current = #[[2]];
					rawdata = KeyClient`rawkeydata[id,"RawTranslateArray",{"Data"->translateArrayRequestXML[current[[All,2]],toP,f]}];
					(*rawdata = ServiceExecute["MicrosoftTranslator", "RawTranslateArray", {"Data"->translateArrayRequestXML[current[[All,2]],toP,f]}];*)
					(*rawdata = StringReplace[ToString[rawdata], "[" ~~ data___ ~~ "]" :> data]; This was before using ContentData*)
					rawdata = ToString[{rawdata[[1]],StringReplace[ToString[FromCharacterCode[rawdata[[2]], "UTF8"]], "[" ~~ data___ ~~ "]" :> data]}];
					dataxml = ImportString[StringReplace[rawdata, Shortest[___] ~~ "," ~~ xml___ ~~ "}" :> xml], "XML"];
					status = StringReplace[rawdata, "{" ~~ Shortest[s___] ~~ "," ~~ ___ :> s];
					If[status != "200",
					(
						msg = Cases[dataxml, XMLElement["p", _, content_List] :> content, Infinity];
						Message[ServiceExecute::serrormsg,msg];
						Throw[$Failed]
					)];
			
					rawdata = parseTranslateArrayOutput[dataxml];
					current[[#]] -> rawdata[[#]] & /@ Range[Length[current]]
					(*rawdata = (entityFromLanguageCode[f]-># &/@ parseTranslateArrayOutput[rawdata])*)
				)&/@ groups;
				results = Flatten[results];			
			)
		)];
		
	If[MatchQ[textP, _String],
		If[!languageRules,results[[2]],results],
		(
			(* sort *)
			results = Sort[results,(#1[[1,3]]<#2[[1,3]]&)];
			If[!languageRules,results=results[[All,2]],results=(entityFromLanguageCode[#[[1,1]]]->#[[2]]&/@results)];
			If[Length[results]==1,results[[1]],results]
		)]
	
]

microsofttranslatorcookeddata["RoundTripTranslate", id_, args_] := Block[{params,textP,toP,fromP,rawdata,dataxml,translations,fwdTranslation,rwdTranslation,status,invalidParameters,msg},
	invalidParameters = Select[Keys[args],!MemberQ[{"Text","From","To"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"GetTranslations"]&/@invalidParameters;
			Throw[$Failed]
		)];
	
	If[KeyExistsQ[args,"Text"],
		textP = URLEncode["Text" /. args],
		(
			Message[ServiceExecute::nparam,"Text"];			
			Throw[$Failed]
		)
	];
	
	If[KeyExistsQ[args,"From"],
		(
			fromP = "From" /. args;
			fromP = mapParameterToLanguageCode[fromP];
			If[fromP == "Failed",
			(	
				Message[ServiceExecute::nval,"From","MicrosoftTranslator"];
				Throw[$Failed]
			)]
		),
		(
			Message[ServiceExecute::nparam,"From"];			
			Throw[$Failed]
		)
	];
	
	If[KeyExistsQ[args,"To"],
		(
			toP = "To" /. args;
			toP = mapParameterToLanguageCode[toP];
			If[toP == "Failed",
			(	
				Message[ServiceExecute::nval,"To","MicrosoftTranslator"];
				Throw[$Failed]
			)]
		),
		(
			Message[ServiceExecute::nparam,"To"];			
			Throw[$Failed]
		)
	];
	
	params = {"text"->textP, "from"->fromP, "to"->toP, "maxTranslations"->"1"};
	
	rawdata = KeyClient`rawkeydata[id,"RawGetTranslations",params];
	(*rawdata = ServiceExecute["MicrosoftTranslator", "RawGetTranslations", params];*)
	rawdata = StringReplace[ToString[rawdata], "[" ~~ data___ ~~ "]" :> data];
	dataxml = ImportString[StringReplace[rawdata, Shortest[___] ~~ "," ~~ xml___ ~~ "}" :> xml], "XML"];
	
	status = StringReplace[rawdata, "{" ~~ Shortest[s___] ~~ "," ~~ ___ :> s];
	If[status != "200",
	(
		msg = Cases[dataxml, XMLElement["p", _, content_List] :> content, Infinity];
		Message[ServiceExecute::serrormsg,msg];
		Throw[$Failed]
	)];
			
	fwdTranslation = Cases[dataxml,XMLElement["TranslatedText",_,text_]:>text,Infinity][[1,1]];
	
	params = {"text"->URLEncode[fwdTranslation], "from"->toP, "to"->fromP, "maxTranslations"->"1"};
	
	rawdata = KeyClient`rawkeydata[id,"RawGetTranslations",params];
	(*rawdata = ServiceExecute["MicrosoftTranslator", "RawGetTranslations", params];*)
	rawdata = StringReplace[ToString[rawdata], "[" ~~ data___ ~~ "]" :> data];
	dataxml = ImportString[StringReplace[rawdata, Shortest[___] ~~ "," ~~ xml___ ~~ "}" :> xml], "XML"];
	status = StringReplace[rawdata, "{" ~~ Shortest[s___] ~~ "," ~~ ___ :> s];
	If[status != "200",
	(
		msg = Cases[dataxml, XMLElement["p", _, content_List] :> content, Infinity];
		Message[ServiceExecute::serrormsg,msg];
		Throw[$Failed]
	)];
	
	rwdTranslation = Cases[dataxml,XMLElement["TranslatedText",_,text_]:>text,Infinity][[1,1]];
	
	Dataset@Association[Rule["Original",URLDecode[textP]],Rule["Translated to " <> toP,fwdTranslation],Rule["Translated to " <> fromP,rwdTranslation]]	
]

microsofttranslatorcookeddata[prop_,id_,rules___Rule]:=microsofttranslatorcookeddata[prop,id,{rules}]

microsofttranslatorcookeddata[___]:=$Failed

microsofttranslatorsendmessage[___]:=$Failed


(* Utilities *)
getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.microsofttranslatordata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

getToken[id_,secret_] := "access_token" /. 
					ImportString[URLFetch["https://datamarket.accesscontrol.windows.net/v2/OAuth2-13/",
											"Method" -> "POST",
											"Parameters" -> {"client_id" -> id, "client_secret" -> secret, 
															"scope" -> "http://api.microsofttranslator.com", "grant_type" -> "client_credentials"}], "JSON"]

parseTranslateOutput[output_] := Module[{xml},(
	(*xml = ImportString[StringReplace[ToString@output, "[" ~~ x___ ~~ "]" :> x], "XML"];*)
	Cases[output, XMLElement["string", _, text_] :> text, Infinity][[1, 1]]
)]

parseTranslateArrayOutput[output_] := Module[{xml},(
	(*xml = ImportString[StringReplace[ToString@output, "[" ~~ x___ ~~ "]" :> x],"XML"];*)
	Cases[output, XMLElement["TranslatedText", _, text_] :> text, Infinity] // Flatten
)]

formatTranslations[t_] := 
 Cases[t, {XMLElement["Count", {}, {count_}], 
    XMLElement["MatchDegree", {}, {mDeg_}], 
    XMLElement["MatchedOriginalText", {}, _], 
    XMLElement["Rating", {}, {rating_}], 
    XMLElement["TranslatedText", {}, {text_}]} :> 
   Sequence[Rule["Count", count], Rule["MatchDegree", mDeg], 
    Rule["Rating", rating], Rule["TranslatedText", text]]]

translateArrayRequestXML[textList_,to_,from_] := Module[{result=""},
	(
		result = result <> "<TranslateArrayRequest><AppId />";
		result = result <> "<From>" <> from <> "</From>";
		result = result <> "<Options> <Category xmlns='http://schemas.datacontract.org/2004/07/Microsoft.MT.Web.Service.V2' />";
		result = result <> "<ContentType xmlns='http://schemas.datacontract.org/2004/07/Microsoft.MT.Web.Service.V2'>text/plain</ContentType>";
		result = result <> "<ReservedFlags xmlns='http://schemas.datacontract.org/2004/07/Microsoft.MT.Web.Service.V2' />";
		result = result <> "<State xmlns='http://schemas.datacontract.org/2004/07/Microsoft.MT.Web.Service.V2' />";
		result = result <> "<Uri xmlns='http://schemas.datacontract.org/2004/07/Microsoft.MT.Web.Service.V2' />";
		result = result <> "<User xmlns='http://schemas.datacontract.org/2004/07/Microsoft.MT.Web.Service.V2' /> </Options>";
		result = result <> "<Texts>";
		
		(result = result <> "<string xmlns='http://schemas.microsoft.com/2003/10/Serialization/Arrays'>" <> # <> "</string>")&/@textList;
		
		result = result <> "</Texts> <To>" <> to <> "</To> </TranslateArrayRequest>";
		
		result
	)]

entityFromLanguageCode[code_] := If[KeyExistsQ[languageCodeToEntityAlignment,code],
									code /. languageCodeToEntityAlignment, 
									Missing["NotAvailble"]]

mapParameterToLanguageCode[param_] := Module[{code,entity},
		If[Head[param]===Entity, (* language represented as a WL Entity *)
		(
			code = entityToLanguageCode[param];
			If[Head[code]===Missing, code = "Failed"]
		),
		(
			If[Head[param]===String,
			(
				If[allowedLanguageCodes==={},allowedLanguageCodes=ServiceExecute["MicrosoftTranslator","RawLanguageCodeList",{}]];
				If[!MemberQ[allowedLanguageCodes,param],
				(
					entity = Interpreter["Language"][param];
					code = entityToLanguageCode[entity];
					If[Head[code]===Missing, code = "Failed"]
				),
					code = param
				]
			),
				code = "Failed" (* Invalid language code *)
			]
			
		)];
		code
	]

allowedLanguageCodes = {};

entityToLanguageCode[entity_] := If[KeyExistsQ[entityToLanguageCodeAlignment,entity],
									entity /. entityToLanguageCodeAlignment, 
									Missing["NotAvailble"]]

entityToLanguageCodeAlignment = {Entity["Language", "Arabic"] -> "ar", 
 Entity["Language", "Bulgarian"] -> "bg", 
 Entity["Language", "CatalanValencianBalear"] -> "ca", 
 Entity["Language", "Czech"] -> "cs", 
 Entity["Language", "Danish"] -> "da", 
 Entity["Language", "Dutch"] -> "nl", 
 Entity["Language", "English"] -> "en", 
 Entity["Language", "Estonian"] -> "et", 
 Entity["Language", "Finnish"] -> "fi", 
 Entity["Language", "French"] -> "fr", 
 Entity["Language", "German"] -> "de", 
 Entity["Language", "Greek"] -> "el", 
 Entity["Language", "HaitianCreoleFrench"] -> "ht", 
 Entity["Language", "Hindi"] -> "hi", 
 Entity["Language", "Hungarian"] -> "hu", 
 Entity["Language", "Indonesian"] -> "id", 
 Entity["Language", "Italian"] -> "it", 
 Entity["Language", "Japanese"] -> "ja", 
 Entity["Language", "Korean"] -> "ko", 
 Entity["Language", "Latvian"] -> "lv", 
 Entity["Language", "Lithuanian"] -> "lt", 
 Entity["Language", "Malay"] -> "ms", 
 Entity["Language", "Maltese"] -> "mt", 
 Entity["Language", "Norwegian"] -> "no", 
 Entity["Language", "FarsiEastern"] -> "fa", 
 Entity["Language", "Polish"] -> "pl", 
 Entity["Language", "Portuguese"] -> "pt", 
 Entity["Language", "Romanian"] -> "ro", 
 Entity["Language", "Russian"] -> "ru", 
 Entity["Language", "Slovak"] -> "sk", 
 Entity["Language", "Slovenian"] -> "sl", 
 Entity["Language", "Spanish"] -> "es", 
 Entity["Language", "Swedish"] -> "sv", 
 Entity["Language", "Thai"] -> "th", 
 Entity["Language", "Turkish"] -> "tr", 
 Entity["Language", "Ukrainian"] -> "uk", 
 Entity["Language", "Urdu"] -> "ur", 
 Entity["Language", "Vietnamese"] -> "vi", 
 Entity["Language", "Welsh"] -> "cy", 
 Entity["Language", "FarsiWestern"] -> "fa", 
 Entity["Language", "Chinese"] -> "zh-CHT", 
 Entity["Language", "ChineseGan"] -> "zh-CHT", 
 Entity["Language", "ChineseHakka"] -> "zh-CHT", 
 Entity["Language", "ChineseHuizhou"] -> "zh-CHT", 
 Entity["Language", "ChineseJinyu"] -> "zh-CHT", 
 Entity["Language", "ChineseMandarin"] -> "zh-CHT", 
 Entity["Language", "ChineseMinBei"] -> "zh-CHT", 
 Entity["Language", "ChineseMinDong"] -> "zh-CHT", 
 Entity["Language", "ChineseMinNan"] -> "zh-CHT", 
 Entity["Language", "ChineseMinZhong"] -> "zh-CHT", 
 Entity["Language", "ChinesePidginEnglish"] -> "zh-CHT", 
 Entity["Language", "ChinesePuXian"] -> "zh-CHT", 
 Entity["Language", "ChineseSignLanguage"] -> "zh-CHT", 
 Entity["Language", "ChineseTibetanMongolian"] -> "zh-CHT", 
 Entity["Language", "ChineseWu"] -> "zh-CHT", 
 Entity["Language", "ChineseXiang"] -> "zh-CHT", 
 Entity["Language", "ChineseYue"] -> "zh-CHT", 
 Entity["Language", "ChineseScript"] -> "zh-CHT", 
 Entity["Language", "Hebrew"] -> "he", 
 Entity["Language", "HmongDaw"] -> "mww"};

languageCodeToEntityAlignment = {"ar" -> Entity["Language", "Arabic"], 
 "bg" -> Entity["Language", "Bulgarian"], 
 "ca" -> Entity["Language", "CatalanValencianBalear"], 
 "zh-CHS" -> Entity["Language", "Chinese"], 
 "zh-CHT" -> Entity["Language", "Chinese"], 
 "cs" -> Entity["Language", "Czech"], 
 "da" -> Entity["Language", "Danish"], 
 "nl" -> Entity["Language", "Dutch"], 
 "en" -> Entity["Language", "English"], 
 "et" -> Entity["Language", "Estonian"], 
 "fi" -> Entity["Language", "Finnish"], 
 "fr" -> Entity["Language", "French"], 
 "de" -> Entity["Language", "German"], 
 "el" -> Entity["Language", "Greek"], 
 "ht" -> Entity["Language", "HaitianCreoleFrench"], 
 "he" -> Entity["Language", "Hebrew"], 
 "hi" -> Entity["Language", "Hindi"], 
 "mww" -> Entity["Language", "HmongDaw"], 
 "hu" -> Entity["Language", "Hungarian"], 
 "id" -> Entity["Language", "Indonesian"], 
 "it" -> Entity["Language", "Italian"], 
 "ja" -> Entity["Language", "Japanese"], 
 "ko" -> Entity["Language", "Korean"], 
 "lv" -> Entity["Language", "Latvian"], 
 "lt" -> Entity["Language", "Lithuanian"], 
 "ms" -> Entity["Language", "Malay"], 
 "mt" -> Entity["Language", "Maltese"], 
 "no" -> Entity["Language", "Norwegian"], 
 "fa" -> Entity["Language", "FarsiEastern"], 
 "pl" -> Entity["Language", "Polish"], 
 "pt" -> Entity["Language", "Portuguese"], 
 "ro" -> Entity["Language", "Romanian"], 
 "ru" -> Entity["Language", "Russian"], 
 "sk" -> Entity["Language", "Slovak"], 
 "sl" -> Entity["Language", "Slovenian"], 
 "es" -> Entity["Language", "Spanish"], 
 "sv" -> Entity["Language", "Swedish"], 
 "th" -> Entity["Language", "Thai"], 
 "tr" -> Entity["Language", "Turkish"], 
 "uk" -> Entity["Language", "Ukrainian"], 
 "ur" -> Entity["Language", "Urdu"], 
 "vi" -> Entity["Language", "Vietnamese"], 
 "cy" -> Entity["Language", "Welsh"]};

End[]

End[]

SetAttributes[{},{ReadProtected, Protected}];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{MicrosoftTranslatorAPI`Private`microsofttranslatordata,MicrosoftTranslatorAPI`Private`microsofttranslatorcookeddata,MicrosoftTranslatorAPI`Private`microsofttranslatorsendmessage}
