Begin["GoogleTranslateAPI`"] (* Begin Private Context *)

Begin["`Private`"](* Begin Private Context *)

(******************************* GoogleTranslate *************************************)

(* Authentication information *)
googletranslatedata[]={
		"ServiceName" 		-> "Google Translate",

        "URLFetchFun"		:> (Module[{params=Lookup[{##2},"Parameters",{}],q},
        	q = "q"/.params;
        	If[MatchQ[Head[q], List], params = Join[DeleteCases[params, Rule["q", _]], Function[v, Rule["q", v]]/@q]];
        	URLFetch[#1,"ContentData",
        		Sequence@@FilterRules[{##2},Except["Parameters"|"Headers"]],
        		"Parameters" -> params,
        		"Headers" -> {}]]&),

        "ClientInfo"		:> OAuthDialogDump`Private`MultipleKeyDialog["GoogleTranslate",{"API Key"->"key"},"https://console.developers.google.com/flows/enableapi?apiid=translate","https://cloud.google.com/translate/v2/terms"],
	 	"Gets"				-> {"Translate"},
	 	"RawGets"			-> {"RawTranslate", "RawLanguageCodeList"},
	 	"Posts"				-> {},
	 	"RawPosts"			-> {},
 		"Information"		-> "Import Google Translate API data to the Wolfram Language"
 		}

googletranslateimport[rawdata_]:=FromCharacterCode[rawdata, "UTF8"]

importLanguageCodes[rawdata_]:= Module[{data=ImportString[FromCharacterCode[rawdata,"UTF8"],"JSON"],error},
	error="error" /. data;
	If[!MatchQ[error,"error"],
		(Message[ServiceExecute::serrormsg,"message"/.error];
		Throw[$Failed]),
		Flatten[("languages"/.("data"/.data))[[All,All,2]]]
	]
]

(* Raw *)
googletranslatedata["RawTranslate"] := {
        "URL"				-> "https://www.googleapis.com/language/translate/v2",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"format","prettyprint","q","source","target"},
        "RequiredParameters"-> {"q","target"},
        "ResultsFunction"	-> googletranslateimport
        }

googletranslatedata["RawLanguageCodeList"] := {
        "URL"				-> "https://www.googleapis.com/language/translate/v2/languages",
		"HTTPSMethod"		-> "GET",
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> importLanguageCodes
    }

(* Cooked *)
googletranslatecookeddata[prop_,id_, rest___] := googletranslatecookeddata[prop,id,{rest}]

googletranslatecookeddata["Translate", id_, args_] := Block[{data, text, from, to, result, languagerules, msl=False, textgroupby, preresult, statictext,invalidParameters},
	invalidParameters = Select[Keys[args],!MemberQ[{"LanguageRules","Text","From","To"},#]&];
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"GoogleTranslate"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[args,"LanguageRules"],
		(
			If[!MemberQ[{True,False},"LanguageRules" /. args],
			(
				Message[ServiceExecute::nval,"LanguageRules","GoogleTranslate"];
				Throw[$Failed]
			)];
			languagerules = "LanguageRules" /. args;
		),
		languagerules = False;
	];

	If[KeyExistsQ[args,"Text"],
		(
			If[!(StringQ["Text"/.args]||MatchQ["Text"/.args,{__String}]),
			(
				Message[ServiceExecute::nval,"Text","GoogleTranslate"];
				Throw[$Failed]
			)];
			text = "Text" /. args;
		),
		(
			Message[ServiceExecute::nparam,"Text","GoogleTranslate"];
			Throw[$Failed]
		)
	];

	If[KeyExistsQ[args,"From"],
		((*Source language has been specified*)
			from = "From" /. args;
			If[!MatchQ[Head[from], String|Entity],
			(
				Message[ServiceExecute::nval,"From","GoogleTranslate"];
				Throw[$Failed]
			)];
			from = mapParameterToLanguageCode[from,id];
			If[from == "Failed",
			(
				Message[ServiceExecute::nval,"From","GoogleTranslate"];
				Throw[$Failed]
			)];
			If[MatchQ[Head[text], List],from = ConstantArray[from, Length[text]]];

		),(*Source language has NOT been specified*)
			If[MatchQ[Head[text], String],(*Autodetects the language using WL*)
				from = mapParameterToLanguageCode[Classify["Language",text],id],
				If[MatchQ[Head[text], List],from = (mapParameterToLanguageCode[Classify["Language",#],id]&)/@text]
			];
	];
	(*Checks if there are multiple languages in a list of texts*)
	If[MatchQ[Head[from], List], If[Length[DeleteDuplicates[from]]>1, msl = True]];

	If[KeyExistsQ[args,"To"],
		(
			to = "To" /. args;
			If[!MatchQ[Head[to], String|Entity],
			(
				Message[ServiceExecute::nval,"To","GoogleTranslate"];
				Throw[$Failed]
			)];
			to = mapParameterToLanguageCode[to,id];

			If[to == "Failed",
			(
				Message[ServiceExecute::nval,"To","GoogleTranslate"];
				Throw[$Failed]
			)]
		),
		(
			Message[ServiceExecute::nparam,"To","GoogleTranslate"];
			Throw[$Failed]
		)
	];
	If[MatchQ[Head[text], String],(*If just one string is provided*)

		If[MatchQ[from,to],(*If source language is the same as target, the same string is returned.*)
		result = {text},
		result=ImportString[ToString[ServiceExecute["GoogleTranslate", "RawTranslate", {"q" -> text, "source" -> from, "target" -> to, "format" -> "text"}], CharacterEncoding->"UTF-8"],"JSON"];
		If[KeyExistsQ[result,"error"],
   		(
   		   	Message[ServiceExecute::serrormsg,("message" /. ("error" /. result))];
   	    	Throw[$Failed]
 		)];
		result="translatedText" /. ("translations" /. ("data" /. result));
		];

		(*If LanguageRules is true, returns the output as source/autodetected language -> translated text*)
		If[result === {}, "", If[languagerules, (from /.languageCodeToEntityAlignment) -> result[[1]], result[[1]]]],

		If[MatchQ[Head[text], List], (*If a list with texts is provided*)
			If[msl,(*If multiple source languages are detected*)

				(*the texts are grouped by language to reduce the number of calls to the api. An index is assigned to texts to keep the original order at the end*)
				textgroupby = GroupBy[MapThread[List, {from, text, Range[Length@text]}], First];

				(*Cheks if any source language matches the target language and removes them from the list to be translated*)
				If[MemberQ[from, to], statictext = Cases[Flatten[List@@textgroupby, 1], {to, _, _Integer}]; textgroupby = Association[DeleteCases[Normal[textgroupby], to -> _]];,statictext={}];

				(*Calls the API*)
				preresult = Function[v, Module[{gtexts = textgroupby[v][[All, 2]], gindexes = textgroupby[v][[All, 3]],temp},
					temp=ImportString[ToString[ServiceExecute["GoogleTranslate","RawTranslate",{"q"->gtexts,"source"->v,"target"->to, "format" -> "text"}],CharacterEncoding->"UTF-8"],"JSON"];
					If[KeyExistsQ[temp,"error"],
   					(
   		  			 	Message[ServiceExecute::serrormsg,("message" /. ("error" /. temp))];
   	    				Throw[$Failed]
 					)];
					MapThread[List,{gindexes, "translatedText"/.("translations"/.("data"/.temp))}]]]/@DeleteDuplicates[DeleteCases[from, to]];

				(*The original order is restored*)
				If[statictext==={},	(*If there's no text with source language = target language*)
					result = SortBy[Flatten[preresult, 1], First][[All, 2]];,
					result = SortBy[Union[Flatten[preresult, 1], (Reverse /@ statictext)[[All, 1 ;; 2]]], First][[All, 2]];
				];
				,
				(*Just one source language*)
				If[MemberQ[from, to],
					result = text;,
					result=ImportString[ToString[ServiceExecute["GoogleTranslate", "RawTranslate", {"q" -> text, "source" -> If[MatchQ[Head[from], List], from[[1]], from], "target" -> to, "format" -> "text"}], CharacterEncoding->"UTF-8"],"JSON"];
					If[KeyExistsQ[result,"error"],
   					(
   		  			 	Message[ServiceExecute::serrormsg,("message" /. ("error" /. result))];
   	    				Throw[$Failed]
 					)];
					result="translatedText" /. ("translations" /. ("data" /. result));
				];

			];

			(*If LanguageRules is true, returns the output as source/autodetected language -> translated text*)
			If[languagerules, MapThread[Rule, {(from /.languageCodeToEntityAlignment), Flatten[result]}], Flatten[result]]
		]
	]

]

(*Utilities*)

mapParameterToLanguageCode[param_,id_] := Module[{code,entity},
		If[Head[param]===Entity, (* language represented as a WL Entity *)
		(
			code = entityToLanguageCode[param];
			If[Head[code]===Missing, code = "Failed"]
		),
		(
			If[Head[param]===String,
			(
				If[allowedLanguageCodes==={},allowedLanguageCodes=importLanguageCodes[KeyClient`rawkeydata[id,"RawLanguageCodeList",{}]]];
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

entityFromLanguageCode[code_] := If[KeyExistsQ[languageCodeToEntityAlignment,code],
									code /. languageCodeToEntityAlignment,
									Missing["NotAvailble"]]

entityToLanguageCodeAlignment = {Entity["Language", "Afrikaans"] -> "af",
 EntityClass["Language", "Albanian"] -> "sq",
 Entity["Language", "Arabic"] -> "ar",
 EntityClass["Language", "Azerbaijani"] -> "az",
 Entity["Language", "Basque"] -> "eu",
 Entity["Language", "Bengali"] -> "bn",
 Entity["Language", "Belarusan"] -> "be",
 Entity["Language", "Bulgarian"] -> "bg",
 Entity["Language", "CatalanValencianBalear"] -> "ca",
 Entity["Language", "Croatian"] -> "hr",
 Entity["Language", "Czech"] -> "cs",
 Entity["Language", "Danish"] -> "da",
 Entity["Language", "Dutch"] -> "nl",
 Entity["Language", "English"] -> "en",
 Entity["Language", "Esperanto"] -> "eo",
 Entity["Language", "Estonian"] -> "et",
 Entity["Language", "Filipino"] -> "tl",
 Entity["Language", "Finnish"] -> "fi",
 Entity["Language", "French"] -> "fr",
 Entity["Language", "Galician"] -> "gl",
 Entity["Language", "Georgian"] -> "ka",
 Entity["Language", "German"] -> "de",
 Entity["Language", "Greek"] -> "el",
 Entity["Language", "Gujarati"] -> "gu",
 Entity["Language", "HaitianCreoleFrench"] -> "ht",
 Entity["Language", "Hebrew"] -> "iw",
 Entity["Language", "Hindi"] -> "hi",
 Entity["Language", "Hungarian"] -> "hu",
 Entity["Language", "Icelandic"] -> "is",
 Entity["Language", "Indonesian"] -> "id",
 Entity["Language", "IrishGaelic"] -> "ga",
 Entity["Language", "Italian"] -> "it",
 Entity["Language", "Japanese"] -> "ja",
 Entity["Language", "Kannada"] -> "kn",
 Entity["Language", "Korean"] -> "ko",
 Entity["Language", "Latin"] -> "la",
 Entity["Language", "Latvian"] -> "lv",
 Entity["Language", "Lithuanian"] -> "lt",
 Entity["Language", "Macedonian"] -> "mk",
 Entity["Language", "Malay"] -> "ms",
 Entity["Language", "Maltese"] -> "mt",
 Entity["Language", "Norwegian"] -> "no",
 Entity["Language", "FarsiEastern"] -> "fa",
 Entity["Language", "Polish"] -> "pl",
 Entity["Language", "Portuguese"] -> "pt",
 Entity["Language", "Romanian"] -> "ro",
 Entity["Language", "Russian"] -> "ru",
 Entity["Language", "Serbian"] -> "sr",
 Entity["Language", "Slovak"] -> "sk",
 Entity["Language", "Slovenian"] -> "sl",
 Entity["Language", "Spanish"] -> "es",
 Entity["Language", "Swahili"] -> "sw",
 Entity["Language", "Swedish"] -> "sv",
 Entity["Language", "Tamil"] -> "ta",
 Entity["Language", "Telugu"] -> "te",
 Entity["Language", "Thai"] -> "th",
 Entity["Language", "Turkish"] -> "tr",
 Entity["Language", "Ukrainian"] -> "uk",
 Entity["Language", "Urdu"] -> "ur",
 Entity["Language", "Vietnamese"] -> "vi",
 Entity["Language", "Welsh"] -> "cy",
 Entity["Language", "YiddishEastern"] -> "yi",
 Entity["Language", "Chinese"] -> "zh-CN",
 Entity["Language", "ChineseGan"] -> "zh-CN",
 Entity["Language", "ChineseHakka"] -> "zh-CN",
 Entity["Language", "ChineseHuizhou"] -> "zh-CN",
 Entity["Language", "ChineseJinyu"] -> "zh-CN",
 Entity["Language", "ChineseMandarin"] -> "zh-CN",
 Entity["Language", "ChineseMinBei"] -> "zh-CN",
 Entity["Language", "ChineseMinDong"] -> "zh-CN",
 Entity["Language", "ChineseMinNan"] -> "zh-CN",
 Entity["Language", "ChineseMinZhong"] -> "zh-CN",
 Entity["Language", "ChinesePidginEnglish"] -> "zh-CN",
 Entity["Language", "ChinesePuXian"] -> "zh-CN",
 Entity["Language", "ChineseSignLanguage"] -> "zh-CN",
 Entity["Language", "ChineseTibetanMongolian"] -> "zh-CN",
 Entity["Language", "ChineseWu"] -> "zh-CN",
 Entity["Language", "ChineseXiang"] -> "zh-CN",
 Entity["Language", "ChineseYue"] -> "zh-CN",
 Entity["Language", "ChineseScript"] -> "zh-CN",
 Entity["Language", "FarsiWestern"] -> "fa"};

 languageCodeToEntityAlignment = {"af" -> Entity["Language", "Afrikaans"],
 "sq" -> EntityClass["Language", "Albanian"],
 "ar" -> Entity["Language", "Arabic"],
 "az" -> EntityClass["Language", "Azerbaijani"],
 "eu" -> Entity["Language", "Basque"],
 "bn" -> Entity["Language", "Bengali"],
 "be" -> Entity["Language", "Belarusan"],
 "bg" -> Entity["Language", "Bulgarian"],
 "ca" -> Entity["Language", "CatalanValencianBalear"],
 "hr" -> Entity["Language", "Croatian"],
 "cs" -> Entity["Language", "Czech"],
 "da" -> Entity["Language", "Danish"],
 "nl" -> Entity["Language", "Dutch"],
 "en" -> Entity["Language", "English"],
 "eo" -> Entity["Language", "Esperanto"],
 "et" -> Entity["Language", "Estonian"],
 "tl" -> Entity["Language", "Filipino"],
 "fi" -> Entity["Language", "Finnish"],
 "fr" -> Entity["Language", "French"],
 "gl" -> Entity["Language", "Galician"],
 "ka" -> Entity["Language", "Georgian"],
 "de" -> Entity["Language", "German"],
 "el" -> Entity["Language", "Greek"],
 "gu" -> Entity["Language", "Gujarati"],
 "ht" -> Entity["Language", "HaitianCreoleFrench"],
 "iw" -> Entity["Language", "Hebrew"],
 "hi" -> Entity["Language", "Hindi"],
 "hu" -> Entity["Language", "Hungarian"],
 "is" -> Entity["Language", "Icelandic"],
 "id" -> Entity["Language", "Indonesian"],
 "ga" -> Entity["Language", "IrishGaelic"],
 "it" -> Entity["Language", "Italian"],
 "ja" -> Entity["Language", "Japanese"],
 "kn" -> Entity["Language", "Kannada"],
 "ko" -> Entity["Language", "Korean"],
 "la" -> Entity["Language", "Latin"],
 "lv" -> Entity["Language", "Latvian"],
 "lt" -> Entity["Language", "Lithuanian"],
 "mk" -> Entity["Language", "Macedonian"],
 "ms" -> Entity["Language", "Malay"],
 "mt" -> Entity["Language", "Maltese"],
 "no" -> Entity["Language", "Norwegian"],
 "fa" -> Entity["Language", "FarsiEastern"],
 "pl" -> Entity["Language", "Polish"],
 "pt" -> Entity["Language", "Portuguese"],
 "ro" -> Entity["Language", "Romanian"],
 "ru" -> Entity["Language", "Russian"],
 "sr" -> Entity["Language", "Serbian"],
 "sk" -> Entity["Language", "Slovak"],
 "sl" -> Entity["Language", "Slovenian"],
 "es" -> Entity["Language", "Spanish"],
 "sw" -> Entity["Language", "Swahili"],
 "sv" -> Entity["Language", "Swedish"],
 "ta" -> Entity["Language", "Tamil"],
 "te" -> Entity["Language", "Telugu"],
 "th" -> Entity["Language", "Thai"],
 "tr" -> Entity["Language", "Turkish"],
 "uk" -> Entity["Language", "Ukrainian"],
 "ur" -> Entity["Language", "Urdu"],
 "vi" -> Entity["Language", "Vietnamese"],
 "cy" -> Entity["Language", "Welsh"],
 "yi" -> Entity["Language", "YiddishEastern"],
 "zh-CN" -> Entity["Language", "Chinese"],
 "zh-CN" -> Entity["Language", "ChineseGan"],
 "zh-CN" -> Entity["Language", "ChineseHakka"],
 "zh-CN" -> Entity["Language", "ChineseHuizhou"],
 "zh-CN" -> Entity["Language", "ChineseJinyu"],
 "zh-CN" -> Entity["Language", "ChineseMandarin"],
 "zh-CN" -> Entity["Language", "ChineseMinBei"],
 "zh-CN" -> Entity["Language", "ChineseMinDong"],
 "zh-CN" -> Entity["Language", "ChineseMinNan"],
 "zh-CN" -> Entity["Language", "ChineseMinZhong"],
 "zh-CN" -> Entity["Language", "ChinesePidginEnglish"],
 "zh-CN" -> Entity["Language", "ChinesePuXian"],
 "zh-CN" -> Entity["Language", "ChineseSignLanguage"],
 "zh-CN" -> Entity["Language", "ChineseTibetanMongolian"],
 "zh-CN" -> Entity["Language", "ChineseWu"],
 "zh-CN" -> Entity["Language", "ChineseXiang"],
 "zh-CN" -> Entity["Language", "ChineseYue"],
 "zh-CN" -> Entity["Language", "ChineseScript"],
 "fa" -> Entity["Language", "FarsiWestern"]};

googletranslatecookeddata[req_, id_]:=googletranslatecookeddata[req, id,{}]

googletranslatecookeddata[___]:=$Failed

googletranslaterawdata[___]:=$Failed

googletranslatesendmessage[args_]:=$Failed

End[]

End[]

SetAttributes[{},{ReadProtected, Protected}];

(* Return two functions to define oauthservicedata, oauthcookeddata  *)

{GoogleTranslateAPI`Private`googletranslatedata,GoogleTranslateAPI`Private`googletranslatecookeddata,GoogleTranslateAPI`Private`googletranslatesendmessage,GoogleTranslateAPI`Private`googletranslaterawdata}
