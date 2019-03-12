Begin["SurveyMonkeyOAuth`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* SurveyMonkey *************************************)

(* Authentication information *)

surveymonkeydata[]=
	If[TrueQ[OAuthClient`Private`$AllowNonBlockingDialogsQ],{
		"OAuthVersion"			-> "2.0",
		"ServiceName" 			-> "SurveyMonkey", 
	 	"AuthorizeEndpoint" 	-> "https://www.surveymonkey.com/user/oauth/authorize", 
     	"AccessEndpoint"    	-> "https://www.surveymonkey.com/oauth/token",
     	"RedirectURI"       	-> "WolframConnectorChannelListen",
     	"Blocking"          	-> False,
        "RedirectURLFunction"	-> (#1&),
        "AuthorizationFunction"	-> "SurveyMonkey",
		"AccessTokenRequestor"	-> "SurveyMonkey",
		"AccessTokenExtractor"	-> "JSON/2.0",
		"VerifierLabel"			-> "code",
		"VerifyPeer"			-> True,
	 	"AuthenticationDialog" 	:> "WolframConnectorChannel",
	 	"ClientInfo"			-> {"Wolfram","Token"}, 
	 	"RequestFormat" 		-> (Block[{APIkey="2kmvw64q5q3vvxsdfrz9zpzf",params=Cases[{##},("Parameters"->x_):>x,Infinity],url=DeleteCases[{##},"Parameters"->_,Infinity]},
	 						URLFetch@@{(Sequence@@(url[[1]]<>"?api_key="<>APIkey)),"ContentData",Sequence@@Rest[url],
	 						"Headers"->Flatten[{"Authorization"-> ("Bearer " <> ("access_token"/.params)),"Content-Type" -> "application/json"}]}
	 	]&), (*Does not support more Headers or Parameters*)
	 	"Gets"					:> {},
	 	"Posts"					-> {"SurveyList","SurveyDetails","CollectorList","ResponseCounts","RespondentList","Responses","UserData","TemplateList","CreateWeblinkCollector","CreateEmailCollector","CreateSurvey","SurveyResults"},
	 	"Scope"					-> {},
	 	"RawGets"				-> {},
	 	"RawPosts"				-> {"RawSurveyList","RawSurveyDetails","RawCollectorList","RawResponseCounts","RawRespondentList","RawResponses","RawUserDetails","RawTemplateList","RawCreateCollector","RawSendFlow","RawCreateFlow"},
 		"Information"			-> "A service for sending and receiving data from SurveyMonkey"
	},
	{
		"OAuthVersion"			-> "2.0",
		"ServiceName" 			-> "SurveyMonkey",
	 	"AuthorizeEndpoint"		-> "https://www.surveymonkey.com/user/oauth/authorize", 
     	"AccessEndpoint"   		-> "https://www.surveymonkey.com/oauth/token",
     	"RedirectURI"     	 	-> "https://www.wolfram.com/oauthlanding?service=SurveyMonkey",
		"VerifierLabel"			-> "code",
        "AuthorizationFunction"	-> "SurveyMonkey",
		"AccessTokenRequestor"	-> "SurveyMonkey",
	 	"ClientInfo"			-> {"Wolfram","Token"},
	 	"AuthenticationDialog" 	:> (OAuthClient`tokenOAuthDialog[StringReplace[#, "?client_id" :> "&client_id"], "SurveyMonkey"]&),
	 	"RequestFormat" 		-> (Block[{APIkey="96fnzpq5a832zwqfsvahkdjw",params=Cases[{##},("Parameters"->x_):>x,Infinity],url=DeleteCases[{##},"Parameters"->_,Infinity]},
	 						URLFetch@@{(Sequence@@(url[[1]]<>"?api_key="<>APIkey)),"ContentData",Sequence@@Rest[url],
	 						"Headers"->Flatten[{"Authorization"-> ("Bearer " <> ("access_token"/.params)),"Content-Type" -> "application/json"}]}
	 	]&), (*Does not support more Headers or Parameters*)
	 	"Gets"					:> {},
	 	"Posts"					-> {"SurveyList","SurveyDetails","CollectorList","ResponseCounts","RespondentList","Responses","UserData","TemplateList","CreateWeblinkCollector","CreateEmailCollector","CreateSurvey","SurveyResults"},
	 	"Scope"					-> {},
	 	"RawGets"				-> {},
	 	"RawPosts"				-> {"RawSurveyList","RawSurveyDetails","RawCollectorList","RawResponseCounts","RawRespondentList","RawResponses","RawUserDetails","RawTemplateList","RawCreateCollector","RawSendFlow","RawCreateFlow"},
 		"Information"			-> "A service for sending and receiving data from SurveyMonkey"
	}
]

(*Raw*)
surveymonkeyimport[rawdata_]:=ImportString[FromCharacterCode[rawdata, "UTF-8"], "RawJSON"]

surveymonkeydata["RawSurveyList"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/surveys/get_survey_list",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}

surveymonkeydata["RawSurveyDetails"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/surveys/get_survey_details",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}
        
surveymonkeydata["RawCollectorList"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/surveys/get_collector_list",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}

surveymonkeydata["RawResponseCounts"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/surveys/get_response_counts",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}

surveymonkeydata["RawRespondentList"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/surveys/get_respondent_list",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}

surveymonkeydata["RawResponses"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/surveys/get_responses",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}

surveymonkeydata["RawUserDetails"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/user/get_user_details",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}

surveymonkeydata["RawTemplateList"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/templates/get_template_list",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}
 
surveymonkeydata["RawCreateCollector"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/collectors/create_collector",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}

surveymonkeydata["RawSendFlow"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/batch/send_flow",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}

surveymonkeydata["RawCreateFlow"]:={
        "URL"				-> "https://api.surveymonkey.net/v2/batch/create_flow",
        "HTTPSMethod"		-> "POST",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> surveymonkeyimport
}               
(*Cooked*)

surveymonkeycookeddata[req_, id_]:=surveymonkeycookeddata[req, id,{}]

surveymonkeycookeddata[prop_,id_,rules___Rule]:=surveymonkeycookeddata[prop,id,{rules}]

camelCase[text_] := Module[{split, partial}, (
    split = StringSplit[text, {" ","_","-"}];
    partial = Prepend[Rest[Characters[#]], ToUpperCase[Characters[#][[1]]]] & /@ split;
    StringJoin[partial]
    )]

fieldsRules = {"survey_id"->"SurveyID","title" -> "Title", "analysis_url" -> "AnalysisURL", "preview_url" -> "PreviewURL", "date_created" -> "DateCreated", 
 "date_modified" -> "DateModified", "language_id" -> "Language", "question_count" -> "QuestionCount", "num_responses" -> "NumResponses","page_id"->"PageID",
 "question_id"->"QuestionID","answer_id"->"AnswerID","collector_id"->"CollectorID","url"->"URL", "open"->"Open", "type"->"Type", "name"->"Name",
  "custom_id" -> "CustomID", "ip_address" -> "IPAddress", "recipient_id" -> "RecipientID", "date_start" -> "DateStart", "collection_mode" -> "CollectionMode",
  "email" -> "Email", "first_name" -> "FirstName", "last_name" -> "LastName", "status" -> "Status",
  "short_description" -> "ShortDescription", "long_description" -> "LongDescription", "is_available_to_current_user" -> "IsAvailableToCurrentUser",
  "is_featured" -> "IsFeatured", "is_certified" -> "IsCertified", "page_count" -> "PageCount", "category_name" -> "CategoryName","category_description" -> "CategoryDescription",
  "redirect_url"->"RedirectURL","respondent_id"->"RespondentID","user_id"->"UserID","category_id"->"CategoryID","template_id"->"TemplateID"}

languages = {"1" -> Entity["Language", "English"], "2" -> "Chinese(Simplified)", 
 "3" -> "Chinese(Traditional)", "4" -> Entity["Language", "Danish"], 
 "5" -> Entity["Language", "Dutch"], 
 "6" -> Entity["Language", "Finnish"], 
 "7" -> Entity["Language", "French"], 
 "8" -> Entity["Language", "German"], 
 "9" -> Entity["Language", "Greek"], 
 "10" -> Entity["Language", "Italian"], 
 "11" -> Entity["Language", "Japanese"], 
 "12" -> Entity["Language", "Korean"], 
 "13" -> Entity["Language", "Malay"], 
 "14" -> Entity["Language", "Norwegian"], 
 "15" -> Entity["Language", "Polish"], "16" -> "Portuguese(Iberian)", 
 "17" -> "Portuguese(Brazilian)", 
 "18" -> Entity["Language", "Russian"], 
 "19" -> Entity["Language", "Spanish"], 
 "20" -> Entity["Language", "Swedish"], 
 "21" -> Entity["Language", "Turkish"], 
 "22" -> Entity["Language", "Ukrainian"], "23" -> "Reverse", 
 "24" -> "Albanian", "25" -> Entity["Language", "Arabic"], 
 "26" -> Entity["Language", "Armenian"], 
 "27" -> Entity["Language", "Basque"], 
 "28" -> Entity["Language", "Bengali"], 
 "29" -> Entity["Language", "Bosnian"], 
 "30" -> Entity["Language", "Bulgarian"], 
 "31" -> Entity["Language", "CatalanValencianBalear"], 
 "32" -> Entity["Language", "Croatian"], 
 "33" -> Entity["Language", "Czech"], 
 "34" -> Entity["Language", "Estonian"], 
 "35" -> Entity["Language", "Filipino"], 
 "36" -> Entity["Language", "Georgian"], 
 "37" -> Entity["Language", "Hebrew"], 
 "38" -> Entity["Language", "Hindi"], 
 "39" -> Entity["Language", "Hungarian"], 
 "40" -> Entity["Language", "Icelandic"], 
 "41" -> Entity["Language", "Indonesian"], 
 "42" -> Entity["Language", "IrishGaelic"], "43" -> "Kurdish", 
 "44" -> Entity["Language", "Latvian"], 
 "45" -> Entity["Language", "Lithuanian"], 
 "46" -> Entity["Language", "Macedonian"], 
 "47" -> Entity["Language", "Malayalam"], 
 "48" -> Entity["Language", "FarsiEastern"], 
 "49" -> Entity["Language", "PanjabiEastern"], 
 "50" -> Entity["Language", "Romanian"], 
 "51" -> Entity["Language", "Serbian"], 
 "52" -> Entity["Language", "Slovak"], 
 "53" -> Entity["Language", "Slovenian"], 
 "54" -> Entity["Language", "Swahili"], 
 "55" -> Entity["Language", "Tamil"], 
 "56" -> Entity["Language", "Telugu"], 
 "57" -> Entity["Language", "Thai"], 
 "58" -> Entity["Language", "Vietnamese"], 
 "59" -> Entity["Language", "Welsh"]}

surveymonkeycookeddata["SurveyResults", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles,resp,questions,GetAnswers,maxitems,startindex,mod,surveyID,responses,newparams},
	newparams=args/.{MaxItems:>"MaxItems"};
	invalidParameters = Select[Keys[newparams],!MemberQ[{"SurveyID","MaxItems","StartIndex"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"SurveyID"],
	(
		If[!(StringQ["SurveyID"/.newparams]||IntegerQ["SurveyID"/.newparams]),
		(	
			Message[ServiceExecute::nval,"SurveyID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["survey_id",ToString["SurveyID"/.newparams]]];
		surveyID = ToString["SurveyID"/.newparams]
	),
	(
		Message[ServiceExecute::nparam,"SurveyID","SurveyMonkey"];
		Throw[$Failed]
	)];
	(*If[KeyExistsQ[newparams,"MaxItems"],
	(
		If[!(IntegerQ["MaxItems"/.newparams]&&("MaxItems"/.newparams)>0&&("MaxItems"/.newparams)<=100),
		(	
			Message[ServiceExecute::nval,"MaxItems","SurveyMonkey"];
			Throw[$Failed]
		)];
		maxitems="MaxItems"/.newparams
	),
  	(
  		maxitems=10
  	)];
	If[KeyExistsQ[newparams,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.newparams]&&("StartIndex"/.newparams)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","SurveyMonkey"];
			Throw[$Failed]
		)];
		startindex="StartIndex"/.newparams
	),
  	(
  		startindex=1
  	)];*)
	resp = FixedPoint[Normal,ServiceExecute["SurveyMonkey","RawRespondentList","ParameterlessBodyData"->ExportString[params,"JSON"]]];
	If[("status"/.resp)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.resp)];
       	Throw[$Failed]
 	)];
 	Pause[0.5];
	resp = ("respondents" /. ("data" /. resp))[[All, 1, 2]];
	questions = FixedPoint[Normal,ServiceExecute["SurveyMonkey","RawSurveyDetails","ParameterlessBodyData"->ExportString[params,"JSON"]]];
	If[("status"/.questions)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.questions)];
       	Throw[$Failed]
 	)];
	questions = Flatten[Cases[questions, a : Rule["questions", b_] :> b, Infinity], 1];
	mod = Mod[Length[resp], 100];
	responses[respid_]:=Module[{},
		Pause[0.5];
		"data" /. FixedPoint[Normal,ServiceExecute["SurveyMonkey","RawResponses", {"ParameterlessBodyData" -> ExportString[{"survey_id" -> surveyID, "respondent_ids" -> respid}, "JSON"]}]]
	];
	If[mod!=0,
		rawdata = Flatten[responses /@ Append[Partition[resp, 100], Take[resp, -mod]], 1],
		rawdata = Flatten[responses & /@ Partition[resp, 100], 1]
	];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
 	
 	GetAnswers[data_] := Replace[data, {({"answers" -> a_, "question_id" -> b_}) :> ({"answers" -> Map[(Replace[Replace[#, {
 			(List["text" -> f_, "row" -> g_]) :> List["text" -> f, "row" -> "0"],
            (List["col" -> f_, "row" -> g_]) :> (List["answer_id" -> {f, g}, "text" -> {("text" /. Select[("answers" /. Select[ questions, (("question_id" /. #) == b) &][[1]]), (("answer_id" /. #) == f) &][[1]]), ("text" /. Select[("answers" /. Select[questions, (("question_id" /. #) == b) &][[1]]), (("answer_id" /. #) == g) &][[1]])}])
             }], {
             ("row" -> "0") :> ("answer_id" -> Missing["NotAvailable"]),
             ("row" -> d_) :> (Sequence["answer_id" -> d, "text" -> ("text" /. Select[("answers" /. Select[questions, (("question_id" /. #) == b) &][[1]]), (("answer_id" /. #) == d) &][[1]])])
            }, Infinity] &), a, {1}], "question_id" -> b,
      "heading" -> ("heading" /. 
         Select[questions, (("question_id" /. #) == b) &][[1]])})
   }, Infinity];
 
 	withCamelTitles=Replace[(GetAnswers /@ (rawdata)),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],(Null | "") -> Missing["NotAvailable"]}, Infinity];
	Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]		
]


surveymonkeycookeddata["SurveyList", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles,sdate,edate,fields,newparams},
	newparams=args/.{MaxItems:>"MaxItems"};
	invalidParameters = Select[Keys[newparams],!MemberQ[{"StartIndex","MaxItems","StartDate","EndDate","Title","RecipientMail","Fields"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"MaxItems"],
	(
		If[!(IntegerQ["MaxItems"/.newparams]&&("MaxItems"/.newparams)>0&&("MaxItems"/.newparams)<=1000),
		(	
			Message[ServiceExecute::nval,"MaxItems","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["page_size","MaxItems"/.newparams]]
	),
  	(
  		params = Append[params,Rule["page_size",10]]
  	)];
	If[KeyExistsQ[newparams,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.newparams]&&("StartIndex"/.newparams)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["page","StartIndex"/.newparams]]
	)];
	If[KeyExistsQ[newparams,"Fields"],
	(
		If[!(And@@StringMatchQ["Fields" /. newparams, Alternatives@@fieldsRules[[2;;9]][[All,2]]]),
		(	
			Message[ServiceExecute::nval,"Fields","SurveyMonkey"];
			Throw[$Failed]
		)];
		fields="Fields"/.newparams;
		params = Append[params,Rule["fields",fields/.Reverse[fieldsRules,2]]]
	),
	(
		fields=0
	)];
	If[ KeyExistsQ[newparams,"StartDate"],
	(
		If[!(StringQ["StartDate"/.newparams]||MatchQ["StartDate"/.newparams,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"StartDate","SurveyMonkey"];
			Throw[$Failed]
		)];
		sdate = DateObject[("StartDate" /. newparams)];
		If[MatchQ[sdate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"StartDate","SurveyMonkey"];
			Throw[$Failed]
		)];
        params = Append[params, Rule["start_date",DateString[TimeZoneConvert[sdate,0], {"Year", "-", "Month", "-", "Day", " ", "Time"}]]]     
	)];
	If[ KeyExistsQ[newparams,"EndDate"],
	(
		If[!(StringQ["EndDate"/.newparams]||MatchQ["EndDate"/.newparams,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"EndDate","SurveyMonkey"];
			Throw[$Failed]
		)];
		edate = DateObject[("EndDate" /. newparams)];
		If[MatchQ[edate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"EndDate","SurveyMonkey"];
			Throw[$Failed]
		)];
        params = Append[params, Rule["end_date",DateString[TimeZoneConvert[edate,0], {"Year", "-", "Month", "-", "Day", " ", "Time"}]]]             
	)];
	If[KeyExistsQ[newparams,"Title"],
	(
		If[!StringQ["Title"/.newparams],
		(	
			Message[ServiceExecute::nval,"Title","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["title","Title"/.newparams]]
	)];
	If[KeyExistsQ[newparams,"RecipientMail"],
	(
		If[!StringQ["RecipientMail"/.newparams],
		(	
			Message[ServiceExecute::nval,"RecipientMail","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["recipient_mail","RecipientMail"/.newparams]]
	)];
	rawdata = FixedPoint[Normal,ServiceExecute["SurveyMonkey","RawSurveyList","ParameterlessBodyData"->ExportString[params,"JSON"]]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("surveys" /. ("data" /. rawdata)),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity]
	/.{(y : ("DateCreated" | "DateModified") -> x_) :> (y -> If[MatchQ[x, _String], DateObject[x, TimeZone -> 0], x]),
		("Language"->a_):>("Language"->ToString[a]/.languages)
	};
	If[MatchQ[fields,0],
		Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]],
		Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]][All,Flatten[{"SurveyID",fields}]]
	]
]

surveymonkeycookeddata["SurveyDetails", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles,newparams},
	newparams=args;
	invalidParameters = Select[Keys[newparams],!MemberQ[{"SurveyID"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"SurveyID"],
	(
		If[!(StringQ["SurveyID"/.newparams]||IntegerQ["SurveyID"/.newparams]),
		(	
			Message[ServiceExecute::nval,"SurveyID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["survey_id",ToString["SurveyID"/.newparams]]]
	),
	(
		Message[ServiceExecute::nparam,"SurveyID","SurveyMonkey"];
		Throw[$Failed]
	)];
	rawdata = FixedPoint[Normal,ServiceExecute["SurveyMonkey","RawSurveyDetails","ParameterlessBodyData"->ExportString[params,"JSON"]]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("data" /. rawdata),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity]
	/.{(y : ("DateCreated" | "DateModified") -> x_) :> (y -> If[MatchQ[x, _String], DateObject[x, TimeZone -> 0], x]),
		("Language"->a_):>("Language"->ToString[a]/.languages)
	};
	Dataset[Association @@ Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]		
]

surveymonkeycookeddata["CollectorList", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles,sdate,edate,fields,newparams},
	newparams=args/.{MaxItems:>"MaxItems"};
	invalidParameters = Select[Keys[newparams],!MemberQ[{"SurveyID","StartIndex","MaxItems","StartDate","EndDate","Name","Fields"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"SurveyID"],
	(
		If[!(StringQ["SurveyID"/.newparams]||IntegerQ["SurveyID"/.newparams]),
		(	
			Message[ServiceExecute::nval,"SurveyID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["survey_id",ToString["SurveyID"/.newparams]]]
	),
	(
		Message[ServiceExecute::nparam,"SurveyID","SurveyMonkey"];
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"MaxItems"],
	(
		If[!(IntegerQ["MaxItems"/.newparams]&&("MaxItems"/.newparams)>0&&("MaxItems"/.newparams)<=1000),
		(	
			Message[ServiceExecute::nval,"MaxItems","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["page_size","MaxItems"/.newparams]]
	),
  	(
  		params = Append[params,Rule["page_size",10]]
  	)];
	If[KeyExistsQ[newparams,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.newparams]&&("StartIndex"/.newparams)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["page","StartIndex"/.newparams]]
	)];
	If[KeyExistsQ[newparams,"Fields"],
	(
		If[!(And@@StringMatchQ["Fields" /. newparams, Alternatives@@fieldsRules[[{5, 6, 14, 15, 16, 17}]][[All, 2]]]),
		(	
			Message[ServiceExecute::nval,"Fields","SurveyMonkey"];
			Throw[$Failed]
		)];
		fields="Fields"/.newparams;
		params = Append[params,Rule["fields",fields/.Reverse[fieldsRules,2]]]
	),
	(
		fields=0
	)];
	If[ KeyExistsQ[newparams,"StartDate"],
	(
		If[!(StringQ["StartDate"/.newparams]||MatchQ["StartDate"/.newparams,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"StartDate","SurveyMonkey"];
			Throw[$Failed]
		)];
		sdate = DateObject[("StartDate" /. newparams)];
		If[MatchQ[sdate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"StartDate","SurveyMonkey"];
			Throw[$Failed]
		)];
        params = Append[params, Rule["start_date",DateString[TimeZoneConvert[sdate,0], {"Year", "-", "Month", "-", "Day", " ", "Time"}]]]     
	)];
	If[ KeyExistsQ[newparams,"EndDate"],
	(
		If[!(StringQ["EndDate"/.newparams]||MatchQ["EndDate"/.newparams,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"EndDate","SurveyMonkey"];
			Throw[$Failed]
		)];
		edate = DateObject[("EndDate" /. newparams)];
		If[MatchQ[edate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"EndDate","SurveyMonkey"];
			Throw[$Failed]
		)];
        params = Append[params, Rule["end_date",DateString[TimeZoneConvert[edate,0], {"Year", "-", "Month", "-", "Day", " ", "Time"}]]]             
	)];
	If[KeyExistsQ[newparams,"Name"],
	(
		If[!StringQ["Name"/.newparams],
		(	
			Message[ServiceExecute::nval,"Name","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["name","Name"/.newparams]]
	)];
	rawdata = FixedPoint[Normal,ServiceExecute["SurveyMonkey","RawCollectorList","ParameterlessBodyData"->ExportString[params,"JSON"]]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("collectors" /. ("data" /. rawdata)),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity]
	/.{(y : ("DateCreated" | "DateModified") -> x_) :> (y -> If[MatchQ[x, _String], DateObject[x, TimeZone -> 0], x])};
	If[MatchQ[fields,0],
		Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]],
		Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]][All,Flatten[{"CollectorID",fields}]]
	]		
]

surveymonkeycookeddata["ResponseCounts", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles,newparams},
	newparams=args;
	invalidParameters = Select[Keys[newparams],!MemberQ[{"CollectorID"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"CollectorID"],
	(
		If[!(StringQ["CollectorID"/.newparams]||IntegerQ["CollectorID"/.newparams]),
		(	
			Message[ServiceExecute::nval,"CollectorID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["collector_id",ToString["CollectorID"/.newparams]]]
	),
	(
		Message[ServiceExecute::nparam,"CollectorID","SurveyMonkey"];
		Throw[$Failed]
	)];
	rawdata = FixedPoint[Normal,ServiceExecute["SurveyMonkey","RawResponseCounts","ParameterlessBodyData"->ExportString[params,"JSON"]]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("data" /. rawdata),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity];
	Dataset[Association @@ withCamelTitles]		
]

surveymonkeycookeddata["RespondentList", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles,sdate,edate,smoddate,emoddate,fields,newparams},
	newparams=args/.{MaxItems:>"MaxItems"};
	invalidParameters = Select[Keys[newparams],!MemberQ[{"SurveyID","CollectorID","StartIndex","MaxItems","StartDate","EndDate","StartModifiedDate","EndModifiedDate","SortBy","Fields"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"SurveyID"],
	(
		If[!(StringQ["SurveyID"/.newparams]||IntegerQ["SurveyID"/.newparams]),
		(	
			Message[ServiceExecute::nval,"SurveyID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["survey_id",ToString["SurveyID"/.newparams]]]
	),
	(
		Message[ServiceExecute::nparam,"SurveyID","SurveyMonkey"];
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"CollectorID"],
	(
		If[!(StringQ["CollectorID"/.newparams]||IntegerQ["CollectorID"/.newparams]),
		(	
			Message[ServiceExecute::nval,"CollectorID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["collector_id",ToString["CollectorID"/.newparams]]]
	)];
	If[KeyExistsQ[newparams,"MaxItems"],
	(
		If[!(IntegerQ["MaxItems"/.newparams]&&("MaxItems"/.newparams)>0&&("MaxItems"/.newparams)<=1000),
		(	
			Message[ServiceExecute::nval,"MaxItems","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["page_size","MaxItems"/.newparams]]
	),
  	(
  		params = Append[params,Rule["page_size",10]]
  	)];
	If[KeyExistsQ[newparams,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.newparams]&&("StartIndex"/.newparams)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["page","StartIndex"/.newparams]]
	)];
	If[KeyExistsQ[newparams,"Fields"],
	(
		If[!(And@@StringMatchQ["Fields" /. newparams, Alternatives@@fieldsRules[[{3, 6, 13, Sequence @@ Range[18, 26]}]][[All, 2]]]),
		(	
			Message[ServiceExecute::nval,"Fields","SurveyMonkey"];
			Throw[$Failed]
		)];
		fields="Fields"/.newparams;
		params = Append[params,Rule["fields",fields/.Reverse[fieldsRules,2]]]
	),
	(
		fields=0
	)];
	If[ KeyExistsQ[newparams,"StartDate"],
	(
		If[!(StringQ["StartDate"/.newparams]||MatchQ["StartDate"/.newparams,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"StartDate","SurveyMonkey"];
			Throw[$Failed]
		)];
		sdate = DateObject[("StartDate" /. newparams)];
		If[MatchQ[sdate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"StartDate","SurveyMonkey"];
			Throw[$Failed]
		)];
        params = Append[params, Rule["start_date",DateString[TimeZoneConvert[sdate,0], {"Year", "-", "Month", "-", "Day", " ", "Time"}]]]     
	)];
	If[ KeyExistsQ[newparams,"EndDate"],
	(
		If[!(StringQ["EndDate"/.newparams]||MatchQ["EndDate"/.newparams,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"EndDate","SurveyMonkey"];
			Throw[$Failed]
		)];
		edate = DateObject[("EndDate" /. newparams)];
		If[MatchQ[edate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"EndDate","SurveyMonkey"];
			Throw[$Failed]
		)];
        params = Append[params, Rule["end_date",DateString[TimeZoneConvert[edate,0], {"Year", "-", "Month", "-", "Day", " ", "Time"}]]]             
	)];
	If[ KeyExistsQ[newparams,"StartModifiedDate"],
	(
		If[!(StringQ["StartModifiedDate"/.newparams]||MatchQ["StartModifiedDate"/.newparams,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"StartModifiedDate","SurveyMonkey"];
			Throw[$Failed]
		)];
		smoddate = DateObject[("StartModifiedDate" /. newparams)];
		If[MatchQ[smoddate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"StartModifiedDate","SurveyMonkey"];
			Throw[$Failed]
		)];
        params = Append[params, Rule["start_modified_date",DateString[TimeZoneConvert[smoddate,0], {"Year", "-", "Month", "-", "Day", " ", "Time"}]]]     
	)];
	If[ KeyExistsQ[newparams,"EndModifiedDate"],
	(
		If[!(StringQ["EndModifiedDate"/.newparams]||MatchQ["EndModifiedDate"/.newparams,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"EndModifiedDate","SurveyMonkey"];
			Throw[$Failed]
		)];
		emoddate = DateObject[("EndModifiedDate" /. newparams)];
		If[MatchQ[emoddate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"EndModifiedDate","SurveyMonkey"];
			Throw[$Failed]
		)];
        params = Append[params, Rule["end_modified_date",DateString[TimeZoneConvert[emoddate,0], {"Year", "-", "Month", "-", "Day", " ", "Time"}]]]             
	)];
	If[KeyExistsQ[newparams,"SortBy"],
	(
		If[!StringMatchQ["SortBy"/.newparams, "RespondentID"|"DateModified"|"DateStart"],
		(	
			Message[ServiceExecute::nval,"SortBy","PubChem"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["order_by",("SortBy"/.newparams)/.{"RespondentID"->"respondent_id","DateModified"->"date_modified","DateStart"->"date_start"}]]
	)];
	rawdata = FixedPoint[Normal,ServiceExecute["SurveyMonkey","RawRespondentList","ParameterlessBodyData"->ExportString[params,"JSON"]]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("respondents" /. ("data" /. rawdata)),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity]
	/.{(y : ("DateStart" | "DateModified") -> x_) :> (y -> If[MatchQ[x, _String], DateObject[x, TimeZone -> 0], x])};
	If[MatchQ[fields,0],
		Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]],
		Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]][All,Flatten[{"RespondentID",fields}]]
	]		
]

surveymonkeycookeddata["Responses", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles,newparams},
	newparams=args;
	invalidParameters = Select[Keys[newparams],!MemberQ[{"SurveyID","RespondentID"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"SurveyID"],
	(
		If[!(StringQ["SurveyID"/.newparams]||IntegerQ["SurveyID"/.newparams]),
		(	
			Message[ServiceExecute::nval,"SurveyID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["survey_id",ToString["SurveyID"/.newparams]]]
	),
	(
		Message[ServiceExecute::nparam,"SurveyID","SurveyMonkey"];
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"RespondentID"],
	(
		If[!(MatchQ["RespondentID"/.newparams,{__String|__Integer}]),
		(	
			Message[ServiceExecute::nval,"RespondentID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["respondent_ids",ToString/@("RespondentID"/.newparams)]]
	),
	(
		Message[ServiceExecute::nparam,"RespondentID","SurveyMonkey"];
		Throw[$Failed]
	)];
	rawdata = FixedPoint[Normal,ServiceExecute["SurveyMonkey","RawResponses","ParameterlessBodyData"->ExportString[params,"JSON"]]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("data" /. rawdata),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity];
	Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]]		
]

surveymonkeycookeddata["UserData", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles,newparams},
	newparams=args;
	invalidParameters = Select[Keys[newparams],!MemberQ[{},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	rawdata = FixedPoint[Normal,ServiceExecute["SurveyMonkey","RawUserDetails","ParameterlessBodyData"->"{}"]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("user_details" /. ("data" /. rawdata)),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity];
	Dataset[Association @@ withCamelTitles]
]

surveymonkeycookeddata["TemplateList", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles,sdate,edate,fields,language,newparams},
	newparams=args/.{MaxItems:>"MaxItems"};
	invalidParameters = Select[Keys[newparams],!MemberQ[{"Language","CategoryID","StartIndex","MaxItems","AvailableToUser","Fields"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"Language"],
	(
		If[!(StringQ["Language"/.newparams]||MatchQ["Language"/.newparams,Entity["Language", _]]),
		(	
			Message[ServiceExecute::nval,"Language","SurveyMonkey"];
			Throw[$Failed]
		)];
		language = Interpreter["Language"]["Language"/.newparams];
		If[MatchQ[language, Failure[__]],
		(
			language = "Language"/.newparams
		)];
		If[!(MemberQ[languages[[All,2]],language]),
		(	
			Message[ServiceExecute::nval,"Language","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["language_id",ToExpression[language/.Reverse[languages,2]]]];
	)];
	If[KeyExistsQ[newparams,"CategoryID"],
	(
		If[!(StringQ["CategoryID"/.newparams]||IntegerQ["CategoryID"/.newparams]),
		(	
			Message[ServiceExecute::nval,"CategoryID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["category_id",ToString["CategoryID"/.newparams]]]
	)];
	If[KeyExistsQ[newparams,"MaxItems"],
	(
		If[!(IntegerQ["MaxItems"/.newparams]&&("MaxItems"/.newparams)>0&&("MaxItems"/.newparams)<=1000),
		(	
			Message[ServiceExecute::nval,"MaxItems","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["page_size","MaxItems"/.newparams]]
	),
  	(
  		params = Append[params,Rule["page_size",10]]
  	)];
	If[KeyExistsQ[newparams,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.newparams]&&("StartIndex"/.newparams)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["page","StartIndex"/.newparams]]
	)];
	If[KeyExistsQ[newparams,"Fields"],
	(
		If[!(And@@StringMatchQ["Fields" /. newparams, Alternatives@@fieldsRules[[{2, 4, 5, 6, 7, 8, 38, Sequence @@ Range[27, 34]}]][[All,2]]]),
		(	
			Message[ServiceExecute::nval,"Fields","SurveyMonkey"];
			Throw[$Failed]
		)];
		fields="Fields"/.newparams;
		params = Append[params,Rule["fields",fields/.Reverse[fieldsRules,2]]]
	),
	(
		fields=0
	)];
	If[KeyExistsQ[newparams,"AvailableToUser"],
	(
		If[!MemberQ[{True,False},"AvailableToUser"/.newparams],
		(	
			Message[ServiceExecute::nval,"AvailableToUser","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["show_only_available_to_current_user","AvailableToUser"/.newparams]]
	)];
	rawdata = FixedPoint[Normal,ServiceExecute["SurveyMonkey","RawTemplateList","ParameterlessBodyData"->ExportString[params,"JSON"]]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("templates" /. ("data" /. rawdata)),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity]
	/.{(y : ("DateCreated" | "DateModified") -> x_) :> (y -> If[MatchQ[x, _String], DateObject[x, TimeZone -> 0], x]),
		("Language"->a_):>("Language"->ToString[a]/.languages)
	};
	If[MatchQ[fields,0],
		Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]][All,{"TemplateID","TemplateID","ShortDescription","PreviewURL","IsAvailableToCurrentUser"}],
		Dataset[Replace[withCamelTitles, r : {__Rule} :> Association[r], -1]][All,Flatten[{"TemplateID",fields}]]
	]
]

surveymonkeycookeddata["CreateWeblinkCollector", id_,args_]:=Block[{rawdata,params={},invalidParameters,withCamelTitles,newparams},
	newparams=args;
	invalidParameters = Select[Keys[newparams],!MemberQ[{"SurveyID","Name"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"SurveyID"],
	(
		If[!(StringQ["SurveyID"/.newparams]||IntegerQ["SurveyID"/.newparams]),
		(	
			Message[ServiceExecute::nval,"SurveyID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["survey_id",ToString["SurveyID"/.newparams]]]
	),
	(
		Message[ServiceExecute::nparam,"SurveyID","SurveyMonkey"];
		Throw[$Failed]
	)];
	params = Append[params,Rule["collector",{Rule["type","weblink"]}]];
	If[KeyExistsQ[newparams,"Name"],
	(
		If[!StringQ["Name"/.newparams],
		(	
			Message[ServiceExecute::nval,"Name","SurveyMonkey"];
			Throw[$Failed]
		)];
		params=params /.{("collector"/.params) :> Append["collector"/.params,Rule["name","Name"/.newparams]]}
	)];
	rawdata = FixedPoint[Normal,ServiceExecute["SurveyMonkey","RawCreateCollector","ParameterlessBodyData"->ExportString[params,"JSON"]]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("collector" /. ("data" /. rawdata)),{ Rule[a_, b_] :> Rule[camelCase[a/.fieldsRules], b],Null -> Missing["NotAvailable"]}, Infinity]
	/.{(y : ("DateCreated" | "DateModified") -> x_) :> (y -> If[MatchQ[x, _String], DateObject[x, TimeZone -> 0], x])};
	Dataset[Association @@ withCamelTitles]
]

(*surveymonkeycookeddata["CreateEmailCollector", id_,args_]:=Block[{rawdata,params={"collector"->{"type"->"email"}},invalidParameters,withCamelTitles},
	newparams=args;
	invalidParameters = Select[Keys[newparams],!MemberQ[{"SurveyID","Name","Send","Subject","FromAddress","Body","Recipients"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"SurveyMonkey"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"SurveyID"],
	(
		If[!(StringQ["SurveyID"/.newparams]||IntegerQ["SurveyID"/.newparams]),
		(	
			Message[ServiceExecute::nval,"SurveyID","SurveyMonkey"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["survey_id",ToString["SurveyID"/.newparams]]]
	),
	(
		Message[ServiceExecute::nparam,"SurveyID","SurveyMonkey"];
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"Name"],
	(
		If[!StringQ["Name"/.newparams],
		(	
			Message[ServiceExecute::nval,"Name","SurveyMonkey"];
			Throw[$Failed]
		)];
		params=params /.{("collector"/.params) :> Append["collector"/.params,Rule["name","Name"/.newparams]]}
	)];
	rawdata = ServiceExecute["SurveyMonkey","RawCreateCollector","ParameterlessBodyData"->ExportString[params,"JSON"]];
	If[("status"/.rawdata)!= 0,
   	(
      	Message[ServiceExecute::serrormsg,("errmsg"/.rawdata)];
       	Throw[$Failed]
 	)];
	withCamelTitles=Replace[("collector" /. ("data" /. rawdata)),{ Rule[a_, b_] :> Rule[camelCase[a], b],Null -> Missing["NotAvailable"]}, Infinity]
	/.{(y : ("DateCreated" | "DateModified") -> x_) :> (y -> If[MatchQ[x, _String], DateObject[x, TimeZone -> 0], x])};
	Dataset[Association @@ withCamelTitles]
]*)

surveymonkeycookeddata[___]:=$Failed

surveymonkeyrawdata[___]:=$Failed

surveymonkeysendmessage[args_]:=$Failed

End[]

End[]

SetAttributes[{},{ReadProtected, Protected}];


(* Return two functions to define oauthservicedata, oauthcookeddata  *)

{SurveyMonkeyOAuth`Private`surveymonkeydata,SurveyMonkeyOAuth`Private`surveymonkeycookeddata,SurveyMonkeyOAuth`Private`surveymonkeysendmessage,SurveyMonkeyOAuth`Private`surveymonkeyrawdata}
