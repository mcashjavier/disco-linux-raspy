Begin["GoogleContacts`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* GoogleContacts *************************************)

(* Authentication information *)

googlecontactsdata[]=
	If[TrueQ[OAuthClient`Private`$AllowNonBlockingDialogsQ],{
    	"OAuthVersion"			-> "2.0",
		"ServiceName"			-> "GoogleContacts",
	    "AuthorizeEndpoint"		-> "https://accounts.google.com/o/oauth2/v2/auth",
	    "AccessEndpoint"		-> "https://www.googleapis.com/oauth2/v4/token",
	    "RedirectURI" 			-> "WolframConnectorChannelListen",
        "Blocking"				-> False,
        "VerifierLabel"			-> "code",
        "ClientInfo"			-> {"Wolfram","Token"},
        "AuthorizationFunction"	-> "GoogleContacts",
        "RedirectURLFunction"	-> (#1&),
		"AccessTokenExtractor"	-> "Refresh/2.0",
		"RefreshAccessTokenFunction" -> Automatic,
		"VerifyPeer"			-> True,
        "AuthenticationDialog"	:> "WolframConnectorChannel",
	 	"AuthenticationDialog" :> "WolframConnectorChannel",
	 	"Gets"				-> {"ContactsList","ContactsDataset","GroupList","GroupDataset","ContactInformation","ContactPhoto"},
	 	"Posts"				-> {},
	 	"RawGets"			-> {"RawContacts","RawContactDetails","RawGroups","RawContactPhoto"},
	 	"RawPosts"			-> {},
	 	"Scope"				-> {"https%3A%2F%2Fwww.google.com%2Fm8%2Ffeeds+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fcontacts.readonly"},
 		"Information"		-> "A service for receiving data from Google Contacts"
},
{
    	"OAuthVersion"			-> "2.0",
		"ServiceName"			-> "GoogleContacts",
	    "AuthorizeEndpoint"		-> "https://accounts.google.com/o/oauth2/v2/auth",
	    "AccessEndpoint"		-> "https://www.googleapis.com/oauth2/v4/token",
        "RedirectURI"			-> "https://www.wolfram.com/oauthlanding/?service=GooglePlus",
        "VerifierLabel"			-> "code",
        "ClientInfo"			-> {"Wolfram","Token"},
        "AuthorizationFunction"	-> "GoogleContacts",
		"AccessTokenExtractor"	-> "Refresh/2.0",
		"RefreshAccessTokenFunction" -> Automatic,
	 	"AuthenticationDialog" :> (OAuthClient`tokenOAuthDialog[#, "GoogleContacts"]&),
	 	"Gets"				-> {"ContactsList","ContactsDataset","GroupList","GroupDataset","ContactInformation","ContactPhoto"},
	 	"Posts"				-> {},
	 	"RawGets"			-> {"RawContacts","RawContactDetails","RawGroups","RawContactPhoto"},
	 	"RawPosts"			-> {},
	 	"Scope"				-> {"https%3A%2F%2Fwww.google.com%2Fm8%2Ffeeds+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fcontacts.readonly"},
 		"Information"		-> "A service for receiving data from Google Contacts"
}]

(* a function for importing the raw data - usually json or xml - from the service *)
googlecontactsimport[$Failed]:=Throw[$Failed]
(*googlecontactsimport[json_String]:=With[{res=ImportString[json,"JSON"]},
	If[FreeQ[res,_["error",_]],
		Association@res,
		Message[ServiceExecute::apierr,"message"/.("error"/.res)];
		Throw[$Failed]
	]
]*)

googlecontactsimport[raw_]:=raw

googlecontactsimportphoto[raw_]:=ImportString[FromCharacterCode[raw],"Image"]


(****** Raw Properties ******)
(* information:
 Each entry includes the api endpoint, the HTTP method ("GET" or "POST") as well as the different types of parameters that are used
 "Parameters" - standard URL parameters that appear in the query string as ?param1=val1&param2=val2...
 "PathParameters" - parameters that are included in the url path scheme://domain/path1/`1`/`2`...  make the URL (ToString[StringForm[...,#1,#2]]&) 
 "BodyData" - parameters in HTTP Post requests that are includes as body data
 "MultipartData" - parameters in HTTP Post requests that are includes as multip part data, 
 		usually large files or images are multipart data, each parameter should be given as {"parametername","datatype"} 
 "RequiredParameters" - all above parameters are assumed to be optional, list required parameters a second time here
 "Headers" - additional headers to be included in the HTTP request
 "RequiredPermissions" - If we support incrementally adding permission for a service, list the required permissions for the request here*)
 
(*** Raw ***) 

(** People **)
googlecontactsdata["RawContacts"] = {
        "URL"					-> (ToString@StringForm["https://www.google.com/m8/feeds/contacts/`1`/full",#]&),
        "PathParameters"		-> {"userEmail"},
        "Parameters"			-> {"max-results","start-index","updated-min","alt","q","orderby","showdeleted","requirealldeleted","sortorder","group"},
        "RequiredParameters"	-> {"userEmail"},
        "HTTPSMethod"			-> "GET",
        "Headers"				-> {"GData-Version"->"3.0"},
        "ResultsFunction"		-> googlecontactsimport
    }

googlecontactsdata["RawContactDetails"] = {
        "URL"					-> (ToString@StringForm["https://www.google.com/m8/feeds/contacts/`1`/full/`2`",##]&),
        "PathParameters"		-> {"userEmail","contactID"},
        "Parameters"			-> {"alt"},
        "RequiredParameters"	-> {"userEmail","contactID"},
        "HTTPSMethod"			-> "GET",
        "Headers"				-> {"GData-Version"->"3.0"},
        "ResultsFunction"		-> googlecontactsimport
    }
    
googlecontactsdata["RawContactPhoto"] = {
        "URL"					-> (ToString@StringForm["https://www.google.com/m8/feeds/photos/media/`1`/`2`",##]&),
        "PathParameters"		-> {"userEmail","contactID"},
        "Parameters"			-> {"alt"},
        "RequiredParameters"	-> {"userEmail","contactID"},
        "HTTPSMethod"			-> "GET",
        "ReturnContentData"		-> True,
        "Headers"				-> {"GData-Version"->"3.0"},
        "ResultsFunction"		-> googlecontactsimportphoto
    }

googlecontactsdata["RawGroups"] = {
        "URL"					-> (ToString@StringForm["https://www.google.com/m8/feeds/groups/`1`/full",#]&),
        "PathParameters"		-> {"userEmail"},
        "Parameters"			-> {"max-results","start-index","updated-min","alt","q","orderby","showdeleted","requirealldeleted","sortorder"},
        "RequiredParameters"	-> {"userEmail"},
        "HTTPSMethod"			-> "GET",
        "Headers"				-> {"GData-Version"->"3.0"},
        "ResultsFunction"		-> googlecontactsimport
    }

$GooglePlusRefreshAPI="https://www.wolframcloud.com/objects/user-fa95220f-871c-4331-84ab-7951dd0666ca/googlecontactsrefresh";
googlecontactsdata["RefreshAccessTokenFunction"]:=(
	ToExpression[
		URLFetch[$GooglePlusRefreshAPI, 
  			"Parameters" -> {"refreshtoken" -> ToString[#,InputForm], 
    		"AccessEndpoint" -> ("AccessEndpoint"/.googlecontactsdata[])},
   			"VerifyPeer" -> False]]&)/;OAuthClient`Private`$OAuthCloudCredentialsQ
   			
googlecontactsdata["RefreshAccessTokenFunction"]:=(Block[{url,info,res, data,key, time},
	If[#===None,Return[$Failed]];
	info=OAuthClient`Private`getclientinfo["GooglePlus"];
	url={"AccessEndpoint"/.googlecontactsdata[],
		"BodyData"->URLQueryEncode[{"refresh_token"->#[[1]],
		 "client_id"->info[[1]] ,
		 "client_secret"->info[[2]],
		 "grant_type"->"refresh_token"
		}],
		"Method"->"POST"
	};
	res=URLFetch@@url;
	If[StringQ[res],
		data=ImportString[res,"JSON"];
		If[MatchQ[data, _?OptionQ],
			key="access_token"/.data;
			If[StringQ[key]&&key=!="access_token",
				time=ToExpression["expires_in"/.data]+AbsoluteTime[];
				{key,time},
				$Failed
			],
			$Failed
		],
		$Failed
	]
]&)

googlecontactsdata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  

googlecontactscookeddata[prop_,id_,rules___Rule]:=googlecontactscookeddata[prop,id,{rules}]  

(* Cooked *)
googlecontactscookeddata[prop:("ContactsList"|"ContactsDataset"), id_, args_] := Module[{params={},date,query,sort,sortVal,sortDir,sd,group,rawdata,invalidParameters,limit,defaultPerPage=25,maxPerPage=250,startIndex,
											argsCopy,calls,residual,progress,data,fieldnames,orderList,result,totalResults,items={}},
		invalidParameters = Select[Keys[args],!MemberQ[{"MaxItems",MaxItems,"StartIndex","UpdatedDate","Query","SortBy","ShowDeleted","GroupID"},#]&]; 
	
		If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,prop]&/@invalidParameters;
			Throw[$Failed]
		)];	
		
		argsCopy = ReplaceAll[args,Rule["MaxItems",m_]:>Rule[MaxItems,m]];
		params = Join[params,{"userEmail"->"default","alt"->"json"}];
		
		If[KeyExistsQ[args,"UpdatedDate"], (* minimum update date *)
		(
			date = "UpdatedDate" /. args;
			If[!DateObjectQ[date],
			(	
				Message[ServiceExecute::nval,"UpdatedDate","GoogleContacts"];
				Throw[$Failed]
			)];	
			date = DateString[date, "ISODateTime"];
			params = Append[params,"updated-min"->date];			
		)];
		
		If[KeyExistsQ[args,"Query"],
		(
			query = "Query" /. args;
			params = Append[params,"q"->query];			
		)];
		
		If[KeyExistsQ[args,"SortBy"],
		(
			sort = "SortBy" /. args;
			If[MatchQ[sort, {_String, _String}],
			(
				If[sort[[2]] == "Ascending", sortDir = "ascending",
				(
					If[sort[[2]] == "Descending", 
						sortDir = "descending",
						(
							Message[ServiceExecute::nval,"SortBy","GoogleContacts"];	
							Throw[$Failed]
						)
					]
				)];		
				params = Append[params,Rule["sortorder",sortDir]];
				sort = sort[[1]];
			)];
			Switch[sort,
				"LastModified",
				sortVal = "lastmodified",
				_,
				(
					Message[ServiceExecute::nval,"SortBy","GoogleContacts"];	
					Throw[$Failed]
				)
			];			
			params = Append[params,"orderby"->"lastmodified"];		
		)];
	
		If[KeyExistsQ[args,"ShowDeleted"],
		(
			sd = "ShowDeleted" /. args;
			Switch[sd,
				True,
				params = Append[params,"showdeleted"->"true"],
				False,
				params = Append[params,"showdeleted"->"false"],
				_,
				(
					Message[ServiceExecute::nval,"ShowDeleted","GoogleContacts"];	
					Throw[$Failed]
				)
			];		
		)];
		
		If[KeyExistsQ[args,"GroupID"],
		(
			group = "GroupID" /. args;
			params = Append[params,"group"->group];			
		)];
		
		If[KeyExistsQ[argsCopy,MaxItems],
		(
			limit = MaxItems /. argsCopy;
			If[!IntegerQ[limit],
			(	
				Message[ServiceExecute::nval,"MaxItems","GoogleContacts"];
				Throw[$Failed]
			)];						
		),
			limit = defaultPerPage;
		];
	
		If[KeyExistsQ[args,"StartIndex"],
		(
			startIndex = "StartIndex" /. args;
			If[!IntegerQ[startIndex],
			(	
				Message[ServiceExecute::nval,"StartIndex","GoogleContacts"];
				Throw[$Failed]
			)];
		),
			startIndex = 1		
		];
		
		calls = Quotient[limit, maxPerPage];	
		residual = limit - (calls*maxPerPage);
	
		params = Join[params,{"max-results"->ToString[maxPerPage], "start-index"->ToString[startIndex]}];
	
		(* this prints the progress indicator bar *)
		PrintTemporary[ProgressIndicator[Dynamic[progress], {0, calls}]];
	
		If[calls > 0,
		(
			(	
				params = ReplaceAll[params,Rule["start-index",_] -> Rule["start-index",ToString[startIndex+#*maxPerPage]]];
				
				rawdata = OAuthClient`rawoauthdata[id,"RawContacts",params];
				data = formatresults[rawdata];
				
				If[KeyExistsQ[data,"error"],
				(
					Message[ServiceExecute::serrormsg,"message"/.("error"/.data)];
					Throw[$Failed]
				)];
				
				data = "feed" /. data;
				totalResults = FromDigits["$t" /. ("openSearch$totalResults" /. data)];
				If[KeyExistsQ[data,"entry"],
					items = Join[items, If[totalResults>0,("entry"/.data),{}]]
				];	
				progress = progress + 1;	
			)& /@ Range[0,calls-1];		
		
		)];
	
		If[residual > 0,
		(
			params = ReplaceAll[params,Rule["start-index",_] -> Rule["start-index",ToString[startIndex+calls*maxPerPage]]];
			params = ReplaceAll[params,Rule["max-results",_] -> Rule["max-results",ToString[residual]]];
			
			rawdata = OAuthClient`rawoauthdata[id,"RawContacts",params];
			data = formatresults[rawdata];
			
			If[KeyExistsQ[data,"error"],
			(
				Message[ServiceExecute::serrormsg,"message"/.("error"/.data)];
				Throw[$Failed]
			)];
			
			data = "feed" /. data;
			totalResults = FromDigits["$t" /. ("openSearch$totalResults" /. data)];
			If[KeyExistsQ[data,"entry"],
				items = Join[items, If[totalResults>0,("entry"/.data),{}]]
			];
		)];
	
		result = items[[1;;Min[limit,Length[items]]]];
	
		fieldnames = {"id","updated","title","gd$email","gd$organization","gd$phoneNumber","gd$postalAddress","content"};
		result = FilterRules[#, fieldnames] & /@ result;
		
		orderList = Thread[fieldnames -> Range[Length[fieldnames]]];
   		result = Function[r,SortBy[r, (#[[1]] /. orderList&)]]/@result;
   
		result = ReplaceAll[result,Rule["id",y_]:>Rule["ID",Last[StringSplit["$t"/.y,"/"]]]];
		result = ReplaceAll[result,Rule["updated",y_]:>Rule["Updated",DateObject["$t"/.y]]];
		result = ReplaceAll[result,Rule["title",y_]:>Rule["Title","$t"/.y]];
		result = ReplaceAll[result,Rule["content",y_]:>Rule["Content","$t"/.y]];
		(*result = ReplaceAll[result,Rule["link",y_]:>Rule["Link",y]];*)
		result = ReplaceAll[result,Rule["gd$email",y_]:>Rule["Email","address"/.y]];
		result = ReplaceAll[result,Rule["gd$organization",y_]:>Rule["Organization",FilterRules[#,{"gd$orgTitle","gd$orgName"}]&/@y]];
		result = ReplaceAll[result,Rule["Organization",y_]:>Rule["Organization",ReplaceAll[#,{Rule["gd$orgTitle",z_]:>Rule["Title","$t"/.z],Rule["gd$orgName",x_]:>Rule["OrganizationName","$t"/.x]}]&/@y]];
		result = ReplaceAll[result,Rule["Organization",y_]:>Rule["Organization",Association/@y]];
		result = ReplaceAll[result,Rule["gd$phoneNumber",y_]:>Rule["PhoneNumber","$t"/.y]];
		result = ReplaceAll[result,Rule["gd$postalAddress",y_]:>Rule["PostalAddress","$t"/.y]];
		
		If[Length[result]==0,
			result = Association[],
			result = Association /@ result
		];
		If[prop=="ContactsList",
			result,
			Dataset[result]
		]	
]

googlecontactscookeddata[prop:("GroupList"|"GroupDataset"), id_, args_] := Module[{params={},date,query,sort,sortVal,sortDir,sd,rawdata,invalidParameters,limit,defaultPerPage=25,maxPerPage=250,startIndex,
											argsCopy,calls,residual,progress,data,fieldnames,orderList,result,totalResults,items={}},
		invalidParameters = Select[Keys[args],!MemberQ[{"MaxItems",MaxItems,"StartIndex","UpdatedDate","Query","SortBy","ShowDeleted"},#]&]; 
	
		If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,prop]&/@invalidParameters;
			Throw[$Failed]
		)];	
		
		argsCopy = ReplaceAll[args,Rule["MaxItems",m_]:>Rule[MaxItems,m]];
		params = Join[params,{"userEmail"->"default","alt"->"json"}];
		
		If[KeyExistsQ[args,"UpdatedDate"],
		(
			date = "UpdatedDate" /. args;
			If[!DateObjectQ[date],
			(	
				Message[ServiceExecute::nval,"UpdatedDate","GoogleContacts"];
				Throw[$Failed]
			)];	
			date = DateString[date, "ISODateTime"] <> "Z";
			params = Append[params,"updated-min"->date];			
		)];
		
		If[KeyExistsQ[args,"Query"],
		(
			query = "Query" /. args;
			params = Append[params,"q"->query];			
		)];
		
		If[KeyExistsQ[args,"SortBy"],
		(
			sort = "SortBy" /. args;
			If[MatchQ[sort, {_String, _String}],
			(
				If[sort[[2]] == "Ascending", sortDir = "ascending",
				(
					If[sort[[2]] == "Descending", 
						sortDir = "descending",
						(
							Message[ServiceExecute::nval,"SortBy","GoogleContacts"];	
							Throw[$Failed]
						)
					]
				)];		
				params = Append[params,Rule["sortorder",sortDir]];
				sort = sort[[1]];
			)];
			Switch[sort,
				"LastModified",
				sortVal = "lastmodified",
				_,
				(
					Message[ServiceExecute::nval,"SortBy","GoogleContacts"];	
					Throw[$Failed]
				)
			];			
			params = Append[params,"orderby"->"lastmodified"];		
		)];		
	
		If[KeyExistsQ[args,"ShowDeleted"],
		(
			sd = "ShowDeleted" /. args;
			Switch[sd,
				True,
				params = Append[params,"showdeleted"->"true"],
				False,
				params = Append[params,"showdeleted"->"false"],
				_,
				(
					Message[ServiceExecute::nval,"ShowDeleted","GoogleContacts"];	
					Throw[$Failed]
				)
			];		
		)];
		
		If[KeyExistsQ[argsCopy,MaxItems],
		(
			limit = MaxItems /. argsCopy;
			If[!IntegerQ[limit],
			(	
				Message[ServiceExecute::nval,"MaxItems","GoogleContacts"];
				Throw[$Failed]
			)];						
		),
			limit = defaultPerPage;
		];
	
		If[KeyExistsQ[args,"StartIndex"],
		(
			startIndex = "StartIndex" /. args;
			If[!IntegerQ[startIndex],
			(	
				Message[ServiceExecute::nval,"StartIndex","GoogleContacts"];
				Throw[$Failed]
			)];
		),
			startIndex = 1		
		];
		
		calls = Quotient[limit, maxPerPage];	
		residual = limit - (calls*maxPerPage);
	
		params = Join[params,{"max-results"->ToString[maxPerPage], "start-index"->ToString[startIndex]}];
	
		(* this prints the progress indicator bar *)
		PrintTemporary[ProgressIndicator[Dynamic[progress], {0, calls}]];
	
		If[calls > 0,
		(
			(	
				params = ReplaceAll[params,Rule["start-index",_] -> Rule["start-index",ToString[startIndex+#*maxPerPage]]];
				rawdata = OAuthClient`rawoauthdata[id,"RawGroups",params];
				data = formatresults[rawdata];
				
				If[KeyExistsQ[data,"error"],
				(
					Message[ServiceExecute::serrormsg,"message"/.("error"/.data)];
					Throw[$Failed]
				)];
				
				(*totalResults = FromDigits["$t" /. ("openSearch$totalResults" /. ("feed" /. data))];*)
				items = Join[items, If[KeyExistsQ["feed"/.data,"entry"],("entry"/.("feed"/.data)),{}]];	
				progress = progress + 1;	
			)& /@ Range[0,calls-1];		
		
		)];
	
		If[residual > 0,
		(
			params = ReplaceAll[params,Rule["start-index",_] -> Rule["start-index",ToString[startIndex+calls*maxPerPage]]];
			params = ReplaceAll[params,Rule["max-results",_] -> Rule["max-results",ToString[residual]]];
			rawdata = OAuthClient`rawoauthdata[id,"RawGroups",params];
			data = formatresults[rawdata];
			
			If[KeyExistsQ[data,"error"],
			(
				Message[ServiceExecute::serrormsg,"message"/.("error"/.data)];
				Throw[$Failed]
			)];
			(*totalResults = FromDigits["$t" /. ("openSearch$totalResults" /. ("feed" /. data))];*)
			items = Join[items, If[KeyExistsQ["feed"/.data,"entry"],("entry"/.("feed"/.data)),{}]];			
		)];
	
		result = items[[1;;Min[limit,Length[items]]]];
			
		fieldnames = {"id","updated","title"};
		result = FilterRules[#, fieldnames] & /@ result;
		
		orderList = Thread[fieldnames -> Range[Length[fieldnames]]];
   		result = Function[r,SortBy[r, (#[[1]] /. orderList&)]]/@result;
   
		result = ReplaceAll[result,Rule["id",y_]:>Rule["GroupID","$t"/.y]];
		result = ReplaceAll[result,Rule["updated",y_]:>Rule["Updated",DateObject["$t"/.y]]];
		result = ReplaceAll[result,Rule["title",y_]:>Rule["Title","$t"/.y]];
		
		If[Length[result]==0,
			result = Association[],
			result = Association /@ result
		];
		If[prop=="GroupList",
			result,
			Dataset[result]
		]	
]

googlecontactscookeddata["ContactInformation", id_, args_] := Module[{rawdata, invalidParameters,cId,fieldnames,orderList,result},
		invalidParameters = Select[Keys[args],!MemberQ[{"ContactID"},#]&]; 
	
		If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"ContactInformation"]&/@invalidParameters;
			Throw[$Failed]
		)];	
	
		If[KeyExistsQ[args,"ContactID"],
			cId = "ContactID" /. args,
			(
				Message[ServiceExecute::nparam,"ContactID"];			
				Throw[$Failed]
			)
		];
		
		rawdata = OAuthClient`rawoauthdata[id,"RawContactDetails",{"userEmail"->"default","alt"->"json","contactID"->ToString[cId]}];
		rawdata = formatresults[rawdata];
				
		If[KeyExistsQ[rawdata,"error"],
		(
			Message[ServiceExecute::serrormsg,"message"/.("error"/.rawdata)];
			Throw[$Failed]
		)];
				
		rawdata = "entry" /. rawdata;
		
		fieldnames = {"id","updated","title","gd$email","gd$organization","gd$phoneNumber","gd$postalAddress","gd$deleted"};
		rawdata = FilterRules[rawdata,fieldnames];
		orderList = Thread[fieldnames -> Range[Length[fieldnames]]];
   	
   		result = SortBy[rawdata, (#[[1]] /. orderList&)];
   		
		result = ReplaceAll[result,Rule["id",y_]:>Rule["ID",Last[StringSplit["$t"/.y,"/"]]]];
		result = ReplaceAll[result,Rule["updated",y_]:>Rule["Updated",DateObject["$t"/.y]]];
		result = ReplaceAll[result,Rule["title",y_]:>Rule["Title","$t"/.y]];
		result = ReplaceAll[result,Rule["content",y_]:>Rule["Content","$t"/.y]];
		result = ReplaceAll[result,Rule["gd$email",y_]:>Rule["Email","address"/.y]];
		result = ReplaceAll[result,Rule["gd$organization",y_]:>Rule["Organization",FilterRules[#,{"gd$orgTitle","gd$orgName"}]&/@y]];
		result = ReplaceAll[result,Rule["Organization",y_]:>Rule["Organization",ReplaceAll[#,{Rule["gd$orgTitle",z_]:>Rule["Title","$t"/.z],Rule["gd$orgName",x_]:>Rule["OrganizationName","$t"/.x]}]&/@y]];
		result = ReplaceAll[result,Rule["Organization",y_]:>Rule["Organization",Association/@y]];
		result = ReplaceAll[result,Rule["gd$phoneNumber",y_]:>Rule["PhoneNumber","$t"/.y]];
		result = ReplaceAll[result,Rule["gd$postalAddress",y_]:>Rule["PostalAddress","$t"/.y]];
		result = ReplaceAll[result,Rule["gd$deleted",y_]:>Rule["Deleted",y]];
		
		Association[result]		
]

googlecontactscookeddata["ContactPhoto", id_, args_] := Module[{rawdata, invalidParameters,cId,tmp},
		invalidParameters = Select[Keys[args],!MemberQ[{"ContactID"},#]&]; 
	
		If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"ContactPhoto"]&/@invalidParameters;
			Throw[$Failed]
		)];	
	
		If[KeyExistsQ[args,"ContactID"],
			cId = "ContactID" /. args,
			(
				Message[ServiceExecute::nparam,"ContactID"];			
				Throw[$Failed]
			)
		];
		
		rawdata = OAuthClient`rawoauthdata[id,"RawContactPhoto",{"userEmail"->"default","contactID"->ToString[cId]}];
		tmp = ImportString[FromCharacterCode[rawdata]];
		If[Head[tmp]===String && tmp === "Photo not found",
			Missing["NotAvailable"],
			googlecontactsimportphoto[rawdata]
		]
				
						
]

(* Send Message *)

googlecontactssendmessage[___]:=$Failed

(*** Utilities ***)
formatresults[rawdata_] := ImportString[ToString[rawdata,CharacterEncoding->"UTF-8"],"JSON"]
filterparameters=OAuthClient`Private`filterParameters;
camelcase=OAuthClient`Private`camelCase;
fp=OAuthClient`Private`formatpath;

getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.googlecontactsdata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

End[] (* End Private Context *)
           		
End[]


SetAttributes[{},{ReadProtected, Protected}];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{GoogleContacts`Private`googlecontactsdata,GoogleContacts`Private`googlecontactscookeddata,GoogleContacts`Private`googlecontactssendmessage}
