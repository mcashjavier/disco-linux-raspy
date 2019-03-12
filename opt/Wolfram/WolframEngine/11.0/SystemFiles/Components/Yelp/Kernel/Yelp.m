Get["YelpFunctions.m"]

Begin["Yelp`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* Yelp *************************************)

(* Authentication information *)

yelpdata[]={
		"ServiceName"       -> "Yelp",
 		"URLFetchFun"		:> (With[{params=Lookup[{##2},"Parameters",{}]},
        		URLFetch[signURL[#1,params],{"StatusCode","Content"}]]&)
        	,
        "ClientInfo"		:> OAuthDialogDump`Private`MultipleKeyDialog["Yelp",{"Consumer Key" -> "consumerKey","Consumer Secret" -> "consumerSecret", 
        															"Token" -> "token", "Token Secret" -> "tokenSecret"},"https://www.yelp.com/developers/manage_api_keys","https://www.yelp.com/developers/api_terms"],
	 	"Gets"				-> {"BusinessList","BusinessDataset","BusinessInformation","Categories"},
	 	"Posts"				-> {},
	 	"RawGets"			-> {"RawSearch","RawBusiness","RawPhoneSearch"},
	 	"RawPosts"			-> {},		
 		"Information"		-> "A service for exchanging data with a Yelp"
}

(**** Raw Requests ****)

yelpdata["RawSearch"] := {
        "URL"				-> "https://api.yelp.com/v2/search",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"term","limit","offset","sort","category_filter","radius_filter","deals_filter",
        						"location","cll","bounds","ll","cc","lang","actionlinks"},
        "RequiredParameters"-> {},
        "ResultsFunction"	-> formatresults
    }

yelpdata["RawBusiness"] := {
		"URL"				-> (ToString@StringForm["https://api.yelp.com/v2/business/`1`", #]&),
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"cc","lang","lang_filter","actionlinks"},
        "PathParameters"	-> {"id"},
        "RequiredParameters"-> {"id"},
        "ResultsFunction"	-> formatresults
	}
	
yelpdata["RawPhoneSearch"] := {
        "URL"				-> "https://api.yelp.com/v2/phone_search",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"phone","cc","category"},
        "RequiredParameters"-> {"phone"},
        "ResultsFunction"	-> formatresults
    }
  
yelpdata[___]:=$Failed   
   
(**** Cooked Requests ****)

yelpcookeddata[prop:("BusinessList"|"BusinessDataset"), id_, args_] := Module[{invalidParameters,location,params={},latitude,longitude,coordinates,
																	point,radius,defaultRadius="40000",sort,sortVal,query,limit,maxPerPage=20,startIndex,
																	calls,residual,progress=0,data,rawdata,errorMsg,totalResults,items={},result,cFilter,
																	swcorner,necorner,swlatitude,swlongitude,nelatitude,nelongitude,fieldnames,phone,
																	argsCopy,locationValues,tmp,fieldNamesCC,interpreterQ=False,orderList,cFilterTmp},
	invalidParameters = Select[Keys[args],!MemberQ[{"Location","Radius","MaxItems",MaxItems,"StartIndex","SortBy",
													"Query","Phone","Categories","InterpretEntities"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,prop]&/@invalidParameters;
			Throw[$Failed]
		)];	
	
	argsCopy = ReplaceAll[args,Rule["MaxItems",m_]:>Rule[MaxItems,m]];
	If[KeyExistsQ[args,"Phone"],
		(
			phone = "Phone" /. args;
			params = Append[params,"phone" -> phone];
			rawdata = KeyClient`rawkeydata[id,"RawPhoneSearch",params];
			data = formatresults[rawdata];
			
			If[rawdata[[1]]!=200,
			(
				If[KeyExistsQ[data,"error"],
					(
						errorMsg = "error" /. data;
						Message[ServiceExecute::serrormsg,errorMsg];
						Throw[$Failed]
					)];
				Message[ServiceExecute::serror];
				Throw[$Failed]
			)];	
			
			result = "businesses"/.data;
					
		),
		(
	(* geo search *)
	If[KeyExistsQ[args,"Location"],
		(
			location = "Location" /. args;
			
			(* this handles the case where the user gives a GeoPosition representation for more than one point e.g. polygons *)
			If[MatchQ[Head[location],Polygon] && MatchQ[Head[QuantityMagnitude[Latitude[location[[1]]], "AngularDegrees"]],List],
				location=GeoBoundingBox[location]];
				
			Switch[location,
				Entity["ZIPCode",_], (* US zip code *)
				(
					params = Append[params,"location"->EntityValue[location, "Name"]]
				),
				_GeoPosition, (* radial search *)
				(
					latitude = QuantityMagnitude[Latitude[location], "AngularDegrees"] //ToString;
					longitude = QuantityMagnitude[Longitude[location], "AngularDegrees"] //ToString;
				
					params = Append[params,"ll"->StringJoin[latitude,",",longitude]]
				),
				{_GeoPosition,_GeoPosition},
				(
					swcorner = location[[1]];
					necorner = location[[2]];
					
					swlatitude = QuantityMagnitude[Latitude[swcorner], "AngularDegrees"] //ToString;
					swlongitude = QuantityMagnitude[Longitude[swcorner], "AngularDegrees"] //ToString;
				
					nelatitude = QuantityMagnitude[Latitude[necorner], "AngularDegrees"] //ToString;
					nelongitude = QuantityMagnitude[Longitude[necorner], "AngularDegrees"] //ToString;
								
					params = Append[params,"bounds"->StringJoin[swlatitude,",",swlongitude,"|",nelatitude,",",nelongitude]]
				),
				_Entity,
				(
					Switch[EntityTypeName[location],
						"Country",
						(
							latitude = QuantityMagnitude[Latitude[location], "AngularDegrees"] //ToString;
							longitude = QuantityMagnitude[Longitude[location], "AngularDegrees"] //ToString;
							
							params = Append[params,"ll"->StringJoin[latitude,",",longitude]]
						),
						"City",
						(
							latitude = QuantityMagnitude[Latitude[location], "AngularDegrees"] //ToString;
							longitude = QuantityMagnitude[Longitude[location], "AngularDegrees"] //ToString;
							
							params = Append[params,"ll"->StringJoin[latitude,",",longitude]]
						),
						_,
						(
							coordinates = LatitudeLongitude[location];
							
							If[MatchQ[Head[coordinates],List],
							(
								latitude = coordinates[[1,1]] // ToString;
								longitude = coordinates[[2,1]] // ToString;
	
								params = Append[params,"ll"->StringJoin[latitude,",",longitude]]
							),
							(
								Message[ServiceExecute::nval,"Location","Yelp"];	
								Throw[$Failed]
							)]
						)
					]
				),
				_GeoDisk,
				(
					Switch[location,
						GeoDisk[],
						(
							point = $GeoLocation;
							radius = defaultRadius;
						),	
						GeoDisk[_],
						(	
							point = location[[1]];
							radius = defaultRadius;
						),
						GeoDisk[_,_,___],
						(
							point = location[[1]];
							radius = location[[2]];
							radius = QuantityMagnitude[radius, "Meters"];
							radius = ToString[Round[radius]]
						)
					];
				
					latitude = QuantityMagnitude[Latitude[point], "AngularDegrees"] //ToString;
					longitude = QuantityMagnitude[Longitude[point], "AngularDegrees"] //ToString;
				
					params = Join[params, {"ll" -> StringJoin[latitude,",",longitude], "radius_filter" -> radius}]
				),
				_, (* unrecognized Location specification *)
				(
					Message[ServiceExecute::nval,"Location","Yelp"];	
					Throw[$Failed]
				)
			]
		),
		(
			Message[ServiceExecute::nparam,"Location"];
			Throw[$Failed]
		)		
	];
	
	If[KeyExistsQ[args,"Radius"],
	(
		radius = "Radius" /. args;
		radius = QuantityMagnitude[radius, "Meters"];
		radius = ToString[Round[radius]];	
		params = Append[params, "radius_filter"->radius];		
	)];
	
	If[KeyExistsQ[args,"SortBy"],
	(
		sort = "SortBy" /. args;
		If[Head[sort]===String,
		(
			Switch[sort,
				"BestMatch",
				sortVal = "0",
				"Distance",
				sortVal = "1",
				"Rating",
				sortVal = "2",
				_,
				(
					Message[ServiceExecute::nval,"SortBy","Yelp"];	
					Throw[$Failed]
				)
			];			
		)];		
		params = Append[params, "sort"->sortVal];		
	)];
	
	If[KeyExistsQ[args,"Query"],
		(
			query = "Query" /. args;
			params = Append[params,"term" -> query]			
		)
	];
	
	If[KeyExistsQ[args,"InterpretEntities"],
		(
			interpreterQ = "InterpretEntities" /. args;
			If[!BooleanQ[interpreterQ],
			(
					Message[ServiceExecute::nval,"InterpretEntities","Yelp"];	
					Throw[$Failed]
			)]			
		)
	];
	
	If[KeyExistsQ[args,"Categories"],
		(
			cFilter = "Categories" /. args;
			Switch[Head[cFilter],
				String,
				cFilter = {cFilter},
				List,
				None,
				_,
				(
					Message[ServiceExecute::nval,"Categories","Yelp"];	
					Throw[$Failed]
				)
			];	
			cFilterTmp = {};
			If[isAlias[#],
				cFilterTmp = Append[cFilterTmp,#],
				cFilterTmp = Join[cFilterTmp,findAlias[#]]
			]&/@cFilter;
				
			cFilterTmp = StringJoin[StringRiffle[cFilterTmp, ","]];
			params = Append[params,"category_filter" -> cFilterTmp];		
			
		)
	];
	
	If[KeyExistsQ[argsCopy,MaxItems],
		(
			limit = MaxItems /. argsCopy;
			If[!IntegerQ[limit],
			(	
				Message[ServiceExecute::nval,"MaxItems","Yelp"];
				Throw[$Failed]
			)];						
	),
		limit = maxPerPage;
	];
	
	If[KeyExistsQ[args,"StartIndex"],
		(
			startIndex = "StartIndex" /. args;
			If[!IntegerQ[startIndex],
			(	
				Message[ServiceExecute::nval,"StartIndex","Yelp"];
				Throw[$Failed]
			)];
		),
		startIndex = 0		
	];
	
	calls = Quotient[limit, maxPerPage];	
	residual = limit - (calls*maxPerPage);
	
	params = Join[params,{"limit"->ToString[maxPerPage], "offset"->ToString[startIndex]}];
	
	(* this prints the progress indicator bar *)
	PrintTemporary[ProgressIndicator[Dynamic[progress], {0, calls}]];
	
	If[calls > 0,
	(
		(	
			params = ReplaceAll[params,Rule["offset",_] -> Rule["offset",ToString[startIndex+#*maxPerPage]]];
			
			rawdata = KeyClient`rawkeydata[id,"RawSearch",params];
			data = formatresults[rawdata];
			
			If[rawdata[[1]]!=200,
			(
				If[KeyExistsQ[data,"error"],
					(
						errorMsg = "error" /. data;
						Message[ServiceExecute::serrormsg,errorMsg];
						Throw[$Failed]
					)];
				Message[ServiceExecute::serror];
				Throw[$Failed]
			)];
			totalResults = "total"/.data;
			items = Join[items, If[totalResults>0,("businesses"/.data),{}]];		
			progress = progress + 1;	
		)& /@ Range[0,calls-1];		
		
	)];
	
	If[residual > 0,
	(
		params = ReplaceAll[params,Rule["offset",_] -> Rule["offset",ToString[startIndex+calls*maxPerPage]]];
		params = ReplaceAll[params,Rule["limit",_] -> Rule["limit",ToString[residual]]];
		
		rawdata = KeyClient`rawkeydata[id,"RawSearch",params];
		data = formatresults[rawdata];
			
		If[rawdata[[1]]!=200,
		(
			If[KeyExistsQ[data,"error"],
				(
					errorMsg = "error" /. data;
					Message[ServiceExecute::serrormsg,errorMsg];
					Throw[$Failed]
				)];
			Message[ServiceExecute::serror];
			Throw[$Failed]
		)];
		
		totalResults = "total"/.data;
		items = Join[items, If[totalResults>0,("businesses"/.data),{}]];
	)];
	
	result = items[[1;;Min[limit,Length[items]]]];
	
	)];

   	fieldnames = {"id","name","location","phone","rating","distance","url","categories","is_claimed","is_closed",
   					"image_url","review_count","snippet_text","deals","gift_certificates","menu_provider",
   					"menu_date_updated","reservation_url","eat24_url"};
   	
   	fieldNamesCC = Join[fieldnames,{"display_address", "address", "coordinate", "city", 
      					"state_code", "postal_code", "country_code", "cross_streets", "neighborhoods"}];
	fieldNamesCC = DeleteCases[fieldNamesCC,"location"];
   	fieldNamesCC = camelCase/@fieldNamesCC;

	result = FilterRules[#, fieldnames] & /@ result;

   	result = ReplaceAll[result,Rule[x_,y_]:>Rule[camelCase[x],y]];
 
	result = (
		locationValues = formatLocation[("Location" /. #),interpreterQ];
		tmp = DeleteCases[#, Rule["Location", _]];
		Join[tmp,locationValues]
	)&/@result;

	result = ReplaceAll[result,Rule["Distance",y_]:>Rule["Distance",Quantity[y,"Meters"]]];
	result = ReplaceAll[result,Rule["MenuDateUpdated",y_]:>Rule["MenuDateUpdated",FromUnixTime[y]]];
	result = ReplaceAll[result,Rule["Categories",y_]:>Rule["Categories",Association[{Rule["CategoryName",#[[1]]],Rule["CategoryAlias",#[[2]]]}]&/@y]];
	result = ReplaceAll[result,Rule["Deals",a_]:>Rule["Deals",Association[ReplaceAll[#,Rule[x_,y_]:>Rule[camelCase[x],y]]&/@a]]];
	result = ReplaceAll[result,Rule["GiftCertificates",a_]:>Rule["GiftCertificates",Association[ReplaceAll[#,Rule[x_,y_]:>Rule[camelCase[x],y]]&/@a]]];
	result = ReplaceAll[result,Rule["GiftCertificates",a_]:>Rule["GiftCertificates",Association[ReplaceAll[#,Rule[x_,y_]:>Rule[camelCase[x],y]]&/@a]]];
	result = ReplaceAll[result,Rule["StateCode",y_]:>Rule["State",y]];
   	result = ReplaceAll[result,Rule["CountryCode",y_]:>Rule["Country",y]];

   	fieldNamesCC = ReplaceAll[fieldNamesCC,"CountryCode":>"Country"];
   	fieldNamesCC = ReplaceAll[fieldNamesCC,"StateCode":>"State"];
   	orderList = Thread[fieldNamesCC -> Range[Length[fieldNamesCC]]];
   	fieldNamesCC = SortBy[Union[Flatten[Keys[result]]],(# /. orderList&)];

   	If[Length[result]==0,
		result = Association[],
		result = Association /@ result
	];

	result = KeyTake[result, fieldNamesCC];

	If[prop=="BusinessList",
		result,
		Dataset[result]
	]	
]

yelpcookeddata["BusinessInformation", id_, args_] := Module[{rawdata, invalidParameters,bId,fieldnames,
													result={},locationValues,errorMsg,tmp,fieldNamesCC,
													interpreterQ=False,orderList},
		invalidParameters = Select[Keys[args],!MemberQ[{"ID","InterpretEntities"},#]&]; 
	
		If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"BusinessInformation"]&/@invalidParameters;
			Throw[$Failed]
		)];	
		
		If[KeyExistsQ[args,"InterpretEntities"],
		(
			interpreterQ = "InterpretEntities" /. args;
			If[!BooleanQ[interpreterQ],
			(
					Message[ServiceExecute::nval,"InterpretEntities","Yelp"];	
					Throw[$Failed]
			)]			
		)];
	
		If[KeyExistsQ[args,"ID"],
			bId = "ID" /. args;
			Switch[bId,
			_String,
				(
					rawdata = KeyClient`rawkeydata[id,"RawBusiness",{"id"->ToString[bId]}];
					If[rawdata[[1]]!=200,
					(
						rawdata = ImportString[rawdata[[2]],"JSON"];
						If[KeyExistsQ[rawdata,"error"],
						(
							errorMsg = "error" /. rawdata;
							Message[ServiceExecute::serrormsg,errorMsg];
							Throw[$Failed]
						)];
						Message[ServiceExecute::serror];
						Throw[$Failed]
					)];
		
					result = Append[result,formatresults[rawdata]];
				),
			_List && Count[bId,_String]==Length[bId] && Length[bId] > 0,
				(
					(
						rawdata = KeyClient`rawkeydata[id,"RawBusiness",{"id"->ToString[#]}];
						If[rawdata[[1]]!=200,
						(
							rawdata = ImportString[rawdata[[2]],"JSON"];
							If[KeyExistsQ[rawdata,"error"],
							(
								errorMsg = "error" /. rawdata;
								Message[ServiceExecute::serrormsg,errorMsg];
								Throw[$Failed]
							)];
							Message[ServiceExecute::serror];
							Throw[$Failed]
						)];
		
						result = Append[result,formatresults[rawdata]];
					)&/@bId;
				), 
			_,
				(
					Message[ServiceExecute::nval,"ID","Yelp"];	
					Throw[$Failed]
				)
			],
			(
				Message[ServiceExecute::nparam,"ID"];			
				Throw[$Failed]
			)
		];
		
		fieldnames = {"id","name","location","phone","rating","distance","url","categories","is_claimed","is_closed",
   					"image_url","review_count","snippet_text","deals","gift_certificates","menu_provider",
   					"menu_date_updated","reservation_url","eat24_url","reviews"};
		fieldNamesCC = Join[fieldnames,{"display_address", "address", "coordinate", "city", 
      					"state_code", "postal_code", "country_code", "cross_streets", "neighborhoods"}];
		fieldNamesCC = DeleteCases[fieldNamesCC,"location"];
   		fieldNamesCC = camelCase/@fieldNamesCC;
   	
   		result = FilterRules[#, fieldnames] & /@ result;
	
   		result = ReplaceAll[result,Rule[x_,y_]:>Rule[camelCase[x],y]];
	
		result = (
			locationValues = formatLocation["Location" /. #,interpreterQ];
			tmp = DeleteCases[#, Rule["Location", _]];
			Join[tmp,locationValues]
		)&/@result;
	
		result = ReplaceAll[result,Rule["Distance",y_]:>Rule["Distance",Quantity[y,"Meters"]]];
		result = ReplaceAll[result,Rule["MenuDateUpdated",y_]:>Rule["MenuDateUpdated",FromUnixTime[y]]];
		result = ReplaceAll[result,Rule["Categories",y_]:>Rule["Categories",Association[{Rule["CategoryName",#[[1]]],Rule["CategoryAlias",#[[2]]]}]&/@y]];
		result = ReplaceAll[result,Rule["Deals",a_]:>Rule["Deals",Association[ReplaceAll[#,Rule[x_,y_]:>Rule[camelCase[x],y]]&/@a]]];
		result = ReplaceAll[result,Rule["GiftCertificates",a_]:>Rule["GiftCertificates",Association[ReplaceAll[#,Rule[x_,y_]:>Rule[camelCase[x],y]]&/@a]]];
		result = ReplaceAll[result,Rule["Reviews",a_]:>Rule["Reviews",formatReviews[a]]];	
   		result = ReplaceAll[result,Rule["StateCode",y_]:>Rule["State",y]];
   		result = ReplaceAll[result,Rule["CountryCode",y_]:>Rule["Country",y]];
   		
   		fieldNamesCC = ReplaceAll[fieldNamesCC,"CountryCode":>"Country"];
   		fieldNamesCC = ReplaceAll[fieldNamesCC,"StateCode":>"State"];
		orderList = Thread[fieldNamesCC -> Range[Length[fieldNamesCC]]];
	   	fieldNamesCC = SortBy[Union[Flatten[Keys[result]]],(# /. orderList&)];
   	
   		If[Length[result]==0,
			result = Association[],
			result = Association /@ result
		];
	
		result = KeyTake[result, fieldNamesCC];
		
		If[Length[result] == 1,
   			result[[1]],
   			result
   		]
]

yelpcookeddata["Categories", id_, args_] := Module[{invalidParameters,jsondata,filter,parent,result},
		invalidParameters = Select[Keys[args],!MemberQ[{"Query","Parent"},#]&]; 
	
		If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Categories"]&/@invalidParameters;
			Throw[$Failed]
		)];	
	
		jsondata = getCategories[];
		
		If[KeyExistsQ[args,"Query"],
			filter = "Query" /. args;
			If[!StringQ[filter],
			(	
				Message[ServiceExecute::nval,"Query","Yelp"];
				Throw[$Failed]
			)];	
			(* Filter categories using query term *)
			jsondata = Select[jsondata, StringContainsQ[ToLowerCase["Title"/.#],ToLowerCase[filter]]&];
		];
		
		If[KeyExistsQ[args,"Parent"],
			parent = "Parent" /. args;
			If[!StringQ[parent],
			(	
				Message[ServiceExecute::nval,"Parent","Yelp"];
				Throw[$Failed]
			)];	
			(* Filter categories by parent *)
			jsondata = Select[jsondata, MemberQ["Parents"/.#,ToLowerCase[parent]]&];
		];
		
		jsondata = ReplaceAll[jsondata,Rule["Alias",a_]:>Rule["CategoryAlias",a]];
		jsondata = ReplaceAll[jsondata,Rule["Title",a_]:>Rule["CategoryName",a]];
		
		result = Association/@jsondata;
		KeyTake[result, Union[Flatten[Keys[result]]]]
]

yelpcookeddata[___]:=$Failed

yelpsendmessage[___]:=$Failed

(* Utilities *)
getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.yelpdata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

formatresults[rawdata_] := ImportString[ToString[rawdata[[2]],CharacterEncoding->"UTF-8"],"JSON"]

signURL[url_,params_] := Module[{unsignedURL,consumerKey,consumerSecret,token,tokenSecret,result},
	(
		unsignedURL = url <> "?" <> StringJoin[Riffle[(#[[1]] <> "=" <> #[[2]]) & /@ FilterRules[params,Except["consumerKey"|"consumerSecret"|"token"|"tokenSecret"]],"&"]];
		consumerKey = "consumerKey" /. params;
		consumerSecret = "consumerSecret" /. params;
		token = "token" /. params;
		tokenSecret = "tokenSecret" /. params; 
		
		result=	OAuthSigning`Private`HMACSha1SignatureService[unsignedURL,"HMAC","GET",consumerKey,consumerSecret,token,tokenSecret];
		result
	)]

camelCase[text_] := Module[{split, partial}, (
    split = StringSplit[text, {" ","_","-"}];
    partial = Prepend[Rest[Characters[#]], ToUpperCase[Characters[#][[1]]]] & /@ split;
    partial = StringJoin[partial];
    partial = StringReplace[partial,RegularExpression["[Uu][Rr][Ll]"]->"URL"];
    partial = StringReplace[partial,RegularExpression["^[Ii][Dd]$"]->"ID"];
    partial
    )]

formatLocation[l_,interpreterQ_:False] := Module[{result,value,valueI},
  (
   result = FilterRules[l, {"display_address", "address", "coordinate", "city", 
      					"state_code", "postal_code", "country_code", "cross_streets", "neighborhoods"}];
   result = ReplaceAll[result, Rule["coordinate", c_] :> Rule["coordinate", GeoPosition[{"latitude" /. c, "longitude" /. c}]]];
   If[interpreterQ,
   (
   		If[KeyExistsQ[result,"city"],
   		(
   			value = "city" /. result;
   			valueI = Interpreter["City"][value];
   			result = ReplaceAll[result, Rule["city", c_] :> Rule["city", If[MatchQ[valueI,_Entity],valueI,value]]];
   		)];
   		If[KeyExistsQ[result,"postal_code"],
   		(
   			value = "postal_code" /. result;
   			valueI = Interpreter["ZIPCode"][value];
   			result = ReplaceAll[result, Rule["postal_code", c_] :> Rule["postal_code", If[MatchQ[valueI,_Entity],valueI,value]]];
   		)];
   		If[KeyExistsQ[result,"country_code"],
   		(
   			value = "country_code" /. result;
   			valueI = Interpreter["Country"][value];
   			result = ReplaceAll[result, Rule["country_code", c_] :> Rule["country_code", If[MatchQ[valueI,_Entity],valueI,value]]];
   		)];
   		If[KeyExistsQ[result,"state_code"],
   		(
   			value = "state_code" /. result;
   			valueI = Interpreter["USState"][value];
   			result = ReplaceAll[result, Rule["state_code", c_] :> Rule["state_code", If[MatchQ[valueI,_Entity],valueI,value]]];
   		)];
   )];
   result = ReplaceAll[result, Rule[x_, y_] :> Rule[camelCase[x], y]];
   result
)]

formatReviews[r_] := Module[{result},
  (
   result = FilterRules[r, {"rating", "time_created", "excerpt", "id", "user"}];
   result = ReplaceAll[result, Rule["time_created", c_] :> Rule["time_created", FromUnixTime[c]]];
   result = ReplaceAll[result,Rule["user",a_]:>Rule["user",Association[ReplaceAll[a,Rule[x_,y_]:>Rule[camelCase[x],y]]]]];
   result = ReplaceAll[result, Rule[x_, y_] :> Rule[camelCase[x], y]];
   result = ReplaceAll[result,Rule["Id",y_]:>Rule["ID",y]];
   result
)]
       
End[]

End[]

SetAttributes[{},{ReadProtected, Protected}];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{Yelp`Private`yelpdata,Yelp`Private`yelpcookeddata,Yelp`Private`yelpsendmessage}
