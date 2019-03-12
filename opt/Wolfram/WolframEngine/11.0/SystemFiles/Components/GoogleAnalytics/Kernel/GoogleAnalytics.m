
Begin["GoogleAnalyticsOAuth`"] (* Begin Private Context *) 

Begin["`Private`"](* Begin Private Context *) 

(******************************* GoogleAnalytics *************************************)

(* Authentication information *)

googleanalyticsdata[]=
	If[TrueQ[OAuthClient`Private`$AllowNonBlockingDialogsQ],{
		"OAuthVersion"		-> "2.0",
		"ServiceName"       -> "Google Analytics",
	    "AuthorizeEndpoint"	-> "https://accounts.google.com/o/oauth2/v2/auth",
	    "AccessEndpoint"	-> "https://www.googleapis.com/oauth2/v4/token",
	    "RedirectURI"       -> "WolframConnectorChannelListen",
	    "Blocking"          -> False,
	    "VerifierLabel"     -> "code",
        "ClientInfo"			-> {"Wolfram","Token"}, 
        "AuthorizationFunction"	-> "GoogleAnalytics",
        "RedirectURLFunction"	-> (#1&),
		"AccessTokenExtractor"	-> "Refresh/2.0",
		"RefreshAccessTokenFunction" -> Automatic,
	 	"AuthenticationDialog" -> "WolframConnectorChannel",
	 	"RequestFormat" 	-> {"Headers","Bearer"},
	 	"Gets"				-> {"ReportData","AllData","Metrics","Dimensions"},
	 	"Posts"				-> {},
	 	"RawGets"			-> {"RawData"},
	 	"RawPosts"			-> {},
	 	"RawDeletes"		-> {},
	 	"Deletes"           -> {},
	 	"Scope"				-> {"https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fanalytics"},
 		"Information"		-> "A service to use Google Analytics"
	},{
		"OAuthVersion"		-> "2.0",
		"ServiceName"       -> "Google Analytics",
	    "AuthorizeEndpoint"	-> "https://accounts.google.com/o/oauth2/v2/auth",
	    "AccessEndpoint"	-> "https://www.googleapis.com/oauth2/v4/token",
	    "RedirectURI"       -> "https://www.wolfram.com/oauthlanding/?service=GoogleAnalytics",
	    "VerifierLabel"     -> "code",
	 	"ClientInfo"		-> {"Wolfram","Token"},
        "AuthorizationFunction"	-> "GoogleAnalytics",
		"AccessTokenExtractor"	-> "Refresh/2.0",
		"RefreshAccessTokenFunction" -> Automatic,
	 	"AuthenticationDialog" :> (OAuthClient`tokenOAuthDialog[#, "Google Analytics"]&),
	 	"RequestFormat" 	-> {"Headers","Bearer"},
	 	"Gets"				-> {"ReportData","AllData","Metrics","Dimensions"},
	 	"Posts"				-> {},
	 	"RawGets"			-> {"RawData"},
	 	"RawPosts"			-> {},
	 	"RawDeletes"		-> {},
	 	"Deletes"           -> {},
	 	"Scope"				-> {"https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fanalytics"},
 		"Information"		-> "A service to use Google Analytics"
	}
]

(* a function for importing the raw data - usually json or xml - from the service *)
googleanalyticsimport[$Failed]:=Throw[$Failed]
(*googleanalyticsimport[json_String]:=With[{res=ImportString[json,"JSON"]},
	If[FreeQ[res,_["error",_]],
		Association@res,
		Message[ServiceExecute::apierr,"message"/.("error"/.res)];
		Throw[$Failed]
	]
]*)


googleanalyticsimport[raw_]:=ImportString[FromCharacterCode[raw, "UTF-8"], "RawJSON"]



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

googleanalyticsdata["RawData"] = {
        "URL"				-> "https://www.googleapis.com/analytics/v3/data/ga",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Headers" 			-> {"Content-Type" -> "application/json"},
        "Parameters"		-> {"ids", "start-date", "end-date", "metrics", "dimensions", "sort", "filters", "segment", "samplingLevel", "start-index", "max-results", "output", "fields", "prettyPrint", "userIp", "quotaUser", "access_token", "callback", "key"},
        "RequiredParameters"-> {"ids", "start-date", "end-date", "metrics"},
        "HTTPSMethod"		-> "GET",
        "ReturnContentData"	-> True,
        "ResultsFunction"	-> googleanalyticsimport
    }

$GoogleAnalyticsRefreshAPI="https://www.wolframcloud.com/objects/user-00e58bd3-2dfd-45b3-b80b-d281d360703a/googleplusrefresh";

googleanalyticsdata["RefreshAccessTokenFunction"]:=(
	ToExpression[
		URLFetch[$GoogleAnalyticsRefreshAPI, 
  			"Parameters" -> {"refreshtoken" -> ToString[#,InputForm], 
    		"AccessEndpoint" -> ("AccessEndpoint"/.googleanalyticsdata[])},
   			"VerifyPeer" -> False]]&)/;OAuthClient`Private`$OAuthCloudCredentialsQ
   			
googleanalyticsdata["RefreshAccessTokenFunction"]:=(Block[{url,info,res, data,key, time},
	If[#===None,Return[$Failed]];
	info=OAuthClient`Private`getclientinfo["GoogleAnalytics"];
	url={"AccessEndpoint"/.googleanalyticsdata[],
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

 
googleanalyticsdata[___]:=$Failed

(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  

googleanalyticscookeddata[prop_,id_]:=googleanalyticscookeddata[prop,id,{}]
  
googleanalyticscookeddata[prop_,id_,rules___Rule]:=googleanalyticscookeddata[prop,id,{rules}]
(* Cooked *)

googleanalyticscookeddata["UserData",id_,args_]:=Module[{params,rawdata, data},
	params=filterparameters[args,getallparameters["RawUserData"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawUserData",params];
	data=googleanalyticsimport[rawdata];
	Association[Replace[Normal[data],(Rule[a_,b_]):>(Rule[camelcase[a],b]),Infinity]]
]

googleanalyticscookeddata[req:"ReportData"|"AllData",id_,args_]:=Block[{rawdata,invalidParameters,params={},fields,columns,maxitems,sdate,edate,metrics,dimensions,sort,newparams},
	newparams=args/.{MaxItems:>"MaxItems"};
	invalidParameters = Select[Keys[newparams],!MemberQ[{"ProfileID","StartDate","EndDate","Metrics","Dimensions","Sort","Filters","Segment","SamplingLevel","MaxItems","StartIndex","UserIP","QuotaUser","Fields"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"GoogleAnalytics"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"ProfileID"],
	(
		If[!(IntegerQ["ProfileID"/.newparams]||StringQ["ProfileID"/.newparams]),
		(	
			Message[ServiceExecute::nval,"ProfileID","GoogleAnalytics"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["ids","ga:"<>ToString["ProfileID" /. newparams]]]
	)];
	If[ KeyExistsQ[newparams,"StartDate"],
	(
		If[!(StringQ["StartDate"/.newparams]||MatchQ["StartDate"/.newparams,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"StartDate","GoogleAnalytics"];
			Throw[$Failed]
		)];
		sdate = DateObject[("StartDate" /. newparams)];
		If[MatchQ[sdate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"StartDate","GoogleAnalytics"];
			Throw[$Failed]
		)];
        params =Append[params, Rule["start-date",DateString[sdate, {"Year", "-", "Month", "-", "Day"}]]]     
	)];
	If[ KeyExistsQ[newparams,"EndDate"],
	(
		If[!(StringQ["EndDate"/.newparams]||MatchQ["EndDate"/.newparams,DateObject[__]]),
		(	
			Message[ServiceExecute::nval,"EndDate","GoogleAnalytics"];
			Throw[$Failed]
		)];
		edate = DateObject[("EndDate" /. newparams)];
		If[MatchQ[edate,DateObject[__String]],
		(	
			Message[ServiceExecute::nval,"EndDate","GoogleAnalytics"];
			Throw[$Failed]
		)];
        params =Append[params, Rule["end-date",DateString[edate, {"Year", "-", "Month", "-", "Day"}]]]             
	)];
	If[KeyExistsQ[newparams,"Metrics"],
	(
		metrics="Metrics" /. newparams;
		If[!(MatchQ[metrics, {__String}]||StringQ[metrics]),
		(	
			Message[ServiceExecute::nval,"Metrics","GoogleAnalytics"];
			Throw[$Failed]
		)];
		If[MatchQ[metrics,{__String}],
			params = Append[params,Rule["metrics",StringJoin[Riffle[deCamelizeDM/@metrics, ","]]]]
		];
		If[StringQ[metrics],	
			params = Append[params,Rule["metrics",deCamelizeDM[metrics]]]
		];
	)];
	If[KeyExistsQ[newparams,"Dimensions"],
	(
		dimensions="Dimensions" /. newparams;
		If[!(MatchQ[dimensions, {__String}]||StringQ[dimensions]),
		(	
			Message[ServiceExecute::nval,"Dimensions","GoogleAnalytics"];
			Throw[$Failed]
		)];
		If[MatchQ[dimensions,{__String}],
			params = Append[params,Rule["dimensions",StringJoin[Riffle[deCamelizeDM/@dimensions, ","]]]]
		];
		If[StringQ[dimensions],	
			params = Append[params,Rule["dimensions",deCamelizeDM[dimensions]]]
		];
	)];
	If[KeyExistsQ[newparams,"Sort"],
	(
		sort="Sort" /. newparams;
		If[!(MatchQ[sort, {__String}]||StringQ[sort]),
		(	
			Message[ServiceExecute::nval,"Sort","GoogleAnalytics"];
			Throw[$Failed]
		)];
		If[MatchQ[sort,{__String}],
			params = Append[params,Rule["sort",StringJoin[Riffle[deCamelizeDM/@sort, ","]]]]
		];
		If[StringQ[sort],	
			params = Append[params,Rule["sort",deCamelizeDM[sort]]]
		];
	)];
	If[KeyExistsQ[newparams,"Filters"],
	(
		
		params = Append[params,Rule["filters",GAFiltersParse["Filters" /. newparams]]]
	)];
	If[KeyExistsQ[newparams,"Segment"],
	(
		If[!(IntegerQ["Segment"/.newparams]),
		(	
			Message[ServiceExecute::nval,"Segment","GoogleAnalytics"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["segment",StringJoin["gaid::",ToString["Segment" /. newparams]]]]
	)];
	If[ KeyExistsQ[newparams,"SamplingLevel"],
  	(
		If[!StringMatchQ[ToString["SamplingLevel" /. newparams], "Default" | "Faster" | "HigherPrecision"],
		(	
			Message[ServiceExecute::nval,"SamplingLevel","GoogleAnalytics"];
			Throw[$Failed]
		)];  	
		params = Append[params,Rule["samplingLevel",("SamplingLevel" /. newparams)/.{"Default"->"DEFAULT","Faster"->"FASTER","HigherPrecision"->"HIGHER_PRECISION"}]];
	)];
	If[KeyExistsQ[newparams,"MaxItems"],
	(
		If[!(IntegerQ["MaxItems"/.newparams]&&("MaxItems"/.newparams)>0&&("MaxItems"/.newparams)<=10000),
		(	
			Message[ServiceExecute::nval,"MaxItems","GoogleAnalytics"];
			Throw[$Failed]
		)];
		maxitems="MaxItems"/.newparams;
		params = Append[params,Rule["max-results",ToString[maxitems]]]
	)];
	If[KeyExistsQ[newparams,"StartIndex"],
	(
		If[!(IntegerQ["StartIndex"/.newparams]&&("StartIndex"/.newparams)>0),
		(	
			Message[ServiceExecute::nval,"StartIndex","GoogleAnalytics"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["start-index",ToString[(("StartIndex"/.newparams)-1)*maxitems + 1]]]
	)];
	If[KeyExistsQ[newparams,"UserIP"],
	(
		If[!StringQ["UserIP"/.newparams],
		(	
			Message[ServiceExecute::nval,"UserIP","GoogleAnalytics"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["userIp","UserIP" /. newparams]]
	)];
	If[KeyExistsQ[newparams,"QuotaUser"],
	(
		If[!StringQ["QuotaUser"/.newparams],
		(	
			Message[ServiceExecute::nval,"QuotaUser","GoogleAnalytics"];
			Throw[$Failed]
		)];
		params = Append[params,Rule["quotaUser","QuotaUser" /. newparams]]
	)];
	
	Switch[req,
		"ReportData",
		(
			rawdata = FixedPoint[Normal,ServiceExecute["GoogleAnalytics","RawData",Join[params,{"fields" -> "rows,columnHeaders/name","output"->"json"}]]];
			If[KeyExistsQ[rawdata,"error"],
  		 	(
   		   		Message[ServiceExecute::serrormsg,("message" /. ("error" /. rawdata))];
   		    	Throw[$Failed]
 			)];
			columns="name" /. ("columnHeaders" /. rawdata);
			If[!KeyExistsQ[rawdata,"rows"],
			(
				Dataset[{}]
			),
			(
				Dataset[Association @@@ (Reverse[Inner[Rule, ("rows" /. rawdata), camelizeDM/@columns, List], 3]/.
					{("Date"->a_):>("Date"->DateObject[a]),("Year"->a_):>("Year"->DateObject[a]),("YearMonth"->a_):>("YearMonth"->DateObject[StringInsert[a," ",5]]),
						("DateHour"->a_):>("DateHour"->DateObject[StringInsert[a," ",9]<>":00:00",TimeZone->0]),
						("Country"->a_String):>("Country"->If[MatchQ[b=Interpreter["Country"][a],Failure[__]],a,b]),
						("City"->a_String):>("City"->If[MatchQ[b=Interpreter["City"][a],Failure[__]],a,b])
					})]
			)]	
		),
		"AllData",
		(
			If[KeyExistsQ[newparams,"Fields"],
			(
				params = Append[params,Rule["fields","Fields" /. newparams]]
			)];
			rawdata = FixedPoint[Normal,ServiceExecute["GoogleAnalytics","RawData",Join[params,{"output"->"json"}]]];
			If[KeyExistsQ[rawdata,"error"],
  		 	(
   		  	 	Message[ServiceExecute::serrormsg,("message" /. ("error" /. rawdata))];
   		    	Throw[$Failed]
 			)];
 			If[!KeyExistsQ[rawdata,"rows"],
			(
				rawdata = Append[rawdata,"rows"->{}]
			)];
			rawdata=Replace[rawdata, Rule[a_, b_] :> Rule[camelCase[a], b], Infinity]/.{
				"Id"->"ID","AccountId"->"AccountID","ProfileId"->"ProfileID","WebPropertyId"->"WebPropertyID","InternalWebPropertyId"->"InternalWebPropertyID",
				Rule["Dimensions",x_String]:>Rule["Dimensions",camelCase/@StringSplit[x,","]], Rule["Metrics",x_List]:>Rule["Metrics",camelCase/@x], Rule["Ids",x_String]:>Rule["IDs",camelCase/@StringSplit[x,","]],
				Rule["TableId",x_String]:>Rule["TableID",camelCase[x]], Rule["Name",x_String]:>Rule["Name",camelCase[x]], Rule[y:"StartDate"|"EndDate",x_String]:>Rule[y,DateObject[x]]
			};
			Dataset[Association @@ Replace[rawdata, r : {__Rule} :> Association[r], -1]]
		)
	]
]

googleanalyticscookeddata[req:"Metrics"|"Dimensions",id_, "Categories"]:=Switch[req,
	"Metrics",("Metrics" /. metdimlist)[[All, 1]],
	"Dimensions",("Dimensions" /. metdimlist)[[All, 1]]
]

googleanalyticscookeddata[req:"Metrics"|"Dimensions", id_,args_]:=Block[{rawdata,invalidParameters,category,newparams},
	newparams=args;
	invalidParameters = Select[Keys[newparams],!MemberQ[{"Category"},#]&]; 
	If[Length[invalidParameters]>0,
	(
		Message[ServiceObject::noget,#,"GoogleAnalytics"]&/@invalidParameters;
		Throw[$Failed]
	)];
	If[KeyExistsQ[newparams,"Category"],
	(
		If[!StringMatchQ[ToString["Category" /. newparams],  Alternatives @@ (Append[("Dimensions" /. metdimlist)[[All, 1]], "All"])],
		(	
			Message[ServiceExecute::nval,"Category","GoogleAnalytics"];
			Throw[$Failed]
		)];
		category = ToString["Category" /. newparams]
	),
	(
		category = "All"
	)];
	Switch[req,
		"Metrics", If[MatchQ[category,"All"],"Metrics"/.metdimlist,category/.("Metrics"/.metdimlist)],
		"Dimensions", If[MatchQ[category,"All"],"Dimensions"/.metdimlist,category/.("Dimensions"/.metdimlist)]
	]
]

metdimlist = {"Metrics" -> {"User" -> {"Users", "NewUsers", "PercentNewSessions", 
     "SessionsPerUser"}, 
   "Session" -> {"Sessions", "Bounces", "BounceRate", 
     "SessionDuration", "AvgSessionDuration", "Hits"}, 
   "Traffic Sources" -> "OrganicSearches", 
   "Adwords" -> {"Impressions", "AdClicks", "AdCost", "CPM", "CPC", 
     "CTR", "CostPerTransaction", "CostPerGoalConversion", 
     "CostPerConversion", "RPC", "ROAS"}, 
   "Goal Conversions" -> {"GoalXXStarts", "GoalStartsAll", 
     "GoalXXCompletions", "GoalCompletionsAll", "GoalXXValue", 
     "GoalValueAll", "GoalValuePerSession", "GoalXXConversionRate", 
     "GoalConversionRateAll", "GoalXXAbandons", "GoalAbandonsAll", 
     "GoalXXAbandonRate", "GoalAbandonRateAll"}, 
   "Platform or Device" -> Missing["NotAvailable"], 
   "Geo Network" -> Missing["NotAvailable"], 
   "System" -> Missing["NotAvailable"], 
   "Social Activities" -> "SocialActivities", 
   "Page Tracking" -> {"PageValue", "Entrances", "EntranceRate", 
     "Pageviews", "PageviewsPerSession", "UniquePageviews", 
     "TimeOnPage", "AvgTimeOnPage", "Exits", "ExitRate"}, 
   "Content Grouping" -> "ContentGroupUniqueViewsXX", 
   "Internal Search" -> {"SearchResultViews", "SearchUniques", 
     "AvgSearchResultViews", "SearchSessions", 
     "PercentSessionsWithSearch", "SearchDepth", "AvgSearchDepth", 
     "SearchRefinements", "PercentSearchRefinements", 
     "SearchDuration", "AvgSearchDuration", "SearchExits", 
     "SearchExitRate", "SearchGoalXXConversionRate", 
     "SearchGoalConversionRateAll", "GoalValueAllPerSearch"}, 
   "Site Speed" -> {"PageLoadTime", "PageLoadSample", 
     "AvgPageLoadTime", "DomainLookupTime", "AvgDomainLookupTime", 
     "PageDownloadTime", "AvgPageDownloadTime", "RedirectionTime", 
     "AvgRedirectionTime", "ServerConnectionTime", 
     "AvgServerConnectionTime", "ServerResponseTime", 
     "AvgServerResponseTime", "SpeedMetricsSample", 
     "DomInteractiveTime", "AvgDomInteractiveTime", 
     "DomContentLoadedTime", "AvgDomContentLoadedTime", 
     "DomLatencyMetricsSample"}, 
   "App Tracking" -> {"Screenviews", "UniqueScreenviews", 
     "ScreenviewsPerSession", "TimeOnScreen", 
     "AvgScreenviewDuration"}, 
   "Event Tracking" -> {"TotalEvents", "UniqueEvents", "EventValue", 
     "AvgEventValue", "SessionsWithEvent", 
     "EventsPerSessionWithEvent"}, 
   "Ecommerce" -> {"Transactions", "TransactionsPerSession", 
     "TransactionRevenue", "RevenuePerTransaction", 
     "TransactionRevenuePerSession", "TransactionShipping", 
     "TransactionTax", "TotalValue", "ItemQuantity", 
     "UniquePurchases", "RevenuePerItem", "ItemRevenue", 
     "ItemsPerPurchase", "LocalTransactionRevenue", 
     "LocalTransactionShipping", "LocalTransactionTax", 
     "LocalItemRevenue", "BuyToDetailRate", "CartToDetailRate", 
     "InternalPromotionCTR", "InternalPromotionClicks", 
     "InternalPromotionViews", "LocalProductRefundAmount", 
     "LocalRefundAmount", "ProductAddsToCart", "ProductCheckouts", 
     "ProductDetailViews", "ProductListCTR", "ProductListClicks", 
     "ProductListViews", "ProductRefundAmount", "ProductRefunds", 
     "ProductRemovesFromCart", "ProductRevenuePerPurchase", 
     "QuantityAddedToCart", "QuantityCheckedOut", "QuantityRefunded", 
     "QuantityRemovedFromCart", "RefundAmount", "RevenuePerUser", 
     "TotalRefunds", "TransactionsPerUser"}, 
   "Social Interactions" -> {"SocialInteractions", 
     "UniqueSocialInteractions", "SocialInteractionsPerSession"}, 
   "User Timings" -> {"UserTimingValue", "UserTimingSample", 
     "AvgUserTimingValue"}, 
   "Exceptions" -> {"Exceptions", "ExceptionsPerScreenview", 
     "FatalExceptions", "FatalExceptionsPerScreenview"}, 
   "Content Experiments" -> Missing["NotAvailable"], 
   "Custom Variables or Columns" -> "MetricXX", 
   "Time" -> Missing["NotAvailable"], 
   "DoubleClick Campaign Manager" -> {"DcmFloodlightQuantity", 
     "DcmFloodlightRevenue", "DcmCPC", "DcmCTR", "DcmClicks", 
     "DcmCost", "DcmImpressions", "DcmROAS", "DcmRPC"}, 
   "Audience" -> Missing["NotAvailable"], 
   "Adsense" -> {"AdsenseRevenue", "AdsenseAdUnitsViewed", 
     "AdsenseAdsViewed", "AdsenseAdsClicks", "AdsensePageImpressions",
      "AdsenseCTR", "AdsenseECPM", "AdsenseExits", 
     "AdsenseViewableImpressionPercent", "AdsenseCoverage"}, 
   "Ad Exchange" -> {"AdxImpressions", "AdxCoverage", 
     "AdxMonetizedPageviews", "AdxImpressionsPerSession", 
     "AdxViewableImpressionsPercent", "AdxClicks", "AdxCTR", 
     "AdxRevenue", "AdxRevenuePer1000Sessions", "AdxECPM"}, 
   "Channel Grouping" -> Missing["NotAvailable"], 
   "Related Products" -> {"CorrelationScore", "QueryProductQuantity", 
     "RelatedProductQuantity"}}, 
 "Dimensions" -> {"User" -> {"UserType", "SessionCount", 
     "DaysSinceLastSession", "UserDefinedValue"}, 
   "Session" -> "SessionDurationBucket", 
   "Traffic Sources" -> {"ReferralPath", "FullReferrer", "Campaign", 
     "Source", "Medium", "SourceMedium", "Keyword", "AdContent", 
     "SocialNetwork", "HasSocialSourceReferral", "CampaignCode"}, 
   "Adwords" -> {"AdGroup", "AdSlot", "AdDistributionNetwork", 
     "AdMatchType", "AdKeywordMatchType", "AdMatchedQuery", 
     "AdPlacementDomain", "AdPlacementUrl", "AdFormat", 
     "AdTargetingType", "AdTargetingOption", "AdDisplayUrl", 
     "AdDestinationUrl", "AdwordsCustomerID", "AdwordsCampaignID", 
     "AdwordsAdGroupID", "AdwordsCreativeID", "AdwordsCriteriaID", 
     "AdQueryWordCount", "IsTrueViewVideoAd"}, 
   "Goal Conversions" -> {"GoalCompletionLocation", 
     "GoalPreviousStep1", "GoalPreviousStep2", "GoalPreviousStep3"}, 
   "Platform or Device" -> {"Browser", "BrowserVersion", 
     "OperatingSystem", "OperatingSystemVersion", 
     "MobileDeviceBranding", "MobileDeviceModel", 
     "MobileInputSelector", "MobileDeviceInfo", 
     "MobileDeviceMarketingName", "DeviceCategory", "DataSource"}, 
   "Geo Network" -> {"Continent", "SubContinent", "Country", "Region",
      "Metro", "City", "Latitude", "Longitude", "NetworkDomain", 
     "NetworkLocation", "CityId", "CountryIsoCode", "RegionId", 
     "RegionIsoCode", "SubContinentCode"}, 
   "System" -> {"FlashVersion", "JavaEnabled", "Language", 
     "ScreenColors", "SourcePropertyDisplayName", 
     "SourcePropertyTrackingId", "ScreenResolution"}, 
   "Social Activities" -> {"SocialActivityEndorsingUrl", 
     "SocialActivityDisplayName", "SocialActivityPost", 
     "SocialActivityTimestamp", "SocialActivityUserHandle", 
     "SocialActivityUserPhotoUrl", "SocialActivityUserProfileUrl", 
     "SocialActivityContentUrl", "SocialActivityTagsSummary", 
     "SocialActivityAction", "SocialActivityNetworkAction"}, 
   "Page Tracking" -> {"Hostname", "PagePath", "PagePathLevel1", 
     "PagePathLevel2", "PagePathLevel3", "PagePathLevel4", 
     "PageTitle", "LandingPagePath", "SecondPagePath", "ExitPagePath",
      "PreviousPagePath", "PageDepth"}, 
   "Content Grouping" -> {"LandingContentGroupXX", 
     "PreviousContentGroupXX", "ContentGroupXX"}, 
   "Internal Search" -> {"SearchUsed", "SearchKeyword", 
     "SearchKeywordRefinement", "SearchCategory", "SearchStartPage", 
     "SearchDestinationPage", "SearchAfterDestinationPage"}, 
   "Site Speed" -> Missing["NotAvailable"], 
   "App Tracking" -> {"AppInstallerId", "AppVersion", "AppName", 
     "AppId", "ScreenName", "ScreenDepth", "LandingScreenName", 
     "ExitScreenName"}, 
   "Event Tracking" -> {"EventCategory", "EventAction", "EventLabel"},
    "Ecommerce" -> {"TransactionId", "Affiliation", 
     "SessionsToTransaction", "DaysToTransaction", "ProductSku", 
     "ProductName", "ProductCategory", "CurrencyCode", 
     "CheckoutOptions", "InternalPromotionCreative", 
     "InternalPromotionId", "InternalPromotionName", 
     "InternalPromotionPosition", "OrderCouponCode", "ProductBrand", 
     "ProductCategoryHierarchy", "ProductCategoryLevelXX", 
     "ProductCouponCode", "ProductListName", "ProductListPosition", 
     "ProductVariant", "ShoppingStage"}, 
   "Social Interactions" -> {"SocialInteractionNetwork", 
     "SocialInteractionAction", "SocialInteractionNetworkAction", 
     "SocialInteractionTarget", "SocialEngagementType"}, 
   "User Timings" -> {"UserTimingCategory", "UserTimingLabel", 
     "UserTimingVariable"}, "Exceptions" -> "ExceptionDescription", 
   "Content Experiments" -> {"ExperimentId", "ExperimentVariant"}, 
   "Custom Variables or Columns" -> {"DimensionXX", "CustomVarNameXX",
      "CustomVarValueXX"}, 
   "Time" -> {"Date", "Year", "Month", "Week", "Day", "Hour", 
     "Minute", "NthMonth", "NthWeek", "NthDay", "NthMinute", 
     "DayOfWeek", "DayOfWeekName", "DateHour", "YearMonth", 
     "YearWeek", "IsoWeek", "IsoYear", "IsoYearIsoWeek", "NthHour"}, 
   "DoubleClick Campaign Manager" -> {"DcmClickAd", "DcmClickAdId", 
     "DcmClickAdType", "DcmClickAdTypeId", "DcmClickAdvertiser", 
     "DcmClickAdvertiserId", "DcmClickCampaign", "DcmClickCampaignId",
      "DcmClickCreativeId", "DcmClickCreative", "DcmClickRenderingId",
      "DcmClickCreativeType", "DcmClickCreativeTypeId", 
     "DcmClickCreativeVersion", "DcmClickSite", "DcmClickSiteId", 
     "DcmClickSitePlacement", "DcmClickSitePlacementId", 
     "DcmClickSpotId", "DcmFloodlightActivity", 
     "DcmFloodlightActivityAndGroup", "DcmFloodlightActivityGroup", 
     "DcmFloodlightActivityGroupId", "DcmFloodlightActivityId", 
     "DcmFloodlightAdvertiserId", "DcmFloodlightSpotId", 
     "DcmLastEventAd", "DcmLastEventAdId", "DcmLastEventAdType", 
     "DcmLastEventAdTypeId", "DcmLastEventAdvertiser", 
     "DcmLastEventAdvertiserId", "DcmLastEventAttributionType", 
     "DcmLastEventCampaign", "DcmLastEventCampaignId", 
     "DcmLastEventCreativeId", "DcmLastEventCreative", 
     "DcmLastEventRenderingId", "DcmLastEventCreativeType", 
     "DcmLastEventCreativeTypeId", "DcmLastEventCreativeVersion", 
     "DcmLastEventSite", "DcmLastEventSiteId", 
     "DcmLastEventSitePlacement", "DcmLastEventSitePlacementId", 
     "DcmLastEventSpotId"}, 
   "Audience" -> {"UserAgeBracket", "UserGender", 
     "InterestOtherCategory", "InterestAffinityCategory", 
     "InterestInMarketCategory"}, 
   "Adsense" -> Missing["NotAvailable"], 
   "Ad Exchange" -> Missing["NotAvailable"], 
   "Channel Grouping" -> "ChannelGrouping", 
   "Related Products" -> {"CorrelationModelId", "QueryProductId", 
     "QueryProductName", "QueryProductVariation", "RelatedProductId", 
     "RelatedProductName", "RelatedProductVariation"}}}

getpagetoken[json_]:=With[
	{tokens=StringCases[json, "\"nextPageToken\": \"" ~~ (t : Shortest[__]) ~~ "\"" :> t]},
	If[Length[tokens]===1,First[tokens],$Failed]
]

(*** Utilities ***)

camelizeDM[text_] := StringReplace[text, StartOfString ~~ "ga:" ~~ a_ ~~ b___ :> ToUpperCase[a] ~~ b]

deCamelizeDM[text_] := StringReplace[text, {StartOfString ~~ "-" ~~ a_ ~~ b___ :> "-ga:" ~~ ToLowerCase[a] ~~ b, StartOfString ~~ a_ ~~ b___ :> "ga:" ~~ ToLowerCase[a] ~~ b}]

camelCase[text_] := Module[{split, partial}, (
    split = StringSplit[text, {" ","_","-","ga:"}];
    partial = Prepend[Rest[Characters[#]], ToUpperCase[Characters[#][[1]]]] & /@ split;
    StringJoin[partial]
    )]

GAFiltersParse[e_] :=
 e //. {Verbatim[Alternatives][x_] :> x, Verbatim[Alternatives][x_, y__] :> "" ~~ x ~~ "," ~~ Alternatives[y] ~~ "",
   Verbatim[Rule][x_, Verbatim[RegularExpression][y_]] :> "" ~~ deCamelizeDM[ToString[x]] ~~ "=~" ~~ ToString[y] ~~ "",
   Verbatim[Rule][x_, Verbatim[Except][Verbatim[RegularExpression][y_]]] :> "" ~~ deCamelizeDM[ToString[x]] ~~ "!~" ~~ ToString[y] ~~ "",
   Verbatim[Rule][x_, Verbatim[Except][y_]] :> "" ~~ deCamelizeDM[ToString[x]] ~~ "!=" ~~ ToString[y] ~~ "",
   Verbatim[Rule][x_, y_] :> "" ~~ deCamelizeDM[ToString[x]] ~~ "==" ~~ ToString[y] ~~ "",
   Verbatim[NotSuperset][x_, y_] :> "" ~~ deCamelizeDM[ToString[x]] ~~ "!@" ~~ ToString[y] ~~ "",
   Verbatim[Superset][x_, y_] :> "" ~~ deCamelizeDM[ToString[x]] ~~ "=@" ~~ ToString[y] ~~ "",
   Verbatim[Greater][x_, y_] :> "" ~~ deCamelizeDM[ToString[x]] ~~ ">" ~~ ToString[y] ~~ "",
   Verbatim[GreaterEqual][x_, y_] :> "" ~~ deCamelizeDM[ToString[x]] ~~ ">=" ~~ ToString[y] ~~ "",
   Verbatim[Less][x_, y_] :> "" ~~ deCamelizeDM[ToString[x]] ~~ "<" ~~ ToString[y] ~~ "",
   Verbatim[LessEqual][x_, y_] :> "" ~~ deCamelizeDM[ToString[x]] ~~ "<=" ~~ ToString[y] ~~ "",
   List[x_] :> x, List[x_, y__] :> "" ~~ x ~~ ";" ~~ List[y] ~~ ""
   }
       
filterparameters=OAuthClient`Private`filterParameters;
camelcase=OAuthClient`Private`camelCase;
fp=OAuthClient`Private`formatpath;

fixlimit=HoldPattern[Rule["maxResults",l_]]:>Rule["maxResults",ToString[l]];

parseactivity[act_]:=Association@Replace[FilterRules[Replace[act
		,{(Rule[a_,b_]):>(Rule[camelcase[a],b])},Infinity],{"Actor","URL","Updated","Object","Published","ID"}]/.{
			OAuthClient`Private`formatvalue["Updated"->(readDate[#]&)],OAuthClient`Private`formatvalue["Published"->(readDate[#]&)]},"ID"->"ActivityID",{2}]
			
parseuser[user_]:=Association@Replace[FilterRules[Replace[user,
	{(Rule[a_,b_]):>(Rule[camelcase[a],b])},Infinity],{"DisplayName","ID"}]/.{OAuthClient`Private`formatvalue["Published"->(readDate[#]&)]},"ID"->"UserID",{2}]
			

tostring[str_String,_]:=str
tostring[default_]:=default
tostring[Automatic,default_]:=default
tostring[str_,_]:=ToString[str]

readDate[date_,form_:DateObject]:=form[DateList[date]]

getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.googleanalyticsdata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

(*
fromHTML[str_]:=StringReplace[str, ("&#" ~~ (char : (WordCharacter ..) /; 
      StringLength[char] < 5) ~~ ";") :> (FromCharacterCode@
    FromDigits[char])]
    *)

fromHTML[str_String]:=ImportString[str,"HTML"]   
fromHTML[l:{___String}]:=ImportString[#,"HTML"]  &/@l  
fromHTML[___]:={}
    

End[] (* End Private Context *)
           		
End[]


SetAttributes[{},{ReadProtected, Protected}];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{GoogleAnalyticsOAuth`Private`googleanalyticsdata,GoogleAnalyticsOAuth`Private`googleanalyticscookeddata,GoogleAnalyticsOAuth`Private`googleanalyticssendmessage}
