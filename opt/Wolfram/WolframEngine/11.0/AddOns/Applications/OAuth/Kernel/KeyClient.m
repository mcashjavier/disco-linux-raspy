
(* ::Package:: *)

(* $Id$ *)

(* :Summary:
	A framework for authenticating and exchanging data with api-key authenticated services
*)

(* :Mathematica Version: Mathematica 11.0 *)

(* :Keywords:
API Key
*)

(* :Examples:
*)
KeyClient`rawkeydata;
KeyClient`keyauthenticate;
KeyClient`keydisconnect;
KeyClient`keydata;

(Unprotect[#]; Clear[#])& /@ {
 KeyClient`rawkeydata,
 KeyClient`keyauthenticate,
 KeyClient`keydisconnect,
 KeyClient`keydata
}

Begin["KeyClient`"];

Begin["`Private`"];

keyservicesdata=KeyClient`KeyServicesData;

(* Use the cloud stored api keys *)
$KeyCloudCredentialsQ=False;

(* Because many of the services update their data frequently (i.e. Twitter) caching is false by default.
	In some places where calls are often repeated, this is set to true *)
KeyClient`$CacheResults=False;

(* Messages *)
ServiceConnect::key1="The `1` service does not save user connections, a standard connection will be created."
ServiceConnect::key2="The `1` service does not create new user connections, a standard connection will be created."

(* Import Functions *)
serviceName=ServiceConnections`Private`serviceName;
getServiceObject=ServiceConnections`Private`getServiceObject;
checkservicelist=ServiceConnections`Private`checkservicelist;
getServiceID=ServiceConnections`Private`getServiceID;
getServiceName=ServiceConnections`Private`getServiceName;
serviceRawRequests=ServiceConnections`Private`serviceRawRequests;
serviceRawPosts=ServiceConnections`Private`serviceRawPosts;
serviceRequests=ServiceConnections`Private`serviceRequests;
servicePosts=ServiceConnections`Private`servicePosts;
logout=ServiceConnections`Private`logout;
urlfetchFun=ServiceConnections`Private`urlfetchFun;
serviceInfo=ServiceConnections`Private`serviceInfo;
debugPrint=ServiceConnections`Private`debugPrint;
serviceAuthentication=ServiceConnections`Private`serviceAuthentication;

(************************************** API Key Authentication **********************************)
KeyClient`keyauthenticate[name_, opts_:{}, "New"]:=(Message[ServiceConnect::key2, name];KeyClient`keyauthenticate[name,opts])

KeyClient`keyauthenticate[name_, opts_:{}, conn_String]:=(Message[ServiceConnect::key1, name];KeyClient`keyauthenticate[name,opts])

KeyClient`keyauthenticate[name_, opts_]:=newkeyauthenticate[name, opts]

newkeyauthenticate[name_,rest_]:=Module[{service,
	rawgets=keyservicesdata[name, "RawGets"],
	gets=keyservicesdata[name, "Gets"],
	rawposts=keyservicesdata[name, "RawPosts"],
	posts=keyservicesdata[name, "Posts"], id,info,key},
	key=OAuthClient`Private`getclientinfo[name,"APIKey"];
	info=Join[{"APIKey"->key},keyservicesdata[name, "Authentication"]];

	service=newunknownkeyauthenticate[name, info, rest];
	id=getServiceID[service];

	serviceRawRequests[id]=sortrequests[serviceRawRequests[id],rawgets];
	serviceRawPosts[id]=sortrequests[serviceRawPosts[id],rawposts];
	serviceRequests[id]=sortrequests[serviceRequests[id],gets];
	servicePosts[id]=sortrequests[servicePosts[id],posts];

	service
]/;MemberQ[KeyClient`$predefinedKeyservicelist,name]

sortrequests[l1_,l2_]:=Sort[Select[Flatten[{l1,l2}],StringQ]]

newunknownkeyauthenticate[name_,opts___]:=Module[{apikey, params = Normal[Association[opts]],urlfetchfun,
	service, id},

	apikey = Lookup[params, "APIKey", ""];
	urlfetchfun = Lookup[params, "URLFetchFun", URLFetch];
	apikey=getapikey[name,apikey];

	If[ListQ[apikey] || StringQ[apikey],
    (
		service=ServiceConnections`Private`createServiceObject["APIKey",name,apikey];

		id=getServiceID[service];
		urlfetchFun[id]=urlfetchfun;
		service
	),
	(
    	Message[ServiceConnect::genconerr, name];
        Throw[$Failed]
    )]
]

newunknownkeyauthenticate[___]:=Throw[$Failed]

(***************************** Exchanging data ***********************************)

KeyClient`keydata[service_ServiceObject,property_,rest___]:=Module[{raw, id=getServiceID[service]},
	If[MemberQ[Join[serviceRequests[id],servicePosts[id]],property],
		KeyClient`keycookeddata[getServiceName[service],property,id,rest]
		,
		If[MemberQ[Join[serviceRawRequests[id],serviceRawPosts[id]],property],
			raw = KeyClient`rawkeydata[id,property,rest];
			parsedata[id,property]@raw
			,
			$Failed
		]
	]
]

KeyClient`keydata[args___]:=$Failed

KeyClient`rawkeydata[id_,parameter_,rest_]:=KeyClient`rawkeydata[id,parameter,{rest}]/;!ListQ[rest]

KeyClient`rawkeydata[id_,url0_String]:=Module[{url, res},
		url=addapikey[url0,serviceAuthentication[id]];
		If[url === $Failed, Throw[$Failed]];
        (
     		res = urlfetchFun[id]@@url;
     		res /; (res =!= $Failed)
        ) /; (url =!= $Failed)
	]/;!MemberQ[ServiceConnections`Private`availablequeries[id],url0]


KeyClient`rawkeydata[id_,property_,rest___]:=Module[
		{url0,method,pathparams,params,bodyparams,mpdata,headers,reqparams,
			url, res, key, pvpairs=Flatten[{rest}], params1, bodyparams1,mpdata1,headers1, useauth,returncontentdata,querydata},
		If[KeyClient`$CacheResults,
			res = Internal`CheckCache[{"OAuth", {id, property, rest}}];
			If[res =!= $Failed, Return[res]];
		];

		querydata=ServiceConnections`Private`getQueryData[id, property];
		{url0,method,pathparams,params,bodyparams,mpdata,headers,reqparams,returncontentdata,useauth}=Drop[querydata,{-3}];

		(* check for required parameters *)
		If[!MemberQ[First/@pvpairs,#],
			Message[ServiceExecute::nparam,#];Throw[$Failed]
		]&/@reqparams;
		(* check for valid parameters*)
		If[!MemberQ[Join[pathparams,params,Values[bodyparams],First/@mpdata],#],
			Message[ServiceExecute::noget,#,serviceName[id]];Throw[$Failed]
		]&/@(First/@pvpairs);

		(* Path Parameters use a StringForm Function *)
		key=serviceAuthentication[id]; (* credentials as parameters *)
		url=If[Head[url0]===Function,
			ServiceConnections`Private`insertpathparameters[url0,pathparams,Join[pvpairs,key]],
			url0
		];

		params1=FilterRules[pvpairs,params];
		If[KeyExistsQ[bodyparams,"ParameterlessBodyData"],
		bodyparams1 = Lookup[pvpairs,Lookup[bodyparams,"ParameterlessBodyData"],""],
		bodyparams1 = ""]; (*BodyData only uses ParameterlessBodyData*)
		mpdata1=Append[List @@ #, Lookup[pvpairs, First[#]]] & /@ FilterRules[(Rule @@ #) & /@ mpdata, Keys[pvpairs]];
		url={url,
		"Parameters"->params1,
		"BodyData"->bodyparams1,
		"MultipartData"->mpdata1,
		"Method"->method};

		If[MatchQ[useauth,True|"APIKey"],
			url=addapikey[url, key]
		];

		If[!MatchQ[url,_String|{_String,___}],Throw[$Failed]];

		If[headers=!={},
			(* Headers should have default values, check for given values *)
			headers1=If[!KeyExistsQ[pvpairs,First[#]],#,First[#]->Lookup[pvpairs,First[#]]]&/@headers;
			url=Join[url,{"Headers"->headers1}]
		];
		If[url === $Canceled, Return[$Canceled]];
		(
     		res=urlfetchFun[id]@@url;
			(
				If[KeyClient`$CacheResults,Internal`SetCache[{"OAuth", {id, property, rest}}, res]];
				res
			) /; (res =!= $Failed)
		) /; (url =!= $Failed)
	]/;MemberQ[Join[serviceRawRequests[id],serviceRawPosts[id]], property]


KeyClient`rawkeydata[___]:=Throw[$Failed]

parsedata[id_,property_]:=(("ResultsFunction"/.keyservicesdata[serviceName[id],property])/."ResultsFunction"->Identity
	)/;MemberQ[Join[serviceRawRequests[id],serviceRawPosts[id]], property]

parsedata[__]:=Identity

(************************************** ServiceDisconnect *****************************)
KeyClient`keydisconnect[service_ServiceObject]:=Module[
	{id=getServiceID[service]},

	serviceName[id]=None;
	serviceRawRequests[id]={};
	serviceRequests[id]={};
	serviceRawPosts[id]={};
	servicePosts[id]={};
	serviceAuthentication[id]={};
	urlfetchFun[id]=URLFetch;

	ServiceConnections`Private`$authenticatedservices=DeleteCases[ServiceConnections`Private`$authenticatedservices,id];
]

KeyClient`keydisconnect[___]:=$Failed

(************************ Utilities *********************************************)
addapikey[url_String,key_]:=URLBuild[url,{"apikey"->key}]
addapikey[url_List,key_String]:=With[{params=Lookup[Rest[url],"Parameters",{}]},
	{First[url],"Parameters"->Join[{"apikey"->key},FilterRules[params,Except["apikey"]]],
		Sequence@@FilterRules[Rest[url],Except["Parameters"]]}
]

(* new function to handle multiple keys dialog *)
addapikey[url_List,fields_List]:=With[{params=Lookup[Rest[url],"Parameters",{}]},
	{First[url],"Parameters"->Join[fields,FilterRules[params,Except[fields[[All,1]]]]],
		Sequence@@FilterRules[Rest[url],Except["Parameters"]]}
]

(*********************** Cloud Stored Client credentials *************************)
cloudapikeBaseURL="https://www.wolframcloud.com/objects/user-00e58bd3-2dfd-45b3-b80b-d281d360703a/apikey";

cloudgetapikey[name_]:=Block[{url, key},
	url=URLBuild[cloudapikeBaseURL,{"ServiceName"->name}];
	key=URLFetch[url];
	url=ToExpression[key];
	If[!StringQ[url],$Failed,url]
]

getapikey[name_,apikey_]:=cloudgetapikey[name]/;$KeyCloudCredentialsQ&&MemberQ[KeyClient`$predefinedKeyservicelist,name]
getapikey[_,apikey_]:=apikey

End[];
End[];