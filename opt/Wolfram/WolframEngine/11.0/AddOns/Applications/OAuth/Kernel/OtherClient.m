
(* ::Package:: *)

(* $Id$ *)

(* :Summary:
	A free-form framework for connecting to services
*)

(* :Mathematica Version: Mathematica 11.0 *)

(* :Keywords: 

*)

(* :Examples:
*)

OtherClient`rawotherdata;
OtherClient`otherauthenticate;
OtherClient`otherdisconnect;
OtherClient`otherdata;

(Unprotect[#]; Clear[#])& /@ {
 OtherClient`rawotherdata,
 OtherClient`otherauthenticate,
 OtherClient`otherdisconnect,
 OtherClient`otherdata
}

Begin["OtherClient`"];

Begin["`Private`"];

otherservicesdata=OtherClient`OtherServicesData;

(* Because the many of the services update thier data frequently (i.e. Twitter) caching is false by default.
	In some places where calls are often repeated, this is set to true *)
OtherClient`$CacheResults=False;

(* Messages *)

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

(************************************** Other Authentication **********************************)
OtherClient`otherauthenticate[name_, opts_:{}, "New"]:=(Message[ServiceConnect::key2, name];OtherClient`otherauthenticate[name,opts])

OtherClient`otherauthenticate[name_, opts_:{}, conn_String]:=(Message[ServiceConnect::key1, name];OtherClient`otherauthenticate[name,opts])

OtherClient`otherauthenticate[name_, opts_]:=newotherauthenticate[name, opts]
	
newotherauthenticate[name_,rest_]:=Module[{service, 
	rawgets=otherservicesdata[name, "RawGets"], 
	gets=otherservicesdata[name, "Gets"],
	rawposts=otherservicesdata[name, "RawPosts"], 
	posts=otherservicesdata[name, "Posts"], id},

	service=newunknownotherauthenticate[name, rest];
	id=getServiceID[service];

	serviceRawRequests[id]=sortrequests[serviceRawRequests[id],rawgets];
	serviceRawPosts[id]=sortrequests[serviceRawPosts[id],rawposts];
	serviceRequests[id]=sortrequests[serviceRequests[id],gets];
	servicePosts[id]=sortrequests[servicePosts[id],posts];

	service	
]/;MemberQ[OtherClient`$predefinedOtherservicelist,name]

sortrequests[l1_,l2_]:=Sort[Select[Flatten[{l1,l2}],StringQ]]

newotherauthenticate[name_,rest_]:=newunknownotherauthenticate[name, rest]

newunknownotherauthenticate[name_,opts_]:=ServiceConnections`Private`createServiceObject["Other",name,None]

newunknownotherauthenticate[___]:=Throw[$Failed]

(***************************** Exchanging data ***********************************)

OtherClient`otherdata[service_ServiceObject,property_,rest_]:=Module[{id=getServiceID[service]},
	If[MemberQ[Join[serviceRequests[id],servicePosts[id]],property],
		OtherClient`othercookeddata[getServiceName[service],property,rest]
		,
		If[MemberQ[Join[serviceRawRequests[id],serviceRawPosts[id]],property],
			OtherClient`rawotherdata[id,property,rest]
			,
			$Failed
		]
	]
]

OtherClient`otherdata[args___]:=$Failed

OtherClient`rawotherdata[id_,property_,rest___]:=Module[
		{res},	
		If[OtherClient`$CacheResults,
			res = Internal`CheckCache[{"OAuth", {id, property, rest}}];
			If[res =!= $Failed, Return[res]]
		];
			res=OtherClient`otherrawdata[serviceName[id],property,rest];
			(
				If[OtherClient`$CacheResults,Internal`SetCache[{"OAuth", {id, property, rest}}, res]];
				res
			) /; (res =!= $Failed)
	]/;MemberQ[Join[serviceRawRequests[id],serviceRawPosts[id]], property]

OtherClient`rawotherdata[___]:=Throw[$Failed]

(************************************** ServiceDisconnect *****************************)
OtherClient`otherdisconnect[service_ServiceObject]:=Module[
	{id=getServiceID[service]},
	
	serviceName[id]=None;
	serviceRawRequests[id]={};
	serviceRequests[id]={};
	serviceRawPosts[id]={};
	servicePosts[id]={};
	serviceAuthentication[id]={};

	ServiceConnections`Private`$authenticatedservices=DeleteCases[ServiceConnections`Private`$authenticatedservices,id];	
]

OtherClient`otherdisconnect[___]:=$Failed

End[];
End[];