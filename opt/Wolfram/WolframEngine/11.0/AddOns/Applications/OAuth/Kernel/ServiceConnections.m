
(* ::Package:: *)

(* $Id$ *)

(* :Summary:
	A framework for authenticating and exchanging data with API services
*)

(* :Mathematica Version: Mathematica 10.0 *)

(* :Keywords:
OAuth
*)

(* :Examples:
*)

System`ServiceConnect;
System`ServiceExecute;
System`ServiceDisconnect;
System`SendMessage;
System`ServiceObject;
System`$Services;


(Unprotect[#]; Clear[#])& /@ {
  ServiceConnect,
  ServiceDisconnect,
  ServiceExecute,
  SendMessage,
  ServiceObject,
  ServiceConnections`ServiceInformation
}
Unprotect[$Services];

Needs["OAuthSigning`"];
Needs["CloudObject`"];

Begin["ServiceConnections`"];

ServiceConnections`ServiceInformation;

Begin["`Private`"];

$debug=False;
debugPrint[args__]:=Print[args/.{("ConsumerSecret"->_):>("ConsumerSecret"->"xxxx")}]/;$debug

$authenticatedservices={};
$savedservices={};

defaultParams[___]:={};
$protectedopts={"AccessEndpoint","AccessTokenExtractor","AccessTokenRequestor","AccessVerb","AdditionalOAuthParameter","APIKey","AuthenticationDialog","AuthorizationFunction",
	"AuthorizeEndpoint", "Blocking","CodeExtractor","ConsumerKey","ConsumerSecret","Information","LogoutURL","OAuthVersion","RedirectURI","RefreshAccessTokenFunction",
	"RequestEndpoint","RequestFormat","RequestTokenExtractor","RequestVerb","ResponseType","SaveServiceConnection","Scope","ScopeDomain","ScopeParameter","ServiceName",
	"SignatureMethod","tokenread","URLFetchFun","URLSignService","VerifierLabel"};
$ServiceConnectChannel=OAuthClient`Private`$ServiceConnectChannel;
$oauthservices = OAuthClient`$predefinedOAuthservicelist;
$keyservices = KeyClient`$predefinedKeyservicelist;
$otherservices = OtherClient`$predefinedOtherservicelist;
$predefinedservices:=Flatten[{$oauthservices,$keyservices,$otherservices,localpacletServices[]}/.{
		HoldPattern[$oauthservices]->{},HoldPattern[$keyservices]->{},HoldPattern[$otherservices]->{}}];

(* Messages *)
ServiceConnect::done="The service `1` has already been authenticated";
ServiceConnect::oauthver="The OAuth version should be 1.0 or 2.0";
ServiceConnect::url="The given url `1` is not a valid string";
ServiceConnect::skey="The key or secret `1` must be a valid string";
ServiceConnect::dialog="The value `1` is not valid for AuthenticationDialog";
ServiceConnect::reqform="The value `1` is not valid for RequestFormat";
ServiceConnect::reffun="The value `1` is not valid for RefreshAccessTokenFunction";
ServiceConnect::token="Could not obtain a request token for the OAuth service `1`";
ServiceConnect::apikey="Could not obtain credentials for the service `1`";
ServiceConnect::genconerr="Could not connect to the service `1`";
ServiceExecute::nolink="The service `1` is not authenticated. Try using ServiceConnect."
SendMessage::nolink="The service `1` is not authenticated. Try using ServiceConnect."
ServiceConnect::multst="There are multiple connections stored in the `1` directory. Please specify an id."
ServiceConnect::nost="The specified connection could not be found. Try to create a new connection."
ServiceConnect::nostc="The given connection id is not stored in your cloud."
ServiceConnect::nsave="The connection could not be saved."
ServiceConnect::ncloud="The specified connection was not found locally. Use CloudConnect before ServiceConnect to check for cloud-stored connections."
ServiceConnect::verif="The service could not be authenticated."
ServiceExecute::nargs="The number of arguments given does not match the number of slots in the url template."
ServiceExecute::ratel="The rate limit for this query has been exceded."
ServiceExecute::nparam="The parameter `1` is required"
ServiceConnect::unkn="The service `1` is unknown, try providing authentication options."
ServiceConnect::invs="`1` should be a valid Service name or ServiceObject, try using $Services"
ServiceObject::noauth="The service is not authenticated. Try using ServiceConnect."
ServiceConnect::multser="One service was chosen from multiple `1` services."
ServiceExecute::multser="One service was chosen from multiple `1` services."
ServiceObject::noget="The parameter `1` is not available for the service `2`."
ServiceExecute::noget="The parameter `1` is not available for the service `2`."
ServiceExecute::apierr="The service returned the following error message: `1`."
ServiceExecute::reauth="The service returned the following error message: `1`. Try reauthenticating with ServiceConnect[service, \"New\"]."
ServiceExecute::ndata="The returned data is missing the `1` value."
ServiceInformation::nolink="The service `1` is not authenticated. Try using ServiceConnect."
ServiceConnect::nameid="The given connection id `1` is corresponds to a different service than the specified service, `2`."
ServiceDisconnect::nsc="The value given, `1`, should be a connected ServiceObject."
ServiceExecute::geoent="The given entity `1` does not include coordinates."
ServiceExecute::geon="The given expression `1` can not be interpreted as a location."

ServiceExecute::nval="Invalid value for parameter `1` in service `2`."
ServiceExecute::serror="Server error."
ServiceExecute::serrormsg="Server error. Error message: `1`"
ServiceExecute::invreq="The request `1` should be one of `2`[\"Requests\"], `2`[\"RawRequests\"], or the url for the desired api."
ServiceExecute::addperm="The requested operation requires additional permissions. Try again after authorizing the WolframConnector.";
ServiceConnections`SaveConnection::invso="The argument `1` should be an active ServiceObject."
ServiceConnections`SaveConnection::nosave="`1` is not an OAuth authenticated service, so the connection does not need to be saved."
ServiceConnections`SavedConnections::nosave="`1` is not an OAuth authenticated service, so the connections are not saved."
ServiceConnections`SavedConnections::invstr="The argument `1` should be the name of a service from $Services."
ServiceConnections`LoadConnection::nosave="`1` is not an OAuth authenticated service, so a connection does not need to be loaded."
ServiceConnections`LoadConnection::invstr="The argument `1` should be the name of a service from $Services."
ServiceConnections`DeleteConnection::invso="The argument `1` should be an active ServiceObject."
ServiceConnections`DeleteConnection::nodel="`1` is not an authenticated service, so the connection does not need to be deleted."

(************************************** ServiceConnect **********************************)
ServiceConnect[args___]:=With[{res=Catch[authenticate[args]]},
	res/;!FailureQ[res]
]

authenticate[]:=$Services

authenticate["Services"]:=authenticate[]

authenticate[so_ServiceObject, opts___]:=With[{id=getServiceID[so], name=getServiceName[so]},
	If[authenticatedServiceQ[id],
		SetDefaultParams[so, opts]; (*Setting new defaults*)
		so
		,
		If[StringQ[id]&&StringQ[name],
			authenticate[name, opts, id]

			,
			(Message[ServiceConnect::invs,so];$Failed)
		]
	]
]

authenticate[name_String, opts___?(!StringQ[#]&)]:=With[{so=checkservicelist[$authenticatedservices,name,ServiceConnect]},
	(
	  SetDefaultParams[so, Flatten[Normal[{opts}]]]; (*Setting new defaults*)
	  so
	)/;!FailureQ[so]
]

(*To allow either a connection-id or "New" string*)
authenticate[name_String, opts : ((_Rule | _RuleDelayed) ...) | _?AssociationQ, id___String]:=authenticate[name, Flatten[Normal[{opts}]], id]/;ArgumentCountQ[ServiceConnect, 1 + Length[{id}], 1, 2]

authenticate[name_String, opts_List?OptionQ, id___String?validuuidQ]:=authenticate0[name, opts, id]/;(MemberQ[$Services,name] && ArgumentCountQ[ServiceConnect, 1 + Length[{id}], 1, 2])

authenticate0[name_, opts_, id___String]:=Module[
  {initialParameters=FilterRules[opts,Except[$protectedopts]],service=OAuthClient`oauthauthenticate[name,FilterRules[opts,$protectedopts],id]},
  defaultParams[getServiceID[service]] = initialParameters;
  service
  ]/;(MemberQ[$oauthservices,name])

authenticate0[name_, opts_, id___String]:=Module[
  {initialParameters=FilterRules[opts,Except[$protectedopts]],service=KeyClient`keyauthenticate[name,FilterRules[opts,$protectedopts],id]},
  defaultParams[getServiceID[service]] = initialParameters;
  service
  ]/;(MemberQ[$keyservices,name])

authenticate0[name_, opts_, id___String]:=Module[
  {initialParameters=FilterRules[opts,Except[$protectedopts]],service=OtherClient`otherauthenticate[name,FilterRules[opts,$protectedopts],id]},
  defaultParams[getServiceID[service]] = initialParameters;
  service
  ]/;(MemberQ[$otherservices,name])

authenticate0[name_, opts_, id___String]:=Block[
  {status=Quiet[MatchQ[URLFetch[$ServiceConnectChannel,"StatusCode"],200]]},
  If[TrueQ[status],
	  pacletService[name, opts, id],
	  Block[{OAuthClient`Private`$AllowNonBlockingDialogsQ=False},pacletService[name, opts, id]]
	]
  ]/;(MemberQ[findallpacletsServices[],name]&& ArgumentCountQ[ServiceConnect, 1 + Length[{id}], 1, 2])

authenticate[name_String, opts_List?OptionQ, id___String?validuuidQ]:=pacletService[name, opts, id]/;(!MemberQ[$Services,name] && ArgumentCountQ[ServiceConnect, 1 + Length[{id}], 1, 2])

authenticate[___]:=$Failed

(**********************************Default Parameters**********************************)

SetDefaultParams[so_ServiceObject,opts__?OptionQ]:=
	Module[{dpnewlist=Flatten[List[opts]],id=getServiceID[so],newDefaultParams=Association[GetDefaultParams[so]]},
		AssociateTo[newDefaultParams,dpnewlist];
		defaultParams[id] = Normal[newDefaultParams];
		so
	]
SetDefaultParams[so_ServiceObject,None]:=
	Module[{id=getServiceID[so]},
		defaultParams[id] = {};
		so
	]
SetDefaultParams[so_ServiceObject,___]:= $Failed

GetDefaultParams[so_ServiceObject]:=defaultParams[getServiceID[so]]
GetDefaultParamsValues[so_ServiceObject]:=Values[GetDefaultParams[so]]
GetDefaultParamsNamesFormatted[so_ServiceObject]:=StringJoin[#,": "]&/@Keys[GetDefaultParams[so]]

GetDefaultParametersListFormatted[so_ServiceObject]:=
List[Dynamic[Column[BoxForm`SummaryItem/@Thread[{GetDefaultParamsNamesFormatted[so],Short[#,0.5]&/@GetDefaultParamsValues[so]}]]]]

(************************************** ServiceDisconnect *****************************)
ServiceDisconnect[args___]:=With[{res=Catch[servicedisconnect[args]]},
	res/;!FailureQ[res]
]/;ArgumentCountQ[ServiceDisconnect, Length[{args}], {1}]

servicedisconnect[service_ServiceObject]:=(Message[ServiceDisconnect::nsc,service];$Failed)/;!authenticatedServiceQ[service]

servicedisconnect[service_ServiceObject]:=OAuthClient`oauthdisconnect[service]/;serviceType[getServiceID[service]]==="OAuth"

servicedisconnect[service_ServiceObject]:=KeyClient`keydisconnect[service]/;serviceType[getServiceID[service]]==="APIKey"

servicedisconnect[service_ServiceObject]:=OtherClient`otherdisconnect[service]/;serviceType[getServiceID[service]]==="Other"

servicedisconnect[___]:=$Failed

(************************************** ServiceObject **********************************)
(* objects can be used as pure functions on certain parameter values *)
(service_ServiceObject)[args___]:=With[{res=Catch[serviceobjectdata[service, args]]},
	res/;!FailureQ[res]
]

serviceobjectdata[service_, ___]:=Message[ServiceObject::noauth]/;!authenticatedServiceQ[service]

serviceobjectdata[service_, req_String, rest___]:=Module[{},
	If[MemberQ[serviceobjectRequests[service],req],
		externalservice[service, req, rest]
		,
		Message[ServiceObject::noget,req, getServiceName[service]];Throw[$Failed]
	]
]

serviceobjectdata[___]:=$Failed

(*************************************** $Services **************************************)
$Services:=Union[$predefinedservices,findallpacletsServices[],serviceName/@$authenticatedservices]
(* Special Requests *)
$specialRequests={"Authentication", "ID", "Information", "Name", "Requests", "RawRequests"};
(************************************** ExternalService **********************************)
ServiceExecute[args___]:=With[{res=Catch[externalservice[args]]},
	res/;!FailureQ[res]
]

externalservice[name_String,rest___]:=With[{service=checkservicelist[$authenticatedservices, name, ServiceExecute]},
	If[FailureQ[service],
		If[MemberQ[$predefinedservices,name],
			externalservice[authenticate[name], rest],
			Message[ServiceExecute::nolink,name];Throw[$Failed]
		]
		,
		externalservice[service,rest]
	]
]

externalservice[service_ServiceObject,rest___]:=(Message[ServiceExecute::nolink,service];$Failed)/;!authenticatedServiceQ[service]
externalservice[service_ServiceObject,req_String,rest___]:=(Message[ServiceExecute::noget,req, getServiceName[service]];$Failed)/;!MemberQ[serviceobjectRequests[service],req]

externalservice[service_ServiceObject,"Name"]:=getServiceName[service]
externalservice[service_ServiceObject,"Requests"]:=With[{id=getServiceID[service]},
	DeleteCases[Union[serviceRequests[id],servicePosts[id],serviceDeletes[id],servicePuts[id],$specialRequests],"Requests"]]
externalservice[service_ServiceObject,"RawRequests"]:=With[{id=getServiceID[service]},
	Union[serviceRawRequests[id],serviceRawPosts[id],serviceRawDeletes[id],serviceRawPuts[id]]]
externalservice[service_ServiceObject,"ID"]:=getServiceID[service]
externalservice[service_ServiceObject,"Information"]:=serviceinfo[service,"Information"]
externalservice[service_ServiceObject,"Authentication"]:=OAuthClient`oauthdata[service,"Authentication"]/;serviceType[getServiceID[service]]==="OAuth"
externalservice[service_ServiceObject,"Authentication"]:={}

externalservice[service_ServiceObject,req_String, rules : ((_Rule | _RuleDelayed) ...) | _?AssociationQ]:=externalservice[service, req, Replace[{rules}, Association[asoc___] :> asoc, {1}]]
externalservice[service_ServiceObject,req_String, rules_List?OptionQ]:=(OAuthClient`oauthdata[service, req, rules])/;serviceType[getServiceID[service]]==="OAuth"
externalservice[service_ServiceObject,req_String, rules_List?OptionQ]:=(KeyClient`keydata[service, req, rules])/;serviceType[getServiceID[service]]==="APIKey"
externalservice[service_ServiceObject,req_String, rules_List?OptionQ]:=(OtherClient`otherdata[service, req, rules])/;serviceType[getServiceID[service]]==="Other"

externalservice[___]:=$Failed

(******************** ServiceInformation **********)
ServiceConnections`ServiceInformation[args___]:=With[{res=Catch[serviceinfo[args]]},
	res/;!FailureQ[res]
]

serviceinfo[service_ServiceObject,rest___]:=(serviceInfo[service, rest])/;(authenticatedServiceQ[service] && Quiet[ArgumentCountQ[ServiceConnections`ServiceInformation,1 + Length[{rest}], 1 ,2]])
serviceinfo[service_,___]:=(Message[ServiceInformation::nolink,service];$Failed)/;!authenticatedServiceQ[service]
serviceinfo[___]:=$Failed

serviceInfo[service_,"Information"]:=OAuthClient`OAuthServicesData[getServiceName[service],"Information"]/;serviceType[getServiceID[service]]==="OAuth"
serviceInfo[service_,"Information"]:=KeyClient`KeyServicesData[getServiceName[service],"Information"]/;serviceType[getServiceID[service]]==="APIKey"
serviceInfo[service_,"Information"]:=OtherClient`OtherServicesData[getServiceName[service],"Information"]/;serviceType[getServiceID[service]]==="Other"

serviceInfo[id_]:=""/;!authenticatedServiceQ[id]

(****************** SendMessage *******************)

SendMessage[args___]:=With[{res=Catch[sendmessage[args]]},
	res/;!FailureQ[res]]

sendmessage[name_String,rest___]:=With[{service=checkservicelist[$authenticatedservices, name, ServiceExecute]},
	If[FailureQ[service],
		If[MemberQ[$predefinedservices,name],
			sendmessage[authenticate[name], rest],
			Message[SendMessage::nolink,name];Throw[$Failed]
		]
		,
		sendmessage[service,rest]
	]
]

sendmessage[service_ServiceObject,rest___]:=(Message[SendMessage::nolink,service];Throw[$Failed])/;!authenticatedServiceQ[service]

sendmessage[service_ServiceObject,rest__]:=(OAuthClient`oauthsendmessage[getServiceName[service],getServiceID[service],rest])/;serviceType[getServiceID[service]]==="OAuth"
sendmessage[service_ServiceObject,rest__]:=(KeyClient`keysendmessage[getServiceName[service],getServiceID[service],rest])/;serviceType[getServiceID[service]]==="APIKey"
sendmessage[service_ServiceObject,rest__]:=(OtherClient`othersendmessage[getServiceName[service],getServiceID[service],rest])/;serviceType[getServiceID[service]]==="Other"

sendmessage["Email", rest__, o:OptionsPattern[]] := SendMail[rest, o]
sendmessage["Email" -> dest_, rest__, o:OptionsPattern[]] := SendMail[dest, rest, o]

sendmessage["Voice", rest__, o:OptionsPattern[]] := Speak[rest, o]

sendmessage[___]:=$Failed

(******************* pacletService **************************)

pacletService[name_, rest___]:=If[TrueQ[findandloadServicePaclet[name]],

		authenticate0[name, rest]
		,
		(Message[ServiceConnect::unkn,name];$Failed)
	]

findandloadServicePaclet[name_]:=With[{paclet=findservicepaclet[name]},
	If[Head[paclet]=!=PacletManager`Paclet,
		Return[$Failed]
	];
	loadServicePaclet[paclet]
]

findservicepaclet[name_]:=Block[{fullname=createPacletName[name], local},
	local=PacletManager`PacletFind[fullname];
	Switch[Length[local],
		0,findservicepacletRemote[name,fullname],
		_,
		First[local]
	]
]

findservicepacletRemote[name_,fullname_]:=Block[{remote, paclet},
	remote=PacletManager`PacletFindRemote[fullname];
	paclet=Switch[Length[remote],
		0,(Message[ServiceConnect::unkn, name];Return[$Failed]),
		_,
		First[remote]
	];
	If[Head[paclet]===PacletManager`Paclet,
		PacletManager`PacletInstall[paclet],
		$Failed
	]
]

createPacletName[name_]:="ServiceConnection_"<>name

loadServicePaclet[paclet_]:=Block[{location, file},
	location=Lookup[PacletManager`PacletInformation[paclet], "Location", "location"];
	If[location==="location"||!StringQ[location],
		Return[$Failed]
	];
	file=FileNameJoin[{location,"Kernel","load.m"}];
	If[FileExistsQ[file],
		If[!ListQ[$Services],Get["OAuth`"]];
		Get[file];
		True,
		$Failed
	]
]

localpacletServices[]:=Union[StringReplace[#["Name"]& /@
	PacletManager`PacletFind["ServiceConnection_*"], "ServiceConnection_" -> ""]]

findallpacletsServices[]:=Union[StringReplace[#["Name"]& /@
	Select[Join[PacletManager`PacletFindRemote["ServiceConnection_*"],
		PacletManager`PacletFind["ServiceConnection_*"]],
	#["Public"] =!= False &], "ServiceConnection_" -> ""]]

(****************** Utilities *********************)
servicesdata[name_,property_]:=(OAuthClient`OAuthServicesData[name,property])/;MemberQ[$oauthservices,name]
servicesdata[name_,property_]:=(KeyClient`KeyServicesData[name,property])/;MemberQ[$keyservices,name]
servicesdata[name_,property_]:=(OtherClient`OtherServicesData[name,property])/;MemberQ[$otherservices,name]
servicesdata[___]:=$Failed

appendservicelist[service_ServiceObject,type_]:=appendservicelist[getServiceName[service],type]

appendservicelist[name_String,"OAuth"]:=($oauthservices=Union[Append[$oauthservices,name]])
appendservicelist[name_String,"APIKey"]:=($keyservices=Union[Append[$keyservices,name]])
appendservicelist[name_String,"Other"]:=($otherservices=Union[Append[$otherservices,name]])

appendauthservicelist[service_]:=($authenticatedservices=Union[Append[$authenticatedservices,service]])
appendsavedservicelist[services_]:=($savedservices=Union[Append[$savedservices,services]])
removefromsavedservicelist[service_]:=($savedservices=DeleteCases[$savedservices,service])

makeuuid[]:=StringJoin["connection-",IntegerString[RandomInteger[{0, 16^32}], 16, 32]]
validuuidQ[id_String]:=StringMatchQ[id, "New" | ("connection-" ~~ RegularExpression["(\\w){32}"])]

createServiceObject[type_,name_, token_, id0_:Automatic, authQ_:True]:=Module[{link, id},
	id=If[id0===Automatic,makeuuid[], id0];
	link=ServiceObject[name, "ID"->id];
	appendservicelist[link,type];
	If[authQ,appendauthservicelist[id]];

	serviceName[id]=name;
	serviceRawRequests[id]={};
	serviceRequests[id]={};
	serviceRawPosts[id]={};
	servicePosts[id]={};
	serviceRawDeletes[id]={};
	serviceDeletes[id]={};
	serviceRawPuts[id]={};
	servicePuts[id]={};
	serviceAuthentication[id]=token;
	serviceType[id]:=type;
	urlfetchFun[id]=URLFetch;
	refreshFun[id]=None;
	link
]

getQueryData[id_,property_]:=With[{data=servicesdata[serviceName[id],property]},
	(* URL, method, pathparams, params, bodyparams, mpdata, headers, requiredparams, returncontentdata *)
	{Lookup[data,"URL",""],
	Lookup[data,"HTTPSMethod","GET"],
	listwrap@Lookup[data,"PathParameters",{}],
	listwrap@Lookup[data,"Parameters",{}],
	listwrap@Lookup[data,"BodyData",{}],
	listwrap@Lookup[data,"MultipartData",{}],
	listwrap@Lookup[data,"Headers",{}],
	listwrap@Lookup[data,"RequiredParameters",{}],
	listwrap@Lookup[data,"RequiredPermissions",{}],
	Lookup[data,"ReturnContentData",False],
	Lookup[data,"IncludeAuth",True]}
]

setQueryData[id_,Rule[prop_,data_]]:=getQueryData[id,prop]=data

servicedata[id_]:=Association[{
	"ServiceName"->serviceName[id],
	"ID"->id,
	"RawRequests"->serviceRawRequests[id],
	"Requests"->serviceRequests[id],
	"RawPostRequests"->serviceRawPosts[id],
	"PostRequests"->servicePosts[id],
	"RawDeleteRequests"->serviceRawDeletes[id],
	"DeleteRequests"->serviceDeletes[id],
	"RawPutRequests"->serviceRawPuts[id],
	"PutRequests"->servicePuts[id],
	"Authentication"->serviceAuthentication[id],
	"Information"->serviceInfo[id]
}]

availablequeries[id_]:=Join[serviceRequests[id],serviceRawRequests[id],servicePosts[id],serviceRawPosts[id],serviceDeletes[id],serviceRawDeletes[id],servicePuts[id],serviceRawPuts[id],
	$specialRequests]/;authenticatedServiceQ[id]
availablequeries[_]:={}

getServiceObject[id_]:=ServiceObject[serviceName[id],"ID"->id]

getServiceName[ServiceObject[args___]]:=First[{args}]
getServiceID[service_ServiceObject]:=Lookup[Rest[List @@ service], "ID"]
getServiceIcon[service_ServiceObject]:=With[{icon=servicesdata[getServiceName[service],"icon"]},
	If[MatchQ[icon,_Image|_Graphics],icon,defaultServiceObjectIcon]]

serviceName[id_]:=None/;!authenticatedServiceQ[id]
serviceRawRequests[id_]:={}/;!authenticatedServiceQ[id]
serviceRequests[id_]:={}/;!authenticatedServiceQ[id]
serviceRawPosts[id_]:={}/;!authenticatedServiceQ[id]
servicePosts[id_]:={}/;!authenticatedServiceQ[id]
serviceRawDeletes[id_]:={}/;!authenticatedServiceQ[id]
serviceDeletes[id_]:={}/;!authenticatedServiceQ[id]
serviceRawPuts[id_]:={}/;!authenticatedServiceQ[id]
servicePuts[id_]:={}/;!authenticatedServiceQ[id]
serviceAuthentication[id_]:={}/;!authenticatedServiceQ[id]

urlfetchFun[id_]:=URLFetch/;!authenticatedServiceQ[id]
refreshFun[id_]:=None/;!authenticatedServiceQ[id]
serviceType[id_]:=None/;!authenticatedServiceQ[id]
logout[id_]:=Null/;!authenticatedServiceQ[id]

serviceobjectRequests[service_]:=With[{id=getServiceID[service]},
	availablequeries[id]
]

authenticatedServiceQ[service_ServiceObject]:=authenticatedServiceQ[getServiceID[service]]
authenticatedServiceQ[id_]:=MemberQ[$authenticatedservices,id]

savedServiceQ[service_ServiceObject]:=savedServiceQ[getServiceID[service]]
savedServiceQ[id_]:=MemberQ[$savedservices,id]

checkservicelist[list_, name_String, fn_]:=With[{matches=Select[list,serviceName[#]===name&]},
	Switch[Length[matches],
		1,getServiceObject[First[matches]],
		0,$Failed,
		_,Message[fn::multser,name];getServiceObject[First[matches]]
	]
]

listwrap[l_List]:=l
listwrap[x_]:={x}

insertpathparameters[url0_,pathparams_,pvpairs_]:=Module[{pparams1},
		pparams1= Lookup[pvpairs, pathparams, Automatic];
		Check[url0@@(pparams1),Message[ServiceExecute::nargs];Throw[$Failed]]
]

(********************************** ServiceObject Typesetting ******************************)

ServiceObject/:
MakeBoxes[service_ServiceObject, form:StandardForm|TraditionalForm] :=
With[{below0=GetDefaultParams[service],below=GetDefaultParametersListFormatted[service], name=getServiceName[service], id=getServiceID[service], icon=getServiceIcon[service]},
	If[$VersionNumber>=10,
		BoxForm`ArrangeSummaryBox[
			(* Head *)ServiceObject,
			(* Interpretation *)service,
			(* Icon *)icon,
			(* Column or Grid *){name, (* id, *) Dynamic[If[TrueQ[authenticatedServiceQ[id]],"Connected","Not Connected"]]},
			(* Plus Box Column or Grid *)
				{(*
					BoxForm`SummaryItem[{"ID: ", id}],
					Dynamic[Switch[
						{TrueQ[authenticatedServiceQ[id]],TrueQ[savedServiceQ[id]]},
						{True, True},
						ChoiceButtons[{"Delete","Disconnect"},{ServiceConnections`DeleteConnection[service],ServiceDisconnect[service]}],
						{True, False},
						ChoiceButtons[{"Save","Disconnect"},{ServiceConnections`SaveConnection[service],ServiceDisconnect[service]}],
						{False,True},
						ChoiceButtons[{"Delete","Connect"},{ServiceConnections`DeleteConnection[service],ServiceConnect[service]}],
						_,
						""
					]
					],*)
					If[MatchQ[below0,{}],Sequence@@{},below]
				},
			form]
			,
		InterpretationBox[#,service]&@ToBoxes[Framed@Row[{"ServiceObject       ",Column[{name, id, "Authenticated"->authenticatedServiceQ[id]}]}]]
	]
]


Unprotect[ServiceObject]

ServiceObject /: SetOptions[so_ServiceObject, opts: {} | <||> | None] := (SetDefaultParams[so, None])
ServiceObject /: SetOptions[so_ServiceObject, opts: (((_Rule | _RuleDelayed) ..) | _List | _Association)?OptionQ] := (SetDefaultParams[so, Flatten[Normal[{opts}]]])

ServiceObject /: HoldPattern[Options][so_ServiceObject] := GetDefaultParams[so]
Protect[ServiceObject]
(**)
defaultServiceObjectIcon=BoxForm`GenericIcon[LinkObject]

End[];
End[];

SetAttributes[{
  ServiceConnect,ServiceDisconnect,ServiceExecute,SendMessage,ServiceObject,ServiceConnections`ServiceInformation,$Services
},
   {ReadProtected, Protected}
];


{System`ServiceConnect,System`ServiceDisconnect,
System`ServiceExecute,System`SendMessage,
System`ServiceObject}
