
(Unprotect[#]; Clear[#])& /@ {
  ServiceConnections`ServiceConnections,
  ServiceConnections`SavedConnections,
  ServiceConnections`SaveConnection,
  ServiceConnections`LoadConnection,
  ServiceConnections`DeleteConnection
}

Begin["ServiceConnections`"];

ServiceConnections`ServiceConnections;
ServiceConnections`SavedConnections;
ServiceConnections`SaveConnection;
ServiceConnections`LoadConnection;
ServiceConnections`DeleteConnection;

Begin["`Private`"];

ServiceConnections`ServiceConnections::usage="ServiceConnections returns a list of active ServiceObjects.";
ServiceConnections`SavedConnections::usage="SavedConnections returns a list of available saved ServiceConnections.";
ServiceConnections`SaveConnection::usage="SaveConnection saves an active ServiceObject in the user's account.";
ServiceConnections`LoadConnection::usage="LoadConnection loads a saved ServiceObject from the user's account.";
ServiceConnections`DeleteConnection::usage="DeleteConnection removes a saved connection from the user's account.";

ServiceConnections`ServiceConnections[args___]:=Catch[serviceConnections[args]]

serviceConnections[]:=serviceConnections[All]

serviceConnections[All]:=Cases[serviceObject/@$authenticatedservices,_ServiceObject,{1}]

serviceConnections["OAuth"]:=Cases[serviceObject/@serviceconnections[$authenticatedservices,$oauthservices],_ServiceObject,{1}]
serviceConnections["APIKey"]:=Cases[serviceObject/@serviceconnections[$authenticatedservices,$keyservices],_ServiceObject,{1}]

serviceConnections[str_String]:=Cases[serviceObject/@serviceconnections[$authenticatedservices,{str}],_ServiceObject,{1}]

serviceConnections[___]:=Throw[$Failed]

serviceconnections[ids_,typenames_]:=Select[ids,MemberQ[typenames,serviceName[#]]&]

serviceObject[id_]:=With[{name=serviceName[id]},
	If[StringQ[name],
		ServiceObject[name,"ID"->id]
	]
]



ServiceConnections`SavedConnections[arg_]:=Catch[savedConnections[arg]]

savedConnections[name_String]:=savedconnections[name]/;MemberQ[$Services,name]

savedConnections[name_String]:=If[TrueQ[findandloadServicePaclet[name]],
	savedconnections[name],
	(Message[ServiceConnections`SavedConnections::invstr,name];Throw[$Failed])
]

savedConnections[expr_]:=(Message[ServiceConnections`SavedConnections::invstr,expr];Throw[$Failed])

savedConnections[_]:=$Failed

savedconnections[name_]:=With[{ids=savedconnections0[name]},
	appendsavedservicelist[ids];
	ServiceObject[name,"ID"->#]&/@ids
]

savedconnections0[name_String]:=OAuthClient`findSavedOAuthConnections[name]/;MemberQ[$oauthservices,name]

savedconnections0[name_String]:=If[TrueQ[findandloadServicePaclet[name]],
	If[MemberQ[$oauthservices,name],
		savedconnections[name],
		(Message[ServiceConnections`SavedConnections::nosave,name];Throw[$Failed])
	]
]/;MemberQ[localpacletServices[],name]

savedconnections0[name_String]:=(Message[ServiceConnections`SavedConnections::nosave,name];$Failed)

savedconnections[___]:=$Failed



ServiceConnections`SaveConnection[args___]:=Catch[saveConnection[args]]

saveConnection[so_ServiceObject,rest___]:=saveconnection[{getServiceID[so],getServiceName[so],so}, rest]

saveConnection[expr_,___]:=(Message[ServiceConnections`SaveConnection::invso,expr];Throw[$Failed])

saveConnection[___]:=$Failed

saveconnection[{id_String,name_String,so_}, rest___]:=With[{res=saveconnection0[{id, name, so},rest]},
	If[!FailureQ[res],
		appendsavedservicelist[{id}]
	];
	res
]/;authenticatedServiceQ[id]

saveconnection[{_,_,so_},___]:=(Message[ServiceConnections`SaveConnection::invso,so];Throw[$Failed])

saveconnection0[{id_String,name_String,so_}, rest___]:=OAuthClient`saveOAuthConnection[so,rest]/;MemberQ[$oauthservices,name]
saveconnection0[{id_String,name_String,so_}, rest___]:=(Message[ServiceConnections`SaveConnection::nosave,name];$Failed)

saveconnection[___]:=$Failed

saveconnection0[___]:=$Failed



ServiceConnections`LoadConnection[args___]:=Catch[loadConnection[args]]

loadConnection[name_String,rest___]:=loadconnection[name, rest]/;MemberQ[$Services,name]

loadConnection[name_String,rest___]:=If[TrueQ[findandloadServicePaclet[name]],
	loadconnection[name, rest],
	(Message[ServiceConnections`LoadConnection::invstr,name];Throw[$Failed])
]

loadConnection[expr_,___]:=(Message[ServiceConnections`LoadConnection::invstr,expr];Throw[$Failed])

loadConnection[___]:=$Failed

loadconnection[name_String, rest___]:=With[{res=loadconnection0[name, rest]},
	If[Head[res]===ServiceObject,
		appendsavedservicelist[{getServiceID[res]}]
	];
	res
]


loadconnection0[name_String, rest___]:=OAuthClient`loadOAuthConnection[name,rest]/;MemberQ[$oauthservices,name]

loadconnection0[name_String, rest___]:=If[TrueQ[findandloadServicePaclet[name]],
	If[MemberQ[$oauthservices,name],
		loadconnection0[name, rest],
		(Message[ServiceConnections`LoadConnection::nosave,name];Throw[$Failed])
	]
]/;MemberQ[localpacletServices[],name]

loadconnection0[name_String, rest___]:=(Message[ServiceConnections`LoadConnection::nosave,name];$Failed)

loadconnection[___]:=$Failed
loadconnection0[___]:=$Failed



ServiceConnections`DeleteConnection[args___]:=Catch[deleteConnection[args]]

deleteConnection[so_ServiceObject,rest___]:=deleteconnection[{getServiceID[so],getServiceName[so],so}, rest]

deleteConnection[expr_,___]:=(Message[ServiceConnections`DeleteConnection::invso,expr];Throw[$Failed])

deleteConnection[___]:=$Failed

deleteconnection[{id_String,name_String,so_}, rest___]:=With[{res=deleteconnection0[{id, name, so},rest]},
	If[!FailureQ[res],
		removefromsavedservicelist[id]
	]
]/;authenticatedServiceQ[id]

deleteconnection[{_,_,so_},___]:=(Message[ServiceConnections`DeleteConnection::invso,so];Throw[$Failed])

deleteconnection0[{id_String,name_String,so_}, rest___]:=OAuthClient`deleteOAuthConnection[name, id]/;MemberQ[$oauthservices,name]
deleteconnection0[{id_String,name_String,so_}, rest___]:=(Message[ServiceConnections`SaveConnection::nosave,name];$Failed)

deleteconnection[___]:=$Failed

deleteconnection0[___]:=$Failed



End[];
End[];

SetAttributes[{
	ServiceConnections`ServiceConnections,
	ServiceConnections`SavedConnections,
	ServiceConnections`SaveConnection,
	ServiceConnections`LoadConnection,
	ServiceConnections`DeleteConnection
},
   {ReadProtected, Protected}
];