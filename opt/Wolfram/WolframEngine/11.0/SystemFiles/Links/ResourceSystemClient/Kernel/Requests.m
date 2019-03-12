(* Wolfram Language Package *)

BeginPackage["ResourceSystemClient`"]
(* Exported symbols added here with SymbolName::usage *)  
ResourceSystemClient`$ResourceSystemRequestBase
Begin["`Private`"] (* Begin Private Context *) 

ResourceSystemClient`$ResourceSystemRequestBase:=$resourceSystemRequestBase

$resourceSystemRequestBase:=resourcesystembase[$CloudBase]

resourcesystembase[wolframcloud_]:=URLBuild[{wolframcloud, "objects/resourcesystem/api/1.0"}]/;!StringFreeQ[wolframcloud,"wolframcloud"]
resourcesystembase[_]:=(Message[ResourceObject::unkbase];Missing[])


$UnauthenticatedRequests={};
$ResourceSystemClientVersion="0.1";

$ClientInformation:=($ClientInformation={
	"PacletVersion"->$ResourceSystemClientVersion,
	"WLVersion"->ToString[$VersionNumber],
	"MachineType"->ToString[$MachineType],
	"WordLength"->ToString[$SystemWordLength]
})


apifun[endpoint_,params_, head_]:=Block[{code, headers,chars, res, fetchfun, base},
	fetchfun=If[$CloudConnected,
		CloudObject`Private`authenticatedURLFetch,
		If[MemberQ[$UnauthenticatedRequests,endpoint],
			  URLFetch
			  ,
			  cloudConnect[head];
			  CloudObject`Private`authenticatedURLFetch
		]
	];
	res=fetchfun[StringJoin[$resourceSystemRequestBase,"/",endpoint],{"StatusCode","Headers","ContentData"},
		"Parameters"->prepareParams[endpoint,params],"VerifyPeer"->False,"CredentialsProvider" -> None,Sequence@@additionalOptions[endpoint]];
	If[Length[res]===3,
		{code,headers, chars}=res,
		Message[head::unavail];Throw[$Failed]
	];
	If[code=!=200,
		handleError[head, endpoint, code, chars]
		,
		importResponse[head, endpoint, headers,chars]
	]
]/;StringQ[$resourceSystemRequestBase]

apifun[___]:=(Message[ResourceObject::unkbase];Throw[$Failed])

$CompressedParameterEndpoints=("SubmitResource"|"ReadResource"|"CopyResource")

prepareParams[$CompressedParameterEndpoints,params_]:={"ContentFormat"->"Compressed",(Sequence@@$ClientInformation),"Data"->Compress[params]}
prepareParams[_,{}]:=$ClientInformation
prepareParams[_,params_]:=Join[MapAt[ToString,params,{All,2}],$ClientInformation]

handleError[head_, endpoint_, code_, chars_]:=(Message[head::apierr,getErrorMessage[chars]];Throw[$Failed])

importResponse[head_,req_,headers_,chars_]:=Block[{mime, expr},
	mime=Cases[headers,{"Content-Type"|"content-type"|"ContentType",ct_}:>ct,{1},1];
	If[Length[mime]>0,mime=First[mime]];
	expr=If[!StringFreeQ[mime,"json",IgnoreCase->True],
		importJSONResponse[chars],
		ToExpression[FromCharacterCode[chars]]
	];
	importresponse[head, req,expr]	
]


importresponse[head_,req_, as_Association]:=importresponse[head,req,compressedCheck[head,warningCheck[head,as]]]/;KeyExistsQ[as,"Format"]
importresponse[head_,_,as_Association]:=warningCheck[head,as]
importresponse[_,_,expr_]:=expr

importJSONResponse[chars_]:=With[{l=ImportString[FromCharacterCode[chars],"JSON"]},
	If[ListQ[l],Association[l],Throw[$Failed]]
]

compressedCheck[head_,as_Association]:=With[{format=as["Format"]},
    If[format==="Compressed",
    	Uncompress[as["Data"]],
    	If[KeyExistsQ[as,"Data"],
    		as["Data"],
    		as
    	]
    ]
]/;KeyExistsQ[as,"Format"]

compressedCheck[_,expr_]:=expr


warningCheck[head_,as_Association]:=With[{warnings=Lookup[as,"Warnings",{}]},
	Message[head::apiwarn,#]&/@warnings;
	as
]/;KeyExistsQ[as,"Warnings"]

warningCheck[_,expr_]:=expr

additionalOptions["SubmitResource"]:={"Method"->"POST"}
additionalOptions[_]:={}

getErrorMessage[chars_]:=Block[{str=FromCharacterCode[chars], list},
	Quiet[list=ImportString[str,"JSON"];
		If[ListQ[list],
			If[KeyExistsQ[list,"Message"],
			    Lookup[list,"Message"]
			    ,
			    str
			],
			str
		]
	]]
	
End[] (* End Private Context *)

EndPackage[]